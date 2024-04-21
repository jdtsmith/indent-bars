;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024  J.D. Smith

;;; Commentary:

;; tree-sitter support for indent-bars:

;;  - Optionally uses tree-sitter to fine-tune indentation depth
;;    (avoiding "too many bars" in e.g. argument lists)
;;  - Uses treesitter to avoid adding bars to blank lines in strings
;;    or other configurable node types.
;;  - Enable alternate out-of-scop styling for bars outside the
;;    treesitter "scope", where scope is defined as the wrapping
;;    elements configured in `indent-bars-treesitscope`.

;; For Developers:
;;
;; This file interfaces treesitter scope queries with font-lock-based
;; bar drawing.  Some of this happens during normal font-lock,
;; querying treesitter for node information at point (typically at the
;; beginning of the line).  The scope highlight capability depends on
;; the position of the point, and so requires some subtle interface
;; between movement, tree-sitter, and font-lock.  At any given point
;; there is a scope range (pair of markers) which determines how the
;; bars get displayed.  Small movements can change the scope.
;; 
;; The technique is as follows:
;; 
;;  - A post-command hook sets up an idle-time callback if none exists.
;;  - In idle time, this checks for changes to the associated buffer
;;    or position of point.
;;  - If either of these has changed, they are saved, and tree-sitter
;;    is queried for the innermost "scope" node configured for the
;;    buffer's language.  Short nodes occupying too-few lines are not
;;    considered.  A nil scope node indicates the entire file is the
;;    scope.
;;  - Font-lock operations occur on an "extended window", which is the
;;    current window bounds +- 50%.
;;  - If the scope node's bounds have not changed from their last
;;    stored (marker) values (modulo insertions/deletions), and the
;;    prior extended window fully encloses the the current window
;;    bounds, no update is needed.
;;  - If either the node has changed, or it has not changed, but its
;;    prior extended window bounds have been breached, mask
;;    (intersect) the new scope node with the extended window:
;;  - Store the new masked node range.
;;  - Compute the union of the new and old masked node range bounds,
;;    and flush font-lock over the associated region(s): typically
;;    just a few tens of lines each.
;;  - Font-lock automatically re-applies indentation bars, consulting
;;    the saved masked node range to determine whether a given line is
;;    in-scope or out-of-scope.
;;    
;;
;; Note the shorthand substitutions for
;; style related prefixes (slot accessors and variables); see
;; file-local-variables at the end:
;; 
;;    ibts/  => indent-bars-ts-scope- (slot accessors)
;;    ibtcs  => indent-bars-ts-current-scope (scope struct)

;;; Code:

;;;; Requires
(require 'cl-lib)
(require 'seq)
(require 'indent-bars)
(require 'treesit nil t)

;;;; Customization
(defgroup indent-bars-ts nil
  "Customization group for indent-bars treesitter options."
  :group 'indent-bars
  :prefix "indent-bars-ts-")

(defgroup indent-bars-ts-style nil
  "Customization group for indent-bars treesitter out-of-scope styling."
  :group 'indent-bars
  :prefix "indent-bars-ts-")

;;;;; Alternate Style Variables
(defun indent-bars-ts--add-customs ()
  "Add all the relevant custom variables for the out-of-scope ts style."
  (cl-labels ((ts-cust (var &rest r)
		(eval `(indent-bars--alt-custom
			"ts" ,var "Tree-sitter (out-of-scope)" ,@r))))
      (ts-cust 'color '(unspecified :blend 0.1) 'add-inherit)
      (dolist (c '( width-frac pad-frac pattern zigzag ;simple types
		    no-stipple-char-font-weight))
	(ts-cust c 'unspecified))
      (dolist (c '(color-by-depth highlight-current-depth))
	(ts-cust c 'unspecified 'add-inherit))))

;;;;; Treesit Customs
(defcustom indent-bars-treesit-wrap nil
  "An alist of language and treesitter node type symbols to wrap.
Inside such wrapping types, indentation bar depth will not be
increased more than one beyond that of the containing node's
depth.  This is typically done for lists, parameters, function
arguments, etc., to avoid unwanted \"extra bars\".  Types must be
valid node types for the grammar of the language indicated.  Note
that the (non-treesitter) options `indent-bars-no-descend-string'
and `indent-bars-no-descend-lists', if set, take precedence over
this option."
  :type '(choice (const :tag "No wrap types" nil)
		 (alist :tag "Alist of node types"
			:key-type (symbol :tag "Language")
			:value-type (repeat :tag "Types" (symbol :tag "Type"))))
  :group 'indent-bars-ts)

(defcustom indent-bars-treesit-scope nil
  "An alist of language and treesitter node types to emphasize.
If non-nil, indentation bars on text outside of the innermost
matching treesitter scope will use the alternative
\"out-of-scope\"style specified in the indent-bars-ts-os-* custom
variables, which mirror and inherit from the normal style
variables."
  :type '(choice (const :tag "No scope types" nil)
		 (alist :tag "Alist of node types"
			:key-type (symbol :tag "Language")
			:value-type (repeat :tag "Types" (symbol :tag "Type"))))
  :group 'indent-bars-ts)

(defcustom indent-bars-treesit-scope-min-lines 3
  "Minimum number of lines a node must span to be counted as a scope."
  :type 'integer
  :group 'indent-bars-ts)

(defcustom indent-bars-treesit-ignore-blank-lines-types nil
  "Do not style blank lines when the type of node at start is in this list.
Either nil, or a list of node type strings to avoid adding blank
line styling to.  Typically \"top-level\" node types like
\"module\", \"program\", and \"translation_unit\" would be used
here, and they need not be valid types for any particular
grammar.  Only applicable if `indent-bars-display-on-blank-lines'
is set."
  :type '(choice (const :tag "None" nil)
		 (repeat :tag "Node types" string))
  :group 'indent-bars-ts)

(defcustom indent-bars-treesit-update-delay 0.125
  "Idle time in seconds for treesitter scope updates to occur.
This has effect only if `indent-bars-treesit-scope' is non-nil."
  :type 'float
  :group 'indent-bars-ts)

;;;; Node Utilities
(defvar-local indent-bars-ts--parser nil)
(defvar-local indent-bars-ts--wrap-query nil)
(defvar-local indent-bars-ts--string-query nil)

(defsubst indent-bars-ts--node-spans-p (node start end)
  "Return NODE if it fully spans START..END.
Otherwise return nil."
  (and (<= (treesit-node-start node) start)
       (>= (treesit-node-end node) end)
       node))

(defun indent-bars-ts--node-query (node query &optional
					start-only spanning min-newlines)
  "Capture node(s) matching QUERY which overlap with NODE.
QUERY is a compiled treesit query.  If START-ONLY is non-nil, the
query searches for matching nodes which overlap with NODE's
starting position.  Otherwise nodes which intersect anywhere with
NODE will be returned.

If SPANNING is non-nil, return a single spanning node which fully
spans the start..end range of NODE, if any (or just the start, if
START-ONLY is non-nil).  If SPANNING is \\='innermost, return the
latest (innermost) node on the list which fully spans NODE, which
could include NODE itself if it matches the QUERY.  For any other
non-nil value of SPANNING, check if the first node matched by
QUERY spans NODE and return it if so.  If MIN-NEWLINES is a
number, a spanning node will be returned only if spans at least
that many newlines.  E.g. MIN-NEWLINES=1 demands a two line
node (or larger).

If no spanning node is found, nil is returned."
  (when-let ((start (treesit-node-start node))
	     (end (if start-only start (treesit-node-end node)))
	     (nodes (treesit-query-capture indent-bars-ts--parser query
					   start end t)))
    (cond ((eq spanning 'innermost)
	   (cl-loop for n in (nreverse nodes)
		    if (and (or (eq n node)
				(indent-bars-ts--node-spans-p n start end))
			    (or (not min-newlines)
				(>= (count-lines
				     (treesit-node-start n) (treesit-node-end n))
				    min-newlines)))
		    return n))
	  (spanning  ; check first node, if it doesn't span, none will
	   (when-let ((n (indent-bars-ts--node-spans-p (car nodes) start end))
		      ((or (not min-newlines)
			   (>= (count-lines
				(treesit-node-start n) (treesit-node-end n))
			       min-newlines))))
	     n))
	  (t nodes))))

(defsubst indent-bars--indent-at-node (node)
  "Return the current indentation at the start of TS node NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (current-indentation)))

;;;; Limiting Indentation depth: string and context-dependent
(defun indent-bars-ts--update-indentation-depth (d)
  "Update target depth D using the tree-sitter region.
Searches for parent nodes with types specified in
`indent-bars-treesit-wrap' for the current buffer's language,
and, if found, limits the indentation depth to one more than the
topmost matching parent node's first line indentation depth.  If
`indent-bars-no-descend-string' is non-nil, also looks for
enclosing string and marks indent depth no deeper than one more
than the indentation depth at string start.  This reduces depth
inside strings, and for wrapping contexts (e.g. function
arguments)."
  (if-let (((not (bobp)))
	   (node (treesit-node-on (1- (point)) (point) indent-bars-ts--parser))
	   (dnew
	    (if (and indent-bars-ts--string-query
		     (indent-bars-ts--node-query
		      node indent-bars-ts--string-query t))
		;; A string: do not descend
		(1+ (indent-bars--depth (indent-bars--indent-at-node node)))
	      ;; Check wrap context
	      (when-let (( indent-bars-ts--wrap-query)
			 (ctx (indent-bars-ts--node-query
			       node indent-bars-ts--wrap-query nil t)))
		(1+ (indent-bars--depth
		     (indent-bars--indent-at-node ctx)))))))
      (min dnew d)
    d))

;;;; Ignoring Certain Blank Lines
;; see also ts--handle-blank-lines, below
(defun indent-bars-ts--ignore-blank (beg)
  "See if blank lines at BEG should be ignored using tree-sitter.
Blank lines to ignore are those within nodes of the types
mentioned in `indent-bars-treesit-ignore-blank-lines-types'."
  (and indent-bars-ts--parser
       indent-bars-treesit-ignore-blank-lines-types
       (when-let ((n (treesit-node-on beg beg)))
	 (seq-contains-p indent-bars-treesit-ignore-blank-lines-types
			 (treesit-node-type n)))))

;;;; Scope Highlighting
(defvar indent-bars-ts-out-scope-style nil)
(cl-declaim (optimize (safety 0))) ; no need for type check
(cl-defstruct
    (indent-bars-ts-scope
     (:copier nil)
     (:conc-name ibts/)
     (:constructor ibts/create))
  "A structure for tracking treesitter-based scope information."
  ( range (cons (point-min-marker) (point-min-marker)) :type cons
    :documentation "The current scope node's range.")
  ( clip-win (cons (point-max-marker) (point-max-marker)) :type cons
    :documentation "The clipping window for the current scope.")
  ( start-bars 0 :type integer
    :documentation "The number of bars shown at start of current scope.")
  ( invalid-ranges nil :type list
    :documentation "List of current invalid ranges -- (start . end) conses.")
  ( query nil :type ts-query
    :documentation "The treesitter scope query object."))

(defvar-local ibtcs nil  ; N.B. see shorthands at bottom of file
  "The current `indent-bars-ts-scope' struct.")

(defsubst indent-bars-ts--out-of-scope (pos)
  "Return whether POS is outside the current treesitter scope.
If there is no scope defined, every position is considered in
scope."
  (and ibtcs (or (< pos (car (ibts/range ibtcs)))
		 (> pos (cdr (ibts/range ibtcs))))))

(defun indent-bars-ts--display ()
  "Display indentation bars, accounting for current treesitter scope."
  (if (indent-bars-ts--out-of-scope (match-beginning 1))
      (indent-bars--display (match-beginning 1) (match-end 1)
			    indent-bars-ts-out-scope-style)
    ;; In scope: switch from out to in-scope style
    (indent-bars--display (match-beginning 1) (match-end 1)
			  indent-bars-ts-out-scope-style
			  (ibts/start-bars ibtcs)
			  indent-bars-style)))

(defun indent-bars-ts--handle-blank-lines ()
  "Display bars on blank lines, respecting treesitter scope."
  (let ((beg (match-beginning 0)))
    (unless (indent-bars-ts--ignore-blank beg)
      (if (indent-bars-ts--out-of-scope beg) ;fully out of scope
	  (indent-bars--handle-blank-lines (match-beginning 0) (match-end 0)
					   indent-bars-ts-out-scope-style)
	;; Switch from out of scope to in scope after start-bars
	(indent-bars--handle-blank-lines (match-beginning 0) (match-end 0)
					 indent-bars-ts-out-scope-style
					 (ibts/start-bars ibtcs)
					 indent-bars-style)))))

(defmacro indent-bars-ts--order-ranges (a b)
  "Order ranges A and B by start position."
  `(if (< (car ,b) (car ,a)) (setq ,b (prog1 ,a (setq ,a ,b)))))

(defun indent-bars-ts--union (a b)
  "Return the union between ranges A and B.
Ranges A and B are (start . end) conses.  Their union is a list
of ranges that either cover."
  (indent-bars-ts--order-ranges a b)
  (if (< (cdr a) (car b))
      (list a b) ; no overlap, use both
    (list (cons (car a) (max (cdr a) (cdr b))))))

(defun indent-bars-ts--union-all (ranges)
  "Return the union of all ranges in the list RANGES.
Each range is a (start . end) cons.  Note that this alters the
input list by side effect."
  (let* ((urs (sort ranges (lambda (a b) (< (car a) (car b)))))
	 (cur (car urs)) new)
    (dolist (r (cdr urs))
      (setq urs (indent-bars-ts--union cur r))
      (if (= (length urs) 1)
	  (setq cur (car urs))
	(push (car urs) new) ; lower range has no overlap
	(setq urs (cdr urs) cur (car urs))))
    (append urs new)))

(defun indent-bars-ts--intersection (a b)
  "Return the intersection between ranges A and B.
Ranges A and B are (start . end) conses.  Their intersection is a
single range that both cover, or nil if none."
  (indent-bars-ts--order-ranges a b)
  (unless (or (< (cdr a) (car b)) (> (car b) (cdr a)))
    (cons (car b) (min (cdr a) (cdr b)))))

(defun indent-bars-ts--intersect-all (clip ranges)
  "Clip the range CLIP against all RANGES, returning all which are non-nil.
RANGES is a list of (start . end) conses, and CLIP is one such
range to clip against."
  (cl-loop for r in ranges
	   for i = (indent-bars-ts--intersection clip r)
	   if i collect i))

(defun indent-bars-ts--symdiff (a b)
  "Return the symmetric difference between ranges A and B.
Ranges A and B are (start . end) conses.  Their symmetric
difference is a list of ranges, possibly nil, that one (but not
both) of them cover."
  (let ((l ()))
    (indent-bars-ts--order-ranges a b)
    (if (< (cdr a) (car b))
	(push a l)			; no overlap below, add a
      (unless (= (car a) (car b))
	(push (cons (car a) (car b)) l)))
    (if (> (car b) (cdr a))
	(push b l)			; no overlap above, add b
      (unless (= (cdr a) (cdr b))
	(push (if (> (cdr a) (cdr b))
		  (cons (cdr b) (cdr a))
		(cons (cdr a) (cdr b)))
	      l)))
    l))

(defvar-local indent-bars-ts--scope-timer nil)
(defun indent-bars-ts--update-scope1 (buf)
  "Perform the treesitter scope font-lock update in buffer BUF.
If the buffer is modified or the point has moved, re-query the
scope bounds at point.  If the current scope range, clipped to
the window's bounds, falls outside the prior scope (beyond simple
marker movement), refontify the union of all old invalid ranges
and the new window ranges clipped to the window(s) showing BUF.
Note that the updated node range clips to an \"extended window\"
with 50% padding on either side."
  (with-current-buffer buf
    (setq indent-bars-ts--scope-timer nil)
    (let* ((pmn (point-min)) (pmx (point-max))
	   (node (treesit-node-on
		  (max pmn (1- (point))) (point)
		  indent-bars-ts--parser))
	   (scope (and node
		       (indent-bars-ts--node-query
			node (ibts/query ibtcs) nil 'innermost
			indent-bars-treesit-scope-min-lines)))
	   (old (ibts/range ibtcs))	; old node range markers
	   (new (if scope		; no scope = full file
		    (cons (treesit-node-start scope) (treesit-node-end scope))
		  (cons pmn pmx)))
	   (last-clip-win (ibts/clip-win ibtcs)) ; primary clip window
	   (win (cons (window-start) (window-end))))
      (unless (and (= (car new) (car old)) ; if node is unchanged (spans
		   (= (cdr new) (cdr old)) ; the same positions) and the
		   (>= (car win) (car last-clip-win)) ; window inside last clip
		   (<= (cdr win) (cdr last-clip-win))) ; no update needed
	(let* ((marg (/ (- (cdr win) (car win)) 2)) ; a bit of space
	       (wide-clip (cons (max pmn (- (car win) marg))
				(min pmx (+ (cdr win) marg))))
	       (all-clips	    ; for all windows showing this buf
		(indent-bars-ts--union-all
		 (cons wide-clip
		       (mapcar (lambda (w)
				 (cons (window-start w) (window-end w)))
			       (cdr (get-buffer-window-list nil nil t))))))
	       (old-invlds (ibts/invalid-ranges ibtcs))
	       (new-invlds ; clip new node against all showing window ranges
		(indent-bars-ts--intersect-all new all-clips))
	       (all-invlds ; combine old and new ranges + clip to buffer
		(indent-bars-ts--intersect-all ; font-lock invalidates
		 (cons pmn pmx)
		 (indent-bars-ts--union-all (append new-invlds old-invlds)))))
	  (setf (ibts/invalid-ranges ibtcs) new-invlds
		(ibts/start-bars ibtcs)
		(save-excursion
		  (goto-char (car new))
		  (indent-bars--current-indentation-depth)))
	  (set-marker (car old) (car new)) ;updates ibts/range
	  (set-marker (cdr old) (cdr new))
	  (set-marker (car last-clip-win) (car wide-clip))
	  (set-marker (cdr last-clip-win) (cdr wide-clip))
	  (dolist (inv all-invlds) (font-lock-flush (car inv) (cdr inv))))))))

(defun indent-bars-ts--update-scope ()
  "Update treesit scope when possible."
  (unless indent-bars-ts--scope-timer
    (setq indent-bars-ts--scope-timer
	  (run-with-idle-timer indent-bars-treesit-update-delay nil
			       #'indent-bars-ts--update-scope1
			       (current-buffer)))))

;;;; Setup
(defun indent-bars-ts--init-scope (&optional force)
  "Initialize scope style and variables.
If FORCE is non-nil, initialize even if this has already been
performed."
  (unless (or force (get 'indent-bars-ts-setup :init-scope))
    (indent-bars-ts--add-customs)
    (put 'indent-bars-ts-setup :init-scope t))
  (indent-bars--initialize-style
   (setq indent-bars-ts-out-scope-style
	 (indent-bars--new-style "ts"))))

(defun indent-bars-ts--teardown ()
  "Teardown indent-bars-ts."
  (when indent-bars-ts--scope-timer
    (cancel-timer indent-bars-ts--scope-timer)
    (setq indent-bars-ts--scope-timer nil))
  (setq
   indent-bars--display-form nil
   indent-bars--handle-blank-lines-form nil)
  (remove-hook 'post-command-hook #'indent-bars-ts--update-scope t)
  (remove-hook 'indent-bars--teardown-functions 'indent-bars-ts--teardown))

;;;###autoload
(defun indent-bars-ts-setup ()
  "Setup indent-bars for using with treesiter."
  (when-let (((fboundp #'treesit-available-p))
	     ((treesit-available-p))
	     (lang (treesit-language-at (point-min))))
    (setq indent-bars-ts--parser
	  (cl-find lang (treesit-parser-list) :key #'treesit-parser-language))

    ;; Wrap: prevent additional bars inside wrapped entities
    (when-let ((types (alist-get lang indent-bars-treesit-wrap)))
      (setq indent-bars-ts--wrap-query
	    (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx))
	    indent-bars--update-depth-function
	    #'indent-bars-ts--update-indentation-depth))

    ;; Strings (avoid descending deeper inside strings using TS)
    (when (stringp indent-bars-no-descend-string)
      (let ((query `([(,indent-bars-no-descend-string)] @s))
	    (pm (point-min)))
	(setq indent-bars-ts--string-query (treesit-query-compile lang query))
	;; Test it to be sure it works
	(condition-case err
	    (treesit-query-capture indent-bars-ts--parser
				   indent-bars-ts--string-query pm pm t)
	  (treesit-query-error
	   (setq indent-bars-no-descend-string nil)
	   (message "%s. See `indent-bars-no-descend-string'.\n%s"
		    "indent-bars: malformed treesitter string query; disabling"
		    err))
	  (:success (setq indent-bars--update-depth-function
			  #'indent-bars-ts--update-indentation-depth)))))

    ;; Emphasis Scope: use alternate styling outside current scope
    (when-let ((types (alist-get lang indent-bars-treesit-scope)))
      (indent-bars-ts--init-scope)
      (setq ibtcs (ibts/create)
	    indent-bars--display-form '(indent-bars-ts--display)
	    indent-bars--handle-blank-lines-form '(indent-bars-ts--handle-blank-lines))
      (setf (ibts/query ibtcs)
	    (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx)))
      (add-hook 'post-command-hook #'indent-bars-ts--update-scope nil t)
      (add-hook 'indent-bars--teardown-functions 'indent-bars-ts--teardown))))

(provide 'indent-bars-ts)
;;; indent-bars-ts.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ibts/" . "indent-bars-ts-scope-") ("ibtcs" . "indent-bars-ts-current-scope"))
;; End:
