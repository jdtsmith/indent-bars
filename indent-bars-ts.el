;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023  J.D. Smith

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
;; Note the shorthand substitutions for style related prefixes (slot
;; accessors and variable; see file-local-variables at the end):
;; 
;;    ibts/  => indent-bars-ts-scope- (slot accessors)
;;    ibtcs  => indent-bars-ts-current-scope (struct)

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
valid node types for the grammar of the language indicated."
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

(defcustom indent-bars-no-descend-string 'string
  "Configure bar behavior inside treesitter-matched strings.
If non-nil, set to a symbol naming a tree-sitter string node type
into which bars will go no deeper than their starting line.  If
this node type is invalid, a message is printed and the feature
is disabled."
  :local t
  :type '(choice (const :tag "Disable" nil) (symbol :tag "Node Type"))
  :group 'indent-bars-ts)

(defcustom indent-bars-ts-update-delay 0.125
  "Minimum delay time in seconds between treesitter scope updates.
Has effect only if `indent-bars-treesit-scope' is non-nil."
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

(defun indent-bars-ts--node-query (node query &optional start-only spanning)
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
QUERY spans NODE and return it if so.  If no spanning node is
found, nil is returned."
  (when-let ((start (treesit-node-start node))
	     (end (if start-only start (treesit-node-end node)))
	     (nodes (treesit-query-capture indent-bars-ts--parser query
					   start end t)))
    (cond ((eq spanning 'innermost)
	   (cl-loop for n in (nreverse nodes)
		    if (or (eq n node)
			   (indent-bars-ts--node-spans-p n start end))
		    return n))
	  (spanning  ; check first node, if it doesn't span, none will
	   (indent-bars-ts--node-spans-p (car nodes) start end))
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
	    (if (and indent-bars-no-descend-string
		     (indent-bars-ts--node-query
		      node indent-bars-ts--string-query t))
		;; A string: do not descend
		(1+ (indent-bars--depth (indent-bars--indent-at-node node)))
	      ;; Check wrap context
	      (when-let ((ctx (indent-bars-ts--node-query
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
  (start (point-min-marker)) (end (point-min-marker))
  (start-bars 0) (point -1) (tick 0) query)

(defvar-local ibtcs nil  ; N.B. see shorthands at bottom of file
  "The current `indent-bars-ts-scope' struct.")

(defsubst indent-bars-ts--out-of-scope (pos)
  "Return whether POS is outside the current treesitter scope.
If there is no scope defined, every position is considered in
scope."
  (and ibtcs (or (< pos (ibts/start ibtcs)) (> pos (ibts/end ibtcs)))))

(defun indent-bars-ts--display ()
  "Display indentation bars, accounting for current treesitter scope."
  (if (indent-bars-ts--out-of-scope (match-beginning 1))
      (indent-bars--display indent-bars-ts-out-scope-style)
    ;; In scope: switch from out to in-scope style
    (indent-bars--display indent-bars-ts-out-scope-style
			  (ibts/start-bars ibtcs)
			  indent-bars-style)))

(defun indent-bars-ts--handle-blank-lines ()
  "Display bars on blank lines, respecting treesitter scope."
  (let ((beg (match-beginning 0)))
    (unless (indent-bars-ts--ignore-blank beg)
      (if (indent-bars-ts--out-of-scope beg) ;fully out of scope
	  (indent-bars--handle-blank-lines indent-bars-ts-out-scope-style)
	;; Switch from out of scope to in scope after start-bars
	(indent-bars--handle-blank-lines indent-bars-ts-out-scope-style
					 (ibts/start-bars ibtcs)
					 indent-bars-style)))))

(defun indent-bars-ts--union (a b)
    "Return the union between ranges A and B.
Ranges A and B are (start . end) conses.  Their union is a list
of ranges that either cover."
    (if (< (car b) (car a)) (setq b (prog1 a (setq a b))))
    (if (< (cdr a) (car b))
        (list a b) ; no overlap
      (list (cons (car a) (max (cdr a) (cdr b))))))

(defun indent-bars-ts--symdiff (a b)
    "Return the symmetric difference between ranges A and B.
Ranges A and B are (start . end) conses.  Their symmetric
difference is a list of ranges, possibly nil, that one (but not
both) of them cover."
    (let ((l ()))
      (if (< (car b) (car a)) (setq b (prog1 a (setq a b))))
      (if (< (cdr a) (car b))
          (push a l) ; no overlap below, add a
        (unless (= (car a) (car b))
          (push (cons (car a) (car b)) l)))
      (if (> (car b) (cdr a))
          (push b l) ; no overlap above, add b
        (unless (= (cdr a) (cdr b))
          (push (if (> (cdr a) (cdr b))
                    (cons (cdr b) (cdr a))
                  (cons (cdr a) (cdr b)))
           l)))
      l))

(defun indent-bars-ts--update-scope1 ()
  "Perform the treesitter scope update.
If the buffer is modified or the point has moved, re-query the
scope bounds.  If it has changed (beyond normal marker movement),
refontify the symmetric difference between the old and new
ranges (i.e those ranges covered by either old or new, but not
both)."
  (unless (and (= (point) (ibts/point ibtcs))
	       (= (buffer-modified-tick) (ibts/tick ibtcs)))
    (when-let ((node (treesit-node-on
		      (max (point-min) (1- (point))) (point)
		      indent-bars-ts--parser))
	       (scope (indent-bars-ts--node-query
		       node (ibts/query ibtcs) nil 'innermost)))
      (let ((old-start (ibts/start ibtcs))
	    (old-end   (ibts/end ibtcs))
	    (tsc-start (treesit-node-start scope))
	    (tsc-end   (treesit-node-end scope)))
	(unless (and (= tsc-start old-start) (= tsc-end old-end))
	  (setf (ibts/tick ibtcs)  (buffer-modified-tick)
		(ibts/point ibtcs) (point)
		(ibts/start-bars ibtcs)
		(save-excursion
		  (goto-char tsc-start)
		  (forward-line 0)
		  (indent-bars--current-indentation-depth)))
	  (cl-loop for (beg . end) in 	; refontify where needed
		   (indent-bars-ts--union
		    (cons old-start old-end) (cons tsc-start tsc-end))
		   do (font-lock-flush beg end))
	  (set-marker (ibts/start ibtcs) tsc-start)
	  (set-marker (ibts/end ibtcs) tsc-end))))))

(defvar indent-bars-ts--scope-timer nil)
(defun indent-bars-ts--update-scope ()
  "Update treesit scope when possible."
  (if-let ((tmr indent-bars-ts--scope-timer))
      (progn ; reschedule timer
	(timer-set-time
	 tmr (time-add (current-time) indent-bars-ts-update-delay))
	(unless (memq tmr timer-list) (timer-activate tmr)))
    (setq indent-bars-ts--scope-timer
	  (run-with-timer indent-bars-ts-update-delay nil
			  #'indent-bars-ts--update-scope1))))

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
  (setq
   indent-bars--display-form nil
   indent-bars--handle-blank-lines-form nil)
  (remove-hook 'post-command-hook #'indent-bars-ts--update-scope t))

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

    ;; Strings (avoid descending deeper inside strings)
    (when indent-bars-no-descend-string
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
		    "indent-bars: malformed string query; disabling"
		    err)))))

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