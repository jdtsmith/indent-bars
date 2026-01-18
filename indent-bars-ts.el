;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;;; Commentary:

;; tree-sitter support for indent-bars:

;;  - Optionally uses tree-sitter to fine-tune indentation depth
;;    (avoiding "too many bars" in e.g. argument lists)
;;  - Uses treesitter to avoid adding bars to blank lines in strings
;;    or other configurable node types.
;;  - Enable alternate styling for bars inside or outside the
;;    treesitter "scope" at point, where scope is defined as the
;;    innermost wrapping node containing point mentioned in
;;    `indent-bars-treesit-scope` for the current buffer's language.

;;;; For Developers:
;;
;; This file interfaces treesitter scope queries with font-lock-based
;; indent bar drawing.  "Scope" is defined as the range of the
;; innermost node containing point that matches the user-configured
;; scope node types for the language of interest.  For any given
;; location of point, there is a scope range (a pair of markers) which
;; helps determine how the bars get displayed.  Short nodes occupying
;; too-few lines are (optionally) not considered for scope.  A nil
;; scope node indicates the entire file is the scope.  Note that even
;; small movements can change the scope and hence the bar styling.

;; The styling aspect does not differ much from normal (non-TS) bar
;; drawing, except there is now an alternate set of bar styles (in-
;; vs. out-of-scope), and bars on a single line can be either all one,
;; all the other, or a combination of the two styles.  The forms in
;; the configured font-lock keywords (FACE eval forms) consult the
;; current scope range to determine how to style bars on a line.

;; Since bar fontification now depends not just on the text in the
;; buffer, but on the position of point, this presents a few
;; challenges to maintain efficiency.  The adopted technique is as
;; follows:
;; 
;;  - A post-command hook sets up an idle-time callback (if none
;;    exists).
;;  - In idle time, we query treesitter for the innermost "scope" node
;;    at point, based on user config for the buffer's language
;;    (`indent-bars-treesit-scope').
;;  - If the scope boundaries have changed from the last time they
;;    were saved (modulo simple marker movement), we invalidate the
;;    fontification over the union of the old and new scope regions.
;;  - jit-lock is modified to apply a special
;;    `indent-bars-font-lock-pending' property to modified text.  The
;;    same happens for contextually-refontified text and explicit
;;    calls to `font-lock-flush'.
;;  - font-lock and jit-lock are configured to skip the core font-lock
;;    region fontification function when font-lock itself is not
;;    pending in the region.  See `indent-bars--fontify'.
;;

;;;;; Shorthands
;; Note the shorthand substitutions for style related prefixes
;; (slot accessors and variables); see file-local-variables at the
;; end:
;; 
;;    ibts/  => indent-bars-ts-scope- (slot accessors)
;;    ibtcs  => indent-bars-ts-current-scope (scope struct)

;;; Code:

;;;; Requires
(require 'cl-lib)
(require 'seq)
(require 'jit-lock)
(require 'indent-bars)
(require 'treesit nil t)

;;;; Customization
(defgroup indent-bars-ts nil
  "Customization group for indent-bars treesitter options."
  :group 'indent-bars
  :prefix "indent-bars-ts-")

(defgroup indent-bars-ts-style nil
  "Customization group for indent-bars treesitter alternate styling."
  :group 'indent-bars
  :prefix "indent-bars-ts-")

;;;;; Alternate Style Variables
(defcustom indent-bars-ts-styling-scope 'out-of-scope
  "Which scope the *-ts-* style variables apply to: in or out.
By default, the *-ts-* custom variables apply to the out-of-scope
style, and in-scope bars make use of the default (non-ts)
styling.  If instead this is set to `in-scope', the out-of-scope
bars share the default style, and in-scope bars are configured
with alternate styling using the *-ts-* variables."
  :type '(choice (const :tag "Out of scope" out-of-scope)
		 (const :tag "In scope" in-scope))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defun indent-bars-ts--add-customs ()
  "Add all the relevant custom variables for the alternate ts style."
  (cl-labels ((ts-cust (var &rest r)
		(eval `(indent-bars--alt-custom
			"ts" ,var ,(format "Tree-sitter (%s)"
					   indent-bars-ts-styling-scope)
			,@r))))
    (ts-cust 'color '(unspecified :blend 0.08) 'add-inherit)
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
  :group 'indent-bars-ts
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-treesit-scope nil
  "An alist of language and treesitter node types to emphasize.
If non-nil, indentation bars on text outside (or inside, if
`indent-bars-ts-styling-scope' is changed from its default) of
the innermost matching treesitter scope will use the alternative
style specified in the indent-bars-ts-os-* custom variables,
which mirror and inherit from the normal style variables."
  :type '(choice (const :tag "No scope types" nil)
		 (alist :tag "Alist of node types"
			:key-type (symbol :tag "Language")
			:value-type (repeat :tag "Types" (symbol :tag "Type"))))
  :group 'indent-bars-ts
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-treesit-scope-min-lines 3
  "Minimum number of lines a node must span to be counted as a scope."
  :type 'integer
  :group 'indent-bars-ts
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

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
  :group 'indent-bars-ts
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

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
  (when-let* ((start (treesit-node-start node))
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
	   (when-let* ((n (indent-bars-ts--node-spans-p (car nodes) start end))
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
  (if-let* (((not (bobp)))
	    (node (treesit-node-on (1- (point)) (point) indent-bars-ts--parser))
	    (dnew
	     (if (and indent-bars-ts--string-query
		      (indent-bars-ts--node-query
		       node indent-bars-ts--string-query t))
		 ;; A string: do not descend
		 (1+ (indent-bars--depth (indent-bars--indent-at-node node)))
	       ;; Check wrap context
	       (when-let* ((indent-bars-ts--wrap-query)
			   (ctx (indent-bars-ts--node-query
				 node indent-bars-ts--wrap-query nil t)))
		 (1+ (indent-bars--depth
		      (indent-bars--indent-at-node ctx)))))))
      (if dnew (min dnew d) d)
    d))

;;;; Ignoring Certain Blank Lines
;; see also ts--handle-blank-lines, below
(defun indent-bars-ts--ignore-blank (beg)
  "See if blank lines at BEG should be ignored using tree-sitter.
Blank lines to ignore are those within nodes of the types
mentioned in `indent-bars-treesit-ignore-blank-lines-types'."
  (and indent-bars-ts--parser
       indent-bars-treesit-ignore-blank-lines-types
       (when-let* ((n (treesit-node-on beg beg)))
	 (seq-contains-p indent-bars-treesit-ignore-blank-lines-types
			 (treesit-node-type n)))))

;;;; Scope Highlighting
(defvar indent-bars-ts-alt-style nil)
(defvar indent-bars-ts-in-out-style nil
  "A cons of (IN-STYLE . OUT-OF-STYLE).")

(cl-declaim (optimize (safety 0))) ; no need for type check
(cl-defstruct
    (indent-bars-ts-scope
     (:copier nil)
     (:conc-name ibts/)
     (:constructor ibts/create))
  "A structure for tracking treesitter-based scope information."
  ( range (cons (point-min-marker) (point-min-marker)) :type cons
    :documentation "The current scope node's range.")
  ( start-bars 0 :type integer
    :documentation "The number of bars shown at start of current scope.")
  ( query nil :type ts-query
    :documentation "The treesitter scope query object."))

(defvar-local ibtcs nil  ; N.B. see shorthands at bottom of file
  "The current `indent-bars-ts-scope' struct.")

(defvar-local indent-bars-ts--scope-timer nil)
(defun indent-bars-ts--out-of-scope (pos)
  "Return t if POS is outside the current treesitter scope.
If there is no scope defined, every position is considered in
scope.  When the timer is running, only consider the most
recently clipped node ranges in scope."
  (when ibtcs
    (or (< pos (car (ibts/range ibtcs)))
	(> pos (cdr (ibts/range ibtcs))))))

(defun indent-bars-ts--display (beg end)
  "Display indentation bars from BEG to END, respecting treesitter scope."
  (if (indent-bars-ts--out-of-scope beg)
      (indent-bars--display beg end
			    (cdr indent-bars-ts-in-out-style))
    ;; In scope: switch from out to in-scope style
    (indent-bars--display beg end
			  (cdr indent-bars-ts-in-out-style)
			  (ibts/start-bars ibtcs)
			  (car indent-bars-ts-in-out-style))))

(defun indent-bars-ts--display-blank-lines (beg end)
  "Display bars on blank lines between BEG and END, respecting treesitter scope."
  (unless (indent-bars-ts--ignore-blank beg)
    (if (indent-bars-ts--out-of-scope beg) ;fully out of scope
	(indent-bars--display-blank-lines beg end
					  (cdr indent-bars-ts-in-out-style))
      ;; Switch from out of scope to in scope after start-bars
      (indent-bars--display-blank-lines beg end
					(cdr indent-bars-ts-in-out-style)
					(ibts/start-bars ibtcs)
					(car indent-bars-ts-in-out-style)))))

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

(defun indent-bars-ts--update-scope1 (buf)
  "Perform the treesitter scope font-lock update in buffer BUF.
Re-query the scope node at point, and if it has moved (beyond
simple marker movement), refontify the union of the old and new
scope range, and mark with the `indent-bars-invalid' property.
Finally, check and possibly update the bars in the current
window."
  (with-current-buffer buf
    (with-silent-modifications
      (setq indent-bars-ts--scope-timer nil)
      (let* ((pmn (point-min)) (pmx (point-max))
	     (old (ibts/range ibtcs))
	     (node (treesit-node-on
		    (max pmn (1- (point))) (point)
		    indent-bars-ts--parser))
	     (scope (and node
			 (indent-bars-ts--node-query
			  node (ibts/query ibtcs) nil 'innermost
			  indent-bars-treesit-scope-min-lines)))
	     (new (if scope		; no scope = full file
		      (cons (treesit-node-start scope) (treesit-node-end scope))
		    (cons pmn pmx))))
	(unless (and (= (car new) (car old)) ; if node is unchanged (spans
		     (= (cdr new) (cdr old))) ; same range) no update needed
	  (cl-loop for (beg . end) in (indent-bars-ts--union old new) do
		   (jit-lock-refontify beg end)) ; sets fontified=nil
	  (setf (ibts/start-bars ibtcs)
		(save-excursion
		  (goto-char (car new))
		  (indent-bars--current-indentation-depth)))
	  (set-marker (car old) (car new)) ;updates ibts/range
	  (set-marker (cdr old) (cdr new)))))))

(defun indent-bars-ts--update-scope ()
  "Update treesit scope when possible."
  (unless indent-bars-ts--scope-timer
    (setq indent-bars-ts--scope-timer
	  (run-with-idle-timer indent-bars-treesit-update-delay nil
			       #'indent-bars-ts--update-scope1
			       (current-buffer)))))

;;;; Jit-lock support
;; Dynamic jit-lock variables
(defvar jit-lock-start) (defvar jit-lock-end)
(defun indent-bars-ts--mark-change (&rest _r)
  "Mark changed regions with a special property.
Applies the `indent-bars-font-lock-pending' property to the
affected text (which font-lock removes).  This allows us to keep
separate track of regions where bars are pending, and where
font-lock is pending."
  (put-text-property jit-lock-start jit-lock-end 'indent-bars-font-lock-pending t))

(defvar indent-bars--ts-mode)
(defun indent-bars-ts--context-fontify (fun)
  "Wrap FUN to keep track of context fontification.
Added as `:around' advice to `jit-lock-context-fontify'.
Applies `indent-bars-font-lock-pending' property to the newly
invalidated text."
   (let (orig)
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
	 (when (and indent-bars--ts-mode jit-lock-context-unfontify-pos)
	   (setf (alist-get buffer orig) jit-lock-context-unfontify-pos))))
     (funcall fun)
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
	 (when (and indent-bars--ts-mode jit-lock-context-unfontify-pos
		    (assq buffer orig)
		    (> jit-lock-context-unfontify-pos (alist-get buffer orig)))
	   (with-silent-modifications
	     (without-restriction
	       (put-text-property (alist-get buffer orig)
				  jit-lock-context-unfontify-pos
				  'indent-bars-font-lock-pending t))))))))

(defun indent-bars-ts--font-lock-inhibit (beg end)
  "Check if font-lock is needed on the region between BEG and END.
Checks for the property `indent-bars-font-lock-pending',
inhibiting font-lock if it is not pending in the region.  The
property is removed if found."
  (let (pending)
    (when (setq pending (text-property-any beg end 'indent-bars-font-lock-pending t))
      (with-silent-modifications
	(put-text-property pending end 'indent-bars-font-lock-pending nil)))
    (not pending)))

(defvar-local indent-bars-ts--orig-fontify-buffer nil)
(defvar-local indent-bars-ts--orig-font-lock-flush nil)
(defun indent-bars-ts--fontify-buffer (&rest r)
  "Fontify the buffer after setting the pending property.
`indent-bars-ts--orig-fontify-buffer' is called with arguments R."
  (save-restriction
    (widen)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'indent-bars-font-lock-pending t))
    (apply indent-bars-ts--orig-fontify-buffer r)))

(defun indent-bars-ts--flush (beg end &rest r)
  "Flush font lock and set pending property between BEG and END.
To be used as a `font-lock-flush-function'.  R are any extra
arguments."
  (with-silent-modifications
    (put-text-property beg end 'indent-bars-font-lock-pending t))
  (apply indent-bars-ts--orig-font-lock-flush beg end r))

;;;; Setup
(defun indent-bars-ts--init-scope (&optional force)
  "Initialize scope style and variables.
If FORCE is non-nil, initialize even if this has already been
performed."
  (unless (or force (get 'indent-bars-ts-setup :init-scope))
    (indent-bars-ts--add-customs)
    (put 'indent-bars-ts-setup :init-scope t))
  (indent-bars--initialize-style
   (setq indent-bars-ts-alt-style
	 (indent-bars--new-style "ts")))
  (setq indent-bars-ts-in-out-style
	(if (eq indent-bars-ts-styling-scope 'out-of-scope)
	    (cons indent-bars-style indent-bars-ts-alt-style)
	  (cons indent-bars-ts-alt-style indent-bars-style))))

(defun indent-bars-ts--finalize-jit-lock ()
  "Finalize jit-lock for indent-bars display with treesitter support.
This sets up jit-lock and font-lock to record our special
`indent-bars-font-lock-pending' property on text it is updating
due to edits or contextual fontification."
  (unless (eq font-lock-fontify-buffer-function 'indent-bars-ts--fontify-buffer)
    (setq-local indent-bars-ts--orig-fontify-buffer font-lock-fontify-buffer-function
		indent-bars-ts--orig-font-lock-flush font-lock-flush-function))
  (setq-local indent-bars--font-lock-inhibit #'indent-bars-ts--font-lock-inhibit
	      font-lock-fontify-buffer-function #'indent-bars-ts--fontify-buffer
	      font-lock-flush-function #'indent-bars-ts--flush)
  ;; We must mark the fontified=nil regions of jit-lock, both after-change and contextual
  (add-hook 'jit-lock-after-change-extend-region-functions
	    #'indent-bars-ts--mark-change 96 t)
  (when (eq jit-lock-contextually t)
    (advice-add #'jit-lock-context-fontify :around #'indent-bars-ts--context-fontify))
  (indent-bars-ts--fontify-buffer))

(defun indent-bars-ts--disable ()
  "Disable `indent-bars--ts-mode'."
  (indent-bars--ts-mode -1))

(defun indent-bars-ts--setup (lang)
  "Setup indent-bars treesitter support in this buffer for language LANG."
  (setq indent-bars-ts--parser
	(cl-find lang (treesit-parser-list) :key #'treesit-parser-language))

  ;; Wrap: prevent additional bars inside wrapped entities
  (when-let* ((types (alist-get lang indent-bars-treesit-wrap)))
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

  ;; Emphasis Scope: use alternate styling outside(/inside) current scope
  (when-let* ((types (alist-get lang indent-bars-treesit-scope)))
    (indent-bars-ts--init-scope)
    (setq ibtcs (ibts/create))
    (setq-local
     indent-bars--display-function #'indent-bars-ts--display
     indent-bars--display-blank-lines-function #'indent-bars-ts--display-blank-lines)
    (setf (ibts/query ibtcs)
	  (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx)))
    (add-hook 'post-command-hook #'indent-bars-ts--update-scope nil t))
  (if indent-bars-ts--orig-fontify-buffer ; setting up again
      (indent-bars-ts--finalize-jit-lock)
    (add-hook 'font-lock-mode-hook #'indent-bars-ts--finalize-jit-lock nil t)))

(defun indent-bars-ts--teardown ()
  "Teardown indent-bars-ts in the buffer.
To be set in `indent-bars--teardown-functions'."
  (when indent-bars-ts--scope-timer
    (cancel-timer indent-bars-ts--scope-timer)
    (setq indent-bars-ts--scope-timer nil))
  (when (and indent-bars-ts--orig-fontify-buffer
	     (not (eq indent-bars-ts--orig-fontify-buffer
		      #'indent-bars-ts--fontify-buffer)))
    (setq-local font-lock-fontify-buffer-function indent-bars-ts--orig-fontify-buffer
		font-lock-flush-function indent-bars-ts--orig-font-lock-flush))
  (setq-local indent-bars--font-lock-inhibit nil)
  (kill-local-variable 'indent-bars--display-function)
  (kill-local-variable 'indent-bars--display-blank-lines-function)
  (remove-hook 'post-command-hook #'indent-bars-ts--update-scope t)
  (remove-hook 'indent-bars--teardown-functions 'indent-bars-ts--disable t)
  (remove-hook 'jit-lock-after-change-extend-region-functions
	       #'indent-bars-ts--mark-change t))

;;;###autoload
(define-minor-mode indent-bars--ts-mode
  "Minor mode for indent-bars using treesitter."
  :group 'indent-bars-ts
  (cond
   (indent-bars--ts-mode
    (add-hook 'indent-bars--teardown-functions 'indent-bars-ts--disable nil t)
    (if-let* (((fboundp #'treesit-available-p))
	      ((treesit-available-p))
	      (lang (treesit-language-at (point-min))))
	(indent-bars-ts--setup lang)
      (setq indent-bars--ts-mode nil)))
   (t (indent-bars-ts--teardown))))

(defun indent-bars-ts--custom-update-scope ()
  "Update the TS scope for custom setting."
  (when (and indent-bars--ts-mode ibtcs)
    (indent-bars-ts--update-scope1 (current-buffer))))
(add-hook 'indent-bars-custom-set #'indent-bars-ts--custom-update-scope)

(provide 'indent-bars-ts)
;;; indent-bars-ts.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ibts/" . "indent-bars-ts-scope-") ("ibtcs" . "indent-bars-ts-current-scope"))
;; End:
