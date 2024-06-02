;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024  J.D. Smith

;;; Commentary:

;; tree-sitter support for indent-bars:

;;  - Optionally uses tree-sitter to fine-tune indentation depth
;;    (avoiding "too many bars" in e.g. argument lists)
;;  - Uses treesitter to avoid adding bars to blank lines in strings
;;    or other configurable node types.
;;  - Enable alternate styling for bars inside or outside the
;;    treesitter "scope", where scope is defined as the wrapping
;;    elements configured in `indent-bars-treesitscope`.

;; For Developers:
;;
;; This file interfaces treesitter scope queries with font-lock-based
;; indent bar drawing.  "Scope" is defined as the range of the
;; innermost node covering point that matches the user-configured
;; scope node types for the language of interest.  For any given
;; location of point, there is a scope range (a pair of markers) which
;; helps determine how the bars get displayed.  Short nodes occupying
;; too-few lines are (optionally) not considered.  A nil scope node
;; indicates the entire file is the scope.  Note that even small
;; movements can change the scope and hence bar styling signficantly.

;; The font-lock aspect does not differ much from normal (non-TS) bar
;; drawing, except there is now an alternate set of bar styles (in-
;; vs. out-of-scope), and bars on a single line can be either all one,
;; all the other, or a combination of the two styles.  The forms in
;; the configured font-lock keywords (FACE eval forms) consult the
;; current scope range to determine how to style bars on a line.

;; But since bar fontification now depends not just on the text in the
;; buffer, but on the position of point, this presents a few
;; challenges to maintain efficiency.  The adopted technique is as
;; follows:
;; 
;;  - A post-command hook sets up an idle-time callback (if none
;;    exists).
;;  - In idle time, we check for the innermost "scope" node at point.
;;  - If the scope node boundaries have changed from the last time
;;    they were saved (modulo simple marker movement), we apply a
;;    special property `indent-bars-invalid' to the union of the old
;;    and new scope regions.
;;  - This property is also added to `font-lock-extra-managed-props',
;;    and so font-lock removes these as it adds bars to modified and
;;    otherwise invalidated text.
;;  - In a `window-scroll-functions' function (see
;;    `indent-bars-ts--update-bars-on-scroll') we check the current
;;    window range for visible text marked has having invalid bars:
;;    `indent-bars-invalid' = t.
;;  - We then search for and draws bars within this text "by hand"
;;    (reusing the font-lock keywords and keyword functions).
;;
;; Note that `window-scroll-functions' are called quite late, after
;; font-locking, so very often (during text changes, but also when new
;; `fontified'=nil text comes into view) this function can return
;; quickly without doing anything, as font-lock will have handled
;; things.

;; Note the shorthand substitutions for style related prefixes (slot
;; accessors and variables); see file-local-variables at the end:
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
		 (const :tag "In scope" in-scope)))

(defun indent-bars-ts--add-customs ()
  "Add all the relevant custom variables for the alternate ts style."
  (cl-labels ((ts-cust (var &rest r)
		(eval `(indent-bars--alt-custom
			"ts" ,var ,(format "Tree-sitter (%s)"
					   indent-bars-ts-styling-scope)
			,@r))))
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
If non-nil, indentation bars on text outside (or inside, if
`indent-bars-ts-styling-scope' is changed from its default) of
the innermost matching treesitter scope will use the alternative
style specified in the indent-bars-ts-os-* custom variables,
which mirror and inherit from the normal style variables."
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
  ( invalid-ranges nil :type list
    :documentation "List of current invalid ranges.
One or more (start . end) conses.")
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

(defun indent-bars-ts--display ()
  "Display indentation bars, accounting for current treesitter scope."
  (if (indent-bars-ts--out-of-scope (match-beginning 1))
      (indent-bars--display (match-beginning 1) (match-end 1)
			    (cdr indent-bars-ts-in-out-style))
    ;; In scope: switch from out to in-scope style
    (indent-bars--display (match-beginning 1) (match-end 1)
			  (cdr indent-bars-ts-in-out-style)
			  (ibts/start-bars ibtcs)
			  (car indent-bars-ts-in-out-style))))

(defun indent-bars-ts--handle-blank-lines ()
  "Display bars on blank lines, respecting treesitter scope."
  (let ((beg (match-beginning 0)))
    (unless (indent-bars-ts--ignore-blank beg)
      (if (indent-bars-ts--out-of-scope beg) ;fully out of scope
	  (indent-bars--handle-blank-lines (match-beginning 0) (match-end 0)
					   (cdr indent-bars-ts-in-out-style))
	;; Switch from out of scope to in scope after start-bars
	(indent-bars--handle-blank-lines (match-beginning 0) (match-end 0)
					 (cdr indent-bars-ts-in-out-style)
					 (ibts/start-bars ibtcs)
					 (car indent-bars-ts-in-out-style))))))

(defun indent-bars-ts--draw-all-bars-between (start end)
  "Search for and draw all bars between START and END.
The beginning of line at START is used to locate real and (if
configured) blank-line bars, which are drawn according to the
appropriate style.  This is basically a very tiny, bar-only
version of what `font-lock-fontify-region-keywords' does."
  (save-excursion
    (goto-char start)
    (forward-line 0)
    (setq start (point))
    (while (and (< (point) end)
		(re-search-forward
		 (caar indent-bars--font-lock-keywords) end t))
      (indent-bars-ts--display))
    (when indent-bars-display-on-blank-lines
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward
		   (caar indent-bars--font-lock-blank-line-keywords) end t))
	(indent-bars-ts--handle-blank-lines)))))

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

(defun indent-bars-ts--update-bars-on-scroll (win start)
  "Update bars as needed within the window WIN after START.
To be added to `window-scroll-functions'.  Consults the invalid
ranges of the current scope."
  (let* ((end (window-end win t))
	 (scope (buffer-local-value 'ibtcs (window-buffer win)))
	 (rngs (indent-bars-ts--intersect-all
		(cons start end) (ibts/invalid-ranges scope))))
    ;; (message "WS: %s %d" win start)
    (cl-loop for (beg . end) in rngs do
	     (indent-bars-ts--add-bars-in-range beg end))))

(defvar-local indent-bars-ts--invalid-range-markers nil)
(defun indent-bars-ts--update-invalid-ranges (ranges)
  "Update invalid ranges for the current scope with RANGES.
Also sets the `indent-bars-invalid' property on the indicates
ranges.  Re-uses markers for efficiency."
  (let* ((lm (length indent-bars-ts--invalid-range-markers))
	 (lr (length ranges)))
    (when (> lr lm)
      (dotimes (_ (- lr lm))
	(push (cons (make-marker) (make-marker))
	      indent-bars-ts--invalid-range-markers))
      (setq lm lr))
    (setf (ibts/invalid-ranges ibtcs)
	  (nthcdr (- lm lr) indent-bars-ts--invalid-range-markers))
    (cl-loop for (beg . end) in ranges
	     for (mbeg . mend) in (ibts/invalid-ranges ibtcs) do
	     (put-text-property beg end 'indent-bars-invalid t)
	     (set-marker mbeg beg) (set-marker mend end))))

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
	  (setf (ibts/start-bars ibtcs)
		(save-excursion
		  (goto-char (car new))
		  (indent-bars--current-indentation-depth)))
	  (indent-bars-ts--update-invalid-ranges (indent-bars-ts--union old new))
	  (set-marker (car old) (car new)) ;updates ibts/range
	  (set-marker (cdr old) (cdr new))
	  ;; Arrange to check the current window's bars, just in case
	  ;; font-lock doesn't handle everything itself
	  (run-at-time 0 nil (let ((win (selected-window)))
			       (lambda ()
				 (indent-bars-ts--update-bars-on-scroll
				  win (window-start win))))))))))

(defun indent-bars-ts--update-scope ()
  "Update treesit scope when possible."
  (unless indent-bars-ts--scope-timer
    (setq indent-bars-ts--scope-timer
	  (run-with-idle-timer indent-bars-treesit-update-delay nil
			       #'indent-bars-ts--update-scope1
			       (current-buffer)))))

(defun indent-bars-ts--add-bars-in-range (start end)
  "Add bars if needed between START and END.
Bars are added on all visible ranges of text (considering both
text properties and overlays) with a non-nil
`indent-bars-invalid' property.  START is assumed to be visible.
Based loosely on `jit-lock-function' and `jit-lock-fontify-now'."
  (when-let ((invld-start (text-property-any start end 'indent-bars-invalid t))
	     (invld-rngs
	      (cl-loop for vs = invld-start then
		       (next-single-char-property-change ve 'indent-bars-invalid nil end)
		       for ve = (next-single-char-property-change vs 'indent-bars-invalid nil end)
		       collect (cons vs ve) while (< ve end))))
    (message "abir: found some invalid ranges: %S" invld-rngs)
    (with-silent-modifications
      (save-match-data
	(cl-loop
	 for vs = start then (next-single-char-property-change ve 'invisible nil end)
	 for ve = (next-single-char-property-change vs 'invisible nil end) do
	 (cl-loop for (beg . end) in (indent-bars-ts--intersect-all (cons vs ve) invld-rngs) do
		  (message "Adding invalid bars %d:%d" beg end)
		  (put-text-property beg end 'indent-bars-invalid nil)
		  (indent-bars-ts--draw-all-bars-between beg end))
	 while (< ve end))))))

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

(defun indent-bars-ts--setup (lang)
  "Setup indent-bars treesitter support in this buffer for language LANG."
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

  ;; Emphasis Scope: use alternate styling outside(/inside) current scope
  (when-let ((types (alist-get lang indent-bars-treesit-scope)))
    (indent-bars-ts--init-scope)
    (setq ibtcs (ibts/create))
    (setq-local
     indent-bars--display-form '(indent-bars-ts--display)
     indent-bars--handle-blank-lines-form '(indent-bars-ts--handle-blank-lines))
    (setf (ibts/query ibtcs)
	  (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx)))
    (make-local-variable 'font-lock-extra-managed-props)
    (cl-pushnew 'indent-bars-invalid font-lock-extra-managed-props)
    (add-hook 'post-command-hook #'indent-bars-ts--update-scope nil t)
    (add-hook 'window-scroll-functions #'indent-bars-ts--update-bars-on-scroll nil t)
    (add-hook 'indent-bars--teardown-functions 'indent-bars-ts--teardown)))

(defun indent-bars-ts--teardown ()
  "Teardown indent-bars-ts."
  (when indent-bars-ts--scope-timer
    (cancel-timer indent-bars-ts--scope-timer)
    (setq indent-bars-ts--scope-timer nil))
  (kill-local-variable 'indent-bars--display-form)
  (kill-local-variable 'indent-bars--handle-blank-lines-form)
  (remove-hook 'post-command-hook #'indent-bars-ts--update-scope t)
  (remove-hook 'indent-bars--teardown-functions 'indent-bars-ts--teardown))

;;;###autoload
(define-minor-mode indent-bars-ts-mode
  "Minor mode for indent-bars using treesitter."
  :group 'indent-bars-ts
  (if indent-bars-ts-mode
      (if-let (((fboundp #'treesit-available-p))
	       ((treesit-available-p))
	       (lang (treesit-language-at (point-min))))
	  (indent-bars-ts--setup lang)
	(setq indent-bars-ts-mode nil))
    (indent-bars-ts--teardown)))

(defun indent-bars-ts--custom-update-scope ()
  "Update the TS scope for custom setting."
  (when indent-bars-ts-mode
    (indent-bars-ts--update-scope1 (current-buffer))))
(add-hook 'indent-bars-custom-set #'indent-bars-ts--custom-update-scope)

(provide 'indent-bars-ts)
;;; indent-bars-ts.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ibts/" . "indent-bars-ts-scope-") ("ibtcs" . "indent-bars-ts-current-scope"))
;; End:
