;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023  J.D. Smith

;;; Commentary:

;; tree-sitter support for indent-bars:

;;  - Optionally uses tree-sitter to fine-tune indentation depth
;;    (avoiding "too many bars" in e.g. argument lists)
;;  - Uses treesitter to avoid adding bars to blank lines in strings
;;    or other configurable node types.
;;  - Enable alternate faded styling for bars outside the treesitter
;;    "scope", where scope is defined as the wrapping elements
;;    configured in `indent-bars-ts`.

;; For Developers:
;;
;; Note the shorthand substitutions for style related prefixes (slot
;; accessors and variable; see file-local-variables at the end):
;; 
;;    ibts/  => indent-bars-ts-scope- (slot accessors)
;;    ibtcs  => indent-bars-ts-current-scope (struct)


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
  "Customization group for indent-bars treesitter styling."
  :group 'indent-bars
  :prefix "indent-bars-ts-")

;;;;; Alternate Style Variables
(defun indent-bars-ts--add-customs ()
  "Add all the relevant custom variables for the de-emphasized style."
  (cl-labels ((ts-cust (var &rest r)
		(eval `(indent-bars--alt-custom
			"ts" ,var "Tree-sitter (de-emphasized)" ,@r))))
      (ts-cust 'color '(nil :blend 0.1) 'add-inherit)
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

(defcustom indent-bars-treesit-emphasis-scope nil
  "An alist of language and treesitter node types to emphasize.
If non-nil, indentation bars on text outside of the innermost
matching treesitter scope will be de-emphasized using the
alternative style specified in the indent-bars-ts-* custom
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
Has effect only if `indent-bars-treesit-emphasis-scope' is
non-nil."
  :type 'float
  :group 'indent-bars-ts)

;;;; Node Utilities
(defvar-local indent-bars-ts--parser nil)
(defvar-local indent-bars-ts--wrap-query nil)
(defvar-local indent-bars-ts--scope-query nil)
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
query searches for matching nodes which overlap with the start of
the node at point.  Otherwise nodes which intersect anywhere with
NODE will be returned.

If SPANNING is non-nil, return a single spanning node which fully
spans the full start..end range of NODE (or just the start ,if
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
topmost matching parent node's initial line's indentation depth.
If `indent-bars-no-descend-string' is non-nil, looks for
enclosing string and marks indent depth no deeper than one more
than the starting line's depth.  This reduces depth inside
strings, and for wrap contexts."
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

;;;; Ignoring Some Blank Lines
(defun indent-bars-ts--ignore-blank (beg)
  "See if blank lines at BEG should be ignored using tree-sitter.
Blank lines to ignore are those within nodes of the types
mentioned in `indent-bars-treesit-ignore-blank-lines-types'."
  (and indent-bars-ts--parser
       indent-bars-treesit-ignore-blank-lines-types
       (when-let ((n (treesit-node-on beg beg)))
	 (seq-contains-p indent-bars-treesit-ignore-blank-lines-types
			 (treesit-node-type n)))))

(defun indent-bars-ts--handle-blank-lines ()
  "Wrapper for `indent-bars--handle-blank-lines' that works with treesitter.

If `indent-bars-treesit-ignore-blank-lines-types' is configured,
ignore blank lines whose starting positions are directly spanned
by nodes of those types (e.g. module)."
  (unless (indent-bars-ts--ignore-blank (match-beginning 0))
    (indent-bars--handle-blank-lines)))

;;;###autoload
(defun indent-bars-ts-setup ()
  "Setup indent-bars for using with treesiter."
  (when-let (((fboundp #'treesit-available-p))
	     ((treesit-available-p))
	     (lang (treesit-language-at (point-min)))
	     (types (alist-get lang indent-bars-treesit-wrap)))
    (setq indent-bars-ts--parser
	  (cl-find lang (treesit-parser-list) :key #'treesit-parser-language)
	  indent-bars-ts--query
	  (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx)))
    (when indent-bars-no-descend-string
      (let ((query `([(,indent-bars-no-descend-string)] @s))
	    (pm (point-min)))
	(setq indent-bars-ts--string-query (treesit-query-compile lang query))
	;; Test it to be sure
	(condition-case err
	    (treesit-query-capture indent-bars-ts--parser
				   indent-bars-ts--string-query pm pm t)
	  (treesit-query-error
	   (setq indent-bars-no-descend-string nil)
	   (message "indent-bars: malformed string query; disabling.  See indent-bars-no-descend-string.\n%s" err)))))
    (setq indent-bars--update-depth-function
	  #'indent-bars-ts--update-indentation-depth)
    ))
