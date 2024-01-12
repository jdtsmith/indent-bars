;;; indent-bars-ts.el --- treesitter support for indent-bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023  J.D. Smith

;;; Commentary:

;; tree-sitter support for indent-bars:

;;  - Optionally uses tree-sitter to fine-tune indentation depth
;;    (avoiding "too many bars" in e.g. argument lists)
;;  - Use treesitter to avoid adding bars to blank lines in strings or
;;    other types.
;;  - Enable alternate styling for bars inside and outside the
;;    treesitter "scope", where scope is defined as the wrapping
;;    elements configured in `indent-bars-ts`.

(require 'seq)
(require 'treesit nil t) ; treesit is optional
(require 'indent-bars)

;;;; Customization



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
    :group 'indent-bars)

(defcustom indent-bars-treesit-shadow-scope nil
  "An alist of language and treesitter node types to emphasize.
Bars on text ouside of the matching treesitter nodes will be
de-emphasized with alternative \"shadow\" style."
  :type '(choice (const :tag "Scope types" nil)
		 (alist :tag "Alist of node types"
			:key-type (symbol :tag "Language")
			:value-type (repeat :tag "Types" (symbol :tag "Type"))))
  :group 'indent-bars)

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
  :group 'indent-bars)

(defcustom indent-bars-no-descend-string 'string
  "Configure bar behavior inside treesitter-matched strings.
If non-nil, set to a symbol naming a tree-sitter string node type
into which bars will go no deeper than their starting line.  If
this node type is invalid, a message is printed and the feature
is disabled."
  :local t
  :type '(choice (const :tag "Disable" nil) (symbol :tag "Node Type"))
  :group 'indent-bars)

;;;; Tree-sitter
(defvar-local indent-bars-ts--parser nil)
(defvar-local indent-bars-ts--query nil)
(defvar-local indent-bars-ts--string-query nil)

(defun indent-bars-ts--node-query (node query &optional start-only first-spanning)
  "Capture node(s) spanning NODE matching QUERY.
QUERY is a compiled treesit query.  If START-ONLY is non-nil, the
query searches for matching nodes spanning the start of the node
at point, otherwise nodes which intersect anywhere with the node
will be returned.  If FIRST-SPANNING is non-nil, return the first
matching node, but only if it fully spans the full start..end
range of NODE, otherwise return nil."
  (let* ((start (treesit-node-start node))
	 (end (if start-only start (treesit-node-end node)))
	 (nodes (treesit-query-capture indent-bars-ts--parser query
				       start end t)))
    (when (and first-spanning nodes)
      (let ((n (car nodes)))
	(setq nodes
	      (and (<= (treesit-node-start n) start)
		   (>= (treesit-node-end n) end)
		   n))))
    nodes))

(defsubst indent-bars--indent-at-node (node)
  "Return the current indentation at the start of TS node NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (current-indentation)))

(defun indent-bars-ts--update-indentation-depth (d)
  "Update depth D using tree-sitter-region."
  (if-let (((not (bobp)))
	   (node (treesit-node-on (1- (point)) (point) indent-bars-ts--parser))
	   (dnew
	    (if (and indent-bars-no-descend-string
		     (indent-bars-ts--node-query
		      node indent-bars-ts--string-query t))
		;; A string: do not descend
		(1+ (indent-bars--depth (indent-bars--indent-at-node node)))
	      ;; Check context
	      (when-let ((ctx (indent-bars-ts--node-query
			       node indent-bars-ts--query nil t)))
		(1+ (indent-bars--depth
		     (indent-bars--indent-at-node ctx)))))))
      (min dnew d)
    d))


(defun indent-bars--ignore-blank (beg)
  "See if blank lines at BEG should be ignored using tree-sitter.
Blank lines to ignore are those with types in
`indent-bars-treesit-ignore-blank-lines-types'."
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
  (unless (indent-bars--ignore-blank (match-beginning 0))
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
