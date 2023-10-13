;;; indent-bars.el --- highlight indentation with bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/indent-bars
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Version: 0.2.1
;; Keywords: convenience
;; Prefix: indent-bars
;; Separator: -

;; indent-bars is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; indent-bars is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; indent-bars highlights indentation with configurable font-lock
;; based vertical bars, using stipples.  The color and appearance
;; (weight, pattern, position within the character, zigzag, etc.) are
;; all configurable.  Includes the option for depth-varying colors and
;; highlighting the indentation level of the current line.  Bars span
;; blank lines, by default.  Optionally uses tree-sitter to fine-tune
;; indentation depth.  indent-bars works in any mode using fixed tab
;; or space-based indentation.  In the terminal (or on request) it
;; uses vertical bar characters instead of stipple patterns.


;;; Code:
;;;; Requires
(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'outline)
(require 'font-lock)
(require 'compat)
(require 'treesit nil t) ; treesit is optional

;;;; Customization
(defgroup indent-bars nil
  "Highlight indentation bars."
  :group 'basic-faces
  :prefix "indent-bars-")

;;;;; Bar Shape
(defcustom indent-bars-width-frac 0.4
  "The width of the indent bar as a fraction of the character width."
  :type '(float :tag "Width Fraction"
		:match (lambda (_ val) (and val (<= val 1) (>= val 0)))
		:type-error "Fraction must be between 0 and 1")
  :group 'indent-bars)

(defcustom indent-bars-pad-frac 0.1
  "The offset of the bar from the left edge of the character.
A float, the fraction of the character width."
  :type '(float :tag "Offset Fraction"
	  :match (lambda (_ val) (and val (<= val 1) (>= val 0)))
	  :type-error "Fraction must be between 0 and 1")
  :group 'indent-bars)

(defcustom indent-bars-pattern " .   .  "
  "A pattern specifying the vertical structure of indent bars.
Space signifies blank regions, and any other character signifies
filled regions.  The pattern length is scaled to match the
character height.  Example: \". . \" would specify alternating
filled and blank regions each approximately one-quarter of the
character height.  Note that the non-blank characters need not be
the same (e.g., see `indent-bars-zigzag')."
  :type '(string :tag "Fill Pattern")
  :group 'indent-bars)

(defcustom indent-bars-zigzag nil
  "The zigzag to apply to the bar pattern.
If non-nil, an alternating zigzag offset will be applied to
consecutive groups of identical non-space characters in
`indent-bars-pattern'.  Starting from the top of the pattern,
positive values will zigzag (right, left, right, ..) and negative
values (left, right, left, ...).

Example:

  pattern: \" .**.\"
  width:   0.5
  pad:     0.25
  zigzag: -0.25

would produce a zigzag pattern which differs from the normal
bar pattern as follows:

    |    |            |    |
    | .. | =========> |..  |
    | .. |            |  ..|
    | .. | apply zig- |  ..|
    | .. | zag -0.25  |..  |

Note that the pattern will be truncated at both left and right
boundaries, so (although this is not required) achieving an equal
zigzag left and right requires leaving sufficient padding on each
side of the bar; see `indent-bars-pad-frac' and
`indent-bars-width-frac'."
  :type '(choice
	  (const :tag "No Zigzag" :value nil)
	  (float :value 0.1 :tag "Zigzag Fraction"
		 :match (lambda (_ val) (and val (<= val 1) (>= val -1)))
		 :type-error "Fraction must be between -1 and 1"))
  :group 'indent-bars)

;;;;; Bar Colors
(defcustom indent-bars-color
  '(highlight :face-bg t :blend 0.4)
  "The main indent bar color.
The format is a list of 1 required element, followed by an
optional plist (keyword/value pairs):

  (main_color [:face-bg :blend])

where:

  MAIN_COLOR: Specifies the main indentation bar
    color (required).  It is either a face name symbol, from
    which the foreground color will be used as the primary bar
    color, or an explicit color (a string).

  FACE-BG: A boolean controlling interpretation of the
    MAIN_COLOR face (if configured).  If non-nil, the background
    color of the face will be used as the main bar color instead
    of its foreground.

  BLEND: an optional blend factor, a float between 0 and 1.  If
    non-nil, the main bar color will be computed as a blend
    between MAIN_COLOR and the frame background color,
    notionally:

      BLEND * MAIN_COLOR + (1 - BLEND) * frame-background

    If BLEND is nil or unspecified, no blending is done, and
    MAIN_COLOR is used as-is."
  :type
  '(list (choice :tag "Main Bar Color"
		 color
		 (face :tag "from Face"))
	 (plist :tag "Options"
		:inline t
		:options
		((:face-bg (boolean
			    :tag "Use Face's Background Color"
			    :value t))
		 (:blend (float
			  :tag "Blend Factor"
			  :value 0.5
			  :match (lambda (_ val) (and val (<= val 1) (>= val 0)))
			  :type-error "Factor must be between 0 and 1")))))
  :group 'indent-bars)

(defcustom indent-bars-color-by-depth
  '(:regexp "outline-\\([0-9]+\\)" :blend 1)
  "Configuration for depth-varying indentation bar coloring.
If non-nil, depth-based coloring is performed.  This should be a
plist with keys:

    (:regexp :face-bg :palette :blend)

with:

  REGEXP: A regular expression string used to match against all
    face names.  For the matching faces, the first match group in
    the regex (if any) will be interpreted as a number, and used
    to sort the resulting list of faces.  The foreground color of
    each matching face will then constitute the depth color
    palette (see also PALETTE, which this option overrides).

  FACE-BG: A boolean.  If non-nil, use the background color
    from the faces matching REGEXP for the palette instead of
    their foreground colors.

  PALETTE: An explicit cyclical palette of colors/faces for
    depth-varying bar colors.  Note that REGEXP takes precedence
    over this setting.  The format is a list of faces (symbols)
    or colors (strings) to be used as a color cycle for coloring
    indentations at increasing levels.  Each face can optionally
    be specified as a cons cell (face . \\='bg) to specify using
    that face's background color instead of its foreground.

      (face_or_color | (face . \\='bg) ...)

    While this list can contain a single element, it makes little
    sense to do so.  The depth palette will be used cyclically,
    i.e. when a bar's indentation depth exceeds the length of the
    palette, colors will be obtained by wrapping around to the
    beginning of the list.

  BLEND: a blend factor (0..1) which controls how palette colors
    are blended with the main color, prior to blending with the
    frame background color (see `indent-bars-color' for
    information on how blend factors are specified).  A nil value
    causes the palette colors to be used as-is.  A unity value
    causes the palette color to be blended directly with the
    background using any blend factor from `indent-bars-color'.

Note that, for this setting to have any effect, one of REGEXP or
PALETTE is required (the former overriding the latter).  If both
are omitted or nil, all bars will have the same color, based on
MAIN_COLOR (aside possibly from the bar at the current
indentation level, if configured; see
`indent-bars-highlight-current-depth')."
  :type '(choice :tag "Depth Palette"
		 (const :tag "No Depth-Coloring" nil)
		 (plist :tag "Depth-Coloring"
			:options
			((:regexp (regexp :tag "Face regexp"))
			 (:face-bg
			  (boolean
			   :value t
			   :tag "Use Matching Face's Background Colors"))
			 (:palette
			  (repeat :tag "Explicit Color/Face List"
				  (choice (color :tag "Color")
					  (face :tag "Foreground from Face")
					  (cons :tag "Background from Face"
						:format "Background from %v"
						face
						(const :format "\n" :value bg)))))
			 (:blend
			  (float :tag "Blend Fraction into Main Color"
				 :value 0.5
				 :match (lambda (_ val)
					  (and val (<= val 1) (>= val 0)))
				 :type-error
				 "Factor must be between 0 and 1")))))
  :group 'indent-bars)

(defcustom indent-bars-highlight-current-depth
  '(:pattern ".")			; solid bar, no color change
  "Current indentation depth bar highlight configuration.
Use this to configure optional highlighting of the bar at the
current line's indentation depth level.

Format:

    nil | (:color :face :face-bg :background :blend :palette
           :width :pad :pattern :zigzag)

If nil, no highlighting will be applied to bars at the current
depth of the line at point.  Otherwise, a plist describes what
highlighting to apply, which can include changes to color and/or
bar pattern.  At least one of :blend, :color, :palette, :face,
:width, :pad, :pattern, or :zigzag must be set and non-nil for
this setting to take effect.

By default, the highlight color will be the same as the
underlying color.  With PALETTE, COLOR or FACE set, all bars at
the current depth will be highlighted in the appropriate color,
either from an explicit PALETTE list (see
`indent-bars-color-by-depth'), a COLOR, or, if FACE is set,
FACE's foreground or background color (the latter if FACE-BG is
non-nil).  If PALETTE is provided, it overrides any other
foreground color setting.  If BACKGROUND is set to a color, this
will be used for the background color of the current
bar (i.e. not the stipple color).

If BLEND is provided, it is a blend fraction between 0 and 1 for
blending the specified highlight color with the
existing (depth-based or main) bar color; see `indent-bars-color'
for its meaning.  BLEND=1 indicates using the full, unblended
highlight color (and is the same as omitting BLEND).

As a special case, if BLEND is provided, but neither COLOR nor
FACE is, this indicates using a (presumably distinct) blend
factor between the usual color for that bar and the frame
background for the current depth highlight.  The original colors
are specified in `indent-bars-color-by-depth' or
`indent-bars-color'.  In this manner the current-depth highlight
can be made a more (or less) prominent version of the default
coloring.

If any of WIDTH, PAD, PATTERN, or ZIGZAG are set, the bar pattern
at the current level will be altered as well.  Note that
`indent-bars-width-frac', `indent-bars-pad-frac',
`indent-bars-pattern', and `indent-bars-zigzag' will be used as
defaults for any missing values; see these variables.

Note: on terminal, or if `indent-bars-prefer-character' is
non-nil, any stipple appearance parameters will be ignored."
  :type '(choice
	  (const :tag "No Current Highlighting" :value nil)
	  (plist :tag "Highlight Current Depth"
		 :options
		 ((:color (color :tag "Highlight Color"))
		  (:face (face :tag "Color from Face"))
		  (:face-bg (boolean :tag "Use Face's Background Color"))
		  (:background (color :tag "Background Color of Current Bar"))
		  (:blend (float :tag "Blend Fraction into Existing Color")
			  :value 0.5
			  :match (lambda (_ val) (and (<= val 1) (>= val 0)))
			  :type-error "Factor must be between 0 and 1")
		  (:palette
		   (repeat :tag "Explicit Color/Face List"
			   (choice (color :tag "Color")
				   (face :tag "Foreground from Face")
				   (cons :tag "Background from Face"
					 :format "Background from %v"
					 face
					 (const :format "\n" :value bg)))))
		  (:width (float :tag "Bar Width"))
		  (:pad (float :tag "Bar Padding (from left)"))
		  (:pattern (string :tag "Fill Pattern"))
		  (:zigzag (float :tag "Zig-Zag")))))
  :group 'indent-bars)


;;;;; Other
(defcustom indent-bars-display-on-blank-lines t
  "Whether to display bars on blank lines."
  :type 'boolean
  :group 'indent-bars)

(defcustom indent-bars-prefer-character nil
  "Use characters instead of stipple to draw bars.
Normally characters are used on terminal only.  A non-nil value
specifies using character bars exclusively.  See
`indent-bars-no-stipple-char'."
  :type 'boolean
  :group 'indent-bars)

(defcustom indent-bars-no-stipple-char ?\│
  "Character to display when stipple is unavailable (as in the terminal)."
  :type 'char
  :group 'indent-bars)

(defcustom indent-bars-no-stipple-char-font-weight nil
  "Font weight to use to draw the character bars.
If non-nil, set the no-stipple character font weight accordingly."
  :type `(choice
          (const :tag "Use Default Weight" nil)
          ,@(mapcar (lambda (item) (list 'const (aref item 1)))
                    font-weight-table))
  :group 'indent-bars)

(defcustom indent-bars-unspecified-bg-color "black"
  "Color to use as the frame background color if unspecified.
Unless actively set, most terminal frames do not have a
background color specified.  This setting controls the background
color to use for color blending in that case."
  :type 'color
  :group 'indent-bars)

(defcustom indent-bars-unspecified-fg-color "white"
  "Color to use as the default foreground color if unspecified."
  :type 'color
  :group 'indent-bars)

(defcustom indent-bars-starting-column nil
  "The starting column on which to display the first bar.
Set to nil, for the default behavior (first bar at the first
indent level) or an integer value for some other column."
  :type '(choice (const :tag "Default: 1st indent position" nil)
		 (integer :tag "Specified column"))
  :group 'indent-bars)

(defcustom indent-bars-spacing-override nil
  "Override for default, major-mode based indentation spacing.
Set only if the default guessed spacing is incorrect.  Becomes
buffer-local automatically."
  :local t
  :type '(choice integer (const :tag "Discover automatically" :value nil))
  :group 'indent-bars)

;;;;; Treesitter
(defcustom indent-bars-treesit-support nil
  "Whether to enable tree-sitter support (if available)."
  :type 'boolean
  :group 'indent-bars)

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
  "Configure bar behavior inside strings.
If non-nil, set to a symbol naming a tree-sitter string node type
into which bars will go no deeper than their starting line.  If
this node type is invalid, a message is printed and the feature
is disabled."
  :local t
  :type '(choice (const :tag "Disable" nil) (symbol :tag "Node Type"))
  :group 'indent-bars)

;;;; Colors
(defvar indent-bars--main-color nil)
(defvar indent-bars--depth-palette nil)
(defvar indent-bars--current-depth-palette nil
  "Palette for highlighting current depth.
May be nil, a color string or a vector of colors strings.")

(defun indent-bars--frame-background-color()
  "Return the frame background color."
  (let ((fb (frame-parameter nil 'background-color)))
    (cond ((not fb) "white")
	  ((string= fb "unspecified-bg") indent-bars-unspecified-bg-color)
	  (t fb))))

(defun indent-bars--blend-colors (c1 c2 fac)
  "Return a fractional color between two colors C1 and C2.
Each is a string color.  The fractional blend point is the
float FAC, with 1.0 matching C1 and 0.0 C2."
  (apply #'color-rgb-to-hex
	 (cl-mapcar (lambda (a b)
		      (+ (* a fac) (* b (- 1.0 fac))))
		    (color-name-to-rgb c1) (color-name-to-rgb c2))))

(defun indent-bars--main-color (&optional tint tint-blend blend-override)
  "Calculate the main bar color.
Uses `indent-bars-color' for color and background blend config.
If TINT and TINT-BLEND are passed, first blend the TINT color
into the main color with the requested blend, prior to blending
into the background color.  If BLEND-OVERRIDE is set, use it
instead of the :blend factor in `indent-bars-color'."
  (cl-destructuring-bind (main &key face-bg blend) indent-bars-color
    (let ((col (cond ((facep main)
		      (funcall (if face-bg #'face-background #'face-foreground)
			       main))
		     ((color-defined-p main) main)))
	  (blend (or blend-override blend)))
      (if (and tint tint-blend (color-defined-p tint)) ;tint main color
	  (setq col (indent-bars--blend-colors tint col tint-blend)))
      (if blend				;now blend into BG
	  (setq col (indent-bars--blend-colors
		     col (indent-bars--frame-background-color) blend)))
      col)))

(defun indent-bars--unpack-palette (palette)
  "Process a face or color-based PALETTE."
  (delq nil
	(cl-loop for el in palette
		 collect (cond
			  ((and (consp el) (facep (car el)))
			   (face-background (car el)))
			  ((facep el)
			   (face-foreground el))
			  ((color-defined-p el) el)
			  (t nil)))))

(defun indent-bars--depth-palette (&optional blend-override)
  "Calculate the palette of depth-based colors (a vector).
If BLEND-OVERRIDE is set, the main color's :blend will be ignored
and this value will be used instead, for blending into the frame
background color.  See `indent-bars-color-by-depth'."
  (when indent-bars-color-by-depth
    (cl-destructuring-bind (&key regexp face-bg palette blend)
	indent-bars-color-by-depth
      (let ((colors
	     (cond
	      (regexp
	       (indent-bars--depth-colors-from-regexp regexp face-bg))
	      (palette
	       (indent-bars--unpack-palette palette)))))
	(vconcat
	 (if (or blend blend-override)
	     (mapcar (lambda (c)
		       (indent-bars--main-color c blend blend-override))
		     colors)
	   colors))))))

(defun indent-bars--current-depth-palette ()
  "Colors for highlighting the current depth bar.
A color or palette (vector) of colors is returned, which may be
nil, in which case no special current depth-coloring is used.
See `indent-bars-highlight-current-depth' for
configuration."
  (when indent-bars-highlight-current-depth
    (cl-destructuring-bind (&key color face face-bg blend palette &allow-other-keys)
	indent-bars-highlight-current-depth
      (let ((color
	     (cond
	      ((facep face)
	       (funcall (if face-bg #'face-background #'face-foreground)
			face))
	      ((and color (color-defined-p color)) color))))

	(cond
	 ;; An explicit palette
	 (palette
	  (vconcat (indent-bars--unpack-palette palette)))

	 ;; A specified color (possibly to blend in)
	 (color
	  (if (string= color "unspecified-fg")
	      (setq color indent-bars-unspecified-fg-color))
	  (if blend
	      (if indent-bars--depth-palette ; blend into normal depth palette
		  (vconcat (mapcar (lambda (c)
				     (indent-bars--blend-colors color c blend))
				   indent-bars--depth-palette))
		;; Just blend into main color
		(indent-bars--blend-colors color indent-bars--main-color blend))
	    color))
	 
	 ;; blend-only without a specified color: re-blend originals with BG
	 (blend
	  (or (indent-bars--depth-palette blend)
	      (indent-bars--main-color nil nil blend))))))))

(defun indent-bars--depth-colors-from-regexp (regexp &optional face-bg)
  "Return a list of depth colors (strings) for faces matching REGEXP.
The first capture group in REGEXP will be interpreted as a number
and used to sort the list numerically.  A list of the foreground
color of the matching, sorted faces will be returned, unless
FACE-BG is non-nil, in which case the background color is
returned."
  (mapcar (lambda (x) (funcall (if face-bg #'face-background #'face-foreground)
			       (cdr x) nil t))
          (seq-sort-by #'car
		       (lambda (a b) (cond
				      ((not (numberp b)) t)
				      ((not (numberp a)) nil)
				      (t (< a b))))
                       (delq nil
			     (seq-map
			      (lambda (x)
				(let ((n (symbol-name x)))
				  (if (string-match regexp n)
                                      (cons (string-to-number (match-string 1 n))
					    x))))
                              (face-list))))))

(defun indent-bars--get-color (depth  &optional current-highlight)
  "Return the color appropriate for indentation DEPTH.
If CURRENT-HIGHLIGHT is non-nil, return the appropriate highlight
color, if setup (see `indent-bars-highlight-current-depth')."
  (let* ((palette (or (and current-highlight
			   indent-bars--current-depth-palette)
		    indent-bars--depth-palette)))
    (cond
     ((vectorp palette)
      (aref palette (mod (1- depth) (length palette))))
     (palette)  ; single color
     (t indent-bars--main-color))))

;;;; Faces
(defvar indent-bars--faces nil)
(defvar-local indent-bars--remap-face nil)

(defun indent-bars--create-stipple-face (w h rot)
  "Create and set the default `indent-bars-stipple' face.
Create for character size W x H with offset ROT."
  (face-spec-set
   'indent-bars-stipple
   `((t ( :inherit nil :stipple ,(indent-bars--stipple w h rot)
	  ,@(when indent-bars-no-stipple-char-font-weight
              `(:weight ,indent-bars-no-stipple-char-font-weight)))))))

(defun indent-bars--calculate-face-spec (depth)
  "Calculate the face spec for indentation bar at an indentation DEPTH.
DEPTH starts at 1."
  `((t . ( :inherit indent-bars-stipple
	   :foreground ,(indent-bars--get-color depth)))))

(defun indent-bars--create-faces (num &optional redefine)
  "Create bar faces up to depth NUM, redefining them if REDEFINE is non-nil.
Saves the vector of face symbols in variable
`indent-bars--faces'."
  (setq indent-bars--faces
	(vconcat
	 (cl-loop for i from 1 to num
		  for face = (intern (format "indent-bars-%d" i))
		  do
		  (if (and redefine (facep face)) (face-spec-reset-face face))
		  (face-spec-set face (indent-bars--calculate-face-spec i))
		  collect face))))

(defsubst indent-bars--face (depth)
  "Return the bar face for bar DEPTH, creating it if necessary."
  (if (> depth (length indent-bars--faces))
      (indent-bars--create-faces depth))
  (aref indent-bars--faces (1- depth)))

(defvar indent-bars-orig-unfontify-region nil)
(defun indent-bars--unfontify (beg end)
  "Unfontify region between BEG and END.
Removes the display properties in addition to the normal managed
font-lock properties."
  (let ((font-lock-extra-managed-props
         (append '(display) font-lock-extra-managed-props)))
    (funcall indent-bars-orig-unfontify-region beg end)))

;;;; Indentation
(defvar-local indent-bars-spacing nil)
(defvar-local indent-bars--offset nil)
(defvar-local indent-bars--no-stipple nil)

(defsubst indent-bars--depth (len)
  "Number of possible bars for initial blank string of length LEN.
Note that the first bar is expected at `indent-bars-starting-column'."
  (setq len (- len indent-bars--offset))
  (cond ((>= len indent-bars-spacing) (/ (1+ len) indent-bars-spacing))
	((> len 0) 1)
	(t 0)))

(defun indent-bars--blank-string (off nbars bar-from &optional width)
  "Return a blank string with bars displayed.
OFF is character offset for the first bar, NBARS is the desired
number of bars to add, and BAR-FROM is the starting index of the
first bar (>=1).  WIDTH is the string width to return, right
padding with space if needed.  Bars are displayed using stipple
properties or characters; see `indent-bars-prefer-character'."
  (concat (make-string off ?\s)
	  (string-join
	   (cl-loop for depth from bar-from to (+ bar-from nbars -1)
		    collect (if indent-bars--no-stipple
				(indent-bars--no-stipple-char depth)
			      (propertize " " 'face (indent-bars--face depth))))
	   (make-string (1- indent-bars-spacing) ?\s))
	  (if width
	      (make-string (- width
			      (+ off nbars (* (1- nbars) (1- indent-bars-spacing))))
			   ?\s))))

(defun indent-bars--tab-display (p off bar-from max)
  "Display up to MAX bars on the tab at P, offseting them by OFF.
Bars are spaced by `indent-bars-spacing'.  BAR-FROM is the bar
number for the first bar.  Returns the number of bars actually
displayed."
  (let* ((nb (min max (/ (- tab-width off -1) indent-bars-spacing)))
	 (str (indent-bars--blank-string off nb bar-from tab-width)))
    (put-text-property p (+ p 1) 'display str)
    nb))

(defun indent-bars--draw-line (nbars start end &optional invent)
  "Draw NBARS bars on the line between START and END.
START is assumed to be on a line beginning position.  Drawing
starts at `indent-bars-starting-column'.  Tabs at the line
beginning are replaced with display properties, if
`indent-tabs-mode' is enabled.  If INVENT is non-nil and the
line's length is insufficient to display all NBARS bars, bars
will be invented.  That is, the line's final newline, which is
only in this case expected to be located at END, will have
display properties set to fill out the remaining bars, if any."
  (let* ((tabs (when (and indent-tabs-mode
			  (save-excursion
			    (goto-char start) (looking-at "^\t+")))
		 (- (match-end 0) (match-beginning 0))))
	 (vp indent-bars--offset)
	 (bar 1) prop fun tnum bcount)
    (when tabs
      (while (and (<= bar nbars) (< (setq tnum (/ vp tab-width)) tabs))
	(setq bcount (indent-bars--tab-display (+ start tnum) (mod vp tab-width)
					       bar (- nbars bar -1)))
	(cl-incf bar bcount)
	(cl-incf vp (* bcount indent-bars-spacing)))
      (cl-incf start (+ (mod vp tab-width) (/ vp tab-width))))
    (when (<= bar nbars) ; still bars to show
      (if indent-bars--no-stipple
	  (setq prop 'display fun #'indent-bars--no-stipple-char)
	(setq prop 'face fun #'indent-bars--face))
      (let ((pos (if tabs start (+ start indent-bars--offset))))
	(while (and (<= bar nbars) (< pos end))
	  (put-text-property pos (1+ pos) prop (funcall fun bar))
	  (cl-incf bar)
	  (cl-incf pos indent-bars-spacing))
	(if (and invent (<= bar nbars)) ; STILL bars to show: invent them
	    (put-text-property
	     end (1+ end) 'display
	     (concat (indent-bars--blank-string (- pos end) (- nbars bar -1) bar nil)
		     "\n")))))))

;;;; Stipple Display
(defsubst indent-bars--block (n)
  "Create a block of N low-order 1 bits."
  (- (ash 1 n) 1))

(defun indent-bars--stipple-rot (w)
  "Return the stipple rotation for pattern with W for the current window."
  (mod (car (window-edges nil t nil t)) w))

(defun indent-bars--rot (num w n)
  "Shift number NUM of W bits up by N bits, carrying around to the low bits.
N should be strictly less than W and the returned value will fit
within W bits."
  (logand (indent-bars--block w) (logior (ash num n) (ash num (- n w)))))

(defun indent-bars--row-data (w pad rot width-frac)
  "Calculate stipple row data to fit in character of width W.
The width of the pattern of filled pixels is determined by
WIDTH-FRAC.  The pattern itself is shifted up by PAD bits (which
shifts the pattern to the right, for positive values of PAD).
Subsequently, the value is shifted up (with W-bit wrap-around) by
ROT bits, and returned.  ROT is the starting bit offset of a
character within the closest stipple repeat to the left; i.e. if
pixel 1 of the stipple aligns with pixel 1 of the chacter, ROT=0.
ROT should be less than W."
  (let* ((bar-width (max 1 (round (* w width-frac))))
	 (num (indent-bars--rot
	       (ash (indent-bars--block bar-width) pad) w rot)))
    (apply #'unibyte-string
	   (cl-loop for boff = 0 then (+ boff 8) while (< boff w)
		    for nbits = (min 8 (- w boff))
		    collect (ash (logand num
					 (ash (indent-bars--block nbits) boff))
				 (- boff))))))

;; ** Notes on the stipples:
;;
;; indent-bars by default uses a selectively-revealed stipple pattern
;; with a width equivalent to the (presumed fixed) width of individual
;; characters to efficiently draw bars.  A stipple pattern is drawn as
;; a fixed repeating bit pattern, with its lowest bits and earlier
;; bytes leftmost.  It is drawn with respect to the *entire frame*,
;; with its first bit aligned with the first (leftmost) frame pixel.
;; 
;; Turning on :stipple for a character merely "opens a window" on that
;; frame-filling, repeating stipple pattern.  Since the pattern starts
;; outside the body (in literally the first frame pixel, typically in
;; the fringe), you must consider the shift between the first pixel of
;; a character and the first pixel of the repeating stipple block at
;; that pixel position or above:
;; 
;;     |<-frame edge |<---buffer/window edge
;;     |<--w-->|<--w-->|<--w-->|     w = pattern width
;;     | marg/fringe |<-chr->|     chr = character width = w
;;             |<-g->|               g = gutter offset of chr start, g<w
;;
;; Or, when the character width exceeds the margin/fringe offset:
;; 
;;     |<-frame edge |<---buffer/window edge
;;     |<--------w-------->|<---------w-------->|
;;     | marg/fringe |<-------chr------->|
;;     |<-----g----->|
;;
;; So g = (mod marg/fringe w).
;; 
;; When the block/zigzag/whatever pattern is made, to align with
;; characters, it must get shifted up (= right) by g bits, with carry
;; over (wrap) around w=(window-font-width) bits (i.e the width of the
;; bitmap).  The byte/bit pattern is first-lowest-leftmost.
;;
;; Note that different window sides will often have different g
;; values, which means the same bitmap cannot work for the buffer in
;; both windows.  So showing the same buffer side by side can lead to
;; mis-alignment in the non-active buffer.
;;
;; Solutions:
;;
;;  - Use window hooks to update the stipple bitmap as focus or
;;    windows change.  So at least the focused buffer looks correct.
;;  - Otherwise, just live with it?
;;  - Suggest using separate frames for this?
;;  - Hide the bars be setting the stipple pattern to 0 in unfocused
;;    windows?  But this isn't great.  The information is useful.
;;  - Could also hide only in non-main window showing the current
;;    buffer, with different g values.  But it will be suprising when
;;    they vanish only when the same buffer is shown twice.
;;  - Provide a helper command to adjust window sizes so g is
;;    preserved (for a given w).  But two *different* buffers, both
;;    side-by-side, make this impossible to work at the same time (if
;;    they have different font sizes).  Maybe that's OK though, if you
;;    are considering the current buffer only.
;;  - Use C-x 4 c (clone-indirect-buffer-other-window).  Probably the
;;    best solution!  But a bug in Emacs <29 means
;;    `face-remapping-alist' is shared between indirect and master
;;    buffers.  Fixed in Emacs 29.

(defun indent-bars--stipple (w h rot
			       &optional width-frac pad-frac pattern zigzag)
  "Calculate stipple bitmap pattern for char width W and height H.
ROT is the number of bits to rotate the pattern around to the
right (with wrap).

Uses configuration variables `indent-bars-width-frac',
`indent-bars-pad-frac', `indent-bars-pattern', and
`indent-bars-zigzag', unless PAD-FRAC, WIDTH-FRAC, PATTERN,
and/or ZIGZAG are set (the latter overriding the config
variables, which see)."
  (unless (or (not (display-graphic-p)) indent-bars-prefer-character)
    (let* ((rowbytes (/ (+ w 7) 8))
	   (pattern (or pattern indent-bars-pattern))
	   (pat (if (< h (length pattern)) (substring pattern 0 h) pattern))
	   (plen (length pat))
	   (chunk (/ (float h) plen))
	   (small (floor chunk))
	   (large (ceiling chunk))
	   (pad-frac (or pad-frac indent-bars-pad-frac))
	   (pad (round (* w pad-frac)))
	   (zigzag (or zigzag indent-bars-zigzag))
	   (zz (if zigzag (round (* w zigzag)) 0))
	   (zeroes (make-string rowbytes ?\0))
	   (width-frac (or width-frac indent-bars-width-frac))
	   (dlist (if (and (= plen 1) (not (string= pat " "))) ; solid bar
		      (list (indent-bars--row-data w pad rot width-frac)) ; one row
		    (cl-loop for last-fill-char = nil then x
			     for x across pat
			     for n = small then (if (and (/= x ?\s) (= n small))
						    large
						  small)
			     for zoff = zz then (if (and last-fill-char
							 (/= x ?\s)
							 (/= x last-fill-char))
						    (- zoff) zoff)
			     for row = (if (= x ?\s) zeroes
					 (indent-bars--row-data w (+ pad zoff)
								rot width-frac))
			     append (cl-loop repeat n collect row)))))
      (list w (length dlist) (string-join dlist)))))

;;;; No stipple characters (e.g. terminal)
(defvar indent-bars--no-stipple-chars nil)

(defun indent-bars--no-stipple-char (depth)
  "Return the no-stipple bar character for DEPTH."
  (if (> depth (length indent-bars--no-stipple-chars))
      (indent-bars--create-no-stipple-chars depth))
  (aref indent-bars--no-stipple-chars (1- depth)))

(defun indent-bars--create-no-stipple-chars (num)
  "Setup bar characters for bar faces up to depth NUM.
Used when not using stipple display (on terminal, or by request;
see `indent-bars-prefer-character')."
  (setq indent-bars--no-stipple-chars
	(vconcat
	 (nreverse
	  (cl-loop with l = (length indent-bars--no-stipple-chars)
		   for d from num downto 1
		   collect
		   (or  (and (< d l) (aref indent-bars--no-stipple-chars (1- d)))
			(propertize (string indent-bars-no-stipple-char)
				    'face (indent-bars--face d))))))))

;;;; Tree-sitter
(defvar-local indent-bars--ts-parser nil)
(defvar-local indent-bars--ts-query nil)
(defvar-local indent-bars--ts-string-query nil)

(defun indent-bars--ts-node-query (node query &optional start-only first-spanning)
  "Capture node(s) spanning NODE matching QUERY.
QUERY is a compiled treesit query.  If START-ONLY is non-nil, the
query searches for matching nodes spanning the start of the node
at point.  If FIRST-SPANNING is non-nil, return the first
matching node, but only if it fully spans the start and end range
of NODE."
  (let* ((start (treesit-node-start node))
	 (end (if start-only start (treesit-node-end node)))
	 (nodes (treesit-query-capture indent-bars--ts-parser query
				       start end t)))
    (when (and first-spanning nodes)
      (let ((n (car nodes)))
	(setq nodes
	      (and (<= (treesit-node-start n) start)
		   (>= (treesit-node-end n) end)
		   n))))
    nodes))

(defsubst indent-bars--indent-at-node (node)
  "Return the current indentation at the start of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (current-indentation)))

(defun indent-bars--current-indentation-depth (&optional on-bar)
  "Calculate current indentation depth.
If ON-BAR is non-nil, report a line with content beginning on a
bar position at that position.  If treesit support is enabled,
searches for parent nodes with types specified in
`indent-bars-treesit-wrap' for the current buffer's language,
and, if found, limits the indentation depth to one more than the
topmost matching parent node's initial line's indentation depth.
If `indent-bars-no-descend-string' is non-nil, also look for
enclosing string and mark indent depth no deeper than one more
than the starting line's depth."
  (let* ((c (current-indentation))
	 (d (indent-bars--depth c))
	 (p (point))
	 (dnew (when-let ((indent-bars--ts-query)
			  ((/= p (point-min)))
			  (node (treesit-node-on (1- p) p indent-bars--ts-parser)))
		 (if (and indent-bars-no-descend-string
			  (indent-bars--ts-node-query
			   node indent-bars--ts-string-query t))
		     (1+ (indent-bars--depth (indent-bars--indent-at-node node)))
		   (when-let ((ctx (indent-bars--ts-node-query
				    node indent-bars--ts-query nil t)))
		     (1+ (indent-bars--depth
			  (indent-bars--indent-at-node ctx))))))))
    (if dnew (setq d (min dnew d)))
    (if (and on-bar (= c (+ indent-bars--offset (* d indent-bars-spacing))))
	(1+ d) d)))

(defun indent-bars--ignore-blank (beg)
  "See if blank lines at BEG should be ignored using tree-sitter.
Blank lines to ignore are those with types in
`indent-bars-treesit-ignore-blank-lines-types'."
  (and indent-bars--ts-parser
       indent-bars-treesit-ignore-blank-lines-types
       (when-let ((n (treesit-node-on beg beg)))
	 (seq-contains-p indent-bars-treesit-ignore-blank-lines-types
			 (treesit-node-type n)))))

;;;; Font Lock
(defvar-local indent-bars--font-lock-keywords nil)
(defvar indent-bars--font-lock-blank-line-keywords nil)

(defun indent-bars--display ()
  "Display indentation bars based on line contents."
  (let* ((b (match-beginning 1))
	 (e (match-end 1))
	 (n (save-excursion
	      (goto-char b)
	      (indent-bars--current-indentation-depth))))
    (when (> n 0) (indent-bars--draw-line n b e)))
  nil)

(defsubst indent-bars--context-bars (end)
  "Maximum number of bars at point and END.
Moves point."
  (max (indent-bars--current-indentation-depth)
       (progn
	 (goto-char (1+ end))		; end is always eol
	 (indent-bars--current-indentation-depth))))

(defun indent-bars--handle-blank-lines ()
  "Display the appropriate bars on regions of one or more blank-only lines.
The region is the full match region of the last match.  Only
called by font-lock if `indent-bars-display-on-blank-lines' is
non-nil.  Called on complete multi-line blank line regions.  Uses
the surrounding line indentation to determine additional bars to
display on each line, using `indent-bars--draw-line'.

Note: blank lines at the very beginning or end of the buffer are
not indicated, even if otherwise they would be.  If
`indent-bars-treesit-ignore-blank-lines-types' is configured,
ignore blank lines whose starting positions are directly spanned
by nodes of those types (e.g. module)."
  (let* ((beg (match-beginning 0))
	 (end (match-end 0))
	 ctxbars)
    (save-excursion
      (goto-char (1- beg))
      (beginning-of-line 1)
      (when (and (not (indent-bars--ignore-blank beg))
		 (> (setq ctxbars (indent-bars--context-bars end)) 0))
	(goto-char beg)
	(while (< (point) end) ;note: end extends 1 char beyond blank line range
	  (let* ((bp (line-beginning-position))
		 (ep (line-end-position))
		 (pm (point-max)))
	    (unless (= ep pm)
	      (indent-bars--draw-line ctxbars bp ep 'invent))
	    (beginning-of-line 2)))))))

(defvar font-lock-beg) (defvar font-lock-end) ; Dynamic font-lock variables!
(defun indent-bars--extend-blank-line-regions ()
  "Extend the region about to be font-locked to include stretches of blank lines."
  ;; (message "request to extend: %d->%d" font-lock-beg font-lock-end)
  (let ((changed nil) (chars " \t\n"))
    (goto-char font-lock-beg)
    (when (< (skip-chars-backward chars) 0)
      (unless (bolp) (beginning-of-line 2)) ; spaces at end don't count
      (when (< (point) font-lock-beg)
	(setq changed t font-lock-beg (point))))
    (goto-char font-lock-end)
    (when (> (skip-chars-forward chars) 0)
      (unless (bolp) (beginning-of-line 1))
      (when (> (point) font-lock-end)
	(setq changed t font-lock-end (point))))
    ;; (if changed (message "expanded to %d->%d" font-lock-beg font-lock-end))
    changed))

;;;; Current indentation highlight
(defvar-local indent-bars--current-depth 0)
(defvar indent-bars--current-bg-color nil)
(defvar-local indent-bars--current-depth-stipple nil)

(defun indent-bars--set-current-bg-color ()
  "Record the current bar background color."
  (cl-destructuring-bind (&key background &allow-other-keys)
      indent-bars-highlight-current-depth
    (setq indent-bars--current-bg-color background)))

(defun indent-bars--set-current-depth-stipple (&optional w h rot)
  "Set the current depth stipple highlight (if any).
One of the keywords :width, :pad, :pattern, or :zigzag must be
set in `indent-bars-highlight-current-depth' config.  W, H, and
ROT are as in `indent-bars--stipple', and have similar default values."
  (cl-destructuring-bind (&key width pad pattern zigzag &allow-other-keys)
      indent-bars-highlight-current-depth
    (when (or width pad pattern zigzag)
      (let* ((w (or w (window-font-width)))
	     (h (or h (window-font-height)))
	     (rot (or rot (indent-bars--stipple-rot w))))
	(setq indent-bars--current-depth-stipple
	      (indent-bars--stipple w h rot width pad pattern zigzag))))))

(defun indent-bars--highlight-current-depth ()
  "Refresh current indentation depth highlight.
Works by remapping the appropriate indent-bars-N face."
  (let* ((depth (indent-bars--current-indentation-depth 'on-bar)))
    (when (and depth (not (= depth indent-bars--current-depth)))
      (if indent-bars--remap-face 	; out with the old
	  (face-remap-remove-relative indent-bars--remap-face))
      (setq indent-bars--current-depth depth)
      (when (> depth 0)
	(let ((face (indent-bars--face depth))
	      (hl-col (and indent-bars--current-depth-palette
			   (indent-bars--get-color depth 'highlight)))
	      (hl-bg indent-bars--current-bg-color))
	  (when (or hl-col hl-bg indent-bars--current-depth-stipple)
	    (setq indent-bars--remap-face
		  (apply #'face-remap-add-relative face
			 `(,@(when hl-col `(:foreground ,hl-col))
			   ,@(when hl-bg `(:background ,hl-bg))
			   ,@(when indent-bars--current-depth-stipple
			       `(:stipple ,indent-bars--current-depth-stipple)))))))))))

;;;; Text scaling and window hooks
(defvar-local indent-bars--remap-stipple nil)
(defvar-local indent-bars--gutter-rot 0)
(defun indent-bars--window-change (win)
  "Update the stipple for buffer in window WIN, if selected."
  (when (eq win (selected-window))
    (let* ((w (window-font-width))
	   (rot (indent-bars--stipple-rot w)))
      (when (/= indent-bars--gutter-rot rot)
	(setq indent-bars--gutter-rot rot)
	(indent-bars--resize-stipple w rot)))))

(defun indent-bars--resize-stipple (&optional w rot)
  "Recreate stipple(s) with updated size.
W is the optional `window-font-width' and ROT the bit rotation If
not passed they will be calculated."
  (if indent-bars--remap-stipple
      (face-remap-remove-relative indent-bars--remap-stipple))
  (let* ((w (or w (window-font-width)))
	 (rot (or rot (indent-bars--stipple-rot w)))
	 (h (window-font-height)))
    (setq indent-bars--remap-stipple
	  (face-remap-add-relative
	   'indent-bars-stipple
	   :stipple (indent-bars--stipple w h rot)))
    (when indent-bars--current-depth-stipple
      (indent-bars--set-current-depth-stipple w h rot)
      (setq indent-bars--current-depth 0)
      (indent-bars--highlight-current-depth))))

;;;; Setup and mode
(defun indent-bars--guess-spacing ()
  "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
  (cond
   (indent-bars-spacing-override)
   ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
    py-indent-offset)
   ((and (derived-mode-p 'python-mode) (boundp 'python-indent-offset))
    python-indent-offset)
   ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
    ruby-indent-level)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
    scala-indent:step)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
    scala-mode-indent:step)
   ((and (or (derived-mode-p 'scss-mode) (derived-mode-p 'css-mode))
	 (boundp 'css-indent-offset))
    css-indent-offset)
   ((and (derived-mode-p 'nxml-mode) (boundp 'nxml-child-indent))
    nxml-child-indent)
   ((and (derived-mode-p 'coffee-mode) (boundp 'coffee-tab-width))
    coffee-tab-width)
   ((and (derived-mode-p 'js-mode) (boundp 'js-indent-level))
    js-indent-level)
   ((and (derived-mode-p 'js2-mode) (boundp 'js2-basic-offset))
    js2-basic-offset)
   ((and (fboundp 'derived-mode-class)
	 (eq (derived-mode-class major-mode) 'sws-mode) (boundp 'sws-tab-width))
    sws-tab-width)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
    web-mode-markup-indent-offset)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
    web-mode-html-offset)
   ((and (local-variable-p 'c-basic-offset) (boundp 'c-basic-offset))
    c-basic-offset)
   ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
    yaml-indent-offset)
   ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
    elixir-smie-indent-basic)
   ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
    lisp-body-indent)
   ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
    cobol-tab-width)
   ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
    tab-width)
   ((and (boundp 'standard-indent) standard-indent))
   (t 4))) 				; backup

(defun indent-bars--setup-font-lock ()
  "Setup font lock keywords and functions for indent-bars."
  (unless (eq font-lock-unfontify-region-function #'indent-bars--unfontify)
    (setq indent-bars-orig-unfontify-region font-lock-unfontify-region-function))
  (setq-local font-lock-unfontify-region-function #'indent-bars--unfontify)
  (setq indent-bars--font-lock-keywords
	`((,(rx-to-string `(seq bol
				(group
				 ,(if (not indent-tabs-mode)
				      `(>= ,(1+ indent-bars--offset) ?\s)
				    '(+ (any ?\t ?\s))))
				(not (any ?\t ?\s ?\n))))
	   (1 (indent-bars--display)))))
  (font-lock-add-keywords nil indent-bars--font-lock-keywords t)
  (if indent-bars-display-on-blank-lines
      (let ((re (rx bol (* (or ?\s ?\t ?\n)) ?\n))) ; multi-line blank region
	(setq indent-bars--font-lock-blank-line-keywords
	      `((,re (0 (indent-bars--handle-blank-lines)))))
	(font-lock-add-keywords nil indent-bars--font-lock-blank-line-keywords t)
	(add-hook 'font-lock-extend-region-functions
		  #'indent-bars--extend-blank-line-regions 95 t))))

(defvar indent-bars-mode)
(defun indent-bars-setup ()
  "Setup all face, color, bar size, and indentation info for the current buffer."
  ;; Spacing
  (setq indent-bars-spacing (indent-bars--guess-spacing)
	indent-bars--offset (or indent-bars-starting-column indent-bars-spacing))

  ;; Colors
  (setq indent-bars--main-color (indent-bars--main-color)
	indent-bars--depth-palette (indent-bars--depth-palette)
	indent-bars--current-depth-palette (indent-bars--current-depth-palette))

  ;; Faces
  (indent-bars--create-stipple-face (frame-char-width) (frame-char-height)
				    (indent-bars--stipple-rot (frame-char-width)))
  (indent-bars--create-faces 9 'reset)	; N.B.: extends as needed

  ;; No Stipple (e.g. terminal)
  (setq indent-bars--no-stipple
	(or (not (display-graphic-p)) indent-bars-prefer-character))
  (indent-bars--create-no-stipple-chars 9)
  
  ;; Window state: selection/size
  (add-hook 'window-state-change-functions #'indent-bars--window-change nil t)

  ;; Treesitter
  (when-let (((and indent-bars-treesit-support
		   (fboundp #'treesit-available-p)
		   (treesit-available-p)))
	     (lang (treesit-language-at (point-min)))
	     (types (alist-get lang indent-bars-treesit-wrap)))
    (setq indent-bars--ts-parser
	  (cl-find lang (treesit-parser-list) :key #'treesit-parser-language)
	  indent-bars--ts-query
	  (treesit-query-compile lang `([,@(mapcar #'list types)] @ctx)))
    (when indent-bars-no-descend-string
      (let ((query `([(,indent-bars-no-descend-string)] @s))
	    (pm (point-min)))
	(setq indent-bars--ts-string-query (treesit-query-compile lang query))
	;; Test it to be sure
	(condition-case err
	    (treesit-query-capture indent-bars--ts-parser
				   indent-bars--ts-string-query pm pm t)
	  (treesit-query-error
	   (setq indent-bars-no-descend-string nil)
	   (message "indent-bars: malformed string query; disabling.  See indent-bars-no-descend-string.\n%s" err))))))

  ;; Current depth highlight
  (when indent-bars-highlight-current-depth
    (indent-bars--set-current-bg-color)
    (indent-bars--set-current-depth-stipple)
    (add-hook 'post-command-hook #'indent-bars--highlight-current-depth nil t)
    (setq indent-bars--current-depth 0)
    (indent-bars--highlight-current-depth))

  ;; Resize
  (add-hook 'text-scale-mode-hook #'indent-bars--resize-stipple nil t)
  (indent-bars--resize-stipple)		; just in case

  ;; Font-lock
  (indent-bars--setup-font-lock)
  (font-lock-flush))

(defun indent-bars-teardown ()
  "Tears down indent-bars."
  (face-spec-set 'indent-bars-stipple nil 'reset)
  (cl-loop for f in indent-bars--faces do (face-spec-set f nil 'reset))
  (font-lock-remove-keywords nil indent-bars--font-lock-keywords)
  (font-lock-remove-keywords nil indent-bars--font-lock-blank-line-keywords)
  (font-lock-flush)
  (font-lock-ensure)
  (if indent-bars--remap-face
      (face-remap-remove-relative indent-bars--remap-face))
  (setq font-lock-unfontify-region-function indent-bars-orig-unfontify-region)
  (setq indent-bars--depth-palette nil
	indent-bars--faces nil
	indent-bars--remap-face nil
	indent-bars--gutter-rot 0
	indent-bars--current-depth-palette nil
	indent-bars--current-depth-stipple nil
	indent-bars--no-stipple-chars nil
	indent-bars--current-bg-color nil
	indent-bars--current-depth 0
	indent-bars--ts-query nil)
  (remove-hook 'text-scale-mode-hook #'indent-bars--resize-stipple t)
  (remove-hook 'post-command-hook #'indent-bars--highlight-current-depth t)
  (remove-hook 'font-lock-extend-region-functions
	       #'indent-bars--extend-blank-line-regions t))

(defun indent-bars-reset ()
  "Reset indent-bars config."
  (interactive)
  (indent-bars-teardown)
  (indent-bars-setup))

(defun indent-bars-setup-and-remove ()
  "Setup indent bars and remove from `after-make-frame-functions'."
  (remove-hook 'after-make-frame-functions #'indent-bars-setup-and-remove)
  (indent-bars-setup))

;;;###autoload
(define-minor-mode indent-bars-mode
  "Indicate indentation with configurable bars."
  :global nil
  :group 'indent-bars
  (if indent-bars-mode
      (if (and (daemonp) (not (frame-parameter nil 'client)))
	  (let ((buf (current-buffer)))
	    (add-hook 'after-make-frame-functions
		      (lambda () (with-current-buffer buf
				   (indent-bars-setup-and-remove)))
		      nil t))
	(indent-bars-setup))
    (indent-bars-teardown)))

(provide 'indent-bars)

;;; indent-bars.el ends here
