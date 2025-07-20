;;; indent-bars.el --- Highlight indentation with bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: J.D. Smith <jdtsmith+elpa@gmail.com>
;; Homepage: https://github.com/jdtsmith/indent-bars
;; Package-Requires: ((emacs "27.1") (compat "30"))
;; Version: 0.9.2
;; Keywords: convenience

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

;; indent-bars highlights indentation with configurable vertical
;; graphical bars, using stipples.  The color and appearance (weight,
;; pattern, position within the character, zigzag, etc.) are all
;; configurable.  Options include depth-varying colors and
;; highlighting the indentation depth of the current line.  Bars span
;; blank lines, by default.  indent-bars works in any mode using fixed
;; tab or space-based indentation.  In the terminal (or on request) it
;; uses vertical bar characters instead of stipple patterns.  Optional
;; treesitter support is also available; see indent-bars-ts.el.

;;;; For Developers:
;;
;; To efficiently accommodate simultaneous alternative bar styling, we
;; do two things:
;;
;;  1. Collect all the style related information (color, stipple
;;     pattern, etc.) into a single struct, operating on one such
;;     "current" style struct at a time.
;;
;;  2. Provide convenience functions for duplicate "alternative"
;;     custom style variables the user can configure; see
;;     `indent-bars--style'.  These variables can "inherit" nil or
;;     omitted plist variables from their parent var.
;;
;; Note the shorthand substitution for style related slots;
;; see file-local-variables at the end:
;;
;;    ibs/  => indent-bars-style-

;;; Code:
;;;; Requires
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'color)
(require 'timer)
(require 'outline)
(require 'font-lock)
(require 'jit-lock)
(require 'face-remap)
(require 'cus-edit)
(require 'compat)

;;;; Variables
(defvar indent-bars-mode)
(defvar-local indent-bars--regexp nil)
;;;; Customization
(defgroup indent-bars nil
  "Highlight indentation bars."
  :group 'convenience
  :prefix "indent-bars-")

(defgroup indent-bars-style nil
  "Highlight indentation bars."
  :group 'indent-bars
  :prefix "indent-bars-")

(defvar indent-bars-depth-update-delay)
(defvar indent-bars-custom-set nil)
(defvar indent-bars--custom-set-inhibit nil)
(defun indent-bars--custom-set (sym val)
  "Set SYM to VAL, and reset `indent-bars' in all windows."
  (set-default-toplevel-value sym val)
  (when (and (not indent-bars--custom-set-inhibit) (boundp 'indent-bars-mode))
    (let ((indent-bars--custom-set-inhibit t)) ; prevent re-entry
      (cl-loop for win in (window-list)
	       if (buffer-local-value 'indent-bars-mode (window-buffer win)) do
	       (with-selected-window win
		 (indent-bars-reset)
		 (let ((indent-bars-depth-update-delay 0))
		   (indent-bars--compute-and-highlight-current-depth 'force))
		 (run-hooks 'indent-bars-custom-set))
	       and return win))))

;;;;; Stipple Bar Shape
(defcustom indent-bars-width-frac 0.25
  "The width of the indent bar as a fraction of the character width.
Applies to stipple-based bars only."
  :type '(float :tag "Width Fraction"
		:match (lambda (_ val) (and val (<= val 1) (>= val 0)))
		:type-error "Fraction must be between 0 and 1")
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-pad-frac 0.1
  "The offset of the bar from the left edge of the character.
A float, the fraction of the character width.  Applies to
 stipple-based bars only."
  :type '(float :tag "Offset Fraction"
		:match (lambda (_ val) (and val (<= val 1) (>= val 0)))
		:type-error "Fraction must be between 0 and 1")
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-pattern " . . ."
  "A pattern specifying the vertical structure of indent bars.
Space signifies blank regions, and any other character signifies
filled regions.  The pattern length is scaled to match the
character height.  Example: \". . \" would specify alternating
filled and blank regions each approximately one-quarter of the
character height.  Note that the non-blank characters need not be
the same (e.g., see `indent-bars-zigzag').  Applies to
stipple-based bars only."
  :type '(string :tag "Fill Pattern")
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

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
`indent-bars-width-frac'.  Applies to stipple-based bars only."
  :type '(choice :tag "Zigzag Options"
		 (const :tag "No Zigzag" :value nil)
		 (float :value 0.1 :tag "Zigzag Fraction"
			:match (lambda (_ val) (and val (<= val 1) (>= val -1)))
			:type-error "Fraction must be between -1 and 1"))
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

;;;;; Bar Colors
(defcustom indent-bars-color
  '(highlight :face-bg t :blend 0.325)
  "The main indent bar color.
The format is a list of 1 required element, followed by an
optional plist (keyword/value pairs):

  (main_color [:face-bg :blend])

where:

  MAIN_COLOR: Specifies the main indentation bar
    color (required).  It is either a face name symbol, from
    which the foreground color will be used as the primary bar
    color, or an explicit color (a string).  If nil, the default
    color foreground will be used.

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
  '(list :tag "Color Options"
	 (choice :tag "Main Bar Color"
		 color
		 (face :tag "from Face")
		 (const :tag "Use default" nil))
	 (plist :tag "Other Options"
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
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-color-by-depth
  '(:regexp "outline-\\([0-9]+\\)" :blend 1)
  "Configuration for depth-varying indentation bar coloring.
If non-nil, depth-based coloring is performed.  This should be a
plist with keys:

    ([:regexp [:face-bg] | :palette] [:blend])

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
    are blended with the main color, prior to possibly blending
    with the frame background color (see `indent-bars-color' for
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
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

;;;;; Depth Highlighting
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

By default, the highlighted bar's color will be the same as the
underlying bar color.  With PALETTE, COLOR or FACE set, all bars
at the current depth will be highlighted in the appropriate
color, either from an explicit COLOR, a PALETTE list (see
`indent-bars-color-by-depth'), or, if FACE is set, FACE's
foreground or background color (the latter if FACE-BG is
non-nil).  If PALETTE is provided, it overrides any other
foreground color setting for the current depth highlight bar.  If
BACKGROUND is set to a color, this will be used for the
background color of the current depth bar.

If BLEND is provided, it is a blend fraction between 0 and 1 for
blending the specified highlight color with the
existing (depth-based or main) bar color; see `indent-bars-color'
for its meaning.  BLEND=1 indicates using the full, unblended
highlight color (and is the same as omitting BLEND).

As a special case, if BLEND is provided, but neither COLOR nor
FACE is, BLEND is used as a (presumably distinct) blend factor
between the usual color for that bar and the frame background.
The original colors are specified in `indent-bars-color-by-depth'
or `indent-bars-color'.  In this manner the current-depth
highlight can be made a more (or less) prominent version of the
default coloring, just by setting BLEND.

If any of WIDTH, PAD, PATTERN, or ZIGZAG are set, the stipple bar
pattern at the current level will be altered as well.  Note that
`indent-bars-width-frac', `indent-bars-pad-frac',
`indent-bars-pattern', and `indent-bars-zigzag' will be used as
defaults for any missing values; see these variables.

Note: on terminal, or if `indent-bars-prefer-character' is
non-nil, any stipple appearance parameters will be ignored."
  :type '(choice
	  :tag "Highlighting Options"
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
  :group 'indent-bars-style
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default)

(defcustom indent-bars-highlight-selection-method 'context
  "Method for selecting bar depth for current indentation highlight.
If nil, the last showing bar on the current line is selected for
highlight.  If the symbol `on-bar', and the start of the text on
the line would have fallen directly on a bar, highlight that bar
depth instead.  If `context', use `on-bar' logic, but only if a
directly adjacent (non-blank) context line is indented deeper, by
more than one indent spacing.  Otherwise select the last bar
showing for highlight (i.e. the same as CONTEXT nil)."
  :type '(choice (const :tag "Containing" nil)
		 (const :tag "On Bar" on-bar)
		 (const :tag "Context" context))
  :group 'indent-bars)

(defcustom indent-bars-depth-update-delay 0.075
  "Minimum delay time in seconds between depth highlight updates.
Has effect only if `indent-bars-highlight-current-depth' is
non-nil.  Set to 0 for instant depth updates."
  :type 'float
  :group 'indent-bars)

;;;;; Other
(defcustom indent-bars-display-on-blank-lines t
  "Whether to display bars on blank lines.
Bars are shown only on blank lines contiguously adjacent to lines
already showing bars, by default the deepest adjacent non-blank
line, or, if set to `least' the least deep such line."
  :type '(choice
	  (const :tag "Disabled" nil)
	  (const :tag "Deepest adjacent" t)
	  (const :tag "Least deep adjacent" least))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-no-descend-string t
  "Configure bar behavior inside strings.
If non-nil, displayed bars inside the string will go no deeper
than the one more than the indent level of the string's starting
line.  If the symbol `all', no bars will be included inside
multiline strings at all."
  :local t
  :type '(choice (const :tag "All normal bars appear inside strings" nil)
		 (const :tag "Only one bar deeper than string start appears" t)
		 (const :tag "No bars in multi-line strings" all))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-no-descend-lists nil
  "Configure bar behavior inside lists.
If non-nil, displayed bars will go no deeper than the indent
level at the starting line of the innermost containing list.  If
t, any list recognized by the active syntax table will be used to
identify enclosing list contexts.  If set to a list of
characters, only list-opening characters on this list will
activate bar suppression."
  :local t
  :type '(choice
	  (const :tag "Disabled" nil)
	  (const :tag "Any list element" t)
	  (repeat :tag "List of open paren chars" character))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-prefer-character nil
  "Use characters instead of stipple to draw bars.
Normally characters are used on terminal only.  A non-nil value
specifies using character bars exclusively.  See
`indent-bars-no-stipple-char'."
  :type 'boolean
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-no-stipple-char ?\│
  "Character to display when stipple is unavailable (as in the terminal)."
  :type 'character
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-no-stipple-char-font-weight nil
  "Font weight to use to draw the character bars.
If non-nil, set the no-stipple character font weight accordingly."
  :type `(choice
          (const :tag "Use Default Weight" nil)
          ,@(mapcar (lambda (item) (list 'const (aref item 1)))
                    font-weight-table))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-unspecified-bg-color "black"
  "Color to use as the frame background color if unspecified.
Unless actively set, most terminal frames do not have a
background color specified.  This setting controls the background
color to use for color blending in that case."
  :type 'color
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-unspecified-fg-color "white"
  "Color to use as the default foreground color if unspecified."
  :type 'color
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-starting-column nil
  "The starting column on which to display the first bar.
Set to nil, for the default behavior (first bar at the first
indent level) or an integer value for some other column."
  :type '(choice (const :tag "Default: 1st indent position" nil)
		 (integer :tag "Specified column"))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-spacing-override nil
  "Override for default, major-mode based indentation spacing.
Set only if the default guessed spacing is incorrect.  Becomes
buffer-local automatically."
  :local t
  :type '(choice integer (const :tag "Discover automatically" :value nil))
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

(defcustom indent-bars-treesit-support nil
  "Whether to enable tree-sitter support (if available)."
  :type 'boolean
  :set #'indent-bars--custom-set
  :initialize #'custom-initialize-default
  :group 'indent-bars)

;;;;; Color Utilities
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

(defun indent-bars--colors-from-regexp (regexp &optional face-bg)
  "Return a list of colors (strings) for faces matching REGEXP.
The first capture group in REGEXP will be interpreted as a number
and used to sort the list numerically.  A list of the foreground
color of the matching, sorted faces will be returned, unless
FACE-BG is non-nil, in which case the background color is
returned."
  (mapcar (lambda (x)
	    (funcall (if face-bg #'face-background #'face-foreground)
		     (cdr x) nil 'default))
          (seq-sort-by #'car
	   (lambda (a b) (cond
			  ((not (numberp b)) t)
			  ((not (numberp a)) nil)
			  (t (< a b))))
	   (mapcan
	    (lambda (x)
	      (let ((n (symbol-name x)))
		(when (and (string-match regexp n) (match-string 1 n))
                  (list (cons (string-to-number (match-string 1 n)) x)))))
	    (face-list)))))

(defun indent-bars--unpack-palette (palette)
  "Process a face or color-based PALETTE."
  (cl-loop for el in palette
	   when (cond
		 ((and (consp el) (facep (car el)))
		  (face-background (car el)))
		 ((facep el)
		  (face-foreground el))
		 ((color-defined-p el) el))
	   collect it))

;;;; Style
(defvar indent-bars-style nil
  "The `indent-bars-style' struct for the main style.")

(defvar indent-bars--styles nil
  "List of known indent-bars style structs.")

(cl-declaim (optimize (safety 0))) ; no need for type check
(cl-defstruct
    (indent-bars-style
     (:copier nil)
     (:conc-name ibs/)	; Note: ibs/ => indent-bars-style- in this file
     (:constructor nil)
     (:constructor ibs/create
		   ( &optional tag &aux
		     (stipple-face
		      (intern (format "indent-bars%s-face"
				      (if tag (concat "-" tag) "")))))))
  "A style configuration structure for indent-bars."
  ( tag nil :type string
    :documentation "An optional tag to include in face name")
  ;; Colors and Faces
  ( main-color nil :type string
    :documentation "The main bar color")
  ( depth-palette nil
    :documentation "Palette of depth colors.
May be nil, a color string or a vector of colors strings.")
  ( faces nil :type vector
    :documentation "Depth-based faces.")
  ;; Stipple
  ( stipple-face nil :type face
    :documentation "A stipple face to inherit from.")
  ( no-stipple-chars nil
    :documentation "A vector of styled non-stipple chars.")
  ;; Current depth highlighting
  ( current-stipple-face nil :type face
    :documentation "A current depth stipple face to inherit from.")
  ( current-bg-color nil :type color
    :documentation "The background color of the current depth highlight.")
  ( current-depth-palette nil
    :documentation "Depth palette of highlighted colors."))

(defsubst indent-bars--tag (format-str s &rest r)
  "Tag FORMAT-STR with style S and return the associate interned symbol.
Additional `format' arguments can be passed as R."
  (intern (apply #'format format-str
		 (if (ibs/tag s) (concat "-" (ibs/tag s)) "") r)))

(defun indent-bars--new-style (&optional tag)
  "Create and record a new style struct with TAG.
A new style is only created if an existing style with that TAG is
no yet recorded."
  (or (seq-find (lambda (s) (equal (ibs/tag s) tag)) indent-bars--styles)
      (let ((style (ibs/create tag)))
	(cl-pushnew style indent-bars--styles :test #'equal)
	style)))

;;;;; Colors
(defun indent-bars--main-color (style &optional tint tint-blend blend-override)
  "Calculate the main bar color for STYLE.
Uses `indent-bars-color' for color and background blend config.
If TINT and TINT-BLEND are passed, first blend the TINT color
into the main color with the requested blend, prior to blending
into the background color.  If BLEND-OVERRIDE is set, use it
instead of the :blend factor in `indent-bars-color'."
  (cl-destructuring-bind (main &key face-bg blend) (indent-bars--style style "color")
    (let ((col (cond ((facep main)
		      (funcall (if face-bg #'face-background #'face-foreground)
			       main nil 'default))
		     ((color-defined-p main) main)))
	  (blend (or blend-override blend)))
      (when (string-prefix-p "unspecified-" col)
	(setq col (if face-bg
		      indent-bars-unspecified-bg-color
		    indent-bars-unspecified-fg-color)))
      (if (and tint tint-blend (color-defined-p tint)) ;tint main color
	  (setq col (indent-bars--blend-colors tint col tint-blend)))
      (if blend				;now blend into BG
	  (setq col (indent-bars--blend-colors
		     col (indent-bars--frame-background-color) blend)))
      col)))

(defun indent-bars--depth-palette (style &optional blend-override)
  "Calculate the palette of depth-based colors (a vector) for STYLE.
If BLEND-OVERRIDE is set, the main color's :blend will be ignored
and this value will be used instead, for blending into the frame
background color.  See `indent-bars-color-by-depth'."
  (when-let* ((cbd (indent-bars--style style "color-by-depth")))
    (cl-destructuring-bind (&key regexp face-bg palette blend) cbd
      (let ((colors
	     (cond
	      (regexp
	       (indent-bars--colors-from-regexp regexp face-bg))
	      (palette
	       (indent-bars--unpack-palette palette)))))
	(vconcat
	 (if (or blend blend-override)
	     (mapcar (lambda (c)
		       (indent-bars--main-color style c blend blend-override))
		     colors)
	   colors))))))

(defun indent-bars--current-depth-palette (style)
  "Colors for highlighting the current depth bar for STYLE.
A color or palette (vector) of colors is returned, which may be
nil, in which case no special current depth-coloring is used.
See `indent-bars-highlight-current-depth' for configuration."
  (when-let* ((hcd (indent-bars--style style "highlight-current-depth")))
    (cl-destructuring-bind (&key color face face-bg
				 blend palette &allow-other-keys)
	hcd
      (let ((color
	     (cond
	      ((facep face)
	       (funcall (if face-bg #'face-background #'face-foreground)
			face nil 'default))
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
	      (if-let* ((palette (indent-bars--depth-palette style)))
		  ;; blend into normal depth palette
		  (vconcat
		   (mapcar (lambda (c)
			     (indent-bars--blend-colors color c blend))
			   palette))
		;; Just blend into main color
		(indent-bars--blend-colors color (ibs/main-color style) blend))
	    color))

	 ;; blend-only without a specified color: re-blend originals with BG
	 (blend
	  (or (indent-bars--depth-palette style blend)
	      (indent-bars--main-color style nil nil blend))))))))

(defun indent-bars--get-color (style depth  &optional current-highlight)
  "Return the color appropriate for indentation DEPTH in STYLE.
If CURRENT-HIGHLIGHT is non-nil, return the appropriate highlight
color, if setup (see `indent-bars-highlight-current-depth')."
  (let ((palette (or (and current-highlight
			  (ibs/current-depth-palette style))
		     (ibs/depth-palette style))))
    (cond
     ((vectorp palette)
      (aref palette (mod (1- depth) (length palette))))
     (palette)  ; single color
     (t (ibs/main-color style)))))

;;;;; Faces
(defun indent-bars--window-font-space-width (&optional win)
  "Return the space width of the font in window WIN.
If WIN is not provided, the selected window is used.  This works for
both variable pitch and fixed pitch fonts."
  (let ((win (or win (selected-window))))
    (with-selected-window win
      (or (when-let* (( (fboundp 'font-info))
		      (ff (face-font 'default))
		      (fi (font-info ff))
		      (space-width (aref fi 10)))
	    (and (natnump space-width) (> space-width 0) space-width))
	  (window-font-width)))))

(defun indent-bars--stipple-face-spec (w h rot &optional style stipple)
  "Create a face specification for the stipple face for STYLE.
Create for character size W x H with offset ROT.  If STIPPLE is
non-nil, use it instead of calculating.  Includes
:weight (affecting only non-stipple character display) if
`indent-bars-no-stipple-char-font-weight' (or equivalent for the
STYLE) is non-nil."
  (let ((stipple (or stipple (indent-bars--stipple w h rot style)))
	(wt (indent-bars--style style "no-stipple-char-font-weight")))
    `((t ( :inherit nil
	   ,@(and stipple `(:stipple ,stipple))
	   ,@(and wt `(:weight ,wt)))))))

(defun indent-bars--calculate-face-spec (style depth)
  "Calculate the face spec for bar at DEPTH in STYLE.
DEPTH starts at 1."
  `((t . ( :inherit ,(ibs/stipple-face style)
	   :foreground ,(indent-bars--get-color style depth)))))

(defun indent-bars--create-faces (style num)
  "Create bar faces up to depth NUM for STYLE."
  (vconcat
   (cl-loop
    for i from 1 to num
    for face = (indent-bars--tag "indent-bars%s-%d" style i) do
    (face-spec-set face (indent-bars--calculate-face-spec style i))
    collect face)))

(defsubst indent-bars--face (style depth)
  "Return the bar face for bar DEPTH in STYLE.
The face is created if necessary."
  (when (> depth (length (ibs/faces style)))
    (setf (ibs/faces style)
	  (indent-bars--create-faces style depth)))
  (aref (ibs/faces style) (1- depth)))

;;;;; No stipple characters (e.g. terminal)
(defun indent-bars--no-stipple-char (style depth)
  "Return the no-stipple bar character for DEPTH in STYLE."
  (when (> depth (length (ibs/no-stipple-chars style)))
    (setf (ibs/no-stipple-chars style)
	  (indent-bars--create-no-stipple-chars style depth)))
  (aref (ibs/no-stipple-chars style) (1- depth)))

(defun indent-bars--create-no-stipple-chars (style num)
  "Setup bar characters for bar faces up to depth NUM in STYLE.
Used when not using stipple display (on terminal, or by request;
see `indent-bars-prefer-character')."
  (vconcat
   (nreverse
    (cl-loop
     with chars = (ibs/no-stipple-chars style)
     with l = (length chars)
     for d from num downto 1
     collect
     (or (and (< d l) (aref chars (1- d)))
	 (propertize (string indent-bars-no-stipple-char)
		     'face (indent-bars--face style d)))))))

;;;;; Alternate Style Support
(defmacro indent-bars--alt-custom
    (alt opt alt-description std-val &optional add-inherit no-inherit &rest r)
  "Define a custom ALT variable for option OPT.
The new custom options default value is set to STD-VAL.  This
creates a new variable indent-bars-alt-opt, based on
indent-bars-opt (referred to as the parent variable).
ALT-DESCRIPTION will be used to identify the alternate variable
in the customize interface.

If ADD-INHERIT is non-nil, expand the type to a cons:

  (inherit . type)

where INHERIT is either `inherit' or `no-inherit', depending
on the value of NO-INHERIT.

Additional `defcustom` keyword arguments can be given as R."
  (require 'cus-edit)
  (let* ((optname (symbol-name opt))
	 (indent-bars--custom-set-inhibit t)
	 (group (intern (concat "indent-bars-" alt "-style")))
	 (symname (concat "indent-bars-" optname))
	 (sym (intern (concat "indent-bars-" optname)))
	 (tsym (intern (concat "indent-bars-" alt "-" optname)))
	 (type (custom-variable-type sym))
	 (choice (cond ((eq (car type) 'choice) type)
		       ((eq (car type) 'list)
			(seq-find
			 (lambda (el) (and (consp el) (eq (car el) 'choice)))
			 type))
		       (t (setq type `(choice ,type))))))
    ;; Add an unspecified choice
    (when-let* ((tag-pos (member :tag choice)))
      (setq choice (cdr tag-pos)))	;after tag
    (setcdr choice
	    (push
	     `(const :tag ,(concat "No-value (use parent " optname ")") unspecified)
	     (cdr choice)))

    ;; Add leading inherit flag, if needed
    (when (or no-inherit add-inherit)
      (setq type
	    `(cons :tag ,(concat alt-description " Style")
		   (choice :tag
			   ,(concat "Inherit missing data from `indent-bars-"
				    optname "'")
			   (const :tag "Do not inherit" no-inherit)
			   (const :tag "Inherit" inherit))
		   ,type)
	    std-val `( ,(if no-inherit 'no-inherit 'inherit) . ,std-val )))
    `(defcustom ,tsym ',std-val
       ,(concat "Alternate " alt-description " version of `" symname "'.")
       :type ',type
       :link '(variable-link ,sym)
       :set #'indent-bars--custom-set
       :initialize #'custom-initialize-default
       :group ',group
       ,@r)))

(defsubst indent-bars--alt (name alt)
  "Find the symbol value of NAME, with alternate style ALT.
NAME is a string, and ALT and be a string or nil."
  (intern (format "indent-bars%s-%s"
		  (if alt (concat "-" alt) "") name)))

(defun indent-bars--style1 (style name)
  "Return the value of style variable NAME for STYLE.
Considers ([no-]inherit . rest) inheritance."
  (let* ((tag (ibs/tag style))
	 (sym (indent-bars--alt name tag))
	 (val (symbol-value sym))
	 (inhrt t))  ; inherit by default
    (when tag
      ;; Check for the ([no-]inherit . actual-val) form
      (when (and (consp val) (memq (car val) '(inherit no-inherit)))
	(setq inhrt (and (car val) (not (eq (car val) 'no-inherit)))
	      val (cdr val)))
      (when inhrt
	(setq val (indent-bars--custom-inherit
		   (symbol-value (indent-bars--alt name nil)) val))))
    val))

(defun indent-bars--style (style name)
  "Return the value of style variable NAME for STYLE.
Determines variables to use based on the style tag.  For style
variable values of the form (`inherit'|`no-inherit' . plist),
inheritance of the plist is handled.  If style is the symbol
`any', return the first non-nil value for all styles in
`indent-bars--styles'."
  (if (eq style 'any)
      (cl-some (lambda (s) (indent-bars--style1 s name))
	       indent-bars--styles)
    (indent-bars--style1 style name)))

(defun indent-bars--custom-inherit (old new)
  "Inherit the values of NEW and OLD, which can be values or lists.
NEW and OLD must have the same form, composed of atoms
and (optionally) a final plist.  The symbol `unspecified' in
NEW indicates that that value should be replaced by the
corresponding value in OLD.  Any trailing PLIST in NEW and OLD
will be merged (with NEW taking precedence).  The merged value is
returned."
  (cond
   ((and old (eq new 'unspecified))
    old) 	; fully inherited

   ((and (atom old) (atom new))
    (if (eq new 'unspecified) old new))

   ((and (consp old) (consp new))
    (let* ((old (copy-sequence old)) 	; avoid editing old
	   (n new) (o old) last-o)
      (while (and n o)
	(if (and (plistp n) (plistp o) (keywordp (car o)))
	    (let ((m (map-merge 'plist o n)))
	      (if last-o (setcdr last-o m) (setq old m))
	      (setq o nil))		; signify list complete
	  (unless (eq (car n) 'unspecified)
	    (setcar o (car n))))
	(setq last-o o n (cdr n) o (cdr o)))
      old))

   (t new)))

(defun indent-bars-reset-styles (&rest _r)
  "Reset all styles' colors and faces.
Useful for calling after theme changes."
  (interactive)
  (unless (equal (terminal-name) "initial_terminal")
    (mapc #'indent-bars--initialize-style indent-bars--styles)))

(defun indent-bars--initialize-style (style)
  "Initialize STYLE."
  ;; Colors
  (setf (ibs/main-color style)
	(indent-bars--main-color style)
	(ibs/depth-palette style)
	(indent-bars--depth-palette style)
	(ibs/current-depth-palette style)
	(indent-bars--current-depth-palette style)
	(ibs/faces style) (indent-bars--create-faces style 7)
	(ibs/no-stipple-chars style) (indent-bars--create-no-stipple-chars style 7))

  ;; Base stipple face
  (let ((width (indent-bars--window-font-space-width)))
    (face-spec-set
     (ibs/stipple-face style)
     (indent-bars--stipple-face-spec width (frame-char-height)
      (indent-bars--stipple-rot (selected-window) width)
      style)))

  ;; Current depth highlight faces/stipple
  (setf (ibs/current-bg-color style)
	(indent-bars--current-bg-color style))
  (when-let* ((stipple (indent-bars--current-depth-stipple nil nil nil style)))
    (setf (ibs/current-stipple-face style)
	  (indent-bars--tag "indent-bars%s-current-face" style))
    (face-spec-set (ibs/current-stipple-face style) nil)))

;;;; Indentation and Drawing
(defvar-local indent-bars-spacing nil)
(defvar-local indent-bars--offset nil)
(defvar-local indent-bars--no-stipple nil)

(defsubst indent-bars--depth (len)
  "Number of possible bars for initial blank string of length LEN.
Note that the first bar is expected at `indent-bars-starting-column'."
  (if (> len indent-bars--offset)
      (1+ (/ (- len indent-bars--offset 1) indent-bars-spacing))
    0))

(defun indent-bars--context-indentation ()
  "Return the maximum `current-indentation' around current line.
Skips any fully blank lines."
  (let ((prior-indent
	 (save-excursion
	   (beginning-of-line)
	   (skip-chars-backward "[:space:]\n")
	   (current-indentation))))
    (save-excursion
      (forward-line 1)
      (skip-chars-forward "[:space:]\n")
      (max (current-indentation) prior-indent))))

(defvar-local indent-bars--update-depth-function nil)
(defvar-local indent-bars--ppss nil)
(defun indent-bars--current-indentation-depth (&optional on-bar)
  "Calculate current indentation depth.
Depth is 1-based (independent of the value of
`indent-bars-starting-column'), with a depth of 1 corresponding
to the outermost bar, and a depth of 0 indicating there is no
valid current depth.

If ON-BAR is nil, return the depth of the last visible bar on the
current line.  If ON-BAR is non-nil and content begins at a
column where a bar would otherwise have fallen, report the depth
of that (undrawn) bar.  If ON-BAR is the symbol `context', and
the first non-blank line immediately above or below the current
line is not at a deeper indentation level (by at least one bar
spacing), disable on-bar and use the last-visible-bar depth for
that line instead.

If `indent-bars-no-descend-string' is non-nil and point at line
beginning is inside a string, do not add bars deeper than one
more than the string's start.  If it is `all', do not add any
bars at all.  If `indent-bars-no-descend-lists' is non-nil,
perform the same check for lists.

If `indent-bars--update-depth-function' is non-nil, it will be
called with the indentation depth (prior to the ON-BAR check),
and can return an updated depth."
  (let* ((c (current-indentation))
	 (d (indent-bars--depth c)) 	;last visible bar
	 ppss-ind)
    (when indent-bars--ppss
      (save-excursion
	(forward-line 0)
	(let* ((ppss (syntax-ppss))	; moves point!
	       (string-start (and indent-bars-no-descend-string (nth 8 ppss)))
	       (list-start (when-let*
			       ((ndl indent-bars-no-descend-lists)
				(open (nth 1 ppss))
				((or (not (consp ndl)) (memq (char-after open) ndl))))
			     open)))
	  (if (and string-start (eq indent-bars-no-descend-string 'all))
	      (setq d 0 c 0) ; always inhibit inside multiline strings
	    (when (setq ppss-ind (if (and string-start list-start)
				     (max string-start list-start)
				   (or string-start list-start)))
	      (goto-char ppss-ind)
	      (let* ((cnew (current-indentation))
		     (dnew (1+ (indent-bars--depth cnew))))
		(when (< dnew d) (setq d dnew c cnew))))))))
    (when (and indent-bars--update-depth-function (not ppss-ind))
      (setq d (funcall indent-bars--update-depth-function d)))
    (when (and (eq on-bar 'context)
	       (< (indent-bars--context-indentation) (+ c indent-bars-spacing)))
      (setq on-bar nil))
    (if (and on-bar (= c (+ indent-bars--offset (* d indent-bars-spacing))))
	(1+ d) d)))

(defun indent-bars--blank-string (style off nbars bar-from
					&optional width
					switch-after style2)
  "Return a blank string with bars displayed, using style STYLE.
OFF is the character offset within the string to draw the first
bar, NBARS is the desired number of bars to add, and BAR-FROM is
the starting index of the first bar (>=1).  WIDTH is the total
string width to return, right padding with space if needed.

If SWITCH-AFTER is supplied and is an integer, switch from STYLE
to STYLE2 after drawing that many bars.  If it is t, use
STYLE2 for all bars.

Bars are displayed using stipple properties or characters; see
`indent-bars-prefer-character'."
  (concat (make-string off ?\s)
	  (string-join
	   (cl-loop
	    for i from 0 to (1- nbars)
	    for depth = (+ bar-from i)
	    for sty = (if switch-after
			  (if (or (eq switch-after t)
				  (>= i switch-after))
			      style2
			    style)
			style)
	    collect (if indent-bars--no-stipple
			(indent-bars--no-stipple-char sty depth)
		      (propertize " " 'face (indent-bars--face sty depth))))
	   (make-string (1- indent-bars-spacing) ?\s))
	  (if width
	      (make-string (- width
			      (+ off nbars (* (1- nbars) (1- indent-bars-spacing))))
			   ?\s))))

(defun indent-bars--tab-display (style p off bar-from max &rest r)
  "Display up to MAX bars on the tab at P, offsetting them by OFF.
Bars are spaced by `indent-bars-spacing' and displayed with style
STYLE.  BAR-FROM is the bar number for the first bar.  Other
arguments R are passed to `indent-bars--blank-string'.  Returns
the number of bars actually displayed."
  (let* ((nb (min max (1+ (/ (- tab-width off 1) indent-bars-spacing))))
	 (str (apply #'indent-bars--blank-string style off nb
		     bar-from tab-width r)))
    (put-text-property p (+ p 1) 'indent-bars-display str)
    nb))

(defun indent-bars--draw-line (style nbars start end &optional
				     invent switch-after style2)
  "Draw NBARS bars on the line between positions START and END.
Bars are drawn in style STYLE, `indent-bars-style' by default.
START is assumed to be on a line beginning position.  Drawing
starts at a column determined by `indent-bars-starting-column'.
Tabs at the line beginning have appropriate display properties
applied if `indent-tabs-mode' is enabled.

If SWITCH-AFTER is an integer, switch from STYLE to STYLE2
after drawing that many bars.  If it is t, use STYLE2
exclusively.

If INVENT is non-nil and the line's length is insufficient to
display all NBARS bars (whether by replacing tabs or adding
properties to existing non-tab whitespace), bars will be
\"invented\".  That is, the line's final newline, which is (only
in this case) expected to be located at END, will have its
display properties set to fill out the remaining bars, if any are
needed."
  (let* ((tabs (and indent-tabs-mode
		    (save-excursion
		      (goto-char start) (looking-at "^\t+"))
		    (- (match-end 0) (match-beginning 0))))
	 (vp indent-bars--offset)
	 (style (or style indent-bars-style))
	 (bar 1) prop fun tnum bars-drawn)
    (when tabs				; deal with initial tabs
      (while (and (<= bar nbars) (< (setq tnum (/ vp tab-width)) tabs))
	(setq bars-drawn
	      (indent-bars--tab-display style (+ start tnum) (mod vp tab-width)
					bar (- nbars bar -1)
					switch-after style2))
	(when (integerp switch-after)
	  (cl-decf switch-after bars-drawn)
	  (if (<= switch-after 0) (setq switch-after t))) ; switch the rest
	(cl-incf bar bars-drawn)
	(cl-incf vp (* bars-drawn indent-bars-spacing)))
      (cl-incf start (+ (/ vp tab-width) (mod vp tab-width))))
    (when (<= bar nbars)		; still bars to show
      (if indent-bars--no-stipple
	  (setq prop 'indent-bars-display fun #'indent-bars--no-stipple-char)
	(setq prop 'face fun #'indent-bars--face))
      (let ((pos (if tabs start (+ start indent-bars--offset))))
	(while (and (<= bar nbars) (< pos end))
	  (put-text-property
	   pos (1+ pos)
	   prop (funcall fun
			 (cond ((integerp switch-after)
				(prog1
				    (if (> switch-after 0) style style2)
				  (cl-decf switch-after)
				  (when (<= switch-after 0)
				    (setq switch-after t))))
			       ((eq switch-after t) style2)
			       (t style))
			 bar))
	  (cl-incf bar)
	  (cl-incf pos indent-bars-spacing))
	;; STILL bars to show: invent them (if requested)
	(when (and invent (<= bar nbars))
	  (add-text-properties
	   end (1+ end) ; atop the final newline
	   `(indent-bars-display
	     ,(concat (indent-bars--blank-string
		       style (- pos end) (- nbars bar -1) bar nil
		       switch-after style2)
		      "\n")
	    rear-nonsticky t)))))))

(defsubst indent-bars--context-bars (end &optional min)
  "Maximum number of bars at point and END.
If MIN is non-nil, return the minimum number of bars instead.
Moves point."
  (funcall (if min #'min #'max)
	   (indent-bars--current-indentation-depth)
	   (progn
	     (goto-char (1+ end))	; end is always eol
	     (indent-bars--current-indentation-depth))))

(defun indent-bars--display (beg end &optional style switch-after style2)
  "Draw indentation bars from BEG..END, based on line contents.
BEG and END should be on the same line.  STYLE, SWITCH-AFTER and
STYLE2 are as in `indent-bars--draw-line'.  If STYLE is not
passed, uses `indent-bars-style' for drawing."
  (let ((n (save-excursion
	     (goto-char beg)
	     (indent-bars--current-indentation-depth))))
    (and (> n 0) (indent-bars--draw-line style n beg end nil
					 switch-after style2))))

(defun indent-bars--display-blank-lines (beg end &optional style switch-after style2)
  "Display appropriate bars over the blank-only lines from BEG..END.
Only called if `indent-bars-display-on-blank-lines' is non-nil.
To be called on complete multi-line blank line regions.

It is ambigious how many bars to draw on blank lines, so this
uses the maximum depth of the surrounding line indentation, above
and below, unless `indent-bars-display-on-blank-lines' is set to
`least', in which case the minimum bar depth of such lines is
used instead.

Drawing uses `indent-bars--draw-line'.  STYLE, SWITCH-AFTER and
STYLE2 are as in `indent-bars--draw-line'.

Note: blank lines at the very beginning or end of the buffer are
not indicated, even if they otherwise would be."
  (let ((pm (point-max))
	(lst (eq indent-bars-display-on-blank-lines 'least))
	ctxbars)
    (save-excursion
      (goto-char (1- beg))
      (beginning-of-line 1)
      (when (> (setq ctxbars (indent-bars--context-bars end lst)) 0)
	(goto-char beg)
	(while (< (point) end) ;note: end extends 1 char beyond blank line range
	  (let* ((bp (line-beginning-position))
		 (ep (line-end-position)))
	    (unless (= ep pm)
	      (indent-bars--draw-line style ctxbars bp ep 'invent
				      switch-after style2))
	    (beginning-of-line 2)))))
    nil))

;;;; jit-lock support
(defvar-local indent-bars--orig-fontify-region nil)
(defun indent-bars--extend-region (start end)
  "Extend the region START..END.
If `indent-bars-display-on-blank-lines' is non-nil, this extends
it to include complete contiguous stretches of blank lines and
always starts and ends on the beginning of a line."
  (save-excursion
    (let ((chars " \t\n"))
      (goto-char start)
      (forward-line 0)
      (when (and indent-bars-display-on-blank-lines
		 (< (skip-chars-backward chars) 0))
	(unless (bolp) (forward-line 1)))
      (when (< (point) start) (setq start (point)))
      (goto-char end)
      (unless (bolp) (forward-line 1))
      (when (and indent-bars-display-on-blank-lines
		 (> (skip-chars-forward chars) 0))
	(unless (bolp) (forward-line 0)))
      (when (> (point) end) (setq end (point)))))
  (cons start end))

(defvar-local indent-bars--display-function
    #'indent-bars--display)
(defvar-local indent-bars--display-blank-lines-function
    #'indent-bars--display-blank-lines)

(defun indent-bars--draw-all-bars-between (start end)
  "Search for and draw all bars between START and END.
The beginning of line at START is used to locate real and (if
configured) blank-line bars, which are drawn in the appropriate
style.  This is basically a very tiny, bar-only version of what
`font-lock-fontify-region-keywords' does."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward indent-bars--regexp end t))
      (if (match-beginning 2)
	  (funcall indent-bars--display-blank-lines-function
		   (match-beginning 2) (match-end 2))
	(funcall indent-bars--display-function
		 (match-beginning 1) (match-end 1))))))

(defvar-local indent-bars--font-lock-inhibit nil)
(defun indent-bars--fontify (beg end verbose)
  "Add indent-bars from BEG to END after calling font-lock there.
The VERBOSE argument is provided by font-lock.  If
`indent-bars--font-lock-inhibit' is a function, call it with BEG
and END.  If it returns non-nil, skip font-lock."
  ;; Fontify with font-lock, unless inhibited (clears + sets 'face)
  (unless (and indent-bars--font-lock-inhibit
	       (funcall indent-bars--font-lock-inhibit beg end))
    (pcase (funcall indent-bars--orig-fontify-region beg end verbose)
      (`(jit-lock-bounds ,beg1 . ,end1) (setq beg beg1 end end1))))
  ;; Always draw bars (which may overwrite 'face in a few places)
  (pcase-let ((`(,beg . ,end) (indent-bars--extend-region beg end)))
    (with-silent-modifications
      ;; We remove our display alias here instead of by adding it to
      ;; `font-lock-extra-managed-keywords' because font-lock will not
      ;; always be run.
      (remove-text-properties beg end '(indent-bars-display nil))
      (indent-bars--draw-all-bars-between beg end))
    `(jit-lock-bounds ,beg . ,end)))

;;;; Current indentation depth highlighting
(defvar-local indent-bars--current-depth 0)
(defvar-local indent-bars--remaps nil
  "An alist of active face-remap cookies for faces.
Keyed by style tag (nil for the main style).  These remaps are
used to change the current depth highlight (aside from stipple changes).")

(defun indent-bars--current-bg-color (style)
  "Return the current bar background color appropriate for STYLE."
  (when-let* ((hcd (indent-bars--style style "highlight-current-depth")))
    (plist-get hcd :background)))

(defun indent-bars--current-depth-stipple (&optional w h rot style)
  "Return the current depth stipple highlight (if any) for STYLE.
One of the keywords :width, :pad, :pattern, or :zigzag must be
set in `indent-bars-highlight-current-depth' config.  W, H, and
ROT are as in `indent-bars--stipple', and have similar default values."
  (cl-destructuring-bind (&key width pad pattern zigzag &allow-other-keys)
      (indent-bars--style style "highlight-current-depth")
    (when (or width pad pattern zigzag)
      (let* ((w (or w (indent-bars--window-font-space-width)))
	     (h (or h (window-font-height)))
	     (rot (or rot (indent-bars--stipple-rot nil w))))
	(indent-bars--stipple w h rot style width pad pattern zigzag)))))

(defun indent-bars--set-current-depth-highlight (depth)
  "Set current highlight to DEPTH.
Works by remapping the appropriate indent-bars[-tag]-N face for
all styles in the `indent-bars--styles' list.  DEPTH should be
greater than or equal to zero (zero meaning: no highlight)."
  (dolist (s indent-bars--styles)
    (when-let* ((c (alist-get (ibs/tag s) indent-bars--remaps))) ; out with the old
      (face-remap-remove-relative c))
    (when (> depth 0)
      (let* ((face (indent-bars--face s depth))
	     (hl-col (and (ibs/current-depth-palette s)
			  (indent-bars--get-color s depth 'highlight)))
	     (hl-bg (ibs/current-bg-color s)))
	(when (or hl-col hl-bg (ibs/current-stipple-face s))
	  (setf (alist-get (ibs/tag s) indent-bars--remaps)
		(face-remap-add-relative
		 face
		 `(,@(when hl-col `(:foreground ,hl-col))
		   ,@(when hl-bg `(:background ,hl-bg)))
		 (ibs/current-stipple-face s))))))))

(defun indent-bars--compute-and-highlight-current-depth (&optional force)
  "Compute and highlight current bar depth in the current buffer.
If FORCE is non-nil, update depth even if it has not changed."
  (let ((depth (indent-bars--current-indentation-depth
		indent-bars-highlight-selection-method)))
    (when (and depth (or force (not (= depth indent-bars--current-depth))))
      (setq indent-bars--current-depth depth)
      (indent-bars--set-current-depth-highlight depth))))

(defvar-local indent-bars--highlight-timer nil)
(defun indent-bars--update-current-depth-highlight-in-buffer (buf)
  "Highlight bar at DEPTH in buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq indent-bars--highlight-timer nil)
      (indent-bars--compute-and-highlight-current-depth))))

(defun indent-bars--update-current-depth-highlight ()
  "Refresh current indentation depth highlight.
Rate limit set by `indent-bars-depth-update-delay', if non-zero."
  (when indent-bars-mode
    (if (zerop indent-bars-depth-update-delay)
	(indent-bars--compute-and-highlight-current-depth)
      (unless indent-bars--highlight-timer
	(setq indent-bars--highlight-timer
	      (run-with-idle-timer
	       indent-bars-depth-update-delay nil
	       #'indent-bars--update-current-depth-highlight-in-buffer
	       (current-buffer)))))))

;;;; Stipple Display
(defsubst indent-bars--block (n)
  "Create a block of N low-order 1 bits."
  (- (ash 1 n) 1))

(defun indent-bars--stipple-rot (win w)
  "Return the stipple rotation for window WIN and pattern width W.
WIN defaults to the selected window if nil."
  (mod (car (window-edges win t nil t)) w))

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
;; outside the body (in literally the first frame pixel, typically
;; within the fringe), you must consider the shift between the first
;; pixel of a window character and the first pixel of the repeating
;; stipple block at that pixel position or above:
;;
;;     |<-frame edge |<---buffer/window edge
;;     |<--w-->|<--w-->|<--w-->|     w = pattern width
;;     | marg+fringe |<-chr->|     chr = character width = w
;;             |<-g->|               g = gutter offset of chr start, g<w
;;
;; Or, when the character width exceeds the margin/fringe offset:
;;
;;     |<-frame edge |<---buffer/window edge
;;     |<--------w-------->|<---------w-------->|
;;     | marg+fringe |<-------chr------->|
;;     |<-----g----->|
;;
;; So g = (mod marg+fringe w).
;;
;; When the block/zigzag/whatever stipple pattern is made, to align
;; with characters, it must be shifted up (= right) by g bits, with
;; carry over (wrap) around w=space-width bits (i.e the width of the
;; bitmap in pixels).  The byte/bit pattern is first-lowest-leftmost.
;;
;; Note that different windows may have different g values
;; (e.g. left/right), which means the same bitmap cannot work for the
;; buffer in both windows.  In practice that means that all stipple
;; face attributes must be set via filtered face remaps, with the
;; filter set to match the pattern size (width and height) as well as
;; gutter offset "rot" value in that window, which we combine into a
;; single integer.
;;
;; Note: a bug in Emacs <29 means `face-remapping-alist' is
;; unintentionally shared between indirect and master buffers.  Fixed
;; in Emacs 29.

(defun indent-bars--stipple (w h rot &optional style
			       width-frac pad-frac pattern zigzag)
  "Calculate stipple bitmap pattern for char width W and height H.
ROT is the number of bits to rotate the pattern around to the
right (with wrap).

Uses configuration variables `indent-bars-width-frac',
`indent-bars-pad-frac', `indent-bars-pattern', and
`indent-bars-zigzag', unless PAD-FRAC, WIDTH-FRAC, PATTERN,
and/or ZIGZAG are set (the latter overriding the config
variables, which see).  If STYLE is set, use config variables
appropriate for that style."
  (unless (or (not (display-graphic-p)) indent-bars-prefer-character)
    (let* ((rowbytes (/ (+ w 7) 8))
	   (pattern (or pattern (indent-bars--style style "pattern")))
	   (pat (if (< h (length pattern)) (substring pattern 0 h) pattern))
	   (plen (max (length pat) 1))
	   (chunk (/ (float h) plen))
	   (small (floor chunk))
	   (large (ceiling chunk))
	   (pad-frac (or pad-frac (indent-bars--style style "pad-frac")))
	   (pad (round (* w pad-frac)))
	   (zigzag (or zigzag (indent-bars--style style "zigzag")))
	   (zz (if zigzag (round (* w zigzag)) 0))
	   (zeroes (make-string rowbytes ?\0))
	   (width-frac (or width-frac (indent-bars--style style "width-frac")))
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

(defsubst indent-bars--whr (w h r)
  "Encoded value for caching remaps.
Combines font width W and height H, as well as window edge pixel
offset R."
  (+ (ash w 16) (ash h 8) r))

(defvar-local indent-bars--stipple-remaps nil
  "A hash table of active stipple face-remap cookies.
The hash table is keyed by the `whr', an integer composing font
width, height, and window starting offset (see
`indent-bars--whr').  Elements of the hash table are plists with
the following keys:

  (:main[-styletag] ... :current[-styletag] ...)

These are used to update relevant stipples for main and current
stipple faces for the various styles as character size and/or
window pixel start (pattern rotation \"rot\") change, due to
window layout or font size changes.  Note that all stipple
attributes are set via filtered face remaps.")

(defsubst indent-bars--remove-plist-remaps (pl)
  "Remove remaps key values from plist PL."
  (cl-loop for (_k r) on pl by #'cddr do
	   (face-remap-remove-relative r)))

(defun indent-bars--create-stipple-remaps (w h rot)
  "Create and store stipple remaps for the given font size and pixel start.
W is the space font width in pixels, H is the corresponding height, and
ROT is the number of bits to rotate the pattern start.  An entry is
created for each active style, for both :main[-styletag] and
:current[-styletag] highlight contexts."
  (let* ((whr (indent-bars--whr w h rot))
	 (filter `(:window indent-bars-whr ,whr))
	 remap)
    (dolist (s indent-bars--styles)
      ;; Main stipple face
      (when (ibs/stipple-face s)
	(setq remap
	      (plist-put
	       remap (indent-bars--tag ":main%s" s)
	       (face-remap-add-relative
		(ibs/stipple-face s)
		`( :filtered ,filter
		   (:stipple ,(indent-bars--stipple w h rot s)))))))
      ;; Currently highlighted stipple face
      (when (ibs/current-stipple-face s)
	(setq remap
	      (plist-put
	       remap (indent-bars--tag ":current%s" s)
	       (face-remap-add-relative
		(ibs/current-stipple-face s)
		`( :filtered ,filter
		   (:stipple ,(indent-bars--current-depth-stipple w h rot s))))))
	(indent-bars--compute-and-highlight-current-depth 'force)))
    (puthash whr remap indent-bars--stipple-remaps)))

;;;; Window change and cleanup
(defvar-local indent-bars--needs-cleanup nil)
(defun indent-bars--schedule-remap-cleanup ()
  "Schedule cleanup of stipple remaps, unless already scheduled."
  (unless indent-bars--needs-cleanup
    (setq indent-bars--needs-cleanup t)
    (run-with-idle-timer
     1 nil
     (apply-partially #'indent-bars--cleanup-stipple-remaps (current-buffer)))))

(defun indent-bars--update-all-stipples ()
  "Update all stipples for current buffer."
  (dolist (w (get-buffer-window-list nil nil t))
    (indent-bars--window-change w)))

(defun indent-bars--window-change (&optional win)
  "Update the stipples for buffer in window WIN.
WIN defaults to the selected window.  To be set as a local
`window-state-change-functions' hook."
  (unless win (setq win (selected-window)))
  (let* ((w (indent-bars--window-font-space-width win))
	 (h (window-font-height win))
	 (rot (indent-bars--stipple-rot win w))
	 (whr (indent-bars--whr w h rot))
	 (cur-whr (window-parameter win 'indent-bars-whr)))
    (unless (eq cur-whr whr)
      (set-window-parameter win 'indent-bars-whr whr))
    (when-let* ((buf (window-buffer win))
		(ht (buffer-local-value 'indent-bars--stipple-remaps buf))
		((null (gethash whr ht))))
      (with-current-buffer buf ; we may be called from an arbitrary buffer
	(indent-bars--create-stipple-remaps w h rot)
	(indent-bars--schedule-remap-cleanup)))))

(defun indent-bars--cleanup-stipple-remaps (buf)
  "Clean up unused stipple face remaps for buffer BUF."
  (when-let* (((buffer-live-p buf))
	      (wins (get-buffer-window-list buf nil t))
	      (whrs (cl-loop for win in wins
			     collect (window-parameter win 'indent-bars-whr)))
	      (rmp-hsh (buffer-local-value 'indent-bars--stipple-remaps buf))
	      (rkeys (hash-table-keys rmp-hsh))
	      (unneeded (seq-difference rkeys whrs)))
    (with-current-buffer buf ;N.B. face-remapping-alist is buffer-local
      (dolist (rk unneeded)
	(indent-bars--remove-plist-remaps (gethash rk rmp-hsh))
	(remhash rk rmp-hsh))
      (setq indent-bars--needs-cleanup nil))))

;;;; Setup and mode
(defun indent-bars--guess-spacing ()
  "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
  (cond
   (indent-bars-spacing-override)
   ((and (derived-mode-p 'ada-mode) (boundp 'ada-indent))
    ada-indent)
   ((and (derived-mode-p 'ada-ts-mode) (boundp 'ada-ts-mode-indent-offset))
    ada-ts-mode-indent-offset)
   ((and (derived-mode-p 'gpr-mode) (boundp 'gpr-indent))
    gpr-indent)
   ((and (derived-mode-p 'gpr-ts-mode) (boundp 'gpr-ts-mode-indent-offset))
    gpr-ts-mode-indent-offset)
   ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
    py-indent-offset)
   ((and (derived-mode-p 'python-mode 'python-base-mode) (boundp 'python-indent-offset))
    python-indent-offset)
   ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
    ruby-indent-level)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
    scala-indent:step)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
    scala-mode-indent:step)
   ((and (derived-mode-p 'scala-ts-mode) (boundp 'scala-ts-indent-offset))
    scala-ts-indent-offset)
   ((and (derived-mode-p 'rust-ts-mode) (boundp 'rust-ts-mode-indent-offset))
    rust-ts-mode-indent-offset)
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
   ((and (derived-mode-p 'typescript-ts-mode) (boundp 'typescript-ts-mode-indent-offset))
    typescript-ts-mode-indent-offset)
   ((and (derived-mode-p 'sws-mode) (boundp 'sws-tab-width))
    sws-tab-width)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
    web-mode-markup-indent-offset)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
    web-mode-html-offset)
   ((and (local-variable-p 'c-basic-offset) (numberp c-basic-offset))
    c-basic-offset)
   ((and (local-variable-p 'c-ts-common-indent-offset)
	 (symbolp c-ts-common-indent-offset)
	 (numberp (symbol-value c-ts-common-indent-offset)))
    (symbol-value c-ts-common-indent-offset))
   ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
    yaml-indent-offset)
   ((and (derived-mode-p 'yaml-pro-mode) (boundp 'yaml-pro-indent))
    yaml-pro-indent)
   ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
    elixir-smie-indent-basic)
   ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
    lisp-body-indent)
   ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
    cobol-tab-width)
   ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
    tab-width)
   ((derived-mode-p 'nix-mode)
    tab-width)
   ((derived-mode-p 'makefile-mode)
    tab-width)
   ((and (derived-mode-p 'nix-ts-mode) (boundp 'nix-ts-mode-indent-offset))
    nix-ts-mode-indent-offset)
   ((and (derived-mode-p 'json-ts-mode) (boundp 'json-ts-mode-indent-offset))
    json-ts-mode-indent-offset)
   ((and (derived-mode-p 'json-mode) (boundp 'js-indent-level))
    js-indent-level)
   ((and (derived-mode-p 'sh-base-mode) (boundp 'sh-basic-offset))
    sh-basic-offset)
   ((and (derived-mode-p 'java-ts-mode) (boundp 'java-ts-mode-indent-offset))
    java-ts-mode-indent-offset)
   ((and (derived-mode-p 'tcl-mode) (boundp 'tcl-indent-level))
    tcl-indent-level)
   ((and (derived-mode-p 'haml-mode) (boundp 'haml-indent-offset))
    haml-indent-offset)
   ((and (boundp 'standard-indent) standard-indent))
   (t 4))) 				; backup

(defun indent-bars--setup-font-lock ()
  "Wrap the `font-lock-fontify-region-function' to provide bars."
  ;; Setup to wrap font-lock-fontify-region
  (unless (eq font-lock-fontify-region-function #'indent-bars--fontify)
    (setq indent-bars--orig-fontify-region font-lock-fontify-region-function)
    (setq-local font-lock-fontify-region-function #'indent-bars--fontify))
  (setq indent-bars--regexp
	(rx-to-string
	 `(seq bol (or (and
			;; group 1: basic blank indent detection
			(group
			 ,(if (not indent-tabs-mode)
			      `(>= ,(1+ indent-bars--offset) ?\s)
			    '(+ (any ?\t ?\s))))
			(not (any ?\t ?\s ?\n)))
		       ;; group 2: multi-line blank regions
		       ,@(if indent-bars-display-on-blank-lines
			     '((group (* (or ?\s ?\t ?\n)) ?\n)))))))
  (unless font-lock-defaults (setq font-lock-defaults '(nil t)))
  (unless font-lock-mode (font-lock-mode 1)))

(declare-function indent-bars--ts-mode "indent-bars-ts")
(defun indent-bars-setup ()
  "Setup all face, color, bar size, and indentation info for the current buffer."
  ;; Spacing
  (setq indent-bars-spacing (indent-bars--guess-spacing)
	indent-bars--offset (or indent-bars-starting-column indent-bars-spacing))

  ;; No Stipple (e.g. terminal)
  (setq indent-bars--no-stipple
	(or (not (display-graphic-p)) indent-bars-prefer-character))

  ;; Style (color + stipple)
  (unless (and indent-bars-style indent-bars--styles)
    (indent-bars--initialize-style
     (setq indent-bars-style (indent-bars--new-style))))

  ;; Window state: selection/size
  (unless indent-bars--no-stipple
    (add-hook 'window-state-change-functions
	      #'indent-bars--window-change nil t))

  ;; PPSS
  (setq indent-bars--ppss
	(or indent-bars-no-descend-string indent-bars-no-descend-lists))

  ;; Treesitter
  (if indent-bars-treesit-support (indent-bars--ts-mode 1)) ; autoloads

  ;; Remap/Resize
  (unless indent-bars--no-stipple
    (setq indent-bars--stipple-remaps (make-hash-table))
    (add-hook 'text-scale-mode-hook #'indent-bars--update-all-stipples nil t)
    (indent-bars--update-all-stipples)) ; sets all remaps for current buffer

  ;; Current depth Highlighting
  (when (indent-bars--style 'any "highlight-current-depth")
    (add-hook 'post-command-hook
	      #'indent-bars--update-current-depth-highlight nil t)
    (setq indent-bars--current-depth 0)
    (indent-bars--update-current-depth-highlight))

  ;;Jit/Font-lock
  (cl-pushnew 'indent-bars-display (alist-get 'display char-property-alias-alist))
  (indent-bars--setup-font-lock)
  (jit-lock-refontify))

(defvar indent-bars--teardown-functions nil)
(defun indent-bars-teardown ()
  "Tears down indent-bars."
  ;; Remove current highlight remaps
  (dolist (s indent-bars--styles)
    (face-remap-remove-relative
     (alist-get (ibs/tag s) indent-bars--remaps)))
  (when indent-bars--highlight-timer
    (cancel-timer indent-bars--highlight-timer)
    (setq indent-bars--highlight-timer nil))

  ;; Remove stipple remaps and window parameters
  (unless indent-bars--no-stipple
    (when indent-bars--stipple-remaps
      (maphash (lambda (_k pl)
		 (indent-bars--remove-plist-remaps pl))
	       indent-bars--stipple-remaps)
      (setq indent-bars--stipple-remaps nil)))

  (when (and indent-bars--orig-fontify-region
	     (eq font-lock-fontify-region-function #'indent-bars--fontify))
    (setq font-lock-fontify-region-function indent-bars--orig-fontify-region
	  indent-bars--orig-fontify-region nil))
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(indent-bars-display nil)))
  (jit-lock-refontify)

  (setq indent-bars--current-depth 0)
  (remove-hook 'text-scale-mode-hook #'indent-bars--update-all-stipples t)
  (remove-hook 'post-command-hook #'indent-bars--update-current-depth-highlight t)
  (remove-hook 'window-state-change-functions
	       #'indent-bars--window-change t)
  (run-hooks 'indent-bars--teardown-functions))

(defun indent-bars-reset (&rest _r)
  "Reset indent-bars config."
  (interactive)
  (indent-bars-teardown)
  (setq indent-bars-style nil indent-bars--styles nil)
  (indent-bars-setup))

(defun indent-bars-setup-and-remove (frame)
  "Setup indent bars for FRAME and remove from `after-make-frame-functions'."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (remove-hook 'after-make-frame-functions #'indent-bars-setup-and-remove t)
      (indent-bars-setup))))

;;;###autoload
(define-minor-mode indent-bars-mode
  "Indicate indentation with configurable bars."
  :global nil
  (if indent-bars-mode
      (if (and (daemonp) (not (frame-parameter nil 'client)))
	  (add-hook 'after-make-frame-functions #'indent-bars-setup-and-remove
		    nil t)
	(indent-bars-setup))
    (indent-bars-teardown)))

;; Theme support
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'indent-bars-reset-styles))

(provide 'indent-bars)

;;; indent-bars.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ibs/" . "indent-bars-style-"))
;; End:
