;;; indent-bars.el --- highlight indentation with bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/indent-bars
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Version: 0.0.1
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

;; indent-bars highlights indentation with configurable graphical
;; bars, including depth-varying color options.  The indentation of
;; the current line is (optionally) specially highlighted.  Works for
;; modes using space-based indentation.

;;; Code:
;;;; Requires
(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'outline)
(require 'font-lock)

;;;; Customization
(defgroup indent-bars nil
  "Highlight indentation bars."
  :group 'basic-faces
  :prefix "indent-bars-")

(defcustom indent-bars-spacing
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "Default indentation spacing, used if major mode isn't detected."
  :local t
  :type 'integer
  :group 'indent-bars)

(defcustom indent-bars-width-frac 0.32
  "The width of the indent bar as a fraction of the character width."
  :type '(float :tag "Width fraction"
	  :match (lambda (_ val) (and (<= val 1) (>= val 0)))
	  :type-error "Fraction must be between 0 and 1")
  :group 'indent-bars)

(defcustom indent-bars-offset-frac 0.1
  "The offset of the bar from the left edge of the character.
A float, the fraction of the character width."
  :type '(float :tag "Offset fraction"
	  :match (lambda (_ val) (and (<= val 1) (>= val 0)))
	  :type-error "Fraction must be between 0 and 1")
  :group 'indent-bars)

(defcustom indent-bars-pattern "."
  "A string specifying the vertical structure pattern of indent bars.
Space signifies blank regions, and any other character signifies
filled regions.  The pattern is scaled to match the character
height.  Example: \". . \" would specify alternating filled and
blank regions each approximately one-quarter of the character
height.  Note that non-blank characters need not be the
same (e.g., see `indent-bars-zigzag')."
  :type 'string
  :group 'indent-bars)

(defcustom indent-bars-zigzag nil
  "The zigzag to apply.
If non-nil, an alternating zigzag offset will be applied to
consecutive groups of identical non-space characters.  From the
top of the pattern, positive values will zigzag (right, left,
right, ..) and negative values (left, right, left, ...).  There
is no wrap-around.

Example:

  pattern: \" .**.\"
  width:   0.5
  offset:  0.25
  zigzag: -0.25

would produce a zigzag pattern which differs from the normal
value like:

    |    |     	      |    |
    | .. | =========> |..  |
    | .. |     	      |  ..|
    | .. | apply zig- |  ..|
    | .. | zag -0.25  |..  |

Note that the offset will be truncated at zero and the bitmap
will not extend past one, so achieving an equal zig-zag left and
right requires leaving room on each side of the bar for the
zig-zag; see `indent-bars-offset-frac' and
`indent-bars-width-frac'."
  :type '(choice
	  (const :value "No zig-zag" :value nil)
	  (float :value 0.1 :tag "Zig-zag fraction"
		 :match (lambda (_ val) (and (<= val 1) (>= val -1)))
		 :type-error "Fraction must be between -1 and 1")))

(defcustom indent-bars-color
  '(highlight :background t :blend 0.25)
  "Configuration for the main indent bar color.
The format is a list of 1 required element, followed by an
optional plist (keyword/value pairs):

  (main_color [:background :blend])

where:

  MAIN_COLOR: Specifies the main indentation bar
    color (required).  It is either a face name symbol, from
    which the foreground color will be used as the primary bar
    color, or an explicit color (a string).

  BACKGROUND: A boolean controlling interpretation of the
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
  '(list (choice :tag "Main Bar Color" color (face :tag "from Face"))
	 (plist :tag "Options"
		:inline t
		:options
		((:background (boolean
			       :tag "Use Face Background Color"
			       :value t))
		 (:blend (float
			  :tag "Blend Factor"
			  :value 0.5
			  :match (lambda (_ val) (and (<= val 1) (>= val 0)))
			  :type-error "Factor must be between 0 and 1")))))
  :group 'indent-bars)

(defcustom indent-bars-color-by-depth
  '(:regexp "outline-\\([0-9]+\\)" :blend 0.25)
  "Configuration for depth-varying indentation bar coloring.
If non-nil, depth-based coloring is performed.  This should be a
plist with keys:

    (:regexp :background :palette :blend)

where:

  REGEXP: A regular expression string used to match against all
    face names.  For the matching faces, the first match group in
    the regex (if any) will be interpreted as a number, and used
    to sort the resulting list of faces.  The foreground color of
    each matching face will then constitute the depth color
    palette (see PALETTE, which this option overrides).

  BACKGROUND: A boolean.  If non-nil, use the background color
    from the faces matching REGEXP for the palette instead of
    foreground.

  PALETTE: An explicit cyclical palette of colors/faces for
    depth-varying bar colors.  Note that REGEXP takes precedence
    over this setting.  The format is a list of faces (symbols)
    or colors (strings) to be used as a color cycle for coloring
    indentations at increasing levels.  Each face can optionally
    be specified as a cons cell (face . 'background) to specify
    using that face's background color instead of its foreground.

      (face_or_color | (face . 'background) ...)

    While this list can contain a single element, it makes little
    sense to do so.  The depth palette will be used cyclically,
    i.e. when a bar's indentation depth exceeds the length of the
    palette, colors will be obtained by wrapping around to the
    beginning of the list.

  BLEND: a blend factor which controls how the palette colors are
    blended with the main bar color (see `indent-bars-color' for
    information on how blend factors are used).  A nil (or unity)
    value causes the palette colors to be used as-is.

Note that, for this setting to have any effect, one of REGEXP or
PALETTE is required (the former overriding the latter).  If both
are omitted or nil, all bars will have the same color, based on
MAIN_COLOR (aside possibly from the bar at the current
indentation level, if configured; see
`indent-bars-highlight-current-indentation')."
  :type '(choice :tag "Depth Palette"
		 (const :tag "No Depth-Coloring" nil)
		 (plist :tag "Depth-Coloring"
			:options
			((:regexp (regexp :tag "Face regexp"))
			 (:background
			  (boolean
			   :value t
			   :tag "Use Matching Face Background Colors"))
			 (:palette
			  (repeat :tag "Explicit Color/Face List"
				  (choice (color :tag "Color")
					  (face :tag "Foreground from Face")
					  (cons :tag "Background from Face"
						:format "Background from %v"
						face
						(const :format "\n"
						       :value background)))))
			 (:blend
			  (float :tag "Fraction"
				 :value 0.5
				 :match (lambda (_ val)
					  (and (<= val 1) (>= val 0)))
				 :type-error
				 "Factor must be between 0 and 1")))))
  :group 'indent-bars)

(defcustom indent-bars-highlight-current-indentation
  '(default :blend 0.5)
  "Current indentation bar highlight configuration.
Use this to configure optional highlighting of the bar at the
current line's indentation level.  Format:

  nil | (color_or_face [:background :blend])

If nil, no highlighting will be applied to bars at the current
depth of the line at point.  If non-nil, all bars at the
indentation depth will be highlighted in this color (after a
delay; see `indent-bars-highlight-current-delay').  COLOR_OR_FACE
can be either a color (string) or face name (symbol) from which
the foreground color is taken.  If BACKGROUND is non-nil, the
face's background color will be used instead.  If BLEND is
provided, it is a blend fraction between 0 and 1 for blending the
highlight color with the depth-based or main color; see
`indent-bars-colors' for its meaning."
  :type '(choice
	  (const :tag "None " :value nil)
	  (list :tag "Highlight"
		(choice :tag "Face or Color" color face)
		(plist :tag "Options"
		       :inline t
		       :options
		       ((:background (boolean :tag "Use Face Background Color"))
			(:blend (float :tag "Blend with Main Color")
				:value 0.5
				:match (lambda (_ val) (and (<= val 1) (>= val 0)))
				:type-error "Factor must be between 0 and 1")))))
  :group 'indent-bars)
  
(defcustom indent-bars-highlight-current-delay 0.2
  "Delay in seconds after any commands before current indentation is highlighted."
  :type 'float
  :group 'indent-bars)

(defcustom indent-bars-display-on-blank-lines t
  "Whether to display bars on blank lines."
  :type 'boolean
  :group 'indent-bars)

;;;; Variables
(defvar indent-bars--current-depth-timer nil)
(defvar indent-bars-orig-unfontify-region nil)
(defvar indent-bars--main-color nil)
(defvar indent-bars--depth-palette nil)
(defvar indent-bars--current-depth-palette nil
  "Palette for highlighting current depth.
May be nil, a color string or a vector of colors strings.")
(defvar indent-bars--faces nil)
(defvar-local indent-bars--current-depth nil)
(defvar-local indent-bars--remap-face nil)
(defvar-local indent-bars--remap-stipple nil)

;;;; Colors
(defun indent-bars--blend-colors (c1 c2 fac)
  "Return a fractional color between two colors C1 and C2.
Each is a string color.  The fractional blend point is the
float FAC, with 1.0 matching C1 and 0.0 C2."
  (apply #'color-rgb-to-hex
	 (cl-mapcar (lambda (a b)
		      (+ (* a fac) (* b (- 1.0 fac))))
		    (color-name-to-rgb c1) (color-name-to-rgb c2))))

(defun indent-bars--main-color ()
  "Calculate the main bar color.
Uses `indent-bars-color'."
  (cl-destructuring-bind (main &key background blend) indent-bars-color
    (let ((col (cond ((facep main)
		      (funcall (if background
				   #'face-background
				 #'face-foreground)
			       main))
		     ((color-defined-p main) main))))
      (if blend
	  (setq col
		(indent-bars--blend-colors
		 col (frame-parameter nil 'background-color) blend)))
      col)))

(defun indent-bars--depth-palette ()
  "Calculate the palette of depth-based colors (a vector).
See `indent-bars-color-by-depth'."
  (cl-destructuring-bind (&key regexp background palette blend)
      indent-bars-color-by-depth
    (let ((colors (cond
		   (regexp
		    (indent-bars--depth-colors-from-regexp regexp background))
		   (palette
		    (delq nil
			  (cl-loop for el in palette
				   collect (cond
					    ((and (consp el) (facep (car el)))
					     (face-background (car el)))
					    ((facep el)
					     (face-foreground el))
					    ((color-defined-p el) el)
					    (t nil))))))))
      (vconcat
       (if blend
	   (mapcar (lambda (c) (indent-bars--blend-colors
				c indent-bars--main-color blend))
		   colors)
	 colors)))))

(defun indent-bars--current-depth-palette ()
  "Calculate the color or color palette for highlighting the current depth bar.
Based on `indent-bars-color-by-depth' or, if not configured for
depth-varying color, simply `indent-bars-color'.  See
`indent-bars-highlight-current-indentation' for configuration."
  (when indent-bars-highlight-current-indentation
    (cl-destructuring-bind (cur (&key background blend))
	indent-bars-highlight-current-indentation
      (when-let ((color
		  (cond ((facep cur)
			 (funcall (if background
				      #'face-background
				    #'face-foreground)
				  cur))
			((color-defined-p cur) cur))))
	(if blend
	    (if indent-bars--depth-palette ; blend into depth palette
		(vconcat (mapcar (lambda (c)
				   (indent-bars--blend-colors color c blend))
			 indent-bars--depth-palette))
	      (indent-bars--blend-colors color indent-bars--main-color blend))
	  color)))))

(defun indent-bars--depth-colors-from-regexp (regexp &optional background)
  "Return a list of depth colors (strings) for faces matching REGEXP.
The first capture group in REGEXP will be interpreted as a number
and used to sort the list numerically.  A list of the foreground
color of the matching, sorted faces will be returned, unless
BACKGROUND is non-nil, in which case the background color is
returned."
  (mapcar (lambda (x) (funcall (if background #'face-background
				 #'face-foreground)
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
color, if setup (see
`indent-bars-highlight-current-indentation')."
  (let* ((palette (if current-highlight
		      indent-bars--current-depth-palette
		    indent-bars--depth-palette)))
    (if palette
	(if (vectorp palette)
	    (aref palette (mod (1- depth) (length palette)))
	  palette)  ;  a standalone color
      indent-bars--main-color)))

;;;; Faces
(defun indent-bars--create-stipple-face (w h)
  "Create and set the `indent-bars-stipple' face for character size W x H."
  (face-spec-set 'indent-bars-stipple
		 `((t (:inherit nil :stipple ,(indent-bars--stipple w h))))))

(defun indent-bars--calculate-face-spec (depth &optional current-highlight)
  "Calculate the face spec for indentation bar at an indentation DEPTH.
DEPTH starts at 1.  If CURRENT-HIGHLIGHT is non-nil, use the
current depth highlight color."
  (let* () ; no depth config just use main
    `((t . ( :inherit indent-bars-stipple
	     :foreground ,(indent-bars--get-color depth current-highlight))))))

;((t (:inherit nil
;; 		 :stipple ,(my/highlight-indentation-stipple
;; 			    (frame-char-width)
;; 			    (frame-char-height))
;; 		 :foreground ,(face-background 'hl-line)))))

(defun indent-bars--create-faces (num &optional redefine)
  "Create bar faces up to depth NUM, redefining them if REDEFINE is non-nil.
Saves the vector of face symbols in variable
`indent-bars--faces'."
  (setq indent-bars--faces
	(vconcat
	 (cl-loop for i from 1 to num
		  for face = (intern (format "indent-bars-%d" i))
		  do (unless (or redefine (facep face))
		       (face-spec-set face (indent-bars--calculate-face-spec i)))
		  collect face))))

(defsubst indent-bars--face (depth)
  "Return the bar face for bar DEPTH, creating it if necessary."
  (if (> depth (length indent-bars--faces))
      (indent-bars--create-faces depth))
  (aref indent-bars--faces (1- depth)))

;;;; Display
(defun indent-bars--unfontify (beg end)
  "Unfontify region between BEG and END.
Removes the display properties in addition to the normal managed
font-lock properties."
  (let ((font-lock-extra-managed-props
         (append '(display) font-lock-extra-managed-props)))
    (funcall indent-bars-orig-unfontify-region beg end)))

(defsubst indent-bars--block (n)
  "Create a number with N lowest bits set."
  (cl-loop for i below n sum (ash 1 i)))


(defun indent-bars--rot (num w n)
  "Rotate number NUM of W bits by N bits right."
  (+ (ash (logand num (indent-bars--block n)) (- w n)) ; shift n bits up
     (ash (logand num (ash (indent-bars--block (- w n)) n)) (- n))))

(defun indent-bars--row-data (w offset)
  "Calculate stipple row data for character width W, shifted by OFFSET bits."
  ;; Note: Low-order bits are leftmost.  Stipple bitmap data are "scrambled":
  ;;  w>8:  multibyte: bytes ordering rotated right by 1, last byte
  ;;        short, with w%8 lower bits
  ;;  w>3:  rotates lower w bits right by 2*(mod w 4)
  ;;  w<=3: rotates lower w bit right by (mod w 2)
  ;; Example: width 10, 1 pixel blank then 6 filled, rest blank (left of
  ;; decimal ignored).  2 bytes required:
  ;;   desired pattern 0001111110 -> 0.0011111 00000.10 -> rot1 = (string 2 31)
  (let* ((rowbytes (/ (+ w 7) 8))
	 (bitwidth (max 1 (round (* w indent-bars-width-frac))))
	 (num (ash (indent-bars--block bitwidth) offset))
	 (leftoverbits (if-let ((val (mod w 8)) ((= val 0))) 8 val)))
    (cond
     ;; Single Byte: bit rotation
     ((<= w 3) (unibyte-string (indent-bars--rot num w (mod w 2))))
     ((< w 8) (unibyte-string (indent-bars--rot num w (* 2 (mod w 4)))))
     ((= w 8) (unibyte-string num))
     ;;  Multibyte
     (t (let ((inds `(,(1- rowbytes) ; rot1 byte ordering
		      ,@(number-sequence 0 (- rowbytes 2)))) 
	      (shifts `(,leftoverbits	; last byte short
			,@(make-list (1- rowbytes) 8))))
	  (apply #'unibyte-string
		 (mapcar (apply-partially
			  #'aref (cl-loop
				  for i in (reverse inds)
				  for n = num then (ash n (- shift))
				  for shift = (nth i shifts)
				  vconcat
				  (list (logand n (indent-bars--block shift)))))
			 inds)))))))

(defun indent-bars--stipple (w h)
  "Calculate the correct stipple bitmap pattern for char width W and height H.
Uses configuration variables `indent-bars-width-frac',
`indent-bars-offset-frac', `indent-bars-pattern', and
`indent-bars-zigzag'."
  (let* ((rowbytes (/ (+ w 7) 8))
	 (plen (length indent-bars-pattern))
	 (pat (if (< h plen) (substring indent-bars-pattern 0 h)
		indent-bars-pattern))
	 (chunk (/ (float h) plen))
	 (small (floor chunk))
	 (large (ceiling chunk))
	 (offset (round (* w indent-bars-offset-frac)))
	 (zz (if indent-bars-zigzag (round (* w indent-bars-zigzag)) 0)) 
	 (zeroes (make-string rowbytes ?\0))
	 (dlist (if (and (= plen 1) (not (string= pat " "))) ; solid bar
		    (list (indent-bars--row-data w offset))  ; one row
		  (cl-loop for last-fill-char = nil
			   for small-p = t then (not small-p)
			   for n = (if small-p small large)
			   for x across pat
			   for zoff = zz then (if (and last-fill-char
						       (not (eq x ?\s))
						       (not (eq x last-fill-char)))
						  (- zoff) zoff)
			   for row = (if (eq x ?\s) zeroes
				       (indent-bars--row-data w (+ offset zoff)))
			   unless (eq x ?\s) do (setq last-fill-char x)
			   append (cl-loop repeat n collect row)))))
    (list w (length dlist) (string-join dlist))))

(defun indent-bars--draw (start end &optional bar-from obj)
  "Set bar text properties from START to END, starting at bar number BAR-FROM.
BAR-FROM is one by default.  If passed, properties are set in
OBJ, otherwise in the buffer."
  (cl-loop for pos = start then (+ pos indent-bars-spacing) while (< pos end)
	   for barnum from (or bar-from 1)
	   ;; XXX need rear-nonsticky font-lock-face???
	   do (put-text-property pos (1+ pos)
				 'font-lock-face (indent-bars--face barnum) obj)))

;;;; Font Lock

(defvar-local indent-bars--font-lock-keywords nil)
(defvar indent-bars--font-lock-blank-line-keywords nil)


(defvar font-lock-beg) (defvar font-lock-end) ; Dynamic font-lock variables!
(defun indent-bars--extend-blank-line-regions ()
  "Extend the region about to be font-locked to include stretches of blank lines."
  (let ((changed nil) (chars " \n"))
    (unless (eq font-lock-beg (point-min))
      (goto-char (1- font-lock-beg))
      (when (< (skip-chars-backward chars) 0)
	(unless (bolp) (beginning-of-line 2)) ; spaces at end don't count
	(if (< (point) font-lock-beg)
	    (setq changed t font-lock-beg (point)))))
    (goto-char font-lock-end)
    (if (> (skip-chars-forward chars) 0)
	(setq changed t font-lock-end (point)))
    changed))


(defun indent-bars--handle-blank-lines ()
  "Display the appropriate bars on regions of one or more blank-only lines.
Only called by font-lock if `indent-bars-display-on-blank-lines'
is non-nil.  Uses surrounding line indentation to determine
additional bars to display on each line, and uses a string
display property on the final newline if necessary to display the
needed bars.  Blank lines at the beginning or end of the buffer
are not indicated."
  (let* ((beg (match-beginning 0))
	 (end (match-end 0))
	 ctxbars)
    (when (and (not (= end (point-max))) (not (= beg (point-min))))
      (goto-char beg)
      (forward-line -1)
      (when (> (setq ctxbars
		     (1- (max (/ (current-indentation) indent-bars-spacing)
			      (progn
				(goto-char end)
				(forward-line 1)
				(/ (current-indentation) indent-bars-spacing)))))
	       0)
       (goto-char beg)
       (while (< (point) end)
	 (let* ((bp (line-beginning-position))
		(ep (line-end-position))
		(len (- ep bp))
		(nbars (/ len indent-bars-spacing))
		nsp off s) ; "natural" bars
	   (if (> nbars 0) (indent-bars--draw bp ep))
	   (when (> ctxbars nbars) ;we need more bars than we have space!
	     (unless (and (eq off (setq off (- (* (1+ nbars) indent-bars-spacing) len)))
			  (eq nsp (setq nsp (- (* ctxbars indent-bars-spacing) len))))
	       (setq s (concat (make-string nsp ?\s) "\n"))
	       (indent-bars--draw off nsp (1+ nbars) s))
	     (set-text-properties ep (1+ ep) `(display ,s)))
	   (forward-line 1)))))))

(defun indent-bars--display ()
  "Display indentation bars based on line contents."
  (save-excursion
    (let* ((beg (goto-char (match-beginning 0)))
	   (end (match-end 0)))
      (indent-bars--draw beg end)))
  nil)

;;;; Text scaling
(defun indent-bars--resize-stipple ()
  "Recreate stipple with font size change."
  (if indent-bars--remap-stipple
      (face-remap-remove-relative indent-bars--remap-stipple))
  (when text-scale-mode
    (setq indent-bars--remap-stipple
	  (face-remap-add-relative 'indent-bars-stipple
				   :stipple (indent-bars--stipple
					     (window-font-width)
					     (window-font-height))))))


;;;; Current indentation highlight
(defun indent-bars--highlight-current-depth ()
  "Refresh current indentation depth highlight.
Works by remapping the appropriate indent-bars-N face."
  (let ((depth (/ (current-indentation) indent-bars-spacing)))
    (when (and (> depth 0) (not (= depth indent-bars--current-depth))
	       (< depth (length indent-bars--faces)))
      (setq indent-bars--current-depth depth)
      (if indent-bars--remap-face 	; out with the old
	  (face-remap-remove-relative indent-bars--remap-face))

      (if-let ((face (indent-bars--face depth))
	       (hl-col (indent-bars--get-color depth 'highlight)))
	(setq indent-bars--remap-face
	      (face-remap-add-relative face :foreground hl-col))))))

(defun indent-bars--post-command ()
  "Schedule current indentation depth highlighting."
  (when indent-bars--current-depth-timer
    (cancel-timer indent-bars--current-depth-timer))
  (setq indent-bars--current-depth-timer
	(run-at-time indent-bars-highlight-current-delay nil
		     #'indent-bars--highlight-current-depth)))

;;;; Setup and mode
(defun indent-bars--guess-spacing ()
  "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
  (cond ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
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
        (t indent-bars-spacing)))

(defun indent-bars--setup-font-lock ()
  "Setup font lock keywords and functions for indent-bars."
  (setq indent-bars--font-lock-keywords
	`((,(rx-to-string `(seq bol (group (>= ,(1+ indent-bars-spacing) ?\s)) nonl))
	   (1 (indent-bars--display)))))
  (if indent-bars-display-on-blank-lines
      (let ((re (rx bol (+ (or ?\s ?\n)) eol)))
	(setq indent-bars--font-lock-blank-line-keywords
	      `((,re (0 (indent-bars--handle-blank-lines))))))))

(defun indent-bars-setup (&optional force)
  "Setup all face, color, bar size, and indentation info for the current buffer.
If FORCE is non-nil, update all parameters even if already set."
  ;; Spacing
  (unless (or force (alist-get 'indent-bars-spacing (buffer-local-variables)))
    (setq indent-bars-spacing (indent-bars--guess-spacing)))

  ;; Colors
  (setq indent-bars--main-color (indent-bars--main-color)
	indent-bars--depth-palette (indent-bars--depth-palette)
	indent-bars--current-depth-palette (indent-bars--current-depth-palette))

  ;; Faces
  (indent-bars--create-stipple-face (frame-char-width) (frame-char-height))
  (indent-bars--create-faces 9 force)   ; extends as needed

  ;; Font-lock
  (indent-bars--setup-font-lock)
  (font-lock-add-keywords nil indent-bars--font-lock-keywords t)
  (setq indent-bars-orig-unfontify-region font-lock-unfontify-region-function)
  (setq-local font-lock-unfontify-region-function #'indent-bars--unfontify)

  ;; Resize
  (add-hook 'text-scale-mode-hook #'indent-bars--resize-stipple nil t)

  ;; Current depth highlight
  (if indent-bars-highlight-current-indentation
      (add-hook 'post-command-hook #'indent-bars--post-command nil t))

  ;; Blank lines
  (when indent-bars-display-on-blank-lines
    (font-lock-add-keywords nil indent-bars--font-lock-blank-line-keywords t)
    (add-hook 'font-lock-extend-region-functions 
	      #'indent-bars--extend-blank-line-regions 95 t))

  (font-lock-flush))

(defun indent-bars-teardown ()
  "Tears down indent-bars."
  (font-lock-remove-keywords nil indent-bars--font-lock-keywords)
  (font-lock-remove-keywords nil indent-bars--font-lock-blank-line-keywords)
  (font-lock-flush)
  (setq font-lock-unfontify-region-function indent-bars-orig-unfontify-region)
  (remove-hook 'text-scale-mode-hook #'indent-bars--resize-stipple t)
  (remove-hook 'post-command-hook #'indent-bars--post-command t)
  (remove-hook 'font-lock-extend-region-functions 
	       #'indent-bars--extend-blank-line-regions t))

;;;###autoload
(define-minor-mode indent-bars-mode
  "Indicate indentation with configurable bars."
  :global nil
  :group 'indent-bars
  (if indent-bars-mode
      (indent-bars-setup)
    (indent-bars-teardown)))

(provide 'indent-bars)

;;; indent-bars.el ends here
