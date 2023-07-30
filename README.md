# indent-bars

Fast, configurable indentation guide-bars for Emacs.

<img align="right" width="514" alt="ib_default" src="https://github.com/jdtsmith/indent-bars/assets/93749/4f652554-bede-4aa6-bdbc-233ec843d782">

This package provides vertical indentation _guide bars_, with the following features:

- Uses stipple face properties with font-lock for ultra-fast performance (simply: *faces on spaces*).
- Learns your buffer indentation spacing from the mode.
- Bar colors can be blended with the frame background color, to reduce their intrusiveness.
- Bar appearance is highly configurable: width, position within the character, vertical fill/blank pattern, even zigzag (see [examples](examples.md)).
- Optional depth-based coloring, with a customizable cyclical palette.
- Properly handles font size changes.
- Optional zero-cost current-depth bar highlighting, permitting bar color and/or appearance changes.
- Optional support for drawing bars on blank lines.

# Why?

There are ([many](#related-packages)) existing packages that provide indentation highlighting/guides.  But none:

1. were fast enough with large files (including current depth highlighting)
2. had enough guide appearance configurability
3. were able to support depth-based coloring
4. offered robust support for guides on blank lines

# Install/config

Not yet in a package database; simply clone and point `use-package` at the correct path.

```elisp
(use-package indent-bars
  :load-path "~/code/emacs/indent-bars"
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
```
## Straight

To clone with `use-package` and `straight`, add to config:

```elisp
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
```
## Compatibility 

For `indent-bars` to work, your port and version of emacs must correctly support the `:stipple` face attribute.  **Most do.**

- All known UNIX/GNU Linux versions support stipples. 
- On Mac, the [emacs-mac](https://bitbucket.org/mituharu/emacs-mac/src/master/)[^1] port has stipple support. 
- Windows Emacs does not apparently support stipples.
- `:stipple` is not supported (to my knowledge) on terminal emacs.

[^1]: Most easily installed [with brew](https://github.com/railwaycat/homebrew-emacsmacport).

Please [open an issue](../../issues) with any updates/corrections to this list.   See also [Testing Stipples](#testing-stipples).

# Customization
`M-x customize-group indent-bars` is the easiest way to customize everything about the appearence and function of `indent-bars`.  Note: when changing any of these variables while `indent-bars` is on, you must `M-x indent-bars-reset` in the buffers of interest to see the resulting changes.  See some [examples](examples.md).
	
The main customization variables:
	
- `indent-bars-width-frac`: The fractional width of the bar (0-1, in terms of fraction of a single character's width).
- `indent-bars-pad-frac`: The fractional padding offset of the bar from the left edge of the character. 
- `indent-bars-pattern`: A string specifying the vertical structure of the bar (space=blank, non-space=filled).  Scaled to the height of one character.
- `indent-bars-zigzag`: A fractional left-right *zigzag* to apply to consecutive groups of identical non-space characters in `pattern`.
- `indent-bars-color`: The main bar color, either a color name or face, from which foreground or background color will be taken.  Also used to set a `:blend` factor, to blend colors into the frame's background color.
- `indent-bars-color-by-depth`: How and whether to alter the color of the indent bars by indentation depth.  Defaults to using the foreground of the `outline-*` faces.
- `indent-bars-highlight-current-depth`: How and whether to highlight the bars at the indentation depth of the current line.  The current depth bar can change color (including blending with the pre-existing color), as well as structure (size, pad, pattern, zigzag).
- `indent-bars-spacing-override`:  Normally the number of spaces for indentation is automatically discovered from the mode and other variables.  If that doesn't work for any reason, it can be explicitly set using this variable.
- `indent-bars-display-on-blank-lines`: Whether to display bars on blank lines.

See the documentation of each variable for more details.

# Details and Caveats

## Speed

`indent-bars` was partially motivated by the inefficiency of older indentation highlight modes, and is designed for speed.  It uses stipples (fixed bitmap patterns) and font lock for fast and efficient bar drawing — *faces on spaces*.  Highlighting the current indentation level is essentially free, since it works by [remapping](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) the relevant face.

The heaviest operation (though still fairly efficient) is **blank-line highlighting**, since the indentation level of blank lines depends on their surrounding context, and strings must be allocated, styled, and used as `'display` properties.  If you experience any speed issues, this is the first setting to turn off. 

## Indentation

`indent-bars` only works with space-based indentation, i.e. `indent-tabs-mode=nil`.  Note that many modes enable this by default.

## Stipples
The fast *stipple* method used for drawing bars enables lots of [interesting patterns](examples.md).

Stipples are repeating patterns anchored to the entire emacs frame.  `indent-bars` basically "opens windows" on this fixed pattern to "reveal" the bars.  

### Testing Stipples

If you are having issues and would like to determine if stipples are working correctly in your version of emacs, enter (via `M-:` or in the `*scratch*` buffer, hitting `C-x C-e` just after it):

```elisp
(let ((w (window-font-width)))
  (set-face-stipple
   'default
   `(,w 1 ,(apply #'unibyte-string
		  (append (make-list (1- (/ (+ w 7) 8)) ?\0) '(1))))))
```

and you should see a "jailbar" pattern in the default foreground color across all windows.

### Per-buffer stipple offsets
To get the bars in the right place, `indent-bars` must consider the starting horizontal pixel position of the current window, and adjust the stipple pattern accordingly.  It does this automatically, per buffer, so you shouldn't ever notice problems, even when re-sizing or re-arranging windows, changing font size, etc.

There is one rare corner case, however: showing the *same buffer* side by side in Emacs versions which support pixel-level window width/offsets (e.g. emacs-mac) can lead to unexpected bar positions in the non-active buffer, since the stipple offset in the remapped face applies *per-buffer*, i.e. it can't be correct for left and right buffers at the same time.  

Options are living with this, or (for Emacs >=29) instead of visiting the same buffer, cloning an indirect buffer (which has other advantages, like an independent region).  Note that Emacs 28 and earlier have a bug which results in cloned buffers sharing the same face remapping list as their parent; this is fixed in Emacs 29.

# Related Packages
- [indent-guide](https://github.com/zk-phi/indent-guide): An older package that uses overlays with `|` characters.  Some reports of performance concerns.  Incompatible with company and other related in-buffer modes.
- [highlight-indentation-mode](https://github.com/antonj/Highlight-Indentation-for-Emacs): Uses overlays to draw indentation guides, and includes a current indentation mode.  Partial support for blank line guides.  `indent-bars` adapts the indentation guessing function from this mode.
- [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides):  a highly configurable mode for indentation highlight, with color and style options, as well as current depth highlighting.
- [hl-indent-scope](https://codeberg.org/ideasman42/emacs-hl-indent-scope): Highlights indentation based on language scope - requiring support for each language, uses overlays to draw indentation guides.
- [visual-indentation-mode](https://github.com/skeeto/visual-indentation-mode): Full character-based alternating color indentation guides.  Package is now archived.
