# indent-bars: fast, configurable indentation guide-bars for Emacs.

<img align="right" width="601" alt="image" src="https://github.com/jdtsmith/indent-bars/assets/93749/776e9c09-a3ef-41af-8237-dfe3f62aa48b">

This package provides vertical indentation _guide bars_, with the following features:

- Supports both space and tab-based indentation.
- Uses stipple face properties with font-lock for ultra-fast performance (simply: *faces on spaces*).
- Works in the terminal, using a vertical bar character.
- Learns your buffer indentation spacing from the active mode.
- Bar colors can blend with the frame background to reduce intrusiveness.
- Bar appearance is highly configurable: width, position within the character, vertical fill/blank pattern, even zigzag (see [examples](examples.md)).
- Bars have optional depth-based coloring, with a customizable cyclical color palette.
- Font size changes are handled automatically.
- Fast current-depth bar highlighting with bar color and/or appearance changes.
- Blank line support.
- Optional tree-sitter support, for context-aware bar depth in strings, wrapped entities like function arguments, and top level blank lines.

## What's New

- v0.1: Initial stipple-based indentation.
- v0.2: 
   - ability to configure the starting column (including col 0)
   - Support for tab-based indent modes
   - optional character-based indent bars (automatic in terminal)
   - tree-sitter context-aware bar depth
   - additional mode support: `go-mode`, `go-ts-mode`, `cobol-mode`
   - other minor improvements

# FAQ's

- **I don't see anything/bars are garbled!** <br>While most do, not all Emacsen support stipples; see [Compatibility](#compatibility).
- **How can I find out if my Emacs supports stipples?!**  <br>See [Testing Stipples](#testing-stipples).
- **These bars are too instrusive!** <br>Reduce the `:blend` value in `indent-bars-color` closer to zero. Consider disabling `indent-bars-color-by-depth`.
- **I can barely see the bars!** <br>Increase the `:blend` value in `indent-bars-color` closer to one.
- **I want completely unique indent guidebars so as to flex on my colleagues!** <br>Check the [Examples](examples.md) for some ideas.  The sky is the limit (submit your examples).
- **I use Emacs on the terminal, you insensitive clod!** <br>`indent-bars` will just work for you (though you don't get any fancy bar patterns).
- **I use graphical Emacs, but am an extreme minimalist.  All my outfits are gray.  Including my socks.** <br>Maybe [this](examples.md#minimal) will suit you?  Otherwise, you can turn off the stipple and use old fashioned `│` characters with [`indent-bars-prefer-character`](#character-display).
- **When I view the same buffer side by side, the bars jump around!** <br>This is a known issue for versions of Emacs with arbitrary pixel-width window; see [Per-buffer stipple offsets](#per-buffer-stipple-offsets).
- **I get too many bars inside function definitions and calls**: You can use [tree-sitter to help](#tree-sitter).
- **I want a bar in the very first column!**: set `indent-bars-starting-column` to 0.

# Install/config

Not yet in a package database; simply clone and point `use-package` at the correct path.  You can also simply use the `vc-package-install` command newly released with Emacs 29.

```elisp
(use-package indent-bars
  :load-path "~/code/emacs/indent-bars"
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
```
## Straight

To clone with `use-package` and `straight`:

```elisp
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
```

## With tree-sitter support

```elisp
(use-package indent-bars
  :load-path "~/code/emacs/indent-bars"
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				      list list_comprehension
				      dictionary dictionary_comprehension
				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))
```

See [tree-sitter](#tree-sitter), and also the [Wiki page](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Compatibility 

For `indent-bars` to display fancy guide bars, your port and version of emacs must correctly display the `:stipple` face attribute.  **Most do.**  It can also be used *without stipples*, drawing a simple vertical character (like `│`) instead.  It automatically does this in non-graphical displays (terminals), but can optionally be configured to always do so; see [Character Display](#character-display).

Known `:stipple` support, by Emacs build:

- Linux:
  - "Pure GTK" (`--with-pgtk` build flag) versions support stipples, but had a display bug that caused them to appear incorrectly (as [reverse video](../../issues/3)) and lead to [crashes](../../issues/6); this was fixed in Emacs [here](https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-07/msg02081.html) and will presumably be released with Emacs 30.
  - Cairo builds (`--with-cairo`) have been [reported](../../issues/33#issuecomment-1768888990) not to display stipples.
  - All other builds do support stipples.
- Mac:  The [emacs-mac](https://bitbucket.org/mituharu/emacs-mac/src/master/)[^1] port has stipple support, but others do not.  `M-x version` should say `Carbon`, not `NS`.
- Windows: Emacs on Windows does not (apparently) support stipples.
- Terminal: Stipples are not supported on terminal emacs.

[^1]: Most easily installed [with brew](https://github.com/railwaycat/homebrew-emacsmacport).

Please [open an issue](../../issues) with any updates/corrections to this list.

See also [Testing Stipples](#testing-stipples).

# Customization
`M-x customize-group indent-bars` is the easiest way to customize everything about the appearence and function of `indent-bars`.  Note: when changing any of these custom variables while `indent-bars` is enabled, you must `M-x indent-bars-reset` in the buffers of interest to see the resulting changes.

See some [examples](examples.md) with relevant settings.

The main customization variables:
	
- `indent-bars-width-frac`: The fractional width of the bar (0-1, in terms of fraction of a single character's width).
- `indent-bars-pad-frac`: The fractional padding offset of the bar from the left edge of the character. 
- `indent-bars-pattern`: A string specifying the vertical structure of the bar (space=blank, non-space=filled).  Scaled to the height of one character.
- `indent-bars-zigzag`: A fractional left-right *zigzag* to apply to consecutive groups of identical non-space characters in `pattern`.
- `indent-bars-color`: The main bar color, either a color name or face, from which foreground or background color will be taken.  Also used to set a `:blend` factor, to blend colors into the frame's background color.
- `indent-bars-color-by-depth`: How and whether to alter the color of the indent bars by indentation depth.  Defaults to using the foreground of the `outline-*` faces.
- `indent-bars-highlight-current-depth`: How and whether to highlight the bars at the indentation depth of the current line.  The current depth bar can change color (including blending with the pre-existing color), as well as structure (size, pad, pattern, zigzag).
- `indent-bars-starting-column`: column to use for the first bar.  Can be set in special modes which start at an unusual fixed offset, or set to 0 to get  "column 0" bars.
- `indent-bars-spacing-override`:  Normally the number of spaces for indentation is automatically discovered from the mode and other variables.  If that doesn't work for any reason, it can be explicitly set using this variable.
- `indent-bars-display-on-blank-lines`: Whether to display bars on blank lines.
- `indent-bars-prefer-character`: Use *characters* to display the vertical bar instead of stipples.  This occurs automatically on non-graphical displays (terminals), but this variable can be used to always prefer character-based display.
- `indent-bars-no-stipple-char`: The character to use when stipples are unavailable or disabled. Defaults to the vertical box character `│`.  Other good options include `┃`, `┋`, and `║`.
- `indent-bars-no-stipple-char-font-weight`: Optional font weight to use for the face displaying the no-stipple character.
- `indent-bars-unspecified-bg|fg-color`: Colors to use for the frame background and default foreground when they are unspecified (e.g. in terminals).  If you intend to use `indent-bars` in the terminal, set to the terminal background/foreground colors you use. 
- `indent-bars-treesit-support`: Whether to use tree-sitter (if available) to help determine appropriate bar depth.
- `indent-bars-treesit-wrap`: A mapping of language to tree-sitter wrap types, to avoid adding extra bars e.g. in wrapped function arguments.
- `indent-bars-treesit-ignore-blank-lines-types`: A list of tree-sitter node types to inhibit styling blank lines at, like "module". 
- `indent-bars-no-descend-string`: Whether to inhibit increasing depth inside of (tree-sitter determined) strings. 



See the documentation of each variable for more details.

# Details and Caveats

## Speed

`indent-bars` was partially motivated by the inefficiency of older indentation highlight modes, and is designed for speed.  It uses stipples (fixed bitmap patterns) and font lock for fast and efficient bar drawing — *faces on spaces*.  Highlighting the current indentation level is essentially free, since it works by [remapping](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) the relevant face.

The heaviest operations (though still quite efficient) are **tree-sitter** support, and  **blank-line highlighting**.  If you experience any speed issues, these are the first settings to experiment with turning off.

## Indentation

`indent-bars` works either with space-based indentation (i.e. `indent-tabs-mode=nil`) or with tab-based.  If possible, prefer space indentation, as it is faster.  Note that some modes explicitly enable or disable `indent-tabs-mode`.

## Tree-sitter

`indent-bars` can optionally use tree-sitter in supported files to improve the calculation of bar depth.  For example, many modes wrap function calls and definitions to align parameters with the opening `(`.  With the help of tree-sitter, `indent-bars` can avoid adding unwanted additional bars in these.  It can also be used to avoid extra depth in strings, and to tweak the behavior of blank line display.

I.e. turn this:

<img width="580" alt="Untitled" src="https://github.com/jdtsmith/indent-bars/assets/93749/f1cb2489-92ee-443d-b941-6237b386ca6f">

into this:

<img width="580" alt="Untitled 2" src="https://github.com/jdtsmith/indent-bars/assets/93749/b0c26c5b-6b38-475f-9bb8-9f1b1f5e54f0">

**Note**: This requires Emacs 29 built with tree-sitter support, and the appropriate tree-sitter grammars installed for languages of interest.

### Configuring tree-sitter

The main thing to do is configure `indent-bars-treesit-wrap` with the node types that lead to unwanted additional bars do their wrapping indentation behavior.  I recommend starting with the minimal possible set.  

The easiest way to discover the node types of interest is to `M-x treesit-explore-mode`, then highlight the beginning of a line with too many bars, and look in the `treesitter explorer` buffer which pops up for the names of obvious nodes in the tree.  Add these types to `indent-bars-treesit-wrap` for the language of interest, then `M-x indent-bars-reset` and see how you did.  You can do something similar for `indent-bars-treesit-ignore-blank-lines-types` (which, please note, are configured as _strings_, unlike in `indent-bars-treesit-wrap`).

Please document good tree-sitter settings for other langauges in the [Wiki](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Moving by columns

If `indent-bars-display-on-blank-lines` is set, the newline at the end of blank lines has a `'display` property set to show the bars.  Emacs does not deal correctly with display properties containing newlines when moving by columns.  This is not normally a problem, but in one instance it is a nuisance: evil tries to "preserve" column during line moves, so can trigger this emacs misfeature.  The symptom is that point jumps a line and moves over as you move down with evil.  A solution is [here](https://github.com/jdtsmith/indent-bars/issues/22#issuecomment-1793886072). 

## Display

### Stipples

Stipples are repeating bitmap patterns anchored to the full emacs frame.  `indent-bars` basically "opens windows" on this fixed pattern to "reveal" the bars.

The fast *stipple* method used for drawing bars enables lots of [interesting patterns](examples.md).

#### Testing Stipples

If you are experiencing issues with stipple bar display (missing, garbled, etc.), and would like to determine if stipples are working correctly in your build of emacs, you can test it as follows.

1. In the `*scratch*` buffer, use first `M-x font-lock-mode` to disable fontification
2. Hit `C-x C-e` just after the last `)` in the following code:
   ```elisp
   (let* ((w (window-font-width))
          (stipple `(,w 1 ,(apply #'unibyte-string
   			       (append (make-list (1- (/ (+ w 7) 8)) ?\0)
   				       '(1))))))
     (insert "\n" (propertize (concat  (make-string 15 ?\s)
   				    "THIS IS A TEST"
   				    (make-string 15 ?\s))
                              'face `(:background "red" :foreground "blue" :stipple ,stipple))))
   ```

This should then look something like:

<img width="668" alt="image" src="https://github.com/jdtsmith/indent-bars/assets/93749/dd0f65f5-3cdc-4865-a66d-41365cecadd0">

If you determine that stipples do not work in your Emacs, consider upgrading to a version which supports them, reporting the bug, or setting `indent-bars-prefer-character=t`.

#### Per-buffer stipple offsets
To get the stipple bars to appear in the correct location within their column, `indent-bars` must consider the starting horizontal pixel position of the current window, and "rotate" the stipple pattern accordingly.  It does this automatically, per buffer, so you shouldn't ever notice problems, even when re-sizing or re-arranging windows, changing font size, etc.

There is one rare corner case, however: showing the *same buffer* side by side in Emacs versions which support pixel-level window width/offsets (e.g. emacs-mac).  This can lead to unexpected bar positions in the non-active buffer, since the stipple offset in the remapped face applies *per-buffer*, not per-window.  I.e. it can't be correct for the same buffer in left and right windows *at the same time*.

Options are:

1. Living with it (simpler stipple patterns may help).
2. Using a build of emacs that always starts windows on the character boundary.
3. Switching to [character-based bars](#character-display).
4. (For Emacs >=29[^2]) instead of visiting the same buffer, cloning an indirect buffer (which has other advantages, like an independent region).

[^2]: Note that Emacs 28 and earlier have a bug which results in cloned buffers sharing the same face remapping list as their parent; this is fixed in Emacs 29.

### Character display

For terminals, (and everywhere, if `indent-bars-prefer-character` is set), `indent-bars` will not attempt stipple display, but instead use simple characters (e.g. `│`; see [an example](examples.md#in-terminal)).

Note that in mixed gui/terminal sessions of the same Emacs process, you need to `M-x indent-bars-reset` when switching a given buffer between graphical and terminal frames.

### Advantages/Disadvantages

#### Advantages of stipples

- Custom appearance and position within the character is possible — [examples](examples.md).
- Fastest option: does not need to apply display properties for normal lines with space-based indentation.
- Results in continuous lines even when `line-spacing` is non-nil (vs. gaps with box characters and additional line spacing).

#### Advantages of character bar display

- Works equally for terminal and GUI.
- Works even for emacs ports which do not support or mishandle stipple display (see [Compatibility](#compatibility)).

# Related Packages

- [indent-guide](https://github.com/zk-phi/indent-guide): An older package that uses overlays with `|` characters.  Some reports of performance concerns.  Incompatible with company and other related in-buffer modes.
- [highlight-indentation-mode](https://github.com/antonj/Highlight-Indentation-for-Emacs): Uses overlays to draw indentation guides, and includes a current indentation mode.  Partial support for blank line guides.  `indent-bars` adapts the indentation guessing function from this mode.
- [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides):  a highly configurable mode for indentation highlight, with color and style options, as well as current depth highlighting.
- [hl-indent-scope](https://codeberg.org/ideasman42/emacs-hl-indent-scope): Highlights indentation based on language scope - requiring support for each language, uses overlays to draw indentation guides.
- [visual-indentation-mode](https://github.com/skeeto/visual-indentation-mode): Full character-based alternating color indentation guides.  Package is now archived.

## Why a new package?

None of the existing packages:

1. were fast enough with large files (including current depth highlighting)
2. had enough guide appearance configurability
3. were able to support depth-based coloring
4. offered robust support for guides on blank lines
