# indent-bars: fast, configurable indentation guide-bars for Emacs.

<img align="right" width="450" alt="image" src="https://github.com/jdtsmith/indent-bars/assets/93749/776e9c09-a3ef-41af-8237-dfe3f62aa48b">

This package provides indentation _guide bars_ enhanced by tree-sitter:

- Uses stipple face properties with font-lock for fast performance.
- Optional tree-sitter support, including highlighting a configurable active _scope_, among [other features](#tree-sitter).
- Supports either space or tab-based indentation, learning indentation spacing from the active mode.
- Bar appearance is _highly_ configurable: color, blending, width, position within the character, vertical fill/blank pattern, even zigzag (see [examples](examples.md)).
- Bars can have optional depth-based coloring, with a cyclical color palette you can customize.
- Fast current-depth bar highlighting with configurable bar color and/or appearance changes.
- Bars can be drawn on blank lines.
- Bar depth can be held constant inside multi-line strings and lists. 
- Works in the terminal, using a vertical bar character.

## What's New

- v0.5: A major update with many new features and improvements.
  - Stipples are now free from artifacts when showing the same buffer in multiple windows.
  - Position-aware tree-sitter _scope_ with completely configurable out-of-scope styling.
  - Theme-awareness: bar styling gets updated on theme change (e.g. for depth-based colors).
  - Two new highlight selection "methods" including a new default ("context").
  - Inhibit string and list bar descent even without treesitter using Emacs' syntax capabilities.
- v0.2.2:
  - Rate-limit updates of the current highlight depth; see `indent-bars-depth-update-delay`. 
- v0.2: 
   - ability to configure the starting column (including col 0)
   - Support for tab-based indent modes
   - optional character-based indent bars (automatic in terminal)
   - tree-sitter context-aware bar depth
   - additional mode support: `go-mode`, `go-ts-mode`, `cobol-mode`
   - other minor improvements
- v0.1: Initial stipple-based indentation.

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
- **The current bar highlight is so fast, but it flashes too rapidly during scrolling!** Update to v0.2.2 or later and set `indent-bars-depth-update-delay` to a comfortable number like 0.1s (0.075s is the default).  If you _like_ the fast updates, set this to 0.

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
  :config
  (require 'indent-bars-ts) 		; not needed with straight
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))
```

See [tree-sitter](#tree-sitter), and also the [Wiki page](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Compatibility 

> [!IMPORTANT]
> For `indent-bars` to display fancy guide bars, your port and version of emacs must correctly display the `:stipple` face attribute.  **Most do**, but some do not.

`indent-bars` can also be used *without stipples*, drawing a simple vertical character (like `│`) instead.  It automatically does this in non-graphical displays (terminals), but can optionally be configured to do so always; see [Character Display](#character-display).

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
`M-x customize-group indent-bars` is the easiest way to customize everything about the appearance and function of `indent-bars`.  Note: when changing any of these custom variables while `indent-bars` is enabled, you must `M-x indent-bars-reset` in the buffers of interest to see the resulting changes.

See some [examples](examples.md) with relevant settings.

The main customization variables are categorized below.  See the documentation of each variable for more details.

## Bar shape and size
- `indent-bars-width-frac`: The fractional width of the bar (0-1, in terms of fraction of a single character's width).
- `indent-bars-pad-frac`: The fractional padding offset of the bar from the left edge of the character. 
- `indent-bars-pattern`: A string specifying the vertical structure of the bar (space=blank, non-space=filled).  Scaled to the height of one character.
- `indent-bars-zigzag`: A fractional left-right *zigzag* to apply to consecutive groups of identical non-space characters in `pattern`.

## Bar color and highlighting
- `indent-bars-color`: The main bar color, either a color name or face, from which foreground or background color will be taken.  Also used to set a `:blend` factor, to blend colors into the frame's background color.
- `indent-bars-color-by-depth`: How and whether to alter the color of the indent bars by indentation depth.  Defaults to using the foreground of the `outline-*` faces.

## Current Depth highlighting
- `indent-bars-highlight-current-depth`: How and whether to highlight the bars at the indentation depth of the current line.  The current depth bar can change color (including blending with the pre-existing color), as well as structure (size, pad, pattern, zigzag).
- `indent-bars-highlight-selection-method`: Method used to select which bar is highlighted.  The default (`'context`) considers surrounding lines for a more natural bar selection.  
- `indent-bars-depth-update-delay`: Delay in seconds after which depth highlighting occurs. 

## Bar setup and location
- `indent-bars-starting-column`: column to use for the first bar.  Can be set in special modes which start at an unusual fixed offset, or set to 0 to get "column 0" bars.
- `indent-bars-spacing-override`:  Normally the number of spaces for indentation is automatically discovered from the mode and other variables.  If that doesn't work for any reason, it can be explicitly set using this variable.
- `indent-bars-display-on-blank-lines`: Whether to display bars on blank lines.
- `indent-bars-no-descend-string`: Whether to inhibit increasing depth inside of strings. 
- `indent-bars-no-descend-list`: Whether to inhibit increasing depth inside of lists. 

## Character-based bars and terminal
- `indent-bars-prefer-character`: Use *characters* to display the vertical bar instead of stipples.  This occurs automatically on non-graphical displays (terminals), but this variable can be used to always prefer character-based display.
- `indent-bars-no-stipple-char`: The character to use when stipples are unavailable or disabled. Defaults to the vertical box character `│`.  Other good options include `┃`, `┋`, and `║`.
- `indent-bars-no-stipple-char-font-weight`: Optional font weight to use for the face displaying the no-stipple character.
- `indent-bars-unspecified-bg|fg-color`: Colors to use for the frame background and default foreground when they are unspecified (e.g. in terminals).  If you intend to use `indent-bars` in the terminal, set to the terminal background/foreground colors you use. 

## Treesitter support
- `indent-bars-treesit-support`: Whether to use tree-sitter (if available) to help determine appropriate bar depth.
- `indent-bars-treesit-scope`: A mapping of language to tree-sitter scope node types, for local scope highlight.
- `indent-bars-treesit-scope-min-lines`: The minimum number of lines a scope node must occupy to be considered a valid scope.
- `indent-bars-treesit-wrap`: A mapping of language to tree-sitter wrap types, to avoid adding extra bars e.g. in wrapped function arguments.  Note that this is considered only after the `no-descend` options above.
- `indent-bars-treesit-ignore-blank-lines-types`: A list of tree-sitter node types inside of which inhibit styling blank lines at, like "module". 
- `indent-bars-treesit-update-delay`: Delay in seconds for updating the treesitter scope highlight.

## Out-of-scope alternate styling variables
If tree-sitter scope is active (`indent-bars-treesit-scope`), the settings above apply only to the _in scope_ text. To permit configuring the appearance of the out-of-scope bars, a parallel set of custom variables with `indent-bars-ts-` prefixes are created.

These can be used in the same manner as above to _fully_ configure the appearance of the "out-of-scope" bars (i.e. bars outside the custom tree-sitter scope), including depth highlighting.  Note that scope highlighting is completely independent of depth highlighting.  The `ts` parallel variables for out-of-scope styling are:

- `indent-bars-ts-color` `inherit`
- `indent-bars-ts-width-frac`
- `indent-bars-ts-pad-frac`
- `indent-bars-ts-pattern`
- `indent-bars-ts-zigzag`
- `indent-bars-ts-no-stipple-char-font-weight`
- `indent-bars-ts-color-by-depth` [`inherit`]
- `indent-bars-ts-highlight-current-depth` [`inherit`]

Each of these parallel variables has an equivalent form to their equivalent non-`ts` version (the "parent" variable), with two difference:

1. Some (marked with [`inherit`] above) can be specified as a cons cell of the form `([no-]inherit . value)`, where `value` has the normal format for the parent variable.  `inherit` (the default, if the cons cell is omitted) means that any `:key` values not specified are inherited from the "parent" customize variable.  `no-inherit` means to omit any missing key values.
2. For any non-`:key` type values, the specific value `'unspecified` can be set to specify using the parent's value for that slot.

For example, a setting of:

`indent-bars-ts-color = '(inherit unspecified :blend 0.15)` means to use the color from the parent variable `indent-bars-color`, set `:blend` to 0.15, and inherit any other missing keyword values from from `indent-bars-color` for out-of-scope bar coloring.

The easiest way to configure inheritance and unspecified values in the `ts` variables is via customize.

# Details and Caveats

## Speed

`indent-bars` was in part motivated by the inefficiency of older indentation highlight modes, and is designed for speed.  It uses stipples (fixed bitmap patterns) and font lock for fast and efficient bar drawing -— *faces on spaces*.  Highlighting the current indentation level is essentially free, since it works by [filtered remapping](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) the relevant face.

The heaviest operations are **tree-sitter** support (especially scope highlighting), and **blank-line highlighting**.  If you experience any speed issues, these are the first settings to experiment with.  Using with tab-based indentation is also slightly slower than with space-based.

Both indentation-depth highlighting and current-tree-sitter-scope highlighting are protected by timers to avoid unnecessary loads (e.g. when pixel-scrolling).  Note that indentation-depth highlighting is _very_ fast and can safely be set to 0 seconds (though bars will then flash very fast as you scroll).  Tree-sitter scope requires querying the tree-sitter core, which can be somewhat slower, so be careful setting its timer too low.

## Indentation

`indent-bars` works with either space- or tab-based indentation.  If possible, prefer space indentation, as it is faster.  Note that some modes explicitly enable or disable `indent-tabs-mode`.

## Current Depth Highlight

`indent-bars` can highlight the bar at the current depth, and supports a few different ways to determine which bar that is (`Variables indent-bars-highlight-selection-method`).  The simplest is selecting the last-visible bar on the current line.  The old default was `on-bar`, which selects the "unseen" bar that the text on the current line starts on.  The new default is `'context`, which selects the last-visible bar unless an adjacent no-blank line is indented deeper by at least one indent spacing, in which case the `on-bar` approach is used.  Experiment with these to see what you prefer.

## Tree-sitter

`indent-bars` can optionally use tree-sitter in supported files to add several significant features:

1. **Scope Highlighting**: The current tree-sitter scope can be configured by specifying matching node types.  The innermost node (covering sufficient lines) will then be rendered using the normal bar color and style.  Bars which are _out-of-scope_ have alternative styling applied.  See configuration, above.
1. **Blank Line Display**: It can be nice to omit the display of bars on blank lines at the top level (e.g. in a _module_).  Tree-sitter can help show those lines.
1. **Wrap Detection**: It can be useful to prevent the drawing of many bars in wrapped entities like argument lists or literal dictionaries.  Tree-sitter can help detect these and inhibit these unwanted bars (see also `indent-bars-no-descend-string/list`, which do not require tree-sitter).

> [!NOTE]
> Tree-sitter capabilities require Emacs 29 or later built with tree-sitter support, and the appropriate tree-sitter grammars installed for your languages of interest.  Additional node type configuration by language required; see below.

### Configuring tree-sitter

Simply configure `indent-bars-treesit-scope` with the node types for which "local scope" highlighting is of interest.  This must be done for each language you use.  This scope could be as granular as classes and functions, or including detailed block statements.   `indent-bars-treesit-wrap` can be configured in a similar manner, if desired.  I recommend starting with the minimal possible set of node types, adding as needed.

The easiest way to discover the node types of interest (in a buffer with working treesit support) is to `M-x treesit-explore-mode`. Then simply highlight the beginning of a line of interest, and look in the `treesitter explorer` buffer which pops up for the names of obvious nodes in the tree.  Add these types to `indent-bars-treesit-scope/wrap` for the language of interest, then `M-x indent-bars-reset` and see how you did.  You can do something similar for `indent-bars-treesit-ignore-blank-lines-types` (which, please note, are configured as _strings_, unlike in `indent-bars-treesit-wrap/scope`).

Please document good tree-sitter settings for other languages in the [Wiki](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Moving by columns

If `indent-bars-display-on-blank-lines` is set, the newline at the end of blank lines may have a `'display` property set to show the bars.  Emacs does not deal correctly with display properties containing newlines when moving by columns.  This is not normally a problem, but in one instance it is a nuisance: `evil-mode` tries to "preserve" column during line moves, so can trigger this emacs misfeature.  The symptom is that point jumps a line and moves over as you move down with evil.  A solution is [here](https://github.com/jdtsmith/indent-bars/issues/22#issuecomment-1793886072). 

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

This should then look something like (note the blue vertical bars):

<img width="668" alt="image" src="https://github.com/jdtsmith/indent-bars/assets/93749/dd0f65f5-3cdc-4865-a66d-41365cecadd0">

If you determine that stipples do not work in your version of Emacs, consider upgrading to a version which supports them, reporting the bug, or setting `indent-bars-prefer-character=t`.

#### Per-buffer stipple offsets

To get the stipple bars to appear in the correct location within their column, `indent-bars` must consider the starting horizontal pixel position of the current window, and "rotate" the stipple pattern accordingly.  It does this automatically, per buffer, so you shouldn't ever notice problems, even when re-sizing or re-arranging windows, changing font size, etc.  Until v0.5, showing the *same buffer* side by side in Emacs versions which support pixel-level window width/offsets could lead to unexpected bar artifacts, since the offset applies *per-buffer*, not per-window.  In v0.5, an alternate method for applying the stipple pattern was used to solve this.

### Character display

For terminals, (and everywhere, if `indent-bars-prefer-character` is set), `indent-bars` will not attempt stipple display, but instead use simple characters (e.g. `│`; see [an example](examples.md#in-terminal)).

Note that in mixed gui/terminal sessions of the same Emacs process, you need to `M-x indent-bars-reset` when switching a given buffer between graphical and terminal frames.

### Advantages/Disadvantages

#### Advantages of stipples

- Highly customized appearance and position within the character is possible — [examples](examples.md).
- Fastest option: does not need to apply display properties for normal lines with space-based indentation.
- Results in continuous lines even when `line-spacing` is non-nil (vs. gaps with box characters and additional line spacing).

#### Advantages of character bar display

- Works equally for terminal and GUI.
- Works even for emacs ports which do not support or mis-handle stipple display (see [Compatibility](#compatibility)).

# Related Packages

- [indent-guide](https://github.com/zk-phi/indent-guide): An older package that uses overlays with `|` characters.  Some reports of performance concerns.  Incompatible with company and other related in-buffer modes.
- [highlight-indentation-mode](https://github.com/antonj/Highlight-Indentation-for-Emacs): Uses overlays to draw indentation guides, and includes a current indentation mode.  Partial support for blank line guides.  
- [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides):  a highly configurable mode for indentation highlight, with color and style options, as well as current depth highlighting.
- [hl-indent-scope](https://codeberg.org/ideasman42/emacs-hl-indent-scope): Highlights indentation based on language scope - requiring support for each language, uses overlays to draw indentation guides.
- [visual-indentation-mode](https://github.com/skeeto/visual-indentation-mode): Full character-based alternating color indentation guides.  Package is now archived.

## Why a new package?

None of the existing packages:

1. were fast enough with large files (including current depth highlighting)
2. had enough guide appearance configurability
3. were able to support depth-based coloring
4. offered robust support for guides on blank lines
5. had tree-sitter capabilities

### Acknowledgments

I'm grateful for in-depth advice and input on the design of `indent-bars` from Eli Zaretski, Stefan Monnier, Dmitry Gutov and many other who opened issues and PRs.

[highlight-indentation-mode](https://github.com/antonj/Highlight-Indentation-for-Emacs) was a source of good ideas, and `indent-bars` adapts the indentation guessing function from this mode.  The original idea of using stipples for "better" indent-bars came from [this comment by @vlcek](https://github.com/antonj/Highlight-Indentation-for-Emacs/issues/16#issuecomment-48593300).
