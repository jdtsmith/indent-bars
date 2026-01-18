# indent-bars: fast, configurable indentation guide-bars for Emacs
<p align="center">
<a href="#faqs"><b>FAQ</b></a> ⏐
<a href="#installconfig"><b>INSTALL</b></a> ⏐
<a href="#customization"><b>CUSTOMIZE</b></a> ⏐
<a href="#details-and-caveats"><b>MORE DETAILS</b></a>
</p> <a href="https://elpa.gnu.org/packages/indent-bars.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/indent-bars.svg"/></a> 
<img align="right" width="500" src="https://github.com/jdtsmith/indent-bars/assets/93749/0eaa0d85-0893-4893-8a56-a63ab6eeac1c"/><img align="right" width="10" height="476" src="https://github.com/jdtsmith/indent-bars/assets/93749/c4df4fbe-7aab-4b4e-bb89-7c6a70755e9d"/>

This package provides indentation _guide bars_ in Emacs, with optional tree-sitter enhancement:

- Optimized for speed.
- Optional tree-sitter support, including _scope focus_, among [other features](#tree-sitter-details).
- Supports either space or tab-based indentation.
- Bar appearance is _highly_ [configurable](#customization): color, blending, width, position within the character, vertical fill/blank pattern, even zigzag (see [examples](examples.md)).
- Bars can have optional depth-based coloring, with a cyclical color palette you can [customize](#customization).
- Fast current-depth bar highlighting with configurable bar color and/or appearance changes.
- Bars can appear on blank lines.
- Bar depth can be held constant inside multi-line strings and lists.
- Works in the terminal, using a vertical bar character.

## What's New

See the release [NEWS](NEWS.org).

# FAQ's

## Bar Appearance

- **I don't see anything/bars are garbled!** <br>While most do, not all Emacsen support stipples; see [Compatibility](#compatibility).
- **How can I find out if my Emacs supports stipples?!** <br>See [Testing Stipples](#testing-stipples).
- **These bars are too intrusive!** <br>Reduce the `:blend` value in `indent-bars-color` closer to zero. Consider disabling `indent-bars-color-by-depth`.
- **I can barely see the bars!** <br>Increase the `:blend` value in `indent-bars-color` closer to one.
- **I want completely unique indent guide-bars so as to flex on my colleagues!** <br>Check the [Examples](examples.md) for some ideas.  The sky is the limit (submit your examples).
- **I use Emacs on the terminal, you insensitive clod!** <br>`indent-bars` will just work for you (though you don't get any fancy bar patterns).
- **I use graphical Emacs, but am an extreme minimalist.  All my outfits are gray.  Including my socks.** <br>Maybe [this](examples.md#minimal) will suit you?  Otherwise, you can turn off the stipple and use old fashioned `│` characters with [`indent-bars-prefer-character`](#character-display).
- **The current bar highlight is so fast, but it flashes too rapidly during scrolling!** <br>Update to v0.2.2 or later and set `indent-bars-depth-update-delay` to a comfortable number like 0.1s (0.075s is the default).  If you _like_ the crazy-fast updates, set this to 0.


## Bar Placement

- **I get too many bars inside function definitions/calls, and/or multi-line parenthesized expressions.** <br>You can turn on `indent-bars-no-descend-lists` or even use [tree-sitter to help](#tree-sitter-details).
- **I want a bar in the very first column!** <br>Set `indent-bars-starting-column` to 0.
- **indent-bars seems to be conflicting with another package I use.** <br>See [these workarounds](#compatibility-with-other-packages).
- **In my brace language (C, JS, etc.) I sometimes get fewer bars than I expected!** <br>Your mode syntax likely interprets `{`/`}` as list context, and you have `indent-bars-no-descend-lists=t`.  Either disable this feature, or see [this config](#bar-setup-and-location) for another option.
- **In my paren language (Elisp, Scheme, etc.) the bars disappear on some lines!**<br> You probably need to disable `indent-bars-no-descend-lists` or set it to `skip` there: almost all lines of these languages are inside nested "continuing lists".
- **Bars are missing on lines with tabs!**<br> You likely have `indent-tabs-mode` set to `nil` in a buffer with a tab-indented file.  See [this for more](#indentation).

## Tree-sitter and Scope

- **I turned on treesitter support but nothing happened** <br>You need to configure `indent-bars-treesit-scope` (and possibly `wrap`) for your language(s) of interest. [More info](#configuring-tree-sitter).
- **My treesitter scope makes no sense!** <br>A common mistake is adding too many node types for your language to the `indent-bars-treesit-scope` variable.  Start small, with thing you _know_ you want (function, method, class, etc.).
- **How can I change the style of the out-of-scope bars?** <br>Using an [alternate set](#tree-sitter-alternate-styling-variables) of `ts-` customizations.
- **What if I want out-of-scope text to have the default style, and in-scope text to be special?** <br>You want to set `indent-bars-ts-styling-scope` to `'in-scope`. 


# Install/config

`indent-bars` is on ELPA; simply install with Emacs' package facilities, and configure by calling `indent-bars-mode` in your desired mode hooks.

## Simple config

Simple default config using `use-package`:

```elisp
(use-package indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
```

## tree-sitter support

> [!IMPORTANT]  
> `treesitter` users: `indent-bars` needs your help!  If you have come up with good settings for treesitter "wrap" and "scope" for your favorite languages, please add them to the [Wiki](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config)!  If we get a large enough collection they may be included as defaults.

Configures `tree-sitter` and `ignore-blank-line` support for an example language.

```elisp
(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists 'skip) ; prevent extra bars in nested lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))
```

See [tree-sitter](#tree-sitter-details), and also the [Wiki page](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Compatibility 

> [!IMPORTANT]
> For `indent-bars` to display fancy guide bars, your port and version of emacs must correctly display the `:stipple` face attribute.  **Most do**, but some do not.


Known `:stipple` support, by Emacs build:

- Linux:
  - "Pure GTK" (`--with-pgtk` build flag) versions support stipples starting with Emacs v30.  There was a display bug that caused them to appear incorrectly (as [reverse video](../../issues/3)) and lead to [crashes](../../issues/6) in Emacs 29 and earlier; these issues were fixed in Emacs [here](https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-07/msg02081.html) and released with Emacs 30.
  - Cairo builds (`--with-cairo`, the default) have been [reported](../../issues/33#issuecomment-1768888990) not to display stipples (but only in [some cases](https://github.com/jdtsmith/indent-bars/issues/54#issuecomment-2330334476)).  You can try building `--without-cairo` or just omitting `--with-cairo` (which still enables Cairo but may have correct stipple display).  Also, the issue may be encountered [only on high-DPI systems](../../issues/97).
  - All other builds support stipples.
- Mac:
  - The [emacs-mac](https://bitbucket.org/mituharu/emacs-mac/src/master/)[^1] port has stipple support.  `M-x version` should say `Carbon`, not `NS`.
  - The `NS` build has partial stipple support in master in Emacs v30.  A patch providing [full stipple support](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=73384) for NS has been merged, and should be released with v31.
- Windows: Emacs on Windows will support stipples starting at v30.
- Android: Android builds (to appear with Emacs 30) support stipples.
- Haiku: Haiku Emacs builds will support stipples starting with v30.
- Terminal: Stipples are not supported on terminal emacs.  Character display is automatically selected instead.

[^1]: Most easily installed [with brew](https://github.com/railwaycat/homebrew-emacsmacport).

Please [open an issue](../../issues) with any updates/corrections to this list.  See also [Testing Stipples](#testing-stipples).

`indent-bars` can also be used *without stipples*, drawing a simple vertical character (like `│`) instead.  It automatically does this in non-graphical displays (terminals), but this can be made the default; see [Character Display](#character-display).

# Customization

> [!NOTE]
> `indent-bars` is _highly_ flexible and can be adapted to most situations.  It can't anticipate all nuances of different languages, modes, and user preferences, however — "some assembly may be required".  If you arrive at customizations you are happy with for a given mode, please consider adding to the  [Wiki page](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).


`M-x customize-group indent-bars` is the easiest way to customize everything about the appearance and function of `indent-bars` (check sub-groups too).  There are many customization variables and bar styling in particular is highly configurable, so use Customize!

> [!TIP]
> The easiest way to achieve a particular style is to customize the groups `indent-bars`, sub-group `indent-bars-style` and (if you use TS) `indent-bars-ts` + `indent-bars-ts-style`.  While in the Customize interface, pull up one of your buffers with bars in another window on the same frame.  When you make changes to variables (`C-c C-c` is convenient in custom buffers), the bar style/etc. will automatically update.  When you are happy, you can either "Set for Future Sessions", or "Show Saved Lisp Expression" for the variables you changed and copy them into your init file.

See some [examples](examples.md) with relevant settings.

The main customization variables are categorized below.  See the documentation of each variable for more details on the valid values.

## Bar colors

Custom variables for configuring bar color, including depth-based palettes:

- `indent-bars-color`: The main bar color, either a color name or face, from which foreground or background color will be taken.  Also used to set a `:blend` factor, to blend colors into the frame's background color.
- `indent-bars-color-by-depth`: How and whether to alter the color of the indent bars by their indentation depth.  Defaults to using the foreground of the `outline-*` faces, but many options are possible, including face sets or a custom color palette.

## Bar shape and size

Variables affecting the visual appearance of bars (color aside):

- `indent-bars-width-frac`: The fractional width of the bar ([0-1], a _fraction_ of a single character's width).
- `indent-bars-pad-frac`: The fractional padding offset of the bar from the left edge of the character. 
- `indent-bars-pattern`: A string specifying the vertical structure of the bar (space=blank, non-space=filled).  Scaled to the height of one character.
- `indent-bars-zigzag`: A fractional left-right *zigzag* to apply to consecutive groups of identical non-space characters in `pattern`.

## Current Depth highlighting

Configuration for highlighting the current indentation bar depth:

- `indent-bars-highlight-current-depth`: How and whether to highlight the bars at the indentation depth of the current line.  The current depth bar can change color (including blending with the pre-existing color), as well as appearance (size, pad, pattern, zigzag).
- `indent-bars-highlight-selection-method`: Method used to select which bar is highlighted as the current depth.  The default (`'context`) considers surrounding lines for a more natural selection depth.
- `indent-bars-depth-update-delay`: Command delay in seconds after which depth highlighting occurs. 

## Bar setup and location

Configuration variables for bar position and line locations (including on blank lines):

- `indent-bars-starting-column`: column to use for the first bar (default: one indent spacing).  Can be set in special modes which start at an unusual fixed offset, or set to 0 to get "column 0" bars (which are possibly superfluous given the left buffer edge).
- `indent-bars-spacing-override`:  Normally the number of spaces for indentation is automatically discovered from the mode and other variables.  If that doesn't work for any reason, it can be explicitly overridden using this variable.
- `indent-bars-display-on-blank-lines`: Whether to display bars on blank lines contiguous with lines already showing bars.  By default the maximum number of adjacent bars on non-blank lines is used for a blank lines, but setting this to `least` instead uses the _least_ number of adjacent line bars.
- `indent-bars-no-descend-string`: Whether to inhibit increasing bar depth inside of strings.
- `indent-bars-no-descend-list`: Whether to inhibit increasing bar depth inside of lists.  Additionally, if set to the symbol `skip`, bars _between_ lists contexts are skipped (not displayed). 
- If you need to alter what `indent-bars` considers a list context, override the variable `indent-bars-ppss-syntax-table`, e.g. for altering `python-mode` to omit `{`/`}` from consideration:

   ```elisp
    (setq indent-bars-ppss-syntax-table
          (let ((table (make-syntax-table python-mode-syntax-table)))
            ;; Remove { and } from list context
            (modify-syntax-entry ?\{ "." table)
            (modify-syntax-entry ?\} "." table)
            table))
    ```

## Character-based bars and terminal

Custom variables affecting character-based bar display, e.g. in the terminal:

- `indent-bars-prefer-character`: Use *characters* to display the vertical bar instead of stipples.  This occurs automatically on non-graphical displays (terminals), but this variable can be used to always prefer character-based display.  Useful if your version of GUI Emacs does not support `:stipple` patterns.
- `indent-bars-no-stipple-char`: The character to use when stipples are unavailable or disabled. Defaults to the vertical box character `│`.  Other good options include `┃`, `┋`, and `║`.  Note that characters in emacs (e.g. in a `setq` command) are specified with a leading `?`, e.g. `?│`.
- `indent-bars-no-stipple-char-font-weight`: Optional font weight to use for the face displaying the no-stipple character.
- `indent-bars-unspecified-bg|fg-color`: Colors to use for the frame background and default foreground when they are unspecified (e.g. in terminals).  If you intend to use `indent-bars` in the terminal, set to the terminal background/foreground colors you use. 

## Tree-sitter

For more information, check [the details](#tree-sitter-details).

### Main treesitter configuration variables

- `indent-bars-treesit-support`: Whether to use tree-sitter (if available) to (optionally) highlight the current scope and help determine bar depth.
- `indent-bars-treesit-scope`: A mapping of language to tree-sitter scope node types (as symbols), for local scope highlight (aka _scope focus_).
- `indent-bars-treesit-scope-min-lines`: The minimum number of lines a scope node must occupy to be considered a valid containing scope.
- `indent-bars-treesit-update-delay`: Delay in seconds for updating the treesitter scope highlight.
- `indent-bars-treesit-wrap`: A mapping of language to tree-sitter wrap types (as symbols), to avoid adding extra bars e.g. in wrapped function arguments.  Note that this is considered only after the `no-descend` options above (which may be sufficient on their own).
- `indent-bars-treesit-ignore-blank-lines-types`: A list of tree-sitter node types (as strings) inside of which to inhibit styling blank lines, like "module". 
- `indent-bars-ts-styling-scope`: Determine whether the `*-ts-*` variables apply to in-scope or (by default) out-of-scope styling.  This is important because one of these styles is shared with the bar style in non-TS buffers.  This allows the default style in non-TS buffers to match either the in-scope (default) or out-of-scope styling.

### Tree-sitter alternate in-scope/out-of-scope styling variables

By default, if tree-sitter and _scope focus_ are active (`indent-bars-treesit-scope`), the style and highlight settings above apply only to the _in-scope_ bars (or visa versa if `indent-bars-ts-styling-scope` is set to `in-scope`). You can separately configure an alternate style for the appearance of the _out-of-scope_ bars — i.e. the bars outside the current tree-sitter scope.  Usually you'd want to de-emphasize out-of-scope bars somehow, but that's not required (go crazy).

To customize the alternate bar appearance, you use the parallel set of custom variables with an `indent-bars-ts-` prefix.  Each of these variables can be set similarly to their default counterparts to _fully_ configure alternate bar appearance, including color, depth highlighting, bar pattern, etc.

You can interchange the role of in-scope and out-of-scope using `indent-bars-ts-styling-scope`.  This is useful if you prefer to have the _default_ style (e.g. the bar style in non-tree-sitter-enabled buffers) match the out-of-scope style within tree-sitter buffers (i.e. if you want to _emphasize_ bars within scope, not _de-emphasize_ out-of-scope bars).

> [!NOTE]
> _Scope focus_ highlighting is completely independent of _current depth highlighting_, and you can style them separately, and can enable one or the other, both, or neither.

The `ts` custom variables for configuring the alternate styling are:

- [I] `indent-bars-ts-color` 
- `indent-bars-ts-width-frac`
- `indent-bars-ts-pad-frac`
- `indent-bars-ts-pattern`
- `indent-bars-ts-zigzag`
- `indent-bars-ts-no-stipple-char-font-weight`
- [I] `indent-bars-ts-color-by-depth`
- [I] `indent-bars-ts-highlight-current-depth`

Each of these parallel variables has the same form as their equivalent non-`ts` version (the "parent" variable), with two additions:

1. Some (marked with [I] above) can optionally use _inheritance_ from their parent.  Inheritance means any missing `:key`-based elements are _inherited_ from the parent style.  To configure whether inheritance happens, you can optionally set these `[I]` variables to a cons cell of the form `([no-]inherit . value)`, where `value` has the normal format for the parent variable.  `inherit` (the default, if the cons cell is omitted and `value` is simply used as-is) means that any unspecified `:key` values are inherited from the parent variable.  The symbol `no-inherit` means to omit (rather than inherit) any missing key values for the alternate styling.  See below for an example of using/overriding inheritance.
2. For any non-`:key` type values, the specific symbol value `'unspecified` can be set to indicate using the parent's value for that slot.

For example, a setting of:

```elisp
(setopt indent-bars-ts-color '(inherit unspecified :blend 0.15))
```

means to configure the color of the alternate style bars as follows:

1. use the color from the parent variable `indent-bars-color` (since it is `unspecified` here)
2. set `:blend` to 0.15
3. inherit any other missing keyword values from `indent-bars-color`

The easiest way to configure inheritance and unspecified values in the `ts` variables is via the customize interface; see the customize group `indent-bars-ts-style`. 

## Per major-mode customization

Sometimes different `indent-bars` settings are appropriate for different major modes.  In that case, you can use `setq-local` to set the appropriate cutomize variables _locally_, directly in the mode's hook, prior to enabling `indent-bars`:

```elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq-local indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
			indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
	    (indent-bars-mode 1)))
```

Note that tree-sitter scope and wrap config are keyed to the parser _language_, which may be sufficient for tailoring these (i.e. no buffer-local values needed).

# Details and Caveats

## Indentation

`indent-bars` works with either space- or tab-based indentation (see `indent-tabs-mode`).  If possible, prefer space indentation, as it is faster.

Note that some modes explicitly enable or disable `indent-tabs-mode`.  If the value of that variable does not match the actual indentation used in a file (e.g. file is indented with tabs, but you have set `indent-tabs-mode=nil`), bars may go missing.  You should ideally pick _one_ indentation-style (tabs or spaces) per mode and stick to it for all files in that mode, but see [dtrt-indent](https://github.com/jscheid/dtrt-indent) for a package that can adapt this variable by examining the file's contents.

## Current Depth Highlight

`indent-bars` can highlight the bar at the current depth, and supports a few different ways to determine which bar gets selected for highlight based on point.  To configure this behavior, you can set `indent-bars-highlight-selection-method` to:

1. `nil`: The simplest version selects the depth of the last-visible bar on the current line for highlight.
2. `on-bar`:  The old default, which selects the "unseen" bar that the very first non-blank character on the current line "covers up", or if no such bar exists, the last visible bar.
3. `context`: The new default, a blend of these two methods.  It selects the last-visible bar (à la `nil`), _unless_ an adjacent non-blank line (above or below) is indented deeper by at least one level, in which case the `on-bar` approach is used.

Experiment with these to see which you prefer.

## Tree-sitter Details

`indent-bars` can optionally use `tree-sitter` in supported files to enable several features:

1. **Scope Focus**: The current `tree-sitter` scope can be _focused_, with out-of-scope bars de-emphasized or in-scope bars emphasized (or actually, styled however you want).  This can be configured by [specifying matching "scope"](#configuring-tree-sitter) node types (e.g. functions, blocks, etc.) for each language of interest.  The innermost node (covering sufficient lines) will then be rendered distinctly from _out-of-scope_ bars.
1. **Selective Blank Line Display**: By default, `indent-bars` displays bars on blank lines (though this can be [configured](#bar-setup-and-location)), so that they remain continuous.  It can be nice to omit the display of blank lines bars at the top structural level (e.g. in a _module_), to make divisions between top-level constructs more visible.  Tree-sitter can help `indent-bars` identify those lines.
1. **Wrap Detection**: It can be useful to prevent excess bars inside wrapped entities which alter indent to "line things up." These include things like argument lists, literal dictionaries, or other heirarchical multi-line structures.  Tree-sitter can help detect these and inhibit unwanted bars (but [see also](#bar-setup-and-location) `indent-bars-no-descend-string/list`, which do not require tree-sitter).

> [!NOTE]
> `indent-bars`' tree-sitter capabilities require Emacs 29 or later built with tree-sitter support, and the appropriate tree-sitter grammars installed for your languages of interest.  Additional node type configuration by language is required; see below.

### Configuring tree-sitter

#### Scope
Simply configure `indent-bars-treesit-scope` with the languages and node types for which "local scope" highlighting nodes are of interest.  This must be done for each tree-sitter language you use.  This scope could be as granular as classes and functions, or include detailed block statements.  You can disable scoping for "short blocks" using `indent-bars-treesit-scope-min-lines`, so that, e.g., a quick `if` statement does not capture scope. I recommend starting with the minimal possible set of scope node types, adding as needed.

> [!TIP]
> If you don't know the name treesitter uses for your language, try `M-: (treesit-language-at (point-min))` in a ts-enabled buffer.

#### Wrap
`indent-bars-treesit-wrap` can be configured in a similar manner (mapping language to wrapping node types). Note that `indent-bars-no-descend-list`, which does not require tree-sitter and is on by default, may be sufficient for your uses.

#### Ignore certain blank lines
You can assign a single (usually top-level) node type to ignore when drawing bars on blanks linkes; see `indent-bars-treesit-ignore-blank-lines-types` (which, please note, is configured as a list of _strings_, unlike `indent-bars-treesit-wrap/scope`).

#### Identifying treesit node types of interest
The easiest way to discover node types of interest (in a buffer with working treesit support) is to `M-x treesit-explore-mode`. Then simply highlight the beginning of a line of interest, and look in the `treesitter explorer` buffer which pops up for the names of obvious nodes in the tree.  Add these types to `indent-bars-treesit-scope/wrap` for the language of interest, then `M-x indent-bars-reset` and see how you did (this will happen automatically if you make the change in the Customize interface).

Please document good tree-sitter settings for other languages in the [Wiki](https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config).

## Compatibility with other Packages

`indent-bars` in general has good compatibility with other packages.  But sometimes conflicts do occur.

### `org-mode` src blocks

In general, `org-mode` src blocks are difficult for many modes to support.  Org actually offsets the indentation of the src contents, copies that text to a special hidden buffer, and then maps all `face` properties (only)) back to the original buffer.  This doesn't work well for `indent-bars` for a few reasons:

- the bars will be offset relative to your expectation by the "extra" indentation org applies.
- stipple bars may no be correctly formatted (though this could be worked around).
- Any `display` properties, e.g. for blank line bar display or bars on tab chars, will not be transferred.

So it is best to disable `indent-bars` in `org` src blocks.  You can achieve this by inhibiting all the `mode-hooks` from running in org's special hidden fontification buffers (one per mode).  E.g., for python:

```elisp
  (defun my/org-simple-python-mode ()
    (if (string-prefix-p " *org-src-fontification:" (buffer-name))
	(delay-mode-hooks (python-mode))
      (python-mode)))
  (setf (alist-get "python" org-src-lang-modes) 'my/org-simple-python)
```

This will inhibit hooks (and hence `indent-bars`, `eglot`, `flymake`, whatever else you have setup) from running in the special ` *org-src-fontification:..` buffers (where they are either harmful or not needed), but these features will still be loaded and work when editing src block contents with `C-c '`. 

### Unwanted `:stipple` inheritance on popups/overlays/etc.

`indent-bars` by default uses `:stipple` face attributes, which have only rarely been used in Emacs in recent decades.  Consequently, some packages which inherit the face of underlying text while adding styled overlays, popups, etc. to the buffer neglect to guard against the presence of `:stipple` (e.g. [this](../../issues/67), or [this](../../issues/73)).  This becomes more likely if you set `indent-bars-starting-column=0` (since often overlays are placed at the line beginning).

If you encounter unwanted bar patterns on text added to your buffer by other packages as seen in these issues, contact the package's maintainer to let them know they should also clear the `:stipple` face attribute.  You can also try restoring `indent-bars-starting-column` to the default, if you've changed it.

Sometimes unwanted stipples can be worked around yourself by explicitly setting `:stipple` to `nil` in appropriate faces, like:

```elisp
(set-face-attribute face nil :stipple nil)
```

for some relevant `face` (e.g. one from which the package's faces used for overlay/popup inherit).  This should be done both when loading `indent-bars-mode` and in the `after-enable-theme-hook`. 

### `font-lock` contention

`indent-bars` overrides and wraps the `font-lock-fontify-region-function` (and, when using treesitter, `font-lock-fontify-buffer-function`).  Other packages which advise or wrap the functions pointed to by these variables may lead to odd behavior on disabling/re-enabling `indent-bars` and/or their associated modes.  There is no generic solution to this issue, but the strong recommendation is to enable `indent-bars` _last_, after any other package which overrides `font-lock` in this way have been loaded.

## Moving by columns

If `indent-bars-display-on-blank-lines` is set, the newline at the end of blank lines may have a `'display` property set to show the bars.  Emacs does not deal correctly with display properties containing newlines when moving by columns.  This is not normally a problem, but in one instance it is a nuisance: `evil-mode` tries to "preserve" column during line moves, so can trigger this emacs misfeature.  The symptom is that point jumps a line and moves over as you move down with evil.  A workaround is [here](https://github.com/jdtsmith/indent-bars/issues/22#issuecomment-1793886072). 

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

If you determine that stipples do not work in your version of Emacs, consider upgrading to a [version which supports them](https://github.com/jdtsmith/indent-bars/blob/main/README.md#compatibility), reporting the bug, or setting `indent-bars-prefer-character=t`.

#### Per-buffer stipple offsets

To get the stipple bars to appear in the correct location within their column, `indent-bars` must consider the starting horizontal pixel position of the current window, and "rotate" the stipple pattern accordingly.  It does this automatically, per buffer, so you shouldn't ever notice problems, even when re-sizing or re-arranging windows, changing font size, etc.  Until v0.5, showing the *same buffer* side by side in Emacs versions which support pixel-level window width/offsets could lead to unexpected bar artifacts, since the offset applies *per-buffer*, not per-window.  In v0.5, an alternate method for applying per-window stipple patterns was used to solve this.

### Character display

For terminals, (and everywhere, if `indent-bars-prefer-character` is set), `indent-bars` will not attempt stipple display, but instead use simple characters (e.g. `│`; see [an example](examples.md#in-terminal)).

Note that in mixed gui/terminal sessions of the same Emacs process, you may need to `M-x indent-bars-reset` when switching a given buffer between graphical and terminal frames.

### Advantages/Disadvantages

#### Advantages of stipples

- Highly customized appearance and position within the character is possible — [examples](examples.md).
- Fastest option: does not need to apply display properties for normal lines with space-based indentation.
- Results in continuous lines even when `line-spacing` is non-nil (vs. gaps with box characters and additional line spacing).

#### Advantages of character bar display

- Works equally for terminal and GUI.
- Works even for emacs ports which do not support or mis-handle stipple display (see [Compatibility](#compatibility)).

## Speed

`indent-bars` was in part motivated by the inefficiency of older indentation highlight modes, and is designed for speed.  It uses stipples (fixed bitmap patterns) and font-lock for fast and efficient bar drawing — simply *faces on spaces*.  Highlighting the current indentation level is essentially free, since it works by [filtered remapping](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) of the relevant face.

The heaviest operations are **tree-sitter** support (especially scope highlighting), and **blank-line highlighting**.  If you experience any speed issues, these are the first settings to experiment with. Using with tab-based indentation may also be slightly (but likely imperceptibly) slower than with space-based.

Both indentation-depth highlighting and current-tree-sitter-scope highlighting are protected by timers to avoid unnecessary loads (e.g. when pixel-scrolling).  Note that indentation-depth highlighting is _very_ fast and can safely be set to 0 seconds (though bars will then flash rapidly as you scroll).  Tree-sitter scope requires querying the tree-sitter core, which can be somewhat slower, so be careful setting its timer too low.

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
