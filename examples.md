# Example `indent-bars` color/layout possibilities
## Old Default:
<img width="514" alt="ib_default" src="https://github.com/jdtsmith/indent-bars/assets/93749/4f652554-bede-4aa6-bdbc-233ec843d782">

``` emacs-lisp
   (setq
    indent-bars-color '(highlight :face-bg t :blend 0.3)
    indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
    indent-bars-width-frac 0.25
    indent-bars-pad-frac 0.1)
```

## Minimal:
<img width="514" alt="ib_minimal" src="https://github.com/jdtsmith/indent-bars/assets/93749/e21da2d6-f6a4-4587-9640-d6a493111473">

``` emacs-lisp
   (setq
    indent-bars-color '(highlight :face-bg t :blend 0.2)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth nil
    indent-bars-display-on-blank-lines nil)
```

## Zebra:
<img width="514" alt="ib_zebra" src="https://github.com/jdtsmith/indent-bars/assets/93749/8cd209d2-c2ce-46e3-98a6-2e286f8ab8c5">

```emacs-lisp
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.25)
   indent-bars-pattern ". .. .  "
   indent-bars-zigzag nil
   indent-bars-width-frac 0.4
   indent-bars-pad-frac 0.1
   indent-bars-color-by-depth '(:palette ("black" "white") :blend 0.65)
   indent-bars-highlight-current-depth '(:color "red" :blend 0.15))
```

## Simple:
<img width="514" alt="ib_simple" src="https://github.com/jdtsmith/indent-bars/assets/93749/94094b4c-a088-4672-b57e-d88dbadb28cc">

```emacs-lisp
   (setq
    indent-bars-pattern "."
    indent-bars-width-frac 0.5
    indent-bars-pad-frac 0.25
    indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth '(:face default :blend 0.4))
```

## Zig-zag:
<img width="514" alt="ib_zz" src="https://github.com/jdtsmith/indent-bars/assets/93749/841dc464-8cb3-4b29-bb3c-1f7d31101800">

```emacs-lisp
  (setq
   indent-bars-pattern ". . . . "
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45))
```
## Zig-zag Smooth:
<img width="514" alt="zz_smooth" src="https://github.com/jdtsmith/indent-bars/assets/93749/645cb211-63cf-44c0-9e72-ff58f1ad5039">

```emacs-lisp
(setq
 indent-bars-pattern ".*.*.*.*.*.*.*.*"
 indent-bars-width-frac 0.25
 indent-bars-pad-frac 0.2
 indent-bars-zigzag 0.1
 indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.5)
 indent-bars-highlight-current-depth '(:face default :blend 0.7))
```

## Background zig-zag:
<img width="514" alt="ib_bgzz" src="https://github.com/jdtsmith/indent-bars/assets/93749/01cb0624-5185-425c-96a1-19f1d632c07d">

```emacs-lisp
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern ".*.*.*.*"
   indent-bars-width-frac 0.5
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:palette ("red" "green" "orange" "cyan") :blend 1)
   indent-bars-highlight-current-depth '(:background "gray10"))
```

## Minimal colorpop:
Showcasing `:blend`-only current coloring.

<img width="602" alt="image" src="https://github.com/jdtsmith/indent-bars/assets/93749/ef02e099-798c-4eb7-947a-6fe144a9104d">

```emacs-lisp
(setq
    indent-bars-color '(highlight :face-bg t :blend 0.15)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
    indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
    indent-bars-display-on-blank-lines t)
```

## In terminal:
<img width="505" alt="ib_term" src="https://github.com/jdtsmith/indent-bars/assets/93749/d2f51fa7-5993-4c34-93b2-effef32a469d">

```emacs-lisp
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.75)
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-unspecified-fg-color "white"
   indent-bars-unspecified-bg-color "black")"
```

Note: `indent-bars-prefer-character` need not be set unless you prefer to use character display in GUI as well.
