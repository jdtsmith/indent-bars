# Example `indent-bars` color/layout possibilities
## Default:

## Minimal:

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

## Stripes:

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

```emacs-lisp
   (setq
    indent-bars-pattern "."
    indent-bars-width-frac 0.5
    indent-bars-pad-frac 0.25
    indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth '(:face default :blend 0.4))
```

## Zig-zag:

```emacs-lisp
  (setq
   indent-bars-pattern ". . . . "
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45))
```

## Background zig-zag:

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