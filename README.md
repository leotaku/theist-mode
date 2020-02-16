# theist-mode.el

`theist-mode` is intended to be an improvement over the unmantained `god-mode` for Emacs.

# Usage

Simply bind `theist-C-x` and/or `theist-C-c` in your modal maps.

The author uses them in their [modalka](https://github.com/mrkkrp/modalka), [lispy](https://github.com/abo-abo/lispy) and [worf](https://github.com/abo-abo/worf) maps.

# Customization

You can define your own theist commands easily.

```emacs-lisp
(defun theist-C-c (arg)
  "Enter theism with `C-c' as the prefix."
  (interactive "P")
  (setq prefix-arg arg)
  (theist-run
   (kbd "C-c")
   (char-to-string last-command-event)))
```

You can also define your own transformations.

``` emacs-lisp
(defun theist-transform-C (key)
  "Transform the given KEY to C-KEY."
  (theist-format-key "C-%s" key))
```

And bind them locally or globally.

``` emacs-lisp
(setq theist-transformations
      '(identity theist-transform-C))

(let ((theist-transformations
       '(identity theist-transform-C)))
  ...)
```

