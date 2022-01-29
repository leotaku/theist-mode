# theist-mode.el

The theist-mode package provides some simplistic utilities for automatically making existing keybindings more ergonomic.
It is heavily inspired by the now unmaintained god-mode, but has a higher focus on simplicity and easy integration with existing modal keybinding packages (e.g. evil, modalka, lispy, worf).

## Usage

Simply bind `theist-C-x` and/or `theist-C-c` in your modal maps.

For example, I personally bind either both or one of these commands in my [modalka](https://github.com/mrkkrp/modalka), [lispy](https://github.com/abo-abo/lispy) and [worf](https://github.com/abo-abo/worf), but also dired, ibuffer and [magit](https://github.com/magit/magit) keymaps.

## Customization

You can define your own theist commands easily.

```emacs-lisp
(defun theist-C-c (&optional arg)
  "Enter theism with `C-c' as the prefix.
ARG is treated as a prefix argument."
  (interactive "P")
  (setq this-command last-command)
  (setq prefix-arg arg)
  (theist-run (kbd "C-c") "c"))
```

When binding directly to a keymap, you can also directly use `theist-menu`, which requires less boilerplate and will be more consistent in certain edge cases.

```emacs-lisp
(define-key some-mode-map (kbd "x") (theist-menu (kbd "C-x")))
```

You can also define your own key transformations.

```emacs-lisp
(defun theist-transform-C (key)
  "Transform the given C-KEY to KEY."
  (kbd (string-remove-prefix "C-" (key-description key))))
```

Binding `theist-transformations` globally or locally in your commands allows you to customize how keys are interpreted.

```emacs-lisp
(setq theist-transformations
      '(identity theist-transform-C))

(let ((theist-transformations
       '(identity theist-transform-C)))
  ...)
```

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright notices are included with all individual files of source code.
