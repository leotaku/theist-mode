;;; theist-mode.el --- A simpler alternative to god-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 03 July 2019
;; Homepage: https://github.com/leotaku/theist-mode
;; Keywords: emulation convenience modal god-mode
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The theist-mode package provides some simplistic utilities for
;; automatically making existing keybindings more ergonomic.  It is
;; heavily inspired by the now unmaintained god-mode, but has a higher
;; focus on simplicity and easy integration with existing modal
;; keybinding packages (e.g. evil, modalka, lispy, worf).
;;
;; Please consult README.md from the package repository and elisp
;; docstrings for more thorough documentation.

;;; Code:

;;;###autoload
(defun theist-C-x (&optional arg)
  "Enter theism with \\`C-x' as the prefix.
ARG is treated as a prefix argument."
  (interactive "P")
  (setq this-command last-command)
  (setq prefix-arg arg)
  (theist-run (kbd "C-x") "x"))

;;;###autoload
(defun theist-C-c (&optional arg)
  "Enter theism with \\`C-c' as the prefix.
ARG is treated as a prefix argument."
  (interactive "P")
  (setq this-command last-command)
  (setq prefix-arg arg)
  (theist-run (kbd "C-c") "c"))

;;;###autoload
(defun theist-run (prefix-keys prefix-string)
  "Enter theism with the given PREFIX-KEYS and PREFIX-STRING.

This allows entering theism from any function call, but has a few
disadvantages as a result of abusing `unread-command-events'.  As
such, for advanced users, it is recommended to bind the result of
the corresponding `theist-menu' call whenever possible.

When not using the predefined commands, it is recommended to set
`prefix-arg' to the argument given to calling command, as well as
`this-command' to `previous-command'.  Consult the implementation
of `theist-C-x' for further guidance.

Key transformations are read from the `theist-transformations'
special variable."
  (let ((keys (listify-key-sequence (kbd prefix-string)))
        (map (make-sparse-keymap)))
    (define-key map (kbd prefix-string) (theist-full-remap prefix-keys))
    (set-transient-map map (lambda () (memq this-command '(which-key-C-h-dispatch))))
    (setq unread-command-events (nconc unread-command-events keys))))

;;;###autoload
(defun theist-menu (prefix-keys)
  "Return an extended menu item to enter theism for PREFIX-KEYS.

This invocation method can only work when the resulting menu will
be bound directly to a key or event.  Whenever theism has to be
entered from a function call, prefer `theist-run'.

Key transformations are read from the `theist-transformations'
special variable."
  `(menu-item ,(format "theist-%s" (key-description prefix-keys)) ,prefix-keys
              :filter (lambda (&optional _)
                        (theist-full-remap ,prefix-keys))))

(defvar theist-transformations
  '(identity theist-transform-C)
  "Transformations for `theist-map' keys.")

(defun theist-transform-C (key)
  "Transform the given C-KEY to KEY."
  (let ((desc (key-description key)))
    (cond
     ((equal desc "RET") (kbd "m"))
     ((equal desc "TAB") (kbd "i"))
     (t (kbd (string-remove-prefix "C-" desc))))))

(defun theist-remap (map &optional transform)
  "Generate a new keymap from MAP by transforming all keys in MAP
and its child keymaps using TRANSFORM."
  (let* ((transform (or transform #'identity))
         (target (make-sparse-keymap))
         (fn (lambda (key def)
               (let ((key (funcall transform (vector key)))
                     (def (if (keymapp def) (theist-remap def transform) def)))
                 (define-key target key def)))))
    (prog1 target
      (map-keymap fn (keymap-canonicalize map)))))

(defun theist-full-remap (prefix-keys)
  "Generate a new keymap from the keymap bound to PREFIX-KEYS by
transforming all keys in the keymap and its child keymaps using
the transformations stored in `theist-transforms'."
  (let ((fn (lambda (it) (theist-remap (key-binding prefix-keys t nil (point)) it))))
    (make-composed-keymap (mapcar fn theist-transformations))))

(provide 'theist-mode)

;;; theist-mode.el ends here
