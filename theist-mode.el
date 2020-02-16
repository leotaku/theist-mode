;;; theist-mode.el --- A simpler alternative to god-mode -*- lexical-binding: t -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 03 July 2019
;; Homepage: https://github.com/leotaku/theist-mode
;; Keywords: god-mode, configuration, lisp
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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
;; 

;;; Code:

;;;###autoload
(defun theist-C-x (arg)
  "Enter theism with `C-x' as the prefix."
  (interactive "P")
  (setq prefix-arg arg)
  (theist-run
   "\C-x"
   (char-to-string last-command-event)))

;;;###autoload
(defun theist-C-c (arg)
  "Enter theism with `C-c' as the prefix."
  (interactive "P")
  (setq prefix-arg arg)
  (theist-run
   "\C-c"
   (char-to-string last-command-event)))

;;;###autoload
(defun theist-run (prefix-keys prefix-string)
  "Enter theism with PREFIX-KEYS as the prefix key sequence and
PREFIX-STRING as the prefix displayed in the minibuffer.
Transformations are read from the `theist-transformations' special variable."
  (unless (theist--keys-toplevel prefix-keys prefix-string)
    (message "found no applicable key sequence")))

(defvar theist-transformations
  '(identity theist-transform-C)
  "Transformations for `theist-run' keys.")

(defun theist-format-key (format key)
  "Format the given internal KEY description with FORMAT.
Returns an internal key description."
  (kbd (format format (key-description key))))

(defun theist-transform-C (key)
  "Transform the given KEY to C-KEY."
  (theist-format-key "C-%s" key))

(defun theist--keys-toplevel (prefix-keys prefix-string &optional recursive)
  (let* ((read-key (vector (read-char (format "%s-" prefix-string))))
         (new-string (concat prefix-string " " (key-description read-key))))
    (cl-loop
     for transform in theist-transformations
     do
     (let* ((key (funcall transform read-key))
            (keys (vconcat prefix-keys key))
            (action (theist--lookup-global keys)))
       (cond
        ((keymapp action)
         (if (not recursive)
             (theist--fi-simulate-key keys)
           (if (theist--keys-recursive keys new-string)
               (cl-return t)
             (cl-return nil))))
        ((functionp action)
         (theist--fi-simulate-key keys)
         (cl-return t)))))))

(defun theist--lookup-key (keyseq keymaps)
  (cl-dolist (keymap keymaps)
    (when keymap
      (let ((key (lookup-key keymap keyseq)))
        (when (and key (not (numberp key)))
          (cl-return key))))))

(defun theist--lookup-global (keys)
  (let ((maps (list key-translation-map (current-active-maps t))))
    (theist--lookup-key keys maps)))

(defun theist--fi-simulate-key (key &optional keymap)
  "Send fake keypresses for KEY in KEYMAP.
KEY should be a key sequence in internal Emacs notation.

Extracted from fi-emacs."
  (let ((overriding-local-map (or keymap global-map)))
    (setq unread-command-events
          (nconc
           (mapcar (lambda (ev) (cons t ev))
                   (listify-key-sequence key))
           unread-command-events))))

(provide 'theist-mode)

;;; theist-mode.el ends here
