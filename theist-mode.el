;;; theist-mode.el --- A better god-mode for everyone. -*- lexical-binding: t -*-

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
  (interactive "P")
  (setq prefix-arg arg)
  (theist--keys-toplevel
   "\C-x"
   (char-to-string last-command-event)))

;;;###autoload
(defun theist-C-c (arg)
  (interactive "P")
  (setq prefix-arg arg)
  (theist--keys-toplevel
   "\C-c"
   (char-to-string last-command-event)))

(defvar theist-transformations
  '(identity theist-transform-C))

(defun theist-transform-key (format key)
  (kbd (format format (key-description key))))

(defun theist-transform-C (key)
  (theist-transform-key "C-%s" key))

(defun theist--keys-toplevel (old-keys old-string)
  (unless (theist--keys-recursive old-keys old-string)
    (message "found no applicable key sequence")))

(defun theist--keys-recursive (old-keys old-string)
  (let* ((read-key (theist--sanitize-char (read-char (format "%s-" old-string))))
         (new-string (concat old-string " " (key-description read-key))))
    (cl-loop
     for transform in theist-transformations
     do
     (when (theist--maybe-key (funcall transform read-key) old-keys new-string)
       (cl-return t)))))

(defun theist--maybe-key (read-key old-keys string)
  (let* ((new-keys (seq-concatenate 'vector old-keys read-key))
         (action (theist--lookup-key
                  new-keys
                  (current-local-map)
                  (current-global-map))))
    (when action
      (fi-simulate-key new-keys)
      (prog1 t))))

(defun theist--lookup-key (keyseq &rest keymaps)
  (cl-dolist (keymap keymaps)
    (when keymap
      (let ((key (lookup-key keymap keyseq)))
        (when (and key (not (numberp key)))
          (cl-return key))))))

(defun theist--sanitize-char (key)
  "Convert any single char to a key singleton vector."
  (kbd (cl-case key
         (tab "TAB")
         (?\  "SPC")
         (left "<left>")
         (right "<right>")
         (S-left "S-<left>")
         (S-right "S-<right>")
         (prior "<prior>")
         (next "<next>")
         (backspace "DEL")
         (return "RET")
         (t (char-to-string key)))))

(provide 'theist-mode)

;;; theist-mode.el ends here
