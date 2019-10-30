;;; theist-mode.el --- A better god mode for everyone. -*- lexical-binding: t -*-

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
