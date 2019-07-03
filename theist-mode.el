;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun theist-C-x (arg)
  (interactive "P")
  (setq prefix-arg arg)
  (theist--keys-toplevel "\C-x" "x"))

(defun theist-C-c (arg)
  (interactive "P")
  (setq prefix-arg arg)
  (theist--keys-toplevel "\C-c" "c"))

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
         (local-action (lookup-key (current-local-map) new-keys t))
         (action (if (not (or (null local-action) (numberp local-action)))
                     local-action
                   (lookup-key (current-global-map) new-keys t))))
    ;; (message "%s" (key-description new-keys))
    ;; (message "%s" action)
    (if (or (null action) (numberp action))
        nil
      (fi-simulate-key new-keys) t)))

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
