(defun key (key)
  (let* ((name (substring (symbol-name key) 1))
         (file (concat "~/.emacs.d/keys/" name)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      (error "No key found for %S" key))))

(provide 'keys)
