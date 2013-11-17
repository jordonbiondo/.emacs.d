
(font-lock-add-keywords 
 'c-mode
 '(("\\(\\<[A-Z_][A-Z_0-9]+\\)\\( \\)?\\((\\)" 
    1 'font-lock-constant-face)
   ("\\(#define\\)\\( +\\)\\([A-Z_][A-Za-z0-9_]+\\)" 
    3 'font-lock-constant-face t)
   ("\\(?:ex\\(?:ec\\(?:l[ep]\\|v[Pep]\\|[lv]\\)\\|it\\)\\|fork\\|wait\\(?:pid\\)?\\)\\( *\\)\\((\\)"
    1 'font-lock-builtin-face t)))




(defun flycheck-clang-toggle-define-at-point ()
  (interactive)
  (when (or (equal major-mode 'c-mode)
            (equal major-mode 'c++-mode))
    (let ((pp-def (let ((def-bound (bounds-of-thing-at-point 'symbol)))
                    (buffer-substring-no-properties (car def-bound) (cdr def-bound)))))
      (setq flycheck-clang-definitions
            (if (member pp-def flycheck-clang-definitions)
                (progn (message "Removed definition: %s" pp-def)
                       (cl-remove-if (lambda(s) (equal s pp-def)) flycheck-clang-definitions))
              (progn (message "Added definition: %s" pp-def)
                     (cl-adjoin pp-def flycheck-clang-definitions)))))))



(defmacro toggle-member (element list)
  `(setq ,list (if (member ,element ,list)
                   (cl-remove-if (lambda(s) (equal s ,element)) ,list)
                 (cl-adjoin ,element ,list))))


