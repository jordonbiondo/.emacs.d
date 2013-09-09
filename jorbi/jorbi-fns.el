
(defun jorbi/indent-repeat()
  "Indent the current line and move to the next."
  (interactive)
  (call-interactively 'indent-for-tab-command)
  (forward-line 1))


(defun jorbi/toggle-comment-line()
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))

(defun jorbi/shell-named(name)
  (interactive "sShell Name: ")
  (shell (generate-new-buffer-name name)))

(defun jorbi/c-doc-comment()
  "Insert a c style doc comment on the current line."
  (interactive)
  (goto-char (point-at-bol))
  (indent-region (point) (progn (insert "/**\n* \n*/\n") (point)))
  (forward-line -2)
  (goto-char (point-at-eol)))


(defun scons/compile(&rest args)
  (interactive "sscons: ")
  (compile (apply 'concat "scons" (mapcar (lambda(s) (concat " " s)) args))))

(defun scons/tree (&rest args)
  (interactive "sscons --tree=all: ")
  (apply 'scons/compile "--tree=all" args))

(defun scons()
  (interactive)
  (scons/compile))

(defun scons/clean()
  (interactive)
  (scons/compile "-c"))

(provide 'jorbi-fns)
