
(defun jorbi/indent-repeat()
  "Indent the current line and move to the next."
  (interactive)
  (call-interactively 'indent-for-tab-command)
  (forward-line 1))


(defun jorbi/lisp-toggle-line-comment()
  (interactive)
  (let ((comment-regexp (format ";+" comment-start))
	(not-comment-regexp (format "[^;]" comment-start)))
    (save-excursion
      (goto-char (point-at-bol))
      (back-to-indentation)
      (if (looking-at comment-regexp)
	  (delete-region (point) 
			 (progn 
			   (1- (search-forward-regexp not-comment-regexp))
			   (point)))
	(insert ";; ")))))




(provide 'jorbi-fns)
