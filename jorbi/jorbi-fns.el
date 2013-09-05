
(defun jorbi/indent-repeat()
  "Indent the current line and move to the next."
  (interactive)
  (call-interactively 'indent-for-tab-command)
  (forward-line 1))


(defun jorbi/toggle-comment-line()
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))


(provide 'jorbi-fns)
