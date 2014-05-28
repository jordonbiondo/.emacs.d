(defun indent-buffer () 
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defmacro add-keywords (mode &rest keywords)
  "Add KEYWORDS to font-lock for MODE."
  (declare (indent defun))
  `(font-lock-add-keywords ',mode ',keywords))

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

(defun jorbi/date()
  (interactive)
  (shell-command "date" (current-buffer)))

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

(defun scons/clean-compile()
  (interactive)
  (scons/compile "-c" "&&" "scons"))

(defun enabled-minor-modes (buffer)
  "Returns a list of minor mode specs containing the modes enabled in BUFFER.
This function returns a lot of modes you probably do not care about. 

Function `enabled-important-minor-modes' is what you are probably looking for."
  (with-current-buffer buffer
    (let (minor-modes)
      (dolist (x minor-mode-alist)
        (setq x (car x))
        (unless (memq x minor-mode-list)
          (push x minor-mode-list)))
      (dolist (mode minor-mode-list)
        (let ((fmode (or (get mode :minor-mode-function) mode)))
          (and (boundp mode) (symbol-value mode)
               (fboundp fmode)
               (let ((pretty-minor-mode
                      (if (string-match "\\(\\(-minor\\)?-mode\\)?\\'"
                                        (symbol-name fmode))
                          (capitalize
                           (substring (symbol-name fmode)
                                      0 (match-beginning 0)))
                        fmode)))
                 (push (list fmode pretty-minor-mode
                             (format-mode-line (assq mode minor-mode-alist)))
                       minor-modes)))))
      (sort minor-modes
            (lambda (a b) (string-lessp (cadr a) (cadr b)))))))

(defun enabled-important-minor-modes (buffer)
  (remove-if (lambda (mode-spec) (equal "" (third mode-spec)))
             (enabled-minor-modes buffer)))

(defun enabled-important-minor-lighters (buffer)
  (mapcar 's-trim (mapcar 'third (enabled-important-minor-modes buffer))))

(provide 'jorbi-fns)
