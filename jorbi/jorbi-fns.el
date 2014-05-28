(defmacro Windows ()
  `(equal system-type 'windows-nt))

(defmacro OSX ()
  `(equal system-type 'darwin))

(defsubst seconds (n)
  "N seconds to time object."
  (seconds-to-time n))

(defsubst minutes (n)
  "N minutes to time object"
  (seconds (* n 60.0)))

(defmacro add-keywords (mode &rest keywords)
  "Add KEYWORDS to font-lock for MODE."
  (declare (indent defun))
  `(font-lock-add-keywords ',mode ',keywords))

(defun jorbi/indent-repeat()
  "Indent the current line and move to the next."
  (interactive)
  ;;(call-interactively 'indent-for-tab-command)
  (indent-according-to-mode)
  (forward-line 1))

(defun jorbi/toggle-comment(&optional duplicate)
  (interactive "P")
  (if (region-active-p)
      (call-interactively 'comment-or-uncomment-region)
    (if duplicate
        (let ((text (buffer-substring (point-at-bol) (point-at-eol)))
              (col (current-column)))
          (comment-or-uncomment-region (point-at-bol) (point-at-eol))
          (goto-char (point-at-eol))
          (newline)
          (insert text)
          (move-to-column col))
      (comment-or-uncomment-region (point-at-bol) (point-at-eol)))))

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
  "Insert the date at point. Value is determined by the date command."
  (interactive)
  (lexical-let ((the-buff (current-buffer))
                (the-point (point)))
    (set-process-filter (start-process"*date-proc*" "*date-proc-buffer*" "date")
                        (lambda (proc string)
                          (save-excursion
                            (with-current-buffer the-buff
                              (goto-char the-point)
                              (insert (remove-if (lambda (char) (= char ?\n)) string))))))))


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


(defun scratch ()
  "Open the *scratch* buffer, create on if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jorbi/find-init-file ()
  (interactive)
  (find-file user-init-file))

(defun jorbi/holy-buffer-cleanse (&optional allow-tabs)
  (interactive "*P")
  (save-excursion
    (replace-string "\t" " " nil (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))
    (let ((indent-tabs-mode allow-tabs))
      (indent-region (point-min) (point-max)))))

(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode (intern
               (completing-read
                "Mode: "
                (mapcar (lambda (e)
                          (list (symbol-name e)))
                        (apropos-internal "-mode$" 'commandp))
                nil t))))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(defun net-flush-dns ()
  "Flush the DNS cache on windows."
  (interactive)
  (let ((ifconfig-program-options '("/flushdns")))
    (ifconfig)))

(defun w32-browser (doc)
  (interactive "fOpen File: ")
  (w32-shell-execute 1 doc))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-face-as-theme-spec(face)
  (insert (format "`(%s  ((t (:foreground %s :background %s%s%s))))"
                  (face-name face)
                  (let ((f (face-foreground face nil t))) (if (stringp f) (concat "\"" f "\"") f))
                  (let ((f (face-background face nil t))) (if (stringp f) (concat "\"" f "\"") f))
                  (if (face-underline-p face nil) (format " :underline t") "")
                  (if (face-bold-p face nil) (format " :bold t") ""))))

(defun dump-face-at-point-as-spec()
  (interactive)
  (let ((face (symbol-at-point)))
    (when (facep face)
      (delete-region (progn (beginning-of-thing 'symbol) (point))
                     (progn (end-of-thing 'symbol) (point)))
      (dump-face-as-theme-spec face))))


(provide 'jorbi-fns)
