(defun jorbi/truncate-lines ()
  (setq truncate-lines t))

(defun jorbi/dont-truncate-lines ()
  (setq truncate-lines nil))

(defmacro Windows ()
  `(equal system-type 'windows-nt))

(defmacro OSX ()
  `(equal system-type 'darwin))

(defmacro GUI ()
  `(window-system))

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

(defun jorbi/c-doc-comment()
  "Insert a c style doc comment on the current line."
  (interactive)
  (goto-char (point-at-bol))
  (indent-region (point) (progn (insert "/**\n* \n*/\n") (point)))
  (forward-line -2)
  (goto-char (point-at-eol)))

(defun jorbi/date()
  "Insert the date at point."
  (interactive)
  (insert (format-time-string "%h %d %Y, %I:%M %p")))

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

(defun align-after-thing (beg end str)
  "Inside region BEG END, Align text after STR."
  (interactive "r\nsAlign After: ")
  (align-regexp beg end (format "%s\\(\\s-*\\)" str)1 1 t))

(defun random-word ()
  (unless (OSX) (error "OS not supported"))
  (with-temp-buffer
    (insert-file-contents "/usr/share/dict/words")
    (forward-line (random (count-lines (point-min) (point-max)) ))
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun insert-random-word ()
  (interactive)
  (insert (random-word)))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; win config stack
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar winstack-stack '()
  "A Stack holding window configurations.
Use `winstack-push' and
`winstack-pop' to modify it.")

(defun winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (first winstack-stack))
         (compare-window-configurations (first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string
                                       (length (window-list (selected-frame)))) " frame config")))))

(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))

(defun osx-copy-region(beg end)
  "Stick the region on yer pastin' board."
  (interactive "r")
  (shell-command (concat "echo " (json-encode-string (buffer-substring beg end)) " | pbcopy")))

(defun osx-safe-paste()
  "Insert the contents from `pbpaste'. Won't trigger chords."
  (interactive)
  (insert (shell-command-to-string "pbpaste -Prefer txt")))

(defun shell-clear()
  "Clear a shell buffer."
  (interactive)
  (when (equal mode-name "Shell")
    (delete-region (point-min) (point-max))
    (call-interactively 'comint-send-input)))

(defun eval-and-replace-sexp()
  "Evaluate sexp behind point and replace it with the result."
  (interactive)
  (insert
   (let ((expr (read (buffer-substring (point) (save-excursion (backward-sexp) (point))))))
     (delete-region (point) (save-excursion (backward-sexp) (point)))
     (format "%S" (save-excursion (eval expr))))))

(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun toggle-80-editting-columns ()
  "Set the right window margin so the edittable space is only 80 columns."
  (interactive)
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (set-window-margins nil 0 (max (- (window-width) 80) 0)))))

(defun toggle-80-editting-columns-balanced (&optional columns)
  "Set both window margins so the edittable space is only 80 columns."
  (interactive "p")
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (let* ((change (max (- (window-width) (or columns 80)) 0))
             (left (/ change 2))
             (right (- change left)))
        (set-window-margins nil left right)))))

(defun guess-all-hooks ()
  "Return a list of all variables that are probably hook lists."
  (let ((syms '()))
    (mapatoms
     (lambda (sym)
       (if (ignore-errors (symbol-value sym))
           (let ((name (symbol-name sym)))
             (when (string-match "-\\(hook[s]?\\|functions\\)$" name)
               (push sym syms))))))
    syms))

(defun face-it (str face)
  "Apply FACE to STR and return."
  (propertize str 'face face))

(defun describe-hook (hook)
  "Display documentation about a hook variable and the
functions it contains."
  (interactive
   (list (completing-read
          "Hook: " (mapcar (lambda (x) (cons x nil)) (guess-all-hooks)))))
  (let* ((sym (intern hook))
         (sym-doc (documentation-property sym 'variable-documentation))
         (hook-docs (mapcar
                     (lambda (func)
                       (cons func (ignore-errors (documentation func))))
                     (symbol-value sym))))
    (switch-to-buffer
     (with-current-buffer (get-buffer-create "*describe-hook*")
       (let ((inhibit-read-only t))
         (delete-region (point-min) (point-max))
         (insert (face-it "Hook: " 'font-lock-constant-face) "\n\n")
         (insert (face-it (concat "`" hook "'") 'font-lock-variable-name-face))
         (replace-string "\n" "\n\t" nil
                         (point)
                         (save-excursion
                           (insert "\n" sym-doc "\n\n")
                           (1- (point))))
         (goto-char (point-max))
         (insert (face-it "Hook Functions: " 'font-lock-constant-face) "\n\n")
         (dolist (hd hook-docs)
           (insert (face-it (concat "`" (symbol-name (car hd)) "'")
                            'font-lock-function-name-face)
                   ": \n\t")
           (replace-string "\n" "\n\t" nil
                           (point)
                           (save-excursion
                             (insert (or (cdr hd) "No Documentation") "\n\n")
                             (1- (point))))
           (goto-char (point-max))))
       (help-mode)
       (help-make-xrefs)
       (read-only-mode t)
       (setq truncate-lines nil)
       (current-buffer)))))

(defun jorbi/init.el-jump (&optional package)
  "Jump to the top level use-package definition in the init file."
  (interactive)
  (unless package
    (setq package
          (ido-completing-read
           "Package: "
           (mapcar (lambda (p) (symbol-name (cadr p)))
                   (remove-if-not (lambda (f) (equal (car-safe f) 'use-package))
                                  (with-temp-buffer
                                    (insert-file-contents user-init-file)
                                    (read (concat "(list " (buffer-string) ")"))))) nil t)))
  (find-file user-init-file)
  (goto-char (point-min))
  (re-search-forward (format "( *use-package +%s *$" package) nil nil 1)
  (recenter-top-bottom)
  (pulse-momentary-highlight-one-line (point)))

(provide 'jorbi-fns)
