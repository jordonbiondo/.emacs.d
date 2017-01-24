;;;###autoload
(defmacro user-config (&rest systems)
  "Config per workstation per user.

  (user-config
    (\"computer-name\"
      (\"user-name\" (do-stuff))
      (\"other-user\" (do-other-stuff))))"
  (declare (indent defun))
  (cons 'progn (mapcar
                (lambda (sys)
                  `(when (or (equal (system-name) ,(car sys))
                             (equal (system-name) ,(concat (car sys) ".local")))
                     ,@(mapcar (lambda (user)
                                 `(when ,(or (equal (car user)
                                                    :everyone)
                                             `(equal (user-login-name)
                                                     ,(car user)))
                                    ,@(cdr user)))
                               (cdr sys))))
                systems)))
;;;###autoload
(defmacro systemp (user-or-host &optional user)
  (if user
      `(and (equal (system-name) ,user-or-host)
            (equal (user-login-name) ,user))
    `(equal (user-login-name) ,user-or-host)))

(defun jordon-truncate-lines ()
  (setq truncate-lines t))

(defun jordon-dont-truncate-lines ()
  (setq truncate-lines nil))

(defmacro windowsp ()
  `(equal system-type 'windows-nt))

(defmacro osxp ()
  `(equal system-type 'darwin))

(defmacro guip ()
  `(window-system))

(defun jordon-indent-repeat()
  "Indent the current line and move to the next."
  (interactive)
  (indent-according-to-mode)
  (forward-line 1))

(defun jordon-toggle-comment(&optional duplicate)
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

(defun jordon-c-doc-comment()
  "Insert a c style doc comment on the current line."
  (interactive)
  (goto-char (point-at-bol))
  (indent-region (point) (progn (insert "/**\n* \n*/\n") (point)))
  (forward-line -2)
  (goto-char (point-at-eol)))

(defun jordon-date()
  "Insert the date at point."
  (interactive)
  (insert (format-time-string "%h %d %Y, %I:%M %p")))

(defun scratch ()
  "Open the *scratch* buffer, create on if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jordon-find-init-file ()
  (interactive)
  (find-file user-init-file))

(defun jordon-holy-buffer-cleanse (&optional allow-tabs)
  (interactive "*P")
  (save-excursion
    (replace-string "\t" " " nil (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))
    (let ((indent-tabs-mode allow-tabs))
      (indent-region (point-min) (point-max)))))

(defun indirect-region (start end)
  "Edit the current region in another indirect buffer.
    Prompt for a major mode to activate."
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

(when (windowsp)
  (defun net-flush-dns ()
    "Flush the DNS cache on windows."
    (interactive)
    (let ((ifconfig-program-options '("/flushdns")))
      (ifconfig)))

  (defun w32-browser (doc)
    (interactive "fOpen File: ")
    (w32-shell-execute 1 doc)))

(defun align-after-thing (beg end str)
  "Inside region BEG END, Align text after STR."
  (interactive "r\nsAlign After: ")
  (align-regexp beg end (format "%s\\(\\s-*\\)" str)1 1 t))

(defun random-word (&optional capitalize)
  (unless (osxp) (error "OS not supported"))
  (let* ((file "/usr/share/dict/words")
         (action (if capitalize 'capitalize 'downcase))
         (command (format "head -n $(jot -r 1 1 $(wc -l < %s)) %s | tail -1"
                          file file)))
    (funcall action (string-trim (shell-command-to-string command)))))

(defun insert-random-word (&optional capitalize)
  (interactive "P")
  (insert (random-word capitalize)))

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
  (when (zerop (let ((inhibit-message t))
                 (shell-command-on-region (region-beginning) (region-end) "pbcopy")))
    (message "copied")))

(defun osx-safe-paste()
  "Insert the contents from `pbpaste'. Won't trigger chords."
  (interactive)
  (insert (shell-command-to-string "pbpaste -Prefer txt")))

(defun soft-caps-capitalize ()
  (upcase-region (1- (point)) (point)))

(define-minor-mode soft-caps-lock-mode
  "A mode for software capslock"
  :init-value nil
  :lighter " softcaps"
  :keymap nil
  (if soft-caps-lock-mode
      (add-hook 'post-self-insert-hook 'soft-caps-capitalize nil t)
    (remove-hook 'post-self-insert-hook 'soft-caps-capitalize t)))

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

(defun jordon-init.el-jump (&optional package)
  "Jump to the top level use-package definition in the init file."
  (interactive)
  (unless package
    (setq package
          (ido-completing-read
           "Package: "
           (mapcar (lambda (p) (format "%s" (cadr p)))
                   (remove-if-not (lambda (f) (equal (car-safe f) 'use-package))
                                  (with-temp-buffer
                                    (insert-file-contents user-init-file)
                                    (read (concat "(list " (buffer-string) ")"))))) nil t)))
  (find-file user-init-file)
  (goto-char (point-min))
  (re-search-forward (format "( *use-package +%s *$" package) nil nil 1)
  (recenter-top-bottom)
  (pulse-momentary-highlight-one-line (point)))

(defun jordon-get-list-bounds (start-char)
  (let ((start (save-excursion
                 (while (and (ignore-errors (backward-up-list nil t) t)
                             (not (eq (char-after) start-char))))
                 (when (eq (char-after) start-char)
                   (point)))))
    (when start
      (cons start
            (save-excursion
              (goto-char start)
              (forward-sexp)
              (point))))))

(defun jordon-mark-a-thing (char)
  "Mark a specified thing.

Thing is specified by the next pressed character.

i: current line without indentation
l: entire current line
w: current word
s: current string
p: current parens
b: current brackets
c: current curlies"
  (interactive "cMark a thing: ")
  (let* ((bounds
          (case char
            (?p (progn (jordon-get-list-bounds ?\()))
            (?s (progn (let ((syntax (syntax-ppss)))
                         (when (eq (syntax-ppss-context syntax) 'string)
                           (goto-char (nth 8 syntax))
                           (cons (point) (progn (forward-sexp) (point)))))))
            (?w (bounds-of-thing-at-point 'word))
            (?y (bounds-of-thing-at-point 'symbol))
            (?b (progn (jordon-get-list-bounds ?\[)))
            (?c (progn (jordon-get-list-bounds ?\{)))
            (?l (cons (line-beginning-position)
                      (line-end-position)))
            (?i (cons (save-excursion
                        (goto-char (line-beginning-position))
                        (backward-to-indentation 0)
                        (point))
                      (line-end-position))))))
    (when bounds
      (setq bounds (sort
                    (list (car bounds) (cdr bounds))
                    (lambda (a b) (> (abs (- a (point))) (abs (- b (point)))))))
      (goto-char (car bounds))
      (set-mark-command nil)
      (goto-char (cadr bounds)))))

(provide 'jordon-fns)
