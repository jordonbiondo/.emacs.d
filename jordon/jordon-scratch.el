;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break taker
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bt/enabled t
  "Whether or not to repeat breaks.")

(defvar bt/last-break-minutes 60)

(defvar bt/snooze-time 5
  "Amount of time in minutes to use for snoozes")

(defun bt/exit-and-repeat-last ()
  (interactive)
  (winstack-pop)
  (break-taker bt/last-break-minutes))

(defun bt/exit ()
  (interactive)
  (winstack-pop))

(defun bt/snooze ()
  (interactive)
  (let ((old-time bt/last-break-minutes))
    (break-taker bt/snooze-time)
    (setq bt/last-break-minutes old-time)))

(defun bt/exit-and-snooze ()
  (interactive)
  (bt/exit)
  (let ((old-time bt/last-break-minutes))
    (break-taker bt/snooze-time)
    (setq bt/last-break-minutes old-time)))


(defun break-taker (minutes)
  (interactive "NMinutes between breaks: ")
  (lexical-let ((minutes (abs minutes)))
    (setq bt/last-break-minutes minutes)
    (run-with-timer
     (* 60 minutes) nil
     (lambda(&rest args)
       (winstack-push)
       (switch-to-buffer "*Break Taker*")
       (read-only-mode t)
       (let ((inhibit-read-only t)
             (fill-column (frame-width)))
         (delete-other-windows)
         (delete-region (point-min) (point-max))
         (insert "\n\n\n")
         (insert (propertize "Time to take a break, walk around a bit!" 'face
                             `(:inherit font-lock-function-name-face)))
         (insert "\n\n\n")
         (insert "Press 'C-c C-k' to resume\n\n")
         (insert (format (concat "Press 'C-c C-c' to resume and take another break in %d minutes\n\n"
                                 "Press 'C-c C-w' to postpone your break %d minutes") minutes bt/snooze-time))
         (set-justification-center (point-min) (point-max))
         (with-current-buffer "*Break Taker*"
           (local-set-key (kbd "C-c C-k") 'bt/exit)
           (local-set-key (kbd "C-c C-c") 'bt/exit-and-repeat-last)
           (local-set-key (kbd "C-c C-w") 'bt/exit-and-snooze)))))))



(defun cloudy (str)
  (switch-to-buffer (generate-new-buffer"c"))
  (setq word-wrap t)
  (center-region
   (point)
   (progn
     (let ((h (face-attribute 'default :height)))
       (mapc (lambda(w) (insert " " w (if (< (random 25) 4) "\n""")))
             (sort
              (mapcar (lambda (w)
                        (propertize (car w) 'face
                                    `(:foreground ,(color-lighten-name "skyblue2"
                                                                       (* (cdr w) 3))
                                                  :height ,(round (* .4 (expt (cdr w) 1.1) h)))))
                      (reduce (lambda (a b)
                                (incf (cdr (or (assoc b a) (car (setq a (cons (cons b 0) a)))))) a)
                              (split-string str "[^a-z0-9]" t) :initial-value nil))
              (lambda(a b) (= 0 (random 2))))))
     (point))))

;; (font-lock-add-keywords
;;  'c-mode
;;  '(("\\(\\<[A-Z_][A-Z_0-9]+\\)\\( \\)?\\((\\)"
;;     1 'font-lock-constant-face)
;;    ("\\(#define\\)\\( +\\)\\([A-Z_][A-Za-z0-9_]+\\)"
;;     3 'font-lock-constant-face t)
;;    ("\\(?:ex\\(?:ec\\(?:l[ep]\\|v[Pep]\\|[lv]\\)\\|it\\)\\|fork\\|wait\\(?:pid\\)?\\)\\( *\\)\\((\\)"
;;     1 'font-lock-builtin-face t)))

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

(defun e$ (type &rest others)
  "make html"
  (declare (indent defun))
  (let* ((given-type (or (and (listp type) (car type)) type))
         (elem-type (if (keywordp given-type)
                        (substring (symbol-name given-type) 1)
                      given-type))
         (attributes (and (listp type) (cadr type))))
    (format "<%s%s>%s</%s>"
            elem-type
            (or
             (and attributes
                  (concat
                   " "
                   (mapconcat
                    (lambda (attr)  (format "%s=\"%s\"" (car attr) (cdr attr)))
                    attributes
                    " ")))
             "")
            (if others (concat "\n" (string-join others "\n") "\n") "")
            elem-type)))

(defun render-html-async (file)
  (promise-async*
   (shell-command-to-string (format "w3m -dump -cols 80 %s" file))))

(defun jordon-substitute-prefix (name sub form)
  (let ((prefix (concat name ".")))
    (if (listp form)
        (mapcar (lambda (ff) (jordon-substitute-prefix name sub ff)) form)
      (if (and (symbolp form) (string-prefix-p prefix (symbol-name form)))
          (prog1 (intern (format "%s%s" sub (substring (symbol-name form) (length prefix))))
            (message "%S" form))
        form))))

(defmacro with-prefixes (names &rest body)
  (declare (indent 1))
  (let ((body (cons 'progn body)))
    (dolist (substitution names)
      (destructuring-bind (prefix sub) substitution
        (setq body (jordon-substitute-prefix (symbol-name prefix) sub body))))
    body))

(defun one-off-hook (hook func &optional name)
  "Add FUNC to HOOK such that the func removes itself when called.
The symbol created and added to HOOK will optionally end with NAME if
provided for debugging purposes."
  (unless lexical-binding (user-error "Using one-off-hook requires `lexical-binding'."))
  (let* ((hook-name (format "*one-off-hook%s*" (if name (concat "-" name) "")))
         (hook-sym (make-symbol hook-name)))
    (setf (symbol-function hook-sym)
          (lambda (&rest args)
            (unwind-protect
                (apply func args)
              (let ((inhibit-message t))
                (remove-hook hook hook-sym)
                (message "removed %s from %s" hook-name hook)))))
    (add-hook hook hook-sym)))
