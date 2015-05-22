
;; experimental unused functions & features

;; should not be loaded on init

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

(defvar tmux-default-command-switches
  '((show . "-v")
    (test . (lambda () 1 "test"))
    (test2 . asdfasdf)
    (test3 . ((lambda () "one") asdfasdf "three" 4))))

(defun tmux--get-additional-command-switches (command)
  (let ((value (cdr-safe (assoc command tmux-default-command-switches))))
    (tmux--get-additional-command-switches-helper value)))

(defun tmux--get-additional-command-switches-helper (value)
  (typecase value
            (string value)
            (number (number-to-string value))
            (function (apply value nil))
            (symbol (if (fboundp value)
                        (apply value nil)
                      (symbol-value value)))
            (list (with-temp-buffer
                    (while value
                      (message "%S" value)
                      (insert (tmux--get-additional-command-switches-helper (pop value)))
                      (when value (insert " ")))
                    (buffer-string)))
            (otherwise "")))

(defmacro tmux (command &rest args)
  `(tmux--handle-output (process-exit-code-and-output "tmux" ,@(cons `(symbol-name ',command) args))))

(defun tmux--handle-output (value)
  (destructuring-bind (code output) value
                      (or (and (zerop code)
                               (with-temp-buffer
                                 (insert output)
                                 (buffer-substring (point-min) (1- (point-max)))))
                          (error "tmux error: %s" output))))

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

