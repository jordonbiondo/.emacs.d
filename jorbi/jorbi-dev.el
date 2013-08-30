(defmacro define-keys (keymap &rest pairs) 
  "In KEYMAP define all of the key/command pairs in PAIRS.
PAIRS is a sequence of lists in the form (key-sequnce command).
Key sequences must adhere to the format of `kbd'.

Example: 
 (define-keys foo-mode
   (\"C-c C-<return>\" 'indent-region)
   (\"C-c w e\" 'kill-whole-line))"
  (declare (indent defun))
  (let ((statement '(progn)))
    (mapc 
     (lambda(pair) (setq statement 
  		 (append statement
				 `((define-key ,keymap (kbd ,(first pair)) ,(second pair)))))) pairs)
    
    statement))


(defmacro defdev (name &rest body)
  "Setup variables and modes for a developer with a username of NAME.

Defines:
  const NAME which will be true if the environment variable USERNAME == NAME

  function NAME-do: evaluates BODY like `progn' but only when the user is NAME

  variable NAME-mode-map: a keymap for NAME-mode

  minor-mode NAME-mode: for user specific settings and keybindings"
  `(progn
     (defconst ,(intern name) (equal (getenv "USER") ,name)
       ,(concat "This variable is true when the current user is " name))
     
     (defvar ,(intern (concat name "-dev-mode-map")) (make-sparse-keymap)
       ,(concat "Keymap for " name "-dev-mode"))
     
     (define-minor-mode ,(intern (concat name "-dev-mode"))
       ,(concat name "'s minor mode.")
       :init-value nil
       :lighter ,(concat " " name)
       :keymap ,(intern (concat name "-dev-mode-map"))
       :global t
       (let ((dev/on-off (if ,(intern (concat name "-dev-mode")) t -1)))
	 ,@body))
     
     (defmacro ,(intern (concat name "-do")) (&rest body)
       ,(concat "Execute body like `progn' if user is " name ".")
       (list if ,(intern name) (progn ,@body)))))


  
