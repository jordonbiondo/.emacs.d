;;; jordon-use-package-chords.el ---
;;
;; Filename: jordon-use-package-chords.el
;; Description: chord binding in use-package
;; Author: Jordon Biondo
;; Package-Requires: (promises)
;; Last-Updated: Thu Dec 17 11:20:45 2015 (-0500)
;;           By: Jordon Biondo
;;     Update #: 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(eval-when-compile
  (require 'use-package))

(defmacro bind-chord (chord command &optional keymap)
  "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed)."
  (let ((key1 (logand 255 (aref chord 0)))
        (key2 (logand 255 (aref chord 1))))
    (if (eq key1 key2)
        `(bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)
      `(progn
         (bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)
         (bind-key (vector 'key-chord ,key2 ,key1) ,command ,keymap)))))

(add-to-list 'use-package-keywords :chords t)

(defalias 'use-package-normalize/:chords 'use-package-normalize-binder)

(defun use-package-handler/:chords (name keyword arg rest state)
  (let* ((commands
          (remq nil (mapcar
                     #'(lambda (arg) (if (listp arg) (cdr arg) nil))
                     arg)))
         (chord-binder
          (list
           (cons 'progn
                 (append
                  (mapcar
                   (lambda (command)
                     `(autoload ',command ,(or (and (stringp name) name)
                                               (symbol-name name))))
                   commands)
                  (mapcar
                   (lambda (binding) `(bind-chord
                                       ,(car binding)
                                       ',(cdr binding)))
                   arg))))))
    (use-package-handler/:preface name keyword chord-binder rest state)))

(provide 'jordon-use-package-chords)
