;;; jordon-use-package-later.el ---
;;
;; Filename: jordon-use-package-later.el
;; Description: Deferred evaluation for forms in use-package
;; Author: Jordon Biondo
;; Package-Requires: (promises)
;; Last-Updated: Fri Dec 11 10:53:59 2015 (-0500)
;;           By: Jordon Biondo
;;     Update #: 2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; (eval-when-compile
;;   (require 'use-package)
;;   (require 'promises))

(use-package promises
  :quelpa (promises :fetcher github :repo "jordonbiondo/promises.el")
  :commands (promise promise* promise-later promise-later*)
  :defer nil)

(add-to-list 'use-package-keywords :later t)

(defalias 'use-package-normalize/:later 'use-package-normalize-forms)

(defun use-package-handler/:later (name keyword arg rest state)
  "Handler for `:later' keyword in `use-package'."
  `((promise-later* (resolve reject)
      (condition-case err
          (progn ,@arg)
        (error (message "Error in %S :later evaluation: %S" ',name err))))))


(provide 'jordon-use-package-later)
