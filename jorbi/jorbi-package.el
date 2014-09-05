;;; jorbi-package.el ---
;;
;; Filename: jorbi-package.el
;; Description:
;; Author: Jordon Biondo
;; Maintainer:
;; Created: Thu Aug 28 09:23:05 2014 (-0400)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Thu Aug 28 09:24:51 2014 (-0400)
;;           By: Jordon Biondo
;;     Update #: 2
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'package)

(dolist (p '(;;("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives p))

(package-initialize)

(if (package-installed-p 'use-package)
    (when (and (member "--" command-line-args)
               (member "-refresh" command-line-args))
      (delete "-refresh" command-line-args)
      (package-refresh-contents))
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

(defun depends--helper (deps body)
  (let ((dep (if (stringp (car deps)) (pop deps) (cons 'quote (list (pop deps))))))
    (list 'eval-after-load dep
          (cons 'lambda (cons nil (if (not deps)
                               body
                             (list (depends--helper deps body))))))))
(defmacro depends (&rest args)
  (declare (indent defun))
  (let ((dependencies nil))
    (while (or (stringp (car args))
              (symbolp (car args)))
      (push (pop args) dependencies))
    (depends--helper dependencies args)))

(use-package use-package
  :config (setq use-package-idle-interval 0))

(provide 'jorbi-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-package.el ends here
