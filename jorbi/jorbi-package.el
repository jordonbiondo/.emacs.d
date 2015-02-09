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
(use-package use-package
  :config (setq use-package-idle-interval 0))

(defmacro lambda-once (args &rest body)
  "Like lambda but body will only once, subsequent calls just return nil.
Does not require lexical-binding."
  (let ((sym (make-symbol "lambda-once-sym")))
    `(progn (defvar ,sym nil)
            (lambda ,args (when (and (boundp ',sym) (makunbound ',sym)) ,@body)))))

(defmacro after (libs &rest body)
  "After all LIBS, specified like (:lib1 :lib2), are loaded, eval BODY.

This is mostly a wrapper for `eval-after-load', with some special behavior described below.

In this example, once, the 'cl 'json and 'python libraries are all loaded, 'Hello will print.

    (after (:cl :json :python) (print 'Hello))

Not that the order of the libraries does not matter.

The BODY is always wrapped in a lambda, so when `lexical-binding' is enabled,
closures will be created to allow you access to local variables.

Unlike `eval-after-load' BODY will only ever be evaluated one time, regardless of multiple loads
of the LIBS."
  (declare (indent defun))
  (when (symbolp libs) (setq libs (list libs)))
  (let ((form (cons 'progn body)))
    (mapc (lambda-once (lib)
            (setq form 
                  `(eval-after-load ',(and (or (equal (substring (symbol-name lib) 0 1) ":")
                                               (error "`after' libs must be like `:lib'"))
                                           (intern (substring (symbol-name lib) 1)))
                     (lambda-once () ,form))))
          libs)
    form))

(defconst jorbi-package-font-lock-keywords
  '(("\\((\\)\\(after\\) "
     (2 font-lock-keyword-face))))
(font-lock-add-keywords 'emacs-lisp-mode jorbi-package-font-lock-keywords)

(provide 'jorbi-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-package.el ends here
