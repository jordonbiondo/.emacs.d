;;; jordon-package.el ---
;;
;; Filename: jordon-package.el
;; Description:
;; Author: Jordon Biondo
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

(require 'package)
;; make this go away
(defalias 'package--ensure-init-file #'ignore)

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

(require 'jordon-use-package)
(use-package jordon-use-package
  :config (progn
            (setq use-package-idle-interval 0)
            (prefer-coding-system 'utf-8)))

(defmacro lambda-once (args &rest body)
  "Like lambda but body will only once, subsequent calls just return nil.
Does not require lexical-binding."
  (let ((sym (make-symbol "lambda-once-sym")))
    `(progn (defvar ,sym nil)
            (lambda ,args (when (and (boundp ',sym) (makunbound ',sym)) ,@body)))))

(defun add-late-hook (hook-modes hook)
  "Add HOOK to all HOOK-SYMBOLSs and also immdiately run HOOK in all MODE-SYMBOL buffers.

A MODE-SYMBOL can either be a possible `major-mode' value symbol or a minor mode variable.

HOOK is run if either `major-mode' equals MODE-SYMBOL or MODE-SYMBOL is bound and true in a buffer.

\(fn ((MODE-SYMBOL HOOK-SYMBOL)...) HOOK)"
  (when (not (car-safe (car hook-modes)))
    (setq hook-modes (list hook-modes)))
  (mapc (lambda (hm)
          (add-hook (cadr hm) hook)
          (mapc (lambda (b)
                  (with-current-buffer b
                    (when (or (equal major-mode (car hm))
                              (and (boundp (car hm)) (symbol-value (car hm))))
                      (apply hook nil))))
                (buffer-list)))
        hook-modes))

(defun jordon-package-sanitize-package-name (name)
  ":keyword => 'keyword, ensure normal things are passed to `eval-after-load'."
  (cond
   ((keywordp name) (list 'quote (intern (substring (symbol-name name) 1))))
   ((symbolp name) (list 'quote name))
   ((stringp name) name)
   ((listp name) name)
   (t (error "`after' macro doesn't know what to do with library: '%S'" name))))

(defmacro after (libs &rest body)
  "After all LIBS, specified like (:lib1 'lib2 \"lib3\"), are loaded, eval BODY.

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
                  `(eval-after-load ,(jordon-package-sanitize-package-name lib)
                     (lambda-once () ,form))))
          libs)
    form))

(defconst jordon-package-font-lock-keywords
  '(("\\((\\)\\(after\\) "
     (2 font-lock-keyword-face))))
(font-lock-add-keywords 'emacs-lisp-mode jordon-package-font-lock-keywords)

(provide 'jordon-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jordon-package.el ends here
