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
             ("melpa" . "https://melpa.org/packages/")
             ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives p))

(package-initialize)

(defun jordon-package-handle-initial-install ()
  (switch-to-buffer "*Initial Setup*")
  (insert "Please wait while the config initializes for the first time")
  (justify-current-line 'center)
  (add-hook
   'after-init-hook
   (lambda ()
     (when (get-buffer "*Initial Setup*")
       (switch-to-buffer "*Initial Setup*")
       (delete-other-windows)
       (setf (buffer-string) "Initial Install Complete! Enjoy!"))))
  (sit-for .001))

(defvar jordon-package-refresh-archives nil)

(when (or (not package-archive-contents)
          (and (member "--" command-line-args)
               (member "-refresh" command-line-args)))
  (when (not package-archive-contents)
    (jordon-package-handle-initial-install))
  (setq jordon-package-refresh-archives t)
  (delete "-refresh" command-line-args)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package quelpa-use-package
  :config (unless jordon-package-refresh-archives
            (setq quelpa-update-melpa-p nil))
  :ensure t)

(use-package promises
  :commands (promise promise* promise-later promise-later*)
  :defer nil
  :if (not (package-installed-p 'promises))
  :quelpa (promises :fetcher github :repo "jordonbiondo/promises.el"))

(use-package jordon-use-package-later)
(use-package jordon-use-package-chords)

(use-package use-package
  :config (progn
            (require 'jordon-use-package-chords)
            (require 'quelpa-use-package)
            (require 'jordon-use-package-later)))

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

(provide 'jordon-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jordon-package.el ends here
