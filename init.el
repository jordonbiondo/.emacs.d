;; things to get rid off
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function #'ignore)

;; common lisp
(require 'cl-lib)

;; package
(push "~/.emacs.d/use-package/" load-path)
(require 'use-package)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(package-refresh-contents)


;; temporary until I fix ample-theme
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

(use-package ido
  :init 
  (progn (ido-everywhere)
	 (ido-mode)))

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package color-theme
  :ensure t)

(use-package ample-theme
  :ensure t)

(use-package switch-window
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
	 ("C-c M-x" . execute-extended-command))
  :ensure t)

(use-package magit
  :bind ("C-x m" . magit-status)
  :ensure t)
