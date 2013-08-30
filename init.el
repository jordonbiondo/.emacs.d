;; things to get rid off

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

;; common lisp
(require 'cl-lib)

;; package
(push "~/.emacs.d/use-package/" load-path)
(push "~/.emacs.d/" load-path)
(require 'use-package)
(load-library "~/.emacs.d/package.el")
(mapc (lambda(p) (push p package-archives)) 
      '(("marmalade" . "http://marmalade-repo.org/packages/") 
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-refresh-contents)
(package-initialize)


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
  :config (ample-theme)
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

(use-package gh
  :ensure t)

(use-package powerline
  :config (powerline-default-theme)
  :ensure t)

(use-package auto-complete
  :config (global-auto-complete-mode)
  :ensure t)

(use-package slime
  :defer t
  :config 
  (progn
    (setq inferior-lisp-program "sbcl"))
  :ensure t)


