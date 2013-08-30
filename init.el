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

(use-package ido
  :init 
  (progn (ido-everywhere)
	 (ido-mode)))

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)


(use-package multiple-cursors
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

(use-package rainbow-mode
  :ensure t)

(use-package header2
  :ensure t)

(use-package auto-complete
  :config (progn
	    (require 'auto-complete-config)
	    (ac-config-default))
  :ensure t)


(use-package slime
  :defer t
  :config 
  (progn
    (setq inferior-lisp-program "sbcl"))
  :ensure t)


(use-package org-bullets
  :defer t
  :config 
  (progn
    (setq org-bullets-bullet-list '("ᚐ" "ᚑ" "ᚒ" "ᚓ" "ᚔ"))
    (autoload 'org-bullets-mode "org-bullets-mode" nil t)
    (add-hook 'org-mode-hook 'org-bullets-mode))
  :ensure t)

  


;; personal
(push "~/.emacs.d/jorbi/" load-path)

