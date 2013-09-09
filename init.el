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
(require 'package)
;;(load-library "~/.emacs.d/package.el")
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
  :bind(("C-s-." . mc/mark-next-like-this))
  :ensure t)


(use-package ample-theme
  ;;:config (ample-theme)
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

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :ensure t)

(use-package google-this
  :ensure t)

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :ensure t)

(use-package auto-complete
  :config (progn
	    (require 'auto-complete-config)
	    (ac-config-default))
  :ensure t)

(use-package slime
  :config 
  (progn
    (setq inferior-lisp-program "sbcl")
    (slime-setup))
  :ensure t)

(use-package js2-refactor
  :ensure t)


(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  :ensure t)

(use-package ac-slime
  :ensure t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
  :ensure t)

(use-package ac-js2
  :ensure t)


(use-package slime-js
  :ensure t)



(use-package org
  :defer t
  :config
  (mapcar* 
   (lambda (pair) (set-face-attribute 
	      (car pair) nil :height 
	      (round (*  (face-attribute 'default :height) (cdr pair)))))
   '((org-level-1 . 2.0) 
     (org-level-2 . 1.6) 
     (org-level-3 . 1.4) 
     (org-level-4 . 1.2) 
     (org-level-5 . 1.1))))

(use-package org-bullets
  :defer t
  :config 
  (progn
    (setq org-bullets-bullet-list '("ᚐ" "ᚑ" "ᚒ" "ᚓ" "ᚔ"))
    (autoload 'org-bullets-mode "org-bullets-mode" nil t)
    (add-hook 'org-mode-hook 'org-bullets-mode))
  :ensure t)

  
(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode t)
  :ensure t)

;; personal
(push "~/.emacs.d/jorbi/" load-path)
(push "/usr/local/bin/" exec-path)
(use-package jordon-mode
  :config (jordon-dev-mode t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("84f201d2ef04c89597d0398f094fa81c1fd077f4b211a91df57787cfaabff48d" "fa29856e364e2b46254503f913637ef6561faadae62668609cc671ecfcf1c3d2" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(font-lock-add-keywords 
 'c-mode
 '(("\\([A-Z_][A-Z_]+\\)" . font-lock-constant-face)))
    
