;;; init.el --- Jordon Biondo's emacs configuration
;;
;; Filename: init.el
;; Description: Jordon Biondo's emacs configuration
;; Author: Jordon Biondo
;; Created: Mon Oct 14 11:37:26 2013 (-0400)
;; Version: 2.0.3
;; Package-Requires: ()
;; Last-Updated: Sun Oct 27 13:09:28 2013 (-0400)
;;           By: Jordon Biondo
;;     Update #: 13
;; URL: www.github.com/jordonbiondo/.emacs.d
;; Keywords: Emacs 24.3
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
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(defvar jordonp (or (equal (getenv "USER") "jordon")
		    (equal (getenv "USERNAME" "jordon"))))

(mapcar (lambda(mode) (if (fboundp mode) (apply mode '(-1))))
	'(tool-bar-mode
	  menu-bar-mode
	  scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up package.el and use-package for init
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mapc (lambda(p) (push p load-path))
      '("~/.emacs.d/use-package/"
	"~/.emacs.d/"))

(require 'use-package)
(require 'package)

;; common lisp
(use-package cl-lib)

;;(load-library "~/.emacs.d/package.el")
(mapc (lambda(p) (push p package-archives))
      '(("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-refresh-contents)
(package-initialize)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hosted packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ample-theme
  :ensure t)


(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)


(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-next-like-this))
  :ensure t)


(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)


(use-package python
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config (progn
	    (use-package elpy
	      :config (elpy-enable)
	      :ensure t)))


(use-package cmake-mode
  :defer t
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)


(use-package switch-window
  :ensure t)


(use-package smex
  :bind (("M-x" . smex)
	 ("C-c M-x" . execute-extended-command))
  :ensure t)


(use-package magit
  :bind ("C-x m" . magit-status)
  :config (when (eq system-type 'darwin)
	    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))
  :ensure t)

(use-package w3m
  :defer t
  :ensure t)

(use-package gh
  :ensure t)

(use-package helm
  :ensure t)

(use-package powerline
  :config (progn
	    (defun jordon-point-progress (length)
	      (let ((p-count (round (* (/ (float (point))
					  (float (point-max))) length))))
		(concat  (make-string p-count ?.)
			 (make-string (- length p-count) ? ) "|")))
	    (defun powerline-jordon-theme ()
	      "Setup a nano-like mode-line."
	      (interactive)
	      (setq-default mode-line-format
			    '("%e"
			      (:eval
			       (let* ((active (powerline-selected-window-active))
				      (lhs (list (powerline-raw
						  (format " |%s|%s" mode-name
							  (jordon-point-progress 10)
							  nil 'l))))
				      (rhs (list (if (not (buffer-file-name))
						     "(-■_■)   "
						   (if (buffer-modified-p)
						       (powerline-raw "(╯°□°)╯<( SAVE! )" nil 'r)
						     (powerline-raw   "( °u°)           " nil 'r)))))
				      (center (list (powerline-raw "%b" nil))))
				 (concat (powerline-render lhs)
					 (powerline-fill-center nil (/ (powerline-width center) 2.0))
					 (powerline-render center)
					 (powerline-fill nil (powerline-width rhs))
					 (powerline-render rhs)))))))
	    (powerline-jordon-theme))
  :ensure t)


(use-package rainbow-mode
  :ensure t)


(use-package auto-indent-mode
  :init (progn
	  (add-hook 'prog-mode-hook 'auto-indent-mode))
  :ensure t)


(use-package header2
  :ensure t)


(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
	 ("C-c k" . undo-tree-redo)
	 ("C-c l" . undo-tree-switch-branch)
	 ("C-c ;" . undo-tree-visualize))
  :ensure t)


(use-package google-this
  :ensure t)


(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config (progn
	    (use-package skewer-mode
	      :config (progn (skewer-setup)
			     (add-hook 'web-mode-hook 'skewer-mode))
	      :ensure t))
  :ensure t)


(use-package auto-complete
  :config (progn
	    (require 'auto-complete-config)
	    (add-to-list 'ac-modes 'slime-repl-mode)
	    (add-to-list 'ac-modes 'js2-mode)
	    (add-to-list 'ac-modes 'js-mode)
	    (ac-config-default)
	    (global-auto-complete-mode t)

	    (use-package auto-complete-clang-async
	      :config (if (file-exists-p "~/.emacs.d/clang-complete")
			  (progn
			    (defun ac-cc-mode-setup ()
			      (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
			      (setq ac-sources '(ac-source-clang-async))
			      (ac-clang-launch-completion-process))

			    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
			    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
			    (global-auto-complete-mode t))
			(message "no clang-complete found"))
	      :if (file-exists-p "~/.emacs.d/clang-complete")
	      :ensure t))
  :ensure t)


(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init (progn
          (setq js2-basic-offset 2))
  :config (progn
            (use-package ac-js2 :ensure t)
            (use-package js2-refactor :ensure t)
            (use-package slime-js
	      :defer t
              :config (progn
                        (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
                        (slime-setup '(slime-repl slime-js)))
              :ensure t))
  :ensure t)


(use-package slime
  :config
  (progn
    (use-package ac-slime :ensure t)

    (setq inferior-lisp-program "sbcl")
    (slime-setup)
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (setq slime-protocol-version 'ignore
	  slime-net-coding-system 'utf-8-unix
	  slime-complete-symbol*-fancy t
	  slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
  :ensure t)


(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  :ensure t)


(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config (progn
	    ;; indent with 2 spaces
	    (setq rust-indent-offset 2) )
  :defer t
  :ensure t)


(use-package paredit
  :ensure t)


(use-package rainbow-delimiters
  :config (global-rainbow-delimiters-mode t)
  :ensure t)


(use-package markdown-mode
  :defer t
  :ensure t)

(use-package twittering-mode
  :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; built-ins
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lisp-mode
  :config (progn))
  

(use-package cc-mode
  :config (progn

            (font-lock-add-keywords
             'c-mode
             '(("\\<\\([A-Z_][A-Z_0-9]+\\)\\>" . font-lock-constant-face) ; caps words
               ("\\(\\<\\(def_\\)?rs\\$ *\\)\\>" . font-lock-preprocessor-face))) ;custom resources

            (defun c-maybe-insert-semicolon()
              "Insert a semicolon a the end of a line only if there isn't one."
              (interactive)
              (if (looking-at ";$")
                  (forward-char 1)
                (call-interactively 'self-insert-command)))

            (define-key c-mode-map (kbd ";") 'c-maybe-insert-semicolon)))


(use-package autoinsert
  :init (progn (auto-insert-mode t)
	       (setq auto-insert-prompt "insert %s? ")))


(use-package ido
  :init
  (progn (ido-everywhere t)
	 (ido-mode t)))


(use-package erc
  :defer t
  :config (progn (setq erc-hide-list '("JOIN" "PART" "QUIT")
		       erc-nick "jordonbiondo"
		       erc-port 6665
		       erc-server "irc.freenode.net")))


(use-package ediff
  :defer t
  :config (progn
	    (setq ediff-split-window-function 'split-window-horizontally)
	    (setq ediff-window-setup-function 'ediff-setup-windows-plain)))


(use-package compile
  :defer t
  :config (progn (setq compilation-scroll-output t)))


(use-package org
  :defer t
  :config (progn
	    (mapcar*
	     (lambda (pair) (set-face-attribute
			     (car pair) nil :height
			     (round (*  (face-attribute 'default :height) (cdr pair)))))
	     '((org-level-1 . 2.0)
	       (org-level-2 . 1.6)
	       (org-level-3 . 1.4)
	       (org-level-4 . 1.2)
	       (org-level-5 . 1.1)))

	    (setq org-confirm-elisp-link-function	nil
		  org-export-html-postamble		nil
		  org-export-html-date-format-string "%d %B %Y"
		  org-export-html-preamble-format `(("en" "%a : %d")))
		  
	    
	    (use-package org-latex
	      :config (progn
			(setq org-export-latex-listings 'minted)
			(add-to-list 'org-export-latex-packages-alist '("" "minted"))
			(setq org-src-fontify-natively t)
			(put 'erase-buffer 'disabled nil)))

	    (use-package org-bullets
	      :config (progn

			(setq org-bullets-bullet-list '("ᚐ" "ᚑ" "ᚒ" "ᚓ" "ᚔ"))
			(autoload 'org-bullets-mode "org-bullets-mode" nil t)
			(add-hook 'org-mode-hook 'org-bullets-mode))
	      :ensure t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal custom stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(push "~/.emacs.d/jorbi/" load-path)
(push "/usr/local/bin/" exec-path)

(use-package jorbi-fns)

(use-package jordon-mode
  :if (or (equal (getenv "USER") "jordon")
	  (equal (getenv "USERNAME") "jordon"))
  :config (jordon-dev-mode t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(ido-everywhere t)
 '(jordon-dev-mode t)
 '(js2-basic-offset 2)
 '(quake-mode t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun align-after-thing (beg end)
  (interactive "r")
  (align-regexp beg end (format "%s\\(\\s-*\\)" (read-string "Align After: "))1 1 t))


;;---------------------------------------------------------------------------
;; win config stack
;;---------------------------------------------------------------------------
(defvar winstack-stack '()
  "A Stack holding window configurations.
Use `winstack-push' and
`winstack-pop' to modify it.")

(defun winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (first winstack-stack))
	   (compare-window-configurations (first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
	   (message (concat "pushed " (number-to-string
				       (length (window-list (selected-frame)))) " frame config")))))

(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
	     (message "popped"))
    (message "End of window stack")))


(defun osx-copy-region(beg end)
  "very unsafe"
  (interactive "r")
  (shell-command (concat "echo \"" (buffer-substring beg end) "\" | pbcopy")))


(defun shell-clear()
  "Clear a shell buffer."
  (interactive)
  (when (equal mode-name "Shell")
    (delete-region (point-min) (point-max))
    (call-interactively 'comint-send-input)))


(defun eval-and-replace-sexp()
  "Evaluate sexp behind point and replace it with the result."
  (interactive)
  (insert
   (let ((expr (read (buffer-substring (point) (save-excursion (backward-sexp) (point))))))
     (delete-region (point) (save-excursion (backward-sexp) (point)))
     (format "%s" (save-excursion (eval expr))))))


;; color theme making helpers
(defun dump-face-as-theme-spec(face)
  (insert (format "`(%s  ((t (:foreground %s :background %s%s%s))))"
                  (face-name face)
                  (let ((f (face-foreground face nil t))) (if (stringp f) (concat "\"" f "\"") f))
                  (let ((f (face-background face nil t))) (if (stringp f) (concat "\"" f "\"") f))
                  (if (face-underline-p face nil t) (format " :underline t") "")
                  (if (face-bold-p face nil t) (format " :bold t") ""))))

(defun dump-face-at-point-as-spec()
  (interactive)
  (let ((face (symbol-at-point)))
    (when (facep face)
      (delete-region (progn (beginning-of-thing 'symbol) (point))
                     (progn (end-of-thing 'symbol) (point)))
      (dump-face-as-theme-spec face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
