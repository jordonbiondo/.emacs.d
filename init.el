;;; init.el --- Jordon Biondo's emacs configuration
;;
;; Filename: init.el
;; Description: Jordon Biondo's emacs configuration
;; Author: Jordon Biondo
;; Created: Mon Oct 14 11:37:26 2013 (-0400)
;; Version: 2.0.3
;; Package-Requires: ()
;; Last-Updated: Tue May 27 21:47:30 2014 (-0400)
;;           By: Jordon Biondo
;;     Update #: 24
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

(defvar jorbi/should-load-theme t)

;; (defmacro ! (identifier &rest args)
;;   (let* ((all (mapcar 'intern (split-string (symbol-name identifier) "\\." t)))
;;          (first (car all))
;;          (last (car (last all))))
;;     (assert (not (and args (= 1 (length all)))) t "Error, attempting to call method on hash, rather than hash key.")
;;     (if args
;;         `(<<! (reduce (lambda (a b) (<< a b))  (cons ,first ',(butlast (cdr all)))) ',last ,@args)
;;       `(reduce (lambda (a b) (<< a b))  (cons ,first ',(cdr all))))))

;; (let ((a ({ 'b ({ 'c 3 'd 4 }) 'x ({ 'name "bob"
;;          'y
;;          (lambda (x) (concat (<< 'name) ": \"" x  " yay!\""))})})))
;;   (! a.x.y "Foobar"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jordonp (or (equal (getenv "USER") "jordon")
                    (equal (getenv "USERNAME") "jordon")))

(mapc (lambda(mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up package.el and use-package for init
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (p) (push p load-path))
      '("~/.emacs.d/use-package/"
        "~/.emacs.d/other/quake-mode/"
        "~/.emacs.d/other/"
        "~/.emacs.d/keys/"
        "~/.emacs.d/jorbi/"
        "~/src/redspot-emacs/"))

(require 'use-package)
(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)
(font-lock-add-keywords 'lisp-interaction-mode use-package-font-lock-keywords)
(require 'package)

(require 'keys)

;; common lisp
(use-package cl-lib)

(dolist (p '(;;("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives p))

(when (and (member "--" command-line-args)
         (member "-refresh" command-line-args))
  (delete "-refresh" command-line-args)
  (package-refresh-contents))

(package-initialize)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal custom stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push "~/.emacs.d/jorbi/" load-path)
(push "/usr/local/bin/" exec-path)

(use-package jorbi-fns
  :defer nil)

(use-package jordon-mode
  :config (jordon-dev-mode t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command Lines Args
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package commander
  :ensure t)

;;(commander
;; (option "--no-theme" "Do not load theme" (lambda (&rest args) )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hosted packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ample-theme
  :ensure t)

(use-package s ;; string lib
  :ensure t)

(use-package dash ;; list lib
  :ensure t)

(use-package jabber
  :bind (("C-c u i" . jabber-chat-with)
         ("C-c u u" . jabber-display-roster))
  :config (progn
            (setq jabber-roster-line-format " %c %-25n %S"
                  jabber-use-sasl nil
                  jabber-history-enabled t
                  jabber-use-global-history nil
                  jabber-backlog-number 40
                  jabber-backlog-days 30
                  jabber-account-list '(("jordon.biondo@parelio.com"
                                         (:network-server . "talk.google.com")
                                         (:connection-type . ssl))))
            (add-hook 'jabber-chat-mode 'visual-line-mode)
            (use-package jorbi-jabber
              :config  (cond
                        ((OSX)
                         (add-hook 'jabber-alert-message-hooks
                                   'jorbi-jabber/terminal-notification))
                        ((Windows)
                         (add-hook 'jabber-alert-message-hooks
                                   'jorbi-jabber/toast-notification)
                         (add-hook 'jabber-alert-message-hooks
                                   'jorbi-jabber/send-mail-notification))))
            (setq jabber-chat-buffer-format "Chat: %n"))
  :ensure t)

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)

(use-package multiple-cursors
  :config (progn (defun jorbi/mc/mark-until-line-change (&optional up)
                   (interactive "P")
                   (unless (save-excursion
                             (let ((col (current-column)))
                               (forward-line (if up -1 1))
                               (move-to-column col))
                             (looking-at "\\( +\\| *$\\)"))
                     (when up (next-line -1)) (mc/mark-next-lines 1) (jorbi/mc/mark-until-line-change up)))

                 (push 'jorbi/mc/mark-until-line-change mc/cmds-to-run-once))

  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c C-m" . jorbi/mc/mark-until-line-change))
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
  :config (setq switch-window-shortcut-style 'qwerty
                switch-window-qwerty-shortcuts
                '("a" "w" "e" "f" "j" "i" "o" ";" "s" "d" "k" "l"))

  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("C-c M-x" . execute-extended-command))
  :ensure t)

(use-package magit
  :bind ("C-x m" . magit-status)
  :config (progn
            (use-package flymake)
            (when (OSX)
              (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))

            (use-package git-gutter
              :config (eval-after-load "ample-theme"
                        '(dolist (face '(git-gutter:added
                                         git-gutter:deleted
                                         git-gutter:modified
                                         git-gutter:separator
                                         git-gutter:unchanged))
                           (set-face-background face (face-foreground face))))
              :ensure t))
  :ensure t)

(use-package w3m
  :defer t
  :ensure t)

(use-package gh
  :ensure t)

(use-package helm
  :ensure t)

(use-package sublimity
  :config (use-package sublimity-scroll
            :config (sublimity-global-mode t))
  :ensure t)

(use-package powerline
  :config  (use-package jorbi-powerline
             :config (setq-default mode-line-format jorbi/powerline-format))
  :ensure t)

(use-package imenu-anywhere
  :config (progn
            (defadvice imenu-anywhere--goto-function (after pulse-the-line activate)
              (pulse-momentary-highlight-one-line (point))))
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package key-chord
  :config (progn

            (key-chord-mode t)

            ;; short waits
            (setq key-chord-two-keys-delay .020
                  key-chord-one-key-delay .030)

            ;; jordon-dev-mode chords
            (dolist (binding
                     `((" i" . previous-multiframe-window)
                       (" o" . next-multiframe-window)
                       ("jf" . switch-window)
                       (" l" . ibuffer)

                       (" m" . magit-status)

                       (" e" . er/expand-region)

                       (" q" . quake-mode)

                       (" 0" . delete-window)
                       (" 1" . delete-other-windows)
                       (" 2" . split-window-below)
                       (" 3" . split-window-right)
                       (" =" . winstack-push)
                       (" -" . winstack-pop)

                       (" w" . whitespace-mode)

                       ("ji" . undo-tree-undo)
                       ("jo" . undo-tree-redo)
                       ("jk" . undo-tree-switch-branch)
                       ("j;" . undo-tree-visualize)

                       (" b" . ido-switch-buffer)
                       (" f" . ido-find-file)
                       (" s" . save-buffer)

                       (" x" . shell)

                       (" \\". jorbi/toggle-comment)

                       ("nw" . jabber-display-roster)
                       ("ne" . jabber-chat-with)

                       ("hf" . helm-do-grep)

                       (" g" . goto-line)

                       ("nv" . jorbi/find-init-file)

                       ("io" . imenu-anywhere)

                       (" r" . recompile)))
              (key-chord-define jordon-dev-mode-map (car binding) (cdr binding))))

  :ensure t)

(use-package projectile
  :config (progn
            (add-hook 'enh-ruby-mode-hook 'projectile-mode)
            (add-hook 'prog-mode-hook 'projectile-mode)
            (use-package projectile-rails
              :config (progn
                        (add-hook 'enh-ruby-mode-hook 'projectile-rails-mode)
                        (add-hook 'haml-mode-hook 'projectile-rails-mode)
                        (add-hook 'yaml-mode-hook 'projectile-rails-mode)
                        (add-hook 'js2-mode-hook 'projectile-rails-mode))
              :ensure t))
  :ensure t)

(use-package auto-indent-mode
  :init (progn
          (add-hook 'prog-mode-hook 'auto-indent-mode)
          (add-hook 'prog-mode-hook (defun indent-tabs-mode-off ()
                                      (interactive)
                                      (setq indent-tabs-mode nil))))
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
              :ensure t)

            (defun web-indirect-this-thing()
              (interactive)
              (let ((beg 0) (end 0))
                (save-excursion
                  (setq beg (progn (web-mode-forward-sexp -1)
                                   (call-interactively 'web-mode-tag-end)
                                   (point)))
                  (setq end (progn  (web-mode-forward-sexp 1)
                                    (point))))
                (indirect-region beg end))))
  :ensure t)


(use-package edit-server
  :config (edit-server-start)
  :ensure t)

;; (use-package d-mode
;;   :init (progn (add-to-list 'exec-path "C:/D/dmd2/windows/bin")
;;                (add-to-list 'exec-path "C:/Users/jordon.biondo/src/DCD"))
;;   :config (progn (require 'ac-dcd)
;;                  (add-to-list 'ac-modes 'd-mode)
;;                  (defun ac-d-mode-setup ()
;;                    (setq ac-sources (append '(ac-source-dcd) ac-sources))
;;                    (global-auto-complete-mode t))
;;                  (add-hook 'd-mode-hook 'ac-d-mode-setup))
;;   :ensure t)

(use-package csharp-mode
  :init (add-to-list 'c-default-style
                     (cons 'csharp-mode "c#"))
  :config (progn
            (add-hook 'csharp-mode-hook 'hs-minor-mode)
            (add-hook 'csharp-mode-hook 'toggle-truncate-lines)
            (add-hook 'csharp-mode-hook (lambda () (setq c-basic-offset 4
                                                         indent-tabs-mode nil)))

            (font-lock-add-keywords 'csharp-mode
                                    '(("\\(// *\\)\\(todo\\)\\(.*$\\)" 2 'font-lock-warning-face t))))

  :ensure t)

(use-package omnisharp
  :bind (("M-i" . jorbi/omnisharp-go-to-definition-smart)
         ("M-m" . omnisharp-find-usages))
  :config (progn

            (setq omnisharp-eldoc-support t)
            (add-hook 'csharp-mode-hook 'eldoc-mode)

            (defun jorbi/omnisharp-go-to-definition-smart (&optional force-ow)
              (interactive "P")
              (let* ((json-result (omnisharp-post-message-curl-as-json
                                   (concat (omnisharp-get-host) "gotodefinition")
                                   (omnisharp--get-common-params)))
                     (filename (cdr (assoc 'FileName json-result))))
                (if (null filename)
                    (message "Cannot go to definition as none was returned by the API.")
                  (omnisharp-go-to-file-line-and-column
                   json-result
                   (or force-ow (not (equal (omnisharp--convert-backslashes-to-forward-slashes filename)
                                            (buffer-file-name)))))
                  (pulse-momentary-highlight-one-line (point)))))

            (defun jorbi/reload-csharp-buffers ()
              "Restart csharp on all csharp buffers."
              (interactive)
              (dolist (b (buffer-list))
                (with-current-buffer b
                  (when (eql major-mode 'csharp-mode)
                    (csharp-mode)))))

            (use-package company
              :config (progn
                        (add-to-list 'company-backends 'company-omnisharp)
                        (add-hook 'csharp-mode-hook 'company-mode))
              :ensure t)
            (add-hook 'csharp-mode-hook 'omnisharp-mode))
  :ensure t)



(use-package flycheck
  :config (progn
            (add-hook 'c-mode-hook 'flycheck-mode)
            (add-hook 'c++-mode-hook 'flycheck-mode))
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

(use-package enh-ruby-mode
  :config (progn
            (require 'auto-complete)
            (add-to-list 'ac-modes 'enh-ruby-mode)
            (add-hook 'enh-ruby-mode-hook 'jorbi/dont-truncate-lines)
            (use-package robe
              :config (progn
                        (add-hook 'enh-ruby-mode-hook 'robe-mode)
                        (add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
                        (add-hook 'robe-mode-hook (defun jorbi-robe:/setup-completeion() (auto-complete-mode -1) (company-mode t)))
                        (eval-after-load 'company
                          '(progn
                             (push 'company-robe company-backends))))
              :ensure t)

            (use-package bundler
              :ensure t)

            (use-package haml-mode
              :ensure t)

            (use-package yaml-mode
              :ensure t))
  :ensure t)

(use-package moz
  :config (progn
            (defun jorbi-moz/refresh ()
              (interactive)
              (if (ignore-errors
                    (comint-send-string (inferior-moz-process)
                                        "setTimeout(BrowserReload(), \"1000\");") t)
                  (message "Moz Refreshing...")))
            
            (define-key moz-minor-mode-map (kbd "C-M-o") 'jorbi-moz/refresh)

            (eval-after-load 'js2-mode
              '(add-hook 'js2-mode-hook 'moz-minor-mode))

            (eval-after-load 'enh-ruby-mode
              '(add-hook 'enh-ruby-mode-hook 'moz-minor-mode)))
  :ensure t)

(use-package rsense
  :ensure t)

(use-package d-mode
  :if (executable-find "dmd")
  :ensure t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init (progn
          (setq js2-basic-offset 2))
  :config (progn
            (when jordonp (use-package redspot
                            :config (define-key js2-mode-map (kbd "M-.") 'redspot:find-js-definition-here)))

            (use-package ac-js2 :ensure t)
            (use-package js2-refactor :ensure t)
            (use-package slime-js
              :defer t
              :config (progn
                        (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
                        (slime-setup '(slime-repl slime-js)))
              :ensure nil))
  :ensure t)

(use-package pretty-mode
  :config (progn
            (dolist (mode-hook '(emacs-lisp-mode-hook
                                 lisp-interaction-mode-hook
                                 js2-mode-hook
                                 enh-ruby-mode-hook))
              (add-hook mode-hook 'pretty-mode)))
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

(when (OSX)
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)
    :ensure t))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config (progn
            ;; indent with 2 spaces
            (setq rust-indent-offset 2) )
  :defer t
  :ensure t)

(use-package paredit
  :config (add-hook 'paredit-space-for-delimiter-predicates (lambda(&rest args) (not (equal major-mode 'csharp-mode))))
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
(use-package files
  :config (setq backup-directory-alist `(("." . "~/.saves"))
                version-control t
                kept-new-versions 10
                kept-old-versions 0
                delete-old-versions t
                backup-by-copying t))

(use-package pulse
  :config (progn (setq pulse-iterations 7
                       pulse-delay .01)))

(use-package eww
  :config (progn
            (add-hook 'eww-mode-hook (apply-partially 'toggle-truncate-lines 1))))

(use-package savehist
  :config (savehist-mode t))

(use-package desktop
  :config (progn (desktop-save-mode t)
                 (setq desktop-path '("~/.emacs.d/"))))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :config (progn
            (eval-after-load 'enh-ruby-mode
              '(add-to-list 'hs-special-modes-alist
                            '(enh-ruby-mode
                              "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                              'enh-ruby-end-of-block nil)))))

(use-package ispell
  :bind (("C-c s w" . ispell-word)
         ("C-c s b" . ispell-buffer)))

(use-package lisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

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
  :init (progn
          (ido-mode t)
          (ido-everywhere t)
          (use-package flx-ido
            :config (flx-ido-mode t)
            :ensure t)
          (use-package ido-vertical-mode
            :config (ido-vertical-mode t)
            :ensure t)))

(use-package erc
  :defer t
  :config (progn (setq erc-hide-list '("JOIN" "PART" "QUIT")
                       erc-nick "jordonbiondo"
                       erc-port 6665
                       erc-server "irc.freenode.net")))

(use-package pivotal-tracker
  :config (progn
            (setq pivotal-api-token (key :pivotal)))
  :ensure t)

(use-package ediff
  :defer t
  :config (progn
            (setq ediff-split-window-function 'split-window-horizontally)
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package compile
  :defer t
  :config (progn (setq compilation-scroll-output t)))

(use-package hl-line
  :config (global-hl-line-mode t))

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

            (setq org-confirm-elisp-link-function nil
                  org-export-html-postamble  nil
                  org-export-html-date-format-string "%d %B %Y"
                  org-export-html-preamble-format `(("en" "%a : %d")))

            ;;(use-package org-latex
            ;;  :config (progn
            ;;            (setq org-export-latex-listings 'minted)
            ;;            (add-to-list 'org-export-latex-packages-alist '("" "minted"))
            ;;            (setq org-src-fontify-natively t))
            ;;  :ensure nil)

            (use-package org-bullets
              :config (progn
                        (setq org-bullets-bullet-list '("ᚐ" "ᚑ" "ᚒ" "ᚓ" "ᚔ"))
                        (autoload 'org-bullets-mode "org-bullets-mode" nil t)
                        (add-hook 'org-mode-hook 'org-bullets-mode))
              :ensure t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-syntax-directory "/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax")
 '(custom-safe-themes t)
 '(send-mail-function (quote smtpmail-send-it))
 '(quake-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

(put 'erase-buffer 'disabled nil)
