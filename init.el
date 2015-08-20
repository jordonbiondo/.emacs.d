;;; init.el --- Jordon Biondo's emacs configuration
;;
;; Filename: init.el
;; Description: Jordon Biondo's emacs configuration
;; Author: Jordon Biondo
;; Created: Mon Oct 14 11:37:26 2013 (-0400)
;; Version: 2.3.3
;; URL: www.github.com/jordonbiondo/.emacs.d
;; Keywords: Emacs 25
;; Compatibility: emacs >= 25
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode))

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up package.el and use-package for init
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/jordon/"
        "~/.emacs.d/jordon/use-package/"))

(require 'jordon-package)
(use-package cl-lib)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jordon configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jordon-fns
  :commands (user-config systemp)
  :chords ((" =" . winstack-push)
           (" -" . winstack-pop)
           ("nv" . jordon-find-init-file)
           (" \\". jordon-toggle-comment))
  :bind (("C-\\" . jordon-toggle-comment)
         ("C-<tab>" . jordon-indent-repeat)
         ("C-M-k" . jordon-c-doc-comment)
         ("C-c f u" . winstack-push)
         ("C-c f o" . winstack-pop))
  :config (when (osxp) (push "/usr/local/bin/" exec-path))
  :defer t)

(use-package jordon-key-chord
  :config
  (progn (key-chord-mode t)
         (setq key-chord-two-keys-delay .020
               key-chord-one-key-delay .020)))

(use-package jordon-jabber
  :init (after (:jabber)
          (require 'jordon-jabber))
  :config  (progn
             (setq jabber-chat-header-line-format
                   jordon-jabber-chat-header-line-format)
             (cond
              ((osxp)
               (add-hook 'jabber-alert-message-hooks
                         'jordon-jabber-terminal-notification))
              ((windowsp)
               (add-hook 'jabber-alert-message-hooks
                         'jordon-jabber-toast-notification)
               (add-hook 'jabber-alert-message-hooks
                         'jordon-jabber-send-mail-notification))))
  :defer t)


(use-package jordon-mode-line
  :config (setq-default mode-line-format jordon-mode-line-format))

(use-package jordon-magit
  :commands 'jordon-magit-cleanup-this-hunk
  :init (after (:magit)
          (bind-keys :map magit-status-mode-map
            ("C-c s d" . jordon-magit-cleanup-this-hunk)))
  :defer t)

;; system specific
(user-config
  ("eeloo"
   (:everyone
    (after (:smex)
      (setq smex-flex-matching nil))))
  ("duna"
   (:everyone
    (ignore-errors
      (set-default-font "Envy Code R")
      (set-face-attribute 'default nil :height 125))
    (when (guip)
      (global-unset-key (kbd "s-t")))
    (setq-default scroll-margin 5)
    (setq-default scroll-step 1))
   ("jordon"
    (setq user-mail-address "jordon.biondo@appropos.com")
    (setq initial-scratch-message "\n;; Welcome Back\n\n")
    (use-package awt
      :load-path "~/src/awt-emacs"
      :init (after (:js2-mode)
              (bind-keys :map js2-mode-map
                ("C-c n f m" . awt-find-model)
                ("C-c n r m" . awt-require-model)
                ("C-c n f c" . awt-find-controller)))
      :defer t))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; built-ins
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :config (setq save-interprogram-paste-before-kill t)
  :chords (" g" . goto-line))

(use-package flymake
  :commands flymake-mode)

(use-package custom
  :init (setq custom-file "~/.emacs.d/custom.el"))

(use-package indent
  :bind (("C-c n i" . indent-region))
  :init (defadvice indent-region (around no-message activate)
          (let ((inhibit-message t))
            ad-do-it))
  :defer t)

(use-package shell
  :defer t
  :chords (" x" . shell))

(use-package files
  :chords (" s" . save-buffer)
  :config (setq backup-directory-alist `(("." . "~/.saves"))
                version-control t
                kept-new-versions 10
                kept-old-versions 0
                delete-old-versions t
                backup-by-copying t))

(use-package grep
  :defer t
  :config (add-hook 'grep-mode-hook 'jordon-truncate-lines))

(use-package window
  :defer t
  :chords ((" 0" . delete-window)
           (" 1" . delete-other-windows)
           (" 2" . split-window-below)
           (" 3" . split-window-right)))

(use-package whitespace
  :defer t
  :chords (" w" . whitespace-mode))

(use-package frame
  :defer t
  :chords ((" i" . previous-multiframe-window)
           (" o" . next-multiframe-window))
  :bind (("C-c i" . previous-multiframe-window)
         ("C-c o" . next-multiframe-window)))

(use-package ibuffer
  :defer t
  :chords ((" l" . ibuffer))
  :bind (("C-x l" . ibuffer)))

(use-package dired
  :defer t
  :config (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package pulse
  :commands pulse-momentary-highlight-one-line
  :config (progn
            (setq pulse-iterations 7
                  pulse-delay .01)
            (set-face-background 'pulse-highlight-start-face
                                 (face-foreground 'font-lock-keyword-face))))

(use-package eww
  :defer t
  :config (add-hook 'eww-mode-hook 'jordon-dont-truncate-lines))

(use-package electric
  :defer t)

(use-package prog-mode
  :init (progn
          (after (:electric)
            (add-hook 'prog-mode-hook 'electric-indent-mode))
          (add-hook 'prog-mode-hook
                    (defun indent-tabs-mode-off ()
                      (interactive)
                      (setq indent-tabs-mode nil)))))

(use-package make-mode
  :init (add-hook
         'makefile-mode-hook
         (defun jordon-makefile-mode-setup ()
           (setq-local indent-tabs-mode t)))
  :defer t)

(use-package python
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config (progn
            (use-package elpy
              :config (elpy-enable)
              :ensure t))
  :defer t)

(use-package savehist
  :idle (savehist-mode t))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :commands hs-toggle-hiding
  :defer t)

(use-package ispell
  :bind (("C-c s w" . ispell-word)
         ("C-c s b" . ispell-buffer))
  :defer t)

(use-package eldoc
  :commands eldoc-mode)

(use-package lisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

(use-package cc-mode
  :defer t
  :config (progn
            (font-lock-add-keywords
             'c-mode
             '(("\\<\\([A-Z_]\\([A-Z_0-9]\\)*\\)\\>" . font-lock-constant-face)))
            (defun c-maybe-insert-semicolon()
              "Insert a semicolon a the end of a line only if there isn't one."
              (interactive)
              (if (looking-at " *; *$")
                  (progn (delete-region (point) (point-at-eol))
                         (call-interactively 'self-insert-command))
                (call-interactively 'self-insert-command)))
            (bind-keys :map c-mode-map
              (";" . c-maybe-insert-semicolon))))

(use-package autoinsert
  :init (progn (auto-insert-mode t)
               (setq auto-insert-prompt "insert %s? ")))

(use-package ido
  :idle (require 'ido)
  :chords ((" b" . ido-switch-buffer)
           (" f" . ido-find-file))
  :config (progn
            (ido-everywhere t)
            (ido-mode t))
  :defer t)

(use-package erc
  :defer t
  :config (progn (setq erc-hide-list '("JOIN" "PART" "QUIT")
                       erc-nick "jordonbiondo"
                       erc-port 6665
                       erc-server "irc.freenode.net")))

(use-package ediff
  :defer t
  :config (progn
            (add-hook 'ediff-before-setup-windows-hook 'winstack-push)
            (add-hook 'ediff-cleanup-hook 'winstack-pop)
            (setq ediff-split-window-function 'split-window-horizontally)
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package compile
  :defer t
  :chords (" r" . recompile)
  :config (progn (setq compilation-scroll-output t)))

(use-package hl-line
  :config (global-hl-line-mode t))

(use-package ruby-mode
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'jordon-truncate-lines)
    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))))

(use-package org
  :defer t
  :config
  (progn
    (add-hook 'org-mode-hook 'jordon-nice-wrap-mode)
    (setq org-confirm-elisp-link-function nil
          org-export-html-postamble  nil
          org-export-html-date-format-string "%d %B %Y"
          org-export-html-preamble-format `(("en" "%a : %d")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hosted Packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ample-theme
  :init (unless (systemp "eeloo")
          (load-theme 'ample t t)
          (load-theme 'ample-flat t t)
          (load-theme 'ample-light t t)
          (enable-theme 'ample-flat))
  :defer t
  :ensure t)

(use-package s
  :defer t
  :ensure t)

(use-package neotree
  :commands (neotree)
  :config (progn
            (setq neo-window-width 30)
            (setq neo-vc-integration '(face)))
  :defer t
  :ensure t)

(use-package dash
  :defer t
  :ensure t)

(use-package jabber
  :chords (("ne" . jabber-chat-with)
           ("nw" . jabber-display-roster))
  :config (progn
            (setq jabber-roster-line-format " %c %-25n %S"
                  jabber-use-sasl nil
                  jabber-history-enabled t
                  jabber-use-global-history nil
                  jabber-backlog-number 40
                  jabber-backlog-days 30
                  jabber-account-list
                  '(("jordonbiondo@gmail.com"
                     (:network-server . "talk.google.com")
                     (:connection-type . ssl))))
            (add-hook 'jabber-chat-mode 'visual-line-mode)
            (setq jabber-chat-buffer-format "Chat: %n"))
  :ensure t)

(use-package avy
  :bind ("C-c <SPC>" . avy-goto-word-1)
  :ensure t)

(use-package multiple-cursors
  :config (progn (defun jordon-mc-mark-until-line-change (&optional up)
                   (interactive "P")
                   (unless (save-excursion
                             (let ((col (current-column)))
                               (forward-line (if up -1 1))
                               (move-to-column col))
                             (looking-at "\\( +\\| *$\\)"))
                     (when up (next-line -1))
                     (mc/mark-next-lines 1)
                     (jordon-mc-mark-until-line-change up)))
                 (add-to-list 'mc/cmds-to-run-once 'jordon-mc-mark-until-line-change)
                 (bind-keys :map mc/keymap
                   ("C-c n" . mc/insert-numbers)))
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c C-m" . jordon-mc-mark-until-line-change))
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :chords (" e" . er/expand-region)
  :ensure t)

(use-package cmake-mode
  :defer t
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)

(use-package switch-window
  :defer t
  :chords (("u " . switch-window))
  :config (setq switch-window-shortcut-style 'qwerty
                switch-window-qwerty-shortcuts
                '("a" "w" "e" "f" "j" "i" "o" ";" "s" "d" "k" "l"))
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("C-c M-x" . execute-extended-command))
  :commands smex
  :ensure t)

(use-package magit
  :bind ("C-x m" . magit-status)
  :chords (" m" . magit-status)
  :commands magit-status
  :config (progn
            (bind-keys :map magit-status-mode-map
              ("C-c g o" . magit-checkout))
            (when (osxp)
              (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))
            (when (systemp "duna" "jordon")
              (add-to-list 'magit-repository-directories
                           (expand-file-name "~/src/")))
            (after (:magit-blame)
              (setq magit-blame-time-format "%m/%d/%Y"))
            (when (guip)
              (setq magit-log-format-graph-function
                    'magit-log-format-unicode-graph))
            (setq magit-status-buffer-switch-function 'switch-to-buffer
                  magit-completing-read-function 'magit-ido-completing-read
                  magit-revert-buffers 1
                  magit-push-always-verify nil))
  :ensure t)

(use-package github-browse-file
  :commands (github-browse-file)
  :bind ("C-c g h" . github-browse-file)
  :defer t
  :ensure t)

(use-package git-gutter
  :defer t
  :config
  (after (:ample-theme)
    (dolist (face '(git-gutter:added
                    git-gutter:deleted
                    git-gutter:modified
                    git-gutter:separator
                    git-gutter:unchanged))
      (set-face-background face (face-foreground face))))
  :ensure t)

(use-package git-messenger
  :config (setq git-messenger:show-detail t)
  :bind ("C-c g m" . git-messenger:popup-message)
  :ensure t)

(use-package git-timemachine
  :defer t
  :bind ("C-c g t" . git-timemachine)
  :ensure t)

(use-package gh
  :defer t
  :ensure t)

(use-package helm
  :defer t
  :init
  (use-package helm-grep
    :defer t
    :chords ("hf" . helm-do-grep))
  :ensure t)

(use-package imenu-anywhere
  :defer t
  :chords ("io" . imenu-anywhere)
  :config (progn
            (use-package cl)
            (defun jordon-imenu-show-used-packages ()
              (add-to-list 'imenu-generic-expression
                           '("Used Packages"
                             "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
            (add-late-hook '((lisp-mode lisp-mode-hook)
                             (emacs-lisp-mode emacs-lisp-mode-hook)
                             (lisp-interaction-mode lisp-interaction-mode-hook))
                           'jordon-imenu-show-used-packages)
            (defadvice imenu-anywhere--goto-function (after pulse-the-line activate)
              (pulse-momentary-highlight-one-line (point))))
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package projectile
  :commands (projectile-project-type)
  :init (add-hook 'prog-mode-hook 'projectile-mode)
  :defer t
  :ensure t)

(use-package projectile-rails
  :init (after (:projectile)
          (after (:ruby-mode) (add-hook 'ruby-mode-hook 'projectile-rails-mode))
          (after (:haml-mode) (add-hook 'haml-mode-hook 'projectile-rails-mode))
          (after (:yaml-mode) (add-hook 'yaml-mode-hook 'projectile-rails-mode))
          (after (:js2-mode) (add-hook 'js2-mode-hook 'projectile-rails-mode)))
  :defer t
  :ensure t)

(use-package rspec-mode
  :config
  (progn
    (bind-keys :map rspec-mode-map
      ("C-c ," . rspec-verify-single)
      ("C-c ." . rspec-verify))
    (bind-chords :map rspec-mode-map
      (" ," . rspec-verify-single)
      (" ." . rspec-verify)))
  :defer t
  :ensure t)

(use-package rvm
  :commands rvm-activate-corresponding-ruby
  :defer t
  :ensure t)

(use-package header2
  :ensure t)

(use-package lua-mode
  :defer t
  :ensure t)

(use-package undo-tree
  :idle (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :chords (("ji" . undo-tree-undo)
           ("jo" . undo-tree-redo)
           ("jk" . undo-tree-switch-branch)
           ("j;" . undo-tree-visualize))
  :ensure t)

(use-package google-this
  :defer t
  :ensure t)

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config
  (progn
    (add-hook 'web-mode-hook
              (defun jordon-web-mode-setup ()
                (setq web-mode-code-indent-offset 2
                      web-mode-markup-indent-offset 2
                      web-mode-attr-indent-offset 2
                      web-mode-css-indent-offset 2)))
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

(use-package php-mode
  :mode ("\\.php$" . php-mode)
  :config (add-hook 'php-mode-hook
                    (defun jordon-php-mode-setup ()
                      (setq-local c-basic-offset 4)
                      (setq-local indent-tabs-mode nil)
                      (setq-local tab-width 4)))
  :defer t
  :ensure t)

(use-package skewer-mode
  :defer t
  :init (after (:web-mode)
          (add-late-hook '((web-mode web-mode-hook)) 'skewer-mode))
  :config (skewer-setup)
  :ensure t)

(use-package edit-server
  :defer t
  :ensure t)

(use-package io-mode
  :defer t
  :ensure t)

(use-package csharp-mode
  :init (after (:cc-mode)
          (add-to-list 'c-default-style
                       (cons 'csharp-mode "c#")))
  :mode ("\\.cs$" . csharp-mode)
  :config (progn
            (add-hook 'csharp-mode-hook
                      (defun jordon-csharp-setup-function ()
                        (setq c-basic-offset 4
                              indent-tabs-mode nil)
                        (hs-minor-mode t)
                        (jordon-dont-truncate-lines)))
            (font-lock-add-keywords
             'csharp-mode
             '(("\\(// *\\)\\(todo\\)\\(.*$\\)" 2 'font-lock-warning-face t))))
  :defer t
  :ensure t)

(use-package company
  :idle (global-company-mode t)
  :ensure t)

(use-package omnisharp
  :config (progn
            (after (:csharp-mode)
              (bind-keys :map csharp-mode-map
                ("M-i" . jordon-omnisharp-go-to-definition-smart)
                ("M-m" . omnisharp-find-usages)))
            (setq omnisharp-eldoc-support t)
            (add-hook 'csharp-mode-hook 'eldoc-mode)

            (defun jordon-omnisharp-go-to-definition-smart (&optional force-ow)
              "Goto definition at point, choose window intelligently."
              (interactive "P")
              (let* ((json-result (omnisharp-post-message-curl-as-json
                                   (concat (omnisharp-get-host)
                                           "gotodefinition")
                                   (omnisharp--get-common-params)))
                     (filename (cdr (assoc 'FileName json-result))))
                (if (null filename)
                    (message "Cannot go to definition: API returned none.")
                  (omnisharp-go-to-file-line-and-column
                   json-result
                   (or force-ow (not (equal (omnisharp--convert-backslashes-to-forward-slashes filename)
                                            (buffer-file-name)))))
                  (pulse-momentary-highlight-one-line (point)))))
            (after (:company :csharp-mode)
              (add-to-list 'company-backends 'company-omnisharp)
              (add-hook 'csharp-mode-hook 'company-mode)
              (add-hook 'csharp-mode-hook 'omnisharp-mode)))
  :if (not (systemp "eeloo" "pi"))
  :ensure t)

(use-package flycheck
  :init (after (:cc-mode)
          (add-hook 'c-mode-hook 'flycheck-mode)
          (add-hook 'c++-mode-hook 'flycheck-mode))
  :defer t
  :ensure t)

(use-package auto-complete
  :defer t
  :config (progn
            (use-package auto-complete-config)
            (after (:slime) (add-to-list 'ac-modes 'slime-repl-mode))
            (after (:js2-mode) (add-to-list 'ac-modes 'js2-mode))
            (after (:js-mode) (add-to-list 'ac-modes 'js-mode))
            (after (:ruby-mode) (add-to-list 'ac-modes 'ruby-mode))
            (ac-config-default))
  :ensure t)

(use-package bundler
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package highlight-indentation
  :defer t
  :ensure t)

(use-package sass-mode
  :defer t
  :config (progn
            (add-hook 'sass-mode-hook 'flycheck-mode)
            (add-hook 'sass-mode-hook (apply-partially 'electric-indent-mode -1)))
  :ensure t)

(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode jordon-nice-wrap-mode)
  :config (defun jordon-nice-wrap-mode ()
            (setq truncate-lines nil)
            (visual-line-mode t)
            (adaptive-wrap-prefix-mode t)
            (electric-indent-mode -1))
  :defer t
  :ensure t)

(use-package haml-mode
  :defer t
  :config
  (progn
    (add-hook 'haml-mode-hook 'flycheck-mode)
    (add-hook 'haml-mode-hook 'jordon-nice-wrap-mode))
  :ensure t)

(use-package robe
  :defer t
  :commands (robe-mode)
  :init
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'robe-mode-hook
              (defun jordon-robe-setup ()
                (company-mode t)
                (add-to-list 'company-backends 'company-robe))))
  :ensure t)

(use-package moz
  :defer t
  :commands js2-mode
  :config (progn
            (defun jordon-moz-refresh ()
              (interactive)
              (if (ignore-errors
                    (comint-send-string
                     (inferior-moz-process)
                     "setTimeout(BrowserReload(), \"1000\");") t)
                  (message "Moz Refreshing...")))

            (define-key moz-minor-mode-map (kbd "C-M-o") 'jordon-moz-refresh)
            (after (:js2-mode)
              (lambda () (add-hook 'js2-mode-hook 'moz-minor-mode)))
            (after (:ruby-mode)
              (lambda () (add-hook 'ruby-mode-hook 'moz-minor-mode)))
            (after (:haml-mode)
              (add-hook 'haml-mode-hook 'moz-minor-mode)))
  :ensure t)

(use-package d-mode
  :defer t
  :ensure t)

(use-package scala-mode2
  :defer t
  :ensure t)

(use-package ensime
  :commands (ensime-mode)
  :init (add-hook 'scala-mode-hook
                  (defun jordon-maybe-ensime-mode ()
                    (when (equal (projectile-project-type) 'sbt)
                      (ensime-mode t))))
  :defer t
  :ensure t)

(use-package json-mode
  :defer t
  :ensure t)

(use-package restclient
  :init (add-hook 'restclient-mode-hook
                  (defun jordon-setup-restclient-mode ()
                    (require 'js)
                    (setq-local indent-line-function 'js-indent-line)))
  :config
  (progn
    (add-hook 'restclient-response-loaded-hook
              (defun maybe-prettify-restclient-errors ()
                (let ((things '("&lt;" "<br>" "&nbsp;" "\\n")))
                  (when (-any?
                         (lambda (str)
                           (save-excursion (search-forward str nil t 1)))
                         things)
                    (let ((reps '(("<br>" . "\n") ("&nbsp;" . " ") ("\\n" . "\n"))))
                      (dolist (rep reps)
                        (replace-string (car rep) (cdr rep) nil 1 (point-max))))))))
    (add-hook 'restclient-response-loaded-hook
              (defun jordon-restclient-delete-headers-when-ok ()
                (when (equal major-mode 'js-mode)
                  (save-excursion
                    (goto-char (point-max))
                    (when (and (search-backward "200 OK" nil t)
                               (search-backward "}" nil t))
                      (forward-char 1)
                      (delete-region (point) (point-max))
                      (json-mode))))))
    (add-hook 'restclient-response-loaded-hook 'jordon-nice-wrap-mode))
  :defer t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init (add-hook 'js2-mode-hook
                  (defun jordon-js2-mode-setup ()
                    (flycheck-mode t)
                    (when (executable-find "eslint")
                      (flycheck-select-checker 'javascript-eslint))))
  :config
  (progn
    (defun jordon-js2-log-arguments ()
      (interactive)
      (save-excursion
        (when (and (beginning-of-defun) (search-forward "function") (search-forward "("))
          (let ((args (buffer-substring-no-properties
                       (point)
                       (progn (backward-char 1) (forward-sexp 1) (1- (point))))))
            (search-forward "{")
            (insert "\nconsole.log({"
                    (mapconcat (lambda (arg) (format "%s: %s" (s-trim arg) (s-trim arg)))
                               (split-string args ", " t) ", ")
                    "});")
            (call-interactively 'indent-for-tab-command)))))
    (bind-keys :map js2-mode-map
      ("C-c n l" . jordon-js2-log-arguments)
      ("C-c n f a" . ffap))
    (setq-default js2-basic-offset 4)
    (setq js-switch-indent-offset js2-basic-offset)
    (setq-default
     js2-global-externs
     '("clearTimeout" "setTimeout" "module" "require"))
    (mapc (lambda (abp)
            (define-abbrev js2-mode-abbrev-table (car abp) (cdr abp)))
          '(("fn" . "function")
            ("tn" . "then")
            ("pr" . "Promise")
            ("re" . "require")
            ("gro" . "global.rootRequire")))
    (add-hook 'js2-mode-hook 'abbrev-mode)
    (add-hook 'js2-mode-hook 'jordon-nice-wrap-mode t))
  :ensure t)

(use-package js2-refactor
  :defer t
  :commands (js2r-add-keybindings-with-prefix)
  :init (after :js2-mode
          (js2r-add-keybindings-with-prefix "C-c u")
          (add-hook 'js2-mode-hook 'js2-refactor-mode))
  :ensure t)

(use-package ac-js2
  :defer t
  :ensure t)

(use-package slime
  :defer t
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
  :defer t
  :if (osxp)
  :idle (exec-path-from-shell-initialize)
  :ensure t)

(use-package dired-subtree
  :commands dired
  :config (after (:dired)
            (bind-keys :map dired-mode-map
              ("C-c C-i" . dired-subtree-insert)
              ("C-c C-r" . dired-subtree-remove)
              ("C-c C-g" . dired-subtree-revert)))
  :ensure t)

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config (setq rust-indent-offset 2)
  :defer t
  :ensure t)

(use-package paredit
  :defer t
  :init (after (:csharp-mode :paredit)
          (add-hook 'paredit-space-for-delimiter-predicates
                    (lambda (&rest args)
                      (not (equal major-mode 'csharp-mode)))))
  :ensure t)

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package twittering-mode
  :defer t
  :ensure t)

(use-package flx-ido
  :defer t
  :init (after (:ido)
          (unless (systemp "eeloo" "pi")
            (flx-ido-mode t)))
  :ensure t)

(use-package ido-vertical-mode
  :defer t
  :init (after (:ido)
          (ido-vertical-mode t))
  :ensure t)

(use-package ido-ubiquitous
  :defer t
  :init (after (:ido)
          (ido-ubiquitous-mode t))
  :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package po-mode
  :load-path "/usr/local/Cellar/gettext/0.19.5.1/share/emacs/site-lisp/"
  :commands (po-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
