;;; init.el --- Jordon Biondo's emacs configuration
;;
;; Filename: init.el
;; Description: Jordon Biondo's emacs configuration
;; Author: Jordon Biondo
;; Created: Mon Oct 14 11:37:26 2013 (-0400)
;; Version: 2.3.1
;; Last-Updated: Thu Apr 23 09:15:09 2015 (-0400)
;;           By: Jordon Biondo
;;     Update #: 36
;; URL: www.github.com/jordonbiondo/.emacs.d
;; Keywords: Emacs 24.4
;; Compatibility: emacs >= 24.4
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jordonp ()
  "Are you jordon?"
  (or (equal (getenv "USER") "jordon")
      (equal (getenv "USERNAME") "jordon")))

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode))

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up package.el and use-package for init
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/jorbi/"
        "~/.emacs.d/jorbi/use-package/"))

(require 'jorbi-package)
(use-package cl-lib)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jorbi/ configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jorbi-fns
  :chords ((" =" . winstack-push)
           (" -" . winstack-pop)
           ("nv" . jorbi/find-init-file)
           (" \\". jorbi/toggle-comment))
  :bind (("C-\\" . jorbi/toggle-comment)
         ("C-<tab>" . jorbi/indent-repeat)
         ("C-M-k" . jorbi/c-doc-comment)
         ("C-c f u" . winstack-push)
         ("C-c f o" . winstack-pop))
  :config (when (OSX) (push "/usr/local/bin/" exec-path))
  :demand t)

(use-package jorbi-jabber
  :init (after (:jabber)
          (require 'jorbi-jabber))
  :config  (progn
             (setq jabber-chat-header-line-format
                   jorbi-jabber/chat-header-line-format)
             (cond
              ((OSX)
               (add-hook 'jabber-alert-message-hooks
                         'jorbi-jabber/terminal-notification))
              ((Windows)
               (add-hook 'jabber-alert-message-hooks
                         'jorbi-jabber/toast-notification)
               (add-hook 'jabber-alert-message-hooks
                         'jorbi-jabber/send-mail-notification))))
  :defer t)

(use-package jorbi-mode-line
  :config (setq-default mode-line-format jorbi/mode-line-format))

;; work related
(when (and (OSX) (jordonp))
  (ignore-errors
    (set-default-font "Envy Code R")
    (set-face-attribute 'default nil :height 125))
  (setq user-mail-address "jordon.biondo@parelio.com")
  (setq-default scroll-margin 5)
  (setq-default scroll-step 1)
  (add-to-list 'load-path "~/src/redspot-emacs/")
  (use-package redspot
    :config
    (progn
      (after (:projectile)
        (defun jorbi/redspot-activate-rvm-once ()
          (when (equal (projectile-project-name) "redspot")
            (rvm-activate-corresponding-ruby)
            (remove-hook 'ruby-mode-hook 'jorbi/redspot-activate-rvm-once)))
        (add-hook 'ruby-mode-hook 'jorbi/redspot-activate-rvm-once))
      (after (:js2-mode)
        (bind-keys
         :map js2-mode-map
         ("M-." . redspot:find-js-definition-here)
         ("C-c n m" . redspot:mvp-mode)
         ("C-c n t" . redspot:mvp-triplet-select)
         ("C-c n c" . redspot:js-console-this-line)
         ("C-c n l" . redspot:js-log-arguments)
         ("C-c n a" . redspot:application.js-go)))
      (after (:haml-mode)
        (define-key haml-mode-map
          (kbd "C-c n p") 'redspot:haml-find-partial-at-point)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hosted Packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package key-chord
  :config (progn (key-chord-mode t)
                 (setq key-chord-two-keys-delay .020
                       key-chord-one-key-delay .020))
  :ensure t)

(use-package ample-theme
  :idle (progn (load-theme 'ample-flat t) (enable-theme 'ample-flat))
  :ensure t)

(use-package s
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
                     (:connection-type . ssl))
                    ("jordon.biondo@parelio.com"
                     (:network-server . "talk.google.com")
                     (:connection-type . ssl))))
            (add-hook 'jabber-chat-mode 'visual-line-mode)
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
                     (when up (next-line -1))
                     (mc/mark-next-lines 1)
                     (jorbi/mc/mark-until-line-change up)))
                 (push 'jorbi/mc/mark-until-line-change mc/cmds-to-run-once)
                 (bind-key "C-c n" 'mc/insert-numbers mc/keymap))
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c C-m" . jorbi/mc/mark-until-line-change))
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
  :chords (("jf" . switch-window))
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
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn
            (when (OSX) (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))
            (setq magit-status-buffer-switch-function 'switch-to-buffer)
            (use-package jorbi-magit
              :commands 'jorbi-magit/cleanup-this-hunk
              :init (bind-key "C-c s d" 'jorbi-magit/cleanup-this-hunk
                              magit-status-mode-map)))
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
  :bind ("C-c g m" . git-messenger:popup-message))

(use-package gh
  :defer t
  :ensure t)

(use-package helm
  :defer t
  :init (use-package helm-grep
          :defer t
          :chords ("hf" . helm-do-grep))
  :ensure t)

(use-package imenu-anywhere
  :defer t
  :chords ("io" . imenu-anywhere)
  :config (progn
            (use-package cl)
            (defun jorbi/imenu-show-used-packages ()
              (add-to-list 'imenu-generic-expression
                           '("Used Packages"
                             "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
            (add-late-hook '((lisp-mode lisp-mode-hook)
                             (emacs-lisp-mode emacs-lisp-mode-hook)
                             (lisp-interaction-mode lisp-interaction-mode-hook))
                           'jorbi/imenu-show-used-packages)
            (defadvice imenu-anywhere--goto-function (after pulse-the-line activate)
              (pulse-momentary-highlight-one-line (point))))
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package projectile
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

(use-package rvm
  :commands rvm-activate-corresponding-ruby
  :defer t)

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
  :config (defun web-indirect-this-thing()
            (interactive)
            (let ((beg 0) (end 0))
              (save-excursion
                (setq beg (progn (web-mode-forward-sexp -1)
                                 (call-interactively 'web-mode-tag-end)
                                 (point)))
                (setq end (progn  (web-mode-forward-sexp 1)
                                  (point))))
              (indirect-region beg end)))
  :ensure t)

(use-package skewer-mode
  :defer t
  :init (after (:web-mode)
          (add-hook 'web-mode-hook 'skewer-mode))
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
                      (defun jorbi/csharp-setup-function ()
                        (setq c-basic-offset 4
                              indent-tabs-mode nil)
                        (hs-minor-mode t)
                        (jorbi/dont-truncate-lines)))
            (font-lock-add-keywords
             'csharp-mode
             '(("\\(// *\\)\\(todo\\)\\(.*$\\)" 2 'font-lock-warning-face t))))
  :ensure t)

(use-package company
  :idle (global-company-mode t)
  :ensure t)

(use-package omnisharp
  :bind (("M-i" . jorbi/omnisharp-go-to-definition-smart)
         ("M-m" . omnisharp-find-usages))
  :config (progn
            (setq omnisharp-eldoc-support t)
            (add-hook 'csharp-mode-hook 'eldoc-mode)

            (defun jorbi/omnisharp-go-to-definition-smart (&optional force-ow)
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

            (defun jorbi/reload-csharp-buffers ()
              "Restart `csharp-mode' on all `csharp-mode' buffers."
              (interactive)
              (dolist (b (buffer-list))
                (with-current-buffer b
                  (when (eql major-mode 'csharp-mode)
                    (csharp-mode)))))

            (after (:company :csharp-mode)
              (add-to-list 'company-backends 'company-omnisharp)
              (add-hook 'csharp-mode-hook 'company-mode)
              (add-hook 'csharp-mode-hook 'omnisharp-mode)))
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
  :defer t
  :ensure t)

(use-package haml-mode
  :defer t
  :config
  (progn
    (defun jorbi-haml/setup-hook ()
      (flycheck-mode t)
      (setq truncate-lines nil)
      (visual-line-mode t)
      (adaptive-wrap-prefix-mode t)
      (electric-indent-mode -1))
    (add-hook 'haml-mode-hook 'jorbi-haml/setup-hook))
  :ensure t)

(use-package robe
  :defer t
  :config (progn
            (after (:ruby-mode)
              (add-hook 'ruby-mode-hook 'robe-mode))
            (add-hook 'robe-mode-hook
                      (defun jorbi-robe/setup-completeion()
                        (company-mode t)))
            (after (:company)
              (lambda () (progn (push 'company-robe company-backends)))))
  :ensure t)

(use-package moz
  :defer t
  :commands js2-mode
  :config (progn
            (defun jorbi-moz/refresh ()
              (interactive)
              (if (ignore-errors
                    (comint-send-string
                     (inferior-moz-process)
                     "setTimeout(BrowserReload(), \"1000\");") t)
                  (message "Moz Refreshing...")))

            (define-key moz-minor-mode-map (kbd "C-M-o") 'jorbi-moz/refresh)
            (after (:js2-mode)
              (lambda () (add-hook 'js2-mode-hook 'moz-minor-mode)))
            (after (:ruby-mode)
              (lambda () (add-hook 'ruby-mode-hook 'moz-minor-mode)))
            (after (:haml-mode)
              (add-hook 'haml-mode-hook 'moz-minor-mode)))
  :ensure t)

(use-package rsense
  :defer t
  :ensure t)

(use-package d-mode
  :defer t
  ;; :if (executable-find "dmd")
  ;; old windows config
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
  :ensure t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config (progn
            (setq-default js2-basic-offset 2
                          js2-global-externs '("clearTimeout" "setTimeout"))
            (font-lock-add-keywords
             'js2-mode
             '(("\\(console\\)\\(\.\\)\\(log\\|trace\\)"
                (1 font-lock-warning-face t)
                (3 font-lock-warning-face t))))
            (use-package ac-js2
              :ensure t)

            (use-package js2-refactor
              :config (js2r-add-keybindings-with-prefix "C-c u")
              :ensure t))
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

(when (OSX)

  (global-unset-key (kbd "s-t"))

  (use-package exec-path-from-shell
    :idle (exec-path-from-shell-initialize)
    :ensure t))

(use-package dired-subtree
  :commands dired
  :config (after (:dired)
            (bind-keys
             :map dired-mode-map
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
          (flx-ido-mode t))
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
;; built-ins
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :chords (" g" . goto-line))

(use-package flymake
  :commands flymake-mode)

(use-package custom
  :init (setq custom-file "~/.emacs.d/custom.el"))

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
  :config (add-hook 'grep-mode-hook 'jorbi/truncate-lines))

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
  :config (add-hook 'eww-mode-hook 'jorbi/dont-truncate-lines))

(use-package electric
  :config (progn))

(use-package prog-mode
  :init (progn
          (after (:electric)
            (add-hook 'prog-mode-hook 'electric-indent-mode))
          (add-hook 'prog-mode-hook
                    (defun indent-tabs-mode-off ()
                      (interactive)
                      (setq indent-tabs-mode nil)))))

(use-package python
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config (progn
            (use-package elpy
              :config (elpy-enable)
              :ensure t)))

(use-package savehist
  :idle (savehist-mode t))

;; (use-package desktop
;;   :config (progn (desktop-save-mode t)
;;                  (setq desktop-path '("~/.emacs.d/"))))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding))

(use-package ispell
  :bind (("C-c s w" . ispell-word)
         ("C-c s b" . ispell-buffer)))

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
             '(("\\<\\([A-Z_]\\([A-Z_0-9]\\)*\\)\\>"
                . font-lock-constant-face)
               ("\\(\\<\\(def_\\)?rs\\$ *\\)\\>"
                . font-lock-preprocessor-face)))

            (defun c-maybe-insert-semicolon()
              "Insert a semicolon a the end of a line only if there isn't one."
              (interactive)
              (if (looking-at " *; *$")
                  (progn (delete-region (point) (point-at-eol))
                         (call-interactively 'self-insert-command))
                (call-interactively 'self-insert-command)))

            (define-key c-mode-map (kbd ";") 'c-maybe-insert-semicolon)))

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

(use-package php-mode
  :mode ("\\.php$" . php-mode)
  :config (add-hook 'php-mode-hook
                    (defun jorbi/php-mode-setup ()
                      (setq-local c-basic-offset 4)
                      (setq-local indent-tabs-mode nil)
                      (setq-local tab-width 4))))

(use-package ruby-mode
  :defer t
  :config (progn
            (add-hook 'ruby-mode-hook 'jorbi/truncate-lines)
            (after (:flycheck) (add-hook 'ruby-mode-hook 'flycheck-mode))
            (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))))

(use-package org
  :defer t
  :config (setq org-confirm-elisp-link-function nil
                org-export-html-postamble  nil
                org-export-html-date-format-string "%d %B %Y"
                org-export-html-preamble-format `(("en" "%a : %d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
