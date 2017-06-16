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

;;(package-initialize)

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
           (" e" . jordon-mark-a-thing)
           (" \\". jordon-toggle-comment))
  :bind (("C-\\" . jordon-toggle-comment)
         ("C-<tab>" . jordon-indent-repeat)
         ("C-M-k" . jordon-c-doc-comment)
         ("C-c f u" . winstack-push)
         ("C-c f o" . winstack-pop))
  :config
  (progn
    (when (osxp)
      (push "/usr/local/bin/" exec-path))
    (after (:lisp-mode)
      (bind-keys :map lisp-interaction-mode-map
        ("C-c C-j" . eval-and-replace-sexp))))
  :defer t)

(use-package key-chord
  :ensure t
  :config
  (progn (key-chord-mode t)
         (setq key-chord-two-keys-delay .020
               key-chord-one-key-delay .020)))

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
    (bind-chord
     "m5"
     (defun jordon-buffer-md5 ()
       (interactive)
       (print (md5 (current-buffer)))))
    (ignore-errors
      (set-default-font "Envy Code R")
      (set-face-attribute 'default nil :height 125))
    (when (guip)
      (global-unset-key (kbd "s-t")))
    (setq-default scroll-margin 5)
    (setq-default scroll-step 1))
   ("jordon"
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq user-mail-address "jordon.biondo@appropos.com")
    (setq initial-scratch-message "\n;; Welcome Back\n\n")
    (use-package spinner
      :defer t)
    (use-package awt
      :load-path "~/src/awt-emacs/"
      :init
      (progn
        (after (:js2-mode)
          (bind-keys :map js2-mode-map
            ("C-c n f m" . awt-find-model)
            ("C-c n r m" . awt-require-model)
            ("C-c n r r" . awt-root-require)
            ("C-c n f c" . awt-find-controller)
            ("C-c n t" . awt-run-test-file)
            ("C-c n s" . awt-run-current-test-in-file)))
        (after (:magit)
          (bind-keys :map git-commit-mode-map
            ("C-c n j" . jira-issues))))
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

(use-package "indent"
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

(use-package "window"
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
            (setq pulse-iterations 8
                  pulse-delay .05)
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
  :defer 1
  :config (savehist-mode t))

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
  :defer 1
  :config (progn (auto-insert-mode t)
                 (setq auto-insert-prompt "insert %s? ")))

(use-package ido
  :chords ((" b" . ido-switch-buffer)
           (" f" . ido-find-file))
  :config (progn
            (ido-everywhere t)
            (ido-mode t))
  :defer 1)

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

(use-package comint
  :defer t
  :config
  (progn
    (add-hook
     'comint-mode-hook
     (defun jordon-comint-mode-setup ()
       (add-to-list
        'comint-preoutput-filter-functions
        (lambda (output)
          (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))
    (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)))

(use-package ansi-color
  :commands (ansi-color-for-comint-mode-on)
  :defer t)

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
          (load-theme 'ample-flat t t)
          (enable-theme 'ample-flat))
  :defer t
  :ensure t)

(use-package s
  :defer t
  :ensure t)

(use-package dash
  :defer t
  :ensure t)

(use-package neotree
  :commands (neotree jordon-neotree-toggle-project)
  :chords ((" 8" . jordon-neotree-toggle-project))
  :config (progn
            (defun jordon-neotree-toggle-project ()
              (interactive)
              (if (and (neo-global--window-exists-p)
                       (equal (projectile-project-root)
                              (with-current-buffer (neo-global--get-buffer)
                                default-directory)))
                  (neotree-hide)
                (neotree-projectile-action)))
            (setq neo-theme 'nerd)
            (setq neo-window-width 30)
            (setq neo-vc-integration '(face)))
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
  :defer t
  :ensure t)

(use-package avy
  :bind ("C-c <SPC>" . avy-goto-word-1)
  :ensure t)

(use-package dumb-jump
  :chords (("j " . dumb-jump-go)
           ("k " . dumb-jump-back)
           ("h " . dumb-jump-quick-look))
  :config
  (add-hook 'dumb-jump-after-jump-hook
            (defun jordon-dumb-jump-pulse-line ()
              (pulse-momentary-highlight-one-line (point))))
  :ensure t)

(use-package multiple-cursors
  :config (progn (defun jordon-mc-mark-until-line-change (&optional up)
                   (interactive "P")
                   (let ((lines 0)
                         (col (current-column)))
                     (save-excursion
                       (forward-line 1)
                       (move-to-column col)
                       (while (not (looking-at "\\( +\\| *$\\)"))
                         (incf lines)
                         (forward-line 1)
                         (move-to-column col)))
                     (unless (zerop lines)
                       (mc/keyboard-quit)
                       (mc/mark-next-like-this lines))))
                 (add-to-list 'mc/cmds-to-run-once 'jordon-mc-mark-until-line-change)
                 (bind-keys :map mc/keymap
                   ("C-c n" . mc/insert-numbers)))
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c C-m" . jordon-mc-mark-until-line-change))
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
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

(use-package magit
  :bind (("C-x m" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file))
  :chords (" m" . magit-status)
  :commands magit-status
  :config (progn
            (bind-keys :map magit-status-mode-map
              ("C-c g o" . magit-checkout)
              ("C-c g r" . magit-diff-toggle-refine-hunk))
            (when (osxp)
              (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))
            (when (systemp "duna" "jordon")
              (add-to-list 'magit-repository-directories
                           (expand-file-name "~/src/")))
            (add-hook
             'magit-section-set-visibility-hook
             (defun jordon-magit-section-visibility (section)
               (and (member (magit-section-type section) '(stashes))
                    'hide)))
            (after (:magit-blame)
              (setq magit-blame-heading-format "%-20a %A %s")
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
  :commands (git-timemachine)
  :bind ("C-c g t" . git-timemachine)
  :defer t
  :ensure t)

(use-package gh
  :defer t
  :ensure t)

(use-package helm
  :defer t
  :bind (("M-x" . helm-M-x)
         ("C-c M-x" . execute-extended-command))
  :chords ("io" . helm-M-x)
  :config
  (progn
    (require 'helm-command)
    (setq helm-M-x-fuzzy-match t))
  :init
  (use-package helm-grep
    :defer t
    :chords ("hf" . helm-do-grep))
  :ensure t)

(use-package imenu-anywhere
  :defer t
  :chords ("jo" . imenu-anywhere)
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
  :config (setq projectile-enable-caching t)
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
  :defer 1
  :config (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package google-this
  :defer t
  :ensure t)

(use-package web-mode
  :mode ("\\.\\(html\\|hbs\\)$" . web-mode)
  :config
  (progn
    (add-hook 'web-mode-hook
              (defun jordon-web-guess-engine ()
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward-regexp " ng-[a-z-]+=" nil t 1)
                    (web-mode-set-engine "angular")))))
    (add-hook 'web-mode-hook
              (defun jordon-web-mode-setup ()
                (let ((offset
                       (if (and (buffer-file-name)
                                (string-match-p  "\.hbs$" (buffer-file-name)))
                           2
                         4)))
                  (setq web-mode-code-indent-offset offset
                        web-mode-markup-indent-offset offset
                        web-mode-attr-indent-offset offset
                        web-mode-css-indent-offset offset))))
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
  :config (global-company-mode t)
  :defer 2
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
  :defer t
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

(use-package scala-mode
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

(use-package dedicated
  :defer 2
  :ensure t)

(use-package restclient
  :init (add-hook 'restclient-mode-hook
                  (defun jordon-setup-restclient-mode ()
                    (require 'js)
                    (setq-local indent-line-function 'js-indent-line)
                    (setq restclient-inhibit-cookies t)))
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode)
  :config
  (progn
    (add-hook 'restclient-response-loaded-hook
              (defun maybe-prettify-restclient-errors ()
                (if (search-forward "\0" nil t 1)
                    (let ((size (buffer-size)))
                      (delete-region (point-min) (point-max))
                      (insert (format "Binary file: %s" size)))
                  (let ((things '("&lt;" "<br>" "&nbsp;" "\\n")))
                    (when (-any?
                           (lambda (str)
                             (save-excursion (search-forward str nil t 1)))
                           things)
                      (let ((reps '(("<br>" . "\n") ("&nbsp;" . " ") ("\\n" . "\n"))))
                        (dolist (rep reps)
                          (replace-string (car rep) (cdr rep) nil 1 (point-max)))))
                    (when (save-excursion
                            (goto-char (point-min))
                            (search-forward "<!DOCTYPE html>" (line-end-position) t 1))
                      (message "Rendering html...")
                      (let ((file (make-temp-file "restreponse" nil ".html"))
                            (str (buffer-string)))
                        (with-temp-file file (insert str))
                        (let ((response-text
                               (save-window-excursion
                                 (eww-open-file file)
                                 (let ((str (buffer-string)))
                                   (kill-buffer (current-buffer))
                                   str))))
                          (setf (buffer-string) response-text)
                          (goto-char (point-min))
                          (when (save-excursion (search-forward-regexp "\\_<def " nil t 1))
                            (ruby-mode)))))))))
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
    (add-hook 'restclient-response-loaded-hook 'jordon-nice-wrap-mode)
    (add-hook 'restclient-response-loaded-hook
              (defun pulse-entire-buffer ()
                (save-excursion
                  (goto-char (point-min))
                  (pulse-momentary-highlight-region (point-min) (point-max))))))
  :defer t)

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :bind (:map js2-mode-map
              ("C-c n l" . jordon-js2-log-arguments)
              ("C-c n f a" . ffap))
  :init
  (progn
    (add-hook 'js2-mode-hook 'jordon-nice-wrap-mode t)
    (add-hook 'js2-mode-hook
              (defun jordon-js2-determine-indent ()
                (setq-local
                 js2-basic-offset
                 (let ((root (ignore-errors (projectile-project-root))))
                   (if root
                       (let ((default-directory root))
                         (if (file-exists-p "ember-cli-build.js") 2 4))
                     4)))))
    (add-hook 'js2-mode-hook
              (defun jordon-js2-mode-setup ()
                (flycheck-mode t)
                (when (executable-find "eslint")
                  (flycheck-select-checker 'javascript-eslint))))
    (add-hook 'js2-mode-hook
              (defun jordon-js2-setup-for-tests ()
                (when (string-match-p "^.*tests?\\.js" (buffer-file-name))
                  (push "msg.no.side.effects" jordon-js2-ignored-warnings)
                  (dolist (extern '("after" "before" "beforeEach" "afterEach"
                                    "describe" "it" "run" "xit" "xdescribe"))
                    (add-to-list 'js2-global-externs extern)))
                (dolist (ig '("msg.return.inconsistent"
                              "msg.anon.no.return.value"
                              "msg.no.return.value"))
                  (push ig jordon-js2-ignored-warnings)))))
  :config
  (progn
    (defvar-local jordon-js2-ignored-warnings nil)
    (defun jordon-filter-js2-warnings ()
      "Filter out warnings from `js2-parsed-warnings'."
      (setq js2-parsed-warnings
            (cl-remove-if
             (lambda (warning)
               (let ((msg (caar warning)))
                 (and msg (member msg jordon-js2-ignored-warnings))))
             js2-parsed-warnings)))
    (add-hook 'js2-post-parse-callbacks
              'jordon-filter-js2-warnings)
    (setq-default js2-basic-offset 4)
    (setq js-switch-indent-offset js2-basic-offset)
    (set-default
     (make-variable-buffer-local 'js2-global-externs)
     '("clearTimeout" "setTimeout" "module" "require" "_")))
  :defer t
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
  :defer 1
  :if (osxp)
  :config (exec-path-from-shell-initialize)
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
(put 'downcase-region 'disabled nil)
