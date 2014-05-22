(require 'jorbi-dev)
(require 'jorbi-fns)

;;(set-default-font "-outline-Segoe UI Mono-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")


(defdev "jordon"
  (when jordon-dev-mode
    (ignore-errors
      (set-default-font "-outline-Inconsolata-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")

      (set-face-attribute 'default nil :height 85))

    (when jordonp
      (setq user-mail-address "jordon.biondo@salespad.net")))

  (define-keys jordon-dev-mode-map
    ("C-\\" 'jorbi/toggle-comment)
    ("C-x l" 'ibuffer)
    ("C-<tab>" 'jorbi/indent-repeat)
    ("<f12>" 'normal-person-mode)
    ("C-c f u" 'winstack-push)
    ("C-c f o" 'winstack-pop)
    ("C-M-k" 'jorbi/c-doc-comment)
    ("C-c i" 'previous-multiframe-window)
    ("C-c o" 'next-multiframe-window)))


(provide 'jordon-mode)
