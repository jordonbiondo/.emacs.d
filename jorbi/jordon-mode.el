(require 'jorbi-dev)
(require 'jorbi-fns)

(defdev "jordon"
  (set-default-font "-apple-Inconsolata-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (define-keys jordon-dev-mode-map
    ("C-\\" 'jorbi/toggle-comment-line)
    ("C-x l" 'ibuffer)
    ("C-<tab>" 'jorbi/indent-repeat)
    ("C-c f u" 'winstack-push)
    ("C-c f o" 'winstack-pop)
    ("C-M-k" 'jorbi/c-doc-comment)
    ("C-c i" 'previous-multiframe-window)
    ("C-c o" 'next-multiframe-window)))


(provide 'jordon-mode)
