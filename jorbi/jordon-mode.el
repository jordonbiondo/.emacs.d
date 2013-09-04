(require 'jorbi-dev)
(require 'jorbi-fns)

(defdev "jordon"
  (set-default-font "-apple-Inconsolata-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (define-keys jordon-dev-mode-map
    ("C-," 'previous-multiframe-window)
    ("C-<tab>" 'jorbi/indent-repeat)
    ("C-." 'next-multiframe-window)))


(provide 'jordon-mode)
