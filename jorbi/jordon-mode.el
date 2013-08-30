(require 'jorbi-dev)

(defdev "jordon"
  (define-keys jordon-dev-mode-map
    ("C-," 'previous-multiframe-window)
    ("C-." 'next-multiframe-window)))


(provide 'jordon-mode)
