;;; jorbi-magit.el ---
;;
;; Filename: jorbi-magit.el
;; Description: Functions for magit.
;; Author: Jordon Biondo
;; Package-Requires: ()
;; Last-Updated: Mon Feb  9 11:09:26 2015 (-0500)
;;           By: Jordon Biondo
;;     Update #: 5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'magit)

(defun jorbi-magit/-line-region-of-section-at-point ()
  "If point is in a hunk return a list of info about the hunk.

The info is like (expanded-file-name starting-line number-of-lines-show)"
  (let* ((section (magit-current-section))
         (type (magit-section-type section)))
    (when (equal type 'hunk)
      (let* ((info
              (mapcar 'string-to-number
                      (split-string
                       (second (split-string
                                (magit-section-info
                                 (magit-current-section))
                                "[ @]" t))
                       ",")))
             (start-line (car info))
             (line-count (or (and (cdr info) (cadr info)) 1)))
        (let ((parent (magit-section-parent section)))
          (while (and parent
                      (not (equal (magit-section-type parent)
                                  'diff)))
            (setq magit-section-parent parent))
          (list (expand-file-name (magit-section-info parent))
                start-line
                line-count))))))

(defun jorbi-magit/delete-hunk-trailing-whitespace ()
  "Run `delete-trailing-whitespace' on the region shown in the hunk under point."
  (interactive)
  (let ((area (jorbi-magit/-line-region-of-section-at-point)))
    (if area
        (progn
          (destructuring-bind (file start-line total-lines) area
            (with-current-buffer (find-file-noselect file)
              (save-some-buffers)
              (save-excursion
                (delete-trailing-whitespace
                 (progn (goto-char (point-min))
                        (forward-line (1- start-line))
                        (point-at-bol))
                 (progn (forward-line (1- total-lines))
                        (point-at-eol))))
              (save-buffer)))
          (magit-refresh))
      (message "Not in a magit hunk!"))))

(provide 'jorbi-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-magit.el ends here
