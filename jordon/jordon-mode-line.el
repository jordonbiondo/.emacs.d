;;; jordon-mode-line.el --- Personal mode-line format
;;
;; Filename: jordon-mode-line.el
;; Description: Personal mode-line format
;; Author: Jordon Biondo <jordonbiondo@gmail.com>
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

(defun jordon-fancy-mode-line-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with 
LEFT, CENTER, and RIGHT spaced out accordingly, LPAD and RPAD,
can be used to add a number of spaces to the front and back of the string."
  (condition-case err
      (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
             (right (if rpad (concat right (make-string rpad ?\s)) right))
             (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
             (total-length (+ (length left) (length center) (length right) 2)))
        (when (> total-length width) (setq left "" right ""))
        (let* ((left-space (/ (- width (length center)) 2))
               (right-space (- width left-space (length center)))
               (lspaces (max (- left-space (length left)) 1))
               (rspaces (max (- right-space (length right)) 1 0)))
          (concat left (make-string lspaces  ?\s)
                  center
                  (make-string rspaces ?\s)
                  right)))
    (error (format "[%s]: (%s) (%s) (%s)" err left center right))))

(defvar jordon-mode-line-format
  '((:eval (jordon-fancy-mode-line-render
            (format-mode-line (format " %s (%%l/%d) %%c "
                                      (downcase (format-mode-line mode-name))
                                      (line-number-at-pos (point-max))))
            (concat (buffer-name)
                    (cond
                     ((not (buffer-file-name)) " ")
                     ((buffer-modified-p) "*")
                     (t " ")))
            (format-mode-line mode-line-process) 1 3))))

(provide 'jordon-mode-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jordon-mode-line.el ends here
