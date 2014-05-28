;;; normal-person-mode.el --- Extended cua

;; Copyright (C) 2014  

;; Author:  <jordonbiondo@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar normal-person-mode-map (make-sparse-keymap)
  "Normal person mode's keymap.")

;;;###autoload
(define-minor-mode normal-person-mode
  "Mode for normal people."
  :init-value nil
  :lighter "norm"
  :keymap normal-person-mode-map
  :global t

  (define-keys normal-person-mode-map
    ("C-o" 'find-file)
    ("C-s" 'save-buffer)
    ("<f5>" 'revert-buffer)
    ("C-f" 'isearch-forward)
    ("ESC" 'keyboard-quit))

  (define-keys isearch-mode-map
    ("S-<return>" 'isearch-repeat-backward)
    ("<return>" 'isearch-repeat-forward))

  (cua-mode (if normal-person-mode t -1)))


(defadvice normal-person-mode (after better-message activate)
  "Normal mode for people."
  (message "normal person mode %s"
           (if normal-person-mode
               (propertize "ENABLED" 'face 'font-glock-function-name-face)
             (propertize "DISABLED" 'face 'font-lock-type-face))))

(provide 'normal-person-mode)
;;; normal-person-mode.el ends here
