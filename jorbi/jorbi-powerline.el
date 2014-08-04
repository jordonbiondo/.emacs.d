;;; jorbi-powerline.el --- Personal powerline format
;;
;; Filename: jorbi-powerline.el
;; Description: Personal powerline format
;; Author: Jordon Biondo <jordonbiondo@gmail.com>
;; Created: Wed May 28 08:32:27 2014 (-0400)
;; Version:
;; Package-Requires: ((powerline "2.3"))
;; Last-Updated: Wed May 28 08:34:12 2014 (-0400)
;;           By: jordon.biondo
;;     Update #: 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
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

(defun jorbi/powerline-point-progress (length)
  (let ((p-count (round (* (/ (float (point))
                              (float (point-max))) length))))
    (concat  (make-string p-count ?.)
             (make-string (- length p-count) ? ) "|")))


(defvar jorbi/powerline-format
  '("%e"
    (:eval
     (let* ((lhs (list (powerline-raw
                        (format " |%s| %d minors |%s" mode-name
                                (length (enabled-important-minor-modes (current-buffer)))
                                (jorbi/powerline-point-progress 10)
                                nil 'l))))
            (rhs (list (if (not (buffer-file-name))
                           "( -_-)zzZ         "
                         (if (buffer-modified-p)
                             (powerline-raw "( ╯°☐°)╯<( SAVE! )" nil 'r)
                           (powerline-raw   "( °u°)            " nil 'r)))))
            (center (list (powerline-raw "%b" nil))))
       (concat (powerline-render lhs)
               (powerline-fill-center nil (/ (powerline-width center) 2.0))
               (powerline-render center)
               (powerline-fill nil (powerline-width rhs))
               (powerline-render rhs))))))


(provide 'jorbi-powerline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-powerline.el ends here
