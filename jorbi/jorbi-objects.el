;;; jorbi-objects.el --- 
;; 
;; Filename: jorbi-objects.el
;; Description: 
;; Author: Jordon Biondo
;; Maintainer: 
;; Created: Fri Dec 20 01:06:15 2013 (-0500)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Fri Dec 20 01:06:20 2013 (-0500)
;;           By: Jordon Biondo
;;     Update #: 1
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
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



(require 'jorbi-object-system)


(defproto object
  ({
   :hash-code (lambda () "")
   :clone (lambda () (copy-hash-table this))
   :str (lambda () "{object}")
   }))

(<<! (new object) :str)


(defproto point2D
  ({ :x 0 :y 0
     :construct 'auto
     :length (lambda () (let ((x (<< this :x))
			      (y (<< this :y)))
			  (sqrt (+ (* x x) (* y y)))))
     
     :str (lambda () (format "(%d, %d)" (<< this :x) (<< this :y)))
     :+ (lambda (other) (new point2D
			     :x (+ (<< this :x ) (<< other :x ))
			     :y (+ (<< this :y ) (<< other :y ))))
     
     :++ (lambda (other) (new point2D
			      :x (- (<< this :x ) (<< other :x ))
			      :y (- (<< this :y ) (<< other :y ))))
     :foo 3
     }))



(<<! (<<! (new point2D :x 9 :y 3) :- (new point2D :x 10 :y 4)) :str)

(<<! (new point2D :x 9 :y 3) :++ ({ :x 3 :y 4 }))
;; (<<! (new point3D 3 9) :length)

;; (defproto point3D
;;   ({
;;    :construct (lambda (x y z)
;; 		(>> this :x x)
;; 		(>> this :y y)
;; 		(>> this :z z))
;;    :x 0
;;    :y 0
;;    :z 0
;;    :length (lambda () (let ((x (<< this :x))
;; 			    (y (<< this :y))
;; 			    (z (<< this :z)))
;; 			(sqrt (+ (* x x) (* y y) (* z z)))))

;;    :str (lambda () (format "(%d, %d, %d)" (<< this :x) (<< this :y) (<< this :z)))

;;    }))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (defproto person
;;   ({
;;    :construct 'auto
;;    :age 0
;;    :height 0
;;    :name ""
;;    :speak (lambda (this) (message "Hello from %s" (<< this :name)))
;;    :greet (lambda(this other) (message "Hello %s, I'm %s."
;; 				       (<< other :name)
;; 				       (<< this :name)))
;;    }))


;; (<< (new person
;; 	 :name "bob"
;; 	 :age 10 ) :name)



;; (let ((my-obj ({ :foo 30
;; 		 :bar 40
;; 		 :name "John"
;; 		 :speak
;; 		 (lambda (this) (message "Hello from %s" (<< this :name)))
;; 		 :greet
;; 		 (lambda(this other) (message "Hello %s, I'm %s."
;; 					      (<< other :name)
;; 					      (<< this :name)))
;; 		 }))
;;       (my-obj2 ({ :foo 30
;; 		  :bar 40
;; 		  :name "Bob"
;; 		  :speak
;; 		  (lambda (this) (message "Hello from %s" (<< this :name)))
;; 		  :greet
;; 		  (lambda(this other) (message "Hello %s, I'm %s."
;; 					       (<< other :name)
;; 					       (<< this :name)))
;; 		  })))
;;   (>> my-obj :name (concat (<< my-obj :name) " Smith"))
;;   (<<! my-obj :speak)
;;   (<<! my-obj :greet my-obj2))



;; (-any? (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents)

;; (dolist (doc (-filter (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents))
;;   (do-things-with doc))




(provide 'jorbi-objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-objects.el ends here
