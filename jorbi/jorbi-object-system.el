;;; jorbi-object-system.el --- 
;; 
;; Filename: jorbi-object-system.el
;; Description: 
;; Author: Jordon Biondo
;; Maintainer: 
;; Created: Fri Dec 20 01:06:52 2013 (-0500)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Sat Mar 22 13:21:11 2014 (-0400)
;;           By: Jordon Biondo
;;     Update #: 25
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
 
 
;; (font-lock-add-keywords 
;;    'lisp-interaction-mode
;;    '(("\\(( *\\)\\<\\(defproto\\|new\\|prototype\\|global\\)\\>" 2 'font-lock-keyword-face)
;;     ("\\(( *\\)\\(defproto\\|new\\|prototype\\|global\\)\\( *\\)\\([a-zA-Z0-9{_}]+\\)" 4 'font-lock-type-face)))
 
(defmacro global (name)
  "Get the global object for NAME, includes prototype and static members."
  `(<< protos ',name))
 
(defmacro prototype (name)
  "Get the global NAME prototype."
  `(<< (global ,name) :proto))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prototypes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defvar protos (make-hash-table :test 'equal)
  "The global prototype table. 
Do not directly modify this table.
See `defproto' for information on adding prototypes.")
 
;; (defmacro obj-merge-smart (predicate objA objB &rest others)
;;   `(let ((this-list `(mapcar 'copy-hash-table (,objA ,objB ,@others)))
;; 	 (this-pred ,predicate))
;;      `(reduce (lambda (base-object incoming-object)
;; 		(maphash (lambda (merge-key merge-value)
;; 			   (if this-pred
;; 			       (if (and (functionp this-pred) (not (equal (<< base-object
;; 				   (if (apply this-pred (list base-object incoming-object))
;; 				       )))))) incoming-object)
 
 
;; 		))))
 
 
 
 
;; shamelessly stolen from use-package
(unless (fboundp 'plist-keys)
  (defun plist-keys (plist)
    "Return a list containing all the keys in PLIST."
    (when plist (cons (car plist) (plist-keys (cddr plist))))))
      
 
(defmacro { (&rest args)
  "Create an object from a plist.
the last element of ARGS must be '}'.
 
Example ({ :name \"bob\" :age 22 :weight 180 })"
  (declare (indent defun))
  (assert (equal (first (last args)) '}) nil
	  "Unbalanced brackets in object declaration")
  `({-internal ,@(butlast args)))
 
 
(defun {-internal (&rest args)
  "Internal function for object declarations, \
works like `{' but doesn't check for matching '}'."
  (let ((obj (make-hash-table :test 'equal)))
    (dolist (key (plist-keys args))
      (puthash key (plist-get args key) obj))
    obj))
 
 
(defun -define-prototype (name &optional docstring proto members)
  "Create a new basic prototype in the global protos table. Do not use this, use `defproto'"
  (declare (doc-string 2))
  (assert (symbolp name) nil)
  (when docstring (assert (stringp docstring)))
  (when proto (assert (hash-table-p proto)))
  (when members (assert (hash-table-p members)))
  (puthash name
	   ({
	     :proto (or proto ({ }))
	     :type (symbol-name name)
	     :docstring (or docstring "")
	     :members (or members ({ }))
	     })
	   protos)
  name)
  
 
(defmacro defproto (name &optional docstring prototype)
  "Define a prototype named NAME with the attributes in the hash-table PROTOTYPE."
  (declare (doc-string 2))
  (let ((the-prototype (if (stringp docstring) prototype (when (or (symbolp docstring) (listp docstring)) docstring)))
	(the-docstring (when (stringp docstring) docstring)))
    `(-define-prototype ',name ,the-docstring ,the-prototype nil)))
 
;; `(let* ((this-name ',name)
  ;; 	  (this-proto (copy-hash-table ,prototype)))
	  
  ;;    (>> this-proto :type this-name)
  ;;    (>> protos this-name this-proto)
  ;;    this-name))
 
 
(defmacro new (prototype &rest values)
  "Instantiate a new object of type PROTOTYPE. 
PROTOTYPE must be a defined type in the global prototype table. See `defproto'.
 
VALUES should be a plist of attributes-value pairs which will be used
to set the values in the returned object."
  
  `(let* ((>>this-proto-name ',prototype)
	  (>>this-proto (<< (<< protos >>this-proto-name) :proto))
	  (>>this-values ',values))
     (if >>this-proto
	 (let* ((>>this (copy-hash-table >>this-proto)))
	   (dolist (key (plist-keys >>this-values))
	     (>> >>this key (eval (plist-get >>this-values key))))
	   >>this)
       (error "Unknown prototype: %s" >>this-proto-name))))
 
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects setting and getting
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
;; get slot value
(defmacro << (object-or-key &optional key)
  "Get the value of OBJECT[KEY]."  
  `(gethash ,(or key object-or-key) ,(if key object-or-key 'this)))
 
;; set slot value
(defun >> (object key value)
  "Set the value of OBJECT[KEY] to VALUE."
  (if (<< object :const)
      (error "Attempting to set %s value on immutable object" key)
    (puthash key value object )))
 
;; function all helpers
(defun function-args (func)
  "Returns the arg list of FUNC, a symbol or lambda."  
  (second (if (and func (symbolp func)) (symbol-function func) func)))
 
(defun function-body (func)
  "Returns the body of FUNC as a list of top level expressions."
  (cddr (if (and func  (symbolp func)) (symbol-function func) func)))
 
(defun apply-method (context key &rest args)
  "Apply the function at CONTEXT's key KEY with 'this bound to CONTEXT."
  (let ((func (<< context key)))
    (apply `(lambda (this ,@(function-args func)) ,@(function-body func))
           `(,context ,@args))))
 
;; call slot function
(defun <<! (object-or-key &optional key &rest args)
  "Call the in OBJECT's KEY slot with args with OBJECT as 'this."
  (apply 'apply-method object key args))
 
 
(defmacro <<!with (object &rest calls)
  "Allow many calls within the context of OBJECT.
 
Example: (<<!chain my-object
                   (:funcA 1 2 3)
                   (:funcB 'hello)
                   (:fizz 3 4 '(a b)))"
  (declare (indent defun))
  `(let ((the-object-given-here ,object))
     ,@(mapcar (lambda (call) `(<<! the-object-given-here ,(first call) ,@(rest call))) calls)))
 
 
;;(defun <<! (object key &rest args)
;;   "Call the function at OBJECT[KEY] with ARGS.
;; The object itself will be prepended to ARGS as the 'this' parameter."
;;   (let ((this object)
;;         (the-fn (<< object key)))
;;     (if (and the-fn (functionp the-fn))
;;         (apply (<< object key) `(,this ,@args))
;;       (error "Object %s of type %s has no method %s" 
;;              (if (<<!? object :str) (<<! object :str) "[Object]")
;;              (or (<< object :type) "undefined")
;;              key))))
     
;; slot exists?
(defun <<? (object key)
  "Returns t if OBJECT has key KEY, else returns nil"
  (not (equal 'that-shit-aint-here (gethash key object 'that-shit-aint-here))))
 
;; function slot exists?
(defun <<!? (object key)
  "Returns t if OBJECT has key KEY, and it's value is a function, \
else returns nil."
  (functionp (<< object key)))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator overloading
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; + overload
(defun <<+ (objectA objectB &rest other-objects)
  "Return sum of any number of arguments, \
which must be objects implementing the `:+' function."
  (reduce (lambda (A B) (<<! A :+ B)) `(,objectA ,objectB ,@other-objects)))
 
 
;; - overload
(defun <<- (objectA objectB &rest other-objects)
  "Return sum of any number of arguments, \
which must be objects implementing the `:-' function."
  (reduce (lambda (A B) (<<! A :- B)) `(,objectA ,objectB ,@other-objects)))
 
;; / overload
(defun <</ (objectA objectB &rest other-objects)
  "Return first argument divided by all the remaining arguments, \
which must be objects implementing the `:/' function."
  (reduce (lambda (A B) (<<! A :/ B)) `(,objectA ,objectB ,@other-objects)))
 
;; * overload
(defun <<* (objectA objectB &rest other-objects)
  "Return product of any number of arguments, \
which must be objects implementing the `:*' function."
  (reduce (lambda (A B) (<<! A :* B)) `(,objectA ,objectB ,@other-objects)))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; objects keys
(defun <<keys (object)
  "Return all the keys from OBJECT."
  (let ((keys)) (maphash (lambda (a b) (setq keys (cons a keys))) object) keys))
 
;; object variables
(defun <<vars (object)
  "Return a list of keys whos values are not functions in OBJECT."
  (let ((keys))
    (maphash (lambda (a b) (unless (functionp b)
			     (setq keys (cons a keys)))) object)
    keys))
 
;; object functions
(defun <<funcs (object)
  "Return a list of keys whos values are functions in OBJECT."
  (let ((keys))
    (maphash (lambda (a b) (when (functionp b)
			     (setq keys (cons a keys)))) object)
    keys))
 
 
 
(provide 'jorbi-object-system)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jorbi-object-system.el ends here
