
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prototypes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar protos (make-hash-table :test 'equal)
  "The global prototype table. 
Do not directly modify this table.
See `defproto' for information on adding prototypes.")

(defmacro obj-merge-smart (predicate objA objB &rest others)
  `(let ((this-list `(mapcar 'copy-hash-table (,objA ,objB ,@others)))
	 (this-pred ,predicate))
     `(reduce (lambda (base-object incoming-object)
		(maphash (lambda (merge-key merge-value)
			   (if this-pred
			       (if (and (functionp this-pred) (not (equal (<< base-object
				   (if (apply this-pred (list base-object incoming-object))
				       )))))) incoming-object)


		))))



(defmacro { (&rest args)
  "Create an object from a plist.
the last element of ARGS must be '}'.

Example ({ :name \"bob\" :age 22 :weight 180 })"
  (message "%s " (first (last args)))
  (assert (equal (first (last args)) '}) nil "Unbalanced brackets in object declaration")
  `({-internal ,@(butlast args)))


(defun {-internal (&rest args)
  "Internal function for object declarations, works like `{' but doesn't check for matching '}'."
  (let ((obj (make-hash-table :test 'equal)))
    (dolist (key (plist-keys args))
      (puthash key (plist-get args key) obj))
    obj))


(defmacro defproto (name prototype)
  "Define a prototype named NAME with the attributes in the hash-table PROTOTYPE."
  `(let* ((this-name ',name)
	  (this-proto (copy-hash-table ,prototype))
	  (parent-type (<< this-proto :type)))
     (>> this-proto :type this-name)
     (>> protos this-name this-proto)
     this-name))



(defmacro new (prototype &rest values)
  "Instantiate a new object of type PROTOTYPE. 
PROTOTYPE must be a defined type in the global prototype table. See `defproto'.

VALUES should be a plist of attributes-value pairs which will be used
to set the values in the returned object."
  `(let* ((this-proto-name ',prototype)
	 (this-proto (<< protos this-proto-name))
	 (this-values ',values))
     (if this-proto
	 (let* ((this (copy-hash-table this-proto))
		(construct (<< this :construct)))
	   (if construct
	     (if (functionp construct)
		 (<<! this :construct ,values)
	       (if (equal construct 'auto)
		   (dolist (key (plist-keys this-values))
		     (>> this key (plist-get this-values key)))
		 (error "Invalid constructor for %s" ,prototype)))
             (when values (error "arguments given to prototype %s which has no constructor" ',prototype)))
	   this)
       (error "Unknown prototype: %s" ,prototype))))
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects setting and getting
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(<< ({ :x 3 }) :x)

(defun << (object key)
  "Get the value of OBJECT[KEY]."
  (gethash key object))

(defun <<? (object key)
  "Returns t if objects has key KEY, else returns nil"
  (not (equal 'that-shit-aint-there (gethash key object 'that-shit-aint-there))))

(defun >> (object key value)
  "Set the value of OBJECT[KEY] to VALUE."
  (if (<< object :const)
      (error "Attempting to set %s value on immutable object" key)
    (puthash key value object )))



(defun <<! (object key &rest args)
  "Call the function at OBJECT[KEY] with ARGS.
The object itself will be prepended to ARGS as the 'this' parameter."
  (apply (<< object key) `(,object ,@args)))

(defmacro method (arglist &rest body)
  "Lamabda wrapper that prepends an additional argument 'this' to the arglist."
  `(lambda (this ,@arglist) ,@body))




(defproto point2D
  ({ :x 0 :y 0
     :length (method () (let ((x (<< this :x))
			      (y (<< this :y))
			      (z (<< this :z)))
			  (sqrt (* (<< this :x) (<< this :x)) (* y y) (* z z))))
     
     :toS (method () (format "(%d, %d)" (<< this :x) (<< this :y)))
     }))

(defproto point3D
  ({
   :construct (method (x y z)
		      (>> this :x x)
		      (>> this :y y)
		      (>> this :z z))
   :x 0
   :y 0
   :z 0
   :length (method () (let ((x (<< this :x))
			    (y (<< this :y))
			    (z (<< this :z)))
			(sqrt (* (<< this :x) (<< this :x)) (* y y) (* z z))))
   
   :add (method (other) (new point3D
			     :x (+ (<< this :x) (<< other :x))
			     :y (+ (<< this :y) (<< other :y))
			     :z (+ (<< this :z) (<< other :z))))
   :toS (method () (format "(%d, %d, %d)" (<< this :x) (<< this :y) (<< this :z)))

   }))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (font-lock-add-keywords 
  'emacs-lisp-mode
  '(("\\(( *\\)\\(\\<defproto\\|new\\>\\)" 2 'font-lock-keyword-face)
    ("\\(( *\\)\\(method\\>\\)" 2 'font-lock-keyword-face)
    ("\\(:[a-zA-Z0-9{}-_!@$%^&*+]+\\)\\([ \n\r]+\\)\\(( *\\)\\(method\\>\\)"  1 'font-lock-function-name-face)
    ("\\(( *\\)\\(defproto\\|new\\)\\( *\\)\\([a-zA-Z0-9{_}]+\\)" 4 'font-lock-type-face)))


(defproto person
  ({
   :construct 'auto
   :age 0
   :height 0
   :name ""
   :speak (lambda (this) (message "Hello from %s" (<< this :name)))
   :greet (lambda(this other) (message "Hello %s, I'm %s."
				       (<< other :name)
				       (<< this :name)))
   }))


(<< (new person
	 :name "bob"
	 :age 10 ) :name)
	 

  
(let ((my-obj ({ :foo 30
		 :bar 40
		 :name "John"
		 :speak
		 (lambda (this) (message "Hello from %s" (<< this :name)))
		 :greet
		 (lambda(this other) (message "Hello %s, I'm %s."
					      (<< other :name)
					      (<< this :name)))
		 }))
      (my-obj2 ({ :foo 30
		  :bar 40
		  :name "Bob"
		  :speak
		  (lambda (this) (message "Hello from %s" (<< this :name)))
		  :greet
		  (lambda(this other) (message "Hello %s, I'm %s."
					       (<< other :name)
					       (<< this :name)))
		  })))
  (>> my-obj :name (concat (<< my-obj :name) " Smith"))
  (<<! my-obj :speak)
  (<<! my-obj :greet my-obj2))



;; (-any? (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents)

;; (dolist (doc (-filter (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents))
;;   (do-things-with doc))









