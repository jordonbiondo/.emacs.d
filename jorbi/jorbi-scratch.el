

(defun cloudy (str)
  (switch-to-buffer (generate-new-buffer"c"))
  (setq word-wrap t)
  (center-region
   (point)
   (progn 
     (let ((h (face-attribute 'default :height)))
       (mapc (lambda(w) (insert " " w (if (< (random 10) 4) "\n""")))
             (sort 
              (mapcar (lambda (w)
                        (propertize (car w) 'face
                                    `(:foreground ,(color-lighten-name "firebrick"
                                                                       (* (cdr w) 8))
                                                  :height ,(round (* 2 (expt (cdr w) 1.4) h)))))
                      (reduce (lambda (a b)
                                (incf (cdr (or (assoc b a) (car (setq a (cons (cons b 0) a)))))) a)
                              (split-string str "[^a-z0-9]" t) :initial-value nil))
              (lambda(a b) (= 0 (random 2))))))
     (point))))


(cloudy "hello world hello world hello world foo bar foo bar foobar fizz buzz")





(defun w(s)(switch-to-buffer (generate-new-buffer"c"))(setq word-wrap t)(center-region(point)(let((h(face-attribute'default :height)))(mapc(lambda(w)(insert" "w(if(<(random 10)3)"\n""")))(sort(mapcar(lambda(w)(propertize(car w)'face`(:foreground,(color-lighten-name"blue"(*(cdr w)8)):height,(round(* 2(expt(cdr w)1.4)h)))))(reduce(lambda(a b)(incf(cdr(or(assoc b a)(car(setq a(cons(cons b 0)a))))))a)(split-string s"[^a-z0-9]"t):initial-value nil))(lambda(a b)(= 0(random 2)))))(point))))








(font-lock-add-keywords
 'c-mode
 '(("\\(\\<[A-Z_][A-Z_0-9]+\\)\\( \\)?\\((\\)"
    1 'font-lock-constant-face)
   ("\\(#define\\)\\( +\\)\\([A-Z_][A-Za-z0-9_]+\\)"
    3 'font-lock-constant-face t)
   ("\\(?:ex\\(?:ec\\(?:l[ep]\\|v[Pep]\\|[lv]\\)\\|it\\)\\|fork\\|wait\\(?:pid\\)?\\)\\( *\\)\\((\\)"
    1 'font-lock-builtin-face t)))



(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\(( *\\)\\(Windows\\>\\)"
    2 'font-lock-preprocessor-face)))

(font-lock-add-keywords
 'lisp-interaction-mode
 '(("\\(( *\\)\\(Windows\\>\\)"
    2 'font-lock-preprocessor-face)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break taker
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bt/enabled t
  "Whether or not to repeat breaks.")

(defvar bt/last-break-minutes 60)


(defvar bt/snooze-time 5
  "Amount of time in minutes to use for snoozes")


(defun bt/exit-and-repeat-last ()
  (interactive)
  (winstack-pop)
  (break-taker bt/last-break-minutes))

(defun bt/exit ()
  (interactive)
  (winstack-pop))

(defun bt/snooze ()
  (interactive)
  (let ((old-time bt/last-break-minutes))
    (break-taker bt/snooze-time)
    (setq bt/last-break-minutes old-time)))

(defun bt/exit-and-snooze ()
  (interactive)
  (bt/exit)
  (let ((old-time bt/last-break-minutes))
    (break-taker bt/snooze-time)
    (setq bt/last-break-minutes old-time)))


(defun break-taker (minutes)
  (interactive "NMinutes between breaks: ")
  (lexical-let ((minutes (abs minutes)))
    (setq bt/last-break-minutes minutes)
    (run-with-timer (* 60 minutes) nil
                    (lambda(&rest args)
                      (winstack-push)
                      (switch-to-buffer "*Break Taker*")
                      (read-only-mode t)
                      (let ((inhibit-read-only t)
                            (fill-column (frame-width)))
                        (delete-other-windows)
                        (delete-region (point-min) (point-max))
                        (insert "\n\n\n")
                        (insert (propertize "Time to take a break, walk around a bit!" 'face
                                            `(:inherit font-lock-function-name-face)))
                        (insert "\n\n\n")
                        (insert "Press 'C-c C-k' to resume\n\n")
                        (insert (format (concat "Press 'C-c C-c' to resume and take another break in %d minutes\n\n"
                                                "Press 'C-c C-w' to postpone your break %d minutes") minutes bt/snooze-time))
                        (set-justification-center (point-min) (point-max))
                        (with-current-buffer "*Break Taker*"
                          (local-set-key (kbd "C-c C-k") 'bt/exit)
                          (local-set-key (kbd "C-c C-c") 'bt/exit-and-repeat-last)
                          (local-set-key (kbd "C-c C-w") 'bt/exit-and-snooze)))))))




(defmacro module (name &rest plist)
  `(progn
     (defvar ,(intern (concat (symbol-name name) "--internal")) (list ,@plist))
     (defmacro ,name (key &rest args)
       (let* ((the-key (elt key 0))
	      (quoted-key (and (listp the-key) (equal 'quote (first the-key)))))
	 (if quoted-key
	     (progn
	       (assert (= (length args) 0) nil (format "Invalid function (quote %s)" (second the-key)))
	       '(list 'apply (quote 'plist-get) (list 'nope--internal (intern (concat ":" (symbol-name (second the-key)))))))
	   '(list 'apply (quote 'plist-get) (append (list 'nope--internal (intern (concat ":" (symbol-name the-key)))) args)))))))

(module nope
        :foo (lambda (x y) (+ x y (/ x 2)))
        :bar "Hello!"
        :fizzbuzz (lambda (x y) `(+ ,x , y)))


(defmacro nope (key &rest args)
  (let* ((the-key (elt key 0))
	 (quoted-key (and (listp the-key) (equal 'quote (first the-key)))))
    (if quoted-key
	(progn
	  (assert (= (length args) 0) nil (format "Invalid function (quote %s)" (second the-key)))
	  (plist-get nope--internal (intern (concat ":" (symbol-name (second the-key)))))))))

      
`(,(plist-get nope--internal (intern (concat ":" (symbol-name the-key)))) ,@args)))

(evil-set-register ?x [?i ?f ?o ?o ?b ?a ?r escape])
(fset 'foobarinsertion [?i ?f ?o ?o ?b ?a ?r escape])
      
(evil-set-register ?x [?i ?f ?o ?o ?b ?a ?r escape])



foobar

foobar
   [?i ?f ?o ?o ?b ?a ?r escape])

ifoobarfoobar



(nope[fizzbuzz] (+ 3 4) (+ 2 2)x)

(lambda (x y) (+ x y (/ x 2)))

(nope['fizzbuzz])

(erc-response-p)




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prototypes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar protos (make-hash-table :test 'equal)
  "The global prototype table.
Do not directly modify this table.
See `defproto' for information on adding prototypes.")



(let ((my-point ({ :x 20 :y 30 })))
  (<< my-point :x))


(defproto vector2D
  ({
   :x 0
   :y 0
   :length
   (lambda () (sqrt (+ (expt (<< this :x) 2) (expt (<< this :y) 2))))
   })
  )

(defvar my-super-point (new vector2D :y 33 :x 23))


(>> my-super-point :name "Bob")

(defproto toS
  ({
   :toS (lambda() (format "{%s}" (<< this :type)))
   }))

(defproto vector3D vector2D toS
  ({
   :z 0
   :length (lambda () (sqrt (+ (expt (<< this :x) 2) (expt (<< this :y) 2) (expt (<< this :z) 2))))
   :toS (lambda() (concat (<<!proto toS :toS this) (format "()%d, %d, %d)" (<< this :x) (<< this :y) (<< this :z))))
   }))

vector3D:toS => {vector3D}(x, y, z)

































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


(defmacro defproto (name &optional prototype)
  "Define a prototype named NAME with the attributes in the hash-table PROTOTYPE."
  `(let* ((this-name ',name)
	  (this-proto-given ,prototype)
	  (this-proto (or this-proto-given ({ })))
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
	   (dolist (key (plist-keys this-values))
             (>> this key (eval (plist-get this-values key))))
	   this)
       (error "Unknown prototype: %s" ,prototype))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects setting and getting
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun << (object key)
  "Get the value of OBJECT[KEY]."
  (gethash key object))


(defun >> (object key value)
  "Set the value of OBJECT[KEY] to VALUE."
  (puthash key value object ))


(defun <<! (object key &rest args)
  "Call the function at OBJECT[KEY] with ARGS.
The object itself will be prepended to ARGS as the 'this' parameter."
  (let ((this object))
    (apply (<< object key) args)))



(defproto point2D
  ({
   :x 0
   :y 0

   :length (lambda ()
	     (let ((x (<< this :x)) (y (<< this :y)))
	       (sqrt (+ (* x x) (* y y)))))

   :add (lambda (other)
	  (new point2D
	       :x (+ (<< this :x) (<< other :x))
	       :y (+ (<< this :y) (<< other :y))))

   :toS (lambda ()
	  (format "(%d, %d)" (<< this :x) (<< this :y)))

   }))

(defproto integer
  ({ :v 0
     :>> (lambda (value) (round value))
     }))

(defun >>+ (object val &rest others)
  (let ((+fn (<< object :>>+)))
    (if (and +fn (functionp +fn))
	(setf(reduce +fn `(val ,@others))) ;;; still here
(defproto point3D
  ({
   :x 0
   :y 0
   :z 0

   :construct
   (lambda (x y z)
     (>> this :x x)
     (>> this :y y)
     (>> this :z z))

   :length
   (lambda () (let ((x (<< this :x))
		    (y (<< this :y))
		    (z (<< this :z)))
		(sqrt (+ (* x x) (* y y) (* z z)))))

   :add
   (lambda (other) (new point3D
			:x (+ (<< this :x) (<< other :x))
			:y (+ (<< this :y) (<< other :y))
			:z (+ (<< this :z) (<< other :z))))
   :toS
   (lambda () (format "(%d, %d, %d)"
		      (<< this :x)
		      (<< this :y)
		      (<< this :z)))

   }))


(<<! (<<! (new point3D :x 3 :y 4 :z 2) :add (new point3D :x 3 :y 4 :z 2)) :toS)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; (font-lock-add-keywords
 ;;  'lisp-interaction-mode
 ;;  '(("\\(( *\\)\\(\\<defproto\\|new\\>\\)" 2 'font-lock-keyword-face)
 ;;    ("\\(( *\\)\\(method\\>\\)" 2 'font-lock-keyword-face)
 ;;    ("\\(:[a-zA-Z0-9{}-_!@$%^&*+]+\\)\\( +\\)\\(( *\\)\\(method\\>\\)"  1 'font-lock-function-name-face)
 ;;    ("\\(( *\\)\\(defproto\\|new\\)\\( *\\)\\([a-zA-Z0-9{_}]+\\)" 4 'font-lock-type-face)))



(defproto person
  ({
   :age 0
   :height 0
   :name ""
   :speak
   (lambda () (message "Hello from %s" (<< this :name)))
   :greet
   (lambda(other) (message "Hello %s, I'm %s."
			   (<< other :name)
			   (<< this :name)))
   }))


(let ((p1 (new point2D :x 3 :y 3))
      (p2 (new point2D :x 1 :y 2)))
  (<<! (<<! p1 :add p2) :toS))
;;(<<! (<<! p1 :add p2) :toS))



(let ((bob (new person :name "Bob"))
      (jim (new person :name "Jim")))
  (<<! bob :greet jim))


(defproto pen ({
	       :color "blue"
	       :thick t
	       :write (lambda (text)
			(propertize text
				    'face `(:foreground ,(<< this :color) :weight ,(if (<< this :thick) 'bold 'normal))))

	       :print (lambda (text)
			(message (<<! this :write text)))
	       }))

(defun foobar()
  (interactive)
  (<<! (new pen :color "olivedrab" :thick nil) :print "Hello World"))




;; (-any? (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents)

;; (dolist (doc (-filter (lambda(doc) (> (plist-get doc :subtotal) 10000)) documents))
;;   (do-things-with doc))



(defun ascii-comment-string (pos)
  "Get the asci codes for string at point and insert them as a comment at the end of the line."
  (interactive "d")
  (save-excursion
    (let* ((str-beg (nth 8 (syntax-ppss)))
	   (str-end (save-excursion (goto-char str-beg)
				    (forward-sexp 1)
				    (point)))
	   (str-text (buffer-substring str-beg str-end))
	   (str-codes (mapcar (lambda (x) x) str-text)))
      (goto-char (point-at-eol))
      (comment-region (progn (insert " ") (point))
		      (progn
			(insert (format " %s = %s" str-text str-codes))
			(point))))))


(defun ascii-replace-string (pos)
  "Replace the string at point with a call to `string' given the correct ascii chars.

Example: \"foo\" becomes (string 102 111 111)"
  (interactive "d")
  (save-excursion
    (let* ((str-beg (nth 8 (syntax-ppss)))
	   (str-end (save-excursion (goto-char str-beg)
				    (forward-sexp 1)
				    (point)))
	   (str-text (buffer-substring (1+ str-beg) (1- str-end)))
	   (str-codes (mapcar (lambda (x) x) str-text)))
      (goto-char (point-at-eol))
      (delete-region str-beg str-end)
      (goto-char str-beg)
      (insert "(string)")
      (backward-char 1)
      (dolist (c str-codes) (insert (concat " " (number-to-string c)))))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point 2D
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun p2D (&optional x-or-list y)
  "Returns a new 2D point. arguments can either two numbers, or another 2D point (list or 2 numbers).

Examples: (p2D 3 4)
          (p2D '(3 4))
          (p2D (p2D 3 4))"
  (if (not (or x-or-list y)) (list 0 0)
    (or (p2D? (if (listp x-or-list) x-or-list (list x-or-list y)))
	(error "Invalid argument to p2D"))))


(defun p2D:x (p)
  "Get the X value of P."
  (first p))


(defun p2D:y (p)
  "Get the Y value of P."
  (second p))


(defun p2D:direction (p)
  "Return the direction of P in radians."
  (atan (/ (float (p2D:x p)) (float (p2D:y p)))))


(defun p2D:length (p)
  "Returns the length of point P."
  (sqrt (+ (expt (abs (p2D:x p)) 2)
	   (expt (abs (p2D:y p)) 2))))


(defun p2D:scale (p s)
  "Scale P by a factor of S."
  (p2D (* (p2D:x p) s)
       (* (p2D:y p) s)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun p2D? (p)
  "Returns P if P is a 2D point, else nil."
  (and (listp p) (= 2 (length p)) (numberp (p2D:x p)) (numberp (p2D:y p)) p))


(defun p2D:zero? (p)
  "Is p2D (0, 0)?"
  (and (= 0 (p2D:x p)) (= 0 (p2D:y p))))


(defun p2D:normal? (p)
  "Returns t if the length of P is 1."
  (= (p2D:length p) 1))


(defun p2D:normalize (p)
  "Return a new 2D point in the same direction as P with a length of 1."
  (if (p2D:zero? p) p ;; return (0, 0) if zero vector
    (let ((scale (/ 1 (p2D:length p))))
      (p2D:scale p scale))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit vectors
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro p2D:i () "Returns the unit vector (1, 0)." ''(1 0))
(defmacro p2D:j () "Returns the unit vector (0, 1)." ''(0 1))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variadic addition and subtraction
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p2D+ (p1 p2 &rest others)
  (reduce (lambda(a b) (p2D (+ (p2D:x a) (p2D:x b))
			    (+ (p2D:y a) (p2D:y b))))
	  (append (list p1 p2) others)))


(defun p2D- (p1 p2 &rest others)
  (reduce (lambda(a b) (p2D (- (p2D:x a) (p2D:x b))
			    (- (p2D:y a) (p2D:y b))))
	  (append (list p1 p2) others)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D Line
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line2D (p1 p2)
  (if(and (p2D? p1) (p2D? p2))
      (list p1 p2)
    (error "Argument to line2D is not 2D point.")))

(defun line2D:p1 (l)
  (first l))

(defun line2D:p2 (l)
  (second l))

(defun line2D:vector (l)
  (p2D- (line2D:p1 l) (line2D:p2 l)))


(defun line2D:length (l)
  (p2D:length (line2D:vector l)))


(defun line2D:direction (l)
  (p2D:direction (line2D:vector l)))




		 
(symbol-file 
