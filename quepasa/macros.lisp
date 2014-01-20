;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2003-2007 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+NIL ;;DLD|
(in-package "QUUX")
(in-package :quepasa) ;; DLD| Bad.


(defmacro implies (a &rest b)

  "Returns true if a implies b, i.e. returns false
   if a is true and b is false."

  `(if ,a (progn ,@b) t))

(defmacro i+ (&rest fixnums)
  `(the fixnum (+ ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i- (number &rest fixnums)
  `(the fixnum (- (the fixnum ,number) ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i* (&rest fixnums)
  `(the fixnum (* ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i= (&rest fixnums)
  `(= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i< (&rest fixnums)
  `(< ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i<= (&rest fixnums)
  `(<= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i> (&rest fixnums)
  `(> ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i>= (&rest fixnums)
  `(>= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i1+ (x)

  "A version of the 1+ function that can only be used on fixnums."

  `(the fixnum (1+ (the fixnum ,x))))

(defmacro i1- (x)

  "A version of the 1- function that can only be used on fixnums."

  `(the fixnum (1- (the fixnum ,x))))

(defmacro izerop (x)

  "A version of the zerop function that can only be used on fixnums."

  `(zerop (the fixnum ,x)))

(defmacro iplusp (x)

  "A version of the plusp function that can only be used on fixnums."

  `(plusp (the fixnum ,x)))

(defmacro iminusp (x)

  "A version of the minusp function that can only be used on fixnums."

  `(minusp (the fixnum ,x)))

(defmacro iash (value count)
  `(the fixnum (ash (the fixnum ,value) (the fixnum ,count))))

(defmacro ilogior (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logior (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogior ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(defmacro ilogand (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logand (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogand ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(define-modify-macro iincf (&optional (delta 1)) i+)
(define-modify-macro idecf (&optional (delta 1)) i-)

(defmacro ildb (bytespec value)
  `(the fixnum (ldb ,bytespec (the fixnum ,value))))

;; indentation hints
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *qres-indentation-hints* (make-hash-table)
    "The key is an OPERATOR such as a symbol naming a macro.
     The value is instructions about how to indent forms
     starting with that operator.  Values are e.g.
     (5 &body), let, (6 6 6 (&whole 4 1 &rest 1) &rest 2).")

  (defmacro declare-indentation (operator &rest arguments)

    "Define a cl-indent spec for OPERATOR.
    OPERATOR is a symbol naming a function or macro and
    ARGUMENTS is either a cl-indent spec list or a
    symbol specifying that OPERATOR should be indented just like that symbol.
    Calls to this macro are typically near the operator's definition."

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',operator *qres-indentation-hints*)
             ',(if (and (= 1 (length arguments))
                        (symbolp (first arguments)))
                   (first arguments)
                   arguments))
       ',operator)))

(declare-indentation declare-indentation 4 2 &rest 2)

(defun clear-qres-indentation ()

  "Get rid of all information from declare-indentation.
   This is only for debugging purposes."

  (clrhash *qres-indentation-hints*)
  ;; If swank is present, tell it to update its cache, etc.
  (when (find-package :swank)
    (funcall (find-symbol (string :perform-indentation-update) (find-package :swank))
             (symbol-value
              (find-symbol (string :*emacs-connection*) (find-package :swank)))
             t)))

;;; Early hash table stuff

;; Make a hash table that is initialized just once and is
;; read-only after that point
;; Read-only hash tables are no longer an optimization,
;; so make this the same as make-hash-table.
;;(defvar *read-only-hash-tables* ())
(defun make-read-only-table (&rest keys)
  "Create a hash table that will (no longer) be made read-only at 'boot' time"
  (declare (dynamic-extent keys))
  (let ((table (apply #'make-hash-table keys)))
    ;;(pushnew table *read-only-hash-tables*)
    table))


;;; Useful function to use within macros.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun constant-form (form)

  "FORM is a Common Lisp form.  If we know for sure
   that this form will always evaluate to the same (equal)
   value, every time, no matter what side effects might
   have happened between two evaluations of the form, no
   matter what environment it is in, then return two
   values: true, and the value to which the form will evaluate.
   Otherwise, return nil."

  ;; It's OK to add more clauses, if you're sure that the above criterion is met.
  (cond ((constantp form)
         (values t form))
        ((and (listp form)
              (eq (first form) 'quote))
         (values t (second form)))
        (t nil)))

) ;end of eval-when

;;; Various macros

(defmacro macroexpansion-time-value (sexp)
  `',(eval sexp))

(defmacro met-value (sexp)
  `(macroexpansion-time-value ,sexp))

(defmacro defun-once (name args &rest body)
  `(unless (fboundp ',name)
     (defun ,name ,args ,@body)))

(defmacro setf-once (name value)
  `(progn
     (unless (boundp ',name) (setq ,name ,value))
     ,name))

;; Creates gensyms for use in a macro expansion.
;;--- This is a less brittle version of the CLiki 'with-unique-names' proposal
(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro with-gensyms ((&rest bindings) &body body)

   "BINDINGS is a list of clauses.  The canonical clause looks like
    (VARIABLE PREFIX) where VARIABLE is a Lisp variable, and PREFIX
    is a string (or anything acceptable to the STRING function).
    Each VARIABLE is bound to a gensym, made with the PREFIX, and
    the body is run inside those bindings.  A clause of the form
    (VARIABLE) or VARIABLE is treated as (VARIABLE VARIABLE).
    This is available at compile-time, so macro bodies can use it."

  `(let ,(mapcar #'(lambda (binding)
                     (multiple-value-bind (var prefix)
                         (if (consp binding)
                           (values (first binding) (or (second binding) (first binding)))
                           (values binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
     ,@body))

)       ;eval-when

(declare-indentation with-gensyms let)

(defmacro define-class (name base-classes &rest doc-slots-props)
  "Simplified class definition.  Expand into a DEFCLASS.
   NAME is the name of the class, and BASE-CLASSES is a list
   of the names of the ancestors, just like DEFCLASS.
   The body can start with a doc-string after the list of base-clases, e.g.
     (define-class carambola (fruit)
        \"Star-shaped fruit\"
        ...)
   The body can also contain
     (:documentation \"same as the doc string\")  String; the :documentation in the defclass
     (:conc-name \"conc-name\")                   String; conc-name for accessors
     (:constructor cname)                       Symbol; constructor name
   If :conc-name is absent or the value is T, use 'NAME-'.  If :conc-name
   is NIL, do not generate accessors.
   Each slot description can be a SLOT-NAME (just a symbol), or
   a list (SLOT-NAME key val key val...) as in DEFCLASS.
   If :accessor is not provided for a slot, generate an accessor
   name by concatening the conc-name to SLOT-NAME.
   If :initarg is not provided for a slot, generate one named SLOT-NAME.
   If :constructor is NIL, do not generate a constructor function.
   If :constructor is not provided, generate a constructor named make-NAME.
   Otherwise, generate a constructor named cname.
   The constructor is a function of keyword arguments, that calls
   make-instance on NAME and those arguments.
   It also defines (list NAME) as a Lisp type.

   NOTE: If a \"DB interface\" type is specified for a slot - eg. (ASCII-LSTRING 1 10) - then
    the same type spec must appear in a DEFINE-DB-CLASS form, because that's where the
    corresponding predicate function is defined (via EMIT-SLOTS-EXTRA-TYPE-FORMS); or the
    function could be defined explicitly, eg:

     (defun-once quake::length-between-1-and-10 (s)
       (quake::length-between-p s 1 10))
"

  (check-type name symbol)
  (check-type base-classes list)
  ;; we will modify class-properties destructively, so make a copy
  (let* ((doc (when (stringp (first doc-slots-props))
                (first doc-slots-props)))
         (slots (if doc
                  (second doc-slots-props)
                  (first doc-slots-props)))
         (class-properties
          (if doc
            (append
             (list (list :documentation doc))
             (cddr doc-slots-props))
            (cdr doc-slots-props))))
    (macrolet ((pop-p (list property &optional default-value)
                 `(let ((entry (assoc ,property ,list)))
                    (cond (entry
                           (setf ,list (remove entry ,list))
                           (second entry))
                      (t ,default-value))))
               (setf-p (plist property value)
                 ;; If PROPERTY is not present in the PLIST, put
                 ;; it into the PLIST with the given VALUE.
                 ;; If it is present and its value is NIL, remove it from the plist.
                 ;; Otherwise, return the current value.
                 ;; (Only works for keyword/value pairs in slot specs)
                 (with-gensyms (current-value)
                   `(let ((,current-value (getf ,plist ,property 'it-was-not-present)))
                      (case ,current-value
                        ((nil) (remf ,plist ,property) nil)
                        ((it-was-not-present) (setf (getf ,plist ,property) ,value))
                        (t ,current-value))))))
      (let* ((conc-name (or (pop-p class-properties :conc-name)
                            (fintern "~A-" name)))
             (constructor (let ((c (pop-p class-properties :constructor t)))
                            (case c
                              ((t)       (fintern "MAKE-~A" name))
                              ((nil)     nil)
                              (otherwise c))))
             (initargs))
        `(progn
           (defclass ,name ,base-classes
               ,(loop for slot in slots
                      collect (let* ((slot (if (listp slot) slot (list slot)))
                                     (slot-name (car slot))
                                     (slot-options (copy-list (cdr slot)))
                                     (initarg (setf-p slot-options :initarg (kintern "~A" slot-name))))
                                (when conc-name
                                  (setf-p slot-options :accessor
                                          (fintern "~A~A" conc-name slot-name)))
                                (when initarg (push (keyword-to-symbol initarg) initargs))
                                (list* slot-name slot-options)))
             ,@class-properties)
           ,@(when constructor
               (setf initargs (nreverse initargs))
               `((defun ,constructor (&rest keys &key ,@initargs &allow-other-keys)
                   (declare (ignorable ,@initargs))
                   (declare (dynamic-extent keys))
                   (apply #'make-instance ',name keys))
                 (define-compiler-macro ,constructor (&rest args)
                   `(make-instance ',',name ,@args))))
           ;; When used on top level, make macro return the class, not something random
           (find-class ',name))))))

(declare-indentation define-class defclass)

(defmacro defstruct-class (name-form &rest slots)
  "DEFSTRUCT-syntax, but to define a CLOS class instead of a struct."
  (unless (listp name-form) (setf name-form (list name-form)))
  (let ((include-classes nil)
	(the-conc-name nil)
	(has-conc-name nil)
	(the-print-function nil)
        (the-constructor nil))
    (destructuring-bind (name &rest declarations) name-form
      ;;--- We don't support the wacky :conc-name syntax of, eg "(defstruct (foo :conc-name) bar)".
      (dolist (decl declarations)
        (destructuring-bind (&key include (conc-name nil conc-name-p)
                                  print-function constructor) decl
          (when include (push include include-classes))
          (when conc-name-p (setf has-conc-name conc-name-p the-conc-name conc-name))
          (when print-function (setf the-print-function print-function))
          (when constructor (setf the-constructor constructor))))
      (setf include-classes (nreverse include-classes))
      `(progn
	 (defclass ,name ,include-classes
	   ,(mapcar
	     (lambda (slot)
	       (unless (listp slot) (setf slot (list slot)))
	       (destructuring-bind (slot-name &optional (initform nil initform-p) &key type) slot
		 `(,slot-name :accessor ,(if has-conc-name
					   (if the-conc-name
					     (read-from-string (format nil "~A~A"
								       the-conc-name slot-name))
					     (read-from-string (format nil "~A" slot-name)))
					   (read-from-string (format nil "~A-~A" name slot-name)))
			      :initarg ,(read-from-string (format nil ":~A" slot-name))
			      ,@(if initform-p `(:initform ,initform) `(:initform nil))
			      ,@(if type `(:type ,type)))))
	     slots))
         (defun ,(or the-constructor
                     (read-from-string (format nil "MAKE-~A" name)))
                (&rest args &key ,@(mapcar (lambda (slot)
                                             (if (listp slot)
                                               (first slot)
                                               slot))
                                           slots))
           (declare (dynamic-extent args)
                    (ignorable ,@(mapcar (lambda (slot)
                                           (if (listp slot)
                                             (first slot)
                                             slot))
                                         slots)))
           (apply 'make-instance ',name args))
	 ,@(when the-print-function
	     `((defmethod print-object ((obj ,name) stream)
		 (apply ,(if (symbolp the-print-function)
			   (list 'function the-print-function)
			   the-print-function)
			(list obj stream 0)))))))))

(declare-indentation defstruct-class defstruct)


(defmacro define-accessor-wrapper (wrapper class wrapped)
  `(progn
     (defmethod ,wrapper ((obj ,class)) (,wrapped obj))
     (defmethod (setf ,wrapper) (value (obj ,class)) (setf (,wrapped obj) value))))


;;; Comes from fare-utils:evaluating-once
(defmacro with-rebinding (vars &body body)

  "VARS is a list of symbols that are local variables whose
   values are Lisp forms.  Generate code that evaluates
   each of the forms, and rebinds a variable with the same
   name to that value, and then executes BODY.  This is
   useful inside macros to make sure that each of the
   forms is only evaluated once.  Typically, you use
   this inside the body of macro, and the code to generate
   the result inside the BODY. This is a conceptual relative
   of  PCL::ONCE-ONLY, Genera's SCL::ONCE-ONLY or CL-UTILITIES:ONCE-ONLY.
   CMUCL's EXT:ONCE-ONLY has a different interface."

  (loop for var in vars for sym = (gensym (symbol-name var))
    collect ``(,',sym ,,var) into rt-bindings
    collect `(,var ',sym) into et-bindings
    finally (return
             ``(let (,,@rt-bindings)
                 ,(let ,et-bindings ,@body)))))

(defmacro static-place (form)
  (if (member :qpxnostatic *features*)
    form
    `(load-time-value ,form)))

;;; Taken from fare-utils:accessors-equal-p
(defmacro accessors-equal-p ((&key (test '#'equal) (prefix "")) accessors obj1 obj2)
  "Tests two objects for equality based on the values of their accessors.
   PREFIX (not evaluated) will be prepended to all accessor names.
   Each item in ACCESSORS (not evaluated) describes an equality test for one accessor.
     (accessor #'test-function) means apply test-function to the accessor applied to OBJ1 and the
       accessor applied to OBJ2
     accessor means the same as (accessor TEST)
   First evaluates TEST, OBJ1, and OBJ2.  For each item in ACCESSORS, evaluates test-function, if
   given, (which should evaluate to a value suitable to be passed as the first argument to funcall),
   applies the accessors to the value of OBJ1 and the value of OBJ2, and finally calls either
   test-function or the value of TEST with these arguments.  If the result is false, the value of
   accessors-equal-p is false.  Otherwise, continues with the next item in ACCESSORS, returning the
   final value.  The result will be true if all clauses in ACCESSORS evaluate to true."
  (with-gensyms (v-test v-obj1 v-obj2)
    `(let ((,v-test ,test)
           (,v-obj1 ,obj1)
           (,v-obj2 ,obj2))
       (declare (ignorable ,v-test))
       (and ,@(loop for x in accessors
                    for slot = (if (symbolp x) x (car x))
                    for tst = (if (symbolp x) v-test (cadr x))
                    for fun = (fintern "~A~A" prefix slot)
                    collect `(funcall ,tst (,fun ,v-obj1) (,fun ,v-obj2)))))))

(defmacro with-null-or-equal ((prefix a1 a2 &optional (default-test 'string-equal)) &body body)
  "Macro used to build equality methods
   It defines a macro called null-or-equal in the scope of body
   that is used to compare slot values of a1 and a2.
   The two values must both be null, or else both be non-null and
   pass the test"
  (with-gensyms (left right reader)
    `(macrolet ((null-or-equal (slot &optional (test ',default-test))
                  (let ((,reader (fintern ,(strcat (string-upcase (string prefix)) "~A") slot)))
                    `(let ((,',left (,,reader ,',a1))
                           (,',right (,,reader ,',a2)))
                       ,(if (member test '(eq eql equal equalp))
                          `(,test ,',left ,',right)
                          `(cond
                            ((and (null ,',left) (null ,',right))
                             t)
                            ((or (null ,',left) (null ,',right))
                             nil)
                            (t
                             (,test ,',left ,',right))))))))
       ,@body)))


;;--- Beware! The WITH-KEYS macro has a semantic catch:
;; it passes all specified arguments, not preserving the upstream absence of keyword argument.
;; This matters when the downstream function detects argument absence, or has a default value
;; that differs from the upstream default value. See notably MAKE-RECORD and :RECORD-ID.
;; To supply arguments only when present, use MAKE-KEYARGS below, as in
;; (with-keys () (key1 key2) (apply #'func arg1 arg2)
;;    (make-keyargs (optarg1 optarg1p) optarg2 ((:optarg3 (+ preoptarg3 foo)) optarg3p)))
;;
(defmacro with-keys ((&optional (arg (gensym)) (expr arg)) keys &optional prefix &rest postfix)
  (append prefix
          (mapcan (eval `(lambda (,arg) (list (kintern "~A" ,arg) ,expr)))
                  keys)
          postfix))

;; MAKE-KEYARGS builds a plist suitable to APPLY to a function expecting &key arguments.
;; a full specification is of the form
;;   ((keyword value) presentp)
;;      where keyword is a keyword argument (e.g. :foo),
;;      value is the value to supply for said keyword argument (e.g. (new-foo 42)),
;;      and presentp is a boolean telling whether to supply the argument to begin with.
;;   short form (symbol presentp) is same as ((:symbol symbol) presentp)
;;   short form symbol is same as ((:symbol symbol) symbol)
;;
(defmacro make-keyargs (&rest specs)
  `(append
    ,@(loop for spec in specs collect
           (let (thing presentp keyword value)
             (cond
               ((consp spec) (setf thing (first spec) presentp (second spec)))
               ((symbolp spec) (setf thing spec presentp spec)) ;; presentp (fintern "~AP" spec) ???
               (t (error "wrong keyarg spec ~A" spec)))
             (cond
               ((consp thing) (setf keyword (first thing) value (second thing)))
               ((symbolp thing) (setf keyword (keywordify thing) value thing))
               (t (error "wrong keyarg spec ~A" spec)))
             `(when ,presentp (list ,keyword ,value))))))

(defmacro make-prefixed-accessor-keyargs ((prefix instance) &rest specs)
  "Similar to make-keyargs, but uses the prefix & instance to do a with-prefixed-accessors on the
   'value' portion first. Only supports specs with the 'short' form. E.g., just a single symbol per
   spec. Each symbol must be a valid accessor for INSTANCE once the PREFIX is applied to it."
  `(with-prefixed-accessors (,@specs)
       (,prefix ,instance)
     (make-keyargs ,@specs)))
  

;;bwagner: second argument to collector function was broken, and barely used, so I removed it.  If
;;you want to add it again, I request that the argument be one of :append or :nconc to make it clear
;;from the caller what is happening.  Note that :append needs to copy!
(defmacro with-collectors ((&rest collection-descriptions)
			   &body body)

  "COLLECTION-DESCRIPTIONS is a list of clauses, each of which is
   (VARIABLE FUNCTION).  The body can call FUNCTION on an argument
   to add that value to the end of a list kept in the value of VARIABLE.
   FUNCTION runs in constant time, regardless of the length of the list.
   Alternatively, a clause can be (PLACE FUNCTION :FREE T), in which
   case no variable is bound, and FUNCTION (destructively) adds the value 
   to the list already stored in PLACE."

  (let ((let-bindings nil)
        (flet-bindings nil)
        (object '#:OBJECT)
        (dynamic-extent-fns nil))
    (dolist (collection-description collection-descriptions)
      (destructuring-bind (collection-place collector-name &key free elem-type)
          collection-description
        (let ((tail-name (make-symbol (format nil "~A-TAIL" collection-place))))
          (unless free
            (assert (not (listp collection-place)) ()
                    "Unless it is free, the collection name must be a symbol, not ~A~%"
                    collection-place)
            (push collector-name dynamic-extent-fns))
          (setq let-bindings
                (nconc let-bindings
                       `(,@(unless free `((,collection-place nil)))
                           (,tail-name ,@(if free
                                           `((last ,collection-place))
                                           `(()))))))
          (setq flet-bindings
                (nconc flet-bindings
                       `((,collector-name (,object)
                           ,@(when elem-type
			       `((check-type ,object ,elem-type)))
			   (setq ,tail-name
				   (if ,tail-name
				     (setf (cdr ,tail-name)  (list ,object))
				     (setf ,collection-place (list ,object)))))))))))
    `(let (,@let-bindings)
       (flet (,@flet-bindings)
         ,@(if dynamic-extent-fns
             `((declare (dynamic-extent ,@(nreverse (loop for fn in dynamic-extent-fns
                                                          collect `#',fn))))))
         ,@body))))

(defmacro with-unique-collectors ((&key (test '#'eql)) (&rest collection-descriptions)
				  &body body)

  "COLLECTION-DESCRIPTIONS is a list of clauses, each of which is
   (VARIABLE FUNCTION).  The body can call FUNCTION on one argument
   to add that value to the end of a list kept in the value of VARIABLE.
   Alternatively, a clause can be (PLACE FUNCTION :FREE T), in which
   case no variable is bound, and FUNCTION adds the value to the list
   already stored in PLACE.

   This collects only a single occurrence of each object, using TEST
   to test the equality."

  (let ((let-bindings nil)
        (flet-bindings nil)
        (object '#:OBJECT)
        (dynamic-extent-fns nil))
    (dolist (collection-description collection-descriptions)
      (destructuring-bind (collection-place collector-name &key free elem-type)
          collection-description
        (let ((tail-name (make-symbol (format nil "~A-TAIL" collection-place))))
          (unless free
            (assert (not (listp collection-place)) ()
                    "Unless it is free, the collection name must be a symbol, not ~A~%"
                    collection-place)
            (push collector-name dynamic-extent-fns))
          (setq let-bindings
                (nconc let-bindings
                       `(,@(unless free `((,collection-place nil)))
                           (,tail-name ,@(if free
                                           `((last ,collection-place))
                                           `(()))))))
          (setq flet-bindings
                (nconc flet-bindings
                       `((,collector-name (,object)
                           ,@(when elem-type `((check-type ,object ,elem-type)))
			   (unless (member ,object ,collection-place :test ,test)
			     (setq ,tail-name (if ,tail-name
						(setf (cdr ,tail-name) (list ,object))
						(setf ,collection-place (list ,object))))))))))))
    `(let (,@let-bindings)
       (flet (,@flet-bindings)
         ,@(if dynamic-extent-fns
             `((declare (dynamic-extent ,@(nreverse (loop for fn in dynamic-extent-fns
                                                          collect `#',fn))))))
         ,@body))))

;; provides the same semantics as the macro WITH-COLLECT from GNU clisp.
;; See also with-collectors above if you need more control.
(defmacro with-collected-results ((&rest collectors) &body body)
  "COLLECTORS is a list of function names.  The BODY can call the functions on an argument
   to add that value to the end of an associated list. The functions run in constant time,
   regardless of the length of the list. Multiple values are returned, which are the collected
   lists corresponding to COLLECTORs, in the same order."
  (loop for c in collectors
        for l = (gensym "LIST")
        collect l into collections
        collect (list l c) into collection-descriptions
        finally (return `(with-collectors ,collection-descriptions
                           ,@body
                           (values ,@collections)))))

(defmacro collected-values (&rest collect-descriptions)
  "Each collect-description is (a b).
   For each collect-description, if a is non-null collect b."
  (with-gensyms (coll)
    `(with-collected-results (,coll)
       ,@(mapcar #'(lambda (c)
                     `(when ,(first c)
                        (,coll ,(second c))))
                 collect-descriptions))))


(defmacro let-streams ((streams-to-bind destination-stream) &body body)
  (let ((finish-forms (loop for stream in streams-to-bind collect `(finish-output ,stream))))
  `(progn
     ,@finish-forms
     (let ,(loop for stream in streams-to-bind collect `(,stream  ,destination-stream))
       (unwind-protect
            (progn ,@body)
         ,@finish-forms
         (finish-output ,destination-stream))))))

(defmacro with-output-stream ((stream-var &optional (stream-val stream-var)) &body body)
  (let ((wos-body (gensym "WITH-OUTPUT-STREAM-BODY")))
    `(flet ((,wos-body (,stream-var) ,@body))
       (declare (dynamic-extent #',wos-body))
       (do-with-output-stream ,stream-val #',wos-body))))

;; Calls the continuation with an actual stream argument,
;; Behaves like FORMAT with respect to stream'ing:
;; If the original stream argument is a stream, use it as the actual stream;
;; If the original stream argument is NIL, use a STRING-OUTPUT-STREAM as the actual stream,
;; and return the resulting string.
;; If the original stream argument is T, use *STANDARD-OUTPUT* as the actual stream;
;; If the original stream argument is a string with a fill-pointer,
;; use it as a string-output-stream;
;; Otherwise, signal an error.
(defun do-with-output-stream (stream continuation)
  (etypecase stream
    (stream
     (funcall continuation stream))
    ((eql t)
     (funcall continuation *standard-output*))
    (null
     (with-output-to-string (stream)
       (funcall continuation stream)))
    ((and string (satisfies array-has-fill-pointer-p))
     (with-output-to-string (stream stream)
       (funcall continuation stream)))))



;; intern-prefixed-symbol :: (Union String Symbol) (Union String Symbol)
;;                           [Package-Designator]
;;                        -> Symbol (Union NIL :inherited :external :internal)
(defun intern-prefixed-symbol (prefix root &optional pkg)
  "Constructs a symbol whose name is PREFIX concatenated with ROOT, converted
to uppercase, and interns the symbol in the package PKG (defaulting to the
current package).  The return values are as with INTERN."
  (let ((sym-name (format nil "~:@(~A~A~)" prefix root)))
    (if pkg
        (intern sym-name pkg)
        (intern sym-name))))

(defmacro with-prefixed-accessors (entries (prefix instance) &body body)
  "A more convenient form of 'with-accessors', bridging the gap to 'with-slots'.

  Each entry in 'entries' takes the form:
     entry := accessor-root | (variable-name accessor-root)
  If 'variable-name' is not provided, it defaults to 'accessor-root'.

  Each 'accessor-root' is prefixed with 'prefix' to construct the name of a slot
   accessor, and then each 'variable-name' is bound to a symbol-macro which applies
   the corresponding accessor to 'instance'.  The 'prefix' may be \"fully-qualified\",
   if it itself contains a package-name prefix followed by one or two colons.

  If no package information is provided in the prefix, the current package is used.

  Example:

      (defclass foo ()
        ((barometry :accessor foo-barometry)
         (bazomorph :accessor foo-bazomorph)))

      (with-prefixed-accessors ((bar barometry)
                                bazomorph)
          (\"the-pkg:foo-\" some-foo-instance)
        (setf bar bazomorph))"
  (flet ((entry-var-root (entry)
           (if (listp entry)
             (progn
               (assert (null (cddr entry)) ()
                       "With-prefixed-accessors expects accessor-root or (variable-name accessor-root)")
               (values (car entry) (cadr entry)))
             (values entry entry))))
    `(with-accessors (,@(loop for (entry . rest) on entries collect
                           (multiple-value-bind (var root)
                               (entry-var-root entry)
                             (when (find var rest :key #'entry-var-root)
                               (error "duplicate definition for variable ~S in WITH-PREFIX-ACCESSOR" var))
                             `(,var ,(intern-readable-symbol-name
                                      (format nil "~:@(~A~A~)" prefix root))))))
         ,instance
       ,@body)))

(declare-indentation with-prefixed-accessors multiple-value-bind)

(defun method-wrap (terms method-list)
  (if method-list
    (method-wrap (list (list* (car method-list) terms)) (cdr method-list))
    (car terms)))

(defmacro define-trampoline-path (method-name lambda-list &rest method-list)
  "Define a path to get to a value.
   (define-trampoline-path name ((foo class) x y) method1 method2)
   expands to
   (defmethod name (foo class) (y (x foo class)))"
  (let ((lambda-vars (mapcar #'(lambda (x) (if (consp x) (car x) x)) lambda-list)))
    `(defmethod ,method-name ,lambda-list
    ,(method-wrap lambda-vars method-list))))

(defmacro define-trampolines (src-class dest-prefix dest-accessor names &key setf-p strict-p)
  "Define multiple trampolines using a single hop to an object referenced by
   dest-accessor:
   src-class: class of object to define defmethod trampolines over
              or (src-class src-prefix)
              where src-prefix is the prefix to be prepended to get the method names
   dest-prefix: prefix to be prepended to access the trampolined value
   dest-accessor: function to navigate from src object to object that contains the
                  trampolined value (use identity if these are the same)
   setf-p: set to define setf methods
   strict-p: set to check if referenced object exists
   names: list of trampolines.  Each entry is either a symbol or a list of two symbols
          if the name is not the same in the destination object and trampoline"
  (flet ((prefix-intern (prefix suffix)
	   (let* ((prefix-string
		   (etypecase prefix
                     (null "")
		     (string prefix)
		     (symbol (symbol-name prefix))))
		  (position (position #\: prefix-string))
		  (package (if position
			     (nstring-upcase (subseq prefix-string 0 position))
			     *package*))
		  (prefix-string (if position
				   (nstring-upcase (subseq prefix-string (1+ position) (length prefix-string)))
				   prefix-string))
		  (last-char (unless (empty-p prefix-string) (elt prefix-string (1- (length prefix-string)))))
		  (sep (if (or (null last-char) (member last-char `(#\- #\. #\_)))
			 ""
			 "-")))
	     (intern (concatenate 'string prefix-string sep (string-upcase suffix))
		     package))))
    (let* ((src-prefix (if (consp src-class) (second src-class) src-class))
	   (src-class  (if (consp src-class) (first src-class) src-class))
	   (lambda-var (gensym "X"))
	   (value-var (gensym "V"))
	   (dest-obj-var (gensym "D"))
	   (dest-obj (if (or (eq dest-accessor 'identity) (eq dest-accessor #'identity))
		       lambda-var
		       (list dest-accessor lambda-var)))
	   (defmethods
	    (loop for name in names
		  for src-name = (prefix-intern src-prefix (if (listp name) (first name) name))
		  for dest-name = (prefix-intern dest-prefix (if (listp name) (second name) name))
		  if strict-p
		  collect `(defmethod ,src-name ((,lambda-var ,src-class))
			     (let ((,dest-obj-var ,dest-obj))
			       (and ,dest-obj-var (,dest-name ,dest-obj-var))))
		  else
		  collect `(defmethod ,src-name ((,lambda-var ,src-class))
			     (,dest-name ,dest-obj))
		  end
		  if setf-p
		  collect `(defmethod (setf ,src-name) (,value-var (,lambda-var ,src-class))
                             (setf (,dest-name ,dest-obj) ,value-var)))))
      `(progn
	 ,@defmethods))))


(defmacro define-accessor-trampolines ((object class) target-accessor methodnames)
  "Define trampolines: Each method in METHODNAMES will generate a new method 
   specialized on the CLASS of OBJECT. Each new method will call the existing method
   (with the same name) on the object returned by calling TARGET-ACCESSOR on OBJECT.
   
   Note that each method must already have an existing specialization on the type of
   the object returned by TARGET-ACCESSOR. If the TARGET-ACCESSOR returns nil, 
   the newly generated method returns nil."
  (let ((target-object (gensym)))
    `(progn 
       ,@(loop for methodname in methodnames
               collect 
               `(defmethod ,methodname ((,object ,class))
                  (when-bind 
                   (,target-object (,target-accessor ,object))
                   (,methodname ,target-object)))))))


;;; A Compiler Macro to Catch Common Errors Calling ERROR

;; This compiler macro for `error' does no optimization; it just does
;; error checking.  This either signals an error (and never returns)
;; or returns exactly the original form supplied, indicating that no
;; transformation is being done, and the form should be compiled as
;; usual.  At present, this is defined for SBCL only, though it ought
;; to, in principle, be defined for all implementations in use.
;;
;; This is intended to catch a somewhat common problem with calling
;; error that is rather simple to detect.  Whereas a call to error
;; with a first argument a string has the subsequent arguments
;; interpreted as an arbitrary-length list of format args, a call to
;; error with a first argument a condition must have an even-length
;; list of keyword/value pairs.  A somewhat common problem, which to
;; date went undetected (in our current version of SBCL, at least),
;; was supplying a condition symbol (a constant) as the first arg and
;; a string as a constant as the second arg.  This macro will trap
;; such cases, and a few similarly simple-to-detect cases.

(defun check-args-to-cl-error (form datum args)
  (when (and (constantp datum)
             (symbolp (eval datum)))    ; supplied a condition name
    ;; general heuristic plist check:
    (loop with fn = (car form)
          for (should-be-keyword . more?) on args by #'cddr
          as arg-no from 1 by 2         ; zero-based
          when (and (constantp should-be-keyword)
                    (not (symbolp (eval should-be-keyword))))
            ;; No check for keywordp; CL allows nonkeyword "keywords".
            do (error "Arg #~d to ~a (==> ~s <==) must be a symbol (typically a keyword)."
                      arg-no fn should-be-keyword)
          when (null more?)
            do (error "Arg #~d and subsequent args to ~a do not form a keyword/value pair list."
                      arg-no fn))))


#+sbcl
(sb-ext:without-package-locks    ; required by SBCL to modify CL's ERROR
(define-compiler-macro error (&whole form datum &rest args)
  (check-args-to-cl-error form datum args)
  form)
) ; end (sb-ext:without-package-locks ...)

;; Note: it's planned to submit this to SBCL maintainers to hopefully
;; make it, or something similar, part of the Lisp compiler.  I've
;; spoken to nsiivola about this, and he seems to be, in principle, in
;; agreement.  Also, consider defining this for all other
;; implementations we may use! Consider testing for an undefined
;; condition name, e.g., (error 'foo ...), where FOO is not a defined
;; condition. Also, consider testing for a keyword argument (condition
;; slot initializer) for a slot not that's not defined for a known,
;; specified condition, e.g., (error 'bar :baz ...), where BAR is
;; defined but doesn't have a BAZ slot.  In that case, the special
;; :allow-other-keys keyword argument would have to be
;; processed. Note, however, that this code takes the view, that
;; :allow-other-keys never permits odd-length keyword argument lists;
;; therefore, that keyword is never processed. -mdavid, 21jun2007





;;; Post-incrementation, etc., copied from fare-utils

(defmacro define-values-modify-macro (name val-vars lambda-list function)
  "Multiple-values variant on define-modify macro, by Tim Moore"
  (let ((env (gensym "ENV")))
    `(defmacro ,name (,@val-vars ,@lambda-list &environment ,env)
       (multiple-value-bind (vars vals store-vars writer-form reader-form)
           (get-setf-expansion `(values ,,@val-vars) ,env)
         (let ((val-temps (mapcar #'(lambda (temp) (gensym (symbol-name temp)))
                                  ',val-vars)))
           `(let* (,@(mapcar #'list vars vals)
                   ,@store-vars)
              (multiple-value-bind ,val-temps ,reader-form
                (multiple-value-setq ,store-vars
                  (,',function ,@val-temps ,,@lambda-list)))
              ,writer-form
              (values ,@store-vars)))))))

(defmacro define-values-post-modify-macro (name val-vars lambda-list function)
  "Multiple-values variant on define-modify macro,
to yield pre-modification values"
  (let ((env (gensym "ENV")))
    `(defmacro ,name (,@val-vars ,@lambda-list &environment ,env)
       (multiple-value-bind (vars vals store-vars writer-form reader-form)
           (get-setf-expansion `(values ,,@val-vars) ,env)
         (let ((val-temps (mapcar #'(lambda (temp) (gensym (symbol-name temp)))
                                  ',val-vars)))
           `(let* (,@(mapcar #'list vars vals)
                   ,@store-vars)
              (multiple-value-bind ,val-temps ,reader-form
                (multiple-value-setq ,store-vars
                  (,',function ,@val-temps ,,@lambda-list))
                ,writer-form
                (values ,@val-temps))))))))

(defmacro define-post-modify-macro (name lambda-list function)
  "Variant on define-modify-macro, to yield pre-modification values"
  `(define-values-post-modify-macro ,name (,(gensym)) ,lambda-list ,function))

(define-post-modify-macro post-incf () 1+)
(define-post-modify-macro post-decf () 1-)
(define-post-modify-macro post-iincf () i1+)
(define-post-modify-macro post-idecf () i1-)

;;; CFFI stuff

#+cffifoo
(progn

;; We don't add assertions or declarations for now, but we preserve the intent.
(defctype :fixnum :int)

(defctype :char* :pointer)

(defctype :int* :pointer)

(defctype :aligned-pointer
    (:wrapper :pointer
              :to-c pointer-to-aligned-pointer
              :from-c aligned-pointer-to-pointer)       ;--- needs testing
  "Aligned pointers crammed into a fixnum by dropping low bits")

)       ;#+cffifoo


;;; Debugging convenience
(defmacro dbg (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of source expressions and their resulting values, returns the last values"
  (let ((res (gensym))(f (gensym)))
    `(let ((,res))
       (flet ((,f (fmt &rest args)
                (apply #'format *trace-output* fmt args)))
         (,f "~&~S~%" ,tag)
         ,@(mapcan
            #'(lambda (x)
                `((,f "~&  ~S => " ',x)
                  (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
            exprs)
         (finish-output *trace-output*)
         (apply 'values ,res)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Bindings

(defparameter *lazy-let*-dump-locals* nil)

(defmacro lazy-let* ((&rest bindings) &body body)
  ;; Each of BINDINGS is of the form (REFERENCE-NAME EXPRESSION) where
  ;; EXPRESSION will be computed at most once and it's value
  ;; associated with REFERENCE-NAME.  If REFERENCE-NAME is a list of
  ;; variables, then they will be set as if by MULTIPLE-VALUE-SETQ.
  ;; EXPRESSION can refer to any of the other REFERENCE-NAMEs but
  ;; you're doomed if to make circular dependencies.
  (let ((no-value-token '#:no-value)
        (being-evalueated-marker '#:being-evaluated)
        (value-vars nil)
        (funs nil)
        (symbol-macros nil)
        (trace-info nil))
    (loop for (reference-name expression) in bindings
          for rnames = (if (symbolp reference-name)
                           (list reference-name)
                           reference-name)
          for vnames = (mapcar #'(lambda (rname)
                                   (make-symbol (format nil "memoized-~a"
                                                        (symbol-name rname))))
                               rnames)
          do
          (mapc #'(lambda (rname vname)
                    (let ((lazy-eval-code
                           `(cond ((or ,@(mapcar #'(lambda (vn)
                                                     `(eq ,vn ',being-evalueated-marker))
                                                 vnames))
                                   (error "Circular references in LAZY-LET*"))
                                  (t
                                   (when (eq ,vname ',no-value-token)
                                     #||
                                     ,@(mapcar #'(lambda (vn)
                                                   `(assert (eq ,vn ',no-value-token) ()
                                                            "One of several variables already has a value in LAZY-LET*."))
                                               (remove vname vnames))
                                     ||#
                                     ,@(mapcar #'(lambda (vn)
                                                   `(setq ,vn ',being-evalueated-marker))
                                               vnames)
                                     (multiple-value-setq ,vnames ,expression))
                                   ,vname))))
                      (push `(,vname ',no-value-token) value-vars)
                      (push `(,vname () ,lazy-eval-code) funs)
                      (push `(,rname (,vname))
                            symbol-macros)
                      (push (list rname vname) trace-info)))
                rnames vnames))
    `(let (,@value-vars)
       ;; (declare (ignorable ,@(mapcar #'first value-vars)))
       (symbol-macrolet (,@symbol-macros)
         (labels (,@funs)
           (declare (dynamic-extent
                     ,@(mapcar #'(lambda (vv)
                                   `(function ,(first vv)))
                               value-vars)))
           (unwind-protect
                (progn ,@body)
             (when *lazy-let*-dump-locals*
               (force-output *trace-output*)
               (fresh-line *trace-output*)
               (terpri *trace-output*)
               ,@(mapcar #'(lambda (v)
                             (destructuring-bind (rname vname) v
                               `(unless (eq ,vname ',no-value-token)
                                  (format *trace-output* "~&~a~30t~s~%"
                                          ',rname ,vname))))
                         (sort trace-info
                               #'string-lessp
                               :key #'(lambda (v)
                                        (symbol-name (first v)))))
               (force-output *trace-output*))))))))

(defmacro dynamic-extent-labels ((&rest fbindings) &body body)
  (let ((fnames (mapcar 'first fbindings)))
    `(labels ,fbindings
       (declare (dynamic-extent ,@(mapcar #'(lambda (f) `#',f) fnames)))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun temporary-file-name-generator (directory prefix counter file-type)
  (let ((p (make-pathname :defaults directory
                          :type file-type
                          :name (format nil "~a~d"
                                        prefix counter))))
    (if (probe-file p)
        (temporary-file-name-generator directory prefix (1+ counter) file-type)
        (values p (1+ counter)))))

#+NIL ;;DLD|
(defmacro with-temporary-file-names ((&rest file-vars-descs) &body body)
  ;; Each of FILE-VARS-DESCS is a three element list of the form
  ;; (VARIABLE-NAME PREFIX FILE-TYPE) VARIABLE-NAME is taken as a
  ;; variable name to be bound within BODY.  The value of each is a
  ;; pathname whose name begins with prefix and includes a random
  ;; number.  The type of the pathname will be FILE-TYPE.  None of
  ;; these files will exist at the start of BODY.  Any which were
  ;; created will be removed when BODY finishes.
  (let ((temp-directory (temporary-directory))
        (temp-counter-var '#:temp-counter))
    `(let ((,temp-counter-var (random 1000000))
           ,@(mapcar #'first file-vars-descs))
       ,@(loop for (variable-name prefix file-type) in file-vars-descs
               collect
               `(multiple-value-setq (,variable-name ,temp-counter-var)
                  (temporary-file-name-generator ,temp-directory
                                                 ,prefix ,temp-counter-var
                                                 ,file-type)))
       (unwind-protect
            (progn ,@body)
         ,@(mapcar #'(lambda (x)
                       (destructuring-bind (variable-name prefix file-type) x
                         (declare (ignore prefix file-type))
                         `(when (probe-file ,variable-name)
                            (delete-file ,variable-name))))
                   file-vars-descs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useful Macro Helpers

(defun quote-non-literals (term)
  (if (typep term '(or null (eql t) number character string))
    term
    `',term))

(defun self-evaluating-arg-p (arg)
  "Returns true if (macro) argument ARG is a self-evaluating value."
  (or (null arg)
      (and (not (symbolp arg))
           (or (atom arg)
               (and (eq (car arg) 'quote)
                    (consp (cdr arg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro dx-flet (functions &body forms)
  "This is like FLET, except that it also declares each of the
   functions to be of dynamic-extent."
  (let ((names (mapcar #'car functions)))
    `(flet
         ,functions
       (declare (dynamic-extent ,@(mapcar (lambda (x) `(function ,x)) names)))
       ,@forms)))

(declare-indentation dx-flet flet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-abstract-method (function-name lambda-list &body options)

  "FUNCTION-NAME is the name of a function.  LAMBDA-LIST is like
   that of a defmethod, and should specialize at least one parameter.
   OPTIONS are like the options to defgeneric.

   Define a generic function named FUNCTION-NAME, with the same
   arguments as LAMBDA-LIST (only without the specifiers), and
   with the OPTIONS.  Also define a method which causes an assertion violation.

   When you are defining a protocol, i.e. a set of generic functions that
   can be called on a type family, this lets you assert explicitly that
   there is no 'default' method for the generic function.  So anybody
   who adds a new subclass is required to provide a method for this
   generic function."

  `(progn
    (defgeneric ,function-name
        ,(loop for arg in lambda-list
               if (listp arg) collect (first arg)
               else collect arg)
        ,@options)
    (defmethod ,function-name ,lambda-list
      (declare (ignorable ,@(loop for v in lambda-list
				  if (listp v) collect (first v)
				  else unless (member v lambda-list-keywords) collect v)))
      (assert () () "Calling abstract method ~A" ',function-name))))

(defun ensure-fpos (function-form)
  "Ensure `function-form' can syntactically be placed in the function position of an s-expression."
  (cond ((and (consp function-form)
              (or (eq (car function-form) 'function)
                  (eq (car function-form) 'quote)))
         (second function-form))
        (t
         (assert (or (symbolp function-form)
                     (and (consp function-form)
                          (eq (car function-form) 'lambda)))
                 () "~s does not designate a function." function-form)
         function-form)))

(defun get-writer-form (writer value object)
  "Generates a form to write VALUE to OBJECT using writer WRITER, supporting both
   (setf foo) expressions for WRITER as fell as function names and lambda expressions."
  (if (and (listp writer)
           (= (length writer) 2)
           (eq (first writer) 'setf))
    `(setf (,(second writer) ,object) ,value)
    `(,(ensure-fpos writer) ,value ,object)))
         
(defun unwrap-or-null-type (type)
  "If TYPE is (or base-type null) or (or null base-type), this will return base-type.
   Otherwise returns nil."
  (when (and (listp type)
             (= (length type) 3)
             (eq (first type) 'or))
    (cond
      ((eq (third type) 'null) (second type))
      ((eq (second type) 'null) (third type)))))

(defun unwrap-list-of-type (type)
  "If TYPE is (list-of base-type), this will return base-type.
   Otherwise returns nil."
  (when (and (listp type)
             (= (length type) 2)
             (eq (first type) 'list-of))
    (second type)))

(defmacro with-non-local-exit-cleanup (protected-form &rest cleanup-forms)
  "Like unwind-protect, but evaluates CLEANUP-FORMS only in case of a non-local exit from
   PROTECTED-FORM."
  (with-gensyms (non-local-p)
    `(let ((,non-local-p t))
       (unwind-protect
            (multiple-value-prog1
                ,protected-form
              (setf ,non-local-p nil))
         (when ,non-local-p
           ,@cleanup-forms)))))
