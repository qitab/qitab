(in-package :quepasa)

(eval-when (:compile-toplevel :load-toplevel :execute) ;; function used by macros

  (defun assert-bug (format-control &rest format-arguments)

    "Detect that a bug has happened.  This is like calling ASSERT but does not have the TEST-FORM
and PLACES stuff."

    (error 'simple-error
           :format-control format-control
           :format-arguments format-arguments))

  (defun debugf (format-control &rest format-args)

    "Utility function for debugging.  It's like format to *debug-io*, and it forces at the end 
so that you can see the message immediately."

    (declare (dynamic-extent format-args))
    (format *debug-io* "~&~?~%" (or format-control "NIL") format-args)
    (force-output *debug-io*))

  (defgeneric empty-p (sequence)
    (:documentation
     "Return true if the sequence is empty, else false."))

  (defmethod empty-p ((x null))
    t)

  (defmethod empty-p ((x cons))
    nil)

  (defmethod empty-p ((x sequence))
    (i= (length x) 0))

  (defun unless-empty (x)

    "Return true if the sequence is not empty, else false."

    (if (empty-p x) nil x))

  (defun lists-equal (list1 list2 &key (key #'identity) (test #'eql))

    "Return T if every element of list1 is equal to the corresponding element of list2."

    (loop for tail1 = list1 then (cdr tail1)
          for tail2 = list2 then (cdr tail2)
          do (when (xor tail1 tail2) (return nil))
          while (or tail1 tail2)
          always (funcall test
                          (funcall key (car tail1))
                          (funcall key (car tail2)))))

  ;; Is the string empty after trailing whitespace has been stripped?
  (defun normalized-string-empty-p (string &optional start end)
    (let* ((start (or start 0))
           (end   (or end (length string)))
           ;; Find leftmost trailing space
           (lts (cond (string
		       (check-type string string)
		       (locally (declare (type simple-string string)
					 (optimize (speed 3) (safety 0)))
			 (do ((index (i- end 1) (i- index 1)))
			     ((or (i< index start)
				  (not (eql (char string index) #\space)))
			      (i+ index 1)))))
		      (t 0))))
      (i= lts start)))

  (defun strcat (&rest strings)
    "Concatenate a bunch of strings."
    (declare (dynamic-extent strings))
    (apply #'concatenate 'string strings))

  (defun upper-case-ascii-letter-p (ch)

    "Return true if the character is an ASCII uppercase character."

    (char<= #\A ch #\Z))

  (defun lower-case-ascii-letter-p (ch)

    "Return true if the character is an ASCII lowercase character."

    (char<= #\a ch #\z))

  (defun ascii-letter-p (ch)

    "Return true if the character is an ASCII alphabetic character."

    ;; Should replace 'alpha-char-p' everywhere in QRes to make it work
    ;; compatibly independently of the CL implementation.
    (or (upper-case-ascii-letter-p ch)
        (lower-case-ascii-letter-p ch)))

  (defun ascii-digit-p (ch)

    "If the character is an ASCII digit, return the value of the digit."

    ;; Should replace 'digit-char-p' everywhere in QRes to make it work
    ;; compatibly independently of the CL implementation.
    ;; return the digit value if it's a digit, to be compatible with digit-char-p (base is always 10)
    (let ((d (- (char-code ch) (char-code #\0))))
      (when (<= 0 d 9) d)))

  (defun ascii-letter-or-digit-p (ch)

    "Return true if the character is an ASCII lowercase character or an ASCII digit."

    ;; Should replace 'alphanumericp' everywhere in QRes to make it work
    ;; compatibly independently of the CL implementation.
    (or (ascii-letter-p ch)
        (ascii-digit-p ch)))

  (defun ascii-letter-digit-or-dash-p (ch)

    "Return true if the character is an ASCII alphabetic character, digit or dash."

    (or (char= ch #\-) (ascii-letter-or-digit-p ch)))

  (defun ascii-letter-digit-or-space-p (ch)

    "Return true if the character is an ASCII alphabetic character, digit or space."

    (or (char= ch #\space) (ascii-letter-or-digit-p ch)))

  (defun ascii-letter-digit-space-or-dash-p (ch)

    "Return true if the character is an ASCII alphabetic character, digit, space, or dash."

    (or (char= ch #\space) (char= ch #\-) (ascii-letter-or-digit-p ch)))

  (defun ascii-letter-or-space-p (ch)

    "Return true if the character is an ASCII alphabetic character or an ASCII space."

    ;; Should replace 'alphanumericp' everywhere
    (or (char= ch #\space) (ascii-letter-p ch)))

  (defun non-whitespace-char-p (ch)

    "Return true if this character is not ASCII whitespace."

    (declare (optimize (speed 3) (safety 0) (debug 1)))
    (not (whitespace-char-p ch)))

  (defun printable-ascii-char-p (c)

    "Returns true if the character is a printable ASCII character."

    ;; Should replace 'graphic-char-p' everywhere in QRes to make it work
    ;; compatibly independently of the CL implementation.
    (and (characterp c)
         (char<= #\space c #\~)))

  (defun whitespace-char-p (ch)

    "Return T if character is a whitespace character in ASCII or UNICODE."

    (declare (optimize (speed 3) (safety 0) (debug 1)))
    (let ((code (char-code ch)))
      (declare (fixnum code))
      (or (<= #x0009 code #x000D)
          (eql code #x0020)
          (eql code #x0085)
          (eql code #x00A0)
          (eql code #x1680)
          (eql code #x180E)
          (<= #x2000 code #x200A)
          (eql code #x2028)
          (eql code #x2029)
          (eql code #x202F)
          (eql code #x205F)
          (eql code #x3000))))

  (defun fintern (format-string &rest format-args)

    "This interns a new symbol in the current package.
     This conses, so don't use it in time-critical code."

    (declare (dynamic-extent format-args))
    (intern (nstring-upcase (apply #'format nil format-string format-args))))

  (defun kintern (format-string &rest format-args)

    "This interns a new symbol in the keyword package..
     This conses, so don't use it in time-critical code."

    (declare (dynamic-extent format-args))
    (intern (nstring-upcase (apply #'format nil format-string format-args)) "KEYWORD"))

  (defmacro xintern (format-string &rest format-args)

    "This interns a symbol in the package of the file in which the call to xintern is compiled.
     This conses, so don't use it in time-critical code."

    `(xintern-1 ,(package-name *package*) ,format-string ,@format-args))

  (defun xintern-1 (package format-string &rest format-args)

    "This interns a symbol in the given package.
     This conses, so don't use it in time-critical code."

    (declare (dynamic-extent format-args))
    (intern (nstring-upcase (apply #'format nil format-string format-args)) package))

  (defun keywordify (x &optional keywordify-nil)

    "Turn this into a keyword.  The argument should be a string,
   or a symbol from any package.  It returns a keyword with
   the same name.  Given the symbol nil, it returns nil.
   Given the string _NIL_, returns:NIL if keywordify-nil is true, else nil.
   This conses, so don't use it in time-critical code."

    (cond ((null x)
           nil)
          ((keywordp x)
           x)
          ((symbolp x)
           (keywordify (symbol-name x)))
          ((normalized-string-empty-p x)
           nil)
          ((string-not-equal x "NIL")
           (intern (string-upcase x) #.(find-package "KEYWORD")))
          (keywordify-nil
           :NIL)))

  (defun keyword-to-symbol (keyword &optional package)

    "Given a keyword, return a symbol with the same name
   in the current or supplied package."

    (or (find-symbol (symbol-name keyword) (or package *package*))
        (read-from-string (symbol-name keyword))))

  (defun symbol-to-keyword (symbol &optional error-p)

    "Given a symbol, return a keyword with the same name.
   If the keyword does not exist, if error-p is true
   then signal an exception, otherwise make the symbol."

    (or (find-symbol (symbol-name symbol) #.(find-package "KEYWORD"))
        (if error-p
          (error "Couldn't find keyword for symbol ~A" symbol)
          (intern (symbol-name symbol) #.(find-package "KEYWORD")))))

  (defun onliest (list)
    "Asserts that list LIST has at most one element.  Returns the sole element or NIL if no elements."
    ;;---*** it would probably make sense to change this function to signal an error if the list is
    ;;---*** empty, but that requires analyzing all callsites
    (assert (null (cdr list))
            () "The list ~S has more than one element" list)
    (car list))

  (defun readable-symbol-name (symbol)

    "Return a string containing the fully qualified symbol name for SYMBOL."

    (with-output-to-string (stream)
      (let ((*package* (find-package :keyword)))
        (write symbol
               :stream stream
               :case :upcase
               :escape t
               :readably t
               :length nil
               :level nil
               :pretty nil))))

  (defun intern-readable-symbol-name (string &optional (default-package *package*))

    "Intern a symbol name, respecting package identifier if fully-qualified.

    - STRING is a symbol name
    - PACKAGE is a package designator (default:  current package)

  Unless it is fully-qualified, the symbol name will be interned in PACKAGE ---
  and an error will be signalled if PACKAGE does not resolve to a package.

  Return values:  <symbol>
                  <access> - one of NIL, :INTERNAL, :EXTERNAL, :INHERITED,
                              as returned from INTERN."

    (multiple-value-bind (package+name access-specified)
        (let ((p (position #\: string)))
          (cond ((and p (char= #\: (char string (1+ p))))
                 (values (list (subseq string 0 p) (subseq string (+ p 2))) :internal))
                (p
                 (values (list (subseq string 0 p) (subseq string (1+ p)) :external)))
                (t nil)))
      (declare (ignorable access-specified)) ; DLD| if assert below is uncommented, remove
      (let ((symbol-name (if package+name
                           (cadr package+name)
                           string))
            (package-name (if package+name
                            (if (empty-p (car package+name))
                              :keyword
                              (car package+name))
                            default-package)))
        (multiple-value-bind (symbol access)
            (intern symbol-name package-name)
          #| DLD| unreachable code?
          (assert (or (not (eq access-specified :external))
                      (not (eq access :internal)))
                  () "~S is not external in the ~S package." symbol package-name)
          |#
          (values symbol access)))))

  (defun xor (&rest args)

    "Return logical xor: true if an odd number of the arguments are non-nil."

    (declare (dynamic-extent args)
             (type list args))
    ;; Optimize for two arguments.
    (case (length args)
      (2 (if (car args) (not (cadr args)) (cadr args)))
      (1 (car args))
      (0 nil)
      (t (if (car args)
           (not (apply #'xor (cdr args)))
           (apply #'xor (cdr args))))))
)
