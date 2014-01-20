;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2011 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author:  Carl de Marcken (Quicket [contiguous 32 hours  ;;;
;;;                      IIRC (ddavies) and very nearly bug free.    ;;;
;;;                      But he's probably done it before :)  ])     ;;;
;;; Supporting cast:  Derek Davies (unquicketization, unparser &     ;;;
;;;                      various other junk).                        ;;;
;;;                   Scott McKay (thunks for better efficiency).    ;;;
;;;                   Matthew Sachs (many improvements & grammar     ;;;
;;;                      ops).                                       ;;;
;;;                   Hugh Robinson (iterators in parser for much    ;;;
;;;                      better efficiency).                         ;;;
;;;                   Ethan Schwartz (packagization).                ;;;
;;;                                                                  ;;;
;;; The vast majority of this code was cribbed directly from         ;;;
;;; Carl de Marcken's quicket framework.  Quepasa is mostly a        ;;;
;;; simplification of Carl's work plus the addition of an "unparser" ;;;
;;; which is a means of serializing sentences from grammatically     ;;;
;;; consitent lists of object hierarchies (such as would be produced ;;;
;;; by a succusessful quepasa parse).                                ;;;
;;;                                                                  ;;;
;;; Quepasa is a grammar driven parser/serializer that comes from    ;;;
;;; experience gained while using the quicket framework.  It can     ;;;
;;; both parse and unparse ("serialize") sentences described by the  ;;;
;;; same user supplied attributed grammar.                           ;;;
;;;                                                                  ;;;
;;; Quicket provides a strong type system, with conditions, at the   ;;;
;;; expense of sacrificing the built in dynamic object system's      ;;;
;;; functionality.  Quepasa seeks to leverage the built in object    ;;; 
;;; system at the expense(?) of strong type checking.  In other      ;;;
;;; words Quicket imposes a stricter type system upon Lisp while     ;;;
;;; Quepasa interacts with the Lisp object system directly.          ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :quepasa)


;;; Grammar stuff.

(defvar *quepasa-grammars* nil)

(defparameter *debug-quepasa-grammar* nil

  "Set to T to print out tags of successful parses.  This will force the non-compacted
   grammar to be used, since it produces better debugging output.
   You can set this to :COMPACTED to debug the compacted grammar.")

(defparameter *quepasa-compact-grammar* T

  "Set to nil to use the non-compacted productions.
   This is necessary to get good error messages for parse errors.  If *DEBUG-QUEPASA-GRAMMAR* is T,
   this will be ignored and compacted productions will not be used.  If *DEBUG-QUEPASA-GRAMMAR*
   is :COMPACTED, this will be ignored and compacted productions /will/ be used.")


(defparameter *quepasa-max-parses-to-return* 1
  "If set to anything greater than 1, quepasa sacrifices some efficiency in favor of a variety of
   answers. This variant probably is best reserved for small inputs and minimally ambiguous
   grammars.")

;;; These parameters control the error messages produced by a parse.

(defparameter *quepasa-parse-error-max-branch-depth* 2

  "Controls the error message emitted for a failed parse.  When there are multiple failed daughter
   production alternatives, this controls how many layers of possibilities will be
   explained in the error message.  Restricting this prevents error messages like:

     'Expected SSR (segment (date or booking class) or names) or OSI (code or free-text).'

   but instead would present the nicer:

     'Expected SSR (segment or names) or OSI (code or free-text).'")

(defparameter *quepasa-parse-error-max-depth* 3

  "Controls the error message emitted for a failed parse.  This is for the non-branching case, to
   stop error messages like:

     'Invalid AIRIMP message SSR segment date month.'")

(defmacro use-compacted-productions ()

  "A 'compacted' grammar is one which has just a single production.  There are no '->' or 'tag'
   operators.  Any tag operators are simply replaced by their daughter production(s).  Any ->
   operators are replaced by their expantions.  This makes quepasa faster since parsing time is
   proportional to the size of the grammar.  There is a catch though: circular grammars are not
   compactable."

  `(or (eq *debug-quepasa-grammar* :compacted)
       (and (null *debug-quepasa-grammar*)
            *quepasa-compact-grammar*)))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass regex-prim-tab-entry ()
  ((type
    :type symbol
    :initarg :type
    :initform (error "Must initialize TYPE slot of REGEX-PRIM-TAB-ENTRY class.")
    :reader regex-prim-tab-entry-type)
   (style
    :type symbol
    :initarg :style
    :initform (error "Must initialize STYLE slot of REGEX-PRIM-TAB-ENTRY class.")
    :reader regex-prim-tab-entry-style)
   (token-regex
    :type string
    :initform (error "Must initialize TOKEN-REGEX slot of REGEX-PRIM-TAB-ENTRY class.")
    :initarg :token-regex
    :reader regex-prim-tab-entry-token-regex)
   (delimiter-regex
    :type string
    :initform (error "Must initialize DELIMITER-REGEX slot of REGEX-PRIM-TAB-ENTRY class.")
    :initarg :delimiter-regex
    :reader regex-prim-tab-entry-delimiter-regex)
   (delimiter-literal
    :type string
    :initform (error "Must initialize DELIMITER-LITERAL slot of REGEX-PRIM-TAB-ENTRY class.")
    :initarg :delimiter-literal
    :reader regex-prim-tab-entry-delimiter-literal)
   (coersion-function
    :type symbol
    :initarg :coersion-function
    :initform (error "Must initialize COERSION-FUNCTION slot of REGEX-PRIM-TAB-ENTRY class.")
    :reader regex-prim-tab-entry-coersion-function)))

(defclass quepasa-grammar ()
  ((id
    :initarg :id
    :reader quepasa-grammar-id
    :documentation
    "")
   (root
    :initform 'root
    :documentation
    "")
   (named-productions
    :initform (make-hash-table)
    :reader quepasa-grammar-named-productions
    :documentation
    "")
   (regex-prim-tab
    :initform NIL
    :type (or null hash-table)
    :accessor quepasa-grammar-regex-prim-tab
    :documentation
    "Table storing information about automatically generated primitive parsers/writers.
Entries are added to the table at load time as quepasa grammars are checked and
rewritten.  The table is used at the end of the build, at load time, to print
warnings while checking the grammar,  and to generate the actual primitive
functions.  The table is then deleted, again at load time, since it's not
needed after regex primitives have been defined.")
   (compacted-production
    :initform NIL
    :documentation
    "")
   (unit-test-cases
    :initarg :unit-test-cases
    :initform NIL
    :type list
    :reader quepasa-grammar-unit-test-cases
    :documentation
    ""))
  (:documentation
   "A quepasa-specific grammar defining the syntax of a language.  Sentences in the language may
    then be parsed or object hierarchies turned into sentences ('unparsing')."))

)  ; end of eval-when

#+make-dependencies
(defmethod initialize-instance :after ((o quepasa-grammar) &key id)
  (asdf-dependency-grovel:signal-provider id 'quepasa-grammar))

(defun find-quepasa-grammar (id)

  "Given an ID for a registered quepasa grammar return the QUEPASA-GRAMMAR instance for
   the grammar.  If the ID is invalid or the grammar has not been properly registered
   return NIL."

  (assert id)
  #+make-dependencies
  (asdf-dependency-grovel:signal-user id 'quepasa-grammar)
  (find id *quepasa-grammars*
	:key #'quepasa-grammar-id))

(defun find-quepasa-grammar! (id)

  "Like FIND-QUEPASA-GRAMMAR only if the ID is invalid or the grammar has not been properly
   registered invoke an error."

  (or (find-quepasa-grammar id)
      (error "no known grammar ~A" id)))

#+NIL
(defmethod print-object ((g quepasa-grammar) stream)

  "Print a pleasing representation of QUEPASA-GRAMMAR G on STREAM."

  (print-unprintable-object (g stream :type t :identity t)
    (format stream "~A" (quepasa-grammar-id g))))

(defun grammar-root-production (grammar)

  "Return the root production of GRAMMAR or NIL if GRAMMAR has not been properly initialized."

  (or (and (use-compacted-productions)
	   (slot-value grammar 'compacted-production))
      (gethash (slot-value grammar 'root)
	       (slot-value grammar 'named-productions))))

(defun quepasa-grammar-check/rewrite-production-syntax (name production grammar)

  "Check the syntax of the production referred to by the symbol NAME.  The PRODUCTION parameter is
   the right hand side of the production (sorry about the confusing naming - renaming PRODUCTION as
   RHS might be better but then how do we name subproduction?  subrhs?  I don't know).  PRODUCTION
   is syntax checked and, depending on the production's operator, may be rewritten for internal
   use.

   The syntax, semantics (only because this is a convenient place to document them - this routine
   does nearly nothing with semantics) and rewriting is as follows:

   * A string literal may not be the empty string.  It is not rewritten since it's a literal.

   * Every subproduction that's not a string literal must be a list whose car is a valid grammar
     operator.

   * Grammar operators are symbols that are internally changed to keywords.  Abbreviated grammar
     operators are expanded into their canonical, more verbose and hopefully more meaningful,
     keywords.

   * The user-level syntax for TAG is:

     (:TAG (:key <key(s)> :label <label>) <daughter>+)

     Where:

      :T is a valid abbreviation for :TAG.

      <key(s)> is a keyword or a list of keywords that are gathered into a list returned by
      PARSE-QUEPASA-GRAMMAR-STRING which represents a derivation of a successfully parsed
      sentence.

      <label> is a string used to create error messages in the event of an unsuccessful
      parse.  Labels at different levels in the grammar are concatenated in order to generate
      a helpful error message for the user.

      <key(s)>, <label> or both may be omitted but if both are omitted there must be an
      empty list in their place, eg '(:TAG () <daughter> ...)'.

     (:TAG (:key <key(s)> :label <label>) <daughter>) recursively checks and rewrites <daughter>.

     (:TAG (:key <key(s)> :label <label>) <daughter1> .. <daughterN>)

       is rewritten as:

     (:TAG (:key <key(s)> :label <label>) (:sequence <daughter1> .. <daughterN>) where each
       <daughterX> is recursively checked and rewritten.

   * :NEVER-WRITE :ALWAYS-WRITE :MATCH-MINIMALLY :MATCH-MAXIMALLY :REQUIRE and :COULD-PARSE
     share the same user level and internal syntax:

     (<op> <daughter> ...)

     :NEVER-WRITE (abbreviated :NOGEN) marks a subproduction as significant to the parser,
     but insignificant to the unparser.  This is often used with :OPTIONAL for when we want
     to accept some language construct but never include it in sentences we output from the
     unparser.

     :ALWAYS-WRITE (abbreviated :GEN) marks a subproduction as significant to the unparser,
     but insignificant to the parser.  This is often used with :IF and :IF-NOT operators to
     conditionalize sentence generation depending on to whom we're speaking.

     :MATCH-MINIMALLY (abbreviated :MIN) and :MATCH-MAXIMALLY (abbreviated :MAX) :REQUIRE
     (no abbreviation) :COULD-PARSE (no abbreviation).  I (ddavies) need to write to
     msachs and ask him to detail this.  What I remember is that these are used for error
     messages in order to provide the right level of detail for the user.

     (<op> <daughter>) remains the same except that <daughter> is checked and rewritten.

     (<op> <daughter1> .. <daughterN>)

       is rewritten as:

     (<op> (:sequence <daughter1> .. <daughterN>) where each <daugterX> is checked and rewritten.

   * :OR (no abbreviation) and :SEQUENCE (abbreviated :SEQ) allow a single daughter production,
     but that's bad style and needlessly adds to the time required to parse and unparse.  They
     share the same user level and internal syntax:

     (<op> <daughter> ...)

     :OR specifies alternate valid daughter syntaxes for a grammar.

     :SEQUENCE specifies a required ordering of daughter syntaxes for a grammar.

     (<op> <daughter>) remains the same except that the <daughter> is checked and rewritten.

     (<op> <daughter1> .. <daughterN>)

       is rewritten as:

     (<op> (:sequence <daughter1> .. <daughterN>) where each <daughterX> is checked and rewritten.

   * :IF and :IF-NOT (no abbreviation for either) share the same user level syntax:

     (<IF-op> <condition> <daughter> [ <daughter> ] )

     Please note that (<IF-op> (COULD-PARSE <daughter> ...)) is not accepted.  I don't know the
     details of why that's the case.  Need to ask msachs about it.

     For parsing these are conceptually :OR - <condition> is not evaluated and the daughters are
     considered in turn until a match is found or there are no more daughters to consider.

     When unparsing <condition> is evaluated in the full runtime environment for truth.  Semantics
     follow the usual rules for if statments with the chosen consequence being significant to the
     output sentence being produced.

     IF and IF-NOT can be used to alter the structure of generated language sentences while
     allowing all the variations to be accepted by the parser.

     <IF-op> is not rewritten, but each <daughter> is recursively checked and rewritten.

   * :OPTIONAL and :ONE-OR-MORE

     :OPTIONAL (abbreviated :?) and :ONE-OR-MORE (abbreviated :1+) share the same user level syntax:

     (<op> <daughter>)

     :OPTIONAL allows <daughter> as a valid construct but does not require it.

     :ONE-OR-MORE allows any number of repitions of <daughter>, requiring at least one.

     These production forms are not rewritten, but <daughter> is recursively checked and rewritten.

   * :ZERO-OR-MORE

     :ZERO-OR-MORE (abbreviated :0+) has the following user level syntax:

     (:ZERO-OR-MORE <daughter>)

     It allows any number or repetitions of <daughter>, including none.

     It is rewritten internally as:

     (:OPTIONAL (:ONE-OR-MORE <daughter>))

     where <daughter> is recursively checked and rewritten.

   * :SEPARATED-LIST

     :SEPARATED-LIST (abbreviated :SLIST) has the following user level syntax:

     (:SEPARATED-LIST <LIST-SLOT-daughter> <separator-daughter>)

     Where <LIST-SLOT-daughter> must be a :LIST-SLOT production form.

     :SEPARATED-LIST is typically enclosed in a :ZERO-OR-MORE or :ONE-OR-MORE form and mandates
     that each repetition of <LIST-SLOT-daughter> be separated by the language construct given
     by <separator-daughter>.

     :SEPARATED-LIST is not rewritten, but each daughter is recursively checked and rewritten.

   * :OBJECT

     :OBJECT has the following user level syntax:

     (:OBJECT <type> <daughter> ...)

     :OBJECT provides for instantiation of an object of type <type>.

     (:OBJECT <type> <daughter>)

       is rewritten internally as:

     (:OBJECT <type> <daughter> <instantiation-of-<type>-function>)

     where <daughter> is recursively checked and rewritten.

     (:OBJECT <type> <daughter1> .. <daughterN>)

       is rewritten internally as:

     (:OBJECT <type> (:SEQUENCE <daughter1> .. <daughterN>) <instantiation-of-<type>-function>)

     where each <daughter> is recursively checked and rewritten.  See also :SLOT and :LIST-SLOT.

   * :SLOT and :LIST-SLOT

     :SLOT and :LIST-SLOT share the same user level syntax:

     (<SLOT-op> <slot-name> <daughter>)

     where <slot-name> takes one of the forms:

     <symbol>

     (<symbol> :object-type <object-type>)

     where <symbol> names a slot on an object instance instantiated by an enclosing :OBJECT
     production form.  In the second, 'extended' <slot-name> form, both <symbol> and <object-type>
     must be given.  In the first, 'simple' <slot-name> form the named slot of the immediately
     enclosing object is filled with the the result of evaluating <daughter>.  In the extended form
     the slot in the first object of <object-type> in the stack of instantiated objects is filled.
     When <SLOT-op> is :SLOT the last :PRIMITIVE, :CONSTANT, :STRING-OFFSET, or literal value
     produced by <daughter> is the value used to populate the slot.  When <SLOT-op> is :LIST-SLOT
     each
     :PRIMITIVE, :CONSTANT, :STRING-OFFSET, and literal value produced by <daughter> are combined,
     in order produced, into a list which is used to populate the slot.

     Internally a <SLOT-op> form is rewritten as:

     (<SLOT-op> <slot-name> <daughter> (<slot-reader-function> <slot-writer-function>))

     where <daughter> is recursively checked and rewritten and <slot-reader-function>
     and <slot-writer-function> are constructed so as to make slot lookup static at
     runtime.

   * :PRIMITIVE

     :PRIMITIVE has the user level and internal form:

     (:PRIMITIVE <type> <style>)

     Both <type> and <style> must be symbols.  Together, <type> and <style> name a primitive
     reader and a primitive writer.  The primitive reader is a tokenizer used to validate a
     token in the input sentence.  The primitive writer transforms a slot value into a token
     in the output sentence.  The values produced by the primitives are typically put into or
     taken from an enclosing <SLOT-op> and <OBJECT> production form depending on if we're
     parsing or unparsing.  Best practice says that <type> should be the lisp type of the
     slot value and <style> should be a descriptive name for the token.  Primitive writers
     are defined using QUEPASA:DEFINE-PRIMITIVE-PARSER and primitive writers are defined using
     QUEPASA:DEFINE-PRIMITIVE-WRITER.
 
   * :REGEX-PRIMITIVE

     :REGEX-PRIMITIVE has the user level and internal form:

     (:REGEX-PRIMITIVE
      <type> <style> <token-regular-expression>
      &key <coersion-function>
           <delimiter-regular-expression>
           <delimiter-literal>)

     Both <type> and <style> have the same semantics as in :PRIMITIVE.  :REGEX-PRIMITIVE
     automatically creates a primitive parser and primitive writer using
     <token-regular-expression> and, optionally, <coersion-function> and/or
     <delimiter-regular-expression> and <delimiter-literal>.  For variable length tokens
     <delimiter-regular-expression> is used to match a delimiter after the variable length
     token being parsed and <delimiter-literal> is temporarily concatenated to the token
     when serializing the token.  Primitives for fixed width tokens don't specify
     <delimiter-regular-expression> or <delimiter-literal>.

     After defining the primitive function the :REGEX-PRIMITIVE form is rewriten as
     ':PRIMITIVE <type> <style>'.

   * :CONSTANT

     :CONSTANT has the user level and internal form:

     (:CONSTANT <constant>)

     where <constant> is a lisp constant that is put into or taken from an enclosing <SLOT-op>
     and <OBJECT> production form depending on if we're parsing or unparsing.

   * :STRING-OFFSET

     :STRING-OFFSET has the user level and internal form:

     (:STRING-OFFSET)

     It causes an integer representing the parser's current position within the string to be put
     into an enclosing <SLOT-op> and <OBJECT> production form when parsing.  It is ignored when
     unparsing.

   * :->

     :-> has the user level and internal syntax:

     (:-> <production-name>)

     or

     (:-> <grammar-id> <production-name>)

     In the first form <production-name> is looked up in the NAMED-PRODUCTIONS slot of the
     current QUEPASA-GRAMMAR.  It's an error if there isn't an entry in the hash table.  The
     value returned is then used as a daughter production for parsing and unparsing.

     In the second form <grammar-id> is used by FIND-QUEPASA-GRAMMAR to temporarily replace
     the current grammar with the grammar named by <grammar-id> and the new grammar is
     used to find the daughter production from a lookup of the NAMED-PRODUCTIONS from it.

     In the second form <grammar-id> must be a symbol.  In both forms <production-name> must
     be a symbol.
"

  (labels
      ((fail (production &optional message)
	 (error "Illegal grammar production in ~A~:[~*~; ~A~]~@[: ~S~]."
		name message message (summarize-prod production)))
       (warning (production &optional message)
         (warn "Grammar producton in ~A~:[~*~; ~A~]~@[: ~S~]."
               name message message (summarize-prod production)))
       (check/rewrite-seq (productions source)
         "If PRODUCTION has more than a single subproduction (aka daughter) make the implicit
          :sequence operator surrounding them explicit, then replace each subproduction in SOURCE
          with its checked and rewritten subproduction."
	 (unless productions
           (fail NIL (format nil "Empty subproduction(s) in ~S." source)))
	 (subst (let ((r (mapcar #'check/rewrite productions)))
		  (if (rest r)
		      (list (cons :sequence r))
		      r))
		productions
		source))
       (check/rewrite (production)
	 (cond ((stringp production)
		(if (string-equal production "")
                  (fail production "Right hand side of production is an empty string.")
                  ;; Match string literal, eg "LITERAL".  No transformation.
                  production))
	       ((not (listp production))
		(fail production
                      (format nil "Right hand side of production ~S is not a list." production)))
	       (t
		(unless (symbolp (first production))
		  (fail
                   production
                   (format nil
                           "First element of right hand side of production ~S is not a symbol."
                           production)))
		(let* ((k (if (keywordp (first production))
                            (first production)
                            ;; There might be a bit of waste here in that typo'd operators
                            ;; (eg, ':constat' [sic]) will be interned, but this all happens
                            ;; at compile time and the production an error will be generated
                            ;; soon due to the big CASE form that follows.  So I don't think
                            ;; there's a problem with these symbols leaking into the keyword
                            ;; package.
                            (intern (symbol-name (first production))
                                    (find-package :keyword))))
                       ;; Expand abbreviations.
		       (ck (second (assoc k '((:t :tag)
					      (:nogen :never-write)
					      (:gen :always-write)
					      (:seq :sequence)
					      (:opt :optional)
					      (:? :optional)
					      (:1+ :one-or-more)
					      (:0+ :zero-or-more)
					      (:slist :separated-list)
					      (:obj :object)
					      (:lslot :list-slot)
					      (:min :match-minimally)
					      (:max :match-maximally)
					      (:prim :primitive)
                                              (:rep :regex-primitive)
					      (:const :constant)
                                              (:string-offset :string-offset))))))
		  (setf production (cons (or ck k) (rest production))))
		(case (first production)
		  (:TAG
		   (destructuring-bind ((&key key label) . daughters) (rest production)
                     (unless (or (null label)
                                 (stringp label))
                       (fail production
                             (format nil
                                     "Illegal tag label ~S in right hand side of production ~S."
                                     label production)))
		     (flet ((legal-key-p (k)
			      (or (keywordp k) (and (listp k) (every #'keywordp k)))))
		       (unless (legal-key-p key)
			 (fail production
                               (format nil "Bad tag key ~S in right hand side of production ~S."
                                       key production)))
		       (check/rewrite-seq daughters production))))
		  ((:NEVER-WRITE :ALWAYS-WRITE :MATCH-MINIMALLY :MATCH-MAXIMALLY :REQUIRE :COULD-PARSE)
		   (check/rewrite-seq (rest production) production))
		  ((:OR :SEQUENCE )
		   (let ((daughters (rest production)))
		     (unless daughters
                       (fail production
                             (format nil "Daughters are required for ~S operator in production ~S"
                                     (first production) production)))
		     (if (rest daughters)
			 (cons (first production) (mapcar #'check/rewrite daughters))
			 (check/rewrite (first daughters)))))
		  ((:IF :IF-NOT)
		    (destructuring-bind (condition . daughters) (rest production)
		      (unless (let ((l (length daughters))) (and (>= l 1) (<= l 2)))
			(fail production
                              (format nil "IF must have one or two sub productions: ~S."
                                      production)))
                      (when (and (listp condition)
                                 (eq (keywordify (first condition)) :COULD-PARSE))
                        (fail production
                              (format nil "(IF (COULD-PARSE ...)) doesn't work as expected; ~
                               use (SEQ (COULD-PARSE ...))")))
		      (cons (first production)
			    (cons condition (mapcar #'check/rewrite daughters)))))
		  ((:OPTIONAL :ONE-OR-MORE)
		   (destructuring-bind (daughter) (rest production)
		     (subst (check/rewrite daughter) daughter production)))
		  (:ZERO-OR-MORE
		   (destructuring-bind (daughter) (rest production)
		     (list :OPTIONAL
			   (cons :ONE-OR-MORE
				 (subst (check/rewrite daughter)
					daughter
					(rest production))))))
		  (:SEPARATED-LIST
		   (destructuring-bind (daughter separator) (rest production)
		     (let* ((daughter-op    (car daughter))
			    (daughter-op-kw (if (keywordp daughter-op)
					      daughter-op
					      (intern (symbol-name daughter-op)
						      (find-package :keyword)))))
		       (unless (or (eq daughter-op-kw :lslot) (eq daughter-op-kw :list-slot))
			 (fail production
                               "SLIST operator needs an LSLOT expression as its first operand.")))
		     (sublis (list (cons daughter (check/rewrite daughter))
				   (cons separator (check/rewrite separator)))
			     production)))
  		  (:OBJECT
		   (destructuring-bind (obj-name . daughters) (rest production)
                     (let ((constructor (compile nil
                                                 `(lambda ()
                                                    (make-instance ',obj-name)))))
                       (if daughters
                           (nconc (check/rewrite-seq daughters production)
                                  (list constructor))
                           (nconc (check/rewrite production)
                                  (list nil constructor))))))
		  ((:SLOT :LIST-SLOT)
		   (destructuring-bind (extended-slot-name daughter) (rest production)
                     (multiple-value-bind (slot-name object-type)
                         (if (symbolp extended-slot-name)
                              extended-slot-name
                              (destructuring-bind (slot-name &key object-type) extended-slot-name
				   (unless (and (symbolp slot-name) object-type)
                                     (fail
                                      production
                                      (format
                                       nil
                                       "Extended slot names must have a slot ~
                                        specifier and object-type: ~S."
                                       production)))
                                   (values slot-name object-type)))
                       (declare (ignore object-type))
                       (let ((reader (compile nil
                                              `(lambda (obj)
                                                 (and obj
                                                      (slot-boundp obj ',slot-name)
                                                      (slot-value obj ',slot-name)))))
                             (writer (if (eq (first production) :slot)
                                         (compile nil
                                                  `(lambda (val obj)
                                                     (when obj
                                                       (setf (slot-value obj ',slot-name) val))))
                                         (compile nil
                                                  `(lambda (val obj)
                                                     (when obj
                                                       (unless (slot-boundp obj ',slot-name)
                                                         (setf (slot-value obj ',slot-name) nil))
                                                       (let* ((old (slot-value obj ',slot-name))
                                                              (new (nconc old (list val))))
                                                         (setf (slot-value obj ',slot-name) new)))))
                                         )))
                         (nconc (subst (check/rewrite daughter) daughter production)
                                (list reader writer))))))
		  (:PRIMITIVE
		   (destructuring-bind (type style)
                       (rest production)
		     (unless (and (symbolp type) (symbolp style))
                       (fail production "Both type and style of :PRIMITIVE must be symbols")))
		   production)
		  (:REGEX-PRIMITIVE
		   (destructuring-bind (type style token-regex
                                        &key coercion-function delimiter-regex delimiter-literal)
                       (rest production)
                     (unless coercion-function
                       (setq coercion-function 'identity))
                     (when (or delimiter-regex delimiter-literal)
                       (unless (and delimiter-regex delimiter-literal)
                         (fail production "When specifying a delmitier both :delimitier-regex and :delmiter-literal are required")))
                     (unless delimiter-regex
                       (setq delimiter-regex ""))
                     (unless delimiter-literal
                       (setq delimiter-literal ""))
		     (unless (and (symbolp type) (symbolp style))
                       (fail production "Both type and style of :REGEX-PRIMITIVE must be symbols"))
                     (unless (quepasa-grammar-regex-prim-tab grammar)
                       (setf (quepasa-grammar-regex-prim-tab grammar)
                             (make-hash-table :test #'eq)))
                     (let* ((key (keywordify (strcat (symbol-name type) (symbol-name style))))
                            (regex-prim-tab (quepasa-grammar-regex-prim-tab grammar))
                            (val (gethash key regex-prim-tab)))
                       (when val
                         (let ((r (regex-prim-tab-entry-token-regex val))
                               (d (regex-prim-tab-entry-delimiter-regex val))
                               (f (regex-prim-tab-entry-coersion-function val)))
                           (unless (and (string= r token-regex)
                                        (string= d delimiter-regex)
                                        (equal f coercion-function))
                             (warning
                              production
                              (format
                               NIL "Redefining primitive parser and writer for type ~S style ~S"
                               type style)))))
                       #+NIL
                       (quux-compat:record-source-file `(',type ',style) 'quepasa-regex-primitive)
                       (setf (gethash key regex-prim-tab)
                             (make-instance 'regex-prim-tab-entry
                               :type type
                               :style style
                               :token-regex token-regex
                               :delimiter-regex delimiter-regex
                               :delimiter-literal delimiter-literal
                               :coersion-function coercion-function)))
                     (list :PRIMITIVE type style)))
		  (:CONSTANT
                   production)
                  (:STRING-OFFSET
                   (when (rest production)
                     (fail production (format nil "Unexpected argument to ~S" (first production))))
		   production)
		  (:->
		   (ecase (length (rest production))
		     (1 (destructuring-bind (name) (rest production)
			  (unless (symbolp name) (fail production))))
		     (2 (destructuring-bind (grammar-id name) (rest production)
			  (unless (and (keywordp grammar-id)
				       (symbolp name))
			    (fail production)))))
		   production)
		  (otherwise
		   (fail production (format nil "unexpected clause type ~S" (first production))))
		  )))))
    (check/rewrite production)))

;;---*** This should detect circuity in the grammar and abort rather
;;---*** than running forever.
(defun compact-quepasa-grammar (grammar &key (remove-tags-p t))
  (labels ((sub-compact (grammar prod)
	     (if (stringp prod)
	       prod
	       (let ((op (first prod))
		     (r  (rest  prod)))
		 (ecase op
		   ;; Check/rewrite rewrites (:tag (:key <tag>) <> <>) =>
		   ;; (:tag (:key <tag>) (:sequence <> <>)) (unless there's
		   ;; only a single daughter.
		   (:tag
		    (destructuring-bind ((&key key label) daughter) r
		      (if remove-tags-p
			(sub-compact grammar daughter)
			`(,op ,(when key `(:key ,key :label ,label)) ,(sub-compact grammar daughter)))))
		   ((:never-write :always-write :optional :one-or-more
		     :match-minimally :match-maximally :require :could-parse)
                    (if (rest r)
                      (cons op (cons :sequence (mapcar #'(lambda (sp)
                                                           (sub-compact grammar sp))
                                                       r)))
                      (cons op (list (sub-compact grammar (first r))))))
		   ;; Check/rewrite rewrites (:zero-or-more <>) => (:optional (:one-or-more <>))
		   ;; and replaces any singleton usage with the daughter.
		   ((:or :sequence)
		    (cons op (mapcar #'(lambda (sp) (sub-compact grammar sp)) r)))
		   ((:if :if-not)
		    (destructuring-bind (conditional . daughters) r
		      `(,op ,conditional ,@(mapcar #'(lambda (d)
						       (apply #'sub-compact (list grammar d)))
						   daughters))))
		   (:separated-list
		    (destructuring-bind (daughter separator) r
		      `(,op ,(sub-compact grammar daughter) ,separator)))
		   ;; Check/rewrite rewrites (:object foo <> <>) => (:object foo (:sequence <> <>))
		   (:object
		    (destructuring-bind (obj-name daughter constructor) r
                      `(,op ,obj-name ,(when daughter (sub-compact grammar daughter))
                            ,constructor)))
		   ((:slot :list-slot)
                    (destructuring-bind (slot-name daughter reader writer) r
                      `(,op ,slot-name ,(sub-compact grammar daughter) ,reader ,writer)))
		   (:primitive
		    (destructuring-bind (type style) r
		      `(,op ,type ,style)))
		   (:constant
		    `(,op ,(first r)))
                   (:string-offset
                    `(,op))
		   (:->
                      (let* ((subgrammar (ecase (length r)
                                           (1 grammar)
                                           (2 (find-quepasa-grammar! (first r)))))
                             (production-name (ecase (length r)
                                                (1 (first r))
                                                (2 (second r))))
                             (target-production (gethash production-name
                                                         (slot-value subgrammar 'named-productions))))
                        (assert target-production (target-production)
                                "Couldn't find production ~S in grammar ~S"
                                production-name
                                (quepasa-grammar-id subgrammar))

                        (sub-compact subgrammar target-production))))))))
    (declare (dynamic-extent #'sub-compact))
    (let* ((root-name (slot-value grammar 'root))
	   (root-prod (gethash root-name (slot-value grammar 'named-productions))))
      (if root-prod
	(setf (slot-value grammar 'compacted-production)
	      (sub-compact grammar root-prod))
	grammar))))

(defun add-quepasa-grammar-production (grammar name production)
  (setf (gethash name (quepasa-grammar-named-productions grammar))
	(quepasa-grammar-check/rewrite-production-syntax
         name production grammar))
  grammar)

(defmacro def-quepasa-grammar-production (name (&key grammar)
					  production &rest productions)
  (with-rebinding (grammar)
    `(progn
       #+NIL
       (quux-compat:record-source-file (list (quepasa-grammar-id ,grammar) ',name)
                                       'quepasa-grammar-production)
       (add-quepasa-grammar-production
        ,grammar ',name ',(list* :sequence production productions))
       ',name)))

(defmacro def-quepasa-grammar-root-production ((&key grammar)
                                               production &rest productions)
  (with-rebinding (grammar)
    `(progn
       #+NIL
       (quux-compat:record-source-file (quepasa-grammar-id ,grammar)
                                       'quepasa-grammar)
       (add-quepasa-grammar-production
        ,grammar 'ROOT ',(list* :sequence production productions))
       'ROOT)))

(defmacro def-quepasa-singleton-grammar (grammar-id production)
  "Register GRAMMAR-ID as a grammar with the single PRODUCTION."
  `(progn
     #+NIL
     (quux-compat:record-source-file ',grammar-id 'quepasa-grammar)
     (register-quepasa-grammar :grammar
       (add-quepasa-grammar-production
           (make-instance 'quepasa-grammar :id ,grammar-id)
         'ROOT
         ',production))))

(defun unregister-quepasa-grammar (&key grammar)
  (assert (slot-value grammar 'id))
  (setf *quepasa-grammars*
	(delete (slot-value grammar 'id)
		*quepasa-grammars*
		:key #'(lambda (g) (slot-value g 'id)))))

(defun register-quepasa-grammar (&key grammar (remove-tags-p t))
  (assert (slot-value grammar 'id))
  (unregister-quepasa-grammar :grammar grammar)
  (compact-quepasa-grammar grammar :remove-tags-p remove-tags-p)
  (push grammar *quepasa-grammars*))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun canonicalize-tag-key (key)
  ;; the key is a path through the tag tree, e.g. (:PNR :PAYMENT)
  (etypecase key
    (null nil)
    (keyword (list key))
    (cons key)))
)

(defun rt-tag-tuple-tags (tag-tuple)
  (third tag-tuple))

(defun rt-tag-key (tag)
  (destructuring-bind (&key key &allow-other-keys) tag
    (canonicalize-tag-key key)))

(defun rt-tag-tuple-key (tag-tuple)
  (rt-tag-key (rt-tag-tuple-tags tag-tuple)))

(defstruct quepasa-grammar-unit-test-case
  (bindings '() :type list)
  (input-sentence "" :type string)
  (expected-result '() :type (or keyword null))
  (expected-tags '() :type list)
  (output-sentence/s '() :type (or string list)))

(defun clear-quepasa-grammar-unit-tests (grammar-id)
  (setf (slot-value (find-quepasa-grammar! grammar-id) 'unit-test-cases) nil))

;; DLD|  I don't know where the real implementation of this went.  Just use this for now.
(defun run-quepasa-grammar-unit-tests (grammar-id)
  (loop with grammar = (find-quepasa-grammar! grammar-id)
        with num-passes = 0
        with num-fails = 0
        for case in (quepasa-grammar-unit-test-cases grammar)
        as test-input-sentence = (quepasa-grammar-unit-test-case-input-sentence case)
        as test-output-sentence/s = (quepasa-grammar-unit-test-case-output-sentence/s
                                     case)
        as exp-res = (quepasa-grammar-unit-test-case-expected-result case)
        as res = (parse-quepasa-grammar-string test-input-sentence grammar)
        as res-objs = (mapcar #'third (parse-result-obj-holders res))
        as res-err = (parse-result-error-string res)
        doing
    (ecase exp-res
      (:PASS
       (cond
         (res-err
          (incf num-fails)
          (format T "FAIL parsing ~S, ~S~%" test-input-sentence res))
         (T
          (cond
            (test-output-sentence/s
             (let ((out/s (unparse-quepasa-objects res-objs grammar)))
               (cond
                 ((string= (FIRST test-output-sentence/s) out/s)
                  (incf num-passes))
                 (T
                  (incf num-fails)
                  (format T "FAILED wrong unparse, got: ~S, expected: ~S~%"
                          out/s (FIRST test-output-sentence/s))))))
            (T (incf num-passes))))))
      (:FAIL
       (cond
         (res-err
          (incf num-passes))
         (T
          (incf num-fails)
          (format T "FAILED to fail parsing ~S, ~S~%" test-input-sentence res)))))
    finally
        (format T "~S tests, passed, ~S failed.~%" num-passes num-fails)))

(defmacro def-quepasa-grammar-unit-test-case (grammar-id (&rest bindings)
					      input-sentence output-sentence/s
					      expected-result &optional expected-tags)
    "For each list in BINDINGS dynamically bind the given variables (or
skip the binding and 'iterate' once if BIDNINGS is nil).  Parse INPUT.
Compare the parser output to EXPECTED-RESULT and, if supplied, EXPECTED-KEYS.
Unparse the parser output.  Compare the unparser output to OUTPUT-SENTENCE/S.

OUTPUT-SENTENCE/S must be given.  It can either be a string or a list of
strings.  If it's a list of strings its length must be equal to the
length of BINDINGS and each string is compared in order to the output
given by the unparser for each bind/parse/unparse iteration, otherwise
the single string is compared in each iteration.  When a list is given
it's an error if the lengths of BINDINGS and OUTPUT-SENTENCE/S are not
the same.

EXPECTED-RESULT must be given as a keyword value.  Legal keyword values
are :PASS, or :FAIL.

If EXPECTED-TAGS is given it must be a list of tags to be matched against
the tags returned from each bind/parse iteration.  When properly placed in
the grammar the tags imply a derivation of the input sentance."
    ;;---*** Check arg lengths, reporting any errors.
    `(push (make-quepasa-grammar-unit-test-case
	    :bindings ',bindings
	    :input-sentence ',input-sentence
	    :output-sentence/s ',output-sentence/s
	    :expected-result ',expected-result
	    :expected-tags ',(mapcar #'canonicalize-tag-key expected-tags))
	   (slot-value (find-quepasa-grammar! ,grammar-id) 'unit-test-cases)))

(defun collect-quepasa-grammar-object-types (grammar)
  (let* ((root-name (slot-value grammar 'root))
         (root-prod (gethash root-name (slot-value grammar 'named-productions)))
         (types nil))
    (labels ((walk (grammar prod)
               (if (stringp prod)
                   prod
                   (ecase (first prod)
                     ((:primitive :constant :string-offset)
                      t)
                     (:tag
                      (walk grammar (third prod)))
                     ((:never-write :always-write :separated-list :require :could-parse
                       :match-minimally :match-maximally)
                      (walk grammar (second prod)))
                     ((:or :sequence)
                      (mapc #'(lambda (p)
                                (walk grammar p))
                            (rest prod)))
                     ((:if :if-not)
                      (mapc #'(lambda (p)
                                (walk grammar p))
                            (cddr prod)))
                     (:->
                      (ecase (length prod)
                        (2 (walk grammar
                                 (gethash (second prod)
                                          (slot-value grammar 'named-productions))))
                        (3 (let ((grammar (find-quepasa-grammar! (second prod))))
                             (walk grammar
                                   (gethash (third prod)
                                            (slot-value grammar 'named-productions)))))))
                     ((:optional :one-or-more)
                      (walk grammar (second prod)))
                     (:object
                      (let ((obj-type (second prod)))
                        (pushnew obj-type types)
                        (walk grammar (third prod))))
                     ((:slot :list-slot)
                      (walk grammar (third prod)))))))
      (when root-prod
        (walk grammar root-prod)))
    types))

(defun print-quepasa-grammar-object-hierarchy (grammar
					       &key (stream *standard-output*)
					            debug-p)
  (let ((obj-tab (make-hash-table :test #'eq)))
    (labels ((next-value (grammar prod)
	       (if (stringp prod)
		 NIL
		 (let ((op (first prod)))
		   (ecase op
		     ((:constant :object)
		      (list op (second prod)))
                     (:string-offset
                      (list op))
		     (:primitive
		      (list op (list (second prod) (third prod))))
		     (:tag
		      (next-value grammar (third prod)))
		     ((:never-write :always-write :separated-list
                       :match-minimally :match-maximally
		       :optional :one-or-more :require :could-parse)
		      (next-value grammar (second prod)))
		     ((:or :sequence)
		      (apply #'append
			     (mapcar #'(lambda (p)
					 (next-value grammar p))
				     (rest prod))))
		     ((:if :if-not)
		      (apply #'append
			     (mapcar #'(lambda (p)
					 (next-value grammar p))
				     (cddr prod))))
		     (:->
		      (ecase (length prod)
			(2 (next-value grammar
				       (gethash (second prod)
						(slot-value grammar 'named-productions))))
			(3 (let ((grammar (find-quepasa-grammar! (second prod))))
			     (next-value grammar
					 (gethash (third prod)
						  (slot-value grammar 'named-productions)))))))
		     ((:slot :list-slot)
		      (next-value grammar (third prod)))))))
	     ;;
	     ;; OBJ-TAB maps <obj-type> => (<slot-table> (<constraint-objs>))
	     ;;
	     ;; <slot-table> is described below.  Each element of <constraint-objs>
	     ;; takes the form:
	     ;;
	     ;;   (<constraint> <parent-obj-types>)
	     ;;
	     ;; Where <constraint> is :optional, :one-or-more or :exactly-one.
	     ;; <parent-obj-types> is a stack of the parent objects of <obj-type>,
	     ;; eg, if <obj-type> is 'inner-obj' <parent-obj-types> might be
	     ;; '(:middle-obj :outer-obj)'.
	     ;;
	     ;; <slot-table> maps <slot-name> => (<slot-type> (<value-types>))
	     ;;
	     ;; <slot-type> is :slot (singleton) or :list-slot.  Each element of
	     ;; <value-types> takes the form:
	     ;;
	     ;;    (<constraint> <value-desc>)
	     ;;
	     ;; where <constraint> is the same as described above and <value-desc>
	     ;; takes the form:
	     ;;
	     ;;    (<value-type> <value-info>)
	     ;;
	     ;; where <value-type> is either :constant, primitive or :object and
	     ;; <value-info> is a literal value when <value-type> is :constant,
	     ;; a list (<primitive-type> <primitive-style>) when <value-type> is
	     ;; :primitive or the object type when <value-type> is :object.
	     ;;
	     (hash-object-hierarchy (grammar prod constraint objs)
	       (if (stringp prod)
		 prod
		 (ecase (first prod)
		   ((:primitive :constant :string-offset)
		    t)
		   (:tag
		    (hash-object-hierarchy grammar (third prod) constraint objs))
		   ((:never-write :always-write :separated-list :require :could-parse
                     :match-minimally :match-maximally)
		    (hash-object-hierarchy grammar (second prod) constraint objs))
		   ((:or :sequence)
		    (mapc #'(lambda (p)
			      (hash-object-hierarchy grammar p constraint objs))
			  (rest prod)))
		   ((:if :if-not)
		    (mapc #'(lambda (p)
			      (hash-object-hierarchy grammar p constraint objs))
			  (cddr prod)))
		   (:->
		    (ecase (length prod)
		      (2 (hash-object-hierarchy grammar
						(gethash (second prod)
							 (slot-value grammar 'named-productions))
						constraint
						objs))
		      (3 (let ((grammar (find-quepasa-grammar! (second prod))))
			   (hash-object-hierarchy grammar
						  (gethash (third prod)
							   (slot-value grammar 'named-productions))
						  constraint
						  objs)))))
		   ((:optional :one-or-more)
		    (hash-object-hierarchy grammar (second prod) (or constraint (first prod)) objs))
		   (:object
		    (let* ((obj-type        (second prod))
			   (obj-entry       (gethash obj-type obj-tab))
			   (constraint-objs (second obj-entry)))
		      (if obj-entry
			(unless (member (cons (or constraint :exactly-one) objs)
					constraint-objs :test #'equal)
			  (setf (second obj-entry)
				(cons
				 (cons (or constraint :exactly-one) (copy-list objs))
				 constraint-objs)))
			(setf (gethash obj-type obj-tab)
			      (list (make-hash-table :test #'eq)
				    (list (cons (or constraint :exactly-one) (copy-list objs))))))
		      (hash-object-hierarchy grammar (third prod) nil (cons obj-type objs))))
		   ((:slot :list-slot)
		    (let* ((slot-type  (first prod))
			   (slot-name* (second prod))
			   (targ-slot? (not (symbolp slot-name*)))
			   (slot-targ  (when targ-slot?
					 (destructuring-bind (&key object-type) (rest slot-name*)
					   object-type)))
			   (slot-name  (if targ-slot? (first slot-name*) slot-name*))
			   (targ-obj   (if targ-slot?
					 (find slot-targ objs
					       :test #'(lambda (a b) (subtypep b a)))
					 (first objs)))
			   (slot-tab   (first (gethash targ-obj obj-tab)))
			   (slot-entry (gethash slot-name slot-tab))
			   (nv         (next-value grammar (third prod))))
		      (if slot-entry
			(let ((value-types (second slot-entry)))
			  (unless (member (cons (or constraint :exactly-one) (list nv))
					  value-types :test #'equal)
			    (setf (second slot-entry)
				  (cons
				   (list (or constraint :exactly-one) nv)
				   value-types))))
			(setf (gethash slot-name slot-tab)
			      (list slot-type
				    (list (list (or constraint :exactly-one) nv)))))
		      (hash-object-hierarchy grammar (third prod) nil objs)))))))
      (declare (dynamic-extent #'hash-object-hierarchy #'next-value))
      (let* ((root-name (slot-value grammar 'root))
	     (root-prod (gethash root-name (slot-value grammar 'named-productions))))
	(when root-prod
	  (hash-object-hierarchy grammar root-prod nil nil)
	  (when debug-p
	    (maphash #'(lambda (k v)
			 (debugf "~S ==> ~S~%" k v)
			 (maphash #'(lambda (k v)
				      (debugf "  ~S => ~S~%" k v))
				  (first v)))
		     obj-tab))
	  (maphash #'(lambda (k v)
		       (let ((obj-type k)
			     (slot-tab (first v))
			     (constraint-objs (second v)))
			 (declare (ignore constraint-objs))
			 (format stream "~A~%" obj-type)
			 (maphash #'(lambda (k v)
				      (let* ((slot-name   k)
					     (slot-type   (first v))
					     (list-slot?  (eq slot-type :list-slot))
					     (value-types (second v)))
					(format stream "  ~A slot:~%" slot-name)
					(format
					 stream "    ~:[~;List of ~]~A~@[ elements~]~%"
					 list-slot?
					 (let ((text
						(labels ((value-type-description (vt)
							   (let* ((constraint   (first vt))
								  (desc         (second vt))
								  (type         (first desc))
								  (info         (second desc))
								  (literal/name (if (eq type
											:primitive)
										  (first info)
										  info))
								  (style        (and
										 (eq type
										     :primitive)
										 (second info))))
							     (format nil
								     "~A~:[~*~;~S style ~]~S ~A"
								     (ecase constraint
								       (:optional
									(if list-slot?
									  "zero or more "
									  "optional "))
								       (:one-or-more
									"one or more ")
								       (:exactly-one
									"exactly one "))
								     style style
								     literal/name
								     (string-downcase type)))))
						  (declare (dynamic-extent
							    #'value-type-description))
						  (let ((l (length value-types)))
						    (cond
						      ((eql l 1)
						       (value-type-description
							(first value-types)))
						      ((eql l 2)
						       (format nil "~A or ~A"
							       (value-type-description
								(first value-types))
							       (value-type-description
								(second value-types))))
						      (t
						       (format nil "~A~{, ~A~} or ~A"
							       (value-type-description
								(first value-types))
							       (mapcar #'value-type-description
								       (subseq value-types
									       1 (1- l)))
							       (value-type-description
								(car (last value-types))))))))))
					   (if list-slot?
					     text
					     (string-capitalize
					      text :start 0 :end (position #\Space text))))
					 list-slot?)))
				  slot-tab))
		       (format stream "~%"))
		   obj-tab))))))

(defun summarize-prod (prod &optional (max-depth 2) (level 0))
  ;;
  ;; Make debugging output more readable.
  ;;
  (when prod
    (if (< level max-depth)
      (let ((op (typecase prod
		  (string :string)
		  (cons (car prod))
		  (t (error "unexpected prod format: ~S" prod)))))
	(ecase op
	  (:->
	   (ecase (length prod)
	     (2 (format nil "(-> ~A)" (second prod)))
	     (3 (format nil "(-> ~A ~A)" (second prod) (third prod)))))
	  (:TAG
	   (format nil "(TAG ~A ~A)"
		   (second prod)
		   (summarize-prod (third prod) max-depth (1+ level))))
          ((:REQUIRE :COULD-PARSE)
           (format nil "(~A ~A)"
                   op
                   (summarize-prod (second prod) max-depth (1+ level))))
	  (:NEVER-WRITE
	   (format nil "(NOGEN ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))))
	  (:ALWAYS-WRITE
	   (format nil "(GEN ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))))
	  (:MATCH-MINIMALLY
	   (format nil "(MIN ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))))
	  (:MATCH-MAXIMALLY
	   (format nil "(MAX ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))))
	  (:OPTIONAL
	   (format nil "(OPT ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))))
	  (:OR
	   (format nil "(OR~{ ~A~})"
		   (loop for sub-prod in (rest prod)
			 collecting
			 (summarize-prod sub-prod max-depth (1+ level)))))
	  (:ONE-OR-MORE
	   (format nil "(1+~{ ~A~})"
		   (loop for sub-prod in (rest prod)
			 collecting
			 (summarize-prod sub-prod max-depth (1+ level)))))
	  (:SEPARATED-LIST
	   (format nil "(SLIST ~A ~A)"
		   (summarize-prod (second prod) max-depth (1+ level))
		   (summarize-prod (third prod) max-depth (1+ level))))
	  (:SEQUENCE
	   (format nil "(SEQ~{ ~A~})"
		   (loop for sub-prod in (rest prod)
			 collecting
			 (summarize-prod sub-prod max-depth (1+ level)))))
	  ((:IF :IF-NOT)
	   (format nil "(~A ~A ~{ ~A~})"
		   op (second prod)
		   (loop for sub-prod in (cddr prod)
			 collecting
			 (summarize-prod sub-prod max-depth (1+ level)))))
	  (:CONSTANT
	   (format nil "(CONST ~S)" (second prod)))
          (:STRING-OFFSET
           (format nil "(STRING-OFFSET)"))
	  (:STRING
	   (format nil "~S" prod))
	  (:PRIMITIVE
	   (format nil "(PRIM ~A ~S)" (second prod) (third prod)))
	  (:REGEX-PRIMITIVE
	   (format nil "(REGEX-PRIM ~A ~S ~S)" (second prod) (third prod) (fourth prod)))
	  (:OBJECT
	   (format nil "(OBJ ~A ~A)"
		   (second prod)
		   (summarize-prod (third prod) max-depth (1+ level))))
	  (:SLOT
	   (format nil "(SLOT ~A ~A)"
		   (second prod)
		   (summarize-prod (third prod) max-depth (1+ level))))
	  (:LIST-SLOT
	   (format nil "(LSLOT ~A ~A)"
		   (second prod)
		   (summarize-prod (third prod) max-depth (1+ level))))
	  (:END-OF-INPUT
	   (format nil "(EOI)"))))
      "..")))

(defun summarize-entry (entry &optional (max-depth 2) (level 0))
  ;;
  ;; Make debugging output more readable.
  ;;
  (when entry
    (if (< level max-depth)
      (let ((op (typecase entry
		  (string :string)
		  (cons (car entry))
		  (t (return-from summarize-entry (format nil "~A" entry))))))
	(ecase op
	  (:TAG
	   (format nil "(TAG ~A ~A)"
		   (second entry)
		   (summarize-entry (third entry) max-depth (1+ level))))
          ((:REQUIRE :COULD-PARSE)
           (format nil "(~A ~A)"
                   op
                   (summarize-entry (second entry) max-depth (1+ level))))
	  (:NEVER-WRITE
	   (format nil "(NOGEN ~A)"
		   (summarize-entry (second entry) max-depth (1+ level))))
	  (:ALWAYS-WRITE
	   (format nil "(GEN ~A)"
		   (summarize-entry (second entry) max-depth (1+ level))))
	  (:OBJECT*
	   (format nil "(OBJ* ~A ~A)"
		   (second entry)
		   (summarize-entry (third entry) max-depth (1+ level))))
	  (:SLOT
	   (format nil "(SLOT ~A ~A ~A)"
		   (second entry)
		   (third entry)
		   (summarize-entry (fourth entry) max-depth (1+ level))))
	  (:LIST-SLOT
	   (format nil "(LSLOT ~A ~A ~A)"
		   (second entry)
		   (third entry)
		   (summarize-entry (fourth entry) max-depth (1+ level))))
	  (:SEQUENCE
	   (format nil "(SEQ~{ ~A~})"
		   (loop for sub-entry in (rest entry)
			 collecting
			 (summarize-entry sub-entry max-depth (1+ level)))))
	  ((:IF :IF-NOT)
	   (format nil "(~A ~A ~{ ~A~})"
		   op (second entry)
		   (loop for sub-entry in (cddr entry)
			 collecting
			 (summarize-entry sub-entry max-depth (1+ level)))))
	  (:VALUE
	   (format nil "(VAL ~A ~S)" (second entry) (third entry)))))
      "..")))

(defun summarize-value (value &optional (max-depth 2) (level 0))
  ;;
  ;; Make debugging output more readable.
  ;;
  (when value
    (if (< level max-depth)
      (let ((op (typecase value
		  (string :string)
		  (cons (car value))
		  (t (return-from summarize-value (format nil "~A" value))))))
	(ecase op
	  (:TAG
	   (format nil "(TAG ~A ~A)"
		   (second value)
		   (summarize-value (third value) max-depth (1+ level))))
          ((:REQUIRE :COULD-PARSE)
           (format nil "(~A ~A)"
                   op
                   (summarize-value (rest value) max-depth (1+ level))))
	  (:OBJECT*
	   (format nil "(OBJ* ~A ~A)"
		   (second value)
		   (summarize-value (third value) max-depth (1+ level))))
	  (:OBJECT
	   (format nil "(OBJ ~S ~S)" (second value) (third value)))
	  (:SLOT
	   (format nil "(SLOT ~A ~A ~A)"
		   (second value)
		   (third value)
		   (summarize-value (fourth value) max-depth (1+ level))))
	  (:LIST-SLOT
	   (format nil "(LSLOT ~A ~A ~A)"
		   (second value)
		   (third value)
		   (summarize-value (fourth value) max-depth (1+ level))))
	  (:SEQUENCE
	   (format nil "(SEQ~{ ~A~})"
		   (loop for sub-value in (rest value)
			 collecting
			 (summarize-value sub-value max-depth (1+ level)))))
	  (:VALUE
	   (format nil "(VAL ~A ~S)" (second value) (third value)))
	  (:PAIR
	   (format nil "(PAIR ~A ~A)"
		   (summarize-value (second value) max-depth (1+ level))
		   (summarize-value (third value) max-depth (1+ level))))))
      "..")))

(defun find-target-object-holder (slot-target obj-stack)
  (if slot-target
    (destructuring-bind (&key object-type) slot-target
      (find-if (lambda (oh)
		 (if (listp oh)
		   ;; When parsing the obj-stack takes the form:
		   ;; ((:OBJECT <class-name> <class-instance>) ...)
		   (and (eq (first oh) :OBJECT)
			(subtypep (second oh) object-type))
		   ;; When unparsing the obj-stack takes the form:
		   ;; (<class-instance> ...)
		   (subtypep (type-of oh) object-type)))
	       obj-stack))
    (first obj-stack)))

#||
qrslaunch
(start-qres "swm-ars-regress" "swm-ars-regress")

(time (QTEST:RUN-TEST 'QRES-ARS-TEST::TEST-PNL/ADL-GENERATION))

(time (QTEST:RUN-TEST 'QRES-ARS-TEST::TEST-AIRIMP-PARSE-ERRORS))

(time (PARSE-QUEPASA-GRAMMAR-STRING "YYZRMAC
.BOSRM1U 040000/DKR9M73A6U5K
BOS1URVJXVD/ITA/5346955/BOS/1U/T/US/USD/SU
1YARLIN/YINGY
1MALCON/MALCOM
AC9541J30JANAKLSYDNN2/07000830
AC0034J30JANSYDYVRNN2/09300725
OSI YY CTCB BOS 617-555-3333
" (find-quepasa-grammar! :airimp)))
||#

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct quepasa-entry
  ;; string index where parse began in this iteration
  (start    0 :type fixnum)
  ;; string index where parse attempt ended (for error reporting only)
  (end      0 :type fixnum)
  ;; parsed value.
  ;; VALUE and ERROR should be mutually exclusive.
  value
  ;; errors encounted during parse.
  error
  ;; whether the current production is inside of a :REQUIRE block
  required)

(defstruct quepasa-iterator
  (production nil :type (or string list quepasa-entry))
  #+debug-quepasa-alloc (allocated nil :type boolean)
  (start 0 :type fixnum)
  (subiter1 nil :type (or quepasa-iterator null))
  (subiter2 nil :type (or quepasa-iterator null))
  (entry1 nil :type (or quepasa-entry null))
  ;;--- 'entries' is a general-purpose list slot.
  (entries nil :type list)
  ;;--- In a sane world we could infer these grammars from 'production'.
  (active-grammar nil :type (or quepasa-grammar null))
  (local-grammar nil :type (or quepasa-grammar null))
  ;; These are only necessary if you care about returning partial successes.
  (token-start most-negative-fixnum :type fixnum)
  (stack nil :type list)
  ;; Slots for error reordering and duplicate-length-parse culling.
  (required-errors nil :type list)
  (optional-errors nil :type list)
  (previously-returned nil :type list))

) ;end of eval-when

#+NIL
(qmp:define-thread-variable *quepasa-entry-resource* :initial-form
  (make-array 0 :element-type 'quepasa-entry :adjustable t :fill-pointer 0))
(defvar *quepasa-entry-resource*
  (make-array 0 :element-type 'quepasa-entry :adjustable t :fill-pointer 0))
#+NIL
(qmp:define-thread-variable *quepasa-iterator-resource* :initial-form
  (make-array 0 :element-type 'quepasa-iterator :adjustable t :fill-pointer 0))
(defvar *quepasa-iterator-resource*
  (make-array 0 :element-type 'quepasa-iterator :adjustable t :fill-pointer 0))
#+debug-quepasa-alloc
(defun reset-iterator-resource-allocation-bits ()
  (loop for res across *quepasa-iterator-resource*
        do (setf (quepasa-iterator-allocated res) nil)))

;; These keep track of how many structs in *quepasa-...-resource* are currently allocated.
;; The only real reason not to use native fill-pointers for this is that these variables
;; are rebound so that structures are automatically freed at the end of the dynamic scope.
#+NIL
(qmp:define-thread-variable *quepasa-entry-resource-pointer* :initial-form 0)
(defvar *quepasa-entry-resource-pointer* 0)
#+NIL
(qmp:define-thread-variable *quepasa-iterator-resource-pointer* :initial-form 0)
(defvar *quepasa-iterator-resource-pointer* 0)

(defun allocate-quepasa-entry ()
  (if (i= (fill-pointer *quepasa-entry-resource*) *quepasa-entry-resource-pointer*)
    (let ((new-object (make-quepasa-entry)))
      (incf *quepasa-entry-resource-pointer*)
      (vector-push-extend new-object *quepasa-entry-resource*)
      new-object)
    (let ((object (aref *quepasa-entry-resource* *quepasa-entry-resource-pointer*)))
      (incf *quepasa-entry-resource-pointer*)
      object)))

;; Don't deallocate anything.  It's faster to clean up once for all afterwards,
;; if we don't mind the extra memory consumption.
(defun deallocate-quepasa-entry (entry)
  (declare (ignore entry)))

(defun allocate-quepasa-iterator (&key production (start 0) subiter1 subiter2 entry1 entries
                                  active-grammar local-grammar
                                  (token-start most-negative-fixnum) stack
                                  required-errors optional-errors previously-returned)
  (assert (i>= 1000000                                  ; check that we aren't holding onto insane amounts of memory
               (fill-pointer *quepasa-iterator-resource*) ; and that we haven't screwed up allocation count
               *quepasa-iterator-resource-pointer*))
  (if (i= (fill-pointer *quepasa-iterator-resource*) *quepasa-iterator-resource-pointer*)
    (let ((new-object
           (make-quepasa-iterator :production production
                                  #+debug-quepasa-alloc :allocated #+debug-quepasa-alloc t
                                  :start start :subiter1 subiter1
                                  :subiter2 subiter2 :entry1 entry1 :entries entries
                                  :active-grammar active-grammar :local-grammar local-grammar
                                  :token-start token-start :stack stack
                                  :required-errors required-errors :optional-errors optional-errors
                                                         :previously-returned previously-returned)))
      #+debug-quepasa-alloc (debugf "+   alloc ~A" *quepasa-iterator-resource-pointer*)
      (incf *quepasa-iterator-resource-pointer*)
      (vector-push-extend new-object *quepasa-iterator-resource*)
      new-object)
    (let ((object (aref *quepasa-iterator-resource* *quepasa-iterator-resource-pointer*)))
      (declare (type quepasa-iterator object))
       #+debug-quepasa-alloc
       (progn
         (debugf "+ realloc ~A" *quepasa-iterator-resource-pointer*)
         (assert (not (quepasa-iterator-allocated object))))

      (incf *quepasa-iterator-resource-pointer*)
      (setf
       (quepasa-iterator-production object) production
       #+debug-quepasa-alloc (quepasa-iterator-allocated object) #+debug-quepasa-alloc t
       (quepasa-iterator-start object) start
       (quepasa-iterator-subiter1 object) subiter1
       (quepasa-iterator-subiter2 object) subiter2
       (quepasa-iterator-entry1 object) entry1
       (quepasa-iterator-entries object) entries
       (quepasa-iterator-active-grammar object) active-grammar
       (quepasa-iterator-local-grammar object) local-grammar
       (quepasa-iterator-token-start object) token-start
       (quepasa-iterator-stack object) stack
       (quepasa-iterator-required-errors object) required-errors
       (quepasa-iterator-optional-errors object) optional-errors
       (quepasa-iterator-previously-returned object) previously-returned)
      object)))

(defun deallocate-quepasa-iterator (object)
  #+debug-quepasa-alloc (declare (type quepasa-iterator object))
  #-debug-quepasa-alloc (declare (ignore object))

  #+debug-quepasa-alloc
  (progn
    ; verify that the the resource being deallocated
    ; was the last one allocated
    (debugf "- dealloc ~A" (i1- *quepasa-iterator-resource-pointer*))
    (assert (quepasa-iterator-allocated object))
    (assert (eq (aref *quepasa-iterator-resource*
                      (i1- *quepasa-iterator-resource-pointer*))
                object))
    (setf (quepasa-iterator-allocated object) nil))

  (decf *quepasa-iterator-resource-pointer*))

(defvar *quepasa-debug-depth* 0)

(defstruct parse-result
  (obj-holders nil :type list) ;; NIL or a result.  a result is a list of quepasa "obj-holders"
  ;; a quepasa "obj-holder" is a list of the form (:OBJECT CLASS-NAME PARSED-REPRESENTATION)
  ;; where :OBJECT is literally the keyword :OBJECT;
  ;; CLASS-NAME is the symbol name of a class; and
  ;; PARSED-REPRESENTATION is an object of type CLASS-NAME
  (error-string nil :type (or null string)) ;; NIL or an error string
  (error-start-position nil :type (or null integer)) ;; a start error position in the source string
  (error-end-position nil :type (or null integer)) ;; an end error position in the source string
  (tags nil :type list) ;; an ordered list of tags
  (alt-results nil)) ;; list of results that are alternatives to 1

(defun parse-result-obj-holder (parse-result)
  "Many quepasa grammars typically parse exactly one object.  This is a convenience function for
   getting that one object.  Asserts that at most object was parsed."
  (check-type parse-result parse-result)
  (onliest (parse-result-obj-holders parse-result)))

(defun obj-holder-parsed-representation (obj-holder)
  "Given a list of the sort parse-quepasa-grammar-string stores a list of in
   parse-result-obj-holders, returns the parsed representation.  Asserts that the obj-holder does in
   fact hold an :OBJECT."
  (assert (eq (first obj-holder) :OBJECT))
  (third obj-holder))

(defun parse-result-object-parsed-representation (parse-result)
  "Many quepasa grammars typically parse exactly one object.  This is a convenience function for
   getting that one object's parsed representations.  Asserts that at most object was parsed."
  (obj-holder-parsed-representation (parse-result-obj-holder parse-result)))

(defun collapse-top-level (result)
  "Helper function for PARSE-QUEPASA-GRAMMAR-STRING."
  (if (eq result t)
    nil
    (if (eq (first result) :PAIR)
      (append (collapse-top-level (second result))
              (collapse-top-level (third result)))
      (list result))))

(defun create-error-message (error-tree &optional (depth 0))
  "Helper function for PARSE-QUEPASA-GRAMMAR-STRING."
  (when (i> depth *quepasa-parse-error-max-depth*)
    (return-from create-error-message ""))

  (if (use-compacted-productions)
    "Invalid input"
    (with-output-to-string (s)
      (cond
        ((null error-tree)
         (format s "Invalid input"))
        ((not (listp error-tree))
         (format s "~A" error-tree))
        ((i> (length error-tree) 1)
         (unless (plusp depth)
           (format s "Expected "))
         (loop for (name . subchildren) in error-tree
               for i from 1 to (length error-tree)
               do (progn
                    (format s "~A" name)
                    (when (and subchildren
                               (i<= depth *quepasa-parse-error-max-branch-depth*))
                      (format s " ~A~A~A"
                              (if (iplusp depth) "" "(")
                              (create-error-message subchildren (i1+ depth))
                              (if (iplusp depth) "" ")")))
                    (cond
                      ((i= i (i1- (length error-tree)))
                       (if (i= (length error-tree) 2)
                         (format s " or ")
                         (format s ", or ")))
                      ((i< i (length error-tree))
                       (format s ", "))))))
        (error-tree
         (let* ((child (first error-tree))
                (name (car child))
                (subchildren (cdr child)))
           (if (plusp depth)
             (format s "~A" name)
             (format s "Invalid: ~A" name))
           (when (and subchildren
                      (i<= depth *quepasa-parse-error-max-depth*))
             (format s ".~A"
                     (create-error-message subchildren (i1+ depth))))))))))

(defun parse-quepasa-grammar-string (string grammar
				     &key (start 0) end debug existing-objects
                                     #+NIL ;; DLD|
                                     (timeout
                                      (qconfig:env-config-property "quepasa.timeout")))
  ;;
  ;; Parser for "simple" grammars.  Takes a string and a grammar and returns either error
  ;; information or a list of objects created by the parsing process.
  ;;
  ;; The underlying implementation is a pretty straightforward top-down left-to-right
  ;; context-free parser.
  ;;
  ;; TIMEOUT -- specifies parsing timeout in milliseconds. If parsing exceeds this value, 
  ;; (VALUE NIL "parsing timed out" START END) is returned. By default a timeout of 2 seconds 
  ;; is used.
  ;;
  ;; Returns a PARSE-RESULT struct.
  ;;
  ;; If the EXISTING-OBJECTS argument is used it should contain a list of objects ordered
  ;; so that they match the order in which :object operators will be encountered as the
  ;; grammar is processed.  In this case, the parser does not create a new objects but
  ;; instead uses the next object from the existing objects list.  Any top level :slot
  ;; operators will fill in a slot in the current exsting object.  Any nested :object operators
  ;; cause a check for the existence of a valid instance in the slot of a parent object.  If
  ;; there is a valid instance in that slot then that instance will be used, otherwise a new
  ;; instance of the child object will be created.  If the list of existing-objects runs out
  ;; but more top level objects are required by the grammar they will be newly created.
  ;;
  ;; The idea here is to allow for incremental parsing where a first parse produces a sort of
  ;; "base" object and further parses refine the data in a way that sort of resembles subclassing.
  ;; Really, it's growing the frontier of a tree of objects.  This is to allow for user defined
  ;; parsers in the future.  For instance AC wants to be able to add new SSRs/OSIs themselves.
  ;; The details of how flexible this can be and how we're going to provide the functionality
  ;; are not well understood yet.
  ;;
  ;; Note that list slots are re-accumulated if the passes of an incremental parse are not
  ;; parsing disjoint sets of slots.  So, at least in general, each pass of an incremental
  ;; parse should be filling slots that haven't been previously initialized.
  ;;
  ;;
  ;; Each "subparse" of a "production" - a recursive call to process a node in the grammar -
  ;; returns a list of entries.  These entries representing a position in the input that was
  ;; either reached successfully, or one that was not reached successfully and information
  ;; about the error occurred there, as well as any values (e.g. things that will go in the
  ;; objects that result from a parse.)  Entries also contain information about whether and
  ;; how they failed (nil for a successful entry, or a list of T or label names indicating
  ;; the labels that the entry failed inside) and whether it was a 'required' entry.
  ;; (See :REQUIRE below for information about required entries.)
  ;;
  ;; The SUBPARSE label does the core of this processing, and then NON-MEMOIZED-SUBPARSE
  ;; does postprocessing on the list of entries.  NON-MEMOIZED-SUBPARSE, which is typically
  ;; invoked via the macrolet SP, groups the entries according to which position in the
  ;; input the subparse for that entry reached.  For each group, if there was a successful
  ;; subparse, that entry is taken and the remaining ones are discarded (unless we're
  ;; generating verbose error messages -- controlled by USE-COMPACTED-PRODUCTIONS -- in
  ;; which case we keep a few failures around to provide error context if the overall parse
  ;; eventually fails.)  Then entries are sorted in either ascending or descending order of
  ;; position reached, depending on whether or not the parser was told to parse greedily.
  ;;
  ;; Productions have different behavior because they treat the results of their subparses
  ;; differently.  The return value from SP represents all of the possible alternative
  ;; parsings of the production.  :OR, for instance, does a subparse on each of its branches
  ;; and appends the results of the subparse to its own.  This way, its results are all
  ;; the possible parsings of any of its branches.  :OPTIONAL returns whatever parsings
  ;; its child returns, plus an additional parsing indicating that the current position was
  ;; successfully reached.  :ONE-OR-MORE tries parsing once; if that fails, it uses those
  ;; errors as its result list, otherwise, it adds that success to its result list and
  ;; passes itself back in to subparse.

  (unless end (setf end (length string)))
  (assert (member *debug-quepasa-grammar* '(nil t :compacted))
          (*debug-quepasa-grammar*)
          "Valid values for *debug-quepasa-grammar* are NIL, T, and :COMPACTED; current value is ~S"
          *debug-quepasa-grammar*)
  #+debug-quepasa-alloc (reset-iterator-resource-allocation-bits)
  (when (or debug *debug-quepasa-grammar*)
    (setf debug (typecase debug (integer debug) (t 2))))
  (let* ((maximum-error-free-position start)
	 (maximum-error-free-parse-stack ())
	 (maximum-posn-entries-to-retain 5)
         (*quepasa-entry-resource-pointer* *quepasa-entry-resource-pointer*)
         (*quepasa-iterator-resource-pointer* *quepasa-iterator-resource-pointer*)
         (*quepasa-debug-depth* 0)
         #+NIL ;;DLD|
         (timeout-check-frequency 1000)
         #+NIL ;;DLD|
         (subparse-count-since-timeout-check 0)
         ;; deadline expressed in terms of internal time units
         #+NIL ;;DLD|
         (deadline-itu (+ (get-internal-real-time)
                          (* timeout #.(/ internal-time-units-per-second 1000)))))

	 ;;--- Not sure if this is worth the consing or not.
	 #+ignore (string (coerce string 'base-string))

    (macrolet ((sp (prod &key (active-grammar 'active-grammar)
			      (local-grammar 'local-grammar)
			      (start 'start)
			      (sequence-pointer `(when (consp ,prod) (cdr ,prod)))
			      (token-start nil token-start-p))
		 (let ((startv (gensym "start")))
		   `(let ((,startv ,start))
		      #-stack-cons
		      (non-memoized-subparse ,prod ,active-grammar ,local-grammar ,startv
					     ,sequence-pointer ,(if token-start-p token-start startv)
					     stack)
		      #+stack-cons
		      (apply #'non-memoized-subparse ,prod ,active-grammar ,local-grammar ,startv
						     ,sequence-pointer ,(if token-start-p token-start startv)
						     stack)))))

      (labels ((record-maximum-error-free-parse (start stack)
		 (when (i> start maximum-error-free-position)
		   ;; we only want parts of the stack that have consumed some input
		   (let ((consuming-stack (member start stack :key #'first :test #'>)))
		     (when consuming-stack
		       (setf maximum-error-free-position    start
			     maximum-error-free-parse-stack #-stack-cons
							    consuming-stack
							    #+stack-cons
							    (and (not (use-compacted-productions))
								 (copy-list consuming-stack)))))))

               (build-error-label-tree (errors &optional nodes)
                 "ENTRIES is a list of path-lists; a path-list is a list of nodes which constitute
                  a path to build in the tree.  NODES is the list to place created nodes into.
                  The resulting tree structure is a list of nodes whose cars are node labels and
                  whose cdrs are lists of child nodes."
                 (with-collectors ((nodes node :free t))
                   (dolist (error errors)
                     (let* ((error-name (first error))
                            (existing-node (find error-name nodes :key #'car :test #'equal)))
                       (when (stringp error-name)
                         (unless existing-node
                           (setf existing-node (cons error-name '()))
                           (node existing-node))
                         (when (cdr error)
                           (setf (cdr existing-node)
                                 (build-error-label-tree (list (cdr error)) (cdr existing-node)))))))
                   nodes))

               

	       (process-result (result #+stack-cons &rest obj-stack)
		 #+stack-cons (declare (dynamic-extent obj-stack))

		 ;;
		 ;; Instantiate objects and populate their slots.
		 ;;
		 ;; Returns two values, a RESULT (suitable for passing to collapse-top-level) and
		 ;; an ordered list of tag-tuples, each (start end tag).
		 ;;
		 ;; OBJ-STACK is used to hold a hierarchy of "object holders".
		 ;;
		 (when (eq result t)
		   (return-from process-result (values t nil)))
		 (let ((op (first result)))
		   (ecase op
		     (:TAG
		      (destructuring-bind (tag start end sub) (rest result)
			(multiple-value-bind (r tags)
			    #-stack-cons (process-result sub obj-stack)
			    #+stack-cons (apply #'process-result sub obj-stack)
			  (values r (cons (list start end tag) tags)))))
		     (:OBJECT*
		      ;;
		      ;; An :OBJECT sub-production has this form:
		      ;;
		      ;;   (:OBJECT <object-name> <sub-production>)
		      ;;
		      ;; When SUBPARSE sees an :OBJECT operator in the grammar it translates it
		      ;; into a different form.  First it creates a form:
		      ;;
		      ;;   (:OBJECT <object-name> NIL)
		      ;;
		      ;; this form is called an "object holder".  SUBPARSE then goes on to
		      ;; surround the object holder with a form:
		      ;;
		      ;;   (:OBJECT* <object-holder> <parsed-sub-production>)
		      ;;
		      ;; to complete the translation.
		      ;;
		      ;; When we encounter an :OBJECT* operator in this routine if we are
		      ;; doing a non-incremental parse (or the first pass of an incremental
		      ;; parse) we instantiate a new object of type <object-name> (---***
		      ;; yes, this should be called <object-type>).  If we're doing a
		      ;; subsequent pass of an incremental parse we use existing object(s)
		      ;; from a previous pass if they are available.
		      ;;
		      ;; In the case of a non-incremental parse the newly instantiated object
		      ;; replaces the NIL value of the object holder.  The object holder is also
		      ;; pushed onto the object holder stack, called OBJ-STACK (for brevity), so
		      ;; that instance slots can be populated in the course of calling
		      ;; PROCESS-RESULT on <sub-production>.  Rather than merely keeping track of
		      ;; the most recent object holder the object holder stack is maintained so
		      ;; that a slot defined by a parent object may be filled by a :[LIST-]SLOT
		      ;; construct nested within arbitary child objects (see PROCESS-SLOT for
		      ;; details regarding <slot-targets>'s).
		      ;;
		      ;; In the case of an incremental parse the caller specifies the existing
		      ;; top level object(s) using the EXISTING-OBJECTS keyword.  As we
		      ;; encounter top level :OBJECT operators we consume the elements of the
		      ;; existing objects list and use them rather than instantiating new ones.
		      ;; For non top level objects (objects that are values of slots that
		      ;; ultimately trace back to a top level object) we need to determine
		      ;; whether the ancestor slot is already populated with a valid object and,
		      ;; if so, use that rather than instantiating a new slot object.
		      ;;
		      ;; For this to work we need to interact with :[LIST-]SLOT processing.
		      ;; Imagine we're processing:
		      ;;
		      ;;   (:OBJECT* (:OBJECT outer-type #<outer>)
		      ;;             (:SLOT inner-instance-slot
		      ;;                    (:OBJECT* (:OBJECT inner-type {NIL | #<inner>})
		      ;;                              <sub-production>)))
		      ;;
		      ;; In a a previous pass we instantiated outer-type but may or may not have
		      ;; instantiated inner-type.
		      ;;
		      ;; Since we're doing a subsequent pass of an incremental parse
		      ;; we've passed in, via the EXISTING-OBJECTS keyword, a list containing
		      ;; <outer-type-instance> which was returned by the previous pass.  When
		      ;; we see the :OBJECT* sub production for outer-type we consume
		      ;; <outer-type-instance> from the list instead of instantiating a new
		      ;; outer-type.  We also push an object holder for <outer-type-instance>
		      ;; onto the object holder stack, before making a recursive call to process
		      ;; the :slot sub production.  Note that if we run out of existing objects
		      ;; but encounter more top level :OBJECTs we proceed as if we were doing
		      ;; a non incremental parse from that point on.
		      ;;
		      ;; While processing the :SLOT sub production we check the object stack
		      ;; to see if the object targeted by it has a corresponding NIL slot value
		      ;; or not.
		      ;;
		      ;; If the slot is non NIL and contains a compatible object we create an
		      ;; existing object holder for the object and push the holder onto the
		      ;; object stack.  An existing object holder is the same as an object
		      ;; holder except its car is :EXISTING-OBJECT rather than :OBJECT.  This
		      ;; is the signal to the processing of the next :object to skip
		      ;; instantiation and change the existing object holder back into an
		      ;; ordinary object holder.  If the corresponding slot has a NIL value we
		      ;; simply proceed as we would for a non incremental parse.
		      ;;
		      (let ((obj-holder (second result))
			    pop-existing-obj)
			(ecase (first obj-holder)
			  (:object
			   (setf (third obj-holder)
				 (cond
				   (obj-stack
				    ;; Nested :OBJECT*.  Look for an existing object holder
				    ;; that may have been put there by PROCESS-SLOT.
				    (let ((oh (car obj-stack)))
				      (cond
					((eq (first oh) :existing-object)
					 (unless (typep (third oh) (second obj-holder))
					   (error "Found existing inner object of type ~A ~
                                                   when grammar dictates type ~A"
						  (type-of (third oh)) (second obj-holder)))
					 (pop obj-stack)
					 (third oh))
					(t (let ((constructor (fourth obj-holder)))
					     (if constructor
					       (funcall constructor)
					       (make-instance (second obj-holder))))))))
				   (t
				    (cond
				      (existing-objects
				       ;; Top level :OBJECT*.  Check for a compatible existing
				       ;; object.  If the existing object is not of the required
				       ;; type signal an error.
				       (unless (typep (car existing-objects) (second obj-holder))
					 (error "Found existing top level object of type ~A ~
                                                 when grammar dictates type ~A"
						(type-of (car existing-objects))
						(second obj-holder)))
				       (setf pop-existing-obj t)
				       (car existing-objects))
				      (t (let ((constructor (fourth obj-holder)))
					   (if constructor
					     (funcall constructor)
					     (make-instance (second obj-holder)))))))))))
			(multiple-value-bind (sub-value tags)
			    #-stack-cons (process-result (third result) (cons obj-holder obj-stack))
			    #+stack-cons (apply #'process-result (third result) obj-holder obj-stack)
			  (declare (ignore sub-value))
			  (when pop-existing-obj
			    (pop existing-objects))
			  (values obj-holder tags))))
		     ((:SLOT :LIST-SLOT)
		      ;; (:[LIST-]SLOT <slot-target> <slot-name> <sub-production>)
		      (let* ((targ-obj-holder (find-target-object-holder (second result) obj-stack))
			     (targ-obj        (third targ-obj-holder))
			     (slot-name       (third result))
			     (slot-reader     (fifth result))
			     (slot-val        (if slot-reader
						(funcall slot-reader targ-obj)
						(and targ-obj
						     (slot-boundp targ-obj slot-name)
						     (slot-value targ-obj slot-name))))
			     (e-objs ()))
			(when (and existing-objects targ-obj)
			  (ecase op
			    (:slot
			     (when (eq (type-of (class-of slot-val)) 'standard-class)
			       (setf e-objs (list :existing-object (type-of slot-val) slot-val))))
			    (:list-slot
			     (when (and slot-val
					(listp slot-val)
					(eq (type-of (class-of (car slot-val))) 'standard-class))
			       (setf e-objs
				     (let ((objs ()))		;collect in reverse order
				       (dolist (obj slot-val objs)
					 (push (list :existing-object (type-of obj) obj) objs))))))))
			(multiple-value-bind (v tags)
			    #-stack-cons
			    (process-result (fourth result) (if e-objs (cons e-objs obj-stack) obj-stack))
			    #+stack-cons
			    (apply #'process-result (fourth result) (if e-objs (cons e-objs obj-stack) obj-stack))
			  (setf (fourth result) v)
			  (process-slot result targ-obj-holder obj-stack)
			  (values v tags))))
		     (:SEQUENCE
		      (multiple-value-bind (v1 tags1)
			  #-stack-cons (process-result (second result) obj-stack)
			  #+stack-cons (apply #'process-result (second result) obj-stack)
			(multiple-value-bind (v2 tags2)
			    #-stack-cons (process-result (third result) obj-stack)
			    #+stack-cons (apply #'process-result (third result) obj-stack)
			  (values (cond ((eq v1 t) v2)
					((eq v2 t) v1)
					(t (list :PAIR v1 v2)))
				  (nconc tags1 tags2)))))
		     (:VALUE (values result nil)))))

	       (process-slot (slot-holder target-obj-holder obj-stack)
		 ;;
		 ;; A slot holder is a list of the form:
		 ;;
		 ;;  (:[LIST-]SLOT <slot-target> <slot-name> <result>)
		 ;;
		 ;; where <slot-target> is the type of an object instance contained in a holder
		 ;; that was previously pushed onto the object holder stack or NIL signifing the
		 ;; object instance in the holder at the top of the object stack.  <slot-name>
		 ;; is the slot name of the target object in which to ultimately store the <result>.
		 ;;
		 ;; <result>s take three forms, a primitive result:
		 ;;
		 ;;  (:VALUE <primitive-type> <primitive-value>)
		 ;;
		 ;; eg, "(:VALUE $TELETYPE-CITY/AIRPORT-CODE :YWG)" or a constant result:
		 ;;
		 ;;  (:VALUE NIL <constant-value>)
		 ;;
		 ;; eg, "(:VALUE NIL T)" or an object result:
		 ;;
		 ;;  (:OBJECT <object-name> <object-instance>)
		 ;;
		 ;; eg, "(:OBJECT AIRIMP-NAME #<AIRIMP-NAME {44032825}>)"
		 ;;
		 ;; PROCESS-SLOT finds the target object using the object holder stack (OBJ-STACK)
		 ;; and sets its slot named <slot-name> to the <primitive-value> or
		 ;; <constant-value> or <object-instance> given in <result>.  If we're processing
		 ;; a :SLOT slot holder the setting is a pure assignment.  If we're processing a
		 ;; :LIST-SLOT slot holder then the value is appended to the list of any existing
		 ;; values.
		 ;;
		 (destructuring-bind (slot-type slot-target slot-name result) slot-holder
		   (let ((slot-writer (sixth slot-holder))
			 (obj-holder  (or target-obj-holder
					  (find-target-object-holder slot-target obj-stack))))
		     (unless obj-holder
		       (if slot-target
			 (error "Attempt to process ~S slot with no ~S object on stack ~S"
				slot-name slot-target obj-stack)
			 (error "Attempt to process ~S slot with no object on stack ~S"
				slot-name obj-stack)))
		     (let ((value (third result)))
		       (ecase (first obj-holder)
			 (:OBJECT
			  (let ((object (third obj-holder)))
			    (ecase slot-type
			      (:SLOT
			       (cond (slot-writer
				      (funcall slot-writer value object))
				     (t
				      (setf (slot-value object slot-name) value))))
			      (:LIST-SLOT
			       (cond (slot-writer
				      (funcall slot-writer value object))
				     (t
				      (unless (slot-boundp object slot-name)
					(setf (slot-value object slot-name) nil))
				      (let* ((old-v (slot-value object slot-name))
					     (new-v (nconc old-v (list value))))
					(setf (slot-value object slot-name) new-v)))))))))))))


               (entry-error-unlabeled-p (e)
                 (declare (type quepasa-entry e))
                 (eq (first (quepasa-entry-error e)) t))

               (entry-error-obsolete-p (e)
                 ;; check whether an error occurred at a position that was ultimately parsable
                 (declare (type quepasa-entry e))
                 (and (quepasa-entry-error e)
                      (< (quepasa-entry-start e)
                         maximum-error-free-position)))

               (cull-entries-for-error-message (entries)
                 (declare (type list entries))
                 ;; *Destructively* removes from the list those entries that contain an error and:
                 ;;
                 ;;  - have a duplicate error message
                 ;;
                 ;;  - have error messages with no labels (e.g. (T)). *Only* perform this if there
                 ;;    are also successful entries in the list; otherwise we preserve unlabled
                 ;;    errors since they may be all we have.
                 ;;
                 ;;  - occurred at a parse position less than MAXIMUM-ERROR-FREE-POSITION
                 ;;
                 (setq entries (stable-sort entries #'> :key #'quepasa-entry-start))
                 (setq entries (delete-if #'entry-error-obsolete-p
                                            entries))
                 (setq entries (or (remove-if #'entry-error-unlabeled-p
                                              entries)
                                   entries))
                 (delete-duplicates entries
				    :test #'(lambda (a b) (lists-equal a b :test #'equal))
				    :key #'quepasa-entry-error
				    :from-end t))

	       (non-memoized-subparse (prod active-grammar local-grammar
                                            start sequence-pointer token-start
                                            #+stack-cons &rest stack)
		 #+stack-cons (declare (dynamic-extent stack))

		 (record-maximum-error-free-parse token-start stack)
		 (let ((posn (list start prod sequence-pointer token-start)))

		   ;; we don't process left-recursive grammars because of the pain of dealing with
		   ;; them in a top-down recursive framework (I don't feel like implementing a
		   ;; _real_ parser)
		   ;;
                   ;; Don't run this test by default: it's slow
                   (when debug
                     (when (find posn stack :test #'equal)
                       (error "left recursive grammar: ~A" posn)))

		   (let ((entries-iter
                          #-stack-cons
                           (subparse prod active-grammar local-grammar
                                     start sequence-pointer token-start
                                     (cons posn stack))
                           #+stack-cons
                           (apply #'subparse prod active-grammar local-grammar
                                  start sequence-pointer token-start
                                  posn stack)))
		     entries-iter)))

               (pass (start &key end required (value t) error)
                                (let ((entry (allocate-quepasa-entry)))
                                  (declare (type quepasa-entry entry))
                                  (setf (quepasa-entry-start   entry) start
                                        (quepasa-entry-end      entry) (or end start)
                                        (quepasa-entry-value    entry) value
                                        (quepasa-entry-error    entry) error
                                        (quepasa-entry-required entry) required)
                                  entry))

               (iterate (iter)
                 (with-prefixed-accessors ((prod production) subiter1 subiter2 entry1 entries stack
                                                             active-grammar local-grammar start token-start
                                                             required-errors optional-errors previously-returned)
                     (quepasa-iterator- iter)


                   #+debug-quepasa-alloc
                   (progn
                     (assert (not (or (eq iter subiter1)
                                      (eq iter subiter2))))
                     (assert (quepasa-iterator-allocated iter)))

                   (let* ((type (typecase prod
                                  (string :string)
                                  (cons (car prod))
                                  (quepasa-entry (return-from iterate ; single-value iterator
                                                   (prog1 prod
                                                     (setf (quepasa-iterator-production iter) nil))))
                                  (null nil) ; return error values, or null iterator
                                  #| DLD| unreachable code?
                                  (t (error "Unexpected prod format(1): ~S" prod))
                                  |#))
                          (*quepasa-debug-depth* (i1+ *quepasa-debug-depth*)))
                     (declare (type fixnum *quepasa-debug-depth*))

                     (labels ((pass-new-value (entry v)
                                (declare (optimize (speed 3) (safety 0)))
                                (declare (type quepasa-entry entry))
                                (pass (quepasa-entry-start entry)
                                      :end   (quepasa-entry-end   entry)
                                      :value v
                                      :error (quepasa-entry-error entry)
                                      :required (quepasa-entry-required entry)))
                              (value-combination (a b)
                                (declare (optimize (speed 3) (safety 0)))
                                (cond ((eq a t) b)
                                      ((eq b t) a)
                                      (t (list :sequence a b))))
                              (error-combination (a b)
                                (declare (optimize (speed 3) (safety 0)))
                                (cond ((null a) b)
                                      ((null b) a)
                                      (t (cons a b))))
                              (allowed-error-combination-p (entry1 entry2)
                                (declare (type quepasa-entry entry1 entry2))
                                (declare (optimize (speed 3) (safety 0)))
                                (not (or (quepasa-entry-error entry1)
                                         (quepasa-entry-error entry2))))
                              (get-combination (e1 e2)
                                (declare (type quepasa-entry e1 e2))
                                (declare (optimize (speed 3) (safety 0)))
                                (when (allowed-error-combination-p e1 e2)
                                  (let ((entry (allocate-quepasa-entry)))
                                    (declare (type quepasa-entry entry))
                                    (setf (quepasa-entry-start entry)
                                          (quepasa-entry-start e2)

                                          (quepasa-entry-end    entry)
                                          (quepasa-entry-end    e2)

                                          (quepasa-entry-value    entry)
                                          (value-combination (quepasa-entry-value e1) (quepasa-entry-value e2))

                                          (quepasa-entry-error    entry)
                                          (error-combination (quepasa-entry-error e1) (quepasa-entry-error e2))

                                          (quepasa-entry-required entry)
                                          (or (quepasa-entry-required e1) (quepasa-entry-required e2)))
                                    entry))))

                       (loop
                         (let ((entry

                                (case type

                                  (:TAG
                                   (destructuring-bind ((&whole tag-key &key label key &allow-other-keys) sub-prod) (rest prod)
                                     (declare (ignore sub-prod))
                                     (let ((entry (iterate subiter1)))
                                       (declare (type (or quepasa-entry null) entry))
                                       (cond
                                         ((null entry)
                                          nil)
                                         ((quepasa-entry-error entry)
                                          (when label
                                            (setf (quepasa-entry-error entry)
                                                  (push label (quepasa-entry-error entry))))
                                          entry)
                                         (key
                                          (prog1 (pass (quepasa-entry-start entry)
                                                       :value (if (eq (quepasa-entry-value entry) t)
                                                                t
                                                                `(:TAG ,tag-key ,start ,(quepasa-entry-start entry)
                                                                       ,(quepasa-entry-value entry)))
                                                       :required (quepasa-entry-required entry))
                                            (deallocate-quepasa-entry entry)))
                                         (t
                                          entry)))))

                                  ;; This denotes a 'high-priority' production.  If it fails,
                                  ;; the overall parse fails, regardless of any other branches
                                  ;; that might have succeeded.  If it succeeds, don't bother
                                  ;; trying any of the other successful branches.
                                  ;;
                                  ;; It's useful for getting better error messages (because
                                  ;; instead of having multiple possible failed branches,
                                  ;; resulting in an error message like "Expected foo or bar",
                                  ;; we'll get a single branch, resulting in "Invalid foo")
                                  ;; and reducing backtracking (which is a performance win.)
                                  (:REQUIRE
                                   (let ((e (iterate subiter1)))
                                     (declare (type (or quepasa-entry null) e))
                                     (when e
                                       (setf (quepasa-entry-required e) t))
                                     e))

                                  ;; This implements lookahead -- it doesn't actually
                                  ;; consume any input.  Useful for things like:
                                  ;;   (? (seq (could-parse (prim $$integer :1-999))
                                  ;;           (require (-> COUNT-LASTNAME-FIRSTNAMES))))
                                  (:COULD-PARSE
                                   (let ((e (iterate subiter1)))
                                     (declare (type (or quepasa-entry null) e))
                                     (when e
                                       (setf (quepasa-entry-start e) start
                                             (quepasa-entry-value e) t))
                                     e))

                                  (:OPTIONAL
                                   (when subiter1
                                     (or (loop for e1 = (iterate subiter1) ; prevent empty success instead of omission
                                               while (and e1 (i<= (quepasa-entry-start e1) start))
                                               doing (deallocate-quepasa-entry e1)
                                               finally (return e1))
                                         (progn
                                           (setf subiter1 nil)
                                           (pass start)))))

                                  ((:NEVER-WRITE :ALWAYS-WRITE)
                                   (iterate subiter1))

                                  ((:MATCH-MINIMALLY :MATCH-MAXIMALLY)
                                   (pop entries))

                                  ((:IF :IF-NOT :OR)
                                   ;; For the parser, :if and :if-not are really :or
                                   (loop as entry = (and subiter1 (iterate subiter1))
                                         until entry
                                         doing (setf entries (cdr entries))
                                         while entries
                                         doing (setf subiter1 (sp (car entries)))
                                         finally
                                         ;; This "optimization" worked in the old world but
                                         ;; now breaks stuff.  I wonder why.
                                         #+nil
                                         (when (and entry (quepasa-entry-required entry))
                                           (setf entries nil))
                                         (return entry)))

                                  (:ONE-OR-MORE
                                   (when (null subiter2)
                                     (setf entry1 (loop for e1 = (iterate subiter1)
                                                        while (and e1 (not (quepasa-entry-error e1))
                                                                   (i<= (quepasa-entry-start e1) start))
                                                        doing (deallocate-quepasa-entry e1)
                                                        finally (return e1)))) ; prevent empty recursion
                                   (cond
                                     ((null entry1) nil)
                                     ((quepasa-entry-error entry1) entry1)
                                     (t
                                      (when (null subiter2)
                                        (setf subiter2 (sp prod :start (quepasa-entry-start entry1))))
                                      (let ((entry2 (iterate subiter2)))
                                        (cond
                                          ((null entry2)
                                           (setf subiter2 nil)
                                           entry1)
                                          ((quepasa-entry-error entry2)
                                           entry2)
                                          (t
                                           (prog1 (get-combination entry1 entry2)
                                             (deallocate-quepasa-entry entry2))))))))

                                  (:SEPARATED-LIST
                                   ;; entries := (subiter0 entry0 . entry01))
                                   (when (null subiter1)
                                     ;; Get a new list element
                                     (setf (cadr entries) (iterate (car entries))))
                                   (cond
                                     ((null (cadr entries)) nil)
                                     ((quepasa-entry-error (cadr entries)) (cadr entries))
                                     (t
                                      (when (null subiter1)
                                        (setf subiter1 (sp (third prod) :start (quepasa-entry-start (cadr entries)))))
                                      (loop
                                        ;; Get a new subsequent list element
                                        (let ((entry2 (and subiter2 (iterate subiter2))))
                                          (cond
                                            ((null entry2)
                                             ;; Get a new separator
                                             ;;--- Should check entry1 has nonzero width?
                                             (setf entry1 (iterate subiter1)
                                                   subiter2 nil)
                                             (cond
                                               ((null entry1)
                                                (setf subiter1 nil)
                                                (return (cadr entries)))
                                               ((quepasa-entry-error entry1)
                                                (return entry1))
                                               (t
                                                (setf (cddr entries) (get-combination (cadr entries) entry1)
                                                      subiter2 (sp prod :start (quepasa-entry-start (cddr entries)))))))
                                            ((quepasa-entry-error entry2)
                                             (return entry2))
                                            (t
                                             (return (get-combination (cddr entries) entry2)))))))))

                                  (:SEQUENCE
                                   (loop as entry2 = (and subiter2 (iterate subiter2))
                                         until entry2
                                         when entry1
                                         doing (deallocate-quepasa-entry entry1)
                                         doing (setf entry1 (iterate subiter1))
                                         when (null entry1)
                                         doing (setf (quepasa-iterator-production subiter1) nil)
                                         (setf subiter2 nil)
                                         (return nil)
                                         when (quepasa-entry-error entry1)
                                         doing (setf subiter2 nil)
                                         (return (pass-new-value entry1 (quepasa-entry-value entry1)))
                                         doing (setf subiter2 (sp prod
                                                                  :start (quepasa-entry-start entry1)
                                                                  :sequence-pointer (cdr entries)
                                                                  :token-start token-start))

                                         finally
                                         (return
                                           (if (quepasa-entry-error entry2)
                                             entry2
                                             (prog1 (get-combination entry1 entry2)
                                               (deallocate-quepasa-entry entry2))))))

                                  ;; ((:-> :CONSTANT :END-OF-INPUT :PRIMITIVE :STRING)) ; can't happen


                                  (:OBJECT
                                   (let ((entry (iterate subiter1)))
                                     (when entry
                                       (prog1 (pass-new-value entry (list :object* entries (quepasa-entry-value entry)))
                                         (deallocate-quepasa-entry entry)))))

                                  ((:SLOT :LIST-SLOT)
                                   (let ((entry (iterate subiter1)))
                                     (when entry
                                       (prog1 (pass-new-value entry (append entries (list (quepasa-entry-value entry))))
                                         (deallocate-quepasa-entry entry)))))

                                  ((nil) nil)

                                  (otherwise
                                   (error "unexpected iterator type: ~A" prod)))))

                           (flet ((get-error-entry ()
                                    ;; Check for required entries,
                                    ;; in which case we return only the first.
                                    ;; This is how we prune dead failure branches.
                                    (if required-errors
                                      (prog1 (first required-errors)
                                        (setq required-errors nil
                                              optional-errors nil))
                                      (pop optional-errors)))
                                  (maybe-return (iter value)
                                    (unless value
                                      (deallocate-quepasa-iterator iter)
                                      (when debug
                                        (dotimes (i *quepasa-debug-depth*) (format *debug-io* "  "))
                                        (format *debug-io* "(~A ~A NIL)~%" *quepasa-debug-depth* type))
                                      (return-from iterate value))
                                    (unless (and (= 1 *quepasa-max-parses-to-return*)
                                                 (find (quepasa-entry-start value) previously-returned
                                                       :test #'=))
                                      (push (quepasa-entry-start value) previously-returned)
                                      (when debug
                                        (dotimes (i *quepasa-debug-depth*) (format *debug-io* "  "))
                                        (format *debug-io* "(~A ~A ~A ~A ~A ~A)~%"
                                                *quepasa-debug-depth* type
                                                (quepasa-entry-start value)
                                                (summarize-entry (quepasa-entry-value value) (+ (or debug 1) 4))
                                                (quepasa-entry-error value)
                                                (quepasa-entry-required value)))
                                      (return-from iterate value))
                                    (when debug
                                      (dotimes (i *quepasa-debug-depth*) (format *debug-io* "  "))
                                      (format *debug-io* "(~A ~A ~A ~A ~A ~A) ; culled~%"
                                              *quepasa-debug-depth* type
                                              (quepasa-entry-start value)
                                              (summarize-entry (quepasa-entry-value value) (+ (or debug 1) 4))
                                              (quepasa-entry-error value)
                                              (quepasa-entry-required value)))))
                             (cond
                               ((null prod)
                                (maybe-return iter (get-error-entry)))
                               ((null entry)
                                (setf prod nil
                                      type nil
                                      required-errors (cull-entries-for-error-message
                                                       (nreverse required-errors))
                                      optional-errors (cull-entries-for-error-message
                                                       (nreverse optional-errors)))
                                (maybe-return iter (get-error-entry)))
                               ((null (quepasa-entry-error entry))
                                ;; This optimization was in the old code.  But including it now breaks tests,
                                ;; because I changed the relative priority of required errors and nonrequired
                                ;; nonerrors.  Here's how it fails:  a production returns a required error and
                                ;; then a nonrequired nonerror.  An enclosing production makes them both required,
                                ;; and a production enclosing that makes them both errorful. The result you want
                                ;; is the original required error, but the middle production stripped that out
                                ;; because it saw there was also a required nonerror.
                                #+nil
                                (when (quepasa-entry-required entry)
                                  (setq iter nil
                                        required-errors nil
                                        optional-errors nil))
                                (maybe-return iter entry))
                               ((quepasa-entry-required entry)
                                (push entry required-errors))
                               (t
                                (push entry optional-errors))))))

                       #| DLD|  unreachable code?
                       (assert-bug "Control never gets here: loop exits only through maybe-return")|#
                       ))))

               #+NIL ;;DLD|
               (subparse-check-timeout ()
                 (incf subparse-count-since-timeout-check)
                 (when (= subparse-count-since-timeout-check timeout-check-frequency)
                   (when (> (get-internal-real-time) deadline-itu)
                     (error 'quepasa-parsing-timeout))
                   (check-request-deadline)
                   (setf subparse-count-since-timeout-check 0)))

               (subparse (prod active-grammar local-grammar
                               start sequence-pointer token-start
                               #+stack-cons &rest stack)
                 #+stack-cons (declare (dynamic-extent stack))
                 #+NIL ;;DLD|
                 (subparse-check-timeout)
		 (let* ((type (typecase prod
                                (string :string)
                                (cons (car prod))
                                (t (error "Unexpected prod format(2): ~S" prod))))
                        (result-iterator

                         (macrolet ((make-qp-iterator (&key subiter1 subiter2 entries entry1 production)
                                      `(let ((iter (allocate-quepasa-iterator
                                                    :active-grammar active-grammar
                                                    :local-grammar local-grammar
                                                    :start start
                                                    :token-start token-start
                                                    :stack stack
                                                    :entry1 ,entry1
                                                    :production ,production
                                                    )))
                                         ;;--- These slots may be being initialized from forms which
                                         ;;--- must be evaluated after allocate-quepasa-iterator above,
                                         ;;--- because they may also allocate quepasa-iterators and
                                         ;;--- because we want the iterator stack in the right order.
                                         (setf (quepasa-iterator-subiter1 iter) ,subiter1
                                               (quepasa-iterator-subiter2 iter) ,subiter2
                                               (quepasa-iterator-entries iter) ,entries)
                                         iter)))
                           (labels ((fail (start &key end errstack newerr)
                                      (when newerr (push newerr errstack))
                                      (unless errstack (push t errstack))
                                      (pass start :end end :value nil :error errstack))
                                    ;; iterator-list conses the entire tree of possible parses: that's exactly what
                                    ;; all this iterator stuff is meant to avoid.  Expect it to be highly detrimental
                                    ;; to performance wherever it is used (MAX and MIN).
                                    (iterator-list (iterator)
                                      (loop for entry = (iterate iterator)
                                            while entry
                                            collecting entry))
                                    (single-entry-iterator (single-entry)
                                      (make-qp-iterator :production single-entry)))

                             (case type

                               (:->
                                (ecase (length (rest prod))
                                  (1
                                   (destructuring-bind (sub-prod-name) (rest prod)
                                     (let ((sub-prod (gethash sub-prod-name
                                                              (slot-value active-grammar 'named-productions))))
                                       (unless sub-prod
                                         (error "failed to find production ~A" sub-prod-name))
                                       (sp sub-prod))))
                                  (2
                                   (destructuring-bind (sub-grammar-id sub-prod-name) (rest prod)
                                     (let ((sub-grammar (if (eq sub-grammar-id :LOCAL)
                                                          local-grammar
                                                          (find-quepasa-grammar sub-grammar-id))))
                                       (unless sub-grammar
                                         (error "failed to find grammar ~A" sub-grammar-id))
                                       (let ((sub-prod (gethash sub-prod-name
                                                                (slot-value sub-grammar 'named-productions))))
                                         (unless sub-prod
                                           (error "failed to find production ~A" sub-prod-name))
                                         (sp sub-prod :active-grammar sub-grammar
                                             ;; HACK HACK HACK FOR CROSS-GDS
                                             :local-grammar (if (eq sub-prod-name 'ROOT)
                                                              sub-grammar
                                                              local-grammar))))))))

                               (:TAG (make-qp-iterator :production prod :subiter1 (sp (third prod))))
                               ((:REQUIRE :COULD-PARSE :OPTIONAL)
                                (make-qp-iterator :production prod :subiter1 (sp (second prod))))


                               ((:NEVER-WRITE :ALWAYS-WRITE) (sp (second prod)))

                               (:MATCH-MINIMALLY
                                (make-qp-iterator :production prod
                                                  :entries (stable-sort (iterator-list (sp (second prod)))
                                                                        #'< :key #'quepasa-entry-start)))
                               (:MATCH-MAXIMALLY
                                (make-qp-iterator :production prod
                                                  :entries (stable-sort (iterator-list (sp (second prod)))
                                                                        #'> :key #'quepasa-entry-start)))
                               ((:IF :IF-NOT :OR)
                                ;; For the parser, :if and :if-not are really :or
                                (make-qp-iterator :production prod
                                                  :entries (if (eq type :OR)
                                                             prod
                                                             ;; IF and IF-NOT need to skip the predicate.
                                                             (cdr prod))
                                                  :subiter1 nil))

                               (:ONE-OR-MORE
                                (make-qp-iterator :production prod :subiter1 (sp (second prod))
                                                  :entry1 nil :subiter2 nil))

                               (:SEPARATED-LIST
                                (make-qp-iterator :production prod :entries (list (sp (second prod)) nil)))

                               (:SEQUENCE
                                (let* ((rest-prod sequence-pointer))
                                  (if (rest rest-prod)
                                    (make-qp-iterator :production prod :subiter1 (sp (first rest-prod))
                                                      :entry1 nil :subiter2 nil :entries rest-prod)
                                    (sp (first rest-prod)))))

                               (:CONSTANT
                                (single-entry-iterator (pass start :value (list :value nil (cadr prod)))))

                               (:STRING-OFFSET
                                (single-entry-iterator (pass start :value (list :value nil start))))

                               (:END-OF-INPUT
                                (single-entry-iterator (if (i= start end)
                                                         (pass end)
                                                         (fail start :end end))))

                               (:STRING
                                (let* ((string-length (length prod))
                                       (e (i+ start string-length)))
                                  (single-entry-iterator
                                   (if (string-equal string prod :start1 start :end1 (min end e))
                                     (pass e)
                                     (fail start :end (min end e))))))

                               (:PRIMITIVE
                                (single-entry-iterator
                                 (unless (i<= end start)
                                   (destructuring-bind (type style-name) (rest prod)
                                     (multiple-value-bind (value sub-end err-context)
                                         (call-primitive-parser type style-name string start end)
                                       (cond
                                         (value
                                          (assert sub-end)
                                          (pass sub-end :value (list :value type value)))
                                         (err-context
                                          (fail start
                                                :end (if sub-end
                                                       (min end (max start sub-end)))))))))))


                               (:OBJECT
                                (if (third prod)
                                  (make-qp-iterator :production prod :subiter1 (sp (third prod))
                                                    :entries (list :object (second prod) nil))
                                  (single-entry-iterator (pass start :value (list :object (second prod) nil)))))

                               ((:SLOT :LIST-SLOT)
                                (destructuring-bind (extended-slot-name sub-prod &optional reader writer) (rest prod)
                                  (declare (ignore reader writer))
                                  (let ((slot-name (if (symbolp extended-slot-name)
                                                     extended-slot-name
                                                     (first extended-slot-name)))
                                        (slot-target (if (symbolp extended-slot-name)
                                                       nil
                                                       (rest extended-slot-name))))
                                    (make-qp-iterator :production prod :subiter1 (sp sub-prod)
                                                      :entries (list (first prod) slot-target slot-name)))))

                               (otherwise
                                (error "unexpected prod format(3): ~A" prod)))))))

                   result-iterator)))

	(declare (dynamic-extent #'record-maximum-error-free-parse
                                 #'build-error-label-tree
				 #'process-result #'process-slot
                                 #'entry-error-unlabeled-p #'entry-error-obsolete-p
                                 #'cull-entries-for-error-message
				 #'non-memoized-subparse #'pass
                                 #'iterate #'subparse
                                 ))

        (handler-case 
            (let* ((*print-length* 10)  ; Prevent runaway backtraces
                   (*print-level* 3)
                   (safety-grammar-prod
                    `(:SEQUENCE ,(grammar-root-production grammar) (:END-OF-INPUT)))
                   (entries-iter
                    #-stack-cons
                     (non-memoized-subparse safety-grammar-prod
                                            grammar                      ; active-grammar
                                            grammar                      ; local-grammar
                                            start                        ; offset into input string
                                            (cdr safety-grammar-prod) ; sequence-point (one-based)
                                            start                     ; token-start (zero-based)
                                            ())                       ; stack
                     #+stack-cons
                     (apply #'non-memoized-subparse safety-grammar-prod
                            grammar                            ; active-grammar
                            grammar                            ; local-grammar
                            start                              ; offset into input string
                            (cdr safety-grammar-prod)          ; sequence-point (one-based)
                            start                              ; token-start (zero-based)
                            ()))                               ; stack
                   (first-entry (iterate entries-iter)))

              (if first-entry
                (let* ((entry first-entry)
                       (value (quepasa-entry-value entry))
                       (error (quepasa-entry-error entry))
                       (other-entries
                        (when (> *quepasa-max-parses-to-return* 1)
                          ;; Entries from the same parse share cons cells and process-result acts
                          ;; destructively on an entry. So if we want to get out multiple entries we
                          ;; could modify process-result to be non-destructive, but for now just call
                          ;; it with a copy of the entries.
                          (loop for i from 1 below *quepasa-max-parses-to-return*
                                for entry = (iterate entries-iter)
                                while (and entry (not (quepasa-entry-error entry)))
                                collect (collapse-top-level
                                         (process-result (copy-tree (quepasa-entry-value entry))
                                                         ()))))))
                  (assert (or error (i= (quepasa-entry-start entry) end)))

                  (if error
                    ;;
                    ;; Parse completed with error.
                    ;;
                    (make-parse-result
                     :obj-holders nil
                     :error-string (create-error-message
                                    (build-error-label-tree
                                     (mapcar #'(lambda (e) (quepasa-entry-error e))
                                             (cons first-entry
                                                   (loop for i from 1 below maximum-posn-entries-to-retain
                                                         as entry = (iterate entries-iter)
                                                         while entry
                                                         collecting entry)))))
                     :error-start-position (quepasa-entry-start entry)
                     :error-end-position (quepasa-entry-end entry)
                     :tags nil
                     :alt-results nil)
                    ;;
                    ;; Valid parse; return result.
                    ;;
                    (multiple-value-bind (result docs)
                        #-stack-cons (process-result value ())
                      #+stack-cons (apply #'process-result value ())
                      (make-parse-result
                       :obj-holders (collapse-top-level result)
                       :error-string nil
                       :error-start-position nil
                       :error-end-position nil
                       :tags docs
                       :alt-results other-entries))))
                ;;
                ;; Failed to parse.
                ;;
                (if (i> maximum-error-free-position 0)
                  ;;
                  ;; We failed to parse the input, but some progress was made through the string.
                  ;; Grab the DOC entries from the stack to get an indication of the context of the
                  ;; error.
                  ;;
                  (let* ((contextual-summary
                          (nreverse
                           (mapcar (lambda (stack-entry)
                                     (list (list (first stack-entry) end)          ; position pair
                                           (list :DOC (second (second stack-entry))))) ; docs
                                   (remove :DOC maximum-error-free-parse-stack
                                           :test-not #'eq :key #'caadr))))
                         (summary-as-doc-tuples
                          (loop for ((start end) (nil doc)) in contextual-summary
                                collect (list start end doc))))

                    (make-parse-result
                     :obj-holders nil
                     :error-string "Illegal input"
                     :error-start-position maximum-error-free-position
                     :error-end-position end
                     :tags nil
                     :alt-results summary-as-doc-tuples))
                  ;;
                  ;; Complete and total failure.
                  ;;
                  (make-parse-result
                   :obj-holders nil
                   :error-string "Illegal input"
                   :error-start-position start
                   :error-end-position end
                   :tags nil
                   :alt-results nil))))
          ;; DLD|
          (quepasa-parsing-timeout ()
            NIL)
          #+NIL ;;DLD|
          (quepasa-parsing-timeout ()
            (make-parse-result
             :obj-holders nil
             :error-string "Parsing timed out"
             :error-start-position start
             :error-end-position end
             :tags nil
             :alt-results nil)
            )
          )
        ))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +quepasa-msgbuf-size+ 8192))
(defvar *quepasa-msgbuf*
  (make-array
   +quepasa-msgbuf-size+ :element-type #-ignore 'character #+ignore 'base-char))
#+NIL ;;DLD|
(define-resource quepasa-msgbuf
    :constructor #'(lambda (resource &rest initargs)
                     (declare (ignore resource initargs))
                     (make-array +quepasa-msgbuf-size+
				 :element-type #-ignore 'character #+ignore 'base-char))
    ;; :initializer #'(lambda (resource array)
    ;;                  (declare (ignore resource))
    ;;                  (loop for i from 0 to *quepasa-msgbuf-size* do
    ;;                    (setf (aref array i) #\space)))
    )

(defconstant +unparse-string-buffer-size+ 8192)
(defvar *unparse-string-buffer*
  (make-array +unparse-string-buffer-size+
              :element-type #-ignore 'character #+ignore base-char
              :adjustable t :fill-pointer 0))
#+NIL ;; DLD|
(define-resource unparse-string-buffer
    :constructor #'(lambda (resource &rest initargs)
                     (declare (ignore resource initargs))
                     (make-array +unparse-string-buffer-size+
                                 :element-type #-ignore 'character #+ignore base-char
                                 :adjustable t
                                 :fill-pointer 0))
    :initializer #'(lambda (resource array)
                     (declare (ignore resource))
                     (setf (fill-pointer array) 0)))
(defvar *unparse-one-or-more-used-an-lslot-value-p* nil)
(defvar *unparse-lslot-values* t)   ;; not listp means not inside a :sequence
(defvar *unparse-error-path* '())

(defun unparse-quepasa-objects (objects grammar &key debug)

  "OBJECTS is a list of instances of objects corresponding to the obj/slot/lslot productions
of GRAMMAR.  Set DEBUG to see a depth first report of the unparser walking the grammar
productions as it attempts to derive an output sentence.  When DEBUG is set to T The
productions in the report are elided after a depth of two.  Set DEBUG to an integer
greater than zero to elide after a depth of the value given.

May Throw: quepasa-unparse-error

Returns NIL if a valid sentence of grammar could not be derived from objects, otherwise
a valid sentence is returned."

  (when debug (setf debug (typecase debug (integer debug) (t 2))))
  (let ((unbuf *unparse-string-buffer*))
    (let ((qp-slot-values nil)
          (current-unbuf-dimension (car (array-dimensions unbuf))))
      (labels
          ((qp-slot-value (obj slot-name)
             (dolist (item qp-slot-values)
               (when (and
                      (eq (first (car item)) obj)
                      (eq (second (car item)) slot-name))
                 (return-from qp-slot-value (cdr item))))
             (slot-value obj slot-name))
           (qp-set-slot-value (obj slot-name new-value)
             (dolist (item qp-slot-values)
               (when (and
                      (eq (first (car item)) obj)
                      (eq (second (car item)) slot-name))
                 (setf (cdr item) new-value)
                 (return-from qp-set-slot-value new-value)))
             (push (cons (list obj slot-name) new-value) qp-slot-values)
             new-value)
           (optional-sp-p (sp grammar)
             (cond
               ((stringp sp) nil)
               (t (ecase (first sp)
                    (:tag (optional-sp-p (third sp) grammar))
                    ((:never-write :match-minimally :match-maximally)
                     (optional-sp-p (second sp) grammar))
                    (:-> (ecase (length sp)
                           (2 (optional-sp-p (gethash (second sp)
                                                      (slot-value grammar 'named-productions))
                                             grammar))
                           (3 (let ((grammar (find-quepasa-grammar! (second sp))))
                                (optional-sp-p (gethash (third sp)
                                                        (slot-value grammar 'named-productions))
                                               grammar)))))
                    (:optional  t)
                    ((:separated-list :one-or-more :require :could-parse :if :if-not :or :slot
                      :list-slot :object :primitive :constant :always-write :string-offset) nil)
                    (:sequence
                     (every #'(lambda (x) (optional-sp-p x grammar))
                            (rest sp)))))))
           (slotless-sp-p (sp grammar)
             (cond
               ((stringp sp) t)
               (t (ecase (first sp)
                    ((:tag :object) (slotless-sp-p (third sp) grammar))
                    ((:optional :never-write :always-write :one-or-more :match-minimally
                      :match-maximally :require :could-parse)
                     (slotless-sp-p (second sp) grammar))
                    (:-> (ecase (length sp)
                           (2 (slotless-sp-p (gethash (second sp)
                                                      (slot-value grammar 'named-productions))
                                             grammar))
                           (3 (let ((grammar (find-quepasa-grammar! (second sp))))
                                (slotless-sp-p (gethash (third sp)
                                                        (slot-value grammar 'named-productions))
                                               grammar)))))
                    ((:sequence :or)
                     (let ((elts (loop for ssp in (rest sp)
                                    collecting (slotless-sp-p ssp grammar))))
                       (every #'identity elts)))
                    ((:if :if-not)
                     (let ((elts (loop for ssp in (cddr sp)
                                    collecting (slotless-sp-p ssp grammar))))
                       (every #'identity elts)))
                    (:separated-list
                     (let ((sp1 (slotless-sp-p (second sp) grammar))
                           (sp2 (slotless-sp-p (third sp) grammar)))
                       (and sp1 sp2)))
                    ((:slot :list-slot) nil)
                    ((:primitive :constant :string-offset) t)))))
           (sub-unparse (val-stack prod grammar)
             (let* ((old-lslot-values *unparse-lslot-values*)
                    (res (sub-unparse-aux val-stack prod grammar)))
               (unless res
                 (loop for tail on *unparse-lslot-values*
                    do
                    (when (eq tail old-lslot-values) (return))
                    (destructuring-bind (obj name . val) (car tail)
                      (qp-set-slot-value obj name val)))
                 (setf *unparse-lslot-values* old-lslot-values))
               res))
           (sub-unparse-aux (val-stack prod grammar)
             (cond
               ((stringp prod)
                (when debug
                  (debugf "~A => ~A~%" prod prod))
                prod)
               (t (let ((op (first prod))
                        (r  (rest prod)))
                    (ecase op
                      (:tag
                       (let ((underiv (sub-unparse val-stack (second r) grammar)))
                         (cond
                           (underiv (let ((new-prod (list op (first r) underiv)))
                                      (when debug
                                        (debugf  "~A => ~A~%"
                                                 (summarize-prod prod debug)
                                                 (summarize-prod new-prod debug)))
                                      new-prod))
                           (t (when debug
                                (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      (:never-write
                       (when debug
                         (debugf "~A => NIL~%" (summarize-prod prod)))
                       nil)
                      (:string-offset
                       (when debug
                         (debugf "~A => ''~%" (summarize-prod prod)))
                       t)
                      (:could-parse
                       (when debug
                         (debugf "~A => ''~%" (summarize-prod prod)))
                       "")
                      ((:always-write :match-minimally :match-maximally :require)
                       (let ((underiv (sub-unparse val-stack (first r) grammar)))
                         (when debug
                           (debugf "~A => ~A~%"
                                   (summarize-prod prod debug)
                                   (summarize-prod underiv debug)))
                         underiv))
                      (:sequence
                       (setf r (remove-if #'(lambda (sp)
                                              (and (optional-sp-p sp grammar)
                                                   (slotless-sp-p sp grammar)))
                                          r))
                       (let* ((underivs (loop with err-path = (copy-tree *unparse-error-path*)
                                           for sp in r
                                           for i upfrom 0
                                           as d = (progn
                                                    (setq *unparse-error-path* err-path)
                                                    (sub-unparse val-stack sp grammar))
                                           do (unless (or d
                                                          (and (listp sp)
                                                               (optional-sp-p sp grammar)))
                                                (return-from sub-unparse-aux nil))
                                           when d
                                           collect d)))
                         (cond
                           ((or (not underivs)
                                (some #'null underivs))
                            (when debug
                              (debugf "~A => NIL~%" (summarize-prod prod))))
                           (t (let ((new-prod (cons op underivs)))
                                (when debug
                                  (debugf "~A => ~A~%"
                                          (summarize-prod prod debug)
                                          (summarize-prod new-prod debug)))
                                new-prod)))))
                      (:-> (let ((underiv (ecase (length prod)
                                            (2 (sub-unparse
                                                val-stack
                                                (gethash (first r)
                                                         (slot-value grammar 'named-productions))
                                                grammar))
                                            (3 (let ((grammar (find-quepasa-grammar! (first r))))
                                                 (sub-unparse
                                                  val-stack
                                                  (gethash (second r)
                                                           (slot-value grammar 'named-productions))
                                                  grammar))))))
                             (cond
                               (underiv (when debug
                                          (format *debug-io* "~A => ~A~%"
                                                  (summarize-prod prod debug)
                                                  (summarize-prod underiv debug)))
                                        underiv)
                               (t (when debug
                                    (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      (:optional
                       (let ((underiv (sub-unparse val-stack (first r) grammar))
                             (err-path (copy-tree *unparse-error-path*)))
                         (cond
                           (underiv (let ((new-prod (list :sequence underiv)))
                                      (when debug
                                        (debugf "~A => ~A~%"
                                                (summarize-prod prod debug)
                                                (summarize-prod new-prod debug)))
                                      new-prod))
                           (t (setq *unparse-error-path* err-path)
                              (when debug
                                (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      ((:one-or-more :separated-list)
                       (let (underivs)
                         ;; If any of the daughters are list slots we need to consume values
                         ;; from the object hierarchy or we will loop forever here.
                         (loop with err-path = (copy-tree *unparse-error-path*)
                            as first-time-p = t then nil
                            as *unparse-one-or-more-used-an-lslot-value-p* = nil
                            as sp = (first r)
                            as underiv = (progn
                                           (setq *unparse-error-path* err-path)
                                           (sub-unparse val-stack sp grammar))
                            do (cond
                                 ;; CASES:
                                 ;; a) Has matched list slot (non-list slots irrelevant).
                                 ;; b) Has no slots, but string literals match.
                                 ;; c) Has unmatched list slot and matched non-list slot.
                                 ;; d) Doesn't match.
                                 ( ;; Case a
                                  (and *unparse-one-or-more-used-an-lslot-value-p* underiv)
                                  (setf underivs (cons underiv underivs)))
                                 ( ;; Cases b & c
                                  (and first-time-p underiv)
                                  (setf underivs (cons underiv underivs))
                                  (return))
                                 ( ;; Case d
                                  t (return))))
                         (cond
                           (underivs
                            (setf underivs
                                  (if (eq op :separated-list)
                                      (flet ((separate (list separator)
                                               (loop with len = (length list)
                                                  for e in list
                                                  for i from 1
                                                  appending (if (< i len)
                                                                (list e separator)
                                                                (list e)))))
                                        (declare (dynamic-extent #'separate))
                                        (let ((sep-underiv (sub-unparse val-stack
                                                                        (second r)
                                                                        grammar)))
                                          (if sep-underiv
                                              (cons :sequence (separate (nreverse underivs)
                                                                        sep-underiv))
                                              (return-from sub-unparse-aux
                                                (when debug
                                                  (debugf "~A => NIL~%"
                                                          (summarize-prod prod)))))))
                                      (cons :sequence (nreverse underivs))))
                            (when debug
                              (debugf "~A => ~A~%"
                                      (summarize-prod prod debug)
                                      (summarize-prod underivs debug)))
                            underivs)
                           (t (when debug
                                (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      (:or
                       (let* ((err-path *unparse-error-path*)
                              (err-daughter-paths '())
                              (underiv (loop as *unparse-error-path* = '()
                                          for sp in r
                                          as d = (sub-unparse val-stack sp grammar)
                                          until d
                                          collect *unparse-error-path* into err-daughter-paths1
                                          finally
                                             (setf err-daughter-paths err-daughter-paths1)
                                             (return d))))
                         (setq *unparse-error-path* err-path)
                         (cond
                           (underiv (when debug
                                      (debugf "~A => ~A~%"
                                              (summarize-prod prod debug)
                                              (summarize-prod underiv debug)))
                                    underiv)
                           (t
                              (push (cons op err-daughter-paths) *unparse-error-path*)
                              (when debug
                                (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      ((:if :if-not)
                       (destructuring-bind (condition d1 d2) r
                         (let ((underiv (sub-unparse val-stack
                                                     (if (ecase op
                                                           ;; These uses of eval are allowed, as special exceptions
                                                           ;; to the usual rule against using eval.
                                                           (:if (eval condition))
                                                           (:if-not (not (eval condition))))
                                                         d1
                                                         d2)
                                                     grammar)))
                           (cond
                             (underiv (when debug
                                        (debugf "~A => ~A~%"
                                                (summarize-prod prod debug)
                                                (summarize-prod underiv debug)))
                                      underiv)
                             (t (when debug
                                  (debugf "~A => NIL~%" (summarize-prod prod))))))))
                      ((:slot :list-slot)
                       (destructuring-bind (extended-slot-name sub-prod &optional reader writer) r
                         (declare (ignore reader writer))
                         (let* ((name (if (symbolp extended-slot-name)
                                          extended-slot-name
                                          (first extended-slot-name)))
                                (targ (unless (symbolp extended-slot-name)
                                        (rest (first r))))
                                (obj  (find-target-object-holder targ val-stack))
                                (bnd? (slot-boundp obj name))
                                (val  (and bnd?
                                           (qp-slot-value obj name))))
                           (push (list op name) *unparse-error-path*)
                           (cond
                             ((and (listp sub-prod)
                                   (eq (car sub-prod) :string-offset))
                              (list op extended-slot-name sub-prod))
                             (bnd?
                              (cond
                                ((eq op :slot)
                                 (let ((underiv (sub-unparse (cons val val-stack)
                                                             sub-prod grammar)))
                                   (cond
                                     ((and underiv (> (length underiv) 1))
                                      (let ((err-path (copy-tree *unparse-error-path*))
                                            (new-prod (list op extended-slot-name underiv)))
                                        (when debug
                                          (debugf "~A => ~A~%"
                                                  (summarize-prod prod debug)
                                                  (summarize-prod new-prod debug)))
                                        (setq *unparse-error-path* err-path)
                                        new-prod))
                                     (t (when debug
                                          (debugf "~A => NIL~%"
                                                  (summarize-prod prod)))))))
                                (t (assert (eq op :list-slot))
                                   (when val
                                     (push `(,obj ,name . ,val) *unparse-lslot-values*)
                                     (qp-set-slot-value obj name (cdr val))
                                     (setf *unparse-one-or-more-used-an-lslot-value-p* t)
                                     (let ((err-path (copy-tree *unparse-error-path*))
                                           (underiv (sub-unparse (cons (first val) val-stack)
                                                                 sub-prod grammar)))
                                       (cond
                                         (underiv
                                          (when debug
                                            (debugf "~A => ~A~%"
                                                    (summarize-prod prod debug)
                                                    (summarize-prod underiv debug)))
                                          (setq *unparse-error-path* err-path)
                                          underiv)
                                         (t (qp-set-slot-value obj name val)
                                            (when debug
                                              (debugf "~A => NIL~%"
                                                      (summarize-prod prod debug))))))))))
                             (t (when debug
                                  (debugf "~A => NIL~%" (summarize-prod prod))))))))
                      (:object
                       (let ((obj-class (first r)))
                         (push (list op obj-class) *unparse-error-path*)
                         (if (typep (car val-stack) obj-class)
                           (let ((err-path (copy-tree *unparse-error-path*))
                                 (underiv (sub-unparse val-stack (second r) grammar)))
                             (cond
                               (underiv
                                (when debug
                                  (debugf "~A => ~A~%"
                                          (summarize-prod prod debug)
                                          (summarize-prod underiv debug)))
                                (setq *unparse-error-path* err-path)
                                underiv)
                               (t (when debug
                                    (debugf "~A => NIL~%" (summarize-prod prod))))))
                           (when debug (debugf "~A => NIL~%" (summarize-prod prod))))))
                      (:primitive
                       (let* ((value (car val-stack))
                              (prim-type (first r))
                              (prim-style (second r))
                              (string-value (and value (call-primitive-writer
                                                        prim-type prim-style value :stream nil))))
                         (cond
                           (string-value
                            (cond
                              (prod
                               (let ((new-prod (list op prim-type prim-style string-value)))
                                 (when debug
                                   (debugf "~A => ~A~%"
                                           (summarize-prod prod debug)
                                           (summarize-prod new-prod debug)))
                                 new-prod))
                              (t (push (list op prim-type prim-style value) *unparse-error-path*)
                                 (when debug
                                   (debugf "~A => NIL~%" (summarize-prod prod))))))
                           (t
                            (push (list op prim-type prim-style value) *unparse-error-path*)
                              (when debug
                                (debugf "~A => NIL~%" (summarize-prod prod)))))))
                      (:constant
                       (let ((tos (car val-stack))
                             (value (cadr prod)))
                         (cond
                           ((equal value tos)
                            (when debug
                              (debugf "~A => ~A~%" prod prod))
                            prod)
                           (t
                            (push (list op value) *unparse-error-path*)
                              (when debug
                                (debugf "~A => NIL~%" prod)))))))))))
           (underiv2string-aux (underiv)
             (flet ((cat (&rest strings)
                      (let ((i (fill-pointer unbuf)))
                        (dolist (s strings)
                          (loop for c across s
                                do
                             ;; geometrically grow unparse buffer as needed
                             (when (= i current-unbuf-dimension)
                               (setq current-unbuf-dimension (* current-unbuf-dimension 2))
                               (setq unbuf (adjust-array unbuf (list current-unbuf-dimension))))
                             (setf (aref unbuf i) c)
                             (iincf i)))
                        (setf (fill-pointer unbuf) i))))
               (declare (dynamic-extent #'cat))
               (cond
                 ((stringp underiv) (cat underiv))
                 (t (ecase (first underiv)
                      (:sequence
                       (mapc #'(lambda (ud)
                                 (underiv2string-aux ud))
                             (rest underiv)))
                      ((:tag :slot :list-slot)
                       (underiv2string-aux (third underiv)))
                      (:primitive (cat (fourth underiv)))
                      ((:constant :string-offset)))))))
           (underiv2string (underiv)
             (setf (fill-pointer unbuf) 0)
             (prog1
                 (progn
                   (underiv2string-aux underiv)
                   (subseq unbuf 0 (fill-pointer unbuf)))
               (setf (fill-pointer unbuf) 0))))
        (declare (dynamic-extent #'underiv2string #'underiv2string-aux #'sub-unparse #'sub-unparse-aux
                                 #'optional-sp-p #'slotless-sp-p #'qp-slot-value #'qp-set-slot-value))
        (setq *unparse-error-path* '())
        (let* ((*print-length* 10) ; Prevent runaway backtraces
               (*print-level* 3)
               (*unparse-lslot-values* nil)
               (root-prod (grammar-root-production grammar))
               (underivation (sub-unparse (reverse objects) root-prod grammar)))
          (when (and debug underivation)
            (debugf "Underivation:~%~A~%" (with-output-to-string (stream) (print underivation stream))))
          (if underivation
            (underiv2string underivation)
            (error 'quepasa-unparse-error :objects objects :error-path *unparse-error-path*)))))))

(define-condition quepasa-unparse-error (simple-condition error)
  ((objects :type list
            :initarg :objects
            :accessor quepasa-unparse-error-objects)
   (error-path :type list
               :initarg :error-path
               :accessor quepasa-unparse-error-error-path))
  (:report (lambda (exception stream)
             (format stream
                     "Could not unparse ~S~%~_
                      Error path is ~S"
                     (quepasa-unparse-error-objects exception)
                     (reverse (quepasa-unparse-error-error-path exception))))))


(define-condition quepasa-parsing-timeout (simple-error)
  ()
  (:documentation 
   "Internal condition signaled when parse-quepasa-grammar-string times out."))


;; End of quepasa.lisp
