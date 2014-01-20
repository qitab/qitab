;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2004-2010 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Derek Davies                                    ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :quepasa)


;; This is used to replace lengthy eql-dispatch on two parameters by a plist lookup and an
;; eql-dispatch on a single parameter in primitive parse/write methods.  This structure is
;; placed on the 'primitive-value' property of the symbol TYPE.  The PARSER and WRITER
;; slots hold methods eql specialized on the STYLE parameter value.  The STYLE specializer
;; is identical for the primitive parser and writer of a given TYPE.
(defstruct (%primitive-value (:constructor %make-primitive-value))
  (parser nil)
  (writer nil))


(defmacro define-primitive-parser ((type style string start end)
                                   &body body)

  (let* ((vtype  (if (consp type)  (first type)  type))
	 (vstyle (if (consp style) (first style) style))
	 (tspec  (if (consp type)  (second type) 't))
	 (tname  (if (and (consp tspec) (eql (first tspec) 'eql))
		   (let ((tname (second tspec)))
		     (if (and (consp tname) (eql (first tname) 'quote))
		       (second tname)
		       tname))
		   (error "Primitive parser type must be EQL-specialized -- ~S" type)))
	 (pname  (xintern "~A-~A" tname 'primitive-parser))
         (doc    (if (stringp (car body)) (car body)))
         (body   (if (stringp (car body)) (cdr body) body)))
    
    `(progn
       (defmethod ,pname (,style ,vtype ,string ,start ,end)
         ,@(when doc `(,doc))
	 (declare (ignorable ,vtype ,vstyle))
         (assert
          (< ,start ,end) ()
          "Primitive parser for type ~S, style ~S called by parser without input string."
          ,vtype ,vstyle)
	 (block parse-command-primitive
	   ,@body))
       (let ((entry (or (get ',tname 'primitive-value)
                        (%make-primitive-value))))
	 (setf (%primitive-value-parser entry) ',pname)
	 (setf (get ',tname 'primitive-value) entry)))))

(defun call-primitive-parser (type style string start end)

  "Call the primitive parser for TYPE and STYLE on STRING.
START and END provide bounds on STRING (although END is
always same as the end of STRING, and is therefore
redundant)."

  (funcall (%primitive-value-parser (get type 'quepasa::primitive-value))
	   ;; Reverse style and type arguments so that the remaining
	   ;; eql-specializer comes first.
	   style type string start end))

(defmacro define-primitive-writer ((type style value stream)
                                   &body body)
  "Define a Quepasa 'primitive' writer.
   'type' is a CLOS-style specifier naming a Quepasa type, e.g., (type (eql '$$string))
   'style' is CLOS-style specifier naming a role, e.g., (style (eql ':tla))
   'value' is the value to be written to the stream 'stream'."
  (let* ((vtype  (if (consp type)  (first type)  type))
	 (vstyle (if (consp style) (first style) style))
	 (tspec  (if (consp type)  (second type) 't))
	 (tname  (if (and (consp tspec) (eql (first tspec) 'eql))
		   (let ((tname (second tspec)))
		     (if (and (consp tname) (eql (first tname) 'quote))
		       (second tname)
		       tname))
		   (error "Primitive writer type must be EQL-specialized -- ~S" type)))
	 (pname  (xintern "~A-~A" tname 'primitive-writer))
         (doc    (if (stringp (car body)) (car body)))
         (body   (if (stringp (car body)) (cdr body) body)))
    
    `(progn
       (defmethod ,pname (,style ,vtype ,value ,stream)
         ,@(when doc `(,doc))
	 (declare (ignorable ,vtype ,vstyle))
	 (block call-primitive-writer
	   ,@body))
       (let ((entry (or (get ',tname 'primitive-value)
			(%make-primitive-value))))
	 (setf (%primitive-value-writer entry) ',pname)
	 (setf (get ',tname 'primitive-value) entry)))))

(defun call-primitive-writer (type style value &key (stream *standard-output*))

  "Call the primitive writer for TYPE and STYLE on VALUE.  Output should be
written to STREAM (although STREAM is currently NIL in all cases and is
therefore redundant."

  (funcall (%primitive-value-writer (get type 'primitive-value))
	   ;; Reverse style and type arguments so that the remaining
	   ;; eql-specializer comes first.
	   style type value stream))


;; End of primitive-value.lisp
