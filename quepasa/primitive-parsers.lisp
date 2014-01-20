;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2004-2011 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Derek Davies                                    ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quepasa)

(defmacro return-command-primitive-parse-error
    (from error-upper-bound &key error-context)
    "Return a value list to the caller of a primitive parser indicating an error.
     
     FROM                   - Name of caller
     ERROR-UPPER-BOUND      - The highest possible string index where the error occurred.
                              Taken together, the starting position and this value
                              should inclusively bound the scope of the error.
                              Use this only for error reporting, not for further parse
                              attempts.
     :ERROR-CONTEXT         - A plist of metadata explaining the error
     "

  `(,@(if from `(return-from ,from) '(return))
    (values nil ,error-upper-bound ,error-context)))

(defun parse-command-primitive-or-unparsable (type style string start end)

  "Call CALL-PRIMITIVE-PARSER and if its first value is NIL return :UNPARSABLE, otherwise
   return what CALL-PRIMITIVE-PARSER returns."

  (multiple-value-bind (value/nil sub-end/err-pt err-ctx sug-alt sug-alts)
      (call-primitive-parser type style string start end)
    (if value/nil
      (values value/nil sub-end/err-pt)
      (values :UNPARSABLE sub-end/err-pt err-ctx sug-alt sug-alts))))

(define-primitive-parser ((type (eql '$$string)) (style (eql ':id))
			  string start end)
    (loop for c across (subseq string start end)
          for i from start
          while
          (alpha-char-p c)
          finally
          (cond
            ((alpha-char-p c)
             (incf i)
             (return (values (subseq string start i) i)))
            ((/= i start)
             (return (values (subseq string start i) i)))
            (T
             (return-command-primitive-parse-error
              parse-command-primitive
              start
              :error-context '(:error-type :illegal-value))))))


;; End of primitive-parsers.lisp
