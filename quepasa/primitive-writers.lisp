;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the MIT license.                             ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2006-2008 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Derek Davies                                    ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :quepasa)


(define-primitive-writer ((type (eql '$$string)) (style (eql ':id))
			  value stream)

  "If VALUE is a valid id string (i.e., all characters are alpha), either
write VALUE to STREAM, if STREAM is not NIL, or return VALUE, if STREAM is NIL."

  (when (and (stringp value)
	     (every #'alpha-char-p value))
    (if stream
      (not (format stream "~A" value))
      (format stream "~A" value))))


;; End of primitive-writers.lisp
