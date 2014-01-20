;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2005-2013 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Carl de Marcken                                 ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :asdf)


(defsystem :quepasa
  :depends-on ()
  :components
  ((:file "pkgdcl")

   ;; DLD|  This stuff is lifted from various quux/qitab files.
   (:file "macros" :depends-on ("pkgdcl"))
   (:file "utilities" :depends-on ("macros"))

   (:file "primitive-value" :depends-on ("utilities"))
   (:file "primitive-parsers" :depends-on ("primitive-value"))
   (:file "primitive-writers" :depends-on ("primitive-value"))
   (:file "quepasa" :depends-on ("primitive-parsers"
                                 "primitive-writers"))

   (:file "lambda-expression-grammar" :depends-on ("quepasa"))
   (:file "lambda-expression-grammar-tests" :depends-on ("lambda-expression-grammar"))
   (:file "lambda-expression-structs" :depends-on ("quepasa"))))


;;; End of quepasa.asd
