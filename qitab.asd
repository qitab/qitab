;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#-asdf3 (error "QITAB requires ASDF 3 or later. Please upgrade your ASDF.")

(defsystem :qitab
  :description "QITAB: collection of Common Lisp libraries"
  :long-description
  "QITAB is a collection of Common Lisp libraries
containing code initially developed by ITA Software."
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (load-op :qitab/test)))
  :perform (test-op (o c) (symbol-call :qitab/test/all :test-suite)))

(defsystem :qitab/test :depends-on (:qitab/test/all))

;;; Register non-trivially-named packages defined by our dependencies
(register-system-packages
 :closer-mop
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))
