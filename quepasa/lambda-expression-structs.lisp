

;;; Structure definitions for lambda calculus expression parsesing instance hierarchy.


;; DLD| This should not be in the quepasa package
(in-package :quepasa)


(defstruct expression
  (e))

(defstruct sym
  (name))

(defstruct abstraction
  (bound-variable)
  (body))

(defstruct application
  (function-expression)
  (argument-expression))


;;; End of lambda-expression-structs.lisp
