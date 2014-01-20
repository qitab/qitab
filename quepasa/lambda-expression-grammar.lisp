

;;; Grammar for lambda calculus expressions.


;; DLD| This should not be in the quepasa package
(in-package :quepasa)


;; DLD|  This should be taking an ID rather than a QUEPASA-GRAMMAR .
;; DLD|  Further, it should be eliminated entirely with it's body incorated into
;; DLD|    QUEPASA-GRAMMAR-ROOT-PRODUCTION .
(register-quepasa-grammar
 :grammar (make-instance 'quepasa-grammar
            :id :lambda-expression))


;; DLD|  This should be taking an ID rather than a QUEPASA-GRAMMAR .
(def-quepasa-grammar-root-production

    (:grammar (find-quepasa-grammar :lambda-expression))

    (-> EXPRESSION))

(def-quepasa-grammar-production EXPRESSION

    (:grammar (find-quepasa-grammar :lambda-expression))

  (or (-> sym)
      (-> abstraction)
      (-> application)))

;; DLD|  This should be taking an ID rather than a QUEPASA-GRAMMAR .
(def-quepasa-grammar-production SYM

    (:grammar (find-quepasa-grammar :lambda-expression))

  (obj sym
       (slot name (prim $$string :id))))

;; DLD|  This should be taking an ID rather than a QUEPASA-GRAMMAR .
(def-quepasa-grammar-production ABSTRACTION

    (:grammar (find-quepasa-grammar :lambda-expression))

  (obj abstraction
       (seq "\\"
            (slot bound-variable (-> sym))
            "."
            (slot body (-> expression)))))

;; DLD|  This should be taking an ID rather than a QUEPASA-GRAMMAR .
(def-quepasa-grammar-production APPLICATION

    (:grammar (find-quepasa-grammar :lambda-expression))

  (obj application
       (seq "("
            (slot function-expression (-> expression))
            " "
            (slot argument-expression (-> expression))
            ")")))


;;; End of lambda-expression-grammar.lisp
