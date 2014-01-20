

;;; Tests for lambda calculus expression grammar.


;; DLD| This should not be in the quepasa package
(in-package :quepasa)


(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "fooque"
  ("fooque")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "\\foo.fum"
  ("\\foo.fum")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "(foo fo)"
  ("(foo fo)")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "\\a.(a \\b.(b a))"
  ("\\a.(a \\b.(b a))")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "\\x.\\y.\\z.((z x) (z y))"
  ("\\x.\\y.\\z.((z x) (z y))")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "(\\f.\\g.(\\h.(g h) f) \\p.\\q.p)"
  ("(\\f.\\g.(\\h.(g h) f) \\p.\\q.p)")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "\\foo.\\fi.\\fo.\\fum.(fum (fo (fi foo)))"
  ("\\foo.\\fi.\\fo.\\fum.(fum (fo (fi foo)))")
  :PASS)

(def-quepasa-grammar-unit-test-case :lambda-expression ()
  "(\\p.(\\q.p \\x.(x p)) \\i.\\j.(j i))"
  ("(\\p.(\\q.p \\x.(x p)) \\i.\\j.(j i))")
  :PASS)


;;; End of lambda-expression-grammar-tests.lisp
