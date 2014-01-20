;;;; Original author: ddavies

(in-package :quepasa)

;; This code needs to run after all quepasa grammars have been defined.

;; Define quepasa "regex primitives".
(defun finalize-quepasa ()
  (loop for grammar in *quepasa-grammars*
        as rptab = (quepasa-grammar-regex-prim-tab grammar)
        doing
     (when rptab
       (loop for v being each hash-value of rptab
             as type = (quepasa:regex-prim-tab-entry-type v)
             as style = (quepasa:regex-prim-tab-entry-style v)
             as token-regex = (quepasa:regex-prim-tab-entry-token-regex v)
             as delimiter-regex = (quepasa:regex-prim-tab-entry-delimiter-regex v)
             as delimiter-literal = (quepasa:regex-prim-tab-entry-delimiter-literal v)
             as fxn = (quepasa:regex-prim-tab-entry-coersion-function v)
             doing
          (compile
            (%primitive-value-parser
             (eval (macroexpand
                    `(define-primitive-parser ((type (EQL ',type)) (style (EQL ',style))
                                               string start end :record-source-file nil)
                       (multiple-value-bind (match-start match-end reg-starts reg-ends)
                           (cl-ppcre:scan (cl-ppcre:create-scanner
                                           ,(strcat "(" token-regex ")(" delimiter-regex ")")
                                           :single-line-mode (not (empty-p ,delimiter-regex)))
                                          string :start start :end end)
                         (declare (ignore match-end))
                         (if match-start
                           (values
                             (apply
                              #',fxn (list (subseq string (aref reg-starts 0) (aref reg-ends 0))))
                             (aref reg-ends 0))
                           (return-command-primitive-parse-error
                            quepasa::parse-command-primitive
                            start
                            :error-context '(:error-type :no-match)))))))))
          (compile
            (%primitive-value-writer
             (eval (macroexpand
                    `(define-primitive-writer ((type (EQL ',type)) (style (EQL ',style))
                                               value stream :record-source-file nil)
                       (let* ((str (princ-to-string value))
                              (dstr (strcat str ,delimiter-literal))
                              (v (call-primitive-parser ',type ',style dstr 0 (length dstr))))
                         (when v str))))))))
       (setf (quepasa:quepasa-grammar-regex-prim-tab grammar) NIL))))

(register-image-dump-hook 'finalize-quepasa)

;; End of quepasa-finalization.lisp
