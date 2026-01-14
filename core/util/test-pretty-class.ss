;;; core/util/test-pretty-class.ss -- Tests for Pretty Type Class

(load "core/testing/test-framework.ss")
(load "core/util/pretty-class.ss")

(display "\n")
(display "====\n")
(display "          PRETTY TYPE CLASS TESTS\n")
(display "====\n")

;;; ====
;;; Primitive Pretty Tests
;;; ====

(test-group primitive-pretty-tests
  (define-test nat-pretty-test
    (assert-equal "42" (pretty-render (nat-pretty 42)))
    (assert-equal "0" (pretty-render (nat-pretty 0))))

  (define-test int-pretty-test
    (assert-equal "-5" (pretty-render (int-pretty -5)))
    (assert-equal "100" (pretty-render (int-pretty 100))))

  (define-test bool-pretty-test
    (assert-equal "#t" (pretty-render (bool-pretty #t)))
    (assert-equal "#f" (pretty-render (bool-pretty #f))))

  (define-test char-pretty-test
    (assert-equal "#\\a" (pretty-render (char-pretty #\a)))
    (assert-equal "#\\Z" (pretty-render (char-pretty #\Z))))

  (define-test string-pretty-test
    (assert-equal "\"hello\"" (pretty-render (string-pretty "hello")))
    (assert-equal "\"\"" (pretty-render (string-pretty ""))))

  (define-test symbol-pretty-test
    (assert-equal "foo" (pretty-render (symbol-pretty 'foo)))
    (assert-equal "bar-baz" (pretty-render (symbol-pretty 'bar-baz)))))

;;; ====
;;; Precedence Tests
;;; ====

(test-group precedence-tests
  (define-test parens-if-true
    (assert-equal "(x)" (pretty-render (parens-if #t (text "x")))))

  (define-test parens-if-false
    (assert-equal "x" (pretty-render (parens-if #f (text "x")))))

  (define-test binop-no-parens
    ;; At prec-top (0), binop at prec-add (6) doesn't need parens
    (let ([doc (pretty-binop prec-top prec-add (text "a") "+" (text "b"))])
      (assert-equal "a + b" (pretty-render doc))))

  (define-test binop-with-parens
    ;; At prec-mul (7), binop at prec-add (6) needs parens (addition in mult context)
    (let ([doc (pretty-binop prec-mul prec-add (text "a") "+" (text "b"))])
      (assert-equal "(a + b)" (pretty-render doc))))

  (define-test unop-test
    (let ([doc (pretty-unop prec-top prec-app "-" (text "x"))])
      (assert-equal "-x" (pretty-render doc)))))

;;; ====
;;; List Pretty Tests
;;; ====

(test-group list-pretty-tests
  (define-test empty-list-pretty
    (assert-equal "()" (pretty-render (list-pretty '()))))

  (define-test singleton-list-pretty
    ;; Uses sexp->doc fallback
    (let ([doc (list-pretty-with (lambda (x) (text (number->string x))) '(42))])
      (assert-equal "(42)" (pretty-render doc))))

  (define-test multi-element-list-pretty
    (let ([doc (list-pretty-with (lambda (x) (text (number->string x))) '(1 2 3))])
      ;; sep will put on one line if it fits
      (assert-equal "(1 2 3)" (pretty-render doc)))))

;;; ====
;;; Width-Aware Tests
;;; ====

(test-group width-tests
  (define-test narrow-width-breaks-list
    ;; With very narrow width, list should break to multiple lines
    (let ([doc (list-pretty-with (lambda (x) (text (number->string x)))
                                 '(100 200 300 400))])
      ;; Width 10 should force line breaks
      (let ([result (pretty-render-width 10 doc)])
        ;; Should contain newline
        (assert-true (string-contains? result "\n")))))

  (define-test wide-width-keeps-list-flat
    ;; With wide width, list should stay on one line
    (let ([doc (list-pretty-with (lambda (x) (text (number->string x)))
                                 '(1 2 3))])
      (let ([result (pretty-render-width 80 doc)])
        ;; Should NOT contain newline
        (assert-false (string-contains? result "\n"))))))

;;; ====
;;; Scheme Vector Tests
;;; ====

(test-group scheme-vec-tests
  (define-test scheme-vec-empty
    (assert-equal "[]" (pretty-render (scheme-vec-pretty (vector)))))

  (define-test scheme-vec-numbers
    (assert-equal "[1, 2, 3]" (pretty-render (scheme-vec-pretty (vector 1 2 3)))))

  (define-test scheme-vec-mixed
    ;; Should handle mixed types via infer-elem-pretty
    (assert-equal "[1, #t, \"hi\"]" (pretty-render (scheme-vec-pretty (vector 1 #t "hi"))))))

;;; ====
;;; Dispatch Hook Tests
;;; ====

(test-group dispatch-tests
  (define-test dispatch-default-uses-core
    ;; Without custom dispatch, uses core-elem-pretty
    (set-pretty-dispatch! #f)
    (assert-equal "(1 2 3)" (pretty-render (list-pretty '(1 2 3)))))

  (define-test sexp->doc-dispatch-uses-infer
    ;; sexp->doc-dispatch should use infer-elem-pretty for nested elements
    ;; Set up custom dispatch that transforms 42 to "ANSWER"
    (set-pretty-dispatch! (lambda (x)
                            (if (and (number? x) (= x 42))
                                (text "ANSWER")
                                (core-elem-pretty x))))
    ;; Unknown tag wrapping 42 should still use dispatch
    (assert-equal "(mystery ANSWER)" (pretty-render (sexp->doc-dispatch '(mystery 42))))
    ;; Cleanup
    (set-pretty-dispatch! #f))

  (define-test dispatch-custom-hook
    ;; Custom dispatch can override behavior
    (set-pretty-dispatch! (lambda (x)
                            (if (and (number? x) (= x 42))
                                (text "THE-ANSWER")
                                (core-elem-pretty x))))
    (assert-equal "(THE-ANSWER 1 2)" (pretty-render (list-pretty '(42 1 2))))
    ;; Cleanup
    (set-pretty-dispatch! #f))

  (define-test infer-elem-numbers
    (set-pretty-dispatch! #f)
    (assert-equal "42" (pretty-render (infer-elem-pretty 42))))

  (define-test infer-elem-nested-list
    (set-pretty-dispatch! #f)
    (assert-equal "((1 2) (3 4))" (pretty-render (list-pretty '((1 2) (3 4)))))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
