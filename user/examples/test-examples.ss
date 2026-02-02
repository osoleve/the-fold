;;; user/examples/test-examples.ss — Regression tests for examples gallery
;;;
;;; Run with: scheme --script user/examples/test-examples.ss
;;;
;;; Ensures all registered examples evaluate without error.

(load "core/testing/test-framework.ss")
(load "user/examples/examples.ss")

(display "\n====\n")
(display "Examples Gallery Tests\n")
(display "====\n\n")

;; Examples that require external infrastructure (CAS, network, etc.)
(define *skip-examples* '(store-and-fetch))

(test-group example-tests

  (define-test test-all-examples-run
    "All registered examples (except infrastructure-dependent) should evaluate without error"
    (let ([keys (let-values ([(k v) (hashtable-entries *examples*)])
                  (vector->list k))])
      (for-each
        (lambda (name)
          (unless (memq name *skip-examples*)
            (let* ([entry (hashtable-ref *examples* name #f)]
                   [code (cdr (assq 'code entry))])
              ;; Just verify it evaluates without throwing
              (guard (ex [else (error 'test-examples
                                (format "Example ~a failed: ~a" name ex))])
                (eval code)))))
        keys)
      (assert-true #t)))

  (define-test test-getting-started-examples
    "Getting started examples produce expected results"
    (let ([result (eval '(+ (* 3 4) (- 10 5)))])
      (assert-equal 17 result)))

  (define-test test-recursion-examples
    "Factorial produces correct result"
    (let ([result (eval '(letrec ([fact (lambda (n)
                                          (if (<= n 1) 1 (* n (fact (- n 1)))))])
                           (fact 5)))])
      (assert-equal 120 result)))

  (define-test test-higher-order-examples
    "Map double produces correct result"
    (let ([result (eval '(map (lambda (x) (* x 2)) '(1 2 3 4 5)))])
      (assert-equal '(2 4 6 8 10) result)))

  (define-test test-dsl-arithmetic-interpreter
    "Arithmetic interpreter evaluates correctly"
    (let ([result (eval (example-code 'arithmetic-interpreter))])
      (assert-equal 30 result)))

  (define-test test-dsl-pattern-matcher
    "Pattern matcher returns expected matches"
    (let ([result (eval (example-code 'pattern-matcher))])
      (assert-equal '(#t #t #t #f) result)))

  (define-test test-dsl-state-machine
    "State machine transitions correctly"
    (let ([result (eval (example-code 'state-machine))])
      (assert-equal '(green yellow red green) result)))

  (define-test test-dsl-query-dsl
    "Query DSL selects correctly"
    (let ([result (eval (example-code 'query-dsl))])
      (assert-equal '(alice carol) result)))

)

(run-all-tests)
