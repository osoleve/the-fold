;;; lattice/fp/meta/test-result.ss — Tests for Result module
;;;
;;; Tests the consolidated Result type operations.

(load "core/testing/test-framework.ss")
(load "lattice/fp/meta/result.ss")

(test-group 'result-basics

  (define-test "ok constructor"
    (assert-true (ok? (ok 42)))
    (assert-equal 42 (unwrap-ok (ok 42))))

  (define-test "err constructor"
    (assert-true (error? (err "oops")))
    (assert-equal '("oops") (cdr (err "oops"))))

  (define-test "result-fold on ok"
    (assert-equal 84
                  (result-fold (lambda (x) (* x 2))
                               (lambda (e) 0)
                               (ok 42))))

  (define-test "result-fold on error"
    (assert-equal 0
                  (result-fold (lambda (x) (* x 2))
                               (lambda (e) 0)
                               (list 'error "fail"))))

  (define-test "result-default on ok"
    (assert-equal 42 (result-default 0 (ok 42))))

  (define-test "result-default on error"
    (assert-equal 0 (result-default 0 (list 'error "fail"))))

  (define-test "result-or returns first ok"
    (let ([r (result-or (ok 1) (ok 2))])
      (assert-true (ok? r))
      (assert-equal 1 (unwrap-ok r))))

  (define-test "result-or returns second if first fails"
    (let ([r (result-or (list 'error "e1") (ok 2))])
      (assert-true (ok? r))
      (assert-equal 2 (unwrap-ok r))))

  (define-test "result-and returns second if first ok"
    (let ([r (result-and (ok 1) (ok 2))])
      (assert-true (ok? r))
      (assert-equal 2 (unwrap-ok r))))

  (define-test "result-and returns first error"
    (let ([r (result-and (list 'error "e1") (ok 2))])
      (assert-true (error? r)))))

(test-group 'result-map-variants

  (define-test "result-map-error transforms error"
    (let ([r (result-map-error
               (lambda (e) (list 'wrapped e))
               (list 'error "inner"))])
      (assert-true (error? r))
      (assert-equal '(wrapped ("inner")) (cdr r))))

  (define-test "result-map-error preserves ok"
    (let ([r (result-map-error
               (lambda (e) "should not run")
               (ok 42))])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r))))

  (define-test "result-bimap transforms ok"
    (let ([r (result-bimap
               (lambda (x) (* x 2))
               (lambda (e) "err")
               (ok 21))])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r))))

  (define-test "result-bimap transforms error"
    (let ([r (result-bimap
               (lambda (x) "ok")
               (lambda (e) (cons 'wrapped e))
               (list 'error "fail"))])
      (assert-true (error? r))
      (assert-equal '(wrapped "fail") (cdr r)))))

(test-group 'result-catch

  (define-test "result-catch captures success"
    (let ([r (result-catch (lambda () (+ 1 2)))])
      (assert-true (ok? r))
      (assert-equal 3 (unwrap-ok r))))

  (define-test "result-catch captures exception"
    (let ([r (result-catch (lambda () (error 'test "boom")))])
      (assert-true (error? r))))

  (define-test "result-try returns value on success"
    (assert-equal 42 (result-try (lambda () 42) 0)))

  (define-test "result-try returns default on error"
    (assert-equal 0 (result-try (lambda () (error 'x "fail")) 0))))

(test-group 'result-assertions

  (define-test "assert-ok! extracts value"
    (assert-equal 42 (assert-ok! (ok 42))))

  (define-test "assert-ok! throws on error"
    (assert-error (lambda () (assert-ok! (list 'error "fail"))))))

(test-group 'result-conversions

  (define-test "result->maybe converts ok to just"
    (let ([m (result->maybe (ok 42))])
      (assert-true (just? m))
      (assert-equal 42 (from-just m))))

  (define-test "result->maybe converts error to nothing"
    (assert-true (nothing? (result->maybe (list 'error "e")))))

  (define-test "maybe->result converts just to ok"
    (let ([r (maybe->result (just 42) "default-err")])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r))))

  (define-test "maybe->result converts nothing to error"
    (let ([r (maybe->result nothing "no-value")])
      (assert-true (error? r))))

  (define-test "result->either converts ok to right"
    (let ([e (result->either (ok 42))])
      (assert-true (right? e))
      (assert-equal 42 (from-right e))))

  (define-test "result->either converts error to left"
    (let ([e (result->either (list 'error "oops"))])
      (assert-true (left? e))))

  (define-test "either->result roundtrip"
    (let* ([r1 (ok 42)]
           [e (result->either r1)]
           [r2 (either->result e)])
      (assert-true (ok? r2))
      (assert-equal 42 (unwrap-ok r2)))))

(test-group 'result-applicative

  (define-test "result-ap applies function"
    (let ([r (result-ap (ok (lambda (x) (* x 2))) (ok 21))])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r))))

  (define-test "result-ap short-circuits on error"
    (let ([r (result-ap (list 'error "no-fn") (ok 21))])
      (assert-true (error? r))))

  (define-test "result-lift2 lifts binary function"
    (let ([r (result-lift2 + (ok 20) (ok 22))])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r)))))

(test-group 'result-traversal

  (define-test "result-traverse success"
    (let ([r (result-traverse
               (lambda (x) (if (positive? x) (ok (* x 2)) (list 'error "neg")))
               '(1 2 3))])
      (assert-true (ok? r))
      (assert-equal '(2 4 6) (unwrap-ok r))))

  (define-test "result-traverse stops on error"
    (let ([r (result-traverse
               (lambda (x) (if (positive? x) (ok x) (list 'error x)))
               '(1 -2 3))])
      (assert-true (error? r)))))

(test-group 'validation

  (define-test "validation-ok creates success"
    (assert-true (validation-ok? (validation-ok 42))))

  (define-test "validation-err creates single error"
    (let ([v (validation-err "oops")])
      (assert-true (validation-err? v))
      (assert-equal '("oops") (validation-errors v))))

  (define-test "validation-ap accumulates errors"
    (let ([v (validation-ap
               (validation-err "e1")
               (validation-err "e2"))])
      (assert-true (validation-err? v))
      (assert-equal '("e1" "e2") (validation-errors v))))

  (define-test "validation-ap applies on success"
    (let ([v (validation-ap
               (validation-ok (lambda (x) (* x 2)))
               (validation-ok 21))])
      (assert-true (validation-ok? v))
      (assert-equal 42 (validation-value v))))

  (define-test "validation->result converts"
    (let ([r (validation->result (validation-ok 42))])
      (assert-true (ok? r))
      (assert-equal 42 (unwrap-ok r)))))

(run-all-tests)
