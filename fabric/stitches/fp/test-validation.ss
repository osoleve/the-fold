;;; fabric/stitches/fp/test-validation.ss — Tests for Validation Library

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/validation.ss")

(display "
")
(display "==============================================================
")
(display "         VALIDATION LIBRARY TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Core Validation Type Tests
;;; ============================================================

(test-group validation-basics
            (define-test success-creation-test
              (let ([v (validation-success 42)])
                   (assert-true (validation-success? v))
                   (assert-false (validation-failure? v))))
            
            (define-test failure-creation-test
              (let ([v (validation-failure "error")])
                   (assert-true (validation-failure? v))
                   (assert-false (validation-success? v))))
            
            (define-test failures-creation-test
              (let ([v (validation-failures '("e1" "e2" "e3"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("e1" "e2" "e3") (from-failure v))))
            
            (define-test from-success-test
              (assert-equal 42 (from-success (validation-success 42))))
            
            (define-test from-failure-test
              (assert-equal '("error") (from-failure (validation-failure "error")))))

;;; ============================================================
;;; Functor Tests
;;; ============================================================

(test-group validation-functor
            (define-test map-success-test
              (let ([v (validation-map add1 (validation-success 5))])
                   (assert-true (validation-success? v))
                   (assert-equal 6 (from-success v))))
            
            (define-test map-failure-test
              (let ([v (validation-map add1 (validation-failure "error"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("error") (from-failure v)))))

;;; ============================================================
;;; Applicative Tests
;;; ============================================================

(test-group validation-applicative
            (define-test pure-test
              (let ([v (validation-pure 42)])
                   (assert-true (validation-success? v))
                   (assert-equal 42 (from-success v))))
            
            (define-test ap-both-success-test
              (let ([v (validation-ap (validation-success add1)
                                      (validation-success 5))])
                   (assert-true (validation-success? v))
                   (assert-equal 6 (from-success v))))
            
            (define-test ap-function-failure-test
              (let ([v (validation-ap (validation-failure "no fn")
                                      (validation-success 5))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("no fn") (from-failure v))))
            
            (define-test ap-value-failure-test
              (let ([v (validation-ap (validation-success add1)
                                      (validation-failure "no val"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("no val") (from-failure v))))
            
            (define-test ap-both-failure-accumulates-test
              (let ([v (validation-ap (validation-failure "err1")
                                      (validation-failure "err2"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("err1" "err2") (from-failure v))))
            
            (define-test ap2-test
              ;; ap2 expects curried function: (a -> b -> c)
              (let ([v (validation-ap2 (lambda (a) (lambda (b) (+ a b)))
                                       (validation-success 3)
                                       (validation-success 4))])
                   (assert-true (validation-success? v))
                   (assert-equal 7 (from-success v))))
            
            (define-test ap2-accumulates-test
              (let ([v (validation-ap2 (lambda (a) (lambda (b) (+ a b)))
                                       (validation-failure "e1")
                                       (validation-failure "e2"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("e1" "e2") (from-failure v))))
            
            (define-test ap3-test
              ;; ap3 expects curried: (a -> b -> c -> d)
              (let ([v (validation-ap3 (lambda (a) (lambda (b) (lambda (c) (+ a b c))))
                                       (validation-success 1)
                                       (validation-success 2)
                                       (validation-success 3))])
                   (assert-true (validation-success? v))
                   (assert-equal 6 (from-success v))))
            
            (define-test ap4-test
              ;; ap4 expects curried: (a -> b -> c -> d -> e)
              (let ([v (validation-ap4 (lambda (a) (lambda (b) (lambda (c) (lambda (d) (+ a b c d)))))
                                       (validation-success 1)
                                       (validation-success 2)
                                       (validation-success 3)
                                       (validation-success 4))])
                   (assert-true (validation-success? v))
                   (assert-equal 10 (from-success v)))))

;;; ============================================================
;;; Combining Validations Tests
;;; ============================================================

(test-group validation-combining
            (define-test sequence-empty-test
              (let ([v (validation-sequence '())])
                   (assert-true (validation-success? v))
                   (assert-equal '() (from-success v))))
            
            (define-test sequence-all-success-test
              (let ([v (validation-sequence
                        (list (validation-success 1)
                              (validation-success 2)
                              (validation-success 3)))])
                   (assert-true (validation-success? v))
                   (assert-equal '(1 2 3) (from-success v))))
            
            (define-test sequence-accumulates-errors-test
              (let ([v (validation-sequence
                        (list (validation-success 1)
                              (validation-failure "e1")
                              (validation-success 3)
                              (validation-failure "e2")))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("e1" "e2") (from-failure v))))
            
            (define-test traverse-test
              (let ([v (validation-traverse
                        (lambda (x)
                                (if (> x 0)
                                    (validation-success x)
                                    (validation-failure "non-positive")))
                        '(1 2 3))])
                   (assert-true (validation-success? v))
                   (assert-equal '(1 2 3) (from-success v))))
            
            (define-test traverse-accumulates-test
              (let ([v (validation-traverse
                        (lambda (x)
                                (if (> x 0)
                                    (validation-success x)
                                    (validation-failure "non-positive")))
                        '(1 -2 3 -4))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("non-positive" "non-positive") (from-failure v))))
            
            (define-test concat-test
              (let ([v (validation-concat
                        (validation-success 'ignored)
                        (validation-success 'kept))])
                   (assert-true (validation-success? v))
                   (assert-equal 'kept (from-success v))))
            
            (define-test all-success-test
              (let ([v (validation-all
                        (list (validation-success 1)
                              (validation-success 2)))])
                   (assert-true (validation-success? v))))
            
            (define-test all-accumulates-test
              (let ([v (validation-all
                        (list (validation-failure "e1")
                              (validation-success 1)
                              (validation-failure "e2")))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("e1" "e2") (from-failure v)))))

;;; ============================================================
;;; Either Conversion Tests
;;; ============================================================

(test-group either-conversion
            (define-test validation-to-either-success-test
              (let ([e (validation->either (validation-success 42))])
                   (assert-true (right? e))
                   (assert-equal 42 (from-right e))))
            
            (define-test validation-to-either-failure-test
              (let ([e (validation->either (validation-failure "err"))])
                   (assert-true (left? e))
                   (assert-equal '("err") (from-left e))))
            
            (define-test either-to-validation-right-test
              (let ([v (either->validation (right 42))])
                   (assert-true (validation-success? v))
                   (assert-equal 42 (from-success v))))
            
            (define-test either-to-validation-left-test
              (let ([v (either->validation (left "err"))])
                   (assert-true (validation-failure? v))
                   (assert-equal '("err") (from-failure v)))))

;;; ============================================================
;;; Validator Tests
;;; ============================================================

(test-group validator-basics
            (define-test make-validator-success-test
              (let* ([v ((make-validator number? "not a number") 42)])
                    (assert-true (validation-success? v))
                    (assert-equal 42 (from-success v))))
            
            (define-test make-validator-failure-test
              (let* ([v ((make-validator number? "not a number") "hello")])
                    (assert-true (validation-failure? v))
                    (assert-equal '("not a number") (from-failure v))))
            
            (define-test run-validator-test
              (let ([validator (make-validator even? "not even")])
                   (assert-true (validation-success? (run-validator validator 4)))
                   (assert-true (validation-failure? (run-validator validator 3))))))

;;; ============================================================
;;; Validator Combinator Tests
;;; ============================================================

(test-group validator-combinators
            (define-test validator-and-both-pass-test
              (let* ([v1 (make-validator number? "not a number")]
                     [v2 (make-validator positive? "not positive")]
                     [combined (validator-and v1 v2)]
                     [result (combined 5)])
                    (assert-true (validation-success? result))))
            
            (define-test validator-and-accumulates-test
              (let* ([v1 (make-validator number? "not a number")]
                     [v2 (make-validator (lambda (x) (and (number? x) (> x 10))) "too small")]
                     [combined (validator-and v1 v2)]
                     [result (combined "hello")])
                    (assert-true (validation-failure? result))
                    (assert-equal '("not a number" "too small") (from-failure result))))
            
            (define-test validator-all-test
              (let* ([validators (list
                                  (make-validator number? "not a number")
                                  (make-validator positive? "not positive")
                                  (make-validator even? "not even"))]
                     [combined (validator-all validators)]
                     [result (combined 4)])
                    (assert-true (validation-success? result))))
            
            (define-test validator-all-accumulates-test
              (let* ([validators (list
                                  (make-validator number? "not a number")
                                  (make-validator positive? "not positive")
                                  (make-validator even? "not even"))]
                     [combined (validator-all validators)]
                     [result (combined -3)])
                    (assert-true (validation-failure? result))
                    ;; -3 is a number, but not positive and not even
                    (assert-equal '("not positive" "not even") (from-failure result))))
            
            (define-test validator-or-first-passes-test
              (let* ([v1 (make-validator even? "not even")]
                     [v2 (make-validator (lambda (x) (= x 5)) "not 5")]
                     [combined (validator-or v1 v2)]
                     [result (combined 4)])
                    (assert-true (validation-success? result))))
            
            (define-test validator-or-second-passes-test
              (let* ([v1 (make-validator even? "not even")]
                     [v2 (make-validator (lambda (x) (= x 5)) "not 5")]
                     [combined (validator-or v1 v2)]
                     [result (combined 5)])
                    (assert-true (validation-success? result))))
            
            (define-test validator-map-error-test
              (let* ([v (make-validator even? "not even")]
                     [mapped (validator-map-error
                              (lambda (e) (string-append "Error: " e))
                              v)]
                     [result (mapped 3)])
                    (assert-true (validation-failure? result))
                    (assert-equal '("Error: not even") (from-failure result))))
            
            (define-test validator-focus-test
              (let* ([v (make-validator positive? "not positive")]
                     [focused (validator-focus car v)]
                     [result (focused '(5 . "data"))])
                    (assert-true (validation-success? result))
                    (assert-equal '(5 . "data") (from-success result)))))

;;; ============================================================
;;; Common Validators Tests
;;; ============================================================

(test-group common-validators
            (define-test required-passes-test
              (let ([v ((required "required") 42)])
                   (assert-true (validation-success? v))))
            
            (define-test required-fails-null-test
              (let ([v ((required "required") '())])
                   (assert-true (validation-failure? v))))
            
            (define-test required-fails-false-test
              (let ([v ((required "required") #f)])
                   (assert-true (validation-failure? v))))
            
            (define-test not-empty-passes-test
              (let ([v ((not-empty "empty") "hello")])
                   (assert-true (validation-success? v))))
            
            (define-test not-empty-fails-test
              (let ([v ((not-empty "empty") "")])
                   (assert-true (validation-failure? v))))
            
            (define-test min-length-passes-test
              (let ([v ((min-length 3 "too short") "hello")])
                   (assert-true (validation-success? v))))
            
            (define-test min-length-fails-test
              (let ([v ((min-length 3 "too short") "hi")])
                   (assert-true (validation-failure? v))))
            
            (define-test max-length-passes-test
              (let ([v ((max-length 5 "too long") "hi")])
                   (assert-true (validation-success? v))))
            
            (define-test max-length-fails-test
              (let ([v ((max-length 5 "too long") "hello world")])
                   (assert-true (validation-failure? v))))
            
            (define-test in-range-passes-test
              (let ([v ((in-range 0 10 "out of range") 5)])
                   (assert-true (validation-success? v))))
            
            (define-test in-range-fails-low-test
              (let ([v ((in-range 0 10 "out of range") -1)])
                   (assert-true (validation-failure? v))))
            
            (define-test in-range-fails-high-test
              (let ([v ((in-range 0 10 "out of range") 11)])
                   (assert-true (validation-failure? v))))
            
            (define-test positive-passes-test
              (let ([v ((positive "not positive") 5)])
                   (assert-true (validation-success? v))))
            
            (define-test positive-fails-test
              (let ([v ((positive "not positive") 0)])
                   (assert-true (validation-failure? v))))
            
            (define-test non-negative-passes-test
              (let ([v ((non-negative "negative") 0)])
                   (assert-true (validation-success? v))))
            
            (define-test non-negative-fails-test
              (let ([v ((non-negative "negative") -1)])
                   (assert-true (validation-failure? v))))
            
            (define-test is-string-passes-test
              (let ([v ((is-string "not string") "hello")])
                   (assert-true (validation-success? v))))
            
            (define-test is-string-fails-test
              (let ([v ((is-string "not string") 42)])
                   (assert-true (validation-failure? v))))
            
            (define-test is-number-passes-test
              (let ([v ((is-number "not number") 42)])
                   (assert-true (validation-success? v))))
            
            (define-test is-list-passes-test
              (let ([v ((is-list "not list") '(1 2 3))])
                   (assert-true (validation-success? v))))
            
            (define-test is-symbol-passes-test
              (let ([v ((is-symbol "not symbol") 'foo)])
                   (assert-true (validation-success? v))))
            
            (define-test one-of-passes-test
              (let ([v ((one-of '(a b c) "not in list") 'b)])
                   (assert-true (validation-success? v))))
            
            (define-test one-of-fails-test
              (let ([v ((one-of '(a b c) "not in list") 'd)])
                   (assert-true (validation-failure? v)))))

;;; ============================================================
;;; Field Validation Tests
;;; ============================================================

(test-group field-validation
            (define-test validate-field-passes-test
              (let* ([data '((name . "Alice") (age . 30))]
                     [v ((validate-field 'name (is-string "not string")) data)])
                    (assert-true (validation-success? v))))
            
            (define-test validate-field-fails-test
              (let* ([data '((name . 42) (age . 30))]
                     [v ((validate-field 'name (is-string "not string")) data)])
                    (assert-true (validation-failure? v))))
            
            (define-test validate-field-missing-test
              (let* ([data '((age . 30))]
                     [v ((validate-field 'name (is-string "not string")) data)])
                    (assert-true (validation-failure? v))))
            
            (define-test validate-optional-field-present-test
              (let* ([data '((name . "Alice"))]
                     [v ((validate-optional-field 'name (is-string "not string")) data)])
                    (assert-true (validation-success? v))))
            
            (define-test validate-optional-field-missing-test
              (let* ([data '((age . 30))]
                     [v ((validate-optional-field 'name (is-string "not string")) data)])
                    (assert-true (validation-success? v))))
            
            (define-test validate-fields-all-pass-test
              (let* ([data '((name . "Alice") (age . 30))]
                     [validators (list
                                  (cons 'name (is-string "name not string"))
                                  (cons 'age (is-number "age not number")))]
                     [v ((validate-fields validators) data)])
                    (assert-true (validation-success? v))))
            
            (define-test validate-fields-accumulates-test
              (let* ([data '((name . 42) (age . "thirty"))]
                     [validators (list
                                  (cons 'name (is-string "name not string"))
                                  (cons 'age (is-number "age not number")))]
                     [v ((validate-fields validators) data)])
                    (assert-true (validation-failure? v))
                    (assert-equal 2 (length (from-failure v))))))

;;; ============================================================
;;; List Validation Tests
;;; ============================================================

(test-group list-validation
            (define-test validate-each-passes-test
              (let* ([v ((validate-each (positive "not positive")) '(1 2 3))])
                    (assert-true (validation-success? v))
                    (assert-equal '(1 2 3) (from-success v))))
            
            (define-test validate-each-accumulates-test
              (let* ([v ((validate-each (positive "not positive")) '(1 -2 3 -4))])
                    (assert-true (validation-failure? v))
                    (assert-equal 2 (length (from-failure v)))))
            
            (define-test validate-at-passes-test
              (let* ([v ((validate-at 1 (is-string "not string")) '(1 "hello" 3))])
                    (assert-true (validation-success? v))))
            
            (define-test validate-at-fails-test
              (let* ([v ((validate-at 1 (is-string "not string")) '(1 42 3))])
                    (assert-true (validation-failure? v))))
            
            (define-test validate-at-out-of-bounds-test
              (let* ([v ((validate-at 10 (is-string "not string")) '(1 2 3))])
                    (assert-true (validation-failure? v)))))

;;; ============================================================
;;; Conditional Validation Tests
;;; ============================================================

(test-group conditional-validation
            (define-test validate-when-true-test
              (let* ([v ((validate-when number? (positive "not positive")) 5)])
                    (assert-true (validation-success? v))))
            
            (define-test validate-when-false-skips-test
              (let* ([v ((validate-when number? (positive "not positive")) "hello")])
                    ;; Should skip validation since "hello" is not a number
                    (assert-true (validation-success? v))))
            
            (define-test validate-unless-true-skips-test
              (let* ([v ((validate-unless null? (is-list "not list")) '())])
                    ;; Should skip validation since input is null
                    (assert-true (validation-success? v))))
            
            (define-test validate-unless-false-validates-test
              (let* ([v ((validate-unless null? (is-list "not list")) '(1 2 3))])
                    (assert-true (validation-success? v)))))

;;; ============================================================
;;; Error Formatting Tests
;;; ============================================================

(test-group error-formatting
            (define-test format-errors-success-test
              (assert-equal "" (format-errors (validation-success 42))))
            
            (define-test format-errors-failure-test
              (let ([formatted (format-errors (validation-failure "error1"))])
                   ;; Should contain the error
                   (assert-true (> (string-length formatted) 0))))
            
            (define-test with-field-prefix-success-test
              (let ([v (with-field-prefix 'name (validation-success 42))])
                   (assert-true (validation-success? v))))
            
            (define-test with-field-prefix-failure-test
              (let ([v (with-field-prefix 'name (validation-failure "invalid"))])
                   (assert-true (validation-failure? v))
                   (let ([err (car (from-failure v))])
                        (assert-equal 'name (car err))
                        (assert-equal "invalid" (cdr err))))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Validate a user record
            (define-test user-validation-success-test
              (let* ([user '((username . "alice") (age . 25))]
                     [username-validator
                      (validator-all
                       (list (is-string "Username must be a string")
                             (min-length 3 "Username too short")
                             (max-length 20 "Username too long")))]
                     [age-validator
                      (validator-all
                       (list (is-number "Age must be a number")
                             (in-range 0 150 "Age out of range")))]
                     [result
                      (validation-ap2
                       (lambda (name) (lambda (age) user))  ; curried
                       (with-field-prefix 'username
                                          (username-validator (cdr (assoc 'username user))))
                       (with-field-prefix 'age
                                          (age-validator (cdr (assoc 'age user)))))])
                    (assert-true (validation-success? result))))
            
            (define-test user-validation-failure-test
              (let* ([user '((username . "ab") (age . -5))]
                     [username-validator
                      (validator-all
                       (list (is-string "Username must be a string")
                             (min-length 3 "Username too short")
                             (max-length 20 "Username too long")))]
                     [age-validator
                      (validator-all
                       (list (is-number "Age must be a number")
                             (in-range 0 150 "Age out of range")))]
                     [result
                      (validation-ap2
                       (lambda (name) (lambda (age) user))  ; curried
                       (with-field-prefix 'username
                                          (username-validator (cdr (assoc 'username user))))
                       (with-field-prefix 'age
                                          (age-validator (cdr (assoc 'age user)))))])
                    (assert-true (validation-failure? result))
                    ;; Should have accumulated both errors
                    (assert-equal 2 (length (from-failure result)))))
            
            ;; Validate a list of numbers
            (define-test list-of-positive-numbers-test
              (let* ([numbers '(1 2 3 4 5)]
                     [result ((validate-each (positive "must be positive")) numbers)])
                    (assert-true (validation-success? result))
                    (assert-equal numbers (from-success result))))
            
            ;; Complex nested validation
            (define-test nested-validation-test
              (let* ([config '((server . ((host . "localhost") (port . 8080)))
                               (debug . #t))]
                     [server-data (cdr (assoc 'server config))]
                     [host-validator (is-string "host must be string")]
                     [port-validator (validator-all
                                      (list (is-number "port must be number")
                                            (in-range 1 65535 "port out of range")))]
                     [result
                      (validation-ap2
                       (lambda (h) (lambda (p) config))  ; curried
                       (host-validator (cdr (assoc 'host server-data)))
                       (port-validator (cdr (assoc 'port server-data))))])
                    (assert-true (validation-success? result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All validation tests passed.
")
    (display "
[FAILURE] Some validation tests failed.
"))
