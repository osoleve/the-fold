;;; test-parser-infinite-loop.ss — Test for infinite loop bug fix

(load "core/test-framework.ss")
(load "lattice/fp/parsing/parser.ss")

(display "
")
(display "====
")
(display "   PARSER INFINITE LOOP BUG FIX TESTS
")
(display "====
")

;;; ====
;;; Test Cases for Infinite Loop Bug
;;; ====

(test-group infinite-loop-tests
            ;; Test many with parser that succeeds without consuming input
            (define-test many-no-consume-test
              (let* ([p (parser-pure 42)]
                     [result (parse (many p) "hello")])
                    ;; Should not hang and should return empty list
                    ;; (since pure succeeds but doesn't consume, loop breaks immediately)
                    (assert-true (right? result))
                    (assert-equal '() (from-right result))))
            
            ;; Test some with parser that succeeds without consuming input
            (define-test some-no-consume-test
              (let* ([p (parser-pure 42)]
                     [result (parse (some p) "hello")])
                    ;; Should not hang and should return single element
                    ;; (first parse succeeds with 42, then many breaks on second attempt)
                    (assert-true (right? result))
                    (assert-equal '(42) (from-right result))))
            
            ;; Test many with optional that succeeds without consuming
            (define-test many-optional-test
              (let* ([p (optional (char (integer->char 97)) #\x)]  ; optional 'a'
                     [result (parse (many p) "bbb")])
                    ;; Should parse successfully
                    ;; First iteration: 'a' fails, returns default #\x (no consume)
                    ;; Loop should break to avoid infinite loop
                    (assert-true (right? result))
                    (assert-equal '() (from-right result))))
            
            ;; Test many with parser that consumes (should still work)
            (define-test many-normal-test
              (let* ([p digit]
                     [result (parse (many p) "123abc")])
                    ;; Should consume all digits
                    (assert-true (right? result))
                    (assert-equal 3 (length (from-right result)))))
            
            ;; Test some with parser that consumes (should still work)
            (define-test some-normal-test
              (let* ([p digit]
                     [result (parse (some p) "456xyz")])
                    ;; Should consume all digits
                    (assert-true (right? result))
                    (assert-equal 3 (length (from-right result)))))
            
            ;; Test many on empty input (should return empty list)
            (define-test many-empty-input-test
              (let* ([p digit]
                     [result (parse (many p) "")])
                    (assert-true (right? result))
                    (assert-equal '() (from-right result))))
            
            ;; Test some on empty input (should fail)
            (define-test some-empty-input-test
              (let* ([p digit]
                     [result (parse (some p) "")])
                    (assert-true (left? result))))
            
            ;; Test many with look-ahead (doesn't consume)
            (define-test many-lookahead-test
              (let* ([p (look-ahead (char (integer->char 97)))]  ; lookahead 'a'
                     [result (parse (many p) "aaa")])
                    ;; look-ahead succeeds but doesn't consume
                    ;; Should break immediately to avoid infinite loop
                    (assert-true (right? result))
                    (assert-equal '() (from-right result))))
            
            ;; Test nested many combinators
            (define-test nested-many-test
              (let* ([inner (many (parser-pure 1))]
                     [outer (many inner)]
                     [result (parse outer "test")])
                    ;; Both should break without hanging
                    (assert-true (right? result))
                    ;; Outer many gets one result from inner many (empty list)
                    ;; Then outer many tries again, inner succeeds again with empty list
                    ;; But outer many sees no input consumed, so breaks
                    (assert-equal '() (from-right result)))))

;;; ====
;;; Regression Tests (ensure normal behavior still works)
;;; ====

(test-group regression-tests
            ;; Test spaces combinator (uses many)
            (define-test spaces-test
              (let ([result (parse spaces "   abc")])
                   (assert-true (right? result))
                   (assert-equal "   " (from-right result))))
            
            ;; Test spaces1 combinator (uses some)
            (define-test spaces1-test
              (let ([result (parse spaces1 "  xyz")])
                   (assert-true (right? result))
                   (assert-equal "  " (from-right result))))
            
            ;; Test identifier (uses many)
            (define-test identifier-test
              (let ([result (parse identifier "foo123")])
                   (assert-true (right? result))
                   (assert-equal "foo123" (from-right result))))
            
            ;; Test natural number (uses some)
            (define-test natural-test
              (let ([result (parse natural "42abc")])
                   (assert-true (right? result))
                   (assert-equal 42 (from-right result))))
            
            ;; Test sep-by (uses many internally)
            (define-test sep-by-test
              (let* ([p (sep-by digit (char (integer->char 44)))]  ; comma
                     [result (parse p "1,2,3,4")])
                    (assert-true (right? result))
                    (assert-equal 4 (length (from-right result))))))

;;; ====
;;; Summary
;;; ====

(display "
")
(display "====
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All infinite loop bug fix tests passed.
")
    (display "
[FAILURE] Some infinite loop bug fix tests failed.
"))
