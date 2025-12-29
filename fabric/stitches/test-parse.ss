;;; fabric/stitches/test-parse.ss — Test Vectors for Parser Combinators
;;;
;;; Comprehensive tests for the parser combinator library.
;;; Tests cover primitives, combinators, and example parsers.
;;;
;;; This is Core test code: verifies pure functionality.

;;; Load the parser library
(load "fabric/stitches/parse.ss")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define (assert-true condition msg)
  (if condition
      #t
      (begin
       (display "FAIL: ")
       (display msg)
       (newline)
       #f)))

(define (assert-equal actual expected msg)
  (if (equal? actual expected)
      #t
      (begin
       (display "FAIL: ")
       (display msg)
       (newline)
       (display "  Expected: ")
       (display expected)
       (newline)
       (display "  Actual:   ")
       (display actual)
       (newline)
       #f)))

(define (assert-parse-ok result msg)
  (assert-true (parse-ok? result) msg))

(define (assert-parse-err result msg)
  (assert-true (parse-err? result) msg))

;;; ============================================================
;;; ParseResult Tests
;;; ============================================================

(define (test-parse-result)
  (display "Testing ParseResult construction...")
  (newline)
  
  ;; Success result
  (let ([result (success 42 "remaining")])
       (assert-parse-ok result "success creates ok result")
       (assert-equal (result-value result) 42 "success value")
       (assert-equal (result-remaining result) "remaining" "success remaining"))
  
  ;; Failure result
  (let ([result (failure "expected char" 5)])
       (assert-parse-err result "failure creates err result")
       (assert-equal (result-expected result) "expected char" "failure expected")
       (assert-equal (result-position result) 5 "failure position"))
  
  (display "ParseResult tests complete.")
  (newline))

;;; ============================================================
;;; Primitive Parser Tests
;;; ============================================================

(define (test-primitives)
  (display "Testing primitive parsers...")
  (newline)
  
  ;; pure always succeeds
  (let ([result (run-parser (pure 42) "hello")])
       (assert-parse-ok result "pure succeeds")
       (assert-equal (result-value result) 42 "pure returns value")
       (assert-equal (result-remaining result) "hello" "pure consumes nothing"))
  
  ;; fail always fails
  (let ([result (run-parser (fail "error message") "hello")])
       (assert-parse-err result "fail fails")
       (assert-equal (result-expected result) "error message" "fail message"))
  
  ;; item consumes one character
  (let ([result (run-parser item "abc")])
       (assert-parse-ok result "item succeeds on non-empty")
       (assert-equal (result-value result) # "item returns first char")
       (assert-equal (result-remaining result) "bc" "item consumes one char"))
  
  ;; item fails on empty
  (let ([result (run-parser item "")])
       (assert-parse-err result "item fails on empty"))
  
  ;; eof succeeds on empty
  (let ([result (run-parser eof "")])
       (assert-parse-ok result "eof succeeds on empty"))
  
  ;; eof fails on non-empty
  (let ([result (run-parser eof "abc")])
       (assert-parse-err result "eof fails on non-empty"))
  
  ;; satisfy with matching predicate
  (let ([result (run-parser (satisfy char-numeric? "digit") "5abc")])
       (assert-parse-ok result "satisfy succeeds on match")
       (assert-equal (result-value result) # "satisfy returns matching char"))
  
  ;; satisfy with non-matching predicate
  (let ([result (run-parser (satisfy char-numeric? "digit") "abc")])
       (assert-parse-err result "satisfy fails on no match"))
  
  (display "Primitive parser tests complete.")
  (newline))

;;; ============================================================
;;; Character Parser Tests
;;; ============================================================

(define (test-char-parsers)
  (display "Testing character parsers...")
  (newline)
  
  ;; char matches specific character
  (let ([result (run-parser (char #) "abc")])
       (assert-parse-ok result "char succeeds on match")
       (assert-equal (result-value result) # "char returns matched char")
       (assert-equal (result-remaining result) "bc" "char consumes one char"))
  
  ;; char fails on mismatch
  (let ([result (run-parser (char #) "xyz")])
       (assert-parse-err result "char fails on mismatch"))
  
  ;; digit matches digit
  (let ([result (run-parser digit "3abc")])
       (assert-parse-ok result "digit matches digit")
       (assert-equal (result-value result) # "digit value"))
  
  ;; digit fails on non-digit
  (let ([result (run-parser digit "abc")])
       (assert-parse-err result "digit fails on non-digit"))
  
  ;; alpha matches letter
  (let ([result (run-parser alpha "abc")])
       (assert-parse-ok result "alpha matches letter")
       (assert-equal (result-value result) # "alpha value"))
  
  ;; space matches whitespace
  (let ([result (run-parser space " abc")])
       (assert-parse-ok result "space matches whitespace")
       (assert-equal (result-value result) #\space "space value"))
  
  ;; string-p matches exact string
  (let ([result (run-parser (string-p "hello") "hello world")])
       (assert-parse-ok result "string-p matches exact string")
       (assert-equal (result-value result) "hello" "string-p value")
       (assert-equal (result-remaining result) " world" "string-p remaining"))
  
  ;; string-p fails on mismatch
  (let ([result (run-parser (string-p "hello") "goodbye")])
       (assert-parse-err result "string-p fails on mismatch"))
  
  ;; string-p fails on partial match
  (let ([result (run-parser (string-p "hello") "hel")])
       (assert-parse-err result "string-p fails on too-short input"))
  
  (display "Character parser tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Sequencing
;;; ============================================================

(define (test-sequencing)
  (display "Testing sequencing combinators...")
  (newline)
  
  ;; bind threads results
  (let ([parser (bind (char #) (lambda (c) (pure (char-upcase c))))])
       (let ([result (run-parser parser "abc")])
            (assert-parse-ok result "bind succeeds")
            (assert-equal (result-value result) #\A "bind threads value")))
  
  ;; bind propagates failure
  (let ([parser (bind (char #) (lambda (c) (fail "error")))])
       (let ([result (run-parser parser "abc")])
            (assert-parse-err result "bind propagates failure from second parser")))
  
  ;; seq keeps second result
  (let ([parser (seq (char #) (char #))])
       (let ([result (run-parser parser "abc")])
            (assert-parse-ok result "seq succeeds")
            (assert-equal (result-value result) # "seq keeps second value")))
  
  ;; seq-left keeps first result
  (let ([parser (seq-left (char #) (char #))])
       (let ([result (run-parser parser "abc")])
            (assert-parse-ok result "seq-left succeeds")
            (assert-equal (result-value result) # "seq-left keeps first value")))
  
  (display "Sequencing combinator tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Choice
;;; ============================================================

(define (test-choice)
  (display "Testing choice combinators...")
  (newline)
  
  ;; alt tries first, succeeds
  (let ([parser (alt (char #) (char #))])
       (let ([result (run-parser parser "abc")])
            (assert-parse-ok result "alt succeeds on first parser")
            (assert-equal (result-value result) # "alt value from first")))
  
  ;; alt tries second on first failure
  (let ([parser (alt (char #) (char #))])
       (let ([result (run-parser parser "bac")])
            (assert-parse-ok result "alt succeeds on second parser")
            (assert-equal (result-value result) # "alt value from second")))
  
  ;; alt fails if both fail
  (let ([parser (alt (char #) (char #))])
       (let ([result (run-parser parser "xyz")])
            (assert-parse-err result "alt fails when both fail")))
  
  ;; choice tries multiple alternatives
  (let ([parser (choice (list (char #) (char #) (char #