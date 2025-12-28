;;; explore-repl-pt2.ss — Further exploration of Fold features

(load "thimble/repl.ss")
(load "thimble/exploration-error-handler.ss")  ; Enhanced error formatting

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║       THE FOLD — EXPLORATION PART 2 (ADVANCED)             ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

;;; Test 1: Format strings
(display "TEST 1: Format String Handling\n")
(display "  (format \"Number: ~a\" 42) => ")
(guard (e (else (display "[ERROR] ")))
  (display (format "Number: ~a" 42)))
(display "\n")

;;; Test 2: Display with newlines
(display "\nTEST 2: Display vs Write\n")
(display "  Using display: ")
(display '(1 2 3))
(display " | ")
(write '(1 2 3))
(display "\n")

;;; Test 3: Vector operations
(display "\nTEST 3: Vector Operations\n")
(display "  (vector 1 2 3) => ")
(guard (e (else (display "[VECTORS NOT SUPPORTED]")))
  (display (vector 1 2 3)))
(display "\n")

;;; Test 4: String indexing edge case
(display "\nTEST 4: String Index Boundary\n")
(display "  (string-length \"abc\") => ")
(display (string-length "abc"))
(display "\n")
(display "  Attempting (string-ref \"abc\" 10)...\n")
(guard (e (else
  (display "    [CAUGHT ERROR] ")
  (display (format-exploration-error e))
  (display "\n")))
  (display "    Result: ")
  (display (string-ref "abc" 10))
  (display "\n"))

;;; Test 5: List operations edge cases
(display "\nTEST 5: List Edge Cases\n")
(display "  (car '()) on empty list...\n")
(guard (e (else
  (display "    [CAUGHT ERROR] ")
  (display (format-exploration-error e))
  (display "\n")))
  (display "    Result: ")
  (display (car '()))
  (display "\n"))

;;; Test 6: Define and redefine variables
(display "\nTEST 6: Variable Definition\n")
(define test-var 42)
(display "  (define test-var 42) => ")
(display test-var)
(display "\n")
(define test-var 99)
(display "  (define test-var 99) [redefinition] => ")
(display test-var)
(display "\n")

;;; Test 7: Comments
(display "\nTEST 7: Comment Handling\n")
(display "  ; This is a comment\n")
(display "  (+ 1 2) => ")
(display (+ 1 2))
(display "\n")

;;; Test 8: Boolean evaluation
(display "\nTEST 8: Truthiness\n")
(display "  (if 0 \"zero is truthy\" \"zero is falsy\") => ")
(display (if 0 "zero is truthy" "zero is falsy"))
(display "\n")
(display "  (if \"\" \"empty string is truthy\" \"empty string is falsy\") => ")
(display (if "" "empty string is truthy" "empty string is falsy"))
(display "\n")
(display "  (if '() \"empty list is truthy\" \"empty list is falsy\") => ")
(display (if '() "empty list is truthy" "empty list is falsy"))
(display "\n")

;;; Test 9: Lambda and call-with-current-continuation
(display "\nTEST 9: Continuation Capture\n")
(display "  Testing call/cc...\n")
(guard (e (else (display "    [NOT AVAILABLE or ERROR]")))
  (let ((result (call/cc (lambda (k) (k 42)))))
    (display "    call/cc result => ")
    (display result)
    (display "\n")))

;;; Test 10: Numeric comparisons
(display "\nTEST 10: Numeric Comparisons\n")
(display "  (< 1 2 3) => ")
(guard (e (else (display "[ERROR]")))
  (display (< 1 2 3)))
(display "\n")
(display "  (> 5 3 1) => ")
(guard (e (else (display "[ERROR]")))
  (display (> 5 3 1)))
(display "\n")

;;; Test 11: Case expressions
(display "\nTEST 11: Case Expressions\n")
(display "  (case 2 ((1) 'one) ((2) 'two) (else 'other)) => ")
(guard (e (else (display "[ERROR]")))
  (display (case 2 ((1) 'one) ((2) 'two) (else 'other))))
(display "\n")

;;; Test 12: Cond expressions
(display "\nTEST 12: Cond Expressions\n")
(display "  (cond ((= 1 2) 'a) ((= 1 1) 'b) (else 'c)) => ")
(display (cond ((= 1 2) 'a) ((= 1 1) 'b) (else 'c)))
(display "\n")

;;; Test 13: String/number coercion
(display "\nTEST 13: Type Coercion\n")
(display "  (+ \"5\" 3) - attempting string + number...\n")
(guard (e (else
  (display "    [CAUGHT ERROR] ")
  (display (format-exploration-error e))
  (display "\n")))
  (display "    Result: ")
  (display (+ "5" 3))
  (display "\n"))

;;; Test 14: Negative numbers and zero
(display "\nTEST 14: Negative Numbers\n")
(display "  (- 5) => ")
(display (- 5))
(display "\n")
(display "  (abs -10) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
  (display (abs -10)))
(display "\n")

;;; Test 15: Modulo/remainder
(display "\nTEST 15: Modulo Operations\n")
(display "  (modulo 10 3) => ")
(guard (e (else (display "[ERROR]")))
  (display (modulo 10 3)))
(display "\n")
(display "  (remainder 10 3) => ")
(guard (e (else (display "[ERROR]")))
  (display (remainder 10 3)))
(display "\n")

;;; Test 16: Exponentiation
(display "\nTEST 16: Power Operation\n")
(display "  (expt 2 10) => ")
(guard (e (else (display "[ERROR]")))
  (display (expt 2 10)))
(display "\n")

;;; Test 17: Square root and other math
(display "\nTEST 17: Math Functions\n")
(display "  (sqrt 16) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
  (display (sqrt 16)))
(display "\n")

;;; Test 18: Min/max
(display "\nTEST 18: Min/Max Functions\n")
(display "  (min 5 3 8 1) => ")
(guard (e (else (display "[ERROR]")))
  (display (min 5 3 8 1)))
(display "\n")
(display "  (max 5 3 8 1) => ")
(guard (e (else (display "[ERROR]")))
  (display (max 5 3 8 1)))
(display "\n")

;;; Test 19: List reversal
(display "\nTEST 19: List Reversal\n")
(display "  (reverse '(1 2 3 4)) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
  (display (reverse '(1 2 3 4))))
(display "\n")

;;; Test 20: String to list conversion
(display "\nTEST 20: String/List Conversion\n")
(display "  (string->list \"abc\") => ")
(guard (e (else (display "[NOT AVAILABLE]")))
  (display (string->list "abc")))
(display "\n")

;;; Test 21: Assoc/member
(display "\nTEST 21: Association Lists\n")
(display "  (assoc 'b '((a 1) (b 2) (c 3))) => ")
(guard (e (else (display "[NOT AVAILABLE or ERROR]")))
  (display (assoc 'b '((a 1) (b 2) (c 3)))))
(display "\n")

;;; Test 22: Null vs empty list
(display "\nTEST 22: Null Checking\n")
(display "  (null? '()) => ")
(display (null? '()))
(display "\n")
(display "  (null? 0) => ")
(display (null? 0))
(display "\n")
(display "  (pair? '(1)) => ")
(display (pair? '(1)))
(display "\n")
(display "  (pair? '()) => ")
(display (pair? '()))
(display "\n")

;;; Test 23: eval and quote interaction
(display "\nTEST 23: Eval Functionality\n")
(display "  (eval '(+ 1 2)) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
  (display (eval '(+ 1 2))))
(display "\n")

;;; Test 24: Multiple return values
(display "\nTEST 24: Values and call-with-values\n")
(display "  Testing (values 1 2 3)...\n")
(guard (e (else 
  (display "    [NOT AVAILABLE or ERROR] ")
  (display (format-exploration-error e))))
  (display "    Result: ")
  (display (values 1 2 3))
  (display "\n"))

;;; Test 25: Immutability check
(display "\nTEST 25: List Immutability\n")
(define test-list '(1 2 3))
(display "  Original list: ")
(display test-list)
(display "\n")
(display "  (Attempting to modify with set-car! [if available])...\n")
(guard (e (else (display "    [NOT AVAILABLE or ERROR]")))
  (set-car! test-list 99)
  (display "    Modified list: ")
  (display test-list)
  (display "\n"))

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║            PART 2 EXPLORATION COMPLETE                     ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n")
