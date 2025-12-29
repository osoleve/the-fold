;;; explore-repl.ss — Comprehensive exploration of the Fold
;;;
;;; This script tests various features and reports any sharp edges found

(load "thimble/repl.ss")
(load "thimble/exploration-error-handler.ss")  ; Enhanced error formatting

(display "╔════════════════════════════════════════════════════════════╗\n")
(display "║         THE FOLD — COMPREHENSIVE EXPLORATION               ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

;;; Test 1: Basic arithmetic
(display "TEST 1: Basic Arithmetic\n")
(display "  (+ 1 2) => ")
(display (+ 1 2))
(display "\n")
(display "  (- 10 3) => ")
(display (- 10 3))
(display "\n")
(display "  (* 4 5) => ")
(display (* 4 5))
(display "\n")
(display "  (/ 20 4) => ")
(display (/ 20 4))
(display "\n")

;;; Test 2: Division by zero
(display "\nTEST 2: Error Handling - Division by Zero\n")
(display "  Attempting (/ 5 0)...\n")
(guard (e (else
           (display "  [CAUGHT ERROR] ")
           (display (format-exploration-error e))
           (display "\n")))
       (display "  Result: ")
       (display (/ 5 0))
       (display "\n"))

;;; Test 3: String operations
(display "\nTEST 3: String Operations\n")
(display "  (string-append \"hello\" \" \" \"world\") => ")
(display (string-append "hello" " " "world"))
(display "\n")
(display "  (string-length \"Fold\") => ")
(display (string-length "Fold"))
(display "\n")
(display "  (string-ref \"Fold\" 0) => ")
(display (string-ref "Fold" 0))
(display "\n")

;;; Test 4: List operations
(display "\nTEST 4: List Operations\n")
(display "  (car '(1 2 3)) => ")
(display (car '(1 2 3)))
(display "\n")
(display "  (cdr '(1 2 3)) => ")
(display (cdr '(1 2 3)))
(display "\n")
(display "  (length '(a b c d)) => ")
(display (length '(a b c d)))
(display "\n")
(display "  (append '(1 2) '(3 4)) => ")
(display (append '(1 2) '(3 4)))
(display "\n")

;;; Test 5: Higher-order functions
(display "\nTEST 5: Higher-Order Functions\n")
(display "  (map (lambda (x) (* x 2)) '(1 2 3)) => ")
(display (map (lambda (x) (* x 2)) '(1 2 3)))
(display "\n")
(display "  (filter (lambda (x) (> x 2)) '(1 2 3 4 5)) => ")
(display (filter (lambda (x) (> x 2)) '(1 2 3 4 5)))
(display "\n")

;;; Test 6: Symbol operations
(display "\nTEST 6: Symbol Operations\n")
(display "  (symbol? 'hello) => ")
(display (symbol? 'hello))
(display "\n")
(display "  (symbol->string 'foo) => ")
(display (symbol->string 'foo))
(display "\n")
(display "  (string->symbol \"bar\") => ")
(display (string->symbol "bar"))
(display "\n")

;;; Test 7: Recursive functions
(display "\nTEST 7: Recursive Functions\n")
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(display "  (factorial 5) => ")
(display (factorial 5))
(display "\n")

;;; Test 8: Variable scoping and closures
(display "\nTEST 8: Closures and Scoping\n")
(define (make-adder x)
  (lambda (y) (+ x y)))
(define add5 (make-adder 5))
(display "  (add5 3) where add5=(make-adder 5) => ")
(display (add5 3))
(display "\n")

;;; Test 9: Deeply nested data structures
(display "\nTEST 9: Nested Data Structures\n")
(define deep '(((a (b (c (d)))))))
(display "  Deep structure: '(((a (b (c (d)))))) => ")
(display deep)
(display "\n")

;;; Test 10: Empty list handling
(display "\nTEST 10: Edge Cases with Empty Lists\n")
(display "  (null? '()) => ")
(display (null? '()))
(display "\n")
(display "  (length '()) => ")
(display (length '()))
(display "\n")
(display "  (append '() '(1 2)) => ")
(display (append '() '(1 2)))
(display "\n")

;;; Test 11: Type checking
(display "\nTEST 11: Type Checking Predicates\n")
(display "  (number? 42) => ")
(display (number? 42))
(display "\n")
(display "  (string? \"hello\") => ")
(display (string? "hello"))
(display "\n")
(display "  (list? '(1 2 3)) => ")
(display (list? '(1 2 3)))
(display "\n")
(display "  (procedure? +) => ")
(display (procedure? +))
(display "\n")

;;; Test 12: Large number handling
(display "\nTEST 12: Large Numbers\n")
(display "  (+ 999999999999 1) => ")
(display (+ 999999999999 1))
(display "\n")
(display "  (* 1000 1000 1000) => ")
(display (* 1000 1000 1000))
(display "\n")

;;; Test 13: Floating point edge cases
(display "\nTEST 13: Floating Point\n")
(display "  (/ 1 3) => ")
(display (/ 1 3))
(display "\n")
(display "  (+ 0.1 0.2) => ")
(display (+ 0.1 0.2))
(display "\n")
(display "  (= (+ 0.1 0.2) 0.3) => ")
(display (= (+ 0.1 0.2) 0.3))
(display "\n")

;;; Test 14: Quoting and unquoting
(display "\nTEST 14: Quoting Mechanics\n")
(display "  'hello => ")
(display 'hello)
(display "\n")
(display "  '(1 2 3) => ")
(display '(1 2 3))
(display "\n")
(let ((x 5))
     (display "  `(a ,x b) where x=5 => ")
     (display `(a ,x b))
     (display "\n"))

;;; Test 15: Multiple value returns (attempt)
(display "\nTEST 15: Control Flow - Let Bindings\n")
(let ((a 1) (b 2) (c 3))
     (display "  (let ((a 1) (b 2) (c 3)) (+ a b c)) => ")
     (display (+ a b c))
     (display "\n"))

;;; Test 16: Map and reduce patterns
(display "\nTEST 16: Fold/Reduce Operations\n")
(display "  (foldr + 0 '(1 2 3 4)) => ")
(display (foldr + 0 '(1 2 3 4)))
(display "\n")

;;; Test 17: Conditionals and booleans
(display "\nTEST 17: Boolean Logic\n")
(display "  (and #t #t) => ")
(display (and #t #t))
(display "\n")
(display "  (or #f #t) => ")
(display (or #f #t))
(display "\n")
(display "  (not #f) => ")
(display (not #f))
(display "\n")

;;; Test 18: Assertion with edge cases
(display "\nTEST 18: Comparing Different Types\n")
(display "  (= 5 5) => ")
(display (= 5 5))
(display "\n")
(display "  (equal? '(1 2) '(1 2)) => ")
(display (equal? '(1 2) '(1 2)))
(display "\n")

;;; Test 19: Variable reassignment/mutation
(display "\nTEST 19: Testing set!\n")
(let ((x 1))
     (set! x 2)
     (display "  After (set! x 2), x => ")
     (display x)
     (display "\n"))

;;; Test 20: String edge cases
(display "\nTEST 20: String Edge Cases\n")
(display "  Empty string length: (string-length \"\") => ")
(display (string-length ""))
(display "\n")
(display "  (string-upcase \"hello\") => ")
(catch (c (else
           (display "[Function not available]")))
       (display (string-upcase "hello")))
(display "\n")

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║              EXPLORATION COMPLETE                           ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n")
