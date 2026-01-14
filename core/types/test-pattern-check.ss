;;; core/types/test-pattern-check.ss — Tests for Pattern Exhaustiveness
;;;
;;; Tests for the pattern match analysis module.

(load "core/base/prelude.ss")
(load "core/types/pattern-check.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

;;; ====
;;; Pattern Parsing Tests
;;; ====

(define (run-parse-tests)
  (display "\n--- Pattern Parsing Tests ---\n")
  
  ;; Wildcard
  (let ([p (parse-pattern '_)])
       (test-true "wildcard pattern" (wildcard-pattern? p)))
  
  ;; Variable
  (let ([p (parse-pattern 'x)])
       (test-true "variable pattern" (var-pattern? p))
       (test "variable name" 'x (var-pattern-name p)))
  
  ;; Boolean literals
  (let ([p (parse-pattern 'true)])
       (test-true "true pattern" (ctor-pattern? p))
       (test "true tag" 'true (ctor-pattern-tag p)))
  
  (let ([p (parse-pattern 'false)])
       (test-true "false pattern" (ctor-pattern? p))
       (test "false tag" 'false (ctor-pattern-tag p)))
  
  ;; Number literal
  (let ([p (parse-pattern 42)])
       (test-true "number literal" (literal-pattern? p))
       (test "literal value" 42 (literal-pattern-value p)))
  
  ;; Constructor pattern
  (let ([p (parse-pattern '(cons x xs))])
       (test-true "cons pattern" (ctor-pattern? p))
       (test "cons tag" 'cons (ctor-pattern-tag p))
       (test "cons arity" 2 (length (ctor-pattern-subpats p))))
  
  ;; Nil pattern
  (let ([p (parse-pattern 'nil)])
       (test-true "nil pattern" (ctor-pattern? p))
       (test "nil tag" 'nil (ctor-pattern-tag p))))

;;; ====
;;; Exhaustiveness Tests
;;; ====

(define (run-exhaustiveness-tests)
  (display "\n--- Exhaustiveness Tests ---\n")
  
  ;; Complete Bool match
  (let* ([patterns '(true false)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list bool-type-info))])
        (test "complete Bool match" '() missing))
  
  ;; Incomplete Bool match (missing false)
  (let* ([patterns '(true)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list bool-type-info))])
        (test-true "incomplete Bool match has missing" (pair? missing)))
  
  ;; Complete List match
  (let* ([patterns '(nil (cons x xs))]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list list-type-info))])
        (test "complete List match" '() missing))
  
  ;; Incomplete List match (missing cons)
  (let* ([patterns '(nil)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list list-type-info))])
        (test-true "incomplete List match has missing" (pair? missing)))
  
  ;; Wildcard covers everything
  (let* ([patterns '(_)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list bool-type-info))])
        (test "wildcard is exhaustive" '() missing))
  
  ;; Variable covers everything
  (let* ([patterns '(x)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list bool-type-info))])
        (test "variable is exhaustive" '() missing))
  
  ;; Maybe type
  (let* ([patterns '(nothing (just x))]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list maybe-type-info))])
        (test "complete Maybe match" '() missing)))

;;; ====
;;; Redundancy Tests
;;; ====

(define (run-redundancy-tests)
  (display "\n--- Redundancy Tests ---\n")
  
  ;; No redundancy in complete match
  (let* ([patterns '(true false)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [redundant (find-redundant parsed (list bool-type-info))])
        (test "no redundancy in complete Bool" '() redundant))
  
  ;; Redundant wildcard after complete match
  (let* ([patterns '(true false _)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [redundant (find-redundant parsed (list bool-type-info))])
        (test "redundant wildcard" '(2) redundant))
  
  ;; Redundant duplicate pattern
  (let* ([patterns '(true true false)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [redundant (find-redundant parsed (list bool-type-info))])
        (test "redundant duplicate" '(1) redundant))
  
  ;; Redundant pattern after wildcard
  (let* ([patterns '(_ true false)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [redundant (find-redundant parsed (list bool-type-info))])
        (test "patterns after wildcard" '(1 2) redundant))
  
  ;; No redundancy with nested patterns
  (let* ([patterns '(nil (cons x nil) (cons x (cons y ys)))]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [redundant (find-redundant parsed (list list-type-info))])
        (test "no redundancy in List patterns" '() redundant)))

;;; ====
;;; Usefulness Tests
;;; ====

(define (run-usefulness-tests)
  (display "\n--- Usefulness Tests ---\n")
  
  ;; First pattern is always useful
  (let* ([row (list (parse-pattern 'true))]
         [matrix '()])
        (test-true "first pattern useful" (useful? row matrix (list bool-type-info))))
  
  ;; Pattern after wildcard not useful
  (let* ([row (list (parse-pattern 'true))]
         [matrix (list (list (parse-pattern '_)))])
        (test-false "pattern after wildcard not useful"
                    (useful? row matrix (list bool-type-info))))
  
  ;; Different constructor is useful
  (let* ([row (list (parse-pattern 'false))]
         [matrix (list (list (parse-pattern 'true)))])
        (test-true "different ctor useful" (useful? row matrix (list bool-type-info))))
  
  ;; Same constructor not useful
  (let* ([row (list (parse-pattern 'true))]
         [matrix (list (list (parse-pattern 'true)))])
        (test-false "same ctor not useful" (useful? row matrix (list bool-type-info)))))

;;; ====
;;; High-Level API Tests
;;; ====

(define (run-api-tests)
  (display "\n--- High-Level API Tests ---\n")
  
  ;; check-patterns complete
  (let* ([result (check-patterns '(true false) 'Bool)]
         [exhaustive (cdr (assq 'exhaustive result))]
         [missing (cdr (assq 'missing result))]
         [redundant (cdr (assq 'redundant result))])
        (test-true "check-patterns exhaustive" exhaustive)
        (test "check-patterns no missing" '() missing)
        (test "check-patterns no redundant" '() redundant))
  
  ;; check-patterns incomplete
  (let* ([result (check-patterns '(true) 'Bool)]
         [exhaustive (cdr (assq 'exhaustive result))])
        (test-false "check-patterns incomplete" exhaustive))
  
  ;; check-patterns with redundancy
  (let* ([result (check-patterns '(true false _) 'Bool)]
         [redundant (cdr (assq 'redundant result))])
        (test "check-patterns redundant" '(2) redundant)))

;;; ====
;;; Type Registration Tests
;;; ====

(define (run-registration-tests)
  (display "\n--- Type Registration Tests ---\n")
  
  ;; Register custom type
  (register-type! 'Color '((red . 0) (green . 0) (blue . 0)))
  (let ([info (lookup-type-info 'Color)])
       (test-true "Color type registered" (type-info? info))
       (test "Color has 3 ctors" 3 (length (type-constructors info))))
  
  ;; Check patterns against custom type
  (let* ([result (check-patterns '(red green blue) 'Color)]
         [exhaustive (cdr (assq 'exhaustive result))])
        (test-true "Color patterns exhaustive" exhaustive))
  
  (let* ([result (check-patterns '(red green) 'Color)]
         [exhaustive (cdr (assq 'exhaustive result))])
        (test-false "incomplete Color patterns" exhaustive)))

;;; ====
;;; Run All Tests
;;; ====

(define (run-tests)
  (display "\n=== Pattern Check Test Suite ===\n\n")
  
  (run-parse-tests)
  (run-exhaustiveness-tests)
  (run-redundancy-tests)
  (run-usefulness-tests)
  (run-api-tests)
  (run-registration-tests)
  
  (display "\n=== Test Summary ===\n")
  (display "Total: ") (display *test-count*)
  (display ", Passed: ") (display *pass-count*)
  (display ", Failed: ") (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!\n")
      (display "Some tests failed.\n"))
  
  (= *fail-count* 0))

;;; Run tests when loaded
(run-tests)
