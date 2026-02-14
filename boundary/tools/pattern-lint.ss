;;; boundary/tools/pattern-lint.ss — Pattern Match Linting for REPL
;;;
;;; REPL commands for pattern match analysis:
;;;   (check-match expr type) — analyze case expression
;;;   (missing-patterns patterns type) — find uncovered cases
;;;   (redundant-patterns patterns type) — find unreachable patterns
;;;   (suggest-patterns type) — generate complete pattern set
;;;
;;; This is Shell code: provides REPL integration.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - pattern-check.ss

(load "core/base/prelude.ss")
(load "core/types/pattern-check.ss")

;;; ====
;;; REPL Commands
;;; ====

;;; check-match : Expr × Symbol → Void
;;; Analyze a case expression for exhaustiveness and redundancy.
(define (check-match expr type-name)
  (let ([result (check-case-expr expr type-name)])
       (display "\n")
       (display "  Pattern Match Analysis\n")
       (display "  --------------------------------------------\n")
       (cond
        [(assq 'error result)
         (printf "  Error: ~a\n" (cdr (assq 'error result)))]
        [else
         (let ([exhaustive (cdr (assq 'exhaustive result))]
               [missing (cdr (assq 'missing result))]
               [redundant (cdr (assq 'redundant result))]
               [count (cdr (assq 'clause-count result))])
              (printf "  Scrutinee type: ~a\n" type-name)
              (printf "  Clause count: ~a\n" count)
              (display "\n")
              
              ;; Exhaustiveness
              (if exhaustive
                  (display "  ✓ Patterns are exhaustive.\n")
                  (begin
                   (display "  ✗ Patterns are NOT exhaustive.\n")
                   (display "    Missing patterns:\n")
                   (for-each
                    (lambda (p)
                            (printf "      ~s\n" p))
                    missing)))
              
              ;; Redundancy
              (if (null? redundant)
                  (display "  ✓ No redundant patterns.\n")
                  (begin
                   (display "  ✗ Redundant patterns found.\n")
                   (printf "    Redundant clause indices: ~a\n" redundant)
                   (display "    (These clauses will never match)\n"))))])
       (display "\n")))

;;; missing-patterns : (List SExpr) × Symbol → Void
;;; Show patterns that would be needed to make the match exhaustive.
(define (missing-patterns patterns type-name)
  (let* ([result (check-patterns patterns type-name)]
         [missing (cdr (assq 'missing result))])
        (display "\n")
        (printf "  Missing Patterns for ~a\n" type-name)
        (display "  --------------------------------------------\n")
        (if (null? missing)
            (display "  ✓ Patterns are exhaustive - no missing cases.\n")
            (begin
             (printf "  ✗ Found ~a missing pattern(s):\n" (length missing))
             (for-each
              (lambda (p)
                      (printf "    ~s\n" p))
              missing)))
        (display "\n")))

;;; redundant-patterns : (List SExpr) × Symbol → Void
;;; Show which patterns are redundant (will never match).
(define (redundant-patterns patterns type-name)
  (let* ([result (check-patterns patterns type-name)]
         [redundant (cdr (assq 'redundant result))])
        (display "\n")
        (printf "  Redundant Patterns for ~a\n" type-name)
        (display "  --------------------------------------------\n")
        (if (null? redundant)
            (display "  ✓ No redundant patterns found.\n")
            (begin
             (printf "  ✗ Found ~a redundant pattern(s):\n" (length redundant))
             (display "    Indices (0-based): ")
             (display redundant)
             (display "\n")
             (display "\n    Redundant patterns:\n")
             (for-each
              (lambda (idx)
                      (printf "      [~a] ~s\n" idx (list-ref patterns idx)))
              redundant)))
        (display "\n")))

;;; suggest-patterns : Symbol → Void
;;; Generate a complete set of patterns for a type.
(define (suggest-patterns type-name)
  (let ([type-info (lookup-type-info type-name)])
       (display "\n")
       (printf "  Suggested Patterns for ~a\n" type-name)
       (display "  --------------------------------------------\n")
       (if (not type-info)
           (printf "  Unknown type: ~a\n" type-name)
           (let ([ctors (type-constructors type-info)])
                (for-each
                 (lambda (ctor)
                         (let* ([name (ctor-info-name ctor)]
                                [arity (ctor-info-arity ctor)]
                                [vars (generate-var-names arity)])
                               (if (= arity 0)
                                   (printf "    [~a ...]\n" name)
                                   (printf "    [(~a ~a) ...]\n"
                                           name
                                           (string-join vars " ")))))
                 ctors)))
       (display "\n")))

;;; generate-var-names : Nat → (List String)
(define (generate-var-names n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (string-append "x" (number->string (+ i 1))) acc)))))

;; string-join is provided by prelude

;;; ====
;;; Type Registration Commands
;;; ====

;;; define-match-type : Symbol × (List (Symbol × Nat)) → Void
;;; Register a new type for pattern matching analysis.
(define (define-match-type name ctors)
  (register-type! name ctors)
  (printf "Registered type ~a with ~a constructor(s).\n"
          name (length ctors)))

;;; ====
;;; Batch Analysis
;;; ====

;;; check-file-patterns : String → Void
;;; Check all case expressions in a file for exhaustiveness.
(define (check-file-patterns path)
  (guard (e [else (printf "\n  Error reading file: ~a\n\n" path)])
         (let* ([content (call-with-input-file path get-string-all)]
                [port (open-input-string content)]
                [cases (find-case-expressions port)])
               (display "\n")
               (printf "  Pattern Analysis: ~a\n" path)
               (display "  ============================================\n")
               (printf "  Found ~a case expression(s)\n\n" (length cases))
               (for-each
                (lambda (case-info)
                        (let* ([line (car case-info)]
                               [expr (cdr case-info)]
                               [patterns (map car (cddr expr))])
                              (printf "  Line ~a: ~a pattern(s)\n" line (length patterns))))
                cases)
               (display "\n"))))

;;; find-case-expressions : Port → (List (Line × Expr))
;;; Find all case expressions in input.
(define (find-case-expressions port)
  (let loop ([line 1] [results '()])
       (let ([expr (read port)])
            (if (eof-object? expr)
                (reverse results)
                (let ([found (find-cases-in expr line)])
                     (loop (+ line 1) (append results found)))))))

;;; find-cases-in : Expr × Line → (List (Line × Expr))
(define (find-cases-in expr line)
  (cond
   [(not (pair? expr)) '()]
   [(eq? (car expr) 'case)
    (list (cons line expr))]
   [else
    (apply append (map (lambda (e) (find-cases-in e line)) (cdr expr)))]))

;;; ====
;;; Examples and Help
;;; ====

(define (pattern-help)
  (display "\n")
  (display "  Pattern Match Linting Commands\n")
  (display "  --------------------------------------------\n")
  (display "  (check-match expr type)         - Analyze case expression\n")
  (display "  (missing-patterns pats type)    - Find uncovered cases\n")
  (display "  (redundant-patterns pats type)  - Find unreachable patterns\n")
  (display "  (suggest-patterns type)         - Generate complete patterns\n")
  (display "  (define-match-type name ctors)  - Register new type\n")
  (display "\n")
  (display "  Built-in types:\n")
  (display "    Bool, List, Maybe, Either, Nat, Pair, Unit\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (check-match '(case x [true 1]) 'Bool)\n")
  (display "    (missing-patterns '(nil (cons x xs)) 'List)\n")
  (display "    (suggest-patterns 'Maybe)\n")
  (display "    (define-match-type 'Color '((red . 0) (green . 0) (blue . 0)))\n")
  (display "\n"))

;;; Run examples
(define (pattern-examples)
  (display "\n  === Pattern Match Examples ===\n")
  
  (display "\n  Example 1: Incomplete Bool match\n")
  (check-match '(case x [true 1]) 'Bool)
  
  (display "\n  Example 2: Complete Bool match\n")
  (check-match '(case x [true 1] [false 0]) 'Bool)
  
  (display "\n  Example 3: Redundant pattern\n")
  (check-match '(case x [true 1] [false 0] [_ 2]) 'Bool)
  
  (display "\n  Example 4: List patterns\n")
  (missing-patterns '(nil (cons x xs)) 'List)
  
  (display "\n  Example 5: Suggested patterns\n")
  (suggest-patterns 'Maybe))

(display "Pattern lint loaded. Use (pattern-help) for usage.\n")
