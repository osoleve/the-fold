;;; shell/diagnostics/fuel-analysis.ss — Primitive-Level Fuel Cost Analysis
;;;
;;; Analyze fuel costs by tracking primitive operations during evaluation.
;;; This provides fine-grained cost analysis at the primitive level,
;;; complementing the evaluation-level fuel profiler (fuel-profile.ss).
;;;
;;; Features:
;;;   - Track all primitive calls during evaluation
;;;   - Sum fuel costs based on prim-fuel-cost
;;;   - Estimate computational complexity from fuel growth
;;;
;;; This is Shell code: uses parameters for instrumentation.
;;;
;;; USAGE:
;;;
;;;   1. Write functions using prim-instrumented instead of prim:
;;;      (define (my-func x)
;;;        (prim-instrumented 'add x 10))
;;;
;;;   2. Analyze fuel consumption:
;;;      (analyze-fuel my-func 5)
;;;      => ((total-fuel . 2)
;;;          (primitive-calls . ((add . 1)))
;;;          (result . 15))
;;;
;;;   3. Estimate complexity:
;;;      (estimate-complexity
;;;        my-func
;;;        (lambda (n) n)  ; input generator
;;;        '(10 100 1000)) ; sizes to test
;;;      => ((complexity . "O(1)")
;;;          (fuel-samples . ((10 . 2) (100 . 2) (1000 . 2)))
;;;          (growth-factor . 1.0))
;;;
;;; See test-fuel-analysis.ss for comprehensive examples.
;;; See fuel-analysis-demo.ss for practical demonstrations.
;;;
;;; Dependencies:
;;;   - Inlines minimal code from core/prelude.ss
;;;   - Inlines prim-fuel-cost from core/prim.ss

;;; We need minimal prelude utilities
;;; Inline them to avoid path issues

;;; fold-left : (β × α → β) × β × (List α) → β
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

;;; filter : (α → Bool) × (List α) → (List α)
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

;;; zip : (List α) × (List β) → (List (Pair α β))
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;;; Inline prim-fuel-cost from core/prim.ss
;;; This avoids loading the entire core module
(define (prim-fuel-cost op)
  (case op
        ;; Tier 1 (Cost 1)
        [(number? integer? symbol? string? char? bytevector? block?
                  vector? list? boolean? procedure? null? pair?)
         1]
        [(car cdr block-tag block-payload block-refs)
         1]
        [(not and or)
         1]
        [(eq? zero? positive? negative?)
         1]
        
        ;; Tier 2 (Cost 2)
        [(add sub mul neg abs)
         2]
        [(lt? le? gt? ge?)
         2]
        [(char->integer integer->char char=? char<?
                        char-alphabetic? char-numeric? char-whitespace?
                        char-upper-case? char-lower-case? char-upcase char-downcase)
         2]
        [(cons)
         2]
        [(bv-ref vec-ref string-ref list-ref block-ref)
         2]
        [(bv-length vec-length string-length)
         2]
        
        ;; Tier 3 (Cost 3)
        [(div mod)
         3]
        [(bitand bitor bitxor bitnot shl shr)
         3]
        [(make-block vec-make vec-empty bv-make make-string)
         3]
        [(string=? string<? string>?)
         3]
        [(memq assq)
         3]
        
        ;; Tier 4 (Cost 5)
        [(length reverse)
         5]
        [(vec->list list->vec)
         5]
        [(string->list list->string)
         5]
        [(list append)
         5]
        [(symbol->string string->symbol)
         5]
        
        ;; Tier 5 (Cost 10)
        [(string-append substring)
         10]
        [(bv-concat bv-copy bv-slice)
         10]
        [(string->utf8 utf8->string)
         10]
        [(block->bytes bytes->block)
         10]
        [(hash->hex hex->hash)
         10]
        [(string->number)
         10]
        
        ;; Tier 6 (Cost 15)
        [(number->string)
         15]
        
        ;; Tier 7 (Cost 100)
        [(sha256)
         100]
        
        ;; Tier 8 (Cost 110)
        [(hash-block)
         110]
        
        [else #f]))

;;; Inline prim from core/prim.ss
;;; Expanded version with commonly used operations
(define (prim op . args)
  (case op
        ;; Arithmetic
        [(add) (apply + args)]
        [(sub) (apply - args)]
        [(mul) (apply * args)]
        [(div)
         (if (zero? (cadr args))
             '(error div-by-zero)
             (quotient (car args) (cadr args)))]
        [(mod)
         (if (zero? (cadr args))
             '(error mod-by-zero)
             (modulo (car args) (cadr args)))]
        [(neg) (- (car args))]
        [(abs) (abs (car args))]
        
        ;; Comparison
        [(eq?) (equal? (car args) (cadr args))]
        [(lt?) (< (car args) (cadr args))]
        [(le?) (<= (car args) (cadr args))]
        [(gt?) (> (car args) (cadr args))]
        [(ge?) (>= (car args) (cadr args))]
        [(zero?) (zero? (car args))]
        [(positive?) (positive? (car args))]
        [(negative?) (negative? (car args))]
        
        ;; List operations
        [(null?) (null? (car args))]
        [(pair?) (pair? (car args))]
        [(car) (car (car args))]
        [(cdr) (cdr (car args))]
        [(cons) (cons (car args) (cadr args))]
        [(list) args]
        [(length) (length (car args))]
        [(append) (apply append args)]
        [(reverse) (reverse (car args))]
        
        ;; Boolean
        [(not) (not (car args))]
        [(and) (and (car args) (cadr args))]
        [(or) (or (car args) (cadr args))]
        
        ;; Type predicates
        [(number?) (number? (car args))]
        [(integer?) (integer? (car args))]
        [(symbol?) (symbol? (car args))]
        [(string?) (string? (car args))]
        [(boolean?) (boolean? (car args))]
        [(list?) (list? (car args))]
        
        [else `(error unknown-primitive ,op)]))

;;; ============================================================
;;; Instrumentation State
;;; ============================================================

;;; We use a parameter to track primitive calls during evaluation.
;;; This is a thread-local mutable cell that holds:
;;;   ((op . count) ...)

(define *prim-trace* (make-parameter '()))

;;; reset-prim-trace! : → void
;;; Reset the primitive call trace.
(define (reset-prim-trace!)
  (*prim-trace* '()))

;;; record-prim-call! : Symbol → void
;;; Record a primitive call in the trace.
(define (record-prim-call! op)
  (let ([trace (*prim-trace*)])
       (let ([existing (assq op trace)])
            (if existing
                (*prim-trace* (cons (cons op (+ 1 (cdr existing)))
                                    (filter (lambda (p) (not (eq? (car p) op))) trace)))
                (*prim-trace* (cons (cons op 1) trace))))))

;;; get-prim-trace : → ((Symbol . Nat) ...)
;;; Get the current primitive call trace.
(define (get-prim-trace)
  (*prim-trace*))

;;; ============================================================
;;; Instrumented Primitive Dispatcher
;;; ============================================================

;;; prim-instrumented : Symbol × Args... → Value
;;; Instrumented version of prim that records calls.
(define (prim-instrumented op . args)
  (record-prim-call! op)
  (apply prim op args))

;;; ============================================================
;;; Fuel Cost Calculation
;;; ============================================================

;;; calculate-total-fuel : ((Symbol . Nat) ...) → Nat
;;; Calculate total fuel cost from primitive call trace.
(define (calculate-total-fuel trace)
  (fold-left
   (lambda (total entry)
           (let ([op (car entry)]
                 [count (cdr entry)])
                (let ([cost (prim-fuel-cost op)])
                     (if cost
                         (+ total (* count cost))
                         total))))  ; Skip unknown primitives
   0
   trace))

;;; ============================================================
;;; Tool 1: analyze-fuel
;;; ============================================================

;;; analyze-fuel : (α → β) × α → ((total-fuel . Nat) (primitive-calls . ...) (result . β))
;;; Analyze fuel consumption for a function call by tracing primitives.
;;;
;;; This is a simplified version that doesn't require full evaluation infrastructure.
;;; It works by:
;;;   1. Resetting the trace
;;;   2. Calling the function (which calls prim operations)
;;;   3. Recording what primitives were called
;;;   4. Computing total fuel from the trace
;;;
;;; NOTE: This only works if the function being analyzed uses the `prim`
;;; function directly. For proper analysis of arbitrary expressions,
;;; you'd need to instrument the evaluator itself.
(define (analyze-fuel func input)
  (guard (e [else
             `((total-fuel . 0)
               (primitive-calls . ())
               (error . ,(if (condition? e)
                             (condition-message e)
                             (format "~a" e))))])
         ;; Reset trace
         (reset-prim-trace!)
         
         ;; Execute function
         (let ([result (func input)])
              
              ;; Get trace and calculate fuel
              (let* ([trace (get-prim-trace)]
                     [total-fuel (calculate-total-fuel trace)])
                    
                    `((total-fuel . ,total-fuel)
                      (primitive-calls . ,trace)
                      (result . ,result))))))

;;; ============================================================
;;; Simple Test Wrapper
;;; ============================================================

;;; For testing, we can wrap primitive operations to demonstrate:

;;; test-add : Nat → Nat
;;; Test function that uses instrumented prim (adds input to itself).
(define (test-add x)
  (prim-instrumented 'add x x))

;;; test-factorial : Nat → Nat
;;; Factorial using instrumented primitives.
(define (test-factorial n)
  (if (prim-instrumented 'zero? n)
      1
      (prim-instrumented 'mul n (test-factorial (prim-instrumented 'sub n 1)))))

;;; ============================================================
;;; Tool 2: estimate-complexity
;;; ============================================================

;;; estimate-complexity : (α → β) × (Nat → α) × (List Nat) → Result
;;; Estimate computational complexity by running with multiple input sizes.
;;;
;;; Returns:
;;;   ((complexity . "O(...)")
;;;    (fuel-samples . ((size . fuel) ...))
;;;    (growth-factor . ratio))
(define (estimate-complexity func input-generator sizes)
  (guard (e [else
             `((error . ,(if (condition? e)
                             (condition-message e)
                             (format "~a" e))))])
         
         ;; Collect fuel samples for each size
         (let ([samples
                (map (lambda (size)
                             (let* ([input (input-generator size)]
                                    [analysis (analyze-fuel func input)])
                                   (cons size (cdr (assq 'total-fuel analysis)))))
                     sizes)])
              
              ;; Analyze growth pattern
              (if (< (length samples) 2)
                  `((complexity . "insufficient-data")
                    (fuel-samples . ,samples)
                    (growth-factor . 0))
                  
                  (let* ([sorted-samples (sort-samples samples)]
                         [complexity-class (classify-complexity sorted-samples)]
                         [growth-factor (calculate-growth-factor sorted-samples)])
                        
                        `((complexity . ,complexity-class)
                          (fuel-samples . ,sorted-samples)
                          (growth-factor . ,growth-factor)))))))

;;; sort-samples : ((Nat . Nat) ...) → ((Nat . Nat) ...)
;;; Sort samples by size.
(define (sort-samples samples)
  (sort-list samples (lambda (a b) (< (car a) (car b)))))

;;; calculate-growth-factor : ((Nat . Nat) ...) → Real
;;; Calculate average growth factor between consecutive samples.
(define (calculate-growth-factor samples)
  (if (< (length samples) 2)
      0
      (let* ([pairs (zip samples (cdr samples))]
             [ratios (map (lambda (pair)
                                  (let ([s1 (car pair)]
                                        [s2 (cdr pair)])
                                       (let ([size1 (car s1)]
                                             [fuel1 (cdr s1)]
                                             [size2 (car s2)]
                                             [fuel2 (cdr s2)])
                                            (if (and (> size1 0) (> fuel1 0))
                                                (/ (exact->inexact fuel2)
                                                   (exact->inexact fuel1))
                                                1.0))))
                          pairs)])
            (if (null? ratios)
                0
                (/ (fold-left + 0 ratios) (length ratios))))))

;;; classify-complexity : ((Nat . Nat) ...) → String
;;; Classify complexity based on growth pattern.
(define (classify-complexity samples)
  (if (< (length samples) 2)
      "O(1)"
      (let* ([first (car samples)]
             [last (car (reverse samples))]
             [size-ratio (/ (exact->inexact (car last))
                            (exact->inexact (car first)))]
             [fuel-ratio (/ (exact->inexact (cdr last))
                            (exact->inexact (cdr first)))])
            
            (cond
             ;; Constant: fuel barely changes
             [(<= fuel-ratio 1.5)
              "O(1)"]
             
             ;; Linear: fuel grows proportionally to input
             [(and (> fuel-ratio (* 0.8 size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio)))
              "O(n)"]
             
             ;; Quadratic: fuel grows with square of input
             ;; Widened lower bound (0.5) to account for constant factors
             [(and (> fuel-ratio (* 0.5 size-ratio size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio size-ratio)))
              "O(n²)"]
             
             ;; Cubic
             [(and (> fuel-ratio (* 0.5 size-ratio size-ratio size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio size-ratio size-ratio)))
              "O(n³)"]
             
             ;; Logarithmic: very slow growth
             [(< fuel-ratio (* 2 (log size-ratio)))
              "O(log n)"]
             
             ;; Linearithmic
             [(and (> fuel-ratio (* 0.8 size-ratio (log size-ratio)))
                   (< fuel-ratio (* 1.5 size-ratio (log size-ratio))))
              "O(n log n)"]
             
             ;; Exponential: very fast growth
             [(> fuel-ratio (* 1.5 size-ratio size-ratio))
              "O(2^n) or worse"]
             
             ;; Unknown
             [else
              (format "unknown (~ax growth)"
                      (exact (round fuel-ratio)))]))))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; print-analysis : Analysis → void
;;; Pretty print fuel analysis results.
(define (print-analysis analysis)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              PRIMITIVE FUEL ANALYSIS                         ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ([total-fuel (cdr (assq 'total-fuel analysis))]
        [prim-calls (cdr (assq 'primitive-calls analysis))]
        [result (assq 'result analysis)])
       
       (display (format "Total Fuel: ~a\n\n" total-fuel))
       
       (display "Primitive Calls:\n")
       (for-each
        (lambda (entry)
                (let* ([op (car entry)]
                       [count (cdr entry)]
                       [cost (prim-fuel-cost op)])
                      (if cost
                          (display (format "  ~a: ~a calls × ~a fuel = ~a total\n"
                                           op count cost (* count cost)))
                          (display (format "  ~a: ~a calls (unknown cost)\n"
                                           op count)))))
        (sort-list prim-calls (lambda (a b) (> (cdr a) (cdr b)))))
       
       (when result
             (display (format "\nResult: ~s\n" (cdr result))))))

;;; print-complexity-analysis : ComplexityAnalysis → void
;;; Pretty print complexity analysis results.
(define (print-complexity-analysis analysis)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            COMPLEXITY ANALYSIS                               ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ([complexity (cdr (assq 'complexity analysis))]
        [samples (cdr (assq 'fuel-samples analysis))]
        [growth (cdr (assq 'growth-factor analysis))])
       
       (display (format "Estimated Complexity: ~a\n\n" complexity))
       
       (display "Fuel Samples:\n")
       (display "  Size       Fuel\n")
       (display "  ────────────────\n")
       (for-each
        (lambda (sample)
                (display (format "  ~a~a~a\n"
                                 (car sample)
                                 (make-string (max 1 (- 11 (string-length (number->string (car sample))))) #\space)
                                 (cdr sample))))
        samples)
       
       (display (format "\nAverage Growth Factor: ~a\n"
                        (if (number? growth)
                            (exact (round (* 100 growth)))
                            growth)))))

;;; sort-list : (List α) × (α × α → Bool) → (List α)
;;; Sort a list using comparison function (renamed to avoid conflicts).
(define (sort-list lst less?)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ([pivot (car lst)]
             [rest (cdr lst)]
             [smaller (filter (lambda (x) (less? x pivot)) rest)]
             [larger (filter (lambda (x) (not (less? x pivot))) rest)])
            (append (sort-list smaller less?)
                    (list pivot)
                    (sort-list larger less?)))))

(display "Primitive fuel analysis tools loaded.\n")
(display "Usage:\n")
(display "  (analyze-fuel func input)\n")
(display "  (estimate-complexity func input-gen sizes)\n")
(display "  (print-analysis (analyze-fuel func input))\n")
(display "  (print-complexity-analysis (estimate-complexity func gen sizes))\n")
