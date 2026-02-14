;;; Load prelude first for doc, fold-left, filter, zip
(load "core/base/prelude.ss")

(doc 'module 'fuel-analysis)
(doc 'description "Primitive-Level Fuel Cost Analysis - analyze fuel costs by tracking primitive operations during evaluation")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Features: track all primitive calls during evaluation, sum fuel costs based on prim-fuel-cost, estimate computational complexity from fuel growth")

(doc 'section 'prim-fuel-cost)

(define (prim-fuel-cost op)
  (doc 'description "Get fuel cost for a primitive operation (inlined from core/prim.ss)")
  (doc 'param '(op "Primitive operation symbol"))
  (doc 'returns "Nat or #f - fuel cost or #f if unknown")

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

(doc 'section 'inline-prim)

(define (prim op . args)
  (doc 'description "Inline prim dispatcher (expanded version with commonly used operations)")
  (doc 'param '(op "Operation symbol"))
  (doc 'param '(args "Operation arguments"))
  (doc 'returns "Result or error")

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

(doc 'section 'instrumentation-state)

(define *prim-trace* (make-parameter '()))

(define (reset-prim-trace!)
  (doc 'description "Reset the primitive call trace")
  (*prim-trace* '()))

(define (record-prim-call! op)
  (doc 'description "Record a primitive call in the trace")
  (doc 'param '(op "Primitive operation symbol"))

  (let ([trace (*prim-trace*)])
       (let ([existing (assq op trace)])
            (if existing
                (*prim-trace* (cons (cons op (+ 1 (cdr existing)))
                                    (filter (lambda (p) (not (eq? (car p) op))) trace)))
                (*prim-trace* (cons (cons op 1) trace))))))

(define (get-prim-trace)
  (doc 'description "Get the current primitive call trace")
  (doc 'returns "List of (symbol . count) pairs")
  (*prim-trace*))

(doc 'section 'instrumented-primitive-dispatcher)

(define (prim-instrumented op . args)
  (doc 'description "Instrumented version of prim that records calls")
  (doc 'param '(op "Operation symbol"))
  (doc 'param '(args "Operation arguments"))
  (doc 'returns "Result of operation")

  (record-prim-call! op)
  (apply prim op args))

(doc 'section 'fuel-cost-calculation)

(define (calculate-total-fuel trace)
  (doc 'description "Calculate total fuel cost from primitive call trace")
  (doc 'param '(trace "List of (symbol . count) pairs"))
  (doc 'returns "Nat - total fuel cost")

  (fold-left
   (lambda (total entry)
           (let ([op (car entry)]
                 [count (cdr entry)])
                (let ([cost (prim-fuel-cost op)])
                     (if cost
                         (+ total (* count cost))
                         total))))
   0
   trace))

(doc 'section 'tool-1-analyze-fuel)

(define (analyze-fuel func input)
  (doc 'description "Analyze fuel consumption for a function call by tracing primitives")
  (doc 'param '(func "Function to analyze"))
  (doc 'param '(input "Input to function"))
  (doc 'returns "Alist with total-fuel, primitive-calls, result or error")

  (guard (e [else
             `((total-fuel . 0)
               (primitive-calls . ())
               (error . ,(if (condition? e)
                             (condition-message e)
                             (format "~a" e))))])
         (reset-prim-trace!)

         (let ([result (func input)])

              (let* ([trace (get-prim-trace)]
                     [total-fuel (calculate-total-fuel trace)])

                    `((total-fuel . ,total-fuel)
                      (primitive-calls . ,trace)
                      (result . ,result))))))

(doc 'section 'simple-test-wrappers)

(define (test-add x)
  (doc 'description "Test function that uses instrumented prim (adds input to itself)")
  (prim-instrumented 'add x x))

(define (test-factorial n)
  (doc 'description "Factorial using instrumented primitives")
  (if (prim-instrumented 'zero? n)
      1
      (prim-instrumented 'mul n (test-factorial (prim-instrumented 'sub n 1)))))

(doc 'section 'tool-2-estimate-complexity)

(define (estimate-complexity func input-generator sizes)
  (doc 'description "Estimate computational complexity by running with multiple input sizes")
  (doc 'param '(func "Function to analyze"))
  (doc 'param '(input-generator "Function: Nat -> Input"))
  (doc 'param '(sizes "List of input sizes to test"))
  (doc 'returns "Alist with complexity, fuel-samples, growth-factor")

  (guard (e [else
             `((error . ,(if (condition? e)
                             (condition-message e)
                             (format "~a" e))))])

         (let ([samples
                (map (lambda (size)
                             (let* ([input (input-generator size)]
                                    [analysis (analyze-fuel func input)])
                                   (cons size (cdr (assq 'total-fuel analysis)))))
                     sizes)])

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

(define (sort-samples samples)
  (doc 'description "Sort samples by size")
  (sort-list samples (lambda (a b) (< (car a) (car b)))))

(define (calculate-growth-factor samples)
  (doc 'description "Calculate average growth factor between consecutive samples")
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

(define (classify-complexity samples)
  (doc 'description "Classify complexity based on growth pattern")
  (if (< (length samples) 2)
      "O(1)"
      (let* ([first (car samples)]
             [last (car (reverse samples))]
             [size-ratio (/ (exact->inexact (car last))
                            (exact->inexact (car first)))]
             [fuel-ratio (/ (exact->inexact (cdr last))
                            (exact->inexact (cdr first)))])

            (cond
             [(<= fuel-ratio 1.5)
              "O(1)"]

             [(and (> fuel-ratio (* 0.8 size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio)))
              "O(n)"]

             [(and (> fuel-ratio (* 0.5 size-ratio size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio size-ratio)))
              "O(n²)"]

             [(and (> fuel-ratio (* 0.5 size-ratio size-ratio size-ratio))
                   (< fuel-ratio (* 1.5 size-ratio size-ratio size-ratio)))
              "O(n³)"]

             [(< fuel-ratio (* 2 (log size-ratio)))
              "O(log n)"]

             [(and (> fuel-ratio (* 0.8 size-ratio (log size-ratio)))
                   (< fuel-ratio (* 1.5 size-ratio (log size-ratio))))
              "O(n log n)"]

             [(> fuel-ratio (* 1.5 size-ratio size-ratio))
              "O(2^n) or worse"]

             [else
              (format "unknown (~ax growth)"
                      (exact (round fuel-ratio)))]))))

(doc 'section 'pretty-printing)

(define (print-analysis analysis)
  (doc 'description "Pretty print fuel analysis results")
  (doc 'param '(analysis "Analysis alist"))

  (display "\n================ PRIMITIVE FUEL ANALYSIS =====================\n\n")

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

(define (print-complexity-analysis analysis)
  (doc 'description "Pretty print complexity analysis results")
  (doc 'param '(analysis "Complexity analysis alist"))

  (display "\n================ COMPLEXITY ANALYSIS =========================\n\n")

  (let ([complexity (cdr (assq 'complexity analysis))]
        [samples (cdr (assq 'fuel-samples analysis))]
        [growth (cdr (assq 'growth-factor analysis))])

       (display (format "Estimated Complexity: ~a\n\n" complexity))

       (display "Fuel Samples:\n")
       (display "  Size       Fuel\n")
       (display "  ----------------\n")
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

(define (sort-list lst less?)
  (doc 'description "Sort a list using comparison function")
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
