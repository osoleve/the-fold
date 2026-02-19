;;; lattice/quickcheck/quickcheck.ss — Property-based testing runner
;;; @module quickcheck
;;; @requires prelude state prng qc-generators qc-shrink

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'state)
(require 'prng)
(require 'qc-generators)
(require 'qc-shrink)

(doc 'module 'quickcheck)
(doc 'description "QuickCheck-style property testing — generators, shrinking, and test framework integration.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================================
;;; Result types
;;; ============================================================================

(doc 'section 'result-types)

(define (qc-success num-tests seed)
  (doc 'export #t)
  (doc 'type '(-> Nat RNG QCResult))
  (doc 'description "Successful property check result")
  (list 'qc-success num-tests seed))

(define (qc-success? r)
  (doc 'export #t)
  (and (pair? r) (eq? (car r) 'qc-success)))

(define (qc-success-num-tests r)
  (doc 'export #t)
  (cadr r))

(define (qc-failure num-tests seed original shrunk shrink-steps)
  (doc 'export #t)
  (doc 'type '(-> Nat RNG a a Nat QCResult))
  (doc 'description "Failed property check result with counterexample")
  (list 'qc-failure num-tests seed original shrunk shrink-steps))

(define (qc-failure? r)
  (doc 'export #t)
  (and (pair? r) (eq? (car r) 'qc-failure)))

(define (qc-failure-num-tests r) (doc 'export #t) (list-ref r 1))
(define (qc-failure-seed r) (doc 'export #t) (list-ref r 2))
(define (qc-failure-original r) (doc 'export #t) (list-ref r 3))
(define (qc-failure-shrunk r) (doc 'export #t) (list-ref r 4))
(define (qc-failure-shrink-steps r) (doc 'export #t) (list-ref r 5))

;;; ============================================================================
;;; Option parsing
;;; ============================================================================

(doc 'section 'options)

(define (parse-qc-opts opts)
  (doc 'description "Parse keyword options list into alist")
  (let loop ([rest opts] [acc '()])
    (if (or (null? rest) (null? (cdr rest)))
        acc
        (loop (cddr rest)
              (cons (cons (car rest) (cadr rest)) acc)))))

(define (qc-opt opts key default)
  (let ([entry (assq key opts)])
    (if entry (cdr entry) default)))

;;; ============================================================================
;;; Size schedule
;;; ============================================================================

(define (size-at-test i num-tests max-size)
  (doc 'description "Linear ramp from 0 to max-size across test count")
  (if (<= num-tests 1)
      max-size
      (let ([fraction (/ (* i max-size) (- num-tests 1))])
        (inexact->exact (floor fraction)))))

;;; ============================================================================
;;; Shrink loop
;;; ============================================================================

(define (shrink-loop prop shrinker value max-shrinks)
  (doc 'description "Greedy DFS shrink — take first failing candidate, recurse")
  (let loop ([current value] [steps 0])
    (if (>= steps max-shrinks)
        (cons current steps)
        (let ([candidates (shrinker current)])
          (let try ([cands candidates])
            (if (null? cands)
                (cons current steps)
                (if (not (prop (car cands)))
                    ;; This candidate still fails — recurse from it
                    (loop (car cands) (+ steps 1))
                    ;; This candidate passes — try next
                    (try (cdr cands)))))))))

;;; ============================================================================
;;; Core runner
;;; ============================================================================

(doc 'section 'core-runner)

(define (check-property gen prop . opts)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a Boolean) Opts ... QCResult))
  (doc 'description
    "Run a property against random inputs. Options: 'tests N, 'seed N, 'max-size N, 'shrink shrinker-fn, 'max-shrinks N")
  (let* ([parsed (parse-qc-opts opts)]
         [num-tests (qc-opt parsed 'tests 100)]
         [seed-val (qc-opt parsed 'seed 12345)]
         [max-size (qc-opt parsed 'max-size 100)]
         [shrinker (qc-opt parsed 'shrink shrink-nothing)]
         [max-shrinks (qc-opt parsed 'max-shrinks 1000)]
         [rng (make-pcg seed-val 1)])
    (let loop ([i 0] [g rng])
      (if (>= i num-tests)
          (qc-success num-tests rng)
          (let* ([size (size-at-test i num-tests max-size)]
                 [result (run-gen-with-state gen size g)]
                 [value (car result)]
                 [g2 (cdr result)])
            (if (prop value)
                (loop (+ i 1) g2)
                ;; Found failure — shrink it
                (let* ([shrink-result (shrink-loop prop shrinker value max-shrinks)]
                       [shrunk (car shrink-result)]
                       [steps (cdr shrink-result)])
                  (qc-failure (+ i 1) rng value shrunk steps))))))))

;;; ============================================================================
;;; Convenience runners
;;; ============================================================================

(doc 'section 'convenience)

(define (for-all gen prop . opts)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a Boolean) Opts ... QCResult))
  (doc 'description "Bare property runner — returns result without test registration")
  (apply check-property gen prop opts))

;;; ============================================================================
;;; Test framework integration
;;; ============================================================================

(doc 'section 'test-integration)

(define (format-qc-failure result)
  (doc 'description "Format a QC failure for test output")
  (let ([original (qc-failure-original result)]
        [shrunk (qc-failure-shrunk result)]
        [steps (qc-failure-shrink-steps result)]
        [num (qc-failure-num-tests result)])
    (string-append
      "Failed after " (number->string num) " test(s).\n"
      "      Counterexample: " (format "~s" shrunk) "\n"
      (if (equal? original shrunk)
          ""
          (string-append
            "      Original:       " (format "~s" original) "\n"
            "      Shrink steps:   " (number->string steps) "\n")))))

(define-syntax assert-property
  (syntax-rules ()
    [(_ gen prop opts ...)
     (let ([result (check-property gen prop opts ...)])
       (when (qc-failure? result)
         (inc-failed!)
         (display "    PROPERTY FAILURE: ")
         (display (format-qc-failure result))))]))

(define-syntax define-property
  (syntax-rules ()
    [(_ name gen prop opts ...)
     (define-test name
       (let ([result (check-property gen prop opts ...)])
         (when (qc-failure? result)
           (display (format-qc-failure result)))
         (assert-true (qc-success? result))))]))
