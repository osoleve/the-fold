;;; thimble/fuel-profile.ss — Fuel Consumption Profiler
;;;
;;; Track and analyze fuel consumption during evaluation.
;;; Helps identify performance bottlenecks and infinite loops.
;;;
;;; Features:
;;;   - Profile expression evaluation fuel usage
;;;   - Track fuel consumption per sub-expression
;;;   - Identify expensive operations
;;;   - Suggest fuel budgets
;;;   - Detect potential non-termination
;;;
;;; This is Shell code: uses IO for display, delegates to Core.
;;;
;;; Usage:
;;;   (fuel-profile expr fuel)
;;;   (fuel-profile-detailed expr fuel)
;;;   (fuel-recommend expr)
;;;   (fuel-compare expr fuel-list)
;;;   (fuel-analyze-file fs "file.ss" fuel)
;;;
;;; Dependencies:
;;;   core/eval.ss
;;;   shell/edit.ss

;;; Set up source-directories to find modules
(source-directories (cons "fabric/stitches" (source-directories)))
(source-directories (cons "shell" (source-directories)))

(load "prelude.ss")
(load "eval.ss")
(load "edit.ss")

;;; ============================================================
;;; Fuel Tracking
;;; ============================================================

;;; We track fuel consumption by running evaluation with decreasing
;;; fuel budgets and observing when evaluation succeeds.

;;; fuel-profile : Expr × Nat → void
;;; Profile fuel consumption for an expression.
(define (fuel-profile expr fuel)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    FUEL PROFILE                              ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  (display (format "Initial fuel budget: ~a\n\n" fuel))
  
  (let ([result (eval-expr empty-env expr fuel)])
       (cond
        [(error? result)
         (display "❌ Evaluation FAILED\n\n")
         (display (format "Error: ~a\n\n" (error-message result)))
         (if (and (pair? (error-data result))
                  (eq? (car (error-data result)) 'fuel-exhausted))
             (begin
              (display "Fuel exhausted during evaluation.\n")
              (display "Suggestion: This expression may not terminate or requires more fuel.\n")
              (fuel-recommend expr))
             (display "Evaluation failed for a reason other than fuel exhaustion.\n"))]
        [else
         (let ([value (result-value result)])
              (display "✓ Evaluation SUCCEEDED\n\n")
              (display "Result:\n")
              (display (format "  ~s\n\n" value))
              
              ;; Find minimum fuel required
              (display "Finding minimum fuel required...\n")
              (let ([min-fuel (find-min-fuel expr fuel)])
                   (display (format "  Minimum fuel: ~a\n" min-fuel))
                   (display (format "  Fuel used: ~a (~a%)\n"
                                    min-fuel
                                    (exact (round (* 100 (/ min-fuel fuel))))))
                   (display "\n")
                   (fuel-usage-assessment min-fuel fuel)))])))

;;; find-min-fuel : Expr × Nat → Nat
;;; Binary search to find minimum fuel required for successful evaluation.
(define (find-min-fuel expr max-fuel)
  (let binary-search ([low 1] [high max-fuel])
       (if (<= (- high low) 1)
           high
           (let* ([mid (quotient (+ low high) 2)]
                  [result (eval-expr empty-env expr mid)])
                 (if (error? result)
                     (binary-search mid high)  ; Need more fuel
                     (binary-search low mid))))))  ; Can use less fuel

;;; fuel-usage-assessment : Nat × Nat → void
;;; Provide assessment of fuel usage.
(define (fuel-usage-assessment used budget)
  (let ([percent (/ used budget)])
       (cond
        [(< percent 0.1)
         (display "Assessment: Very efficient (< 10% of budget)\n")
         (display "  → This expression uses minimal fuel.\n")]
        [(< percent 0.3)
         (display "Assessment: Efficient (< 30% of budget)\n")
         (display "  → Good fuel usage.\n")]
        [(< percent 0.7)
         (display "Assessment: Moderate (30-70% of budget)\n")
         (display "  → Consider if this is expected complexity.\n")]
        [(< percent 0.9)
         (display "Assessment: Heavy (70-90% of budget)\n")
         (display "  → ⚠ This expression is fuel-intensive.\n")
         (display "  → Consider optimization or increasing budget.\n")]
        [else
         (display "Assessment: Critical (> 90% of budget)\n")
         (display "  → ⚠ Dangerously close to fuel exhaustion!\n")
         (display "  → Increase budget or optimize expression.\n")])))

;;; ============================================================
;;; Detailed Profiling
;;; ============================================================

;;; fuel-profile-detailed : Expr × Nat → void
;;; Profile with multiple fuel levels to show scaling behavior.
(define (fuel-profile-detailed expr base-fuel)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              DETAILED FUEL PROFILE                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  
  (display "Testing fuel levels:\n\n")
  (display "  Fuel    Status    Time (relative)\n")
  (display "  ─────────────────────────────────\n")
  
  (let ([levels (list
                 (quotient base-fuel 10)
                 (quotient base-fuel 5)
                 (quotient base-fuel 2)
                 base-fuel
                 (* 2 base-fuel))])
       (for-each
        (lambda (fuel-level)
                (let* ([start (current-time)]
                       [result (eval-expr empty-env expr fuel-level)]
                       [end (current-time)]
                       [duration (time-difference end start)])
                      (display (format "  ~a~a"
                                       fuel-level
                                       (make-string (max 1 (- 8 (string-length (number->string fuel-level)))) #\space)))
                      (if (error? result)
                          (display "  FAIL      ")
                          (display "  OK        "))
                      (display (format "~a ns\n" (+ (* (time-second duration) 1000000000)
                                                    (time-nanosecond duration))))))
        levels))
  (display "\n"))

;;; ============================================================
;;; Fuel Recommendations
;;; ============================================================

;;; fuel-recommend : Expr → void
;;; Recommend appropriate fuel budget for an expression.
(define (fuel-recommend expr)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              FUEL RECOMMENDATION                             ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  
  ;; Try progressively larger fuel budgets
  (display "Testing fuel budgets...\n\n")
  (let try-fuel ([fuel 100])
       (if (> fuel 1000000)
           (begin
            (display "  ⚠ No successful evaluation found with fuel up to 1,000,000\n")
            (display "  → This expression may not terminate.\n")
            (display "  → Consider adding a base case or checking for infinite recursion.\n"))
           (let ([result (eval-expr empty-env expr fuel)])
                (if (error? result)
                    (begin
                     (display (format "  Fuel ~a: FAIL\n" fuel))
                     (try-fuel (* fuel 10)))
                    (let ([min-fuel (find-min-fuel expr fuel)])
                         (display (format "  Fuel ~a: OK\n\n" fuel))
                         (display "Recommendation:\n")
                         (display (format "  Minimum fuel: ~a\n" min-fuel))
                         (display (format "  Recommended: ~a (safety margin: 50%)\n"
                                          (exact (ceiling (* min-fuel 1.5)))))
                         (display (format "  Conservative: ~a (safety margin: 100%)\n"
                                          (* min-fuel 2)))))))))

;;; ============================================================
;;; Fuel Comparison
;;; ============================================================

;;; fuel-compare : Expr × (List Nat) → void
;;; Compare expression behavior across different fuel levels.
(define (fuel-compare expr fuel-levels)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                 FUEL COMPARISON                              ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  
  (display "Results across fuel levels:\n\n")
  (for-each
   (lambda (fuel)
           (let ([result (eval-expr empty-env expr fuel)])
                (display (format "Fuel ~a:\n" fuel))
                (if (error? result)
                    (display (format "  ❌ ~a\n" (error-message result)))
                    (display (format "  ✓ ~s\n" (result-value result))))
                (display "\n")))
   fuel-levels))

;;; ============================================================
;;; File Analysis
;;; ============================================================

;;; fuel-analyze-file : FS × String × Nat → void
;;; Analyze fuel consumption for all expressions in a file.
(define (fuel-analyze-file fs file-path fuel)
  (guard (e [else
             (display (format "Error reading file: ~a\n"
                              (if (condition? e)
                                  (condition-message e)
                                  e)))])
         (let* ([content (read-text-file fs file-path)]
                [port (open-input-string content)])
               
               (display "\n╔══════════════════════════════════════════════════════════════╗\n")
               (display "║                FILE FUEL ANALYSIS                            ║\n")
               (display "╚══════════════════════════════════════════════════════════════╝\n\n")
               (display (format "File: ~a\n" file-path))
               (display (format "Fuel budget: ~a\n\n" fuel))
               
               (let loop ([expr-num 1]
                          [total-fuel 0]
                          [failures '()])
                    (let ([expr (guard (e [else #f])
                                       (read port))])
                         (cond
                          [(eof-object? expr)
                           (display "\n═══════════════════════════════════════════════════════════\n")
                           (display "Summary:\n")
                           (display (format "  Total expressions: ~a\n" (- expr-num 1)))
                           (display (format "  Total fuel used: ~a\n" total-fuel))
                           (display (format "  Failed expressions: ~a\n" (length failures)))
                           (when (not (null? failures))
                                 (display "\nFailures:\n")
                                 (for-each
                                  (lambda (fail)
                                          (display (format "  Expression ~a: ~a\n" (car fail) (cdr fail))))
                                  (reverse failures)))]
                          [(not expr)
                           (display (format "Parse error at expression ~a\n" expr-num))]
                          [else
                           (let ([result (eval-expr empty-env expr fuel)])
                                (cond
                                 [(error? result)
                                  (display (format "~a. ❌ " expr-num))
                                  (display (format "~s\n" expr))
                                  (display (format "   Error: ~a\n\n" (error-message result)))
                                  (loop (+ expr-num 1) total-fuel
                                        (cons (cons expr-num (error-message result)) failures))]
                                 [else
                                  (let ([min-fuel (find-min-fuel expr fuel)])
                                       (display (format "~a. ✓ " expr-num))
                                       (display (format "Fuel: ~a  " min-fuel))
                                       (display (format "~s\n\n" expr))
                                       (loop (+ expr-num 1) (+ total-fuel min-fuel) failures))]))]))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; time-difference : Time × Time → Time
;;; Compute difference between two times.
(define (time-difference t2 t1)
  (make-time 'time-duration
             (- (time-nanosecond t2) (time-nanosecond t1))
             (- (time-second t2) (time-second t1))))

;;; make-string : Nat × Char → String
;;; Create a string of n copies of character c.
(define (make-string-char n c)
  (list->string (make-list n c)))

(define make-string make-string-char)

;;; make-list : Nat × α → (List α)
;;; Create a list of n copies of x.
(define (make-list n x)
  (let loop ([i 0] [result '()])
       (if (= i n)
           result
           (loop (+ i 1) (cons x result)))))

(display "Fuel profiler loaded.\n")
(display "Usage:\n")
(display "  (fuel-profile expr fuel)\n")
(display "  (fuel-profile-detailed expr fuel)\n")
(display "  (fuel-recommend expr)\n")
(display "  (fuel-compare expr (list 100 500 1000))\n")
(display "  (fuel-analyze-file fs \"file.ss\" 10000)\n")
