;;; Set up source-directories to find modules
(source-directories (cons "core" (source-directories)))
(source-directories (cons "shell" (source-directories)))

(load "prelude.ss")
(load "eval.ss")
(load "edit.ss")

(doc 'module 'fuel-profile)
(doc 'description "Fuel Consumption Profiler - track and analyze fuel consumption during evaluation")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Features: profile expression evaluation fuel usage, track fuel consumption per sub-expression, identify expensive operations, suggest fuel budgets, detect potential non-termination")

(doc 'section 'fuel-tracking)

(define (fuel-profile expr fuel)
  (doc 'description "Profile fuel consumption for an expression")
  (doc 'param '(expr "Expression to profile"))
  (doc 'param '(fuel "Fuel budget"))
  (doc 'returns "void - displays profile report")

  (display "\n==================== FUEL PROFILE ============================\n\n")

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

              (display "Finding minimum fuel required...\n")
              (let ([min-fuel (find-min-fuel expr fuel)])
                   (display (format "  Minimum fuel: ~a\n" min-fuel))
                   (display (format "  Fuel used: ~a (~a%)\n"
                                    min-fuel
                                    (exact (round (* 100 (/ min-fuel fuel))))))
                   (display "\n")
                   (fuel-usage-assessment min-fuel fuel)))])))

(define (find-min-fuel expr max-fuel)
  (doc 'description "Binary search to find minimum fuel required for successful evaluation")
  (doc 'param '(expr "Expression to evaluate"))
  (doc 'param '(max-fuel "Maximum fuel budget to search"))
  (doc 'returns "Nat - minimum fuel needed")

  (let binary-search ([low 1] [high max-fuel])
       (if (<= (- high low) 1)
           high
           (let* ([mid (quotient (+ low high) 2)]
                  [result (eval-expr empty-env expr mid)])
                 (if (error? result)
                     (binary-search mid high)
                     (binary-search low mid))))))

(define (fuel-usage-assessment used budget)
  (doc 'description "Provide assessment of fuel usage")
  (doc 'param '(used "Fuel used"))
  (doc 'param '(budget "Total fuel budget"))
  (doc 'returns "void - displays assessment")

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

(doc 'section 'detailed-profiling)

(define (fuel-profile-detailed expr base-fuel)
  (doc 'description "Profile with multiple fuel levels to show scaling behavior")
  (doc 'param '(expr "Expression to profile"))
  (doc 'param '(base-fuel "Base fuel budget"))
  (doc 'returns "void - displays detailed profile")

  (display "\n================ DETAILED FUEL PROFILE =======================\n\n")

  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))

  (display "Testing fuel levels:\n\n")
  (display "  Fuel    Status    Time (relative)\n")
  (display "  ---------------------------------\n")

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

(doc 'section 'fuel-recommendations)

(define (fuel-recommend expr)
  (doc 'description "Recommend appropriate fuel budget for an expression")
  (doc 'param '(expr "Expression to analyze"))
  (doc 'returns "void - displays recommendation")

  (display "\n================ FUEL RECOMMENDATION ========================\n\n")

  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))

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

(doc 'section 'fuel-comparison)

(define (fuel-compare expr fuel-levels)
  (doc 'description "Compare expression behavior across different fuel levels")
  (doc 'param '(expr "Expression to test"))
  (doc 'param '(fuel-levels "List of fuel budgets to compare"))
  (doc 'returns "void - displays comparison")

  (display "\n================= FUEL COMPARISON ============================\n\n")

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

(doc 'section 'file-analysis)

(define (fuel-analyze-file fs file-path fuel)
  (doc 'description "Analyze fuel consumption for all expressions in a file")
  (doc 'param '(fs "Filesystem capability"))
  (doc 'param '(file-path "Path to file"))
  (doc 'param '(fuel "Fuel budget per expression"))
  (doc 'returns "void - displays analysis report")

  (guard (e [else
             (display (format "Error reading file: ~a\n"
                              (if (condition? e)
                                  (condition-message e)
                                  e)))])
         (let* ([content (read-text-file fs file-path)]
                [port (open-input-string content)])

               (display "\n================== FILE FUEL ANALYSIS ========================\n\n")
               (display (format "File: ~a\n" file-path))
               (display (format "Fuel budget: ~a\n\n" fuel))

               (let loop ([expr-num 1]
                          [total-fuel 0]
                          [failures '()])
                    (let ([expr (guard (e [else #f])
                                       (read port))])
                         (cond
                          [(eof-object? expr)
                           (display "\n===========================================================\n")
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

(doc 'section 'utilities)

(define (time-difference t2 t1)
  (doc 'description "Compute difference between two times")
  (doc 'param '(t2 "Later time"))
  (doc 'param '(t1 "Earlier time"))
  (doc 'returns "Time duration")

  (make-time 'time-duration
             (- (time-nanosecond t2) (time-nanosecond t1))
             (- (time-second t2) (time-second t1))))

(define (make-string-char n c)
  (doc 'description "Create a string of n copies of character c")
  (doc 'param '(n "Number of characters"))
  (doc 'param '(c "Character to repeat"))
  (doc 'returns "String")

  (list->string (make-list n c)))

(define make-string make-string-char)

;; make-list is provided by prelude (alias for replicate)

(display "Fuel profiler loaded.\n")
(display "Usage:\n")
(display "  (fuel-profile expr fuel)\n")
(display "  (fuel-profile-detailed expr fuel)\n")
(display "  (fuel-recommend expr)\n")
(display "  (fuel-compare expr (list 100 500 1000))\n")
(display "  (fuel-analyze-file fs \"file.ss\" 10000)\n")
