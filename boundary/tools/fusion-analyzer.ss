(load "lattice/fp/analysis/fusion-detect.ss")

(doc 'module 'fusion-analyzer)
(doc 'description "Auto-parallelization detection, fusion hints, and safe rewrite suggestions for functional pipelines")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "fusion-detect.ss and parallel-detect.ss both define detect-at-depth, so we capture fusion detection function first")

(define *lzr-fusion-detect* detect-fusion-static)

(load "lattice/fp/analysis/parallel-detect.ss")
(load "lattice/fp/analysis/cost-analysis.ss")
(load "lattice/fp/rewrite/fusion-rules.ss")
(load "lattice/fp/rewrite/verify.ss")

(doc 'section 'fusion-analysis-interface)

(define (analyze-fusion expr)
  (doc 'type "Expr -> (List FusionOpportunity)")
  (doc 'description "Analyze an expression for fusion opportunities")
  (if (or (null? expr) (not (pair? expr)))
      '()
      (*lzr-fusion-detect* expr)))

;;; ====
;;; Parallel Analysis Interface
;;; ====

;;; suggest-parallel : Expr -> (List ParallelOpportunity)
;;; Analyze an expression for parallelization opportunities.
(define (suggest-parallel expr)
  (if (or (null? expr) (not (pair? expr)))
      '()
      (detect-parallel-regions expr)))

;;; ====
;;; Optimization Interface
;;; ====

;;; optimize : Expr -> Expr
;;; Apply fusion rules to optimize an expression.
(define (optimize expr)
  (if (or (null? expr) (not (pair? expr)))
      expr
      (let ([result (fusion-simplify expr)])
           (if result result expr))))

;;; optimize-with-stats : Expr -> Analysis
;;; Optimize an expression and return statistics.
(define (optimize-with-stats expr)
  (let* ([before-count (count-traversals expr)]
         [result (optimize expr)]
         [after-count (count-traversals result)]
         [savings (- before-count after-count)]
         [percent-reduction (if (zero? before-count)
                                0
                                (* 100 (/ savings before-count)))])
        `((before-traversals . ,before-count)
          (after-traversals . ,after-count)
          (traversals-saved . ,savings)
          (percent-reduction . ,percent-reduction)
          (result . ,result))))

;;; ====
;;; Combined Analysis
;;; ====

;;; analyze-all : Expr -> Analysis
;;; Perform both fusion and parallel analysis.
(define (analyze-all expr)
  (let* ([fusion-opps (analyze-fusion expr)]
         [parallel-opps (suggest-parallel expr)]
         [fusion-sum (if (null? fusion-opps)
                         '((total . 0) (safe . 0))
                         (fusion-summary fusion-opps))]
         [parallel-sum (if (null? parallel-opps)
                           '((total . 0) (high-value . 0))
                           (parallel-summary parallel-opps))])
        `((fusion . ,fusion-sum)
          (parallel . ,parallel-sum)
          (fusion-opportunities . ,fusion-opps)
          (parallel-opportunities . ,parallel-opps))))

;;; lzr-report : Expr -> Report
;;; Generate a comprehensive LZR analysis report.
(define (lzr-report expr)
  (let* ([analysis (analyze-all expr)]
         [optimized (optimize expr)]
         [stats (optimize-with-stats expr)])
        `((expression . ,expr)
          (fusion-opportunities . ,(cdr (assq 'fusion-opportunities analysis)))
          (parallel-opportunities . ,(cdr (assq 'parallel-opportunities analysis)))
          (optimized . ,optimized)
          (optimization-stats . ,stats))))

;;; ====
;;; Display Utilities
;;; ====

;;; truncate-expr : Expr -> String
(define (truncate-expr expr)
  (let* ([s (format "~s" expr)]
         [len (string-length s)])
        (if (> len 60)
            (string-append (substring s 0 57) "...")
            s)))

;;; format-position : Position -> String
(define (format-position pos)
  (if (or (null? pos) (not (list? pos)))
      "(root)"
      (format "(~a)" (string-join (map number->string pos) " "))))

;;; format-confidence : Symbol -> String
(define (format-confidence conf)
  (case conf
        [(safe) "safe"]
        [(likely-pure) "likely-pure"]
        [(check-effects) "needs-review"]
        [else (format "~a" conf)]))

;;; format-speedup : Number -> String
(define (format-speedup s)
  (let ([rounded (/ (round (* s 10)) 10)])
       (format "~a" rounded)))

;;; take-n : Nat x (List a) -> (List a)
(define (take-n n xs)
  (let loop ([n n] [xs xs] [acc '()])
       (if (or (<= n 0) (null? xs))
           (reverse acc)
           (loop (- n 1) (cdr xs) (cons (car xs) acc)))))

;;; string-join : (List String) x String -> String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)] [result (car strs)])
           (if (null? rest)
               result
               (loop (cdr rest) (string-append result sep (car rest)))))))

;;; display-lzr-report : Report -> Void
(define (display-lzr-report report)
  (display "\n")
  (display "+====+\n")
  (display "|           FOLD-LZR: AUTO-PARALLELIZATION & FUSION              |\n")
  (display "+====+\n\n")
  
  ;; Fusion opportunities
  (let ([fusion-opps (cdr (assq 'fusion-opportunities report))])
       (display "-- FUSION OPPORTUNITIES --\n\n")
       (if (null? fusion-opps)
           (display "  No fusion opportunities detected.\n\n")
           (for-each (lambda (opp)
                             (display (format "  [~a] at ~a\n"
                                              (fusion-type opp)
                                              (format-position (fusion-location opp))))
                             (display (format "    Before: ~a\n" (truncate-expr (fusion-before opp))))
                             (display (format "    After:  ~a\n" (truncate-expr (fusion-after opp))))
                             (display (format "    Savings: ~a%  Confidence: ~a\n\n"
                                              (fusion-savings opp)
                                              (format-confidence (fusion-confidence opp)))))
                     fusion-opps)))
  
  ;; Parallel opportunities
  (let ([parallel-opps (cdr (assq 'parallel-opportunities report))])
       (display "-- PARALLELIZATION HINTS --\n\n")
       (if (null? parallel-opps)
           (display "  No parallelization opportunities detected.\n\n")
           (for-each (lambda (opp)
                             (display (format "  [~a] at ~a\n"
                                              (par-type opp)
                                              (format-position (par-location opp))))
                             (let ([branches (par-branches opp)])
                                  (display (format "    Branches: ~a\n"
                                                   (string-join (map truncate-expr (take-n 3 branches)) ", ")))
                                  (when (> (length branches) 3)
                                        (display (format "             ... and ~a more\n" (- (length branches) 3)))))
                             (display (format "    Speedup: ~~~ax\n\n"
                                              (format-speedup (par-estimated-speedup opp)))))
                     parallel-opps)))
  
  ;; Optimization summary
  (let ([stats (cdr (assq 'optimization-stats report))])
       (display "-- SUGGESTED OPTIMIZATION --\n\n")
       (let ([original (cdr (assq 'expression report))]
             [optimized (cdr (assq 'optimized report))]
             [before (cdr (assq 'before-traversals stats))]
             [after (cdr (assq 'after-traversals stats))]
             [reduction (cdr (assq 'percent-reduction stats))])
            (display (format "  Before: ~a  (cost: ~a)\n" (truncate-expr original) before))
            (display (format "  After:  ~a  (cost: ~a)\n" (truncate-expr optimized) after))
            (display (format "  Savings: ~a%\n\n" reduction))))
  
  (display "+====+\n"))

;;; lzr : Expr -> Void
;;; Main entry point - analyze and display report.
(define (lzr expr)
  (display-lzr-report (lzr-report expr)))

;;; ====
;;; Equivalence Verification
;;; ====

;;; verify-rewrite : Expr x Expr [Alist] -> Boolean
;;; Test semantic equivalence of two expressions.
(define verify-rewrite
  (case-lambda
   [(before after)
    (verify-rewrite before after '())]
   [(before after sample-inputs)
    (display "\n")
    (display "+====+\n")
    (display "|                 EQUIVALENCE VERIFICATION                       |\n")
    (display "+====+\n\n")
    (display (format "  Before: ~a\n" (truncate-expr before)))
    (display (format "  After:  ~a\n\n" (truncate-expr after)))
    
    (let ([result (verify-equivalence before after)])
         (cond
          [(and (equiv-ok? result) (equiv-result result))
           (display (format "  Status: EQUIVALENT (~a)\n\n" (equiv-reason result)))
           #t]
          
          [(and (equiv-ok? result) (not (equiv-result result)))
           (display "  Status: NOT STRUCTURALLY EQUIVALENT\n")
           (let ([diff (find-difference before after)])
                (when diff
                      (display (format "  Difference: ~a\n" (diff-description diff)))))
           (display "\n")
           (if (null? sample-inputs)
               (begin
                (display "  Tip: Provide sample inputs for testing.\n")
                (display "  Usage: (verify-rewrite before after '((x . 1)))\n\n")
                #f)
               (test-with-samples before after sample-inputs))]
          
          [else
           (display "  Status: UNDETERMINED\n\n")
           #f]))]))

;;; test-with-samples : Expr x Expr x Alist -> Boolean
(define (test-with-samples before after sample-inputs)
  (let* ([before-sub (substitute-all before sample-inputs)]
         [after-sub (substitute-all after sample-inputs)]
         [result (verify-equivalence before-sub after-sub)])
        (display (format "  With inputs: ~a\n" sample-inputs))
        (display (format "    Before: ~a\n" (truncate-expr before-sub)))
        (display (format "    After:  ~a\n" (truncate-expr after-sub)))
        (if (and (equiv-ok? result) (equiv-result result))
            (begin (display "    Result: PASS\n\n") #t)
            (begin (display "    Result: FAIL\n\n") #f))))

;;; substitute-all : Expr x Alist -> Expr
(define (substitute-all expr bindings)
  (if (null? bindings)
      expr
      (substitute-all (substitute expr (caar bindings) (cdar bindings))
                      (cdr bindings))))

;;; ====
;;; Quick Analysis Helpers
;;; ====

;;; fusion-count : Expr -> Nat
(define (fusion-count expr)
  (length (analyze-fusion expr)))

;;; parallel-count : Expr -> Nat
(define (parallel-count expr)
  (length (suggest-parallel expr)))

;;; worth-optimizing? : Expr -> Boolean
(define (worth-optimizing? expr)
  (or (> (length (*lzr-fusion-detect* expr)) 0)
      (> (length (detect-parallel-regions expr)) 0)))

;;; ====
;;; Help
;;; ====

;;; lzr-help : -> Void
(define (lzr-help)
  (display "\n")
  (display "+====+\n")
  (display "|           FOLD-LZR: AUTO-PARALLELIZATION & FUSION              |\n")
  (display "+====+\n\n")
  (display "Commands:\n")
  (display "  (lzr expr)                    - Full analysis with report\n")
  (display "  (analyze-fusion expr)         - List fusion opportunities\n")
  (display "  (suggest-parallel expr)       - List parallelization opportunities\n")
  (display "  (optimize expr)               - Apply fusion rules\n")
  (display "  (lzr-report expr)             - Get structured report\n")
  (display "  (verify-rewrite before after) - Test semantic equivalence\n")
  (display "  (lzr-help)                    - This help message\n")
  (display "\n")
  (display "Quick checks (no output):\n")
  (display "  (fusion-count expr)           - Count fusion opportunities\n")
  (display "  (parallel-count expr)         - Count parallel opportunities\n")
  (display "  (worth-optimizing? expr)      - Check if optimization helps\n")
  (display "\n")
  (display "Example:\n")
  (display "  (lzr '(map f (map g xs)))\n")
  (display "  => Detects map-map fusion, suggests (map (compose f g) xs)\n\n"))

;;; Load message
(display "LZR optimization analyzer loaded.\n")
(display "Use (lzr-help) for usage information.\n")
