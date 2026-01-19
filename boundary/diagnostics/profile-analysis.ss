;;; boundary/diagnostics/profile-analysis.ss — Profile Analysis and Optimization Hints
;;;
;;; Analyzes profiler output to suggest optimizations:
;;;   - Tail-call opportunities
;;;   - Map/fold fusion candidates
;;;   - O(n) → O(1) lookup improvements
;;;   - Repeated computation detection
;;;
;;; This is Shell code: heuristic analysis with suggestions.
;;;
;;; Dependencies:
;;;   - core/util/profile.ss

(load "core/util/profile.ss")

;;; ====
;;; Hint Data Structure
;;; ====

;;; A hint is:
;;;   (hint type severity location message suggestion)
;;;
;;; Types: tail-call, fusion, lookup, memoize, inline
;;; Severity: info, warning, critical

(define (make-hint type severity location message suggestion)
  `(hint
    (type . ,type)
    (severity . ,severity)
    (location . ,location)
    (message . ,message)
    (suggestion . ,suggestion)))

(define (hint? h) (and (pair? h) (eq? (car h) 'hint)))
(define (hint-get h key)
  (let ([e (assq key (cdr h))])
       (and e (cdr e))))
(define (hint-type h) (hint-get h 'type))
(define (hint-severity h) (hint-get h 'severity))
(define (hint-location h) (hint-get h 'location))
(define (hint-message h) (hint-get h 'message))
(define (hint-suggestion h) (hint-get h 'suggestion))

;;; ====
;;; Pattern Detection: Tail-Call Opportunities
;;; ====

;;; Detect recursive calls that could be tail-calls
;;; Pattern: (fix name (fn (args) ... (name ...)))
;;; where (name ...) is not in tail position

(define (detect-tail-call-opportunities p)
  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [recursive-fns (filter (lambda (entry)
                                        (> (cddr entry) 10))  ; Called > 10 times
                                by-fn)])
        (map (lambda (entry)
                     (let ([name (car entry)]
                           [fuel (cadr entry)]
                           [calls (cddr entry)])
                          (make-hint
                           'tail-call
                           (if (> calls 100) 'warning 'info)
                           name
                           (format "~a called ~a times, consuming ~a fuel"
                                   name calls fuel)
                           (format "Consider making recursive call to ~a a tail-call for stack efficiency"
                                   name))))
             recursive-fns)))

;;; ====
;;; Pattern Detection: Fusion Opportunities
;;; ====

;;; Detect consecutive map/filter/fold operations that could fuse
;;; Pattern: (map f (map g xs)) → (map (compose f g) xs)
;;; Pattern: (map f (filter p xs)) → (filter-map ...)

(define *fusable-ops* '(map filter foldl foldr))

(define (detect-fusion-opportunities p)
  (let* ([root (profiler-root p)]
         [pairs (find-adjacent-calls root *fusable-ops*)])
        (map (lambda (pair)
                     (let ([outer (car pair)]
                           [inner (cadr pair)])
                          (make-hint
                           'fusion
                           'info
                           (list outer inner)
                           (format "~a followed by ~a could be fused" inner outer)
                           (format "Combine into single traversal: (fuse-~a-~a ...)" inner outer))))
             pairs)))

;;; find-adjacent-calls : Node × List → List of pairs
(define (find-adjacent-calls node ops)
  (let* ([children (node-children node)]
         [pairs (find-adjacent-in-list children ops)]
         [recursive-pairs (apply append
                                 (map (lambda (c) (find-adjacent-calls c ops))
                                      children))])
        (append pairs recursive-pairs)))

(define (find-adjacent-in-list nodes ops)
  (if (or (null? nodes) (null? (cdr nodes)))
      '()
      (let* ([a (car nodes)]
             [b (cadr nodes)]
             [a-name (node-name a)]
             [b-name (node-name b)])
            (if (and (member a-name ops)
                     (member b-name ops))
                (cons (list a-name b-name)
                      (find-adjacent-in-list (cdr nodes) ops))
                (find-adjacent-in-list (cdr nodes) ops)))))

;;; ====
;;; Pattern Detection: Expensive Lookups
;;; ====

;;; Detect repeated assq/assv/member calls that could use hashtables
;;; Pattern: Many calls to assq with similar call patterns

(define *lookup-ops* '(assq assv assoc member memq memv elem))

(define (detect-lookup-opportunities p)
  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [lookup-fns (filter (lambda (entry)
                                     (member (car entry) *lookup-ops*))
                             by-fn)]
         [expensive (filter (lambda (entry)
                                    (> (cadr entry) 100))  ; > 100 fuel
                            lookup-fns)])
        (map (lambda (entry)
                     (let ([name (car entry)]
                           [fuel (cadr entry)]
                           [calls (cddr entry)])
                          (make-hint
                           'lookup
                           (if (> fuel 500) 'warning 'info)
                           name
                           (format "~a: O(n) lookup called ~a times, ~a fuel"
                                   name calls fuel)
                           "Consider using a hashtable for O(1) lookup")))
             expensive)))

;;; ====
;;; Pattern Detection: Memoization Candidates
;;; ====

;;; Detect pure functions called many times (potential memoization)

(define (detect-memoization-candidates p)
  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [frequent (filter (lambda (entry)
                                   (and (> (cddr entry) 50)   ; > 50 calls
                                        (not (member (car entry) *lookup-ops*))
                                        (not (eq? (car entry) 'if))
                                        (not (eq? (car entry) 'let))))
                           by-fn)]
         ;; Filter to only pure-looking functions (no prim side effects)
         [candidates (filter (lambda (entry)
                                     (let ([fuel-per-call (/ (cadr entry) (cddr entry))])
                                          (> fuel-per-call 5)))  ; Non-trivial work
                             frequent)])
        (map (lambda (entry)
                     (let ([name (car entry)]
                           [fuel (cadr entry)]
                           [calls (cddr entry)])
                          (make-hint
                           'memoize
                           (if (> calls 200) 'warning 'info)
                           name
                           (format "~a called ~a times (~a fuel total)"
                                   name calls fuel)
                           (format "If ~a is pure with repeated inputs, consider memoization"
                                   name))))
             candidates)))

;;; ====
;;; Pattern Detection: Inline Candidates
;;; ====

;;; Detect small functions called many times (inline candidates)

(define (detect-inline-candidates p)
  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [frequent-small (filter (lambda (entry)
                                         (let ([fuel (cadr entry)]
                                               [calls (cddr entry)])
                                              (and (> calls 100)
                                                   (< (/ fuel calls) 3))))  ; < 3 fuel per call
                                 by-fn)])
        (map (lambda (entry)
                     (let ([name (car entry)]
                           [fuel (cadr entry)]
                           [calls (cddr entry)])
                          (make-hint
                           'inline
                           'info
                           name
                           (format "~a: trivial function called ~a times"
                                   name calls)
                           (format "Consider inlining ~a to reduce call overhead"
                                   name))))
             frequent-small)))

;;; ====
;;; Full Analysis
;;; ====

;;; analyze-profile : Profiler → List of Hints
;;; Run all analyses and collect hints
(define (analyze-profile p)
  (append
   (detect-tail-call-opportunities p)
   (detect-fusion-opportunities p)
   (detect-lookup-opportunities p)
   (detect-memoization-candidates p)
   (detect-inline-candidates p)))

;;; ====
;;; Hint Rendering
;;; ====

(define (severity-symbol sev)
  (case sev
        [(critical) "!!!"]
        [(warning) "! "]
        [(info) "  "]
        [else "  "]))

(define (severity-color sev)
  ;; ANSI color codes (if terminal supports)
  ;; Use string constructor since \x1b syntax is invalid in Chez
  (let ([esc (integer->char 27)])
       (case sev
             [(critical) (string esc #\[ #\3 #\1 #\m)]  ; Red
             [(warning) (string esc #\[ #\3 #\3 #\m)]   ; Yellow
             [(info) (string esc #\[ #\3 #\6 #\m)]      ; Cyan
             [else ""])))

(define (reset-color)
  (string (integer->char 27) #\[ #\0 #\m))

;;; render-hint : Hint → String
(define (render-hint h)
  (let ([type (hint-type h)]
        [sev (hint-severity h)]
        [loc (hint-location h)]
        [msg (hint-message h)]
        [sug (hint-suggestion h)])
       (format "  ~a [~a] ~a\n      ~a\n      >> ~a\n\n"
               (severity-symbol sev)
               type
               (if (symbol? loc) loc (format "~a" loc))
               msg
               sug)))

;;; render-hints : List of Hints → String
(define (render-hints hints)
  (if (null? hints)
      "  No optimization hints detected.\n"
      (apply string-append
             (map render-hint
                  (list-sort (lambda (a b)
                                     (let ([sa (hint-severity a)]
                                           [sb (hint-severity b)])
                                          (cond
                                           [(eq? sa 'critical) #t]
                                           [(eq? sb 'critical) #f]
                                           [(eq? sa 'warning) #t]
                                           [(eq? sb 'warning) #f]
                                           [else #f])))
                             hints)))))

;;; display-analysis : Profiler → void
(define (display-analysis p)
  (let ([hints (analyze-profile p)])
       (display "\n")
       (display "  ====\n")
       (display "           OPTIMIZATION HINTS\n")
       (display "  ====\n\n")
       (display (render-hints hints))
       (display (format "  Total: ~a hints\n\n" (length hints)))))

;;; ====
;;; Comparative Analysis
;;; ====

;;; compare-profiles : Profiler × Profiler → String
;;; Compare two profile runs (before/after optimization)
(define (compare-profiles p1 p2)
  (let* ([stats1 (profile-stats p1)]
         [stats2 (profile-stats p2)]
         [used1 (cdr (assq 'used-fuel stats1))]
         [used2 (cdr (assq 'used-fuel stats2))]
         [diff (- used1 used2)]
         [pct (if (zero? used1) 0 (* 100.0 (/ diff used1)))])
        (format
         "  Profile Comparison\n  ====\n  Before: ~a fuel\n  After:  ~a fuel\n  Saved:  ~a fuel (~a%)\n"
         used1 used2 diff (round pct))))

;;; ====
;;; Regression Detection
;;; ====

;;; check-regression : Profiler × Nat → List of Hints
;;; Check if profile exceeds baseline by more than threshold %
(define (check-regression p baseline threshold-pct)
  (let* ([stats (profile-stats p)]
         [used (cdr (assq 'used-fuel stats))]
         [pct-increase (if (zero? baseline) 0
                           (* 100.0 (/ (- used baseline) baseline)))])
        (if (> pct-increase threshold-pct)
            (list (make-hint
                   'regression
                   'critical
                   'performance
                   (format "Fuel usage increased by ~a% (baseline: ~a, current: ~a)"
                           (round pct-increase) baseline used)
                   "Review recent changes for performance regressions"))
            '())))
