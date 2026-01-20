(load "core/util/profile.ss")

(doc 'module 'profile-analysis)
(doc 'description "Profile Analysis and Optimization Hints - analyzes profiler output to suggest optimizations")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Features: tail-call opportunities, map/fold fusion candidates, O(n) → O(1) lookup improvements, repeated computation detection")

(doc 'section 'hint-data-structure)

(define (make-hint type severity location message suggestion)
  (doc 'description "Create an optimization hint")
  (doc 'param '(type "Hint type: tail-call, fusion, lookup, memoize, inline"))
  (doc 'param '(severity "Severity: info, warning, critical"))
  (doc 'param '(location "Location symbol or list"))
  (doc 'param '(message "Hint message"))
  (doc 'param '(suggestion "Suggestion text"))
  (doc 'returns "Hint structure")

  \`(hint
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

(doc 'section 'pattern-detection-tail-call)

(define (detect-tail-call-opportunities p)
  (doc 'description "Detect recursive calls that could be tail-calls")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of hints")

  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [recursive-fns (filter (lambda (entry)
                                        (> (cddr entry) 10))
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

(doc 'section 'pattern-detection-fusion)

(define *fusable-ops* '(map filter foldl foldr))

(define (detect-fusion-opportunities p)
  (doc 'description "Detect consecutive map/filter/fold operations that could fuse")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of hints")

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

(define (find-adjacent-calls node ops)
  (doc 'description "Find adjacent calls to operations in ops")
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

(doc 'section 'pattern-detection-lookup)

(define *lookup-ops* '(assq assv assoc member memq memv elem))

(define (detect-lookup-opportunities p)
  (doc 'description "Detect repeated assq/member calls that could use hashtables")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of hints")

  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [lookup-fns (filter (lambda (entry)
                                     (member (car entry) *lookup-ops*))
                             by-fn)]
         [expensive (filter (lambda (entry)
                                    (> (cadr entry) 100))
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

(doc 'section 'pattern-detection-memoize)

(define (detect-memoization-candidates p)
  (doc 'description "Detect pure functions called many times (potential memoization)")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of hints")

  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [frequent (filter (lambda (entry)
                                   (and (> (cddr entry) 50)
                                        (not (member (car entry) *lookup-ops*))
                                        (not (eq? (car entry) 'if))
                                        (not (eq? (car entry) 'let))))
                           by-fn)]
         [candidates (filter (lambda (entry)
                                     (let ([fuel-per-call (/ (cadr entry) (cddr entry))])
                                          (> fuel-per-call 5)))
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

(doc 'section 'pattern-detection-inline)

(define (detect-inline-candidates p)
  (doc 'description "Detect small functions called many times (inline candidates)")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of hints")

  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [frequent-small (filter (lambda (entry)
                                         (let ([fuel (cadr entry)]
                                               [calls (cddr entry)])
                                              (and (> calls 100)
                                                   (< (/ fuel calls) 3))))
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

(doc 'section 'full-analysis)

(define (analyze-profile p)
  (doc 'description "Run all analyses and collect hints")
  (doc 'param '(p "Profiler"))
  (doc 'returns "List of all hints")

  (append
   (detect-tail-call-opportunities p)
   (detect-fusion-opportunities p)
   (detect-lookup-opportunities p)
   (detect-memoization-candidates p)
   (detect-inline-candidates p)))

(doc 'section 'hint-rendering)

(define (severity-symbol sev)
  (case sev
        [(critical) "!!!"]
        [(warning) "! "]
        [(info) "  "]
        [else "  "]))

(define (severity-color sev)
  (let ([esc (integer->char 27)])
       (case sev
             [(critical) (string esc #\\[ #\\3 #\\1 #\\m)]
             [(warning) (string esc #\\[ #\\3 #\\3 #\\m)]
             [(info) (string esc #\\[ #\\3 #\\6 #\\m)]
             [else ""])))

(define (reset-color)
  (string (integer->char 27) #\\[ #\\0 #\\m))

(define (render-hint h)
  (doc 'description "Render a hint as a string")
  (let ([type (hint-type h)]
        [sev (hint-severity h)]
        [loc (hint-location h)]
        [msg (hint-message h)]
        [sug (hint-suggestion h)])
       (format "  ~a [~a] ~a\\n      ~a\\n      >> ~a\\n\\n"
               (severity-symbol sev)
               type
               (if (symbol? loc) loc (format "~a" loc))
               msg
               sug)))

(define (render-hints hints)
  (doc 'description "Render all hints as a string")
  (if (null? hints)
      "  No optimization hints detected.\\n"
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

(define (display-analysis p)
  (doc 'description "Display optimization hints for a profile")
  (doc 'param '(p "Profiler"))

  (let ([hints (analyze-profile p)])
       (display "\\n")
       (display "  ====\\n")
       (display "           OPTIMIZATION HINTS\\n")
       (display "  ====\\n\\n")
       (display (render-hints hints))
       (display (format "  Total: ~a hints\\n\\n" (length hints)))))

(doc 'section 'comparative-analysis)

(define (compare-profiles p1 p2)
  (doc 'description "Compare two profile runs (before/after optimization)")
  (doc 'param '(p1 "Profiler before"))
  (doc 'param '(p2 "Profiler after"))
  (doc 'returns "String comparison report")

  (let* ([stats1 (profile-stats p1)]
         [stats2 (profile-stats p2)]
         [used1 (cdr (assq 'used-fuel stats1))]
         [used2 (cdr (assq 'used-fuel stats2))]
         [diff (- used1 used2)]
         [pct (if (zero? used1) 0 (* 100.0 (/ diff used1)))])
        (format
         "  Profile Comparison\\n  ====\\n  Before: ~a fuel\\n  After:  ~a fuel\\n  Saved:  ~a fuel (~a%)\\n"
         used1 used2 diff (round pct))))

(doc 'section 'regression-detection)

(define (check-regression p baseline threshold-pct)
  (doc 'description "Check if profile exceeds baseline by more than threshold %")
  (doc 'param '(p "Profiler"))
  (doc 'param '(baseline "Baseline fuel count"))
  (doc 'param '(threshold-pct "Threshold percentage"))
  (doc 'returns "List of hints if regression detected")

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
