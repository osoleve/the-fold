;;; boundary/tests/test-profile-analysis.ss — Tests for Profile Analysis Heuristics
;;;
;;; Tests the optimization hint detection in profile-analysis.ss:
;;;   - Hint data structure
;;;   - Tail-call opportunity detection
;;;   - Fusion opportunity detection
;;;   - Lookup opportunity detection
;;;   - Memoization candidate detection
;;;   - Inline candidate detection
;;;   - Hint rendering
;;;   - Profile comparison and regression detection

(load "core/test-framework.ss")
(load "boundary/diagnostics/profile-analysis.ss")

;;; ====
;;; Mock Profiler Construction
;;; ====

;;; Create a mock profiler with specified stats
;;; by-function is list of (name fuel . calls)
(define (make-mock-profiler total-fuel used-fuel by-function root)
  `(profiler
    (initial-fuel . ,total-fuel)
    (root . ,root)
    (mock-stats . ((total-fuel . ,total-fuel)
                   (used-fuel . ,used-fuel)
                   (efficiency . ,(if (zero? total-fuel) 0 (/ used-fuel total-fuel)))
                   (by-function . ,by-function)))))

;;; Override profile-stats for mock profilers
(define original-profile-stats profile-stats)

(define (profile-stats p)
  (if (assq 'mock-stats (cdr p))
      (cdr (assq 'mock-stats (cdr p)))
      (original-profile-stats p)))

;;; Create a mock node for tree testing
(define (make-mock-node name children)
  `(node
    (name . ,name)
    (start-fuel . 1000)
    (end-fuel . 900)
    (children . ,children)
    (call-count . 1)))

;;; ====
;;; Hint Data Structure Tests
;;; ====

(test-group hint-data-structure
            
            (define-test make-hint-creates-structure
              (let ([h (make-hint 'tail-call 'warning 'foo "message" "suggestion")])
                   (assert-true (hint? h))))
            
            (define-test hint-type-accessor
              (let ([h (make-hint 'fusion 'info 'bar "msg" "sug")])
                   (assert-equal 'fusion (hint-type h))))
            
            (define-test hint-severity-accessor
              (let ([h (make-hint 'lookup 'critical 'baz "msg" "sug")])
                   (assert-equal 'critical (hint-severity h))))
            
            (define-test hint-location-accessor
              (let ([h (make-hint 'memoize 'warning 'qux "msg" "sug")])
                   (assert-equal 'qux (hint-location h))))
            
            (define-test hint-message-accessor
              (let ([h (make-hint 'inline 'info 'fn "the message" "sug")])
                   (assert-equal "the message" (hint-message h))))
            
            (define-test hint-suggestion-accessor
              (let ([h (make-hint 'tail-call 'info 'fn "msg" "the suggestion")])
                   (assert-equal "the suggestion" (hint-suggestion h))))
            
            (define-test hint-predicate-rejects-non-hints
              (assert-false (hint? '(not a hint)))
              (assert-false (hint? 42))
              (assert-false (hint? "string")))
            )

;;; ====
;;; Tail-Call Opportunity Detection Tests
;;; ====

(test-group tail-call-detection
            
            (define-test no-hints-for-low-call-count
              (let* ([by-fn '((foo 50 . 5) (bar 30 . 3))]  ; All < 10 calls
                     [p (make-mock-profiler 1000 500 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-tail-call-opportunities p))))
            
            (define-test detects-recursive-function-over-10-calls
              (let* ([by-fn '((recurse 500 . 50))]  ; 50 calls > 10
                     [p (make-mock-profiler 1000 500 by-fn (make-mock-node 'root '()))]
                     [hints (detect-tail-call-opportunities p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'tail-call (hint-type (car hints)))
                    (assert-equal 'recurse (hint-location (car hints)))))
            
            (define-test severity-info-under-100-calls
              (let* ([by-fn '((recurse 200 . 50))]  ; 50 calls: info
                     [p (make-mock-profiler 1000 200 by-fn (make-mock-node 'root '()))]
                     [hints (detect-tail-call-opportunities p)])
                    (assert-equal 'info (hint-severity (car hints)))))
            
            (define-test severity-warning-over-100-calls
              (let* ([by-fn '((recurse 1000 . 150))]  ; 150 calls: warning
                     [p (make-mock-profiler 2000 1000 by-fn (make-mock-node 'root '()))]
                     [hints (detect-tail-call-opportunities p)])
                    (assert-equal 'warning (hint-severity (car hints)))))
            
            (define-test detects-multiple-recursive-functions
              (let* ([by-fn '((foo 100 . 20) (bar 200 . 30) (baz 50 . 5))]
                     [p (make-mock-profiler 1000 350 by-fn (make-mock-node 'root '()))]
                     [hints (detect-tail-call-opportunities p)])
                    (assert-equal 2 (length hints))))  ; foo and bar, not baz
            )

;;; ====
;;; Fusion Opportunity Detection Tests
;;; ====

(test-group fusion-detection
            
            (define-test no-hints-for-single-op
              (let* ([root (make-mock-node 'root
                                           (list (make-mock-node 'map '())))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal '() hints)))
            
            (define-test detects-adjacent-map-map
              (let* ([root (make-mock-node 'root
                                           (list (make-mock-node 'map '())
                                                 (make-mock-node 'map '())))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'fusion (hint-type (car hints)))))
            
            (define-test detects-adjacent-map-filter
              (let* ([root (make-mock-node 'root
                                           (list (make-mock-node 'map '())
                                                 (make-mock-node 'filter '())))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal 1 (length hints))))
            
            (define-test detects-adjacent-filter-foldl
              (let* ([root (make-mock-node 'root
                                           (list (make-mock-node 'filter '())
                                                 (make-mock-node 'foldl '())))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal 1 (length hints))))
            
            (define-test no-fusion-for-non-adjacent
              (let* ([root (make-mock-node 'root
                                           (list (make-mock-node 'map '())
                                                 (make-mock-node 'other '())
                                                 (make-mock-node 'filter '())))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal '() hints)))
            
            (define-test finds-fusion-in-nested-children
              (let* ([inner (make-mock-node 'inner
                                            (list (make-mock-node 'map '())
                                                  (make-mock-node 'map '())))]
                     [root (make-mock-node 'root (list inner))]
                     [p (make-mock-profiler 1000 100 '() root)]
                     [hints (detect-fusion-opportunities p)])
                    (assert-equal 1 (length hints))))
            )

;;; ====
;;; Lookup Opportunity Detection Tests
;;; ====

(test-group lookup-detection
            
            (define-test no-hints-for-cheap-lookups
              (let* ([by-fn '((assq 50 . 10))]  ; < 100 fuel
                     [p (make-mock-profiler 1000 50 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-lookup-opportunities p))))
            
            (define-test detects-expensive-assq
              (let* ([by-fn '((assq 200 . 50))]  ; > 100 fuel
                     [p (make-mock-profiler 1000 200 by-fn (make-mock-node 'root '()))]
                     [hints (detect-lookup-opportunities p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'lookup (hint-type (car hints)))
                    (assert-equal 'assq (hint-location (car hints)))))
            
            (define-test detects-expensive-member
              (let* ([by-fn '((member 300 . 100))]
                     [p (make-mock-profiler 1000 300 by-fn (make-mock-node 'root '()))]
                     [hints (detect-lookup-opportunities p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'member (hint-location (car hints)))))
            
            (define-test severity-info-under-500-fuel
              (let* ([by-fn '((assq 200 . 50))]
                     [p (make-mock-profiler 1000 200 by-fn (make-mock-node 'root '()))]
                     [hints (detect-lookup-opportunities p)])
                    (assert-equal 'info (hint-severity (car hints)))))
            
            (define-test severity-warning-over-500-fuel
              (let* ([by-fn '((assq 600 . 100))]
                     [p (make-mock-profiler 1000 600 by-fn (make-mock-node 'root '()))]
                     [hints (detect-lookup-opportunities p)])
                    (assert-equal 'warning (hint-severity (car hints)))))
            
            (define-test ignores-non-lookup-ops
              (let* ([by-fn '((custom-fn 500 . 100))]
                     [p (make-mock-profiler 1000 500 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-lookup-opportunities p))))
            )

;;; ====
;;; Memoization Candidate Detection Tests
;;; ====

(test-group memoization-detection
            
            (define-test no-hints-for-low-call-count
              (let* ([by-fn '((expensive 500 . 10))]  ; < 50 calls
                     [p (make-mock-profiler 1000 500 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-memoization-candidates p))))
            
            (define-test no-hints-for-trivial-work
              (let* ([by-fn '((cheap 100 . 100))]  ; 1 fuel/call < 5
                     [p (make-mock-profiler 1000 100 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-memoization-candidates p))))
            
            (define-test detects-expensive-frequent-function
              (let* ([by-fn '((fib 1000 . 100))]  ; 100 calls, 10 fuel/call
                     [p (make-mock-profiler 2000 1000 by-fn (make-mock-node 'root '()))]
                     [hints (detect-memoization-candidates p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'memoize (hint-type (car hints)))
                    (assert-equal 'fib (hint-location (car hints)))))
            
            (define-test excludes-lookup-ops-from-memoization
              (let* ([by-fn '((assq 500 . 100))]  ; Would qualify but is lookup op
                     [p (make-mock-profiler 1000 500 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-memoization-candidates p))))
            
            (define-test excludes-control-flow-from-memoization
              (let* ([by-fn '((if 500 . 100) (let 500 . 100))]
                     [p (make-mock-profiler 2000 1000 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-memoization-candidates p))))
            
            (define-test severity-warning-over-200-calls
              (let* ([by-fn '((compute 2000 . 250))]  ; 250 > 200 calls
                     [p (make-mock-profiler 3000 2000 by-fn (make-mock-node 'root '()))]
                     [hints (detect-memoization-candidates p)])
                    (assert-equal 'warning (hint-severity (car hints)))))
            )

;;; ====
;;; Inline Candidate Detection Tests
;;; ====

(test-group inline-detection
            
            (define-test no-hints-for-low-call-count
              (let* ([by-fn '((small 50 . 50))]  ; < 100 calls
                     [p (make-mock-profiler 1000 50 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-inline-candidates p))))
            
            (define-test no-hints-for-expensive-functions
              (let* ([by-fn '((expensive 1000 . 100))]  ; 10 fuel/call > 3
                     [p (make-mock-profiler 2000 1000 by-fn (make-mock-node 'root '()))])
                    (assert-equal '() (detect-inline-candidates p))))
            
            (define-test detects-trivial-frequent-function
              (let* ([by-fn '((getter 200 . 150))]  ; ~1.3 fuel/call, 150 calls
                     [p (make-mock-profiler 1000 200 by-fn (make-mock-node 'root '()))]
                     [hints (detect-inline-candidates p)])
                    (assert-equal 1 (length hints))
                    (assert-equal 'inline (hint-type (car hints)))
                    (assert-equal 'getter (hint-location (car hints)))))
            
            (define-test inline-always-info-severity
              (let* ([by-fn '((tiny 100 . 200))]
                     [p (make-mock-profiler 1000 100 by-fn (make-mock-node 'root '()))]
                     [hints (detect-inline-candidates p)])
                    (assert-equal 'info (hint-severity (car hints)))))
            )

;;; ====
;;; Full Analysis Tests
;;; ====

(test-group full-analysis
            
            (define-test analyze-profile-combines-all-detectors
              (let* ([by-fn '((recurse 500 . 50)    ; tail-call
                              (assq 200 . 30)        ; lookup
                              (compute 600 . 60)     ; memoize
                              (getter 150 . 150))]   ; inline
                     [root (make-mock-node 'root
                                           (list (make-mock-node 'map '())
                                                 (make-mock-node 'filter '())))]  ; fusion
                     [p (make-mock-profiler 2000 1450 by-fn root)]
                     [hints (analyze-profile p)])
                    ;; Should have hints from multiple detectors
                    (assert-true (> (length hints) 0))
                    ;; Check we got different types
                    (let ([types (map hint-type hints)])
                         (assert-true (and (member 'tail-call types) #t))
                         (assert-true (and (member 'fusion types) #t)))))
            
            (define-test analyze-profile-returns-empty-for-clean-profile
              (let* ([by-fn '((foo 10 . 2) (bar 20 . 3))]  ; Nothing triggers
                     [root (make-mock-node 'root '())]
                     [p (make-mock-profiler 100 30 by-fn root)]
                     [hints (analyze-profile p)])
                    (assert-equal '() hints)))
            )

;;; ====
;;; Hint Rendering Tests
;;; ====

(test-group hint-rendering
            
            (define-test severity-symbol-critical
              (assert-equal "!!!" (severity-symbol 'critical)))
            
            (define-test severity-symbol-warning
              (assert-equal "! " (severity-symbol 'warning)))
            
            (define-test severity-symbol-info
              (assert-equal "  " (severity-symbol 'info)))
            
            (define-test severity-color-returns-string
              (assert-true (string? (severity-color 'critical)))
              (assert-true (string? (severity-color 'warning)))
              (assert-true (string? (severity-color 'info))))
            
            (define-test reset-color-returns-string
              (assert-true (string? (reset-color))))
            
            (define-test render-hint-returns-string
              (let* ([h (make-hint 'tail-call 'warning 'foo "msg" "sug")]
                     [rendered (render-hint h)])
                    (assert-true (string? rendered))
                    (assert-true (> (string-length rendered) 0))))
            
            (define-test render-hints-empty-list
              (let ([rendered (render-hints '())])
                   (assert-true (string? rendered))
                   (assert-true (and (string-contains rendered "No optimization") #t))))
            
            (define-test render-hints-sorts-by-severity
              (let* ([h1 (make-hint 'a 'info 'x "m" "s")]
                     [h2 (make-hint 'b 'critical 'y "m" "s")]
                     [h3 (make-hint 'c 'warning 'z "m" "s")]
                     [rendered (render-hints (list h1 h2 h3))])
                    ;; Critical should come first, then warning, then info
                    (let ([pos-crit (string-contains rendered "[b]")]
                          [pos-warn (string-contains rendered "[c]")]
                          [pos-info (string-contains rendered "[a]")])
                         (assert-true (and pos-crit pos-warn pos-info
                                           (< pos-crit pos-warn)
                                           (< pos-warn pos-info))))))
            )

;;; ====
;;; Profile Comparison Tests
;;; ====

(test-group profile-comparison
            
            (define-test compare-profiles-shows-improvement
              (let* ([p1 (make-mock-profiler 1000 800 '() (make-mock-node 'root '()))]
                     [p2 (make-mock-profiler 1000 400 '() (make-mock-node 'root '()))]
                     [result (compare-profiles p1 p2)])
                    (assert-true (string? result))
                    (assert-true (and (string-contains result "800") #t))
                    (assert-true (and (string-contains result "400") #t))))
            
            (define-test compare-profiles-shows-saved-fuel
              (let* ([p1 (make-mock-profiler 1000 1000 '() (make-mock-node 'root '()))]
                     [p2 (make-mock-profiler 1000 500 '() (make-mock-node 'root '()))]
                     [result (compare-profiles p1 p2)])
                    (assert-true (and (string-contains result "500") #t))  ; Saved amount
                    (assert-true (and (string-contains result "50") #t))))  ; ~50%
            )

;;; ====
;;; Regression Detection Tests
;;; ====

(test-group regression-detection
            
            (define-test no-regression-within-threshold
              (let* ([p (make-mock-profiler 1000 105 '() (make-mock-node 'root '()))]
                     [hints (check-regression p 100 10)])  ; 5% < 10% threshold
                    (assert-equal '() hints)))
            
            (define-test detects-regression-over-threshold
              (let* ([p (make-mock-profiler 1000 150 '() (make-mock-node 'root '()))]
                     [hints (check-regression p 100 10)])  ; 50% > 10% threshold
                    (assert-equal 1 (length hints))
                    (assert-equal 'regression (hint-type (car hints)))
                    (assert-equal 'critical (hint-severity (car hints)))))
            
            (define-test regression-hint-includes-percentages
              (let* ([p (make-mock-profiler 1000 200 '() (make-mock-node 'root '()))]
                     [hints (check-regression p 100 10)]
                     [msg (hint-message (car hints))])
                    (assert-true (and (string-contains msg "100") #t))  ; baseline
                    (assert-true (and (string-contains msg "200") #t))))  ; current
            
            (define-test handles-zero-baseline
              (let* ([p (make-mock-profiler 1000 100 '() (make-mock-node 'root '()))]
                     [hints (check-regression p 0 10)])
                    (assert-equal '() hints)))
            )

;;; ====
;;; Helper: string-contains
;;; ====

(define (string-contains str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) i]
             [else (loop (+ i 1))]))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
