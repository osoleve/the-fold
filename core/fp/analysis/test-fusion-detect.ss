;;; core/fp/analysis/test-fusion-detect.ss --- Tests for Static Fusion Detection
;;;
;;; Tests the fusion pattern detection module.

(load "core/test-framework.ss")
(load "core/fp/analysis/fusion-detect.ss")

(display "\n")
(display "=== Fusion Detection Tests ===\n")
(display "\n")

(test-group fusion-detect
            
            ;;; ============================================================
            ;;; Data Structure Tests
            ;;; ============================================================
            
            (define-test fusion-opportunity-structure
              (let ([opp (make-fusion-opportunity
                          'map-map-fuse
                          '(1 2)
                          '(map f (map g xs))
                          '(map (compose f g) xs)
                          50
                          'safe)])
                   (assert-true (fusion-opportunity? opp))
                   (assert-equal 'map-map-fuse (fusion-type opp))
                   (assert-equal '(1 2) (fusion-location opp))
                   (assert-equal 50 (fusion-savings opp))
                   (assert-equal 'safe (fusion-confidence opp))))
            
            (define-test fusion-opportunity-rejects-invalid
              (assert-false (fusion-opportunity? '()))
              (assert-false (fusion-opportunity? 42))
              (assert-false (fusion-opportunity? '(not-an-opportunity)))
              (assert-false (fusion-opportunity? '(fusion-opportunity))))
            
            ;;; ============================================================
            ;;; Pattern Matching: map-map
            ;;; ============================================================
            
            (define-test detect-map-map-fusion
              (let* ([expr '(map f (map g xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'map-map-fuse (fusion-type opp))
                         (assert-equal '() (fusion-location opp))
                         (assert-equal '(map (compose f g) xs) (fusion-after opp)))))
            
            (define-test detect-nested-map-map-fusion
              (let* ([expr '(let ([result (map f (map g xs))]) result)]
                     [opps (detect-fusion-static expr)])
                    (assert-true (>= (length opps) 1))
                    ;; find returns #f if not found, or the element
                    (let ([opp (find (lambda (o) (eq? (fusion-type o) 'map-map-fuse)) opps)])
                         (assert-true (fusion-opportunity? opp)))))
            
            (define-test map-map-with-lambda-functions
              (let* ([expr '(map (lambda (x) (* x 2)) (map (lambda (y) (+ y 1)) xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'safe (fusion-confidence (car opps)))))
            
            ;;; ============================================================
            ;;; Pattern Matching: filter-map
            ;;; ============================================================
            
            (define-test detect-filter-map-fusion
              (let* ([expr '(map f (filter p xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'filter-map-fuse (fusion-type opp))
                         (assert-equal '(filter-map p f xs) (fusion-after opp)))))
            
            (define-test filter-map-with-complex-predicates
              (let* ([expr '(map transform (filter (lambda (x) (> x 0)) numbers))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'filter-map-fuse (fusion-type (car opps)))))
            
            ;;; ============================================================
            ;;; Pattern Matching: map-filter
            ;;; ============================================================
            
            (define-test detect-map-filter-fusion
              (let* ([expr '(filter p (map f xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'map-filter-fuse (fusion-type opp))
                         (assert-equal '(filter-map (compose p f) f xs) (fusion-after opp)))))
            
            ;;; ============================================================
            ;;; Pattern Matching: fold-map
            ;;; ============================================================
            
            (define-test detect-fold-map-fusion-foldl
              (let* ([expr '(foldl + 0 (map square xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'fold-map-fuse (fusion-type opp)))))
            
            (define-test detect-fold-map-fusion-fold-left
              (let* ([expr '(fold-left cons '() (map inc xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'fold-map-fuse (fusion-type (car opps)))))
            
            (define-test detect-fold-map-fusion-foldr
              (let* ([expr '(foldr cons '() (map f xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'fold-map-fuse (fusion-type opp))
                         ;; Right fold should generate (lambda (x acc) ...)
                         (let ([after (fusion-after opp)])
                              (assert-equal 'foldr (car after))))))
            
            ;;; ============================================================
            ;;; Pattern Matching: concat-map
            ;;; ============================================================
            
            (define-test detect-concat-map-fusion-flatten
              (let* ([expr '(flatten (map f xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'concat-map-fuse (fusion-type opp))
                         (assert-equal '(flatMap f xs) (fusion-after opp)))))
            
            (define-test detect-concat-map-fusion-apply-append
              (let* ([expr '(apply append (map expand items))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'concat-map-fuse (fusion-type (car opps)))))
            
            ;;; ============================================================
            ;;; Pattern Matching: stream-map-map
            ;;; ============================================================
            
            (define-test detect-stream-map-map-fusion
              (let* ([expr '(stream-map f (stream-map g s))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (car opps)])
                         (assert-equal 'stream-map-map-fuse (fusion-type opp))
                         (assert-equal '(stream-map (compose f g) s) (fusion-after opp)))))
            
            ;;; ============================================================
            ;;; Confidence Estimation
            ;;; ============================================================
            
            (define-test pure-primitives-get-safe-confidence
              (let* ([expr '(map + (map - xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'safe (fusion-confidence (car opps)))))
            
            (define-test lambdas-get-safe-confidence
              (let* ([expr '(map (lambda (x) x) (map (lambda (y) y) xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'safe (fusion-confidence (car opps)))))
            
            (define-test unknown-functions-get-likely-pure-confidence
              ;; Unknown symbol functions (not lambda, not known-pure) get likely-pure
              (let* ([expr '(map custom-fn (map another-fn xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (> (length opps) 0))
                    ;; Both custom-fn and another-fn are unknown symbols,
                    ;; so combined confidence is likely-pure
                    (assert-equal 'likely-pure (fusion-confidence (car opps)))))
            
            ;;; ============================================================
            ;;; No False Positives
            ;;; ============================================================
            
            (define-test single-map-no-fusion
              (let* ([expr '(map f xs)]
                     [opps (detect-fusion-static expr)])
                    (assert-equal 0 (length opps))))
            
            (define-test single-filter-no-fusion
              (let* ([expr '(filter p xs)]
                     [opps (detect-fusion-static expr)])
                    (assert-equal 0 (length opps))))
            
            (define-test non-consecutive-operations-not-detected
              (let* ([expr '(map f (cons x (map g xs)))]
                     [opps (detect-fusion-static expr)])
                    ;; Should not detect map-map at root (cons in between)
                    (let ([root-opps (filter (lambda (o) (equal? (fusion-location o) '())) opps)])
                         (assert-equal 0 (length root-opps)))))
            
            ;;; ============================================================
            ;;; Nested Detection
            ;;; ============================================================
            
            (define-test detect-multiple-opportunities
              (let* ([expr '(begin
                             (map f (map g xs))
                             (filter p (map h ys)))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (>= (length opps) 2))))
            
            (define-test detect-opportunities-at-various-depths
              ;; Expression with 3 fusable patterns at different nesting levels
              (let* ([expr '(list
                             (map f (map g xs))
                             (filter p (map h ys))
                             (flatten (map expand a)))]
                     [opps (detect-fusion-static expr)])
                    (assert-true (>= (length opps) 3))))
            
            ;;; ============================================================
            ;;; Utility Functions
            ;;; ============================================================
            
            (define-test count-opportunities-works
              (let* ([expr '(map f (map g xs))]
                     [opps (detect-fusion-static expr)])
                    (assert-equal (length opps) (count-opportunities opps))))
            
            (define-test safe-opportunities-filters-correctly
              (let* ([expr '(map custom (map (lambda (x) x) xs))]
                     [opps (detect-fusion-static expr)]
                     [safe (safe-opportunities opps)])
                    (assert-true (andmap (lambda (o) (eq? (fusion-confidence o) 'safe)) safe))))
            
            (define-test opportunities-by-type-groups-correctly
              (let* ([expr '(begin
                             (map f (map g xs))
                             (filter p (map h ys)))]
                     [opps (detect-fusion-static expr)]
                     [by-type (opportunities-by-type opps)])
                    (assert-true (pair? by-type))
                    (assert-true
                     (andmap
                      (lambda (group)
                              (let ([type (car group)]
                                    [items (cdr group)])
                                   (andmap (lambda (item) (eq? (fusion-type item) type)) items)))
                      by-type))))
            
            ;;; ============================================================
            ;;; Summary Report
            ;;; ============================================================
            
            (define-test fusion-summary-generates-valid-structure
              (let* ([expr '(map f (map g xs))]
                     [opps (detect-fusion-static expr)]
                     [summary (fusion-summary opps)])
                    (assert-true (pair? summary))
                    (assert-equal 'fusion-summary (car summary))
                    (assert-true (pair? (assq 'total (cdr summary))))
                    (assert-true (pair? (assq 'safe (cdr summary))))
                    (assert-true (pair? (assq 'by-type (cdr summary))))))
            
            (define-test empty-expression-produces-empty-summary
              (let* ([expr 42]
                     [opps (detect-fusion-static expr)]
                     [summary (fusion-summary opps)])
                    (assert-equal 0 (cdr (assq 'total (cdr summary))))))
            
            ;;; ============================================================
            ;;; Edge Cases
            ;;; ============================================================
            
            (define-test handles-empty-list
              (let ([opps (detect-fusion-static '())])
                   (assert-equal 0 (length opps))))
            
            (define-test handles-atoms
              (assert-equal 0 (length (detect-fusion-static 42)))
              (assert-equal 0 (length (detect-fusion-static 'symbol)))
              (assert-equal 0 (length (detect-fusion-static "string"))))
            
            (define-test handles-deeply-nested-non-fusable
              (let* ([expr '(if (> x 0)
                             (let ([y (+ x 1)])
                                  (cons y (cdr xs)))
                             '())]
                     [opps (detect-fusion-static expr)])
                    (assert-equal 0 (length opps))))
            
            (define-test handles-malformed-patterns-gracefully
              ;; map with wrong arity
              (assert-equal 0 (length (detect-fusion-static '(map f))))
              (assert-equal 0 (length (detect-fusion-static '(map))))
              (assert-equal 0 (length (detect-fusion-static '(map f g h i))))))

(print-summary)
(when (> *tests-failed* 0) (exit 1))
