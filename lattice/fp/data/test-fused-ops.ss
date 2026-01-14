;;; core/fp/data/test-fused-ops.ss --- Tests for Fused Operations
;;;
;;; Comprehensive test suite for fused operation primitives.
;;; Verifies both correctness (fused = unfused) and basic functionality.
;;;
;;; Run with: scheme --script core/fp/data/test-fused-ops.ss

(load "core/test-framework.ss")
(load "lattice/fp/data/fused-ops.ss")

(display "\n")
(display "====\n")
(display "        Running Fused Operations Tests\n")
(display "====\n")
(newline)

(test-group fused-ops-basic
            
            ;; filter-map tests
            (define-test filter-map-basic
              (assert-equal '(4 8)
                            (filter-map even? (lambda (x) (* x 2)) '(1 2 3 4 5))))
            
            (define-test filter-map-empty
              (assert-equal '()
                            (filter-map even? (lambda (x) (* x 2)) '())))
            
            (define-test filter-map-none-pass
              (assert-equal '()
                            (filter-map (lambda (x) (> x 100)) identity '(1 2 3))))
            
            (define-test filter-map-all-pass
              (assert-equal '(2 4 6)
                            (filter-map (lambda (_) #t) (lambda (x) (* x 2)) '(1 2 3))))
            
            ;; Correctness: filter-map = map after filter
            (define-test filter-map-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
                     [pred even?]
                     [f (lambda (x) (* x x))]
                     [unfused (map f (filter pred xs))]
                     [fused (filter-map pred f xs)])
                    (assert-equal unfused fused)))
            
            ;; map-filter tests
            (define-test map-filter-basic
              (assert-equal '(16 25)
                            (map-filter (lambda (x) (* x x)) (lambda (y) (> y 10)) '(1 2 3 4 5))))
            
            (define-test map-filter-empty
              (assert-equal '()
                            (map-filter identity even? '())))
            
            ;; Correctness: map-filter = filter after map
            (define-test map-filter-correctness
              (let* ([xs '(1 2 3 4 5 6)]
                     [f (lambda (x) (+ x 10))]
                     [pred even?]
                     [unfused (filter pred (map f xs))]
                     [fused (map-filter f pred xs)])
                    (assert-equal unfused fused)))
            
            ;; fold-filter tests
            (define-test fold-filter-sum-evens
              (assert-equal 12
                            (fold-filter + 0 even? '(1 2 3 4 5 6))))
            
            (define-test fold-filter-empty
              (assert-equal 100
                            (fold-filter + 100 even? '())))
            
            ;; Correctness: fold-filter = foldl of filtered list
            (define-test fold-filter-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
                     [pred odd?]
                     [f +]
                     [z 0]
                     [unfused (foldl f z (filter pred xs))]
                     [fused (fold-filter f z pred xs)])
                    (assert-equal unfused fused)))
            
            ;; fold-map tests
            (define-test fold-map-sum-squares
              (assert-equal 14
                            (fold-map + 0 (lambda (x) (* x x)) '(1 2 3))))
            
            (define-test fold-map-empty
              (assert-equal 0
                            (fold-map + 0 identity '())))
            
            ;; Correctness: fold-map = foldl of mapped list
            (define-test fold-map-correctness
              (let* ([xs '(1 2 3 4 5)]
                     [g (lambda (x) (* x 2))]
                     [f +]
                     [z 0]
                     [unfused (foldl f z (map g xs))]
                     [fused (fold-map f z g xs)])
                    (assert-equal unfused fused))))

(test-group fused-ops-flatmap
            
            ;; flatMap tests
            (define-test flatMap-basic
              (assert-equal '(1 1 2 2 3 3)
                            (flatMap (lambda (x) (list x x)) '(1 2 3))))
            
            (define-test flatMap-empty-input
              (assert-equal '()
                            (flatMap (lambda (x) (list x x)) '())))
            
            (define-test flatMap-some-empty-outputs
              (assert-equal '(2 4)
                            (flatMap (lambda (x) (if (even? x) (list x) '())) '(1 2 3 4 5))))
            
            (define-test flatMap-all-empty-outputs
              (assert-equal '()
                            (flatMap (lambda (_) '()) '(1 2 3))))
            
            ;; Correctness: flatMap = flatten after map
            (define-test flatMap-correctness
              (let* ([xs '(1 2 3)]
                     [f (lambda (x) (list x (* x 10) (* x 100)))]
                     [unfused (flatten (map f xs))]
                     [fused (flatMap f xs)])
                    (assert-equal unfused fused)))
            
            ;; flatMap monad laws
            (define-test flatMap-left-identity
              ;; return x >>= f  =  f x
              (let* ([x 5]
                     [f (lambda (n) (list n (+ n 1)))]
                     [lhs (flatMap f (list x))]
                     [rhs (f x)])
                    (assert-equal lhs rhs)))
            
            (define-test flatMap-right-identity
              ;; m >>= return  =  m
              (let* ([m '(1 2 3)]
                     [result (flatMap list m)])
                    (assert-equal m result)))
            
            (define-test flatMap-associativity
              ;; (m >>= f) >>= g  =  m >>= (x -> f x >>= g)
              (let* ([m '(1 2)]
                     [f (lambda (x) (list x (+ x 1)))]
                     [g (lambda (y) (list y (* y 10)))]
                     [lhs (flatMap g (flatMap f m))]
                     [rhs (flatMap (lambda (x) (flatMap g (f x))) m)])
                    (assert-equal lhs rhs))))

(test-group fused-ops-foldr
            
            ;; foldr-map tests
            (define-test foldr-map-basic
              (assert-equal '(2 4 6)
                            (foldr-map cons '() (lambda (x) (* x 2)) '(1 2 3))))
            
            ;; Correctness: foldr-map = foldr of mapped list
            (define-test foldr-map-correctness
              (let* ([xs '(1 2 3 4 5)]
                     [g (lambda (x) (+ x 100))]
                     [f cons]
                     [z '()]
                     [unfused (foldr f z (map g xs))]
                     [fused (foldr-map f z g xs)])
                    (assert-equal unfused fused)))
            
            ;; foldr-filter tests
            (define-test foldr-filter-basic
              (assert-equal '(2 4 6)
                            (foldr-filter cons '() even? '(1 2 3 4 5 6))))
            
            ;; Correctness: foldr-filter = foldr of filtered list
            (define-test foldr-filter-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8)]
                     [pred (lambda (x) (> x 3))]
                     [f cons]
                     [z '()]
                     [unfused (foldr f z (filter pred xs))]
                     [fused (foldr-filter f z pred xs)])
                    (assert-equal unfused fused))))

(test-group fused-ops-take-drop
            
            ;; take-map tests
            (define-test take-map-basic
              (assert-equal '(2 4 6)
                            (take-map 3 (lambda (x) (* x 2)) '(1 2 3 4 5))))
            
            (define-test take-map-more-than-available
              (assert-equal '(2 4)
                            (take-map 5 (lambda (x) (* x 2)) '(1 2))))
            
            (define-test take-map-zero
              (assert-equal '()
                            (take-map 0 identity '(1 2 3))))
            
            ;; Correctness: take-map = map of take
            (define-test take-map-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8)]
                     [n 4]
                     [f (lambda (x) (* x x))]
                     [unfused (map f (take n xs))]
                     [fused (take-map n f xs)])
                    (assert-equal unfused fused)))
            
            ;; drop-map tests
            (define-test drop-map-basic
              (assert-equal '(6 8 10)
                            (drop-map 2 (lambda (x) (* x 2)) '(1 2 3 4 5))))
            
            (define-test drop-map-all
              (assert-equal '()
                            (drop-map 10 identity '(1 2 3))))
            
            ;; Correctness: drop-map = map of drop
            (define-test drop-map-correctness
              (let* ([xs '(1 2 3 4 5 6)]
                     [n 2]
                     [f (lambda (x) (+ x 100))]
                     [unfused (map f (drop n xs))]
                     [fused (drop-map n f xs)])
                    (assert-equal unfused fused)))
            
            ;; take-while-map tests
            (define-test take-while-map-basic
              (assert-equal '(2 4 6)
                            (take-while-map (lambda (x) (< x 4)) (lambda (x) (* x 2)) '(1 2 3 4 5))))
            
            ;; drop-while-map tests
            (define-test drop-while-map-basic
              (assert-equal '(6 8 10)
                            (drop-while-map (lambda (x) (< x 3)) (lambda (x) (* x 2)) '(1 2 3 4 5)))))

(test-group fused-ops-specialized
            
            ;; count-if tests
            (define-test count-if-basic
              (assert-equal 5
                            (count-if even? '(1 2 3 4 5 6 7 8 9 10))))
            
            (define-test count-if-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
                     [pred odd?]
                     [unfused (length (filter pred xs))]
                     [fused (count-if pred xs)])
                    (assert-equal unfused fused)))
            
            ;; sum-map tests
            (define-test sum-map-basic
              (assert-equal 14
                            (sum-map (lambda (x) (* x x)) '(1 2 3))))
            
            (define-test sum-map-correctness
              (let* ([xs '(1 2 3 4 5)]
                     [f (lambda (x) (* x 2))]
                     [unfused (apply + (map f xs))]
                     [fused (sum-map f xs)])
                    (assert-equal unfused fused)))
            
            ;; product-map tests
            (define-test product-map-basic
              (assert-equal 36
                            (product-map (lambda (x) (* x x)) '(1 2 3))))
            
            ;; maximum-by tests
            (define-test maximum-by-basic
              (assert-equal -1
                            (maximum-by (lambda (x) (* x x)) '(-1 0 1))))
            
            (define-test maximum-by-first-max
              ;; When there are ties, maximum-by returns the first occurrence
              (assert-equal '(2 . 9)
                            (maximum-by cdr '((1 . 5) (2 . 9) (3 . 9)))))
            
            ;; minimum-by tests
            (define-test minimum-by-basic
              (assert-equal "hi"
                            (minimum-by string-length '("hello" "hi" "world")))))

(test-group fused-ops-indexed
            
            ;; mapi tests
            (define-test mapi-basic
              (assert-equal '((0 . a) (1 . b) (2 . c))
                            (mapi cons '(a b c))))
            
            (define-test mapi-indices
              (assert-equal '(0 1 2 3 4)
                            (mapi (lambda (i _) i) '(a b c d e))))
            
            ;; filteri tests
            (define-test filteri-even-indices
              (assert-equal '(a c e)
                            (filteri (lambda (i _) (even? i)) '(a b c d e))))
            
            ;; filter-mapi tests
            (define-test filter-mapi-basic
              (assert-equal '((0 . 10) (2 . 30) (4 . 50))
                            (filter-mapi (lambda (i _) (even? i))
                                         (lambda (i x) (cons i (* x 10)))
                                         '(1 2 3 4 5))))
            
            ;; foldli tests
            (define-test foldli-basic
              (assert-equal '((2 . c) (1 . b) (0 . a))
                            (foldli (lambda (acc i x) (cons (cons i x) acc)) '() '(a b c)))))

(test-group fused-ops-streams
            
            ;; stream-filter-map tests
            (define-test stream-filter-map-basic
              (assert-equal '(4 8)
                            (stream->list 5 (stream-filter-map
                                             even?
                                             (lambda (x) (* x 2))
                                             (list->stream '(1 2 3 4 5))))))
            
            ;; Correctness: stream-filter-map = stream-map of stream-filter
            (define-test stream-filter-map-correctness
              (let* ([s (stream-from 1)]
                     [pred even?]
                     [f (lambda (x) (* x x))]
                     [unfused (stream->list 5 (stream-map f (stream-filter pred s)))]
                     [fused (stream->list 5 (stream-filter-map pred f s))])
                    (assert-equal unfused fused)))
            
            ;; stream-map-filter tests
            (define-test stream-map-filter-basic
              (let* ([s (list->stream '(1 2 3 4 5))]
                     [f (lambda (x) (* x x))]
                     [pred (lambda (y) (> y 10))])
                    (assert-equal '(16 25)
                                  (stream->list 5 (stream-map-filter f pred s)))))
            
            ;; Correctness: stream-map-filter = stream-filter of stream-map
            (define-test stream-map-filter-correctness
              (let* ([s (stream-from 1)]
                     [f (lambda (x) (+ x 5))]
                     [pred odd?]
                     [unfused (stream->list 5 (stream-filter pred (stream-map f s)))]
                     [fused (stream->list 5 (stream-map-filter f pred s))])
                    (assert-equal unfused fused)))
            
            ;; stream-take-map tests
            (define-test stream-take-map-basic
              (assert-equal '(2 4 6)
                            (stream->list 10 (stream-take-map
                                              3
                                              (lambda (x) (* x 2))
                                              (stream-from 1)))))
            
            ;; Correctness
            (define-test stream-take-map-correctness
              (let* ([s (stream-from 1)]
                     [n 5]
                     [f (lambda (x) (* x x))]
                     [unfused (stream->list 10 (stream-map f (stream-take n s)))]
                     [fused (stream->list 10 (stream-take-map n f s))])
                    (assert-equal unfused fused)))
            
            ;; stream-fold-map tests
            (define-test stream-fold-map-basic
              (assert-equal 14  ; 1 + 4 + 9
                            (stream-fold-map + 0 (lambda (x) (* x x)) 3 (stream-from 1))))
            
            ;; stream-fold-filter tests
            (define-test stream-fold-filter-basic
              (assert-equal 20
                            (stream-fold-filter + 0 even? 10 (stream-from 0)))))

(test-group fused-ops-multi-filter
            
            ;; filter-filter tests
            (define-test filter-filter-basic
              (assert-equal '(6 12 18)
                            (filter-filter even? (lambda (x) (= 0 (modulo x 3)))
                                           '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))))
            
            ;; Correctness: filter-filter = nested filters
            (define-test filter-filter-correctness
              (let* ([xs '(1 2 3 4 5 6 7 8 9 10 11 12)]
                     [p1 even?]
                     [p2 (lambda (x) (> x 5))]
                     [unfused (filter p2 (filter p1 xs))]
                     [fused (filter-filter p1 p2 xs)])
                    (assert-equal unfused fused)))
            
            ;; filter-all tests
            (define-test filter-all-basic
              (assert-equal '(12)
                            (filter-all (list even?
                                              (lambda (x) (> x 10))
                                              (lambda (x) (< x 20)))
                                        '(1 5 10 11 12 15 20 25))))
            
            ;; filter-any tests
            (define-test filter-any-basic
              (assert-equal '(2 4 5 6 7)
                            (filter-any (list even?
                                              (lambda (x) (> x 4)))
                                        '(1 2 3 4 5 6 7)))))

(test-group fused-ops-chunk-window
            
            ;; chunk-map tests
            (define-test chunk-map-basic
              (assert-equal '(6 15 24)
                            (chunk-map 3 (lambda (chunk) (apply + chunk)) '(1 2 3 4 5 6 7 8 9))))
            
            (define-test chunk-map-last-partial
              (assert-equal '(3 7 5)
                            (chunk-map 2 (lambda (chunk) (apply + chunk)) '(1 2 3 4 5))))
            
            ;; window-map tests
            (define-test window-map-basic
              (assert-equal '(2 3 4)
                            (window-map 3 (lambda (w) (/ (apply + w) (length w))) '(1 2 3 4 5))))
            
            (define-test window-map-pair
              (assert-equal '((1 . 2) (2 . 3) (3 . 4))
                            (window-map 2 (lambda (w) (cons (car w) (cadr w))) '(1 2 3 4)))))

(test-group fused-ops-parallel-ready
            
            ;; reduce-map tests
            (define-test reduce-map-basic
              (assert-equal 14
                            (reduce-map + 0 (lambda (x) (* x x)) '(1 2 3))))
            
            ;; partition-reduce tests
            (define-test partition-reduce-basic
              (let ([result (partition-reduce even?
                                              + +
                                              0 0
                                              '(1 2 3 4 5 6 7 8 9 10))])
                   (assert-equal (cons 30 25) result))))

;;; Print summary
(newline)
(display "====\n")
(display "        Fused Operations Test Summary\n")
(display "====\n")
(display "  Total tests: ")
(display *tests-run*)
(newline)
(display "  Passed:      ")
(display *tests-passed*)
(newline)
(display "  Failed:      ")
(display *tests-failed*)
(newline)
(newline)
(if (= *tests-failed* 0)
    (display "All fused operations tests passed!\n")
    (begin
     (display "Some tests failed!\n")
     (exit 1)))
