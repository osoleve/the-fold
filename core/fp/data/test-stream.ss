;;; fabric/stitches/fp/test-stream.ss — Tests for Lazy Streams

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/data/stream.ss")

(display "
")
(display "==============================================================
")
(display "         LAZY STREAM TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Stream Construction Tests
;;; ============================================================

(test-group stream-construction
            (define-test stream-nil-test
              (assert-true (stream-nil? stream-nil)))
            
            (define-test stream-cons-test
              (let ([s (stream-cons 1 (lambda () stream-nil))])
                   (assert-true (stream-cons? s))
                   (assert-false (stream-nil? s))))
            
            (define-test stream-head-test
              (let ([s (stream-cons 42 (lambda () stream-nil))])
                   (assert-equal 42 (stream-head s))))
            
            (define-test stream-tail-test
              (let ([s (stream-cons 1 (lambda () (stream-cons 2 (lambda () stream-nil))))])
                   (assert-equal 2 (stream-head (stream-tail s)))))
            
            (define-test list-to-stream-empty-test
              (assert-true (stream-nil? (list->stream '()))))
            
            (define-test list-to-stream-test
              (let ([s (list->stream '(1 2 3))])
                   (assert-equal 1 (stream-head s))
                   (assert-equal 2 (stream-head (stream-tail s)))
                   (assert-equal 3 (stream-head (stream-tail (stream-tail s))))))
            
            (define-test stream-to-list-test
              (let ([s (list->stream '(1 2 3 4 5))])
                   (assert-equal '(1 2 3) (stream->list 3 s))
                   (assert-equal '(1 2 3 4 5) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Generator Tests
;;; ============================================================

(test-group stream-generators
            (define-test stream-iterate-test
              (let ([s (stream-iterate (lambda (x) (+ x 1)) 0)])
                   (assert-equal '(0 1 2 3 4) (stream->list 5 s))))
            
            (define-test stream-repeat-test
              (let ([s (stream-repeat 'x)])
                   (assert-equal '(x x x x x) (stream->list 5 s))))
            
            (define-test stream-cycle-test
              (let ([s (stream-cycle '(a b c))])
                   (assert-equal '(a b c a b c a) (stream->list 7 s))))
            
            (define-test stream-cycle-empty-test
              (assert-true (stream-nil? (stream-cycle '()))))
            
            (define-test stream-from-test
              (let ([s (stream-from 10)])
                   (assert-equal '(10 11 12 13 14) (stream->list 5 s))))
            
            (define-test stream-range-test
              (assert-equal '(0 1 2 3 4) (stream->list 10 (stream-range 0 5))))
            
            (define-test stream-range-empty-test
              (assert-true (stream-nil? (stream-range 5 5)))
              (assert-true (stream-nil? (stream-range 10 5))))
            
            (define-test naturals-test
              (assert-equal '(0 1 2 3 4 5) (stream->list 6 naturals)))
            
            (define-test stream-unfold-test
              (let ([s (stream-unfold
                        (lambda (seed)
                                (if (> seed 5)
                                    nothing
                                    (just (cons (* seed 2) (+ seed 1)))))
                        1)])
                   (assert-equal '(2 4 6 8 10) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Transformer Tests
;;; ============================================================

(test-group stream-transformers
            (define-test stream-map-test
              (let ([s (stream-map (lambda (x) (* x 2)) (stream-from 1))])
                   (assert-equal '(2 4 6 8 10) (stream->list 5 s))))
            
            (define-test stream-map-empty-test
              (assert-true (stream-nil? (stream-map add1 stream-nil))))
            
            (define-test stream-filter-test
              (let ([s (stream-filter even? (stream-from 0))])
                   (assert-equal '(0 2 4 6 8) (stream->list 5 s))))
            
            (define-test stream-filter-all-test
              (let ([s (stream-filter (lambda (x) #t) (list->stream '(1 2 3)))])
                   (assert-equal '(1 2 3) (stream->list 10 s))))
            
            (define-test stream-take-test
              (let ([s (stream-take 3 (stream-from 0))])
                   (assert-equal '(0 1 2) (stream->list 10 s))))
            
            (define-test stream-take-zero-test
              (assert-true (stream-nil? (stream-take 0 (stream-from 0)))))
            
            (define-test stream-drop-test
              (let ([s (stream-drop 3 (stream-from 0))])
                   (assert-equal '(3 4 5) (stream->list 3 s))))
            
            (define-test stream-drop-all-test
              (assert-true (stream-nil? (stream-drop 100 (list->stream '(1 2 3))))))
            
            (define-test stream-take-while-test
              (let ([s (stream-take-while (lambda (x) (< x 5)) (stream-from 0))])
                   (assert-equal '(0 1 2 3 4) (stream->list 10 s))))
            
            (define-test stream-drop-while-test
              (let ([s (stream-drop-while (lambda (x) (< x 5)) (stream-from 0))])
                   (assert-equal '(5 6 7) (stream->list 3 s))))
            
            (define-test stream-nth-test
              (let ([s (stream-from 10)])
                   (assert-equal 13 (stream-nth 3 s))))
            
            (define-test stream-scan-test
              (let ([s (stream-scan + 0 (stream-from 1))])
                   (assert-equal '(0 1 3 6 10) (stream->list 5 s)))))

;;; ============================================================
;;; Stream Combinator Tests
;;; ============================================================

(test-group stream-combinators
            (define-test stream-zip-test
              (let ([s (stream-zip (stream-from 0) (stream-repeat 'x))])
                   (assert-equal '((0 . x) (1 . x) (2 . x)) (stream->list 3 s))))
            
            (define-test stream-zip-different-lengths-test
              (let ([s (stream-zip (list->stream '(a b)) (stream-from 1))])
                   (assert-equal '((a . 1) (b . 2)) (stream->list 10 s))))
            
            (define-test stream-zip-with-test
              (let ([s (stream-zip-with + (stream-from 1) (stream-from 10))])
                   (assert-equal '(11 13 15 17) (stream->list 4 s))))
            
            (define-test stream-interleave-test
              (let ([s (stream-interleave (list->stream '(a b c)) (list->stream '(1 2 3)))])
                   (assert-equal '(a 1 b 2 c 3) (stream->list 10 s))))
            
            (define-test stream-merge-test
              (let* ([odds (stream-filter odd? (stream-from 1))]
                     [evens (stream-filter even? (stream-from 2))]
                     [merged (stream-merge < odds evens)])
                    (assert-equal '(1 2 3 4 5 6 7 8) (stream->list 8 merged))))
            
            (define-test stream-concat-test
              (let ([s (stream-concat (list->stream '(1 2)) (list->stream '(3 4)))])
                   (assert-equal '(1 2 3 4) (stream->list 10 s))))
            
            (define-test stream-concat-first-empty-test
              (let ([s (stream-concat stream-nil (list->stream '(1 2)))])
                   (assert-equal '(1 2) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Fold Tests
;;; ============================================================

(test-group stream-folds
            (define-test stream-fold-test
              (let ([result (stream-fold + 0 5 (stream-from 1))])
                   (assert-equal 15 result)))  ; 1+2+3+4+5 = 15
            
            (define-test stream-any-test
              (assert-true (stream-any (lambda (x) (> x 50)) 100 (stream-from 0)))
              (assert-false (stream-any (lambda (x) (> x 50)) 10 (stream-from 0))))
            
            (define-test stream-all-test
              (assert-true (stream-all (lambda (x) (< x 100)) 50 (stream-from 0)))
              (assert-false (stream-all even? 5 (stream-from 0))))
            
            (define-test stream-find-test
              (let ([result (stream-find (lambda (x) (> x 5)) 100 (stream-from 0))])
                   (assert-true (just? result))
                   (assert-equal 6 (from-just result))))
            
            (define-test stream-find-not-found-test
              (assert-true (nothing? (stream-find (lambda (x) (> x 100)) 50 (stream-from 0))))))

;;; ============================================================
;;; Classic Stream Tests
;;; ============================================================

(test-group classic-streams
            (define-test fibonacci-test
              (assert-equal '(0 1 1 2 3 5 8 13 21 34) (stream->list 10 fibonacci)))
            
            (define-test primes-test
              (assert-equal '(2 3 5 7 11 13 17 19 23 29) (stream->list 10 primes)))
            
            (define-test powers-of-test
              (assert-equal '(1 2 4 8 16 32) (stream->list 6 (powers-of 2))))
            
            (define-test factorials-test
              (assert-equal '(1 1 2 6 24 120) (stream->list 6 factorials)))
            
            (define-test triangular-test
              (assert-equal '(0 1 3 6 10 15) (stream->list 6 triangular))))

;;; ============================================================
;;; Generator Tests
;;; ============================================================

(test-group generators
            (define-test counter-generator-test
              (let* ([gen (counter-generator 5 10)]
                     [s (make-generator gen)])
                    (assert-equal '(5 6 7 8 9) (stream->list 10 s))))
            
            (define-test random-stream-test
              (let ([s (random-stream 1 5 13)])
                   ;; Just verify it produces numbers
                   (assert-equal 5 (length (stream->list 5 s)))))
            
            (define-test make-generator-exhausted-test
              (let* ([called 0]
                     [gen (lambda ()
                                  (set! called (+ called 1))
                                  (if (> called 3) nothing (just called)))]
                     [s (make-generator gen)])
                    (assert-equal '(1 2 3) (stream->list 10 s)))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group stream-utilities
            (define-test stream-partition-test
              (let* ([parts (stream-partition even? (stream-from 0))]
                     [evens (car parts)]
                     [odds (cdr parts)])
                    (assert-equal '(0 2 4) (stream->list 3 evens))
                    (assert-equal '(1 3 5) (stream->list 3 odds))))
            
            (define-test stream-group-test
              (let ([s (stream-group 3 (stream-from 0))])
                   (assert-equal '((0 1 2) (3 4 5) (6 7 8)) (stream->list 3 s))))
            
            (define-test stream-distinct-test
              (let ([s (stream-distinct (list->stream '(1 1 2 2 2 3 3 1 1)))])
                   (assert-equal '(1 2 3 1) (stream->list 10 s))))
            
            (define-test stream-enumerate-test
              (let ([s (stream-enumerate (list->stream '(a b c)))])
                   (assert-equal '((0 . a) (1 . b) (2 . c)) (stream->list 10 s)))))

;;; ============================================================
;;; Memoization Tests
;;; ============================================================

(test-group memoization
            (define-test memo-stream-test
              (let* ([count 0]
                     [s (memo-stream-cons 1
                                          (lambda ()
                                                  (set! count (+ count 1))
                                                  (memo-stream-cons 2 (lambda () stream-nil))))])
                    ;; Access tail multiple times
                    (stream-head (stream-tail s))
                    (stream-head (stream-tail s))
                    (stream-head (stream-tail s))
                    ;; Should only have computed once
                    (assert-equal 1 count)))
            
            (define-test stream-force-test
              (let ([s (stream-take 5 (stream-from 0))])
                   (let ([forced (stream-force 10 s)])
                        (assert-equal '(0 1 2 3 4) (stream->list 10 forced))))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(test-group edge-cases
            (define-test empty-operations-test
              (assert-true (stream-nil? (stream-map add1 stream-nil)))
              (assert-true (stream-nil? (stream-filter even? stream-nil)))
              (assert-true (stream-nil? (stream-take 5 stream-nil)))
              (assert-true (stream-nil? (stream-drop 5 stream-nil))))
            
            (define-test large-stream-test
              ;; Just verify we can handle large lazy operations
              (let ([s (stream-take 1000 (stream-from 0))])
                   (assert-equal 999 (stream-nth 999 s))))
            
            (define-test nested-transforms-test
              (let* ([s (stream-from 0)]
                     [s1 (stream-filter even? s)]
                     [s2 (stream-map (lambda (x) (* x 2)) s1)]
                     [s3 (stream-take 5 s2)])
                    (assert-equal '(0 4 8 12 16) (stream->list 10 s3)))))

;;; ============================================================
;;; Practical Application Tests
;;; ============================================================

(test-group practical-applications
            (define-test running-sum-test
              (let ([sums (stream-scan + 0 (list->stream '(1 2 3 4 5)))])
                   (assert-equal '(0 1 3 6 10 15) (stream->list 10 sums))))
            
            (define-test sieve-correctness-test
              ;; Verify first 20 primes
              (let ([first-20 (stream->list 20 primes)])
                   (assert-equal 2 (list-ref first-20 0))
                   (assert-equal 71 (list-ref first-20 19))))
            
            (define-test fibonacci-property-test
              ;; F(n) = F(n-1) + F(n-2)
              (let ([fibs (stream->list 10 fibonacci)])
                   (assert-equal (list-ref fibs 9)
                                 (+ (list-ref fibs 8) (list-ref fibs 7))))))

;;; ============================================================
;;; Delay/Force Tests
;;; ============================================================

(test-group delay-force
            (define-test delay-creates-delayed
              (let ([d (delay (lambda () 42))])
                   (assert-true (delayed? d))))
            
            (define-test force-evaluates-thunk
              (let ([d (delay (lambda () (+ 20 22)))])
                   (assert-equal 42 (force d))))
            
            (define-test force-memoizes-result
              (let* ([count 0]
                     [d (delay (lambda ()
                                       (set! count (+ count 1))
                                       (* count 10)))])
                    ;; Force multiple times
                    (force d)
                    (force d)
                    (force d)
                    ;; Should only have evaluated once
                    (assert-equal 1 count)
                    (assert-equal 10 (force d))))
            
            (define-test delayed-predicate
              (assert-true (delayed? (delay (lambda () 1))))
              (assert-false (delayed? 42))
              (assert-false (delayed? '(not delayed)))))

;;; ============================================================
;;; Functor Instance Tests
;;; ============================================================

(test-group stream-functor-tests
            (define-test functor-identity-law
              ;; fmap id = id
              (let* ([s (list->stream '(1 2 3 4 5))]
                     [mapped (stream-fmap identity s)])
                    (assert-equal '(1 2 3 4 5) (stream->list 10 mapped))))
            
            (define-test functor-composition-law
              ;; fmap (f . g) = fmap f . fmap g
              (let* ([s (list->stream '(1 2 3))]
                     [f (lambda (x) (* x 2))]
                     [g (lambda (x) (+ x 1))]
                     [composed (stream-fmap (lambda (x) (f (g x))) s)]
                     [sequential (stream-fmap f (stream-fmap g s))])
                    (assert-equal (stream->list 10 composed)
                                  (stream->list 10 sequential))))
            
            (define-test functor-dictionary
              ;; Verify the functor dictionary structure
              (assert-equal 'functor (car stream-functor))
              (let ([fmap-fn (functor-fmap stream-functor)])
                   (assert-equal '(2 4 6)
                                 (stream->list 3 (fmap-fn (lambda (x) (* x 2)) (stream-from 1)))))))

;;; ============================================================
;;; Applicative Instance Tests
;;; ============================================================

(test-group stream-applicative-tests
            (define-test applicative-pure-creates-infinite
              ;; pure x = repeat x
              (let ([s (stream-pure 42)])
                   (assert-equal '(42 42 42 42 42) (stream->list 5 s))))
            
            (define-test applicative-ap-zips
              ;; fs <*> xs applies pointwise
              (let* ([fs (list->stream (list add1 (lambda (x) (* x 2)) (lambda (x) (- x 3))))]
                     [xs (list->stream '(10 20 30))]
                     [result (stream-ap fs xs)])
                    (assert-equal '(11 40 27) (stream->list 10 result))))
            
            (define-test applicative-identity-law
              ;; pure id <*> v = v
              (let* ([v (list->stream '(1 2 3 4))]
                     [result (stream-ap (stream-pure identity) v)])
                    (assert-equal '(1 2 3 4) (stream->list 10 result))))
            
            (define-test applicative-homomorphism-law
              ;; pure f <*> pure x = pure (f x)
              (let* ([f (lambda (x) (* x 2))]
                     [x 21]
                     [left (stream-ap (stream-pure f) (stream-pure x))]
                     [right (stream-pure (f x))])
                    ;; Both should produce infinite streams of 42
                    (assert-equal (stream->list 5 left) (stream->list 5 right))))
            
            (define-test stream-lift2-test
              (let* ([xs (list->stream '(1 2 3))]
                     [ys (list->stream '(10 20 30))]
                     [sums (stream-lift2 + xs ys)])
                    (assert-equal '(11 22 33) (stream->list 5 sums))))
            
            (define-test stream-lift3-test
              (let* ([xs (list->stream '(1 2 3))]
                     [ys (list->stream '(10 20 30))]
                     [zs (list->stream '(100 200 300))]
                     [sums (stream-lift3 (lambda (x y z) (+ x y z)) xs ys zs)])
                    (assert-equal '(111 222 333) (stream->list 5 sums)))))

;;; ============================================================
;;; Monad Instance Tests
;;; ============================================================

(test-group stream-monad-tests
            (define-test monad-return-creates-stream
              ;; return x = repeat x
              (let ([s (stream-return 42)])
                   (assert-equal '(42 42 42) (stream->list 3 s))))
            
            (define-test stream-diagonal-test
              ;; Diagonal extracts i-th element from i-th stream
              (let* ([streams (list->stream
                               (list (list->stream '(a b c))
                                     (list->stream '(d e f))
                                     (list->stream '(g h i))))]
                     [diag (stream-diagonal streams)])
                    (assert-equal '(a e i) (stream->list 10 diag))))
            
            (define-test stream-diagonal-infinite-test
              ;; Diagonal works with infinite streams
              (let* ([make-row (lambda (n) (stream-iterate (lambda (x) (+ x 10)) n))]
                     [matrix (stream-map make-row (stream-from 0))]
                     [diag (stream-diagonal matrix)])
                    ;; Row 0: 0, 10, 20, 30, ...
                    ;; Row 1: 1, 11, 21, 31, ...
                    ;; Row 2: 2, 12, 22, 32, ...
                    ;; Diagonal: 0, 11, 22, 33, 44, ...
                    (assert-equal '(0 11 22 33 44) (stream->list 5 diag))))
            
            (define-test stream-join-test
              ;; join = diagonal for streams
              (let* ([nested (list->stream
                              (list (list->stream '(1 2 3))
                                    (list->stream '(4 5 6))
                                    (list->stream '(7 8 9))))]
                     [joined (stream-join nested)])
                    (assert-equal '(1 5 9) (stream->list 5 joined))))
            
            (define-test stream-then-test
              ;; Sequence discards first result
              (let* ([s1 (list->stream '(1 2 3))]
                     [s2 (list->stream '(a b c))]
                     [result (stream-then s1 s2)])
                    ;; With diagonal semantics, s1 produces rows
                    ;; and we get diagonal of those rows
                    (assert-true (not (stream-nil? result))))))

;;; ============================================================
;;; Codata Pattern Tests
;;; ============================================================

(test-group codata-patterns
            (define-test coalgebra-step-test
              ;; Build stream from coalgebra
              (let* ([step (lambda (n) (cons n (+ n 1)))]  ; produce n, next state is n+1
                     [s (coalgebra-step step 0)])
                    (assert-equal '(0 1 2 3 4) (stream->list 5 s))))
            
            (define-test anamorphism-test
              ;; Same as unfold
              (let* ([step (lambda (n)
                                   (if (> n 5) nothing
                                       (just (cons (* n 2) (+ n 1)))))]
                     [s (anamorphism step 1)])
                    (assert-equal '(2 4 6 8 10) (stream->list 10 s))))
            
            (define-test coiterate-test
              ;; Coiterate produces stream of states
              (let ([s (coiterate (lambda (x) (* x 2)) 1)])
                   (assert-equal '(1 2 4 8 16) (stream->list 5 s))))
            
            (define-test bisimulation-equal-test
              ;; Bisimulation equality check
              (let ([s1 (stream-from 0)]
                    [s2 (stream-from 0)]
                    [s3 (stream-from 1)])
                   (assert-true (bisimulation-equal? 100 s1 s2))
                   (assert-false (bisimulation-equal? 1 s1 s3))))
            
            (define-test bisimulation-empty-test
              (assert-true (bisimulation-equal? 10 stream-nil stream-nil))))

;;; ============================================================
;;; Additional Infinite Stream Tests
;;; ============================================================

(test-group additional-infinite-streams
            (define-test squares-test
              (assert-equal '(0 1 4 9 16 25) (stream->list 6 squares)))
            
            (define-test cubes-test
              (assert-equal '(0 1 8 27 64 125) (stream->list 6 cubes)))
            
            (define-test evens-test
              (assert-equal '(0 2 4 6 8 10) (stream->list 6 evens)))
            
            (define-test odds-test
              (assert-equal '(1 3 5 7 9 11) (stream->list 6 odds))))

;;; ============================================================
;;; Fuel-Based Operation Tests
;;; ============================================================

(test-group fuel-operations
            (define-test stream-length-fuel-finite
              (let ([s (list->stream '(1 2 3 4 5))])
                   (assert-equal 5 (stream-length/fuel 100 s))))
            
            (define-test stream-length-fuel-infinite
              (let ([s (stream-from 0)])
                   ;; Returns fuel limit when stream is longer
                   (assert-equal 10 (stream-length/fuel 10 s))))
            
            (define-test stream-reverse-fuel-test
              (let* ([s (stream-from 0)]
                     [rev (stream-reverse/fuel 5 s)])
                    (assert-equal '(4 3 2 1 0) (stream->list 10 rev))))
            
            (define-test stream-last-fuel-finite
              (let* ([s (list->stream '(1 2 3 4 5))]
                     [last (stream-last/fuel 100 s)])
                    (assert-true (just? last))
                    (assert-equal 5 (from-just last))))
            
            (define-test stream-last-fuel-limited
              (let* ([s (stream-from 10)]
                     [last (stream-last/fuel 5 s)])
                    (assert-true (just? last))
                    (assert-equal 14 (from-just last))))
            
            (define-test stream-last-fuel-empty
              (assert-true (nothing? (stream-last/fuel 100 stream-nil)))))

;;; ============================================================
;;; Stream Comprehension Tests
;;; ============================================================

(test-group stream-comprehensions
            (define-test stream-guard-true
              (let ([s (stream-guard #t)])
                   (assert-equal 1 (stream-length/fuel 10 s))
                   (assert-equal '(()) (stream->list 1 s))))
            
            (define-test stream-guard-false
              (assert-true (stream-nil? (stream-guard #f))))
            
            (define-test stream-cartesian-finite
              (let* ([xs (list->stream '(1 2))]
                     [ys (list->stream '(a b))]
                     [prod (stream-cartesian xs ys)]
                     [all-pairs (stream->list 10 prod)])
                    ;; Cantor diagonal enumeration produces all 4 pairs
                    (assert-equal 4 (length all-pairs))
                    ;; First element is (1 . a) from diagonal 0
                    (assert-equal (cons 1 'a) (stream-head prod))
                    ;; All pairs should be present
                    (assert-true (if (member (cons 1 'a) all-pairs) #t #f))
                    (assert-true (if (member (cons 2 'a) all-pairs) #t #f))
                    (assert-true (if (member (cons 1 'b) all-pairs) #t #f))
                    (assert-true (if (member (cons 2 'b) all-pairs) #t #f))))
            
            (define-test stream-cartesian-infinite
              ;; Test that infinite streams produce off-diagonal elements
              (let* ([nats (stream-iterate (lambda (x) (+ x 1)) 0)]
                     [prod (stream-cartesian nats nats)]
                     [first-20 (stream->list 20 prod)])
                    ;; Off-diagonal elements must appear (this was the bug)
                    (assert-true (if (member (cons 0 1) first-20) #t #f))
                    (assert-true (if (member (cons 1 0) first-20) #t #f))
                    (assert-true (if (member (cons 0 2) first-20) #t #f))
                    (assert-true (if (member (cons 2 0) first-20) #t #f)))))

;;; ============================================================
;;; Type Class Dictionary Tests
;;; ============================================================

(test-group type-class-dictionaries
            (define-test applicative-dictionary-structure
              (assert-equal 'applicative (car stream-applicative))
              ;; Contains parent functor
              (assert-equal 'functor (car (cadr stream-applicative))))
            
            (define-test monad-dictionary-structure
              (assert-equal 'monad (car stream-monad))
              ;; Contains parent applicative
              (assert-equal 'applicative (car (cadr stream-monad))))
            
            (define-test applicative-accessors
              (let ([pure (applicative-pure stream-applicative)]
                    [ap (applicative-ap stream-applicative)])
                   (assert-equal '(5 5 5) (stream->list 3 (pure 5)))
                   (assert-equal '(2 4 6)
                                 (stream->list 3
                                               (ap (stream-pure (lambda (x) (* x 2)))
                                                   (list->stream '(1 2 3)))))))
            
            (define-test monad-accessors
              (let ([ret (monad-return stream-monad)])
                   (assert-equal '(7 7 7) (stream->list 3 (ret 7))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All stream tests passed.
")
    (display "
[FAILURE] Some stream tests failed.
"))
