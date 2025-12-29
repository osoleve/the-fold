;;; fabric/stitches/fp/test-representable.ss - Tests for Representable/Distributive
;;;
;;; Tests for Representable and Distributive functor typeclasses.

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/representable.ss")

(display "
")
(display "==============================================================
")
(display "         REPRESENTABLE AND DISTRIBUTIVE TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Representable Construction Tests
;;; ============================================================

(test-group representable-construction-tests
            
            (define-test make-representable-test
              (assert-true (representable? identity-representable))
              (assert-true (representable? pair-representable))
              (assert-true (representable? stream-representable)))
            
            (define-test representable-functor-test
              (assert-true (functor? (representable-functor identity-representable)))
              (assert-true (functor? (representable-functor pair-representable))))
            
            (define-test representable-rep-name-test
              (assert-equal 'unit (representable-rep-name identity-representable))
              (assert-equal 'bool (representable-rep-name pair-representable))
              (assert-equal 'nat (representable-rep-name stream-representable))))

;;; ============================================================
;;; Identity Representable Tests
;;; ============================================================

(test-group identity-representable-tests
            
            (define-test identity-tabulate-test
              (let ([fa (rep-tabulate identity-representable (const 42))])
                   (assert-equal 42 fa)))
            
            (define-test identity-index-test
              (let ([fa 42])
                   (assert-equal 42 (rep-index identity-representable fa '()))))
            
            (define-test identity-roundtrip-test
              ;; tabulate . index = id
              (let* ([fa 42]
                     [result (rep-tabulate identity-representable
                                           (lambda (k) (rep-index identity-representable fa k)))])
                    (assert-equal fa result)))
            
            (define-test identity-pure-test
              (let ([fa (rep-pure identity-representable 99)])
                   (assert-equal 99 fa)))
            
            (define-test identity-ap-test
              (let* ([ff add1]
                     [fa 5]
                     [result (rep-ap identity-representable ff fa)])
                    (assert-equal 6 result))))

;;; ============================================================
;;; Pair Representable Tests
;;; ============================================================

(test-group pair-representable-tests
            
            (define-test pair-tabulate-test
              (let ([p (rep-tabulate pair-representable
                                     (lambda (b) (if b 'first 'second)))])
                   (assert-equal 'first (pair-rep-fst p))
                   (assert-equal 'second (pair-rep-snd p))))
            
            (define-test pair-index-test
              (let ([p (make-pair-rep 10 20)])
                   (assert-equal 10 (rep-index pair-representable p #t))
                   (assert-equal 20 (rep-index pair-representable p #f))))
            
            (define-test pair-roundtrip-tabulate-index-test
              ;; index (tabulate f) k = f k
              (let* ([f (lambda (b) (if b 'yes 'no))]
                     [p (rep-tabulate pair-representable f)])
                    (assert-equal 'yes (rep-index pair-representable p #t))
                    (assert-equal 'no (rep-index pair-representable p #f))))
            
            (define-test pair-roundtrip-index-tabulate-test
              ;; tabulate (index p) = p
              (let* ([p (make-pair-rep 'a 'b)]
                     [result (rep-tabulate pair-representable
                                           (lambda (k) (rep-index pair-representable p k)))])
                    (assert-equal (pair-rep-fst p) (pair-rep-fst result))
                    (assert-equal (pair-rep-snd p) (pair-rep-snd result))))
            
            (define-test pair-pure-test
              (let ([p (rep-pure pair-representable 42)])
                   (assert-equal 42 (pair-rep-fst p))
                   (assert-equal 42 (pair-rep-snd p))))
            
            (define-test pair-ap-test
              (let* ([pf (make-pair-rep add1 (lambda (x) (* x 2)))]
                     [pa (make-pair-rep 10 5)]
                     [result (rep-ap pair-representable pf pa)])
                    (assert-equal 11 (pair-rep-fst result))
                    (assert-equal 10 (pair-rep-snd result))))
            
            (define-test pair-indexed-map-test
              (let* ([p (make-pair-rep 'a 'b)]
                     [result (rep-indexed-map pair-representable
                                              (lambda (k v) (list k v))
                                              p)])
                    (assert-equal '(#t a) (pair-rep-fst result))
                    (assert-equal '(#f b) (pair-rep-snd result))))
            
            (define-test pair-zip-with-test
              (let* ([p1 (make-pair-rep 1 2)]
                     [p2 (make-pair-rep 10 20)]
                     [result (rep-zip-with pair-representable + p1 p2)])
                    (assert-equal 11 (pair-rep-fst result))
                    (assert-equal 22 (pair-rep-snd result)))))

;;; ============================================================
;;; Stream Representable Tests
;;; ============================================================

(test-group stream-representable-tests
            
            (define-test stream-tabulate-test
              (let ([s (rep-tabulate stream-representable (lambda (n) (* n n)))])
                   (assert-equal 0 (stream-nth 0 s))
                   (assert-equal 1 (stream-nth 1 s))
                   (assert-equal 4 (stream-nth 2 s))
                   (assert-equal 9 (stream-nth 3 s))))
            
            (define-test stream-index-test
              (let ([s (rep-tabulate stream-representable (lambda (n) (+ n 100)))])
                   (assert-equal 100 (rep-index stream-representable s 0))
                   (assert-equal 105 (rep-index stream-representable s 5))))
            
            (define-test stream-pure-test
              (let ([s (rep-pure stream-representable 42)])
                   (assert-equal 42 (stream-nth 0 s))
                   (assert-equal 42 (stream-nth 10 s))
                   (assert-equal 42 (stream-nth 100 s)))))

;;; ============================================================
;;; Distributive Construction Tests
;;; ============================================================

(test-group distributive-construction-tests
            
            (define-test make-distributive-test
              (assert-true (distributive? identity-distributive))
              (assert-true (distributive? pair-distributive)))
            
            (define-test distributive-functor-test
              (assert-true (functor? (distributive-functor identity-distributive)))
              (assert-true (functor? (distributive-functor pair-distributive)))))

;;; ============================================================
;;; Identity Distributive Tests
;;; ============================================================

(test-group identity-distributive-tests
            
            (define-test identity-distribute-list-test
              ;; distribute [1, 2, 3] over Identity should give Identity [1, 2, 3]
              (let ([result (dist-distribute identity-distributive list-functor '(1 2 3))])
                   (assert-equal '(1 2 3) result)))
            
            (define-test identity-distribute-maybe-test
              (let ([result (dist-distribute identity-distributive maybe-functor (just 42))])
                   (assert-equal (just 42) result))))

;;; ============================================================
;;; Pair Distributive Tests
;;; ============================================================

(test-group pair-distributive-tests
            
            (define-test pair-distribute-list-test
              ;; distribute [(1,2), (3,4), (5,6)] = ([1,3,5], [2,4,6])
              (let* ([input (list (make-pair-rep 1 2)
                                  (make-pair-rep 3 4)
                                  (make-pair-rep 5 6))]
                     [result (dist-distribute pair-distributive list-functor input)])
                    (assert-equal '(1 3 5) (pair-rep-fst result))
                    (assert-equal '(2 4 6) (pair-rep-snd result))))
            
            (define-test pair-distribute-maybe-just-test
              ;; distribute (Just (1, 2)) = (Just 1, Just 2)
              (let* ([input (just (make-pair-rep 1 2))]
                     [result (dist-distribute pair-distributive maybe-functor input)])
                    (assert-equal (just 1) (pair-rep-fst result))
                    (assert-equal (just 2) (pair-rep-snd result))))
            
            (define-test pair-distribute-maybe-nothing-test
              ;; distribute Nothing = (Nothing, Nothing)
              (let ([result (dist-distribute pair-distributive maybe-functor nothing)])
                   (assert-true (nothing? (pair-rep-fst result)))
                   (assert-true (nothing? (pair-rep-snd result)))))
            
            (define-test pair-collect-test
              ;; collect (\x -> (x, x+1)) [1,2,3] = ([1,2,3], [2,3,4])
              (let* ([f (lambda (x) (make-pair-rep x (+ x 1)))]
                     [input '(1 2 3)]
                     [result (dist-collect pair-distributive list-functor f input)])
                    (assert-equal '(1 2 3) (pair-rep-fst result))
                    (assert-equal '(2 3 4) (pair-rep-snd result)))))

;;; ============================================================
;;; Representable Laws Tests
;;; ============================================================

(test-group representable-laws-tests
            
            ;; Law: tabulate . index = id
            (define-test pair-law-tabulate-index-test
              (let* ([p (make-pair-rep 'a 'b)]
                     [result (rep-tabulate pair-representable
                                           (lambda (k) (rep-index pair-representable p k)))])
                    (assert-equal (pair-rep-fst p) (pair-rep-fst result))
                    (assert-equal (pair-rep-snd p) (pair-rep-snd result))))
            
            ;; Law: index (tabulate f) k = f k
            (define-test pair-law-index-tabulate-test
              (let* ([f (lambda (b) (if b 100 200))]
                     [p (rep-tabulate pair-representable f)])
                    (assert-equal (f #t) (rep-index pair-representable p #t))
                    (assert-equal (f #f) (rep-index pair-representable p #f)))))

;;; ============================================================
;;; Derived Operations Tests
;;; ============================================================

(test-group derived-operations-tests
            
            (define-test pair-tabulated-test
              ;; tabulated gives structure with keys at each position
              (let ([p (rep-tabulated pair-representable)])
                   (assert-equal #t (pair-rep-fst p))
                   (assert-equal #f (pair-rep-snd p))))
            
            (define-test pair-bind-test
              ;; Monadic bind through Representable
              (let* ([p (make-pair-rep 10 20)]
                     [f (lambda (x) (make-pair-rep (+ x 1) (+ x 2)))]
                     [result (rep-bind pair-representable p f)])
                    ;; At key #t: f(p[#t])[#t] = f(10)[#t] = 11
                    ;; At key #f: f(p[#f])[#f] = f(20)[#f] = 22
                    (assert-equal 11 (pair-rep-fst result))
                    (assert-equal 22 (pair-rep-snd result))))
            
            (define-test pair-local-test
              ;; local flips the keys
              (let* ([p (make-pair-rep 'first 'second)]
                     [result (rep-local pair-representable not p)])
                    ;; Accessing with #t gives p[not #t] = p[#f] = 'second
                    (assert-equal 'second (pair-rep-fst result))
                    (assert-equal 'first (pair-rep-snd result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
