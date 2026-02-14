;;; core/info-theory/test-statistical-measures.ss — Tests for Statistical Measures
;;;
;;; Comprehensive test suite for statistical divergence measures.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'statistical-measures)

;;; ====
;;; Helper Functions
;;; ====

;;; approx= : Number -> Number -> Number -> Boolean
;;; Check if two numbers are approximately equal within epsilon.
(define (approx= eps)
  (lambda (expected actual)
          (< (abs (- expected actual)) eps)))

;;; assert-approx : Number -> Number -> Number -> Unit
(define (assert-approx eps expected actual)
  (assert-true ((approx= eps) expected actual)))

;;; ====
;;; Test Data
;;; ====

;;; Identical distributions
(define uniform-3 '(0.333333 0.333333 0.333334))
(define uniform-3-copy '(0.333333 0.333333 0.333334))

;;; Similar distributions
(define dist-a '(0.5 0.3 0.2))
(define dist-b '(0.4 0.35 0.25))

;;; Very different distributions
(define dist-peaked '(0.9 0.05 0.05))
(define dist-uniform '(0.333333 0.333333 0.333334))

;;; Disjoint support
(define dist-left '(0.5 0.5 0.0 0.0))
(define dist-right '(0.0 0.0 0.5 0.5))

;;; Binary distributions
(define coin-fair '(0.5 0.5))
(define coin-biased '(0.8 0.2))
(define coin-certain '(1.0 0.0))

;;; ====
;;; Bhattacharyya Tests
;;; ====

(test-group bhattacharyya
            
            (define-test bhattacharyya-identical
              (let ([bc (bhattacharyya-coefficient uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 1.0 bc)))
            
            (define-test bhattacharyya-disjoint
              (let ([bc (bhattacharyya-coefficient dist-left dist-right)])
                   (assert-approx 0.001 0.0 bc)))
            
            (define-test bhattacharyya-distance-identical
              (let ([bd (bhattacharyya-distance uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 bd)))
            
            (define-test bhattacharyya-distance-disjoint
              (let ([bd (bhattacharyya-distance dist-left dist-right)])
                   (assert-true (> bd 100))))  ; Should be very large (approaching +inf)
            
            (define-test bhattacharyya-symmetric
              (let ([bc1 (bhattacharyya-coefficient dist-a dist-b)]
                    [bc2 (bhattacharyya-coefficient dist-b dist-a)])
                   (assert-approx 0.0001 bc1 bc2)))
            
            (define-test bhattacharyya-range
              (let ([bc (bhattacharyya-coefficient dist-a dist-b)])
                   (assert-true (and (>= bc 0) (<= bc 1)))))
            
            ) ; end bhattacharyya group

;;; ====
;;; Hellinger Distance Tests
;;; ====

(test-group hellinger
            
            (define-test hellinger-identical
              (let ([h (hellinger-distance uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 h)))
            
            (define-test hellinger-disjoint
              (let ([h (hellinger-distance dist-left dist-right)])
                   (assert-approx 0.001 1.0 h)))
            
            (define-test hellinger-symmetric
              (let ([h1 (hellinger-distance dist-a dist-b)]
                    [h2 (hellinger-distance dist-b dist-a)])
                   (assert-approx 0.0001 h1 h2)))
            
            (define-test hellinger-range
              (let ([h (hellinger-distance dist-a dist-b)])
                   (assert-true (and (>= h 0) (<= h 1)))))
            
            (define-test hellinger-from-bhattacharyya
              (let* ([bc (bhattacharyya-coefficient dist-a dist-b)]
                     [h-direct (hellinger-distance dist-a dist-b)]
                     [h-from-bc (hellinger-distance-from-bc bc)])
                    (assert-approx 0.001 h-direct h-from-bc)))
            
            (define-test hellinger-triangle-inequality
              ;; H(P,R) <= H(P,Q) + H(Q,R) for any P, Q, R
              (let ([p dist-a]
                    [q dist-b]
                    [r dist-peaked])
                   (let ([h-pr (hellinger-distance p r)]
                         [h-pq (hellinger-distance p q)]
                         [h-qr (hellinger-distance q r)])
                        (assert-true (<= h-pr (+ h-pq h-qr 0.001))))))
            
            ) ; end hellinger group

;;; ====
;;; Total Variation Distance Tests
;;; ====

(test-group total-variation
            
            (define-test tv-identical
              (let ([tv (total-variation-distance uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 tv)))
            
            (define-test tv-disjoint
              (let ([tv (total-variation-distance dist-left dist-right)])
                   (assert-approx 0.001 1.0 tv)))
            
            (define-test tv-symmetric
              (let ([tv1 (total-variation-distance dist-a dist-b)]
                    [tv2 (total-variation-distance dist-b dist-a)])
                   (assert-approx 0.0001 tv1 tv2)))
            
            (define-test tv-range
              (let ([tv (total-variation-distance dist-a dist-b)])
                   (assert-true (and (>= tv 0) (<= tv 1)))))
            
            (define-test tv-binary-known
              ;; TV((0.5,0.5), (0.8,0.2)) = 0.5 * (|0.5-0.8| + |0.5-0.2|) = 0.5 * 0.6 = 0.3
              (let ([tv (total-variation-distance coin-fair coin-biased)])
                   (assert-approx 0.001 0.3 tv)))
            
            (define-test tv-triangle-inequality
              ;; TV(P,R) <= TV(P,Q) + TV(Q,R)
              (let ([p dist-a]
                    [q dist-b]
                    [r dist-peaked])
                   (let ([tv-pr (total-variation-distance p r)]
                         [tv-pq (total-variation-distance p q)]
                         [tv-qr (total-variation-distance q r)])
                        (assert-true (<= tv-pr (+ tv-pq tv-qr 0.001))))))
            
            ) ; end total-variation group

;;; ====
;;; Chi-Squared Divergence Tests
;;; ====

(test-group chi-squared
            
            (define-test chi-squared-identical
              (let ([chi2 (chi-squared-divergence uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 chi2)))
            
            (define-test chi-squared-asymmetric
              (let ([chi2-pq (chi-squared-divergence dist-a dist-b)]
                    [chi2-qp (chi-squared-divergence dist-b dist-a)])
                   ;; Should be different (asymmetric)
                   (assert-true (> (abs (- chi2-pq chi2-qp)) 0.001))))
            
            (define-test chi-squared-disjoint-support
              (let ([chi2 (chi-squared-divergence dist-left dist-right)])
                   ;; Should be +inf when Q has zeros where P is non-zero
                   (assert-equal +inf.0 chi2)))
            
            (define-test chi-squared-non-negative
              (let ([chi2 (chi-squared-divergence dist-a dist-b)])
                   (assert-true (>= chi2 0))))
            
            (define-test symmetric-chi-squared-test
              (let ([sym-chi2 (symmetric-chi-squared dist-a dist-b)])
                   (assert-true (>= sym-chi2 0))))
            
            (define-test chi-squared-symmetric-property
              (let ([sym1 (symmetric-chi-squared dist-a dist-b)]
                    [sym2 (symmetric-chi-squared dist-b dist-a)])
                   (assert-approx 0.0001 sym1 sym2)))
            
            ) ; end chi-squared group

;;; ====
;;; Jeffreys Divergence Tests
;;; ====

(test-group jeffreys
            
            (define-test jeffreys-identical
              (let ([j (jeffreys-divergence uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 j)))
            
            (define-test jeffreys-symmetric
              (let ([j1 (jeffreys-divergence dist-a dist-b)]
                    [j2 (jeffreys-divergence dist-b dist-a)])
                   (assert-approx 0.0001 j1 j2)))
            
            (define-test jeffreys-non-negative
              (let ([j (jeffreys-divergence dist-a dist-b)])
                   (assert-true (>= j 0))))
            
            (define-test jeffreys-vs-kl
              ;; Jeffreys should equal sum of forward and reverse KL
              (let ([j (jeffreys-divergence dist-a dist-b)]
                    [kl-pq (kl-divergence dist-a dist-b)]
                    [kl-qp (kl-divergence dist-b dist-a)])
                   (assert-approx 0.0001 j (+ kl-pq kl-qp))))
            
            ) ; end jeffreys group

;;; ====
;;; Alpha-Divergence Tests
;;; ====

(test-group alpha-divergence
            
            (define-test alpha-div-identity
              (let ([d (alpha-divergence 0.5 uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 d)))
            
            (define-test alpha-div-limit-to-kl
              ;; As α → 1, alpha-divergence → KL divergence
              (let ([kl (kl-divergence dist-a dist-b)]
                    [alpha-close (alpha-divergence 0.9999 dist-a dist-b)])
                   ;; Should be very close
                   (assert-approx 0.1 kl alpha-close)))
            
            (define-test alpha-div-at-one
              ;; α = 1 should exactly equal KL
              (let ([kl (kl-divergence dist-a dist-b)]
                    [alpha1 (alpha-divergence 1 dist-a dist-b)])
                   (assert-approx 0.0001 kl alpha1)))
            
            (define-test alpha-div-non-negative
              (let ([d (alpha-divergence 2 dist-a dist-b)])
                   (assert-true (>= d 0))))
            
            ) ; end alpha-divergence group

;;; ====
;;; Triangular Discrimination Tests
;;; ====

(test-group triangular
            
            (define-test triangular-identical
              (let ([tri (triangular-discrimination uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 tri)))
            
            (define-test triangular-symmetric
              (let ([tri1 (triangular-discrimination dist-a dist-b)]
                    [tri2 (triangular-discrimination dist-b dist-a)])
                   (assert-approx 0.0001 tri1 tri2)))
            
            (define-test triangular-non-negative
              (let ([tri (triangular-discrimination dist-a dist-b)])
                   (assert-true (>= tri 0))))
            
            (define-test triangular-bounded
              (let ([tri (triangular-discrimination dist-a dist-b)])
                   (assert-true (<= tri 1))))
            
            ) ; end triangular group

;;; ====
;;; K-Divergence Tests
;;; ====

(test-group k-divergence
            
            (define-test k-div-identical
              (let ([k (k-divergence uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 k)))
            
            (define-test k-div-symmetric
              (let ([k1 (k-divergence dist-a dist-b)]
                    [k2 (k-divergence dist-b dist-a)])
                   (assert-approx 0.0001 k1 k2)))
            
            (define-test k-div-non-negative
              (let ([k (k-divergence dist-a dist-b)])
                   (assert-true (>= k 0))))
            
            ) ; end k-divergence group

;;; ====
;;; Squared Loss and Euclidean Distance Tests
;;; ====

(test-group squared-loss
            
            (define-test squared-loss-identical
              (let ([sl (squared-loss uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 sl)))
            
            (define-test squared-loss-symmetric
              (let ([sl1 (squared-loss dist-a dist-b)]
                    [sl2 (squared-loss dist-b dist-a)])
                   (assert-approx 0.0001 sl1 sl2)))
            
            (define-test squared-loss-non-negative
              (let ([sl (squared-loss dist-a dist-b)])
                   (assert-true (>= sl 0))))
            
            (define-test euclidean-vs-squared
              (let ([sl (squared-loss dist-a dist-b)]
                    [euc (euclidean-distance dist-a dist-b)])
                   (assert-approx 0.0001 (sqrt sl) euc)))
            
            (define-test euclidean-symmetric
              (let ([euc1 (euclidean-distance dist-a dist-b)]
                    [euc2 (euclidean-distance dist-b dist-a)])
                   (assert-approx 0.0001 euc1 euc2)))
            
            ) ; end squared-loss group

;;; ====
;;; Matusita Distance Tests
;;; ====

(test-group matusita
            
            (define-test matusita-identical
              (let ([m (matusita-distance uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 0.0 m)))
            
            (define-test matusita-symmetric
              (let ([m1 (matusita-distance dist-a dist-b)]
                    [m2 (matusita-distance dist-b dist-a)])
                   (assert-approx 0.0001 m1 m2)))
            
            (define-test matusita-vs-hellinger
              ;; Matusita = sqrt(2) * Hellinger
              (let ([m (matusita-distance dist-a dist-b)]
                    [h (hellinger-distance dist-a dist-b)])
                   (assert-approx 0.001 m (* (sqrt 2) h))))
            
            ) ; end matusita group

;;; ====
;;; Fidelity Tests
;;; ====

(test-group fidelity
            
            (define-test fidelity-identical
              (let ([f (fidelity uniform-3 uniform-3-copy)])
                   (assert-approx 0.001 1.0 f)))
            
            (define-test fidelity-disjoint
              (let ([f (fidelity dist-left dist-right)])
                   (assert-approx 0.001 0.0 f)))
            
            (define-test fidelity-symmetric
              (let ([f1 (fidelity dist-a dist-b)]
                    [f2 (fidelity dist-b dist-a)])
                   (assert-approx 0.0001 f1 f2)))
            
            (define-test fidelity-range
              (let ([f (fidelity dist-a dist-b)])
                   (assert-true (and (>= f 0) (<= f 1)))))
            
            ) ; end fidelity group

;;; ====
;;; Utility Function Tests
;;; ====

(test-group utilities
            
            (define-test valid-distribution-check-true
              (assert-true (valid-distribution? '(0.3 0.3 0.4))))
            
            (define-test valid-distribution-check-false-negative
              (assert-false (valid-distribution? '(0.3 -0.1 0.8))))
            
            (define-test valid-distribution-check-false-sum
              (assert-false (valid-distribution? '(0.3 0.3 0.3))))
            
            (define-test normalize-distribution-test
              (let ([normalized (normalize-distribution '(1 2 3))])
                   (assert-approx 0.001 1.0 (fold-left + 0 normalized))))
            
            (define-test normalize-zero-weights
              (let ([normalized (normalize-distribution '(0 0 0))])
                   (assert-approx 0.001 1.0 (fold-left + 0 normalized))))
            
            ) ; end utilities group

;;; ====
;;; Cross-Measure Consistency Tests
;;; ====

(test-group consistency
            
            (define-test hellinger-bhattacharyya-relation
              ;; H²(P,Q) = 1 - BC(P,Q)
              (let ([h (hellinger-distance dist-a dist-b)]
                    [bc (bhattacharyya-coefficient dist-a dist-b)])
                   (assert-approx 0.001 (* h h) (- 1 bc))))
            
            (define-test jensen-shannon-vs-kl
              ;; JSD should be <= average of forward and reverse KL
              (let ([jsd (jensen-shannon-divergence dist-a dist-b)]
                    [kl-pq (kl-divergence dist-a dist-b)]
                    [kl-qp (kl-divergence dist-b dist-a)])
                   (assert-true (<= jsd (/ (+ kl-pq kl-qp) 2)))))
            
            (define-test all-divergences-test
              ;; Test that all-divergences returns an association list
              (let ([divs (all-divergences dist-a dist-b)])
                   (assert-true (list? divs))
                   (assert-true (> (length divs) 10))
                   (assert-true (pair? (car divs)))))
            
            ) ; end consistency group

;;; ====
;;; Known Value Tests
;;; ====

(test-group known-values
            
            (define-test kl-uniform-vs-peaked
              ;; KL divergence from uniform to peaked distribution (known ballpark)
              (let ([kl (kl-divergence '(0.333 0.333 0.334) '(0.9 0.05 0.05))])
                   (assert-true (> kl 0.5))  ; Should be substantial
                   (assert-true (< kl 2.0))))
            
            (define-test cross-entropy-known
              ;; H(P,Q) = H(P) + D_KL(P||Q)
              (let ([h-p (entropy dist-a)]
                    [kl (kl-divergence dist-a dist-b)]
                    [ce (cross-entropy dist-a dist-b)])
                   (assert-approx 0.001 ce (+ h-p kl))))
            
            ) ; end known-values group

;;; ====
;;; Edge Cases
;;; ====

(test-group edge-cases
            
            (define-test empty-lists
              (assert-equal 0 (bhattacharyya-coefficient '() '()))
              (assert-equal 1 (hellinger-distance '() '()))
              (assert-equal 1 (total-variation-distance '() '()))
              (assert-equal 0 (chi-squared-divergence '() '())))
            
            (define-test single-element
              (let ([p '(1.0)]
                    [q '(1.0)])
                   (assert-approx 0.001 1.0 (bhattacharyya-coefficient p q))
                   (assert-approx 0.001 0.0 (hellinger-distance p q))))
            
            (define-test zero-probability-handling
              ;; Test that measures handle zeros gracefully
              (let ([p '(1.0 0.0)]
                    [q '(0.5 0.5)])
                   (assert-true (>= (bhattacharyya-coefficient p q) 0))
                   (assert-true (>= (hellinger-distance p q) 0))))
            
            ) ; end edge-cases group

;;; ====
;;; Run All Tests
;;; ====

(newline)
(display "========= Statistical Measures Test Suite ==========\n")
(newline)

(exit-with-summary)
