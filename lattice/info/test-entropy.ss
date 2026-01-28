;;; core/info-theory/test-entropy.ss — Tests for Entropy Module
;;;
;;; Comprehensive tests for information-theoretic quantities.

(load "core/testing/test-framework.ss")
(load "lattice/info/entropy.ss")

;;; Helper for approximate equality tests
(define (approx-equal? expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

(test-group "log2"
  (define-test "log2(1) = 0"
    (assert-true (approx-equal? 0 (log2 1) 0.0001)))
  (define-test "log2(2) = 1"
    (assert-true (approx-equal? 1 (log2 2) 0.0001)))
  (define-test "log2(4) = 2"
    (assert-true (approx-equal? 2 (log2 4) 0.0001)))
  (define-test "log2(8) = 3"
    (assert-true (approx-equal? 3 (log2 8) 0.0001)))
  (define-test "log2(0.5) = -1"
    (assert-true (approx-equal? -1 (log2 0.5) 0.0001)))
  (define-test "entropy-log2(0) = 0 (convention)"
    (assert-equal 0 (entropy-log2 0))))

(test-group "Shannon entropy"
  ;; Uniform distributions
  (define-test "H(fair coin) = 1 bit"
    (assert-true (approx-equal? 1.0 (entropy '(0.5 0.5)) 0.0001)))
  (define-test "H(fair die) = log2(6) ≈ 2.585"
    (assert-true (approx-equal? 2.584962 (entropy '(1/6 1/6 1/6 1/6 1/6 1/6)) 0.0001)))
  (define-test "H(4 uniform) = 2 bits"
    (assert-true (approx-equal? 2.0 (entropy '(0.25 0.25 0.25 0.25)) 0.0001)))

  ;; Deterministic (zero entropy)
  (define-test "H(certain event) = 0"
    (assert-true (approx-equal? 0.0 (entropy '(1 0 0 0)) 0.0001)))
  (define-test "H(single outcome) = 0"
    (assert-true (approx-equal? 0.0 (entropy '(1.0)) 0.0001)))

  ;; Biased distributions
  (define-test "H(biased coin 0.9/0.1) ≈ 0.469"
    (assert-true (approx-equal? 0.468996 (entropy '(0.9 0.1)) 0.001)))
  (define-test "H(biased coin 0.75/0.25) ≈ 0.811"
    (assert-true (approx-equal? 0.811278 (entropy '(0.75 0.25)) 0.001))))

(test-group "Binary entropy"
  (define-test "H(0.5) = 1"
    (assert-true (approx-equal? 1.0 (binary-entropy 0.5) 0.0001)))
  (define-test "H(0) = 0"
    (assert-true (approx-equal? 0.0 (binary-entropy 0) 0.0001)))
  (define-test "H(1) = 0"
    (assert-true (approx-equal? 0.0 (binary-entropy 1) 0.0001)))
  (define-test "H(0.1) ≈ 0.469"
    (assert-true (approx-equal? 0.468996 (binary-entropy 0.1) 0.001))))

(test-group "Maximum entropy"
  (define-test "max-entropy(2) = 1"
    (assert-true (approx-equal? 1.0 (max-entropy 2) 0.0001)))
  (define-test "max-entropy(4) = 2"
    (assert-true (approx-equal? 2.0 (max-entropy 4) 0.0001)))
  (define-test "max-entropy(8) = 3"
    (assert-true (approx-equal? 3.0 (max-entropy 8) 0.0001))))

(test-group "Entropy ratio"
  (define-test "ratio(uniform) = 1"
    (assert-true (approx-equal? 1.0 (entropy-ratio '(0.5 0.5)) 0.0001)))
  (define-test "ratio(certain) = 0"
    (assert-true (approx-equal? 0.0 (entropy-ratio '(1 0)) 0.0001))))

(test-group "Joint entropy"
  (define-test "H(X,Y) for independent = 2"
    (let ([independent-joint '((0.25 0.25) (0.25 0.25))])  ; X, Y independent uniform
      (assert-true (approx-equal? 2.0 (joint-entropy independent-joint) 0.0001))))
  (define-test "H(X,Y) when X=Y = 1"
    (let ([correlated-joint '((0.5 0) (0 0.5))])  ; X = Y
      (assert-true (approx-equal? 1.0 (joint-entropy correlated-joint) 0.0001)))))

(test-group "Mutual information"
  (define-test "I(X;Y) for independent = 0"
    (let ([independent-joint '((0.25 0.25) (0.25 0.25))]
          [marginal-x '(0.5 0.5)]
          [marginal-y '(0.5 0.5)])
      (assert-true (approx-equal? 0.0 (mutual-information independent-joint marginal-x marginal-y) 0.0001))))
  (define-test "I(X;Y) when X=Y = 1"
    (let ([correlated-joint '((0.5 0) (0 0.5))]
          [marginal-x '(0.5 0.5)]
          [marginal-y '(0.5 0.5)])
      (assert-true (approx-equal? 1.0 (mutual-information correlated-joint marginal-x marginal-y) 0.0001)))))

(test-group "Marginal computation"
  (define-test "compute-marginal-x"
    (assert-equal '(0.5 0.5)
                  (compute-marginal-x '((0.25 0.25) (0.25 0.25)))))
  (define-test "compute-marginal-y"
    (assert-equal '(0.5 0.5)
                  (compute-marginal-y '((0.25 0.25) (0.25 0.25))))))

(test-group "Cross entropy"
  (define-test "H(P,P) = H(P)"
    (assert-true (approx-equal? 1.0 (cross-entropy '(0.5 0.5) '(0.5 0.5)) 0.0001)))
  (define-test "H(P,Q) >= H(P) when P≠Q"
    (let ([p '(0.8 0.2)]
          [q '(0.5 0.5)])
      (assert-true (>= (cross-entropy p q) (entropy p)))))
  ;; Support mismatch: Q=0 where P>0 means infinite cross-entropy
  (define-test "H(P,Q) = +inf when Q has zero where P doesn't"
    (assert-equal +inf.0 (cross-entropy '(0.5 0.5) '(1.0 0.0))))
  (define-test "H(P,Q) = +inf with complete mismatch"
    (assert-equal +inf.0 (cross-entropy '(0.0 1.0) '(1.0 0.0)))))

(test-group "KL divergence"
  (define-test "D_KL(P||P) = 0"
    (assert-true (approx-equal? 0.0 (kl-divergence '(0.5 0.5) '(0.5 0.5)) 0.0001)))
  (define-test "D_KL(P||Q) >= 0"
    (assert-true (>= (kl-divergence '(0.9 0.1) '(0.5 0.5)) 0)))
  (define-test "D_KL asymmetric"
    (let ([d1 (kl-divergence '(0.9 0.1) '(0.5 0.5))]
          [d2 (kl-divergence '(0.5 0.5) '(0.9 0.1))])
      (assert-true (not (= d1 d2)))))
  (define-test "D_KL = +inf when Q has zero where P doesn't"
    (assert-equal +inf.0 (kl-divergence '(0.5 0.5) '(1.0 0.0)))))

(test-group "Jensen-Shannon divergence"
  (define-test "JSD(P,P) = 0"
    (assert-true (approx-equal? 0.0 (jensen-shannon-divergence '(0.5 0.5) '(0.5 0.5)) 0.0001)))
  (define-test "JSD is symmetric"
    (let ([jsd1 (jensen-shannon-divergence '(0.9 0.1) '(0.5 0.5))]
          [jsd2 (jensen-shannon-divergence '(0.5 0.5) '(0.9 0.1))])
      (assert-true (< (abs (- jsd1 jsd2)) 0.0001)))))

(test-group "Conditional entropy"
  (define-test "H(X|Y) for independent = H(X)"
    (let ([independent-joint '((0.25 0.25) (0.25 0.25))]
          [marginal-y '(0.5 0.5)])
      (assert-true (approx-equal? 1.0 (conditional-entropy independent-joint marginal-y) 0.0001))))
  (define-test "H(X|Y) when X=Y = 0"
    (let ([correlated-joint '((0.5 0) (0 0.5))]
          [marginal-y '(0.5 0.5)])
      (assert-true (approx-equal? 0.0 (conditional-entropy correlated-joint marginal-y) 0.0001))))
  ;; conditional-entropy-direct tests
  (define-test "H(X|Y) direct computation"
    (assert-true (approx-equal? 1.0 (conditional-entropy-direct '((0.5 0.5) (0.5 0.5)) '(0.5 0.5)) 0.0001)))
  ;; Zero weights should return 0, not crash with division by zero
  (define-test "H(X|Y) direct with zero weights = 0"
    (assert-equal 0 (conditional-entropy-direct '((0.5 0.5)) '(0)))))

(test-group "Differential entropy"
  ;; Gaussian entropy in bits: (1/2) * log2(2 * pi * e * sigma^2)
  ;; For sigma=1: ≈ 2.047 bits
  (define-test "H(Gaussian σ=1) ≈ 2.047 bits"
    (assert-true (approx-equal? 2.047 (gaussian-entropy 1) 0.01)))
  (define-test "H(Uniform [0,1]) = 0"
    (assert-true (approx-equal? 0.0 (uniform-entropy 0 1) 0.0001)))
  ;; Differential entropy CAN be negative (unlike discrete entropy)
  (define-test "H(Uniform [0,0.5]) = -1 (differential can be negative)"
    (assert-true (approx-equal? -1.0 (uniform-entropy 0 0.5) 0.0001)))
  (define-test "H(Uniform [0,2]) = 1"
    (assert-true (approx-equal? 1.0 (uniform-entropy 0 2) 0.0001)))
  ;; Exponential entropy: log2(e/λ) = log2(e) ≈ 1.4427 for λ=1
  (define-test "H(Exponential λ=1) = log2(e) ≈ 1.443"
    (assert-true (approx-equal? 1.4427 (exponential-entropy 1) 0.001)))
  ;; Laplace entropy: log2(2*b*e) for b=1: log2(2e) ≈ 2.4427
  (define-test "H(Laplace b=1) = log2(2e) ≈ 2.443"
    (assert-true (approx-equal? 2.4427 (laplace-entropy 1) 0.001))))

(test-group "Renyi entropy"
  (define-test "H_1(uniform) = Shannon entropy"
    (assert-true (approx-equal? 1.0 (renyi-entropy 1 '(0.5 0.5)) 0.0001)))
  (define-test "H_0(uniform) = Hartley = log2(2)"
    (assert-true (approx-equal? 1.0 (renyi-entropy 0 '(0.5 0.5)) 0.0001)))
  (define-test "H_2(uniform) = 1"
    (assert-true (approx-equal? 1.0 (collision-entropy '(0.5 0.5)) 0.0001)))
  (define-test "H_inf(uniform) = 1"
    (assert-true (approx-equal? 1.0 (min-entropy '(0.5 0.5)) 0.0001)))

  ;; For non-uniform: H_inf <= H_2 <= H_1 <= H_0
  (define-test "H_inf <= H_2 <= H_1 <= H_0"
    (let* ([probs '(0.7 0.2 0.1)]
           [h0 (hartley-entropy probs)]
           [h1 (entropy probs)]
           [h2 (collision-entropy probs)]
           [hinf (min-entropy probs)])
      (assert-true (and (<= hinf h2) (<= h2 h1) (<= h1 h0))))))

(test-group "Surprisal"
  (define-test "I(1) = 0"
    (assert-true (approx-equal? 0.0 (surprisal 1) 0.0001)))
  (define-test "I(0.5) = 1"
    (assert-true (approx-equal? 1.0 (surprisal 0.5) 0.0001)))
  (define-test "I(0.25) = 2"
    (assert-true (approx-equal? 2.0 (surprisal 0.25) 0.0001)))
  (define-test "I(0) = +inf"
    (assert-equal +inf.0 (surprisal 0))))

(test-group "Pointwise mutual information"
  ;; PMI = log2(P(x,y) / (P(x) * P(y)))
  ;; For independent: P(x,y) = P(x)*P(y), so PMI = 0
  (define-test "PMI(independent) = 0"
    (assert-true (approx-equal? 0.0 (pointwise-mutual-information 0.25 0.5 0.5) 0.0001)))
  ;; For P(x,y)=0.5, P(x)=0.5, P(y)=0.5: PMI = log2(0.5 / 0.25) = log2(2) = 1
  ;; This indicates positive association (co-occurrence above independence)
  (define-test "PMI with positive association"
    (assert-true (approx-equal? 1.0 (pointwise-mutual-information 0.5 0.5 0.5) 0.0001))))

(test-group "Sample entropy"
  (define-test "sample entropy uniform"
    (assert-true (approx-equal? 1.0 (sample-entropy '(a b a b a b a b)) 0.0001)))
  (define-test "sample entropy all same"
    (assert-true (approx-equal? 0.0 (sample-entropy '(a a a a a)) 0.0001))))

;; Run tests
(run-all-tests)
