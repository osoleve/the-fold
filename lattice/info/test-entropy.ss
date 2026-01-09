;;; core/info-theory/test-entropy.ss — Tests for Entropy Module
;;;
;;; Comprehensive tests for information-theoretic quantities.

(load "lattice/info/entropy.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-approx name expected actual tolerance)
  (set! *test-count* (+ *test-count* 1))
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (display " ± ")
       (write tolerance)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (run-tests)
  (display "\n=== Entropy Module Test Suite ===\n\n")
  
  ;; log2 tests
  (display "--- log2 Tests ---\n")
  (test-approx "log2(1) = 0" 0 (log2 1) 0.0001)
  (test-approx "log2(2) = 1" 1 (log2 2) 0.0001)
  (test-approx "log2(4) = 2" 2 (log2 4) 0.0001)
  (test-approx "log2(8) = 3" 3 (log2 8) 0.0001)
  (test-approx "log2(0.5) = -1" -1 (log2 0.5) 0.0001)
  (test "entropy-log2(0) = 0 (convention)" 0 (entropy-log2 0))
  
  ;; Shannon entropy tests
  (display "\n--- Shannon Entropy Tests ---\n")
  
  ;; Uniform distributions
  (test-approx "H(fair coin) = 1 bit"
               1.0 (entropy '(0.5 0.5)) 0.0001)
  (test-approx "H(fair die) = log2(6) ≈ 2.585"
               2.584962 (entropy '(1/6 1/6 1/6 1/6 1/6 1/6)) 0.0001)
  (test-approx "H(4 uniform) = 2 bits"
               2.0 (entropy '(0.25 0.25 0.25 0.25)) 0.0001)
  
  ;; Deterministic (zero entropy)
  (test-approx "H(certain event) = 0"
               0.0 (entropy '(1 0 0 0)) 0.0001)
  (test-approx "H(single outcome) = 0"
               0.0 (entropy '(1.0)) 0.0001)
  
  ;; Biased distributions
  (test-approx "H(biased coin 0.9/0.1) ≈ 0.469"
               0.468996 (entropy '(0.9 0.1)) 0.001)
  (test-approx "H(biased coin 0.75/0.25) ≈ 0.811"
               0.811278 (entropy '(0.75 0.25)) 0.001)
  
  ;; Binary entropy
  (display "\n--- Binary Entropy Tests ---\n")
  (test-approx "H(0.5) = 1" 1.0 (binary-entropy 0.5) 0.0001)
  (test-approx "H(0) = 0" 0.0 (binary-entropy 0) 0.0001)
  (test-approx "H(1) = 0" 0.0 (binary-entropy 1) 0.0001)
  (test-approx "H(0.1) ≈ 0.469" 0.468996 (binary-entropy 0.1) 0.001)
  
  ;; Max entropy
  (display "\n--- Maximum Entropy Tests ---\n")
  (test-approx "max-entropy(2) = 1" 1.0 (max-entropy 2) 0.0001)
  (test-approx "max-entropy(4) = 2" 2.0 (max-entropy 4) 0.0001)
  (test-approx "max-entropy(8) = 3" 3.0 (max-entropy 8) 0.0001)
  
  ;; Entropy ratio
  (display "\n--- Entropy Ratio Tests ---\n")
  (test-approx "ratio(uniform) = 1" 1.0 (entropy-ratio '(0.5 0.5)) 0.0001)
  (test-approx "ratio(certain) = 0" 0.0 (entropy-ratio '(1 0)) 0.0001)
  
  ;; Joint entropy
  (display "\n--- Joint Entropy Tests ---\n")
  (let ([independent-joint '((0.25 0.25) (0.25 0.25))])  ; X, Y independent uniform
       (test-approx "H(X,Y) for independent = 2"
                    2.0 (joint-entropy independent-joint) 0.0001))
  (let ([correlated-joint '((0.5 0) (0 0.5))])  ; X = Y
       (test-approx "H(X,Y) when X=Y = 1"
                    1.0 (joint-entropy correlated-joint) 0.0001))
  
  ;; Mutual information
  (display "\n--- Mutual Information Tests ---\n")
  (let ([independent-joint '((0.25 0.25) (0.25 0.25))]
        [marginal-x '(0.5 0.5)]
        [marginal-y '(0.5 0.5)])
       (test-approx "I(X;Y) for independent = 0"
                    0.0 (mutual-information independent-joint marginal-x marginal-y) 0.0001))
  (let ([correlated-joint '((0.5 0) (0 0.5))]
        [marginal-x '(0.5 0.5)]
        [marginal-y '(0.5 0.5)])
       (test-approx "I(X;Y) when X=Y = 1"
                    1.0 (mutual-information correlated-joint marginal-x marginal-y) 0.0001))
  
  ;; Marginal computation
  (display "\n--- Marginal Computation Tests ---\n")
  (test "compute-marginal-x"
        '(0.5 0.5)
        (compute-marginal-x '((0.25 0.25) (0.25 0.25))))
  (test "compute-marginal-y"
        '(0.5 0.5)
        (compute-marginal-y '((0.25 0.25) (0.25 0.25))))
  
  ;; Cross entropy
  (display "\n--- Cross Entropy Tests ---\n")
  (test-approx "H(P,P) = H(P)"
               1.0 (cross-entropy '(0.5 0.5) '(0.5 0.5)) 0.0001)
  (test-true "H(P,Q) >= H(P) when P≠Q"
             (let ([p '(0.8 0.2)]
                   [q '(0.5 0.5)])
                  (>= (cross-entropy p q) (entropy p))))
  ;; Support mismatch: Q=0 where P>0 means infinite cross-entropy
  (test "H(P,Q) = +inf when Q has zero where P doesn't"
        +inf.0 (cross-entropy '(0.5 0.5) '(1.0 0.0)))
  (test "H(P,Q) = +inf with complete mismatch"
        +inf.0 (cross-entropy '(0.0 1.0) '(1.0 0.0)))
  
  ;; KL divergence
  (display "\n--- KL Divergence Tests ---\n")
  (test-approx "D_KL(P||P) = 0"
               0.0 (kl-divergence '(0.5 0.5) '(0.5 0.5)) 0.0001)
  (test-true "D_KL(P||Q) >= 0"
             (>= (kl-divergence '(0.9 0.1) '(0.5 0.5)) 0))
  (test-true "D_KL asymmetric"
             (let ([d1 (kl-divergence '(0.9 0.1) '(0.5 0.5))]
                   [d2 (kl-divergence '(0.5 0.5) '(0.9 0.1))])
                  (not (= d1 d2))))
  (test "D_KL = +inf when Q has zero where P doesn't"
        +inf.0 (kl-divergence '(0.5 0.5) '(1.0 0.0)))
  
  ;; Jensen-Shannon divergence
  (display "\n--- Jensen-Shannon Divergence Tests ---\n")
  (test-approx "JSD(P,P) = 0"
               0.0 (jensen-shannon-divergence '(0.5 0.5) '(0.5 0.5)) 0.0001)
  (test-true "JSD is symmetric"
             (let ([jsd1 (jensen-shannon-divergence '(0.9 0.1) '(0.5 0.5))]
                   [jsd2 (jensen-shannon-divergence '(0.5 0.5) '(0.9 0.1))])
                  (< (abs (- jsd1 jsd2)) 0.0001)))
  
  ;; Conditional entropy
  (display "\n--- Conditional Entropy Tests ---\n")
  (let ([independent-joint '((0.25 0.25) (0.25 0.25))]
        [marginal-y '(0.5 0.5)])
       (test-approx "H(X|Y) for independent = H(X)"
                    1.0 (conditional-entropy independent-joint marginal-y) 0.0001))
  (let ([correlated-joint '((0.5 0) (0 0.5))]
        [marginal-y '(0.5 0.5)])
       (test-approx "H(X|Y) when X=Y = 0"
                    0.0 (conditional-entropy correlated-joint marginal-y) 0.0001))
  ;; conditional-entropy-direct tests
  (test-approx "H(X|Y) direct computation"
               1.0 (conditional-entropy-direct '((0.5 0.5) (0.5 0.5)) '(0.5 0.5)) 0.0001)
  ;; Zero weights should return 0, not crash with division by zero
  (test "H(X|Y) direct with zero weights = 0"
        0 (conditional-entropy-direct '((0.5 0.5)) '(0)))
  
  ;; Continuous entropy
  (display "\n--- Differential Entropy Tests ---\n")
  ;; Gaussian entropy in bits: (1/2) * log2(2 * pi * e * sigma^2)
  ;; For sigma=1: ≈ 2.047 bits
  (test-approx "H(Gaussian σ=1) ≈ 2.047 bits"
               2.047 (gaussian-entropy 1) 0.01)
  (test-approx "H(Uniform [0,1]) = 0"
               0.0 (uniform-entropy 0 1) 0.0001)
  ;; Differential entropy CAN be negative (unlike discrete entropy)
  (test-approx "H(Uniform [0,0.5]) = -1 (differential can be negative)"
               -1.0 (uniform-entropy 0 0.5) 0.0001)
  (test-approx "H(Uniform [0,2]) = 1"
               1.0 (uniform-entropy 0 2) 0.0001)
  ;; Exponential entropy: log2(e/λ) = log2(e) ≈ 1.4427 for λ=1
  (test-approx "H(Exponential λ=1) = log2(e) ≈ 1.443"
               1.4427 (exponential-entropy 1) 0.001)
  ;; Laplace entropy: log2(2*b*e) for b=1: log2(2e) ≈ 2.4427
  (test-approx "H(Laplace b=1) = log2(2e) ≈ 2.443"
               2.4427 (laplace-entropy 1) 0.001)
  
  ;; Renyi entropy
  (display "\n--- Renyi Entropy Tests ---\n")
  (test-approx "H_1(uniform) = Shannon entropy"
               1.0 (renyi-entropy 1 '(0.5 0.5)) 0.0001)
  (test-approx "H_0(uniform) = Hartley = log2(2)"
               1.0 (renyi-entropy 0 '(0.5 0.5)) 0.0001)
  (test-approx "H_2(uniform) = 1"
               1.0 (collision-entropy '(0.5 0.5)) 0.0001)
  (test-approx "H_inf(uniform) = 1"
               1.0 (min-entropy '(0.5 0.5)) 0.0001)
  
  ;; For non-uniform: H_inf <= H_2 <= H_1 <= H_0
  (let ([probs '(0.7 0.2 0.1)])
       (test-true "H_inf <= H_2 <= H_1 <= H_0"
                  (let ([h0 (hartley-entropy probs)]
                        [h1 (entropy probs)]
                        [h2 (collision-entropy probs)]
                        [hinf (min-entropy probs)])
                       (and (<= hinf h2) (<= h2 h1) (<= h1 h0)))))
  
  ;; Surprisal / self-information
  (display "\n--- Surprisal Tests ---\n")
  (test-approx "I(1) = 0" 0.0 (surprisal 1) 0.0001)
  (test-approx "I(0.5) = 1" 1.0 (surprisal 0.5) 0.0001)
  (test-approx "I(0.25) = 2" 2.0 (surprisal 0.25) 0.0001)
  (test "I(0) = +inf" +inf.0 (surprisal 0))
  
  ;; PMI
  (display "\n--- Pointwise Mutual Information Tests ---\n")
  ;; PMI = log2(P(x,y) / (P(x) * P(y)))
  ;; For independent: P(x,y) = P(x)*P(y), so PMI = 0
  (test-approx "PMI(independent) = 0"
               0.0 (pointwise-mutual-information 0.25 0.5 0.5) 0.0001)
  ;; For P(x,y)=0.5, P(x)=0.5, P(y)=0.5: PMI = log2(0.5 / 0.25) = log2(2) = 1
  ;; This indicates positive association (co-occurrence above independence)
  (test-approx "PMI with positive association"
               1.0 (pointwise-mutual-information 0.5 0.5 0.5) 0.0001)
  
  ;; Sample entropy
  (display "\n--- Sample Entropy Tests ---\n")
  (test-approx "sample entropy uniform"
               1.0 (sample-entropy '(a b a b a b a b)) 0.0001)
  (test-approx "sample entropy all same"
               0.0 (sample-entropy '(a a a a a)) 0.0001)
  
  ;; Summary
  (display "\n=== Test Summary ===\n")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!\n")
      (display "Some tests failed.\n"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
