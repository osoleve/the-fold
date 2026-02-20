(load "core/testing/test-framework.ss")
(load "lattice/info/model-selection.ss")

(test-group "model-selection-info"

  (define-test "log-likelihood-gaussian: perfect residuals"
    ;; Zero residuals -> log-lik should be 0 (degenerate case)
    (assert-equal 0 (log-likelihood-gaussian '(0 0 0 0))))

  (define-test "log-likelihood-gaussian: known residuals"
    ;; For residuals with sigma^2 = 1, n = 2:
    ;; logL = -0.5 * 2 * (1 + log(2*pi*1)) = -(1 + log(2*pi))
    (let* ([r '(1 -1)]  ; ss = 2, sigma^2 = 1
           [ll (log-likelihood-gaussian r)]
           [expected (* -0.5 2 (+ 1 (log (* 2 3.141592653589793))))])
      (assert-true (< (abs (- ll expected)) 1e-6))))

  (define-test "log-likelihood-gaussian-vec matches list version"
    (let* ([r '(0.5 -0.3 0.8 -0.2 0.1)]
           [rv (list->vector r)]
           [ll-list (log-likelihood-gaussian r)]
           [ll-vec (log-likelihood-gaussian-vec rv)])
      (assert-true (< (abs (- ll-list ll-vec)) 1e-10))))

  (define-test "aic: basic formula"
    ;; AIC = -2 * logL + 2 * k
    (assert-equal -6 (aic 5 2)))  ; -2*5 + 2*2 = -6

  (define-test "bic: basic formula"
    ;; BIC = -2 * logL + k * log(n)
    (let ([result (bic 5 2 100)])  ; -2*5 + 2*log(100) = -10 + 2*4.605.. = -0.789..
      (assert-true (< (abs (- result (+ -10 (* 2 (log 100))))) 1e-10))))

  (define-test "aicc: converges to aic for large n"
    (let* ([ll -50]
           [k 3]
           [n 10000]
           [aic-val (aic ll k)]
           [aicc-val (aicc ll k n)])
      ;; AICc correction should be tiny for large n
      (assert-true (< (abs (- aic-val aicc-val)) 0.01))))

  (define-test "aicc: small sample correction is substantial"
    (let* ([ll -50]
           [k 5]
           [n 10]
           [aic-val (aic ll k)]
           [aicc-val (aicc ll k n)])
      ;; AICc should be noticeably larger than AIC
      (assert-true (> aicc-val aic-val))
      ;; Correction = 2*5*6 / (10-5-1) = 60/4 = 15
      (assert-true (< (abs (- (- aicc-val aic-val) 15)) 1e-10))))

  (define-test "aicc: undefined when n <= k+1"
    (assert-equal +inf.0 (aicc -50 5 6)))

  (define-test "aic-weights: sum to 1"
    (let* ([aics '(100 102 105 110)]
           [weights (aic-weights aics)]
           [total (fold-left + 0 weights)])
      (assert-true (< (abs (- total 1.0)) 1e-10))))

  (define-test "aic-weights: best model gets highest weight"
    (let* ([aics '(100 102 105 110)]
           [weights (aic-weights aics)])
      ;; First model (AIC=100) should have highest weight
      (assert-true (> (car weights) (cadr weights)))
      (assert-true (> (cadr weights) (caddr weights)))))

  (define-test "aic-weights: identical AICs give equal weights"
    (let* ([aics '(100 100 100)]
           [weights (aic-weights aics)])
      (assert-true (< (abs (- (car weights) (/ 1.0 3))) 1e-10))
      (assert-true (< (abs (- (cadr weights) (/ 1.0 3))) 1e-10))))

  (define-test "evidence-ratio: basic"
    (assert-true (< (abs (- (evidence-ratio 0.6 0.3) 2.0)) 1e-10)))

  (define-test "residual-entropy-bits: positive for nonzero residuals"
    (let ([h (residual-entropy-bits '(1.0 -0.5 0.3 -0.8 0.2))])
      (assert-true (> h 0))))

  (define-test "residual-entropy-bits: smaller for tighter residuals"
    (let ([h-tight (residual-entropy-bits '(0.01 -0.01 0.005 -0.008))]
          [h-wide (residual-entropy-bits '(5.0 -3.0 4.0 -2.0))])
      (assert-true (< h-tight h-wide))))

  ) ;; end group

(run-all-tests)
