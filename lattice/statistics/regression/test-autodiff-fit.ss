;;; lattice/statistics/regression/test-autodiff-fit.ss — Tests for autodiff-GLM bridge
;;;
;;; Tests the optimization-statistics-autodiff triangle bridge.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")

(require 'autodiff-fit)

;;; ============================================================
;;; Helper: build design matrix with intercept column
;;; ============================================================

;;; make-design-matrix : (List (List Number)) -> Matrix
;;; Each inner list is a row [1, x1, x2, ...] (intercept included).
(define (make-design-matrix rows)
  (let* ([n (length rows)]
         [p (length (car rows))]
         [data (make-vector (* n p))])
    (let loop ([rs rows] [i 0])
      (if (null? rs)
          (list 'matrix n p data)
          (let inner ([cols (car rs)] [j 0])
            (if (null? cols)
                (loop (cdr rs) (+ i 1))
                (begin
                  (vector-set! data (+ (* i p) j) (car cols))
                  (inner (cdr cols) (+ j 1)))))))))

;;; ============================================================
;;; Test: Gaussian GLM (should recover OLS coefficients)
;;; ============================================================

(test-group "autodiff-gaussian-glm"

  (define-test "Gaussian GLM recovers linear regression coefficients (Newton)"
    ;; y = 2 + 3*x exactly. True beta = [2, 3].
    ;; Newton converges exactly for quadratic objectives.
    (let* ([X (make-design-matrix
               '((1 1) (1 2) (1 3) (1 4) (1 5)
                 (1 6) (1 7) (1 8) (1 9) (1 10)))]
           [y (vector 5 8 11 14 17 20 23 26 29 32)]
           [result (autodiff-glm-fit gaussian-family identity-link X y 'newton)]
           [beta (glm-coefficients result)])
      ;; Newton nails quadratics
      (assert-true (< (abs (- (vector-ref beta 0) 2.0)) 0.01))
      (assert-true (< (abs (- (vector-ref beta 1) 3.0)) 0.01))))

  (define-test "Gaussian GLM with L-BFGS gets reasonable coefficients"
    ;; L-BFGS may not converge to machine precision but should get close
    (let* ([X (make-design-matrix
               '((1 1) (1 2) (1 3) (1 4) (1 5)
                 (1 6) (1 7) (1 8) (1 9) (1 10)))]
           [y (vector 5 8 11 14 17 20 23 26 29 32)]
           [result (autodiff-glm-fit gaussian-family identity-link X y)]
           [beta (glm-coefficients result)])
      ;; L-BFGS should at least get the slope right
      (assert-true (< (abs (- (vector-ref beta 1) 3.0)) 0.5))))

  (define-test "Gaussian GLM returns valid result structure"
    (let* ([X (make-design-matrix
               '((1 1) (1 2) (1 3) (1 4) (1 5)))]
           [y (vector 3 5 7 9 11)]
           [result (autodiff-glm-fit gaussian-family identity-link X y 'newton)])
      (assert-true (glm-result? result))
      (assert-true (eq? (glm-family result) 'gaussian))
      (assert-true (eq? (glm-link result) 'identity))
      (assert-true (> (glm-n result) 0))))
)

;;; ============================================================
;;; Test: Logistic Regression on linearly separable data
;;; ============================================================

(test-group "autodiff-logistic-regression"

  (define-test "logistic regression separates linearly separable classes"
    ;; Class 0: x in {-3, -2, -1, -0.5}, class 1: x in {0.5, 1, 2, 3}
    (let* ([X (make-design-matrix
               '((1 -3) (1 -2) (1 -1) (1 -0.5)
                 (1 0.5) (1 1) (1 2) (1 3)))]
           [y (vector 0 0 0 0 1 1 1 1)]
           [result (autodiff-logistic-regression X y)]
           [beta (glm-coefficients result)])
      ;; beta1 should be positive (higher x -> class 1)
      (assert-true (> (vector-ref beta 1) 0))
      ;; beta0 should be near 0 (data is symmetric around 0)
      (assert-true (< (abs (vector-ref beta 0)) 2.0))))

  (define-test "logistic regression predicts correct probabilities"
    (let* ([X (make-design-matrix
               '((1 -3) (1 -2) (1 -1) (1 -0.5)
                 (1 0.5) (1 1) (1 2) (1 3)))]
           [y (vector 0 0 0 0 1 1 1 1)]
           [result (autodiff-logistic-regression X y)]
           ;; Predict using our wrapper (calls glm-predict internally)
           [preds (autodiff-logistic-predict result X)])
      ;; Predictions for class 0 extremes should be < 0.5
      (assert-true (< (vector-ref preds 0) 0.5))
      (assert-true (< (vector-ref preds 1) 0.5))
      ;; Predictions for class 1 extremes should be > 0.5
      (assert-true (> (vector-ref preds 6) 0.5))
      (assert-true (> (vector-ref preds 7) 0.5))))

  (define-test "logistic classification matches expected labels"
    (let* ([X (make-design-matrix
               '((1 -3) (1 -2) (1 -1) (1 -0.5)
                 (1 0.5) (1 1) (1 2) (1 3)))]
           [y (vector 0 0 0 0 1 1 1 1)]
           [result (autodiff-logistic-regression X y)]
           [classes (autodiff-logistic-classify result X 0.5)])
      ;; Extreme points should classify correctly
      (assert-equal 0 (vector-ref classes 0))  ; x = -3
      (assert-equal 1 (vector-ref classes 7)))) ; x = 3

  (define-test "logistic regression returns valid GLM result"
    (let* ([X (make-design-matrix
               '((1 -2) (1 -1) (1 1) (1 2)))]
           [y (vector 0 0 1 1)]
           [result (autodiff-logistic-regression X y)])
      (assert-true (glm-result? result))
      (assert-true (eq? (glm-family result) 'binomial))
      (assert-true (eq? (glm-link result) 'logit))))
)

;;; ============================================================
;;; Test: Poisson Regression
;;; ============================================================

(test-group "autodiff-poisson-glm"

  (define-test "Poisson GLM fits count data"
    ;; y ~ Poisson(exp(beta0 + beta1*x))
    ;; Use rounded expected counts as data
    (let* ([X (make-design-matrix
               '((1 0) (1 1) (1 2) (1 3) (1 4)
                 (1 0) (1 1) (1 2) (1 3) (1 4)))]
           [y (vector 2 2 3 4 5 2 2 3 4 5)]
           [result (autodiff-glm-fit poisson-family log-link X y)]
           [beta (glm-coefficients result)])
      ;; Should roughly recover beta0 ~ 0.5, beta1 ~ 0.3
      (assert-true (< (abs (- (vector-ref beta 0) 0.5)) 0.5))
      (assert-true (< (abs (- (vector-ref beta 1) 0.3)) 0.3))))
)

;;; ============================================================
;;; Test: Generic autodiff-nll
;;; ============================================================

(test-group "autodiff-nll-generic"

  (define-test "autodiff-nll computes binomial NLL correctly"
    ;; Compare traced-binomial-nll against autodiff-nll with sigmoid link
    (let* ([X (make-design-matrix '((1 1) (1 -1)))]
           [y (vector 1 0)]
           [beta0 (list 0.0 0.0)]
           ;; Evaluate traced-binomial-nll at beta0
           [nll1 (apply (lambda beta-traced
                          (traced-binomial-nll X y beta-traced))
                        beta0)]
           ;; Evaluate autodiff-nll with explicit log-pdf
           [nll2 (apply (lambda beta-traced
                          (autodiff-nll
                           ;; log p(y|mu) = y*log(mu) + (1-y)*log(1-mu)
                           (lambda (yi mu)
                             (traced-add
                              (traced-mul yi (traced-log (traced-add mu 1e-10)))
                              (traced-mul (traced-sub 1 yi)
                                          (traced-log (traced-add (traced-sub 1 mu) 1e-10)))))
                           y
                           traced-sigmoid
                           X
                           beta-traced))
                        beta0)])
      ;; Both should give the same NLL value (at beta=0, p=0.5 for all obs)
      ;; At beta=0: each obs contributes -log(0.5) = 0.693...
      ;; Total NLL should be ~1.386
      (assert-true (< (abs (- nll1 nll2)) 0.01))))
)

;;; ============================================================
;;; Test: Optimizer method selection
;;; ============================================================

(test-group "autodiff-fit-options"

  (define-test "autodiff-glm-fit works with Newton optimizer"
    ;; Newton is best for small well-conditioned problems
    (let* ([X (make-design-matrix
               '((1 1) (1 2) (1 3) (1 4) (1 5)))]
           [y (vector 3 5 7 9 11)]
           [result (autodiff-glm-fit gaussian-family identity-link X y 'newton)]
           [beta (glm-coefficients result)])
      (assert-true (glm-result? result))
      ;; y = 1 + 2*x, so beta0~1, beta1~2
      (assert-true (< (abs (- (vector-ref beta 0) 1.0)) 0.01))
      (assert-true (< (abs (- (vector-ref beta 1) 2.0)) 0.01))))
)

;;; ============================================================
;;; Test: Standard errors
;;; ============================================================

(test-group "autodiff-fit-se"

  (define-test "standard errors are finite and positive for well-conditioned problem"
    (let* ([X (make-design-matrix
               '((1 1) (1 2) (1 3) (1 4) (1 5)
                 (1 6) (1 7) (1 8) (1 9) (1 10)))]
           [y (vector 5 8 11 14 17 20 23 26 29 32)]
           [result (autodiff-glm-fit gaussian-family identity-link X y 'newton)]
           [se (glm-se result)])
      ;; SE should be positive and finite
      (assert-true (> (vector-ref se 0) 0))
      (assert-true (> (vector-ref se 1) 0))
      (assert-true (< (vector-ref se 0) 1e10))
      (assert-true (< (vector-ref se 1) 1e10))))
)

(run-all-tests)
