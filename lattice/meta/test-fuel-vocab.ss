;;; lattice/meta/test-fuel-vocab.ss — Tests for Fuel-Cost Vocabulary

(load "core/testing/test-framework.ss")
(load "lattice/meta/fuel-vocab.ss")

(test-group "fuel-vocab"

  ;;; ====
  ;;; Validation
  ;;; ====

  (define-test "fuel-cost-valid? accepts constant"
    (assert-true (fuel-cost-valid? '(constant 50))))

  (define-test "fuel-cost-valid? accepts linear"
    (assert-true (fuel-cost-valid? '(linear n))))

  (define-test "fuel-cost-valid? accepts quadratic"
    (assert-true (fuel-cost-valid? '(quadratic n))))

  (define-test "fuel-cost-valid? accepts cubic"
    (assert-true (fuel-cost-valid? '(cubic n))))

  (define-test "fuel-cost-valid? accepts polynomial"
    (assert-true (fuel-cost-valid? '(polynomial n 4))))

  (define-test "fuel-cost-valid? accepts exponential"
    (assert-true (fuel-cost-valid? '(exponential n))))

  (define-test "fuel-cost-valid? accepts logarithmic"
    (assert-true (fuel-cost-valid? '(logarithmic n))))

  (define-test "fuel-cost-valid? accepts log-linear"
    (assert-true (fuel-cost-valid? '(log-linear n))))

  (define-test "fuel-cost-valid? accepts nested product"
    (assert-true (fuel-cost-valid? '(product (linear m) (quadratic n)))))

  (define-test "fuel-cost-valid? accepts nested max"
    (assert-true (fuel-cost-valid? '(max (log-linear n) (quadratic n)))))

  (define-test "fuel-cost-valid? accepts nested sum"
    (assert-true (fuel-cost-valid? '(sum (linear n) (constant 10)))))

  (define-test "fuel-cost-valid? rejects bare symbol"
    (assert-false (fuel-cost-valid? 'linear)))

  (define-test "fuel-cost-valid? rejects number"
    (assert-false (fuel-cost-valid? 42)))

  (define-test "fuel-cost-valid? rejects unknown tag"
    (assert-false (fuel-cost-valid? '(factorial n))))

  (define-test "fuel-cost-valid? rejects constant with symbol arg"
    (assert-false (fuel-cost-valid? '(constant n))))

  (define-test "fuel-cost-valid? rejects negative constant"
    (assert-false (fuel-cost-valid? '(constant -5))))

  (define-test "fuel-cost-valid? rejects linear with number arg"
    (assert-false (fuel-cost-valid? '(linear 42))))

  (define-test "fuel-cost-valid? rejects polynomial with missing degree"
    (assert-false (fuel-cost-valid? '(polynomial n))))

  (define-test "fuel-cost-valid? rejects product with non-cost children"
    (assert-false (fuel-cost-valid? '(product 5 10))))

  ;;; ====
  ;;; Comparison
  ;;; ====

  (define-test "compare: constant < linear"
    (assert-equal -1 (fuel-cost-compare '(constant 100) '(linear n))))

  (define-test "compare: linear < quadratic"
    (assert-equal -1 (fuel-cost-compare '(linear n) '(quadratic n))))

  (define-test "compare: quadratic < cubic"
    (assert-equal -1 (fuel-cost-compare '(quadratic n) '(cubic n))))

  (define-test "compare: cubic < exponential"
    (assert-equal -1 (fuel-cost-compare '(cubic n) '(exponential n))))

  (define-test "compare: log-linear < quadratic"
    (assert-equal -1 (fuel-cost-compare '(log-linear n) '(quadratic n))))

  (define-test "compare: logarithmic < linear"
    (assert-equal -1 (fuel-cost-compare '(logarithmic n) '(linear n))))

  (define-test "compare: same class returns 0"
    (assert-equal 0 (fuel-cost-compare '(linear n) '(linear m))))

  (define-test "compare: constants compared by value"
    (assert-equal -1 (fuel-cost-compare '(constant 10) '(constant 100))))

  (define-test "compare: polynomial degree matters"
    (assert-equal -1 (fuel-cost-compare '(polynomial n 2) '(polynomial n 5))))

  (define-test "compare: polynomial n^2 = quadratic"
    (assert-equal 0 (fuel-cost-compare '(polynomial n 2) '(quadratic n))))

  (define-test "compare: polynomial n^3 = cubic"
    (assert-equal 0 (fuel-cost-compare '(polynomial n 3) '(cubic n))))

  (define-test "compare: exponential > cubic"
    (assert-equal 1 (fuel-cost-compare '(exponential n) '(cubic n))))

  (define-test "compare: product picks dominant"
    ;; product(linear, quadratic) is dominated by quadratic
    (assert-equal 0 (fuel-cost-compare
                     '(product (linear m) (quadratic n))
                     '(quadratic n))))

  ;;; ====
  ;;; Display
  ;;; ====

  (define-test "display: constant"
    (assert-equal "O(1) [50 fuel]" (fuel-cost->string '(constant 50))))

  (define-test "display: linear"
    (assert-equal "O(n)" (fuel-cost->string '(linear n))))

  (define-test "display: quadratic"
    (assert-equal "O(n^2)" (fuel-cost->string '(quadratic n))))

  (define-test "display: cubic"
    (assert-equal "O(n^3)" (fuel-cost->string '(cubic n))))

  (define-test "display: polynomial"
    (assert-equal "O(n^4)" (fuel-cost->string '(polynomial n 4))))

  (define-test "display: exponential"
    (assert-equal "O(2^n)" (fuel-cost->string '(exponential n))))

  (define-test "display: logarithmic"
    (assert-equal "O(log n)" (fuel-cost->string '(logarithmic n))))

  (define-test "display: log-linear"
    (assert-equal "O(n log n)" (fuel-cost->string '(log-linear n))))

  (define-test "display: product"
    (assert-equal "O(n) * O(n^2)"
                  (fuel-cost->string '(product (linear n) (quadratic n)))))

  (define-test "display: max"
    (assert-equal "max(O(n log n), O(n^2))"
                  (fuel-cost->string '(max (log-linear n) (quadratic n)))))

  ;;; ====
  ;;; Estimation
  ;;; ====

  (define-test "estimate: constant"
    (assert-equal 50 (estimate-fuel '(constant 50) 1000)))

  (define-test "estimate: linear"
    (assert-equal 100 (estimate-fuel '(linear n) 100)))

  (define-test "estimate: quadratic"
    (assert-equal 10000 (estimate-fuel '(quadratic n) 100)))

  (define-test "estimate: cubic"
    (assert-equal 1000000 (estimate-fuel '(cubic n) 100)))

  (define-test "estimate: polynomial degree 4"
    (assert-equal 10000 (estimate-fuel '(polynomial n 4) 10)))

  (define-test "estimate: exponential"
    (assert-equal 1024 (estimate-fuel '(exponential n) 10)))

  (define-test "estimate: product of linear costs"
    (assert-equal 10000 (estimate-fuel '(product (linear m) (linear n)) 100)))

  (define-test "estimate: max picks larger"
    (assert-equal 10000 (estimate-fuel '(max (linear n) (quadratic n)) 100)))

  (define-test "estimate: sum adds"
    (assert-equal 10100 (estimate-fuel '(sum (linear n) (quadratic n)) 100)))

  ;;; ====
  ;;; Constructors
  ;;; ====

  (define-test "constructors produce valid forms"
    (assert-true (fuel-cost-valid? (fuel/constant 10)))
    (assert-true (fuel-cost-valid? (fuel/linear 'n)))
    (assert-true (fuel-cost-valid? (fuel/quadratic 'n)))
    (assert-true (fuel-cost-valid? (fuel/cubic 'n)))
    (assert-true (fuel-cost-valid? (fuel/polynomial 'n 5)))
    (assert-true (fuel-cost-valid? (fuel/exponential 'n)))
    (assert-true (fuel-cost-valid? (fuel/logarithmic 'n)))
    (assert-true (fuel-cost-valid? (fuel/log-linear 'n)))
    (assert-true (fuel-cost-valid? (fuel/product (fuel/linear 'n) (fuel/linear 'm))))
    (assert-true (fuel-cost-valid? (fuel/max (fuel/linear 'n) (fuel/cubic 'n))))
    (assert-true (fuel-cost-valid? (fuel/sum (fuel/constant 5) (fuel/linear 'n)))))

  (define-test "constructor round-trips through display"
    (assert-equal "O(n^3)" (fuel-cost->string (fuel/cubic 'n)))
    (assert-equal "O(n log n)" (fuel-cost->string (fuel/log-linear 'n))))

) ;; end test-group

(run-all-tests)
