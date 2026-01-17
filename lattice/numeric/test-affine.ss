;;; lattice/numeric/test-affine.ss — Tests for Affine Arithmetic
;;;
;;; Tests demonstrating:
;;; 1. The dependency problem solution
;;; 2. Tighter bounds vs interval arithmetic
;;; 3. Correctness of all operations

(load "core/testing/test-framework.ss")
(load "lattice/numeric/affine.ss")

;;; ============================================================================
;;; Dependency Problem Tests
;;; ============================================================================

(test-group "dependency-problem"
  (define-test "x - x should be zero (not [-1, 1])"
    ;; This is THE canonical test for affine arithmetic
    ;; With standard intervals: [1,2] - [1,2] = [-1, 1] (WRONG)
    ;; With affine forms: the ε₁ terms cancel!
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]  ; 1.5 + 0.5*ε₀
           [result (affine-sub x x)]                   ; Should cancel
           [iv (affine->interval result)])
      (assert-true (< (interval-width iv) 1e-10)
                   "x - x should have zero width")))

  (define-test "2x - x should equal x"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [two-x (affine-scale x 2)]
           [result (affine-sub two-x x)]
           [iv (affine->interval result)])
      ;; Result should be same as x: [1, 2]
      (assert-true (< (abs (- (interval-lo iv) 1)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 2)) 1e-10))))

  (define-test "x + x - 2x should be zero"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 0 1))]
           [x-plus-x (affine-add x x)]
           [two-x (affine-scale x 2)]
           [result (affine-sub x-plus-x two-x)]
           [iv (affine->interval result)])
      (assert-true (< (interval-width iv) 1e-10))))

  (define-test "(x + y) - x should equal y"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]   ; ε₀
           [y (affine-from-interval (interval 10 20))] ; ε₁
           [sum (affine-add x y)]
           [result (affine-sub sum x)]
           [iv (affine->interval result)])
      ;; Should recover y's bounds [10, 20]
      (assert-true (< (abs (- (interval-lo iv) 10)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 20)) 1e-10)))))

;;; ============================================================================
;;; Tighter Bounds Tests
;;; ============================================================================

(test-group "tighter-bounds"
  (define-test "x² is tighter than x*x (intervals)"
    ;; For x ∈ [-1, 1]:
    ;; Interval x*x: [-1,1] * [-1,1] = [-1, 1] (WRONG, should be [0, 1])
    ;; Affine x²: uses dedicated sqr, much tighter
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -1 1))]
           [sqr-result (affine-sqr x)]
           [iv (affine->interval sqr-result)])
      ;; Should be close to [0, 1] (not [-1, 1])
      (assert-true (>= (interval-lo iv) -0.1)
                   "Lower bound should be near 0")
      (assert-true (<= (interval-hi iv) 1.5)
                   "Upper bound should be near 1")))

  (define-test "correlated expressions give tighter bounds"
    ;; f(x) = x² - x where x ∈ [0, 2]
    ;; True range: f(0)=0, f(1)=0, f(2)=2, min at x=0.5 where f=-0.25
    ;; So true range is [-0.25, 2]
    ;; Interval arithmetic: [0,4] - [0,2] = [-2, 4] (way too wide)
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 0 2))]
           [x-sqr (affine-sqr x)]
           [result (affine-sub x-sqr x)]
           [iv (affine->interval result)])
      ;; Affine should be significantly tighter than [-2, 4]
      (assert-true (< (interval-width iv) 5)
                   "Affine should be tighter than naive interval"))))

;;; ============================================================================
;;; Basic Operation Tests
;;; ============================================================================

(test-group "basic-operations"
  (define-test "constant affine forms"
    (let* ([c (affine-constant 5)]
           [iv (affine->interval c)])
      (assert-equal 5 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "negation"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 3))]
           [neg-x (affine-neg x)]
           [iv (affine->interval neg-x)])
      (assert-true (< (abs (- (interval-lo iv) -3)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) -1)) 1e-10))))

  (define-test "scalar multiplication"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [scaled (affine-scale x 3)]
           [iv (affine->interval scaled)])
      (assert-true (< (abs (- (interval-lo iv) 3)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 6)) 1e-10))))

  (define-test "addition of independent variables"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 3 5))]
           [sum (affine-add x y)]
           [iv (affine->interval sum)])
      (assert-true (< (abs (- (interval-lo iv) 4)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 7)) 1e-10)))))

;;; ============================================================================
;;; Non-Affine Operation Tests
;;; ============================================================================

(test-group "non-affine-operations"
  (define-test "multiplication contains true product"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 2 3))]
           [y (affine-from-interval (interval 4 5))]
           [prod (affine-mul x y)]
           [iv (affine->interval prod)])
      ;; True product range: [2*4, 3*5] = [8, 15]
      (assert-true (<= (interval-lo iv) 8))
      (assert-true (>= (interval-hi iv) 15))))

  (define-test "division contains true quotient"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 4 8))]
           [y (affine-from-interval (interval 2 4))]
           [quot (affine-div x y)]
           [iv (affine->interval quot)])
      ;; True range: [4/4, 8/2] = [1, 4]
      (assert-true (<= (interval-lo iv) 1))
      (assert-true (>= (interval-hi iv) 4))))

  (define-test "division by zero detection"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval -1 1))]  ; Contains zero
           [result (affine-div x y)])
      (assert-equal 'division-by-zero result)))

  (define-test "sqrt contains true sqrt"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 4 9))]
           [result (affine-sqrt x)]
           [iv (affine->interval result)])
      ;; True range: [sqrt(4), sqrt(9)] = [2, 3]
      (assert-true (<= (interval-lo iv) 2))
      (assert-true (>= (interval-hi iv) 3))))

  (define-test "sqrt domain error for negative"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -2 -1))]
           [result (affine-sqrt x)])
      (assert-equal 'domain-error result))))

;;; ============================================================================
;;; Elementary Function Tests
;;; ============================================================================

(test-group "elementary-functions"
  (define-test "exp contains true exp"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 0 1))]
           [result (affine-exp x)]
           [iv (affine->interval result)])
      ;; True range: [exp(0), exp(1)] = [1, e ≈ 2.718]
      (assert-true (<= (interval-lo iv) 1))
      (assert-true (>= (interval-hi iv) (exp 1)))))

  (define-test "log contains true log"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 (exp 1)))]
           [result (affine-log x)]
           [iv (affine->interval result)])
      ;; True range: [log(1), log(e)] = [0, 1]
      (assert-true (<= (interval-lo iv) 0))
      (assert-true (>= (interval-hi iv) 1))))

  (define-test "log domain error for non-positive"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -2 -1))]
           [result (affine-log x)])
      (assert-equal 'domain-error result))))

;;; ============================================================================
;;; Complex Expression Tests
;;; ============================================================================

(test-group "complex-expressions"
  (define-test "quadratic formula discriminant"
    ;; For ax² + bx + c, discriminant = b² - 4ac
    ;; Let's compute with a,b,c uncertain
    (affine-reset-noise-counter!)
    (let* ([a (affine-from-interval (interval 1 1.1))]
           [b (affine-from-interval (interval 2 2.2))]
           [c (affine-from-interval (interval 0.5 0.6))]
           [b-sqr (affine-sqr b)]
           [four-a-c (affine-scale (affine-mul a c) 4)]
           [disc (affine-sub b-sqr four-a-c)]
           [iv (affine->interval disc)])
      ;; Check it contains some reasonable values
      ;; b²-4ac with b=2, a=1, c=0.5: 4 - 2 = 2
      ;; b²-4ac with b=2.2, a=1.1, c=0.6: 4.84 - 2.64 = 2.2
      (assert-true (<= (interval-lo iv) 1.5))
      (assert-true (>= (interval-hi iv) 2.5))))

  (define-test "polynomial evaluation via Horner"
    ;; p(x) = 1 + 2x + 3x² at x ∈ [0, 1]
    ;; p(0) = 1, p(1) = 6, min at x = -2/6 ≈ -0.33 (outside range)
    ;; So true range is [1, 6]
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 0 1))]
           [result (affine-horner '(1 2 3) x)]
           [iv (affine->interval result)])
      (assert-true (<= (interval-lo iv) 1))
      (assert-true (>= (interval-hi iv) 6)))))

;;; ============================================================================
;;; Comparison Tests
;;; ============================================================================

(test-group "comparisons"
  (define-test "definitely less than"
    (affine-reset-noise-counter!)
    (let ([x (affine-from-interval (interval 1 2))]
          [y (affine-from-interval (interval 5 6))])
      (assert-true (affine-definitely< x y))))

  (define-test "not definitely less when overlapping"
    (affine-reset-noise-counter!)
    (let ([x (affine-from-interval (interval 1 5))]
          [y (affine-from-interval (interval 3 7))])
      (assert-false (affine-definitely< x y))))

  (define-test "possibly zero"
    (affine-reset-noise-counter!)
    (let ([x (affine-from-interval (interval -1 1))])
      (assert-true (affine-possibly-zero? x))))

  (define-test "definitely positive"
    (affine-reset-noise-counter!)
    (let ([x (affine-from-interval (interval 1 2))])
      (assert-true (affine-definitely-positive? x)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
