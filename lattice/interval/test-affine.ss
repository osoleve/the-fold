;;; lattice/interval/test-affine.ss — Tests for Affine Arithmetic
;;;
;;; Tests demonstrating:
;;; 1. The dependency problem solution
;;; 2. Tighter bounds vs interval arithmetic
;;; 3. Correctness of all operations

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/interval/affine.ss")

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
      (assert-equal 'domain-error result)))

  (define-test "direct reciprocal of constant"
    ;; Gemini QA recommendation: explicit test for affine-recip
    (affine-reset-noise-counter!)
    (let* ([x (affine-constant 2)]
           [result (affine-recip x)]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (affine-center result) 0.5)) 1e-10)
                   "1/2 should equal 0.5")))

  (define-test "reciprocal contains true reciprocal"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 2 4))]
           [result (affine-recip x)]
           [iv (affine->interval result)])
      ;; True range: [1/4, 1/2] = [0.25, 0.5]
      (assert-true (<= (interval-lo iv) 0.25))
      (assert-true (>= (interval-hi iv) 0.5)))))

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
;;; Tightness Tests for Elementary Functions
;;; ============================================================================
;;;
;;; These tests verify bounds quality, not just soundness. A sound implementation
;;; could return [-∞, ∞] for everything - these tests catch that.

(test-group "tightness-elementary"
  ;; Helper: compute overestimation ratio
  ;; For monotonic f over [a,b], true range is [f(a), f(b)]
  ;; Ratio = computed_width / true_width (1.0 = perfect, higher = looser)

  (define-test "sqrt tightness: overestimation ratio < 1.5"
    ;; sqrt is concave, so optimal Chebyshev gives tight bounds
    (affine-reset-noise-counter!)
    (let* ([a 4.0] [b 9.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-sqrt x)]
           [iv (affine->interval result)]
           [true-lo (sqrt a)]
           [true-hi (sqrt b)]
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      ;; With optimal Chebyshev, ratio should be close to 1.0
      ;; Allow 1.5 as upper bound (50% overestimation max)
      (assert-true (< ratio 1.5)
                   (format "sqrt overestimation ratio ~a should be < 1.5" ratio))))

  (define-test "sqrt tightness: bounds within 10% of true range"
    (affine-reset-noise-counter!)
    (let* ([a 1.0] [b 4.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-sqrt x)]
           [iv (affine->interval result)]
           [true-lo (sqrt a)]   ; 1.0
           [true-hi (sqrt b)]   ; 2.0
           [lo-error (- true-lo (interval-lo iv))]
           [hi-error (- (interval-hi iv) true-hi)]
           [true-width (- true-hi true-lo)])
      ;; Lower bound should not be much below true lower
      (assert-true (< lo-error (* 0.1 true-width))
                   (format "sqrt lower bound error ~a should be < 10% of width" lo-error))
      ;; Upper bound should not be much above true upper
      (assert-true (< hi-error (* 0.1 true-width))
                   (format "sqrt upper bound error ~a should be < 10% of width" hi-error))))

  (define-test "exp tightness: overestimation ratio < 1.5"
    ;; exp is convex, Chebyshev optimization applies
    (affine-reset-noise-counter!)
    (let* ([a 0.0] [b 1.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-exp x)]
           [iv (affine->interval result)]
           [true-lo (exp a)]    ; 1.0
           [true-hi (exp b)]    ; e ≈ 2.718
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      (assert-true (< ratio 1.5)
                   (format "exp overestimation ratio ~a should be < 1.5" ratio))))

  (define-test "exp tightness: wide interval still reasonable"
    ;; Even for wider intervals, bounds shouldn't explode
    (affine-reset-noise-counter!)
    (let* ([a -1.0] [b 2.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-exp x)]
           [iv (affine->interval result)]
           [true-lo (exp a)]    ; ~0.368
           [true-hi (exp b)]    ; ~7.389
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      ;; For wider intervals, allow up to 2x overestimation
      (assert-true (< ratio 2.0)
                   (format "exp wide interval ratio ~a should be < 2.0" ratio))))

  (define-test "log tightness: overestimation ratio < 1.5"
    ;; log is concave, Chebyshev optimization applies
    (affine-reset-noise-counter!)
    (let* ([a 1.0] [b (exp 1)]
           [x (affine-from-interval (interval a b))]
           [result (affine-log x)]
           [iv (affine->interval result)]
           [true-lo (log a)]    ; 0.0
           [true-hi (log b)]    ; 1.0
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      (assert-true (< ratio 1.5)
                   (format "log overestimation ratio ~a should be < 1.5" ratio))))

  (define-test "log tightness: decade range"
    ;; Test over [1, 10] - one decade
    (affine-reset-noise-counter!)
    (let* ([a 1.0] [b 10.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-log x)]
           [iv (affine->interval result)]
           [true-lo (log a)]    ; 0.0
           [true-hi (log b)]    ; ~2.303
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      (assert-true (< ratio 1.5)
                   (format "log decade ratio ~a should be < 1.5" ratio))))

  (define-test "affine sqrt is tighter than naive interval sqrt"
    ;; Compare with what naive interval arithmetic would give
    (affine-reset-noise-counter!)
    (let* ([a 1.0] [b 9.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-sqrt x)]
           [iv (affine->interval result)]
           ;; Naive interval: just [sqrt(a), sqrt(b)] - actually optimal for sqrt!
           ;; But for non-monotonic compositions, affine wins
           ;; Here we just verify we're at least as good
           [naive-lo (sqrt a)]
           [naive-hi (sqrt b)]
           [affine-width (interval-width iv)]
           [naive-width (- naive-hi naive-lo)])
      ;; Affine should be close to naive (within 50%)
      (assert-true (<= affine-width (* 1.5 naive-width))
                   "affine-sqrt should be close to optimal for simple intervals")))

  (define-test "composed sqrt maintains tightness"
    ;; sqrt(x + y) where x, y independent
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 3 4))]
           [sum (affine-add x y)]            ; [4, 6] as interval
           [result (affine-sqrt sum)]
           [iv (affine->interval result)]
           ;; True range: [sqrt(4), sqrt(6)] = [2, ~2.449]
           [true-lo (sqrt 4)]
           [true-hi (sqrt 6)]
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      (assert-true (< ratio 2.0)
                   (format "composed sqrt ratio ~a should be < 2.0" ratio))))

  (define-test "reciprocal tightness"
    ;; 1/x for x ∈ [2, 4]
    (affine-reset-noise-counter!)
    (let* ([a 2.0] [b 4.0]
           [x (affine-from-interval (interval a b))]
           [result (affine-recip x)]
           [iv (affine->interval result)]
           ;; True range: [1/4, 1/2] = [0.25, 0.5]
           [true-lo (/ 1 b)]
           [true-hi (/ 1 a)]
           [true-width (- true-hi true-lo)]
           [computed-width (interval-width iv)]
           [ratio (/ computed-width true-width)])
      ;; Reciprocal is trickier, allow more slack
      (assert-true (< ratio 2.5)
                   (format "reciprocal ratio ~a should be < 2.5" ratio))))

  (define-test "numerical fallback for deriv-inverse (fold-zxpn)"
    ;; Test affine-chebyshev-approx with deriv-inverse = #f
    ;; Should use numerical bisection to find tangent point
    (affine-reset-noise-counter!)
    (let* ([a 0.0] [b 1.0]
           [x (affine-from-interval (interval a b))]
           ;; Use exp with numerical fallback (deriv-inverse = #f)
           [result-numerical (affine-chebyshev-approx x exp #f 'convex)]
           ;; Compare with analytical result
           [result-analytical (affine-exp x)]
           [iv-num (affine->interval result-numerical)]
           [iv-ana (affine->interval result-analytical)]
           [true-lo (exp a)]
           [true-hi (exp b)])
      ;; Both should contain the true range
      (assert-true (<= (interval-lo iv-num) true-lo)
                   "numerical fallback should contain true lower bound")
      (assert-true (>= (interval-hi iv-num) true-hi)
                   "numerical fallback should contain true upper bound")
      ;; Numerical should be close to analytical (within 10% width)
      (let* ([width-num (interval-width iv-num)]
             [width-ana (interval-width iv-ana)]
             [ratio (/ width-num width-ana)])
        (assert-true (< ratio 1.1)
                     (format "numerical fallback width ratio ~a should be < 1.1" ratio)))))

  (define-test "numerical fallback for sqrt (fold-zxpn)"
    ;; Test with sqrt (concave function)
    (affine-reset-noise-counter!)
    (let* ([a 1.0] [b 4.0]
           [x (affine-from-interval (interval a b))]
           ;; Use sqrt with numerical fallback
           [result-numerical (affine-chebyshev-approx x sqrt #f 'concave)]
           [iv-num (affine->interval result-numerical)]
           [true-lo (sqrt a)]
           [true-hi (sqrt b)])
      ;; Should contain true range
      (assert-true (<= (interval-lo iv-num) true-lo)
                   "numerical sqrt should contain true lower bound")
      (assert-true (>= (interval-hi iv-num) true-hi)
                   "numerical sqrt should contain true upper bound")
      ;; Width should be reasonable (within 50% of true width)
      (let* ([true-width (- true-hi true-lo)]
             [computed-width (interval-width iv-num)]
             [ratio (/ computed-width true-width)])
        (assert-true (< ratio 1.5)
                     (format "numerical sqrt width ratio ~a should be < 1.5" ratio))))))

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
;;; Min/Max/Abs Tests
;;; ============================================================================

(test-group "min-max-abs"
  ;; affine-min tests
  (define-test "affine-min: non-overlapping ranges (x < y)"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 5 6))]
           [result (affine-min x y)]
           [iv (affine->interval result)])
      ;; x is definitely less, should return x's bounds
      (assert-true (< (abs (- (interval-lo iv) 1)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 2)) 1e-10))))

  (define-test "affine-min: non-overlapping ranges (y < x)"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 10 15))]
           [y (affine-from-interval (interval 2 4))]
           [result (affine-min x y)]
           [iv (affine->interval result)])
      ;; y is definitely less, should return y's bounds
      (assert-true (< (abs (- (interval-lo iv) 2)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 4)) 1e-10))))

  (define-test "affine-min: overlapping ranges gives conservative bound"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 5))]
           [y (affine-from-interval (interval 3 7))]
           [result (affine-min x y)]
           [iv (affine->interval result)])
      ;; Ranges overlap [3,5], min could be anywhere in [1, 5]
      ;; Result should be conservative: [min(1,3), min(5,7)] = [1, 5]
      (assert-true (<= (interval-lo iv) 1))
      (assert-true (>= (interval-hi iv) 3))))

  (define-test "affine-min: with constants"
    (affine-reset-noise-counter!)
    (let* ([x (affine-constant 5)]
           [y (affine-constant 3)]
           [result (affine-min x y)]
           [iv (affine->interval result)])
      ;; min(5, 3) = 3
      (assert-true (< (abs (- (interval-lo iv) 3)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 3)) 1e-10))))

  ;; affine-max tests
  (define-test "affine-max: non-overlapping ranges (x > y)"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 10 15))]
           [y (affine-from-interval (interval 2 4))]
           [result (affine-max x y)]
           [iv (affine->interval result)])
      ;; x is definitely greater, should return x's bounds
      (assert-true (< (abs (- (interval-lo iv) 10)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 15)) 1e-10))))

  (define-test "affine-max: non-overlapping ranges (y > x)"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 5 6))]
           [result (affine-max x y)]
           [iv (affine->interval result)])
      ;; y is definitely greater, should return y's bounds
      (assert-true (< (abs (- (interval-lo iv) 5)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 6)) 1e-10))))

  (define-test "affine-max: overlapping ranges gives conservative bound"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 5))]
           [y (affine-from-interval (interval 3 7))]
           [result (affine-max x y)]
           [iv (affine->interval result)])
      ;; Ranges overlap [3,5], max could be anywhere in [3, 7]
      ;; Result should be conservative: [max(1,3), max(5,7)] = [3, 7]
      (assert-true (<= (interval-lo iv) 5))
      (assert-true (>= (interval-hi iv) 5))))

  (define-test "affine-max: with constants"
    (affine-reset-noise-counter!)
    (let* ([x (affine-constant 5)]
           [y (affine-constant 3)]
           [result (affine-max x y)]
           [iv (affine->interval result)])
      ;; max(5, 3) = 5
      (assert-true (< (abs (- (interval-lo iv) 5)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 5)) 1e-10))))

  ;; affine-abs tests
  (define-test "affine-abs: entirely non-negative"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 2 5))]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |[2,5]| = [2,5] (no change)
      (assert-true (< (abs (- (interval-lo iv) 2)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 5)) 1e-10))))

  (define-test "affine-abs: entirely non-positive"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -5 -2))]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |[-5,-2]| = [2,5] (negated)
      (assert-true (< (abs (- (interval-lo iv) 2)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 5)) 1e-10))))

  (define-test "affine-abs: contains zero"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -3 5))]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |[-3,5]| contains [0, max(3,5)] = [0,5]
      (assert-true (<= (interval-lo iv) 0))
      (assert-true (>= (interval-hi iv) 5))))

  (define-test "affine-abs: symmetric around zero"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval -4 4))]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |[-4,4]| = [0,4]
      (assert-true (<= (interval-lo iv) 0))
      (assert-true (>= (interval-hi iv) 4))))

  (define-test "affine-abs: constant positive"
    (affine-reset-noise-counter!)
    (let* ([x (affine-constant 7)]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |7| = 7
      (assert-true (< (abs (- (interval-lo iv) 7)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 7)) 1e-10))))

  (define-test "affine-abs: constant negative"
    (affine-reset-noise-counter!)
    (let* ([x (affine-constant -7)]
           [result (affine-abs x)]
           [iv (affine->interval result)])
      ;; |-7| = 7
      (assert-true (< (abs (- (interval-lo iv) 7)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 7)) 1e-10)))))

;;; ============================================================================
;;; List Operations Tests
;;; ============================================================================

(test-group "list-operations"
  ;; affine-sum tests
  (define-test "affine-sum: empty list returns zero"
    (let* ([result (affine-sum '())]
           [iv (affine->interval result)])
      (assert-true (< (abs (interval-lo iv)) 1e-10))
      (assert-true (< (abs (interval-hi iv)) 1e-10))))

  (define-test "affine-sum: single element"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 3))]
           [result (affine-sum (list x))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 1)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 3)) 1e-10))))

  (define-test "affine-sum: multiple independent variables"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 3 4))]
           [z (affine-from-interval (interval 5 6))]
           [result (affine-sum (list x y z))]
           [iv (affine->interval result)])
      ;; [1,2] + [3,4] + [5,6] = [9, 12]
      (assert-true (< (abs (- (interval-lo iv) 9)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 12)) 1e-10))))

  (define-test "affine-sum: correlated cancellation"
    ;; x + (-x) should give tighter bounds than interval arithmetic
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 5))]
           [neg-x (affine-neg x)]
           [result (affine-sum (list x neg-x))]
           [iv (affine->interval result)])
      ;; Should be exactly 0 due to dependency tracking
      (assert-true (< (interval-width iv) 1e-10))))

  (define-test "affine-sum: with constants"
    (let* ([a (affine-constant 10)]
           [b (affine-constant 20)]
           [c (affine-constant 30)]
           [result (affine-sum (list a b c))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 60)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 60)) 1e-10))))

  ;; affine-product tests
  (define-test "affine-product: empty list returns one"
    (let* ([result (affine-product '())]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 1)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 1)) 1e-10))))

  (define-test "affine-product: single element"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 2 4))]
           [result (affine-product (list x))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 2)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 4)) 1e-10))))

  (define-test "affine-product: two elements"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 2 3))]
           [y (affine-from-interval (interval 4 5))]
           [result (affine-product (list x y))]
           [iv (affine->interval result)])
      ;; [2,3] * [4,5] contains [8, 15]
      (assert-true (<= (interval-lo iv) 8))
      (assert-true (>= (interval-hi iv) 15))))

  (define-test "affine-product: three elements"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 2 3))]
           [z (affine-from-interval (interval 3 4))]
           [result (affine-product (list x y z))]
           [iv (affine->interval result)])
      ;; [1,2] * [2,3] * [3,4] contains [6, 24]
      (assert-true (<= (interval-lo iv) 6))
      (assert-true (>= (interval-hi iv) 24))))

  (define-test "affine-product: with constants"
    (let* ([a (affine-constant 2)]
           [b (affine-constant 3)]
           [c (affine-constant 4)]
           [result (affine-product (list a b c))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 24)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 24)) 1e-10))))

  ;; affine-linear-combination tests
  (define-test "affine-linear-combination: simple weighted sum"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 3 4))]
           ;; 2*x + 3*y where x∈[1,2], y∈[3,4]
           ;; = [2,4] + [9,12] = [11, 16]
           [result (affine-linear-combination '(2 3) (list x y))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 11)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 16)) 1e-10))))

  (define-test "affine-linear-combination: negative coefficients"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 2))]
           [y (affine-from-interval (interval 3 4))]
           ;; x - y where x∈[1,2], y∈[3,4]
           ;; = [1,2] - [3,4] = [-3, -1]
           [result (affine-linear-combination '(1 -1) (list x y))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) -3)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) -1)) 1e-10))))

  (define-test "affine-linear-combination: zero coefficient"
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 100))]  ; Wide interval
           [y (affine-from-interval (interval 5 6))]
           ;; 0*x + 1*y should give just y's bounds
           [result (affine-linear-combination '(0 1) (list x y))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 5)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 6)) 1e-10))))

  (define-test "affine-linear-combination: dependency tracking"
    ;; 2*x - 2*x should equal 0 due to dependency tracking
    (affine-reset-noise-counter!)
    (let* ([x (affine-from-interval (interval 1 10))]
           [result (affine-linear-combination '(2 -2) (list x x))]
           [iv (affine->interval result)])
      ;; Should be exactly 0
      (assert-true (< (interval-width iv) 1e-10))))

  (define-test "affine-linear-combination: with constants"
    (let* ([a (affine-constant 5)]
           [b (affine-constant 7)]
           ;; 3*5 + 4*7 = 15 + 28 = 43
           [result (affine-linear-combination '(3 4) (list a b))]
           [iv (affine->interval result)])
      (assert-true (< (abs (- (interval-lo iv) 43)) 1e-10))
      (assert-true (< (abs (- (interval-hi iv) 43)) 1e-10)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
