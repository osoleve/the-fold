(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/forms.ss")

(printf "\n=== Differential Forms Tests ===\n\n")

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

;; Use Cartesian chart for R^3
(define cart3 (make-identity-chart 'cartesian 3))
(define p0 (vector 1 2 3))  ; test point

;; Use Cartesian chart for R^2
(define cart2 (make-identity-chart 'cartesian 2))
(define p2 (vector 1 1))

;;; ============================================================================
;;; Multi-Index Tests
;;; ============================================================================

(test-group "multi-index"
  (define-test "multi-index? recognizes valid indices"
    (assert-true (multi-index? '()))
    (assert-true (multi-index? '(0)))
    (assert-true (multi-index? '(0 1)))
    (assert-true (multi-index? '(0 1 2)))
    (assert-true (multi-index? '(0 2 4))))

  (define-test "multi-index? rejects invalid indices"
    (assert-false (multi-index? '(1 0)))    ; not increasing
    (assert-false (multi-index? '(1 1)))    ; not strictly increasing
    (assert-false (multi-index? "abc")))    ; not a list

  (define-test "generate-multi-indices produces correct count"
    ;; C(3,0) = 1, C(3,1) = 3, C(3,2) = 3, C(3,3) = 1
    (assert-equal 1 (length (generate-multi-indices 3 0)))
    (assert-equal 3 (length (generate-multi-indices 3 1)))
    (assert-equal 3 (length (generate-multi-indices 3 2)))
    (assert-equal 1 (length (generate-multi-indices 3 3)))
    (assert-equal 0 (length (generate-multi-indices 3 4))))

  (define-test "generate-multi-indices content is correct"
    (assert-equal '(()) (generate-multi-indices 3 0))
    (assert-equal '((0) (1) (2)) (generate-multi-indices 3 1))
    (assert-equal '((0 1) (0 2) (1 2)) (generate-multi-indices 3 2))
    (assert-equal '((0 1 2)) (generate-multi-indices 3 3)))

  (define-test "binomial coefficients"
    (assert-equal 1 (binomial 5 0))
    (assert-equal 5 (binomial 5 1))
    (assert-equal 10 (binomial 5 2))
    (assert-equal 10 (binomial 5 3))
    (assert-equal 5 (binomial 5 4))
    (assert-equal 1 (binomial 5 5))
    (assert-equal 0 (binomial 5 6))))

;;; ============================================================================
;;; Permutation Sign Tests
;;; ============================================================================

(test-group "permutation-sign"
  (define-test "identity permutation has sign +1"
    (assert-equal 1 (permutation-sign '(0 1 2))))

  (define-test "single transposition has sign -1"
    (assert-equal -1 (permutation-sign '(1 0 2)))
    (assert-equal -1 (permutation-sign '(0 2 1)))
    (assert-equal -1 (permutation-sign '(2 1 0))))

  (define-test "double transposition has sign +1"
    (assert-equal 1 (permutation-sign '(1 2 0)))   ; (0 1 2) -> (1 0 2) -> (1 2 0)
    (assert-equal 1 (permutation-sign '(2 0 1)))))

;;; ============================================================================
;;; k-Form Construction Tests
;;; ============================================================================

(test-group "k-form-construction"
  (define-test "0-form creation"
    (let ([f0 (make-k-form p0 cart3 0 (vector 5.0))])
      (assert-true (k-form? f0))
      (assert-equal 0 (k-form-degree f0))
      (assert-equal 1 (vector-length (k-form-components f0)))))

  (define-test "1-form creation (3 components in R^3)"
    (let ([f1 (make-k-form p0 cart3 1 (vector 1 2 3))])
      (assert-true (k-form? f1))
      (assert-equal 1 (k-form-degree f1))
      (assert-equal 3 (vector-length (k-form-components f1)))))

  (define-test "2-form creation (3 components in R^3)"
    (let ([f2 (make-k-form p0 cart3 2 (vector 1 2 3))])
      (assert-true (k-form? f2))
      (assert-equal 2 (k-form-degree f2))
      (assert-equal 3 (vector-length (k-form-components f2)))))

  (define-test "3-form creation (1 component in R^3)"
    (let ([f3 (make-k-form p0 cart3 3 (vector 1))])
      (assert-true (k-form? f3))
      (assert-equal 3 (k-form-degree f3))
      (assert-equal 1 (vector-length (k-form-components f3)))))

  (define-test "basis 1-forms"
    (let ([dx0 (k-form-basis p0 cart3 '(0))]
          [dx1 (k-form-basis p0 cart3 '(1))]
          [dx2 (k-form-basis p0 cart3 '(2))])
      (assert-equal (vector 1 0 0) (k-form-components dx0))
      (assert-equal (vector 0 1 0) (k-form-components dx1))
      (assert-equal (vector 0 0 1) (k-form-components dx2))))

  (define-test "basis 2-forms"
    (let ([dx01 (k-form-basis p0 cart3 '(0 1))]
          [dx02 (k-form-basis p0 cart3 '(0 2))]
          [dx12 (k-form-basis p0 cart3 '(1 2))])
      ;; Order: (0 1), (0 2), (1 2)
      (assert-equal (vector 1 0 0) (k-form-components dx01))
      (assert-equal (vector 0 1 0) (k-form-components dx02))
      (assert-equal (vector 0 0 1) (k-form-components dx12)))))

;;; ============================================================================
;;; k-Form Arithmetic Tests
;;; ============================================================================

(test-group "k-form-arithmetic"
  (define-test "1-form addition"
    (let* ([a (make-k-form p0 cart3 1 (vector 1 2 3))]
           [b (make-k-form p0 cart3 1 (vector 4 5 6))]
           [c (k-form-add a b)])
      (assert-equal (vector 5 7 9) (k-form-components c))))

  (define-test "1-form scaling"
    (let* ([a (make-k-form p0 cart3 1 (vector 1 2 3))]
           [b (k-form-scale 2 a)])
      (assert-equal (vector 2 4 6) (k-form-components b))))

  (define-test "degree mismatch error"
    (let ([f1 (make-k-form p0 cart3 1 (vector 1 2 3))]
          [f2 (make-k-form p0 cart3 2 (vector 1 2 3))])
      (let ([result (k-form-add f1 f2)])
        (assert-true (and (pair? result) (eq? (car result) 'error)))))))

;;; ============================================================================
;;; Wedge Product Tests
;;; ============================================================================

(test-group "wedge-product"
  (define-test "dx^0 ∧ dx^1 = dx^{01}"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [dx1 (k-form-basis p0 cart3 '(1))]
           [dx01 (wedge dx0 dx1)])
      (assert-equal 2 (k-form-degree dx01))
      ;; Should be (1, 0, 0) for basis (01), (02), (12)
      (assert-equal (vector 1 0 0) (k-form-components dx01))))

  (define-test "dx^1 ∧ dx^0 = -dx^{01}"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [dx1 (k-form-basis p0 cart3 '(1))]
           [dx10 (wedge dx1 dx0)])
      (assert-equal 2 (k-form-degree dx10))
      ;; Should be (-1, 0, 0) - antisymmetric!
      (assert-equal (vector -1 0 0) (k-form-components dx10))))

  (define-test "dx^0 ∧ dx^0 = 0"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [result (wedge dx0 dx0)])
      (assert-equal (vector 0 0 0) (k-form-components result))))

  (define-test "triple wedge product"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [dx1 (k-form-basis p0 cart3 '(1))]
           [dx2 (k-form-basis p0 cart3 '(2))]
           [vol (wedge* dx0 dx1 dx2)])
      (assert-equal 3 (k-form-degree vol))
      (assert-equal (vector 1) (k-form-components vol))))

  (define-test "wedge of 1-form with 2-form"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [dx12 (k-form-basis p0 cart3 '(1 2))]
           [result (wedge dx0 dx12)])
      (assert-equal 3 (k-form-degree result))
      ;; dx^0 ∧ dx^{12} = dx^{012} with positive sign
      (assert-equal (vector 1) (k-form-components result))))

  (define-test "2-form ∧ 2-form = 0 in R^3"
    (let* ([dx01 (k-form-basis p0 cart3 '(0 1))]
           [dx12 (k-form-basis p0 cart3 '(1 2))]
           [result (wedge dx01 dx12)])
      ;; 4-forms don't exist in R^3
      (assert-equal 4 (k-form-degree result))
      (assert-equal 0 (vector-length (k-form-components result))))))

;;; ============================================================================
;;; Interior Product Tests
;;; ============================================================================

(test-group "interior-product"
  (define-test "interior product of 1-form with vector"
    (let* ([dx0 (k-form-basis p0 cart3 '(0))]
           [v (make-tangent-vector p0 cart3 (vector 3 4 5))]
           [result (interior-product v dx0)])
      ;; ι_v(dx^0) = v^0 (a 0-form)
      (assert-equal 0 (k-form-degree result))
      (assert-equal 3 (vector-ref (k-form-components result) 0))))

  (define-test "interior product of 2-form"
    (let* ([dx01 (k-form-basis p0 cart3 '(0 1))]
           [v (make-tangent-vector p0 cart3 (vector 3 4 5))]
           [result (interior-product v dx01)])
      ;; ι_v(dx^0 ∧ dx^1) = v^0 dx^1 - v^1 dx^0
      ;; = 3 dx^1 - 4 dx^0 = (-4, 3, 0)
      (assert-equal 1 (k-form-degree result))
      (assert-equal -4 (vector-ref (k-form-components result) 0))
      (assert-equal 3 (vector-ref (k-form-components result) 1))
      (assert-equal 0 (vector-ref (k-form-components result) 2))))

  (define-test "interior product of 0-form is zero"
    (let* ([f0 (make-k-form p0 cart3 0 (vector 5))]
           [v (make-tangent-vector p0 cart3 (vector 1 2 3))]
           [result (interior-product v f0)])
      ;; Interior product lowers degree; 0-form becomes trivial zero
      (assert-equal 0 (k-form-degree result)))))

;;; ============================================================================
;;; Exterior Derivative Tests
;;; ============================================================================

(test-group "exterior-derivative"
  (define-test "d of scalar function (gradient)"
    ;; f(x,y) = x^2 + y^2 at point (1,1)
    ;; df = 2x dx + 2y dy = 2 dx + 2 dy
    (let* ([f (lambda (p) (+ (* (vector-ref p 0) (vector-ref p 0))
                             (* (vector-ref p 1) (vector-ref p 1))))]
           [df (d-scalar f p2 cart2)]
           [comps (k-form-components df)])
      (assert-equal 1 (k-form-degree df))
      ;; Should be approximately (2, 2)
      (assert-true (< (abs (- (vector-ref comps 0) 2)) 1e-5))
      (assert-true (< (abs (- (vector-ref comps 1) 2)) 1e-5))))

  (define-test "exterior derivative raises degree"
    ;; Create a constant 1-form field
    (let* ([omega-field (lambda (p) (make-k-form p cart2 1 (vector 1 0)))]
           [d-omega (exterior-derivative omega-field p2 cart2)])
      ;; d of constant 1-form should be 0
      (assert-equal 2 (k-form-degree d-omega))
      ;; Component should be ~0
      (assert-true (< (abs (vector-ref (k-form-components d-omega) 0)) 1e-5))))

  (define-test "d(d(f)) = 0 (exactness)"
    ;; For f(x,y) = x*y, df = y dx + x dy
    ;; d(df) should be 0
    (let* ([f (lambda (p) (* (vector-ref p 0) (vector-ref p 1)))]
           [df-field (lambda (p) (d-scalar f p cart2))]
           [ddf (exterior-derivative df-field p2 cart2)])
      (assert-equal 2 (k-form-degree ddf))
      ;; The (0,1) component should be ~0
      (assert-true (< (abs (vector-ref (k-form-components ddf) 0)) 1e-4)))))

;;; ============================================================================
;;; k-Form Application Tests
;;; ============================================================================

(test-group "k-form-application"
  (define-test "0-form application (scalar)"
    (let* ([f0 (make-k-form p0 cart3 0 (vector 42))]
           [result (k-form-apply f0 '())])
      (assert-equal 42 result)))

  (define-test "1-form applied to vector"
    ;; ω = dx^0 + 2 dx^1 + 3 dx^2
    ;; v = (4, 5, 6)
    ;; ω(v) = 4 + 10 + 18 = 32
    (let* ([omega (make-k-form p0 cart3 1 (vector 1 2 3))]
           [v (make-tangent-vector p0 cart3 (vector 4 5 6))]
           [result (k-form-apply omega (list v))])
      (assert-equal 32 result)))

  (define-test "2-form applied to two vectors (determinant)"
    ;; dx^0 ∧ dx^1 applied to v1=(1,0,0), v2=(0,1,0) gives det = 1
    (let* ([dx01 (k-form-basis p0 cart3 '(0 1))]
           [v1 (make-tangent-vector p0 cart3 (vector 1 0 0))]
           [v2 (make-tangent-vector p0 cart3 (vector 0 1 0))]
           [result (k-form-apply dx01 (list v1 v2))])
      (assert-equal 1 result)))

  (define-test "2-form antisymmetry in application"
    ;; ω(v1, v2) = -ω(v2, v1)
    (let* ([dx01 (k-form-basis p0 cart3 '(0 1))]
           [v1 (make-tangent-vector p0 cart3 (vector 1 2 0))]
           [v2 (make-tangent-vector p0 cart3 (vector 3 4 0))]
           [result1 (k-form-apply dx01 (list v1 v2))]
           [result2 (k-form-apply dx01 (list v2 v1))])
      (assert-equal (- result1) result2))))

;;; ============================================================================
;;; Hodge Star Tests
;;; ============================================================================

(test-group "hodge-star"
  (define-test "★(dx) = dy in R^2"
    (let* ([dx (k-form-basis p2 cart2 '(0))]
           [star-dx (hodge-star-euclidean dx)])
      (assert-equal 1 (k-form-degree star-dx))
      ;; ★(dx) should be dy (index 1)
      (assert-equal (vector 0 1) (k-form-components star-dx))))

  (define-test "★(dy) = -dx in R^2"
    (let* ([dy (k-form-basis p2 cart2 '(1))]
           [star-dy (hodge-star-euclidean dy)])
      (assert-equal 1 (k-form-degree star-dy))
      ;; ★(dy) should be -dx
      (assert-equal (vector -1 0) (k-form-components star-dy))))

  (define-test "★★ = (-1)^{k(n-k)} identity for 1-forms in R^2"
    ;; For k=1, n=2: (-1)^{1*1} = -1
    ;; So ★★ω = -ω for 1-forms in R^2
    (let* ([omega (make-k-form p2 cart2 1 (vector 3 4))]
           [star-omega (hodge-star-euclidean omega)]
           [star-star-omega (hodge-star-euclidean star-omega)])
      (assert-equal (vector -3 -4) (k-form-components star-star-omega))))

  (define-test "★(1) = volume form"
    (let* ([one (make-k-form p0 cart3 0 (vector 1))]
           [vol (hodge-star-euclidean one)])
      (assert-equal 3 (k-form-degree vol))
      (assert-equal (vector 1) (k-form-components vol)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test-group "integration"
  (define-test "line integral of exact 1-form"
    ;; ∫_γ df = f(end) - f(start) for exact forms
    ;; f(x,y) = x, df = dx
    ;; γ(t) = (t, 0) for t ∈ [0, 1]
    ;; ∫ dx = 1 - 0 = 1
    (let* ([dx-field (lambda (p) (make-k-form p cart2 1 (vector 1 0)))]
           [gamma (lambda (t) (vector t 0))]
           [integral (integrate-1-form-line dx-field gamma 0 1 100)])
      (assert-true (< (abs (- integral 1)) 0.01))))

  (define-test "line integral around unit circle"
    ;; ∫ (-y dx + x dy) around unit circle = 2π (area formula)
    ;; γ(t) = (cos t, sin t) for t ∈ [0, 2π]
    (let* ([omega-field (lambda (p)
                          (let ([x (vector-ref p 0)]
                                [y (vector-ref p 1)])
                            (make-k-form p cart2 1 (vector (- y) x))))]
           [gamma (lambda (t) (vector (cos t) (sin t)))]
           [pi 3.141592653589793]
           [integral (integrate-1-form-line omega-field gamma 0 (* 2 pi) 100)])
      ;; Should be 2π ≈ 6.28
      (assert-true (< (abs (- integral (* 2 pi))) 0.1)))))

;;; ============================================================================
;;; Cotangent Conversion Tests
;;; ============================================================================

(test-group "cotangent-conversion"
  (define-test "cotangent to 1-form roundtrip"
    (let* ([cv (make-cotangent-vector p0 cart3 (vector 1 2 3))]
           [kf (cotangent->1-form cv)]
           [cv2 (1-form->cotangent kf)])
      (assert-equal (cotangent-vector-components cv)
                    (cotangent-vector-components cv2))))

  (define-test "1-form to cotangent requires degree 1"
    (let* ([kf2 (make-k-form p0 cart3 2 (vector 1 2 3))]
           [result (1-form->cotangent kf2)])
      (assert-true (and (pair? result) (eq? (car result) 'error))))))

;;; ============================================================================
;;; Volume Form Tests
;;; ============================================================================

(test-group "volume-form"
  (define-test "volume form has correct degree"
    (let ([vol (volume-form p0 cart3)])
      (assert-equal 3 (k-form-degree vol))))

  (define-test "volume form is the top-degree basis"
    (let* ([vol (volume-form p0 cart3)]
           [comps (k-form-components vol)])
      (assert-equal 1 (vector-length comps))
      (assert-equal 1 (vector-ref comps 0)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
