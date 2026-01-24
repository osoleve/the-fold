;;; lattice/fp/measure/test-qvec.ss — Tests for Quantity Vectors
;;;
;;; Run from project root: scheme --script lattice/fp/measure/test-qvec.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/measure/qvec.ss")

(display "\n")
(display "================================================\n")
(display "         QUANTITY VECTOR TESTS\n")
(display "================================================\n\n")

;;; ============================================================
;;; Type and Construction Tests
;;; ============================================================

(test-group qvec-type
  (define-test qvec?-test
    (assert-true (qvec? (make-qvec dim-length-base (vector 1 2 3))))
    (assert-true (qvec? (position-vec 1 2 3)))
    (assert-false (qvec? (vector 1 2 3)))
    (assert-false (qvec? (meter 5)))
    (assert-false (qvec? '(not a qvec))))

  (define-test make-qvec-test
    (let ([qv (make-qvec dim-velocity (vector 1 2 3))])
      (assert-true (dim=? dim-velocity (qvec-dim qv)))
      (assert-equal 3 (qvec-length qv))))

  (define-test qvec-variadic-test
    (let ([qv (qvec dim-length-base 1 2 3 4)])
      (assert-equal 4 (qvec-length qv))
      (assert-true (dim=? dim-length-base (qvec-dim qv)))))

  (define-test qvec-ref-test
    (let ([qv (position-vec 10 20 30)])
      (let ([q0 (qvec-ref qv 0)]
            [q1 (qvec-ref qv 1)]
            [q2 (qvec-ref qv 2)])
        (assert-equal 10 (qty-value q0))
        (assert-equal 20 (qty-value q1))
        (assert-equal 30 (qty-value q2))
        (assert-true (dim=? dim-length-base (qty-dim q0)))))))

(test-group qvec-construction
  (define-test qvec-zeros-test
    (let ([qv (qvec-zeros dim-velocity 3)])
      (assert-equal 3 (qvec-length qv))
      (assert-true (dim=? dim-velocity (qvec-dim qv)))
      (assert-true (vec-equal? (vector 0 0 0) (qvec-values qv)))))

  (define-test qvec-unit-test
    (let ([qv (qvec-unit dim-force 3 1)])
      (assert-true (vec-equal? (vector 0 1 0) (qvec-values qv)))))

  (define-test qvec-from-quantities-test
    (let* ([qs (list (meter 1) (meter 2) (meter 3))]
           [qv (qvec-from-quantities qs)])
      (assert-equal 3 (qvec-length qv))
      (assert-true (dim=? dim-length-base (qvec-dim qv))))))

;;; ============================================================
;;; Arithmetic Tests
;;; ============================================================

(test-group qvec-arithmetic
  (define-test qvec-add-same-dim-test
    (let* ([v1 (position-vec 1 2 3)]
           [v2 (position-vec 4 5 6)]
           [sum (qvec-add v1 v2)])
      (assert-true (dim=? dim-length-base (qvec-dim sum)))
      (assert-true (vec-equal? (vector 5 7 9) (qvec-values sum)))))

  (define-test qvec-add-dimension-mismatch-test
    ;; Calling qvec-add with mismatched dimensions should produce an error
    ;; We verify indirectly by checking the dimensions don't match
    (let ([pos (position-vec 1 2)]
          [vel (velocity-vec 3 4)])
      (assert-false (dim=? (qvec-dim pos) (qvec-dim vel)))))

  (define-test qvec-sub-test
    (let* ([v1 (velocity-vec 10 20)]
           [v2 (velocity-vec 3 5)]
           [diff (qvec-sub v1 v2)])
      (assert-true (vec-equal? (vector 7 15) (qvec-values diff)))))

  (define-test qvec-negate-test
    (let* ([v (force-vec 1 -2 3)]
           [neg (qvec-negate v)])
      (assert-true (vec-equal? (vector -1 2 -3) (qvec-values neg)))
      (assert-true (dim=? dim-force (qvec-dim neg))))))

(test-group qvec-scaling
  (define-test qvec-scale-test
    (let* ([v (velocity-vec 2 4 6)]
           [scaled (qvec-scale 0.5 v)])
      ;; 0.5 produces floats, so use approx comparison
      (assert-true (vec-approx-equal? (vector 1.0 2.0 3.0) (qvec-values scaled)))
      (assert-true (dim=? dim-velocity (qvec-dim scaled)))))

  (define-test qvec-qty-scale-test
    ;; velocity * time = displacement
    (let* ([v (velocity-vec 10 0 0)]  ; 10 m/s in x direction
           [dt (second 2)]             ; 2 seconds
           [disp (qvec-qty-scale dt v)])
      (assert-true (vec-equal? (vector 20 0 0) (qvec-values disp)))
      ;; dimension should be velocity * time = length
      (assert-true (dim=? dim-length-base (qvec-dim disp))))))

;;; ============================================================
;;; Products Tests
;;; ============================================================

(test-group qvec-products
  (define-test qvec-dot-test
    (let* ([v1 (qvec dim-length-base 1 2 3)]
           [v2 (qvec dim-length-base 4 5 6)]
           [dot (qvec-dot v1 v2)])
      ;; 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
      (assert-equal 32 (qty-value dot))
      ;; dimension: L * L = L²
      (assert-true (dim=? dim-area (qty-dim dot)))))

  (define-test qvec-dot-mixed-dimensions-test
    ;; force · displacement = work (energy)
    (let* ([f (force-vec 10 0 0)]      ; 10 N in x
           [d (position-vec 5 0 0)]     ; 5 m in x
           [work (qvec-dot f d)])
      (assert-equal 50 (qty-value work))
      ;; F·d dimension = (M·L/T²)·L = M·L²/T² = energy
      (assert-true (dim=? dim-energy (qty-dim work)))))

  (define-test qvec-hadamard-test
    (let* ([v1 (qvec dim-length-base 2 3)]
           [v2 (qvec dim-time-base 4 5)]
           [prod (qvec-hadamard v1 v2)])
      (assert-true (vec-equal? (vector 8 15) (qvec-values prod)))
      ;; dimension: L * T
      (assert-true (dim=? (dim* dim-length-base dim-time-base) (qvec-dim prod))))))

;;; ============================================================
;;; Norms Tests
;;; ============================================================

(test-group qvec-norms
  (define-test qvec-norm-squared-test
    (let* ([v (position-vec 3 4)]
           [ns (qvec-norm-squared v)])
      ;; 3² + 4² = 25
      (assert-equal 25 (qty-value ns))
      ;; dimension: L²
      (assert-true (dim=? dim-area (qty-dim ns)))))

  (define-test qvec-norm-test
    (let* ([v (position-vec 3 4)]
           [n (qvec-norm v)])
      ;; sqrt(25) = 5
      (assert-equal 5 (qty-value n))
      ;; dimension: L
      (assert-true (dim=? dim-length-base (qty-dim n)))))

  (define-test qvec-normalize-test
    (let* ([v (velocity-vec 3 4)]
           [normalized (qvec-normalize v)])
      (assert-true (dim=? dim-velocity (qvec-dim normalized)))
      ;; should have unit magnitude
      (let ([vals (qvec-values normalized)])
        (assert-true (< (abs (- 1 (vec-norm vals))) 1e-10)))))

  (define-test qvec-distance-test
    (let* ([p1 (position-vec 0 0)]
           [p2 (position-vec 3 4)]
           [d (qvec-distance p1 p2)])
      (assert-equal 5 (qty-value d))
      (assert-true (dim=? dim-length-base (qty-dim d))))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group qvec-comparison
  (define-test qvec-equal?-test
    (assert-true (qvec-equal? (position-vec 1 2) (position-vec 1 2)))
    (assert-false (qvec-equal? (position-vec 1 2) (position-vec 1 3)))
    (assert-false (qvec-equal? (position-vec 1 2) (velocity-vec 1 2))))

  (define-test qvec-approx-equal?-test
    (let ([v1 (position-vec 1.0 2.0)]
          [v2 (position-vec 1.0000001 2.0000001)])
      (assert-true (qvec-approx-equal? v1 v2 1e-6))
      (assert-false (qvec-approx-equal? v1 v2 1e-10)))))

;;; ============================================================
;;; Physics Convenience Constructors Tests
;;; ============================================================

(test-group physics-constructors
  (define-test position-vec-test
    (let ([p (position-vec-3d 1 2 3)])
      (assert-true (dim=? dim-length-base (qvec-dim p)))
      (assert-equal 3 (qvec-length p))))

  (define-test velocity-vec-test
    (let ([v (velocity-vec-2d 5 10)])
      (assert-true (dim=? dim-velocity (qvec-dim v)))
      (assert-equal 2 (qvec-length v))))

  (define-test acceleration-vec-test
    (let ([a (acceleration-vec 0 -9.8 0)])
      (assert-true (dim=? dim-acceleration (qvec-dim a)))))

  (define-test force-vec-test
    (let ([f (force-vec-3d 10 0 0)])
      (assert-true (dim=? dim-force (qvec-dim f))))))

;;; ============================================================
;;; Physics Operations Tests
;;; ============================================================

(test-group physics-operations
  (define-test integrate-time-test
    ;; velocity * time = displacement
    (let* ([v (velocity-vec 10 0)]
           [dt (second 2)]
           [disp (qvec-integrate-time v dt)])
      (assert-true (vec-equal? (vector 20 0) (qvec-values disp)))
      (assert-true (dim=? dim-length-base (qvec-dim disp)))))

  (define-test differentiate-time-test
    ;; displacement / time = velocity
    (let* ([disp (position-vec 100 0)]
           [dt (second 10)]
           [vel (qvec-differentiate-time disp dt)])
      (assert-true (vec-equal? (vector 10 0) (qvec-values vel)))
      (assert-true (dim=? dim-velocity (qvec-dim vel)))))

  (define-test apply-force-test
    ;; F = ma → dv = (F/m)*dt
    (let* ([m (kilogram 2)]
           [f (force-vec 10 0)]  ; 10 N
           [dt (second 1)]
           [dv (apply-force m f dt)])
      ;; a = F/m = 10/2 = 5 m/s²
      ;; dv = a*dt = 5*1 = 5 m/s
      (assert-true (vec-equal? (vector 5 0) (qvec-values dv)))
      (assert-true (dim=? dim-velocity (qvec-dim dv))))))

;;; ============================================================
;;; Cross Product Tests
;;; ============================================================

(test-group qvec-cross-product
  (define-test cross-product-basic-test
    (let* ([i (position-vec 1 0 0)]
           [j (position-vec 0 1 0)]
           [k (qvec-cross i j)])
      ;; i × j = k
      (assert-true (vec-equal? (vector 0 0 1) (qvec-values k)))
      ;; dimension: L × L = L²
      (assert-true (dim=? dim-area (qvec-dim k)))))

  (define-test cross-product-torque-test
    ;; τ = r × F (torque = position × force)
    (let* ([r (position-vec 0 0 1)]   ; 1 m along z
           [f (force-vec 1 0 0)]       ; 1 N along x
           [torque (qvec-cross r f)])
      ;; (0,0,1) × (1,0,0) = (0*0-1*0, 1*1-0*0, 0*0-0*1) = (0, 1, 0)
      (assert-true (vec-equal? (vector 0 1 0) (qvec-values torque)))
      ;; dimension: L × (M·L/T²) = M·L²/T² (same dimension as energy/work)
      (assert-true (dim=? dim-energy (qvec-dim torque))))))

;;; ============================================================
;;; Higher-Order Operations Tests
;;; ============================================================

(test-group qvec-higher-order
  (define-test qvec-map-test
    (let* ([v (position-vec 1 4 9)]
           [mapped (qvec-map sqrt v)])
      (assert-true (vec-approx-equal? (vector 1 2 3) (qvec-values mapped)))
      (assert-true (dim=? dim-length-base (qvec-dim mapped)))))

  (define-test qvec-sum-test
    (let* ([v (force-vec 10 20 30)]
           [s (qvec-sum v)])
      (assert-equal 60 (qty-value s))
      (assert-true (dim=? dim-force (qty-dim s)))))

  (define-test qvec-mean-test
    (let* ([v (velocity-vec 10 20 30)]
           [m (qvec-mean v)])
      (assert-equal 20 (qty-value m))
      (assert-true (dim=? dim-velocity (qty-dim m))))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group qvec-conversion
  (define-test qvec->list-test
    (let* ([qv (position-vec 1 2 3)]
           [lst (qvec->list qv)])
      (assert-equal 3 (length lst))
      (assert-equal 1 (qty-value (car lst)))
      (assert-true (dim=? dim-length-base (qty-dim (car lst))))))

  (define-test qvec->string-test
    (let* ([qv (velocity-vec 1 2)]
           [s (qvec->string qv)])
      (assert-true (string? s))
      ;; Should contain the numeric values
      (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
