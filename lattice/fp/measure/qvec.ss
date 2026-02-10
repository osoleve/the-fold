;;; lattice/fp/measure/qvec.ss — Unit-Aware Vectors
;;; @module qvec
;;; @requires prelude units vec

(require 'prelude)
(require 'units)
(require 'vec)

(doc 'module 'qvec)
(doc 'description "Unit-aware vectors for physics computations. All elements share a common dimension.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A qvec is a vector where all elements have the same physical dimension.
The dimension is stored once (not per-element), making operations efficient.
This enables type-safe physics: you can't add a position to a velocity.")

;;; ============================================================
;;; Quantity Vector Type
;;; ============================================================

(doc 'section 'qvec-type)

(doc 'note "A qvec is represented as (list 'qvec dimension numeric-vector).
All elements share the same dimension, which is stored once.")

(define (make-qvec dim values)
  (doc 'type '(-> Dimension (Vec Number) QVec))
  (doc 'description "Construct a quantity vector from a dimension and numeric vector.")
  (list 'qvec dim values))

(define (qvec? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a quantity vector.")
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'qvec)
       (dim? (cadr x))
       (vector? (caddr x))))

(define (qvec-dim qv)
  (doc 'type '(-> QVec Dimension))
  (doc 'description "Extract the dimension from a quantity vector.")
  (cadr qv))

(define (qvec-values qv)
  (doc 'type '(-> QVec (Vec Number)))
  (doc 'description "Extract the numeric values from a quantity vector.")
  (caddr qv))

(define (qvec-length qv)
  (doc 'type '(-> QVec Nat))
  (doc 'description "Get the number of elements in a quantity vector.")
  (vec-length (qvec-values qv)))

(define (qvec-ref qv i)
  (doc 'type '(-> QVec Nat Quantity))
  (doc 'description "Get element at index as a Quantity.")
  (let ([v (vec-ref (qvec-values qv) i)])
    (if (and (pair? v) (eq? (car v) 'error))
        v
        (make-qty v (qvec-dim qv)))))

;;; ============================================================
;;; Construction
;;; ============================================================

(doc 'section 'qvec-construction)

(define (qvec dim . elems)
  (doc 'type '(-> Dimension Number ... QVec))
  (doc 'description "Variadic construction: (qvec dim-velocity 1.0 2.0 3.0)")
  (make-qvec dim (list->vector elems)))

(define (qvec-from-list dim lst)
  (doc 'type '(-> Dimension (List Number) QVec))
  (doc 'description "Create quantity vector from dimension and list of numbers.")
  (make-qvec dim (list->vector lst)))

(define (qvec-from-quantities qs)
  (doc 'type '(-> (List Quantity) (Result QVec Error)))
  (doc 'description "Create from list of quantities (all must have same dimension).")
  (if (null? qs)
      (make-qvec dim-one (vector))
      (let ([dim (qty-dim (car qs))])
        (if (andmap (lambda (q) (dim=? dim (qty-dim q))) qs)
            (make-qvec dim (list->vector (map qty-value qs)))
            (error 'qvec-from-quantities "dimension mismatch in list")))))

(define (qvec-zeros dim n)
  (doc 'type '(-> Dimension Nat QVec))
  (doc 'description "Zero vector with given dimension.")
  (make-qvec dim (vec-zeros n)))

(define (qvec-unit dim n k)
  (doc 'type '(-> Dimension Nat Nat QVec))
  (doc 'description "Unit vector: 1 at position k, 0 elsewhere.")
  (make-qvec dim (vec-unit n k)))

;;; ============================================================
;;; Conversion
;;; ============================================================

(doc 'section 'qvec-conversion)

(define (qvec->list qv)
  (doc 'type '(-> QVec (List Quantity)))
  (doc 'description "Convert to list of quantities.")
  (let ([dim (qvec-dim qv)]
        [vals (vec->list (qvec-values qv))])
    (map (lambda (v) (make-qty v dim)) vals)))

(define (qvec->string qv)
  (doc 'type '(-> QVec String))
  (doc 'description "Display quantity vector with units.")
  (let ([dim-str (dim->string (qvec-dim qv))]
        [vals (vec->list (qvec-values qv))])
    (string-append "#qvec["
                   (string-join (map number->string vals) " ")
                   "] "
                   dim-str)))

;;; ============================================================
;;; Arithmetic: Same-Dimension Operations
;;; ============================================================

(doc 'section 'qvec-arithmetic-same-dim)

(doc 'note "Addition and subtraction require matching dimensions.")

(define (qvec-add qv1 qv2)
  (doc 'type '(-> QVec QVec (Result QVec Error)))
  (doc 'description "Add two quantity vectors (dimensions must match).")
  (if (not (dim=? (qvec-dim qv1) (qvec-dim qv2)))
      (error 'qvec-add "dimension mismatch")
      (let ([result (vec-add (qvec-values qv1) (qvec-values qv2))])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (make-qvec (qvec-dim qv1) result)))))

(define (qvec-sub qv1 qv2)
  (doc 'type '(-> QVec QVec (Result QVec Error)))
  (doc 'description "Subtract two quantity vectors (dimensions must match).")
  (if (not (dim=? (qvec-dim qv1) (qvec-dim qv2)))
      (error 'qvec-sub "dimension mismatch")
      (let ([result (vec-sub (qvec-values qv1) (qvec-values qv2))])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (make-qvec (qvec-dim qv1) result)))))

(define (qvec-negate qv)
  (doc 'type '(-> QVec QVec))
  (doc 'description "Negate all elements.")
  (make-qvec (qvec-dim qv) (vec-negate (qvec-values qv))))

;;; ============================================================
;;; Arithmetic: Scaling
;;; ============================================================

(doc 'section 'qvec-scaling)

(define (qvec-scale k qv)
  (doc 'type '(-> Number QVec QVec))
  (doc 'description "Scale by dimensionless number (preserves dimension).")
  (make-qvec (qvec-dim qv) (vec-scale k (qvec-values qv))))

(define (qvec-qty-scale qty qv)
  (doc 'type '(-> Quantity QVec QVec))
  (doc 'description "Scale by quantity (dimensions multiply).")
  (make-qvec (dim* (qty-dim qty) (qvec-dim qv))
             (vec-scale (qty-value qty) (qvec-values qv))))

(define (qvec-qty-div qv qty)
  (doc 'type '(-> QVec Quantity QVec))
  (doc 'description "Divide vector by quantity (dimensions divide).")
  (doc 'note "Example: displacement / time = velocity")
  (make-qvec (dim/ (qvec-dim qv) (qty-dim qty))
             (vec-scale (/ 1 (qty-value qty)) (qvec-values qv))))

;;; ============================================================
;;; Products
;;; ============================================================

(doc 'section 'qvec-products)

(define (qvec-dot qv1 qv2)
  (doc 'type '(-> QVec QVec (Result Quantity Error)))
  (doc 'description "Dot product. Returns quantity with multiplied dimensions.")
  (doc 'note "Velocity · Time = Displacement (L/T · T = L)")
  (let ([result (vec-dot (qvec-values qv1) (qvec-values qv2))])
    (if (and (pair? result) (eq? (car result) 'error))
        result
        (make-qty result (dim* (qvec-dim qv1) (qvec-dim qv2))))))

(define (qvec-hadamard qv1 qv2)
  (doc 'type '(-> QVec QVec (Result QVec Error)))
  (doc 'description "Element-wise multiplication. Dimensions multiply.")
  (let ([result (vec-mul (qvec-values qv1) (qvec-values qv2))])
    (if (and (pair? result) (eq? (car result) 'error))
        result
        (make-qvec (dim* (qvec-dim qv1) (qvec-dim qv2)) result))))

;;; ============================================================
;;; Norms
;;; ============================================================

(doc 'section 'qvec-norms)

(define (qvec-norm-squared qv)
  (doc 'type '(-> QVec Quantity))
  (doc 'description "Squared L2 norm. Returns quantity with squared dimension.")
  (make-qty (vec-norm-squared (qvec-values qv))
            (dim-pow (qvec-dim qv) 2)))

(define (qvec-norm qv)
  (doc 'type '(-> QVec Quantity))
  (doc 'description "L2 norm (Euclidean magnitude). Returns quantity with same dimension.")
  (doc 'note "||position|| is a length, ||velocity|| is a speed, etc.")
  (make-qty (vec-norm (qvec-values qv)) (qvec-dim qv)))

(define (qvec-normalize qv)
  (doc 'type '(-> QVec (Result QVec Error)))
  (doc 'description "Normalize to unit magnitude (keeps dimension).")
  (let ([result (vec-normalize (qvec-values qv))])
    (if (and (pair? result) (eq? (car result) 'error))
        result
        (make-qvec (qvec-dim qv) result))))

(define (qvec-distance qv1 qv2)
  (doc 'type '(-> QVec QVec (Result Quantity Error)))
  (doc 'description "Euclidean distance (dimensions must match).")
  (if (not (dim=? (qvec-dim qv1) (qvec-dim qv2)))
      (error 'qvec-distance "dimension mismatch")
      (let ([result (vec-distance (qvec-values qv1) (qvec-values qv2))])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (make-qty result (qvec-dim qv1))))))

;;; ============================================================
;;; Comparison
;;; ============================================================

(doc 'section 'qvec-comparison)

(define (qvec-equal? qv1 qv2)
  (doc 'type '(-> QVec QVec Boolean))
  (doc 'description "Exact equality (dimension and values).")
  (and (dim=? (qvec-dim qv1) (qvec-dim qv2))
       (vec-equal? (qvec-values qv1) (qvec-values qv2))))

(define (qvec-approx-equal? qv1 qv2 . epsilon-arg)
  (doc 'type '(-> QVec QVec &opt Number Boolean))
  (doc 'description "Approximate equality within tolerance.")
  (and (dim=? (qvec-dim qv1) (qvec-dim qv2))
       (apply vec-approx-equal? (qvec-values qv1) (qvec-values qv2) epsilon-arg)))

;;; ============================================================
;;; Physics Convenience Constructors
;;; ============================================================

(doc 'section 'physics-constructors)

(doc 'note "Common vector types for physics simulations")

;; Position vectors (length dimension)
(define (position-vec . coords)
  (doc 'type '(-> Number ... QVec))
  (doc 'description "Position vector in meters.")
  (make-qvec dim-length-base (list->vector coords)))

(define (position-vec-2d x y)
  (doc 'type '(-> Number Number QVec))
  (doc 'description "2D position vector in meters.")
  (make-qvec dim-length-base (vector x y)))

(define (position-vec-3d x y z)
  (doc 'type '(-> Number Number Number QVec))
  (doc 'description "3D position vector in meters.")
  (make-qvec dim-length-base (vector x y z)))

;; Velocity vectors (length/time dimension)
(define (velocity-vec . coords)
  (doc 'type '(-> Number ... QVec))
  (doc 'description "Velocity vector in m/s.")
  (make-qvec dim-velocity (list->vector coords)))

(define (velocity-vec-2d vx vy)
  (doc 'type '(-> Number Number QVec))
  (doc 'description "2D velocity vector in m/s.")
  (make-qvec dim-velocity (vector vx vy)))

(define (velocity-vec-3d vx vy vz)
  (doc 'type '(-> Number Number Number QVec))
  (doc 'description "3D velocity vector in m/s.")
  (make-qvec dim-velocity (vector vx vy vz)))

;; Acceleration vectors
(define (acceleration-vec . coords)
  (doc 'type '(-> Number ... QVec))
  (doc 'description "Acceleration vector in m/s².")
  (make-qvec dim-acceleration (list->vector coords)))

(define (acceleration-vec-2d ax ay)
  (doc 'type '(-> Number Number QVec))
  (doc 'description "2D acceleration vector in m/s².")
  (make-qvec dim-acceleration (vector ax ay)))

(define (acceleration-vec-3d ax ay az)
  (doc 'type '(-> Number Number Number QVec))
  (doc 'description "3D acceleration vector in m/s².")
  (make-qvec dim-acceleration (vector ax ay az)))

;; Force vectors
(define (force-vec . coords)
  (doc 'type '(-> Number ... QVec))
  (doc 'description "Force vector in newtons.")
  (make-qvec dim-force (list->vector coords)))

(define (force-vec-2d fx fy)
  (doc 'type '(-> Number Number QVec))
  (doc 'description "2D force vector in newtons.")
  (make-qvec dim-force (vector fx fy)))

(define (force-vec-3d fx fy fz)
  (doc 'type '(-> Number Number Number QVec))
  (doc 'description "3D force vector in newtons.")
  (make-qvec dim-force (vector fx fy fz)))

;;; ============================================================
;;; Physics Operations
;;; ============================================================

(doc 'section 'physics-operations)

(define (qvec-integrate-time qv dt)
  (doc 'type '(-> QVec Quantity (Result QVec Error)))
  (doc 'description "Integrate over time: v*dt → displacement, a*dt → velocity change.")
  (doc 'note "dt must have time dimension")
  (if (not (dim=? (qty-dim dt) dim-time-base))
      (error 'qvec-integrate-time "dt must have time dimension")
      (qvec-qty-scale dt qv)))

(define (qvec-differentiate-time qv dt)
  (doc 'type '(-> QVec Quantity (Result QVec Error)))
  (doc 'description "Differentiate w.r.t. time: displacement/dt → velocity, velocity/dt → acceleration.")
  (doc 'note "dt must have time dimension")
  (if (not (dim=? (qty-dim dt) dim-time-base))
      (error 'qvec-differentiate-time "dt must have time dimension")
      (make-qvec (dim/ (qvec-dim qv) dim-time-base)
                 (vec-scale (/ 1 (qty-value dt)) (qvec-values qv)))))

(define (apply-force mass force dt)
  (doc 'type '(-> Quantity QVec Quantity QVec))
  (doc 'description "Compute velocity change from F=ma: dv = (F/m)*dt")
  (doc 'note "mass: kg, force: N, dt: s → returns m/s")
  ;; Derive acceleration dimension from force/mass rather than hardcoding
  (let* ([accel-dim (dim/ (qvec-dim force) (qty-dim mass))]
         [acceleration (make-qvec accel-dim
                                  (vec-scale (/ 1 (qty-value mass))
                                            (qvec-values force)))]
         [dv (qvec-integrate-time acceleration dt)])
    dv))

;;; ============================================================
;;; Cross Product (3D only)
;;; ============================================================

(doc 'section 'qvec-cross-product)

(define (qvec-cross qv1 qv2)
  (doc 'type '(-> QVec QVec (Result QVec Error)))
  (doc 'description "3D cross product. Dimensions multiply.")
  (doc 'note "Torque = r × F where r is position, F is force")
  (let ([v1 (qvec-values qv1)]
        [v2 (qvec-values qv2)])
    (if (not (and (= 3 (vec-length v1)) (= 3 (vec-length v2))))
        (error 'qvec-cross "cross product requires 3D vectors")
        (let ([x1 (vector-ref v1 0)] [y1 (vector-ref v1 1)] [z1 (vector-ref v1 2)]
              [x2 (vector-ref v2 0)] [y2 (vector-ref v2 1)] [z2 (vector-ref v2 2)])
          (make-qvec (dim* (qvec-dim qv1) (qvec-dim qv2))
                     (vector (- (* y1 z2) (* z1 y2))
                             (- (* z1 x2) (* x1 z2))
                             (- (* x1 y2) (* y1 x2))))))))

;;; ============================================================
;;; Map and Fold with Quantities
;;; ============================================================

(doc 'section 'qvec-higher-order)

(define (qvec-map f qv)
  (doc 'type '(-> (-> Number Number) QVec QVec))
  (doc 'description "Apply numeric function to each element (preserves dimension).")
  (make-qvec (qvec-dim qv) (vec-map f (qvec-values qv))))

(define (qvec-fold f init qv)
  (doc 'type '(-> (-> a Number a) a QVec a))
  (doc 'description "Fold over numeric values.")
  (vec-fold f init (qvec-values qv)))

(define (qvec-sum qv)
  (doc 'type '(-> QVec Quantity))
  (doc 'description "Sum all elements as a quantity.")
  (make-qty (vec-sum (qvec-values qv)) (qvec-dim qv)))

(define (qvec-mean qv)
  (doc 'type '(-> QVec Quantity))
  (doc 'description "Mean of all elements as a quantity.")
  (make-qty (vec-mean (qvec-values qv)) (qvec-dim qv)))
