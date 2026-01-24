((name "measure")
 (description "Units of measure for type-safe physical quantity computations.
  Prevents unit errors at definition time by tracking dimensions
  through arithmetic operations using the SI system of 7 base dimensions.")

 (modules
  ((file "units.ss")
   (purpose "Dimensional analysis and quantity arithmetic")
   (exports
    ;; Dimension type
    (make-dim "Create 7-tuple of SI exponents: (L T M I Theta N J)")
    (dim? "Check if value is a valid dimension")
    (dim=? "Check dimension equality")
    (dim* "Multiply dimensions (add exponents)")
    (dim/ "Divide dimensions (subtract exponents)")
    (dim-pow "Raise dimension to integer power")
    (dim-sqrt "Square root dimension (requires even exponents)")
    (dim-inv "Inverse dimension")
    ;; Base dimensions
    (dim-one "Dimensionless")
    (dim-length-base "Length [L]")
    (dim-time-base "Time [T]")
    (dim-mass-base "Mass [M]")
    (dim-velocity "Velocity [L/T]")
    (dim-acceleration "Acceleration [L/T^2]")
    (dim-force "Force [M*L/T^2]")
    (dim-energy "Energy [M*L^2/T^2]")
    ;; Quantity type
    (make-qty "Create quantity from value and dimension")
    (qty-value "Extract numeric value")
    (qty-dim "Extract dimension")
    (qty+ "Add quantities (dimensions must match)")
    (qty- "Subtract quantities (dimensions must match)")
    (qty* "Multiply quantities (dimensions multiply)")
    (qty/ "Divide quantities (dimensions divide)")
    (qty-scale "Scale by dimensionless number")
    (qty-sqrt "Square root (dimension must have even exponents)")
    ;; Unit constructors
    (meter "Length in meters")
    (second "Time in seconds")
    (kilogram "Mass in kilograms")
    (newton "Force in newtons")
    (joule "Energy in joules")
    ;; Prefixed units
    (kilometer "1000 meters")
    (millisecond "0.001 seconds")
    (kilowatt "1000 watts"))
   (example
    ";; Calculate kinetic energy: E = 0.5 * m * v^2
     (define m (kilogram 10))
     (define v (qty/ (meter 20) (second 1)))  ;; 20 m/s
     (define KE (qty-scale (qty* m (qty-pow v 2)) 0.5))
     (qty-value KE)  ;; => 2000
     (qty-dim KE)    ;; => (2 -2 1 0 0 0 0) = M*L^2/T^2 = energy

     ;; This errors: (qty+ (meter 5) (second 3))
     ;; dimension mismatch"))

  ((file "qvec.ss")
   (purpose "Unit-aware vectors for physics computations")
   (exports
    ;; QVec type
    (make-qvec "Create from dimension and numeric vector")
    (qvec "Variadic construction: (qvec dim-velocity 1 2 3)")
    (qvec? "Check if value is a quantity vector")
    (qvec-dim "Extract dimension")
    (qvec-values "Extract numeric vector")
    (qvec-length "Number of elements")
    (qvec-ref "Get element as Quantity")
    ;; Arithmetic
    (qvec-add "Add vectors (dimensions must match)")
    (qvec-sub "Subtract vectors (dimensions must match)")
    (qvec-scale "Scale by dimensionless number")
    (qvec-qty-scale "Scale by Quantity (dimensions multiply)")
    (qvec-dot "Dot product (returns Quantity)")
    (qvec-cross "3D cross product (dimensions multiply)")
    ;; Norms
    (qvec-norm "L2 norm as Quantity")
    (qvec-norm-squared "Squared norm (dimension squared)")
    (qvec-normalize "Unit magnitude, same dimension")
    (qvec-distance "Euclidean distance")
    ;; Physics constructors
    (position-vec "Position in meters")
    (velocity-vec "Velocity in m/s")
    (acceleration-vec "Acceleration in m/s^2")
    (force-vec "Force in newtons")
    ;; Physics operations
    (qvec-integrate-time "v*dt -> displacement")
    (qvec-differentiate-time "dx/dt -> velocity")
    (apply-force "F=ma: compute dv from force"))
   (example
    ";; Projectile motion
     (define pos (position-vec 0 100))    ;; 100m up
     (define vel (velocity-vec 10 0))     ;; 10 m/s horizontal
     (define g (acceleration-vec 0 -9.8)) ;; gravity
     (define dt (second 0.1))

     ;; Update velocity: v' = v + a*dt
     (define vel' (qvec-add vel (qvec-integrate-time g dt)))

     ;; Calculate kinetic energy: dot(v, v) * m / 2
     (define m (kilogram 1))
     (define KE (qty-scale (qvec-dot vel vel) (/ (qty-value m) 2)))")))

 (test-files
  ("test-units.ss" "47 tests for dimension arithmetic and quantities")
  ("test-qvec.ss" "36 tests for quantity vectors and physics ops"))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("lattice/linalg/vec.ss" "Numeric vector operations"))

 (notes
  "The system tracks dimensions through all computations:
   - Adding/subtracting requires matching dimensions
   - Multiplying creates product dimensions (e.g., m * m = m^2)
   - Dividing creates quotient dimensions (e.g., m / s = velocity)

   Quantity vectors (qvec) store dimension once for all elements,
   enabling efficient physics simulations with type safety.

   All units are normalized to SI base units: meter, second, kilogram,
   ampere, kelvin, mole, candela."))
