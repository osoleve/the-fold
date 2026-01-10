((name "measure")
 (description "Units of measure for type-safe physical quantity computations.
  Prevents unit errors at definition time by tracking dimensions
  through arithmetic operations.")

 (modules
  ((file "units.ss")
   (purpose "Dimensional analysis and unit tracking")
   (exports
    (make-quantity "Create a quantity with value and unit")
    (quantity-value "Extract the numeric value")
    (quantity-unit "Extract the unit")
    (quantity-dimension "Extract the dimension (e.g., length, time)")
    (q+ "Add quantities (must have same dimension)")
    (q- "Subtract quantities (must have same dimension)")
    (q* "Multiply quantities (dimensions multiply)")
    (q/ "Divide quantities (dimensions divide)")
    (q-sqrt "Square root (dimension must be square)")
    (q-expt "Raise to integer power")
    (q-convert "Convert to different unit of same dimension")
    (q-compatible? "Check if two quantities have same dimension")
    (make-unit "Define a new unit with dimension and conversion factor")
    (make-dimension "Define a new base dimension")
    (unit-dimension "Get dimension of a unit")
    (unit-factor "Get conversion factor to SI base")
    ;; Predefined dimensions
    (dim-length "Length dimension")
    (dim-time "Time dimension")
    (dim-mass "Mass dimension")
    (dim-temperature "Temperature dimension")
    (dim-current "Electric current dimension")
    ;; Predefined units
    (meter "SI unit of length")
    (second "SI unit of time")
    (kilogram "SI unit of mass")
    (kilometer "1000 meters")
    (mile "1609.344 meters")
    (hour "3600 seconds")
    (minute "60 seconds"))
   (example
    ";; Safe computation: distance = speed * time
     (define speed (make-quantity 60 (q/ mile hour)))
     (define time (make-quantity 2.5 hour))
     (define distance (q* speed time))
     (quantity-value distance)  ;; => 150
     (quantity-unit distance)   ;; => mile

     ;; Convert to kilometers
     (q-convert distance kilometer)
     ;; => quantity with value ~241.4

     ;; This would signal an error:
     ;; (q+ (make-quantity 5 meter) (make-quantity 3 second))")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/base/error.ss" "Error handling"))

 (notes
  "Units of measure prevent a class of errors that are easy to make
   and hard to debug: mixing incompatible physical quantities.

   The system tracks dimensions through computation:
   - Adding/subtracting requires matching dimensions
   - Multiplying creates product dimensions (e.g., m * m = m^2)
   - Dividing creates quotient dimensions (e.g., m / s = velocity)

   All units are internally normalized to SI base units for comparison.

   Test suite: 47 tests covering arithmetic, conversion, and dimension checking."))
