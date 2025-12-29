;;; fabric/stitches/fp/units.ss — Units of Measure Library
;;;
;;; Type-safe physical units using dimension types.
;;; Based on the SI system of units.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; The 7 SI base dimensions:
;;;   - Length (L)      — meter (m)
;;;   - Time (T)        — second (s)
;;;   - Mass (M)        — kilogram (kg)
;;;   - Current (I)     — ampere (A)
;;;   - Temperature (Θ) — kelvin (K)
;;;   - Amount (N)      — mole (mol)
;;;   - Luminosity (J)  — candela (cd)
;;;
;;; Dimensions are represented as 7-tuples of integer exponents:
;;;   (L T M I Θ N J)
;;;
;;; Examples:
;;;   Velocity = L¹ T⁻¹ → (1 -1 0 0 0 0 0)
;;;   Force = M¹ L¹ T⁻² → (1 -2 1 0 0 0 0)
;;;   Energy = M¹ L² T⁻² → (2 -2 1 0 0 0 0)
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; Dimension Type
;;; ============================================================

;;; A dimension is a 7-tuple of integer exponents:
;;; (length time mass current temperature amount luminosity)

;;; make-dim : Int × Int × Int × Int × Int × Int × Int → Dimension
(define (make-dim l t m i θ n j)
  (list l t m i θ n j))

;;; Dimension accessors
(define (dim-length d)      (list-ref d 0))
(define (dim-time d)        (list-ref d 1))
(define (dim-mass d)        (list-ref d 2))
(define (dim-current d)     (list-ref d 3))
(define (dim-temperature d) (list-ref d 4))
(define (dim-amount d)      (list-ref d 5))
(define (dim-luminosity d)  (list-ref d 6))

;;; dim? : Any → Boolean
;;; Check if value is a valid dimension.
(define (dim? d)
  (and (list? d)
       (= (length d) 7)
       (andmap integer? d)))

;;; dim=? : Dimension × Dimension → Boolean
;;; Check if two dimensions are equal.
(define (dim=? d1 d2)
  (equal? d1 d2))

;;; ============================================================
;;; SI Base Dimensions
;;; ============================================================

;;; Dimensionless (scalar)
(define dim-one (make-dim 0 0 0 0 0 0 0))

;;; Length — meter
(define dim-length-base (make-dim 1 0 0 0 0 0 0))

;;; Time — second
(define dim-time-base (make-dim 0 1 0 0 0 0 0))

;;; Mass — kilogram
(define dim-mass-base (make-dim 0 0 1 0 0 0 0))

;;; Electric current — ampere
(define dim-current-base (make-dim 0 0 0 1 0 0 0))

;;; Temperature — kelvin
(define dim-temperature-base (make-dim 0 0 0 0 1 0 0))

;;; Amount of substance — mole
(define dim-amount-base (make-dim 0 0 0 0 0 1 0))

;;; Luminous intensity — candela
(define dim-luminosity-base (make-dim 0 0 0 0 0 0 1))

;;; ============================================================
;;; Dimension Arithmetic
;;; ============================================================

;;; dim* : Dimension × Dimension → Dimension
;;; Multiply dimensions (add exponents).
(define (dim* d1 d2)
  (map + d1 d2))

;;; dim/ : Dimension × Dimension → Dimension
;;; Divide dimensions (subtract exponents).
(define (dim/ d1 d2)
  (map - d1 d2))

;;; dim-pow : Dimension × Int → Dimension
;;; Raise dimension to integer power.
(define (dim-pow d n)
  (map (lambda (e) (* e n)) d))

;;; dim-sqrt : Dimension → Dimension | #f
;;; Square root of dimension (all exponents must be even).
(define (dim-sqrt d)
  (if (andmap even? d)
      (map (lambda (e) (quotient e 2)) d)
      #f))

;;; dim-inv : Dimension → Dimension
;;; Inverse of dimension (negate all exponents).
(define (dim-inv d)
  (map - d))

;;; ============================================================
;;; Quantity Type
;;; ============================================================

;;; A quantity is a value with a dimension.
;;; Represented as (value . dimension).

;;; make-qty : Number × Dimension → Quantity
(define (make-qty value dim)
  (cons value dim))

;;; qty-value : Quantity → Number
(define (qty-value q) (car q))

;;; qty-dim : Quantity → Dimension
(define (qty-dim q) (cdr q))

;;; qty? : Any → Boolean
(define (qty? q)
  (and (pair? q)
       (number? (car q))
       (dim? (cdr q))))

;;; ============================================================
;;; Quantity Arithmetic
;;; ============================================================

;;; qty+ : Quantity × Quantity → Quantity | error
;;; Add quantities (dimensions must match).
(define (qty+ q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (make-qty (+ (qty-value q1) (qty-value q2))
                (qty-dim q1))
      (error 'qty+ "dimension mismatch")))

;;; qty- : Quantity × Quantity → Quantity | error
;;; Subtract quantities (dimensions must match).
(define (qty- q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (make-qty (- (qty-value q1) (qty-value q2))
                (qty-dim q1))
      (error 'qty- "dimension mismatch")))

;;; qty* : Quantity × Quantity → Quantity
;;; Multiply quantities.
(define (qty* q1 q2)
  (make-qty (* (qty-value q1) (qty-value q2))
            (dim* (qty-dim q1) (qty-dim q2))))

;;; qty/ : Quantity × Quantity → Quantity
;;; Divide quantities.
(define (qty/ q1 q2)
  (make-qty (/ (qty-value q1) (qty-value q2))
            (dim/ (qty-dim q1) (qty-dim q2))))

;;; qty-scale : Quantity × Number → Quantity
;;; Scale quantity by dimensionless factor.
(define (qty-scale q n)
  (make-qty (* (qty-value q) n) (qty-dim q)))

;;; qty-pow : Quantity × Int → Quantity
;;; Raise quantity to integer power.
(define (qty-pow q n)
  (make-qty (expt (qty-value q) n)
            (dim-pow (qty-dim q) n)))

;;; qty-sqrt : Quantity → Quantity | #f
;;; Square root (dimension exponents must be even).
(define (qty-sqrt q)
  (let ([new-dim (dim-sqrt (qty-dim q))])
       (if new-dim
           (make-qty (sqrt (qty-value q)) new-dim)
           #f)))

;;; qty-neg : Quantity → Quantity
;;; Negate quantity value.
(define (qty-neg q)
  (make-qty (- (qty-value q)) (qty-dim q)))

;;; qty-abs : Quantity → Quantity
;;; Absolute value.
(define (qty-abs q)
  (make-qty (abs (qty-value q)) (qty-dim q)))

;;; ============================================================
;;; Quantity Comparison
;;; ============================================================

;;; qty=? : Quantity × Quantity → Boolean | error
;;; Equal quantities (dimensions must match).
(define (qty=? q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (= (qty-value q1) (qty-value q2))
      (error 'qty=? "dimension mismatch")))

;;; qty<? : Quantity × Quantity → Boolean | error
(define (qty<? q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (< (qty-value q1) (qty-value q2))
      (error 'qty<? "dimension mismatch")))

;;; qty>? : Quantity × Quantity → Boolean | error
(define (qty>? q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (> (qty-value q1) (qty-value q2))
      (error 'qty>? "dimension mismatch")))

;;; qty<=? : Quantity × Quantity → Boolean | error
(define (qty<=? q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (<= (qty-value q1) (qty-value q2))
      (error 'qty<=? "dimension mismatch")))

;;; qty>=? : Quantity × Quantity → Boolean | error
(define (qty>=? q1 q2)
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (>= (qty-value q1) (qty-value q2))
      (error 'qty>=? "dimension mismatch")))

;;; ============================================================
;;; SI Base Units
;;; ============================================================

;;; meter : Number → Quantity
;;; Create a length quantity in meters.
(define (meter x)
  (make-qty x dim-length-base))

;;; second : Number → Quantity
;;; Create a time quantity in seconds.
(define (second x)
  (make-qty x dim-time-base))

;;; kilogram : Number → Quantity
;;; Create a mass quantity in kilograms.
(define (kilogram x)
  (make-qty x dim-mass-base))

;;; ampere : Number → Quantity
;;; Create a current quantity in amperes.
(define (ampere x)
  (make-qty x dim-current-base))

;;; kelvin : Number → Quantity
;;; Create a temperature quantity in kelvin.
(define (kelvin x)
  (make-qty x dim-temperature-base))

;;; mole : Number → Quantity
;;; Create an amount quantity in moles.
(define (mole x)
  (make-qty x dim-amount-base))

;;; candela : Number → Quantity
;;; Create a luminosity quantity in candelas.
(define (candela x)
  (make-qty x dim-luminosity-base))

;;; ============================================================
;;; Common Derived Dimensions
;;; ============================================================

;;; Velocity: L/T
(define dim-velocity (dim/ dim-length-base dim-time-base))

;;; Acceleration: L/T²
(define dim-acceleration (dim/ dim-length-base (dim-pow dim-time-base 2)))

;;; Force: M·L/T² (Newton)
(define dim-force (dim* dim-mass-base dim-acceleration))

;;; Energy: M·L²/T² (Joule)
(define dim-energy (dim* dim-mass-base
                         (dim/ (dim-pow dim-length-base 2)
                               (dim-pow dim-time-base 2))))

;;; Power: M·L²/T³ (Watt)
(define dim-power (dim/ dim-energy dim-time-base))

;;; Pressure: M/(L·T²) (Pascal)
(define dim-pressure (dim/ dim-mass-base
                           (dim* dim-length-base (dim-pow dim-time-base 2))))

;;; Frequency: 1/T (Hertz)
(define dim-frequency (dim-inv dim-time-base))

;;; Electric charge: I·T (Coulomb)
(define dim-charge (dim* dim-current-base dim-time-base))

;;; Voltage: M·L²/(T³·I) (Volt)
(define dim-voltage (dim/ dim-power dim-current-base))

;;; Resistance: M·L²/(T³·I²) (Ohm)
(define dim-resistance (dim/ dim-voltage dim-current-base))

;;; Area: L²
(define dim-area (dim-pow dim-length-base 2))

;;; Volume: L³
(define dim-volume (dim-pow dim-length-base 3))

;;; ============================================================
;;; Common Derived Units
;;; ============================================================

;;; newton : Number → Quantity
;;; Force in newtons.
(define (newton x)
  (make-qty x dim-force))

;;; joule : Number → Quantity
;;; Energy in joules.
(define (joule x)
  (make-qty x dim-energy))

;;; watt : Number → Quantity
;;; Power in watts.
(define (watt x)
  (make-qty x dim-power))

;;; pascal : Number → Quantity
;;; Pressure in pascals.
(define (pascal x)
  (make-qty x dim-pressure))

;;; hertz : Number → Quantity
;;; Frequency in hertz.
(define (hertz x)
  (make-qty x dim-frequency))

;;; coulomb : Number → Quantity
;;; Electric charge in coulombs.
(define (coulomb x)
  (make-qty x dim-charge))

;;; volt : Number → Quantity
;;; Voltage in volts.
(define (volt x)
  (make-qty x dim-voltage))

;;; ohm : Number → Quantity
;;; Resistance in ohms.
(define (ohm x)
  (make-qty x dim-resistance))

;;; ============================================================
;;; SI Prefixes
;;; ============================================================

;;; SI prefix multipliers
(define yotta 1e24)
(define zetta 1e21)
(define exa   1e18)
(define peta  1e15)
(define tera  1e12)
(define giga  1e9)
(define mega  1e6)
(define kilo  1e3)
(define hecto 1e2)
(define deca  1e1)
(define deci  1e-1)
(define centi 1e-2)
(define milli 1e-3)
(define micro 1e-6)
(define nano  1e-9)
(define pico  1e-12)
(define femto 1e-15)
(define atto  1e-18)
(define zepto 1e-21)
(define yocto 1e-24)

;;; Convenient prefixed units
(define (kilometer x) (meter (* x kilo)))
(define (centimeter x) (meter (* x centi)))
(define (millimeter x) (meter (* x milli)))
(define (micrometer x) (meter (* x micro)))
(define (nanometer x) (meter (* x nano)))

(define (millisecond x) (second (* x milli)))
(define (microsecond x) (second (* x micro)))
(define (nanosecond x) (second (* x nano)))

(define (gram x) (kilogram (* x milli)))
(define (milligram x) (kilogram (* x micro)))

(define (kilowatt x) (watt (* x kilo)))
(define (megawatt x) (watt (* x mega)))
(define (gigawatt x) (watt (* x giga)))

(define (kilojoule x) (joule (* x kilo)))
(define (megajoule x) (joule (* x mega)))

(define (milliampere x) (ampere (* x milli)))
(define (microampere x) (ampere (* x micro)))

(define (kilohertz x) (hertz (* x kilo)))
(define (megahertz x) (hertz (* x mega)))
(define (gigahertz x) (hertz (* x giga)))

(define (kilovolt x) (volt (* x kilo)))
(define (millivolt x) (volt (* x milli)))

(define (kiloohm x) (ohm (* x kilo)))
(define (megaohm x) (ohm (* x mega)))

;;; ============================================================
;;; Dimension Display
;;; ============================================================

;;; dim->string : Dimension → String
;;; Display dimension in standard notation.
(define (dim->string d)
  (let ([parts '()]
        [names '("m" "s" "kg" "A" "K" "mol" "cd")]
        [exps d])
       (let loop ([i 0] [parts '()])
            (if (= i 7)
                (if (null? parts)
                    "1"
                    (string-join (reverse parts) "·"))
                (let ([exp (list-ref exps i)]
                      [name (list-ref names i)])
                     (loop (+ i 1)
                           (cond
                            [(= exp 0) parts]
                            [(= exp 1) (cons name parts)]
                            [else (cons (string-append name "^" (number->string exp))
                                        parts)])))))))

;;; qty->string : Quantity → String
;;; Display quantity with units.
(define (qty->string q)
  (string-append (number->string (qty-value q))
                 " "
                 (dim->string (qty-dim q))))

;;; ============================================================
;;; Dimensionless Quantities
;;; ============================================================

;;; scalar : Number → Quantity
;;; Create a dimensionless quantity.
(define (scalar x)
  (make-qty x dim-one))

;;; qty-dimensionless? : Quantity → Boolean
;;; Check if quantity is dimensionless.
(define (qty-dimensionless? q)
  (dim=? (qty-dim q) dim-one))

;;; qty->number : Quantity → Number | error
;;; Extract number from dimensionless quantity.
(define (qty->number q)
  (if (qty-dimensionless? q)
      (qty-value q)
      (error 'qty->number "quantity has dimension")))

;;; ============================================================
;;; Unit Conversions
;;; ============================================================

;;; convert : Quantity × Quantity → Number
;;; Convert quantity to different unit (same dimension).
;;; Returns the numeric factor.
;;; Example: (convert (kilometer 1) (meter 1)) → 1000
(define (convert q unit)
  (if (dim=? (qty-dim q) (qty-dim unit))
      (/ (qty-value q) (qty-value unit))
      (error 'convert "dimension mismatch")))

;;; in-units : Quantity × (Number → Quantity) → Number
;;; Express quantity in given units.
;;; Example: (in-units (kilometer 5) meter) → 5000
(define (in-units q unit-fn)
  (let ([unit (unit-fn 1)])
       (if (dim=? (qty-dim q) (qty-dim unit))
           (/ (qty-value q) (qty-value unit))
           (error 'in-units "dimension mismatch"))))

