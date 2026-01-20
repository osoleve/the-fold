(load "core/base/prelude.ss")

(doc 'module 'units)
(doc 'description "Type-safe physical units using dimension types. Based on the SI system of units.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "This is Core code: pure, total, assumes reasonable input.")
(doc 'note "The 7 SI base dimensions:
  - Length (L)      — meter (m)
  - Time (T)        — second (s)
  - Mass (M)        — kilogram (kg)
  - Current (I)     — ampere (A)
  - Temperature (Θ) — kelvin (K)
  - Amount (N)      — mole (mol)
  - Luminosity (J)  — candela (cd)

Dimensions are represented as 7-tuples of integer exponents:
  (L T M I Θ N J)

Examples:
  Velocity = L¹ T⁻¹ → (1 -1 0 0 0 0 0)
  Force = M¹ L¹ T⁻² → (1 -2 1 0 0 0 0)
  Energy = M¹ L² T⁻² → (2 -2 1 0 0 0 0)")

(doc 'section 'dimension-type)

(doc 'note "A dimension is a 7-tuple of integer exponents:
(length time mass current temperature amount luminosity)")

(define (make-dim l t m i θ n j)
  (doc 'type '(-> Int Int Int Int Int Int Int Dimension))
  (doc 'description "Construct a dimension from seven integer exponents.")
  (list l t m i θ n j))

(define (dim-length d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the length exponent from a dimension.")
  (list-ref d 0))

(define (dim-time d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the time exponent from a dimension.")
  (list-ref d 1))

(define (dim-mass d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the mass exponent from a dimension.")
  (list-ref d 2))

(define (dim-current d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the current exponent from a dimension.")
  (list-ref d 3))

(define (dim-temperature d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the temperature exponent from a dimension.")
  (list-ref d 4))

(define (dim-amount d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the amount exponent from a dimension.")
  (list-ref d 5))

(define (dim-luminosity d)
  (doc 'type '(-> Dimension Int))
  (doc 'description "Extract the luminosity exponent from a dimension.")
  (list-ref d 6))

(define (dim? d)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a valid dimension.")
  (and (list? d)
       (= (length d) 7)
       (andmap integer? d)))

(define (dim=? d1 d2)
  (doc 'type '(-> Dimension Dimension Boolean))
  (doc 'description "Check if two dimensions are equal.")
  (equal? d1 d2))

(doc 'section 'si-base-dimensions)

(doc dim-one 'type 'Dimension)
(doc dim-one 'description "Dimensionless (scalar)")
(define dim-one (make-dim 0 0 0 0 0 0 0))

(doc dim-length-base 'type 'Dimension)
(doc dim-length-base 'description "Length — meter")
(define dim-length-base (make-dim 1 0 0 0 0 0 0))

(doc dim-time-base 'type 'Dimension)
(doc dim-time-base 'description "Time — second")
(define dim-time-base (make-dim 0 1 0 0 0 0 0))

(doc dim-mass-base 'type 'Dimension)
(doc dim-mass-base 'description "Mass — kilogram")
(define dim-mass-base (make-dim 0 0 1 0 0 0 0))

(doc dim-current-base 'type 'Dimension)
(doc dim-current-base 'description "Electric current — ampere")
(define dim-current-base (make-dim 0 0 0 1 0 0 0))

(doc dim-temperature-base 'type 'Dimension)
(doc dim-temperature-base 'description "Temperature — kelvin")
(define dim-temperature-base (make-dim 0 0 0 0 1 0 0))

(doc dim-amount-base 'type 'Dimension)
(doc dim-amount-base 'description "Amount of substance — mole")
(define dim-amount-base (make-dim 0 0 0 0 0 1 0))

(doc dim-luminosity-base 'type 'Dimension)
(doc dim-luminosity-base 'description "Luminous intensity — candela")
(define dim-luminosity-base (make-dim 0 0 0 0 0 0 1))

(doc 'section 'dimension-arithmetic)

(define (dim* d1 d2)
  (doc 'type '(-> Dimension Dimension Dimension))
  (doc 'description "Multiply dimensions (add exponents).")
  (map + d1 d2))

(define (dim/ d1 d2)
  (doc 'type '(-> Dimension Dimension Dimension))
  (doc 'description "Divide dimensions (subtract exponents).")
  (map - d1 d2))

(define (dim-pow d n)
  (doc 'type '(-> Dimension Int Dimension))
  (doc 'description "Raise dimension to integer power.")
  (map (lambda (e) (* e n)) d))

(define (dim-sqrt d)
  (doc 'type '(-> Dimension (Maybe Dimension)))
  (doc 'description "Square root of dimension (all exponents must be even).")
  (if (andmap even? d)
      (map (lambda (e) (quotient e 2)) d)
      #f))

(define (dim-inv d)
  (doc 'type '(-> Dimension Dimension))
  (doc 'description "Inverse of dimension (negate all exponents).")
  (map - d))

(doc 'section 'quantity-type)

(doc 'note "A quantity is a value with a dimension.
Represented as (value . dimension).")

(define (make-qty value dim)
  (doc 'type '(-> Number Dimension Quantity))
  (doc 'description "Construct a quantity from a numeric value and dimension.")
  (cons value dim))

(define (qty-value q)
  (doc 'type '(-> Quantity Number))
  (doc 'description "Extract the numeric value from a quantity.")
  (car q))

(define (qty-dim q)
  (doc 'type '(-> Quantity Dimension))
  (doc 'description "Extract the dimension from a quantity.")
  (cdr q))

(define (qty? q)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a valid quantity.")
  (and (pair? q)
       (number? (car q))
       (dim? (cdr q))))

(doc 'section 'quantity-arithmetic)

(define (qty+ q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Quantity Error)))
  (doc 'description "Add quantities (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (make-qty (+ (qty-value q1) (qty-value q2))
                (qty-dim q1))
      (error 'qty+ "dimension mismatch")))

(define (qty- q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Quantity Error)))
  (doc 'description "Subtract quantities (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (make-qty (- (qty-value q1) (qty-value q2))
                (qty-dim q1))
      (error 'qty- "dimension mismatch")))

(define (qty* q1 q2)
  (doc 'type '(-> Quantity Quantity Quantity))
  (doc 'description "Multiply quantities.")
  (make-qty (* (qty-value q1) (qty-value q2))
            (dim* (qty-dim q1) (qty-dim q2))))

(define (qty/ q1 q2)
  (doc 'type '(-> Quantity Quantity Quantity))
  (doc 'description "Divide quantities.")
  (make-qty (/ (qty-value q1) (qty-value q2))
            (dim/ (qty-dim q1) (qty-dim q2))))

(define (qty-scale q n)
  (doc 'type '(-> Quantity Number Quantity))
  (doc 'description "Scale quantity by dimensionless factor.")
  (make-qty (* (qty-value q) n) (qty-dim q)))

(define (qty-pow q n)
  (doc 'type '(-> Quantity Int Quantity))
  (doc 'description "Raise quantity to integer power.")
  (let ([v (qty-value q)])
       (if (and (= v 0) (< n 0))
           (error 'qty-pow "division by zero: 0^(negative exponent)")
           (make-qty (expt v n)
                     (dim-pow (qty-dim q) n)))))

(define (qty-sqrt q)
  (doc 'type '(-> Quantity (Maybe Quantity)))
  (doc 'description "Square root (dimension exponents must be even).")
  (let ([new-dim (dim-sqrt (qty-dim q))])
       (if new-dim
           (make-qty (sqrt (qty-value q)) new-dim)
           #f)))

(define (qty-neg q)
  (doc 'type '(-> Quantity Quantity))
  (doc 'description "Negate quantity value.")
  (make-qty (- (qty-value q)) (qty-dim q)))

(define (qty-abs q)
  (doc 'type '(-> Quantity Quantity))
  (doc 'description "Absolute value.")
  (make-qty (abs (qty-value q)) (qty-dim q)))

(doc 'section 'quantity-comparison)

(define (qty=? q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Boolean Error)))
  (doc 'description "Equal quantities (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (= (qty-value q1) (qty-value q2))
      (error 'qty=? "dimension mismatch")))

(define (qty<? q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Boolean Error)))
  (doc 'description "Less than comparison (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (< (qty-value q1) (qty-value q2))
      (error 'qty<? "dimension mismatch")))

(define (qty>? q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Boolean Error)))
  (doc 'description "Greater than comparison (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (> (qty-value q1) (qty-value q2))
      (error 'qty>? "dimension mismatch")))

(define (qty<=? q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Boolean Error)))
  (doc 'description "Less than or equal comparison (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (<= (qty-value q1) (qty-value q2))
      (error 'qty<=? "dimension mismatch")))

(define (qty>=? q1 q2)
  (doc 'type '(-> Quantity Quantity (Result Boolean Error)))
  (doc 'description "Greater than or equal comparison (dimensions must match).")
  (if (dim=? (qty-dim q1) (qty-dim q2))
      (>= (qty-value q1) (qty-value q2))
      (error 'qty>=? "dimension mismatch")))

(doc 'section 'si-base-units)

(define (meter x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in meters.")
  (make-qty x dim-length-base))

(define (second x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a time quantity in seconds.")
  (make-qty x dim-time-base))

(define (kilogram x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a mass quantity in kilograms.")
  (make-qty x dim-mass-base))

(define (ampere x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a current quantity in amperes.")
  (make-qty x dim-current-base))

(define (kelvin x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a temperature quantity in kelvin.")
  (make-qty x dim-temperature-base))

(define (mole x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create an amount quantity in moles.")
  (make-qty x dim-amount-base))

(define (candela x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a luminosity quantity in candelas.")
  (make-qty x dim-luminosity-base))

(doc 'section 'common-derived-dimensions)

(doc dim-velocity 'type 'Dimension)
(doc dim-velocity 'description "Velocity: L/T")
(define dim-velocity (dim/ dim-length-base dim-time-base))

(doc dim-acceleration 'type 'Dimension)
(doc dim-acceleration 'description "Acceleration: L/T²")
(define dim-acceleration (dim/ dim-length-base (dim-pow dim-time-base 2)))

(doc dim-force 'type 'Dimension)
(doc dim-force 'description "Force: M·L/T² (Newton)")
(define dim-force (dim* dim-mass-base dim-acceleration))

(doc dim-energy 'type 'Dimension)
(doc dim-energy 'description "Energy: M·L²/T² (Joule)")
(define dim-energy (dim* dim-mass-base
                         (dim/ (dim-pow dim-length-base 2)
                               (dim-pow dim-time-base 2))))

(doc dim-power 'type 'Dimension)
(doc dim-power 'description "Power: M·L²/T³ (Watt)")
(define dim-power (dim/ dim-energy dim-time-base))

(doc dim-pressure 'type 'Dimension)
(doc dim-pressure 'description "Pressure: M/(L·T²) (Pascal)")
(define dim-pressure (dim/ dim-mass-base
                           (dim* dim-length-base (dim-pow dim-time-base 2))))

(doc dim-frequency 'type 'Dimension)
(doc dim-frequency 'description "Frequency: 1/T (Hertz)")
(define dim-frequency (dim-inv dim-time-base))

(doc dim-charge 'type 'Dimension)
(doc dim-charge 'description "Electric charge: I·T (Coulomb)")
(define dim-charge (dim* dim-current-base dim-time-base))

(doc dim-voltage 'type 'Dimension)
(doc dim-voltage 'description "Voltage: M·L²/(T³·I) (Volt)")
(define dim-voltage (dim/ dim-power dim-current-base))

(doc dim-resistance 'type 'Dimension)
(doc dim-resistance 'description "Resistance: M·L²/(T³·I²) (Ohm)")
(define dim-resistance (dim/ dim-voltage dim-current-base))

(doc dim-area 'type 'Dimension)
(doc dim-area 'description "Area: L squared")
(define dim-area (dim-pow dim-length-base 2))

(doc dim-volume 'type 'Dimension)
(doc dim-volume 'description "Volume: L cubed")
(define dim-volume (dim-pow dim-length-base 3))

(doc 'section 'common-derived-units)

(define (newton x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Force in newtons.")
  (make-qty x dim-force))

(define (joule x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Energy in joules.")
  (make-qty x dim-energy))

(define (watt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Power in watts.")
  (make-qty x dim-power))

(define (pascal x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Pressure in pascals.")
  (make-qty x dim-pressure))

(define (hertz x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Frequency in hertz.")
  (make-qty x dim-frequency))

(define (coulomb x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Electric charge in coulombs.")
  (make-qty x dim-charge))

(define (volt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Voltage in volts.")
  (make-qty x dim-voltage))

(define (ohm x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Resistance in ohms.")
  (make-qty x dim-resistance))

(doc 'section 'si-prefixes)

(doc 'note "SI prefix multipliers")

(doc yotta 'type 'Number)
(doc yotta 'description "10^24 multiplier")
(define yotta 1e24)

(doc zetta 'type 'Number)
(doc zetta 'description "10^21 multiplier")
(define zetta 1e21)

(doc exa 'type 'Number)
(doc exa 'description "10^18 multiplier")
(define exa   1e18)

(doc peta 'type 'Number)
(doc peta 'description "10^15 multiplier")
(define peta  1e15)

(doc tera 'type 'Number)
(doc tera 'description "10^12 multiplier")
(define tera  1e12)

(doc giga 'type 'Number)
(doc giga 'description "10^9 multiplier")
(define giga  1e9)

(doc mega 'type 'Number)
(doc mega 'description "10^6 multiplier")
(define mega  1e6)

(doc kilo 'type 'Number)
(doc kilo 'description "10^3 multiplier")
(define kilo  1e3)

(doc hecto 'type 'Number)
(doc hecto 'description "10^2 multiplier")
(define hecto 1e2)

(doc deca 'type 'Number)
(doc deca 'description "10^1 multiplier")
(define deca  1e1)

(doc deci 'type 'Number)
(doc deci 'description "10^-1 multiplier")
(define deci  1e-1)

(doc centi 'type 'Number)
(doc centi 'description "10^-2 multiplier")
(define centi 1e-2)

(doc milli 'type 'Number)
(doc milli 'description "10^-3 multiplier")
(define milli 1e-3)

(doc micro 'type 'Number)
(doc micro 'description "10^-6 multiplier")
(define micro 1e-6)

(doc nano 'type 'Number)
(doc nano 'description "10^-9 multiplier")
(define nano  1e-9)

(doc pico 'type 'Number)
(doc pico 'description "10^-12 multiplier")
(define pico  1e-12)

(doc femto 'type 'Number)
(doc femto 'description "10^-15 multiplier")
(define femto 1e-15)

(doc atto 'type 'Number)
(doc atto 'description "10^-18 multiplier")
(define atto  1e-18)

(doc zepto 'type 'Number)
(doc zepto 'description "10^-21 multiplier")
(define zepto 1e-21)

(doc yocto 'type 'Number)
(doc yocto 'description "10^-24 multiplier")
(define yocto 1e-24)

(doc 'note "Convenient prefixed units")

(define (kilometer x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in kilometers.")
  (meter (* x kilo)))

(define (centimeter x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in centimeters.")
  (meter (* x centi)))

(define (millimeter x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in millimeters.")
  (meter (* x milli)))

(define (micrometer x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in micrometers.")
  (meter (* x micro)))

(define (nanometer x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a length quantity in nanometers.")
  (meter (* x nano)))

(define (millisecond x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a time quantity in milliseconds.")
  (second (* x milli)))

(define (microsecond x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a time quantity in microseconds.")
  (second (* x micro)))

(define (nanosecond x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a time quantity in nanoseconds.")
  (second (* x nano)))

(define (gram x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a mass quantity in grams.")
  (kilogram (* x milli)))

(define (milligram x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a mass quantity in milligrams.")
  (kilogram (* x micro)))

(define (kilowatt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a power quantity in kilowatts.")
  (watt (* x kilo)))

(define (megawatt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a power quantity in megawatts.")
  (watt (* x mega)))

(define (gigawatt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a power quantity in gigawatts.")
  (watt (* x giga)))

(define (kilojoule x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create an energy quantity in kilojoules.")
  (joule (* x kilo)))

(define (megajoule x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create an energy quantity in megajoules.")
  (joule (* x mega)))

(define (milliampere x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a current quantity in milliamperes.")
  (ampere (* x milli)))

(define (microampere x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a current quantity in microamperes.")
  (ampere (* x micro)))

(define (kilohertz x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a frequency quantity in kilohertz.")
  (hertz (* x kilo)))

(define (megahertz x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a frequency quantity in megahertz.")
  (hertz (* x mega)))

(define (gigahertz x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a frequency quantity in gigahertz.")
  (hertz (* x giga)))

(define (kilovolt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a voltage quantity in kilovolts.")
  (volt (* x kilo)))

(define (millivolt x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a voltage quantity in millivolts.")
  (volt (* x milli)))

(define (kiloohm x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a resistance quantity in kiloohms.")
  (ohm (* x kilo)))

(define (megaohm x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a resistance quantity in megaohms.")
  (ohm (* x mega)))

(doc 'section 'dimension-display)

(define (dim->string d)
  (doc 'type '(-> Dimension String))
  (doc 'description "Display dimension in standard notation.")
  (let ([names '("m" "s" "kg" "A" "K" "mol" "cd")]
        [exps d])
       (let loop ([i 0] [parts '()])
            (if (= i 7)
                (if (null? parts)
                    "1"
                    (string-join (reverse parts) "."))
                (let ([exp (list-ref exps i)]
                      [name (list-ref names i)])
                     (loop (+ i 1)
                           (cond
                            [(= exp 0) parts]
                            [(= exp 1) (cons name parts)]
                            [else (cons (string-append name "^" (number->string exp))
                                        parts)])))))))

(define (qty->string q)
  (doc 'type '(-> Quantity String))
  (doc 'description "Display quantity with units.")
  (string-append (number->string (qty-value q))
                 " "
                 (dim->string (qty-dim q))))

(doc 'section 'dimensionless-quantities)

(define (scalar x)
  (doc 'type '(-> Number Quantity))
  (doc 'description "Create a dimensionless quantity.")
  (make-qty x dim-one))

(define (qty-dimensionless? q)
  (doc 'type '(-> Quantity Boolean))
  (doc 'description "Check if quantity is dimensionless.")
  (dim=? (qty-dim q) dim-one))

(define (qty->number q)
  (doc 'type '(-> Quantity (Result Number Error)))
  (doc 'description "Extract number from dimensionless quantity.")
  (if (qty-dimensionless? q)
      (qty-value q)
      (error 'qty->number "quantity has dimension")))

(doc 'section 'unit-conversions)

(define (convert q unit)
  (doc 'type '(-> Quantity Quantity Number))
  (doc 'description "Convert quantity to different unit (same dimension). Returns the numeric factor.")
  (doc 'note "Example: (convert (kilometer 1) (meter 1)) → 1000")
  (if (dim=? (qty-dim q) (qty-dim unit))
      (/ (qty-value q) (qty-value unit))
      (error 'convert "dimension mismatch")))

(define (in-units q unit-fn)
  (doc 'type '(-> Quantity (-> Number Quantity) Number))
  (doc 'description "Express quantity in given units.")
  (doc 'note "Example: (in-units (kilometer 5) meter) → 5000")
  (let ([unit (unit-fn 1)])
       (if (dim=? (qty-dim q) (qty-dim unit))
           (/ (qty-value q) (qty-value unit))
           (error 'in-units "dimension mismatch"))))
