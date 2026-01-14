;;; core/fp/measure/test-units.ss — Tests for Units of Measure
;;;
;;; NOTE: Run from project root: scheme --script core/fp/measure/test-units.ss

(load "core/test-framework.ss")
(load "lattice/fp/measure/units.ss")

(display "\n")
(display "====\n")
(display "         UNITS OF MEASURE TESTS\n")
(display "====\n")

;;; ====
;;; Dimension Tests
;;; ====

(test-group dimensions
            (define-test make-dim-test
              (let ([d (make-dim 1 -2 1 0 0 0 0)])
                   (assert-equal 1 (dim-length d))
                   (assert-equal -2 (dim-time d))
                   (assert-equal 1 (dim-mass d))
                   (assert-equal 0 (dim-current d))))
            
            (define-test dim?-test
              (assert-true (dim? (make-dim 0 0 0 0 0 0 0)))
              (assert-true (dim? dim-length-base))
              (assert-false (dim? 42))
              (assert-false (dim? '(1 2 3))))
            
            (define-test dim=?-test
              (assert-true (dim=? dim-one dim-one))
              (assert-true (dim=? dim-length-base dim-length-base))
              (assert-false (dim=? dim-length-base dim-time-base)))
            
            (define-test base-dimensions-test
              (assert-equal '(0 0 0 0 0 0 0) dim-one)
              (assert-equal '(1 0 0 0 0 0 0) dim-length-base)
              (assert-equal '(0 1 0 0 0 0 0) dim-time-base)
              (assert-equal '(0 0 1 0 0 0 0) dim-mass-base)
              (assert-equal '(0 0 0 1 0 0 0) dim-current-base)
              (assert-equal '(0 0 0 0 1 0 0) dim-temperature-base)
              (assert-equal '(0 0 0 0 0 1 0) dim-amount-base)
              (assert-equal '(0 0 0 0 0 0 1) dim-luminosity-base)))

;;; ====
;;; Dimension Arithmetic Tests
;;; ====

(test-group dimension-arithmetic
            (define-test dim*-test
              ;; velocity = length / time = length * time^-1
              (let ([velocity (dim* dim-length-base (dim-inv dim-time-base))])
                   (assert-equal '(1 -1 0 0 0 0 0) velocity)))
            
            (define-test dim/-test
              ;; velocity = length / time
              (let ([velocity (dim/ dim-length-base dim-time-base)])
                   (assert-equal '(1 -1 0 0 0 0 0) velocity)))
            
            (define-test dim-pow-test
              ;; area = length^2
              (let ([area (dim-pow dim-length-base 2)])
                   (assert-equal '(2 0 0 0 0 0 0) area))
              ;; volume = length^3
              (let ([volume (dim-pow dim-length-base 3)])
                   (assert-equal '(3 0 0 0 0 0 0) volume)))
            
            (define-test dim-sqrt-test
              ;; sqrt(area) = length
              (let ([area (dim-pow dim-length-base 2)])
                   (assert-equal dim-length-base (dim-sqrt area)))
              ;; sqrt of odd exponent fails
              (assert-false (dim-sqrt dim-length-base)))
            
            (define-test dim-inv-test
              (assert-equal '(0 -1 0 0 0 0 0) (dim-inv dim-time-base))
              (assert-equal '(-1 0 0 0 0 0 0) (dim-inv dim-length-base)))
            
            (define-test derived-dimensions-test
              ;; Force = M * L / T^2
              (assert-equal '(1 -2 1 0 0 0 0) dim-force)
              ;; Energy = M * L^2 / T^2
              (assert-equal '(2 -2 1 0 0 0 0) dim-energy)
              ;; Velocity = L / T
              (assert-equal '(1 -1 0 0 0 0 0) dim-velocity)))

;;; ====
;;; Quantity Tests
;;; ====

(test-group quantities
            (define-test make-qty-test
              (let ([q (make-qty 5 dim-length-base)])
                   (assert-equal 5 (qty-value q))
                   (assert-equal dim-length-base (qty-dim q))))
            
            (define-test qty?-test
              (assert-true (qty? (meter 5)))
              (assert-true (qty? (second 10)))
              (assert-false (qty? 42))
              (assert-false (qty? '(5 . wrong))))
            
            (define-test base-units-test
              (let ([m (meter 5)]
                    [s (second 10)]
                    [kg (kilogram 2)]
                    [a (ampere 1)]
                    [k (kelvin 300)]
                    [mol (mole 6.02e23)]
                    [cd (candela 100)])
                   (assert-equal dim-length-base (qty-dim m))
                   (assert-equal dim-time-base (qty-dim s))
                   (assert-equal dim-mass-base (qty-dim kg))
                   (assert-equal dim-current-base (qty-dim a))
                   (assert-equal dim-temperature-base (qty-dim k))
                   (assert-equal dim-amount-base (qty-dim mol))
                   (assert-equal dim-luminosity-base (qty-dim cd)))))

;;; ====
;;; Quantity Arithmetic Tests
;;; ====

(test-group quantity-arithmetic
            (define-test qty+-test
              (let ([q (qty+ (meter 3) (meter 5))])
                   (assert-equal 8 (qty-value q))
                   (assert-equal dim-length-base (qty-dim q))))
            
            (define-test qty--test
              (let ([q (qty- (meter 10) (meter 3))])
                   (assert-equal 7 (qty-value q))
                   (assert-equal dim-length-base (qty-dim q))))
            
            (define-test qty*-test
              ;; length * time = length*time
              (let ([q (qty* (meter 5) (second 2))])
                   (assert-equal 10 (qty-value q))
                   (assert-equal '(1 1 0 0 0 0 0) (qty-dim q)))
              ;; area = length * length
              (let ([area (qty* (meter 3) (meter 4))])
                   (assert-equal 12 (qty-value area))
                   (assert-equal dim-area (qty-dim area))))
            
            (define-test qty/-test
              ;; velocity = length / time
              (let ([v (qty/ (meter 100) (second 10))])
                   (assert-equal 10 (qty-value v))
                   (assert-equal dim-velocity (qty-dim v))))
            
            (define-test qty-scale-test
              (let ([q (qty-scale (meter 5) 3)])
                   (assert-equal 15 (qty-value q))
                   (assert-equal dim-length-base (qty-dim q))))
            
            (define-test qty-pow-test
              (let ([area (qty-pow (meter 5) 2)])
                   (assert-equal 25 (qty-value area))
                   (assert-equal dim-area (qty-dim area))))
            
            (define-test qty-sqrt-test
              (let* ([area (qty-pow (meter 9) 2)]
                     [side (qty-sqrt area)])
                    (assert-equal 9 (qty-value side))
                    (assert-equal dim-length-base (qty-dim side))))
            
            (define-test qty-neg-test
              (let ([q (qty-neg (meter 5))])
                   (assert-equal -5 (qty-value q))
                   (assert-equal dim-length-base (qty-dim q))))
            
            (define-test qty-abs-test
              (let ([q (qty-abs (meter -5))])
                   (assert-equal 5 (qty-value q)))))

;;; ====
;;; Quantity Comparison Tests
;;; ====

(test-group quantity-comparison
            (define-test qty=?-test
              (assert-true (qty=? (meter 5) (meter 5)))
              (assert-false (qty=? (meter 5) (meter 6))))
            
            (define-test qty<?-test
              (assert-true (qty<? (meter 3) (meter 5)))
              (assert-false (qty<? (meter 5) (meter 3))))
            
            (define-test qty>?-test
              (assert-true (qty>? (meter 5) (meter 3)))
              (assert-false (qty>? (meter 3) (meter 5))))
            
            (define-test qty<=?-test
              (assert-true (qty<=? (meter 3) (meter 5)))
              (assert-true (qty<=? (meter 5) (meter 5)))
              (assert-false (qty<=? (meter 6) (meter 5))))
            
            (define-test qty>=?-test
              (assert-true (qty>=? (meter 5) (meter 3)))
              (assert-true (qty>=? (meter 5) (meter 5)))
              (assert-false (qty>=? (meter 3) (meter 5)))))

;;; ====
;;; Derived Units Tests
;;; ====

(test-group derived-units
            (define-test newton-test
              (let ([f (newton 10)])
                   (assert-equal 10 (qty-value f))
                   (assert-equal dim-force (qty-dim f))))
            
            (define-test joule-test
              (let ([e (joule 100)])
                   (assert-equal 100 (qty-value e))
                   (assert-equal dim-energy (qty-dim e))))
            
            (define-test watt-test
              (let ([p (watt 60)])
                   (assert-equal 60 (qty-value p))
                   (assert-equal dim-power (qty-dim p))))
            
            (define-test hertz-test
              (let ([f (hertz 440)])
                   (assert-equal 440 (qty-value f))
                   (assert-equal dim-frequency (qty-dim f))))
            
            (define-test volt-test
              (let ([v (volt 220)])
                   (assert-equal 220 (qty-value v))
                   (assert-equal dim-voltage (qty-dim v))))
            
            (define-test ohm-test
              (let ([r (ohm 1000)])
                   (assert-equal 1000 (qty-value r))
                   (assert-equal dim-resistance (qty-dim r)))))

;;; ====
;;; SI Prefix Tests
;;; ====

(test-group si-prefixes
            (define-test kilo-prefix-test
              (assert-equal 1e3 kilo)
              (let ([km (kilometer 5)])
                   ;; 5 km = 5000 m (stored in base unit)
                   (assert-equal 5000.0 (qty-value km))))
            
            (define-test milli-prefix-test
              (assert-equal 1e-3 milli)
              (let ([mm (millimeter 100)])
                   ;; 100 mm = 0.1 m
                   (assert-equal 0.1 (qty-value mm))))
            
            (define-test prefixed-units-test
              ;; 100 cm = 1 m
              (assert-equal 1.0 (qty-value (centimeter 100)))
              ;; 1 um = 1e-6 m
              (assert-equal 1e-6 (qty-value (micrometer 1)))
              ;; 1 ms = 1e-3 s
              (assert-equal 1e-3 (qty-value (millisecond 1)))
              ;; 1 g = 1e-3 kg
              (assert-equal 1e-3 (qty-value (gram 1)))
              ;; 1 kW = 1000 W
              (assert-equal 1000.0 (qty-value (kilowatt 1)))
              ;; 1 MHz = 1e6 Hz
              (assert-equal 1e6 (qty-value (megahertz 1)))))

;;; ====
;;; Physics Calculations Tests
;;; ====

(test-group physics-calculations
            ;; F = m * a
            (define-test force-calculation-test
              (let* ([mass (kilogram 10)]
                     [accel (qty/ (meter 9.8) (qty-pow (second 1) 2))]
                     [force (qty* mass accel)])
                    (assert-equal 98.0 (qty-value force))
                    (assert-equal dim-force (qty-dim force))))
            
            ;; E = m * c^2 (simplified)
            (define-test energy-calculation-test
              (let* ([mass (kilogram 1)]
                     [c (qty/ (meter 3e8) (second 1))]
                     [c-squared (qty-pow c 2)]
                     [energy (qty* mass c-squared)])
                    (assert-equal 9e16 (qty-value energy))
                    (assert-equal dim-energy (qty-dim energy))))
            
            ;; P = V * I (Ohm's law)
            (define-test power-calculation-test
              (let* ([voltage (volt 220)]
                     [current (ampere 2)]
                     [power (qty* voltage current)])
                    (assert-equal 440 (qty-value power))
                    (assert-equal dim-power (qty-dim power))))
            
            ;; v = d / t
            (define-test velocity-calculation-test
              (let* ([distance (kilometer 100)]
                     [time (second 3600)]  ; 1 hour
                     [velocity (qty/ distance time)])
                    ;; 100 km/h = 27.78 m/s
                    (assert-true (< (abs (- (qty-value velocity) 27.777777)) 0.001))
                    (assert-equal dim-velocity (qty-dim velocity)))))

;;; ====
;;; Dimensionless Tests
;;; ====

(test-group dimensionless
            (define-test scalar-test
              (let ([s (scalar 3.14)])
                   (assert-equal 3.14 (qty-value s))
                   (assert-true (qty-dimensionless? s))))
            
            (define-test qty-dimensionless?-test
              (assert-true (qty-dimensionless? (scalar 1)))
              (assert-false (qty-dimensionless? (meter 1))))
            
            (define-test qty->number-test
              (assert-equal 42 (qty->number (scalar 42)))))

;;; ====
;;; Unit Conversion Tests
;;; ====

(test-group unit-conversion
            (define-test convert-test
              ;; 1 km = 1000 m
              (assert-equal 1000.0 (convert (kilometer 1) (meter 1)))
              ;; 5 km = 5000 m
              (assert-equal 5000.0 (convert (kilometer 5) (meter 1))))
            
            (define-test in-units-test
              ;; 5 km in meters
              (assert-equal 5000.0 (in-units (kilometer 5) meter))
              ;; 1000 m in kilometers
              (assert-equal 1.0 (in-units (meter 1000) kilometer))
              ;; 3600 s in hours (3600 s = 1 h)
              (let ([hour (lambda (x) (second (* x 3600)))])
                   (assert-equal 1 (in-units (second 3600) hour)))))

;;; ====
;;; Display Tests
;;; ====

(test-group display
            (define-test dim->string-test
              (assert-equal "1" (dim->string dim-one))
              (assert-equal "m" (dim->string dim-length-base))
              (assert-equal "s" (dim->string dim-time-base))
              ;; Note: order changed from "m.s^-1" due to using "." separator
              (assert-equal "m.s^-1" (dim->string dim-velocity))
              (assert-equal "m.s^-2.kg" (dim->string dim-force)))
            
            (define-test qty->string-test
              (assert-equal "5 m" (qty->string (meter 5)))
              (assert-equal "10 s" (qty->string (second 10)))
              (assert-equal "100 1" (qty->string (scalar 100)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All units of measure tests passed.\n")
    (display "\n[FAILURE] Some units of measure tests failed.\n"))
