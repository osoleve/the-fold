(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/symbolic-metrics.ss")

(doc 'module 'test-symbolic-metrics)
(doc 'description "Tests for symbolic metric derivatives")

;;; Helper: compare two Christoffel tensors within tolerance
(define (christoffel-approx-equal? gamma1 gamma2 tol)
  (let* ([n (vector-length gamma1)]
         [equal? #t])
    (do ([k 0 (+ k 1)])
        ((or (not equal?) (= k n)) equal?)
        (do ([i 0 (+ i 1)])
            ((or (not equal?) (= i n)))
            (do ([j 0 (+ j 1)])
                ((or (not equal?) (= j n)))
                (let* ([v1 (christoffel-ref gamma1 k i j)]
                       [v2 (christoffel-ref gamma2 k i j)]
                       [diff (abs (- v1 v2))])
                  (when (> diff tol)
                        (set! equal? #f))))))))

;;; Helper: print Christoffel tensor
(define (print-christoffel gamma label)
  (let ([n (vector-length gamma)])
    (printf "~a:\n" label)
    (do ([k 0 (+ k 1)])
        ((= k n))
        (printf "  Γ^~a:\n" k)
        (do ([i 0 (+ i 1)])
            ((= i n))
            (printf "    ")
            (do ([j 0 (+ j 1)])
                ((= j n))
                (printf "~8,4f " (christoffel-ref gamma k i j)))
            (printf "\n")))))

(test-group "polar-symbolic"
  (define-test "polar symbolic matches numerical"
    (let* ([chart (make-polar-chart)]
           [m-num (make-polar-metric chart)]
           [m-sym (make-polar-metric-exact chart)]
           [coords (vector 2.5 1.0)]  ; r=2.5, θ=1
           [gamma-num (christoffel-symbols m-num coords)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)])
      (assert-true (christoffel-approx-equal? gamma-num gamma-sym 1e-5))))

  (define-test "polar pre-computed matches symbolic"
    (let* ([chart (make-polar-chart)]
           [m-sym (make-polar-metric-exact chart)]
           [coords (vector 3.0 0.7)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)]
           [gamma-pre (polar-christoffel-exact coords)])
      (assert-true (christoffel-approx-equal? gamma-sym gamma-pre 1e-10))))

  (define-test "polar Christoffel values are correct"
    ;; At r=2: Γ^r_{θθ} = -r = -2, Γ^θ_{rθ} = Γ^θ_{θr} = 1/r = 0.5
    (let* ([coords (vector 2.0 1.0)]
           [gamma (polar-christoffel-exact coords)])
      (assert-true (< (abs (- (christoffel-ref gamma 0 1 1) -2.0)) 1e-10))
      (assert-true (< (abs (- (christoffel-ref gamma 1 0 1) 0.5)) 1e-10))
      (assert-true (< (abs (- (christoffel-ref gamma 1 1 0) 0.5)) 1e-10)))))

(test-group "spherical-symbolic"
  (define-test "spherical symbolic matches numerical"
    (let* ([chart (make-spherical-chart)]
           [m-num (make-spherical-metric chart)]
           [m-sym (make-spherical-metric-exact chart)]
           [coords (vector 2.0 1.0 0.5)]  ; r=2, θ=1, φ=0.5
           [gamma-num (christoffel-symbols m-num coords)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)])
      (assert-true (christoffel-approx-equal? gamma-num gamma-sym 1e-4))))

  (define-test "spherical pre-computed matches symbolic"
    (let* ([chart (make-spherical-chart)]
           [m-sym (make-spherical-metric-exact chart)]
           [coords (vector 3.0 0.8 1.2)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)]
           [gamma-pre (spherical-christoffel-exact coords)])
      (assert-true (christoffel-approx-equal? gamma-sym gamma-pre 1e-10))))

  (define-test "spherical Christoffel key values"
    ;; At r=2, θ=π/4: various known values
    (let* ([r 2.0]
           [theta (/ 3.14159265 4)]  ; π/4
           [coords (vector r theta 0.0)]
           [gamma (spherical-christoffel-exact coords)]
           ;; Γ^r_{θθ} = -r
           [expected-r-tt (- r)]
           ;; Γ^θ_{rθ} = 1/r
           [expected-t-rt (/ 1.0 r)]
           ;; Γ^θ_{φφ} = -sinθ·cosθ
           [expected-t-pp (- (* (sin theta) (cos theta)))])
      (assert-true (< (abs (- (christoffel-ref gamma 0 1 1) expected-r-tt)) 1e-10))
      (assert-true (< (abs (- (christoffel-ref gamma 1 0 1) expected-t-rt)) 1e-10))
      (assert-true (< (abs (- (christoffel-ref gamma 1 2 2) expected-t-pp)) 1e-10)))))

(test-group "cylindrical-symbolic"
  (define-test "cylindrical symbolic matches numerical"
    (let* ([chart (make-cylindrical-chart)]
           [m-num (make-cylindrical-metric chart)]
           [m-sym (make-cylindrical-metric-exact chart)]
           [coords (vector 2.0 1.0 3.0)]  ; ρ=2, φ=1, z=3
           [gamma-num (christoffel-symbols m-num coords)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)])
      (assert-true (christoffel-approx-equal? gamma-num gamma-sym 1e-5))))

  (define-test "cylindrical pre-computed matches symbolic"
    (let* ([chart (make-cylindrical-chart)]
           [m-sym (make-cylindrical-metric-exact chart)]
           [coords (vector 3.0 0.8 1.5)]
           [gamma-sym (christoffel-symbols-exact m-sym coords)]
           [gamma-pre (cylindrical-christoffel-exact coords)])
      (assert-true (christoffel-approx-equal? gamma-sym gamma-pre 1e-10))))

  (define-test "cylindrical Christoffel values are correct"
    ;; At ρ=2: Γ^ρ_{φφ} = -ρ = -2, Γ^φ_{ρφ} = Γ^φ_{φρ} = 1/ρ = 0.5
    (let* ([coords (vector 2.0 1.0 3.0)]
           [gamma (cylindrical-christoffel-exact coords)])
      ;; Γ^ρ_{φφ} = -ρ
      (assert-true (< (abs (- (christoffel-ref gamma 0 1 1) -2.0)) 1e-10))
      ;; Γ^φ_{ρφ} = 1/ρ
      (assert-true (< (abs (- (christoffel-ref gamma 1 0 1) 0.5)) 1e-10))
      ;; Γ^φ_{φρ} = 1/ρ
      (assert-true (< (abs (- (christoffel-ref gamma 1 1 0) 0.5)) 1e-10))
      ;; All z-related components should be zero
      (assert-true (< (abs (christoffel-ref gamma 2 0 0)) 1e-10))
      (assert-true (< (abs (christoffel-ref gamma 2 1 1)) 1e-10))
      (assert-true (< (abs (christoffel-ref gamma 2 2 2)) 1e-10)))))

(test-group "euclidean-symbolic"
  (define-test "euclidean has zero Christoffels"
    (let* ([chart (make-identity-chart 'euclidean 3)]
           [m-sym (make-euclidean-metric-exact chart)]
           [coords (vector 1.0 2.0 3.0)]
           [gamma (christoffel-symbols-exact m-sym coords)]
           [all-zero #t])
      (do ([k 0 (+ k 1)])
          ((or (not all-zero) (= k 3)))
          (do ([i 0 (+ i 1)])
              ((or (not all-zero) (= i 3)))
              (do ([j 0 (+ j 1)])
                  ((or (not all-zero) (= j 3)))
                  (when (> (abs (christoffel-ref gamma k i j)) 1e-15)
                        (set! all-zero #f)))))
      (assert-true all-zero))))

(test-group "unified-interface"
  (define-test "christoffel-symbols* uses exact for symbolic"
    (let* ([chart (make-polar-chart)]
           [m-sym (make-polar-metric-exact chart)]
           [coords (vector 2.0 1.0)]
           ;; christoffel-symbols* should detect symbolic and use exact
           [gamma (christoffel-symbols* m-sym coords)]
           [gamma-pre (polar-christoffel-exact coords)])
      (assert-true (christoffel-approx-equal? gamma gamma-pre 1e-10))))

  (define-test "christoffel-symbols* falls back to numerical"
    (let* ([chart (make-polar-chart)]
           [m-num (make-polar-metric chart)]
           [coords (vector 2.0 1.0)]
           ;; Should use numerical since not metric-symbolic
           [gamma (christoffel-symbols* m-num coords)]
           [gamma-pre (polar-christoffel-exact coords)])
      (assert-true (christoffel-approx-equal? gamma gamma-pre 1e-4)))))

(run-all-tests)
