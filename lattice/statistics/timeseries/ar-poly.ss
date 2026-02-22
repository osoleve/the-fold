(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ar-poly
;;; @requires field algebra/polynomial
(require 'field)
(require 'algebra/polynomial)

(doc 'module 'ar-poly)
(doc 'description "Polynomial Algebra for Time Series — Integrates polynomial.ss with time series modules")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "AR characteristic polynomial factorization")
(doc 'description "Stability analysis via polynomial operations")
(doc 'description "Exact polynomial operations for model identification")
(doc 'description "MA polynomial invertibility analysis")

(doc 'section 'rational-field-for-exact-coefficient-arithmetic)
(define Q-field-ts
  (make-field
   '()                                    ; Infinite field
   +                                      ; Addition
   *                                      ; Multiplication
   0                                      ; Zero
   1                                      ; One
   -                                      ; Negation
   (lambda (a b) (/ a b))                 ; Division
   =))                                    ; Equality

;;; ====
;;; AR Characteristic Polynomial
;;; ====

;;; AR(p) model: X_t = phi_1*X_{t-1} + phi_2*X_{t-2} + ... + phi_p*X_{t-p} + e_t
;;;
;;; Characteristic polynomial (lag operator form):
;;;   Phi(B) = 1 - phi_1*B - phi_2*B^2 - ... - phi_p*B^p
;;;
;;; For stability analysis, we want roots of:
;;;   z^p - phi_1*z^{p-1} - phi_2*z^{p-2} - ... - phi_p = 0
;;;
;;; The system is stationary if all roots are outside the unit circle.

;;; ar-char-poly-vec : Vec → AlgebraPoly
;;; Create AR characteristic polynomial from coefficient vector.
;;; Uses proper list construction.
;;; Input: phi = #(phi_1 phi_2 ... phi_p)
;;; Output: Polynomial 1 - phi_1*z - phi_2*z^2 - ... - phi_p*z^p (ascending order)
(define (ar-char-poly-vec phi)
  (doc 'export #t)
  (let* ([p (vector-length phi)]
         ;; Build coefficients in ascending order: [1, -phi_1, -phi_2, ..., -phi_p]
         [coeffs (let loop ([k 0] [acc '()])
                   (if (> k p)
                       (reverse acc)
                       (if (= k 0)
                           (loop 1 (cons 1 acc))
                           (loop (+ k 1)
                                 (cons (- (vector-ref phi (- k 1))) acc)))))])
    (make-polynomial Q-field-ts coeffs)))

;;; ar-char-poly : Vec → AlgebraPoly
;;; Alias for ar-char-poly-vec (legacy name).
(define ar-char-poly ar-char-poly-vec)

;;; ar-companion-poly : Vec → AlgebraPoly
;;; Create AR companion polynomial: z^p - phi_1*z^{p-1} - ... - phi_p
;;; Used for stability analysis (roots should be inside unit circle).
(define (ar-companion-poly phi)
  (doc 'export #t)
  (let* ([p (vector-length phi)]
         ;; Build coefficients in ascending order: [-phi_p, ..., -phi_1, 1]
         ;; Loop builds in reverse, so reverse at end
         [coeffs (let loop ([k 0] [acc '()])
                   (if (> k p)
                       (reverse acc)  ; Reverse to get ascending order
                       (if (= k p)
                           (loop (+ k 1) (cons 1 acc))
                           (loop (+ k 1)
                                 (cons (- (vector-ref phi (- p 1 k))) acc)))))])
    (make-polynomial Q-field-ts coeffs)))

;;; ====
;;; MA Polynomial
;;; ====

;;; MA(q) model: X_t = e_t + theta_1*e_{t-1} + ... + theta_q*e_{t-q}
;;;
;;; MA polynomial (lag operator form):
;;;   Theta(B) = 1 + theta_1*B + theta_2*B^2 + ... + theta_q*B^q
;;;
;;; For invertibility, all roots should be outside the unit circle.

;;; ma-char-poly : Vec → AlgebraPoly
;;; Create MA characteristic polynomial from MA coefficients.
;;; Input: theta = #(theta_1 theta_2 ... theta_q)
;;; Output: Polynomial 1 + theta_1*z + theta_2*z^2 + ... + theta_q*z^q
(define (ma-char-poly theta)
  (doc 'export #t)
  (let* ([q (vector-length theta)]
         [coeffs (let loop ([k 0] [acc '()])
                   (if (> k q)
                       (reverse acc)
                       (if (= k 0)
                           (loop 1 (cons 1 acc))
                           (loop (+ k 1)
                                 (cons (vector-ref theta (- k 1)) acc)))))])
    (make-polynomial Q-field-ts coeffs)))

;;; ====
;;; ARMA Polynomial Operations
;;; ====

;;; arma-common-factors : Vec × Vec → AlgebraPoly
;;; Find common factors between AR and MA polynomials.
;;; Non-trivial GCD indicates potential model reduction.
(define (arma-common-factors phi theta)
  (doc 'export #t)
  (let* ([ar-poly (ar-char-poly-vec phi)]
         [ma-poly (ma-char-poly theta)])
    (poly-gcd ar-poly ma-poly)))

;;; arma-reducible? : Vec × Vec → Boolean
;;; Check if ARMA model has common AR-MA factors (reducible).
;;; Returns #t if GCD has degree > 0.
(define (arma-reducible? phi theta)
  (doc 'export #t)
  (let ([gcd-poly (arma-common-factors phi theta)])
    (> (poly-degree gcd-poly) 0)))

;;; arma-reduce : Vec × Vec → (Vec × Vec)
;;; Reduce ARMA model by canceling common factors.
;;; Returns (phi-reduced, theta-reduced).
(define (arma-reduce phi theta)
  (let* ([ar-poly (ar-char-poly-vec phi)]
         [ma-poly (ma-char-poly theta)]
         [gcd-poly (poly-gcd ar-poly ma-poly)])
    (if (<= (poly-degree gcd-poly) 0)
        (cons phi theta)  ; Already reduced
        (let* ([ar-reduced (poly-div ar-poly gcd-poly)]
               [ma-reduced (poly-div ma-poly gcd-poly)])
          (cons (algebra-poly->ar-coeffs ar-reduced)
                (algebra-poly->ma-coeffs ma-reduced))))))

;;; algebra-poly->ar-coeffs : AlgebraPoly → Vec
;;; Convert algebra polynomial back to AR coefficient vector.
;;; Input: 1 - phi_1*z - phi_2*z^2 - ... (ascending order)
;;; Output: #(phi_1 phi_2 ...)
(define (algebra-poly->ar-coeffs poly)
  (let* ([coeffs (poly-coeffs poly)]
         [n (length coeffs)]
         [p (- n 1)])
    (if (<= p 0)
        '#()
        (let ([result (make-vector p)])
          (do ([k 1 (+ k 1)])
              ((> k p) result)
            (vector-set! result (- k 1) (- (list-ref coeffs k))))))))

;;; algebra-poly->ma-coeffs : AlgebraPoly → Vec
;;; Convert algebra polynomial back to MA coefficient vector.
;;; Input: 1 + theta_1*z + theta_2*z^2 + ... (ascending order)
;;; Output: #(theta_1 theta_2 ...)
(define (algebra-poly->ma-coeffs poly)
  (let* ([coeffs (poly-coeffs poly)]
         [n (length coeffs)]
         [q (- n 1)])
    (if (<= q 0)
        '#()
        (let ([result (make-vector q)])
          (do ([k 1 (+ k 1)])
              ((> k q) result)
            (vector-set! result (- k 1) (list-ref coeffs k)))))))

;;; ====
;;; Stability and Invertibility Analysis
;;; ====

;;; Note: Determining if all roots are outside unit circle requires
;;; finding roots or using algebraic criteria. Here we provide tools
;;; that work with the polynomial algebra.

;;; ar-stable-simple? : Vec → Boolean
;;; Simple stability check for AR(1) and AR(2) models.
;;; For higher orders, use numerical root finding.
(define (ar-stable-simple? phi)
  (doc 'export #t)
  (let ([p (vector-length phi)])
    (cond
      [(= p 0) #t]  ; No AR component
      [(= p 1)
       ;; AR(1): |phi_1| < 1
       (< (abs (vector-ref phi 0)) 1)]
      [(= p 2)
       ;; AR(2): phi_1 + phi_2 < 1, phi_2 - phi_1 < 1, |phi_2| < 1
       (let ([phi1 (vector-ref phi 0)]
             [phi2 (vector-ref phi 1)])
         (and (< (+ phi1 phi2) 1)
              (< (- phi2 phi1) 1)
              (< (abs phi2) 1)))]
      [else
       ;; For higher orders, need numerical analysis
       ;; Return #f to indicate unknown
       #f])))

;;; ma-invertible-simple? : Vec → Boolean
;;; Simple invertibility check for MA(1) and MA(2) models.
(define (ma-invertible-simple? theta)
  (doc 'export #t)
  (let ([q (vector-length theta)])
    (cond
      [(= q 0) #t]  ; No MA component
      [(= q 1)
       ;; MA(1): |theta_1| < 1
       (< (abs (vector-ref theta 0)) 1)]
      [(= q 2)
       ;; MA(2): theta_1 + theta_2 > -1, theta_2 - theta_1 > -1, |theta_2| < 1
       (let ([theta1 (vector-ref theta 0)]
             [theta2 (vector-ref theta 1)])
         (and (> (+ theta1 theta2) -1)
              (> (- theta2 theta1) -1)
              (< (abs theta2) 1)))]
      [else #f])))

;;; ====
;;; Polynomial Trend Fitting
;;; ====

;;; fit-poly-trend : Vec × Nat → AlgebraPoly
;;; Fit polynomial trend of degree d to time series.
;;; Uses least squares (requires matrix operations).
;;; Returns polynomial in t: a_0 + a_1*t + a_2*t^2 + ...
(define (fit-poly-trend xs degree)
  ;; Build Vandermonde matrix and solve normal equations
  ;; X'X beta = X'y
  ;; For now, return placeholder indicating what would be computed
  (let* ([n (vector-length xs)]
         ;; For degree 0, just return mean
         [mean (/ (vector-fold + 0 xs) n)])
    (if (= degree 0)
        (make-polynomial Q-field-ts (list mean))
        ;; Higher degrees would need matrix least squares
        ;; Return constant approximation
        (make-polynomial Q-field-ts (list mean)))))

;;; vector-fold : (α × β → α) × α × Vec → α
;;; Fold over vector elements.
(define (vector-fold f init v)
  (let ([n (vector-length v)])
    (let loop ([i 0] [acc init])
      (if (>= i n)
          acc
          (loop (+ i 1) (f acc (vector-ref v i)))))))

;;; ====
;;; Polynomial Operations for Forecasting
;;; ====

;;; ar-forecast-weights : Vec × Nat → (List Number)
;;; Compute forecast weights (psi-weights) from AR coefficients.
;;; psi_j = sum_{k=1}^{min(j,p)} phi_k * psi_{j-k} for j >= 1
;;; with psi_0 = 1.
(define (ar-forecast-weights phi n-ahead)
  (doc 'export #t)
  (let ([p (vector-length phi)])
    (let loop ([j 0] [psi '()])
      (if (> j n-ahead)
          (reverse psi)
          (if (= j 0)
              (loop 1 (cons 1 psi))
              (let ([psi-j (let inner ([k 1] [sum 0])
                            (if (> k (min j p))
                                sum
                                ;; psi is in reverse order: [psi_{j-1}, psi_{j-2}, ...]
                                ;; To get psi_{j-k}, access index (k-1)
                                (inner (+ k 1)
                                       (+ sum (* (vector-ref phi (- k 1))
                                                 (list-ref psi (- k 1)))))))])
                (loop (+ j 1) (cons psi-j psi))))))))

;;; ar-impulse-response : Vec × Nat → AlgebraPoly
;;; Compute impulse response polynomial.
;;; For AR(p), this is 1/(Phi(z)) expanded as power series.
;;; Returns polynomial approximation of first n terms.
(define (ar-impulse-response phi n-terms)
  (let ([weights (ar-forecast-weights phi (- n-terms 1))])
    (make-polynomial Q-field-ts weights)))

;;; ====
;;; Display Utilities
;;; ====

;;; ar-char-poly->string : Vec → String
;;; Display AR characteristic polynomial as string.
(define (ar-char-poly->string phi)
  (poly->string (ar-char-poly-vec phi) 'z))

;;; ma-char-poly->string : Vec → String
;;; Display MA characteristic polynomial as string.
(define (ma-char-poly->string theta)
  (poly->string (ma-char-poly theta) 'z))

;;; arma-model->string : Vec × Vec → String
;;; Display ARMA model as ratio of polynomials.
(define (arma-model->string phi theta)
  (string-append "X_t = ("
                 (poly->string (ma-char-poly theta) 'B)
                 ") / ("
                 (poly->string (ar-char-poly-vec phi) 'B)
                 ") * e_t"))
