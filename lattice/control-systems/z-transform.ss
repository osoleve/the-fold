;;; lattice/control-systems/z-transform.ss — Discrete Transfer Functions
;;;
;;; Z-domain transfer functions for discrete-time control systems.
;;; A discrete transfer function H(z) = N(z)/D(z) represents the
;;; input-output relationship of a discrete-time LTI system.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/numeric/polynomial.ss
;;;   - lattice/control-systems/discrete-control.ss

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude numeric/polynomial control/discrete-control
(require 'prelude)
(require 'numeric/polynomial)
(require 'control/discrete-control)

(doc 'module 'z-transform)
(doc 'purity 'partial)
(doc 'description "Discrete transfer functions in z-domain for discrete-time LTI systems")
(doc 'layer 'lattice)

;;; ====
;;; Discrete Transfer Function Representation
;;; ====

;;; A discrete transfer function is: (dtf num den Ts)
;;; where:
;;;   num = numerator polynomial in z (Poly)
;;;   den = denominator polynomial in z (Poly)
;;;   Ts  = sample time
;;;
;;; H(z) = num(z) / den(z)
;;;
;;; Convention: Denominator is normalized (monic) by default.

;;; dtf? : Any → Boolean
;;; Check if value is a discrete transfer function.
(define (dtf? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'dtf)
       (= (length x) 4)
       (poly? (cadr x))
       (poly? (caddr x))
       (number? (cadddr x))))

;;; make-dtf : Poly × Poly × Number → DTF | Error
;;; Create discrete transfer function from numerator, denominator, and sample time.
;;; Normalizes denominator to monic form.
(define (make-dtf num den Ts)
  (doc 'export #t)
  (let ([lead (poly-leading den)])
       (if (zero? lead)
           '(error zero-denominator)
           (let ([scale (/ 1 lead)])
                (list 'dtf
                      (poly-scale num scale)
                      (poly-normalize den)
                      Ts)))))

;;; dtf-from-coeffs : Vector × Vector × Number → DTF
;;; Create discrete TF from coefficient vectors (descending powers of z).
(define (dtf-from-coeffs num-coeffs den-coeffs Ts)
  (doc 'export #t)
  (make-dtf (make-poly num-coeffs) (make-poly den-coeffs) Ts))

;;; dtf-from-lists : (List Number) × (List Number) × Number → DTF
;;; Create discrete TF from lists of coefficients.
(define (dtf-from-lists num-list den-list Ts)
  (doc 'export #t)
  (make-dtf (poly-from-list num-list) (poly-from-list den-list) Ts))

;;; dtf-num : DTF → Poly
;;; Get the numerator polynomial.
(define (dtf-num dtf)
  (doc 'export #t)
  (cadr dtf))

;;; dtf-den : DTF → Poly
;;; Get the denominator polynomial.
(define (dtf-den dtf)
  (doc 'export #t)
  (caddr dtf))

;;; dtf-Ts : DTF → Number
;;; Get the sample time.
(define (dtf-Ts dtf)
  (doc 'export #t)
  (cadddr dtf))

;;; dtf-order : DTF → Nat
;;; Get the order (degree of denominator).
(define (dtf-order dtf)
  (doc 'export #t)
  (poly-degree (dtf-den dtf)))

;;; dtf-relative-degree : DTF → Int
;;; Get the relative degree (deg(den) - deg(num)).
(define (dtf-relative-degree dtf)
  (doc 'export #t)
  (- (poly-degree (dtf-den dtf)) (poly-degree (dtf-num dtf))))

;;; dtf-proper? : DTF → Boolean
;;; Check if discrete TF is proper (causal).
(define (dtf-proper? dtf)
  (doc 'export #t)
  (>= (dtf-relative-degree dtf) 0))

;;; ====
;;; Poles and Zeros
;;; ====

;;; dtf-poles : DTF → (List Complex)
;;; Get the poles (roots of denominator).
(define (dtf-poles dtf)
  (doc 'export #t)
  (poly-roots (dtf-den dtf)))

;;; dtf-zeros : DTF → (List Complex)
;;; Get the zeros (roots of numerator).
(define (dtf-zeros dtf)
  (doc 'export #t)
  (poly-roots (dtf-num dtf)))

;;; dtf-gain : DTF → Number
;;; Get the static gain H(1) - DC gain for discrete systems.
(define (dtf-gain dtf)
  (doc 'export #t)
  (let ([den-at-1 (poly-eval (dtf-den dtf) 1)])
       (if (< (abs den-at-1) 1e-15)
           'infinite
           (/ (poly-eval (dtf-num dtf) 1) den-at-1))))

;;; dtf-from-poles-zeros : (List Complex) × (List Complex) × Number × Number → DTF
;;; Construct discrete TF from poles, zeros, gain, and sample time.
(define (dtf-from-poles-zeros poles zeros gain Ts)
  (doc 'export #t)
  (make-dtf (poly-from-roots zeros gain)
            (poly-from-roots poles 1)
            Ts))

;;; ====
;;; Discrete Transfer Function Evaluation
;;; ====

;;; dtf-eval : DTF × Complex → Complex
;;; Evaluate H(z) at complex point z.
(define (dtf-eval dtf z)
  (doc 'export #t)
  (let ([num-val (poly-eval-complex (dtf-num dtf) z)]
        [den-val (poly-eval-complex (dtf-den dtf) z)])
       (complex-div num-val den-val)))

;;; dtf-eval-real : DTF × Number → Number
;;; Evaluate H(z) at real z value.
(define (dtf-eval-real dtf z)
  (doc 'export #t)
  (/ (poly-eval (dtf-num dtf) z) (poly-eval (dtf-den dtf) z)))

;;; ====
;;; Frequency Response
;;; ====

;;; dtf-freq-response : DTF × Vector → Vector
;;; Compute frequency response H(e^{jωTs}) for vector of frequencies ω.
;;; Returns vector of complex values.
(define (dtf-freq-response dtf frequencies)
  (doc 'export #t)
  (let* ([Ts (dtf-Ts dtf)]
         [n (vector-length frequencies)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let* ([w (vector-ref frequencies i)]
                   ;; z = e^{jωTs} = cos(ωTs) + j*sin(ωTs)
                   [wTs (* w Ts)]
                   [z (make-complex (cos wTs) (sin wTs))])
                  (vector-set! result i (dtf-eval dtf z))))))

;;; dtf-magnitude : DTF × Vector → Vector
;;; Compute magnitude |H(e^{jωTs})| for vector of frequencies.
(define (dtf-magnitude dtf frequencies)
  (doc 'export #t)
  (let* ([resp (dtf-freq-response dtf frequencies)]
         [n (vector-length resp)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-magnitude (vector-ref resp i))))))

;;; dtf-magnitude-db : DTF × Vector → Vector
;;; Compute magnitude in decibels.
(define (dtf-magnitude-db dtf frequencies)
  (doc 'export #t)
  (let* ([mag (dtf-magnitude dtf frequencies)]
         [n (vector-length mag)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([m (vector-ref mag i)])
                 (vector-set! result i (if (> m 0) (* 20 (log10 m)) -200))))))

;;; dtf-phase : DTF × Vector → Vector
;;; Compute phase angle of H(e^{jωTs}) in radians.
(define (dtf-phase dtf frequencies)
  (doc 'export #t)
  (let* ([resp (dtf-freq-response dtf frequencies)]
         [n (vector-length resp)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-angle (vector-ref resp i))))))

;;; dtf-phase-deg : DTF × Vector → Vector
;;; Compute phase angle in degrees.
(define (dtf-phase-deg dtf frequencies)
  (doc 'export #t)
  (let* ([phase-rad (dtf-phase dtf frequencies)]
         [n (vector-length phase-rad)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (* (vector-ref phase-rad i) (/ 180 pi))))))

;;; ====
;;; System Connections
;;; ====

;;; dtf-series : DTF × DTF → DTF
;;; Series connection: H1(z) * H2(z)
;;; Both must have same sample time.
(define (dtf-series dtf1 dtf2)
  (doc 'export #t)
  (let ([Ts1 (dtf-Ts dtf1)]
        [Ts2 (dtf-Ts dtf2)])
       (if (not (= Ts1 Ts2))
           `(error sample-time-mismatch ,Ts1 ,Ts2)
           (make-dtf (poly-mul (dtf-num dtf1) (dtf-num dtf2))
                     (poly-mul (dtf-den dtf1) (dtf-den dtf2))
                     Ts1))))

;;; dtf-parallel : DTF × DTF → DTF
;;; Parallel connection: H1(z) + H2(z)
(define (dtf-parallel dtf1 dtf2)
  (doc 'export #t)
  (let ([Ts1 (dtf-Ts dtf1)]
        [Ts2 (dtf-Ts dtf2)])
       (if (not (= Ts1 Ts2))
           `(error sample-time-mismatch ,Ts1 ,Ts2)
           (let* ([n1 (dtf-num dtf1)] [d1 (dtf-den dtf1)]
                  [n2 (dtf-num dtf2)] [d2 (dtf-den dtf2)])
                 (make-dtf (poly-add (poly-mul n1 d2) (poly-mul n2 d1))
                           (poly-mul d1 d2)
                           Ts1)))))

;;; dtf-feedback : DTF × DTF × [Symbol] → DTF
;;; Feedback connection.
;;; For negative feedback (default): H_cl = G / (1 + G*H)
;;; For positive feedback: H_cl = G / (1 - G*H)
(define (dtf-feedback G H . sign)
  (doc 'export #t)
  (let ([Ts-G (dtf-Ts G)]
        [Ts-H (dtf-Ts H)])
       (if (not (= Ts-G Ts-H))
           `(error sample-time-mismatch ,Ts-G ,Ts-H)
           (let* ([neg? (or (null? sign) (eq? (car sign) 'negative))]
                  [nG (dtf-num G)] [dG (dtf-den G)]
                  [nH (dtf-num H)] [dH (dtf-den H)]
                  [nGH (poly-mul nG nH)]
                  [dGH (poly-mul dG dH)]
                  [denom-num (if neg?
                                 (poly-add dGH nGH)
                                 (poly-sub dGH nGH))])
                 (make-dtf (poly-mul nG dH) denom-num Ts-G)))))

;;; dtf-unity-feedback : DTF → DTF
;;; Unity negative feedback: H_cl = G / (1 + G)
(define (dtf-unity-feedback G)
  (doc 'export #t)
  (let ([unity (dtf-from-lists '(1) '(1) (dtf-Ts G))])
       (dtf-feedback G unity 'negative)))

;;; ====
;;; Stability Analysis
;;; ====

;;; dtf-stable? : DTF → Boolean
;;; Check if discrete TF is stable.
;;; Stable if all poles have magnitude < 1 (inside unit circle).
(define (dtf-stable? dtf)
  (doc 'export #t)
  (let ([poles (dtf-poles dtf)])
       (if (pair? poles)
           (and (list? poles)
                (andmap (lambda (p)
                                (< (complex-magnitude p) 1))
                        poles))
           #t)))  ; No poles = stable

;;; dtf-pole-magnitudes : DTF → (List Number)
;;; Get magnitudes of all poles.
(define (dtf-pole-magnitudes dtf)
  (doc 'export #t)
  (map complex-magnitude (dtf-poles dtf)))

;;; ====
;;; DSS ↔ DTF Conversion
;;; ====

;;; dss->dtf : DSS → DTF
;;; Convert discrete state-space to discrete transfer function.
;;; Uses the same algorithm as ss->tf but interprets as z-domain.
(define (dss->dtf dsys)
  (doc 'export #t)
  (let* ([A (dss-A dsys)]
         [B (dss-B dsys)]
         [C (dss-C dsys)]
         [D (dss-D dsys)]
         [Ts (dss-Ts dsys)]
         [n (dss-order dsys)])
        (if (= n 0)
            ;; Static gain
            (dtf-from-lists (list (matrix-ref D 0 0)) (list 1) Ts)
            ;; General case: use characteristic polynomial as denominator
            (let* ([char-poly (compute-char-poly A n)]
                   [num-poly (compute-dtf-numerator A B C D n char-poly)])
                  (make-dtf num-poly char-poly Ts)))))

;;; compute-char-poly : Matrix × Nat → Poly
;;; Compute characteristic polynomial det(zI - A) using Faddeev-LeVerrier.
(define (compute-char-poly A n)
  (doc 'export #t)
  (let ([coeffs (make-vector (+ n 1) 0)])
       (vector-set! coeffs 0 1)  ; Leading coefficient
       (let loop ([k 1] [M (matrix-identity n)])
            (if (> k n)
                (make-poly coeffs)
                (let* ([AM (matrix-mul A M)]
                       [p-k (/ (- (matrix-trace-local AM)) k)]
                       [M-next (if (= k n)
                                   (make-matrix n n 0)
                                   (matrix-add AM (matrix-scale p-k (matrix-identity n))))])
                      (vector-set! coeffs k p-k)
                      (if (= k n)
                          (make-poly coeffs)
                          (loop (+ k 1) M-next)))))))

;;; compute-dtf-numerator : Matrix × Matrix × Matrix × Matrix × Nat × Poly → Poly
;;; Compute numerator polynomial for DTF from state-space.
(define (compute-dtf-numerator A B C D n char-poly)
  (doc 'export #t)
  (let* ([b-col (matrix-column-v B 0)]
         [c-row (matrix-row-v C 0)]
         [d-val (matrix-ref D 0 0)]
         [char-coeffs (poly-coeffs char-poly)]
         [num-len (+ n 1)]
         [result (make-vector num-len 0)])
        ;; D contributes: D * char_poly
        (do ([i 0 (+ i 1)])
            ((= i num-len))
            (vector-set! result i (* d-val (vector-ref char-coeffs i))))
        ;; Compute adjugate contributions using Faddeev-LeVerrier
        (let loop ([k 0] [M (matrix-identity n)])
             (if (> k (- n 1))
                 (make-poly result)
                 (let* ([CB (dot-product-local c-row (matrix-vec-mul-local M b-col))]
                        [coeff-idx (+ 1 k)])
                       (when (< coeff-idx num-len)
                             (vector-set! result coeff-idx
                                          (+ (vector-ref result coeff-idx) CB)))
                       (if (= k (- n 1))
                           (make-poly result)
                           (let* ([AM (matrix-mul A M)]
                                  [p-k (vector-ref char-coeffs (+ k 1))]
                                  [M-next (matrix-add AM (matrix-scale p-k (matrix-identity n)))])
                                 (loop (+ k 1) M-next))))))))

;;; dtf->dss : DTF → DSS
;;; Convert discrete TF to discrete state-space in controllable canonical form.
(define (dtf->dss dtf)
  (doc 'export #t)
  (let* ([num (dtf-num dtf)]
         [den (dtf-den dtf)]
         [Ts (dtf-Ts dtf)]
         [n (poly-degree den)]
         [m (poly-degree num)])
        (cond
         [(= n 0)
          ;; Constant gain
          (let ([gain (/ (poly-leading num) (poly-leading den))])
               (make-dss (make-matrix 0 0 0)
                         (make-matrix 0 1 0)
                         (make-matrix 1 0 0)
                         (make-matrix 1 1 gain)
                         Ts))]
         [else
          ;; Build controllable canonical form
          (dtf->dss-controllable num den Ts n m)])))

;;; dtf->dss-controllable : Poly × Poly × Number × Nat × Nat → DSS
(define (dtf->dss-controllable num den Ts n m)
  (doc 'export #t)
  (let* ([den-coeffs (poly-coeffs den)]
         [num-coeffs (poly-coeffs num)]
         [num-padded (pad-poly-coeffs-local num-coeffs (+ n 1))]
         [b-n (vector-ref num-padded 0)]
         [A (make-matrix n n 0)])
        ;; Set superdiagonal to 1
        (do ([i 0 (+ i 1)])
            ((= i (- n 1)))
            (matrix-set! A i (+ i 1) 1))
        ;; Set last row to -a_i
        (do ([j 0 (+ j 1)])
            ((= j n))
            (matrix-set! A (- n 1) j (- (vector-ref den-coeffs (- n j)))))
        ;; Build B matrix
        (let ([B (make-matrix n 1 0)])
             (matrix-set! B (- n 1) 0 1)
             ;; Build C matrix
             (let ([C (make-matrix 1 n 0)])
                  (do ([j 0 (+ j 1)])
                      ((= j n))
                      (let* ([b-j (vector-ref num-padded (- n j))]
                             [a-j (vector-ref den-coeffs (- n j))]
                             [c-j (- b-j (* a-j b-n))])
                            (matrix-set! C 0 j c-j)))
                  ;; Build D matrix
                  (let ([D (make-matrix 1 1 b-n)])
                       (make-dss A B C D Ts))))))

;;; ====
;;; Display
;;; ====

;;; dtf->string : DTF → String
;;; Pretty-print discrete transfer function.
(define (dtf->string dtf)
  (doc 'export #t)
  (let ([num-str (poly->string-z (dtf-num dtf))]
        [den-str (poly->string-z (dtf-den dtf))]
        [Ts (dtf-Ts dtf)])
       (string-append "(" num-str ") / (" den-str "), Ts=" (number->string Ts))))

;;; poly->string-z : Poly → String
;;; Convert polynomial to string using 'z' as variable.
(define (poly->string-z p)
  (doc 'export #t)
  (let* ([coeffs (poly-coeffs p)]
         [n (- (vector-length coeffs) 1)]
         [terms '()])
        (do ([i 0 (+ i 1)])
            ((> i n))
            (let* ([coeff (vector-ref coeffs i)]
                   [power (- n i)])
                  (when (not (zero? coeff))
                        (let ([term (cond
                                     [(= power 0) (number->string coeff)]
                                     [(= power 1) (if (= coeff 1)
                                                      "z"
                                                      (string-append (number->string coeff) "*z"))]
                                     [else (if (= coeff 1)
                                               (string-append "z^" (number->string power))
                                               (string-append (number->string coeff) "*z^" (number->string power)))])])
                             (set! terms (cons term terms))))))
        (if (null? terms)
            "0"
            (string-join (reverse terms) " + "))))

;;; ====
;;; Helper Functions
;;; ====

;;; log10 : Number → Number
(define (log10 x)
  (doc 'export #t)
  (/ (log x) (log 10)))

;;; pi constant
(define pi 3.141592653589793)

;; string-join is provided by prelude

;;; matrix-trace-local : Matrix → Number
(define (matrix-trace-local M)
  (doc 'export #t)
  (let ([n (min (matrix-rows M) (matrix-cols M))])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (matrix-ref M i i)))))))

;;; matrix-column-v : Matrix × Nat → Vec
(define (matrix-column-v M j)
  (doc 'export #t)
  (let* ([rows (matrix-rows M)]
         [result (make-vector rows)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (vector-set! result i (matrix-ref M i j)))))

;;; matrix-row-v : Matrix × Nat → Vec
(define (matrix-row-v M i)
  (doc 'export #t)
  (let* ([cols (matrix-cols M)]
         [result (make-vector cols)])
        (do ([j 0 (+ j 1)])
            ((= j cols) result)
            (vector-set! result j (matrix-ref M i j)))))

;;; matrix-vec-mul-local : Matrix × Vec → Vec
(define (matrix-vec-mul-local M v)
  (doc 'export #t)
  (let* ([rows (matrix-rows M)]
         [cols (matrix-cols M)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (let loop ([j 0] [sum 0])
                 (if (= j cols)
                     (vector-set! result i sum)
                     (loop (+ j 1) (+ sum (* (matrix-ref M i j) (vector-ref v j)))))))))

;;; dot-product-local : Vec × Vec → Number
(define (dot-product-local v1 v2)
  (doc 'export #t)
  (let ([n (vector-length v1)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

;;; pad-poly-coeffs-local : Vec × Nat → Vec
(define (pad-poly-coeffs-local coeffs n)
  (doc 'export #t)
  (let ([len (vector-length coeffs)])
       (if (>= len n)
           coeffs
           (let ([result (make-vector n 0)]
                 [offset (- n len)])
                (do ([i 0 (+ i 1)])
                    ((= i len) result)
                    (vector-set! result (+ i offset) (vector-ref coeffs i)))))))
