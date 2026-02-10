;;; lattice/control-systems/transfer-function.ss — Transfer Functions
;;;
;;; Transfer function representation for control systems analysis.
;;; A transfer function H(s) = N(s)/D(s) represents the input-output
;;; relationship of a linear time-invariant system.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/numeric/polynomial.ss
;;;   - lattice/numeric/complex.ss

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'numeric/polynomial)

(doc 'module 'transfer-function)
(doc 'description "Transfer function representation for LTI systems with frequency response analysis")
(doc 'layer 'lattice)

;;; ====
;;; Transfer Function Representation
;;; ====

;;; A transfer function is: (tf num den)
;;; where:
;;;   num = numerator polynomial (Poly)
;;;   den = denominator polynomial (Poly)
;;;
;;; H(s) = num(s) / den(s)
;;;
;;; Convention: Denominator is normalized (monic) by default.

;;; tf? : Any → Boolean
;;; Check if value is a transfer function.
(define (tf? x)
  (and (pair? x)
       (eq? (car x) 'tf)
       (= (length x) 3)
       (poly? (cadr x))
       (poly? (caddr x))))

;;; make-tf : Poly × Poly → TF | Error
;;; Create transfer function from numerator and denominator polynomials.
;;; Normalizes denominator to monic form.
(define (make-tf num den)
  (let ([lead (poly-leading den)])
       (if (zero? lead)
           '(error zero-denominator)
           (let ([scale (/ 1 lead)])
                (list 'tf
                      (poly-scale num scale)
                      (poly-normalize den))))))

;;; tf-from-coeffs : Vector × Vector → TF
;;; Create transfer function from coefficient vectors (descending powers).
(define (tf-from-coeffs num-coeffs den-coeffs)
  (make-tf (make-poly num-coeffs) (make-poly den-coeffs)))

;;; tf-from-lists : (List Number) × (List Number) → TF
;;; Create transfer function from lists of coefficients.
(define (tf-from-lists num-list den-list)
  (make-tf (poly-from-list num-list) (poly-from-list den-list)))

;;; tf-num : TF → Poly
;;; Get the numerator polynomial.
(define (tf-num tf)
  (cadr tf))

;;; tf-den : TF → Poly
;;; Get the denominator polynomial.
(define (tf-den tf)
  (caddr tf))

;;; tf-order : TF → Nat
;;; Get the order (degree of denominator).
(define (tf-order tf)
  (poly-degree (tf-den tf)))

;;; tf-relative-degree : TF → Int
;;; Get the relative degree (deg(den) - deg(num)).
;;; Positive means strictly proper, zero means proper, negative means improper.
(define (tf-relative-degree tf)
  (- (poly-degree (tf-den tf)) (poly-degree (tf-num tf))))

;;; tf-proper? : TF → Boolean
;;; Check if transfer function is proper (deg(num) <= deg(den)).
(define (tf-proper? tf)
  (>= (tf-relative-degree tf) 0))

;;; tf-strictly-proper? : TF → Boolean
;;; Check if transfer function is strictly proper (deg(num) < deg(den)).
(define (tf-strictly-proper? tf)
  (> (tf-relative-degree tf) 0))

;;; ====
;;; Poles and Zeros
;;; ====

;;; tf-poles : TF → (List Complex)
;;; Get the poles (roots of denominator).
(define (tf-poles tf)
  (poly-roots (tf-den tf)))

;;; tf-zeros : TF → (List Complex)
;;; Get the zeros (roots of numerator).
(define (tf-zeros tf)
  (poly-roots (tf-num tf)))

;;; tf-from-poles-zeros : (List Complex) × (List Complex) × Number → TF
;;; Construct transfer function from poles, zeros, and gain.
(define (tf-from-poles-zeros poles zeros gain)
  (make-tf (poly-from-roots zeros gain)
           (poly-from-roots poles 1)))

;;; tf-gain : TF → Number
;;; Get the static gain (high-frequency gain for proper systems).
;;; For H(s) = (b_n*s^n + ...)/(s^m + ...), gain = b_n when n=m.
(define (tf-gain tf)
  (/ (poly-leading (tf-num tf)) (poly-leading (tf-den tf))))

;;; ====
;;; Transfer Function Evaluation
;;; ====

;;; tf-eval : TF × Complex → Complex
;;; Evaluate H(s) at complex frequency s.
(define (tf-eval tf s)
  (let ([num-val (poly-eval-complex (tf-num tf) s)]
        [den-val (poly-eval-complex (tf-den tf) s)])
       (complex-div num-val den-val)))

;;; tf-eval-real : TF × Number → Number
;;; Evaluate H(s) at real s value (for real systems on real axis).
(define (tf-eval-real tf s)
  (/ (poly-eval (tf-num tf) s) (poly-eval (tf-den tf) s)))

;;; tf-dc-gain : TF → Number | 'infinite
;;; Compute DC gain H(0).
;;; Returns 'infinite if there's a pole at origin.
(define (tf-dc-gain tf)
  (let ([den-at-zero (poly-eval (tf-den tf) 0)])
       (if (< (abs den-at-zero) 1e-15)
           'infinite
           (/ (poly-eval (tf-num tf) 0) den-at-zero))))

;;; ====
;;; Frequency Response
;;; ====

;;; tf-freq-response : TF × Vector → Vector
;;; Compute frequency response H(jw) for vector of frequencies w.
;;; Returns vector of complex values.
(define (tf-freq-response tf frequencies)
  (let* ([n (vector-length frequencies)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let* ([w (vector-ref frequencies i)]
                   [jw (make-complex 0 w)])
                  (vector-set! result i (tf-eval tf jw))))))

;;; tf-magnitude : TF × Vector → Vector
;;; Compute magnitude |H(jw)| for vector of frequencies.
(define (tf-magnitude tf frequencies)
  (let* ([resp (tf-freq-response tf frequencies)]
         [n (vector-length resp)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-magnitude (vector-ref resp i))))))

;;; tf-magnitude-db : TF × Vector → Vector
;;; Compute magnitude in decibels: 20*log10(|H(jw)|).
(define (tf-magnitude-db tf frequencies)
  (let* ([mag (tf-magnitude tf frequencies)]
         [n (vector-length mag)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([m (vector-ref mag i)])
                 (vector-set! result i (if (> m 0) (* 20 (log10 m)) -200))))))

;;; tf-phase : TF × Vector → Vector
;;; Compute phase angle of H(jw) in radians.
(define (tf-phase tf frequencies)
  (let* ([resp (tf-freq-response tf frequencies)]
         [n (vector-length resp)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-angle (vector-ref resp i))))))

;;; tf-phase-deg : TF × Vector → Vector
;;; Compute phase angle in degrees.
(define (tf-phase-deg tf frequencies)
  (let* ([phase-rad (tf-phase tf frequencies)]
         [n (vector-length phase-rad)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (* (vector-ref phase-rad i) (/ 180 pi))))))

;;; tf-bode-data : TF × Number × Number × Nat → (freqs mag-db phase-deg)
;;; Generate Bode plot data.
;;;   start-exp: starting frequency as 10^start-exp
;;;   end-exp: ending frequency as 10^end-exp
;;;   n-points: number of points
;;; Returns: (frequencies magnitude-dB phase-degrees)
(define (tf-bode-data tf start-exp end-exp n-points)
  (let* ([freqs (logspace start-exp end-exp n-points)]
         [mag-db (tf-magnitude-db tf freqs)]
         [phase-deg (tf-phase-deg tf freqs)])
        (list freqs mag-db phase-deg)))

;;; ====
;;; System Connections
;;; ====

;;; tf-series : TF × TF → TF
;;; Series connection: H1(s) * H2(s)
;;; Output of H1 feeds into input of H2.
(define (tf-series tf1 tf2)
  (make-tf (poly-mul (tf-num tf1) (tf-num tf2))
           (poly-mul (tf-den tf1) (tf-den tf2))))

;;; tf-parallel : TF × TF → TF
;;; Parallel connection: H1(s) + H2(s)
;;; Cross-multiply to get common denominator.
(define (tf-parallel tf1 tf2)
  (let* ([n1 (tf-num tf1)] [d1 (tf-den tf1)]
         [n2 (tf-num tf2)] [d2 (tf-den tf2)])
        (make-tf (poly-add (poly-mul n1 d2) (poly-mul n2 d1))
                 (poly-mul d1 d2))))

;;; tf-feedback : TF × TF × [Symbol] → TF
;;; Feedback connection.
;;; For negative feedback (default): H_cl = G / (1 + G*H)
;;; For positive feedback: H_cl = G / (1 - G*H)
;;; G is forward path, H is feedback path.
(define (tf-feedback G H . sign)
  (let* ([neg? (or (null? sign) (eq? (car sign) 'negative))]
         [nG (tf-num G)] [dG (tf-den G)]
         [nH (tf-num H)] [dH (tf-den H)]
         ;; G*H numerator and denominator
         [nGH (poly-mul nG nH)]
         [dGH (poly-mul dG dH)]
         ;; 1 +/- G*H = (dGH +/- nGH) / dGH
         [denom-num (if neg?
                        (poly-add dGH nGH)
                        (poly-sub dGH nGH))])
        ;; H_cl = (nG/dG) / ((dGH +/- nGH)/dGH)
        ;;      = (nG * dH) / (dG * dH +/- nG * nH)
        ;;      = (nG * dH) / denom-num  (when denom-num uses dG*dH as base)
        (make-tf (poly-mul nG dH) denom-num)))

;;; tf-unity-feedback : TF → TF
;;; Unity negative feedback: H_cl = G / (1 + G)
(define (tf-unity-feedback G)
  (tf-feedback G (tf-from-lists '(1) '(1)) 'negative))

;;; ====
;;; Stability Analysis (Basic)
;;; ====

;;; tf-stable? : TF → Boolean
;;; Check if transfer function is stable.
;;; Stable if all poles have negative real parts.
(define (tf-stable? tf)
  (let ([poles (tf-poles tf)])
       (if (pair? poles)
           (and (list? poles)
                (andmap (lambda (p)
                                (< (complex-real p) 0))
                        poles))
           #t)))  ; No poles = stable

;;; tf-pole-real-parts : TF → (List Number)
;;; Get real parts of all poles.
(define (tf-pole-real-parts tf)
  (map complex-real (tf-poles tf)))

;;; ====
;;; Display
;;; ====

;;; tf->string : TF → String
;;; Pretty-print transfer function.
(define (tf->string tf)
  (let ([num-str (poly->string (tf-num tf))]
        [den-str (poly->string (tf-den tf))])
       (string-append "(" num-str ") / (" den-str ")")))

;;; ====
;;; Utility Functions
;;; ====

;;; log10 : Number → Number
(define (log10 x)
  (/ (log x) (log 10)))

;;; pi constant
(define pi 3.141592653589793)

;;; andmap : (a → Boolean) × (List a) → Boolean
(define (andmap f lst)
  (if (null? lst)
      #t
      (and (f (car lst)) (andmap f (cdr lst)))))
