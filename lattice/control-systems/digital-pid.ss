;;; lattice/control-systems/digital-pid.ss — Digital PID Controller
;;;
;;; Implementation of discrete-time PID controllers with various
;;; features including anti-windup and derivative filtering.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude
(require 'prelude)

(doc 'module 'digital-pid)
(doc 'purity 'total)
(doc 'description "Digital PID controller with anti-windup and derivative filtering")
(doc 'layer 'lattice)

;;; ====
;;; PID Controller Structure
;;; ====

;;; A digital PID controller has the form:
;;;   u[k] = Kp * e[k] + Ki * Ts * sum(e[i]) + Kd/Ts * (e[k] - e[k-1])
;;;
;;; Where:
;;;   u[k] = control output at sample k
;;;   e[k] = error at sample k (setpoint - measurement)
;;;   Kp   = proportional gain
;;;   Ki   = integral gain (per second)
;;;   Kd   = derivative gain (seconds)
;;;   Ts   = sample time

;;; make-digital-pid : Number × Number × Number × Number → PID
;;; Create a digital PID controller with gains and sample time.
;;; Parameters:
;;;   Kp = proportional gain
;;;   Ki = integral gain (units: 1/second)
;;;   Kd = derivative gain (units: seconds)
;;;   Ts = sample time (seconds)
(define (make-digital-pid Kp Ki Kd Ts)
  (doc 'export #t)
  (list 'digital-pid Kp Ki Kd Ts))

;;; digital-pid? : Any → Boolean
(define (digital-pid? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'digital-pid)
       (= (length x) 5)))

;;; Accessors
(define (pid-Kp pid) (cadr pid))
(define (pid-Ki pid) (caddr pid))
(define (pid-Kd pid) (cadddr pid))
(define (pid-Ts pid) (car (cddddr pid)))

;;; ====
;;; PID State
;;; ====

;;; PID state tracks the running state of a PID controller.
;;; Structure: (pid-state integral-sum previous-error previous-output)

;;; make-pid-state : → PIDState
;;; Create initial PID state.
(define (make-pid-state)
  (doc 'export #t)
  (list 'pid-state 0 0 0))

;;; pid-state? : Any → Boolean
(define (pid-state? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'pid-state)
       (= (length x) 4)))

;;; Accessors
(define (pid-integral state) (cadr state))
(define (pid-prev-error state) (caddr state))
(define (pid-prev-output state) (cadddr state))

;;; ====
;;; Basic PID Step
;;; ====

;;; pid-step : PID × PIDState × Number → (Number . PIDState)
;;; Compute one PID step.
;;; Returns (output . new-state).
;;;
;;; Uses position form (parallel PID):
;;;   P = Kp * e
;;;   I = Ki * Ts * sum(e)
;;;   D = Kd / Ts * (e - e_prev)
;;;   u = P + I + D
(define (pid-step pid state error)
  (doc 'export #t)
  (let* ([Kp (pid-Kp pid)]
         [Ki (pid-Ki pid)]
         [Kd (pid-Kd pid)]
         [Ts (pid-Ts pid)]
         [integral-prev (pid-integral state)]
         [error-prev (pid-prev-error state)]
         ;; Compute terms
         [P (* Kp error)]
         [integral-new (+ integral-prev (* error Ts))]
         [I (* Ki integral-new)]
         [D (* Kd (/ (- error error-prev) Ts))]
         ;; Total output
         [output (+ P I D)]
         ;; New state
         [new-state (list 'pid-state integral-new error output)])
        (cons output new-state)))

;;; ====
;;; PID with Anti-Windup
;;; ====

;;; pid-step-antiwindup : PID × PIDState × Number × Number × Number → (Number . PIDState)
;;; PID step with anti-windup limiting.
;;; Clamps output to [out-min, out-max] and conditionally integrates
;;; only when output is not saturated.
;;;
;;; Uses back-calculation anti-windup:
;;; - If output would exceed limits, clamp it
;;; - Adjust integral term to match clamped output
(define (pid-step-antiwindup pid state error out-min out-max)
  (doc 'export #t)
  (let* ([Kp (pid-Kp pid)]
         [Ki (pid-Ki pid)]
         [Kd (pid-Kd pid)]
         [Ts (pid-Ts pid)]
         [integral-prev (pid-integral state)]
         [error-prev (pid-prev-error state)]
         ;; Compute P and D terms
         [P (* Kp error)]
         [D (* Kd (/ (- error error-prev) Ts))]
         ;; Compute unclamped integral
         [integral-unclamped (+ integral-prev (* error Ts))]
         [I-unclamped (* Ki integral-unclamped)]
         ;; Total unclamped output
         [output-unclamped (+ P I-unclamped D)]
         ;; Clamp output
         [output-clamped (clamp output-unclamped out-min out-max)]
         ;; Anti-windup: if saturated, don't accumulate more integral
         [saturated? (not (= output-unclamped output-clamped))]
         [integral-new (if saturated?
                           ;; Keep previous integral when saturated
                           integral-prev
                           ;; Normal integration
                           integral-unclamped)]
         ;; New state
         [new-state (list 'pid-state integral-new error output-clamped)])
        (cons output-clamped new-state)))

;;; clamp : Number × Number × Number → Number
(define (clamp x min-val max-val)
  (doc 'export #t)
  (cond
   [(< x min-val) min-val]
   [(> x max-val) max-val]
   [else x]))

;;; ====
;;; PID with Derivative Filter
;;; ====

;;; The derivative term can amplify high-frequency noise.
;;; A filtered derivative uses a first-order filter:
;;;   D_filtered = (Kd * s) / (1 + Ts*N * s)
;;; where N is the filter coefficient (typically 10-20).

;;; make-filtered-pid-state : → FilteredPIDState
;;; State for PID with derivative filter.
;;; Structure: (filtered-pid-state integral prev-error prev-derivative)
(define (make-filtered-pid-state)
  (doc 'export #t)
  (list 'filtered-pid-state 0 0 0))

;;; filtered-pid-state? : Any → Boolean
(define (filtered-pid-state? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'filtered-pid-state)
       (= (length x) 4)))

(define (fpid-integral state) (cadr state))
(define (fpid-prev-error state) (caddr state))
(define (fpid-prev-derivative state) (cadddr state))

;;; pid-step-filtered : PID × FilteredPIDState × Number × Number → (Number . FilteredPIDState)
;;; PID step with filtered derivative.
;;; N is the filter coefficient (larger = less filtering).
(define (pid-step-filtered pid state error N)
  (doc 'export #t)
  (let* ([Kp (pid-Kp pid)]
         [Ki (pid-Ki pid)]
         [Kd (pid-Kd pid)]
         [Ts (pid-Ts pid)]
         [integral-prev (fpid-integral state)]
         [error-prev (fpid-prev-error state)]
         [D-prev (fpid-prev-derivative state)]
         ;; P term
         [P (* Kp error)]
         ;; I term
         [integral-new (+ integral-prev (* error Ts))]
         [I (* Ki integral-new)]
         ;; Filtered D term (first-order digital filter)
         ;; D[k] = (1 - a) * D[k-1] + a * Kd/Ts * (e[k] - e[k-1])
         ;; where a = N*Ts / (1 + N*Ts)
         [a (/ (* N Ts) (+ 1 (* N Ts)))]
         [D-raw (* (/ Kd Ts) (- error error-prev))]
         [D-new (+ (* (- 1 a) D-prev) (* a D-raw))]
         ;; Output
         [output (+ P I D-new)]
         ;; New state
         [new-state (list 'filtered-pid-state integral-new error D-new)])
        (cons output new-state)))

;;; ====
;;; PID Reset
;;; ====

;;; pid-reset : PIDState → PIDState
;;; Reset PID state (bumpless transfer).
(define (pid-reset state)
  (doc 'export #t)
  (make-pid-state))

;;; pid-reset-to : Number × Number → PIDState
;;; Reset PID state with specified integral value and previous error.
;;; Useful for bumpless transfer when switching modes.
(define (pid-reset-to integral prev-error)
  (doc 'export #t)
  (list 'pid-state integral prev-error 0))

;;; ====
;;; PID Tuning Methods
;;; ====

;;; ziegler-nichols-tune : Number × Number × Symbol → PID
;;; Compute PID gains using Ziegler-Nichols method.
;;; Parameters:
;;;   Ku = ultimate gain (gain at which system oscillates)
;;;   Tu = oscillation period
;;;   type = 'P, 'PI, or 'PID
;;;   Ts = sample time
;;;
;;; Classic Ziegler-Nichols formulas:
;;;   P:   Kp = 0.5*Ku
;;;   PI:  Kp = 0.45*Ku, Ti = Tu/1.2
;;;   PID: Kp = 0.6*Ku, Ti = Tu/2, Td = Tu/8
;;;
;;; Where Ti = Kp/Ki (integral time) and Td = Kd/Kp (derivative time)
(define (ziegler-nichols-tune Ku Tu type Ts)
  (doc 'export #t)
  (case type
        [(P)
         (let ([Kp (* 0.5 Ku)])
              (make-digital-pid Kp 0 0 Ts))]
        [(PI)
         (let* ([Kp (* 0.45 Ku)]
                [Ti (/ Tu 1.2)]
                [Ki (/ Kp Ti)])
               (make-digital-pid Kp Ki 0 Ts))]
        [(PID)
         (let* ([Kp (* 0.6 Ku)]
                [Ti (/ Tu 2)]
                [Td (/ Tu 8)]
                [Ki (/ Kp Ti)]
                [Kd (* Kp Td)])
               (make-digital-pid Kp Ki Kd Ts))]
        [else
         `(error invalid-pid-type ,type)]))

;;; tyreus-luyben-tune : Number × Number × Number → PID
;;; Compute PID gains using Tyreus-Luyben method (less aggressive).
;;; Parameters:
;;;   Ku = ultimate gain
;;;   Tu = oscillation period
;;;   Ts = sample time
;;;
;;; Tyreus-Luyben formulas (for PI):
;;;   Kp = Ku/3.2
;;;   Ti = 2.2*Tu
(define (tyreus-luyben-tune Ku Tu Ts)
  (doc 'export #t)
  (let* ([Kp (/ Ku 3.2)]
         [Ti (* 2.2 Tu)]
         [Ki (/ Kp Ti)])
        (make-digital-pid Kp Ki 0 Ts)))

;;; cohen-coon-tune : Number × Number × Number × Number → PID
;;; Compute PID gains using Cohen-Coon method.
;;; Parameters:
;;;   K  = process gain
;;;   tau = process time constant
;;;   theta = dead time
;;;   Ts = sample time
;;;
;;; For PI controller:
;;;   Kp = (1/K) * (tau/theta) * (0.9 + theta/(12*tau))
;;;   Ti = theta * (30 + 3*theta/tau) / (9 + 20*theta/tau)
(define (cohen-coon-tune K tau theta Ts)
  (doc 'export #t)
  (let* ([ratio (/ theta tau)]
         [Kp (* (/ 1 K) (/ tau theta) (+ 0.9 (/ theta (* 12 tau))))]
         [Ti (* theta (/ (+ 30 (* 3 ratio)) (+ 9 (* 20 ratio))))]
         [Ki (/ Kp Ti)])
        (make-digital-pid Kp Ki 0 Ts)))

;;; ====
;;; Simulation Helper
;;; ====

;;; pid-simulate : PID × PIDState × Number × (List Number) → (List Number × List Number)
;;; Simulate PID controller response.
;;; Takes a setpoint and list of process values (measurements).
;;; Returns (outputs . errors).
(define (pid-simulate pid state setpoint measurements)
  (doc 'export #t)
  (let loop ([state state]
             [ms measurements]
             [outputs '()]
             [errors '()])
       (if (null? ms)
           (cons (reverse outputs) (reverse errors))
           (let* ([pv (car ms)]
                  [error (- setpoint pv)]
                  [result (pid-step pid state error)]
                  [output (car result)]
                  [new-state (cdr result)])
                 (loop new-state
                       (cdr ms)
                       (cons output outputs)
                       (cons error errors))))))

;;; pid-simulate-antiwindup : PID × PIDState × Number × (List Number) × Number × Number → (List Number × List Number)
;;; Simulate PID with anti-windup.
(define (pid-simulate-antiwindup pid state setpoint measurements out-min out-max)
  (doc 'export #t)
  (let loop ([state state]
             [ms measurements]
             [outputs '()]
             [errors '()])
       (if (null? ms)
           (cons (reverse outputs) (reverse errors))
           (let* ([pv (car ms)]
                  [error (- setpoint pv)]
                  [result (pid-step-antiwindup pid state error out-min out-max)]
                  [output (car result)]
                  [new-state (cdr result)])
                 (loop new-state
                       (cdr ms)
                       (cons output outputs)
                       (cons error errors))))))

;;; ====
;;; Velocity Form PID
;;; ====

;;; The velocity (incremental) form computes the change in output:
;;;   delta_u = Kp*(e[k] - e[k-1]) + Ki*Ts*e[k] + Kd/Ts*(e[k] - 2*e[k-1] + e[k-2])
;;;   u[k] = u[k-1] + delta_u
;;;
;;; Advantages:
;;; - Natural anti-windup (incremental changes are bounded)
;;; - Bumpless transfer when changing setpoint
;;; - No integral windup issue

;;; make-velocity-pid-state : → VelocityPIDState
;;; State for velocity form: (velocity-pid-state prev-error prev-prev-error prev-output)
(define (make-velocity-pid-state)
  (doc 'export #t)
  (list 'velocity-pid-state 0 0 0))

;;; velocity-pid-state? : Any → Boolean
(define (velocity-pid-state? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'velocity-pid-state)
       (= (length x) 4)))

(define (vpid-prev-error state) (cadr state))
(define (vpid-prev-prev-error state) (caddr state))
(define (vpid-prev-output state) (cadddr state))

;;; pid-step-velocity : PID × VelocityPIDState × Number → (Number . VelocityPIDState)
;;; Velocity form PID step.
(define (pid-step-velocity pid state error)
  (doc 'export #t)
  (let* ([Kp (pid-Kp pid)]
         [Ki (pid-Ki pid)]
         [Kd (pid-Kd pid)]
         [Ts (pid-Ts pid)]
         [e0 error]
         [e1 (vpid-prev-error state)]
         [e2 (vpid-prev-prev-error state)]
         [u-prev (vpid-prev-output state)]
         ;; Velocity form
         [delta-P (* Kp (- e0 e1))]
         [delta-I (* Ki Ts e0)]
         [delta-D (* (/ Kd Ts) (+ e0 (* -2 e1) e2))]
         [delta-u (+ delta-P delta-I delta-D)]
         [output (+ u-prev delta-u)]
         ;; New state
         [new-state (list 'velocity-pid-state e0 e1 output)])
        (cons output new-state)))

;;; ====
;;; Display
;;; ====

;;; pid->string : PID → String
(define (pid->string pid)
  (doc 'export #t)
  (let ([Kp (pid-Kp pid)]
        [Ki (pid-Ki pid)]
        [Kd (pid-Kd pid)]
        [Ts (pid-Ts pid)])
       (string-append "PID(Kp=" (number->string Kp)
                      ", Ki=" (number->string Ki)
                      ", Kd=" (number->string Kd)
                      ", Ts=" (number->string Ts) ")")))
