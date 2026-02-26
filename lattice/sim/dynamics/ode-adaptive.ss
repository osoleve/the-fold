(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-adaptive
;;; @requires prelude ode-system ode-state-vec
(require 'prelude)
(require 'ode-system)
(require 'ode-state-vec)

(doc 'module 'ode-adaptive)
(doc 'description "Generic adaptive step-size ODE solver using embedded Dormand-Prince RK4(5).
Provides error-controlled integration with PI step controller, min/max dt protection,
and structured result output. Works with list-based state vectors and bridges to ode-system.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Dormand-Prince RK4(5) Single Step
;;; ============================================================

(doc 'section 'rk45)

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Number
               (List (List Number) Number Number)))
(doc 'description "Single Dormand-Prince RK4(5) step. Takes a right-hand-side
function f(t, y) → dy/dt, current time, state, and step size.
Returns (new-state error-estimate suggested-dt) where error-estimate is
the RMS difference between the 5th and 4th order solutions.")
(define (dp45-step f t y dt tol)
  (let* (;; Stage evaluations (Dormand-Prince coefficients)
         [k1 (f t y)]
         [k2 (f (+ t (* 1/5 dt))
                (sv-madd y (* 1/5 dt) k1))]
         [k3 (f (+ t (* 3/10 dt))
                (sv-add y
                        (sv-add (sv-scale (* 3/40 dt) k1)
                                (sv-scale (* 9/40 dt) k2))))]
         [k4 (f (+ t (* 4/5 dt))
                (sv-add y
                        (sv-add (sv-scale (* 44/45 dt) k1)
                                (sv-add (sv-scale (* -56/15 dt) k2)
                                        (sv-scale (* 32/9 dt) k3)))))]
         [k5 (f (+ t (* 8/9 dt))
                (sv-add y
                        (sv-add (sv-scale (* 19372/6561 dt) k1)
                                (sv-add (sv-scale (* -25360/2187 dt) k2)
                                        (sv-add (sv-scale (* 64448/6561 dt) k3)
                                                (sv-scale (* -212/729 dt) k4))))))]
         [k6 (f (+ t dt)
                (sv-add y
                        (sv-add (sv-scale (* 9017/3168 dt) k1)
                                (sv-add (sv-scale (* -355/33 dt) k2)
                                        (sv-add (sv-scale (* 46732/5247 dt) k3)
                                                (sv-add (sv-scale (* 49/176 dt) k4)
                                                        (sv-scale (* -5103/18656 dt) k5)))))))]
         ;; 5th order solution (used as the result)
         [y5 (sv-add y
                     (sv-scale dt
                               (sv-add (sv-scale 35/384 k1)
                                       (sv-add (sv-scale 500/1113 k3)
                                               (sv-add (sv-scale 125/192 k4)
                                                       (sv-add (sv-scale -2187/6784 k5)
                                                               (sv-scale 11/84 k6)))))))]
         ;; 4th order solution (for error estimate)
         [k7 (f (+ t dt) y5)]
         [y4 (sv-add y
                     (sv-scale dt
                               (sv-add (sv-scale 5179/57600 k1)
                                       (sv-add (sv-scale 7571/16695 k3)
                                               (sv-add (sv-scale 393/640 k4)
                                                       (sv-add (sv-scale -92097/339200 k5)
                                                               (sv-add (sv-scale 187/2100 k6)
                                                                       (sv-scale 1/40 k7))))))))]
         ;; RMS error
         [err-vec (sv-sub y5 y4)]
         [err (sqrt (/ (apply + (map (lambda (e) (* e e)) err-vec))
                       (length err-vec)))]
         ;; PI step controller: safety * (tol/error)^(1/5)
         [safety 0.9]
         [new-dt (* dt safety (expt (/ tol (max err 1e-10)) 1/5))])
    (list y5 err new-dt)))

;;; ============================================================
;;; Options
;;; ============================================================

(doc 'section 'options)

(doc 'type '(-> Alist))
(doc 'description "Default options for adaptive integration.")
(define (default-ode-opts)
  '((tolerance . 1e-6)
    (max-steps . 10000)
    (min-dt . 1e-12)
    (max-dt-growth . 2.0)))

(doc 'type '(-> Alist Symbol Any Any))
(doc 'description "Look up option value with fallback to defaults.")
(define (ode-opt opts key)
  (let ([pair (assq key opts)])
    (if pair
        (cdr pair)
        (let ([default-pair (assq key (default-ode-opts))])
          (if default-pair
              (cdr default-pair)
              (error 'ode-opt "unknown option" key))))))

;;; ============================================================
;;; Adaptive Integration
;;; ============================================================

(doc 'section 'integration)

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Number Alist
               Alist))
(doc 'description "Integrate an ODE with adaptive step-size control.

  f      : (t, y) → dy/dt  (right-hand side)
  t0     : initial time
  y0     : initial state (list of numbers)
  t-final: end time
  opts   : options alist (tolerance, max-steps, min-dt, max-dt-growth)

Returns a result alist:
  trajectory : list of (time . state) pairs
  steps      : total accepted steps
  rejections : total rejected steps
  final-dt   : final step size used")
(define (ode-adaptive-integrate f t0 y0 t-final opts)
  (let ([tol (ode-opt opts 'tolerance)]
        [max-steps (ode-opt opts 'max-steps)]
        [min-dt (ode-opt opts 'min-dt)]
        [max-growth (ode-opt opts 'max-dt-growth)]
        [dt0 (let ([pair (assq 'initial-dt opts)])
               (if pair (cdr pair)
                   ;; Default: 1/100th of interval
                   (/ (- t-final t0) 100)))])
    (let loop ([t t0]
               [y y0]
               [dt dt0]
               [steps 0]
               [rejections 0]
               [traj (list (cons t0 y0))])
      (cond
        ;; Reached end time
        [(>= t t-final)
         (list (cons 'trajectory (reverse traj))
               (cons 'steps steps)
               (cons 'rejections rejections)
               (cons 'final-dt dt))]
        ;; Exceeded step budget
        [(>= steps max-steps)
         (list (cons 'trajectory (reverse traj))
               (cons 'steps steps)
               (cons 'rejections rejections)
               (cons 'final-dt dt)
               (cons 'truncated #t))]
        [else
         (let* ([dt-clamped (min dt (- t-final t))]
                [result (dp45-step f t y dt-clamped tol)]
                [y-new (car result)]
                [err (cadr result)]
                [dt-suggested (caddr result)])
           (if (< err tol)
               ;; Accept step
               (let ([t-new (+ t dt-clamped)]
                     [dt-next (min dt-suggested (* max-growth dt))])
                 (loop t-new y-new dt-next
                       (+ steps 1) rejections
                       (cons (cons t-new y-new) traj)))
               ;; Reject step — shrink dt, enforce minimum
               (let ([dt-new (max (/ dt 2) min-dt)])
                 (if (<= dt-new min-dt)
                     ;; Hit minimum dt — accept anyway to avoid infinite loop
                     (let ([t-new (+ t dt-clamped)])
                       (loop t-new y-new min-dt
                             (+ steps 1) (+ rejections 1)
                             (cons (cons t-new y-new) traj)))
                     ;; Retry with smaller dt
                     (loop t y dt-new
                           steps (+ rejections 1)
                           traj)))))]))))

;;; ============================================================
;;; Result Accessors
;;; ============================================================

(doc 'section 'result-accessors)

(doc 'type '(-> Alist (List (Pair Number (List Number)))))
(doc 'description "Extract trajectory from integration result.")
(define (ode-result-trajectory result)
  (cdr (assq 'trajectory result)))

(doc 'type '(-> Alist Nat))
(doc 'description "Number of accepted steps.")
(define (ode-result-steps result)
  (cdr (assq 'steps result)))

(doc 'type '(-> Alist Nat))
(doc 'description "Number of rejected steps.")
(define (ode-result-rejections result)
  (cdr (assq 'rejections result)))

(doc 'type '(-> Alist Boolean))
(doc 'description "Was the integration truncated by step limit?")
(define (ode-result-truncated? result)
  (let ([pair (assq 'truncated result)])
    (and pair (cdr pair))))

(doc 'type '(-> Alist (Pair Number (List Number))))
(doc 'description "Extract the final (time . state) pair.")
(define (ode-result-final result)
  (let ([traj (ode-result-trajectory result)])
    (list-ref traj (- (length traj) 1))))

(doc 'type '(-> Alist (List Number)))
(doc 'description "Extract just the final state vector.")
(define (ode-result-final-state result)
  (cdr (ode-result-final result)))

(doc 'type '(-> Alist (List Number)))
(doc 'description "Extract time values from trajectory.")
(define (ode-result-times result)
  (map car (ode-result-trajectory result)))

;;; ============================================================
;;; ode-system Bridge
;;; ============================================================

(doc 'section 'ode-system-bridge)

(doc 'type '(-> Any Number (List Number) Number Alist Alist))
(doc 'description "Integrate an ode-system with adaptive stepping.
Bridges the ode-system abstraction (which uses Scheme vectors) to the
list-based adaptive solver. State is provided and returned as lists.")
(define (ode-system-integrate sys t0 y0 t-final opts)
  (let ([vf (ode-vector-field sys)])
    (ode-adaptive-integrate
      (lambda (t y)
        ;; Convert list→vector for the vector field, then vector→list back
        (let ([result (vf t (list->vector y))])
          (if (vector? result)
              (vector->list result)
              (if (list? result) result (list result)))))
      t0 y0 t-final opts)))

;;; ============================================================
;;; Convenience
;;; ============================================================

(doc 'section 'convenience)

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Number
               Alist))
(doc 'description "Quick adaptive integration with default options.
Equivalent to (ode-adaptive-integrate f t0 y0 t-final '()).")
(define (ode-solve f t0 y0 t-final)
  (ode-adaptive-integrate f t0 y0 t-final '()))

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Number Number
               Alist))
(doc 'description "Quick adaptive integration with specified tolerance.
Equivalent to (ode-adaptive-integrate f t0 y0 t-final '((tolerance . tol))).")
(define (ode-solve/tol f t0 y0 t-final tol)
  (ode-adaptive-integrate f t0 y0 t-final (list (cons 'tolerance tol))))
