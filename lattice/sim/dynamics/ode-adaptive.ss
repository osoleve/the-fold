(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-adaptive
;;; @requires prelude ode-system ode-state-vec vector-space
(require 'prelude)
(require 'ode-system)
(require 'ode-state-vec)
(require 'vector-space)

(doc 'module 'ode-adaptive)
(doc 'description "Generic adaptive step-size ODE solver using embedded Dormand-Prince RK4(5).
Provides error-controlled integration with PI step controller, min/max dt protection,
and structured result output. Parameterized by vector-space for generic vector operations.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Dormand-Prince RK4(5) Single Step
;;; ============================================================

(doc 'section 'rk45)

(doc 'description "Single Dormand-Prince RK4(5) step, parameterized by vector-space.
Takes a vspace, right-hand-side function f(t, y) → dy/dt, current time, state,
step size, and tolerance. Returns (new-state error-estimate suggested-dt).")
(define (dp45-step f t y dt tol . rest)
  (doc 'export #t)
  ;; Optional vspace argument; defaults to list-vspace for backward compat
  (let* ([vs (if (null? rest) list-vspace (car rest))]
         [add (vspace-add vs)]
         [scale (vspace-scale vs)]
         [sub (vspace-sub vs)]
         [madd (vspace-madd vs)]
         [norm (vspace-norm vs)]
         ;; Stage evaluations (Dormand-Prince coefficients)
         [k1 (f t y)]
         [k2 (f (+ t (* 1/5 dt))
                (madd y (* 1/5 dt) k1))]
         [k3 (f (+ t (* 3/10 dt))
                (add y
                     (add (scale (* 3/40 dt) k1)
                          (scale (* 9/40 dt) k2))))]
         [k4 (f (+ t (* 4/5 dt))
                (add y
                     (add (scale (* 44/45 dt) k1)
                          (add (scale (* -56/15 dt) k2)
                               (scale (* 32/9 dt) k3)))))]
         [k5 (f (+ t (* 8/9 dt))
                (add y
                     (add (scale (* 19372/6561 dt) k1)
                          (add (scale (* -25360/2187 dt) k2)
                               (add (scale (* 64448/6561 dt) k3)
                                    (scale (* -212/729 dt) k4))))))]
         [k6 (f (+ t dt)
                (add y
                     (add (scale (* 9017/3168 dt) k1)
                          (add (scale (* -355/33 dt) k2)
                               (add (scale (* 46732/5247 dt) k3)
                                    (add (scale (* 49/176 dt) k4)
                                         (scale (* -5103/18656 dt) k5)))))))]
         ;; 5th order solution (used as the result)
         [y5 (add y
                  (scale dt
                         (add (scale 35/384 k1)
                              (add (scale 500/1113 k3)
                                   (add (scale 125/192 k4)
                                        (add (scale -2187/6784 k5)
                                             (scale 11/84 k6)))))))]
         ;; 4th order solution (for error estimate)
         [k7 (f (+ t dt) y5)]
         [y4 (add y
                  (scale dt
                         (add (scale 5179/57600 k1)
                              (add (scale 7571/16695 k3)
                                   (add (scale 393/640 k4)
                                        (add (scale -92097/339200 k5)
                                             (add (scale 187/2100 k6)
                                                  (scale 1/40 k7))))))))]
         ;; RMS error via vspace norm
         [err-vec (sub y5 y4)]
         [dim ((vspace-dim vs) y)]
         [err (/ (norm err-vec) (sqrt dim))]
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
  (doc 'export #t)
  '((tolerance . 1e-6)
    (max-steps . 10000)
    (min-dt . 1e-12)
    (max-dt-growth . 2.0)))

(doc 'type '(-> Alist Symbol Any Any))
(doc 'description "Look up option value with fallback to defaults.")
(define (ode-opt opts key)
  (doc 'export #t)
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

(doc 'description "Integrate an ODE with adaptive step-size control.

  f      : (t, y) → dy/dt  (right-hand side)
  t0     : initial time
  y0     : initial state
  t-final: end time
  opts   : options alist (tolerance, max-steps, min-dt, max-dt-growth, vspace)

Returns a result alist:
  trajectory : list of (time . state) pairs
  steps      : total accepted steps
  rejections : total rejected steps
  final-dt   : final step size used")
(define (ode-adaptive-integrate f t0 y0 t-final opts)
  (doc 'export #t)
  (let ([tol (ode-opt opts 'tolerance)]
        [max-steps (ode-opt opts 'max-steps)]
        [min-dt (ode-opt opts 'min-dt)]
        [max-growth (ode-opt opts 'max-dt-growth)]
        [vs (let ([pair (assq 'vspace opts)])
              (if pair (cdr pair) list-vspace))]
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
                [result (dp45-step f t y dt-clamped tol vs)]
                [y-new (car result)]
                [err (cadr result)]
                [dt-suggested (caddr result)])
           (if (< err tol)
               ;; Accept step
               (let ([t-new (+ t dt-clamped)]
                     [dt-next (max min-dt (min dt-suggested (* max-growth dt)))])
                 (loop t-new y-new dt-next
                       (+ steps 1) rejections
                       (cons (cons t-new y-new) traj)))
               ;; Reject step — shrink dt, enforce minimum
               (if (<= dt-clamped min-dt)
                   ;; Already at minimum dt — accept anyway to avoid infinite loop
                   (let ([t-new (+ t dt-clamped)])
                     (loop t-new y-new min-dt
                           (+ steps 1) (+ rejections 1)
                           (cons (cons t-new y-new) traj)))
                   ;; Retry with smaller dt
                   (let ([dt-new (max (/ dt 2) min-dt)])
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
  (doc 'export #t)
  (cdr (assq 'trajectory result)))

(doc 'type '(-> Alist Nat))
(doc 'description "Number of accepted steps.")
(define (ode-result-steps result)
  (doc 'export #t)
  (cdr (assq 'steps result)))

(doc 'type '(-> Alist Nat))
(doc 'description "Number of rejected steps.")
(define (ode-result-rejections result)
  (doc 'export #t)
  (cdr (assq 'rejections result)))

(doc 'type '(-> Alist Boolean))
(doc 'description "Was the integration truncated by step limit?")
(define (ode-result-truncated? result)
  (doc 'export #t)
  (let ([pair (assq 'truncated result)])
    (and pair (cdr pair))))

(doc 'type '(-> Alist (Pair Number (List Number))))
(doc 'description "Extract the final (time . state) pair.")
(define (ode-result-final result)
  (doc 'export #t)
  (let ([traj (ode-result-trajectory result)])
    (list-ref traj (- (length traj) 1))))

(doc 'type '(-> Alist (List Number)))
(doc 'description "Extract just the final state vector.")
(define (ode-result-final-state result)
  (doc 'export #t)
  (cdr (ode-result-final result)))

(doc 'type '(-> Alist (List Number)))
(doc 'description "Extract time values from trajectory.")
(define (ode-result-times result)
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
  (ode-adaptive-integrate f t0 y0 t-final '()))

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Number Number
               Alist))
(doc 'description "Quick adaptive integration with specified tolerance.
Equivalent to (ode-adaptive-integrate f t0 y0 t-final '((tolerance . tol))).")
(define (ode-solve/tol f t0 y0 t-final tol)
  (doc 'export #t)
  (ode-adaptive-integrate f t0 y0 t-final (list (cons 'tolerance tol))))
