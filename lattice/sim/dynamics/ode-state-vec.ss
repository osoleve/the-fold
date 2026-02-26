(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-state-vec
;;; @requires prelude
(require 'prelude)

(doc 'module 'ode-state-vec)
(doc 'description "State vector arithmetic for ODE solvers. State vectors are
plain lists of numbers. These operations are shared between ode-adaptive and
ode-integrators to avoid duplication.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; sv-add : (List Number) x (List Number) -> (List Number)
;;; Add two state vectors element-wise.
(define (sv-add a b)
  (map + a b))

;;; sv-sub : (List Number) x (List Number) -> (List Number)
;;; Subtract two state vectors element-wise.
(define (sv-sub a b)
  (map - a b))

;;; sv-scale : Number x (List Number) -> (List Number)
;;; Scale a state vector by a scalar.
(define (sv-scale k v)
  (map (lambda (x) (* k x)) v))

;;; sv-madd : (List Number) x Number x (List Number) -> (List Number)
;;; Fused multiply-add: a + k*b
(define (sv-madd a k b)
  (map (lambda (ai bi) (+ ai (* k bi))) a b))

;;; sv-norm : (List Number) -> Number
;;; Euclidean norm of a state vector.
(define (sv-norm v)
  (sqrt (apply + (map (lambda (x) (* x x)) v))))
