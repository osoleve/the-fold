(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module dynamics-ops
;;; @requires prelude
(require 'prelude)

(doc 'module 'dynamics-ops)
(doc 'description "Dimension-Agnostic Dynamics Operations

Generic loop machinery for differentiable physics simulation.
These functions are parameterized only by a step function — they
treat the state as opaque. Both 2D (rollout.ss) and 3D (rollout3d.ss)
delegate to these implementations.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'basic-rollout)

;;; generic-rollout : α × (α → α) × Nat → α
;;; Apply step-fn to state n times. Returns final state.
(define (generic-rollout initial step-fn n)
  (let loop ([state initial] [i 0])
       (if (>= i n)
           state
           (loop (step-fn state) (+ i 1)))))

;;; generic-rollout-trajectory : α × (α → α) × Nat → (List α)
;;; Apply step-fn n times, collecting all intermediate states.
;;; Returns list of states from step 0 to step n.
(define (generic-rollout-trajectory initial step-fn n)
  (let loop ([state initial] [i 0] [trajectory (list initial)])
       (if (>= i n)
           (reverse trajectory)
           (let ([new-state (step-fn state)])
                (loop new-state (+ i 1) (cons new-state trajectory))))))

;;; generic-trajectory-final : (List α) → α
;;; Extract the final state from a trajectory.
(define (generic-trajectory-final traj)
  (car (reverse traj)))

(doc 'section 'checkpointing)

;;; generic-checkpoint-interval : Nat → Nat
;;; Compute optimal checkpoint interval for T steps.
;;; Returns approximately sqrt(T).
(define (generic-checkpoint-interval num-steps)
  (max 1 (exact (ceiling (sqrt num-steps)))))

;;; generic-rollout-checkpoints : α × (α → α) × Nat × Nat → (Vector α)
;;; Roll out simulation storing periodic checkpoints.
;;; Uses non-traced arithmetic to save memory.
(define (generic-rollout-checkpoints initial step-fn num-steps checkpoint-freq)
  (let* ([num-checkpoints (+ 1 (exact (ceiling (/ num-steps checkpoint-freq))))]
         [checkpoints (make-vector num-checkpoints)])
        (vector-set! checkpoints 0 initial)
        (let loop ([state initial] [step 0] [ckpt-idx 1])
             (if (>= step num-steps)
                 checkpoints
                 (let ([new-state (step-fn state)]
                       [next-step (+ step 1)])
                      (if (and (= (mod next-step checkpoint-freq) 0)
                               (< ckpt-idx num-checkpoints))
                          (begin
                           (vector-set! checkpoints ckpt-idx new-state)
                           (loop new-state next-step (+ ckpt-idx 1)))
                          (loop new-state next-step ckpt-idx)))))))

(doc 'section 'substepping)

;;; generic-rollout-substep : α × (α → α) × Nat × Nat → α
;;; Roll out with substepping.
;;; substeps: number of sub-iterations per main step.
(define (generic-rollout-substep initial step-fn num-steps substeps)
  (generic-rollout initial
                   (lambda (state) (generic-rollout state step-fn substeps))
                   num-steps))
