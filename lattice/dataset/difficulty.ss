;;; lattice/dataset/difficulty.ss — Difficulty Scoring and Calibration
;;; @module difficulty
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'difficulty)
(doc 'description "Difficulty scoring for visual reasoning problems. Calibrates based on inference depth, visual complexity, temporal reasoning, and discretization requirements.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Part 1: Difficulty Dimensions
;;; ============================================================
;;;
;;; Difficulty is multidimensional:
;;;   - inference-depth: direct → derivative → latent → system
;;;   - object-count: more objects = more tracking
;;;   - temporal-steps: more frames = more integration
;;;   - visual-precision: how carefully must you measure?
;;;   - formula-complexity: conceptual difficulty

(doc 'section 'dimensions)

;;; make-difficulty-vector : Nat × Nat × Nat × Nat × Nat → DifficultyVec
(define (make-difficulty-vector inference-depth object-count temporal-steps
                                  visual-precision formula-complexity)
  (doc 'export #t)
  (list 'difficulty-vec
        inference-depth
        object-count
        temporal-steps
        visual-precision
        formula-complexity))

(define (difficulty-vec? x) (and (pair? x) (eq? (car x) 'difficulty-vec)))
(define (dv-inference-depth dv) (list-ref dv 1))
(define (dv-object-count dv) (list-ref dv 2))
(define (dv-temporal-steps dv) (list-ref dv 3))
(define (dv-visual-precision dv) (list-ref dv 4))
(define (dv-formula-complexity dv) (list-ref dv 5))

;;; ============================================================
;;; Part 2: Inference Depth Levels
;;; ============================================================
;;;
;;; 0 = Direct observation (count objects, compare sizes)
;;; 1 = First derivative (velocity from position change)
;;; 2 = Latent property (mass from collision outcome)
;;; 3 = System dynamics (pendulum period, spring stiffness)

(doc 'section 'inference-depth)

(doc inference-direct 'export #t)
(define inference-direct 0)

(doc inference-derivative 'export #t)
(define inference-derivative 1)

(doc inference-latent 'export #t)
(define inference-latent 2)

(doc inference-system 'export #t)
(define inference-system 3)

;;; classify-inference-depth : Symbol → Nat
;;; Map problem category to inference depth
(define (classify-inference-depth category)
  (doc 'export #t)
  (case category
    [(count size position distance spatial) inference-direct]
    [(velocity speed direction kinematic) inference-derivative]
    [(mass momentum restitution dynamic) inference-latent]
    [(period frequency constraint) inference-system]
    [else inference-direct]))

;;; ============================================================
;;; Part 3: Difficulty Scoring
;;; ============================================================

(doc 'section 'scoring)

;;; difficulty-weights : Alist
;;; Default weights for each dimension
(doc difficulty-weights 'export #t)
(define difficulty-weights
  '((inference-depth . 2.0)
    (object-count . 0.5)
    (temporal-steps . 0.3)
    (visual-precision . 1.0)
    (formula-complexity . 1.5)))

;;; difficulty-score : DifficultyVec × Alist → Number
;;; Compute weighted difficulty score
(define (difficulty-score dv weights)
  (doc 'export #t)
  (let ([w-inf (cdr (or (assq 'inference-depth weights) '(_ . 2.0)))]
        [w-obj (cdr (or (assq 'object-count weights) '(_ . 0.5)))]
        [w-tmp (cdr (or (assq 'temporal-steps weights) '(_ . 0.3)))]
        [w-vis (cdr (or (assq 'visual-precision weights) '(_ . 1.0)))]
        [w-frm (cdr (or (assq 'formula-complexity weights) '(_ . 1.5)))])
    (+ (* w-inf (dv-inference-depth dv))
       (* w-obj (log (+ 1 (dv-object-count dv))))
       (* w-tmp (log (+ 1 (dv-temporal-steps dv))))
       (* w-vis (dv-visual-precision dv))
       (* w-frm (dv-formula-complexity dv)))))

;;; difficulty-score-default : DifficultyVec → Number
(define (difficulty-score-default dv)
  (doc 'export #t)
  (difficulty-score dv difficulty-weights))

;;; ============================================================
;;; Part 4: Difficulty Tiers
;;; ============================================================
;;;
;;; Map continuous scores to discrete tiers for curriculum design.

(doc 'section 'tiers)

;;; difficulty-tier : Number → Symbol
;;; Classify score into tier
(define (difficulty-tier score)
  (doc 'export #t)
  (cond
    [(< score 2.0) 'trivial]
    [(< score 4.0) 'easy]
    [(< score 6.0) 'medium]
    [(< score 8.0) 'hard]
    [else 'expert]))

;;; tier-order : Symbol → Nat
;;; Numeric ordering for tiers
(define (tier-order tier)
  (doc 'export #t)
  (case tier
    [(trivial) 0]
    [(easy) 1]
    [(medium) 2]
    [(hard) 3]
    [(expert) 4]
    [else 2]))

;;; tier-compare : Symbol × Symbol → Number
;;; Compare two tiers (-1, 0, 1)
(define (tier-compare t1 t2)
  (doc 'export #t)
  (let ([o1 (tier-order t1)]
        [o2 (tier-order t2)])
    (cond
      [(< o1 o2) -1]
      [(> o1 o2) 1]
      [else 0])))

;;; ============================================================
;;; Part 5: Solvability Constraints
;;; ============================================================
;;;
;;; Problems must be solvable from the visual information provided.

(doc 'section 'solvability)

;;; make-solvability-check : Alist → SolvabilityCheck
;;; Create a solvability constraint specification
(define (make-solvability-check constraints)
  (doc 'export #t)
  (list 'solvability-check constraints))

(define (solvability-check? x) (and (pair? x) (eq? (car x) 'solvability-check)))
(define (solvability-constraints sc) (list-ref sc 1))

;;; check-temporal-resolution : Nat × Number × Number → Boolean
;;; Ensure objects don't move too fast (no tunneling)
;;; max-velocity: physics units/second, dt: seconds, scale: pixels/unit
(define (check-temporal-resolution max-velocity dt scale)
  (doc 'export #t)
  (doc 'description "Check that objects move at most 3 characters per frame")
  (let ([chars-per-frame (* max-velocity dt scale)])
    (<= chars-per-frame 3.0)))

;;; check-visual-distinctness : Number × Number × Number → Boolean
;;; Ensure objects are visually distinct (different sizes)
(define (check-visual-distinctness size1 size2 scale min-diff)
  (doc 'export #t)
  (doc 'description "Check that size difference is visible (at least min-diff chars)")
  (let ([char-diff (abs (* (- size1 size2) scale))])
    (>= char-diff min-diff)))

;;; check-discretization : Number × Number × Number → Boolean
;;; Ensure answer can be derived from ASCII (not just floats)
(define (check-discretization answer tolerance scale)
  (doc 'export #t)
  (doc 'description "Check that answer is recoverable within tolerance from visual")
  (let ([char-precision (/ 1.0 scale)])
    (<= char-precision tolerance)))

;;; ============================================================
;;; Part 6: Problem Categories
;;; ============================================================
;;;
;;; Pre-defined difficulty profiles for problem types.

(doc 'section 'categories)

;;; spatial-difficulty : DifficultyVec
;;; Baseline for spatial problems (count, size, distance)
(doc spatial-difficulty 'export #t)
(define spatial-difficulty
  (make-difficulty-vector inference-direct 2 1 1 0))

;;; kinematic-difficulty : DifficultyVec
;;; Baseline for kinematic problems (velocity, timing)
(doc kinematic-difficulty 'export #t)
(define kinematic-difficulty
  (make-difficulty-vector inference-derivative 2 4 2 1))

;;; dynamic-difficulty : DifficultyVec
;;; Baseline for dynamic problems (mass, momentum)
(doc dynamic-difficulty 'export #t)
(define dynamic-difficulty
  (make-difficulty-vector inference-latent 2 6 2 2))

;;; constraint-difficulty : DifficultyVec
;;; Baseline for constraint problems (pendulum, spring)
(doc constraint-difficulty 'export #t)
(define constraint-difficulty
  (make-difficulty-vector inference-system 1 8 2 3))

;;; ============================================================
;;; Part 7: Difficulty Adjustment
;;; ============================================================
;;;
;;; Modify difficulty by adjusting parameters.

(doc 'section 'adjustment)

;;; adjust-for-objects : DifficultyVec × Nat → DifficultyVec
;;; Increase difficulty for more objects
(define (adjust-for-objects dv n)
  (doc 'export #t)
  (make-difficulty-vector
   (dv-inference-depth dv)
   n
   (dv-temporal-steps dv)
   (dv-visual-precision dv)
   (dv-formula-complexity dv)))

;;; adjust-for-frames : DifficultyVec × Nat → DifficultyVec
;;; Increase difficulty for more temporal steps
(define (adjust-for-frames dv n)
  (doc 'export #t)
  (make-difficulty-vector
   (dv-inference-depth dv)
   (dv-object-count dv)
   n
   (dv-visual-precision dv)
   (dv-formula-complexity dv)))

;;; adjust-for-precision : DifficultyVec × Nat → DifficultyVec
;;; Increase difficulty for higher visual precision needed
(define (adjust-for-precision dv p)
  (doc 'export #t)
  (make-difficulty-vector
   (dv-inference-depth dv)
   (dv-object-count dv)
   (dv-temporal-steps dv)
   p
   (dv-formula-complexity dv)))

;;; ============================================================
;;; Part 8: Curriculum Sequencing
;;; ============================================================

(doc 'section 'curriculum)

;;; curriculum-stages : (List Symbol)
;;; Recommended progression order
(doc curriculum-stages 'export #t)
(define curriculum-stages
  '(object-permanence  ; Single static frames, count/compare
    linear-motion      ; 1 object, constant velocity
    gravity            ; Projectile motion, parabolic
    collisions         ; 2 objects, momentum
    multi-step         ; Causal chains
    constraints))      ; Pendulums, springs

;;; stage-categories : Symbol → (List Symbol)
;;; Map stage to allowed problem categories
(define (stage-categories stage)
  (doc 'export #t)
  (case stage
    [(object-permanence) '(count size position distance)]
    [(linear-motion) '(velocity direction speed)]
    [(gravity) '(velocity trajectory position)]
    [(collisions) '(velocity momentum restitution mass)]
    [(multi-step) '(trajectory collision velocity)]
    [(constraints) '(period frequency amplitude)]
    [else '()]))

;;; stage-max-difficulty : Symbol → Number
;;; Maximum difficulty score for stage
(define (stage-max-difficulty stage)
  (doc 'export #t)
  (case stage
    [(object-permanence) 2.0]
    [(linear-motion) 4.0]
    [(gravity) 5.0]
    [(collisions) 6.5]
    [(multi-step) 7.5]
    [(constraints) 9.0]
    [else 5.0]))
