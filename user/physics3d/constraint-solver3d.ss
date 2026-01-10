;;; user/physics3d/constraint-solver3d.ss — 3D Constraint Solvers
;;;
;;; Sequential impulse solvers for 3D physics constraints:
;;;   - Distance constraint (1 DOF)
;;;   - Ball-socket joint (3 DOF constraint)
;;;   - Hinge joint (5 DOF constraint)
;;;   - Fixed joint (6 DOF constraint)
;;;   - Spring constraint (soft 1 DOF)
;;;
;;; Each solver has:
;;;   - Velocity solver: Applies corrective impulses
;;;   - Position solver: Fixes positional drift
;;;
;;; Uses Baumgarte stabilization for position correction.
;;;
;;; This is user code: pragmatic implementation with world mutation.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec3.ss
;;;   - core/linalg/quaternion.ss
;;;   - user/physics3d/rigid-body3d.ss
;;;   - user/physics3d/constraints3d.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec3.ss")
(load "core/linalg/quaternion.ss")
(load "user/physics3d/rigid-body3d.ss")
(load "user/physics3d/constraints3d.ss")

;;; ============================================================
;;; Solver Constants
;;; ============================================================

(define *baumgarte-factor-3d* 0.2)      ; Position correction bias
(define *position-slop-3d* 0.005)       ; Allow small penetration
(define *angular-slop-3d* 0.0001)       ; Allow small angle error

;;; ============================================================
;;; Helper: Apply Impulse to Body
;;; ============================================================

;;; apply-impulse-at-point-3d : RigidBody3D × Vec3 × Vec3 → RigidBody3D
;;; Apply an impulse at a world point to a body.
;;; Updates both linear and angular velocity.
;;; (Uses rigid-body-3d-apply-impulse from rigid-body3d.ss)
(define (apply-impulse-at-point-3d body impulse world-point)
  (rigid-body-3d-apply-impulse body impulse world-point))

;;; apply-angular-impulse-3d : RigidBody3D × Vec3 → RigidBody3D
;;; Apply a pure angular impulse (no linear component).
;;; (Uses rigid-body-3d-apply-torque-impulse from rigid-body3d.ss)
(define (apply-angular-impulse-3d body angular-impulse)
  (rigid-body-3d-apply-torque-impulse body angular-impulse))

;;; ============================================================
;;; Helper: Velocity at Point
;;; ============================================================

;;; velocity-at-point-3d : RigidBody3D × Vec3 → Vec3
;;; Get velocity at a world-space point on the body.
;;; (Uses rigid-body-3d-velocity-at from rigid-body3d.ss)
(define (velocity-at-point-3d body world-point)
  (rigid-body-3d-velocity-at body world-point))

;;; ============================================================
;;; Helper: Effective Mass for 3D Point Constraint
;;; ============================================================

;;; compute-effective-mass-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 → Mat3
;;; Compute the effective mass matrix for a point constraint.
;;; K = 1/m_a + 1/m_b + [r_a×]^T I_a^-1 [r_a×] + [r_b×]^T I_b^-1 [r_b×]
;;; Returns the inverse of the effective mass matrix.
(define (compute-effective-mass-inv-3d body-a body-b r-a r-b)
  (let* ([inv-mass-a (if body-a (rigid-body-3d-inv-mass body-a) 0)]
         [inv-mass-b (if body-b (rigid-body-3d-inv-mass body-b) 0)]
         [I-inv-a (if body-a (rigid-body-3d-world-inv-inertia body-a) (mat3-zero))]
         [I-inv-b (if body-b (rigid-body-3d-world-inv-inertia body-b) (mat3-zero))]
         ;; The effective mass matrix in 3D is:
         ;; K_ii = 1/m + [r×]^T I^-1 [r×]
         ;; For simplicity, compute diagonal elements first
         ;; This is a simplification; full implementation would compute 3x3 matrix
         [total-inv-mass (+ inv-mass-a inv-mass-b)]
         ;; Angular contribution (simplified for diagonal inertia):
         ;; K += [r×]^T I^-1 [r×]
         ;; For unit vector n, the effective mass along n is:
         ;; 1/m + (r × n)^T I^-1 (r × n)
         )
        ;; Return scalar effective mass for now (simplification)
        ;; Full 3x3 matrix would be needed for accurate solver
        total-inv-mass))

;;; ============================================================
;;; Distance Constraint Solver
;;; ============================================================

;;; solve-distance-velocity-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 × Number × Number → (RigidBody3D × RigidBody3D | #f)
;;; Solve velocity constraint for distance joint.
(define (solve-distance-velocity-3d body-a body-b pos-a pos-b target-dist dt)
  (let* (;; Constraint axis (normalized direction)
         [delta (vec3-sub pos-b pos-a)]
         [current-dist (vec3-magnitude delta)]
         [n (if (< current-dist 0.0001)
                (vec3 1 0 0)
                (vec3-normalize delta))]
         
         ;; Get velocities at anchor points
         [vel-a (velocity-at-point-3d body-a pos-a)]
         [vel-b (if body-b (velocity-at-point-3d body-b pos-b) (vec3-zero))]
         
         ;; Relative velocity along constraint axis
         [rel-vel (vec3-sub vel-b vel-a)]
         [v-n (vec3-dot rel-vel n)]
         
         ;; Compute effective mass
         [r-a (vec3-sub pos-a (rigid-body-3d-pos body-a))]
         [r-b (if body-b (vec3-sub pos-b (rigid-body-3d-pos body-b)) (vec3-zero))]
         [inv-mass-a (rigid-body-3d-inv-mass body-a)]
         [inv-mass-b (if body-b (rigid-body-3d-inv-mass body-b) 0)]
         
         ;; For distance constraint, effective mass along axis n:
         ;; K = 1/m_a + 1/m_b + (r_a × n) · I_a^-1 · (r_a × n) + (r_b × n) · I_b^-1 · (r_b × n)
         [rn-a (vec3-cross r-a n)]
         [rn-b (vec3-cross r-b n)]
         [I-inv-a (rigid-body-3d-world-inv-inertia body-a)]
         [I-inv-b (if body-b (rigid-body-3d-world-inv-inertia body-b) (mat3-zero))]
         [ang-term-a (vec3-dot rn-a (mat3-mul-vec3 I-inv-a rn-a))]
         [ang-term-b (vec3-dot rn-b (mat3-mul-vec3 I-inv-b rn-b))]
         [effective-mass (+ inv-mass-a inv-mass-b ang-term-a ang-term-b)]
         
         ;; Compute impulse magnitude
         [lambda (if (< effective-mass 0.0001)
                     0
                     (/ (- v-n) effective-mass))]
         [impulse (vec3-scale n lambda)]
         
         ;; Apply impulses
         [new-a (apply-impulse-at-point-3d body-a (vec3-neg impulse) pos-a)]
         [new-b (if body-b (apply-impulse-at-point-3d body-b impulse pos-b) #f)])
        
        (values new-a new-b)))

;;; correct-distance-position-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 × Number → (RigidBody3D × RigidBody3D | #f)
;;; Apply position correction for distance constraint.
(define (correct-distance-position-3d body-a body-b pos-a pos-b target-dist)
  (let* (;; Position error
         [delta (vec3-sub pos-b pos-a)]
         [current-dist (vec3-magnitude delta)]
         [C (- current-dist target-dist)]
         [n (if (< current-dist 0.0001)
                (vec3 1 0 0)
                (vec3-normalize delta))])
        
        ;; Skip if within tolerance
        (if (<= (abs C) *position-slop-3d*)
            (values body-a body-b)
            (let* ([inv-mass-a (rigid-body-3d-inv-mass body-a)]
                   [inv-mass-b (if body-b (rigid-body-3d-inv-mass body-b) 0)]
                   [total-inv-mass (+ inv-mass-a inv-mass-b)]
                   [correction-mag (* *baumgarte-factor-3d* (- (abs C) *position-slop-3d*)
                                      (if (> C 0) 1 -1))]
                   [correction-vec (vec3-scale n correction-mag)]
                   
                   ;; Apply position corrections (move centers of mass)
                   [new-pos-a (if (and (not (rigid-body-3d-static? body-a))
                                       (> total-inv-mass 0.0001))
                                  (vec3-add (rigid-body-3d-pos body-a)
                                            (vec3-scale correction-vec
                                                        (/ inv-mass-a total-inv-mass)))
                                  (rigid-body-3d-pos body-a))]
                   [new-pos-b (if (and body-b
                                       (not (rigid-body-3d-static? body-b))
                                       (> total-inv-mass 0.0001))
                                  (vec3-sub (rigid-body-3d-pos body-b)
                                            (vec3-scale correction-vec
                                                        (/ inv-mass-b total-inv-mass)))
                                  (if body-b (rigid-body-3d-pos body-b) #f))]
                   
                   [new-a (rigid-body-3d-with-pos body-a new-pos-a)]
                   [new-b (if body-b (rigid-body-3d-with-pos body-b new-pos-b) #f)])
                  
                  (values new-a new-b)))))

;;; ============================================================
;;; Ball-Socket Joint Solver (3 DOF Point Constraint)
;;; ============================================================

;;; solve-ball-socket-velocity-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 × Number → (RigidBody3D × RigidBody3D | #f)
;;; Solve velocity constraint for ball-socket joint.
;;; Both anchors must move together (3D point-to-point).
(define (solve-ball-socket-velocity-3d body-a body-b pos-a pos-b dt)
  (let* (;; Get velocities at anchor points
         [vel-a (velocity-at-point-3d body-a pos-a)]
         [vel-b (if body-b (velocity-at-point-3d body-b pos-b) (vec3-zero))]
         
         ;; Relative velocity
         [rel-vel (vec3-sub vel-b vel-a)]
         
         ;; Lever arms
         [r-a (vec3-sub pos-a (rigid-body-3d-pos body-a))]
         [r-b (if body-b (vec3-sub pos-b (rigid-body-3d-pos body-b)) (vec3-zero))]
         
         ;; Compute effective mass (simplified: scalar for each axis)
         [inv-mass-a (rigid-body-3d-inv-mass body-a)]
         [inv-mass-b (if body-b (rigid-body-3d-inv-mass body-b) 0)]
         [effective-mass (+ inv-mass-a inv-mass-b)]
         
         ;; Compute impulse (simplified: direct solve)
         [impulse (if (< effective-mass 0.0001)
                      (vec3-zero)
                      (vec3-scale rel-vel (/ -1 effective-mass)))]
         
         ;; Apply impulses
         [new-a (apply-impulse-at-point-3d body-a (vec3-neg impulse) pos-a)]
         [new-b (if body-b (apply-impulse-at-point-3d body-b impulse pos-b) #f)])
        
        (values new-a new-b)))

;;; correct-ball-socket-position-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 → (RigidBody3D × RigidBody3D | #f)
;;; Apply position correction for ball-socket joint.
(define (correct-ball-socket-position-3d body-a body-b pos-a pos-b)
  (let* (;; Position error
         [C (vec3-sub pos-b pos-a)]
         [error-mag (vec3-magnitude C)])
        
        ;; Skip if within tolerance
        (if (<= error-mag *position-slop-3d*)
            (values body-a body-b)
            (let* ([inv-mass-a (rigid-body-3d-inv-mass body-a)]
                   [inv-mass-b (if body-b (rigid-body-3d-inv-mass body-b) 0)]
                   [total-inv-mass (+ inv-mass-a inv-mass-b)]
                   [correction (vec3-scale C *baumgarte-factor-3d*)]
                   
                   ;; Apply position corrections
                   [new-pos-a (if (and (not (rigid-body-3d-static? body-a))
                                       (> total-inv-mass 0.0001))
                                  (vec3-add (rigid-body-3d-pos body-a)
                                            (vec3-scale correction
                                                        (/ inv-mass-a total-inv-mass)))
                                  (rigid-body-3d-pos body-a))]
                   [new-pos-b (if (and body-b
                                       (not (rigid-body-3d-static? body-b))
                                       (> total-inv-mass 0.0001))
                                  (vec3-sub (rigid-body-3d-pos body-b)
                                            (vec3-scale correction
                                                        (/ inv-mass-b total-inv-mass)))
                                  (if body-b (rigid-body-3d-pos body-b) #f))]
                   
                   [new-a (rigid-body-3d-with-pos body-a new-pos-a)]
                   [new-b (if body-b (rigid-body-3d-with-pos body-b new-pos-b) #f)])
                  
                  (values new-a new-b)))))

;;; ============================================================
;;; Spring Constraint Solver (Soft 1 DOF)
;;; ============================================================

;;; solve-spring-velocity-3d : RigidBody3D × RigidBody3D | #f × Vec3 × Vec3 × Number × Number × Number × Number
;;;                          → (RigidBody3D × RigidBody3D | #f)
;;; Solve velocity constraint for spring (soft constraint).
(define (solve-spring-velocity-3d body-a body-b pos-a pos-b rest-length stiffness damping dt)
  (let* (;; Spring direction
         [delta (vec3-sub pos-b pos-a)]
         [current-dist (vec3-magnitude delta)]
         [n (if (< current-dist 0.0001)
                (vec3 1 0 0)
                (vec3-normalize delta))]
         
         ;; Extension from rest length
         [stretch (- current-dist rest-length)]
         
         ;; Get velocities at anchor points
         [vel-a (velocity-at-point-3d body-a pos-a)]
         [vel-b (if body-b (velocity-at-point-3d body-b pos-b) (vec3-zero))]
         
         ;; Relative velocity along spring axis
         [rel-vel (vec3-sub vel-b vel-a)]
         [v-n (vec3-dot rel-vel n)]
         
         ;; Spring force: F = k*x + c*v (semi-implicit)
         [force-magnitude (+ (* stiffness stretch) (* damping v-n))]
         [impulse-mag (* force-magnitude dt)]
         [impulse (vec3-scale n impulse-mag)]
         
         ;; Apply impulses
         ;; When stretched (positive force), A should be pulled toward B (+n direction for A)
         [new-a (apply-impulse-at-point-3d body-a impulse pos-a)]
         [new-b (if body-b (apply-impulse-at-point-3d body-b (vec3-neg impulse) pos-b) #f)])
        
        (values new-a new-b)))

;;; Springs don't need position correction (soft constraint)
(define (correct-spring-position-3d body-a body-b pos-a pos-b rest-length)
  (values body-a body-b))

;;; ============================================================
;;; Hinge Joint Solver (5 DOF: 3 point + 2 angular)
;;; ============================================================

;;; solve-hinge-velocity-3d : RigidBody3D × RigidBody3D × Vec3 × Vec3 × Vec3 × Vec3 × Number
;;;                         → (RigidBody3D × RigidBody3D)
;;; Solve velocity constraint for hinge joint.
(define (solve-hinge-velocity-3d body-a body-b pos-a pos-b axis-a axis-b dt)
  ;; First solve the point constraint (like ball-socket)
  (call-with-values
   (lambda () (solve-ball-socket-velocity-3d body-a body-b pos-a pos-b dt))
   (lambda (new-a new-b)
           ;; Then solve the angular alignment constraint
           ;; The axes should be parallel: axis-a × axis-b = 0
           (let* ([world-axis-a (local-dir-to-world-3d new-a axis-a)]
                  [world-axis-b (if new-b (local-dir-to-world-3d new-b axis-b) axis-b)]
                  [axis-error (vec3-cross world-axis-a world-axis-b)]
                  
                  ;; Angular velocities
                  [omega-a (rigid-body-3d-angular-vel new-a)]
                  [omega-b (if new-b (rigid-body-3d-angular-vel new-b) (vec3-zero))]
                  [rel-omega (vec3-sub omega-b omega-a)]
                  
                  ;; Project out the hinge axis component
                  [along-axis (vec3-dot rel-omega world-axis-a)]
                  [rel-omega-perp (vec3-sub rel-omega (vec3-scale world-axis-a along-axis))]
                  
                  ;; Angular impulse (simplified)
                  [I-inv-a (rigid-body-3d-world-inv-inertia new-a)]
                  [I-inv-b (if new-b (rigid-body-3d-world-inv-inertia new-b) (mat3-zero))]
                  [effective-I-inv (+ (mat3-trace I-inv-a) (mat3-trace I-inv-b))]
                  [angular-impulse (if (< effective-I-inv 0.0001)
                                       (vec3-zero)
                                       (vec3-scale rel-omega-perp (/ -1 (max 0.1 effective-I-inv))))]
                  
                  [final-a (apply-angular-impulse-3d new-a (vec3-neg angular-impulse))]
                  [final-b (if new-b (apply-angular-impulse-3d new-b angular-impulse) #f)])
                 
                 (values final-a final-b)))))

;;; correct-hinge-position-3d : Similar to ball-socket + angular correction
(define (correct-hinge-position-3d body-a body-b pos-a pos-b axis-a axis-b)
  ;; First correct point constraint
  (call-with-values
   (lambda () (correct-ball-socket-position-3d body-a body-b pos-a pos-b))
   (lambda (new-a new-b)
           ;; Angular correction would be done here if needed
           ;; For now, return the position-corrected bodies
           (values new-a new-b))))

;;; ============================================================
;;; Fixed Joint Solver (6 DOF)
;;; ============================================================

;;; solve-fixed-velocity-3d : RigidBody3D × RigidBody3D × Vec3 × Vec3 × Quat × Number
;;;                         → (RigidBody3D × RigidBody3D)
;;; Solve velocity constraint for fixed joint (no relative motion).
(define (solve-fixed-velocity-3d body-a body-b pos-a pos-b ref-orient dt)
  ;; First solve the point constraint
  (call-with-values
   (lambda () (solve-ball-socket-velocity-3d body-a body-b pos-a pos-b dt))
   (lambda (new-a new-b)
           ;; Then solve all angular constraints
           (let* (;; Angular velocities should be equal
                  [omega-a (rigid-body-3d-angular-vel new-a)]
                  [omega-b (if new-b (rigid-body-3d-angular-vel new-b) (vec3-zero))]
                  [rel-omega (vec3-sub omega-b omega-a)]
                  
                  ;; Angular impulse
                  [I-inv-a (rigid-body-3d-world-inv-inertia new-a)]
                  [I-inv-b (if new-b (rigid-body-3d-world-inv-inertia new-b) (mat3-zero))]
                  [effective-I-inv (+ (mat3-trace I-inv-a) (mat3-trace I-inv-b))]
                  [angular-impulse (if (< effective-I-inv 0.0001)
                                       (vec3-zero)
                                       (vec3-scale rel-omega (/ -1 (max 0.1 effective-I-inv))))]
                  
                  [final-a (apply-angular-impulse-3d new-a (vec3-neg angular-impulse))]
                  [final-b (if new-b (apply-angular-impulse-3d new-b angular-impulse) #f)])
                 
                 (values final-a final-b)))))

;;; correct-fixed-position-3d : Correct position and orientation for fixed joint
(define (correct-fixed-position-3d body-a body-b pos-a pos-b ref-orient)
  ;; First correct point constraint
  (call-with-values
   (lambda () (correct-ball-socket-position-3d body-a body-b pos-a pos-b))
   (lambda (new-a new-b)
           ;; Then correct orientation
           (if (not new-b)
               (values new-a new-b)
               (let* ([orient-a (rigid-body-3d-orientation new-a)]
                      [orient-b (rigid-body-3d-orientation new-b)]
                      ;; Desired orientation of B: q_a * ref
                      [q-desired (quat-multiply orient-a ref-orient)]
                      ;; Error quaternion
                      [q-error (quat-multiply orient-b (quat-conjugate q-desired))]
                      ;; Extract error axis-angle (vector part of quaternion)
                      [error-vec (vec3 (* 2 (quat-x q-error))
                                       (* 2 (quat-y q-error))
                                       (* 2 (quat-z q-error)))]
                      [error-mag (vec3-magnitude error-vec)])
                     
                     (if (<= error-mag *angular-slop-3d*)
                         (values new-a new-b)
                         ;; Apply angular correction (simplified)
                         (let* ([correction (vec3-scale error-vec *baumgarte-factor-3d*)]
                                [delta-q-a (quat-from-axis-angle (vec3-normalize correction)
                                                                 (* 0.5 (vec3-magnitude correction)))]
                                [delta-q-b (quat-from-axis-angle (vec3-normalize correction)
                                                                 (* -0.5 (vec3-magnitude correction)))]
                                [new-orient-a (quat-normalize (quat-multiply delta-q-a orient-a))]
                                [new-orient-b (quat-normalize (quat-multiply delta-q-b orient-b))]
                                [final-a (rigid-body-3d-with-orientation new-a new-orient-a)]
                                [final-b (rigid-body-3d-with-orientation new-b new-orient-b)])
                               (values final-a final-b))))))))

;;; ============================================================
;;; Utility: Additional Mat3 Operations (not in rigid-body3d.ss)
;;; ============================================================

;;; mat3-trace : Mat3 → Number
;;; Sum of diagonal elements.
(define (mat3-trace m)
  (+ (mat3-ref m 0 0)
     (mat3-ref m 1 1)
     (mat3-ref m 2 2)))

;;; ============================================================
;;; Constraint Factory Functions (with solvers attached)
;;; ============================================================

;;; make-distance-joint-3d : Any × Any × Any × Vec3 × Vec3 × Number → Constraint3D
;;; Create a distance constraint between two entities.
(define (make-distance-joint-3d id entity-a entity-b local-anchor-a local-anchor-b distance)
  (make-constraint-3d 'distance-3d id entity-a entity-b
                      (make-distance-constraint-data-3d local-anchor-a local-anchor-b distance)
                      solve-distance-velocity-3d
                      correct-distance-position-3d
                      0))  ; cached lambda

;;; make-ball-socket-joint-3d : Any × Any × Any × Vec3 × Vec3 → Constraint3D
;;; Create a ball-socket joint between two entities.
(define (make-ball-socket-joint-3d id entity-a entity-b local-anchor-a local-anchor-b)
  (make-constraint-3d 'ball-socket-3d id entity-a entity-b
                      (make-ball-socket-constraint-data-3d local-anchor-a local-anchor-b)
                      solve-ball-socket-velocity-3d
                      correct-ball-socket-position-3d
                      (vec3-zero)))  ; cached lambda (3D)

;;; make-hinge-joint-3d : Any × Any × Any × Vec3 × Vec3 × Vec3 × Vec3 → Constraint3D
;;; Create a hinge joint between two entities.
(define (make-hinge-joint-3d id entity-a entity-b
                             local-anchor-a local-anchor-b
                             local-axis-a local-axis-b)
  (make-constraint-3d 'hinge-3d id entity-a entity-b
                      (make-hinge-constraint-data-3d local-anchor-a local-anchor-b
                                                     local-axis-a local-axis-b)
                      solve-hinge-velocity-3d
                      correct-hinge-position-3d
                      (list (vec3-zero) (vec3-zero))))  ; cached lambda

;;; make-fixed-joint-3d : Any × Any × Any × Vec3 × Vec3 × Quat → Constraint3D
;;; Create a fixed joint between two entities.
(define (make-fixed-joint-3d id entity-a entity-b
                             local-anchor-a local-anchor-b
                             reference-orientation)
  (make-constraint-3d 'fixed-3d id entity-a entity-b
                      (make-fixed-constraint-data-3d local-anchor-a local-anchor-b
                                                     reference-orientation)
                      solve-fixed-velocity-3d
                      correct-fixed-position-3d
                      (list (vec3-zero) (vec3-zero))))  ; cached lambda

;;; make-spring-joint-3d : Any × Any × Any × Vec3 × Vec3 × Number × Number × Number → Constraint3D
;;; Create a spring constraint between two entities.
(define (make-spring-joint-3d id entity-a entity-b
                              local-anchor-a local-anchor-b
                              rest-length stiffness damping)
  (make-constraint-3d 'spring-3d id entity-a entity-b
                      (make-spring-constraint-data-3d local-anchor-a local-anchor-b
                                                      rest-length stiffness damping)
                      solve-spring-velocity-3d
                      correct-spring-position-3d
                      0))

