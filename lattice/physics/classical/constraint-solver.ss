;;; user/physics/constraint-solver.ss — Constraint Solvers
;;;
;;; Sequential impulse solvers for 2D physics constraints:
;;;   - Distance constraint (1 DOF)
;;;   - Revolute joint (2 DOF)
;;;   - Spring constraint (soft 1 DOF)
;;;   - Weld joint (3 DOF)
;;;
;;; Each solver has:
;;;   - Velocity solver: Applies corrective impulses
;;;   - Position solver: Fixes positional drift
;;;
;;; Uses Baumgarte stabilization for position correction.
;;;
;;; This is Core code: pure constraint math with world mutation.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec2.ss
;;;   - user/physics/rigid-body.ss
;;;   - user/physics/constraints.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/physics/classical/rigid-body.ss")
(load "lattice/physics/classical/constraints.ss")

;;; ============================================================
;;; Solver Constants
;;; ============================================================

(define *baumgarte-factor* 0.2)      ; Position correction bias
(define *position-slop* 0.005)       ; Allow small penetration
(define *angular-slop* 0.0001)       ; Allow small angle error

;;; ============================================================
;;; Helper: Apply Impulse to Entity
;;; ============================================================

;;; apply-constraint-impulse! : World x Any x Vec2 x Vec2 -> Void
;;; Apply an impulse at a world point to an entity.
;;; Updates both linear and angular velocity.
(define (apply-constraint-impulse! world entity-id impulse world-point)
  (let ([entity (world-get-entity world entity-id)])
       (when (and entity (not (entity-static? entity)))
             (let* ([body (entity-body entity)]
                    [new-body (if (rigid-body? body)
                                  (apply-impulse-at-point body impulse world-point)
                                  ;; Body2D: linear only
                                  (let* ([inv-mass (body-inv-mass body)]
                                         [new-vel (vec2-add (body-vel body)
                                                            (vec2-scale impulse inv-mass))])
                                        (body-with-vel body new-vel)))])
                   (world-update-entity! world entity-id
                                         (lambda (e) (entity-with-body e new-body)))))))

;;; ============================================================
;;; Distance Constraint Solver
;;; ============================================================

;;; solve-distance-velocity : World x Constraint x Number -> Void
;;; Solve velocity constraint for distance joint.
(define (solve-distance-velocity world constraint dt)
  (let* ([data (constraint-data constraint)]
         [local-a (distance-data-anchor-a data)]
         [local-b (distance-data-anchor-b data)]
         [target-dist (distance-data-target data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when entity-a
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     
                     ;; World positions of anchors
                     [pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     
                     ;; Constraint axis (normalized direction)
                     [delta (vec2-sub pos-b pos-a)]
                     [current-dist (vec2-magnitude delta)]
                     [n (if (< current-dist 0.0001)
                            (vec2 1 0)
                            (vec2-normalize delta))]
                     
                     ;; Get velocities at anchor points
                     [vel-a (if (rigid-body? body-a)
                                (rigid-body-velocity-at body-a pos-a)
                                (body-vel body-a))]
                     [vel-b (if body-b
                                (if (rigid-body? body-b)
                                    (rigid-body-velocity-at body-b pos-b)
                                    (body-vel body-b))
                                (vec2 0 0))]
                     
                     ;; Relative velocity along constraint axis
                     [rel-vel (vec2-sub vel-b vel-a)]
                     [v-n (vec2-dot rel-vel n)]
                     
                     ;; Compute effective mass
                     ;; K = 1/m_a + 1/m_b + (r_a x n)^2/I_a + (r_b x n)^2/I_b
                     [r-a (vec2-sub pos-a (if (rigid-body? body-a)
                                              (rigid-body-pos body-a)
                                              (body-pos body-a)))]
                     [r-b (if body-b
                              (vec2-sub pos-b (if (rigid-body? body-b)
                                                  (rigid-body-pos body-b)
                                                  (body-pos body-b)))
                              (vec2 0 0))]
                     [inv-mass-a (if (rigid-body? body-a)
                                     (rigid-body-inv-mass body-a)
                                     (body-inv-mass body-a))]
                     [inv-mass-b (if body-b
                                     (if (rigid-body? body-b)
                                         (rigid-body-inv-mass body-b)
                                         (body-inv-mass body-b))
                                     0)]
                     [inv-i-a (if (rigid-body? body-a)
                                  (rigid-body-inv-inertia body-a)
                                  0)]
                     [inv-i-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-inv-inertia body-b)
                                  0)]
                     [rn-a (vec2-cross r-a n)]
                     [rn-b (vec2-cross r-b n)]
                     [effective-mass (+ inv-mass-a inv-mass-b
                                        (* rn-a rn-a inv-i-a)
                                        (* rn-b rn-b inv-i-b))]
                     
                     ;; Position error (for clamping, not Baumgarte here)
                     [C (- current-dist target-dist)]
                     
                     ;; Compute impulse magnitude (no bias - position solver handles drift)
                     [lambda (if (< effective-mass 0.0001)
                                 0
                                 (/ (- v-n) effective-mass))]
                     [impulse (vec2-scale n lambda)])
                    
                    ;; Apply impulses
                    (apply-constraint-impulse! world entity-a-id (vec2-neg impulse) pos-a)
                    (when entity-b-id
                          (apply-constraint-impulse! world entity-b-id impulse pos-b))))))

;;; correct-distance-position : World x Constraint -> Void
;;; Apply position correction for distance constraint.
(define (correct-distance-position world constraint)
  (let* ([data (constraint-data constraint)]
         [target-dist (distance-data-target data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when entity-a
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     
                     ;; World positions of anchors
                     [pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     
                     ;; Position error
                     [delta (vec2-sub pos-b pos-a)]
                     [current-dist (vec2-magnitude delta)]
                     [C (- current-dist target-dist)]
                     [n (if (< current-dist 0.0001)
                            (vec2 1 0)
                            (vec2-normalize delta))])
                    
                    ;; Skip if within tolerance
                    (when (> (abs C) *position-slop*)
                          (let* ([inv-mass-a (if (rigid-body? body-a)
                                                 (rigid-body-inv-mass body-a)
                                                 (body-inv-mass body-a))]
                                 [inv-mass-b (if body-b
                                                 (if (rigid-body? body-b)
                                                     (rigid-body-inv-mass body-b)
                                                     (body-inv-mass body-b))
                                                 0)]
                                 [total-inv-mass (+ inv-mass-a inv-mass-b)]
                                 [correction (* *baumgarte-factor* (- (abs C) *position-slop*)
                                                (if (> C 0) 1 -1))]
                                 [correction-vec (vec2-scale n correction)])
                                
                                ;; Apply position corrections
                                ;; n points from a to b
                                ;; When stretched (C > 0): a moves toward b (+n), b moves toward a (-n)
                                (unless (or (entity-static? entity-a) (< total-inv-mass 0.0001))
                                        (let* ([pos (if (rigid-body? body-a)
                                                        (rigid-body-pos body-a)
                                                        (body-pos body-a))]
                                               [new-pos (vec2-add pos
                                                                  (vec2-scale correction-vec
                                                                              (/ inv-mass-a total-inv-mass)))]
                                               [new-body (if (rigid-body? body-a)
                                                             (rigid-body-with-pos body-a new-pos)
                                                             (body-with-pos body-a new-pos))])
                                              (world-update-entity! world entity-a-id
                                                                    (lambda (e) (entity-with-body e new-body)))))
                                
                                (when (and entity-b (not (entity-static? entity-b)))
                                      (let* ([pos (if (rigid-body? body-b)
                                                      (rigid-body-pos body-b)
                                                      (body-pos body-b))]
                                             [new-pos (vec2-sub pos
                                                                (vec2-scale correction-vec
                                                                            (/ inv-mass-b total-inv-mass)))]
                                             [new-body (if (rigid-body? body-b)
                                                           (rigid-body-with-pos body-b new-pos)
                                                           (body-with-pos body-b new-pos))])
                                            (world-update-entity! world entity-b-id
                                                                  (lambda (e) (entity-with-body e new-body)))))))))))

;;; ============================================================
;;; Revolute Joint Solver (2 DOF Point Constraint)
;;; ============================================================

;;; solve-revolute-velocity : World x Constraint x Number -> Void
;;; Solve velocity constraint for revolute joint.
;;; Both anchors must move together (2D point-to-point).
(define (solve-revolute-velocity world constraint dt)
  (let* ([data (constraint-data constraint)]
         [local-a (revolute-data-anchor-a data)]
         [local-b (revolute-data-anchor-b data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when entity-a
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     
                     ;; World positions of anchors
                     [pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     
                     ;; Position error (should be zero)
                     [C (vec2-sub pos-b pos-a)]
                     
                     ;; Get velocities at anchor points
                     [vel-a (if (rigid-body? body-a)
                                (rigid-body-velocity-at body-a pos-a)
                                (body-vel body-a))]
                     [vel-b (if body-b
                                (if (rigid-body? body-b)
                                    (rigid-body-velocity-at body-b pos-b)
                                    (body-vel body-b))
                                (vec2 0 0))]
                     
                     ;; Relative velocity
                     [rel-vel (vec2-sub vel-b vel-a)]
                     
                     ;; Lever arms
                     [r-a (vec2-sub pos-a (if (rigid-body? body-a)
                                              (rigid-body-pos body-a)
                                              (body-pos body-a)))]
                     [r-b (if body-b
                              (vec2-sub pos-b (if (rigid-body? body-b)
                                                  (rigid-body-pos body-b)
                                                  (body-pos body-b)))
                              (vec2 0 0))]
                     
                     ;; Inverse masses and inertias
                     [inv-mass-a (if (rigid-body? body-a)
                                     (rigid-body-inv-mass body-a)
                                     (body-inv-mass body-a))]
                     [inv-mass-b (if body-b
                                     (if (rigid-body? body-b)
                                         (rigid-body-inv-mass body-b)
                                         (body-inv-mass body-b))
                                     0)]
                     [inv-i-a (if (rigid-body? body-a)
                                  (rigid-body-inv-inertia body-a)
                                  0)]
                     [inv-i-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-inv-inertia body-b)
                                  0)]
                     
                     ;; Effective mass matrix (2x2)
                     ;; K = [1/m_a + 1/m_b + ra.y^2/I_a + rb.y^2/I_b,    -ra.x*ra.y/I_a - rb.x*rb.y/I_b]
                     ;;     [-ra.x*ra.y/I_a - rb.x*rb.y/I_b,              1/m_a + 1/m_b + ra.x^2/I_a + rb.x^2/I_b]
                     [rax (vec2-x r-a)] [ray (vec2-y r-a)]
                     [rbx (vec2-x r-b)] [rby (vec2-y r-b)]
                     [k11 (+ inv-mass-a inv-mass-b
                             (* ray ray inv-i-a) (* rby rby inv-i-b))]
                     [k12 (+ (* (- rax) ray inv-i-a) (* (- rbx) rby inv-i-b))]
                     [k22 (+ inv-mass-a inv-mass-b
                             (* rax rax inv-i-a) (* rbx rbx inv-i-b))]
                     
                     ;; Invert 2x2 matrix: K^-1
                     [det (- (* k11 k22) (* k12 k12))]
                     [inv-det (if (< (abs det) 0.0001) 0 (/ 1 det))]
                     
                     ;; RHS = -v (no Baumgarte in velocity solver - position solver handles drift)
                     [rhs-x (- (vec2-x rel-vel))]
                     [rhs-y (- (vec2-y rel-vel))]
                     
                     ;; Solve: lambda = K^-1 * rhs
                     [lambda-x (* inv-det (- (* k22 rhs-x) (* k12 rhs-y)))]
                     [lambda-y (* inv-det (- (* k11 rhs-y) (* k12 rhs-x)))]
                     [impulse (vec2 lambda-x lambda-y)])
                    
                    ;; Apply impulses
                    (apply-constraint-impulse! world entity-a-id (vec2-neg impulse) pos-a)
                    (when entity-b-id
                          (apply-constraint-impulse! world entity-b-id impulse pos-b))))))

;;; correct-revolute-position : World x Constraint -> Void
;;; Apply position correction for revolute joint.
(define (correct-revolute-position world constraint)
  (let* ([entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when entity-a
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     
                     ;; World positions of anchors
                     [pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     
                     ;; Position error
                     [C (vec2-sub pos-b pos-a)]
                     [error-mag (vec2-magnitude C)])
                    
                    ;; Skip if within tolerance
                    (when (> error-mag *position-slop*)
                          (let* ([inv-mass-a (if (rigid-body? body-a)
                                                 (rigid-body-inv-mass body-a)
                                                 (body-inv-mass body-a))]
                                 [inv-mass-b (if body-b
                                                 (if (rigid-body? body-b)
                                                     (rigid-body-inv-mass body-b)
                                                     (body-inv-mass body-b))
                                                 0)]
                                 [total-inv-mass (+ inv-mass-a inv-mass-b)]
                                 [correction (vec2-scale C *baumgarte-factor*)])
                                
                                ;; Apply position corrections
                                ;; C points from anchor-a to anchor-b (the error)
                                ;; Entity A moves toward B (add correction), Entity B moves toward A (sub correction)
                                (unless (or (entity-static? entity-a) (< total-inv-mass 0.0001))
                                        (let* ([pos (if (rigid-body? body-a)
                                                        (rigid-body-pos body-a)
                                                        (body-pos body-a))]
                                               [new-pos (vec2-add pos
                                                                  (vec2-scale correction
                                                                              (/ inv-mass-a total-inv-mass)))]
                                               [new-body (if (rigid-body? body-a)
                                                             (rigid-body-with-pos body-a new-pos)
                                                             (body-with-pos body-a new-pos))])
                                              (world-update-entity! world entity-a-id
                                                                    (lambda (e) (entity-with-body e new-body)))))
                                
                                (when (and entity-b (not (entity-static? entity-b)))
                                      (let* ([pos (if (rigid-body? body-b)
                                                      (rigid-body-pos body-b)
                                                      (body-pos body-b))]
                                             [new-pos (vec2-sub pos
                                                                (vec2-scale correction
                                                                            (/ inv-mass-b total-inv-mass)))]
                                             [new-body (if (rigid-body? body-b)
                                                           (rigid-body-with-pos body-b new-pos)
                                                           (body-with-pos body-b new-pos))])
                                            (world-update-entity! world entity-b-id
                                                                  (lambda (e) (entity-with-body e new-body)))))))))))

;;; ============================================================
;;; Spring Constraint Solver (Soft 1 DOF)
;;; ============================================================

;;; solve-spring-velocity : World x Constraint x Number -> Void
;;; Solve velocity constraint for spring (soft constraint).
;;; No position correction needed - spring force handles it.
(define (solve-spring-velocity world constraint dt)
  (let* ([data (constraint-data constraint)]
         [rest-length (spring-data-rest-length data)]
         [stiffness (spring-data-stiffness data)]
         [damping (spring-data-damping data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when entity-a
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     
                     ;; World positions of anchors
                     [pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     
                     ;; Spring direction
                     [delta (vec2-sub pos-b pos-a)]
                     [current-dist (vec2-magnitude delta)]
                     [n (if (< current-dist 0.0001)
                            (vec2 1 0)
                            (vec2-normalize delta))]
                     
                     ;; Extension from rest length
                     [stretch (- current-dist rest-length)]
                     
                     ;; Get velocities at anchor points
                     [vel-a (if (rigid-body? body-a)
                                (rigid-body-velocity-at body-a pos-a)
                                (body-vel body-a))]
                     [vel-b (if body-b
                                (if (rigid-body? body-b)
                                    (rigid-body-velocity-at body-b pos-b)
                                    (body-vel body-b))
                                (vec2 0 0))]
                     
                     ;; Relative velocity along spring axis
                     [rel-vel (vec2-sub vel-b vel-a)]
                     [v-n (vec2-dot rel-vel n)]
                     
                     ;; Spring force: F = k*x + c*v (semi-implicit)
                     [force-magnitude (+ (* stiffness stretch) (* damping v-n))]
                     [impulse-mag (* force-magnitude dt)]
                     [impulse (vec2-scale n impulse-mag)])
                    
                    ;; Apply impulses
                    ;; When stretched (positive force), A should be pulled toward B (+ direction)
                    ;; and B should be pulled toward A (- direction)
                    (apply-constraint-impulse! world entity-a-id impulse pos-a)
                    (when entity-b-id
                          (apply-constraint-impulse! world entity-b-id (vec2-neg impulse) pos-b))))))

;;; Springs don't need position correction (soft constraint)
(define (correct-spring-position world constraint)
  (void))

;;; ============================================================
;;; Weld Joint Solver (3 DOF: 2 position + 1 angle)
;;; ============================================================

;;; solve-weld-velocity : World x Constraint x Number -> Void
;;; Solve velocity constraint for weld joint (no relative motion).
(define (solve-weld-velocity world constraint dt)
  ;; First solve the point constraint (same as revolute)
  (solve-revolute-velocity world constraint dt)
  
  ;; Then solve the angular constraint
  (let* ([data (constraint-data constraint)]
         [ref-angle (weld-data-reference-angle data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when (and entity-a (rigid-body? (entity-body entity-a)))
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     [omega-a (rigid-body-angular-vel body-a)]
                     [omega-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-angular-vel body-b)
                                  0)]
                     [angle-a (rigid-body-angle body-a)]
                     [angle-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-angle body-b)
                                  0)]
                     
                     ;; Angular error and velocity
                     [C-angle (- (- angle-b angle-a) ref-angle)]
                     [rel-omega (- omega-b omega-a)]
                     
                     ;; Effective rotational mass
                     [inv-i-a (rigid-body-inv-inertia body-a)]
                     [inv-i-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-inv-inertia body-b)
                                  0)]
                     [effective-i (+ inv-i-a inv-i-b)]
                     
                     ;; Compute angular impulse (no Baumgarte in velocity solver)
                     [lambda-angle (if (< effective-i 0.0001)
                                       0
                                       (/ (- rel-omega) effective-i))])
                    
                    ;; Apply angular impulses
                    (unless (entity-static? entity-a)
                            (let ([new-omega (- omega-a (* lambda-angle inv-i-a))]
                                  [new-body (rigid-body-with-angular-vel body-a
                                                                         (- omega-a (* lambda-angle inv-i-a)))])
                                 (world-update-entity! world entity-a-id
                                                       (lambda (e) (entity-with-body e new-body)))))
                    
                    (when (and entity-b (not (entity-static? entity-b))
                               (rigid-body? body-b))
                          (let ([new-body (rigid-body-with-angular-vel body-b
                                                                       (+ omega-b (* lambda-angle inv-i-b)))])
                               (world-update-entity! world entity-b-id
                                                     (lambda (e) (entity-with-body e new-body)))))))))

;;; correct-weld-position : World x Constraint -> Void
;;; Apply position and angle correction for weld joint.
(define (correct-weld-position world constraint)
  ;; First correct the point constraint
  (correct-revolute-position world constraint)
  
  ;; Then correct the angular constraint
  (let* ([data (constraint-data constraint)]
         [ref-angle (weld-data-reference-angle data)]
         [entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        
        (when (and entity-a (rigid-body? (entity-body entity-a)))
              (let* ([body-a (entity-body entity-a)]
                     [body-b (and entity-b (entity-body entity-b))]
                     [angle-a (rigid-body-angle body-a)]
                     [angle-b (if (and body-b (rigid-body? body-b))
                                  (rigid-body-angle body-b)
                                  0)]
                     
                     ;; Angular error
                     [C-angle (- (- angle-b angle-a) ref-angle)])
                    
                    ;; Skip if within tolerance
                    (when (> (abs C-angle) *angular-slop*)
                          (let* ([inv-i-a (rigid-body-inv-inertia body-a)]
                                 [inv-i-b (if (and body-b (rigid-body? body-b))
                                              (rigid-body-inv-inertia body-b)
                                              0)]
                                 [total-inv-i (+ inv-i-a inv-i-b)]
                                 [correction (* *baumgarte-factor* C-angle)])
                                
                                ;; Apply angle corrections
                                (unless (or (entity-static? entity-a) (< total-inv-i 0.0001))
                                        (let* ([new-angle (+ angle-a
                                                             (* correction (/ inv-i-a total-inv-i)))]
                                               [new-body (rigid-body-with-angle body-a new-angle)])
                                              (world-update-entity! world entity-a-id
                                                                    (lambda (e) (entity-with-body e new-body)))))
                                
                                (when (and entity-b (not (entity-static? entity-b))
                                           (rigid-body? body-b))
                                      (let* ([new-angle (- angle-b
                                                           (* correction (/ inv-i-b total-inv-i)))]
                                             [new-body (rigid-body-with-angle body-b new-angle)])
                                            (world-update-entity! world entity-b-id
                                                                  (lambda (e) (entity-with-body e new-body)))))))))))

;;; ============================================================
;;; Constraint Factory Functions
;;; ============================================================

;;; make-distance-joint : Any x Any x Any x Vec2 x Vec2 x Number -> Constraint
;;; Create a distance constraint between two entities.
(define (make-distance-joint id entity-a entity-b local-anchor-a local-anchor-b distance)
  (make-constraint 'distance id entity-a entity-b
                   (make-distance-constraint-data local-anchor-a local-anchor-b distance)
                   solve-distance-velocity
                   correct-distance-position
                   0))  ; cached lambda

;;; make-revolute-joint : Any x Any x Any x Vec2 x Vec2 -> Constraint
;;; Create a revolute joint between two entities.
(define (make-revolute-joint id entity-a entity-b local-anchor-a local-anchor-b)
  (make-constraint 'revolute id entity-a entity-b
                   (make-revolute-constraint-data local-anchor-a local-anchor-b)
                   solve-revolute-velocity
                   correct-revolute-position
                   (vec2 0 0)))  ; cached lambda (2D)

;;; make-spring-joint : Any x Any x Any x Vec2 x Vec2 x Number x Number x Number -> Constraint
;;; Create a spring constraint between two entities.
(define (make-spring-joint id entity-a entity-b local-anchor-a local-anchor-b
                           rest-length stiffness damping)
  (make-constraint 'spring id entity-a entity-b
                   (make-spring-constraint-data local-anchor-a local-anchor-b
                                                rest-length stiffness damping)
                   solve-spring-velocity
                   correct-spring-position
                   0))

;;; make-weld-joint : Any x Any x Any x Vec2 x Vec2 x Number -> Constraint
;;; Create a weld joint between two entities.
(define (make-weld-joint id entity-a entity-b local-anchor-a local-anchor-b ref-angle)
  (make-constraint 'weld id entity-a entity-b
                   (make-weld-constraint-data local-anchor-a local-anchor-b ref-angle)
                   solve-weld-velocity
                   correct-weld-position
                   (list (vec2 0 0) 0)))  ; cached lambda (2D + scalar)
