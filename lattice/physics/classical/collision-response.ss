(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-response
;;; @requires prelude vec2 integrators
(require 'prelude)
(require 'vec2)
(require 'integrators)

(doc 'module 'collision-response)
(doc 'description "Impulse-based collision resolution for 2D physics")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'section 'collision)

(doc "A collision manifold describes the contact between two bodies:")
(doc "  - body-a: first body")
(doc "  - body-b: second body")
(doc "  - normal: collision normal (from A to B)")
(doc "  - penetration: overlap depth (positive means overlapping)")
(doc "  - contact-point: point of contact")

(define (make-manifold body-a body-b normal penetration contact-point)
  (doc 'type '(make-manifold : Body2D × Body2D × Vec2 × Number × Vec2 → Manifold))
  (list 'manifold body-a body-b normal penetration contact-point))

(doc "manifold? : Any → Boolean")
(define (manifold? m)
  (and (pair? m) (eq? (car m) 'manifold)))

(define (manifold-body-a m) (list-ref m 1))
  (doc 'type '(manifold-body-a : Manifold → Body2D))

(define (manifold-body-b m) (list-ref m 2))
  (doc 'type '(manifold-body-b : Manifold → Body2D))

(define (manifold-normal m) (list-ref m 3))
  (doc 'type '(manifold-normal : Manifold → Vec2))

(define (manifold-penetration m) (list-ref m 4))
  (doc 'type '(manifold-penetration : Manifold → Number))

(define (manifold-contact m) (list-ref m 5))
  (doc 'type '(manifold-contact : Manifold → Vec2))

(doc 'section 'material)

(doc "Material properties for collision response")
(doc "  - restitution: bounciness (0 = perfectly inelastic, 1 = perfectly elastic)")
(doc "  - static-friction: friction when stationary")
(doc "  - dynamic-friction: friction when sliding")

(define (make-material restitution static-friction dynamic-friction)
  (doc 'type '(make-material : Number × Number × Number → Material))
  (list 'material restitution static-friction dynamic-friction))

(doc "material? : Any → Boolean")
(define (material? m)
  (and (pair? m) (eq? (car m) 'material)))

(define (material-restitution m) (list-ref m 1))
  (doc 'type '(material-restitution : Material → Number))

(define (material-static-friction m) (list-ref m 2))
  (doc 'type '(material-static-friction : Material → Number))

(define (material-dynamic-friction m) (list-ref m 3))
  (doc 'type '(material-dynamic-friction : Material → Number))

(doc "Default materials")
(define rubber-material (make-material 0.8 0.9 0.7))
(define metal-material (make-material 0.3 0.4 0.3))
(define wood-material (make-material 0.4 0.5 0.4))
(define ice-material (make-material 0.1 0.02 0.01))
(define default-material (make-material 0.5 0.5 0.3))

(doc 'combine-materials 'type 'Material × Material → (Restitution × StaticFriction × DynamicFriction))
(doc "Combine two materials for collision using geometric mean.")
(define (combine-materials mat-a mat-b)
  (let ([e-a (material-restitution mat-a)]
        [e-b (material-restitution mat-b)]
        [sf-a (material-static-friction mat-a)]
        [sf-b (material-static-friction mat-b)]
        [df-a (material-dynamic-friction mat-a)]
        [df-b (material-dynamic-friction mat-b)])
       (list (min e-a e-b)  ; Use minimum for restitution
             (sqrt (* sf-a sf-b))  ; Geometric mean for static friction
             (sqrt (* df-a df-b))))) ; Geometric mean for dynamic friction

(doc 'section 'relative)

(doc 'velocity-at-point 'type 'Body × Vec2 → Vec2)
(doc "Get velocity at a world point, including angular contribution for rigid bodies.")
(define (velocity-at-point body point)
  (if (rigid-body? body)
      (rigid-body-velocity-at body point)
      (body-vel body)))

(doc 'relative-velocity-at 'type 'Body × Body × Vec2 → Vec2)
(doc "Calculate relative velocity at contact point (B relative to A).")
(define (relative-velocity-at body-a body-b contact)
  (vec2-sub (velocity-at-point body-b contact)
            (velocity-at-point body-a contact)))

(doc "Legacy: simple relative velocity for backwards compatibility")
(define (relative-velocity body-a body-b)
  (vec2-sub (body-vel body-b) (body-vel body-a)))

(doc 'normal-velocity-at 'type 'Body × Body × Vec2 × Vec2 → Number)
(doc "Velocity along the collision normal at contact point (positive = approaching).")
(define (normal-velocity-at body-a body-b contact normal)
  (vec2-dot (relative-velocity-at body-a body-b contact) normal))

(doc 'normal-velocity 'type 'Body2D × Body2D × Vec2 → Number)
(doc "Legacy: Velocity along normal (no angular).")
(define (normal-velocity body-a body-b normal)
  (vec2-dot (relative-velocity body-a body-b) normal))

(doc 'tangent-velocity 'type 'Body2D × Body2D × Vec2 → Vec2)
(doc "Velocity perpendicular to the collision normal.")
(define (tangent-velocity body-a body-b normal)
  (let* ([rel-vel (relative-velocity body-a body-b)]
         [normal-component (vec2-scale normal (vec2-dot rel-vel normal))])
        (vec2-sub rel-vel normal-component)))

(doc 'section 'impulse)

(doc 'get-inv-mass 'type 'Body → Number)
(doc "Get inverse mass from either Body2D or RigidBody2D.")
(define (get-inv-mass body)
  (if (rigid-body? body)
      (rigid-body-inv-mass body)
      (body-inv-mass body)))

(doc 'get-inv-inertia 'type 'Body → Number)
(doc "Get inverse inertia (0 for non-rigid bodies).")
(define (get-inv-inertia body)
  (if (rigid-body? body)
      (rigid-body-inv-inertia body)
      0))

(doc 'get-body-pos 'type 'Body → Vec2)
(doc "Get position from either Body2D or RigidBody2D.")
(define (get-body-pos body)
  (if (rigid-body? body)
      (rigid-body-pos body)
      (body-pos body)))

(doc 'calculate-impulse-with-rotation 'type 'Body × Body × Vec2 × Vec2 × Number → Number)
(doc "Calculate normal impulse magnitude including rotational effects.")
(doc "contact: world space contact point")
(define (calculate-impulse-with-rotation body-a body-b contact normal restitution)
  (let* ([vel-along-normal (normal-velocity-at body-a body-b contact normal)]
         [inv-mass-a (get-inv-mass body-a)]
         [inv-mass-b (get-inv-mass body-b)]
         [inv-i-a (get-inv-inertia body-a)]
         [inv-i-b (get-inv-inertia body-b)]
         ;; Lever arms from body centers to contact
         [r-a (vec2-sub contact (get-body-pos body-a))]
         [r-b (vec2-sub contact (get-body-pos body-b))]
         ;; r × n (2D cross product gives scalar)
         [rn-a (vec2-cross r-a normal)]
         [rn-b (vec2-cross r-b normal)]
         ;; Effective mass: 1/m_a + 1/m_b + (r_a × n)²/I_a + (r_b × n)²/I_b
         [effective-mass (+ inv-mass-a inv-mass-b
                            (* rn-a rn-a inv-i-a)
                            (* rn-b rn-b inv-i-b))])
        ;; Don't resolve if separating
        (if (> vel-along-normal 0)
            0
            ;; Impulse magnitude
            (if (< effective-mass 0.0001)
                0
                (/ (* (- (+ 1 restitution)) vel-along-normal)
                   effective-mass)))))

(doc "Legacy: calculate-impulse without rotation")
(define (calculate-impulse body-a body-b normal restitution)
  (let* ([vel-along-normal (normal-velocity body-a body-b normal)]
         [inv-mass-a (body-inv-mass body-a)]
         [inv-mass-b (body-inv-mass body-b)]
         [total-inv-mass (+ inv-mass-a inv-mass-b)])
        ;; Don't resolve if separating
        (if (> vel-along-normal 0)
            0
            ;; Impulse magnitude
            (/ (* (- (+ 1 restitution)) vel-along-normal)
               total-inv-mass))))

(doc 'calculate-friction-impulse 'type 'Body2D × Body2D × Vec2 × Number × Number × Number → Number)
(doc "Calculate friction impulse magnitude.")
(doc "j-normal: the normal impulse magnitude")
(doc "static-friction, dynamic-friction: friction coefficients")
(define (calculate-friction-impulse body-a body-b normal j-normal static-friction dynamic-friction)
  (let* ([rel-vel (relative-velocity body-a body-b)]
         ;; Get tangent direction
         [normal-component (vec2-scale normal (vec2-dot rel-vel normal))]
         [tangent-vel (vec2-sub rel-vel normal-component)]
         [tangent-speed (vec2-magnitude tangent-vel)]
         [inv-mass-a (body-inv-mass body-a)]
         [inv-mass-b (body-inv-mass body-b)]
         [total-inv-mass (+ inv-mass-a inv-mass-b)])
        (if (< tangent-speed 0.0001)
            0
            ;; Coulomb friction
            (let ([j-tangent (/ (- tangent-speed) total-inv-mass)])
                 (if (< (abs j-tangent) (* j-normal static-friction))
                     ;; Static friction
                     j-tangent
                     ;; Dynamic friction
                     (* (- j-normal) dynamic-friction (if (< j-tangent 0) -1 1)))))))

(doc 'section 'impulse)

(doc "body-is-static? : Body → Boolean")
(doc "Check if body is static (works for both Body2D and RigidBody2D).")
(define (body-is-static? body)
  (if (rigid-body? body)
      (< (rigid-body-inv-mass body) 0.0001)
      (body-static? body)))

(doc 'apply-impulse-at 'type 'Body × Vec2 × Vec2 → Body)
(doc "Apply an impulse at a contact point, updating both linear and angular velocity.")
(define (apply-impulse-at body impulse contact)
  (if (body-is-static? body)
      body
      (if (rigid-body? body)
          ;; RigidBody2D: apply linear + angular impulse
          (let* ([inv-mass (rigid-body-inv-mass body)]
                 [inv-i (rigid-body-inv-inertia body)]
                 [r (vec2-sub contact (rigid-body-pos body))]
                 [torque (vec2-cross r impulse)]
                 [new-vel (vec2-add (rigid-body-vel body)
                                    (vec2-scale impulse inv-mass))]
                 [new-omega (+ (rigid-body-angular-vel body)
                               (* torque inv-i))])
                (rigid-body-with-angular-vel
                 (rigid-body-with-vel body new-vel)
                 new-omega))
          ;; Body2D: just linear impulse
          (let ([new-vel (vec2-add (body-vel body)
                                   (vec2-scale impulse (body-inv-mass body)))])
               (body-with-vel body new-vel)))))

(doc "Legacy: apply-impulse without rotation")
(define (apply-impulse body impulse)
  (if (body-static? body)
      body
      (let ([new-vel (vec2-add (body-vel body)
                               (vec2-scale impulse (body-inv-mass body)))])
           (body-with-vel body new-vel))))

(doc 'resolve-collision 'type 'Manifold × Material × Material → (Body2D × Body2D))
(doc "Resolve collision between two bodies using impulse method.")
(doc "Returns updated bodies.")
(define (resolve-collision manifold mat-a mat-b)
  (let* ([body-a (manifold-body-a manifold)]
         [body-b (manifold-body-b manifold)]
         [normal (manifold-normal manifold)]
         ;; Combine materials
         [combined (combine-materials mat-a mat-b)]
         [restitution (car combined)]
         [static-friction (cadr combined)]
         [dynamic-friction (caddr combined)]
         ;; Calculate normal impulse
         [j-normal (calculate-impulse body-a body-b normal restitution)])
        (if (= j-normal 0)
            ;; No collision (separating)
            (list body-a body-b)
            ;; Apply normal impulse
            (let* ([normal-impulse (vec2-scale normal j-normal)]
                   [body-a-1 (apply-impulse body-a (vec2-neg normal-impulse))]
                   [body-b-1 (apply-impulse body-b normal-impulse)]
                   ;; Calculate friction impulse
                   [j-friction (calculate-friction-impulse body-a-1 body-b-1
                                                           normal j-normal
                                                           static-friction dynamic-friction)]
                   ;; Get tangent direction
                   [rel-vel (relative-velocity body-a-1 body-b-1)]
                   [normal-comp (vec2-scale normal (vec2-dot rel-vel normal))]
                   [tangent-vel (vec2-sub rel-vel normal-comp)]
                   [tangent (if (< (vec2-magnitude tangent-vel) 0.0001)
                                (vec2 0 0)
                                (vec2-normalize tangent-vel))]
                   [friction-impulse (vec2-scale tangent j-friction)]
                   ;; Apply friction impulse
                   [body-a-2 (apply-impulse body-a-1 friction-impulse)]
                   [body-b-2 (apply-impulse body-b-1 (vec2-neg friction-impulse))])
                  (list body-a-2 body-b-2)))))

(doc 'resolve-collision-with-rotation 'type 'Manifold × Material × Material → (Body × Body))
(doc "Resolve collision with full angular impulse support for rigid bodies.")
(define (resolve-collision-with-rotation manifold mat-a mat-b)
  (let* ([body-a (manifold-body-a manifold)]
         [body-b (manifold-body-b manifold)]
         [normal (manifold-normal manifold)]
         [contact (manifold-contact manifold)]
         ;; Combine materials
         [combined (combine-materials mat-a mat-b)]
         [restitution (car combined)]
         [static-friction (cadr combined)]
         [dynamic-friction (caddr combined)]
         ;; Calculate normal impulse with rotation
         [j-normal (calculate-impulse-with-rotation body-a body-b contact normal restitution)])
        (if (= j-normal 0)
            ;; No collision (separating)
            (list body-a body-b)
            ;; Apply normal impulse at contact point
            (let* ([normal-impulse (vec2-scale normal j-normal)]
                   [body-a-1 (apply-impulse-at body-a (vec2-neg normal-impulse) contact)]
                   [body-b-1 (apply-impulse-at body-b normal-impulse contact)]
                   ;; Calculate friction impulse (using updated velocities)
                   [rel-vel (relative-velocity-at body-a-1 body-b-1 contact)]
                   [normal-comp (vec2-scale normal (vec2-dot rel-vel normal))]
                   [tangent-vel (vec2-sub rel-vel normal-comp)]
                   [tangent-speed (vec2-magnitude tangent-vel)])
                  (if (< tangent-speed 0.0001)
                      ;; No friction needed
                      (list body-a-1 body-b-1)
                      ;; Apply friction
                      (let* ([tangent (vec2-normalize tangent-vel)]
                             ;; Compute friction impulse magnitude with rotation
                             [inv-mass-a (get-inv-mass body-a-1)]
                             [inv-mass-b (get-inv-mass body-b-1)]
                             [inv-i-a (get-inv-inertia body-a-1)]
                             [inv-i-b (get-inv-inertia body-b-1)]
                             [r-a (vec2-sub contact (get-body-pos body-a-1))]
                             [r-b (vec2-sub contact (get-body-pos body-b-1))]
                             [rt-a (vec2-cross r-a tangent)]
                             [rt-b (vec2-cross r-b tangent)]
                             [effective-mass-t (+ inv-mass-a inv-mass-b
                                                  (* rt-a rt-a inv-i-a)
                                                  (* rt-b rt-b inv-i-b))]
                             [j-tangent (if (< effective-mass-t 0.0001)
                                            0
                                            (/ (- tangent-speed) effective-mass-t))]
                             ;; Coulomb friction clamping
                             [j-friction (if (< (abs j-tangent) (* j-normal static-friction))
                                             j-tangent
                                             (* (- j-normal) dynamic-friction
                                                (if (< j-tangent 0) -1 1)))]
                             [friction-impulse (vec2-scale tangent j-friction)]
                             ;; Apply friction impulse at contact
                             [body-a-2 (apply-impulse-at body-a-1 friction-impulse contact)]
                             [body-b-2 (apply-impulse-at body-b-1 (vec2-neg friction-impulse) contact)])
                            (list body-a-2 body-b-2)))))))

(doc 'section 'position)

(doc "Position correction parameters")
(define *position-slop* 0.01)      ; Allow slight penetration
(define *position-percent* 0.8)    ; Correction percentage

(doc 'body-with-position 'type 'Body × Vec2 → Body)
(doc "Set position for either Body2D or RigidBody2D.")
(define (body-with-position body new-pos)
  (if (rigid-body? body)
      (rigid-body-with-pos body new-pos)
      (body-with-pos body new-pos)))

(doc 'correct-positions 'type 'Manifold → (Body × Body))
(doc "Apply position correction to prevent sinking. Works with both Body2D and RigidBody2D.")
(define (correct-positions manifold)
  (let* ([body-a (manifold-body-a manifold)]
         [body-b (manifold-body-b manifold)]
         [normal (manifold-normal manifold)]
         [penetration (manifold-penetration manifold)]
         [inv-mass-a (get-inv-mass body-a)]
         [inv-mass-b (get-inv-mass body-b)]
         [total-inv-mass (+ inv-mass-a inv-mass-b)])
        (if (or (< penetration *position-slop*) (< total-inv-mass 0.0001))
            ;; No correction needed or both static
            (list body-a body-b)
            ;; Calculate correction
            (let* ([correction-mag (* *position-percent*
                                      (/ (max 0 (- penetration *position-slop*))
                                         total-inv-mass))]
                   [correction (vec2-scale normal correction-mag)]
                   [pos-a (get-body-pos body-a)]
                   [pos-b (get-body-pos body-b)]
                   [new-pos-a (vec2-sub pos-a (vec2-scale correction inv-mass-a))]
                   [new-pos-b (vec2-add pos-b (vec2-scale correction inv-mass-b))])
                  (list (body-with-position body-a new-pos-a)
                        (body-with-position body-b new-pos-b))))))

(doc 'section 'full)

(doc 'resolve-with-correction 'type 'Manifold × Material × Material → (Body2D × Body2D))
(doc "Full collision resolution with impulse and position correction.")
(define (resolve-with-correction manifold mat-a mat-b)
  (let* ([result (resolve-collision manifold mat-a mat-b)]
         [body-a (car result)]
         [body-b (cadr result)]
         ;; Update manifold with new bodies
         [corrected-manifold (make-manifold body-a body-b
                                            (manifold-normal manifold)
                                            (manifold-penetration manifold)
                                            (manifold-contact manifold))]
         [corrected (correct-positions corrected-manifold)])
        corrected))

(doc 'resolve-with-rotation-and-correction 'type 'Manifold × Material × Material → (Body × Body))
(doc "Full collision resolution with angular impulse and position correction.")
(doc "Supports both Body2D and RigidBody2D.")
(define (resolve-with-rotation-and-correction manifold mat-a mat-b)
  (let* ([result (resolve-collision-with-rotation manifold mat-a mat-b)]
         [body-a (car result)]
         [body-b (cadr result)]
         ;; Update manifold with new bodies
         [corrected-manifold (make-manifold body-a body-b
                                            (manifold-normal manifold)
                                            (manifold-penetration manifold)
                                            (manifold-contact manifold))]
         [corrected (correct-positions corrected-manifold)])
        corrected))

(doc 'section 'simple)

(doc 'body-circle-circle-manifold 'type 'Body2D × Number × Body2D × Number → Manifold or #f)
(doc "Generate collision manifold for two circles using bodies.")
(doc "Returns #f if no collision.")
(define (body-circle-circle-manifold body-a radius-a body-b radius-b)
  (let* ([pos-a (body-pos body-a)]
         [pos-b (body-pos body-b)]
         [diff (vec2-sub pos-b pos-a)]
         [dist-sq (vec2-magnitude-sq diff)]
         [radius-sum (+ radius-a radius-b)]
         [radius-sum-sq (* radius-sum radius-sum)])
        (if (> dist-sq radius-sum-sq)
            #f  ; No collision
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               (vec2 1 0)  ; Arbitrary normal for coincident circles
                               (vec2-scale diff (/ 1 dist)))]
                   [penetration (- radius-sum dist)]
                   [contact (vec2-add pos-a (vec2-scale normal radius-a))])
                  (make-manifold body-a body-b normal penetration contact)))))

(doc 'body-circle-aabb-manifold 'type 'Body2D × Number × Vec2 × Vec2 → Manifold or #f)
(doc "Generate manifold for circle vs axis-aligned bounding box using bodies.")
(doc "aabb-min and aabb-max define the box corners.")
(define (body-circle-aabb-manifold body radius aabb-min aabb-max)
  (let* ([pos (body-pos body)]
         [px (vec2-x pos)]
         [py (vec2-y pos)]
         ;; Clamp circle center to box
         [closest-x (max (vec2-x aabb-min) (min px (vec2-x aabb-max)))]
         [closest-y (max (vec2-y aabb-min) (min py (vec2-y aabb-max)))]
         [closest (vec2 closest-x closest-y)]
         [diff (vec2-sub pos closest)]
         [dist-sq (vec2-magnitude-sq diff)])
        (if (> dist-sq (* radius radius))
            #f  ; No collision
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               ;; Circle center inside box
                               (let ([dx (min (- px (vec2-x aabb-min))
                                              (- (vec2-x aabb-max) px))]
                                     [dy (min (- py (vec2-y aabb-min))
                                              (- (vec2-y aabb-max) py))])
                                    (if (< dx dy)
                                        (vec2 (if (< px (/ (+ (vec2-x aabb-min) (vec2-x aabb-max)) 2)) -1 1) 0)
                                        (vec2 0 (if (< py (/ (+ (vec2-y aabb-min) (vec2-y aabb-max)) 2)) -1 1))))
                               (vec2-scale diff (/ 1 dist)))]
                   [penetration (- radius dist)])
                  ;; Create a "static" body for the AABB
                  (let ([aabb-body (make-static-body closest)])
                       (make-manifold body aabb-body normal penetration closest))))))

(doc 'section 'batch)

(doc 'resolve-all-collisions 'type '(List Manifold) × (Manifold → Material × Material) → (List Body2D))
(doc "Resolve multiple collisions, returning updated unique bodies.")
(doc "mat-fn extracts materials for a collision.")
(define (resolve-all-collisions manifolds mat-fn)
  (if (null? manifolds)
      '()
      ;; Collect all unique bodies
      (let* ([all-bodies
              (fold-left
               (lambda (bodies m)
                       (let ([a (manifold-body-a m)]
                             [b (manifold-body-b m)])
                            (cons a (cons b bodies))))
               '()
               manifolds)]
             ;; Build body table
             [body-table (make-hashtable equal-hash equal?)])
            ;; Initialize table with current bodies
            (for-each
             (lambda (b)
                     (let ([key (body-pos b)])  ; Use position as key (simplified)
                          (unless (hashtable-ref body-table key #f)
                                  (hashtable-set! body-table key b))))
             all-bodies)
            ;; Resolve each collision
            (for-each
             (lambda (m)
                     (let* ([mats (mat-fn m)]
                            [mat-a (car mats)]
                            [mat-b (cadr mats)]
                            [result (resolve-with-correction m mat-a mat-b)]
                            [new-a (car result)]
                            [new-b (cadr result)])
                           ;; Update table
                           (hashtable-set! body-table (body-pos (manifold-body-a m)) new-a)
                           (hashtable-set! body-table (body-pos (manifold-body-b m)) new-b)))
             manifolds)
            ;; Extract updated bodies
            (let-values ([(keys vals) (hashtable-entries body-table)])
                        (vector->list vals)))))

(doc 'section 'iterative)

(doc 'solve-iterations 'type '(List Manifold) × (Manifold → Material × Material) × Nat → (List Body2D))
(doc "Iteratively solve collisions for stability (sequential impulses).")
(define (solve-iterations manifolds mat-fn iterations)
  (if (or (null? manifolds) (= iterations 0))
      (if (null? manifolds)
          '()
          ;; Return bodies from manifolds
          (fold-left
           (lambda (bodies m)
                   (cons (manifold-body-a m) (cons (manifold-body-b m) bodies)))
           '()
           manifolds))
      ;; One iteration
      (let loop ([ms manifolds] [i 0])
           (if (>= i iterations)
               ;; Extract final bodies
               (fold-left
                (lambda (bodies m)
                        (cons (manifold-body-a m) (cons (manifold-body-b m) bodies)))
                '()
                ms)
               ;; Resolve all collisions once
               (loop
                (map (lambda (m)
                             (let* ([mats (mat-fn m)]
                                    [result (resolve-with-correction m (car mats) (cadr mats))])
                                   (make-manifold (car result) (cadr result)
                                                  (manifold-normal m)
                                                  (manifold-penetration m)
                                                  (manifold-contact m))))
                     ms)
                (+ i 1))))))
