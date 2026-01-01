;;; fabric/stitches/physics-2d/collision-response.ss — 2D Collision Response
;;;
;;; Implements impulse-based collision resolution for 2D physics.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Impulse-based collision resolution
;;;   - Restitution (bounciness) coefficient
;;;   - Friction (tangent impulse)
;;;   - Position correction for penetration
;;;   - Support for static and dynamic bodies
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec2.ss
;;;   - physics-2d/integrators.ss (for Body2D)

(load "core/prelude.ss")
(load "core/vec2.ss")
(load "core/physics-2d/integrators.ss")

;;; ============================================================
;;; Collision Manifold
;;; ============================================================

;;; A collision manifold describes the contact between two bodies:
;;;   - body-a: first body
;;;   - body-b: second body
;;;   - normal: collision normal (from A to B)
;;;   - penetration: overlap depth (positive means overlapping)
;;;   - contact-point: point of contact

;;; make-manifold : Body2D × Body2D × Vec2 × Number × Vec2 → Manifold
(define (make-manifold body-a body-b normal penetration contact-point)
  (list 'manifold body-a body-b normal penetration contact-point))

;;; manifold? : Any → Boolean
(define (manifold? m)
  (and (pair? m) (eq? (car m) 'manifold)))

;;; manifold-body-a : Manifold → Body2D
(define (manifold-body-a m) (list-ref m 1))

;;; manifold-body-b : Manifold → Body2D
(define (manifold-body-b m) (list-ref m 2))

;;; manifold-normal : Manifold → Vec2
(define (manifold-normal m) (list-ref m 3))

;;; manifold-penetration : Manifold → Number
(define (manifold-penetration m) (list-ref m 4))

;;; manifold-contact : Manifold → Vec2
(define (manifold-contact m) (list-ref m 5))

;;; ============================================================
;;; Material Properties
;;; ============================================================

;;; Material properties for collision response
;;;   - restitution: bounciness (0 = perfectly inelastic, 1 = perfectly elastic)
;;;   - static-friction: friction when stationary
;;;   - dynamic-friction: friction when sliding

;;; make-material : Number × Number × Number → Material
(define (make-material restitution static-friction dynamic-friction)
  (list 'material restitution static-friction dynamic-friction))

;;; material? : Any → Boolean
(define (material? m)
  (and (pair? m) (eq? (car m) 'material)))

;;; material-restitution : Material → Number
(define (material-restitution m) (list-ref m 1))

;;; material-static-friction : Material → Number
(define (material-static-friction m) (list-ref m 2))

;;; material-dynamic-friction : Material → Number
(define (material-dynamic-friction m) (list-ref m 3))

;;; Default materials
(define rubber-material (make-material 0.8 0.9 0.7))
(define metal-material (make-material 0.3 0.4 0.3))
(define wood-material (make-material 0.4 0.5 0.4))
(define ice-material (make-material 0.1 0.02 0.01))
(define default-material (make-material 0.5 0.5 0.3))

;;; combine-materials : Material × Material → (Restitution × StaticFriction × DynamicFriction)
;;; Combine two materials for collision using geometric mean.
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

;;; ============================================================
;;; Relative Velocity Calculation
;;; ============================================================

;;; relative-velocity : Body2D × Body2D → Vec2
;;; Calculate relative velocity of body B with respect to body A.
(define (relative-velocity body-a body-b)
  (vec2-sub (body-vel body-b) (body-vel body-a)))

;;; normal-velocity : Body2D × Body2D × Vec2 → Number
;;; Velocity along the collision normal (positive = approaching).
(define (normal-velocity body-a body-b normal)
  (vec2-dot (relative-velocity body-a body-b) normal))

;;; tangent-velocity : Body2D × Body2D × Vec2 → Vec2
;;; Velocity perpendicular to the collision normal.
(define (tangent-velocity body-a body-b normal)
  (let* ([rel-vel (relative-velocity body-a body-b)]
         [normal-component (vec2-scale normal (vec2-dot rel-vel normal))])
        (vec2-sub rel-vel normal-component)))

;;; ============================================================
;;; Impulse Calculation
;;; ============================================================

;;; calculate-impulse : Body2D × Body2D × Vec2 × Number → Number
;;; Calculate the normal impulse magnitude for collision response.
;;; restitution: coefficient of restitution (0-1)
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

;;; calculate-friction-impulse : Body2D × Body2D × Vec2 × Number × Number × Number → Number
;;; Calculate friction impulse magnitude.
;;; j-normal: the normal impulse magnitude
;;; static-friction, dynamic-friction: friction coefficients
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

;;; ============================================================
;;; Impulse Application
;;; ============================================================

;;; apply-impulse : Body2D × Vec2 → Body2D
;;; Apply an impulse to a body, changing its velocity.
(define (apply-impulse body impulse)
  (if (body-static? body)
      body
      (let ([new-vel (vec2-add (body-vel body)
                               (vec2-scale impulse (body-inv-mass body)))])
           (body-with-vel body new-vel))))

;;; resolve-collision : Manifold × Material × Material → (Body2D × Body2D)
;;; Resolve collision between two bodies using impulse method.
;;; Returns updated bodies.
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

;;; ============================================================
;;; Position Correction
;;; ============================================================

;;; Position correction parameters
(define *position-slop* 0.01)      ; Allow slight penetration
(define *position-percent* 0.8)    ; Correction percentage

;;; correct-positions : Manifold → (Body2D × Body2D)
;;; Apply position correction to prevent sinking.
(define (correct-positions manifold)
  (let* ([body-a (manifold-body-a manifold)]
         [body-b (manifold-body-b manifold)]
         [normal (manifold-normal manifold)]
         [penetration (manifold-penetration manifold)]
         [inv-mass-a (body-inv-mass body-a)]
         [inv-mass-b (body-inv-mass body-b)]
         [total-inv-mass (+ inv-mass-a inv-mass-b)])
        (if (or (< penetration *position-slop*) (= total-inv-mass 0))
            ;; No correction needed or both static
            (list body-a body-b)
            ;; Calculate correction
            (let* ([correction-mag (* *position-percent*
                                      (/ (max 0 (- penetration *position-slop*))
                                         total-inv-mass))]
                   [correction (vec2-scale normal correction-mag)]
                   [new-pos-a (vec2-sub (body-pos body-a)
                                        (vec2-scale correction inv-mass-a))]
                   [new-pos-b (vec2-add (body-pos body-b)
                                        (vec2-scale correction inv-mass-b))])
                  (list (body-with-pos body-a new-pos-a)
                        (body-with-pos body-b new-pos-b))))))

;;; ============================================================
;;; Full Collision Resolution
;;; ============================================================

;;; resolve-with-correction : Manifold × Material × Material → (Body2D × Body2D)
;;; Full collision resolution with impulse and position correction.
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

;;; ============================================================
;;; Simple Collision Detection Helpers
;;; ============================================================

;;; body-circle-circle-manifold : Body2D × Number × Body2D × Number → Manifold | #f
;;; Generate collision manifold for two circles using bodies.
;;; Returns #f if no collision.
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

;;; body-circle-aabb-manifold : Body2D × Number × Vec2 × Vec2 → Manifold | #f
;;; Generate manifold for circle vs axis-aligned bounding box using bodies.
;;; aabb-min and aabb-max define the box corners.
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

;;; ============================================================
;;; Batch Collision Resolution
;;; ============================================================

;;; resolve-all-collisions : (List Manifold) × (Manifold → Material × Material) → (List Body2D)
;;; Resolve multiple collisions, returning updated unique bodies.
;;; mat-fn extracts materials for a collision.
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

;;; ============================================================
;;; Iterative Solver
;;; ============================================================

;;; solve-iterations : (List Manifold) × (Manifold → Material × Material) × Nat → (List Body2D)
;;; Iteratively solve collisions for stability (sequential impulses).
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
