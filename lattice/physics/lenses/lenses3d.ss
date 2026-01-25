;;; lattice/physics/lenses/lenses3d.ss — Optics for 3D Physics
;;; @module lenses3d
;;; @requires prelude optics geometry-optics rigid-body3d quaternion

(load "core/base/prelude.ss")
(load "lattice/fp/optics/optics.ss")
(load "lattice/geometry/geometry-optics.ss")  ; vec3 lenses
(load "lattice/linalg/quaternion.ss")
(load "lattice/physics/classical3d/rigid-body3d.ss")

(doc 'module 'lenses3d)
(doc 'description "Optics for 3D physics state. Lenses for rigid-body-3d fields, quaternion components, and composed paths for deep access.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Usage: (load \"lattice/physics/lenses/lenses3d.ss\")")
(doc 'note "View: (^. body rigid-body-3d-pos-lens)")
(doc 'note "Modify: (& body (%~ (>>> rigid-body-3d-pos-lens vec3-z-lens) add1))")
(doc 'note "Compose: (>>> rigid-body-3d-vel-lens vec3-x-lens) for velocity's x component")

;;; ============================================================
;;; Section 1: Quaternion Lenses
;;; ============================================================

(doc 'section 'quaternion-lenses)

(doc quat-w-lens 'type '(Lens Quaternion Number))
(doc quat-w-lens 'description "Focus on the w (scalar) component of a quaternion")
(doc quat-w-lens 'export #t)
(define quat-w-lens
  (make-lens
   quat-w
   (lambda (new-w q) (quat new-w (quat-x q) (quat-y q) (quat-z q)))))

(doc quat-x-lens 'type '(Lens Quaternion Number))
(doc quat-x-lens 'description "Focus on the x component of a quaternion")
(doc quat-x-lens 'export #t)
(define quat-x-lens
  (make-lens
   quat-x
   (lambda (new-x q) (quat (quat-w q) new-x (quat-y q) (quat-z q)))))

(doc quat-y-lens 'type '(Lens Quaternion Number))
(doc quat-y-lens 'description "Focus on the y component of a quaternion")
(doc quat-y-lens 'export #t)
(define quat-y-lens
  (make-lens
   quat-y
   (lambda (new-y q) (quat (quat-w q) (quat-x q) new-y (quat-z q)))))

(doc quat-z-lens 'type '(Lens Quaternion Number))
(doc quat-z-lens 'description "Focus on the z component of a quaternion")
(doc quat-z-lens 'export #t)
(define quat-z-lens
  (make-lens
   quat-z
   (lambda (new-z q) (quat (quat-w q) (quat-x q) (quat-y q) new-z))))

;;; ============================================================
;;; Section 2: Rigid Body 3D Core Lenses
;;; ============================================================

(doc 'section 'rigid-body-3d-lenses)

(doc rigid-body-3d-pos-lens 'type '(Lens RigidBody3D Vec3))
(doc rigid-body-3d-pos-lens 'description "Focus on the position (world-space center of mass)")
(doc rigid-body-3d-pos-lens 'export #t)
(define rigid-body-3d-pos-lens
  (make-lens
   rigid-body-3d-pos
   (lambda (new-pos b) (rigid-body-3d-with-pos b new-pos))))

(doc rigid-body-3d-vel-lens 'type '(Lens RigidBody3D Vec3))
(doc rigid-body-3d-vel-lens 'description "Focus on the linear velocity (world-space)")
(doc rigid-body-3d-vel-lens 'export #t)
(define rigid-body-3d-vel-lens
  (make-lens
   rigid-body-3d-vel
   (lambda (new-vel b) (rigid-body-3d-with-vel b new-vel))))

(doc rigid-body-3d-orientation-lens 'type '(Lens RigidBody3D Quaternion))
(doc rigid-body-3d-orientation-lens 'description "Focus on the rotation quaternion")
(doc rigid-body-3d-orientation-lens 'export #t)
(define rigid-body-3d-orientation-lens
  (make-lens
   rigid-body-3d-orientation
   (lambda (new-orient b) (rigid-body-3d-with-orientation b new-orient))))

(doc rigid-body-3d-angular-vel-lens 'type '(Lens RigidBody3D Vec3))
(doc rigid-body-3d-angular-vel-lens 'description "Focus on the angular velocity (world-space, radians/second)")
(doc rigid-body-3d-angular-vel-lens 'export #t)
(define rigid-body-3d-angular-vel-lens
  (make-lens
   rigid-body-3d-angular-vel
   (lambda (new-omega b) (rigid-body-3d-with-angular-vel b new-omega))))

(doc rigid-body-3d-mass-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-mass-lens 'description "Focus on the mass")
(doc rigid-body-3d-mass-lens 'note "Setting mass recalculates inverse mass")
(doc rigid-body-3d-mass-lens 'export #t)
(define rigid-body-3d-mass-lens
  (make-lens
   rigid-body-3d-mass
   (lambda (new-mass b)
     (make-rigid-body-3d (rigid-body-3d-pos b)
                         (rigid-body-3d-vel b)
                         (rigid-body-3d-orientation b)
                         (rigid-body-3d-angular-vel b)
                         new-mass
                         (rigid-body-3d-inertia b)))))

(doc rigid-body-3d-inertia-lens 'type '(Lens RigidBody3D Mat3))
(doc rigid-body-3d-inertia-lens 'description "Focus on the inertia tensor (body-space)")
(doc rigid-body-3d-inertia-lens 'note "Setting inertia recalculates inverse inertia")
(doc rigid-body-3d-inertia-lens 'export #t)
(define rigid-body-3d-inertia-lens
  (make-lens
   rigid-body-3d-inertia
   (lambda (new-inertia b)
     (make-rigid-body-3d (rigid-body-3d-pos b)
                         (rigid-body-3d-vel b)
                         (rigid-body-3d-orientation b)
                         (rigid-body-3d-angular-vel b)
                         (rigid-body-3d-mass b)
                         new-inertia))))

;;; Read-only getters for derived/cached fields
(doc rigid-body-3d-inv-mass-getter 'type '(Getter RigidBody3D Number))
(doc rigid-body-3d-inv-mass-getter 'description "Read-only access to inverse mass (computed)")
(doc rigid-body-3d-inv-mass-getter 'export #t)
(define rigid-body-3d-inv-mass-getter
  (make-getter rigid-body-3d-inv-mass))

(doc rigid-body-3d-inv-inertia-getter 'type '(Getter RigidBody3D Mat3))
(doc rigid-body-3d-inv-inertia-getter 'description "Read-only access to inverse inertia tensor (computed)")
(doc rigid-body-3d-inv-inertia-getter 'export #t)
(define rigid-body-3d-inv-inertia-getter
  (make-getter rigid-body-3d-inv-inertia))

;;; ============================================================
;;; Section 3: Composed Lenses for Deep Access
;;; ============================================================

(doc 'section 'composed-lenses)
(doc 'note "Position component access")

(doc rigid-body-3d-pos-x-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-pos-x-lens 'description "Focus on position's x component")
(doc rigid-body-3d-pos-x-lens 'export #t)
(define rigid-body-3d-pos-x-lens
  (lens-compose rigid-body-3d-pos-lens vec3-x-lens))

(doc rigid-body-3d-pos-y-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-pos-y-lens 'export #t)
(define rigid-body-3d-pos-y-lens
  (lens-compose rigid-body-3d-pos-lens vec3-y-lens))

(doc rigid-body-3d-pos-z-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-pos-z-lens 'export #t)
(define rigid-body-3d-pos-z-lens
  (lens-compose rigid-body-3d-pos-lens vec3-z-lens))

(doc 'note "Velocity component access")

(doc rigid-body-3d-vel-x-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-vel-x-lens 'description "Focus on velocity's x component")
(doc rigid-body-3d-vel-x-lens 'export #t)
(define rigid-body-3d-vel-x-lens
  (lens-compose rigid-body-3d-vel-lens vec3-x-lens))

(doc rigid-body-3d-vel-y-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-vel-y-lens 'export #t)
(define rigid-body-3d-vel-y-lens
  (lens-compose rigid-body-3d-vel-lens vec3-y-lens))

(doc rigid-body-3d-vel-z-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-vel-z-lens 'export #t)
(define rigid-body-3d-vel-z-lens
  (lens-compose rigid-body-3d-vel-lens vec3-z-lens))

(doc 'note "Angular velocity component access")

(doc rigid-body-3d-angular-vel-x-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-angular-vel-x-lens 'description "Focus on angular velocity's x component (roll rate)")
(doc rigid-body-3d-angular-vel-x-lens 'export #t)
(define rigid-body-3d-angular-vel-x-lens
  (lens-compose rigid-body-3d-angular-vel-lens vec3-x-lens))

(doc rigid-body-3d-angular-vel-y-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-angular-vel-y-lens 'description "Focus on angular velocity's y component (pitch rate)")
(doc rigid-body-3d-angular-vel-y-lens 'export #t)
(define rigid-body-3d-angular-vel-y-lens
  (lens-compose rigid-body-3d-angular-vel-lens vec3-y-lens))

(doc rigid-body-3d-angular-vel-z-lens 'type '(Lens RigidBody3D Number))
(doc rigid-body-3d-angular-vel-z-lens 'description "Focus on angular velocity's z component (yaw rate)")
(doc rigid-body-3d-angular-vel-z-lens 'export #t)
(define rigid-body-3d-angular-vel-z-lens
  (lens-compose rigid-body-3d-angular-vel-lens vec3-z-lens))

;;; ============================================================
;;; Section 4: Dot Notation Macro
;;; ============================================================

(doc 'section 'dot-notation)

(doc body3d. 'type '(-> Symbol ... Lens))
(doc body3d. 'description "Convenience macro for composing 3D physics lenses using dot notation")
(doc body3d. 'note "(body3d. pos x) => (lens-compose rigid-body-3d-pos-lens vec3-x-lens)")
(doc body3d. 'export #t)

(define-syntax body3d.
  (syntax-rules (pos vel orientation angular-vel mass inertia x y z w)
    ;; Position paths
    [(body3d. pos) rigid-body-3d-pos-lens]
    [(body3d. pos x) rigid-body-3d-pos-x-lens]
    [(body3d. pos y) rigid-body-3d-pos-y-lens]
    [(body3d. pos z) rigid-body-3d-pos-z-lens]
    ;; Velocity paths
    [(body3d. vel) rigid-body-3d-vel-lens]
    [(body3d. vel x) rigid-body-3d-vel-x-lens]
    [(body3d. vel y) rigid-body-3d-vel-y-lens]
    [(body3d. vel z) rigid-body-3d-vel-z-lens]
    ;; Orientation paths
    [(body3d. orientation) rigid-body-3d-orientation-lens]
    [(body3d. orientation w) (lens-compose rigid-body-3d-orientation-lens quat-w-lens)]
    [(body3d. orientation x) (lens-compose rigid-body-3d-orientation-lens quat-x-lens)]
    [(body3d. orientation y) (lens-compose rigid-body-3d-orientation-lens quat-y-lens)]
    [(body3d. orientation z) (lens-compose rigid-body-3d-orientation-lens quat-z-lens)]
    ;; Angular velocity paths
    [(body3d. angular-vel) rigid-body-3d-angular-vel-lens]
    [(body3d. angular-vel x) rigid-body-3d-angular-vel-x-lens]
    [(body3d. angular-vel y) rigid-body-3d-angular-vel-y-lens]
    [(body3d. angular-vel z) rigid-body-3d-angular-vel-z-lens]
    ;; Mass and inertia
    [(body3d. mass) rigid-body-3d-mass-lens]
    [(body3d. inertia) rigid-body-3d-inertia-lens]))

;;; ============================================================
;;; Section 5: Traversals for Body Collections
;;; ============================================================

(doc 'section 'traversals)

(doc bodies-3d-each 'type '(Traversal (List RigidBody3D) RigidBody3D))
(doc bodies-3d-each 'description "Traverse each 3D body in a collection")
(doc bodies-3d-each 'export #t)
(define bodies-3d-each
  (make-traversal
   (lambda (f bodies) (map f bodies))
   identity))

(doc bodies-3d-filtered 'type '(-> (-> RigidBody3D Boolean) (Traversal (List RigidBody3D) RigidBody3D)))
(doc bodies-3d-filtered 'description "Traverse 3D bodies matching predicate")
(doc bodies-3d-filtered 'export #t)
(define (bodies-3d-filtered pred)
  (make-traversal
   (lambda (f bodies)
     (map (lambda (b) (if (pred b) (f b) b)) bodies))
   (lambda (bodies) (filter pred bodies))))

(doc dynamic-bodies-3d 'type '(Traversal (List RigidBody3D) RigidBody3D))
(doc dynamic-bodies-3d 'description "Traverse only dynamic (non-static) bodies")
(doc dynamic-bodies-3d 'export #t)
(define dynamic-bodies-3d
  (bodies-3d-filtered rigid-body-3d-dynamic?))

(doc static-bodies-3d 'type '(Traversal (List RigidBody3D) RigidBody3D))
(doc static-bodies-3d 'description "Traverse only static bodies")
(doc static-bodies-3d 'export #t)
(define static-bodies-3d
  (bodies-3d-filtered rigid-body-3d-static?))

;;; ============================================================
;;; Section 6: Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

(doc translate-body-3d 'type '(-> Vec3 (-> RigidBody3D RigidBody3D)))
(doc translate-body-3d 'description "Translate a 3D body by offset")
(doc translate-body-3d 'export #t)
(define (translate-body-3d offset)
  (lambda (body)
    (& body (%~ rigid-body-3d-pos-lens (lambda (p) (vec3-add p offset))))))

(doc apply-central-impulse-3d 'type '(-> Vec3 (-> RigidBody3D RigidBody3D)))
(doc apply-central-impulse-3d 'description "Apply impulse at center of mass (no torque)")
(doc apply-central-impulse-3d 'export #t)
(define (apply-central-impulse-3d impulse)
  (lambda (body)
    (if (rigid-body-3d-static? body)
        body
        (let ([delta-v (vec3-scale impulse (rigid-body-3d-inv-mass body))])
          (& body (%~ rigid-body-3d-vel-lens (lambda (v) (vec3-add v delta-v))))))))

(doc apply-gravity-3d 'type '(-> Vec3 Number (-> RigidBody3D RigidBody3D)))
(doc apply-gravity-3d 'description "Apply gravitational acceleration for time dt")
(doc apply-gravity-3d 'export #t)
(define (apply-gravity-3d gravity dt)
  (lambda (body)
    (if (rigid-body-3d-static? body)
        body
        (& body (%~ rigid-body-3d-vel-lens
                    (lambda (v) (vec3-add v (vec3-scale gravity dt))))))))

(doc step-body-3d 'type '(-> Number (-> RigidBody3D RigidBody3D)))
(doc step-body-3d 'description "Single Euler integration step (position only)")
(doc step-body-3d 'export #t)
(define (step-body-3d dt)
  (lambda (body)
    (let ([vel (^. body rigid-body-3d-vel-lens)])
      (& body (%~ rigid-body-3d-pos-lens
                  (lambda (p) (vec3-add p (vec3-scale vel dt))))))))

(doc step-bodies-3d 'type '(-> Number (-> (List RigidBody3D) (List RigidBody3D))))
(doc step-bodies-3d 'description "Step all bodies using traversal")
(doc step-bodies-3d 'export #t)
(define (step-bodies-3d dt)
  (lambda (bodies)
    (traversal-over bodies-3d-each (step-body-3d dt) bodies)))

;;; ============================================================
;;; Section 7: Folds for Aggregate Queries
;;; ============================================================

(doc 'section 'folds)

(doc total-mass-3d 'type '(-> (List RigidBody3D) Number))
(doc total-mass-3d 'description "Sum masses of all 3D bodies")
(doc total-mass-3d 'export #t)
(define (total-mass-3d bodies)
  (fold-sum (fold-compose (make-fold identity) (lens->fold rigid-body-3d-mass-lens)) bodies))

(doc center-of-mass-3d 'type '(-> (List RigidBody3D) Vec3))
(doc center-of-mass-3d 'description "Compute center of mass for 3D bodies")
(doc center-of-mass-3d 'export #t)
(define (center-of-mass-3d bodies)
  (let* ([total-m (total-mass-3d bodies)]
         [weighted-sum
          (fold-left
           (lambda (acc b)
             (let ([m (^. b rigid-body-3d-mass-lens)]
                   [p (^. b rigid-body-3d-pos-lens)])
               (vec3-add acc (vec3-scale p m))))
           (vec3-zero)
           bodies)])
    (if (= total-m 0)
        (vec3-zero)
        (vec3-scale weighted-sum (/ 1 total-m)))))

(doc total-kinetic-energy 'type '(-> (List RigidBody3D) Number))
(doc total-kinetic-energy 'description "Sum kinetic energies of all bodies")
(doc total-kinetic-energy 'export #t)
(define (total-kinetic-energy bodies)
  (apply + (map rigid-body-3d-kinetic-energy bodies)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

(doc 'section 'exports)
(doc 'note "Quaternion lenses: quat-w-lens, quat-x-lens, quat-y-lens, quat-z-lens")
(doc 'note "Core lenses: rigid-body-3d-{pos,vel,orientation,angular-vel,mass,inertia}-lens")
(doc 'note "Composed lenses: rigid-body-3d-{pos,vel,angular-vel}-{x,y,z}-lens")
(doc 'note "Getters: rigid-body-3d-{inv-mass,inv-inertia}-getter")
(doc 'note "Dot notation: (body3d. pos x), (body3d. orientation w), etc.")
(doc 'note "Traversals: bodies-3d-each, bodies-3d-filtered, dynamic-bodies-3d, static-bodies-3d")
(doc 'note "Convenience: translate-body-3d, apply-central-impulse-3d, apply-gravity-3d")
(doc 'note "Integration: step-body-3d, step-bodies-3d")
(doc 'note "Folds: total-mass-3d, center-of-mass-3d, total-kinetic-energy")
(doc 'note "Re-exports vec3 lenses from geometry-optics.ss")

(display "lenses3d.ss loaded.\n")
(display "  Quaternion:    quat-{w,x,y,z}-lens\n")
(display "  RigidBody3D:   rigid-body-3d-{pos,vel,orientation,angular-vel}-lens\n")
(display "                 rigid-body-3d-{mass,inertia}-lens\n")
(display "  Composed:      rigid-body-3d-pos-{x,y,z}-lens, etc.\n")
(display "  Dot notation:  (body3d. pos x), (body3d. orientation w)\n")
(display "  Traversals:    bodies-3d-each, dynamic-bodies-3d\n")
(display "  Helpers:       translate-body-3d, apply-gravity-3d, step-body-3d\n")
