;;; lattice/physics/lenses/README.sexp --- Physics Lenses Documentation
;;;
;;; Module documentation in S-expression format.

(module physics/lenses
  (description "Functional optics (lenses) for physics state access")

  (purpose
    "Enable elegant, composable access to nested physics structures:"
    "- View/set/modify body position, velocity, angle without boilerplate"
    "- Compose lenses for deep access (body -> position -> x)"
    "- Type-polymorphic: same lens works on rigid-body and particle"
    "- Satisfy lens laws for predictable behavior")

  (quick-start
    ";;; Load the lens library"
    "(load \"lattice/physics/lenses/lenses.ss\")"
    ""
    ";;; Create a rigid body"
    "(define body (make-rigid-body (vec2 10 20) (vec2 1 2) 0 0 5 2))"
    ""
    ";;; View position x-coordinate"
    "(view rigid-body-pos-x-lens body)  ; => 10"
    ""
    ";;; Modify position x-coordinate"
    "(over (body. pos x) (lambda (x) (+ x 100)) body)"
    ""
    ";;; Set velocity"
    "(set-lens rigid-body-vel-lens (vec2 0 0) body)")

  (exports
    (vec2-lenses
      "vec2-x-lens : Lens Vec2 Number"
      "vec2-y-lens : Lens Vec2 Number")

    (rigid-body-lenses
      "rigid-body-pos-lens : Lens RigidBody2D Vec2"
      "rigid-body-vel-lens : Lens RigidBody2D Vec2"
      "rigid-body-angle-lens : Lens RigidBody2D Number"
      "rigid-body-angular-vel-lens : Lens RigidBody2D Number"
      "rigid-body-mass-lens : Lens RigidBody2D Number"
      "rigid-body-inertia-lens : Lens RigidBody2D Number"
      "rigid-body-pos-x-lens : Lens RigidBody2D Number (composed)"
      "rigid-body-pos-y-lens : Lens RigidBody2D Number (composed)"
      "rigid-body-vel-x-lens : Lens RigidBody2D Number (composed)"
      "rigid-body-vel-y-lens : Lens RigidBody2D Number (composed)")

    (particle-lenses
      "particle-pos-lens : Lens Particle Vec2"
      "particle-vel-lens : Lens Particle Vec2"
      "particle-lifetime-lens : Lens Particle Number"
      "particle-size-lens : Lens Particle Number"
      "particle-color-lens : Lens Particle Any"
      "particle-pos-x-lens : Lens Particle Number (composed)"
      "particle-pos-y-lens : Lens Particle Number (composed)"
      "particle-vel-x-lens : Lens Particle Number (composed)"
      "particle-vel-y-lens : Lens Particle Number (composed)")

    (generic-lenses
      "body-pos-lens : Lens Body Vec2 (polymorphic)"
      "body-vel-lens : Lens Body Vec2 (polymorphic)"
      "body-pos : Body -> Vec2 (accessor)"
      "body-vel : Body -> Vec2 (accessor)")

    (aliases
      "position-lens = body-pos-lens"
      "velocity-lens = body-vel-lens"
      "mass-lens = rigid-body-mass-lens"
      "rotation-lens = rigid-body-angle-lens")

    (utilities
      "apply-force-via-lens : Lens Lens Vec2 Number Body -> Body"
      "integrate-position-via-lens : Lens Lens Number Body -> Body")

    (syntax
      "(body. pos)       => body-pos-lens"
      "(body. pos x)     => (lens-compose body-pos-lens vec2-x-lens)"
      "(body. vel y)     => (lens-compose body-vel-lens vec2-y-lens)"
      "(body. angle)     => rigid-body-angle-lens"
      "(body. angular-vel) => rigid-body-angular-vel-lens"
      "(body. mass)      => rigid-body-mass-lens"
      "(body. inertia)   => rigid-body-inertia-lens"
      "(body. lifetime)  => particle-lifetime-lens"
      "(body. size)      => particle-size-lens"
      "(body. color)     => particle-color-lens"))

  (lens-laws
    "All lenses satisfy the three fundamental lens laws:"
    ""
    "Get-Put Law: (set-lens l (view l s) s) = s"
    "  Setting what you just got is a no-op"
    ""
    "Put-Get Law: (view l (set-lens l a s)) = a"
    "  Getting what you just set returns what you set"
    ""
    "Put-Put Law: (set-lens l a' (set-lens l a s)) = (set-lens l a' s)"
    "  Consecutive sets collapse to just the last set")

  (example-gravity-system
    ";;; Apply gravity to all bodies using lenses"
    "(define gravity (vec2 0 9.8))"
    ""
    "(define (apply-gravity body dt)"
    "  (over rigid-body-vel-lens"
    "        (lambda (v) (vec2-add v (vec2-scale gravity dt)))"
    "        body))"
    ""
    ";;; Process multiple bodies"
    "(define (step-world bodies dt)"
    "  (map (lambda (b) (apply-gravity b dt)) bodies))")

  (tier 1)
  (purity total)
  (dependencies
    (fp/templates.ss "Core lens infrastructure")
    (linalg/vec2.ss "Vec2 data type")
    (physics/classical/rigid-body.ss "RigidBody2D data type")
    (physics/classical/particles.ss "Particle data type"))

  (test-command "scheme --script lattice/physics/lenses/test-lenses.ss")

  (author "Claude Opus 4.5")
  (created "2026-01-16"))
