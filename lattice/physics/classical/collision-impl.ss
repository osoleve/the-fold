(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-impl
;;; @requires integrators rigid-body particles
(require 'integrators)
(require 'rigid-body)
(require 'particles)

(doc "lattice/physics/classical/collision-impl.ss — Collision Protocol Implementations")

(doc 'module 'collision-impl)
(doc 'description "Collision protocol implementations for standard body types")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'body2d)

(implement-protocol! 'col-inv-mass 'body-2d
  body-inv-mass)

(implement-protocol! 'col-inv-inertia 'body-2d
  (lambda (b) 0))  ; No rotational inertia

(implement-protocol! 'col-static? 'body-2d
  body-static?)

(implement-protocol! 'col-pos 'body-2d
  body-pos)

(implement-protocol! 'col-vel-at 'body-2d
  (lambda (b point)
    (body-vel b)))  ; No angular contribution

(implement-protocol! 'col-apply-impulse 'body-2d
  (lambda (b impulse contact)
    (if (body-static? b)
        b
        (let ([new-vel (vec2-add (body-vel b)
                                 (vec2-scale impulse (body-inv-mass b)))])
          (body-with-vel b new-vel)))))

(doc 'section 'rigidbody2d)

(implement-protocol! 'col-inv-mass 'rigid-body-2d
  rigid-body-inv-mass)

(implement-protocol! 'col-inv-inertia 'rigid-body-2d
  rigid-body-inv-inertia)

(implement-protocol! 'col-static? 'rigid-body-2d
  rigid-body-static?)

(implement-protocol! 'col-pos 'rigid-body-2d
  rigid-body-pos)

(implement-protocol! 'col-vel-at 'rigid-body-2d
  (lambda (b point)
    (rigid-body-velocity-at b point)))

(implement-protocol! 'col-apply-impulse 'rigid-body-2d
  (lambda (b impulse contact)
    (if (rigid-body-static? b)
        b  ; Static body
        (let* ([inv-mass (rigid-body-inv-mass b)]
               [inv-i (rigid-body-inv-inertia b)]
               [r (vec2-sub contact (rigid-body-pos b))]
               [torque (vec2-cross r impulse)]
               [new-vel (vec2-add (rigid-body-vel b)
                                  (vec2-scale impulse inv-mass))]
               [new-omega (+ (rigid-body-angular-vel b)
                             (* torque inv-i))])
          (rigid-body-with-angular-vel
           (rigid-body-with-vel b new-vel)
           new-omega)))))

(doc 'section 'particle)

(implement-protocol! 'col-inv-mass 'particle
  (lambda (p) 1.0))  ; Implicit unit mass

(implement-protocol! 'col-inv-inertia 'particle
  (lambda (p) 0))  ; No rotational inertia

(implement-protocol! 'col-static? 'particle
  (lambda (p) #f))  ; Particles are never static

(implement-protocol! 'col-pos 'particle
  particle-pos)

(implement-protocol! 'col-vel-at 'particle
  (lambda (p point)
    (particle-vel p)))  ; No angular contribution

(implement-protocol! 'col-apply-impulse 'particle
  (lambda (p impulse contact)
    ;; Particles have unit mass, so impulse directly changes velocity
    (let ([new-vel (vec2-add (particle-vel p) impulse)])
      (particle-with-vel p new-vel))))

(doc 'section 'print)

(display "  Collision protocols registered for: body-2d, rigid-body-2d, particle\n")
(display "  Check with: (collision-capable? 'body-2d) => #t\n")
