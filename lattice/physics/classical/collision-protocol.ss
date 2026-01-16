;;; lattice/physics/classical/collision-protocol.ss — Collision Response Protocols
;;;
;;; Defines extensible protocols for collision response operations.
;;; This enables polymorphic collision handling across body types without
;;; scattered type checks.
;;;
;;; Protocols:
;;;   - col-inv-mass      : Body → Number
;;;   - col-inv-inertia   : Body → Number
;;;   - col-static?       : Body → Boolean
;;;   - col-pos           : Body → Vec2
;;;   - col-vel-at        : Body × Vec2 → Vec2
;;;   - col-apply-impulse : Body × Vec2 × Vec2 → Body
;;;
;;; Dependencies:
;;;   - lattice/fp/protocol.ss
;;;   - lattice/linalg/vec2.ss

(load "lattice/fp/protocol.ss")
(load "lattice/linalg/vec2.ss")

;;; ====
;;; Collision Protocol Definitions
;;; ====

;;; col-inv-mass : Body → Number
;;; Get inverse mass for impulse calculations. Returns 0 for static bodies.
(define-protocol (col-inv-mass body)
  "Get inverse mass for collision impulse calculation")

;;; col-inv-inertia : Body → Number
;;; Get inverse moment of inertia. Returns 0 for non-rotating bodies.
(define-protocol (col-inv-inertia body)
  "Get inverse inertia for rotational impulse calculation")

;;; col-static? : Body → Boolean
;;; Check if body is static (immovable).
(define-protocol (col-static? body)
  "Check if body is static/immovable")

;;; col-pos : Body → Vec2
;;; Get body position for lever arm calculation.
(define-protocol (col-pos body)
  "Get body position for collision calculations")

;;; col-vel-at : Body × Vec2 → Vec2
;;; Get velocity at a world point, including angular contribution.
(define-protocol (col-vel-at body point)
  "Get velocity at a world point including angular contribution")

;;; col-apply-impulse : Body × Vec2 × Vec2 → Body
;;; Apply impulse at a contact point, updating linear and angular velocity.
;;; Returns the updated body.
(define-protocol (col-apply-impulse body impulse contact)
  "Apply impulse at contact point, returning updated body")

;;; ====
;;; Helper: Check if type is collision-capable
;;; ====

;;; collision-capable? : Symbol → Boolean
;;; Check if a type has registered all collision protocols.
(define (collision-capable? type-tag)
  (and (type-implements? type-tag 'col-inv-mass)
       (type-implements? type-tag 'col-inv-inertia)
       (type-implements? type-tag 'col-static?)
       (type-implements? type-tag 'col-pos)
       (type-implements? type-tag 'col-vel-at)
       (type-implements? type-tag 'col-apply-impulse)))

;;; assert-collision-capable! : Any → Any | Error
;;; Assert that a value's type implements all collision protocols.
(define (assert-collision-capable! body)
  (let ([type-tag (get-type-tag body)])
    (if (collision-capable? type-tag)
        body
        (error 'assert-collision-capable!
               (format "Type '~a' does not implement collision protocols" type-tag)))))

;;; ====
;;; Derived Operations (use protocols internally)
;;; ====

;;; col-relative-vel-at : Body × Body × Vec2 → Vec2
;;; Relative velocity at contact point (B relative to A).
(define (col-relative-vel-at body-a body-b contact)
  (vec2-sub (col-vel-at body-b contact)
            (col-vel-at body-a contact)))

;;; col-normal-vel-at : Body × Body × Vec2 × Vec2 → Number
;;; Velocity along collision normal at contact point.
(define (col-normal-vel-at body-a body-b contact normal)
  (vec2-dot (col-relative-vel-at body-a body-b contact) normal))

;;; col-effective-mass : Body × Body × Vec2 × Vec2 → Number
;;; Calculate effective mass for impulse calculation including rotation.
(define (col-effective-mass body-a body-b contact normal)
  (let* ([inv-m-a (col-inv-mass body-a)]
         [inv-m-b (col-inv-mass body-b)]
         [inv-i-a (col-inv-inertia body-a)]
         [inv-i-b (col-inv-inertia body-b)]
         [r-a (vec2-sub contact (col-pos body-a))]
         [r-b (vec2-sub contact (col-pos body-b))]
         [rn-a (vec2-cross r-a normal)]
         [rn-b (vec2-cross r-b normal)])
    (+ inv-m-a inv-m-b
       (* rn-a rn-a inv-i-a)
       (* rn-b rn-b inv-i-b))))

;;; ====
;;; Print Help
;;; ====

(display "collision-protocol.ss loaded.\n")
(display "  Protocols: col-inv-mass, col-inv-inertia, col-static?, col-pos, col-vel-at, col-apply-impulse\n")
(display "  Check:     (collision-capable? 'type-tag)\n")
(display "  Assert:    (assert-collision-capable! body)\n")
