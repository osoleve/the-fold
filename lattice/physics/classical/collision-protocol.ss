(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-protocol
;;; @requires prelude protocol vec2
(require 'prelude)
(require 'protocol)
(require 'vec2)

(doc "lattice/physics/classical/collision-protocol.ss — Collision Response Protocols")

(doc 'module 'collision-protocol)
(doc 'description "Extensible collision response protocols")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'collision)

(doc 'col-inv-mass 'type 'Body → Number)
(doc "Get inverse mass for impulse calculations. Returns 0 for static bodies.")
(define-protocol (col-inv-mass body)
  "Get inverse mass for collision impulse calculation")

(doc 'col-inv-inertia 'type 'Body → Number)
(doc "Get inverse moment of inertia. Returns 0 for non-rotating bodies.")
(define-protocol (col-inv-inertia body)
  "Get inverse inertia for rotational impulse calculation")

(doc "col-static? : Body → Boolean")
(doc "Check if body is static (immovable).")
(define-protocol (col-static? body)
  "Check if body is static/immovable")

(doc 'col-pos 'type 'Body → Vec2)
(doc "Get body position for lever arm calculation.")
(define-protocol (col-pos body)
  "Get body position for collision calculations")

(doc 'col-vel-at 'type 'Body × Vec2 → Vec2)
(doc "Get velocity at a world point, including angular contribution.")
(define-protocol (col-vel-at body point)
  "Get velocity at a world point including angular contribution")

(doc 'col-apply-impulse 'type 'Body × Vec2 × Vec2 → Body)
(doc "Apply impulse at a contact point, updating linear and angular velocity.")
(doc "Returns the updated body.")
(define-protocol (col-apply-impulse body impulse contact)
  "Apply impulse at contact point, returning updated body")

(doc 'section 'helper:)

(doc "collision-capable? : Symbol → Boolean")
(doc "Check if a type has registered all collision protocols.")
(define (collision-capable? type-tag)
  (and (type-implements? type-tag 'col-inv-mass)
       (type-implements? type-tag 'col-inv-inertia)
       (type-implements? type-tag 'col-static?)
       (type-implements? type-tag 'col-pos)
       (type-implements? type-tag 'col-vel-at)
       (type-implements? type-tag 'col-apply-impulse)))

(doc "assert-collision-capable! : Any → Any or Error")
(doc "Assert that a value's type implements all collision protocols.")
(define (assert-collision-capable! body)
  (let ([type-tag (get-type-tag body)])
    (if (collision-capable? type-tag)
        body
        (error 'assert-collision-capable!
               (format "Type '~a' does not implement collision protocols" type-tag)))))

(doc 'section 'derived)

(doc 'col-relative-vel-at 'type 'Body × Body × Vec2 → Vec2)
(doc "Relative velocity at contact point (B relative to A).")
(define (col-relative-vel-at body-a body-b contact)
  (vec2-sub (col-vel-at body-b contact)
            (col-vel-at body-a contact)))

(doc 'col-normal-vel-at 'type 'Body × Body × Vec2 × Vec2 → Number)
(doc "Velocity along collision normal at contact point.")
(define (col-normal-vel-at body-a body-b contact normal)
  (vec2-dot (col-relative-vel-at body-a body-b contact) normal))

(doc 'col-effective-mass 'type 'Body × Body × Vec2 × Vec2 → Number)
(doc "Calculate effective mass for impulse calculation including rotation.")
(doc "Returns +inf.0 for static-static collisions (infinite effective mass).")
(define (col-effective-mass body-a body-b contact normal)
  (let* ([inv-m-a (col-inv-mass body-a)]
         [inv-m-b (col-inv-mass body-b)]
         [inv-i-a (col-inv-inertia body-a)]
         [inv-i-b (col-inv-inertia body-b)]
         [r-a (vec2-sub contact (col-pos body-a))]
         [r-b (vec2-sub contact (col-pos body-b))]
         [rn-a (vec2-cross r-a normal)]
         [rn-b (vec2-cross r-b normal)]
         [raw-eff-mass (+ inv-m-a inv-m-b
                          (* rn-a rn-a inv-i-a)
                          (* rn-b rn-b inv-i-b))])
    ;; Return +inf.0 for static-static collisions to avoid division by zero
    ;; Semantically: infinite effective mass means no impulse can move them
    (if (< raw-eff-mass 1e-10)
        +inf.0
        raw-eff-mass)))

(doc 'section 'print)

(display "  Protocols: col-inv-mass, col-inv-inertia, col-static?, col-pos, col-vel-at, col-apply-impulse\n")
(display "  Check:     (collision-capable? 'type-tag)\n")
(display "  Assert:    (assert-collision-capable! body)\n")
