(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-protocol3d
;;; @requires fp/protocol vec3 rigid-body3d
(require 'fp/protocol)
(require 'vec3)
(require 'rigid-body3d)

(doc 'module 'collision-protocol3d)
(doc 'description "Extensible 3D collision response protocols.
Mirrors collision-protocol.ss (2D) with Mat3 inertia instead of scalar.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'collision-3d)

(doc 'col3d-inv-mass 'type 'Body → Number)
(doc "Get inverse mass for impulse calculations. Returns 0 for static bodies.")
(define-protocol (col3d-inv-mass body)
  "Get inverse mass for 3D collision impulse calculation")

(doc 'col3d-inv-inertia-world 'type 'Body → Mat3)
(doc "Get world-space inverse inertia tensor. Returns mat3-zero for static bodies.")
(define-protocol (col3d-inv-inertia-world body)
  "Get world-space inverse inertia tensor for rotational impulse calculation")

(doc "col3d-static? : Body → Boolean")
(doc "Check if body is static (immovable).")
(define-protocol (col3d-static? body)
  "Check if body is static/immovable")

(doc 'col3d-pos 'type 'Body → Vec3)
(doc "Get body position for lever arm calculation.")
(define-protocol (col3d-pos body)
  "Get body position for collision calculations")

(doc 'col3d-vel-at 'type 'Body × Vec3 → Vec3)
(doc "Get velocity at a world point, including angular contribution.")
(define-protocol (col3d-vel-at body point)
  "Get velocity at a world point including angular contribution")

(doc 'col3d-apply-impulse 'type 'Body × Vec3 × Vec3 → Body)
(doc "Apply impulse at a contact point, updating linear and angular velocity.")
(doc "Returns the updated body.")
(define-protocol (col3d-apply-impulse body impulse contact)
  "Apply impulse at contact point, returning updated body")

(doc 'section 'helper)

(doc "collision-capable-3d? : Symbol → Boolean")
(doc "Check if a type has registered all 3D collision protocols.")
(define (collision-capable-3d? type-tag)
  (doc 'export #t)
  (and (type-implements? type-tag 'col3d-inv-mass)
       (type-implements? type-tag 'col3d-inv-inertia-world)
       (type-implements? type-tag 'col3d-static?)
       (type-implements? type-tag 'col3d-pos)
       (type-implements? type-tag 'col3d-vel-at)
       (type-implements? type-tag 'col3d-apply-impulse)))

(doc "assert-collision-capable-3d! : Any → Any or Error")
(doc "Assert that a value's type implements all 3D collision protocols.")
(define (assert-collision-capable-3d! body)
  (doc 'export #t)
  (let ([type-tag (get-type-tag body)])
    (if (collision-capable-3d? type-tag)
        body
        (error 'assert-collision-capable-3d!
               (format "Type '~a' does not implement 3D collision protocols" type-tag)))))

(doc 'section 'derived)

(doc 'col3d-relative-vel-at 'type 'Body × Body × Vec3 → Vec3)
(doc "Relative velocity at contact point (B relative to A).")
(define (col3d-relative-vel-at body-a body-b contact)
  (doc 'export #t)
  (vec3-sub (col3d-vel-at body-b contact)
            (col3d-vel-at body-a contact)))

(doc 'col3d-normal-vel-at 'type 'Body × Body × Vec3 × Vec3 → Number)
(doc "Velocity along collision normal at contact point.")
(define (col3d-normal-vel-at body-a body-b contact normal)
  (doc 'export #t)
  (vec3-dot (col3d-relative-vel-at body-a body-b contact) normal))

(doc 'col3d-effective-mass 'type 'Body × Body × Vec3 × Vec3 → Number)
(doc "Calculate effective mass for impulse calculation including rotation.")
(doc "Uses 3D inertia tensor: K = inv_m_a + inv_m_b + (r_a × n)·I_a^-1·(r_a × n) + (r_b × n)·I_b^-1·(r_b × n)")
(define (col3d-effective-mass body-a body-b contact normal)
  (doc 'export #t)
  (let* ([inv-m-a (col3d-inv-mass body-a)]
         [inv-m-b (col3d-inv-mass body-b)]
         [inv-I-a (col3d-inv-inertia-world body-a)]
         [inv-I-b (col3d-inv-inertia-world body-b)]
         [r-a (vec3-sub contact (col3d-pos body-a))]
         [r-b (vec3-sub contact (col3d-pos body-b))]
         [r-a-cross-n (vec3-cross r-a normal)]
         [r-b-cross-n (vec3-cross r-b normal)]
         [ang-term-a (vec3-cross (mat3-mul-vec3 inv-I-a r-a-cross-n) r-a)]
         [ang-term-b (vec3-cross (mat3-mul-vec3 inv-I-b r-b-cross-n) r-b)]
         [raw-eff-mass (+ inv-m-a inv-m-b
                          (vec3-dot ang-term-a normal)
                          (vec3-dot ang-term-b normal))])
    (if (< raw-eff-mass 1e-10)
        +inf.0
        raw-eff-mass)))

(display "  Protocols: col3d-inv-mass, col3d-inv-inertia-world, col3d-static?, col3d-pos, col3d-vel-at, col3d-apply-impulse\n")
(display "  Check:     (collision-capable-3d? 'type-tag)\n")
(display "  Assert:    (assert-collision-capable-3d! body)\n")
