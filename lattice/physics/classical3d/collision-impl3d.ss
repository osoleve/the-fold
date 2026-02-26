(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-impl3d
;;; @requires prelude collision-protocol3d rigid-body3d
(require 'prelude)
(require 'collision-protocol3d)
(require 'rigid-body3d)

(doc 'module 'collision-impl3d)
(doc 'description "3D collision protocol implementations for rigid-body-3d")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'rigid-body-3d)

(implement-protocol! 'col3d-inv-mass 'rigid-body-3d
  rigid-body-3d-inv-mass)

(implement-protocol! 'col3d-inv-inertia-world 'rigid-body-3d
  rigid-body-3d-world-inv-inertia)

(implement-protocol! 'col3d-static? 'rigid-body-3d
  rigid-body-3d-static?)

(implement-protocol! 'col3d-pos 'rigid-body-3d
  rigid-body-3d-pos)

(implement-protocol! 'col3d-vel-at 'rigid-body-3d
  (lambda (b point)
    (rigid-body-3d-velocity-at b point)))

(implement-protocol! 'col3d-apply-impulse 'rigid-body-3d
  (lambda (b impulse contact)
    (rigid-body-3d-apply-impulse b impulse contact)))

(display "  Collision protocols registered for: rigid-body-3d\n")
(display "  Check with: (collision-capable-3d? 'rigid-body-3d) => #t\n")
