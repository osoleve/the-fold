(load "lattice/physics/diff/traced-body.ss")

(doc 'module 'traced-body-protocols)
(doc 'description "Protocol implementations for TracedBody

Register traced-body implementations for the body lens protocols.
Load this file after loading lenses.ss to enable generic lenses
with traced bodies.

Usage:
  (load \"lattice/physics/lenses/lenses.ss\")
  (load \"lattice/physics/diff/traced-body-protocols.ss\")

  ;; Now generic lenses work with traced bodies
  (view body-pos-lens my-traced-body)  ; => TracedVec2")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'protocol-implementations-tracedbody)
(doc 'note "Uses naming convention: traced-body-<field>, traced-body-with-<field>")
(doc 'note "No traced-body-with-mass exists, so we override the mass slot")
(doc 'note "Mass is not traced (treated as constant in autodiff)")

(derive-bundle! body-ops 'traced-body traced-body
  ("mass"
   traced-body-mass
   (lambda (b m)
     (make-traced-body (traced-body-pos b)
                       (traced-body-vel b)
                       (traced-body-angle b)
                       (traced-body-angular-vel b)
                       m
                       (traced-body-inertia b)))))

(display "  TracedBody now works with body-pos-lens, body-vel-lens, body-mass-lens\n")
