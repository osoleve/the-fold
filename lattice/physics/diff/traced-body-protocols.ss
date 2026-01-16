;;; lattice/physics/diff/traced-body-protocols.ss — Protocol implementations for TracedBody
;;;
;;; Register traced-body implementations for the body lens protocols.
;;; Load this file after loading lenses.ss to enable generic lenses
;;; with traced bodies.
;;;
;;; Usage:
;;;   (load "lattice/physics/lenses/lenses.ss")
;;;   (load "lattice/physics/diff/traced-body-protocols.ss")
;;;
;;;   ;; Now generic lenses work with traced bodies
;;;   (view body-pos-lens my-traced-body)  ; => TracedVec2
;;;
;;; Dependencies:
;;;   - lattice/fp/protocol.ss (already loaded by lenses.ss)
;;;   - lattice/physics/diff/traced-body.ss

(load "lattice/physics/diff/traced-body.ss")

;;; ====
;;; Protocol Implementations: TracedBody
;;; ====

;;; body-pos : TracedBody → TracedVec2
(implement-protocol! 'body-pos 'traced-body
  traced-body-pos)

;;; body-set-pos : TracedBody × TracedVec2 → TracedBody
(implement-protocol! 'body-set-pos 'traced-body
  (lambda (b p) (traced-body-with-pos b p)))

;;; body-vel : TracedBody → TracedVec2
(implement-protocol! 'body-vel 'traced-body
  traced-body-vel)

;;; body-set-vel : TracedBody × TracedVec2 → TracedBody
(implement-protocol! 'body-set-vel 'traced-body
  (lambda (b v) (traced-body-with-vel b v)))

;;; body-mass : TracedBody → Number
;;; Note: Mass is not traced (treated as constant in autodiff).
(implement-protocol! 'body-mass 'traced-body
  traced-body-mass)

;;; body-set-mass : TracedBody × Number → TracedBody
(implement-protocol! 'body-set-mass 'traced-body
  (lambda (b m)
    (make-traced-body (traced-body-pos b)
                      (traced-body-vel b)
                      (traced-body-angle b)
                      (traced-body-angular-vel b)
                      m
                      (traced-body-inertia b))))

(display "traced-body-protocols.ss loaded.\n")
(display "  TracedBody now works with body-pos-lens, body-vel-lens, body-mass-lens\n")
