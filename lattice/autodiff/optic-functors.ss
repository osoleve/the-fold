(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module optic-functors
;;; @requires prelude reverse-diff traced-optics functor-general hamt
(require 'prelude)
(require 'reverse-diff)
(require 'traced-optics)
(require 'functor-general)
(require 'hamt)

(doc 'module 'optic-functors)
(doc 'description "Categorical bridge from optics tower to autodiff.

Each optic type defines a DiffFunctor: a structure-preserving map from
the category of types to the category of differentiable computations.

  Lens s a       → focus-one     (exactly one target, gradient = scalar/vec)
  Prism s a      → focus-maybe   (zero or one target, gradient = Maybe)
  Affine s a     → focus-maybe   (zero or one target)
  Traversal s a  → focus-many    (zero or more targets, independent gradients)
  Iso s a        → focus-one     (bijective, trivial routing)
  Grate s a      → focus-one     (cotraversable, modify-in-place differentiation)

The bridge replaces the hand-wired cond dispatch in traced-optics.ss with
an extensible, table-driven architecture. New optic types can register their
AD behavior without modifying existing code.

Composition is functorial: diff-optic-compose composes the underlying optics
AND the gradient routing, preserving the chain rule.

Provides a bridge to the categorical functor framework (functor-general.ss):
  (diff-optic->functor dop)  → FunctorGeneral on cat-Diff")
(doc 'layer 'lattice)
(doc 'purity 'mixed)

;;; ============================================================
;;; Diff Category
;;; ============================================================

(doc 'section 'diff-category)

(doc 'description "Category of differentiable types. Objects are Scheme types
that support tracing (numbers, vec2, pairs, lists). Morphisms are functions
that preserve differentiable structure.")
(doc cat-Diff 'export #t)
(define cat-Diff
  (make-category
   'Diff
   (lambda (_) #t)          ; All values are objects
   procedure?               ; Morphisms are procedures
   (lambda (f) 'any)        ; Domain (untracked)
   (lambda (f) 'any)        ; Codomain (untracked)
   (lambda (a) identity)    ; Identity morphism
   (lambda (g f)             ; Composition
     (lambda (x) (g (f x))))))

;;; ============================================================
;;; DiffOptic Type
;;; ============================================================

(doc 'section 'diff-optic)

(doc 'type '(Record diff-optic kind optic focus-type))
(doc 'description "A DiffOptic pairs an optic with its categorical AD metadata.
kind: symbol naming the optic type (iso, lens, prism, affine, traversal, grate)
optic: the underlying optic value
focus-type: how the focus presents to AD (one, maybe, many)")

(define (make-diff-optic optic)
  (doc 'export #t)
  (doc 'type '(-> Optic DiffOptic))
  (doc 'description "Wrap an optic as a DiffOptic with auto-detected kind.
Validates that the optic type supports differentiation (getter/fold/setter rejected).")
  (let ([kind (optic-type optic)])
    (case kind
      [(iso lens prism affine traversal grate)
       (list 'diff-optic kind optic (kind->focus-type kind))]
      [(getter)
       (error 'make-diff-optic
              "Cannot differentiate through Getter (read-only, no set for backprop)")]
      [(fold)
       (error 'make-diff-optic
              "Cannot differentiate through Fold (read-only, no set for backprop)")]
      [(setter)
       (error 'make-diff-optic
              "Cannot differentiate through Setter (write-only, no get for forward pass)")]
      [else
       (error 'make-diff-optic "Unknown optic type" kind)])))

(define (diff-optic? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'diff-optic)))

(define (diff-optic-kind dop) (list-ref dop 1))
(define (diff-optic-optic dop) (list-ref dop 2))
(define (diff-optic-focus-type dop) (list-ref dop 3))

(define (kind->focus-type kind)
  (case kind
    [(iso lens grate) 'one]
    [(prism affine) 'maybe]
    [(traversal) 'many]
    [else (error 'kind->focus-type "unknown kind" kind)]))

;;; ============================================================
;;; Lift/Extract Registry
;;; ============================================================

(doc 'section 'registry)

(doc 'description "Extensible registry mapping optic kinds to their AD behavior.
Each entry is a triple (lift-fn, extract-fn, update-fn):
  lift-fn    : (optic, s, tape) → (values s-traced focus)
  extract-fn : (focus, grads) → gradient
  update-fn  : (optic, s, gradient, rate) → s'")

(define *diff-kind-registry* hamt-empty)

(define (register-diff-kind! kind lift-fn extract-fn update-fn)
  (doc 'export #t)
  (doc 'type '(-> Symbol (-> Optic Any Tape (Values Any Any))
                         (-> Any GradTable Any)
                         (-> Optic Any Any Number Any)
                         Void))
  (doc 'description "Register AD behavior for an optic kind.")
  (set! *diff-kind-registry*
        (hamt-assoc kind (list lift-fn extract-fn update-fn) *diff-kind-registry*)))

(define (lookup-diff-kind kind)
  (or (hamt-lookup kind *diff-kind-registry*)
      (error 'lookup-diff-kind "No AD behavior registered for optic kind" kind)))

(define (diff-kind-lift kind) (car (lookup-diff-kind kind)))
(define (diff-kind-extract kind) (cadr (lookup-diff-kind kind)))
(define (diff-kind-update kind) (caddr (lookup-diff-kind kind)))

;;; ============================================================
;;; Built-in Kind Registrations
;;; ============================================================

(doc 'section 'built-in-kinds)

;;; --- Iso: bijective, exactly one focus ---
;;; Note: iso-forward/iso-backward return functions; iso-view/iso-review apply them.
(register-diff-kind! 'iso
  ;; lift
  (lambda (optic s tape)
    (let* ([focus (iso-view optic s)]
           [traced-focus (trace-value focus tape)]
           [s-traced (iso-review optic traced-focus)])
      (values s-traced traced-focus)))
  ;; extract
  (lambda (focus grads) (extract-gradient focus grads))
  ;; update
  (lambda (optic s grad rate)
    (let ([focus (iso-view optic s)])
      (iso-review optic (subtract-scaled focus grad rate)))))

;;; --- Lens: exactly one focus ---
(register-diff-kind! 'lens
  ;; lift
  (lambda (optic s tape)
    (let* ([focus (view optic s)]
           [traced-focus (trace-value focus tape)]
           [s-traced (set-lens optic traced-focus s)])
      (values s-traced traced-focus)))
  ;; extract
  (lambda (focus grads) (extract-gradient focus grads))
  ;; update
  (lambda (optic s grad rate)
    (let ([focus (view optic s)])
      (set-lens optic (subtract-scaled focus grad rate) s))))

;;; --- Prism: zero or one focus ---
(register-diff-kind! 'prism
  ;; lift
  (lambda (optic s tape)
    (let ([result (preview optic s)])
      (if (nothing? result)
          (values s nothing)
          (let* ([focus (from-just result)]
                 [traced-focus (trace-value focus tape)]
                 [s-traced (review optic traced-focus)])
            (values s-traced (just traced-focus))))))
  ;; extract
  (lambda (focus grads)
    (if (nothing? focus) nothing
        (just (extract-gradient (from-just focus) grads))))
  ;; update
  (lambda (optic s grad rate)
    (let ([result (preview optic s)])
      (if (nothing? result) s
          (let ([focus (from-just result)])
            (review optic (subtract-scaled focus (from-just grad) rate)))))))

;;; --- Affine: zero or one focus (with set, unlike prism) ---
(register-diff-kind! 'affine
  ;; lift
  (lambda (optic s tape)
    (let ([result (preview optic s)])
      (if (nothing? result)
          (values s nothing)
          (let* ([focus (from-just result)]
                 [traced-focus (trace-value focus tape)]
                 [s-traced (affine-set optic traced-focus s)])
            (values s-traced (just traced-focus))))))
  ;; extract
  (lambda (focus grads)
    (if (nothing? focus) nothing
        (just (extract-gradient (from-just focus) grads))))
  ;; update
  (lambda (optic s grad rate)
    (let ([result (preview optic s)])
      (if (nothing? result) s
          (let ([focus (from-just result)])
            (affine-set optic (subtract-scaled focus (from-just grad) rate) s))))))

;;; --- Traversal: zero or more focuses ---
(register-diff-kind! 'traversal
  ;; lift
  (lambda (optic s tape)
    (let* ([focuses (traversal-to-list optic s)]
           [traced-focuses (map (lambda (f) (trace-value f tape)) focuses)])
      (if (and (list? s) (eq? optic traversal-each))
          (values traced-focuses traced-focuses)
          (let ([queue traced-focuses])
            (values
             (traversal-over optic
                             (lambda (_)
                               (let ([v (car queue)])
                                 (set! queue (cdr queue))
                                 v))
                             s)
             traced-focuses)))))
  ;; extract
  (lambda (focus grads)
    (map (lambda (tf) (extract-gradient tf grads)) focus))
  ;; update
  (lambda (optic s grad rate)
    (let* ([focuses (traversal-to-list optic s)]
           [new-vals (map (lambda (v g) (subtract-scaled v g rate)) focuses grad)])
      (if (and (list? s) (eq? optic traversal-each))
          new-vals
          (let ([queue new-vals])
            (traversal-over optic
                            (lambda (_)
                              (let ([v (car queue)])
                                (set! queue (cdr queue))
                                v))
                            s))))))

;;; --- Grate: modify-in-place via cotraversal ---
;;; The grate's cotraverse applies a function "inside" the structure.
;;; For AD: we use grate-over to trace the focus, then extract gradient.
(register-diff-kind! 'grate
  ;; lift
  (lambda (optic s tape)
    (let ([focus-traced #f])
      (let ([s-traced (grate-over optic
                                  (lambda (a)
                                    (let ([a-tr (trace-value a tape)])
                                      (set! focus-traced a-tr)
                                      a-tr))
                                  s)])
        (values s-traced focus-traced))))
  ;; extract
  (lambda (focus grads) (extract-gradient focus grads))
  ;; update
  (lambda (optic s grad rate)
    (grate-over optic
                (lambda (a) (subtract-scaled a grad rate))
                s)))

;;; ============================================================
;;; Generic Gradient Computation
;;; ============================================================

(doc 'section 'gradient)

(doc 'type '(-> (-> s-traced Traced) DiffOptic s gradient))
(doc 'description "Compute gradient of loss function with respect to the
diff-optic's focus. Dispatches via the registry rather than cond.")
(define (diff-optic-gradient loss-fn dop s)
  (doc 'export #t)
  (let ([kind (diff-optic-kind dop)]
        [optic (diff-optic-optic dop)])
    (with-fresh-ad-scope
     (lambda ()
       (let ([tape (make-reverse-tape)])
         (let-values ([(s-traced traced-focus)
                       ((diff-kind-lift kind) optic s tape)])
           (let ([loss (loss-fn s-traced)])
             (if (traced? loss)
                 (let ([grads (backward tape (traced-id loss) 1)])
                   ((diff-kind-extract kind) traced-focus grads))
                 (zero-like traced-focus)))))))))

(doc 'type '(-> (-> s-traced Traced) DiffOptic s (Values Number gradient)))
(doc 'description "Compute both loss value and gradient in one forward+backward pass.")
(define (diff-optic-value-and-gradient loss-fn dop s)
  (doc 'export #t)
  (let ([kind (diff-optic-kind dop)]
        [optic (diff-optic-optic dop)])
    (with-fresh-ad-scope
     (lambda ()
       (let ([tape (make-reverse-tape)])
         (let-values ([(s-traced traced-focus)
                       ((diff-kind-lift kind) optic s tape)])
           (let ([loss (loss-fn s-traced)])
             (if (traced? loss)
                 (let ([val (traced-value loss)]
                       [grads (backward tape (traced-id loss) 1)])
                   (values val ((diff-kind-extract kind) traced-focus grads)))
                 (values loss (zero-like traced-focus))))))))))

;;; ============================================================
;;; Gradient Descent
;;; ============================================================

(doc 'section 'optimize)

(doc 'type '(-> (-> s-traced Traced) DiffOptic s Number s))
(doc 'description "Single gradient descent step: focus' = focus - rate * gradient.")
(define (diff-optic-optimize loss-fn dop s rate)
  (doc 'export #t)
  (let ([kind (diff-optic-kind dop)]
        [optic (diff-optic-optic dop)]
        [grad (diff-optic-gradient loss-fn dop s)])
    ((diff-kind-update kind) optic s grad rate)))

(doc 'type '(-> (-> s-traced Traced) DiffOptic s Number Nat s))
(doc 'description "Multiple gradient descent steps.")
(define (diff-optic-optimize-n loss-fn dop s rate n)
  (doc 'export #t)
  (if (<= n 0) s
      (diff-optic-optimize-n loss-fn dop
                             (diff-optic-optimize loss-fn dop s rate)
                             rate (- n 1))))

;;; ============================================================
;;; Functorial Composition
;;; ============================================================

(doc 'section 'composition)

(doc 'type '(-> DiffOptic DiffOptic DiffOptic))
(doc 'description "Compose two diff-optics. Uses the optics tower's >>> for the
underlying optic (which handles type downgrading: lens+prism→affine, etc.),
and auto-detects the resulting kind for AD dispatch.")
(define (diff-optic-compose outer inner)
  (doc 'export #t)
  (let ([composed-optic (>>> (diff-optic-optic outer) (diff-optic-optic inner))])
    (make-diff-optic composed-optic)))

;;; ============================================================
;;; Chain Rule Verification
;;; ============================================================

(doc 'section 'chain-rule)

(doc 'type '(-> DiffOptic DiffOptic s (-> s-traced Traced) Number Boolean))
(doc 'description "Verify the chain rule: gradient through composed optics equals
gradient through the composition. Compares:
  (1) diff-optic-gradient loss (diff-optic-compose outer inner) s
  (2) diff-optic-gradient loss (>>> outer inner) s  [via traced-optics]
Returns #t if gradients agree within epsilon tolerance.")
(define (verify-chain-rule outer inner s loss-fn epsilon)
  (doc 'export #t)
  (let* ([composed (diff-optic-compose outer inner)]
         [grad-composed (diff-optic-gradient loss-fn composed s)]
         [grad-direct (optic-gradient loss-fn
                                      (>>> (diff-optic-optic outer)
                                           (diff-optic-optic inner))
                                      s)])
    (gradients-close? grad-composed grad-direct epsilon)))

(doc 'type '(-> DiffOptic s (-> s-traced Traced) Number Boolean))
(doc 'description "Verify that the registry-dispatched gradient matches
the hand-wired traced-optics.ss gradient within epsilon tolerance.")
(define (verify-registry-agreement dop s loss-fn epsilon)
  (doc 'export #t)
  (let* ([grad-registry (diff-optic-gradient loss-fn dop s)]
         [grad-handwired (optic-gradient loss-fn (diff-optic-optic dop) s)])
    (gradients-close? grad-registry grad-handwired epsilon)))

;;; gradients-close? : gradient × gradient × Number → Boolean
;;; Compare gradients structurally, allowing epsilon tolerance on numbers.
(define (gradients-close? a b eps)
  (cond
    [(and (number? a) (number? b))
     (< (abs (- a b)) eps)]
    [(and (vec2? a) (vec2? b))
     (and (< (abs (- (vec2-x a) (vec2-x b))) eps)
          (< (abs (- (vec2-y a) (vec2-y b))) eps))]
    [(and (just? a) (just? b))
     (gradients-close? (from-just a) (from-just b) eps)]
    [(and (nothing? a) (nothing? b)) #t]
    [(and (list? a) (list? b) (= (length a) (length b)))
     (let loop ([xs a] [ys b])
       (or (null? xs)
           (and (gradients-close? (car xs) (car ys) eps)
                (loop (cdr xs) (cdr ys)))))]
    [(and (pair? a) (pair? b))
     (and (gradients-close? (car a) (car b) eps)
          (gradients-close? (cdr a) (cdr b) eps))]
    [else (equal? a b)]))

;;; ============================================================
;;; Bridge to Categorical Functor Framework
;;; ============================================================

(doc 'section 'categorical-bridge)

(doc 'type '(-> DiffOptic FunctorGeneral))
(doc 'description "Express a DiffOptic as a FunctorGeneral on cat-Diff.

The functor F_l associated with an optic l sends:
  - Objects: F_l(s) = a  (the focused type)
  - Morphisms: F_l(f : a → a) = over(l, f) : s → s

This is the 'modify functor' — applying a morphism at the focus.
For a lens, this is exactly the lens-over operation.
For a traversal, it maps f over each target independently.

Functorial properties:
  F_l(id) = id                    (modifying with identity = identity)
  F_l(g ∘ f) = F_l(g) ∘ F_l(f)   (modifying with composition = composing modifications)

These are exactly the optic laws in functorial form.")
(define (diff-optic->functor dop)
  (doc 'export #t)
  (let ([optic (diff-optic-optic dop)]
        [kind (diff-optic-kind dop)])
    (make-functor-general
     (string->symbol (string-append "F-" (symbol->string kind)))
     cat-Diff
     cat-Diff
     ;; on-obj: F(s) = s (optic acts within the same type space)
     identity
     ;; on-mor: F(f) = over(optic, f) — apply f at the optic's focus
     (lambda (f)
       (lambda (s)
         (case kind
           [(lens) (set-lens optic (f (view optic s)) s)]
           [(iso) (iso-review optic (f (iso-view optic s)))]
           [(prism)
            (let ([result (preview optic s)])
              (if (nothing? result) s
                  (review optic (f (from-just result)))))]
           [(affine)
            (let ([result (preview optic s)])
              (if (nothing? result) s
                  (affine-set optic (f (from-just result)) s)))]
           [(traversal) (traversal-over optic f s)]
           [(grate) (grate-over optic f s)]))))))

(doc 'type '(-> DiffOptic (List s) Boolean))
(doc 'description "Verify functor laws for a DiffOptic:
  Identity: F(id)(s) = s
  Composition: F(g ∘ f)(s) = F(g)(F(f)(s))")
(define (verify-diff-functor-laws dop test-values)
  (doc 'export #t)
  (let* ([F (diff-optic->functor dop)]
         [F-mor (functor-general-on-mor F)]
         [f (lambda (x) (if (number? x) (+ x 1) x))]
         [g (lambda (x) (if (number? x) (* x 2) x))])
    (andmap
     (lambda (s)
       (and
        ;; Identity: F(id)(s) = s
        (equal? ((F-mor identity) s) s)
        ;; Composition: F(g ∘ f)(s) = F(g)(F(f)(s))
        (equal? ((F-mor (lambda (x) (g (f x)))) s)
                ((F-mor g) ((F-mor f) s)))))
     test-values)))

;;; ============================================================
;;; Convenience: Natural Transformation Between DiffOptics
;;; ============================================================

(doc 'section 'natural-transforms)

(doc 'type '(-> DiffOptic DiffOptic (-> a b) NatIndexed))
(doc 'description "Construct a natural transformation α : F_{dop1} → F_{dop2}
from a function converting between their focus types.

If dop1 : DiffOptic s a and dop2 : DiffOptic s b, and convert : a → b,
then α_s : F_{dop1}(s) → F_{dop2}(s) maps modifications at the first
optic's focus to modifications at the second's.

This is useful when two optics view 'the same data differently' — e.g.,
a lens on a pair vs an iso that reorders the pair.")
(define (diff-optic-transform dop1 dop2 convert-fn)
  (doc 'export #t)
  (let ([F1 (diff-optic->functor dop1)]
        [F2 (diff-optic->functor dop2)])
    (make-nat-indexed
     'diff-optic-transform
     F1 F2
     ;; component-at: for any object s, produce the component α_s
     (lambda (s)
       (lambda (f-result)
         ;; Apply convert-fn to transform the focus
         convert-fn)))))
