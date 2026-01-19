;;; boundary/reactive/README.sexp — Reactive Derivations from Optic Dependencies
;;;
;;; Automatic reactivity by tracking optic access paths as dependencies.
;;; When an optic's target changes, derived values automatically invalidate.

(module reactive
  (path "boundary/reactive")
  (layer shell)

  (description
   "Reactive state management via optic-based dependency tracking.

Optics define data dependencies. When a derivation is computed, we track
which optics were accessed. When any of those optics are written to,
the derivation is marked stale and recomputes on next access.

This is the pattern behind lens-based state management (MobX, Recoil, Jotai).
The optic graph becomes a dependency graph automatically.

Key insight: Because optics are composable and registered by name,
we can build a reactive system where:
  1. Dependencies are discovered automatically during computation
  2. Invalidation is precise (only affected derivations recompute)
  3. Computation is lazy (recompute only when accessed)

Integrates with the provenance system for optic name registration.")

  (modules
   ("reactive.ss"
    "Core reactive derivations: define-reactive, reactive-value, invalidation"))

  (usage
   ";;; Define a reactive derivation
    (define-reactive 'player-health
      world-state
      (lambda (world)
        (reactive-view world (>>> (body-lens 'player) health-lens))))

    ;; Get value (computed lazily, cached until stale)
    (reactive-value 'player-health)  ; => 100

    ;; Modify through optic - derivation auto-invalidates
    (set! world-state
      (reactive-set! (>>> (body-lens 'player) health-lens) 80 world-state))

    ;; Next access recomputes with new source
    (reactive-update-source! 'player-health world-state)
    (reactive-value 'player-health)  ; => 80

    ;;; Batch multiple changes without intermediate recomputation
    (with-batch
     (lambda ()
       (reactive-set! lens-fst 10 data)
       (reactive-set! lens-snd 20 data)))

    ;;; Introspect the dependency graph
    (reactive-dependencies 'player-health)  ; => (lens-fst body-lens ...)
    (dependency-graph)  ; => ((lens-fst . (deriv-a deriv-b)) ...)")

  (concepts
   ((derivation
     "A cached computed value that tracks its optic dependencies.
Created with define-reactive, accessed with reactive-value.")
    (access-tracking
     "During computation, we log which optics are accessed.
This builds the dependency list automatically.")
    (invalidation
     "When reactive-set! or reactive-over! is called on an optic,
all derivations depending on that optic are marked stale.")
    (lazy-recomputation
     "Derivations only recompute when accessed, not immediately
when invalidated. This avoids unnecessary work.")))

  (exports
   ;; Derivation Management
   (define-reactive reactive-value reactive-refresh!)
   (reactive-stale? reactive-dependencies undefine-reactive)

   ;; Access Tracking
   (with-access-tracking log-optic-access!)

   ;; Unregistered Optic Configuration (fold-zxpe)
   (*warn-unregistered-optics?* *strict-optic-registration?*)
   (reset-optic-warnings! handle-unregistered-optic!)

   ;; Invalidation
   (invalidate-optic! invalidate-optics!)

   ;; Reactive Optic Operations
   (reactive-view reactive-preview reactive-to-list)
   (reactive-set! reactive-over!)

   ;; Source Updates
   (reactive-update-source!)

   ;; Batch Operations
   (with-batch)

   ;; Introspection
   (list-derivations derivation-info dependency-graph))

  (dependencies
   ((boundary/provenance/traced-optics "Optic registry and traced operations")
    (lattice/fp/optics "The optics tower being tracked")))

  (limitations
   ((not-thread-safe
     "Uses global mutable state without synchronization.
Concurrent access from multiple threads will corrupt the dependency graph.
This is acceptable for single-threaded shell use; do not share derivations
across threads.")
    (named-optics-only
     "Reactivity only works with optics registered in the provenance system.
Anonymous optics (those without a name in the optic registry) will not
trigger dependency tracking or invalidation.

As of fold-zxpe fix: By default, using unregistered optics emits a warning
to help catch missing registrations. Configure this behavior with:
  - *warn-unregistered-optics?* (#t by default - emit warnings)
  - *strict-optic-registration?* (#f by default - set #t to raise errors)
  - (reset-optic-warnings!) to clear the warning cache")
    (no-derivation-composition
     "Derivations cannot depend on other derivations directly.
If derivation A calls (reactive-value 'B), A will not be invalidated
when B updates. Derivations only track optic dependencies, not
inter-derivation dependencies.")))

  (related
   (boundary/provenance "Provenance tracking (similar audit trail pattern)")
   (lattice/fp/optics "The optics system this builds on")))
