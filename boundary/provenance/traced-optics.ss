;;; boundary/provenance/traced-optics.ss — Traced Optic Operations
;;;
;;; Wrappers around standard optic operations that record provenance.
;;; Each traced operation performs the normal optic operation AND
;;; logs a provenance record to the CAS.
;;;
;;; Usage:
;;;   ;; Instead of (^. data lens-fst)
;;;   (traced-view data lens-fst)
;;;
;;;   ;; Instead of (& data (.~ lens-fst 42))
;;;   (traced-set lens-fst 42 data)
;;;
;;; The provenance record captures:
;;;   - Source structure (hashed and stored)
;;;   - Result (hashed and stored)
;;;   - The optic used (by registered name if available)
;;;   - Timestamp and identity
;;;
;;; This is Shell code: depends on provenance.ss for recording.
;;;
;;; Dependencies:
;;;   - lattice/fp/optics/optics.ss (or require 'optics)
;;;   - boundary/provenance/provenance.ss

(load "lattice/fp/optics/optics.ss")
(load "boundary/provenance/provenance.ss")

;;; ============================================================
;;; Traced View Operations
;;; ============================================================

;;; traced-view : s Optic -> a
;;; View through an optic with provenance tracking.
;;; Equivalent to (^. s optic) but records the operation.
(define (traced-view s optic)
  (let ([result (^. s optic)])
    (record-optic-operation! 'view optic s result #f)
    result))

;;; traced-preview : s Optic -> Maybe a
;;; Preview through an optic with provenance tracking.
;;; Equivalent to (^? s optic) but records the operation.
(define (traced-preview s optic)
  (let ([result (^? s optic)])
    (record-optic-operation! 'preview optic s result #f)
    result))

;;; traced-to-list : s Optic -> (List a)
;;; Get all targets through an optic with provenance tracking.
;;; Equivalent to (^.. s optic) but records the operation.
(define (traced-to-list s optic)
  (let ([result (^.. s optic)])
    (record-optic-operation! 'to-list optic s result #f)
    result))

;;; ============================================================
;;; Traced Modification Operations
;;; ============================================================

;;; traced-set : Optic b s -> t
;;; Set through an optic with provenance tracking.
;;; Equivalent to (& s (.~ optic val)) but records the operation.
(define (traced-set optic val s)
  (let ([result (& s (.~ optic val))])
    (record-optic-operation! 'set optic s result val)
    result))

;;; traced-over : Optic (a -> b) s -> t
;;; Modify through an optic with provenance tracking.
;;; Equivalent to (& s (%~ optic f)) but records the operation.
;;; Note: The transformation function is not stored, only its name if named.
(define (traced-over optic f s)
  (let ([result (& s (%~ optic f))])
    ;; For 'over', we record the function symbolically if possible
    ;; The value-hash field stores a description
    (record-optic-operation! 'over optic s result
                             `(function . ,(or (procedure-name f) 'anonymous)))
    result))

;;; procedure-name : Procedure -> Symbol | #f
;;; Try to extract the name of a procedure.
(define (procedure-name proc)
  (guard (e [else #f])
    (let ([info (inspect/object proc)])
      (and info
           (let ([name (inspect/object-name info)])
             (and (symbol? name) name))))))

;;; ============================================================
;;; Traced Iso Operations
;;; ============================================================

;;; traced-iso-view : s Iso -> a
;;; Forward through an iso with provenance tracking.
(define (traced-iso-view s iso)
  (let ([result (iso-view iso s)])
    (record-optic-operation! 'iso-view iso s result #f)
    result))

;;; traced-iso-review : a Iso -> s
;;; Backward through an iso with provenance tracking.
(define (traced-iso-review a iso)
  (let ([result (iso-review iso a)])
    (record-optic-operation! 'iso-review iso a result #f)
    result))

;;; traced-iso-over : Iso (a -> b) s -> t
;;; Modify through an iso with provenance tracking.
(define (traced-iso-over iso f s)
  (let ([result (iso-over iso f s)])
    (record-optic-operation! 'iso-over iso s result
                             `(function . ,(or (procedure-name f) 'anonymous)))
    result))

;;; ============================================================
;;; Traced Lens Operations
;;; ============================================================

;;; traced-lens-view : s Lens -> a
;;; View through a lens with provenance tracking.
(define (traced-lens-view s lens)
  (let ([result (view lens s)])
    (record-optic-operation! 'lens-view lens s result #f)
    result))

;;; traced-lens-set : Lens b s -> t
;;; Set through a lens with provenance tracking.
(define (traced-lens-set lens val s)
  (let ([result (set-lens lens val s)])
    (record-optic-operation! 'lens-set lens s result val)
    result))

;;; traced-lens-over : Lens (a -> b) s -> t
;;; Modify through a lens with provenance tracking.
(define (traced-lens-over lens f s)
  (let ([result (over lens f s)])
    (record-optic-operation! 'lens-over lens s result
                             `(function . ,(or (procedure-name f) 'anonymous)))
    result))

;;; ============================================================
;;; Traced Prism Operations
;;; ============================================================

;;; traced-prism-preview : s Prism -> Maybe a
;;; Preview through a prism with provenance tracking.
(define (traced-prism-preview s prism)
  (let ([result (preview prism s)])
    (record-optic-operation! 'prism-preview prism s result #f)
    result))

;;; traced-prism-review : b Prism -> t
;;; Review through a prism with provenance tracking.
(define (traced-prism-review b prism)
  (let ([result (review prism b)])
    (record-optic-operation! 'prism-review prism b result #f)
    result))

;;; traced-prism-set : Prism b s -> t
;;; Set through a prism with provenance tracking.
(define (traced-prism-set prism val s)
  (let ([result (prism-set prism val s)])
    (record-optic-operation! 'prism-set prism s result val)
    result))

;;; ============================================================
;;; Traced Traversal Operations
;;; ============================================================

;;; traced-traversal-to-list : s Traversal -> (List a)
;;; Get all targets from a traversal with provenance tracking.
(define (traced-traversal-to-list s trav)
  (let ([result (traversal-to-list trav s)])
    (record-optic-operation! 'traversal-to-list trav s result #f)
    result))

;;; traced-traversal-over : Traversal (a -> b) s -> t
;;; Modify all targets through a traversal with provenance tracking.
(define (traced-traversal-over trav f s)
  (let ([result (traversal-over trav f s)])
    (record-optic-operation! 'traversal-over trav s result
                             `(function . ,(or (procedure-name f) 'anonymous)))
    result))

;;; traced-traversal-set : Traversal b s -> t
;;; Set all targets through a traversal with provenance tracking.
(define (traced-traversal-set trav val s)
  (let ([result (traversal-set trav val s)])
    (record-optic-operation! 'traversal-set trav s result val)
    result))

;;; ============================================================
;;; Traced Affine Operations
;;; ============================================================

;;; traced-affine-preview : s Affine -> Maybe a
;;; Preview through an affine with provenance tracking.
(define (traced-affine-preview s affine)
  (let ([result (affine-preview affine s)])
    (record-optic-operation! 'affine-preview affine s result #f)
    result))

;;; traced-affine-set : Affine b s -> t
;;; Set through an affine with provenance tracking.
(define (traced-affine-set affine val s)
  (let ([result (affine-set affine val s)])
    (record-optic-operation! 'affine-set affine s result val)
    result))

;;; traced-affine-over : Affine (a -> b) s -> t
;;; Modify through an affine with provenance tracking.
(define (traced-affine-over affine f s)
  (let ([result (affine-over affine f s)])
    (record-optic-operation! 'affine-over affine s result
                             `(function . ,(or (procedure-name f) 'anonymous)))
    result))

;;; ============================================================
;;; Traced Composition
;;; ============================================================

;;; traced->>> : Optic Optic -> Optic
;;; Compose optics with provenance awareness.
;;; The composed optic is automatically registered if both inputs are registered.
(define (traced->>> outer inner)
  (let* ([composed (>>> outer inner)]
         [outer-name (lookup-optic-name outer)]
         [inner-name (lookup-optic-name inner)])
    ;; Auto-register composed optics if both parts are named
    (when (and outer-name inner-name)
      (let ([composed-name (string->symbol
                            (string-append (symbol->string outer-name)
                                           ">>>"
                                           (symbol->string inner-name)))])
        (register-optic! composed-name composed)))
    composed))

;;; ============================================================
;;; Batch Tracing
;;; ============================================================

;;; with-tracing : (-> a) -> a
;;; Execute a thunk with provenance tracking enabled.
;;; Returns the result of the thunk.
(define (with-tracing thunk)
  (let ([was-enabled? *provenance-enabled?*])
    (dynamic-wind
      (lambda () (set! *provenance-enabled?* #t))
      thunk
      (lambda () (set! *provenance-enabled?* was-enabled?)))))

;;; without-tracing : (-> a) -> a
;;; Execute a thunk with provenance tracking disabled.
;;; Useful for performance-critical sections or avoiding redundant records.
(define (without-tracing thunk)
  (let ([was-enabled? *provenance-enabled?*])
    (dynamic-wind
      (lambda () (set! *provenance-enabled?* #f))
      thunk
      (lambda () (set! *provenance-enabled?* was-enabled?)))))

;;; ============================================================
;;; Standard Optic Registration
;;; ============================================================
;;;
;;; Register commonly used optics so they appear with names in provenance.

(register-optic! 'lens-fst lens-fst)
(register-optic! 'lens-snd lens-snd)
(register-optic! 'lens-head lens-head)
(register-optic! 'lens-tail lens-tail)
(register-optic! 'lens-id lens-id)

(register-optic! 'prism-just prism-just)
(register-optic! 'prism-left prism-left)
(register-optic! 'prism-right prism-right)
(register-optic! 'prism-cons prism-cons)
(register-optic! 'prism-nil prism-nil)
(register-optic! 'prism-id prism-id)

(register-optic! 'traversal-each traversal-each)
(register-optic! 'traversal-both traversal-both)
(register-optic! 'traversal-left traversal-left)
(register-optic! 'traversal-right traversal-right)
(register-optic! 'traversal-just traversal-just)

(register-optic! 'iso-id iso-id)
(register-optic! 'iso-swapped iso-swapped)
(register-optic! 'iso-reversed iso-reversed)
(register-optic! 'iso-curried iso-curried)
(register-optic! 'iso-flipped iso-flipped)

(register-optic! 'affine-id affine-id)

(register-optic! 'getter-id getter-id)
(register-optic! 'getter-fst getter-fst)
(register-optic! 'getter-snd getter-snd)

(register-optic! 'fold-each fold-each)

(register-optic! 'setter-mapped setter-mapped)

(register-optic! 'grate-id grate-id)
(register-optic! 'grate-fn grate-fn)
(register-optic! 'grate-pair-same grate-pair-same)

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Traced View Operations:
;;;   traced-view, traced-preview, traced-to-list
;;;
;;; Traced Modification Operations:
;;;   traced-set, traced-over
;;;
;;; Traced Iso Operations:
;;;   traced-iso-view, traced-iso-review, traced-iso-over
;;;
;;; Traced Lens Operations:
;;;   traced-lens-view, traced-lens-set, traced-lens-over
;;;
;;; Traced Prism Operations:
;;;   traced-prism-preview, traced-prism-review, traced-prism-set
;;;
;;; Traced Traversal Operations:
;;;   traced-traversal-to-list, traced-traversal-over, traced-traversal-set
;;;
;;; Traced Affine Operations:
;;;   traced-affine-preview, traced-affine-set, traced-affine-over
;;;
;;; Composition:
;;;   traced->>>
;;;
;;; Scoping:
;;;   with-tracing, without-tracing
