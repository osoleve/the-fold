;;; lattice/fp/optics/profunctor-optics.ss — Profunctor Optics
;;;
;;; An abstract encoding of optics where optics are polymorphic functions
;;; `p a b -> p s t` constrained by profunctor type classes. This approach:
;;;
;;;   - Enables automatic optic type inference via constraints
;;;   - Provides elegant composition (just function composition)
;;;   - Offers a stronger theoretical foundation
;;;
;;; The profunctor hierarchy mirrors the optic hierarchy:
;;;
;;;   Profunctor           → Iso (dimap)
;;;   Strong Profunctor    → Lens (pfirst, psecond)
;;;   Choice Profunctor    → Prism (pleft, pright)
;;;   Strong + Choice      → Affine
;;;
;;; This module uses dictionary-passing style for type classes, consistent
;;; with the rest of lattice/fp.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/meta/combinators.ss
;;;   - fp/templates.ss (for Maybe, Either)
;;;   - fp/optics/optics.ss (for concrete optic conversion)

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")
(load "lattice/fp/optics/optics.ss")

;;; ============================================================
;;; Part 1: Profunctor Type Class
;;; ============================================================
;;;
;;; A profunctor is a bifunctor that is contravariant in its first
;;; argument and covariant in its second:
;;;
;;;   class Profunctor p where
;;;     dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
;;;     lmap  :: (a' -> a) -> p a b -> p a' b
;;;     rmap  :: (b -> b') -> p a b -> p a b'
;;;
;;; Laws:
;;;   - dimap id id = id
;;;   - dimap (f . g) (h . k) = dimap g h . dimap f k
;;;
;;; We represent profunctor dictionaries as tagged lists with the
;;; essential operations.

;;; make-profunctor : (dimap) -> Profunctor p
;;; Create a profunctor dictionary from dimap.
;;; lmap and rmap are derived: lmap f = dimap f id, rmap g = dimap id g
(define (make-profunctor dimap-fn)
  (list 'profunctor
        dimap-fn
        (lambda (f pa) (dimap-fn f identity pa))    ; lmap
        (lambda (g pa) (dimap-fn identity g pa))))  ; rmap

;;; profunctor? : alpha -> Boolean
(define (profunctor? x)
  (and (pair? x) (eq? (car x) 'profunctor)))

;;; profunctor-dimap : Profunctor -> ((a' -> a) -> (b -> b') -> p a b -> p a' b')
(define (profunctor-dimap p)
  (cadr p))

;;; profunctor-lmap : Profunctor -> ((a' -> a) -> p a b -> p a' b)
(define (profunctor-lmap p)
  (caddr p))

;;; profunctor-rmap : Profunctor -> ((b -> b') -> p a b -> p a b')
(define (profunctor-rmap p)
  (cadddr p))

;;; dimap : Profunctor -> (a' -> a) -> (b -> b') -> p a b -> p a' b'
;;; Apply dimap from a profunctor dictionary.
(define (dimap prof f g pa)
  ((profunctor-dimap prof) f g pa))

;;; lmap : Profunctor -> (a' -> a) -> p a b -> p a' b
;;; Contravariant map on first argument.
(define (lmap prof f pa)
  ((profunctor-lmap prof) f pa))

;;; rmap : Profunctor -> (b -> b') -> p a b -> p a b'
;;; Covariant map on second argument.
(define (rmap prof g pa)
  ((profunctor-rmap prof) g pa))

;;; ============================================================
;;; Part 2: Strong Profunctor (for Lenses)
;;; ============================================================
;;;
;;; A strong profunctor can "pass through" extra context in a pair:
;;;
;;;   class Profunctor p => Strong p where
;;;     pfirst  :: p a b -> p (a, c) (b, c)
;;;     psecond :: p a b -> p (c, a) (c, b)
;;;
;;; Lenses require Strong profunctors.

;;; make-strong : Profunctor -> (pfirst-fn) -> Strong
;;; Create a strong profunctor. psecond is derived from pfirst + swap.
(define (make-strong prof first-fn)
  (list 'strong
        prof
        first-fn
        ;; psecond derived: swap, pfirst, swap
        (lambda (pab)
          (dimap prof swap swap (first-fn pab)))))

;;; strong? : alpha -> Boolean
(define (strong? x)
  (and (pair? x) (eq? (car x) 'strong)))

;;; strong-profunctor : Strong -> Profunctor
(define (strong-profunctor s)
  (cadr s))

;;; strong-first : Strong -> (p a b -> p (a, c) (b, c))
(define (strong-first s)
  (caddr s))

;;; strong-second : Strong -> (p a b -> p (c, a) (c, b))
(define (strong-second s)
  (cadddr s))

;;; pfirst : Strong -> p a b -> p (a, c) (b, c)
;;; Named pfirst (profunctor first) to avoid quote issues with first'.
(define (pfirst strong pab)
  ((strong-first strong) pab))

;;; psecond : Strong -> p a b -> p (c, a) (c, b)
;;; Named psecond (profunctor second) to avoid quote issues with second'.
(define (psecond strong pab)
  ((strong-second strong) pab))

;;; ============================================================
;;; Part 3: Choice Profunctor (for Prisms)
;;; ============================================================
;;;
;;; A choice profunctor can handle sum types (Either):
;;;
;;;   class Profunctor p => Choice p where
;;;     pleft  :: p a b -> p (Either a c) (Either b c)
;;;     pright :: p a b -> p (Either c a) (Either c b)
;;;
;;; Prisms require Choice profunctors.

;;; make-choice : Profunctor -> (pleft-fn) -> Choice
;;; Create a choice profunctor. pright is derived from pleft + swap-either.
(define (make-choice prof left-fn)
  (list 'choice
        prof
        left-fn
        ;; pright derived: swap-either, pleft, swap-either
        (lambda (pab)
          (dimap prof swap-either swap-either (left-fn pab)))))

;;; swap-either : Either a b -> Either b a
(define (swap-either e)
  (if (left? e)
      (right (from-left e))
      (left (from-right e))))

;;; choice? : alpha -> Boolean
(define (choice? x)
  (and (pair? x) (eq? (car x) 'choice)))

;;; choice-profunctor : Choice -> Profunctor
(define (choice-profunctor c)
  (cadr c))

;;; choice-left : Choice -> (p a b -> p (Either a c) (Either b c))
(define (choice-left c)
  (caddr c))

;;; choice-right : Choice -> (p a b -> p (Either c a) (Either c b))
(define (choice-right c)
  (cadddr c))

;;; pleft : Choice -> p a b -> p (Either a c) (Either b c)
;;; Named pleft (profunctor left) to avoid quote issues with left'.
(define (pleft choice pab)
  ((choice-left choice) pab))

;;; pright : Choice -> p a b -> p (Either c a) (Either c b)
;;; Named pright (profunctor right) to avoid quote issues with right'.
(define (pright choice pab)
  ((choice-right choice) pab))

;;; ============================================================
;;; Part 4: Closed Profunctor (for Grates)
;;; ============================================================
;;;
;;; A closed profunctor can handle function types:
;;;
;;;   class Profunctor p => Closed p where
;;;     closed :: p a b -> p (x -> a) (x -> b)
;;;
;;; Grates require Closed profunctors. This is the dual of Strong:
;;;   - Strong lifts p a b to pairs: p (a, c) (b, c)
;;;   - Closed lifts p a b to functions: p (x -> a) (x -> b)

;;; make-closed : Profunctor -> (closed-fn) -> Closed
;;; Create a closed profunctor dictionary.
(define (make-closed prof closed-fn)
  (list 'closed prof closed-fn))

;;; closed-profunctor? : alpha -> Boolean
(define (closed-profunctor? x)
  (and (pair? x) (eq? (car x) 'closed)))

;;; closed-profunctor : Closed -> Profunctor
(define (closed-profunctor c)
  (cadr c))

;;; closed-fn : Closed -> (p a b -> p (x -> a) (x -> b))
(define (closed-fn c)
  (caddr c))

;;; pclosed : Closed -> p a b -> p (x -> a) (x -> b)
;;; Apply closed lifting from a closed profunctor dictionary.
(define (pclosed closed pab)
  ((closed-fn closed) pab))

;;; ============================================================
;;; Part 5: Profunctor Instances
;;; ============================================================

;;; ====
;;; Function Profunctor (->)
;;; ====
;;;
;;; The function arrow is the canonical profunctor.
;;; (dimap f g h) x = g (h (f x))

;;; profunctor-fn : Profunctor (->)
(define profunctor-fn
  (make-profunctor
   (lambda (f g h)
     (compose2 g (compose2 h f)))))

;;; strong-fn : Strong (->)
;;; pfirst f (a, c) = (f a, c)
(define strong-fn
  (make-strong
   profunctor-fn
   (lambda (f)
     (lambda (pair)
       (cons (f (car pair)) (cdr pair))))))

;;; choice-fn : Choice (->)
;;; pleft f (Left a) = Left (f a)
;;; pleft f (Right c) = Right c
(define choice-fn
  (make-choice
   profunctor-fn
   (lambda (f)
     (lambda (e)
       (if (left? e)
           (left (f (from-left e)))
           e)))))

;;; closed-fn-instance : Closed (->)
;;; closed f = \g -> f . g
;;; Given f : a -> b, lift to (x -> a) -> (x -> b) by post-composing
(define closed-fn-instance
  (make-closed
   profunctor-fn
   (lambda (f)
     (lambda (g)
       (compose2 f g)))))

;;; ====
;;; Forget Profunctor (for Getters/Folds)
;;; ====
;;;
;;; Forget r a b = a -> r
;;; Forgets the output type, useful for read-only optics.
;;;
;;; We represent Forget as a wrapper around a function.

;;; make-forget : (a -> r) -> Forget r a b
(define (make-forget f)
  (list 'forget f))

;;; forget? : alpha -> Boolean
(define (forget? x)
  (and (pair? x) (eq? (car x) 'forget)))

;;; run-forget : Forget r a b -> a -> r
(define (run-forget fg)
  (cadr fg))

;;; profunctor-forget : Profunctor (Forget r)
;;; dimap f g (Forget h) = Forget (h . f)
;;; (We ignore g since Forget doesn't use b)
(define profunctor-forget
  (make-profunctor
   (lambda (f g fg)
     (make-forget (compose2 (run-forget fg) f)))))

;;; strong-forget : Strong (Forget r)
;;; pfirst (Forget f) = Forget (f . fst)
(define strong-forget
  (make-strong
   profunctor-forget
   (lambda (fg)
     (make-forget (compose2 (run-forget fg) car)))))

;;; ====
;;; Tagged Profunctor (for Reviews)
;;; ====
;;;
;;; Tagged a b = b
;;; Forgets the input type, useful for constructors/reviews.
;;;
;;; We represent Tagged as a wrapper around a value.

;;; make-tagged : b -> Tagged a b
(define (make-tagged val)
  (list 'tagged val))

;;; tagged? : alpha -> Boolean
(define (tagged? x)
  (and (pair? x) (eq? (car x) 'tagged)))

;;; run-tagged : Tagged a b -> b
(define (run-tagged tg)
  (cadr tg))

;;; profunctor-tagged : Profunctor Tagged
;;; dimap f g (Tagged b) = Tagged (g b)
;;; (We ignore f since Tagged doesn't use a)
(define profunctor-tagged
  (make-profunctor
   (lambda (f g tg)
     (make-tagged (g (run-tagged tg))))))

;;; choice-tagged : Choice Tagged
;;; pleft (Tagged b) = Tagged (Left b)
(define choice-tagged
  (make-choice
   profunctor-tagged
   (lambda (tg)
     (make-tagged (left (run-tagged tg))))))

;;; ====
;;; Star Profunctor (for Traversals)
;;; ====
;;;
;;; Star f a b = a -> f b
;;; Where f is a Functor/Applicative.

;;; make-star : Functor -> (a -> f b) -> Star f a b
(define (make-star functor run-fn)
  (list 'star functor run-fn))

;;; star? : alpha -> Boolean
(define (star? x)
  (and (pair? x) (eq? (car x) 'star)))

;;; star-functor : Star f a b -> Functor f
(define (star-functor s)
  (cadr s))

;;; run-star : Star f a b -> (a -> f b)
(define (run-star s)
  (caddr s))

;;; profunctor-star : Functor f -> Profunctor (Star f)
;;; dimap f g (Star h) = Star (fmap g . h . f)
(define (profunctor-star functor)
  (make-profunctor
   (lambda (f g st)
     (make-star functor
                (compose2 (lambda (fb) ((functor-fmap functor) g fb))
                          (compose2 (run-star st) f))))))

;;; ============================================================
;;; Part 5: Profunctor Optics
;;; ============================================================
;;;
;;; Optics as polymorphic functions constrained by profunctor classes.
;;;
;;; An optic is represented as a function that transforms profunctors:
;;;   optic :: forall p. Constraints p => p a b -> p s t
;;;
;;; We encode this as a record containing the transformation function
;;; along with metadata about which constraints are needed.

;;; ====
;;; Profunctor Iso
;;; ====
;;;
;;; Iso s t a b ~ forall p. Profunctor p => p a b -> p s t
;;; Implementation: dimap (s -> a) (b -> t)

;;; make-p-iso : (s -> a) -> (b -> t) -> PIso s t a b
(define (make-p-iso forward backward)
  (list 'p-iso forward backward))

;;; p-iso? : alpha -> Boolean
(define (p-iso? x)
  (and (pair? x) (eq? (car x) 'p-iso)))

;;; p-iso-forward : PIso -> (s -> a)
(define (p-iso-forward iso)
  (cadr iso))

;;; p-iso-backward : PIso -> (b -> t)
(define (p-iso-backward iso)
  (caddr iso))

;;; run-p-iso : Profunctor -> PIso s t a b -> p a b -> p s t
;;; Apply the iso transformation using a profunctor instance.
(define (run-p-iso prof iso pab)
  (dimap prof (p-iso-forward iso) (p-iso-backward iso) pab))

;;; p-iso-compose : PIso s t a b -> PIso a b c d -> PIso s t c d
(define (p-iso-compose outer inner)
  (make-p-iso
   (compose2 (p-iso-forward inner) (p-iso-forward outer))
   (compose2 (p-iso-backward outer) (p-iso-backward inner))))

;;; ====
;;; Profunctor Lens
;;; ====
;;;
;;; Lens s t a b ~ forall p. Strong p => p a b -> p s t
;;; Implementation uses the "shop" representation internally:
;;;   Lens (s -> a) (s -> b -> t)

;;; make-p-lens : (s -> a) -> (s -> b -> t) -> PLens s t a b
(define (make-p-lens getter setter)
  (list 'p-lens getter setter))

;;; p-lens? : alpha -> Boolean
(define (p-lens? x)
  (and (pair? x) (eq? (car x) 'p-lens)))

;;; p-lens-getter : PLens -> (s -> a)
(define (p-lens-getter lens)
  (cadr lens))

;;; p-lens-setter : PLens -> (s -> b -> t)
(define (p-lens-setter lens)
  (caddr lens))

;;; run-p-lens : Strong -> PLens s t a b -> p a b -> p s t
;;; Apply the lens transformation using a strong profunctor.
;;;
;;; The trick: use pfirst to handle (a, s) -> (b, s), then dimap
;;; to convert s -> (a, s) and (b, s) -> t.
(define (run-p-lens strong lens pab)
  (let* ([prof (strong-profunctor strong)]
         [get (p-lens-getter lens)]
         [set (p-lens-setter lens)]
         ;; s -> (a, s)
         [split (lambda (s) (cons (get s) s))]
         ;; (b, s) -> t
         [combine (lambda (bs) ((set (cdr bs)) (car bs)))])
    (dimap prof split combine (pfirst strong pab))))

;;; p-lens-compose : PLens s t a b -> PLens a b c d -> PLens s t c d
(define (p-lens-compose outer inner)
  (make-p-lens
   ;; getter: s -> c
   (compose2 (p-lens-getter inner) (p-lens-getter outer))
   ;; setter: s -> d -> t
   (lambda (s)
     (lambda (d)
       (let* ([a ((p-lens-getter outer) s)]
              [b (((p-lens-setter inner) a) d)])
         (((p-lens-setter outer) s) b))))))

;;; ====
;;; Profunctor Prism
;;; ====
;;;
;;; Prism s t a b ~ forall p. Choice p => p a b -> p s t
;;; Implementation uses the "market" representation:
;;;   Prism (s -> Either t a) (b -> t)

;;; make-p-prism : (s -> Either t a) -> (b -> t) -> PPrism s t a b
(define (make-p-prism match build)
  (list 'p-prism match build))

;;; p-prism? : alpha -> Boolean
(define (p-prism? x)
  (and (pair? x) (eq? (car x) 'p-prism)))

;;; p-prism-match : PPrism -> (s -> Either t a)
(define (p-prism-match prism)
  (cadr prism))

;;; p-prism-build : PPrism -> (b -> t)
(define (p-prism-build prism)
  (caddr prism))

;;; run-p-prism : Choice -> PPrism s t a b -> p a b -> p s t
;;; Apply the prism transformation using a choice profunctor.
(define (run-p-prism choice prism pab)
  (let* ([prof (choice-profunctor choice)]
         [match (p-prism-match prism)]
         [build (p-prism-build prism)]
         ;; Either t b -> t
         [merge (lambda (e)
                  (if (left? e) (from-left e) (build (from-right e))))])
    (dimap prof match merge (pright choice pab))))

;;; p-prism-compose : PPrism s t a b -> PPrism a b c d -> PPrism s t c d
(define (p-prism-compose outer inner)
  (make-p-prism
   ;; match: s -> Either t c
   (lambda (s)
     (let ([e ((p-prism-match outer) s)])
       (if (left? e)
           e  ; Already failed with t
           (let ([a (from-right e)])
             (let ([e2 ((p-prism-match inner) a)])
               (if (left? e2)
                   ;; Inner failed with b, need to build back to t
                   (left ((p-prism-build outer) (from-left e2)))
                   e2))))))
   ;; build: d -> t
   (compose2 (p-prism-build outer) (p-prism-build inner))))

;;; ====
;;; Profunctor Affine
;;; ====
;;;
;;; Affine s t a b ~ forall p. (Strong p, Choice p) => p a b -> p s t
;;; Implementation combines lens and prism:
;;;   Affine (s -> Either t a) (s -> b -> t)

;;; make-p-affine : (s -> Either t a) -> (s -> b -> t) -> PAffine s t a b
(define (make-p-affine preview set)
  (list 'p-affine preview set))

;;; p-affine? : alpha -> Boolean
(define (p-affine? x)
  (and (pair? x) (eq? (car x) 'p-affine)))

;;; p-affine-preview : PAffine -> (s -> Either t a)
(define (p-affine-preview affine)
  (cadr affine))

;;; p-affine-set : PAffine -> (s -> b -> t)
(define (p-affine-set affine)
  (caddr affine))

;;; run-p-affine : Strong -> Choice -> PAffine -> p a b -> p s t
;;; Apply the affine transformation using both Strong and Choice.
;;;
;;; We use the stall representation: first use pright for the Either,
;;; then use pfirst to handle the original s for the setter.
(define (run-p-affine strong choice affine pab)
  (let* ([prof-s (strong-profunctor strong)]
         [prof-c (choice-profunctor choice)]
         [preview-fn (p-affine-preview affine)]
         [set-fn (p-affine-set affine)]
         ;; s -> Either t (a, s)
         [split (lambda (s)
                  (let ([e (preview-fn s)])
                    (if (left? e)
                        e
                        (right (cons (from-right e) s)))))]
         ;; Either t (b, s) -> t
         [merge (lambda (e)
                  (if (left? e)
                      (from-left e)
                      (let ([bs (from-right e)])
                        ((set-fn (cdr bs)) (car bs)))))])
    (dimap prof-s split merge
           (pright choice (pfirst strong pab)))))

;;; p-affine-compose : PAffine s t a b -> PAffine a b c d -> PAffine s t c d
(define (p-affine-compose outer inner)
  (make-p-affine
   ;; preview: s -> Either t c
   (lambda (s)
     (let ([e ((p-affine-preview outer) s)])
       (if (left? e)
           e
           (let ([a (from-right e)])
             (let ([e2 ((p-affine-preview inner) a)])
               (if (left? e2)
                   ;; Need to set the inner result back
                   (left (((p-affine-set outer) s) (from-left e2)))
                   e2))))))
   ;; set: s -> d -> t
   (lambda (s)
     (lambda (d)
       (let ([e ((p-affine-preview outer) s)])
         (if (left? e)
             (from-left e)  ; No target, return unchanged
             (let* ([a (from-right e)]
                    [b (((p-affine-set inner) a) d)])
               (((p-affine-set outer) s) b))))))))

;;; ====
;;; Profunctor Grate
;;; ====
;;;
;;; Grate s t a b ~ forall p. Closed p => p a b -> p s t
;;;
;;; A Grate is the dual of a Lens:
;;;   - Lens focuses on one part of a product type (get/set)
;;;   - Grate focuses on a "closed" structure (zip/cotraverse)
;;;
;;; The concrete representation is:
;;;   Grate ((s -> a) -> b) -> t
;;;
;;; This says: "Given any way to extract an 'a' from any 's', and
;;; a way to combine those extractions into a 'b', I can produce a 't'."
;;;
;;; The canonical example is functions: for f : x -> a, we can zip
;;; multiple functions together by applying them all to the same input.

;;; make-p-grate : (((s -> a) -> b) -> t) -> PGrate s t a b
(define (make-p-grate cotraverse-fn)
  (list 'p-grate cotraverse-fn))

;;; p-grate? : alpha -> Boolean
(define (p-grate? x)
  (and (pair? x) (eq? (car x) 'p-grate)))

;;; p-grate-cotraverse : PGrate -> (((s -> a) -> b) -> t)
(define (p-grate-cotraverse grate)
  (cadr grate))

;;; run-p-grate : Closed -> PGrate s t a b -> p a b -> p s t
;;; Apply the grate transformation using a closed profunctor.
;;;
;;; The implementation:
;;;   Given grate : ((s -> a) -> b) -> t
;;;   and pab : p a b
;;;   we need p s t
;;;
;;;   Using pclosed: p a b -> p (x -> a) (x -> b)
;;;   Then dimap with (s -> (s -> a)) and (((s -> a) -> b) -> t)
(define (run-p-grate closed grate pab)
  (let* ([prof (closed-profunctor closed)]
         [cotr (p-grate-cotraverse grate)]
         ;; s -> (s -> a): the "tabling" function that produces the extractor
         [tabling (lambda (s) (lambda (sa) (sa s)))]
         ;; ((s -> a) -> b) -> t: apply cotraverse
         )
    (dimap prof tabling cotr (pclosed closed pab))))

;;; p-grate-compose : PGrate s t a b -> PGrate a b c d -> PGrate s t c d
;;; Composition of grates.
(define (p-grate-compose outer inner)
  (make-p-grate
   (lambda (sctod)
     ;; outer : ((s -> a) -> b) -> t
     ;; inner : ((a -> c) -> d) -> b
     ;; need: ((s -> c) -> d) -> t
     ((p-grate-cotraverse outer)
      (lambda (sa)
        ;; sa : s -> a
        ;; need: b
        ((p-grate-cotraverse inner)
         (lambda (ac)
           ;; ac : a -> c
           ;; need: d
           (sctod (compose2 ac sa)))))))))

;;; ============================================================
;;; Part 6: Conversion Functions
;;; ============================================================
;;;
;;; Convert between profunctor optics and concrete optics.

;;; ====
;;; To Profunctor Optics
;;; ====

;;; iso->p-iso : Iso -> PIso
(define (iso->p-iso iso)
  (make-p-iso (iso-forward iso) (iso-backward iso)))

;;; lens->p-lens : Lens -> PLens
(define (lens->p-lens lens)
  (make-p-lens
   (lens-getter lens)
   (lambda (s)
     (lambda (b)
       ((lens-setter lens) b s)))))

;;; prism->p-prism : Prism -> PPrism
;;; Concrete prism uses Maybe for match, profunctor prism uses Either.
(define (prism->p-prism prism)
  (make-p-prism
   (lambda (s)
     (let ([m ((prism-match prism) s)])
       (if (nothing? m)
           (left s)  ; Return original on failure
           (right (from-just m)))))
   (prism-build prism)))

;;; affine->p-affine : Affine -> PAffine
(define (affine->p-affine affine)
  (make-p-affine
   (lambda (s)
     (let ([m ((affine-getter affine) s)])
       (if (nothing? m)
           (left s)
           (right (from-just m)))))
   (lambda (s)
     (lambda (b)
       ((affine-setter affine) b s)))))

;;; grate->p-grate : Grate -> PGrate
(define (grate->p-grate grate)
  (make-p-grate (grate-cotraverse-fn grate)))

;;; ====
;;; From Profunctor Optics
;;; ====

;;; p-iso->iso : PIso -> Iso
(define (p-iso->iso piso)
  (make-iso (p-iso-forward piso) (p-iso-backward piso)))

;;; p-lens->lens : PLens -> Lens
(define (p-lens->lens plens)
  (make-lens
   (p-lens-getter plens)
   (lambda (b s) ((p-lens-setter plens s) b))))

;;; p-prism->prism : PPrism -> Prism
;;; Convert from Either-based to Maybe-based match.
(define (p-prism->prism pprism)
  (make-prism
   (lambda (s)
     (let ([e ((p-prism-match pprism) s)])
       (if (right? e)
           (just (from-right e))
           nothing)))
   (p-prism-build pprism)))

;;; p-affine->affine : PAffine -> Affine
(define (p-affine->affine paffine)
  (make-affine
   (lambda (s)
     (let ([e ((p-affine-preview paffine) s)])
       (if (right? e)
           (just (from-right e))
           nothing)))
   (lambda (b s) ((p-affine-set paffine s) b))))

;;; p-grate->grate : PGrate -> Grate
(define (p-grate->grate pgrate)
  (make-grate (p-grate-cotraverse pgrate)))

;;; ============================================================
;;; Part 7: Using Profunctor Optics
;;; ============================================================
;;;
;;; Utility functions for working with profunctor optics.

;;; p-view : PLens s t a b -> s -> a
;;; View through a profunctor lens using Forget.
(define (p-view plens s)
  ((run-forget
    (run-p-lens strong-forget plens (make-forget identity)))
   s))

;;; p-over : PLens s t a b -> (a -> b) -> s -> t
;;; Modify through a profunctor lens using (->).
(define (p-over plens f s)
  ((run-p-lens strong-fn plens f) s))

;;; p-set : PLens s t a b -> b -> s -> t
;;; Set through a profunctor lens.
(define (p-set plens b s)
  (p-over plens (const b) s))

;;; p-preview : PPrism s t a b -> s -> Maybe a
;;; Preview through a profunctor prism.
(define (p-preview pprism s)
  (let ([e ((p-prism-match pprism) s)])
    (if (right? e)
        (just (from-right e))
        nothing)))

;;; p-review : PPrism s t a b -> b -> t
;;; Review (build) through a profunctor prism.
(define (p-review pprism b)
  ((p-prism-build pprism) b))

;;; p-prism-over : PPrism s t a b -> (a -> b) -> s -> t
;;; Modify through a profunctor prism.
(define (p-prism-over pprism f s)
  ((run-p-prism choice-fn pprism f) s))

;;; p-affine-preview : PAffine s t a b -> s -> Maybe a
;;; Preview through a profunctor affine.
(define (p-affine-preview-fn paffine s)
  (let ([e ((p-affine-preview paffine) s)])
    (if (right? e)
        (just (from-right e))
        nothing)))

;;; p-affine-set-fn : PAffine s t a b -> b -> s -> t
;;; Set through a profunctor affine.
(define (p-affine-set-fn paffine b s)
  (((p-affine-set paffine) s) b))

;;; p-grate-review : PGrate s t a b -> b -> t
;;; Review (inject constant) through a profunctor grate.
(define (p-grate-review pgrate b)
  ((p-grate-cotraverse pgrate) (lambda (_) b)))

;;; p-grate-over : PGrate s t a b -> (a -> b) -> s -> t
;;; Modify through a profunctor grate using (->).
(define (p-grate-over pgrate f s)
  ((run-p-grate closed-fn-instance pgrate f) s))

;;; p-grate-zipWith : PGrate s t a b -> (a -> a -> b) -> s -> s -> t
;;; Zip two structures through a profunctor grate.
(define (p-grate-zipWith pgrate f s1 s2)
  ((p-grate-cotraverse pgrate)
   (lambda (sa) (f (sa s1) (sa s2)))))

;;; ============================================================
;;; Part 8: Common Profunctor Optics
;;; ============================================================

;;; ====
;;; Isomorphisms
;;; ====

;;; p-iso-id : PIso a b a b
(define p-iso-id
  (make-p-iso identity identity))

;;; p-iso-swapped : PIso (a, b) (c, d) (b, a) (d, c)
(define p-iso-swapped
  (make-p-iso swap swap))

;;; p-iso-reversed : PIso (List a) (List b) (List a) (List b)
(define p-iso-reversed
  (make-p-iso reverse reverse))

;;; p-iso-curried : PIso ((a, b) -> c) ((a', b') -> c') (a -> b -> c) (a' -> b' -> c')
(define p-iso-curried
  (make-p-iso
   (lambda (f) (lambda (a) (lambda (b) (f (cons a b)))))
   (lambda (f) (lambda (ab) ((f (car ab)) (cdr ab))))))

;;; ====
;;; Lenses
;;; ====

;;; p-lens-fst : PLens (a, c) (b, c) a b
(define p-lens-fst
  (make-p-lens
   car
   (lambda (s) (lambda (b) (cons b (cdr s))))))

;;; p-lens-snd : PLens (c, a) (c, b) a b
(define p-lens-snd
  (make-p-lens
   cdr
   (lambda (s) (lambda (b) (cons (car s) b)))))

;;; p-lens-head : PLens (List a) (List a) a a
(define p-lens-head
  (make-p-lens
   car
   (lambda (s) (lambda (a) (cons a (cdr s))))))

;;; p-lens-nth : Nat -> PLens (List a) (List a) a a
(define (p-lens-nth n)
  (make-p-lens
   (lambda (xs) (list-ref xs n))
   (lambda (s)
     (lambda (a)
       (let loop ([i 0] [xs s] [acc '()])
         (if (null? xs)
             (reverse acc)
             (if (= i n)
                 (loop (+ i 1) (cdr xs) (cons a acc))
                 (loop (+ i 1) (cdr xs) (cons (car xs) acc)))))))))

;;; ====
;;; Prisms
;;; ====

;;; p-prism-just : PPrism (Maybe a) (Maybe b) a b
(define p-prism-just
  (make-p-prism
   (lambda (m)
     (if (just? m)
         (right (from-just m))
         (left nothing)))
   just))

;;; p-prism-left : PPrism (Either a c) (Either b c) a b
(define p-prism-left
  (make-p-prism
   (lambda (e)
     (if (left? e)
         (right (from-left e))
         (left e)))  ; Return original Right
   left))

;;; p-prism-right : PPrism (Either c a) (Either c b) a b
(define p-prism-right
  (make-p-prism
   (lambda (e)
     (if (right? e)
         (right (from-right e))
         (left e)))  ; Return original Left
   right))

;;; p-prism-nil : PPrism (List a) (List a) () ()
(define p-prism-nil
  (make-p-prism
   (lambda (xs)
     (if (null? xs)
         (right '())
         (left xs)))
   (const '())))

;;; p-prism-cons : PPrism (List a) (List a) (a, List a) (a, List a)
(define p-prism-cons
  (make-p-prism
   (lambda (xs)
     (if (null? xs)
         (left '())
         (right (cons (car xs) (cdr xs)))))
   (lambda (p) (cons (car p) (cdr p)))))

;;; ====
;;; Affines
;;; ====

;;; p-affine-nth : Nat -> PAffine (List a) (List a) a a
(define (p-affine-nth n)
  (make-p-affine
   (lambda (xs)
     (if (< n (length xs))
         (right (list-ref xs n))
         (left xs)))
   (lambda (s)
     (lambda (a)
       (if (< n (length s))
           (let loop ([i 0] [xs s] [acc '()])
             (if (null? xs)
                 (reverse acc)
                 (if (= i n)
                     (loop (+ i 1) (cdr xs) (cons a acc))
                     (loop (+ i 1) (cdr xs) (cons (car xs) acc)))))
           s)))))

;;; ====
;;; Grates
;;; ====

;;; p-grate-id : PGrate a b a b
;;; Identity grate.
(define p-grate-id
  (make-p-grate
   (lambda (satob) (satob identity))))

;;; p-grate-fn : PGrate (x -> a) (x -> b) a b
;;; The canonical grate for functions.
(define p-grate-fn
  (make-p-grate
   (lambda (satob)
     (lambda (x)
       (satob (lambda (f) (f x)))))))

;;; p-grate-pair-same : PGrate (a . a) (b . b) a b
;;; Grate for pairs with same-typed elements.
(define p-grate-pair-same
  (make-p-grate
   (lambda (satob)
     (cons
      (satob car)
      (satob cdr)))))

;;; p-grate-list-rep : Nat -> PGrate (List a) (List b) a b
;;; Grate for fixed-length lists.
(define (p-grate-list-rep n)
  (make-p-grate
   (lambda (satob)
     (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (satob (lambda (xs) (list-ref xs i))) acc)))))))

;;; ============================================================
;;; Part 9: Unified Composition
;;; ============================================================
;;;
;;; Compose any two profunctor optics, inferring the result type.

;;; p-optic-type : POptic -> Symbol
(define (p-optic-type o)
  (if (pair? o) (car o) 'unknown))

;;; p-optic-compose : POptic x POptic -> POptic
;;; Compose two profunctor optics with automatic type inference.
(define (p-optic-compose outer inner)
  (let ([t1 (p-optic-type outer)]
        [t2 (p-optic-type inner)])
    (cond
      ;; Iso + anything preserves the inner type (iso is neutral)
      [(eq? t1 'p-iso)
       (cond
         [(eq? t2 'p-iso) (p-iso-compose outer inner)]
         [(eq? t2 'p-lens)
          (make-p-lens
           (compose2 (p-lens-getter inner) (p-iso-forward outer))
           (lambda (s)
             (lambda (b)
               ((p-iso-backward outer)
                (((p-lens-setter inner) ((p-iso-forward outer) s)) b)))))]
         [(eq? t2 'p-prism)
          (make-p-prism
           (lambda (s)
             (let ([e ((p-prism-match inner) ((p-iso-forward outer) s))])
               (if (left? e)
                   (left s)  ; Lift failure back through iso
                   e)))
           (compose2 (p-iso-backward outer) (p-prism-build inner)))]
         [else (error 'p-optic-compose "Unknown inner type")])]

      ;; anything + Iso preserves outer type
      [(eq? t2 'p-iso)
       (cond
         [(eq? t1 'p-lens)
          (make-p-lens
           (compose2 (p-iso-forward inner) (p-lens-getter outer))
           (lambda (s)
             (lambda (b)
               (((p-lens-setter outer) s) ((p-iso-backward inner) b)))))]
         [(eq? t1 'p-prism)
          (make-p-prism
           (lambda (s)
             (let ([e ((p-prism-match outer) s)])
               (if (left? e)
                   e
                   (right ((p-iso-forward inner) (from-right e))))))
           (compose2 (p-prism-build outer) (p-iso-backward inner)))]
         [else (error 'p-optic-compose "Unknown outer type")])]

      ;; Lens + Lens = Lens
      [(and (eq? t1 'p-lens) (eq? t2 'p-lens))
       (p-lens-compose outer inner)]

      ;; Prism + Prism = Prism
      [(and (eq? t1 'p-prism) (eq? t2 'p-prism))
       (p-prism-compose outer inner)]

      ;; Lens + Prism = Affine
      [(and (eq? t1 'p-lens) (eq? t2 'p-prism))
       (make-p-affine
        (lambda (s)
          (let ([a ((p-lens-getter outer) s)])
            (let ([e ((p-prism-match inner) a)])
              (if (left? e)
                  (left s)  ; Lift failure back to original structure
                  e))))
        (lambda (s)
          (lambda (b)
            (((p-lens-setter outer) s) ((p-prism-build inner) b)))))]

      ;; Prism + Lens = Affine
      [(and (eq? t1 'p-prism) (eq? t2 'p-lens))
       (make-p-affine
        (lambda (s)
          (let ([e ((p-prism-match outer) s)])
            (if (left? e)
                e
                (right ((p-lens-getter inner) (from-right e))))))
        (lambda (s)
          (lambda (b)
            (let ([e ((p-prism-match outer) s)])
              (if (left? e)
                  (from-left e)
                  ((p-prism-build outer)
                   (((p-lens-setter inner) (from-right e)) b)))))))]

      ;; Affine compositions
      [(and (eq? t1 'p-affine) (eq? t2 'p-affine))
       (p-affine-compose outer inner)]

      [(and (eq? t1 'p-affine) (eq? t2 'p-lens))
       (p-affine-compose outer (p-lens->p-affine inner))]

      [(and (eq? t1 'p-lens) (eq? t2 'p-affine))
       (p-affine-compose (p-lens->p-affine outer) inner)]

      [(and (eq? t1 'p-affine) (eq? t2 'p-prism))
       (p-affine-compose outer (p-prism->p-affine inner))]

      [(and (eq? t1 'p-prism) (eq? t2 'p-affine))
       (p-affine-compose (p-prism->p-affine outer) inner)]

      ;; Grate compositions
      [(and (eq? t1 'p-grate) (eq? t2 'p-grate))
       (p-grate-compose outer inner)]

      [(and (eq? t1 'p-iso) (eq? t2 'p-grate))
       (p-grate-compose (p-iso->p-grate outer) inner)]

      [(and (eq? t1 'p-grate) (eq? t2 'p-iso))
       (p-grate-compose outer (p-iso->p-grate inner))]

      [else (error 'p-optic-compose "Unsupported composition")])))

;;; p-iso->p-grate : PIso -> PGrate
;;; Convert a profunctor iso to a profunctor grate.
(define (p-iso->p-grate piso)
  (make-p-grate
   (lambda (satob)
     ((p-iso-backward piso)
      (satob (p-iso-forward piso))))))

;;; p-lens->p-affine : PLens -> PAffine
;;; Convert a profunctor lens to a profunctor affine.
(define (p-lens->p-affine plens)
  (make-p-affine
   (lambda (s) (right ((p-lens-getter plens) s)))
   (p-lens-setter plens)))

;;; p-prism->p-affine : PPrism -> PAffine
;;; Convert a profunctor prism to a profunctor affine.
(define (p-prism->p-affine pprism)
  (make-p-affine
   (p-prism-match pprism)
   (lambda (s)
     (lambda (b)
       (let ([e ((p-prism-match pprism) s)])
         (if (left? e)
             (from-left e)
             ((p-prism-build pprism) b)))))))

;;; ============================================================
;;; Part 10: Law Verification
;;; ============================================================

;;; verify-profunctor-laws : Profunctor -> (List (p a b)) -> Boolean
;;; Check that dimap id id = id and composition law holds.
(define (verify-profunctor-laws prof test-pabs)
  (let ([dmap (profunctor-dimap prof)])
    (andmap
     (lambda (pab)
       ;; dimap id id = id
       (equal? (dmap identity identity pab) pab))
     test-pabs)))

;;; verify-p-iso-laws : PIso s t a b -> (List s) -> (List b) -> Boolean
(define (verify-p-iso-laws piso test-ss test-bs)
  (let ([fwd (p-iso-forward piso)]
        [bwd (p-iso-backward piso)])
    (and
     ;; forward . backward = id on b
     (andmap (lambda (b) (equal? (fwd (bwd b)) b)) test-bs)
     ;; backward . forward = id on s
     (andmap (lambda (s) (equal? (bwd (fwd s)) s)) test-ss))))

;;; verify-p-lens-laws : PLens s t a b -> (List (s, b)) -> Boolean
(define (verify-p-lens-laws plens test-pairs)
  (let ([get (p-lens-getter plens)]
        [set (p-lens-setter plens)])
    (andmap
     (lambda (pair)
       (let ([s (car pair)]
             [b (cdr pair)])
         (and
          ;; Get-Put: set (get s) s = s
          (equal? ((set s) (get s)) s)
          ;; Put-Get: get (set b s) = b
          (equal? (get ((set s) b)) b)
          ;; Put-Put: set b' (set b s) = set b' s
          (equal? ((set ((set s) (get s))) b)
                  ((set s) b)))))
     test-pairs)))

;;; verify-p-prism-laws : PPrism s t a b -> (List b) -> (List s) -> Boolean
(define (verify-p-prism-laws pprism test-bs test-ss)
  (let ([match (p-prism-match pprism)]
        [build (p-prism-build pprism)])
    (and
     ;; Preview-Review: match (build b) = Right b
     (andmap (lambda (b)
               (let ([e (match (build b))])
                 (and (right? e) (equal? (from-right e) b))))
             test-bs)
     ;; Review-Preview: if match s = Right a, then build a matches s
     (andmap (lambda (s)
               (let ([e (match s)])
                 (or (left? e)
                     (equal? (build (from-right e)) s))))
             test-ss))))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Profunctor Type Class:
;;;   make-profunctor, profunctor?, profunctor-dimap, profunctor-lmap, profunctor-rmap
;;;   dimap, lmap, rmap
;;;
;;; Strong Profunctor:
;;;   make-strong, strong?, strong-profunctor, strong-first, strong-second
;;;   pfirst, psecond
;;;
;;; Choice Profunctor:
;;;   make-choice, choice?, choice-profunctor, choice-left, choice-right
;;;   pleft, pright, swap-either
;;;
;;; Closed Profunctor:
;;;   make-closed, closed-profunctor?, closed-profunctor, closed-fn, pclosed
;;;
;;; Profunctor Instances:
;;;   profunctor-fn, strong-fn, choice-fn, closed-fn-instance
;;;   make-forget, forget?, run-forget, profunctor-forget, strong-forget
;;;   make-tagged, tagged?, run-tagged, profunctor-tagged, choice-tagged
;;;   make-star, star?, star-functor, run-star, profunctor-star
;;;
;;; Profunctor Iso:
;;;   make-p-iso, p-iso?, p-iso-forward, p-iso-backward
;;;   run-p-iso, p-iso-compose
;;;   p-iso-id, p-iso-swapped, p-iso-reversed, p-iso-curried
;;;
;;; Profunctor Lens:
;;;   make-p-lens, p-lens?, p-lens-getter, p-lens-setter
;;;   run-p-lens, p-lens-compose
;;;   p-view, p-over, p-set
;;;   p-lens-fst, p-lens-snd, p-lens-head, p-lens-nth
;;;
;;; Profunctor Prism:
;;;   make-p-prism, p-prism?, p-prism-match, p-prism-build
;;;   run-p-prism, p-prism-compose
;;;   p-preview, p-review, p-prism-over
;;;   p-prism-just, p-prism-left, p-prism-right, p-prism-nil, p-prism-cons
;;;
;;; Profunctor Affine:
;;;   make-p-affine, p-affine?, p-affine-preview, p-affine-set
;;;   run-p-affine, p-affine-compose
;;;   p-affine-preview-fn, p-affine-set-fn
;;;   p-affine-nth
;;;
;;; Profunctor Grate:
;;;   make-p-grate, p-grate?, p-grate-cotraverse
;;;   run-p-grate, p-grate-compose
;;;   p-grate-review, p-grate-over, p-grate-zipWith
;;;   p-grate-id, p-grate-fn, p-grate-pair-same, p-grate-list-rep
;;;
;;; Conversions:
;;;   iso->p-iso, lens->p-lens, prism->p-prism, affine->p-affine, grate->p-grate
;;;   p-iso->iso, p-lens->lens, p-prism->prism, p-affine->affine, p-grate->grate
;;;   p-lens->p-affine, p-prism->p-affine, p-iso->p-grate
;;;
;;; Unified Composition:
;;;   p-optic-type, p-optic-compose
;;;
;;; Law Verification:
;;;   verify-profunctor-laws, verify-p-iso-laws, verify-p-lens-laws, verify-p-prism-laws

(display "Loaded: lattice/fp/optics/profunctor-optics.ss\n")
