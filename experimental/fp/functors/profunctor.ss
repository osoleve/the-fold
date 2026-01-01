;;; fabric/stitches/fp/profunctor.ss — Profunctor Abstraction
;;;
;;; A profunctor is a bifunctor that is contravariant in its first
;;; argument and covariant in its second. Profunctors generalize
;;; functions and are the foundation for advanced optics.
;;;
;;; A profunctor p has:
;;;   dimap : (a' -> a) -> (b -> b') -> p a b -> p a' b'
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Profunctor interface (dimap, lmap, rmap)
;;;   - Function profunctor (->)
;;;   - Tagged profunctor (for Prisms)
;;;   - Star and Costar (Kleisli profunctors)
;;;   - Strong profunctor (for Lenses)
;;;   - Choice profunctor (for Prisms)
;;;   - Closed profunctor
;;;   - Profunctor composition
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Profunctor Laws:
;;;   dimap id id        = id
;;;   dimap f g . dimap h k = dimap (h . f) (g . k)
;;;
;;; Strong Laws (in addition):
;;;   lmap fst            = rmap fst . first'
;;;   lmap snd            = rmap snd . second'
;;;   lmap (second f) . first'   = rmap (second f) . first'
;;;   first' . first'    = dimap assoc unassoc . first'
;;;
;;; Choice Laws (in addition):
;;;   lmap Left           = rmap Left . left'
;;;   lmap Right          = rmap Right . right'
;;;   left' . left'       = dimap unassocE assocE . left'

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Function Profunctor (->)
;;; ============================================================
;;;
;;; The canonical profunctor: ordinary functions.

;;; fn-dimap : (a' -> a) -> (b -> b') -> (a -> b) -> (a' -> b')
;;; Profunctor dimap for functions.
(define (fn-dimap f g h)
  (lambda (a-prime)
          (g (h (f a-prime)))))

;;; fn-lmap : (a' -> a) -> (a -> b) -> (a' -> b)
;;; Contravariant map on input.
(define (fn-lmap f h)
  (lambda (a-prime) (h (f a-prime))))

;;; fn-rmap : (b -> b') -> (a -> b) -> (a -> b')
;;; Covariant map on output.
(define (fn-rmap g h)
  (lambda (a) (g (h a))))

;;; ============================================================
;;; Tagged Profunctor
;;; ============================================================
;;;
;;; Tagged a b ignores a and wraps b.
;;; Used for Prisms and Reviews.

;;; make-tagged : b -> Tagged a b
(define (make-tagged val)
  (list 'tagged val))

;;; tagged? : Any -> Boolean
(define (tagged? x)
  (and (pair? x) (eq? (car x) 'tagged)))

;;; untag : Tagged a b -> b
(define (untag t)
  (list-ref t 1))

;;; tagged-dimap : (a' -> a) -> (b -> b') -> Tagged a b -> Tagged a' b'
;;; dimap for Tagged ignores first argument.
(define (tagged-dimap f g t)
  (make-tagged (g (untag t))))

;;; tagged-lmap : (a' -> a) -> Tagged a b -> Tagged a' b
(define (tagged-lmap f t)
  t)  ; Ignores input type transformation

;;; tagged-rmap : (b -> b') -> Tagged a b -> Tagged a b'
(define (tagged-rmap g t)
  (make-tagged (g (untag t))))

;;; ============================================================
;;; Star Profunctor (Kleisli)
;;; ============================================================
;;;
;;; Star f a b = a -> f b
;;; Lifts a functor into a profunctor.

;;; make-star : (a -> f b) -> Star f a b
(define (make-star run-fn)
  (list 'star run-fn))

;;; star? : Any -> Boolean
(define (star? x)
  (and (pair? x) (eq? (car x) 'star)))

;;; run-star : Star f a b -> a -> f b
(define (run-star s a)
  ((list-ref s 1) a))

;;; star-dimap : ((a -> b) -> f a -> f b) -> (a' -> a) -> (b -> b') -> Star f a b -> Star f a' b'
;;; dimap for Star, requires functor's fmap.
(define (star-dimap fmap f g s)
  (make-star (lambda (a-prime)
                     (fmap g (run-star s (f a-prime))))))

;;; star-lmap : (a' -> a) -> Star f a b -> Star f a' b
(define (star-lmap f s)
  (make-star (lambda (a-prime)
                     (run-star s (f a-prime)))))

;;; star-rmap : ((a -> b) -> f a -> f b) -> (b -> b') -> Star f a b -> Star f a b'
(define (star-rmap fmap g s)
  (make-star (lambda (a)
                     (fmap g (run-star s a)))))

;;; ============================================================
;;; Costar Profunctor (Cokleisli)
;;; ============================================================
;;;
;;; Costar f a b = f a -> b
;;; Dual of Star.

;;; make-costar : (f a -> b) -> Costar f a b
(define (make-costar run-fn)
  (list 'costar run-fn))

;;; costar? : Any -> Boolean
(define (costar? x)
  (and (pair? x) (eq? (car x) 'costar)))

;;; run-costar : Costar f a b -> f a -> b
(define (run-costar cs fa)
  ((list-ref cs 1) fa))

;;; costar-dimap : ((a -> b) -> f a -> f b) -> (a' -> a) -> (b -> b') -> Costar f a b -> Costar f a' b'
;;; dimap for Costar, requires functor's fmap.
(define (costar-dimap fmap f g cs)
  (make-costar (lambda (fa-prime)
                       (g (run-costar cs (fmap f fa-prime))))))

;;; ============================================================
;;; Strong Profunctor
;;; ============================================================
;;;
;;; Strong profunctors can thread a value through computation.
;;; Required for lenses.
;;;
;;; first' : p a b -> p (a, c) (b, c)
;;; second' : p a b -> p (c, a) (c, b)

;;; fn-first : (a -> b) -> (a, c) -> (b, c)
;;; Strong instance for functions.
(define (fn-first f)
  (lambda (pair)
          (cons (f (car pair)) (cdr pair))))

;;; fn-second : (a -> b) -> (c, a) -> (c, b)
(define (fn-second f)
  (lambda (pair)
          (cons (car pair) (f (cdr pair)))))

;;; star-first : ((a -> b) -> f a -> f b) -> Star f a b -> Star f (a, c) (b, c)
;;; Strong instance for Star.
(define (star-first fmap s)
  (make-star (lambda (pair)
                     (fmap (lambda (b) (cons b (cdr pair)))
                           (run-star s (car pair))))))

;;; star-second : ((a -> b) -> f a -> f b) -> Star f a b -> Star f (c, a) (c, b)
(define (star-second fmap s)
  (make-star (lambda (pair)
                     (fmap (lambda (b) (cons (car pair) b))
                           (run-star s (cdr pair))))))

;;; ============================================================
;;; Choice Profunctor
;;; ============================================================
;;;
;;; Choice profunctors can handle branching.
;;; Required for prisms.
;;;
;;; left' : p a b -> p (Either a c) (Either b c)
;;; right' : p a b -> p (Either c a) (Either c b)

;;; fn-left : (a -> b) -> Either a c -> Either b c
;;; Choice instance for functions.
(define (fn-left f)
  (lambda (e)
          (if (left? e)
              (left (f (from-left e)))
              e)))

;;; fn-right : (a -> b) -> Either c a -> Either c b
(define (fn-right f)
  (lambda (e)
          (if (right? e)
              (right (f (from-right e)))
              e)))

;;; star-left : ((a -> b) -> f a -> f b) -> (b -> f b) -> Star f a b -> Star f (Either a c) (Either b c)
;;; Choice instance for Star.
(define (star-left fmap pure s)
  (make-star (lambda (e)
                     (if (left? e)
                         (fmap left (run-star s (from-left e)))
                         (pure (right (from-right e)))))))

;;; star-right : ((a -> b) -> f a -> f b) -> (b -> f b) -> Star f a b -> Star f (Either c a) (Either c b)
(define (star-right fmap pure s)
  (make-star (lambda (e)
                     (if (right? e)
                         (fmap right (run-star s (from-right e)))
                         (pure (left (from-left e)))))))

;;; tagged-left : Tagged a b -> Tagged (Either a c) (Either b c)
;;; Choice instance for Tagged.
(define (tagged-left t)
  (make-tagged (left (untag t))))

;;; tagged-right : Tagged a b -> Tagged (Either c a) (Either c b)
(define (tagged-right t)
  (make-tagged (right (untag t))))

;;; ============================================================
;;; Closed Profunctor
;;; ============================================================
;;;
;;; Closed profunctors can handle function types.
;;;
;;; closed : p a b -> p (x -> a) (x -> b)

;;; fn-closed : (a -> b) -> (x -> a) -> (x -> b)
;;; Closed instance for functions.
(define (fn-closed f)
  (lambda (g)
          (lambda (x) (f (g x)))))

;;; ============================================================
;;; Profunctor Composition
;;; ============================================================
;;;
;;; Procompose p q a b = exists x. (p x b, q a x)
;;; Composes profunctors.

;;; make-procompose : p x b -> q a x -> Procompose p q a b
(define (make-procompose pxb qax)
  (list 'procompose pxb qax))

;;; procompose? : Any -> Boolean
(define (procompose? x)
  (and (pair? x) (eq? (car x) 'procompose)))

;;; procompose-p : Procompose p q a b -> p x b
(define (procompose-p pc) (list-ref pc 1))

;;; procompose-q : Procompose p q a b -> q a x
(define (procompose-q pc) (list-ref pc 2))

;;; ============================================================
;;; Optic Building Blocks
;;; ============================================================

;;; Iso s t a b = forall p. Profunctor p => p a b -> p s t
;;; An isomorphism - invertible transformation

;;; make-iso : (s -> a) -> (b -> t) -> Iso s t a b
;;; Create an iso from getter and setter.
(define (make-iso getter setter)
  (lambda (dimap-fn pab)
          (dimap-fn getter setter pab)))

;;; iso-forward : Iso s t a b -> s -> a
;;; Extract the forward function.
(define (iso-forward iso)
  (lambda (s)
          (let ([pab (lambda (x) x)])  ; Identity on a -> b
               ((iso fn-dimap pab) s))))

;;; Lens s t a b = forall p. Strong p => p a b -> p s t
;;; A lens focuses on a part of a structure

;;; lens-to-profunctor : (s -> a) -> (s -> b -> t) -> ((p a b -> p s t))
;;; Convert lens getter/setter to profunctor form.
(define (lens-to-profunctor getter setter)
  (lambda (dimap-fn first-fn pab)
          (dimap-fn (lambda (s) (cons (getter s) s))
                    (lambda (pair) (setter (cdr pair) (car pair)))
                    (first-fn pab))))

;;; Prism s t a b = forall p. Choice p => p a b -> p s t
;;; A prism focuses on one branch of a sum type

;;; prism-to-profunctor : (b -> t) -> (s -> Either t a) -> (p a b -> p s t)
;;; Convert prism review/match to profunctor form.
(define (prism-to-profunctor review match)
  (lambda (dimap-fn left-fn pab)
          (dimap-fn match
                    (lambda (e) (if (left? e) (from-left e) (review (from-right e))))
                    (left-fn pab))))

;;; ============================================================
;;; Practical Profunctor Utilities
;;; ============================================================

;;; dimap-compose : Dimap -> Dimap -> Dimap
;;; Compose two dimap functions (for profunctor composition).
(define (dimap-compose dimap1 dimap2)
  (lambda (f g pab)
          (dimap1 f g (dimap2 identity identity pab))))

;;; arr-to-profunctor : Arrow -> Profunctor operations
;;; Arrows are profunctors.
(define (arr-to-dimap arr-fn arr-compose)
  (lambda (f g arr)
          (arr-compose (arr-fn f) (arr-compose arr (arr-fn g)))))

;;; ============================================================
;;; Market Profunctor (for Prisms)
;;; ============================================================
;;;
;;; Market a b s t = Market (b -> t) (s -> Either t a)
;;; The canonical profunctor for prisms.

;;; make-market : (b -> t) -> (s -> Either t a) -> Market a b s t
(define (make-market setter getter)
  (list 'market setter getter))

;;; market? : Any -> Boolean
(define (market? x)
  (and (pair? x) (eq? (car x) 'market)))

;;; market-setter : Market a b s t -> (b -> t)
(define (market-setter m) (list-ref m 1))

;;; market-getter : Market a b s t -> (s -> Either t a)
(define (market-getter m) (list-ref m 2))

;;; market-dimap : (a' -> a) -> (b -> b') -> Market a b s t -> Market a' b' s t
(define (market-dimap f g m)
  (make-market (lambda (b-prime) ((market-setter m) (g b-prime)))
               (lambda (s)
                       (let ([e ((market-getter m) s)])
                            (if (left? e) e (right (f (from-right e))))))))

;;; market-left : Market a b s t -> Market a b (Either s x) (Either t x)
(define (market-left m)
  (make-market (lambda (b) (left ((market-setter m) b)))
               (lambda (e)
                       (if (left? e)
                           (let ([inner ((market-getter m) (from-left e))])
                                (if (left? inner)
                                    (left (left (from-left inner)))
                                    (right (from-right inner))))
                           (left (right (from-right e)))))))

;;; market-right : Market a b s t -> Market a b (Either x s) (Either x t)
(define (market-right m)
  (make-market (lambda (b) (right ((market-setter m) b)))
               (lambda (e)
                       (if (right? e)
                           (let ([inner ((market-getter m) (from-right e))])
                                (if (left? inner)
                                    (left (right (from-left inner)))
                                    (right (from-right inner))))
                           (left (left (from-left e)))))))

;;; ============================================================
;;; Exchange Profunctor (for Isos)
;;; ============================================================
;;;
;;; Exchange a b s t = Exchange (s -> a) (b -> t)
;;; The canonical profunctor for isomorphisms.

;;; make-exchange : (s -> a) -> (b -> t) -> Exchange a b s t
(define (make-exchange getter setter)
  (list 'exchange getter setter))

;;; exchange? : Any -> Boolean
(define (exchange? x)
  (and (pair? x) (eq? (car x) 'exchange)))

;;; exchange-getter : Exchange a b s t -> (s -> a)
(define (exchange-getter e) (list-ref e 1))

;;; exchange-setter : Exchange a b s t -> (b -> t)
(define (exchange-setter e) (list-ref e 2))

;;; exchange-dimap : (a' -> a) -> (b -> b') -> Exchange a b s t -> Exchange a' b' s t
(define (exchange-dimap f g e)
  (make-exchange (lambda (s) (f ((exchange-getter e) s)))
                 (lambda (b-prime) ((exchange-setter e) (g b-prime)))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Function profunctor
;;; (define double (lambda (x) (* x 2)))
;;; (define str->num string->number)
;;; (define show number->string)
;;; (define doubled-str (fn-dimap str->num show double))
;;; (doubled-str "21")  ; => "42"
;;;
;;; ;; Strong profunctor (for lenses)
;;; ((fn-first double) (cons 5 "hello"))  ; => (10 . "hello")
;;;
;;; ;; Choice profunctor (for prisms)
;;; ((fn-left double) (left 5))   ; => (left 10)
;;; ((fn-left double) (right 5))  ; => (right 5)
;;;
;;; ;; Star profunctor with Maybe
;;; (define safe-head
;;;   (make-star (lambda (lst) (if (null? lst) nothing (just (car lst))))))
;;; (run-star safe-head '(1 2 3))  ; => (just 1)
;;; (run-star safe-head '())       ; => nothing
