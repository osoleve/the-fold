;;; lattice/optics/profunctor-optics.ss — Profunctor Optics
;;; @module profunctor-optics
;;; @requires prelude combinators templates optics

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'combinators)
(require 'templates)
(require 'optics)

(doc 'module 'profunctor-optics)
(doc 'purity 'total)
(doc 'description "Profunctor optics: the full optic hierarchy encoded via profunctor constraints. Profunctor p => dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'")
(doc 'layer 'lattice)

(doc 'section 'profunctor-type-class)

(define (make-profunctor dimap-fn)
  (doc 'type '(-> (-> (-> a2 a) (-> b b2) (-> (p a b) (p a2 b2))) (Profunctor p)))
  (doc 'export #t)
  (list 'profunctor
        dimap-fn
        (lambda (f pa) (dimap-fn f identity pa))    ; lmap
        (lambda (g pa) (dimap-fn identity g pa))))  ; rmap

(define (profunctor? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'profunctor)))

(define (profunctor-dimap p)
  (doc 'type '(-> (Profunctor p) (-> (-> a2 a) (-> b b2) (-> (p a b) (p a2 b2)))))
  (doc 'export #t)
  (cadr p))

(define (profunctor-lmap p)
  (doc 'type '(-> (Profunctor p) (-> (-> a2 a) (-> (p a b) (p a2 b)))))
  (doc 'export #t)
  (caddr p))

(define (profunctor-rmap p)
  (doc 'type '(-> (Profunctor p) (-> (-> b b2) (-> (p a b) (p a b2)))))
  (doc 'export #t)
  (cadddr p))

(define (dimap prof f g pa)
  (doc 'type '(-> (Profunctor p) (-> a2 a) (-> b b2) (p a b) (p a2 b2)))
  (doc 'export #t)
  ((profunctor-dimap prof) f g pa))

(define (lmap prof f pa)
  (doc 'type '(-> (Profunctor p) (-> a2 a) (p a b) (p a2 b)))
  (doc 'export #t)
  ((profunctor-lmap prof) f pa))

(define (rmap prof g pa)
  (doc 'type '(-> (Profunctor p) (-> b b2) (p a b) (p a b2)))
  (doc 'export #t)
  ((profunctor-rmap prof) g pa))

(doc 'section 'strong-profunctor)

(define (make-strong prof first-fn)
  (doc 'type '(-> (Profunctor p) (-> (p a b) (p (Pair a c) (Pair b c))) (Strong p)))
  (doc 'export #t)
  (list 'strong
        prof
        first-fn
        ;; psecond derived: swap, pfirst, swap
        (lambda (pab)
          (dimap prof swap swap (first-fn pab)))))

(define (strong? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'strong)))

(define (strong-profunctor s)
  (doc 'type '(-> (Strong p) (Profunctor p)))
  (doc 'export #t)
  (cadr s))

(define (strong-first s)
  (doc 'type '(-> (Strong p) (-> (p a b) (p (Pair a c) (Pair b c)))))
  (doc 'export #t)
  (caddr s))

(define (strong-second s)
  (doc 'type '(-> (Strong p) (-> (p a b) (p (Pair c a) (Pair c b)))))
  (doc 'export #t)
  (cadddr s))

(define (pfirst strong pab)
  (doc 'type '(-> (Strong p) (p a b) (p (Pair a c) (Pair b c))))
  (doc 'export #t)
  ((strong-first strong) pab))

(define (psecond strong pab)
  (doc 'type '(-> (Strong p) (p a b) (p (Pair c a) (Pair c b))))
  (doc 'export #t)
  ((strong-second strong) pab))

(doc 'section 'choice-profunctor)

(define (make-choice prof left-fn)
  (doc 'type '(-> (Profunctor p) (-> (p a b) (p (Either a c) (Either b c))) (Choice p)))
  (doc 'export #t)
  (list 'choice
        prof
        left-fn
        ;; pright derived: swap-either, pleft, swap-either
        (lambda (pab)
          (dimap prof swap-either swap-either (left-fn pab)))))

(define (swap-either e)
  (doc 'type '(-> (Either a b) (Either b a)))
  (if (left? e)
      (right (from-left e))
      (left (from-right e))))

(define (choice? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'choice)))

(define (choice-profunctor c)
  (doc 'type '(-> (Choice p) (Profunctor p)))
  (doc 'export #t)
  (cadr c))

(define (choice-left c)
  (doc 'type '(-> (Choice p) (-> (p a b) (p (Either a c) (Either b c)))))
  (doc 'export #t)
  (caddr c))

(define (choice-right c)
  (doc 'type '(-> (Choice p) (-> (p a b) (p (Either c a) (Either c b)))))
  (doc 'export #t)
  (cadddr c))

(define (pleft choice pab)
  (doc 'type '(-> (Choice p) (p a b) (p (Either a c) (Either b c))))
  (doc 'export #t)
  ((choice-left choice) pab))

(define (pright choice pab)
  (doc 'type '(-> (Choice p) (p a b) (p (Either c a) (Either c b))))
  (doc 'export #t)
  ((choice-right choice) pab))

(doc 'section 'closed-profunctor)

(define (make-closed prof closed-fn)
  (doc 'type '(-> (Profunctor p) (-> (p a b) (p (-> x a) (-> x b))) (Closed p)))
  (doc 'export #t)
  (list 'closed prof closed-fn))

(define (closed-profunctor? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'closed)))

(define (closed-profunctor c)
  (doc 'type '(-> (Closed p) (Profunctor p)))
  (doc 'export #t)
  (cadr c))

(define (closed-fn c)
  (doc 'type '(-> (Closed p) (-> (p a b) (p (-> x a) (-> x b)))))
  (doc 'export #t)
  (caddr c))

(define (pclosed closed pab)
  (doc 'type '(-> (Closed p) (p a b) (p (-> x a) (-> x b))))
  (doc 'export #t)
  ((closed-fn closed) pab))

(doc 'section 'wander-profunctor)

(define (make-wander strong choice wander-fn)
  (doc 'type '(-> (Strong p) (Choice p) (-> Traverser (p a b) (p s t)) (Wander p)))
  (doc 'export #t)
  (list 'wander strong choice wander-fn))

(define (wander? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'wander)))

(define (wander-strong w)
  (doc 'type '(-> (Wander p) (Strong p)))
  (doc 'export #t)
  (cadr w))

(define (wander-choice w)
  (doc 'type '(-> (Wander p) (Choice p)))
  (doc 'export #t)
  (caddr w))

(define (wander-wander w)
  (doc 'type '(-> (Wander p) (-> Traverser (p a b) (p s t))))
  (doc 'export #t)
  (cadddr w))

(define (pwander wander traverser pab)
  (doc 'type '(-> (Wander p) Traverser (p a b) (p s t)))
  (doc 'export #t)
  ((wander-wander wander) traverser pab))

(doc 'section 'profunctor-instances)

(doc profunctor-fn 'type '(Profunctor (->)))
(doc profunctor-fn 'description "The function arrow as a profunctor: dimap f g h = g . h . f")
(doc profunctor-fn 'export #t)
(define profunctor-fn
  (make-profunctor
   (lambda (f g h)
     (compose2 g (compose2 h f)))))

(doc strong-fn 'type '(Strong (->)))
(doc strong-fn 'export #t)
(define strong-fn
  (make-strong
   profunctor-fn
   (lambda (f)
     (lambda (pair)
       (cons (f (car pair)) (cdr pair))))))

(doc choice-fn 'type '(Choice (->)))
(doc choice-fn 'export #t)
(define choice-fn
  (make-choice
   profunctor-fn
   (lambda (f)
     (lambda (e)
       (if (left? e)
           (left (f (from-left e)))
           e)))))

(doc closed-fn-instance 'type '(Closed (->)))
(doc closed-fn-instance 'export #t)
(define closed-fn-instance
  (make-closed
   profunctor-fn
   (lambda (f)
     (lambda (g)
       (compose2 f g)))))

(doc wander-fn 'type '(Wander (->)))
(doc wander-fn 'export #t)
(define wander-fn
  (make-wander
   strong-fn
   choice-fn
   (lambda (traverse-fn f)
     ;; traverse-fn : (a -> Id b) -> s -> Id t
     ;; For identity functor, this is just (a -> b) -> s -> t
     (lambda (s) (traverse-fn f s)))))

(doc 'section 'forget-profunctor)

(define (make-forget f)
  (doc 'type '(-> (-> a r) (Forget r a b)))
  (doc 'export #t)
  (list 'forget f))

(define (forget? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'forget)))

(define (run-forget fg)
  (doc 'type '(-> (Forget r a b) (-> a r)))
  (doc 'export #t)
  (cadr fg))

(doc profunctor-forget 'type '(Profunctor (Forget r)))
(doc profunctor-forget 'description "Forget profunctor: dimap f g (Forget h) = Forget (h . f), ignoring g")
(doc profunctor-forget 'export #t)
(define profunctor-forget
  (make-profunctor
   (lambda (f g fg)
     (make-forget (compose2 (run-forget fg) f)))))

(doc strong-forget 'type '(Strong (Forget r)))
(define strong-forget
  (make-strong
   profunctor-forget
   (lambda (fg)
     (make-forget (compose2 (run-forget fg) car)))))

(doc 'section 'tagged-profunctor)

(define (make-tagged val)
  (doc 'type '(-> b (Tagged a b)))
  (doc 'export #t)
  (list 'tagged val))

(define (tagged? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'tagged)))

(define (run-tagged tg)
  (doc 'type '(-> (Tagged a b) b))
  (doc 'export #t)
  (cadr tg))

(doc profunctor-tagged 'type '(Profunctor Tagged))
(doc profunctor-tagged 'description "Tagged profunctor: dimap f g (Tagged b) = Tagged (g b), ignoring f")
(doc profunctor-tagged 'export #t)
(define profunctor-tagged
  (make-profunctor
   (lambda (f g tg)
     (make-tagged (g (run-tagged tg))))))

(doc choice-tagged 'type '(Choice Tagged))
(doc choice-tagged 'export #t)
(define choice-tagged
  (make-choice
   profunctor-tagged
   (lambda (tg)
     (make-tagged (left (run-tagged tg))))))

(doc 'section 'star-profunctor)

(define (make-star functor run-fn)
  (doc 'type '(-> (FunctorDict f) (-> a (f b)) (Star f a b)))
  (list 'star functor run-fn))

(define (star? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'star)))

(define (star-functor s)
  (doc 'type '(-> (Star f a b) (FunctorDict f)))
  (cadr s))

(define (run-star s)
  (doc 'type '(-> (Star f a b) (-> a (f b))))
  (caddr s))

(define (profunctor-star functor)
  (doc 'type '(-> (FunctorDict f) (Profunctor (Star f))))
  (make-profunctor
   (lambda (f g st)
     (make-star functor
                (compose2 (lambda (fb) ((functor-fmap functor) g fb))
                          (compose2 (run-star st) f))))))

(doc 'section 'profunctor-optics)

(define (make-p-iso forward backward)
  (doc 'type '(-> (-> s a) (-> b t) (PIso s t a b)))
  (doc 'export #t)
  (list 'p-iso forward backward))

(define (p-iso? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-iso)))

(define (p-iso-forward iso)
  (doc 'type '(-> (PIso s t a b) (-> s a)))
  (doc 'export #t)
  (cadr iso))

(define (p-iso-backward iso)
  (doc 'type '(-> (PIso s t a b) (-> b t)))
  (doc 'export #t)
  (caddr iso))

(define (run-p-iso prof iso pab)
  (doc 'type '(-> (Profunctor p) (PIso s t a b) (p a b) (p s t)))
  (dimap prof (p-iso-forward iso) (p-iso-backward iso) pab))

(define (p-iso-compose outer inner)
  (doc 'type '(-> (PIso s t a b) (PIso a b c d) (PIso s t c d)))
  (doc 'export #t)
  (make-p-iso
   (compose2 (p-iso-forward inner) (p-iso-forward outer))
   (compose2 (p-iso-backward outer) (p-iso-backward inner))))

(doc 'section 'profunctor-lens)

(define (make-p-lens getter setter)
  (doc 'type '(-> (-> s a) (-> s (-> b t)) (PLens s t a b)))
  (doc 'export #t)
  (list 'p-lens getter setter))

(define (p-lens? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-lens)))

(define (p-lens-getter lens)
  (doc 'type '(-> (PLens s t a b) (-> s a)))
  (doc 'export #t)
  (cadr lens))

(define (p-lens-setter lens)
  (doc 'type '(-> (PLens s t a b) (-> s (-> b t))))
  (doc 'export #t)
  (caddr lens))

(define (run-p-lens strong lens pab)
  (doc 'type '(-> (Strong p) (PLens s t a b) (p a b) (p s t)))
  (let* ([prof (strong-profunctor strong)]
         [get (p-lens-getter lens)]
         [set (p-lens-setter lens)]
         [split (lambda (s) (cons (get s) s))]
         [combine (lambda (bs) ((set (cdr bs)) (car bs)))])
    (dimap prof split combine (pfirst strong pab))))

(define (p-lens-compose outer inner)
  (doc 'type '(-> (PLens s t a b) (PLens a b c d) (PLens s t c d)))
  (doc 'export #t)
  (make-p-lens
   (compose2 (p-lens-getter inner) (p-lens-getter outer))
   (lambda (s)
     (lambda (d)
       (let* ([a ((p-lens-getter outer) s)]
              [b (((p-lens-setter inner) a) d)])
         (((p-lens-setter outer) s) b))))))

(doc 'section 'profunctor-prism)

(define (make-p-prism match build)
  (doc 'type '(-> (-> s (Either t a)) (-> b t) (PPrism s t a b)))
  (doc 'export #t)
  (list 'p-prism match build))

(define (p-prism? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-prism)))

(define (p-prism-match prism)
  (doc 'type '(-> (PPrism s t a b) (-> s (Either t a))))
  (doc 'export #t)
  (cadr prism))

(define (p-prism-build prism)
  (doc 'type '(-> (PPrism s t a b) (-> b t)))
  (doc 'export #t)
  (caddr prism))

(define (run-p-prism choice prism pab)
  (doc 'type '(-> (Choice p) (PPrism s t a b) (p a b) (p s t)))
  (let* ([prof (choice-profunctor choice)]
         [match (p-prism-match prism)]
         [build (p-prism-build prism)]
         [merge (lambda (e)
                  (if (left? e) (from-left e) (build (from-right e))))])
    (dimap prof match merge (pright choice pab))))

(define (p-prism-compose outer inner)
  (doc 'type '(-> (PPrism s t a b) (PPrism a b c d) (PPrism s t c d)))
  (doc 'export #t)
  (make-p-prism
   (lambda (s)
     (let ([e ((p-prism-match outer) s)])
       (if (left? e)
           e
           (let ([a (from-right e)])
             (let ([e2 ((p-prism-match inner) a)])
               (if (left? e2)
                   (left ((p-prism-build outer) (from-left e2)))
                   e2))))))
   (compose2 (p-prism-build outer) (p-prism-build inner))))

(doc 'section 'profunctor-affine)

(define (make-p-affine preview set)
  (doc 'type '(-> (-> s (Either t a)) (-> s (-> b t)) (PAffine s t a b)))
  (doc 'export #t)
  (list 'p-affine preview set))

(define (p-affine? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-affine)))

(define (p-affine-preview affine)
  (doc 'type '(-> (PAffine s t a b) (-> s (Either t a))))
  (doc 'export #t)
  (cadr affine))

(define (p-affine-set affine)
  (doc 'type '(-> (PAffine s t a b) (-> s (-> b t))))
  (doc 'export #t)
  (caddr affine))

(define (run-p-affine strong choice affine pab)
  (doc 'type '(-> (Strong p) (Choice p) (PAffine s t a b) (p a b) (p s t)))
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

(define (p-affine-compose outer inner)
  (doc 'type '(-> (PAffine s t a b) (PAffine a b c d) (PAffine s t c d)))
  (doc 'export #t)
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

(doc 'section 'profunctor-traversal)

(define (make-p-traversal traverse-fn fold-fn)
  (doc 'type '(-> (-> (-> a b) s t) (-> s (List a)) (PTraversal s t a b)))
  (doc 'export #t)
  (list 'p-traversal traverse-fn fold-fn))

(define (p-traversal? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-traversal)))

(define (p-traversal-traverse ptrav)
  (doc 'type '(-> (PTraversal s t a b) (-> (-> a b) s t)))
  (doc 'export #t)
  (cadr ptrav))

(define (p-traversal-fold-fn ptrav)
  (doc 'type '(-> (PTraversal s t a b) (-> s (List a))))
  (doc 'export #t)
  (caddr ptrav))

(define (run-p-traversal wander ptrav pab)
  (doc 'type '(-> (Wander p) (PTraversal s t a b) (p a b) (p s t)))
  (doc 'export #t)
  (pwander wander (p-traversal-traverse ptrav) pab))

(define (p-traversal-to-list ptrav s)
  (doc 'type '(-> (PTraversal s t a b) s (List a)))
  (doc 'export #t)
  ((p-traversal-fold-fn ptrav) s))

(define (p-traversal-over ptrav f s)
  (doc 'type '(-> (PTraversal s t a b) (-> a b) s t))
  (doc 'export #t)
  ((p-traversal-traverse ptrav) f s))

(define (p-traversal-set ptrav b s)
  (doc 'type '(-> (PTraversal s t a b) b s t))
  (doc 'export #t)
  (p-traversal-over ptrav (const b) s))

(define (p-traversal-compose outer inner)
  (doc 'type '(-> (PTraversal s t a b) (PTraversal a b c d) (PTraversal s t c d)))
  (doc 'export #t)
  (make-p-traversal
   ;; traverse: (c -> d) -> s -> t
   (lambda (f s)
     ((p-traversal-traverse outer)
      (lambda (a) ((p-traversal-traverse inner) f a))
      s))
   ;; fold: s -> List c
   (lambda (s)
     (append-map (p-traversal-fold-fn inner)
                 ((p-traversal-fold-fn outer) s)))))

(doc p-traversal-each 'type '(PTraversal (List a) (List b) a b))
(doc p-traversal-each 'export #t)
(define p-traversal-each
  (make-p-traversal
   (lambda (f xs) (map f xs))
   identity))

(doc p-traversal-both 'type '(PTraversal (Pair a a) (Pair b b) a b))
(doc p-traversal-both 'export #t)
(define p-traversal-both
  (make-p-traversal
   (lambda (f p) (cons (f (car p)) (f (cdr p))))
   (lambda (p) (list (car p) (cdr p)))))

(define (p-traversal-filtered pred)
  (doc 'type '(-> (-> a Boolean) (PTraversal (List a) (List a) a a)))
  (doc 'export #t)
  (make-p-traversal
   (lambda (f xs)
     (map (lambda (x) (if (pred x) (f x) x)) xs))
   (lambda (xs) (filter pred xs))))

(doc 'section 'profunctor-fold)

(define (make-p-fold fold-fn)
  (doc 'type '(-> (-> s (List a)) (PFold s a)))
  (doc 'export #t)
  (list 'p-fold fold-fn))

(define (p-fold? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-fold)))

(define (p-fold-fn pfold)
  (doc 'type '(-> (PFold s a) (-> s (List a))))
  (doc 'export #t)
  (cadr pfold))

(define (p-fold-to-list pfold s)
  (doc 'type '(-> (PFold s a) s (List a)))
  (doc 'export #t)
  ((p-fold-fn pfold) s))

(define (p-fold-preview pfold s)
  (doc 'type '(-> (PFold s a) s (Maybe a)))
  (doc 'export #t)
  (let ([targets ((p-fold-fn pfold) s)])
    (if (null? targets) nothing (just (car targets)))))

(define (p-fold-has pfold s)
  (doc 'type '(-> (PFold s a) s Boolean))
  (doc 'export #t)
  (not (null? ((p-fold-fn pfold) s))))

(define (p-fold-length pfold s)
  (doc 'type '(-> (PFold s a) s Nat))
  (doc 'export #t)
  (length ((p-fold-fn pfold) s)))

(define (p-fold-all pfold pred s)
  (doc 'type '(-> (PFold s a) (-> a Boolean) s Boolean))
  (doc 'export #t)
  (andmap pred ((p-fold-fn pfold) s)))

(define (p-fold-any pfold pred s)
  (doc 'type '(-> (PFold s a) (-> a Boolean) s Boolean))
  (doc 'export #t)
  (ormap pred ((p-fold-fn pfold) s)))

(define (p-fold-compose outer inner)
  (doc 'type '(-> (PFold s a) (PFold a b) (PFold s b)))
  (doc 'export #t)
  (make-p-fold
   (lambda (s)
     (append-map (p-fold-fn inner)
                 ((p-fold-fn outer) s)))))

(doc p-fold-each 'type '(PFold (List a) a))
(doc p-fold-each 'export #t)
(define p-fold-each
  (make-p-fold identity))

(define (p-fold-filtered pred)
  (doc 'type '(-> (-> a Boolean) (PFold (List a) a)))
  (doc 'export #t)
  (make-p-fold (lambda (xs) (filter pred xs))))

(define (p-fold-taking n)
  (doc 'type '(-> Nat (PFold (List a) a)))
  (doc 'export #t)
  (make-p-fold (lambda (xs) (take n xs))))

(doc 'section 'optic-conversions)

(define (traversal->p-traversal trav)
  (doc 'type '(-> Traversal PTraversal))
  (doc 'export #t)
  (make-p-traversal
   (traversal-traverse trav)
   (traversal-fold trav)))

(define (p-traversal->traversal ptrav)
  (doc 'type '(-> PTraversal Traversal))
  (doc 'export #t)
  (make-traversal
   (p-traversal-traverse ptrav)
   (p-traversal-fold-fn ptrav)))

(define (p-lens->p-traversal plens)
  (doc 'type '(-> (PLens s t a b) (PTraversal s t a b)))
  (doc 'export #t)
  (make-p-traversal
   (lambda (f s)
     (((p-lens-setter plens) s) (f ((p-lens-getter plens) s))))
   (lambda (s) (list ((p-lens-getter plens) s)))))

(define (p-prism->p-traversal pprism)
  (doc 'type '(-> (PPrism s t a b) (PTraversal s t a b)))
  (doc 'export #t)
  (make-p-traversal
   (lambda (f s)
     (let ([e ((p-prism-match pprism) s)])
       (if (left? e)
           (from-left e)
           ((p-prism-build pprism) (f (from-right e))))))
   (lambda (s)
     (let ([e ((p-prism-match pprism) s)])
       (if (left? e) '() (list (from-right e)))))))

(define (p-affine->p-traversal paffine)
  (doc 'type '(-> (PAffine s t a b) (PTraversal s t a b)))
  (doc 'export #t)
  (make-p-traversal
   (lambda (f s)
     (let ([e ((p-affine-preview paffine) s)])
       (if (left? e)
           (from-left e)
           (((p-affine-set paffine) s) (f (from-right e))))))
   (lambda (s)
     (let ([e ((p-affine-preview paffine) s)])
       (if (left? e) '() (list (from-right e)))))))

(define (p-traversal->p-fold ptrav)
  (doc 'type '(-> (PTraversal s t a b) (PFold s a)))
  (doc 'export #t)
  (make-p-fold (p-traversal-fold-fn ptrav)))

(define (p-lens->p-fold plens)
  (doc 'type '(-> (PLens s t a b) (PFold s a)))
  (doc 'export #t)
  (make-p-fold (lambda (s) (list ((p-lens-getter plens) s)))))

(define (p-prism->p-fold pprism)
  (doc 'type '(-> (PPrism s t a b) (PFold s a)))
  (doc 'export #t)
  (make-p-fold
   (lambda (s)
     (let ([e ((p-prism-match pprism) s)])
       (if (right? e) (list (from-right e)) '())))))

(doc 'section 'profunctor-grate)

(define (make-p-grate cotraverse-fn)
  (doc 'type '(-> (-> (-> (-> s a) b) t) (PGrate s t a b)))
  (doc 'export #t)
  (list 'p-grate cotraverse-fn))

(define (p-grate? x)
  (doc 'type '(-> Any Boolean))
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'p-grate)))

(define (p-grate-cotraverse grate)
  (doc 'type '(-> (PGrate s t a b) (-> (-> (-> s a) b) t)))
  (doc 'export #t)
  (cadr grate))

(define (run-p-grate closed grate pab)
  (doc 'type '(-> (Closed p) (PGrate s t a b) (p a b) (p s t)))
  (doc 'export #t)
  (let* ([prof (closed-profunctor closed)]
         [cotr (p-grate-cotraverse grate)]
         [tabling (lambda (s) (lambda (sa) (sa s)))])
    (dimap prof tabling cotr (pclosed closed pab))))

(define (p-grate-compose outer inner)
  (doc 'type '(-> (PGrate s t a b) (PGrate a b c d) (PGrate s t c d)))
  (doc 'export #t)
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

(doc 'section 'concrete-to-profunctor)

(define (iso->p-iso iso)
  (doc 'type '(-> Iso PIso))
  (doc 'export #t)
  (make-p-iso (iso-forward iso) (iso-backward iso)))

(define (lens->p-lens lens)
  (doc 'type '(-> Lens PLens))
  (doc 'export #t)
  (make-p-lens
   (lens-getter lens)
   (lambda (s)
     (lambda (b)
       ((lens-setter lens) b s)))))

(define (prism->p-prism prism)
  (doc 'type '(-> Prism PPrism))
  (doc 'export #t)
  (make-p-prism
   (lambda (s)
     (let ([m ((prism-match prism) s)])
       (if (nothing? m)
           (left s)  ; Return original on failure
           (right (from-just m)))))
   (prism-build prism)))

(define (affine->p-affine affine)
  (doc 'type '(-> Affine PAffine))
  (doc 'export #t)
  (make-p-affine
   (lambda (s)
     (let ([m ((affine-getter affine) s)])
       (if (nothing? m)
           (left s)
           (right (from-just m)))))
   (lambda (s)
     (lambda (b)
       ((affine-setter affine) b s)))))

(define (grate->p-grate grate)
  (doc 'type '(-> Grate PGrate))
  (doc 'export #t)
  (make-p-grate (grate-cotraverse-fn grate)))

(doc 'section 'profunctor-to-concrete)

(define (p-iso->iso piso)
  (doc 'type '(-> PIso Iso))
  (doc 'export #t)
  (make-iso (p-iso-forward piso) (p-iso-backward piso)))

(define (p-lens->lens plens)
  (doc 'type '(-> PLens Lens))
  (doc 'export #t)
  (make-lens
   (p-lens-getter plens)
   (lambda (b s) ((p-lens-setter plens s) b))))

(define (p-prism->prism pprism)
  (doc 'type '(-> PPrism Prism))
  (doc 'export #t)
  (make-prism
   (lambda (s)
     (let ([e ((p-prism-match pprism) s)])
       (if (right? e)
           (just (from-right e))
           nothing)))
   (p-prism-build pprism)))

(define (p-affine->affine paffine)
  (doc 'type '(-> PAffine Affine))
  (doc 'export #t)
  (make-affine
   (lambda (s)
     (let ([e ((p-affine-preview paffine) s)])
       (if (right? e)
           (just (from-right e))
           nothing)))
   (lambda (b s) ((p-affine-set paffine s) b))))

(define (p-grate->grate pgrate)
  (doc 'type '(-> PGrate Grate))
  (doc 'export #t)
  (make-grate (p-grate-cotraverse pgrate)))

(doc 'section 'optic-operations)

(define (p-view plens s)
  (doc 'type '(-> (PLens s t a b) s a))
  (doc 'export #t)
  ((run-forget
    (run-p-lens strong-forget plens (make-forget identity)))
   s))

(define (p-over plens f s)
  (doc 'type '(-> (PLens s t a b) (-> a b) s t))
  (doc 'export #t)
  ((run-p-lens strong-fn plens f) s))

(define (p-set plens b s)
  (doc 'type '(-> (PLens s t a b) b s t))
  (doc 'export #t)
  (p-over plens (const b) s))

(define (p-preview pprism s)
  (doc 'type '(-> (PPrism s t a b) s (Maybe a)))
  (doc 'export #t)
  (let ([e ((p-prism-match pprism) s)])
    (if (right? e)
        (just (from-right e))
        nothing)))

(define (p-review pprism b)
  (doc 'type '(-> (PPrism s t a b) b t))
  (doc 'export #t)
  ((p-prism-build pprism) b))

(define (p-prism-over pprism f s)
  (doc 'type '(-> (PPrism s t a b) (-> a b) s t))
  ((run-p-prism choice-fn pprism f) s))

(define (p-affine-preview-fn paffine s)
  (doc 'type '(-> (PAffine s t a b) s (Maybe a)))
  (let ([e ((p-affine-preview paffine) s)])
    (if (right? e)
        (just (from-right e))
        nothing)))

(define (p-affine-set-fn paffine b s)
  (doc 'type '(-> (PAffine s t a b) b s t))
  (doc 'export #t)
  (((p-affine-set paffine) s) b))

(define (p-grate-review pgrate b)
  (doc 'type '(-> (PGrate s t a b) b t))
  (doc 'export #t)
  ((p-grate-cotraverse pgrate) (lambda (_) b)))

(define (p-grate-over pgrate f s)
  (doc 'type '(-> (PGrate s t a b) (-> a b) s t))
  (doc 'export #t)
  ((run-p-grate closed-fn-instance pgrate f) s))

(define (p-grate-zipWith pgrate f s1 s2)
  (doc 'type '(-> (PGrate s t a b) (-> a a b) s s t))
  (doc 'export #t)
  ((p-grate-cotraverse pgrate)
   (lambda (sa) (f (sa s1) (sa s2)))))

(doc 'section 'common-optics)

(doc p-iso-id 'type '(PIso a b a b))
(doc p-iso-id 'export #t)
(define p-iso-id
  (make-p-iso identity identity))

(doc p-iso-swapped 'type '(PIso (Pair a b) (Pair c d) (Pair b a) (Pair d c)))
(doc p-iso-swapped 'export #t)
(define p-iso-swapped
  (make-p-iso swap swap))

(doc p-iso-reversed 'type '(PIso (List a) (List b) (List a) (List b)))
(define p-iso-reversed
  (make-p-iso reverse reverse))

(doc p-iso-curried 'type '(PIso (-> (Pair a b) c) (-> (Pair a2 b2) c2) (-> a (-> b c)) (-> a2 (-> b2 c2))))
(define p-iso-curried
  (make-p-iso
   (lambda (f) (lambda (a) (lambda (b) (f (cons a b)))))
   (lambda (f) (lambda (ab) ((f (car ab)) (cdr ab))))))

(doc p-lens-fst 'type '(PLens (Pair a c) (Pair b c) a b))
(doc p-lens-fst 'export #t)
(define p-lens-fst
  (make-p-lens
   car
   (lambda (s) (lambda (b) (cons b (cdr s))))))

(doc p-lens-snd 'type '(PLens (Pair c a) (Pair c b) a b))
(doc p-lens-snd 'export #t)
(define p-lens-snd
  (make-p-lens
   cdr
   (lambda (s) (lambda (b) (cons (car s) b)))))

(doc p-lens-head 'type '(PLens (List a) (List a) a a))
(define p-lens-head
  (make-p-lens
   car
   (lambda (s) (lambda (a) (cons a (cdr s))))))

(define (p-lens-nth n)
  (doc 'type '(-> Nat (PLens (List a) (List a) a a)))
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

(doc p-prism-just 'type '(PPrism (Maybe a) (Maybe b) a b))
(doc p-prism-just 'export #t)
(define p-prism-just
  (make-p-prism
   (lambda (m)
     (if (just? m)
         (right (from-just m))
         (left nothing)))
   just))

(doc p-prism-left 'type '(PPrism (Either a c) (Either b c) a b))
(doc p-prism-left 'export #t)
(define p-prism-left
  (make-p-prism
   (lambda (e)
     (if (left? e)
         (right (from-left e))
         (left e)))  ; Return original Right
   left))

(doc p-prism-right 'type '(PPrism (Either c a) (Either c b) a b))
(doc p-prism-right 'export #t)
(define p-prism-right
  (make-p-prism
   (lambda (e)
     (if (right? e)
         (right (from-right e))
         (left e)))  ; Return original Left
   right))

(doc p-prism-nil 'type '(PPrism (List a) (List a) () ()))
(define p-prism-nil
  (make-p-prism
   (lambda (xs)
     (if (null? xs)
         (right '())
         (left xs)))
   (const '())))

(doc p-prism-cons 'type '(PPrism (List a) (List a) (Pair a (List a)) (Pair a (List a))))
(define p-prism-cons
  (make-p-prism
   (lambda (xs)
     (if (null? xs)
         (left '())
         (right (cons (car xs) (cdr xs)))))
   (lambda (p) (cons (car p) (cdr p)))))

(define (p-affine-nth n)
  (doc 'type '(-> Nat (PAffine (List a) (List a) a a)))
  (doc 'export #t)
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

(doc p-grate-id 'type '(PGrate a b a b))
(doc p-grate-id 'export #t)
(define p-grate-id
  (make-p-grate
   (lambda (satob) (satob identity))))

(doc p-grate-fn 'type '(PGrate (-> x a) (-> x b) a b))
(doc p-grate-fn 'export #t)
(define p-grate-fn
  (make-p-grate
   (lambda (satob)
     (lambda (x)
       (satob (lambda (f) (f x)))))))

(doc p-grate-pair-same 'type '(PGrate (Pair a a) (Pair b b) a b))
(doc p-grate-pair-same 'export #t)
(define p-grate-pair-same
  (make-p-grate
   (lambda (satob)
     (cons
      (satob car)
      (satob cdr)))))

(define (p-grate-list-rep n)
  (doc 'type '(-> Nat (PGrate (List a) (List b) a b)))
  (doc 'export #t)
  (make-p-grate
   (lambda (satob)
     (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (satob (lambda (xs) (list-ref xs i))) acc)))))))

(doc 'section 'unified-composition)

(define (p-optic-type o)
  (doc 'type '(-> POptic Symbol))
  (doc 'export #t)
  (if (pair? o) (car o) 'unknown))

(define (p-optic-compose outer inner)
  (doc 'type '(-> POptic POptic POptic))
  (doc 'description "Compose two profunctor optics with automatic type inference based on optic tags")
  (doc 'export #t)
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

      ;; Traversal compositions
      [(and (eq? t1 'p-traversal) (eq? t2 'p-traversal))
       (p-traversal-compose outer inner)]

      [(and (eq? t1 'p-lens) (eq? t2 'p-traversal))
       (p-traversal-compose (p-lens->p-traversal outer) inner)]

      [(and (eq? t1 'p-traversal) (eq? t2 'p-lens))
       (p-traversal-compose outer (p-lens->p-traversal inner))]

      [(and (eq? t1 'p-prism) (eq? t2 'p-traversal))
       (p-traversal-compose (p-prism->p-traversal outer) inner)]

      [(and (eq? t1 'p-traversal) (eq? t2 'p-prism))
       (p-traversal-compose outer (p-prism->p-traversal inner))]

      [(and (eq? t1 'p-affine) (eq? t2 'p-traversal))
       (p-traversal-compose (p-affine->p-traversal outer) inner)]

      [(and (eq? t1 'p-traversal) (eq? t2 'p-affine))
       (p-traversal-compose outer (p-affine->p-traversal inner))]

      [(and (eq? t1 'p-iso) (eq? t2 'p-traversal))
       (p-traversal-compose (p-lens->p-traversal (p-iso->p-lens outer)) inner)]

      [(and (eq? t1 'p-traversal) (eq? t2 'p-iso))
       (p-traversal-compose outer (p-lens->p-traversal (p-iso->p-lens inner)))]

      ;; Fold compositions (always produce a fold)
      [(and (eq? t1 'p-fold) (eq? t2 'p-fold))
       (p-fold-compose outer inner)]

      [(eq? t1 'p-fold)
       (p-fold-compose outer (->p-fold inner))]

      [(eq? t2 'p-fold)
       (p-fold-compose (->p-fold outer) inner)]

      ;; Traversal + Fold = Fold
      [(and (eq? t1 'p-traversal) (eq? t2 'p-fold))
       (p-fold-compose (p-traversal->p-fold outer) inner)]

      [else (error 'p-optic-compose "Unsupported composition")])))

(define (->p-fold o)
  (doc 'type '(-> POptic (PFold s a)))
  (doc 'export #t)
  (case (p-optic-type o)
    [(p-fold) o]
    [(p-traversal) (p-traversal->p-fold o)]
    [(p-lens) (p-lens->p-fold o)]
    [(p-prism) (p-prism->p-fold o)]
    [(p-affine) (p-traversal->p-fold (p-affine->p-traversal o))]
    [(p-iso) (p-lens->p-fold (p-iso->p-lens o))]
    [else (error '->p-fold "Cannot convert to fold")]))

(define (p-iso->p-lens piso)
  (doc 'type '(-> PIso PLens))
  (doc 'export #t)
  (make-p-lens
   (p-iso-forward piso)
   (lambda (s)
     (lambda (b)
       ((p-iso-backward piso) b)))))

(define (p-iso->p-grate piso)
  (doc 'type '(-> PIso PGrate))
  (doc 'export #t)
  (make-p-grate
   (lambda (satob)
     ((p-iso-backward piso)
      (satob (p-iso-forward piso))))))

(define (p-lens->p-affine plens)
  (doc 'type '(-> PLens PAffine))
  (doc 'export #t)
  (make-p-affine
   (lambda (s) (right ((p-lens-getter plens) s)))
   (p-lens-setter plens)))

(define (p-prism->p-affine pprism)
  (doc 'type '(-> PPrism PAffine))
  (doc 'export #t)
  (make-p-affine
   (p-prism-match pprism)
   (lambda (s)
     (lambda (b)
       (let ([e ((p-prism-match pprism) s)])
         (if (left? e)
             (from-left e)
             ((p-prism-build pprism) b)))))))

(doc 'section 'law-verification)

(define (verify-profunctor-laws prof test-pabs)
  (doc 'type '(-> (Profunctor p) (List (p a b)) Boolean))
  (let ([dmap (profunctor-dimap prof)])
    (andmap
     (lambda (pab)
       ;; dimap id id = id
       (equal? (dmap identity identity pab) pab))
     test-pabs)))

(define (verify-p-iso-laws piso test-ss test-bs)
  (doc 'type '(-> (PIso s t a b) (List s) (List b) Boolean))
  (let ([fwd (p-iso-forward piso)]
        [bwd (p-iso-backward piso)])
    (and
     ;; forward . backward = id on b
     (andmap (lambda (b) (equal? (fwd (bwd b)) b)) test-bs)
     ;; backward . forward = id on s
     (andmap (lambda (s) (equal? (bwd (fwd s)) s)) test-ss))))

(define (verify-p-lens-laws plens test-pairs)
  (doc 'type '(-> (PLens s t a b) (List (Pair s b)) Boolean))
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

(define (verify-p-prism-laws pprism test-bs test-ss)
  (doc 'type '(-> (PPrism s t a b) (List b) (List s) Boolean))
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

(doc 'section 'exports)

