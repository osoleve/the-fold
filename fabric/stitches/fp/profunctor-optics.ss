;;; fabric/stitches/fp/profunctor-optics.ss — Profunctor Optics
;;;
;;; Optics using profunctor encoding where:
;;;   Optic p s t a b = p a b -> p s t
;;;
;;; Constraints on p determine optic type:
;;;   Iso:       Profunctor p
;;;   Lens:      Strong p
;;;   Prism:     Choice p
;;;   Affine:    Strong p, Choice p
;;;   Traversal: Wander p
;;;
;;; Key benefit: composition is ordinary function composition!
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Profunctor optics with unified composition
;;;   - Bundled typeclass dictionaries
;;;   - At/Ixed typeclasses for indexed access
;;;   - Sequence prisms (Empty, Cons, Snoc)
;;;   - Migration utilities from van Laarhoven encoding
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/profunctor.ss
;;;   - fp/traversable.ss (for Wander)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/profunctor.ss")
(load "fabric/stitches/fp/traversable.ss")

;;; ============================================================
;;; Typeclass Dictionaries
;;; ============================================================
;;;
;;; We bundle profunctor operations into dictionaries for clean
;;; passing to optics.

;;; Profunctor dictionary: (dimap, lmap, rmap)
(define (make-profunctor-dict name dimap lmap rmap)
  (list 'profunctor-dict name dimap lmap rmap))

(define (profunctor-dict? x)
  (and (pair? x) (eq? (car x) 'profunctor-dict)))

(define (profunctor-dict-name d) (list-ref d 1))
(define (profunctor-dict-dimap d) (list-ref d 2))
(define (profunctor-dict-lmap d) (list-ref d 3))
(define (profunctor-dict-rmap d) (list-ref d 4))

;;; Strong dictionary: profunctor + (first*, second*)
(define (make-strong-dict profunctor first* second*)
  (list 'strong-dict profunctor first* second*))

(define (strong-dict? x)
  (and (pair? x) (eq? (car x) 'strong-dict)))

(define (strong-dict-profunctor d) (list-ref d 1))
(define (strong-dict-first* d) (list-ref d 2))
(define (strong-dict-second* d) (list-ref d 3))

;;; Choice dictionary: profunctor + (left*, right*)
(define (make-choice-dict profunctor left* right*)
  (list 'choice-dict profunctor left* right*))

(define (choice-dict? x)
  (and (pair? x) (eq? (car x) 'choice-dict)))

(define (choice-dict-profunctor d) (list-ref d 1))
(define (choice-dict-left* d) (list-ref d 2))
(define (choice-dict-right* d) (list-ref d 3))

;;; Affine dictionary: Strong + Choice
(define (make-affine-dict strong choice)
  (list 'affine-dict strong choice))

(define (affine-dict? x)
  (and (pair? x) (eq? (car x) 'affine-dict)))

(define (affine-dict-strong d) (list-ref d 1))
(define (affine-dict-choice d) (list-ref d 2))

;;; Wander dictionary: Strong + wander operation
;;; wander : ((a -> f b) -> s -> f t) -> p a b -> p s t
(define (make-wander-dict strong wander)
  (list 'wander-dict strong wander))

(define (wander-dict? x)
  (and (pair? x) (eq? (car x) 'wander-dict)))

(define (wander-dict-strong d) (list-ref d 1))
(define (wander-dict-wander d) (list-ref d 2))

;;; ============================================================
;;; Standard Profunctor Instances
;;; ============================================================

;;; Function profunctor (->)
(define fn-profunctor-dict
  (make-profunctor-dict 'function fn-dimap fn-lmap fn-rmap))

(define fn-strong-dict
  (make-strong-dict fn-profunctor-dict fn-first fn-second))

(define fn-choice-dict
  (make-choice-dict fn-profunctor-dict fn-left fn-right))

(define fn-affine-dict
  (make-affine-dict fn-strong-dict fn-choice-dict))

;;; ============================================================
;;; Forget Profunctor (for viewing)
;;; ============================================================
;;;
;;; Forget r a b = a -> r
;;; Used to extract values from optics.

(define (make-forget run-fn)
  (list 'forget run-fn))

(define (forget? x)
  (and (pair? x) (eq? (car x) 'forget)))

(define (run-forget f a)
  ((list-ref f 1) a))

;;; forget-dimap : (a* -> a) -> (b -> b*) -> Forget r a b -> Forget r a* b*
(define (forget-dimap f g fgt)
  (make-forget (lambda (x) (run-forget fgt (f x)))))

;;; forget-lmap : (a* -> a) -> Forget r a b -> Forget r a* b
(define (forget-lmap f fgt)
  (make-forget (lambda (x) (run-forget fgt (f x)))))

;;; forget-rmap : (b -> b*) -> Forget r a b -> Forget r a b*
;;; Ignores the output transformation (phantom type)
(define (forget-rmap g fgt)
  fgt)

(define forget-profunctor-dict
  (make-profunctor-dict 'forget forget-dimap forget-lmap forget-rmap))

;;; forget-first : Forget r a b -> Forget r (a, c) (b, c)
(define (forget-first fgt)
  (make-forget (lambda (pair) (run-forget fgt (car pair)))))

;;; forget-second : Forget r a b -> Forget r (c, a) (c, b)
(define (forget-second fgt)
  (make-forget (lambda (pair) (run-forget fgt (cdr pair)))))

(define forget-strong-dict
  (make-strong-dict forget-profunctor-dict forget-first forget-second))

;;; ============================================================
;;; ForgetMaybe Profunctor (for previewing)
;;; ============================================================
;;;
;;; ForgetMaybe a b = a -> Maybe b (at target type)
;;; Used for prisms where focus might not exist.

;;; forget-left : Forget (Maybe r) a b -> Forget (Maybe r) (Either a c) (Either b c)
(define (forget-left fgt)
  (make-forget (lambda (e)
                       (if (left? e)
                           (run-forget fgt (from-left e))
                           nothing))))

;;; forget-right : Forget (Maybe r) a b -> Forget (Maybe r) (Either c a) (Either c b)
(define (forget-right fgt)
  (make-forget (lambda (e)
                       (if (right? e)
                           (run-forget fgt (from-right e))
                           nothing))))

(define forget-choice-dict
  (make-choice-dict forget-profunctor-dict forget-left forget-right))

(define forget-affine-dict
  (make-affine-dict forget-strong-dict forget-choice-dict))

;;; ============================================================
;;; Tagged Profunctor Dictionaries (for review)
;;; ============================================================

(define tagged-profunctor-dict
  (make-profunctor-dict 'tagged tagged-dimap tagged-lmap tagged-rmap))

(define tagged-choice-dict
  (make-choice-dict tagged-profunctor-dict tagged-left tagged-right))

;;; ============================================================
;;; Optic Constructors
;;; ============================================================

;;; pro-iso : (s -> a) -> (b -> t) -> Iso s t a b
;;; Iso only requires Profunctor (dimap).
(define (pro-iso forward backward)
  (lambda (dict pab)
          (let ([dimap (profunctor-dict-dimap
                        (cond
                         [(affine-dict? dict)
                          (strong-dict-profunctor (affine-dict-strong dict))]
                         [(strong-dict? dict)
                          (strong-dict-profunctor dict)]
                         [(choice-dict? dict)
                          (choice-dict-profunctor dict)]
                         [else dict]))])
               (dimap forward backward pab))))

;;; pro-lens : (s -> a) -> (s -> b -> t) -> Lens s t a b
;;; Lens requires Strong (first*).
(define (pro-lens getter setter)
  (lambda (dict pab)
          ;; Handle both strong-dict and affine-dict
          (let* ([strong (if (affine-dict? dict) (affine-dict-strong dict) dict)]
                 [profunctor (strong-dict-profunctor strong)]
                 [dimap (profunctor-dict-dimap profunctor)]
                 [first* (strong-dict-first* strong)])
                (dimap (lambda (s) (cons (getter s) s))
                       (lambda (pair) (setter (cdr pair) (car pair)))
                       (first* pab)))))

;;; pro-prism : (b -> t) -> (s -> Either t a) -> Prism s t a b
;;; Prism requires Choice (right*).
;;; Standard encoding: dimap preview (either id review) . right'
(define (pro-prism review preview)
  (lambda (dict pab)
          ;; Handle both choice-dict and affine-dict
          (let* ([choice (if (affine-dict? dict) (affine-dict-choice dict) dict)]
                 [profunctor (choice-dict-profunctor choice)]
                 [dimap (profunctor-dict-dimap profunctor)]
                 [right* (choice-dict-right* choice)])
                (dimap preview
                       (lambda (e) (if (left? e) (from-left e) (review (from-right e))))
                       (right* pab)))))

;;; pro-affine : (s -> Maybe a) -> (s -> b -> t) -> Affine s t a b
;;; Affine requires both Strong and Choice.
(define (pro-affine preview set)
  (lambda (dict pab)
          (let* ([strong (if (affine-dict? dict) (affine-dict-strong dict) dict)]
                 [choice (if (affine-dict? dict) (affine-dict-choice dict) dict)]
                 [profunctor (strong-dict-profunctor strong)]
                 [dimap (profunctor-dict-dimap profunctor)]
                 [first* (strong-dict-first* strong)]
                 [right* (choice-dict-right* choice)])
                ;; s -> Either s (a, s)
                (let ([match-fn (lambda (s)
                                        (let ([ma (preview s)])
                                             (if (nothing? ma)
                                                 (left s)
                                                 (right (cons (from-just ma) s)))))])
                     (dimap match-fn
                            (lambda (e)
                                    (if (left? e)
                                        (from-left e)
                                        (set (cdr (from-right e)) (car (from-right e)))))
                            (right* (first* pab)))))))

;;; pro-traversal : ((Applicative f) => (a -> f b) -> s -> f t) -> Traversal s t a b
;;; Traversal requires Wander.
(define (pro-traversal traverse-fn)
  (lambda (dict pab)
          ((wander-dict-wander dict) traverse-fn pab)))

;;; ============================================================
;;; Optic Composition
;;; ============================================================
;;;
;;; The key benefit of profunctor optics: composition is just
;;; function composition!

;;; pro-compose : Optic outer -> Optic inner -> Optic composed
;;; Compose two optics (outer . inner).
(define (pro-compose outer inner)
  (lambda (dict pab)
          (outer dict (inner dict pab))))

;;; pro-. : Like pro-compose but with natural reading order
;;; (pro-. _1 _Just) focuses on Just inside first element
(define pro-. pro-compose)

;;; ============================================================
;;; Optic Operations
;;; ============================================================

;;; pro-view : Lens s t a b -> s -> a
;;; Get the focused value through a lens.
(define (pro-view lens s)
  (run-forget (lens forget-strong-dict (make-forget identity)) s))

;;; pro-over : Lens s t a b -> (a -> b) -> s -> t
;;; Modify the focused value through a lens.
;;; Uses fn-affine-dict to support lenses, prisms, AND affines.
(define (pro-over lens f s)
  ((lens fn-affine-dict f) s))

;;; pro-set : Lens s t a b -> b -> s -> t
;;; Set the focused value through a lens.
(define (pro-set lens b s)
  (pro-over lens (const b) s))

;;; pro-preview : Prism/Affine s t a b -> s -> Maybe a
;;; Try to get the focused value (may fail for prisms/affines).
(define (pro-preview optic s)
  (run-forget (optic forget-affine-dict (make-forget just)) s))

;;; pro-review : Prism s t a b -> b -> t
;;; Construct a value through a prism (the "reverse" direction).
(define (pro-review prism b)
  (untag (prism tagged-choice-dict (make-tagged b))))

;;; pro-has : Affine s t a b -> s -> Boolean
;;; Check if the optic matches.
(define (pro-has optic s)
  (just? (pro-preview optic s)))

;;; pro-isnt : Affine s t a b -> s -> Boolean
;;; Check if the optic does NOT match.
(define (pro-isnt optic s)
  (not (pro-has optic s)))

;;; ============================================================
;;; Stock Lenses
;;; ============================================================

;;; pro-_1 : Lens' (a, b) a
;;; Focus on the first element of a pair.
(define pro-_1
  (pro-lens car (lambda (p b) (cons b (cdr p)))))

;;; pro-_2 : Lens' (a, b) b
;;; Focus on the second element of a pair.
(define pro-_2
  (pro-lens cdr (lambda (p b) (cons (car p) b))))

;;; pro-car-lens : Lens' (Cons a b) a
;;; Focus on car (alias for _1).
(define pro-car-lens pro-_1)

;;; pro-cdr-lens : Lens' (Cons a b) b
;;; Focus on cdr (alias for _2).
(define pro-cdr-lens pro-_2)

;;; ============================================================
;;; Stock Prisms
;;; ============================================================

;;; pro-_Left : Prism' (Either a b) a
;;; Focus on Left branch.
(define pro-_Left
  (pro-prism left
             (lambda (e) (if (left? e) (right (from-left e)) (left e)))))

;;; pro-_Right : Prism' (Either a b) b
;;; Focus on Right branch.
(define pro-_Right
  (pro-prism right
             (lambda (e) (if (right? e) (right (from-right e)) (left e)))))

;;; pro-_Just : Prism' (Maybe a) a
;;; Focus on Just value.
(define pro-_Just
  (pro-prism just
             (lambda (m) (if (just? m) (right (from-just m)) (left m)))))

;;; pro-_Nothing : Prism' (Maybe a) ()
;;; Focus on Nothing.
(define pro-_Nothing
  (pro-prism (const nothing)
             (lambda (m) (if (nothing? m) (right '()) (left m)))))

;;; ============================================================
;;; Sequence Prisms
;;; ============================================================

;;; pro-_Empty : Prism' [a] ()
;;; Focus on empty list.
(define pro-_Empty
  (pro-prism (const '())
             (lambda (lst) (if (null? lst) (right '()) (left lst)))))

;;; pro-_Cons : Prism' [a] (a, [a])
;;; Focus on non-empty list as (head, tail).
(define pro-_Cons
  (pro-prism (lambda (p) (cons (car p) (cdr p)))
             (lambda (lst)
                     (if (null? lst)
                         (left lst)
                         (right (cons (car lst) (cdr lst)))))))

;;; Helper for _Snoc
(define (list-init lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (list-init (cdr lst)))))

(define (list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (list-last (cdr lst))))

;;; pro-_Snoc : Prism' [a] ([a], a)
;;; Focus on non-empty list as (init, last).
(define pro-_Snoc
  (pro-prism (lambda (p) (append (car p) (list (cdr p))))
             (lambda (lst)
                     (if (null? lst)
                         (left lst)
                         (right (cons (list-init lst) (list-last lst)))))))

;;; ============================================================
;;; At/Ixed Typeclasses
;;; ============================================================
;;;
;;; At: Focus on a key that may or may not exist (Lens to Maybe)
;;; Ixed: Focus on an index that may or may not exist (Affine)

;;; pro-at : Key -> Lens' (Alist k v) (Maybe v)
;;; Lens focusing on an alist entry (as Maybe).
(define (pro-at key)
  (pro-lens
   (lambda (alist)
           (let ([result (assoc key alist)])
                (if result (just (cdr result)) nothing)))
   (lambda (alist mv)
           (let ([without (filter (lambda (p) (not (equal? (car p) key))) alist)])
                (if (just? mv)
                    (cons (cons key (from-just mv)) without)
                    without)))))

;;; pro-ix : Int -> Affine' [a] a
;;; Affine focusing on list element at index.
(define (pro-ix n)
  (pro-affine
   (lambda (lst)
           (if (and (>= n 0) (< n (length lst)))
               (just (list-ref lst n))
               nothing))
   (lambda (lst v)
           (if (and (>= n 0) (< n (length lst)))
               (list-set lst n v)
               lst))))

;;; Helper: list-set for pro-ix
(define (list-set lst n v)
  (if (= n 0)
      (cons v (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- n 1) v))))

;;; pro-key : Key -> Affine' (Alist k v) v
;;; Affine focusing on alist value at key (fails if missing).
(define (pro-key k)
  (pro-affine
   (lambda (alist)
           (let ([result (assoc k alist)])
                (if result (just (cdr result)) nothing)))
   (lambda (alist v)
           (map (lambda (p)
                        (if (equal? (car p) k)
                            (cons k v)
                            p))
                alist))))

;;; ============================================================
;;; Star Profunctor Dictionaries (for traversals)
;;; ============================================================

;;; make-star-strong-dict : fmap -> Strong Star
(define (make-star-strong-dict fmap)
  (make-strong-dict
   (make-profunctor-dict 'star
                         (lambda (f g s) (star-dimap fmap f g s))
                         star-lmap
                         (lambda (g s) (star-rmap fmap g s)))
   (lambda (s) (star-first fmap s))
   (lambda (s) (star-second fmap s))))

;;; make-star-wander-dict : Applicative -> Wander Star
(define (make-star-wander-dict applicative)
  (let* ([fmap (applicative-fmap applicative)]
         [pure (applicative-pure applicative)]
         [ap (applicative-ap applicative)])
        (make-wander-dict
         (make-star-strong-dict fmap)
         (lambda (traverse-fn star)
                 (make-star (lambda (s)
                                    (traverse-fn applicative (lambda (a) (run-star star a)) s)))))))

;;; ============================================================
;;; Stock Traversals
;;; ============================================================

;;; list-traverse : Applicative f -> (a -> f b) -> [a] -> f [b]
(define (list-traverse applicative a->fb lst)
  (let ([fmap (applicative-fmap applicative)]
        [pure (applicative-pure applicative)]
        [ap (applicative-ap applicative)])
       (if (null? lst)
           (pure '())
           (ap (fmap (lambda (h) (lambda (t) (cons h t)))
                     (a->fb (car lst)))
               (list-traverse applicative a->fb (cdr lst))))))

;;; pro-each : Traversal' [a] a
;;; Traverse all elements of a list.
(define pro-each
  (pro-traversal (lambda (applicative a->fb lst)
                         (list-traverse applicative a->fb lst))))

;;; pro-both : Traversal' (a, a) a
;;; Traverse both elements of a homogeneous pair.
(define pro-both
  (pro-traversal (lambda (applicative a->fb pair)
                         (let ([fmap (applicative-fmap applicative)]
                               [ap (applicative-ap applicative)])
                              (ap (fmap (lambda (x) (lambda (y) (cons x y)))
                                        (a->fb (car pair)))
                                  (a->fb (cdr pair)))))))

;;; pro-filtered : (a -> Bool) -> Affine' a a
;;; Focus only on values satisfying predicate.
(define (pro-filtered pred)
  (pro-affine
   (lambda (a) (if (pred a) (just a) nothing))
   (lambda (a b) (if (pred a) b a))))

;;; ============================================================
;;; Migration Utilities
;;; ============================================================
;;;
;;; Convert between van Laarhoven and profunctor encodings.

;;; vl-lens->pro-lens : VL-Lens -> Pro-Lens
;;; Convert a van Laarhoven lens to profunctor form.
;;; Requires the VL lens to provide view/set operations.
(define (vl-lens->pro-lens vl-view vl-set)
  (pro-lens vl-view (lambda (s b) (vl-set b s))))

;;; pro-lens->vl-lens : Pro-Lens -> (view . set)
;;; Convert profunctor lens to getter/setter pair.
(define (pro-lens->vl-lens pro-l)
  (cons (lambda (s) (pro-view pro-l s))
        (lambda (b s) (pro-set pro-l b s))))

;;; ============================================================
;;; Composition Examples
;;; ============================================================
;;;
;;; ;; Compose two lenses
;;; (define inner-first (pro-compose pro-_1 pro-_1))
;;; (pro-view inner-first (cons (cons 1 2) 3))  ; => 1
;;;
;;; ;; Compose lens with prism (result is Affine)
;;; (define first-just (pro-compose pro-_1 pro-_Just))
;;; (pro-preview first-just (cons (just 5) 10))  ; => (just 5)
;;; (pro-preview first-just (cons nothing 10))   ; => nothing
;;;
;;; ;; At for alist access
;;; (pro-view (pro-at 'name) '((name . "Alice") (age . 30)))  ; => (just "Alice")
;;;
;;; ;; Ix for list access
;;; (pro-preview (pro-ix 1) '(a b c))  ; => (just b)
;;; (pro-preview (pro-ix 10) '(a b c)) ; => nothing
;;;
;;; ;; Sequence prisms
;;; (pro-preview pro-_Cons '(1 2 3))  ; => (just (1 . (2 3)))
;;; (pro-preview pro-_Empty '())      ; => (just ())
;;; (pro-review pro-_Cons (cons 'a '(b c)))  ; => (a b c)
