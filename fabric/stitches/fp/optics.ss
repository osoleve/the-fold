;;; fabric/stitches/fp/optics.ss — Van Laarhoven Optics
;;;
;;; Optics are composable, first-class accessors for immutable data.
;;; This implementation uses the van Laarhoven encoding where optics
;;; are functions polymorphic over functors/profunctors.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Iso: Invertible transformations
;;;   - Lens: Focus on parts of products
;;;   - Prism: Focus on branches of sums
;;;   - Traversal: Focus on multiple targets
;;;   - Affine: At-most-one target (optional)
;;;   - Getter/Setter: Read-only/write-only access
;;;   - Optic composition
;;;   - Stock optics for common data structures
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/profunctor.ss
;;;   - fp/traversable.ss
;;;
;;; Laws:
;;;   Lens laws:
;;;     view l (set l v s) = v                    (set-get)
;;;     set l (view l s) s = s                    (get-set)
;;;     set l v (set l w s) = set l v s           (set-set)
;;;
;;;   Prism laws:
;;;     preview p (review p v) = Just v           (review-preview)
;;;     maybe s review p (preview p s) = s        (preview-review)
;;;
;;;   Iso laws:
;;;     view i (review i a) = a                   (from-to)
;;;     review i (view i s) = s                   (to-from)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/profunctor.ss")
(load "fabric/stitches/fp/traversable.ss")

;;; ============================================================
;;; Identity Functor
;;; ============================================================
;;;
;;; Used for getting values through lenses.

;;; make-identity : a -> Identity a
(define (make-identity x)
  (list 'identity x))

;;; identity? : Any -> Boolean
(define (identity? x)
  (and (pair? x) (eq? (car x) 'identity)))

;;; run-identity : Identity a -> a
(define (run-identity id)
  (list-ref id 1))

;;; identity-fmap : (a -> b) -> Identity a -> Identity b
(define (identity-fmap f id)
  (make-identity (f (run-identity id))))

;;; identity-applicative : Applicative Identity
(define identity-applicative
  (make-applicative
   make-identity
   (lambda (ff fa)
           (make-identity ((run-identity ff) (run-identity fa))))))

;;; ============================================================
;;; Const Functor
;;; ============================================================
;;;
;;; Used for viewing values (the "getter" part of optics).

;;; make-const : a -> Const a b
(define (make-const x)
  (list 'const x))

;;; const? : Any -> Boolean
(define (const? x)
  (and (pair? x) (eq? (car x) 'const)))

;;; get-const : Const a b -> a
(define (get-const c)
  (list-ref c 1))

;;; const-fmap : (b -> c) -> Const a b -> Const a c
;;; Ignores the function, keeps the constant.
(define (const-fmap f c)
  c)

;;; const-applicative : Monoid a -> Applicative (Const a)
;;; Const is applicative when the constant type is a monoid.
(define (const-applicative monoid)
  (make-applicative
   (lambda (x) (make-const (monoid-empty monoid)))
   (lambda (cf ca)
           (make-const ((monoid-append monoid) (get-const cf) (get-const ca))))))

;;; Note: first-monoid is already defined in traversable.ss

;;; ============================================================
;;; Lens Type
;;; ============================================================
;;;
;;; A Lens s t a b focuses on exactly one 'a' inside an 's',
;;; and can replace it with a 'b' to get a 't'.
;;;
;;; Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
;;;
;;; For simple lenses where s=t and a=b:
;;; Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

;;; make-lens : (s -> a) -> (s -> b -> t) -> Lens s t a b
;;; Create a lens from getter and setter.
(define (make-lens getter setter)
  (lambda (fmap a->fb s)
          (fmap (lambda (b) (setter s b))
                (a->fb (getter s)))))

;;; lens-view : Lens s t a b -> s -> a
;;; Get the focused value.
(define (lens-view lens s)
  (get-const (lens const-fmap (lambda (a) (make-const a)) s)))

;;; lens-set : Lens s t a b -> b -> s -> t
;;; Set the focused value.
(define (lens-set lens b s)
  (run-identity (lens identity-fmap (lambda (_) (make-identity b)) s)))

;;; lens-over : Lens s t a b -> (a -> b) -> s -> t
;;; Modify the focused value.
(define (lens-over lens f s)
  (run-identity (lens identity-fmap (lambda (a) (make-identity (f a))) s)))

;;; ============================================================
;;; Iso Type
;;; ============================================================
;;;
;;; An Iso s t a b is an invertible transformation.
;;; Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
;;;
;;; Simplified: just store the two functions.

;;; make-iso : (s -> a) -> (b -> t) -> Iso s t a b
(define (make-iso forward backward)
  (list 'iso forward backward))

;;; iso? : Any -> Boolean
(define (iso? x)
  (and (pair? x) (eq? (car x) 'iso)))

;;; iso-forward : Iso s t a b -> s -> a
(define (iso-forward iso)
  (list-ref iso 1))

;;; iso-backward : Iso s t a b -> b -> t
(define (iso-backward iso)
  (list-ref iso 2))

;;; iso-view : Iso s t a b -> s -> a
(define (iso-view iso s)
  ((iso-forward iso) s))

;;; iso-review : Iso s t a b -> b -> t
(define (iso-review iso b)
  ((iso-backward iso) b))

;;; iso->lens : Iso s t a b -> Lens s t a b
;;; Every iso is a lens.
(define (iso->lens iso)
  (make-lens (iso-forward iso)
             (lambda (s b) ((iso-backward iso) b))))

;;; iso-flip : Iso s t a b -> Iso b a t s
;;; Flip an iso.
(define (iso-flip iso)
  (make-iso (iso-backward iso) (iso-forward iso)))

;;; ============================================================
;;; Prism Type
;;; ============================================================
;;;
;;; A Prism s t a b focuses on one branch of a sum type.
;;; Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
;;;
;;; Simplified representation with review and preview.

;;; make-prism : (b -> t) -> (s -> Either t a) -> Prism s t a b
;;; Create a prism from review and preview.
(define (make-prism review preview)
  (list 'prism review preview))

;;; prism? : Any -> Boolean
(define (prism? x)
  (and (pair? x) (eq? (car x) 'prism)))

;;; prism-review : Prism s t a b -> b -> t
;;; Build the whole from the part.
(define (prism-review prism b)
  ((list-ref prism 1) b))

;;; prism-preview : Prism s t a b -> s -> Either t a
;;; Try to extract the part.
(define (prism-get-preview prism)
  (list-ref prism 2))

;;; prism-match : Prism s t a b -> s -> Maybe a
;;; Try to match and extract.
(define (prism-match prism s)
  (let ([result ((prism-get-preview prism) s)])
       (if (right? result)
           (just (from-right result))
           nothing)))

;;; prism-over : Prism s t a b -> (a -> b) -> s -> t
;;; Modify if the prism matches.
(define (prism-over prism f s)
  (let ([result ((prism-get-preview prism) s)])
       (if (right? result)
           (prism-review prism (f (from-right result)))
           (from-left result))))

;;; prism-set : Prism s t a b -> b -> s -> t
;;; Set if the prism matches.
(define (prism-set prism b s)
  (prism-over prism (lambda (_) b) s))

;;; ============================================================
;;; Affine Type
;;; ============================================================
;;;
;;; An Affine focuses on at-most-one target.
;;; Combination of Lens and Prism properties.

;;; make-affine : (s -> Maybe a) -> (s -> b -> t) -> Affine s t a b
(define (make-affine preview set)
  (list 'affine preview set))

;;; affine? : Any -> Boolean
(define (affine? x)
  (and (pair? x) (eq? (car x) 'affine)))

;;; affine-preview : Affine s t a b -> s -> Maybe a
(define (affine-preview aff s)
  ((list-ref aff 1) s))

;;; affine-set : Affine s t a b -> b -> s -> t
(define (affine-set aff b s)
  ((list-ref aff 2) s b))

;;; affine-over : Affine s t a b -> (a -> b) -> s -> t
(define (affine-over aff f s)
  (let ([ma (affine-preview aff s)])
       (if (just? ma)
           (affine-set aff (f (from-just ma)) s)
           s)))

;;; ============================================================
;;; Traversal Type
;;; ============================================================
;;;
;;; A Traversal focuses on zero or more targets.
;;; Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
;;;
;;; Since Scheme lacks typeclasses, traversals take an applicative bundle
;;; containing pure, ap, and fmap operations for the functor being used.

;;; make-traversal : ((a -> f b) -> Applicative f -> s -> f t) -> Traversal s t a b
;;; Where the function should work for any applicative f passed as second arg.
(define (make-traversal traverse-fn)
  (list 'traversal traverse-fn))

;;; traversal? : Any -> Boolean
(define (traversal? x)
  (and (pair? x) (eq? (car x) 'traversal)))

;;; get-traversal : Traversal s t a b -> ((a -> f b) -> Applicative f -> s -> f t)
(define (get-traversal trav)
  (list-ref trav 1))

;;; traversal-toListOf : Traversal s t a b -> s -> List a
;;; Get all focused values.
(define (traversal-toListOf trav s)
  (get-const
   ((get-traversal trav)
    (lambda (a) (make-const (list a)))
    (const-applicative list-monoid)
    s)))

;;; traversal-over : Traversal s t a b -> (a -> b) -> s -> t
;;; Modify all focused values.
(define (traversal-over trav f s)
  (run-identity
   ((get-traversal trav)
    (lambda (a) (make-identity (f a)))
    identity-applicative
    s)))

;;; traversal-set : Traversal s t a b -> b -> s -> t
;;; Set all focused values.
(define (traversal-set trav b s)
  (traversal-over trav (lambda (_) b) s))

;;; ============================================================
;;; Optic Composition
;;; ============================================================

;;; lens-compose : Lens s t a b -> Lens a b x y -> Lens s t x y
;;; Compose two lenses (outer . inner).
(define (lens-compose outer inner)
  (make-lens
   (lambda (s) (lens-view inner (lens-view outer s)))
   (lambda (s y) (lens-over outer (lambda (a) (lens-set inner y a)) s))))

;;; iso-compose : Iso s t a b -> Iso a b x y -> Iso s t x y
(define (iso-compose outer inner)
  (make-iso
   (lambda (s) (iso-view inner (iso-view outer s)))
   (lambda (y) (iso-review outer (iso-review inner y)))))

;;; prism-compose : Prism s t a b -> Prism a b x y -> Prism s t x y
(define (prism-compose outer inner)
  (make-prism
   (lambda (y) (prism-review outer (prism-review inner y)))
   (lambda (s)
           (let ([outer-result ((prism-get-preview outer) s)])
                (if (left? outer-result)
                    outer-result
                    (let ([inner-result ((prism-get-preview inner) (from-right outer-result))])
                         (if (left? inner-result)
                             (left (prism-review outer (from-left inner-result)))
                             inner-result)))))))

;;; ============================================================
;;; Stock Lenses
;;; ============================================================

;;; _1 : Lens (a, b) (a', b) a a'
;;; Focus on the first element of a pair.
(define _1
  (make-lens car (lambda (p b) (cons b (cdr p)))))

;;; _2 : Lens (a, b) (a, b') b b'
;;; Focus on the second element of a pair.
(define _2
  (make-lens cdr (lambda (p b) (cons (car p) b))))

;;; at : Key -> Lens (Alist k v) (Alist k v) (Maybe v) (Maybe v)
;;; Focus on an element in an association list.
(define (at key)
  (make-lens
   (lambda (alist)
           (let ([result (assoc key alist)])
                (if result (just (cdr result)) nothing)))
   (lambda (alist mv)
           (let ([without (filter (lambda (p) (not (equal? (car p) key))) alist)])
                (if (just? mv)
                    (cons (cons key (from-just mv)) without)
                    without)))))

;;; ix : Int -> Affine (List a) (List a) a a
;;; Focus on an element at an index.
(define (ix n)
  (make-affine
   (lambda (lst)
           (if (and (>= n 0) (< n (length lst)))
               (just (list-ref lst n))
               nothing))
   (lambda (lst v)
           (if (and (>= n 0) (< n (length lst)))
               (list-update lst n v)
               lst))))

;;; list-update : List a -> Int -> a -> List a
;;; Helper to update element at index.
(define (list-update lst n v)
  (if (= n 0)
      (cons v (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- n 1) v))))

;;; ============================================================
;;; Stock Prisms
;;; ============================================================

;;; _Left : Prism (Either a b) (Either a' b) a a'
;;; Focus on the Left branch.
(define _Left
  (make-prism
   left
   (lambda (e)
           (if (left? e)
               (right (from-left e))
               (left (right (from-right e)))))))

;;; _Right : Prism (Either a b) (Either a b') b b'
;;; Focus on the Right branch.
(define _Right
  (make-prism
   right
   (lambda (e)
           (if (right? e)
               (right (from-right e))
               (left (left (from-left e)))))))

;;; _Just : Prism (Maybe a) (Maybe b) a b
;;; Focus on the Just branch.
(define _Just
  (make-prism
   just
   (lambda (m)
           (if (just? m)
               (right (from-just m))
               (left nothing)))))

;;; _Nothing : Prism (Maybe a) (Maybe a) () ()
;;; Match Nothing.
(define _Nothing
  (make-prism
   (lambda (_) nothing)
   (lambda (m)
           (if (nothing? m)
               (right '())
               (left m)))))

;;; _Cons : Prism (List a) (List a) (a, List a) (a, List a)
;;; Focus on non-empty list as (head, tail).
(define _Cons
  (make-prism
   (lambda (pair) (cons (car pair) (cdr pair)))
   (lambda (lst)
           (if (null? lst)
               (left '())
               (right (cons (car lst) (cdr lst)))))))

;;; _Nil : Prism (List a) (List a) () ()
;;; Match empty list.
(define _Nil
  (make-prism
   (lambda (_) '())
   (lambda (lst)
           (if (null? lst)
               (right '())
               (left lst)))))

;;; ============================================================
;;; Stock Isos
;;; ============================================================

;;; _id : Iso a a a a
;;; Identity iso.
(define _id
  (make-iso identity identity))

;;; _swap : Iso (a, b) (b, a) (b, a) (a, b)
;;; Swap pair elements.
(define _swap
  (make-iso
   (lambda (p) (cons (cdr p) (car p)))
   (lambda (p) (cons (cdr p) (car p)))))

;;; _curried : Iso ((a, b) -> c) ((a, b) -> c) (a -> b -> c) (a -> b -> c)
;;; Curry/uncurry iso.
(define _curried
  (make-iso
   (lambda (f) (lambda (a) (lambda (b) (f (cons a b)))))
   (lambda (f) (lambda (p) ((f (car p)) (cdr p))))))

;;; _flipped : Iso (a -> b -> c) (a -> b -> c) (b -> a -> c) (b -> a -> c)
;;; Flip argument order.
(define _flipped
  (make-iso
   (lambda (f) (lambda (b) (lambda (a) ((f a) b))))
   (lambda (f) (lambda (a) (lambda (b) ((f b) a))))))

;;; _chars : Iso String String (List Char) (List Char)
;;; String as list of chars.
(define _chars
  (make-iso
   string->list
   list->string))

;;; ============================================================
;;; Stock Traversals
;;; ============================================================

;;; each : Traversal (List a) (List b) a b
;;; Traverse each element of a list.
(define each
  (make-traversal
   (lambda (a->fb app s)
           ;; Use the passed applicative's pure, ap, and fmap
           (if (null? s)
               ((app-pure app) '())
               ((app-ap app)
                (app-fmap app (lambda (h) (lambda (t) (cons h t)))
                          (a->fb (car s)))
                ((get-traversal each) a->fb app (cdr s)))))))

;;; both : Traversal (a, a) (b, b) a b
;;; Traverse both elements of a homogeneous pair.
(define both
  (make-traversal
   (lambda (a->fb app p)
           ;; Use the passed applicative's fmap and ap
           ((app-ap app)
            (app-fmap app (lambda (a) (lambda (b) (cons a b)))
                      (a->fb (car p)))
            (a->fb (cdr p))))))

;;; ============================================================
;;; Getter/Setter Only Optics
;;; ============================================================

;;; make-getter : (s -> a) -> Getter s a
;;; A getter is a read-only optic.
(define (make-getter get-fn)
  (list 'getter get-fn))

;;; getter? : Any -> Boolean
(define (getter? x)
  (and (pair? x) (eq? (car x) 'getter)))

;;; getter-view : Getter s a -> s -> a
(define (getter-view g s)
  ((list-ref g 1) s))

;;; to : (s -> a) -> Getter s a
;;; Create a getter from any function.
(define to make-getter)

;;; make-setter : ((a -> b) -> s -> t) -> Setter s t a b
;;; A setter is a write-only optic.
(define (make-setter over-fn)
  (list 'setter over-fn))

;;; setter? : Any -> Boolean
(define (setter? x)
  (and (pair? x) (eq? (car x) 'setter)))

;;; setter-over : Setter s t a b -> (a -> b) -> s -> t
(define (setter-over s f x)
  ((list-ref s 1) f x))

;;; setter-set : Setter s t a b -> b -> s -> t
(define (setter-set s b x)
  (setter-over s (lambda (_) b) x))

;;; sets : ((a -> b) -> s -> t) -> Setter s t a b
;;; Create a setter from an over function.
(define sets make-setter)

;;; mapped : Setter (List a) (List b) a b
;;; Setter that maps over a list.
(define mapped
  (make-setter map))

;;; ============================================================
;;; Optic Combinators
;;; ============================================================

;;; failing : Affine s t a b -> Affine s t a b -> Affine s t a b
;;; Try first affine, fall back to second.
(define (failing aff1 aff2)
  (make-affine
   (lambda (s)
           (let ([r1 (affine-preview aff1 s)])
                (if (just? r1) r1 (affine-preview aff2 s))))
   (lambda (s b)
           (let ([r1 (affine-preview aff1 s)])
                (if (just? r1)
                    (affine-set aff1 b s)
                    (affine-set aff2 b s))))))

;;; filtered : (a -> Bool) -> Affine a a a a
;;; Focus only if predicate holds.
(define (filtered pred)
  (make-affine
   (lambda (a) (if (pred a) (just a) nothing))
   (lambda (a b) (if (pred a) b a))))

;;; taking : Int -> Traversal (List a) (List a) (List a) (List a)
;;; Focus on first n elements as a group.
(define (taking n)
  (make-affine
   (lambda (lst)
           (if (>= (length lst) n)
               (just (list-take lst n))
               nothing))
   (lambda (lst new-prefix)
           (append new-prefix (list-drop lst n)))))

;;; dropping : Int -> Traversal (List a) (List a) (List a) (List a)
;;; Focus on elements after first n as a group.
(define (dropping n)
  (make-affine
   (lambda (lst)
           (if (>= (length lst) n)
               (just (list-drop lst n))
               nothing))
   (lambda (lst new-suffix)
           (append (list-take lst n) new-suffix))))

;;; list-take : List a -> Int -> List a
(define (list-take lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (list-take (cdr lst) (- n 1)))))

;;; list-drop : List a -> Int -> List a
(define (list-drop lst n)
  (if (or (= n 0) (null? lst))
      lst
      (list-drop (cdr lst) (- n 1))))

;;; ============================================================
;;; Useful Derived Operations
;;; ============================================================

;;; preview : Prism s t a b -> s -> Maybe a
;;; Same as prism-match.
(define preview prism-match)

;;; review : Prism s t a b -> b -> t
;;; Same as prism-review.
(define review prism-review)

;;; view : Lens s t a b -> s -> a
;;; Same as lens-view.
(define view lens-view)

;;; over : Lens s t a b -> (a -> b) -> s -> t
;;; Same as lens-over.
(define over lens-over)

;;; set : Lens s t a b -> b -> s -> t
;;; Same as lens-set, curried properly.
(define (set lens b)
  (lambda (s) (lens-set lens b s)))

;;; has : Prism s t a b -> s -> Bool
;;; Check if prism matches.
(define (has prism s)
  (just? (prism-match prism s)))

;;; isnt : Prism s t a b -> s -> Bool
;;; Check if prism doesn't match.
(define (isnt prism s)
  (nothing? (prism-match prism s)))

;;; ============================================================
;;; Lens Operations with Default
;;; ============================================================

;;; pre : Affine s t a b -> s -> Maybe a
;;; Preview through an affine.
(define pre affine-preview)

;;; (??) : Affine s t a b -> a -> s -> a
;;; Preview with default.
(define (lens-default aff default s)
  (maybe default identity (affine-preview aff s)))

;;; singular : Traversal s t a b -> Affine s t a b
;;; Convert traversal to affine (takes first element if any).
;;; Note: This is a simplification; full implementation would need more machinery.
(define (singular trav)
  (make-affine
   (lambda (s)
           (let ([lst (traversal-toListOf trav s)])
                (if (null? lst) nothing (just (car lst)))))
   (lambda (s b)
           ;; Set only the first element
           (let ([done (list #f)])
                (traversal-over trav
                                (lambda (a)
                                        (if (car done)
                                            a
                                            (begin (set-car! done #t) b)))
                                s)))))

;;; ============================================================
;;; Practical Examples (for documentation)
;;; ============================================================
;;;
;;; ;; Working with pairs
;;; (lens-view _1 (cons "hello" 42))        ; => "hello"
;;; (lens-set _1 "world" (cons "hello" 42)) ; => ("world" . 42)
;;; (lens-over _2 add1 (cons "x" 1))        ; => ("x" . 2)
;;;
;;; ;; Composing lenses
;;; (define nested (cons (cons 1 2) 3))
;;; (define inner-first (lens-compose _1 _1))
;;; (lens-view inner-first nested)          ; => 1
;;; (lens-set inner-first 100 nested)       ; => ((100 . 2) . 3)
;;;
;;; ;; Working with Either
;;; (prism-match _Left (left 42))           ; => (just 42)
;;; (prism-match _Left (right "x"))         ; => nothing
;;; (prism-review _Left 42)                 ; => (left 42)
;;;
;;; ;; Working with Maybe
;;; (prism-match _Just (just 1))            ; => (just 1)
;;; (prism-over _Just add1 (just 5))        ; => (just 6)
;;; (prism-over _Just add1 nothing)         ; => nothing
;;;
;;; ;; Indexed access
;;; (affine-preview (ix 1) '(a b c))        ; => (just b)
;;; (affine-set (ix 1) 'x '(a b c))         ; => (a x c)
;;; (affine-preview (ix 5) '(a b c))        ; => nothing
;;;
;;; ;; Association lists
;;; (define alist '((name . "Alice") (age . 30)))
;;; (lens-view (at 'name) alist)            ; => (just "Alice")
;;; (lens-set (at 'name) (just "Bob") alist) ; => ((name . "Bob") (age . 30))
;;;
;;; ;; Isos
;;; (iso-view _chars "hello")               ; => (#\h # #\l #\l #\o)
;;; (iso-review _chars '(#\h #\i))          ; => "hi"
