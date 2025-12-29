;;; fabric/stitches/fp/lens.ss — Functional Lenses
;;;
;;; Lenses provide a composable way to focus on parts of data structures.
;;; They enable getting, setting, and modifying nested immutable data.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; A Lens s a represents a focus on a value of type 'a' within a structure 's'.
;;; Lenses compose: if you have a lens from A to B and a lens from B to C,
;;; you can compose them to get a lens from A to C.
;;;
;;; Features:
;;;   - Core lens operations (view, set, over)
;;;   - Lens composition
;;;   - Convenience lenses for pairs and lists
;;;   - Prism support for sum types
;;;   - Traversal for multiple focuses
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Lens Representation
;;; ============================================================
;;;
;;; A lens is represented as a pair of functions:
;;;   - getter : s -> a       (extract the focused value)
;;;   - setter : a -> s -> s  (replace the focused value)
;;;
;;; This is the simple "getter/setter" representation.
;;; More advanced representations (van Laarhoven) exist but this
;;; suffices for practical use.

;;; make-lens : (s -> a) -> (a -> s -> s) -> Lens s a
;;; Create a lens from getter and setter functions.
(define (make-lens getter setter)
  (list 'lens getter setter))

;;; lens? : Any -> Boolean
(define (lens? x)
  (and (pair? x) (eq? (car x) 'lens)))

;;; lens-getter : Lens s a -> (s -> a)
(define (lens-getter lens)
  (list-ref lens 1))

;;; lens-setter : Lens s a -> (a -> s -> s)
(define (lens-setter lens)
  (list-ref lens 2))

;;; ============================================================
;;; Core Lens Operations
;;; ============================================================

;;; view : Lens s a -> s -> a
;;; Extract the focused value from a structure.
(define (view lens s)
  ((lens-getter lens) s))

;;; set : Lens s a -> a -> s -> s
;;; Replace the focused value in a structure.
(define (set lens a s)
  ((lens-setter lens) a s))

;;; over : Lens s a -> (a -> a) -> s -> s
;;; Modify the focused value using a function.
(define (over lens f s)
  (set lens (f (view lens s)) s))

;;; ============================================================
;;; Lens Composition
;;; ============================================================

;;; lens-compose : Lens b c -> Lens a b -> Lens a c
;;; Compose two lenses (outer lens first, then inner).
;;; Focus first on b within a, then on c within b.
(define (lens-compose outer inner)
  (make-lens
   ;; getter: a -> c
   (lambda (a)
           (view outer (view inner a)))
   ;; setter: c -> a -> a
   (lambda (c a)
           (over inner (lambda (b) (set outer c b)) a))))

;;; >>> for lenses: compose left-to-right
;;; (lens-then l1 l2) focuses first via l1, then via l2
(define (lens-then l1 l2)
  (lens-compose l2 l1))

;;; Infix-style composition helper
(define (.. . lenses)
  (if (null? lenses)
      id-lens
      (fold-right lens-compose id-lens lenses)))

;;; ============================================================
;;; Identity Lens
;;; ============================================================

;;; id-lens : Lens a a
;;; The identity lens focuses on the entire structure.
(define id-lens
  (make-lens
   id
   (lambda (a s) a)))

;;; ============================================================
;;; Pair Lenses
;;; ============================================================

;;; fst-lens : Lens (a . b) a
;;; Focus on the first element of a pair.
(define fst-lens
  (make-lens
   car
   (lambda (a pair) (cons a (cdr pair)))))

;;; snd-lens : Lens (a . b) b
;;; Focus on the second element of a pair.
(define snd-lens
  (make-lens
   cdr
   (lambda (b pair) (cons (car pair) b))))

;;; ============================================================
;;; List Lenses
;;; ============================================================

;;; car-lens : Lens (List a) a
;;; Focus on the first element of a non-empty list.
;;; Partial: assumes list is non-empty.
(define car-lens
  (make-lens
   car
   (lambda (a lst) (cons a (cdr lst)))))

;;; cdr-lens : Lens (List a) (List a)
;;; Focus on the tail of a non-empty list.
(define cdr-lens
  (make-lens
   cdr
   (lambda (tail lst) (cons (car lst) tail))))

;;; nth-lens : Nat -> Lens (List a) a
;;; Focus on the nth element of a list (0-indexed).
;;; Partial: assumes index is valid.
(define (nth-lens n)
  (if (= n 0)
      car-lens
      (lens-compose (nth-lens (- n 1)) cdr-lens)))

;;; head-lens : Lens (List a) a
;;; Alias for car-lens.
(define head-lens car-lens)

;;; tail-lens : Lens (List a) (List a)
;;; Alias for cdr-lens.
(define tail-lens cdr-lens)

;;; ============================================================
;;; Association List Lenses
;;; ============================================================

;;; key-lens : k -> Lens (Alist k v) v
;;; Focus on the value associated with a key in an alist.
;;; If key doesn't exist, getter returns #f.
;;; Setter adds/updates the key-value pair.
(define (key-lens key)
  (make-lens
   ;; getter
   (lambda (alist)
           (let ([pair (assoc key alist)])
                (if pair (cdr pair) #f)))
   ;; setter
   (lambda (val alist)
           (let ([new-pair (cons key val)])
                (let loop ([lst alist] [acc '()])
                     (cond
                      [(null? lst) (reverse (cons new-pair acc))]
                      [(equal? (caar lst) key)
                       (append (reverse acc) (cons new-pair (cdr lst)))]
                      [else (loop (cdr lst) (cons (car lst) acc))]))))))

;;; ============================================================
;;; Prism (for Sum Types)
;;; ============================================================
;;;
;;; A Prism focuses on one variant of a sum type.
;;; Unlike a lens, a prism may fail to match.
;;;
;;; Prism s a:
;;;   - preview : s -> Maybe a  (extract if matching)
;;;   - review  : a -> s        (construct the variant)

;;; make-prism : (s -> Maybe a) -> (a -> s) -> Prism s a
(define (make-prism preview review)
  (list 'prism preview review))

;;; prism? : Any -> Boolean
(define (prism? x)
  (and (pair? x) (eq? (car x) 'prism)))

;;; prism-preview : Prism s a -> (s -> Maybe a)
(define (prism-preview prism)
  (list-ref prism 1))

;;; prism-review : Prism s a -> (a -> s)
(define (prism-review prism)
  (list-ref prism 2))

;;; preview : Prism s a -> s -> Maybe a
;;; Try to extract the focused variant.
(define (preview prism s)
  ((prism-preview prism) s))

;;; review : Prism s a -> a -> s
;;; Construct the sum type from the variant.
(define (review prism a)
  ((prism-review prism) a))

;;; over-prism : Prism s a -> (a -> a) -> s -> s
;;; Modify if the prism matches, otherwise return unchanged.
(define (over-prism prism f s)
  (let ([result (preview prism s)])
       (if (just? result)
           (review prism (f (from-just result)))
           s)))

;;; ============================================================
;;; Maybe Prisms
;;; ============================================================

;;; just-prism : Prism (Maybe a) a
;;; Focus on the value inside Just.
(define just-prism
  (make-prism
   (lambda (m)
           (if (just? m)
               (just (from-just m))
               nothing))
   just))

;;; ============================================================
;;; Either Prisms
;;; ============================================================

;;; left-prism : Prism (Either a b) a
;;; Focus on Left values.
(define left-prism
  (make-prism
   (lambda (e)
           (if (left? e)
               (just (from-left e))
               nothing))
   left))

;;; right-prism : Prism (Either a b) b
;;; Focus on Right values.
(define right-prism
  (make-prism
   (lambda (e)
           (if (right? e)
               (just (from-right e))
               nothing))
   right))

;;; ============================================================
;;; List Prisms
;;; ============================================================

;;; cons-prism : Prism (List a) (a . List a)
;;; View a non-empty list as a cons cell.
(define cons-prism
  (make-prism
   (lambda (lst)
           (if (null? lst)
               nothing
               (just (cons (car lst) (cdr lst)))))
   (lambda (pair)
           (cons (car pair) (cdr pair)))))

;;; nil-prism : Prism (List a) ()
;;; Match empty lists.
(define nil-prism
  (make-prism
   (lambda (lst)
           (if (null? lst)
               (just '())
               nothing))
   (lambda (unit) '())))

;;; ============================================================
;;; Traversal (Multiple Focuses)
;;; ============================================================
;;;
;;; A Traversal focuses on zero or more values within a structure.
;;; It's like a lens but can have multiple targets.
;;;
;;; Traversal s a:
;;;   - to-list  : s -> List a      (extract all focused values)
;;;   - over-all : (a -> a) -> s -> s (modify all focused values)

;;; make-traversal : (s -> List a) -> ((a -> a) -> s -> s) -> Traversal s a
(define (make-traversal to-list over-all)
  (list 'traversal to-list over-all))

;;; traversal? : Any -> Boolean
(define (traversal? x)
  (and (pair? x) (eq? (car x) 'traversal)))

;;; traversal-to-list : Traversal s a -> (s -> List a)
(define (traversal-to-list trav)
  (list-ref trav 1))

;;; traversal-over-all : Traversal s a -> ((a -> a) -> s -> s)
(define (traversal-over-all trav)
  (list-ref trav 2))

;;; to-list-of : Traversal s a -> s -> List a
;;; Extract all focused values.
(define (to-list-of trav s)
  ((traversal-to-list trav) s))

;;; over-all : Traversal s a -> (a -> a) -> s -> s
;;; Modify all focused values.
(define (over-traversal trav f s)
  ((traversal-over-all trav) f s))

;;; ============================================================
;;; List Traversals
;;; ============================================================

;;; each : Traversal (List a) a
;;; Focus on each element of a list.
(define each
  (make-traversal
   id
   (lambda (f lst) (map f lst))))

;;; filtered : (a -> Boolean) -> Traversal (List a) a
;;; Focus on elements satisfying a predicate.
(define (filtered pred)
  (make-traversal
   (lambda (lst) (filter pred lst))
   (lambda (f lst)
           (map (lambda (x) (if (pred x) (f x) x)) lst))))

;;; ============================================================
;;; Lens from Traversal
;;; ============================================================

;;; A lens can be converted to a traversal (with single focus).
;;; A traversal cannot generally be converted to a lens.

;;; lens->traversal : Lens s a -> Traversal s a
(define (lens->traversal lens)
  (make-traversal
   (lambda (s) (list (view lens s)))
   (lambda (f s) (over lens f s))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; modify : Lens s a -> (a -> a) -> s -> s
;;; Alias for over.
(define modify over)

;;; update : Lens s a -> a -> s -> s
;;; Alias for set (different argument order).
(define (update lens val s)
  (set lens val s))

;;; view-maybe : Lens s (Maybe a) -> s -> Maybe a
;;; View through a lens that focuses on a Maybe.
(define (view-maybe lens s)
  (view lens s))

;;; ============================================================
;;; Lens Laws (for documentation)
;;; ============================================================
;;;
;;; A well-behaved lens must satisfy:
;;;
;;; 1. Get-Put: view l (set l a s) = a
;;;    What you put is what you get.
;;;
;;; 2. Put-Get: set l (view l s) s = s
;;;    Setting what's already there changes nothing.
;;;
;;; 3. Put-Put: set l a2 (set l a1 s) = set l a2 s
;;;    Setting twice is the same as setting once.

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Basic lens usage
;;; (define point (cons 3 4))
;;; (view fst-lens point)           ; => 3
;;; (set fst-lens 10 point)         ; => (10 . 4)
;;; (over fst-lens add1 point)      ; => (4 . 4)
;;;
;;; ;; Lens composition
;;; (define nested (cons (cons 1 2) 3))
;;; (define inner-fst (lens-compose fst-lens fst-lens))
;;; (view inner-fst nested)         ; => 1
;;; (set inner-fst 100 nested)      ; => ((100 . 2) . 3)
;;;
;;; ;; Alist lens
;;; (define data '((name . "Alice") (age . 30)))
;;; (view (key-lens 'name) data)    ; => "Alice"
;;; (set (key-lens 'age) 31 data)   ; => ((name . "Alice") (age . 31))
;;;
;;; ;; Traversal
;;; (to-list-of each '(1 2 3))      ; => (1 2 3)
;;; (over-traversal each add1 '(1 2 3)) ; => (2 3 4)
;;; (to-list-of (filtered even?) '(1 2 3 4)) ; => (2 4)
