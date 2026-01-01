;;; fabric/stitches/fp/nonempty.ss - NonEmpty List Type
;;;
;;; NonEmpty represents a list guaranteed to have at least one element.
;;; This enables safe head/last operations and Semigroup without Monoid.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - NonEmpty construction and deconstruction
;;;   - Functor instance (map over elements)
;;;   - Semigroup instance (concatenation, no identity)
;;;   - Foldable1 instance (fold guaranteed to have first element)
;;;   - Apply instance (Applicative without pure)
;;;   - Bind instance (Monad without return)
;;;   - Safe head/last/tail operations
;;;   - Conversion to/from regular lists
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/typeclasses.ss
;;;   - fp/algebraic.ss

(load "core/prelude.ss")
(load "core/fp/typeclasses/typeclasses.ss")
(load "core/fp/typeclasses/algebraic.ss")

;;; ============================================================
;;; NonEmpty Type
;;; ============================================================
;;;
;;; NonEmpty a = (nonempty head tail)
;;; where head : a, tail : List a
;;;
;;; Invariant: A NonEmpty always has at least one element (head).

;;; make-nonempty : a -> List a -> NonEmpty a
;;; Construct a NonEmpty from head and tail.
(define (make-nonempty head tail)
  (list 'nonempty head tail))

;;; nonempty? : Any -> Boolean
(define (nonempty? x)
  (and (pair? x) (eq? (car x) 'nonempty)))

;;; nonempty-head : NonEmpty a -> a
;;; Get the first element (always safe).
(define (nonempty-head ne)
  (list-ref ne 1))

;;; nonempty-tail : NonEmpty a -> List a
;;; Get the remaining elements (may be empty).
(define (nonempty-tail ne)
  (list-ref ne 2))

;;; nonempty : a -> a ... -> NonEmpty a
;;; Varargs constructor for NonEmpty.
(define (nonempty head . rest)
  (make-nonempty head rest))

;;; singleton : a -> NonEmpty a
;;; Create a NonEmpty with exactly one element.
(define (ne-singleton x)
  (make-nonempty x '()))

;;; ============================================================
;;; Conversion Functions
;;; ============================================================

;;; nonempty->list : NonEmpty a -> List a
;;; Convert to regular list (always non-empty).
(define (nonempty->list ne)
  (cons (nonempty-head ne) (nonempty-tail ne)))

;;; list->nonempty : List a -> Maybe (NonEmpty a)
;;; Convert from list, returning Nothing for empty list.
(define (list->nonempty lst)
  (if (null? lst)
      nothing
      (just (make-nonempty (car lst) (cdr lst)))))

;;; from-list : List a -> NonEmpty a
;;; Convert from list, error on empty.
;;; PARTIAL: Use list->nonempty for safe version.
(define (ne-from-list lst)
  (if (null? lst)
      (error 'ne-from-list "empty list")
      (make-nonempty (car lst) (cdr lst))))

;;; ============================================================
;;; Basic Operations
;;; ============================================================

;;; ne-length : NonEmpty a -> Int
;;; Length is always >= 1.
(define (ne-length ne)
  (+ 1 (length (nonempty-tail ne))))

;;; ne-last : NonEmpty a -> a
;;; Get the last element (always safe).
(define (ne-last ne)
  (let ([tail (nonempty-tail ne)])
       (if (null? tail)
           (nonempty-head ne)
           (let loop ([lst tail])
                (if (null? (cdr lst))
                    (car lst)
                    (loop (cdr lst)))))))

;;; ne-init : NonEmpty a -> List a
;;; Get all elements except the last.
(define (ne-init ne)
  (let ([head (nonempty-head ne)]
        [tail (nonempty-tail ne)])
       (if (null? tail)
           '()
           (cons head (let loop ([lst tail])
                           (if (null? (cdr lst))
                               '()
                               (cons (car lst) (loop (cdr lst)))))))))

;;; ne-reverse : NonEmpty a -> NonEmpty a
(define (ne-reverse ne)
  (let ([reversed (reverse (nonempty->list ne))])
       (make-nonempty (car reversed) (cdr reversed))))

;;; ne-cons : a -> NonEmpty a -> NonEmpty a
;;; Prepend an element.
(define (ne-cons x ne)
  (make-nonempty x (nonempty->list ne)))

;;; ne-snoc : NonEmpty a -> a -> NonEmpty a
;;; Append an element.
(define (ne-snoc ne x)
  (make-nonempty (nonempty-head ne)
                 (append (nonempty-tail ne) (list x))))

;;; ============================================================
;;; Functor Instance
;;; ============================================================

;;; ne-map : (a -> b) -> NonEmpty a -> NonEmpty b
(define (ne-map f ne)
  (make-nonempty (f (nonempty-head ne))
                 (map f (nonempty-tail ne))))

;;; nonempty-functor : Functor NonEmpty
(define nonempty-functor
  (make-functor ne-map))

;;; ============================================================
;;; Semigroup Instance (NOT Monoid - no empty)
;;; ============================================================

;;; ne-append : NonEmpty a -> NonEmpty a -> NonEmpty a
;;; Concatenate two NonEmpty lists.
(define (ne-append ne1 ne2)
  (make-nonempty (nonempty-head ne1)
                 (append (nonempty-tail ne1) (nonempty->list ne2))))

;;; nonempty-semigroup : Semigroup (NonEmpty a)
(define nonempty-semigroup
  (make-semigroup 'nonempty ne-append))

;;; ============================================================
;;; Foldable1 (fold with guaranteed first element)
;;; ============================================================

;;; ne-foldr : (a -> b -> b) -> b -> NonEmpty a -> b
;;; Right fold over NonEmpty.
(define (ne-foldr f init ne)
  (f (nonempty-head ne)
     (fold-right f init (nonempty-tail ne))))

;;; ne-foldl : (b -> a -> b) -> b -> NonEmpty a -> b
;;; Left fold over NonEmpty.
(define (ne-foldl f init ne)
  (fold-left f (f init (nonempty-head ne)) (nonempty-tail ne)))

;;; ne-foldr1 : (a -> a -> a) -> NonEmpty a -> a
;;; Right fold using first element as initial value.
;;; This is the key Foldable1 operation - always safe!
(define (ne-foldr1 f ne)
  (let ([head (nonempty-head ne)]
        [tail (nonempty-tail ne)])
       (if (null? tail)
           head
           (f head (fold-right f (car (reverse tail))
                               (cdr (reverse tail)))))))

;;; ne-foldl1 : (a -> a -> a) -> NonEmpty a -> a
;;; Left fold using first element as initial value.
(define (ne-foldl1 f ne)
  (fold-left f (nonempty-head ne) (nonempty-tail ne)))

;;; ne-fold-map : Semigroup m -> (a -> m) -> NonEmpty a -> m
;;; Map each element to a semigroup value and combine.
(define (ne-fold-map sg f ne)
  (let ([op (semigroup-op sg)]
        [head (nonempty-head ne)]
        [tail (nonempty-tail ne)])
       (if (null? tail)
           (f head)
           (fold-left (lambda (acc x) (op acc (f x)))
                      (f head)
                      tail))))

;;; ============================================================
;;; Apply Instance (Applicative without pure)
;;; ============================================================

;;; ne-ap : NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
;;; Apply functions to values, Cartesian product style.
(define (ne-ap nef nea)
  (let* ([fs (nonempty->list nef)]
         [as (nonempty->list nea)]
         [results (apply append (map (lambda (f) (map f as)) fs))])
        (make-nonempty (car results) (cdr results))))

;;; nonempty-apply : Apply NonEmpty
(define nonempty-apply
  (make-apply nonempty-functor ne-ap))

;;; ne-lift2 : (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
;;; Lift binary function over NonEmpty (curried).
(define (ne-lift2 f nea neb)
  (ne-ap (ne-map f nea) neb))

;;; ============================================================
;;; Bind Instance (Monad without return)
;;; ============================================================

;;; ne-bind : NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
;;; Bind/flatMap for NonEmpty.
(define (ne-bind ne f)
  (let* ([results (map f (nonempty->list ne))]
         [flattened (apply append (map nonempty->list results))])
        (make-nonempty (car flattened) (cdr flattened))))

;;; nonempty-bind : Bind NonEmpty
(define nonempty-bind
  (make-bind nonempty-apply ne-bind))

;;; ne-join : NonEmpty (NonEmpty a) -> NonEmpty a
;;; Flatten nested NonEmpty.
(define (ne-join nene)
  (ne-bind nene id))

;;; ============================================================
;;; Additional Utilities
;;; ============================================================

;;; ne-take : Int -> NonEmpty a -> List a
;;; Take first n elements (may return fewer).
(define (ne-take n ne)
  (take n (nonempty->list ne)))

;;; ne-drop : Int -> NonEmpty a -> List a
;;; Drop first n elements.
(define (ne-drop n ne)
  (drop n (nonempty->list ne)))

;;; ne-zip : NonEmpty a -> NonEmpty b -> NonEmpty (a . b)
;;; Zip two NonEmpty lists, truncating to shorter.
(define (ne-zip nea neb)
  (let* ([as (nonempty->list nea)]
         [bs (nonempty->list neb)]
         [zipped (let loop ([xs as] [ys bs] [acc '()])
                      (if (or (null? xs) (null? ys))
                          (reverse acc)
                          (loop (cdr xs) (cdr ys) (cons (cons (car xs) (car ys)) acc))))])
        (make-nonempty (car zipped) (cdr zipped))))

;;; ne-zip-with : (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
;;; Zip with custom combining function.
(define (ne-zip-with f nea neb)
  (let* ([as (nonempty->list nea)]
         [bs (nonempty->list neb)]
         [zipped (let loop ([xs as] [ys bs] [acc '()])
                      (if (or (null? xs) (null? ys))
                          (reverse acc)
                          (loop (cdr xs) (cdr ys) (cons (f (car xs) (car ys)) acc))))])
        (make-nonempty (car zipped) (cdr zipped))))

;;; ne-filter : (a -> Bool) -> NonEmpty a -> List a
;;; Filter elements (may return empty list).
(define (ne-filter pred ne)
  (filter pred (nonempty->list ne)))

;;; ne-partition : (a -> Bool) -> NonEmpty a -> (List a . List a)
;;; Partition into (matching . non-matching).
(define (ne-partition pred ne)
  (partition pred (nonempty->list ne)))

;;; ne-span : (a -> Bool) -> NonEmpty a -> (List a . List a)
;;; Split at first element not satisfying predicate.
(define (ne-span pred ne)
  (let loop ([lst (nonempty->list ne)] [acc '()])
       (cond
        [(null? lst) (cons (reverse acc) '())]
        [(pred (car lst)) (loop (cdr lst) (cons (car lst) acc))]
        [else (cons (reverse acc) lst)])))

;;; ne-group-by : (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
;;; Group adjacent elements by equivalence relation.
(define (ne-group-by eq? ne)
  (let loop ([lst (nonempty-tail ne)]
             [current (ne-singleton (nonempty-head ne))]
             [groups '()])
       (cond
        [(null? lst)
         (ne-reverse (ne-cons current (if (null? groups)
                                          (ne-singleton current)
                                          (ne-from-list (reverse (cons current groups))))))]
        [(eq? (nonempty-head current) (car lst))
         (loop (cdr lst) (ne-snoc current (car lst)) groups)]
        [else
         (loop (cdr lst) (ne-singleton (car lst)) (cons current groups))])))

;;; ne-sort-by : (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
;;; Sort NonEmpty by comparison function.
(define (ne-sort-by less? ne)
  (let ([sorted (list-sort less? (nonempty->list ne))])
       (make-nonempty (car sorted) (cdr sorted))))

;;; ne-nub : NonEmpty a -> NonEmpty a
;;; Remove adjacent duplicates.
(define (ne-nub ne)
  (let loop ([lst (nonempty-tail ne)]
             [last (nonempty-head ne)]
             [acc (list (nonempty-head ne))])
       (cond
        [(null? lst) (ne-from-list (reverse acc))]
        [(equal? (car lst) last) (loop (cdr lst) last acc)]
        [else (loop (cdr lst) (car lst) (cons (car lst) acc))])))

;;; ne-maximum : NonEmpty a -> a (assuming Ord)
;;; Get maximum element (always safe!).
(define (ne-maximum ne)
  (ne-foldl1 max ne))

;;; ne-minimum : NonEmpty a -> a (assuming Ord)
;;; Get minimum element (always safe!).
(define (ne-minimum ne)
  (ne-foldl1 min ne))

;;; ne-sum : NonEmpty Num -> Num
;;; Sum all elements.
(define (ne-sum ne)
  (ne-foldl1 + ne))

;;; ne-product : NonEmpty Num -> Num
;;; Product of all elements.
(define (ne-product ne)
  (ne-foldl1 * ne))

;;; ============================================================
;;; Comonad Instance
;;; ============================================================
;;;
;;; NonEmpty forms a Comonad:
;;;   extract = head
;;;   extend f ne = map over all tails including self

;;; ne-extract : NonEmpty a -> a
;;; Extract the focus (head).
(define ne-extract nonempty-head)

;;; ne-tails : NonEmpty a -> NonEmpty (NonEmpty a)
;;; Get all non-empty tails (including self).
(define (ne-tails ne)
  (let loop ([lst (nonempty->list ne)] [acc '()])
       (if (null? lst)
           (ne-from-list (reverse acc))
           (loop (cdr lst) (cons (ne-from-list lst) acc)))))

;;; ne-extend : (NonEmpty a -> b) -> NonEmpty a -> NonEmpty b
;;; Extend a function over all tails.
(define (ne-extend f ne)
  (ne-map f (ne-tails ne)))

;;; nonempty-comonad : Comonad NonEmpty
(define nonempty-comonad
  (make-comonad nonempty-functor ne-extract ne-extend))
