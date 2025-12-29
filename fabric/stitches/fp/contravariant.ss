;;; fabric/stitches/fp/contravariant.ss — Contravariant Functor
;;;
;;; A Contravariant functor is like a regular functor but "backwards".
;;; Where Functor has fmap : (a -> b) -> f a -> f b,
;;; Contravariant has contramap : (a -> b) -> f b -> f a
;;;
;;; The direction of the function is reversed!
;;;
;;; class Contravariant f where
;;;   contramap :: (a -> b) -> f b -> f a
;;;
;;; Laws:
;;;   contramap id = id
;;;   contramap f . contramap g = contramap (g . f)
;;;
;;; Common examples:
;;;   - Predicate: (a -> Bool) — testing membership
;;;   - Comparison: (a -> a -> Ordering) — sorting
;;;   - Equivalence: (a -> a -> Bool) — equality testing
;;;   - Op: Functions from a type (opposite category)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Contravariant Type
;;; ============================================================

;;; make-contravariant : ((a -> b) -> f b -> f a) -> Contravariant f
;;; Create a contravariant functor from its contramap operation.
(define (make-contravariant cmap)
  (list 'contravariant cmap))

;;; contravariant? : Any -> Boolean
(define (contravariant? x)
  (and (pair? x) (eq? (car x) 'contravariant)))

;;; contravariant-cmap : Contravariant f -> ((a -> b) -> f b -> f a)
(define (contravariant-cmap cf)
  (list-ref cf 1))

;;; cmap : Contravariant f -> (a -> b) -> f b -> f a
;;; Apply contramap.
(define (cmap cf f x)
  ((contravariant-cmap cf) f x))

;;; (>$<) : Contravariant f -> (a -> b) -> f b -> f a
;;; Operator alias for cmap (flip of <$> direction).
(define >$< cmap)

;;; (>$) : Contravariant f -> b -> f b -> f a
;;; Replace all values with a constant.
(define (>$ cf b fb)
  (cmap cf (const b) fb))

;;; ============================================================
;;; Predicate
;;; ============================================================
;;;
;;; Predicate a = a -> Bool
;;; Tests whether a value satisfies some condition.

;;; make-predicate : (a -> Bool) -> Predicate a
(define (make-predicate pred)
  (list 'predicate pred))

;;; predicate? : Any -> Boolean
(define (predicate? x)
  (and (pair? x) (eq? (car x) 'predicate)))

;;; get-predicate : Predicate a -> (a -> Bool)
(define (get-predicate p)
  (list-ref p 1))

;;; run-predicate : Predicate a -> a -> Bool
(define (run-predicate p x)
  ((get-predicate p) x))

;;; predicate-contravariant : Contravariant Predicate
(define predicate-contravariant
  (make-contravariant
   (lambda (f pb)
           (make-predicate (lambda (a) (run-predicate pb (f a)))))))

;;; predicate-contramap : (a -> b) -> Predicate b -> Predicate a
(define (predicate-contramap f pb)
  (cmap predicate-contravariant f pb))

;;; ============================================================
;;; Predicate Combinators
;;; ============================================================

;;; predicate-and : Predicate a -> Predicate a -> Predicate a
;;; Combine predicates with AND.
(define (predicate-and p1 p2)
  (make-predicate (lambda (x) (and (run-predicate p1 x) (run-predicate p2 x)))))

;;; predicate-or : Predicate a -> Predicate a -> Predicate a
;;; Combine predicates with OR.
(define (predicate-or p1 p2)
  (make-predicate (lambda (x) (or (run-predicate p1 x) (run-predicate p2 x)))))

;;; predicate-not : Predicate a -> Predicate a
;;; Negate a predicate.
(define (predicate-not p)
  (make-predicate (lambda (x) (not (run-predicate p x)))))

;;; predicate-all : Predicate a
;;; Always true.
(define predicate-all
  (make-predicate (lambda (x) #t)))

;;; predicate-none : Predicate a
;;; Always false.
(define predicate-none
  (make-predicate (lambda (x) #f)))

;;; ============================================================
;;; Comparison
;;; ============================================================
;;;
;;; Comparison a = a -> a -> Ordering
;;; where Ordering = 'lt | 'eq | 'gt

;;; make-comparison : (a -> a -> Symbol) -> Comparison a
(define (make-comparison cmp)
  (list 'comparison cmp))

;;; comparison? : Any -> Boolean
(define (comparison? x)
  (and (pair? x) (eq? (car x) 'comparison)))

;;; get-comparison : Comparison a -> (a -> a -> Symbol)
(define (get-comparison c)
  (list-ref c 1))

;;; run-comparison : Comparison a -> a -> a -> Symbol
(define (run-comparison c x y)
  ((get-comparison c) x y))

;;; comparison-contravariant : Contravariant Comparison
(define comparison-contravariant
  (make-contravariant
   (lambda (f cb)
           (make-comparison (lambda (a1 a2) (run-comparison cb (f a1) (f a2)))))))

;;; comparison-contramap : (a -> b) -> Comparison b -> Comparison a
;;; Compare by projecting to b first.
(define (comparison-contramap f cb)
  (cmap comparison-contravariant f cb))

;;; ============================================================
;;; Comparison Combinators
;;; ============================================================

;;; default-comparison : Comparison Number
;;; Standard numeric comparison.
(define default-comparison
  (make-comparison
   (lambda (x y)
           (cond [(< x y) 'lt]
                 [(> x y) 'gt]
                 [else 'eq]))))

;;; string-comparison : Comparison String
;;; Lexicographic string comparison.
(define string-comparison
  (make-comparison
   (lambda (x y)
           (cond [(string<? x y) 'lt]
                 [(string>? x y) 'gt]
                 [else 'eq]))))

;;; reverse-comparison : Comparison a -> Comparison a
;;; Flip the ordering.
(define (reverse-comparison c)
  (make-comparison
   (lambda (x y)
           (case (run-comparison c x y)
                 [(lt) 'gt]
                 [(gt) 'lt]
                 [(eq) 'eq]))))

;;; comparison-chain : Comparison a -> Comparison a -> Comparison a
;;; Use first comparison, fall back to second on equality.
(define (comparison-chain c1 c2)
  (make-comparison
   (lambda (x y)
           (let ([r (run-comparison c1 x y)])
                (if (eq? r 'eq)
                    (run-comparison c2 x y)
                    r)))))

;;; comparing : (a -> b) -> Comparison b -> Comparison a
;;; Alias for comparison-contramap.
(define comparing comparison-contramap)

;;; ============================================================
;;; Equivalence
;;; ============================================================
;;;
;;; Equivalence a = a -> a -> Bool
;;; Tests whether two values are equivalent.

;;; make-equivalence : (a -> a -> Bool) -> Equivalence a
(define (make-equivalence eq-fn)
  (list 'equivalence eq-fn))

;;; equivalence? : Any -> Boolean
(define (equivalence? x)
  (and (pair? x) (eq? (car x) 'equivalence)))

;;; get-equivalence : Equivalence a -> (a -> a -> Bool)
(define (get-equivalence e)
  (list-ref e 1))

;;; run-equivalence : Equivalence a -> a -> a -> Bool
(define (run-equivalence e x y)
  ((get-equivalence e) x y))

;;; equivalence-contravariant : Contravariant Equivalence
(define equivalence-contravariant
  (make-contravariant
   (lambda (f eb)
           (make-equivalence (lambda (a1 a2) (run-equivalence eb (f a1) (f a2)))))))

;;; equivalence-contramap : (a -> b) -> Equivalence b -> Equivalence a
;;; Test equivalence by projecting to b first.
(define (equivalence-contramap f eb)
  (cmap equivalence-contravariant f eb))

;;; ============================================================
;;; Equivalence Combinators
;;; ============================================================

;;; default-equivalence : Equivalence a
;;; Uses equal? for comparison.
(define default-equivalence
  (make-equivalence equal?))

;;; equivalence-and : Equivalence a -> Equivalence a -> Equivalence a
;;; Both equivalences must agree.
(define (equivalence-and e1 e2)
  (make-equivalence (lambda (x y) (and (run-equivalence e1 x y) (run-equivalence e2 x y)))))

;;; equivalence-or : Equivalence a -> Equivalence a -> Equivalence a
;;; Either equivalence agrees.
(define (equivalence-or e1 e2)
  (make-equivalence (lambda (x y) (or (run-equivalence e1 x y) (run-equivalence e2 x y)))))

;;; equivalence-from-comparison : Comparison a -> Equivalence a
;;; Two values are equivalent if comparison returns 'eq.
(define (equivalence-from-comparison c)
  (make-equivalence (lambda (x y) (eq? 'eq (run-comparison c x y)))))

;;; equating : (a -> b) -> Equivalence a
;;; Two values are equivalent if their projections are equal?.
(define (equating f)
  (make-equivalence (lambda (x y) (equal? (f x) (f y)))))

;;; ============================================================
;;; Op (Opposite Category)
;;; ============================================================
;;;
;;; Op a b = a -> b but with reversed composition
;;; This is the canonical contravariant functor.

;;; make-op : (a -> b) -> Op a b
(define (make-op f)
  (list 'op f))

;;; op? : Any -> Boolean
(define (op? x)
  (and (pair? x) (eq? (car x) 'op)))

;;; get-op : Op a b -> (a -> b)
(define (get-op o)
  (list-ref o 1))

;;; run-op : Op a b -> a -> b
(define (run-op o x)
  ((get-op o) x))

;;; op-contravariant : Contravariant (Op _ b) for fixed b
;;; contramap f (Op g) = Op (g . f)
(define op-contravariant
  (make-contravariant
   (lambda (f ob)
           (make-op (compose (get-op ob) f)))))

;;; ============================================================
;;; Law Verification
;;; ============================================================

;;; verify-contravariant-identity : Contravariant f -> f b -> Boolean
;;; Check that contramap id = id
(define (verify-contravariant-identity cf equiv-fn fb)
  (equiv-fn fb (cmap cf identity fb)))

;;; verify-contravariant-composition : Contravariant f -> (a -> b) -> (b -> c) -> f c -> (f a -> f a -> Boolean) -> Boolean
;;; Check that contramap f . contramap g = contramap (g . f)
(define (verify-contravariant-composition cf f g fc equiv-fn)
  (equiv-fn (cmap cf f (cmap cf g fc))
            (cmap cf (compose g f) fc)))

;;; ============================================================
;;; Divisible (Contravariant Applicative)
;;; ============================================================
;;;
;;; Divisible is to Contravariant as Applicative is to Functor.
;;;
;;; class Contravariant f => Divisible f where
;;;   divide  :: (a -> (b, c)) -> f b -> f c -> f a
;;;   conquer :: f a
;;;
;;; Laws:
;;;   divide f m conquer = contramap (fst . f) m
;;;   divide f conquer m = contramap (snd . f) m
;;;   divide f (divide g m n) o = divide f' m (divide g' n o)
;;;     where f' a = let (bc, d) = f a; (b, c) = g bc in (b, (c, d))
;;;           g' (c, d) = (c, d)

;;; make-divisible : Contravariant f -> f a -> ((a -> (b . c)) -> f b -> f c -> f a) -> Divisible f
(define (make-divisible cf conquer divide)
  (list 'divisible cf conquer divide))

;;; divisible? : Any -> Boolean
(define (divisible? x)
  (and (pair? x) (eq? (car x) 'divisible)))

;;; divisible-contravariant : Divisible f -> Contravariant f
(define (divisible-contravariant d)
  (list-ref d 1))

;;; divisible-conquer : Divisible f -> f a
(define (divisible-conquer d)
  (list-ref d 2))

;;; divisible-divide : Divisible f -> (a -> (b . c)) -> f b -> f c -> f a
(define (divisible-divide d)
  (list-ref d 3))

;;; predicate-divisible : Divisible Predicate
(define predicate-divisible
  (make-divisible
   predicate-contravariant
   predicate-all
   (lambda (f pb pc)
           (make-predicate
            (lambda (a)
                    (let ([bc (f a)])
                         (and (run-predicate pb (car bc))
                              (run-predicate pc (cdr bc)))))))))

;;; comparison-divisible : Divisible Comparison
(define comparison-divisible
  (make-divisible
   comparison-contravariant
   (make-comparison (lambda (x y) 'eq))
   (lambda (f cb cc)
           (make-comparison
            (lambda (a1 a2)
                    (let ([bc1 (f a1)]
                          [bc2 (f a2)])
                         (let ([r (run-comparison cb (car bc1) (car bc2))])
                              (if (eq? r 'eq)
                                  (run-comparison cc (cdr bc1) (cdr bc2))
                                  r))))))))

;;; equivalence-divisible : Divisible Equivalence
(define equivalence-divisible
  (make-divisible
   equivalence-contravariant
   (make-equivalence (lambda (x y) #t))
   (lambda (f eb ec)
           (make-equivalence
            (lambda (a1 a2)
                    (let ([bc1 (f a1)]
                          [bc2 (f a2)])
                         (and (run-equivalence eb (car bc1) (car bc2))
                              (run-equivalence ec (cdr bc1) (cdr bc2)))))))))

;;; ============================================================
;;; Practical Utilities
;;; ============================================================

;;; sort-by : Comparison a -> List a -> List a
;;; Sort a list using the given comparison.
(define (sort-by cmp lst)
  (define (merge l1 l2)
    (cond [(null? l1) l2]
          [(null? l2) l1]
          [(eq? 'gt (run-comparison cmp (car l1) (car l2)))
           (cons (car l2) (merge l1 (cdr l2)))]
          [else
           (cons (car l1) (merge (cdr l1) l2))]))
  (define (split lst)
    (let loop ([lst lst] [left '()] [right '()] [toggle #t])
         (if (null? lst)
             (cons left right)
             (if toggle
                 (loop (cdr lst) (cons (car lst) left) right #f)
                 (loop (cdr lst) left (cons (car lst) right) #t)))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ([halves (split lst)])
           (merge (sort-by cmp (car halves))
                  (sort-by cmp (cdr halves))))))

;;; filter-by : Predicate a -> List a -> List a
;;; Filter a list using the given predicate.
(define (filter-by pred lst)
  (filter (lambda (x) (run-predicate pred x)) lst))

;;; group-by-equivalence : Equivalence a -> List a -> List (List a)
;;; Group elements that are equivalent.
(define (group-by-equivalence eq lst)
  (if (null? lst)
      '()
      (let loop ([rest (cdr lst)] [group (list (car lst))] [groups '()])
           (cond [(null? rest)
                  (reverse (cons (reverse group) groups))]
                 [(run-equivalence eq (car group) (car rest))
                  (loop (cdr rest) (cons (car rest) group) groups)]
                 [else
                  (loop (cdr rest) (list (car rest)) (cons (reverse group) groups))]))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Predicate: check if string length > 5
;;; (define long-string (predicate-contramap string-length
;;;                                          (make-predicate (lambda (n) (> n 5)))))
;;; (run-predicate long-string "hello")      ; => #f
;;; (run-predicate long-string "hello world") ; => #t
;;;
;;; ;; Comparison: sort people by age
;;; (define person-age-cmp (comparing (lambda (p) (cdr (assoc 'age p)))
;;;                                   default-comparison))
;;; (sort-by person-age-cmp
;;;          '(((name . "Alice") (age . 30))
;;;            ((name . "Bob") (age . 25))
;;;            ((name . "Carol") (age . 35))))
;;; ; => sorted by age ascending
;;;
;;; ;; Equivalence: group by first letter
;;; (define first-letter-eq (equating (lambda (s) (string-ref s 0))))
;;; (group-by-equivalence first-letter-eq '("apple" "ant" "banana" "apricot"))
;;; ; => (("apple" "ant" "apricot") ("banana"))  ; if pre-sorted
;;;
;;; ;; Reverse comparison for descending sort
;;; (sort-by (reverse-comparison default-comparison) '(3 1 4 1 5 9))
;;; ; => (9 5 4 3 1 1)
