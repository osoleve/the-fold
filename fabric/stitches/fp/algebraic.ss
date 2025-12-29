;;; fabric/stitches/fp/algebraic.ss — Algebraic Typeclasses
;;;
;;; Implements the algebraic structure hierarchy:
;;;   Semigroup → Monoid → Group
;;;   with Abelian (commutative) variants
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Semigroup: associative binary operation (<>)
;;;   - Monoid: Semigroup with identity element (mempty)
;;;   - Group: Monoid with inverse
;;;   - Abelian variants: commutative operations
;;;   - Dual, Endo, First, Last, Sum, Product wrappers
;;;   - Generic folding operations
;;;   - Law verification utilities
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Laws:
;;;   Semigroup:
;;;     (a <> b) <> c = a <> (b <> c)     (associativity)
;;;
;;;   Monoid:
;;;     mempty <> a = a                    (left identity)
;;;     a <> mempty = a                    (right identity)
;;;
;;;   Group:
;;;     a <> inverse(a) = mempty           (right inverse)
;;;     inverse(a) <> a = mempty           (left inverse)
;;;
;;;   Abelian (commutative):
;;;     a <> b = b <> a                    (commutativity)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Semigroup Type Class
;;; ============================================================
;;;
;;; A semigroup is a set with an associative binary operation.
;;; The operation is typically called (<>) or mappend.

;;; make-semigroup : Symbol -> (a -> a -> a) -> Semigroup a
(define (make-semigroup name op)
  (list 'semigroup name op))

;;; semigroup? : Any -> Boolean
(define (semigroup? x)
  (and (pair? x) (eq? (car x) 'semigroup)))

;;; semigroup-name : Semigroup a -> Symbol
(define (semigroup-name sg)
  (list-ref sg 1))

;;; semigroup-op : Semigroup a -> (a -> a -> a)
(define (semigroup-op sg)
  (list-ref sg 2))

;;; (<>) : Semigroup a -> a -> a -> a
;;; The semigroup operation.
(define (sg-append sg a b)
  ((semigroup-op sg) a b))

;;; sconcat : Semigroup a -> NonEmpty (List a) -> a
;;; Fold a non-empty list with the semigroup operation.
;;; Assumes list is non-empty.
(define (sconcat sg lst)
  (if (null? (cdr lst))
      (car lst)
      (sg-append sg (car lst) (sconcat sg (cdr lst)))))

;;; stimes : Semigroup a -> Int -> a -> a
;;; Repeat an element n times using the semigroup operation.
;;; n must be >= 1.
(define (stimes sg n a)
  (if (<= n 1)
      a
      (sg-append sg a (stimes sg (- n 1) a))))

;;; ============================================================
;;; Monoid Type Class
;;; ============================================================
;;;
;;; A monoid is a semigroup with an identity element.

;;; make-monoid : Symbol -> a -> (a -> a -> a) -> Monoid a
(define (make-monoid name mempty op)
  (list 'monoid name mempty op))

;;; monoid? : Any -> Boolean
(define (monoid? x)
  (and (pair? x) (eq? (car x) 'monoid)))

;;; monoid-name : Monoid a -> Symbol
(define (monoid-name m)
  (list-ref m 1))

;;; monoid-empty : Monoid a -> a
(define (monoid-empty m)
  (list-ref m 2))

;;; monoid-op : Monoid a -> (a -> a -> a)
(define (monoid-op m)
  (list-ref m 3))

;;; mempty : Monoid a -> a
;;; The identity element.
(define monoid-mempty monoid-empty)

;;; mappend : Monoid a -> a -> a -> a
;;; The monoid operation.
(define (mappend m a b)
  ((monoid-op m) a b))

;;; mconcat : Monoid a -> List a -> a
;;; Fold a list with the monoid operation.
(define (mconcat m lst)
  (foldr (monoid-op m) (monoid-empty m) lst))

;;; monoid->semigroup : Monoid a -> Semigroup a
;;; Extract the underlying semigroup.
(define (monoid->semigroup m)
  (make-semigroup (monoid-name m) (monoid-op m)))

;;; ============================================================
;;; Group Type Class
;;; ============================================================
;;;
;;; A group is a monoid where every element has an inverse.

;;; make-group : Symbol -> a -> (a -> a -> a) -> (a -> a) -> Group a
(define (make-group name identity op inverse)
  (list 'group name identity op inverse))

;;; group? : Any -> Boolean
(define (group? x)
  (and (pair? x) (eq? (car x) 'group)))

;;; group-name : Group a -> Symbol
(define (group-name g)
  (list-ref g 1))

;;; group-identity : Group a -> a
(define (group-identity g)
  (list-ref g 2))

;;; group-op : Group a -> (a -> a -> a)
(define (group-op g)
  (list-ref g 3))

;;; group-inverse : Group a -> (a -> a)
(define (group-inverse-fn g)
  (list-ref g 4))

;;; inverse : Group a -> a -> a
;;; Get the inverse of an element.
(define (group-inverse g a)
  ((group-inverse-fn g) a))

;;; group-diff : Group a -> a -> a -> a
;;; Compute a <> inverse(b) = a - b in additive notation.
(define (group-diff g a b)
  ((group-op g) a ((group-inverse-fn g) b)))

;;; group->monoid : Group a -> Monoid a
;;; Extract the underlying monoid.
(define (group->monoid g)
  (make-monoid (group-name g) (group-identity g) (group-op g)))

;;; group->semigroup : Group a -> Semigroup a
;;; Extract the underlying semigroup.
(define (group->semigroup g)
  (make-semigroup (group-name g) (group-op g)))

;;; ============================================================
;;; Abelian (Commutative) Variants
;;; ============================================================

;;; make-abelian-semigroup : Symbol -> (a -> a -> a) -> AbelianSemigroup a
(define (make-abelian-semigroup name op)
  (list 'abelian-semigroup name op))

;;; abelian-semigroup? : Any -> Boolean
(define (abelian-semigroup? x)
  (and (pair? x) (eq? (car x) 'abelian-semigroup)))

;;; make-abelian-monoid : Symbol -> a -> (a -> a -> a) -> AbelianMonoid a
(define (make-abelian-monoid name mempty op)
  (list 'abelian-monoid name mempty op))

;;; abelian-monoid? : Any -> Boolean
(define (abelian-monoid? x)
  (and (pair? x) (eq? (car x) 'abelian-monoid)))

;;; make-abelian-group : Symbol -> a -> (a -> a -> a) -> (a -> a) -> AbelianGroup a
(define (make-abelian-group name identity op inverse)
  (list 'abelian-group name identity op inverse))

;;; abelian-group? : Any -> Boolean
(define (abelian-group? x)
  (and (pair? x) (eq? (car x) 'abelian-group)))

;;; ============================================================
;;; Common Instances: List
;;; ============================================================

;;; list-semigroup : Semigroup (List a)
;;; Lists form a semigroup under concatenation.
(define list-semigroup
  (make-semigroup 'list append))

;;; list-monoid : Monoid (List a)
;;; Lists form a monoid with empty list as identity.
(define list-monoid
  (make-monoid 'list '() append))

;;; ============================================================
;;; Common Instances: String
;;; ============================================================

;;; string-semigroup : Semigroup String
(define string-semigroup
  (make-semigroup 'string string-append))

;;; string-monoid : Monoid String
(define string-monoid
  (make-monoid 'string "" string-append))

;;; ============================================================
;;; Common Instances: Numbers (Additive)
;;; ============================================================

;;; sum-semigroup : Semigroup Number
;;; Numbers form a semigroup under addition.
(define sum-semigroup
  (make-abelian-semigroup 'sum +))

;;; sum-monoid : Monoid Number
;;; Numbers form a monoid under addition with 0 as identity.
(define sum-monoid
  (make-abelian-monoid 'sum 0 +))

;;; sum-group : Group Number
;;; Numbers form a group under addition with negation as inverse.
(define sum-group
  (make-abelian-group 'sum 0 + -))

;;; ============================================================
;;; Common Instances: Numbers (Multiplicative)
;;; ============================================================

;;; product-semigroup : Semigroup Number
;;; Numbers form a semigroup under multiplication.
(define product-semigroup
  (make-abelian-semigroup 'product *))

;;; product-monoid : Monoid Number
;;; Numbers form a monoid under multiplication with 1 as identity.
(define product-monoid
  (make-abelian-monoid 'product 1 *))

;;; nonzero-product-group : Group Number
;;; Non-zero numbers form a group under multiplication.
;;; Note: Caller must ensure non-zero values.
(define nonzero-product-group
  (make-abelian-group 'nonzero-product 1 * (lambda (x) (/ 1 x))))

;;; ============================================================
;;; Common Instances: Boolean
;;; ============================================================

;;; all-monoid : Monoid Boolean
;;; Booleans under conjunction (AND).
(define all-monoid
  (make-abelian-monoid 'all #t (lambda (a b) (and a b))))

;;; any-monoid : Monoid Boolean
;;; Booleans under disjunction (OR).
(define any-monoid
  (make-abelian-monoid 'any #f (lambda (a b) (or a b))))

;;; xor-group : Group Boolean
;;; Booleans form a group under XOR.
(define (bool-xor a b)
  (and (or a b) (not (and a b))))

(define xor-group
  (make-abelian-group 'xor #f bool-xor identity))

;;; ============================================================
;;; Common Instances: Max/Min
;;; ============================================================

;;; Note: These require a bounded type for true monoid.
;;; We use +inf.0/-inf.0 for numbers.

;;; max-monoid : Monoid Number
;;; Numbers under maximum with -inf as identity.
(define max-monoid
  (make-abelian-monoid 'max -inf.0 max))

;;; min-monoid : Monoid Number
;;; Numbers under minimum with +inf as identity.
(define min-monoid
  (make-abelian-monoid 'min +inf.0 min))

;;; ============================================================
;;; Common Instances: Functions
;;; ============================================================

;;; endo-semigroup : Semigroup (a -> a)
;;; Endomorphisms form a semigroup under composition.
(define endo-semigroup
  (make-semigroup 'endo compose))

;;; endo-monoid : Monoid (a -> a)
;;; Endomorphisms form a monoid with identity function.
(define endo-monoid
  (make-monoid 'endo identity compose))

;;; ============================================================
;;; Wrapper Types
;;; ============================================================

;;; Dual: Reverses the operation order
;;; dual-op(a, b) = original-op(b, a)

;;; make-dual : a -> Dual a
(define (make-dual value)
  (list 'dual value))

;;; dual? : Any -> Boolean
(define (dual? x)
  (and (pair? x) (eq? (car x) 'dual)))

;;; get-dual : Dual a -> a
(define (get-dual d)
  (list-ref d 1))

;;; dual-semigroup : Semigroup a -> Semigroup (Dual a)
(define (dual-semigroup sg)
  (make-semigroup
   (string->symbol (string-append "dual-" (symbol->string (semigroup-name sg))))
   (lambda (d1 d2)
           (make-dual ((semigroup-op sg) (get-dual d2) (get-dual d1))))))

;;; dual-monoid : Monoid a -> Monoid (Dual a)
(define (dual-monoid m)
  (make-monoid
   (string->symbol (string-append "dual-" (symbol->string (monoid-name m))))
   (make-dual (monoid-empty m))
   (lambda (d1 d2)
           (make-dual ((monoid-op m) (get-dual d2) (get-dual d1))))))

;;; ============================================================
;;; Endo Wrapper
;;; ============================================================

;;; Endo wraps a -> a functions to make the monoid instance explicit.

;;; make-endo : (a -> a) -> Endo a
(define (make-endo f)
  (list 'endo f))

;;; endo? : Any -> Boolean
(define (endo? x)
  (and (pair? x) (eq? (car x) 'endo)))

;;; app-endo : Endo a -> a -> a
(define (app-endo e x)
  ((list-ref e 1) x))

;;; endo-wrapped-monoid : Monoid (Endo a)
(define endo-wrapped-monoid
  (make-monoid
   'endo-wrapped
   (make-endo identity)
   (lambda (e1 e2)
           (make-endo (compose (list-ref e1 1) (list-ref e2 1))))))

;;; ============================================================
;;; First and Last Wrappers
;;; ============================================================

;;; First: Keep the first non-Nothing value

;;; make-first : Maybe a -> First a
(define (make-first maybe-val)
  (list 'first maybe-val))

;;; first? : Any -> Boolean
(define (first? x)
  (and (pair? x) (eq? (car x) 'first)))

;;; get-first : First a -> Maybe a
(define (get-first f)
  (list-ref f 1))

;;; first-semigroup : Semigroup (First a)
(define first-semigroup
  (make-semigroup
   'first
   (lambda (f1 f2)
           (let ([m1 (get-first f1)])
                (if (just? m1) f1 f2)))))

;;; first-monoid : Monoid (First a)
(define first-monoid
  (make-monoid
   'first
   (make-first nothing)
   (lambda (f1 f2)
           (let ([m1 (get-first f1)])
                (if (just? m1) f1 f2)))))

;;; Last: Keep the last non-Nothing value

;;; make-last : Maybe a -> Last a
(define (make-last maybe-val)
  (list 'last maybe-val))

;;; last? : Any -> Boolean
(define (last? x)
  (and (pair? x) (eq? (car x) 'last)))

;;; get-last : Last a -> Maybe a
(define (get-last l)
  (list-ref l 1))

;;; last-semigroup : Semigroup (Last a)
(define last-semigroup
  (make-semigroup
   'last
   (lambda (l1 l2)
           (let ([m2 (get-last l2)])
                (if (just? m2) l2 l1)))))

;;; last-monoid : Monoid (Last a)
(define last-monoid
  (make-monoid
   'last
   (make-last nothing)
   (lambda (l1 l2)
           (let ([m2 (get-last l2)])
                (if (just? m2) l2 l1)))))

;;; ============================================================
;;; Sum and Product Wrappers
;;; ============================================================

;;; Sum: Numbers under addition

;;; make-sum : Number -> Sum
(define (make-sum n)
  (list 'sum n))

;;; sum? : Any -> Boolean
(define (sum? x)
  (and (pair? x) (eq? (car x) 'sum)))

;;; get-sum : Sum -> Number
(define (get-sum s)
  (list-ref s 1))

;;; sum-wrapped-monoid : Monoid Sum
(define sum-wrapped-monoid
  (make-abelian-monoid
   'sum-wrapped
   (make-sum 0)
   (lambda (s1 s2)
           (make-sum (+ (get-sum s1) (get-sum s2))))))

;;; Product: Numbers under multiplication

;;; make-product : Number -> Product
(define (make-product n)
  (list 'product n))

;;; product? : Any -> Boolean
(define (product? x)
  (and (pair? x) (eq? (car x) 'product)))

;;; get-product : Product -> Number
(define (get-product p)
  (list-ref p 1))

;;; product-wrapped-monoid : Monoid Product
(define product-wrapped-monoid
  (make-abelian-monoid
   'product-wrapped
   (make-product 1)
   (lambda (p1 p2)
           (make-product (* (get-product p1) (get-product p2))))))

;;; ============================================================
;;; Generic Folding with Monoids
;;; ============================================================

;;; fold-map : Monoid b -> (a -> b) -> List a -> b
;;; Map each element to a monoid value, then combine.
(define (fold-map m f lst)
  (mconcat m (map f lst)))

;;; fold-map-semigroup : Semigroup b -> (a -> b) -> NonEmpty (List a) -> b
;;; For semigroups (non-empty lists only).
(define (fold-map-semigroup sg f lst)
  (sconcat sg (map f lst)))

;;; when-monoid : Monoid a -> Bool -> a -> a
;;; Return the value if condition is true, mempty otherwise.
(define (when-monoid m condition value)
  (if condition value (monoid-empty m)))

;;; guard-monoid : Monoid a -> Bool -> a
;;; Return mempty if condition is true, used with filter semantics.
(define (guard-monoid m condition)
  (if condition (monoid-empty m) (monoid-empty m)))

;;; ============================================================
;;; Monoid Actions
;;; ============================================================

;;; A monoid action is when a monoid "acts" on a set.
;;; act : m -> a -> a where:
;;;   act(mempty, x) = x
;;;   act(m1 <> m2, x) = act(m1, act(m2, x))

;;; make-action : Monoid m -> (m -> a -> a) -> Action m a
(define (make-action monoid act-fn)
  (list 'action monoid act-fn))

;;; action? : Any -> Boolean
(define (action? x)
  (and (pair? x) (eq? (car x) 'action)))

;;; action-monoid : Action m a -> Monoid m
(define (action-monoid act)
  (list-ref act 1))

;;; action-act : Action m a -> m -> a -> a
(define (action-act act m a)
  ((list-ref act 2) m a))

;;; Example: List length action on natural numbers
;;; The natural numbers act on lists by taking the first n elements.
(define list-take-action
  (make-action
   sum-monoid
   (lambda (n lst)
           (take-n n lst))))

(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; ============================================================
;;; Tuple Instances
;;; ============================================================

;;; Pairs of monoids form a monoid.

;;; pair-monoid : Monoid a -> Monoid b -> Monoid (a, b)
(define (pair-monoid m1 m2)
  (make-monoid
   (string->symbol (format "pair-~a-~a" (monoid-name m1) (monoid-name m2)))
   (cons (monoid-empty m1) (monoid-empty m2))
   (lambda (p1 p2)
           (cons ((monoid-op m1) (car p1) (car p2))
                 ((monoid-op m2) (cdr p1) (cdr p2))))))

;;; triple-monoid : Monoid a -> Monoid b -> Monoid c -> Monoid (a, b, c)
(define (triple-monoid m1 m2 m3)
  (make-monoid
   (string->symbol (format "triple-~a-~a-~a"
                           (monoid-name m1) (monoid-name m2) (monoid-name m3)))
   (list (monoid-empty m1) (monoid-empty m2) (monoid-empty m3))
   (lambda (t1 t2)
           (list ((monoid-op m1) (car t1) (car t2))
                 ((monoid-op m2) (cadr t1) (cadr t2))
                 ((monoid-op m3) (caddr t1) (caddr t2))))))

;;; ============================================================
;;; Maybe/Option Instances
;;; ============================================================

;;; maybe-semigroup : Semigroup a -> Semigroup (Maybe a)
;;; Lift a semigroup to Maybe, combining Just values.
(define (maybe-semigroup sg)
  (make-semigroup
   (string->symbol (string-append "maybe-" (symbol->string (semigroup-name sg))))
   (lambda (m1 m2)
           (cond
            [(nothing? m1) m2]
            [(nothing? m2) m1]
            [else (just ((semigroup-op sg) (from-just m1) (from-just m2)))]))))

;;; maybe-monoid : Semigroup a -> Monoid (Maybe a)
;;; Lift a semigroup to a Maybe monoid (Nothing is identity).
(define (maybe-monoid sg)
  (make-monoid
   (string->symbol (string-append "maybe-" (symbol->string (semigroup-name sg))))
   nothing
   (lambda (m1 m2)
           (cond
            [(nothing? m1) m2]
            [(nothing? m2) m1]
            [else (just ((semigroup-op sg) (from-just m1) (from-just m2)))]))))

;;; ============================================================
;;; Law Verification
;;; ============================================================

;;; verify-semigroup-law : Semigroup a -> a -> a -> a -> Boolean
;;; Check associativity: (a <> b) <> c = a <> (b <> c)
(define (verify-semigroup-law sg a b c)
  (let ([op (semigroup-op sg)])
       (equal? (op (op a b) c)
               (op a (op b c)))))

;;; verify-monoid-laws : Monoid a -> a -> Boolean
;;; Check identity laws: mempty <> a = a, a <> mempty = a
(define (verify-monoid-laws m a)
  (let ([op (monoid-op m)]
        [e (monoid-empty m)])
       (and (equal? (op e a) a)
            (equal? (op a e) a))))

;;; verify-group-laws : Group a -> a -> Boolean
;;; Check inverse laws.
(define (verify-group-laws g a)
  (let ([op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)])
       (and (equal? (op a (inv a)) e)
            (equal? (op (inv a) a) e))))

;;; verify-commutativity : (a -> a -> a) -> a -> a -> Boolean
(define (verify-commutativity op a b)
  (equal? (op a b) (op b a)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; replicate-monoid : Monoid a -> Int -> a -> a
;;; Combine n copies of a value (0 gives mempty).
(define (replicate-monoid m n a)
  (if (<= n 0)
      (monoid-empty m)
      (mconcat m (make-list n a))))

(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;;; power-group : Group a -> Int -> a -> a
;;; Raise to integer power (negative uses inverse).
(define (power-group g n a)
  (cond
   [(= n 0) (group-identity g)]
   [(< n 0) (power-group g (- n) ((group-inverse-fn g) a))]
   [else (let ([op (group-op g)])
              (let loop ([k n] [acc (group-identity g)])
                   (if (<= k 0)
                       acc
                       (loop (- k 1) (op acc a)))))]))

;;; ============================================================
;;; Comparison Semigroups
;;; ============================================================

;;; ordering-semigroup : Semigroup Ordering
;;; For combining comparison results (like lexicographic ordering).
;;; Ordering = 'LT | 'EQ | 'GT
(define ordering-semigroup
  (make-semigroup
   'ordering
   (lambda (o1 o2)
           (if (eq? o1 'EQ) o2 o1))))

;;; ordering-monoid : Monoid Ordering
(define ordering-monoid
  (make-monoid
   'ordering
   'EQ
   (lambda (o1 o2)
           (if (eq? o1 'EQ) o2 o1))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; List monoid
;;; (mconcat list-monoid '((1 2) (3) (4 5 6)))  ; => (1 2 3 4 5 6)
;;;
;;; ;; Sum monoid
;;; (mconcat sum-monoid '(1 2 3 4 5))  ; => 15
;;;
;;; ;; Product monoid
;;; (mconcat product-monoid '(1 2 3 4 5))  ; => 120
;;;
;;; ;; First monoid (gets first non-Nothing)
;;; (mconcat first-monoid (list (make-first nothing)
;;;                             (make-first (just 1))
;;;                             (make-first (just 2))))
;;; ; => (first (just 1))
;;;
;;; ;; Fold-map: sum of lengths
;;; (fold-map sum-monoid length '((1 2) (3 4 5) (6)))  ; => 6
;;;
;;; ;; Dual reverses operation order
;;; (mconcat (dual-monoid list-monoid)
;;;          (list (make-dual '(1)) (make-dual '(2)) (make-dual '(3))))
;;; ; => (dual (3 2 1))
;;;
;;; ;; Pair monoid
;;; (mconcat (pair-monoid sum-monoid product-monoid)
;;;          '((1 . 2) (3 . 4) (5 . 6)))
;;; ; => (9 . 48)
;;;
;;; ;; Endo monoid (function composition)
;;; (app-endo (mconcat endo-wrapped-monoid
;;;                    (list (make-endo add1)
;;;                          (make-endo (lambda (x) (* x 2)))
;;;                          (make-endo add1)))
;;;           5)
;;; ; => 13  (computed as: add1(mul2(add1(5))) = add1(mul2(6)) = add1(12) = 13)
