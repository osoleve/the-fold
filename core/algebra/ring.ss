;;; core/algebra/ring.ss — Ring Theory Library
;;;
;;; Pure, functional implementation of ring structures:
;;; - Ring representation and operations
;;; - Commutative rings
;;; - Integral domains
;;; - Unique factorization domains (UFD)
;;; - Ring homomorphisms
;;; - Ideals and quotient rings
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - algebra/group.ss (for additive group structure)

(load "core/base/prelude.ss")
(load "core/algebra/group.ss")

;;; ============================================================
;;; Ring Representation
;;; ============================================================

;;; A Ring is represented as:
;;; (ring elements add-op mul-op zero one neg-fn equal-fn)
;;; - elements: list of ring elements
;;; - add-op: addition (a, b) → a + b
;;; - mul-op: multiplication (a, b) → a × b
;;; - zero: additive identity (0)
;;; - one: multiplicative identity (1)
;;; - neg-fn: additive inverse a → -a
;;; - equal-fn: equality predicate for elements
;;;
;;; Ring axioms:
;;; 1. (R, +) is an abelian group
;;; 2. Multiplication is associative: (a × b) × c = a × (b × c)
;;; 3. Distributive: a × (b + c) = (a × b) + (a × c)
;;;                 (a + b) × c = (a × c) + (b × c)

;;; make-ring : (List α) × (α × α → α) × (α × α → α) × α × α × (α → α) × (α × α → Boolean) → Ring
(define (make-ring elements add-op mul-op zero one neg-fn equal-fn)
  (list 'ring elements add-op mul-op zero one neg-fn equal-fn))

;;; ring? : α → Boolean
(define (ring? x)
  (and (list? x)
       (>= (length x) 8)
       (eq? (car x) 'ring)))

;;; ring-elements : Ring → (List Element)
(define (ring-elements r) (list-ref r 1))
;;; ring-add-op : Ring → (Element × Element → Element)
(define (ring-add-op r) (list-ref r 2))
;;; ring-mul-op : Ring → (Element × Element → Element)
(define (ring-mul-op r) (list-ref r 3))
;;; ring-zero : Ring → Element
(define (ring-zero r) (list-ref r 4))
;;; ring-one : Ring → Element
(define (ring-one r) (list-ref r 5))
;;; ring-neg-fn : Ring → (Element → Element)
(define (ring-neg-fn r) (list-ref r 6))
;;; ring-equal-fn : Ring → (Element × Element → Boolean)
(define (ring-equal-fn r) (list-ref r 7))

;;; ring-order : Ring → Integer
(define (ring-order r)
  (length (ring-elements r)))

;;; ============================================================
;;; Ring Operations
;;; ============================================================

;;; ring-add : Ring × Element × Element → Element
(define (ring-add r a b)
  ((ring-add-op r) a b))

;;; ring-mul : Ring × Element × Element → Element
(define (ring-mul r a b)
  ((ring-mul-op r) a b))

;;; ring-neg : Ring × Element → Element
;;; Additive inverse: -a
(define (ring-neg r a)
  ((ring-neg-fn r) a))

;;; ring-sub : Ring × Element × Element → Element
;;; Subtraction: a - b = a + (-b)
(define (ring-sub r a b)
  (ring-add r a (ring-neg r b)))

;;; ring-equal? : Ring × Element × Element → Boolean
(define (ring-equal? r a b)
  ((ring-equal-fn r) a b))

;;; ring-power : Ring × Element × Natural → Element
;;; Compute a^n using repeated squaring.
(define (ring-power r a n)
  (cond
   [(= n 0) (ring-one r)]
   [(= n 1) a]
   [(even? n)
    (let ([half (ring-power r a (/ n 2))])
         (ring-mul r half half))]
   [else
    (ring-mul r a (ring-power r a (- n 1)))]))

;;; ring-sum : Ring × (List Element) → Element
;;; Sum of a list of elements.
(define (ring-sum r elements)
  (if (null? elements)
      (ring-zero r)
      (let loop ([elems (cdr elements)] [acc (car elements)])
           (if (null? elems)
               acc
               (loop (cdr elems) (ring-add r acc (car elems)))))))

;;; ring-product : Ring × (List Element) → Element
;;; Product of a list of elements.
(define (ring-product r elements)
  (if (null? elements)
      (ring-one r)
      (let loop ([elems (cdr elements)] [acc (car elements)])
           (if (null? elems)
               acc
               (loop (cdr elems) (ring-mul r acc (car elems)))))))

;;; ============================================================
;;; Ring Properties
;;; ============================================================

;;; is-commutative-ring? : Ring → Boolean
;;; Check if multiplication is commutative: a × b = b × a
(define (is-commutative-ring? r)
  (let ([elems (ring-elements r)])
       (let outer-loop ([as elems])
            (if (null? as)
                #t
                (let inner-loop ([bs elems])
                     (if (null? bs)
                         (outer-loop (cdr as))
                         (let ([a (car as)]
                               [b (car bs)])
                              (if (ring-equal? r
                                               (ring-mul r a b)
                                               (ring-mul r b a))
                                  (inner-loop (cdr bs))
                                  #f))))))))

;;; is-zero-divisor? : Ring × Element → Boolean
;;; Check if a is a zero divisor: ∃b≠0 such that a×b=0 or b×a=0
(define (is-zero-divisor? r a)
  (let ([elems (ring-elements r)]
        [zero (ring-zero r)])
       (if (ring-equal? r a zero)
           #f  ; Zero itself is not considered a zero divisor
           (let loop ([bs elems])
                (if (null? bs)
                    #f
                    (let ([b (car bs)])
                         (if (and (not (ring-equal? r b zero))
                                  (or (ring-equal? r (ring-mul r a b) zero)
                                      (ring-equal? r (ring-mul r b a) zero)))
                             #t
                             (loop (cdr bs)))))))))

;;; has-zero-divisors? : Ring → Boolean
(define (has-zero-divisors? r)
  (let ([elems (ring-elements r)])
       (let loop ([as elems])
            (if (null? as)
                #f
                (if (is-zero-divisor? r (car as))
                    #t
                    (loop (cdr as)))))))

;;; is-integral-domain? : Ring → Boolean
;;; An integral domain is a commutative ring with no zero divisors.
(define (is-integral-domain? r)
  (and (is-commutative-ring? r)
       (not (has-zero-divisors? r))))

;;; is-unit? : Ring × Element → Boolean
;;; Check if element has a multiplicative inverse.
(define (is-unit? r a)
  (let ([elems (ring-elements r)]
        [one (ring-one r)])
       (let loop ([bs elems])
            (if (null? bs)
                #f
                (if (ring-equal? r (ring-mul r a (car bs)) one)
                    #t
                    (loop (cdr bs)))))))

;;; ring-units : Ring → (List Element)
;;; Return all units (invertible elements) in the ring.
(define (ring-units r)
  (filter (lambda (a) (is-unit? r a))
          (ring-elements r)))

;;; ============================================================
;;; Standard Rings
;;; ============================================================

;;; make-ring-zn : Natural → Ring
;;; Create the ring Z_n of integers modulo n.
(define (make-ring-zn n)
  (let ([elements (range 0 n)])
       (make-ring
        elements
        (lambda (a b) (modulo (+ a b) n))  ; Addition mod n
        (lambda (a b) (modulo (* a b) n))  ; Multiplication mod n
        0                                   ; Zero
        1                                   ; One
        (lambda (a) (modulo (- a) n))      ; Negation
        =)))                                ; Equality

;;; make-ring-z : → Ring
;;; WARNING: This is a DEMONSTRATION-ONLY truncated representation of Z.
;;; The finite element set [-100, 100] VIOLATES ring closure axioms:
;;;   - Addition: 60 + 60 = 120 (outside range)
;;;   - Multiplication: 50 * 50 = 2500 (outside range)
;;; Use ONLY for testing with small values. For production, use make-ring-zn
;;; for modular arithmetic, or represent Z symbolically.
(define (make-ring-z)
  (let ([elements (range -100 101)])
       (make-ring
        elements
        +         ; Addition
        *         ; Multiplication
        0         ; Zero
        1         ; One
        -         ; Negation
        =)))      ; Equality

;;; ============================================================
;;; Ring Homomorphisms
;;; ============================================================

;;; A ring homomorphism is a function φ: R → S such that:
;;; - φ(a + b) = φ(a) + φ(b)
;;; - φ(a × b) = φ(a) × φ(b)
;;; - φ(1_R) = 1_S

;;; make-ring-homomorphism : Ring × Ring × (Element → Element) → Homomorphism
(define (make-ring-homomorphism source target phi)
  (list 'ring-homomorphism source target phi))

;;; ring-hom? : α → Boolean
(define (ring-hom? h)
  (and (list? h) (eq? (car h) 'ring-homomorphism)))

;;; ring-hom-source : RingHomomorphism → Ring
(define (ring-hom-source h) (list-ref h 1))
;;; ring-hom-target : RingHomomorphism → Ring
(define (ring-hom-target h) (list-ref h 2))
;;; ring-hom-phi : RingHomomorphism → (Element → Element)
(define (ring-hom-phi h) (list-ref h 3))

;;; ring-hom-apply : Homomorphism × Element → Element
(define (ring-hom-apply h a)
  ((ring-hom-phi h) a))

;;; is-valid-ring-homomorphism? : Homomorphism → Boolean
;;; Verify homomorphism properties.
(define (is-valid-ring-homomorphism? h)
  (let ([r (ring-hom-source h)]
        [s (ring-hom-target h)]
        [phi (ring-hom-phi h)])
       (let ([elems (ring-elements r)])
            ; Check φ(1_R) = 1_S
            (and (ring-equal? s (phi (ring-one r)) (ring-one s))
                 ; Check φ(a + b) = φ(a) + φ(b) and φ(a × b) = φ(a) × φ(b)
                 (let loop ([as elems])
                      (if (null? as)
                          #t
                          (let ([a (car as)])
                               (let inner-loop ([bs elems])
                                    (if (null? bs)
                                        (loop (cdr as))
                                        (let* ([b (car bs)]
                                               [sum-ab (ring-add r a b)]
                                               [prod-ab (ring-mul r a b)])
                                              (and (ring-equal? s
                                                                (phi sum-ab)
                                                                (ring-add s (phi a) (phi b)))
                                                   (ring-equal? s
                                                                (phi prod-ab)
                                                                (ring-mul s (phi a) (phi b)))
                                                   (inner-loop (cdr bs)))))))))))))

;;; ring-hom-kernel : Homomorphism → (List Element)
;;; The kernel is {a ∈ R | φ(a) = 0_S}
(define (ring-hom-kernel h)
  (let ([r (ring-hom-source h)]
        [s (ring-hom-target h)]
        [phi (ring-hom-phi h)])
       (filter (lambda (a)
                       (ring-equal? s (phi a) (ring-zero s)))
               (ring-elements r))))

;;; ring-hom-image : Homomorphism → (List Element)
;;; The image is {φ(a) | a ∈ R}
(define (ring-hom-image h)
  (let ([phi (ring-hom-phi h)]
        [r (ring-hom-source h)])
       (remove-duplicates
        (map phi (ring-elements r))
        (ring-equal-fn (ring-hom-target h)))))

;;; ============================================================
;;; Ideals
;;; ============================================================

;;; An ideal I of ring R is a subset such that:
;;; 1. (I, +) is a subgroup of (R, +)
;;; 2. For all r ∈ R and i ∈ I: r×i ∈ I and i×r ∈ I

;;; make-ideal : Ring × (List Element) → Ideal
(define (make-ideal r elements)
  (list 'ideal r elements))

;;; ideal? : α → Boolean
(define (ideal? i)
  (and (list? i) (eq? (car i) 'ideal)))

;;; ideal-ring : Ideal → Ring
(define (ideal-ring i) (list-ref i 1))
;;; ideal-elements : Ideal → (List Element)
(define (ideal-elements i) (list-ref i 2))

;;; is-valid-ideal? : Ideal → Boolean
;;; Verify ideal properties.
(define (is-valid-ideal? ideal)
  (let ([r (ideal-ring ideal)]
        [i-elems (ideal-elements ideal)]
        [r-elems (ring-elements r)])
       ; Check closure under addition
       (and (let loop ([as i-elems])
                 (if (null? as)
                     #t
                     (let inner-loop ([bs i-elems])
                          (if (null? bs)
                              (loop (cdr as))
                              (and (member-equal (ring-add r (car as) (car bs)) i-elems (ring-equal-fn r))
                                   (inner-loop (cdr bs)))))))
            ; Check closure under ring multiplication
            (let loop ([rs r-elems])
                 (if (null? rs)
                     #t
                     (let inner-loop ([is i-elems])
                          (if (null? is)
                              (loop (cdr rs))
                              (and (member-equal (ring-mul r (car rs) (car is)) i-elems (ring-equal-fn r))
                                   (member-equal (ring-mul r (car is) (car rs)) i-elems (ring-equal-fn r))
                                   (inner-loop (cdr is))))))))))

;;; make-principal-ideal : Ring × Element → Ideal
;;; Generate the principal ideal <a> = {r×a | r ∈ R}
(define (make-principal-ideal r a)
  (let ([elems (remove-duplicates
                (map (lambda (r-elem) (ring-mul r r-elem a))
                     (ring-elements r))
                (ring-equal-fn r))])
       (make-ideal r elems)))

;;; ideal-sum : Ideal × Ideal → Ideal
;;; I + J = {i + j | i ∈ I, j ∈ J}
(define (ideal-sum i1 i2)
  (let* ([r (ideal-ring i1)]
         [elems (remove-duplicates
                 (apply append
                        (map (lambda (a)
                                     (map (lambda (b)
                                                  (ring-add r a b))
                                          (ideal-elements i2)))
                             (ideal-elements i1)))
                 (ring-equal-fn r))])
        (make-ideal r elems)))

;;; ideal-product : Ideal × Ideal → Ideal
;;; I × J = {finite sums of i×j | i ∈ I, j ∈ J}
;;; Computes additive closure via fixed-point iteration
(define (ideal-product i1 i2)
  (let* ([r (ideal-ring i1)]
         [eq-fn (ring-equal-fn r)]
         [initial-products (apply append
                                  (map (lambda (a)
                                               (map (lambda (b)
                                                            (ring-mul r a b))
                                                    (ideal-elements i2)))
                                       (ideal-elements i1)))]
         [seeds (remove-duplicates initial-products eq-fn)])
        (let loop ([elems seeds])
             (let* ([new-sums (apply append
                                     (map (lambda (a)
                                                  (map (lambda (b)
                                                               (ring-add r a b))
                                                       elems))
                                          elems))]
                    [next-elems (remove-duplicates (append elems new-sums) eq-fn)])
                   (if (= (length next-elems) (length elems))
                       (make-ideal r next-elems)
                       (loop next-elems))))))

;;; is-prime-ideal? : Ideal → Boolean
;;; I is prime if a×b ∈ I implies a ∈ I or b ∈ I
(define (is-prime-ideal? ideal)
  (let ([r (ideal-ring ideal)]
        [i-elems (ideal-elements ideal)]
        [r-elems (ring-elements r)])
       (let loop ([as r-elems])
            (if (null? as)
                #t
                (let inner-loop ([bs r-elems])
                     (if (null? bs)
                         (loop (cdr as))
                         (let ([a (car as)]
                               [b (car bs)]
                               [prod (ring-mul r (car as) (car bs))])
                              (if (member-equal prod i-elems (ring-equal-fn r))
                                  ; If a×b is in ideal, then a or b must be in ideal
                                  (if (or (member-equal a i-elems (ring-equal-fn r))
                                          (member-equal b i-elems (ring-equal-fn r)))
                                      (inner-loop (cdr bs))
                                      #f)
                                  (inner-loop (cdr bs))))))))))

;;; is-maximal-ideal? : Ideal → Boolean
;;; I is maximal if it's proper and no proper ideal contains it
;;; Equivalently: for every x ∉ I, the ideal <I ∪ {x}> = R
(define (is-maximal-ideal? ideal)
  (let ([r (ideal-ring ideal)]
        [i-elems (ideal-elements ideal)]
        [r-elems (ring-elements r)]
        [eq-fn (ring-equal-fn r)])
       ; Check if ideal is proper (doesn't contain 1)
       (and (not (member-equal (ring-one r) i-elems eq-fn))
            ; For each element not in ideal, check if adding it generates R
            (let loop ([candidates r-elems])
                 (if (null? candidates)
                     #t  ; All non-members generate R
                     (let ([x (car candidates)])
                          (if (member-equal x i-elems eq-fn)
                              (loop (cdr candidates))  ; Skip elements already in ideal
                              ; Check if <I ∪ {x}> contains 1
                              (let ([generated (apply append
                                                      (map (lambda (i)
                                                                   (map (lambda (s)
                                                                                (ring-add r i (ring-mul r s x)))
                                                                        r-elems))
                                                           i-elems))])
                                   (if (member-equal (ring-one r) generated eq-fn)
                                       (loop (cdr candidates))
                                       #f))))))))) ; Found proper ideal containing I

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; Helper: range
(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

;;; Helper: remove-duplicates
(define (remove-duplicates lst equal-fn)
  (let loop ([items lst] [seen '()])
       (if (null? items)
           seen
           (let ([item (car items)])
                (if (member-equal item seen equal-fn)
                    (loop (cdr items) seen)
                    (loop (cdr items) (cons item seen)))))))

;;; Helper: member-equal
(define (member-equal item lst equal-fn)
  (cond
   [(null? lst) #f]
   [(equal-fn item (car lst)) #t]
   [else (member-equal item (cdr lst) equal-fn)]))

;;; Helper: filter
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))
