(load "lattice/fp/clp/store.ss")

(doc 'module 'fd-constraints)
(doc 'description "Finite Domain Arithmetic Constraints")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Implements arithmetic constraints over finite domains with arc consistency propagation")
(doc 'exports "in-range, in-domain, =fd, <fd, <=fd, >fd, >=fd, =/=fd, +fd, -fd, *fd, abs-fd")
(doc 'dependencies "store.ss, domain.ss")

(doc 'section 'constraint-pattern)
(doc 'note "Each constraint follows this pattern:")
(doc 'note "1. Goal constructor: creates constraint and adds to store")
(doc 'note "2. Propagator: enforces constraint by narrowing domains")
(doc 'note "3. Initial propagation: run propagator when constraint posted")

(doc 'section 'domain-declaration-constraints)

(define (in-range var lo hi)
  (doc 'type '(-> LVar Int Int (-> CStore (Maybe CStore))))
  (doc 'description "Constrain variable to be in range [lo, hi]")
  (lambda (cs)
          (cstore-narrow-domain cs var (make-domain lo hi))))

(define (in-domain var values)
  (doc 'type '(-> LVar (List Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain variable to be one of the given values")
  (lambda (cs)
          (cstore-narrow-domain cs var (domain-from-list values))))

(doc 'section 'equality-constraints)

(define (=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain two values to be equal (in the FD sense)")
  (lambda (cs)
          (cond
           ;; Both are integers - just check equality
           [(and (integer? x) (integer? y))
            (if (= x y) cs #f)]
           
           ;; x is integer, y is variable
           [(integer? x)
            (=fd-var-const cs y x)]
           
           ;; y is integer, x is variable
           [(integer? y)
            (=fd-var-const cs x y)]
           
           ;; Both are variables
           [else
            (=fd-var-var cs x y)])))

(define (=fd-var-const cs var val)
  (doc 'type '(-> CStore LVar Int (Maybe CStore)))
  (doc 'description "Constrain variable to equal a constant")
  (let ([dom (cstore-get-domain cs var)])
       (cond
        ;; No domain - just set to singleton
        [(not dom)
         (cstore-set-domain cs var (domain-singleton val))]
        ;; Has domain - check containment and narrow
        [(domain-contains? dom val)
         (cstore-set-domain cs var (domain-singleton val))]
        ;; Value not in domain - fail
        [else #f])))

(define (=fd-var-var cs x y)
  (doc 'type '(-> CStore LVar LVar (Maybe CStore)))
  (doc 'description "Constrain two variables to be equal")
  (let ([dom-x (cstore-get-domain cs x)]
        [dom-y (cstore-get-domain cs y)])
       (cond
        ;; Both have domains - intersect
        [(and dom-x dom-y)
         (let ([merged (domain-intersect dom-x dom-y)])
              (if (domain-empty? merged)
                  #f
                  (let* ([cs1 (cstore-set-domain cs x merged)])
                        (and cs1 (cstore-set-domain cs1 y merged)))))]
        ;; Only x has domain - propagate to y
        [dom-x
         (cstore-set-domain cs y dom-x)]
        ;; Only y has domain - propagate to x
        [dom-y
         (cstore-set-domain cs x dom-y)]
        ;; Neither has domain - no propagation needed yet
        [else cs])))

(doc 'section 'comparison-constraints)

(define (<fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x < y")
  (lambda (cs)
          (cond
           ;; Both constants
           [(and (integer? x) (integer? y))
            (if (< x y) cs #f)]
           
           ;; x is constant, y is variable
           [(integer? x)
            (let ([dom-y (cstore-get-domain cs y)])
                 (if dom-y
                     ;; y must be > x, so y >= x+1
                     (cstore-narrow-domain cs y (domain-restrict-min dom-y (+ x 1)))
                     cs))]
           
           ;; y is constant, x is variable
           [(integer? y)
            (let ([dom-x (cstore-get-domain cs x)])
                 (if dom-x
                     ;; x must be < y, so x <= y-1
                     (cstore-narrow-domain cs x (domain-restrict-max dom-x (- y 1)))
                     cs))]
           
           ;; Both variables
           [else
            (<fd-var-var cs x y)])))

(define (<fd-var-var cs x y)
  (doc 'type '(-> CStore LVar LVar (Maybe CStore)))
  (doc 'description "Propagate x < y constraint for two variables")
  (let ([dom-x (cstore-get-domain cs x)]
        [dom-y (cstore-get-domain cs y)])
       (cond
        [(and dom-x dom-y)
         ;; x < y means:
         ;; - max(x) < max(y), so x <= max(y) - 1
         ;; - min(x) < min(y), so y >= min(x) + 1
         (let* ([cs1 (cstore-narrow-domain cs x
                                           (domain-restrict-max dom-x (- (domain-max dom-y) 1)))]
                [cs2 (and cs1
                          (cstore-narrow-domain cs1 y
                                                (domain-restrict-min (cstore-get-domain cs1 y)
                                                                     (+ (domain-min (cstore-get-domain cs1 x)) 1))))])
               cs2)]
        [else cs])))

(define (<=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x <= y")
  (lambda (cs)
          (cond
           ;; Both constants
           [(and (integer? x) (integer? y))
            (if (<= x y) cs #f)]
           
           ;; x is constant, y is variable
           [(integer? x)
            (let ([dom-y (cstore-get-domain cs y)])
                 (if dom-y
                     (cstore-narrow-domain cs y (domain-restrict-min dom-y x))
                     cs))]
           
           ;; y is constant, x is variable
           [(integer? y)
            (let ([dom-x (cstore-get-domain cs x)])
                 (if dom-x
                     (cstore-narrow-domain cs x (domain-restrict-max dom-x y))
                     cs))]
           
           ;; Both variables
           [else
            (<=fd-var-var cs x y)])))

(define (<=fd-var-var cs x y)
  (doc 'type '(-> CStore LVar LVar (Maybe CStore)))
  (doc 'description "Propagate x <= y constraint for two variables")
  (let ([dom-x (cstore-get-domain cs x)]
        [dom-y (cstore-get-domain cs y)])
       (cond
        [(and dom-x dom-y)
         ;; x <= y means:
         ;; - max(x) <= max(y)
         ;; - min(y) >= min(x)
         (let* ([cs1 (cstore-narrow-domain cs x
                                           (domain-restrict-max dom-x (domain-max dom-y)))]
                [cs2 (and cs1
                          (cstore-narrow-domain cs1 y
                                                (domain-restrict-min (cstore-get-domain cs1 y)
                                                                     (domain-min (cstore-get-domain cs1 x)))))])
               cs2)]
        [else cs])))

(define (>fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x > y")
  (<fd y x))

(define (>=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x >= y")
  (<=fd y x))

(define (=/=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x ≠ y (disequality)")
  (lambda (cs)
          (cond
           ;; Both constants
           [(and (integer? x) (integer? y))
            (if (not (= x y)) cs #f)]
           
           ;; x is constant, y is variable
           [(integer? x)
            (=/=fd-var-const cs y x)]
           
           ;; y is constant, x is variable
           [(integer? y)
            (=/=fd-var-const cs x y)]
           
           ;; Both variables - can only propagate if one is singleton
           [else
            (=/=fd-var-var cs x y)])))

(define (=/=fd-var-const cs var val)
  (doc 'type '(-> CStore LVar Int (Maybe CStore)))
  (doc 'description "Constrain variable to not equal constant")
  (let ([dom (cstore-get-domain cs var)])
       (if dom
           (cstore-set-domain cs var (domain-subtract-value dom val))
           cs)))  ; No domain = unconstrained, can't propagate yet

(define (=/=fd-var-var cs x y)
  (doc 'type '(-> CStore LVar LVar (Maybe CStore)))
  (doc 'description "Constrain two variables to be unequal")
  (let ([dom-x (cstore-get-domain cs x)]
        [dom-y (cstore-get-domain cs y)])
       (cond
        ;; If x is singleton, remove its value from y
        [(and dom-x (domain-singleton? dom-x) dom-y)
         (cstore-remove-value cs y (domain-min dom-x))]
        ;; If y is singleton, remove its value from x
        [(and dom-y (domain-singleton? dom-y) dom-x)
         (cstore-remove-value cs x (domain-min dom-y))]
        ;; Can't propagate yet
        [else cs])))

(doc 'section 'arithmetic-constraints)

(define (+fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x + y = z using bounds consistency")
  (lambda (cs)
          ;; Handle various combinations of constants and variables
          (let ([x-val (fd-value cs x)]
                [y-val (fd-value cs y)]
                [z-val (fd-value cs z)])
               (cond
                ;; All constants - just check
                [(and x-val y-val z-val)
                 (if (= (+ x-val y-val) z-val) cs #f)]
                
                ;; x and y known, constrain z
                [(and x-val y-val)
                 ((=fd z (+ x-val y-val)) cs)]
                
                ;; x and z known, constrain y = z - x
                [(and x-val z-val)
                 ((=fd y (- z-val x-val)) cs)]
                
                ;; y and z known, constrain x = z - y
                [(and y-val z-val)
                 ((=fd x (- z-val y-val)) cs)]
                
                ;; One or zero known - use bounds propagation
                [else
                 (+fd-bounds cs x y z)]))))

(define (fd-value cs term)
  (doc 'type '(-> CStore (Or LVar Int) (Maybe Int)))
  (doc 'description "Get the value of a term if it's ground or singleton")
  (cond
   [(integer? term) term]
   [(lvar? term)
    (let ([dom (cstore-get-domain cs term)])
         (if (and dom (domain-singleton? dom))
             (domain-min dom)
             #f))]
   [else #f]))

(define (+fd-bounds cs x y z)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Bounds propagation for x + y = z")
  (let* ([x-bounds (fd-bounds cs x)]
         [y-bounds (fd-bounds cs y)]
         [z-bounds (fd-bounds cs z)])
        (if (and x-bounds y-bounds z-bounds)
            (let* ([x-lo (car x-bounds)] [x-hi (cdr x-bounds)]
                   [y-lo (car y-bounds)] [y-hi (cdr y-bounds)]
                   [z-lo (car z-bounds)] [z-hi (cdr z-bounds)]
                   ;; z = x + y, so:
                   ;; z >= x-lo + y-lo, z <= x-hi + y-hi
                   ;; x >= z-lo - y-hi, x <= z-hi - y-lo
                   ;; y >= z-lo - x-hi, y <= z-hi - x-lo
                   [new-z-lo (max z-lo (+ x-lo y-lo))]
                   [new-z-hi (min z-hi (+ x-hi y-hi))]
                   [new-x-lo (max x-lo (- z-lo y-hi))]
                   [new-x-hi (min x-hi (- z-hi y-lo))]
                   [new-y-lo (max y-lo (- z-lo x-hi))]
                   [new-y-hi (min y-hi (- z-hi x-lo))])
                  ;; Apply narrowed bounds
                  (let* ([cs1 (fd-narrow-bounds cs z new-z-lo new-z-hi)]
                         [cs2 (and cs1 (fd-narrow-bounds cs1 x new-x-lo new-x-hi))]
                         [cs3 (and cs2 (fd-narrow-bounds cs2 y new-y-lo new-y-hi))])
                        cs3))
            cs)))

(define (fd-bounds cs term)
  (doc 'type '(-> CStore (Or LVar Int) (Maybe (Pair Int Int))))
  (doc 'description "Get bounds of a term")
  (cond
   [(integer? term) (cons term term)]
   [(lvar? term)
    (let ([dom (cstore-get-domain cs term)])
         (if dom
             (domain-bounds dom)
             #f))]
   [else #f]))

(define (fd-narrow-bounds cs term lo hi)
  (doc 'type '(-> CStore (Or LVar Int) Int Int (Maybe CStore)))
  (doc 'description "Narrow a term's bounds")
  (cond
   [(integer? term)
    (if (and (>= term lo) (<= term hi)) cs #f)]
   [(lvar? term)
    (let ([dom (cstore-get-domain cs term)])
         (if dom
             (cstore-set-domain cs term
                                (domain-intersect dom (make-domain lo hi)))
             cs))]
   [else cs]))

(define (-fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x - y = z, which is x = z + y")
  (+fd z y x))  ; z + y = x

(define (*fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain x * y = z using bounds consistency")
  (lambda (cs)
          (let ([x-val (fd-value cs x)]
                [y-val (fd-value cs y)]
                [z-val (fd-value cs z)])
               (cond
                ;; All constants
                [(and x-val y-val z-val)
                 (if (= (* x-val y-val) z-val) cs #f)]
                
                ;; x and y known
                [(and x-val y-val)
                 ((=fd z (* x-val y-val)) cs)]
                
                ;; x and z known, y = z / x (if x ≠ 0 and z divisible by x)
                [(and x-val z-val)
                 (if (and (not (= x-val 0)) (= 0 (modulo z-val x-val)))
                     ((=fd y (quotient z-val x-val)) cs)
                     (if (= x-val 0)
                         (if (= z-val 0) cs #f)  ; 0 * y = 0
                         #f))]
                
                ;; y and z known, x = z / y
                [(and y-val z-val)
                 (if (and (not (= y-val 0)) (= 0 (modulo z-val y-val)))
                     ((=fd x (quotient z-val y-val)) cs)
                     (if (= y-val 0)
                         (if (= z-val 0) cs #f)
                         #f))]
                
                ;; Bounds propagation for multiplication
                [else
                 (*fd-bounds cs x y z)]))))

(define (*fd-bounds cs x y z)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Bounds propagation for x * y = z")
  (doc 'note "More complex due to sign considerations")
  (let* ([x-bounds (fd-bounds cs x)]
         [y-bounds (fd-bounds cs y)]
         [z-bounds (fd-bounds cs z)])
        (if (and x-bounds y-bounds z-bounds)
            (let* ([x-lo (car x-bounds)] [x-hi (cdr x-bounds)]
                   [y-lo (car y-bounds)] [y-hi (cdr y-bounds)]
                   [z-lo (car z-bounds)] [z-hi (cdr z-bounds)]
                   ;; Compute all products to find bounds
                   [products (list (* x-lo y-lo) (* x-lo y-hi)
                                   (* x-hi y-lo) (* x-hi y-hi))]
                   [new-z-lo (max z-lo (apply min products))]
                   [new-z-hi (min z-hi (apply max products))])
                  ;; Just narrow z for now (full propagation is complex)
                  (fd-narrow-bounds cs z new-z-lo new-z-hi))
            cs)))

(doc 'section 'absolute-value)

(define (abs-fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain |x| = y")
  (lambda (cs)
          (let ([x-val (fd-value cs x)]
                [y-val (fd-value cs y)])
               (cond
                ;; Both known
                [(and x-val y-val)
                 (if (= (abs x-val) y-val) cs #f)]
                
                ;; x known
                [x-val
                 ((=fd y (abs x-val)) cs)]
                
                ;; y known - x can be y or -y
                [y-val
                 (cstore-narrow-domain cs x
                                       (domain-union (domain-singleton y-val)
                                                     (domain-singleton (- y-val))))]
                
                ;; Both unknown - y >= 0 and propagate bounds
                [else
                 (let* ([cs1 ((>=fd y 0) cs)]
                        [x-bounds (and cs1 (fd-bounds cs1 x))]
                        [y-bounds (and cs1 (fd-bounds cs1 y))])
                       (if (and x-bounds y-bounds)
                           (let* ([x-lo (car x-bounds)] [x-hi (cdr x-bounds)]
                                  [y-hi (cdr y-bounds)]
                                  ;; |x| = y, so x in [-y-hi, y-hi]
                                  [cs2 (fd-narrow-bounds cs1 x (- y-hi) y-hi)])
                                 cs2)
                           cs1))]))))
