;;; @module group
;;; @requires prelude

(require 'prelude)

(doc 'module 'group)
(doc 'description "Group theory library")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Pure, functional implementation of group structures:")
(doc 'note "- Group representation and operations")
(doc 'note "- Cyclic groups (Z_n)")
(doc 'note "- Permutation groups (S_n)")
(doc 'note "- Dihedral groups (D_n)")
(doc 'note "- Subgroup testing")
(doc 'note "- Group homomorphisms")

(doc 'section 'group-representation)

(doc 'note "A Group is represented as:")
(doc 'note "(group elements op identity inverse-fn equal-fn)")
(doc 'note "- elements: list of group elements")
(doc 'note "- op: binary operation (a, b) → a ∘ b")
(doc 'note "- identity: identity element e such that e ∘ a = a ∘ e = a")
(doc 'note "- inverse-fn: function a → a⁻¹ such that a ∘ a⁻¹ = e")
(doc 'note "- equal-fn: equality predicate for elements")
(define (make-group elements op identity inverse-fn equal-fn)
  (doc 'export #t)
  (doc 'type '(-> (List α) (-> α α α) α (-> α α) (-> α α Boolean) Group))
  (list 'group elements op identity inverse-fn equal-fn))
(define (group? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (list? x)
       (>= (length x) 6)
       (eq? (car x) 'group)))
(define (group-elements g) (list-ref g 1))
(doc 'export #t)
(doc group-elements 'type '(-> Group (List Element)))

(define (group-op g) (list-ref g 2))
(doc 'export #t)
(doc group-op 'type '(-> Group (-> Element Element Element)))

(define (group-identity g) (list-ref g 3))
(doc 'export #t)
(doc group-identity 'type '(-> Group Element))

(define (group-inverse-fn g) (list-ref g 4))
(doc 'export #t)
(doc group-inverse-fn 'type '(-> Group (-> Element Element)))

(define (group-equal-fn g) (list-ref g 5))
(doc 'export #t)
(doc group-equal-fn 'type '(-> Group (-> Element Element Boolean)))
(define (group-order g)
  (doc 'export #t)
  (doc 'type '(-> Group Integer))
  (doc 'description "Returns the number of elements in the group")
  (length (group-elements g)))

(doc 'section 'group-operations)
(define (group-compose g a b)
  (doc 'export #t)
  (doc 'type '(-> Group Element Element Element))
  (doc 'description "Apply the group operation: a ∘ b")
  ((group-op g) a b))
(define (group-inverse g a)
  (doc 'export #t)
  (doc 'type '(-> Group Element Element))
  (doc 'description "Compute the inverse: a⁻¹")
  ((group-inverse-fn g) a))
(define (group-power g a n)
  (doc 'export #t)
  (doc 'type '(-> Group Element Integer Element))
  (doc 'description "Compute a^n using repeated squaring")
  (doc 'description "Works for negative powers via inverse")
  (cond
   [(= n 0) (group-identity g)]
   [(< n 0) (group-power g (group-inverse g a) (- n))]
   [(= n 1) a]
   [(even? n)
    (let ([half (group-power g a (/ n 2))])
         (group-compose g half half))]
   [else
    (group-compose g a (group-power g a (- n 1)))]))
(define (group-equal? g a b)
  (doc 'export #t)
  (doc 'type '(-> Group Element Element Boolean))
  (doc 'description "Test equality of two elements")
  ((group-equal-fn g) a b))
(define (element-order g a)
  (doc 'export #t)
  (doc 'type '(-> Group Element (Union Integer Boolean)))
  (doc 'description "Find the smallest positive n such that a^n = e")
  (doc 'returns "#f if element has infinite order (shouldn't happen for finite groups)")
  (let loop ([n 1] [current a])
       (if (> n (group-order g))
           #f  ; Infinite order or not in group
           (if (group-equal? g current (group-identity g))
               n
               (loop (+ n 1) (group-compose g current a))))))

(doc 'section 'group-axiom-verification)
(define (verify-closure g)
  (doc 'export #t)
  (doc 'type '(-> Group Boolean))
  (doc 'description "Check that the operation is closed over the elements")
  (let ([elems (group-elements g)]
        [op (group-op g)]
        [eq? (group-equal-fn g)])
       (let loop-a ([as elems])
            (if (null? as)
                #t
                (let loop-b ([bs elems])
                     (if (null? bs)
                         (loop-a (cdr as))
                         (let ([result (op (car as) (car bs))])
                              (if (ormap (lambda (e) (eq? e result)) elems)
                                  (loop-b (cdr bs))
                                  #f))))))))
(define (verify-associativity g)
  (doc 'export #t)
  (doc 'type '(-> Group Boolean))
  (doc 'description "Check that (a ∘ b) ∘ c = a ∘ (b ∘ c) for all elements")
  (let ([elems (group-elements g)]
        [op (group-op g)]
        [eq? (group-equal-fn g)])
       (let loop-a ([as elems])
            (if (null? as)
                #t
                (let loop-b ([bs elems])
                     (if (null? bs)
                         (loop-a (cdr as))
                         (let loop-c ([cs elems])
                              (if (null? cs)
                                  (loop-b (cdr bs))
                                  (let* ([a (car as)]
                                         [b (car bs)]
                                         [c (car cs)]
                                         [left (op (op a b) c)]
                                         [right (op a (op b c))])
                                        (if (eq? left right)
                                            (loop-c (cdr cs))
                                            #f))))))))))
(define (verify-identity g)
  (doc 'export #t)
  (doc 'type '(-> Group Boolean))
  (doc 'description "Check that e ∘ a = a ∘ e = a for all elements")
  (let ([elems (group-elements g)]
        [op (group-op g)]
        [e (group-identity g)]
        [eq? (group-equal-fn g)])
       (let loop ([es elems])
            (if (null? es)
                #t
                (let ([a (car es)])
                     (if (and (eq? (op e a) a)
                              (eq? (op a e) a))
                         (loop (cdr es))
                         #f))))))
(define (verify-inverses g)
  (doc 'export #t)
  (doc 'type '(-> Group Boolean))
  (doc 'description "Check that a ∘ a⁻¹ = a⁻¹ ∘ a = e for all elements")
  (let ([elems (group-elements g)]
        [op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)]
        [eq? (group-equal-fn g)])
       (let loop ([es elems])
            (if (null? es)
                #t
                (let* ([a (car es)]
                       [a-inv (inv a)])
                      (if (and (eq? (op a a-inv) e)
                               (eq? (op a-inv a) e))
                          (loop (cdr es))
                          #f))))))
(define (verify-group-axioms g)
  (doc 'export #t)
  (doc 'type '(-> Group Boolean))
  (doc 'description "Verify all group axioms hold")
  (and (verify-closure g)
       (verify-associativity g)
       (verify-identity g)
       (verify-inverses g)))

(doc 'section 'cyclic-groups)
(define (make-cyclic-group n)
  (doc 'export #t)
  (doc 'type '(-> Integer Group))
  (doc 'description "Create the cyclic group Z_n (integers mod n under addition)")
  (let ([elements (iota n)]
        [op (lambda (a b) (modulo (+ a b) n))]
        [identity 0]
        [inverse-fn (lambda (a) (modulo (- n a) n))]
        [equal-fn =])
       (make-group elements op identity inverse-fn equal-fn)))

(doc Z 'export #t)
(define Z make-cyclic-group)
(doc Z 'type '(-> Integer Group))
(doc Z 'description "Alias for make-cyclic-group")
(define (cyclic-generator n)
  (doc 'export #t)
  (doc 'type '(-> Integer Integer))
  (doc 'description "Returns the standard generator of Z_n (which is 1)")
  1)

(doc 'section 'permutation-groups)

(doc 'note "A permutation is represented as a list mapping indices to values")
(doc 'note "E.g., (1 0 2) means: 0 → 1, 1 → 0, 2 → 2")
(define (permutation-apply perm i)
  (doc 'export #t)
  (doc 'type '(-> Permutation Integer Integer))
  (doc 'description "Apply permutation to an index")
  (list-ref perm i))
(define (permutation-compose sigma tau)
  (doc 'export #t)
  (doc 'type '(-> Permutation Permutation Permutation))
  (doc 'description "Compose two permutations: (σ ∘ τ)(i) = σ(τ(i))")
  (map (lambda (i) (permutation-apply sigma (permutation-apply tau i)))
       (iota (length sigma))))
(define (permutation-inverse perm)
  (doc 'export #t)
  (doc 'type '(-> Permutation Permutation))
  (doc 'description "Compute the inverse permutation")
  (let* ([n (length perm)]
         [result (make-list n 0)])
        (let loop ([i 0] [res result])
             (if (= i n)
                 res
                 (loop (+ i 1)
                       (list-set res (list-ref perm i) i))))))
(define (list-set lst idx val)
  (doc 'type '(-> List Integer Value List))
  (doc 'description "Functional update of list at index")
  (let loop ([i 0] [rest lst] [acc '()])
       (if (null? rest)
           (reverse acc)
           (loop (+ i 1)
                 (cdr rest)
                 (cons (if (= i idx) val (car rest)) acc)))))
(define (permutation-identity n)
  (doc 'export #t)
  (doc 'type '(-> Integer Permutation))
  (doc 'description "The identity permutation of length n")
  (iota n))
(define (permutation-equal? p1 p2)
  (doc 'export #t)
  (doc 'type '(-> Permutation Permutation Boolean))
  (doc 'description "Test if two permutations are equal")
  (equal? p1 p2))
(define (all-permutations n)
  (doc 'export #t)
  (doc 'type '(-> Integer (List Permutation)))
  (doc 'description "Generate all permutations of n elements")
  (if (<= n 0)
      '(())
      (let ([elems (iota n)])
           (permute-list elems))))
(define (permute-list lst)
  (doc 'type '(-> List (List List)))
  (doc 'description "Generate all permutations of a list")
  (if (null? lst)
      '(())
      (append-map (lambda (x)
                          (map (lambda (p) (cons x p))
                               (permute-list (remove x lst))))
                  lst)))
(define (remove x lst)
  (doc 'type '(-> Element List List))
  (doc 'description "Remove first occurrence of element from list")
  (cond
   [(null? lst) '()]
   [(equal? x (car lst)) (cdr lst)]
   [else (cons (car lst) (remove x (cdr lst)))]))
(define (make-symmetric-group n)
  (doc 'export #t)
  (doc 'type '(-> Integer Group))
  (doc 'description "Create the symmetric group S_n (all permutations of n elements)")
  (let ([elements (all-permutations n)]
        [op permutation-compose]
        [identity (permutation-identity n)]
        [inverse-fn permutation-inverse]
        [equal-fn permutation-equal?])
       (make-group elements op identity inverse-fn equal-fn)))

(doc S 'export #t)
(define S make-symmetric-group)
(doc S 'type '(-> Integer Group))
(doc S 'description "Alias for make-symmetric-group")
(define (cycle-to-permutation n cycle)
  (doc 'export #t)
  (doc 'type '(-> Integer (List Integer) Permutation))
  (doc 'description "Convert a cycle notation to a permutation")
  (doc 'description "E.g., (cycle-to-permutation 3 '(0 1 2)) for the 3-cycle (0 1 2)")
  (let ([result (iota n)])
       (if (null? cycle)
           result
           (let loop ([rest cycle] [res result])
                (if (null? (cdr rest))
                    (list-set res (car rest) (car cycle))
                    (loop (cdr rest)
                          (list-set res (car rest) (cadr rest))))))))
(define (transposition n i j)
  (doc 'export #t)
  (doc 'type '(-> Integer Integer Integer Permutation))
  (doc 'description "Create a transposition swapping i and j in n elements")
  (cycle-to-permutation n (list i j)))
(define (permutation-parity perm)
  (doc 'export #t)
  (doc 'type '(-> Permutation Integer))
  (doc 'description "Returns 0 for even permutation, 1 for odd")
  (let* ([n (length perm)]
         [inversions
          (let loop-i ([i 0] [count 0])
               (if (= i n)
                   count
                   (loop-i (+ i 1)
                           (let loop-j ([j (+ i 1)] [c count])
                                (if (= j n)
                                    c
                                    (loop-j (+ j 1)
                                            (if (> (list-ref perm i)
                                                   (list-ref perm j))
                                                (+ c 1)
                                                c)))))))])
        (modulo inversions 2)))

(doc 'section 'dihedral-groups)

(doc 'note "Dihedral group D_n: symmetries of a regular n-gon")
(doc 'note "Elements: rotations r^k (k = 0..n-1) and reflections s*r^k")
(doc 'note "Represented as (type . k) where type is 'r or 's")
(doc 'note "Using relation: sr = r^(-1)s (reflection inverts rotation)")
(doc 'note "r^a * r^b = r^(a+b mod n)")
(doc 'note "r^a * sr^b = sr^(b-a mod n)")
(doc 'note "sr^a * r^b = sr^(a+b mod n)")
(doc 'note "sr^a * sr^b = r^(b-a mod n)")
(define (dihedral-op n)
  (doc 'type '(-> Integer (-> Element Element Element)))
  (doc 'description "Group operation for D_n")
  (lambda (a b)
          (let ([a-type (car a)] [a-k (cdr a)]
                [b-type (car b)] [b-k (cdr b)])
               (cond
                [(and (eq? a-type 'r) (eq? b-type 'r))
                 (cons 'r (modulo (+ a-k b-k) n))]
                [(and (eq? a-type 'r) (eq? b-type 's))
                 (cons 's (modulo (- b-k a-k) n))]
                [(and (eq? a-type 's) (eq? b-type 'r))
                 (cons 's (modulo (+ a-k b-k) n))]
                [(and (eq? a-type 's) (eq? b-type 's))
                 (cons 'r (modulo (- b-k a-k) n))]))))
(define (dihedral-inverse n)
  (doc 'type '(-> Integer (-> Element Element)))
  (doc 'description "Inverse in D_n")
  (doc 'note "(r^k)⁻¹ = r^(-k mod n)")
  (doc 'note "(sr^k)⁻¹ = sr^k (reflections are self-inverse)")
  (lambda (a)
          (let ([a-type (car a)] [a-k (cdr a)])
               (if (eq? a-type 'r)
                   (cons 'r (modulo (- n a-k) n))
                   a))))
(define (make-dihedral-group n)
  (doc 'export #t)
  (doc 'type '(-> Integer Group))
  (doc 'description "Create the dihedral group D_n")
  (let* ([rotations (map (lambda (k) (cons 'r k)) (iota n))]
         [reflections (map (lambda (k) (cons 's k)) (iota n))]
         [elements (append rotations reflections)]
         [op (dihedral-op n)]
         [identity (cons 'r 0)]
         [inverse-fn (dihedral-inverse n)]
         [equal-fn equal?])
        (make-group elements op identity inverse-fn equal-fn)))

(doc D 'export #t)
(define D make-dihedral-group)
(doc D 'type '(-> Integer Group))
(doc D 'description "Alias for make-dihedral-group")

(doc 'section 'subgroup-testing)
(define (is-subgroup? g h-elements)
  (doc 'export #t)
  (doc 'type '(-> Group (List Element) Boolean))
  (doc 'description "Test if a subset H forms a subgroup of G")
  (let ([op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)]
        [eq? (group-equal-fn g)])
       (and
        ;; H is non-empty
        (not (null? h-elements))
        ;; Identity in H
        (ormap (lambda (x) (eq? x e)) h-elements)
        ;; Closed under operation
        (let loop-a ([as h-elements])
             (if (null? as)
                 #t
                 (let loop-b ([bs h-elements])
                      (if (null? bs)
                          (loop-a (cdr as))
                          (let ([result (op (car as) (car bs))])
                               (if (ormap (lambda (x) (eq? x result)) h-elements)
                                   (loop-b (cdr bs))
                                   #f))))))
        ;; Closed under inverses
        (let loop ([es h-elements])
             (if (null? es)
                 #t
                 (let ([a-inv (inv (car es))])
                      (if (ormap (lambda (x) (eq? x a-inv)) h-elements)
                          (loop (cdr es))
                          #f)))))))
(define (generate-subgroup g generators)
  (doc 'export #t)
  (doc 'type '(-> Group (List Element) (List Element)))
  (doc 'description "Generate the subgroup of G generated by the given elements")
  (let ([op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)]
        [eq? (group-equal-fn g)])
       (let loop ([current (cons e generators)])
            (let* ([new-elements
                    (append-map (lambda (a)
                                        (append
                                         (map (lambda (b) (op a b)) current)
                                         (list (inv a))))
                                current)]
                   [unique
                    (fold-left
                     (lambda (acc x)
                             (if (ormap (lambda (y) (eq? x y)) acc)
                                 acc
                                 (cons x acc)))
                     current
                     new-elements)])
                  (if (= (length unique) (length current))
                      current
                      (loop unique))))))

(doc 'section 'group-homomorphisms)

(doc 'note "A homomorphism is represented as:")
(doc 'note "(homomorphism source-group target-group mapping)")
(doc 'note "where mapping is a function Element → Element")
(define (make-homomorphism source target mapping)
  (doc 'export #t)
  (doc 'type '(-> Group Group (-> Element Element) Homomorphism))
  (list 'homomorphism source target mapping))
(define (homomorphism? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (list? x)
       (= (length x) 4)
       (eq? (car x) 'homomorphism)))

(define (homomorphism-source h) (list-ref h 1))
(doc 'export #t)
(doc homomorphism-source 'type '(-> Homomorphism Group))

(define (homomorphism-target h) (list-ref h 2))
(doc 'export #t)
(doc homomorphism-target 'type '(-> Homomorphism Group))

(define (homomorphism-mapping h) (list-ref h 3))
(doc 'export #t)
(doc homomorphism-mapping 'type '(-> Homomorphism (-> Element Element)))
(define (homomorphism-apply h a)
  (doc 'export #t)
  (doc 'type '(-> Homomorphism Element Element))
  (doc 'description "Apply the homomorphism to an element")
  ((homomorphism-mapping h) a))
(define (verify-homomorphism h)
  (doc 'export #t)
  (doc 'type '(-> Homomorphism Boolean))
  (doc 'description "Check that φ(a ∘ b) = φ(a) ∘ φ(b) for all a, b in source")
  (let* ([source (homomorphism-source h)]
         [target (homomorphism-target h)]
         [phi (homomorphism-mapping h)]
         [elems (group-elements source)]
         [op-s (group-op source)]
         [op-t (group-op target)]
         [eq? (group-equal-fn target)])
        (let loop-a ([as elems])
             (if (null? as)
                 #t
                 (let loop-b ([bs elems])
                      (if (null? bs)
                          (loop-a (cdr as))
                          (let* ([a (car as)]
                                 [b (car bs)]
                                 [left (phi (op-s a b))]
                                 [right (op-t (phi a) (phi b))])
                                (if (eq? left right)
                                    (loop-b (cdr bs))
                                    #f))))))))
(define (is-isomorphism? h)
  (doc 'export #t)
  (doc 'type '(-> Homomorphism Boolean))
  (doc 'description "Check if a homomorphism is an isomorphism (bijective)")
  (and (verify-homomorphism h)
       (let* ([source (homomorphism-source h)]
              [target (homomorphism-target h)]
              [phi (homomorphism-mapping h)]
              [source-elems (group-elements source)]
              [target-elems (group-elements target)]
              [eq? (group-equal-fn target)]
              [images (map phi source-elems)])
             (and
              ;; Same order (necessary for bijection)
              (= (length source-elems) (length target-elems))
              ;; All images are distinct (injective)
              (let loop ([imgs images])
                   (if (null? imgs)
                       #t
                       (if (ormap (lambda (x) (eq? x (car imgs))) (cdr imgs))
                           #f
                           (loop (cdr imgs)))))
              ;; All target elements hit (surjective)
              (let loop ([targets target-elems])
                   (if (null? targets)
                       #t
                       (if (ormap (lambda (x) (eq? x (car targets))) images)
                           (loop (cdr targets))
                           #f)))))))
(define (kernel h)
  (doc 'export #t)
  (doc 'type '(-> Homomorphism (List Element)))
  (doc 'description "Compute the kernel: elements that map to the identity")
  (let* ([source (homomorphism-source h)]
         [target (homomorphism-target h)]
         [phi (homomorphism-mapping h)]
         [e-t (group-identity target)]
         [eq? (group-equal-fn target)])
        (filter (lambda (a) (eq? (phi a) e-t))
                (group-elements source))))
(define (image h)
  (doc 'export #t)
  (doc 'type '(-> Homomorphism (List Element)))
  (doc 'description "Compute the image: elements in target that are hit")
  (let* ([source (homomorphism-source h)]
         [target (homomorphism-target h)]
         [phi (homomorphism-mapping h)]
         [eq? (group-equal-fn target)])
        (fold-left
         (lambda (acc x)
                 (let ([img (phi x)])
                      (if (ormap (lambda (y) (eq? y img)) acc)
                          acc
                          (cons img acc))))
         '()
         (group-elements source))))

(doc 'section 'cayley-table)
(define (cayley-table g)
  (doc 'export #t)
  (doc 'type '(-> Group (List (List Element))))
  (doc 'description "Generate the Cayley table (multiplication table) for a group")
  (let ([elems (group-elements g)]
        [op (group-op g)])
       (map (lambda (a)
                    (map (lambda (b) (op a b)) elems))
            elems)))

;; filter, fold-left, iota, ormap provided by prelude

(doc 'section 'common-groups)
(define (trivial-group)
  (doc 'type '(-> Group))
  (doc 'description "The trivial group with one element")
  (make-group '(e)
              (lambda (a b) 'e)
              'e
              (lambda (a) 'e)
              eq?))
(define (klein-four-group)
  (doc 'type '(-> Group))
  (doc 'description "The Klein four-group V₄ = Z₂ × Z₂")
  (let ([elements '(e a b c)]
        [table '((e e a b c)
                 (a a e c b)
                 (b b c e a)
                 (c c b a e))])
       (make-group
        elements
        (lambda (x y)
                (let ([i (index-of x elements)]
                      [j (index-of y elements)])
                     (list-ref (list-ref table (+ i 1)) (+ j 1))))
        'e
        (lambda (x) x)  ; All elements are self-inverse
        eq?)))
(define (index-of x lst)
  (doc 'type '(-> Element List Integer))
  (let loop ([i 0] [rest lst])
       (cond
        [(null? rest) -1]
        [(equal? x (car rest)) i]
        [else (loop (+ i 1) (cdr rest))])))


(define (quaternion-group)
  (doc 'type '(-> Group))
  (doc 'description "The quaternion group Q₈")
  (doc 'note "Uses qi, qj, qk to avoid collision with Scheme's complex number i")
  (let ([elements '(e1 e-1 qi q-i qj q-j qk q-k)])
       (make-group
        elements
        (lambda (a b)
                (case a
                      [(e1) b]
                      [(e-1) (case b
                                   [(e1) 'e-1] [(e-1) 'e1]
                                   [(qi) 'q-i] [(q-i) 'qi]
                                   [(qj) 'q-j] [(q-j) 'qj]
                                   [(qk) 'q-k] [(q-k) 'qk])]
                      [(qi) (case b
                                  [(e1) 'qi] [(e-1) 'q-i] [(qi) 'e-1] [(q-i) 'e1]
                                  [(qj) 'qk] [(q-j) 'q-k] [(qk) 'q-j] [(q-k) 'qj])]
                      [(q-i) (case b
                                   [(e1) 'q-i] [(e-1) 'qi] [(qi) 'e1] [(q-i) 'e-1]
                                   [(qj) 'q-k] [(q-j) 'qk] [(qk) 'qj] [(q-k) 'q-j])]
                      [(qj) (case b
                                  [(e1) 'qj] [(e-1) 'q-j] [(qj) 'e-1] [(q-j) 'e1]
                                  [(qi) 'q-k] [(q-i) 'qk] [(qk) 'qi] [(q-k) 'q-i])]
                      [(q-j) (case b
                                   [(e1) 'q-j] [(e-1) 'qj] [(qj) 'e1] [(q-j) 'e-1]
                                   [(qi) 'qk] [(q-i) 'q-k] [(qk) 'q-i] [(q-k) 'qi])]
                      [(qk) (case b
                                  [(e1) 'qk] [(e-1) 'q-k] [(qk) 'e-1] [(q-k) 'e1]
                                  [(qi) 'qj] [(q-i) 'q-j] [(qj) 'q-i] [(q-j) 'qi])]
                      [(q-k) (case b
                                   [(e1) 'q-k] [(e-1) 'qk] [(qk) 'e1] [(q-k) 'e-1]
                                   [(qi) 'q-j] [(q-i) 'qj] [(qj) 'qi] [(q-j) 'q-i])]))
        'e1
        (lambda (a)
                (case a
                      [(e1) 'e1] [(e-1) 'e-1]
                      [(qi) 'q-i] [(q-i) 'qi]
                      [(qj) 'q-j] [(q-j) 'qj]
                      [(qk) 'q-k] [(q-k) 'qk]))
        eq?)))
