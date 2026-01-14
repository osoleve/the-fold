;;; core/algebra/group.ss — Group Theory Library
;;;
;;; Pure, functional implementation of group structures:
;;; - Group representation and operations
;;; - Cyclic groups (Z_n)
;;; - Permutation groups (S_n)
;;; - Dihedral groups (D_n)
;;; - Subgroup testing
;;; - Group homomorphisms
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Group Representation
;;; ====

;;; A Group is represented as:
;;; (group elements op identity inverse-fn equal-fn)
;;; - elements: list of group elements
;;; - op: binary operation (a, b) → a ∘ b
;;; - identity: identity element e such that e ∘ a = a ∘ e = a
;;; - inverse-fn: function a → a⁻¹ such that a ∘ a⁻¹ = e
;;; - equal-fn: equality predicate for elements

;;; make-group : (List α) × (α × α → α) × α × (α → α) × (α × α → Boolean) → Group
(define (make-group elements op identity inverse-fn equal-fn)
  (list 'group elements op identity inverse-fn equal-fn))

;;; group? : α → Boolean
(define (group? x)
  (and (list? x)
       (>= (length x) 6)
       (eq? (car x) 'group)))

;;; group-elements : Group → (List Element)
(define (group-elements g) (list-ref g 1))
;;; group-op : Group → (Element × Element → Element)
(define (group-op g) (list-ref g 2))
;;; group-identity : Group → Element
(define (group-identity g) (list-ref g 3))
;;; group-inverse-fn : Group → (Element → Element)
(define (group-inverse-fn g) (list-ref g 4))
;;; group-equal-fn : Group → (Element × Element → Boolean)
(define (group-equal-fn g) (list-ref g 5))

;;; group-order : Group → Integer
;;; Returns the number of elements in the group.
(define (group-order g)
  (length (group-elements g)))

;;; ====
;;; Group Operations
;;; ====

;;; group-compose : Group × Element × Element → Element
;;; Apply the group operation: a ∘ b
(define (group-compose g a b)
  ((group-op g) a b))

;;; group-inverse : Group × Element → Element
;;; Compute the inverse: a⁻¹
(define (group-inverse g a)
  ((group-inverse-fn g) a))

;;; group-power : Group × Element × Integer → Element
;;; Compute a^n using repeated squaring.
;;; Works for negative powers via inverse.
(define (group-power g a n)
  (cond
   [(= n 0) (group-identity g)]
   [(< n 0) (group-power g (group-inverse g a) (- n))]
   [(= n 1) a]
   [(even? n)
    (let ([half (group-power g a (/ n 2))])
         (group-compose g half half))]
   [else
    (group-compose g a (group-power g a (- n 1)))]))

;;; group-equal? : Group × Element × Element → Boolean
;;; Test equality of two elements.
(define (group-equal? g a b)
  ((group-equal-fn g) a b))

;;; element-order : Group × Element → Integer
;;; Find the smallest positive n such that a^n = e.
;;; Returns #f if element has infinite order (shouldn't happen for finite groups).
(define (element-order g a)
  (let loop ([n 1] [current a])
       (if (> n (group-order g))
           #f  ; Infinite order or not in group
           (if (group-equal? g current (group-identity g))
               n
               (loop (+ n 1) (group-compose g current a))))))

;;; ====
;;; Group Axiom Verification
;;; ====

;;; verify-closure : Group → Boolean
;;; Check that the operation is closed over the elements.
(define (verify-closure g)
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
                              (if (any (lambda (e) (eq? e result)) elems)
                                  (loop-b (cdr bs))
                                  #f))))))))

;;; verify-associativity : Group → Boolean
;;; Check that (a ∘ b) ∘ c = a ∘ (b ∘ c) for all elements.
(define (verify-associativity g)
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

;;; verify-identity : Group → Boolean
;;; Check that e ∘ a = a ∘ e = a for all elements.
(define (verify-identity g)
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

;;; verify-inverses : Group → Boolean
;;; Check that a ∘ a⁻¹ = a⁻¹ ∘ a = e for all elements.
(define (verify-inverses g)
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

;;; verify-group-axioms : Group → Boolean
;;; Verify all group axioms hold.
(define (verify-group-axioms g)
  (and (verify-closure g)
       (verify-associativity g)
       (verify-identity g)
       (verify-inverses g)))

;;; ====
;;; Cyclic Groups Z_n
;;; ====

;;; make-cyclic-group : Integer → Group
;;; Create the cyclic group Z_n (integers mod n under addition).
(define (make-cyclic-group n)
  (let ([elements (iota n)]
        [op (lambda (a b) (modulo (+ a b) n))]
        [identity 0]
        [inverse-fn (lambda (a) (modulo (- n a) n))]
        [equal-fn =])
       (make-group elements op identity inverse-fn equal-fn)))

;;; Z_n : Integer → Group
;;; Alias for make-cyclic-group.
(define Z make-cyclic-group)

;;; cyclic-generator : Integer → Integer
;;; Returns the standard generator of Z_n (which is 1).
(define (cyclic-generator n) 1)

;;; ====
;;; Permutation Groups S_n
;;; ====

;;; A permutation is represented as a list mapping indices to values.
;;; E.g., (1 0 2) means: 0 → 1, 1 → 0, 2 → 2

;;; permutation-apply : Permutation × Integer → Integer
;;; Apply permutation to an index.
(define (permutation-apply perm i)
  (list-ref perm i))

;;; permutation-compose : Permutation × Permutation → Permutation
;;; Compose two permutations: (σ ∘ τ)(i) = σ(τ(i))
(define (permutation-compose sigma tau)
  (map (lambda (i) (permutation-apply sigma (permutation-apply tau i)))
       (iota (length sigma))))

;;; permutation-inverse : Permutation → Permutation
;;; Compute the inverse permutation.
(define (permutation-inverse perm)
  (let* ([n (length perm)]
         [result (make-list n 0)])
        (let loop ([i 0] [res result])
             (if (= i n)
                 res
                 (loop (+ i 1)
                       (list-set res (list-ref perm i) i))))))

;;; list-set : List × Integer × Value → List
;;; Functional update of list at index.
(define (list-set lst idx val)
  (let loop ([i 0] [rest lst] [acc '()])
       (if (null? rest)
           (reverse acc)
           (loop (+ i 1)
                 (cdr rest)
                 (cons (if (= i idx) val (car rest)) acc)))))

;;; permutation-identity : Integer → Permutation
;;; The identity permutation of length n.
(define (permutation-identity n)
  (iota n))

;;; permutation-equal? : Permutation × Permutation → Boolean
;;; Test if two permutations are equal.
(define (permutation-equal? p1 p2)
  (equal? p1 p2))

;;; all-permutations : Integer → List<Permutation>
;;; Generate all permutations of n elements.
(define (all-permutations n)
  (if (<= n 0)
      '(())
      (let ([elems (iota n)])
           (permute-list elems))))

;;; permute-list : List → List<List>
;;; Generate all permutations of a list.
(define (permute-list lst)
  (if (null? lst)
      '(())
      (apply append
             (map (lambda (x)
                          (map (lambda (p) (cons x p))
                               (permute-list (remove x lst))))
                  lst))))

;;; remove : Element × List → List
;;; Remove first occurrence of element from list.
(define (remove x lst)
  (cond
   [(null? lst) '()]
   [(equal? x (car lst)) (cdr lst)]
   [else (cons (car lst) (remove x (cdr lst)))]))

;;; make-symmetric-group : Integer → Group
;;; Create the symmetric group S_n (all permutations of n elements).
(define (make-symmetric-group n)
  (let ([elements (all-permutations n)]
        [op permutation-compose]
        [identity (permutation-identity n)]
        [inverse-fn permutation-inverse]
        [equal-fn permutation-equal?])
       (make-group elements op identity inverse-fn equal-fn)))

;;; S : Integer → Group
;;; Alias for make-symmetric-group.
(define S make-symmetric-group)

;;; cycle-to-permutation : Integer × List<Integer> → Permutation
;;; Convert a cycle notation to a permutation.
;;; E.g., (cycle-to-permutation 3 '(0 1 2)) for the 3-cycle (0 1 2).
(define (cycle-to-permutation n cycle)
  (let ([result (iota n)])
       (if (null? cycle)
           result
           (let loop ([rest cycle] [res result])
                (if (null? (cdr rest))
                    (list-set res (car rest) (car cycle))
                    (loop (cdr rest)
                          (list-set res (car rest) (cadr rest))))))))

;;; transposition : Integer × Integer × Integer → Permutation
;;; Create a transposition swapping i and j in n elements.
(define (transposition n i j)
  (cycle-to-permutation n (list i j)))

;;; permutation-parity : Permutation → Integer
;;; Returns 0 for even permutation, 1 for odd.
(define (permutation-parity perm)
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

;;; ====
;;; Dihedral Groups D_n
;;; ====

;;; Dihedral group D_n: symmetries of a regular n-gon.
;;; Elements: rotations r^k (k = 0..n-1) and reflections s*r^k
;;; Represented as (type . k) where type is 'r or 's.

;;; dihedral-op : Integer × Element × Element → Element
;;; Group operation for D_n.
;;; Using relation: sr = r^(-1)s (reflection inverts rotation)
;;; r^a * r^b = r^(a+b mod n)
;;; r^a * sr^b = sr^(b-a mod n)
;;; sr^a * r^b = sr^(a+b mod n)
;;; sr^a * sr^b = r^(b-a mod n)
(define (dihedral-op n)
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

;;; dihedral-inverse : Integer × Element → Element
;;; Inverse in D_n.
;;; (r^k)⁻¹ = r^(-k mod n)
;;; (sr^k)⁻¹ = sr^k (reflections are self-inverse)
(define (dihedral-inverse n)
  (lambda (a)
          (let ([a-type (car a)] [a-k (cdr a)])
               (if (eq? a-type 'r)
                   (cons 'r (modulo (- n a-k) n))
                   a))))

;;; make-dihedral-group : Integer → Group
;;; Create the dihedral group D_n.
(define (make-dihedral-group n)
  (let* ([rotations (map (lambda (k) (cons 'r k)) (iota n))]
         [reflections (map (lambda (k) (cons 's k)) (iota n))]
         [elements (append rotations reflections)]
         [op (dihedral-op n)]
         [identity (cons 'r 0)]
         [inverse-fn (dihedral-inverse n)]
         [equal-fn equal?])
        (make-group elements op identity inverse-fn equal-fn)))

;;; D : Integer → Group
;;; Alias for make-dihedral-group.
(define D make-dihedral-group)

;;; ====
;;; Subgroup Testing
;;; ====

;;; is-subgroup? : Group × List<Element> → Boolean
;;; Test if a subset H forms a subgroup of G.
(define (is-subgroup? g h-elements)
  (let ([op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)]
        [eq? (group-equal-fn g)])
       (and
        ;; H is non-empty
        (not (null? h-elements))
        ;; Identity in H
        (any (lambda (x) (eq? x e)) h-elements)
        ;; Closed under operation
        (let loop-a ([as h-elements])
             (if (null? as)
                 #t
                 (let loop-b ([bs h-elements])
                      (if (null? bs)
                          (loop-a (cdr as))
                          (let ([result (op (car as) (car bs))])
                               (if (any (lambda (x) (eq? x result)) h-elements)
                                   (loop-b (cdr bs))
                                   #f))))))
        ;; Closed under inverses
        (let loop ([es h-elements])
             (if (null? es)
                 #t
                 (let ([a-inv (inv (car es))])
                      (if (any (lambda (x) (eq? x a-inv)) h-elements)
                          (loop (cdr es))
                          #f)))))))

;;; generate-subgroup : Group × List<Element> → List<Element>
;;; Generate the subgroup of G generated by the given elements.
(define (generate-subgroup g generators)
  (let ([op (group-op g)]
        [e (group-identity g)]
        [inv (group-inverse-fn g)]
        [eq? (group-equal-fn g)])
       (let loop ([current (cons e generators)])
            (let* ([new-elements
                    (apply append
                           (map (lambda (a)
                                        (append
                                         (map (lambda (b) (op a b)) current)
                                         (list (inv a))))
                                current))]
                   [unique
                    (fold-left
                     (lambda (acc x)
                             (if (any (lambda (y) (eq? x y)) acc)
                                 acc
                                 (cons x acc)))
                     current
                     new-elements)])
                  (if (= (length unique) (length current))
                      current
                      (loop unique))))))

;;; ====
;;; Group Homomorphisms
;;; ====

;;; A homomorphism is represented as:
;;; (homomorphism source-group target-group mapping)
;;; where mapping is a function Element → Element.

;;; make-homomorphism : Group × Group × (Element → Element) → Homomorphism
(define (make-homomorphism source target mapping)
  (list 'homomorphism source target mapping))

;;; homomorphism? : α → Boolean
(define (homomorphism? x)
  (and (list? x)
       (= (length x) 4)
       (eq? (car x) 'homomorphism)))

;;; homomorphism-source : Homomorphism → Group
(define (homomorphism-source h) (list-ref h 1))
;;; homomorphism-target : Homomorphism → Group
(define (homomorphism-target h) (list-ref h 2))
;;; homomorphism-mapping : Homomorphism → (Element → Element)
(define (homomorphism-mapping h) (list-ref h 3))

;;; homomorphism-apply : Homomorphism × Element → Element
;;; Apply the homomorphism to an element.
(define (homomorphism-apply h a)
  ((homomorphism-mapping h) a))

;;; verify-homomorphism : Homomorphism → Boolean
;;; Check that φ(a ∘ b) = φ(a) ∘ φ(b) for all a, b in source.
(define (verify-homomorphism h)
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

;;; is-isomorphism? : Homomorphism → Boolean
;;; Check if a homomorphism is an isomorphism (bijective).
(define (is-isomorphism? h)
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
                       (if (any (lambda (x) (eq? x (car imgs))) (cdr imgs))
                           #f
                           (loop (cdr imgs)))))
              ;; All target elements hit (surjective)
              (let loop ([targets target-elems])
                   (if (null? targets)
                       #t
                       (if (any (lambda (x) (eq? x (car targets))) images)
                           (loop (cdr targets))
                           #f)))))))

;;; kernel : Homomorphism → List<Element>
;;; Compute the kernel: elements that map to the identity.
(define (kernel h)
  (let* ([source (homomorphism-source h)]
         [target (homomorphism-target h)]
         [phi (homomorphism-mapping h)]
         [e-t (group-identity target)]
         [eq? (group-equal-fn target)])
        (filter (lambda (a) (eq? (phi a) e-t))
                (group-elements source))))

;;; image : Homomorphism → List<Element>
;;; Compute the image: elements in target that are hit.
(define (image h)
  (let* ([source (homomorphism-source h)]
         [target (homomorphism-target h)]
         [phi (homomorphism-mapping h)]
         [eq? (group-equal-fn target)])
        (fold-left
         (lambda (acc x)
                 (let ([img (phi x)])
                      (if (any (lambda (y) (eq? y img)) acc)
                          acc
                          (cons img acc))))
         '()
         (group-elements source))))

;;; ====
;;; Cayley Table
;;; ====

;;; cayley-table : Group → List<List<Element>>
;;; Generate the Cayley table (multiplication table) for a group.
(define (cayley-table g)
  (let ([elems (group-elements g)]
        [op (group-op g)])
       (map (lambda (a)
                    (map (lambda (b) (op a b)) elems))
            elems)))

;;; ====
;;; Utility Functions
;;; ====

;;; any : (Element → Boolean) × List → Boolean
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; filter : (Element → Boolean) × List → List
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

;;; iota : Integer → List<Integer>
;;; Generate list (0 1 2 ... n-1)
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; fold-left : (Acc × Element → Acc) × Acc × List → Acc
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

;;; ====
;;; Common Groups
;;; ====

;;; trivial-group : → Group
;;; The trivial group with one element.
(define (trivial-group)
  (make-group '(e)
              (lambda (a b) 'e)
              'e
              (lambda (a) 'e)
              eq?))

;;; klein-four-group : → Group
;;; The Klein four-group V₄ = Z₂ × Z₂.
(define (klein-four-group)
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

;;; index-of : Element × List → Integer
(define (index-of x lst)
  (let loop ([i 0] [rest lst])
       (cond
        [(null? rest) -1]
        [(equal? x (car rest)) i]
        [else (loop (+ i 1) (cdr rest))])))

;;; quaternion-group : → Group
;;; The quaternion group Q₈.
;;; Uses qi, qj, qk to avoid collision with Scheme's complex number i.
(define (quaternion-group)
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
