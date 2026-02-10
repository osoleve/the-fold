;;; lattice/algebra/module.ss — Module Theory over Rings
;;; @module module-theory
;;; @requires prelude ring
;;;
;;; Modules are generalizations of vector spaces where scalars come from
;;; a ring (not necessarily a field). This is fundamental for:
;;;   - Linear algebra over integers
;;;   - Homological algebra
;;;   - Representation theory
;;;   - Algebraic topology (homology/cohomology)
;;;
;;; This is Lattice code: pure, total, decomposes to Fold primitives.

(require 'prelude)
(require 'ring)

(doc 'module 'module-theory)
(doc 'description "Module theory: generalizations of vector spaces over rings")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Module Representation
;;; ============================================================================

(doc 'section 'module-representation)

(doc 'note "A module M over ring R is represented as:")
(doc 'note "(module ring elements add-op zero neg-fn scalar-mul equal-fn)")
(doc 'note "")
(doc 'note "Module axioms (M is an abelian group under +, with scalar mult ·):")
(doc 'note "  1. (M, +) is an abelian group")
(doc 'note "  2. 1·m = m (identity)")
(doc 'note "  3. (rs)·m = r·(s·m) (associativity)")
(doc 'note "  4. r·(m+n) = r·m + r·n (distributivity over module)")
(doc 'note "  5. (r+s)·m = r·m + s·m (distributivity over ring)")

;;; make-module : Ring × (List E) × (E × E → E) × E × (E → E) × (R × E → E) × (E × E → Bool) → Module
;;; Create a module over the given ring.
(define (make-module ring elements add-op zero neg-fn scalar-mul equal-fn)
  (doc 'export #t)
  (doc 'type '(-> Ring (List E) (-> E E E) E (-> E E) (-> R E E) (-> E E Bool) Module))
  (doc 'description "Create a module over the given ring")
  (list 'module ring elements add-op zero neg-fn scalar-mul equal-fn))

;;; module? : Any → Boolean
(define (module? x)
  (doc 'export #t)
  (and (list? x)
       (>= (length x) 7)
       (eq? (car x) 'module)))

;;; Module accessors
(define (module-ring m) (doc 'export #t) (list-ref m 1))
(define (module-elements m) (doc 'export #t) (list-ref m 2))
(define (module-add-op m) (doc 'export #t) (list-ref m 3))
(define (module-zero m) (doc 'export #t) (list-ref m 4))
(define (module-neg-fn m) (doc 'export #t) (list-ref m 5))
(define (module-scalar-mul m) (doc 'export #t) (list-ref m 6))
(define (module-equal-fn m) (doc 'export #t) (list-ref m 7))

;;; ============================================================================
;;; Module Operations
;;; ============================================================================

(doc 'section 'module-operations)

;;; module-add : Module × E × E → E
;;; Add two module elements.
(define (module-add mod a b)
  (doc 'export #t)
  ((module-add-op mod) a b))

;;; module-neg : Module × E → E
;;; Negate a module element.
(define (module-neg mod a)
  (doc 'export #t)
  ((module-neg-fn mod) a))

;;; module-sub : Module × E × E → E
;;; Subtract module elements: a - b = a + (-b)
(define (module-sub mod a b)
  (doc 'export #t)
  (module-add mod a (module-neg mod b)))

;;; module-smul : Module × R × E → E
;;; Scalar multiplication: r · m
(define (module-smul mod r elem)
  (doc 'export #t)
  ((module-scalar-mul mod) r elem))

;;; module-equal? : Module × E × E → Boolean
;;; Test equality of module elements.
(define (module-equal? mod a b)
  (doc 'export #t)
  ((module-equal-fn mod) a b))

;;; module-sum : Module × (List E) → E
;;; Sum of module elements.
(define (module-sum mod elements)
  (doc 'export #t)
  (if (null? elements)
      (module-zero mod)
      (let loop ([elems (cdr elements)] [acc (car elements)])
        (if (null? elems)
            acc
            (loop (cdr elems) (module-add mod acc (car elems)))))))

;;; module-linear-combination : Module × (List R) × (List E) → E
;;; Compute r1·e1 + r2·e2 + ... + rn·en
(define (module-linear-combination mod scalars elements)
  (doc 'export #t)
  (doc 'description "Compute linear combination of module elements")
  (if (or (null? scalars) (null? elements))
      (module-zero mod)
      (let loop ([rs scalars] [es elements] [acc (module-zero mod)])
        (if (or (null? rs) (null? es))
            acc
            (loop (cdr rs) (cdr es)
                  (module-add mod acc (module-smul mod (car rs) (car es))))))))

;;; ============================================================================
;;; Standard Modules
;;; ============================================================================

(doc 'section 'standard-modules)

;;; make-free-module-zn : Int × Int → Module
;;; Free module Z_n^k (k copies of Z_n).
;;; Elements are lists of k integers (mod n).
(define (make-free-module-zn n k)
  (doc 'export #t)
  (doc 'type '(-> Int Int Module))
  (doc 'description "Free module Z_n^k: k-tuples of integers mod n")
  (let ([ring (make-ring-zn n)])
    (make-module
     ring
     #f  ; Infinite (conceptually) - we don't enumerate
     (lambda (a b)  ; Addition
       (map (lambda (ai bi) (modulo (+ ai bi) n))
            a b))
     (make-list k 0)  ; Zero vector
     (lambda (a)  ; Negation
       (map (lambda (ai) (modulo (- ai) n)) a))
     (lambda (r a)  ; Scalar multiplication
       (map (lambda (ai) (modulo (* r ai) n)) a))
     equal?)))  ; Equality

;;; make-free-module-z : Int → Module
;;; Free module Z^k (k copies of Z).
;;; WARNING: Like make-ring-z, this is demonstration-only (unbounded integers).
(define (make-free-module-z k)
  (doc 'export #t)
  (doc 'type '(-> Int Module))
  (doc 'description "Free module Z^k: k-tuples of integers")
  (let ([ring (make-ring-z)])
    (make-module
     ring
     #f  ; Infinite
     (lambda (a b) (map + a b))  ; Addition
     (make-list k 0)  ; Zero
     (lambda (a) (map - a))  ; Negation
     (lambda (r a) (map (lambda (ai) (* r ai)) a))  ; Scalar mult
     equal?)))

;;; ============================================================================
;;; Submodules
;;; ============================================================================

(doc 'section 'submodules)

(doc 'note "A submodule N of M is a subset that is closed under:")
(doc 'note "  1. Addition: n1, n2 ∈ N ⟹ n1 + n2 ∈ N")
(doc 'note "  2. Scalar multiplication: r ∈ R, n ∈ N ⟹ r·n ∈ N")

;;; make-submodule : Module × (List E) → Submodule
;;; Create a submodule from a module and a list of generators.
(define (make-submodule parent generators)
  (doc 'export #t)
  (doc 'type '(-> Module (List E) Submodule))
  (doc 'description "Create submodule from generators (span of elements)")
  (list 'submodule parent generators))

;;; submodule? : Any → Boolean
(define (submodule? x)
  (doc 'export #t)
  (and (list? x) (eq? (car x) 'submodule)))

;;; submodule-parent : Submodule → Module
(define (submodule-parent s) (doc 'export #t) (list-ref s 1))

;;; submodule-generators : Submodule → (List E)
(define (submodule-generators s) (doc 'export #t) (list-ref s 2))

;;; is-in-submodule? : Submodule × E → Boolean | #f
;;; Test if element is in the submodule (for finite modules).
;;; Uses linear combination check over all ring elements.
;;; Returns #f if the ring is infinite (cannot enumerate coefficients).
(define (is-in-submodule? sub elem)
  (doc 'export #t)
  (doc 'type '(-> Submodule E (Or Boolean False)))
  (doc 'description "Test if element is in submodule span (returns #f for infinite rings)")
  (let* ([parent (submodule-parent sub)]
         [ring (module-ring parent)]
         [gens (submodule-generators sub)]
         [eq-fn (module-equal-fn parent)]
         [r-elems (ring-elements ring)])
    ;; Guard: cannot check membership for infinite rings
    (if (not r-elems)
        #f
        (if (null? gens)
            ;; Zero submodule: only zero is in it
            (eq-fn elem (module-zero parent))
            ;; Try all linear combinations (for finite rings)
            (let check-combos ([scalars (make-list (length gens) r-elems)])
              (or (null? scalars)
                  (let try-coeffs ([coeffs (cartesian-first scalars)])
                    (if (not coeffs)
                        #f
                        (let ([combo (module-linear-combination parent coeffs gens)])
                          (or (eq-fn combo elem)
                              (try-coeffs (cartesian-next scalars coeffs))))))))))))

;;; ============================================================================
;;; Module Homomorphisms
;;; ============================================================================

(doc 'section 'module-homomorphisms)

(doc 'note "A module homomorphism φ: M → N is a function satisfying:")
(doc 'note "  1. φ(m1 + m2) = φ(m1) + φ(m2)")
(doc 'note "  2. φ(r·m) = r·φ(m)")

;;; make-module-hom : Module × Module × (E → F) → ModuleHom
(define (make-module-hom source target phi)
  (doc 'export #t)
  (doc 'type '(-> Module Module (-> E F) ModuleHom))
  (doc 'description "Create a module homomorphism")
  (list 'module-hom source target phi))

;;; module-hom? : Any → Boolean
(define (module-hom? h)
  (doc 'export #t)
  (and (list? h) (eq? (car h) 'module-hom)))

;;; Module homomorphism accessors
(define (module-hom-source h) (doc 'export #t) (list-ref h 1))
(define (module-hom-target h) (doc 'export #t) (list-ref h 2))
(define (module-hom-phi h) (doc 'export #t) (list-ref h 3))

;;; module-hom-apply : ModuleHom × E → F
;;; Apply homomorphism to element.
(define (module-hom-apply h elem)
  (doc 'export #t)
  ((module-hom-phi h) elem))

;;; is-valid-module-hom? : ModuleHom → Boolean
;;; Verify homomorphism properties (for finite modules).
(define (is-valid-module-hom? h)
  (doc 'export #t)
  (doc 'type '(-> ModuleHom Boolean))
  (doc 'description "Verify module homomorphism properties")
  (let* ([M (module-hom-source h)]
         [N (module-hom-target h)]
         [phi (module-hom-phi h)]
         [m-elems (module-elements M)]
         [r-elems (ring-elements (module-ring M))]
         [n-eq (module-equal-fn N)])
    (and
     ;; Check φ(m1 + m2) = φ(m1) + φ(m2)
     (every? (lambda (m1)
               (every? (lambda (m2)
                        (n-eq (phi (module-add M m1 m2))
                              (module-add N (phi m1) (phi m2))))
                      m-elems))
            m-elems)
     ;; Check φ(r·m) = r·φ(m)
     (every? (lambda (r)
               (every? (lambda (m)
                        (n-eq (phi (module-smul M r m))
                              (module-smul N r (phi m))))
                      m-elems))
            r-elems))))

;;; module-hom-kernel : ModuleHom → Submodule
;;; Kernel of homomorphism: {m ∈ M | φ(m) = 0}
(define (module-hom-kernel h)
  (doc 'export #t)
  (doc 'type '(-> ModuleHom Submodule))
  (doc 'description "Compute kernel of homomorphism")
  (let* ([M (module-hom-source h)]
         [N (module-hom-target h)]
         [phi (module-hom-phi h)]
         [n-zero (module-zero N)]
         [n-eq (module-equal-fn N)]
         [kernel-elems (filter (lambda (m) (n-eq (phi m) n-zero))
                               (module-elements M))])
    (make-submodule M kernel-elems)))

;;; module-hom-image : ModuleHom → Submodule
;;; Image of homomorphism: {φ(m) | m ∈ M}
(define (module-hom-image h)
  (doc 'export #t)
  (doc 'type '(-> ModuleHom Submodule))
  (doc 'description "Compute image of homomorphism")
  (let* ([M (module-hom-source h)]
         [N (module-hom-target h)]
         [phi (module-hom-phi h)]
         [n-eq (module-equal-fn N)]
         [image-elems (unique-with-equal (map phi (module-elements M)) n-eq)])
    (make-submodule N image-elems)))

;;; module-hom-compose : ModuleHom × ModuleHom → ModuleHom
;;; Compose homomorphisms: (ψ ∘ φ)(m) = ψ(φ(m))
(define (module-hom-compose psi phi)
  (doc 'export #t)
  (doc 'type '(-> ModuleHom ModuleHom ModuleHom))
  (doc 'description "Compose two module homomorphisms")
  (make-module-hom
   (module-hom-source phi)
   (module-hom-target psi)
   (lambda (m) (module-hom-apply psi (module-hom-apply phi m)))))

;;; ============================================================================
;;; Quotient Modules
;;; ============================================================================

(doc 'section 'quotient-modules)

(doc 'note "The quotient module M/N consists of cosets m + N.")
(doc 'note "Two elements are equivalent if their difference is in N.")

;;; make-quotient-module : Module × Submodule → QuotientModule
;;; Construct quotient M/N.
(define (make-quotient-module mod sub)
  (doc 'export #t)
  (doc 'type '(-> Module Submodule QuotientModule))
  (doc 'description "Construct quotient module M/N")
  (let* ([ring (module-ring mod)]
         [m-elems (module-elements mod)]
         [m-eq (module-equal-fn mod)]
         [m-add (module-add-op mod)]
         [m-neg (module-neg-fn mod)]
         [m-smul (module-scalar-mul mod)]
         ;; Equivalence: a ~ b iff a - b ∈ N
         [equiv? (lambda (a b)
                   (is-in-submodule? sub (m-add a (m-neg b))))]
         ;; Coset representatives (one per equivalence class)
         [cosets (quotient-classes m-elems equiv?)])
    (make-module
     ring
     cosets  ; Elements are coset representatives
     (lambda (a b)  ; Addition: [a] + [b] = [a + b]
       (find-coset-rep cosets (m-add a b) equiv?))
     (find-coset-rep cosets (module-zero mod) equiv?)  ; Zero coset
     (lambda (a)  ; Negation
       (find-coset-rep cosets (m-neg a) equiv?))
     (lambda (r a)  ; Scalar multiplication
       (find-coset-rep cosets (m-smul r a) equiv?))
     equiv?)))  ; Equality is equivalence

;;; canonical-projection : Module × Submodule → ModuleHom
;;; The canonical projection π: M → M/N sending m to its coset [m].
(define (canonical-projection mod sub)
  (doc 'export #t)
  (doc 'type '(-> Module Submodule ModuleHom))
  (doc 'description "Canonical projection π: M → M/N")
  (let ([quotient (make-quotient-module mod sub)])
    (make-module-hom
     mod
     quotient
     (lambda (m)
       ;; Find representative of m's coset
       (find-coset-rep (module-elements quotient) m
                       (module-equal-fn quotient))))))

;;; ============================================================================
;;; Tensor Products
;;; ============================================================================

(doc 'section 'tensor-products)

(doc 'note "The tensor product M ⊗_R N is the universal object for R-bilinear maps.")
(doc 'note "Elements are formal sums of pure tensors m ⊗ n, subject to:")
(doc 'note "  (m1 + m2) ⊗ n = m1 ⊗ n + m2 ⊗ n")
(doc 'note "  m ⊗ (n1 + n2) = m ⊗ n1 + m ⊗ n2")
(doc 'note "  (r·m) ⊗ n = m ⊗ (r·n) = r·(m ⊗ n)")

;;; For finite modules, we represent tensor product elements as lists of
;;; (coefficient . (m . n)) pairs, simplified using the tensor relations.

;;; make-tensor-product : Module × Module → Module | #f
;;; Construct tensor product M ⊗_R N.
;;; Returns #f if either module is infinite (cannot enumerate tensor basis).
(define (make-tensor-product mod1 mod2)
  (doc 'export #t)
  (doc 'type '(-> Module Module (Or Module False)))
  (doc 'description "Construct tensor product M ⊗ N (returns #f for infinite modules)")
  (let* ([m1-elems (module-elements mod1)]
         [m2-elems (module-elements mod2)])
    ;; Guard: cannot construct tensor product of infinite modules
    (if (not (and m1-elems m2-elems))
        #f
        (let* ([ring (module-ring mod1)]  ; Both modules should be over same ring
               [r-elems (ring-elements ring)]
               [r-add (ring-add-op ring)]
               [r-mul (ring-mul-op ring)]
               [r-zero (ring-zero ring)]
               [r-one (ring-one ring)]
               [r-eq (ring-equal-fn ring)]
               ;; Pure tensors: all pairs (m, n)
               [pure-tensors (cartesian-product m1-elems m2-elems)]
         ;; Tensor elements are linear combinations of pure tensors
         ;; Represented as association lists: ((m . n) . coeff) ...
         [tensor-add (lambda (t1 t2)
                       (tensor-simplify (append t1 t2) r-add r-zero r-eq))]
         [tensor-zero '()]
         [tensor-neg (lambda (t)
                       (map (lambda (term)
                              (cons (car term) ((ring-neg-fn ring) (cdr term))))
                            t))]
         [tensor-smul (lambda (r t)
                        (tensor-simplify
                         (map (lambda (term)
                                (cons (car term) (r-mul r (cdr term))))
                              t)
                         r-add r-zero r-eq))]
         [tensor-eq (lambda (t1 t2)
                      (tensor-equal? t1 t2 r-eq r-zero))])
      ;; Generate all tensor elements (for small finite modules)
      (let ([all-tensors
             (if (null? pure-tensors)
                 (list tensor-zero)
                 (generate-tensor-elements pure-tensors r-elems r-add r-zero r-eq))])
        (make-module
         ring
         all-tensors
         tensor-add
         tensor-zero
         tensor-neg
         tensor-smul
         tensor-eq))))))

;;; tensor : Module × E × F → TensorElement
;;; Create pure tensor m ⊗ n.
(define (tensor mod1 mod2 m n)
  (doc 'export #t)
  (doc 'type '(-> Module Module E F TensorElement))
  (doc 'description "Create pure tensor m ⊗ n")
  (list (cons (cons m n) (ring-one (module-ring mod1)))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; every? : (a → Bool) × (List a) → Bool
(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every? pred (cdr lst)))))

;;; cartesian-product : (List a) × (List b) → (List (a . b))
(define (cartesian-product as bs)
  (append-map (lambda (a)
                (map (lambda (b) (cons a b)) bs))
              as))

;;; cartesian-first : (List (List a)) → (List a) | #f
;;; Get first element of cartesian product of lists.
(define (cartesian-first lists)
  (if (or (null? lists) (any? null? lists))
      #f
      (map car lists)))

;;; cartesian-next : (List (List a)) × (List a) → (List a) | #f
;;; Get next element in cartesian product iteration.
(define (cartesian-next lists current)
  (let loop ([ls (reverse lists)] [cur (reverse current)] [carry #t])
    (if (null? ls)
        (if carry #f (reverse cur))
        (if carry
            (let* ([this-list (car ls)]
                   [this-val (car cur)]
                   [rest (member this-val this-list)]
                   [next-rest (if rest (cdr rest) '())])
              (if (null? next-rest)
                  (loop (cdr ls) (cons (car this-list) (cdr cur)) #t)
                  (loop (cdr ls) (cons (car next-rest) (cdr cur)) #f)))
            (loop (cdr ls) (cons (car cur) (cdr cur)) #f)))))

;;; any? : (a → Bool) × (List a) → Bool
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))

;;; quotient-classes : (List a) × (a × a → Bool) → (List a)
;;; Return one representative from each equivalence class.
(define (quotient-classes elements equiv?)
  (let loop ([elems elements] [reps '()])
    (if (null? elems)
        (reverse reps)
        (let ([e (car elems)])
          (if (any? (lambda (r) (equiv? e r)) reps)
              (loop (cdr elems) reps)
              (loop (cdr elems) (cons e reps)))))))

;;; find-coset-rep : (List a) × a × (a × a → Bool) → a
;;; Find the coset representative equivalent to elem.
(define (find-coset-rep reps elem equiv?)
  (let loop ([rs reps])
    (if (null? rs)
        elem  ; Fallback: use elem itself
        (if (equiv? elem (car rs))
            (car rs)
            (loop (cdr rs))))))

;;; tensor-simplify : TensorElement × (R × R → R) × R × (R × R → Bool) → TensorElement
;;; Combine like terms and remove zeros.
(define (tensor-simplify terms r-add r-zero r-eq)
  (let loop ([ts terms] [result '()])
    (if (null? ts)
        (filter (lambda (term) (not (r-eq (cdr term) r-zero))) result)
        (let* ([term (car ts)]
               [key (car term)]
               [coeff (cdr term)]
               [existing (assoc key result)])
          (if existing
              (loop (cdr ts)
                    (cons (cons key (r-add (cdr existing) coeff))
                          (remove-first (lambda (t) (equal? (car t) key)) result)))
              (loop (cdr ts) (cons term result)))))))

;;; tensor-equal? : TensorElement × TensorElement × (R × R → Bool) × R → Bool
(define (tensor-equal? t1 t2 r-eq r-zero)
  (let ([s1 (tensor-simplify t1 (lambda (a b) a) r-zero r-eq)]
        [s2 (tensor-simplify t2 (lambda (a b) a) r-zero r-eq)])
    (and (= (length s1) (length s2))
         (every? (lambda (term1)
                   (let ([term2 (assoc (car term1) s2)])
                     (and term2 (r-eq (cdr term1) (cdr term2)))))
                 s1))))

;;; remove-first : (a → Bool) × (List a) → (List a)
(define (remove-first pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cdr lst)]
    [else (cons (car lst) (remove-first pred (cdr lst)))]))

;;; generate-tensor-elements : (List (a . b)) × (List R) × ... → (List TensorElement)
;;; Generate all tensor elements for small finite cases.
(define (generate-tensor-elements pure-tensors r-elems r-add r-zero r-eq)
  ;; For now, just generate pure tensors with coefficient 1
  ;; Full enumeration would be exponential
  (cons '()  ; Zero tensor
        (map (lambda (pt)
               (list (cons pt (car r-elems))))  ; Use first non-zero ring element
             pure-tensors)))

;;; make-list : Int × a → (List a)
;;; Create list of n copies of x.
(define (make-list n x)
  (let loop ([i n] [result '()])
    (if (<= i 0)
        result
        (loop (- i 1) (cons x result)))))
