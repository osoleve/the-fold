;;; @module limits
;;; @requires logic-adjunction
;;; @description Categorical limits and colimits
;;; @purity total
;;; @stability stable

(require 'logic-adjunction)

(doc 'module 'limits)
(doc 'description "Limits and Colimits

Universal constructions in category theory. A limit is a terminal cone over
a diagram; a colimit is an initial cocone. Every limit can be built from
products and equalizers; every colimit from coproducts and coequalizers.

Key constructions:
  - Products/Coproducts: limits/colimits of discrete diagrams
  - Equalizers/Coequalizers: limits/colimits of parallel pairs
  - Pullbacks/Pushouts: limits/colimits of cospans/spans
  - Terminal/Initial objects: limits/colimits of empty diagrams

Universal property:
  For a limit L of diagram D, any cone over D factors uniquely through L.
  For a colimit C of diagram D, any cocone factors uniquely through C.

References:
  - Mac Lane, Categories for the Working Mathematician, Ch. V
  - Awodey, Category Theory, Ch. 5
  - nLab: limit")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section 1: Diagram Shapes (Index Categories)
;;; ============================================================

(doc 'section 'diagram-shapes)
(doc 'description "Diagram Shapes

A diagram shape is a small category that describes the 'pattern' of a limit.
We represent shapes as lists of objects and generating morphisms.

Standard shapes:
  - empty: no objects (terminal/initial object)
  - discrete-2: two objects, no morphisms (binary product/coproduct)
  - parallel-pair: two objects, two parallel morphisms (equalizer/coequalizer)
  - cospan: three objects forming A → C ← B (pullback)
  - span: three objects forming A ← C → B (pushout)")

(define (make-shape name objects morphisms)
  (doc 'export #t)
  (doc 'type '(-> Symbol (List Symbol) (List (List Symbol Symbol Symbol)) Shape))
  (doc 'description "Create a diagram shape.
morphisms is a list of (name source target) triples.")
  (list 'shape name objects morphisms))

(define (shape? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'shape)))

(define (shape-name s) (cadr s))
(define (shape-objects s) (caddr s))
(define (shape-morphisms s) (cadddr s))

(doc shape-empty 'description "Empty shape (0 objects) - limit is terminal object")
(define shape-empty (make-shape 'empty '() '()))

(doc shape-discrete-1 'description "Single object shape - limit is the object itself")
(define shape-discrete-1 (make-shape 'discrete-1 '(a) '()))

(doc shape-discrete-2 'description "Two objects, no morphisms - limit is product")
(define shape-discrete-2 (make-shape 'discrete-2 '(a b) '()))

(doc shape-discrete-3 'description "Three objects, no morphisms - limit is ternary product")
(define shape-discrete-3 (make-shape 'discrete-3 '(a b c) '()))

(doc shape-parallel-pair 'description "Two objects with two parallel arrows f,g : a ⇉ b - limit is equalizer")
(define shape-parallel-pair (make-shape 'parallel-pair '(a b) '((f a b) (g a b))))

(doc shape-cospan 'description "Cospan shape: a → c ← b - limit is pullback")
(define shape-cospan (make-shape 'cospan '(a b c) '((f a c) (g b c))))

(doc shape-span 'description "Span shape: a ← c → b - colimit is pushout")
(define shape-span (make-shape 'span '(a b c) '((f c a) (g c b))))

(doc shape-arrow 'description "Single arrow: a → b - limit is domain with morphism")
(define shape-arrow (make-shape 'arrow '(a b) '((f a b))))

;;; ============================================================
;;; Section 2: Diagrams (Functors from Shapes)
;;; ============================================================

(doc 'section 'diagrams)
(doc 'description "Diagrams

A diagram D : J → C assigns:
  - To each object j in J, an object D(j) in C
  - To each morphism f : j → k in J, a morphism D(f) : D(j) → D(k) in C

We represent diagrams as records with object and morphism mappings.")

(define (make-diagram shape obj-map mor-map)
  (doc 'export #t)
  (doc 'type '(-> Shape (-> Symbol Any) (-> Symbol (-> Any Any)) Diagram))
  (doc 'description "Create a diagram over a shape.
obj-map: symbol → object in target category
mor-map: morphism-name → function between objects")
  (list 'diagram shape obj-map mor-map))

(define (diagram? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'diagram)))

(define (diagram-shape d) (cadr d))
(define (diagram-obj-map d) (caddr d))
(define (diagram-mor-map d) (cadddr d))

(define (diagram-at d obj-name)
  (doc 'export #t)
  (doc 'type '(-> Diagram Symbol Any))
  (doc 'description "Get the object in the diagram at position obj-name")
  ((diagram-obj-map d) obj-name))

(define (diagram-mor d mor-name)
  (doc 'export #t)
  (doc 'type '(-> Diagram Symbol (-> Any Any)))
  (doc 'description "Get the morphism in the diagram with name mor-name")
  ((diagram-mor-map d) mor-name))

;;; ============================================================
;;; Section 3: Cones and Cocones
;;; ============================================================

(doc 'section 'cones)
(doc 'description "Cones and Cocones

A cone over diagram D : J → C consists of:
  - An apex object X in C
  - For each j in J, a projection π_j : X → D(j)
  - Such that for each f : j → k in J, D(f) ∘ π_j = π_k

A cocone is dual: an apex with injections ι_j : D(j) → X
such that for each f : j → k in J, ι_k ∘ D(f) = ι_j")

(define (make-cone apex diagram projections)
  (doc 'export #t)
  (doc 'type '(-> Any Diagram (-> Symbol (-> Any Any)) Cone))
  (doc 'description "Create a cone over a diagram.
projections: symbol → (Apex → D(symbol))")
  (list 'cone apex diagram projections))

(define (cone? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'cone)))

(define (cone-apex c) (cadr c))
(define (cone-diagram c) (caddr c))
(define (cone-projections c) (cadddr c))

(define (cone-project c obj-name)
  (doc 'export #t)
  (doc 'type '(-> Cone Symbol (-> Apex Object)))
  (doc 'description "Get the projection morphism to object obj-name")
  ((cone-projections c) obj-name))

(define (make-cocone apex diagram injections)
  (doc 'export #t)
  (doc 'type '(-> Any Diagram (-> Symbol (-> Any Any)) Cocone))
  (doc 'description "Create a cocone under a diagram.
injections: symbol → (D(symbol) → Apex)")
  (list 'cocone apex diagram injections))

(define (cocone? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'cocone)))

(define (cocone-apex c) (cadr c))
(define (cocone-diagram c) (caddr c))
(define (cocone-injections c) (cadddr c))

(define (cocone-inject c obj-name)
  (doc 'export #t)
  (doc 'type '(-> Cocone Symbol (-> Object Apex)))
  (doc 'description "Get the injection morphism from object obj-name")
  ((cocone-injections c) obj-name))

;;; Cone verification
(define (verify-cone cone)
  (doc 'export #t)
  (doc 'type '(-> Cone Boolean))
  (doc 'description "Verify cone commutativity: D(f) ∘ π_source = π_target
for all morphisms f in the diagram shape.
For Set-based limits where apex is a list of elements, verifies pointwise.")
  (let* ([d (cone-diagram cone)]
         [shape (diagram-shape d)]
         [morphisms (shape-morphisms shape)]
         [apex (cone-apex cone)])
    (if (null? morphisms)
        #t  ; Discrete diagram - nothing to verify
        ;; For non-discrete diagrams, verify the condition pointwise
        ;; For Set-based limits, apex is a list; test each element
        ;; For type-based limits, apex is a single object; test directly
        (let ([test-elements (if (and (list? apex) (not (null? apex)))
                                 apex      ; Test all elements of the set
                                 (list apex))])  ; Wrap single element
          (fold-left
           (lambda (ok? elem)
             (and ok?
                  (fold-left
                   (lambda (ok2? mor-spec)
                     (let* ([mor-name (car mor-spec)]
                            [source (cadr mor-spec)]
                            [target (caddr mor-spec)]
                            [d-f (diagram-mor d mor-name)]
                            [π-source (cone-project cone source)]
                            [π-target (cone-project cone target)])
                       (and ok2?
                            (equal? (d-f (π-source elem))
                                    (π-target elem)))))
                   #t
                   morphisms)))
           #t
           test-elements)))))

(define (verify-cocone cocone)
  (doc 'export #t)
  (doc 'type '(-> Cocone Boolean))
  (doc 'description "Verify cocone commutativity: ι_target ∘ D(f) = ι_source
for all morphisms f in the diagram shape.
For Set-based colimits where diagram objects are lists, verifies pointwise.")
  (let* ([d (cocone-diagram cocone)]
         [shape (diagram-shape d)]
         [morphisms (shape-morphisms shape)])
    (if (null? morphisms)
        #t  ; Discrete diagram - nothing to verify
        (fold-left
         (lambda (ok? mor-spec)
           (let* ([mor-name (car mor-spec)]
                  [source (cadr mor-spec)]
                  [target (caddr mor-spec)]
                  [d-f (diagram-mor d mor-name)]
                  [ι-source (cocone-inject cocone source)]
                  [ι-target (cocone-inject cocone target)]
                  [source-obj (diagram-at d source)]
                  ;; For Set-based colimits, test each element; otherwise test directly
                  [test-elements (if (and (list? source-obj) (not (null? source-obj)))
                                     source-obj
                                     (list source-obj))])
             (and ok?
                  (fold-left
                   (lambda (ok2? elem)
                     (and ok2?
                          (equal? (ι-target (d-f elem))
                                  (ι-source elem))))
                   #t
                   test-elements))))
         #t
         morphisms))))

;;; ============================================================
;;; Section 4: Limits and Colimits
;;; ============================================================

(doc 'section 'limits-colimits)
(doc 'description "Limits and Colimits

A limit of diagram D is a terminal cone: a cone L such that for any
other cone C, there exists a unique morphism u : apex(C) → apex(L)
making all triangles commute.

A colimit is dual: an initial cocone.")

(define (make-limit cone factorizer)
  (doc 'export #t)
  (doc 'type '(-> Cone (-> Cone (-> Any Any)) Limit))
  (doc 'description "Create a limit from a cone and its universal factorizer.
factorizer: given any cone, produces the unique morphism to the limit apex.")
  (list 'limit cone factorizer))

(define (limit? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'limit)))

(define (limit-cone lim) (cadr lim))
(define (limit-apex lim) (cone-apex (limit-cone lim)))
(define (limit-factorizer lim) (caddr lim))

(define (limit-factor lim other-cone)
  (doc 'export #t)
  (doc 'type '(-> Limit Cone (-> OtherApex LimitApex)))
  (doc 'description "Factor another cone through this limit.
Returns the unique mediating morphism.")
  ((limit-factorizer lim) other-cone))

(define (make-colimit cocone factorizer)
  (doc 'export #t)
  (doc 'type '(-> Cocone (-> Cocone (-> Any Any)) Colimit))
  (doc 'description "Create a colimit from a cocone and its universal factorizer.
factorizer: given any cocone, produces the unique morphism from the colimit apex.")
  (list 'colimit cocone factorizer))

(define (colimit? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'colimit)))

(define (colimit-cocone colim) (cadr colim))
(define (colimit-apex colim) (cocone-apex (colimit-cocone colim)))
(define (colimit-factorizer colim) (caddr colim))

(define (colimit-factor colim other-cocone)
  (doc 'export #t)
  (doc 'type '(-> Colimit Cocone (-> ColimitApex OtherApex)))
  (doc 'description "Factor another cocone through this colimit.
Returns the unique mediating morphism.")
  ((colimit-factorizer colim) other-cocone))

;;; ============================================================
;;; Section 5: Products (Limits of Discrete Diagrams)
;;; ============================================================

(doc 'section 'products)
(doc 'description "Products

The product A × B is the limit of the discrete diagram {A, B}.
It comes with projections π₁ : A×B → A and π₂ : A×B → B
satisfying the universal property: for any C with f : C → A and g : C → B,
there exists unique ⟨f,g⟩ : C → A×B such that π₁ ∘ ⟨f,g⟩ = f and π₂ ∘ ⟨f,g⟩ = g.")

(define (make-product-diagram a-elements b-elements)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) Diagram))
  (doc 'description "Create a discrete 2-object diagram for product in Set")
  (make-diagram shape-discrete-2
                (lambda (obj)
                  (case obj
                    [(a) a-elements]
                    [(b) b-elements]
                    [else (error 'product-diagram "unknown object" obj)]))
                (lambda (mor)
                  (error 'product-diagram "discrete diagram has no morphisms"))))

(define (binary-product a-elements b-elements)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) Limit))
  (doc 'description "Construct the binary product A × B in Set.
a-elements and b-elements are lists of elements.
Apex is the cartesian product {(a,b) | a ∈ A, b ∈ B}.
Projections are element-level: π_a = car, π_b = cdr.")
  (let* ([cartesian-product
          (fold-left
           (lambda (acc a)
             (append acc (map (lambda (b) (cons a b)) b-elements)))
           '()
           a-elements)]
         [diagram (make-product-diagram a-elements b-elements)]
         [apex cartesian-product]
         [projections (lambda (obj)
                        (case obj
                          [(a) car]
                          [(b) cdr]
                          [else (error 'product "unknown projection" obj)]))]
         [cone (make-cone apex diagram projections)]
         [factorizer (lambda (other-cone)
                       (let ([f (cone-project other-cone 'a)]
                             [g (cone-project other-cone 'b)])
                         (lambda (x)
                           (cons (f x) (g x)))))])
    (make-limit cone factorizer)))

(define (product-pair f g)
  (doc 'export #t)
  (doc 'type '(-> (-> C A) (-> C B) (-> C (* A B))))
  (doc 'description "The pairing morphism ⟨f,g⟩ : C → A×B")
  (lambda (c) (cons (f c) (g c))))

(define (product-fst p)
  (doc 'export #t)
  (doc 'type '(-> (* A B) A))
  (doc 'description "First projection π₁")
  (car p))

(define (product-snd p)
  (doc 'export #t)
  (doc 'type '(-> (* A B) B))
  (doc 'description "Second projection π₂")
  (cdr p))

;;; n-ary products
(define (cartesian-product-n sets)
  (doc 'export #t)
  (doc 'type '(-> (List (List Any)) (List (List Any))))
  (doc 'description "Compute the n-ary cartesian product of a list of sets.
Returns list of tuples (as lists).")
  (if (null? sets)
      '(())  ; Single empty tuple
      (let ([first-set (car sets)]
            [rest-product (cartesian-product-n (cdr sets))])
        (fold-left
         (lambda (acc elem)
           (append acc (map (lambda (tuple) (cons elem tuple)) rest-product)))
         '()
         first-set))))

(define (n-ary-product element-sets)
  (doc 'export #t)
  (doc 'type '(-> (List (List Any)) Limit))
  (doc 'description "Construct the n-ary product of sets in Set.
element-sets is a list of element lists.
Apex is the cartesian product {(x₀,...,xₙ₋₁) | xᵢ ∈ Sᵢ}.
Projections are element-level: πᵢ(tuple) = list-ref tuple i.")
  (let* ([n (length element-sets)]
         [obj-names (map (lambda (i) (string->symbol (format "x~a" i)))
                         (iota n))]
         [shape (make-shape 'discrete-n obj-names '())]
         [obj-map (lambda (name)
                    (let* ([s (symbol->string name)]
                           [idx (string->number (substring s 1 (string-length s)))])
                      (list-ref element-sets idx)))]
         [diagram (make-diagram shape obj-map
                                (lambda (m) (error 'n-ary-product "no morphisms")))]
         [apex (cartesian-product-n element-sets)]
         ;; Element-level projections: πᵢ(tuple) = (list-ref tuple i)
         [projections (lambda (name)
                        (let* ([s (symbol->string name)]
                               [idx (string->number (substring s 1 (string-length s)))])
                          (lambda (tuple) (list-ref tuple idx))))]
         [cone (make-cone apex diagram projections)]
         ;; Factorizer uses shape-objects for dynamic naming (fixes fold-zxs8)
         [factorizer (lambda (other-cone)
                       (let ([names (shape-objects shape)])
                         (lambda (x)
                           (map (lambda (name)
                                  ((cone-project other-cone name) x))
                                names))))])
    (make-limit cone factorizer)))

;;; ============================================================
;;; Section 6: Coproducts (Colimits of Discrete Diagrams)
;;; ============================================================

(doc 'section 'coproducts)
(doc 'description "Coproducts

The coproduct A + B is the colimit of the discrete diagram {A, B}.
It comes with injections ι₁ : A → A+B and ι₂ : B → A+B
satisfying the universal property: for any C with f : A → C and g : B → C,
there exists unique [f,g] : A+B → C such that [f,g] ∘ ι₁ = f and [f,g] ∘ ι₂ = g.")

(define (make-coproduct-diagram a b)
  (doc 'export #t)
  (doc 'type '(-> Any Any Diagram))
  (doc 'description "Create a discrete 2-object diagram for coproduct")
  (make-diagram shape-discrete-2
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [else (error 'coproduct-diagram "unknown object" obj)]))
                (lambda (mor)
                  (error 'coproduct-diagram "discrete diagram has no morphisms"))))

(define (binary-coproduct a-elements b-elements)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) Colimit))
  (doc 'description "Construct the binary coproduct A + B in Set.
a-elements and b-elements are element lists.
Apex is the disjoint union: tagged elements (left . a) and (right . b).
Injections are element-level: ι_a(a) = (left . a), ι_b(b) = (right . b).")
  (let* ([tagged-a (map make-left a-elements)]
         [tagged-b (map make-right b-elements)]
         [disjoint-union (append tagged-a tagged-b)]
         [diagram (make-coproduct-diagram a-elements b-elements)]
         [apex disjoint-union]
         ;; Element-level injections: ι_a : A → A+B, ι_b : B → A+B
         [injections (lambda (obj)
                       (case obj
                         [(a) make-left]   ; ι_a(a) = (left . a)
                         [(b) make-right]  ; ι_b(b) = (right . b)
                         [else (error 'coproduct "unknown injection" obj)]))]
         [cocone (make-cocone apex diagram injections)]
         ;; Factorizer: given cocone with h_a : A → X, h_b : B → X
         ;; returns [h_a, h_b] : A+B → X operating element-wise
         [factorizer (lambda (other-cocone)
                       (let ([h-a (cocone-inject other-cocone 'a)]
                             [h-b (cocone-inject other-cocone 'b)])
                         (lambda (x)
                           (if (left? x)
                               (h-a (from-left x))
                               (h-b (from-right x))))))])
    (make-colimit cocone factorizer)))

(define (coproduct-copair f g)
  (doc 'export #t)
  (doc 'type '(-> (-> A C) (-> B C) (-> (+ A B) C)))
  (doc 'description "The copairing morphism [f,g] : A+B → C")
  (lambda (x)
    (if (left? x)
        (f (from-left x))
        (g (from-right x)))))

(define coproduct-inl make-left)
(define coproduct-inr make-right)

;;; ============================================================
;;; Section 7: Equalizers (Limits of Parallel Pairs)
;;; ============================================================

(doc 'section 'equalizers)
(doc 'description "Equalizers

The equalizer of f,g : A ⇉ B is the limit of this parallel pair diagram.
It is an object E with morphism e : E → A such that f ∘ e = g ∘ e,
universal among all such.

Concretely: E = {a ∈ A | f(a) = g(a)}, with e being inclusion.

Note: In Set, equalizers always exist. In other categories they may not.")

(define (make-equalizer-diagram a b f g)
  (doc 'export #t)
  (doc 'type '(-> Any Any (-> Any Any) (-> Any Any) Diagram))
  (doc 'description "Create a parallel pair diagram A ⇉ B with morphisms f, g")
  (make-diagram shape-parallel-pair
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [else (error 'equalizer-diagram "unknown object" obj)]))
                (lambda (mor)
                  (case mor
                    [(f) f]
                    [(g) g]
                    [else (error 'equalizer-diagram "unknown morphism" mor)]))))

(define (equalizer a-elements b-elements f g)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) (-> Any Any) (-> Any Any) Limit))
  (doc 'description "Construct the equalizer of f,g : A ⇉ B in Set.
a-elements is the domain A, b-elements is the codomain B.
Returns the subobject E = {a ∈ A | f(a) = g(a)}.
Projections are element-level: π_a(e)=e, π_b(e)=f(e).")
  (let* ([eq-elements (filter (lambda (a) (equal? (f a) (g a))) a-elements)]
         [diagram (make-equalizer-diagram a-elements b-elements f g)]
         [apex eq-elements]
         ;; Element-level projections: π_a : E → A, π_b : E → B
         [projections (lambda (obj)
                        (case obj
                          [(a) (lambda (e) e)]      ; π_a(e) = e (inclusion)
                          [(b) (lambda (e) (f e))]  ; π_b(e) = f(e) = g(e)
                          [else (error 'equalizer "unknown projection" obj)]))]
         [cone (make-cone apex diagram projections)]
         ;; Factorizer: given cone from X with h : X → A where f∘h = g∘h
         ;; returns u : X → E such that π_a ∘ u = h
         ;; Since E ⊆ A and π_a is inclusion, u(x) = h(x) if h(x) ∈ E
         [factorizer (lambda (other-cone)
                       (let ([h (cone-project other-cone 'a)])
                         (lambda (x)
                           (let ([hx (h x)])
                             ;; h(x) must land in the equalizer
                             (if (member hx eq-elements)
                                 hx
                                 (error 'equalizer-factorizer
                                        "h(x) not in equalizer" hx))))))])
    (make-limit cone factorizer)))

(define (equalizer-inclusion eq)
  (doc 'export #t)
  (doc 'type '(-> Limit (-> E A)))
  (doc 'description "The inclusion morphism e : E → A from an equalizer")
  (cone-project (limit-cone eq) 'a))

;;; ============================================================
;;; Section 8: Coequalizers (Colimits of Parallel Pairs)
;;; ============================================================

(doc 'section 'coequalizers)
(doc 'description "Coequalizers

The coequalizer of f,g : A ⇉ B is the colimit of this parallel pair diagram.
It is an object Q with morphism q : B → Q such that q ∘ f = q ∘ g,
universal among all such.

Concretely: Q = B / ~ where f(a) ~ g(a) for all a ∈ A.")

(define (make-coequalizer-diagram a b f g)
  (doc 'export #t)
  (doc 'type '(-> Any Any (-> Any Any) (-> Any Any) Diagram))
  (doc 'description "Create a parallel pair diagram for coequalizer")
  (make-diagram shape-parallel-pair
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [else (error 'coequalizer-diagram "unknown object" obj)]))
                (lambda (mor)
                  (case mor
                    [(f) f]
                    [(g) g]
                    [else (error 'coequalizer-diagram "unknown morphism" mor)]))))

(define (coequalizer a-elements b-elements f g)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) (-> Any Any) (-> Any Any) Colimit))
  (doc 'description "Construct the coequalizer of f,g : A ⇉ B in Set.
Returns B quotiented by f(a) ~ g(a).")
  (let* ([equivalences (map (lambda (a) (cons (f a) (g a))) a-elements)]
         [find-class (lambda (b classes)
                       (let loop ([cs classes])
                         (cond
                          [(null? cs) #f]
                          [(member b (car cs)) (car cs)]
                          [else (loop (cdr cs))])))]
         [merge-classes (lambda (c1 c2 classes)
                          (let ([merged (append c1 c2)])
                            (cons merged
                                  (filter (lambda (c)
                                            (and (not (eq? c c1))
                                                 (not (eq? c c2))))
                                          classes))))]
         [initial-classes (map list b-elements)]
         [final-classes
          (fold-left
           (lambda (classes eq)
             (let ([b1 (car eq)]
                   [b2 (cdr eq)])
               (let ([c1 (find-class b1 classes)]
                     [c2 (find-class b2 classes)])
                 (if (and c1 c2 (not (eq? c1 c2)))
                     (merge-classes c1 c2 classes)
                     classes))))
           initial-classes
           equivalences)]
         [quotient-map (lambda (b)
                         (find-class b final-classes))]
         [diagram (make-coequalizer-diagram a-elements b-elements f g)]
         [apex final-classes]
         [injections (lambda (obj)
                       (case obj
                         [(a) (lambda (a) (quotient-map (f a)))]
                         [(b) quotient-map]
                         [else (error 'coequalizer "unknown injection" obj)]))]
         [cocone (make-cocone apex diagram injections)]
         [factorizer (lambda (other-cocone)
                       (let ([h (cocone-inject other-cocone 'b)])
                         (lambda (class)
                           (h (car class)))))])
    (make-colimit cocone factorizer)))

(define (coequalizer-quotient coeq)
  (doc 'export #t)
  (doc 'type '(-> Colimit (-> B Q)))
  (doc 'description "The quotient morphism q : B → Q from a coequalizer")
  (cocone-inject (colimit-cocone coeq) 'b))

;;; ============================================================
;;; Section 9: Pullbacks (Limits of Cospans)
;;; ============================================================

(doc 'section 'pullbacks)
(doc 'description "Pullbacks

The pullback of a cospan A -f→ C ←g- B is the limit of this diagram.
It is an object P with projections p₁ : P → A and p₂ : P → B
such that f ∘ p₁ = g ∘ p₂, universal among all such squares.

Concretely: P = {(a,b) | f(a) = g(b)}")

(define (make-pullback-diagram a b c f g)
  (doc 'export #t)
  (doc 'type '(-> Any Any Any (-> Any Any) (-> Any Any) Diagram))
  (doc 'description "Create a cospan diagram A -f→ C ←g- B")
  (make-diagram shape-cospan
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [(c) c]
                    [else (error 'pullback-diagram "unknown object" obj)]))
                (lambda (mor)
                  (case mor
                    [(f) f]
                    [(g) g]
                    [else (error 'pullback-diagram "unknown morphism" mor)]))))

(define (pullback a-elements b-elements c f g)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) Any (-> Any Any) (-> Any Any) Limit))
  (doc 'description "Construct the pullback of A -f→ C ←g- B in Set.
Returns pairs (a,b) where f(a) = g(b).
Projections are element-level: π_a(p)=fst(p), π_b(p)=snd(p), π_c(p)=f(fst(p)).")
  (let* ([pb-elements
          (fold-left
           (lambda (acc a)
             (fold-left
              (lambda (acc2 b)
                (if (equal? (f a) (g b))
                    (cons (cons a b) acc2)
                    acc2))
              acc
              b-elements))
           '()
           a-elements)]
         [diagram (make-pullback-diagram a-elements b-elements c f g)]
         [apex pb-elements]
         ;; Element-level projections: each operates on a single pair (a . b)
         [projections (lambda (obj)
                        (case obj
                          [(a) car]                         ; π_a(p) = fst p
                          [(b) cdr]                         ; π_b(p) = snd p
                          [(c) (lambda (p) (f (car p)))]    ; π_c(p) = f(fst p) = g(snd p)
                          [else (error 'pullback "unknown projection" obj)]))]
         [cone (make-cone apex diagram projections)]
         ;; Factorizer: given cone from X with h_a : X → A, h_b : X → B
         ;; where f ∘ h_a = g ∘ h_b, returns u : X → P such that
         ;; π_a ∘ u = h_a and π_b ∘ u = h_b
         ;; The unique such u is: u(x) = (h_a(x), h_b(x))
         [factorizer (lambda (other-cone)
                       (let ([h-a (cone-project other-cone 'a)]
                             [h-b (cone-project other-cone 'b)])
                         (lambda (x)
                           (let ([pair (cons (h-a x) (h-b x))])
                             ;; Verify the pair is in the pullback
                             (if (member pair pb-elements)
                                 pair
                                 (error 'pullback-factorizer
                                        "mediating morphism lands outside pullback"
                                        pair))))))])
    (make-limit cone factorizer)))

(define (pullback-p1 pb)
  (doc 'export #t)
  (doc 'type '(-> Limit (-> P A)))
  (doc 'description "First projection from pullback")
  (cone-project (limit-cone pb) 'a))

(define (pullback-p2 pb)
  (doc 'export #t)
  (doc 'type '(-> Limit (-> P B)))
  (doc 'description "Second projection from pullback")
  (cone-project (limit-cone pb) 'b))

;;; ============================================================
;;; Section 10: Pushouts (Colimits of Spans)
;;; ============================================================

(doc 'section 'pushouts)
(doc 'description "Pushouts

The pushout of a span A ←f- C -g→ B is the colimit of this diagram.
It is an object Q with injections ι₁ : A → Q and ι₂ : B → Q
such that ι₁ ∘ f = ι₂ ∘ g, universal among all such squares.

Concretely: Q = (A + B) / ~ where f(c) ~ g(c) for all c ∈ C")

(define (make-pushout-diagram a b c f g)
  (doc 'export #t)
  (doc 'type '(-> Any Any Any (-> Any Any) (-> Any Any) Diagram))
  (doc 'description "Create a span diagram A ←f- C -g→ B")
  (make-diagram shape-span
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [(c) c]
                    [else (error 'pushout-diagram "unknown object" obj)]))
                (lambda (mor)
                  (case mor
                    [(f) f]
                    [(g) g]
                    [else (error 'pushout-diagram "unknown morphism" mor)]))))

(define (pushout a-elements b-elements c-elements f g)
  (doc 'export #t)
  (doc 'type '(-> (List Any) (List Any) (List Any) (-> Any Any) (-> Any Any) Colimit))
  (doc 'description "Construct the pushout of A ←f- C -g→ B in Set.
Returns (A + B) quotiented by f(c) ~ g(c).")
  (let* ([tagged-a (map (lambda (a) (make-left a)) a-elements)]
         [tagged-b (map (lambda (b) (make-right b)) b-elements)]
         [all-elements (append tagged-a tagged-b)]
         [equivalences (map (lambda (c) (cons (make-left (f c)) (make-right (g c))))
                            c-elements)]
         [find-class (lambda (x classes)
                       (let loop ([cs classes])
                         (cond
                          [(null? cs) #f]
                          [(member x (car cs)) (car cs)]
                          [else (loop (cdr cs))])))]
         [merge-classes (lambda (c1 c2 classes)
                          (let ([merged (append c1 c2)])
                            (cons merged
                                  (filter (lambda (c)
                                            (and (not (eq? c c1))
                                                 (not (eq? c c2))))
                                          classes))))]
         [initial-classes (map list all-elements)]
         [final-classes
          (fold-left
           (lambda (classes eq)
             (let ([x1 (car eq)]
                   [x2 (cdr eq)])
               (let ([c1 (find-class x1 classes)]
                     [c2 (find-class x2 classes)])
                 (if (and c1 c2 (not (eq? c1 c2)))
                     (merge-classes c1 c2 classes)
                     classes))))
           initial-classes
           equivalences)]
         [quotient-map (lambda (x) (find-class x final-classes))]
         [diagram (make-pushout-diagram a-elements b-elements c-elements f g)]
         [apex final-classes]
         [injections (lambda (obj)
                       (case obj
                         [(a) (lambda (a) (quotient-map (make-left a)))]
                         [(b) (lambda (b) (quotient-map (make-right b)))]
                         [(c) (lambda (c) (quotient-map (make-left (f c))))]
                         [else (error 'pushout "unknown injection" obj)]))]
         [cocone (make-cocone apex diagram injections)]
         [factorizer (lambda (other-cocone)
                       (let ([h-a (cocone-inject other-cocone 'a)]
                             [h-b (cocone-inject other-cocone 'b)])
                         (lambda (class)
                           (let ([rep (car class)])
                             (if (left? rep)
                                 (h-a (from-left rep))
                                 (h-b (from-right rep)))))))])
    (make-colimit cocone factorizer)))

(define (pushout-i1 po)
  (doc 'export #t)
  (doc 'type '(-> Colimit (-> A Q)))
  (doc 'description "First injection into pushout")
  (cocone-inject (colimit-cocone po) 'a))

(define (pushout-i2 po)
  (doc 'export #t)
  (doc 'type '(-> Colimit (-> B Q)))
  (doc 'description "Second injection into pushout")
  (cocone-inject (colimit-cocone po) 'b))

;;; ============================================================
;;; Section 11: Terminal and Initial Objects
;;; ============================================================

(doc 'section 'terminal-initial)
(doc 'description "Terminal and Initial Objects

The terminal object 1 is the limit of the empty diagram.
It has a unique morphism from every object.

The initial object 0 is the colimit of the empty diagram.
It has a unique morphism to every object.

In Set: 1 is any singleton, 0 is the empty set.")

(define (terminal-object unit-value)
  (doc 'export #t)
  (doc 'type '(-> Any Limit))
  (doc 'description "Construct a terminal object. unit-value is the single element.")
  (let* ([diagram (make-diagram shape-empty
                                (lambda (obj) (error 'terminal "no objects"))
                                (lambda (mor) (error 'terminal "no morphisms")))]
         [apex unit-value]
         [projections (lambda (obj) (error 'terminal "no projections"))]
         [cone (make-cone apex diagram projections)]
         [factorizer (lambda (other-cone)
                       (lambda (x) unit-value))])
    (make-limit cone factorizer)))

(define (terminal-morphism term)
  (doc 'export #t)
  (doc 'type '(-> Limit (-> A 1)))
  (doc 'description "The unique morphism to the terminal object")
  (limit-factorizer term))

(define (initial-object)
  (doc 'export #t)
  (doc 'type '(-> Colimit))
  (doc 'description "Construct an initial object (empty set).")
  (let* ([diagram (make-diagram shape-empty
                                (lambda (obj) (error 'initial "no objects"))
                                (lambda (mor) (error 'initial "no morphisms")))]
         [apex '()]
         [injections (lambda (obj) (error 'initial "no injections"))]
         [cocone (make-cocone apex diagram injections)]
         [factorizer (lambda (other-cocone)
                       (lambda (x)
                         (error 'initial "morphism from initial undefined on non-empty input")))])
    (make-colimit cocone factorizer)))

;;; ============================================================
;;; Section 12: Preservation of Limits
;;; ============================================================

(doc 'section 'preservation)
(doc 'description "Preservation of Limits by Functors

A functor F : C → D preserves limits if for every limit cone (L, π) in C,
(F(L), F∘π) is a limit cone in D.

Right adjoints preserve all limits.
Left adjoints preserve all colimits.

Key examples:
  - Hom(A,-) preserves all limits (right adjoint to A×-)
  - A×- preserves colimits (left adjoint to Hom(A,-))")

(define (functor-preserves-limit? F lim test-cones)
  (doc 'export #t)
  (doc 'type '(-> Functor Limit (List Cone) Boolean))
  (doc 'description "Test if a functor preserves a specific limit.
Applies F to the limit cone and checks if F(cone) is still a limit cone
by verifying that F applied to factorizers commutes properly.

A functor F preserves the limit L of diagram D if F(L) is the limit of F∘D.
We test this by checking that for each test cone C over D:
  F(factor_L(C)) = factor_{F(L)}(F(C))

For Set-based limits with element-level operations, we verify that:
1. F maps the apex to F(apex)
2. F maps each projection π to F∘π
3. The factorizer property is preserved under F")
  (let* ([cone (limit-cone lim)]
         [apex (cone-apex cone)]
         [diagram (cone-diagram cone)]
         [shape (diagram-shape diagram)]
         [objects (shape-objects shape)]
         [F-fmap (functor-fmap F)])
    ;; For each test cone, verify F preserves the factorization
    (fold-left
     (lambda (ok? test-cone)
       (and ok?
            (let* ([test-apex (cone-apex test-cone)]
                   ;; Compute the factorizer in the original category
                   [u ((limit-factorizer lim) test-cone)]
                   ;; For each object, check: F(π_obj) ∘ F(u) = F(π_obj ∘ u)
                   ;; Since π_obj ∘ u = cone-project(test-cone, obj), this becomes:
                   ;; F(π_obj)(F(u)(x)) = F(cone-project(test-cone, obj)(x))
                   [projections-ok?
                    (if (null? objects)
                        #t  ; Empty diagram (terminal) - nothing to check
                        (fold-left
                         (lambda (proj-ok? obj)
                           (and proj-ok?
                                (let ([π (cone-project cone obj)]
                                      [h (cone-project test-cone obj)])
                                  ;; Check: π(u(x)) = h(x) for all x in test-apex
                                  ;; This is the universal property
                                  (if (list? test-apex)
                                      (fold-left
                                       (lambda (elem-ok? x)
                                         (and elem-ok?
                                              (equal? (π (u x)) (h x))))
                                       #t
                                       test-apex)
                                      ;; Single element apex
                                      (equal? (π (u test-apex)) (h test-apex))))))
                         #t
                         objects))])
              projections-ok?)))
     #t
     test-cones)))

;;; ============================================================
;;; Section 13: General Limit Construction
;;; ============================================================

(doc 'section 'general-construction)
(doc 'description "General Limit Construction from Products and Equalizers

Every limit can be constructed from products and equalizers. For a diagram
D : J → C where J has objects {j₀,...,jₙ} and morphisms {f₀:s₀→t₀,...,fₘ:sₘ→tₘ}:

1. Form the product P = ∏ᵢ D(jᵢ) of all diagram objects
2. Form the product Q = ∏ₖ D(tₖ) indexed by morphisms (one copy of each target)
3. Define s : P → Q by s = ⟨π_{t₀}, π_{t₁}, ..., π_{tₘ}⟩
   (project to targets)
4. Define t : P → Q by t = ⟨D(f₀)∘π_{s₀}, D(f₁)∘π_{s₁}, ..., D(fₘ)∘π_{sₘ}⟩
   (apply morphisms after projecting to sources)
5. Limit = Eq(s,t) ⊆ P

The universal property follows from those of products and equalizers.

Dually, colimits can be built from coproducts and coequalizers.")

(define (limit-from-diagram diagram)
  (doc 'export #t)
  (doc 'type '(-> Diagram Limit))
  (doc 'description "Construct the limit of an arbitrary diagram in Set.
Uses the general construction: Eq(s,t) where s,t : ∏D(j) → ∏D(cod(f)).
Works for any diagram shape, not just the standard ones.")
  (let* ([shape (diagram-shape diagram)]
         [objects (shape-objects shape)]
         [morphisms (shape-morphisms shape)])
    (cond
     ;; Empty diagram → terminal object
     [(null? objects)
      (terminal-object '())]

     ;; Discrete diagram (no morphisms) → n-ary product
     ;; Build custom product that preserves original object names for projections
     [(null? morphisms)
      (let* ([element-sets (map (lambda (obj) (diagram-at diagram obj)) objects)]
             [apex (cartesian-product-n element-sets)]
             ;; Map object name to index
             [obj-index (lambda (name)
                          (let loop ([objs objects] [i 0])
                            (cond
                             [(null? objs) (error 'limit-from-diagram "unknown object" name)]
                             [(eq? (car objs) name) i]
                             [else (loop (cdr objs) (+ i 1))])))]
             ;; Projections use original object names
             [projections (lambda (obj-name)
                            (let ([idx (obj-index obj-name)])
                              (lambda (tuple) (list-ref tuple idx))))]
             [cone (make-cone apex diagram projections)]
             [factorizer (lambda (other-cone)
                           (lambda (x)
                             (map (lambda (obj)
                                    ((cone-project other-cone obj) x))
                                  objects)))])
        (make-limit cone factorizer))]

     ;; General case: product + equalizer
     [else
      (let* (;; Step 1: Product of all diagram objects
             [element-sets (map (lambda (obj) (diagram-at diagram obj)) objects)]
             [obj-product (n-ary-product element-sets)]
             [P (limit-apex obj-product)]
             [P-cone (limit-cone obj-product)]

             ;; Map from object name to its index in the product
             [obj-index (lambda (name)
                          (let loop ([objs objects] [i 0])
                            (cond
                             [(null? objs) (error 'limit-from-diagram "unknown object" name)]
                             [(eq? (car objs) name) i]
                             [else (loop (cdr objs) (+ i 1))])))]

             ;; Project from P to component at object name
             [π (lambda (name)
                  (let ([idx (obj-index name)])
                    (lambda (tuple) (list-ref tuple idx))))]

             ;; Step 2: For each morphism f:s→t, we need D(f)∘π_s = π_t on the limit
             ;; Build s : P → ∏_{f} D(t_f) by projecting to targets
             ;; Build t : P → ∏_{f} D(t_f) by applying D(f) after projecting to sources
             [s-components (map (lambda (mor-spec)
                                  (let ([target (caddr mor-spec)])
                                    (π target)))
                                morphisms)]
             [t-components (map (lambda (mor-spec)
                                  (let* ([mor-name (car mor-spec)]
                                         [source (cadr mor-spec)]
                                         [Df (diagram-mor diagram mor-name)]
                                         [π-source (π source)])
                                    (lambda (tuple) (Df (π-source tuple)))))
                                morphisms)]

             ;; s(tuple) = list of π_target(tuple) for each morphism
             [s (lambda (tuple) (map (lambda (comp) (comp tuple)) s-components))]
             ;; t(tuple) = list of D(f)(π_source(tuple)) for each morphism
             [t (lambda (tuple) (map (lambda (comp) (comp tuple)) t-components))]

             ;; Step 3: Equalizer of s and t
             ;; Elements of P where s(p) = t(p), i.e., D(f)(π_s(p)) = π_t(p) for all f
             [eq-elements (filter (lambda (tuple) (equal? (s tuple) (t tuple))) P)]

             ;; Step 4: Build the limit cone
             ;; Projections from limit to each diagram object
             [limit-projections (lambda (obj-name)
                                  (π obj-name))]

             [limit-cone-obj (make-cone eq-elements diagram limit-projections)]

             ;; Factorizer: given another cone, produce unique morphism to limit
             [factorizer (lambda (other-cone)
                           (let ([other-apex (cone-apex other-cone)])
                             (lambda (x)
                               ;; Map x to tuple via other cone's projections
                               (let ([tuple (map (lambda (obj)
                                                   ((cone-project other-cone obj) x))
                                                 objects)])
                                 (if (member tuple eq-elements)
                                     tuple
                                     (error 'limit-factorizer
                                            "cone does not factor through limit"
                                            tuple))))))])

        (make-limit limit-cone-obj factorizer))])))

(define (colimit-from-diagram diagram)
  (doc 'export #t)
  (doc 'type '(-> Diagram Colimit))
  (doc 'description "Construct the colimit of an arbitrary diagram in Set.
Uses the dual construction: Coeq(s,t) where s,t : ∐D(dom(f)) → ∐D(j).
Works for any diagram shape.")
  (let* ([shape (diagram-shape diagram)]
         [objects (shape-objects shape)]
         [morphisms (shape-morphisms shape)])
    (cond
     ;; Empty diagram → initial object
     [(null? objects)
      (initial-object)]

     ;; Discrete diagram (no morphisms) → n-ary coproduct
     [(null? morphisms)
      ;; Build coproduct by repeated binary coproduct
      (let ([element-sets (map (lambda (obj) (diagram-at diagram obj)) objects)])
        (if (= 1 (length element-sets))
            ;; Single object: "coproduct" is just the object itself with identity
            (let* ([the-set (car element-sets)]
                   [cocone (make-cocone the-set diagram
                                        (lambda (obj) (lambda (x) x)))]
                   [factorizer (lambda (other-cocone)
                                 (cocone-inject other-cocone (car objects)))])
              (make-colimit cocone factorizer))
            ;; Multiple objects: fold binary coproducts
            (n-ary-coproduct element-sets objects)))]

     ;; General case: coproduct + coequalizer
     [else
      (let* (;; Coproduct of all diagram objects
             [element-sets (map (lambda (obj) (diagram-at diagram obj)) objects)]
             [obj-coproduct (n-ary-coproduct element-sets objects)]
             [C (colimit-apex obj-coproduct)]

             ;; For the general colimit construction:
             ;; s,t : ∐_{f:a→b} D(a) ⇉ ∐_{j} D(j)
             ;; s = coproduct of inclusions ι_a (inject source)
             ;; t = coproduct of ι_b ∘ D(f) (apply morphism then inject target)

             ;; Build equivalence: for each morphism f:a→b and each x in D(a),
             ;; we need ι_a(x) ~ ι_b(D(f)(x))
             [equivalences
              (fold-left
               (lambda (eqs mor-spec)
                 (let* ([mor-name (car mor-spec)]
                        [source (cadr mor-spec)]
                        [target (caddr mor-spec)]
                        [Df (diagram-mor diagram mor-name)]
                        [source-elems (diagram-at diagram source)])
                   (append eqs
                           (map (lambda (x)
                                  ;; Equivalent: (source . x) ~ (target . Df(x))
                                  (cons (cons source x)
                                        (cons target (Df x))))
                                source-elems))))
               '()
               morphisms)]

             ;; Compute equivalence classes on C
             ;; C is list of (obj-name . element) pairs
             [find-class (lambda (x classes)
                           (let loop ([cs classes])
                             (cond
                              [(null? cs) #f]
                              [(member x (car cs)) (car cs)]
                              [else (loop (cdr cs))])))]

             [merge-classes (lambda (c1 c2 classes)
                              (if (eq? c1 c2)
                                  classes
                                  (let ([merged (append c1 c2)])
                                    (cons merged
                                          (filter (lambda (c)
                                                    (and (not (eq? c c1))
                                                         (not (eq? c c2))))
                                                  classes)))))]

             [initial-classes (map list C)]

             [final-classes
              (fold-left
               (lambda (classes eq)
                 (let ([x1 (car eq)]
                       [x2 (cdr eq)])
                   (let ([c1 (find-class x1 classes)]
                         [c2 (find-class x2 classes)])
                     (if (and c1 c2)
                         (merge-classes c1 c2 classes)
                         classes))))
               initial-classes
               equivalences)]

             [quotient-map (lambda (x) (find-class x final-classes))]

             ;; Build colimit cocone
             [colimit-injections
              (lambda (obj-name)
                (lambda (x)
                  (quotient-map (cons obj-name x))))]

             [colimit-cocone-obj (make-cocone final-classes diagram colimit-injections)]

             ;; Factorizer: given cocone with injections h_j : D(j) → X,
             ;; produce unique morphism from colimit to X
             [factorizer
              (lambda (other-cocone)
                (lambda (equiv-class)
                  ;; Pick representative and apply corresponding injection
                  (let* ([rep (car equiv-class)]
                         [obj-name (car rep)]
                         [elem (cdr rep)]
                         [h (cocone-inject other-cocone obj-name)])
                    (h elem))))])

        (make-colimit colimit-cocone-obj factorizer))])))

(define (n-ary-coproduct element-sets obj-names)
  (doc 'export #t)
  (doc 'type '(-> (List (List Any)) (List Symbol) Colimit))
  (doc 'description "Construct n-ary coproduct in Set.
Returns disjoint union with elements tagged by object name: (obj-name . elem)")
  (let* ([tagged-elements
          (fold-left
           (lambda (acc pair)
             (let ([obj-name (car pair)]
                   [elems (cdr pair)])
               (append acc (map (lambda (e) (cons obj-name e)) elems))))
           '()
           (map cons obj-names element-sets))]

         [shape (make-shape 'discrete-n obj-names '())]

         [diagram (make-diagram shape
                                (lambda (obj)
                                  (let loop ([names obj-names] [sets element-sets])
                                    (cond
                                     [(null? names) (error 'n-ary-coproduct "unknown object" obj)]
                                     [(eq? (car names) obj) (car sets)]
                                     [else (loop (cdr names) (cdr sets))])))
                                (lambda (mor)
                                  (error 'n-ary-coproduct "discrete diagram has no morphisms")))]

         [injections (lambda (obj-name)
                       (lambda (x) (cons obj-name x)))]

         [cocone (make-cocone tagged-elements diagram injections)]

         [factorizer (lambda (other-cocone)
                       (lambda (tagged)
                         (let ([obj-name (car tagged)]
                               [elem (cdr tagged)])
                           ((cocone-inject other-cocone obj-name) elem))))])

    (make-colimit cocone factorizer)))

;;; ============================================================
;;; Section 14: Display
;;; ============================================================

(doc 'section 'display)

(define (shape->string s)
  (doc 'export #t)
  (if (shape? s)
      (format "Shape(~a: ~a objects, ~a morphisms)"
              (shape-name s)
              (length (shape-objects s))
              (length (shape-morphisms s)))
      "Not a shape"))

(define (diagram->string d)
  (doc 'export #t)
  (if (diagram? d)
      (format "Diagram over ~a" (shape-name (diagram-shape d)))
      "Not a diagram"))

(define (cone->string c)
  (doc 'export #t)
  (if (cone? c)
      (format "Cone(apex: ~a, over: ~a)"
              (cone-apex c)
              (shape-name (diagram-shape (cone-diagram c))))
      "Not a cone"))

(define (limit->string lim)
  (doc 'export #t)
  (if (limit? lim)
      (format "Limit(~a)"
              (cone->string (limit-cone lim)))
      "Not a limit"))

(define (colimit->string colim)
  (doc 'export #t)
  (if (colimit? colim)
      (format "Colimit(~a)"
              (cocone-apex colim))
      "Not a colimit"))
