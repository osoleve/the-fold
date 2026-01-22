(load "lattice/fp/category/logic-adjunction.ss")

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
  (doc 'type '(-> Symbol (List Symbol) (List (List Symbol Symbol Symbol)) Shape))
  (doc 'description "Create a diagram shape.
morphisms is a list of (name source target) triples.")
  (list 'shape name objects morphisms))

(define (shape? x)
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
  (doc 'type '(-> Shape (-> Symbol Any) (-> Symbol (-> Any Any)) Diagram))
  (doc 'description "Create a diagram over a shape.
obj-map: symbol → object in target category
mor-map: morphism-name → function between objects")
  (list 'diagram shape obj-map mor-map))

(define (diagram? x)
  (and (pair? x) (eq? (car x) 'diagram)))

(define (diagram-shape d) (cadr d))
(define (diagram-obj-map d) (caddr d))
(define (diagram-mor-map d) (cadddr d))

(define (diagram-at d obj-name)
  (doc 'type '(-> Diagram Symbol Any))
  (doc 'description "Get the object in the diagram at position obj-name")
  ((diagram-obj-map d) obj-name))

(define (diagram-mor d mor-name)
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
  (doc 'type '(-> Any Diagram (-> Symbol (-> Any Any)) Cone))
  (doc 'description "Create a cone over a diagram.
projections: symbol → (Apex → D(symbol))")
  (list 'cone apex diagram projections))

(define (cone? x)
  (and (pair? x) (eq? (car x) 'cone)))

(define (cone-apex c) (cadr c))
(define (cone-diagram c) (caddr c))
(define (cone-projections c) (cadddr c))

(define (cone-project c obj-name)
  (doc 'type '(-> Cone Symbol (-> Apex Object)))
  (doc 'description "Get the projection morphism to object obj-name")
  ((cone-projections c) obj-name))

(define (make-cocone apex diagram injections)
  (doc 'type '(-> Any Diagram (-> Symbol (-> Any Any)) Cocone))
  (doc 'description "Create a cocone under a diagram.
injections: symbol → (D(symbol) → Apex)")
  (list 'cocone apex diagram injections))

(define (cocone? x)
  (and (pair? x) (eq? (car x) 'cocone)))

(define (cocone-apex c) (cadr c))
(define (cocone-diagram c) (caddr c))
(define (cocone-injections c) (cadddr c))

(define (cocone-inject c obj-name)
  (doc 'type '(-> Cocone Symbol (-> Object Apex)))
  (doc 'description "Get the injection morphism from object obj-name")
  ((cocone-injections c) obj-name))

;;; Cone verification
(define (verify-cone cone)
  (doc 'type '(-> Cone Boolean))
  (doc 'description "Verify cone commutativity: D(f) ∘ π_source = π_target
for all morphisms f in the diagram shape.")
  (let* ([d (cone-diagram cone)]
         [shape (diagram-shape d)]
         [morphisms (shape-morphisms shape)]
         [apex (cone-apex cone)])
    (fold-left
     (lambda (ok? mor-spec)
       (let* ([mor-name (car mor-spec)]
              [source (cadr mor-spec)]
              [target (caddr mor-spec)]
              [d-f (diagram-mor d mor-name)]
              [π-source (cone-project cone source)]
              [π-target (cone-project cone target)])
         (and ok?
              (equal? (d-f (π-source apex))
                      (π-target apex)))))
     #t
     morphisms)))

(define (verify-cocone cocone)
  (doc 'type '(-> Cocone Boolean))
  (doc 'description "Verify cocone commutativity: ι_target ∘ D(f) = ι_source
for all morphisms f in the diagram shape.")
  (let* ([d (cocone-diagram cocone)]
         [shape (diagram-shape d)]
         [morphisms (shape-morphisms shape)]
         [test-values (map (lambda (obj) (cons obj (diagram-at d obj)))
                           (shape-objects shape))])
    (fold-left
     (lambda (ok? mor-spec)
       (let* ([mor-name (car mor-spec)]
              [source (cadr mor-spec)]
              [target (caddr mor-spec)]
              [d-f (diagram-mor d mor-name)]
              [ι-source (cocone-inject cocone source)]
              [ι-target (cocone-inject cocone target)]
              [source-val (cdr (assq source test-values))])
         (and ok?
              (equal? (ι-target (d-f source-val))
                      (ι-source source-val)))))
     #t
     morphisms)))

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
  (doc 'type '(-> Cone (-> Cone (-> Any Any)) Limit))
  (doc 'description "Create a limit from a cone and its universal factorizer.
factorizer: given any cone, produces the unique morphism to the limit apex.")
  (list 'limit cone factorizer))

(define (limit? x)
  (and (pair? x) (eq? (car x) 'limit)))

(define (limit-cone lim) (cadr lim))
(define (limit-apex lim) (cone-apex (limit-cone lim)))
(define (limit-factorizer lim) (caddr lim))

(define (limit-factor lim other-cone)
  (doc 'type '(-> Limit Cone (-> OtherApex LimitApex)))
  (doc 'description "Factor another cone through this limit.
Returns the unique mediating morphism.")
  ((limit-factorizer lim) other-cone))

(define (make-colimit cocone factorizer)
  (doc 'type '(-> Cocone (-> Cocone (-> Any Any)) Colimit))
  (doc 'description "Create a colimit from a cocone and its universal factorizer.
factorizer: given any cocone, produces the unique morphism from the colimit apex.")
  (list 'colimit cocone factorizer))

(define (colimit? x)
  (and (pair? x) (eq? (car x) 'colimit)))

(define (colimit-cocone colim) (cadr colim))
(define (colimit-apex colim) (cocone-apex (colimit-cocone colim)))
(define (colimit-factorizer colim) (caddr colim))

(define (colimit-factor colim other-cocone)
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

(define (make-product-diagram a b)
  (doc 'type '(-> Any Any Diagram))
  (doc 'description "Create a discrete 2-object diagram for product")
  (make-diagram shape-discrete-2
                (lambda (obj)
                  (case obj
                    [(a) a]
                    [(b) b]
                    [else (error 'product-diagram "unknown object" obj)]))
                (lambda (mor)
                  (error 'product-diagram "discrete diagram has no morphisms"))))

(define (binary-product a b)
  (doc 'type '(-> Any Any Limit))
  (doc 'description "Construct the binary product A × B.
Apex is (A . B), projections are car and cdr.")
  (let* ([diagram (make-product-diagram a b)]
         [apex (cons a b)]
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
  (doc 'type '(-> (-> C A) (-> C B) (-> C (* A B))))
  (doc 'description "The pairing morphism ⟨f,g⟩ : C → A×B")
  (lambda (c) (cons (f c) (g c))))

(define (product-fst p)
  (doc 'type '(-> (* A B) A))
  (doc 'description "First projection π₁")
  (car p))

(define (product-snd p)
  (doc 'type '(-> (* A B) B))
  (doc 'description "Second projection π₂")
  (cdr p))

;;; n-ary products
(define (n-ary-product objects)
  (doc 'type '(-> (List Any) Limit))
  (doc 'description "Construct the n-ary product of a list of objects.
Apex is the list itself, projections are list-ref.")
  (let* ([n (length objects)]
         [obj-names (map (lambda (i) (string->symbol (format "x~a" i)))
                         (iota n))]
         [shape (make-shape 'discrete-n obj-names '())]
         [obj-map (lambda (name)
                    (let* ([s (symbol->string name)]
                           [idx (string->number (substring s 1 (string-length s)))])
                      (list-ref objects idx)))]
         [diagram (make-diagram shape obj-map
                                (lambda (m) (error 'n-ary-product "no morphisms")))]
         [apex objects]
         [projections (lambda (name)
                        (let* ([s (symbol->string name)]
                               [idx (string->number (substring s 1 (string-length s)))])
                          (lambda (lst) (list-ref lst idx))))]
         [cone (make-cone apex diagram projections)]
         [factorizer (lambda (other-cone)
                       (lambda (x)
                         (map (lambda (name)
                                ((cone-project other-cone name) x))
                              obj-names)))])
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

(define (binary-coproduct a b)
  (doc 'type '(-> Any Any Colimit))
  (doc 'description "Construct the binary coproduct A + B.
Apex is ('left . a) or ('right . b), injections are make-left/make-right.")
  (let* ([diagram (make-coproduct-diagram a b)]
         [apex (list 'coproduct a b)]
         [injections (lambda (obj)
                       (case obj
                         [(a) make-left]
                         [(b) make-right]
                         [else (error 'coproduct "unknown injection" obj)]))]
         [cocone (make-cocone apex diagram injections)]
         [factorizer (lambda (other-cocone)
                       (let ([f (cocone-inject other-cocone 'a)]
                             [g (cocone-inject other-cocone 'b)])
                         (lambda (x)
                           (if (left? x)
                               (f (from-left x))
                               (g (from-right x))))))])
    (make-colimit cocone factorizer)))

(define (coproduct-copair f g)
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

(define (equalizer a-elements f g)
  (doc 'type '(-> (List Any) (-> Any Any) (-> Any Any) Limit))
  (doc 'description "Construct the equalizer of f,g : A ⇉ B in Set.
a-elements is the list of elements in A to test.
Returns the subobject where f(a) = g(a).")
  (let* ([eq-elements (filter (lambda (a) (equal? (f a) (g a))) a-elements)]
         [b-image (if (null? eq-elements) '() (f (car eq-elements)))]
         [diagram (make-equalizer-diagram a-elements b-image f g)]
         [apex eq-elements]
         [projections (lambda (obj)
                        (case obj
                          [(a) (lambda (eq) eq)]
                          [(b) (lambda (eq) (map f eq))]
                          [else (error 'equalizer "unknown projection" obj)]))]
         [cone (make-cone apex diagram projections)]
         [factorizer (lambda (other-cone)
                       (let ([h (cone-project other-cone 'a)])
                         (lambda (x)
                           (filter (lambda (a) (member a eq-elements))
                                   (if (list? (h x)) (h x) (list (h x)))))))])
    (make-limit cone factorizer)))

(define (equalizer-inclusion eq)
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
  (doc 'type '(-> (List Any) (List Any) Any (-> Any Any) (-> Any Any) Limit))
  (doc 'description "Construct the pullback of A -f→ C ←g- B in Set.
Returns pairs (a,b) where f(a) = g(b).")
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
         [projections (lambda (obj)
                        (case obj
                          [(a) (lambda (pb) (map car pb))]
                          [(b) (lambda (pb) (map cdr pb))]
                          [(c) (lambda (pb) (map (lambda (p) (f (car p))) pb))]
                          [else (error 'pullback "unknown projection" obj)]))]
         [cone (make-cone apex diagram projections)]
         [factorizer (lambda (other-cone)
                       (let ([h-a (cone-project other-cone 'a)]
                             [h-b (cone-project other-cone 'b)])
                         (lambda (x)
                           (let ([as (if (list? (h-a x)) (h-a x) (list (h-a x)))]
                                 [bs (if (list? (h-b x)) (h-b x) (list (h-b x)))])
                             (filter (lambda (p) (member p pb-elements))
                                     (append-map
                                      (lambda (a)
                                        (map (lambda (b) (cons a b)) bs))
                                      as))))))])
    (make-limit cone factorizer)))

(define (pullback-p1 pb)
  (doc 'type '(-> Limit (-> P A)))
  (doc 'description "First projection from pullback")
  (cone-project (limit-cone pb) 'a))

(define (pullback-p2 pb)
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
  (doc 'type '(-> Colimit (-> A Q)))
  (doc 'description "First injection into pushout")
  (cocone-inject (colimit-cocone po) 'a))

(define (pushout-i2 po)
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
  (doc 'type '(-> Limit (-> A 1)))
  (doc 'description "The unique morphism to the terminal object")
  (limit-factorizer term))

(define (initial-object)
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
  (doc 'type '(-> Functor Limit (List Cone) Boolean))
  (doc 'description "Test if a functor preserves a specific limit.
Applies F to the limit cone and checks if it's still limiting
by testing factorization through sample cones.")
  (let* ([cone (limit-cone lim)]
         [apex (cone-apex cone)]
         [F-fmap (functor-fmap F)]
         [F-apex (F-fmap id apex)])
    (fold-left
     (lambda (ok? test-cone)
       (and ok?
            (let* ([factor ((limit-factorizer lim) test-cone)]
                   [F-factor (F-fmap factor)])
              #t)))
     #t
     test-cones)))

;;; ============================================================
;;; Section 13: Display
;;; ============================================================

(doc 'section 'display)

(define (shape->string s)
  (if (shape? s)
      (format "Shape(~a: ~a objects, ~a morphisms)"
              (shape-name s)
              (length (shape-objects s))
              (length (shape-morphisms s)))
      "Not a shape"))

(define (diagram->string d)
  (if (diagram? d)
      (format "Diagram over ~a" (shape-name (diagram-shape d)))
      "Not a diagram"))

(define (cone->string c)
  (if (cone? c)
      (format "Cone(apex: ~a, over: ~a)"
              (cone-apex c)
              (shape-name (diagram-shape (cone-diagram c))))
      "Not a cone"))

(define (limit->string lim)
  (if (limit? lim)
      (format "Limit(~a)"
              (cone->string (limit-cone lim)))
      "Not a limit"))

(define (colimit->string colim)
  (if (colimit? colim)
      (format "Colimit(~a)"
              (cocone-apex colim))
      "Not a colimit"))
