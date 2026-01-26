(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/diffgeo/charts.ss")
(load "lattice/diffgeo/tangent.ss")

(doc 'module 'forms)
(doc 'description "Differential Forms - Exterior algebra, wedge product, and exterior derivative")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A k-form is an alternating multilinear map: (TpM)^k → R")
(doc 'note "The exterior algebra Λ(T*pM) = ⊕ Λ^k(T*pM) where Λ^k is the space of k-forms")
(doc 'note "Components use multi-index notation: ω = Σ_{I} ω_I dx^I where I = (i₁ < i₂ < ... < iₖ)")

;;; ============================================================================
;;; Multi-Index Utilities
;;; ============================================================================

(doc 'section 'multi-index)
(doc 'note "Multi-indices are strictly increasing sequences (i₁ < i₂ < ... < iₖ)")
(doc 'note "For n dimensions and k-forms, there are C(n,k) basis elements")

(doc multi-index? 'type '(-> Any Boolean))
(doc multi-index? 'description "Check if x is a valid multi-index (strictly increasing list)")
(define (multi-index? x)
  (doc 'export #t)
  (and (list? x)
       (or (null? x)
           (and (for-all integer? x)
                (strictly-increasing? x)))))

(doc strictly-increasing? 'type '(-> (List Int) Boolean))
(define (strictly-increasing? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (< (car lst) (cadr lst))
           (strictly-increasing? (cdr lst)))))

(doc multi-index-degree 'type '(-> MultiIndex Nat))
(define (multi-index-degree idx)
  (doc 'export #t)
  (length idx))

(doc generate-multi-indices 'type '(-> Nat Nat (List MultiIndex)))
(doc generate-multi-indices 'description "Generate all k-element subsets of {0, 1, ..., n-1} in lexicographic order")
(define (generate-multi-indices n k)
  (doc 'export #t)
  (letrec ([gen-from
            (lambda (start remaining)
              ;; Generate k-element subsets starting from 'start'
              (cond
                [(= remaining 0) '(())]
                [(> (+ start remaining) n) '()]
                [else
                 (let loop ([s start] [acc '()])
                   (if (> s (- n remaining))
                       (reverse acc)
                       (let ([rest (gen-from (+ s 1) (- remaining 1))])
                         (loop (+ s 1)
                               (append (reverse (map (lambda (r) (cons s r)) rest))
                                       acc)))))]))])
    (gen-from 0 k)))

(doc multi-index->offset 'type '(-> Nat MultiIndex Nat))
(doc multi-index->offset 'description "Convert a multi-index to its position in the canonical ordering")
(define (multi-index->offset n idx)
  (doc 'export #t)
  (let* ([degree (length idx)]
         [indices (generate-multi-indices n degree)])
    (list-index idx indices)))

(doc offset->multi-index 'type '(-> Nat Nat Nat MultiIndex))
(doc offset->multi-index 'description "Convert an offset back to a multi-index")
(define (offset->multi-index n k offset)
  (doc 'export #t)
  (list-ref (generate-multi-indices n k) offset))

(doc list-index 'type '(-> a (List a) (Or Nat Bool)))
(define (list-index elem lst)
  (let loop ([lst lst] [i 0])
    (cond
     [(null? lst) #f]
     [(equal? (car lst) elem) i]
     [else (loop (cdr lst) (+ i 1))])))

;;; range provided by prelude

(doc binomial 'type '(-> Nat Nat Nat))
(doc binomial 'description "Binomial coefficient C(n,k)")
(define (binomial n k)
  (doc 'export #t)
  (cond
   [(or (< k 0) (> k n)) 0]
   [(or (= k 0) (= k n)) 1]
   [else
    (let loop ([i 1] [result 1])
      (if (> i k)
          result
          (loop (+ i 1)
                (/ (* result (- n (- i 1))) i))))]))

;;; ============================================================================
;;; Permutation Sign
;;; ============================================================================

(doc 'section 'permutation)
(doc 'note "The sign of a permutation is (-1)^{number of inversions}")

(doc permutation-sign 'type '(-> (List Int) Int))
(doc permutation-sign 'description "Compute the sign of a permutation: +1 for even, -1 for odd")
(define (permutation-sign perm)
  (doc 'export #t)
  (let ([inversions 0])
    (let outer ([lst perm])
      (when (not (null? lst))
        (let inner ([rest (cdr lst)])
          (when (not (null? rest))
            (when (> (car lst) (car rest))
              (set! inversions (+ inversions 1)))
            (inner (cdr rest))))
        (outer (cdr lst))))
    (if (even? inversions) 1 -1)))

(doc shuffle-sign 'type '(-> MultiIndex MultiIndex (Or Int Bool)))
(doc shuffle-sign 'description "Sign of the shuffle that interleaves two multi-indices, or #f if they overlap")
(define (shuffle-sign idx1 idx2)
  (doc 'export #t)
  ;; Check for overlap
  (if (indices-overlap? idx1 idx2)
      #f
      ;; Merge and compute sign
      (let* ([merged (merge-indices idx1 idx2)]
             [combined (append idx1 idx2)]
             [n (length combined)])
        ;; Count inversions needed to sort combined into merged
        (count-inversions-to-sort combined merged))))

(doc indices-overlap? 'type '(-> MultiIndex MultiIndex Boolean))
(define (indices-overlap? idx1 idx2)
  (let loop ([lst1 idx1] [lst2 idx2])
    (cond
     [(or (null? lst1) (null? lst2)) #f]
     [(= (car lst1) (car lst2)) #t]
     [(< (car lst1) (car lst2)) (loop (cdr lst1) lst2)]
     [else (loop lst1 (cdr lst2))])))

(doc merge-indices 'type '(-> MultiIndex MultiIndex MultiIndex))
(define (merge-indices idx1 idx2)
  (let loop ([lst1 idx1] [lst2 idx2] [acc '()])
    (cond
     [(null? lst1) (append (reverse acc) lst2)]
     [(null? lst2) (append (reverse acc) lst1)]
     [(<= (car lst1) (car lst2))
      (loop (cdr lst1) lst2 (cons (car lst1) acc))]
     [else
      (loop lst1 (cdr lst2) (cons (car lst2) acc))])))

(doc count-inversions-to-sort 'type '(-> (List Int) (List Int) Int))
(define (count-inversions-to-sort from to)
  ;; Count how many transpositions to go from 'from' to 'to'
  ;; Both lists have the same elements
  ;; Use linear search for position (fine for small k-form degrees)
  (letrec ([find-pos (lambda (x lst i)
                       (cond
                        [(null? lst) #f]
                        [(equal? x (car lst)) i]
                        [else (find-pos x (cdr lst) (+ i 1))]))])
    (let* ([mapped (map (lambda (x) (find-pos x to 0)) from)]
           [inversions 0])
      ;; Count inversions in mapped
      (let outer ([lst mapped])
        (when (not (null? lst))
          (let inner ([rest (cdr lst)])
            (when (not (null? rest))
              (when (> (car lst) (car rest))
                (set! inversions (+ inversions 1)))
              (inner (cdr rest))))
          (outer (cdr lst))))
      (if (even? inversions) 1 -1))))

;;; ============================================================================
;;; k-Form Representation
;;; ============================================================================

(doc 'section 'k-form)
(doc 'note "A k-form is: (k-form point chart degree components)")
(doc 'note "components is a vector of length C(n,k) where n = dimension")
(doc 'note "Component at index i corresponds to multi-index (offset->multi-index n k i)")

(doc k-form? 'type '(-> Any Boolean))
(define (k-form? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'k-form)
       (= (length x) 5)
       (chart? (k-form-chart x))
       (integer? (k-form-degree x))
       (>= (k-form-degree x) 0)
       (vector? (k-form-components x))))

(doc k-form-point 'type '(-> KForm Point))
(define (k-form-point kf)
  (doc 'export #t)
  (list-ref kf 1))

(doc k-form-chart 'type '(-> KForm Chart))
(define (k-form-chart kf)
  (doc 'export #t)
  (list-ref kf 2))

(doc k-form-degree 'type '(-> KForm Nat))
(define (k-form-degree kf)
  (doc 'export #t)
  (list-ref kf 3))

(doc k-form-components 'type '(-> KForm Vec))
(define (k-form-components kf)
  (doc 'export #t)
  (list-ref kf 4))

(doc k-form-dim 'type '(-> KForm Nat))
(doc k-form-dim 'description "Get the ambient dimension (from the chart)")
(define (k-form-dim kf)
  (doc 'export #t)
  (chart-dim (k-form-chart kf)))

;;; ============================================================================
;;; k-Form Construction
;;; ============================================================================

(doc make-k-form 'type '(-> Point Chart Nat Vec KForm))
(doc make-k-form 'description "Create a k-form at a point with given components")
(define (make-k-form point chart degree components)
  (doc 'export #t)
  (let* ([n (chart-dim chart)]
         [expected-size (binomial n degree)])
    (cond
     [(not (chart-contains? chart point))
      `(error point-not-in-chart ,(chart-name chart))]
     [(not (= (vector-length components) expected-size))
      `(error wrong-component-count expected ,expected-size got ,(vector-length components))]
     [else
      (list 'k-form point chart degree components)])))

(doc k-form-zero 'type '(-> Point Chart Nat KForm))
(doc k-form-zero 'description "The zero k-form at a point")
(define (k-form-zero point chart degree)
  (doc 'export #t)
  (let ([n (chart-dim chart)])
    (make-k-form point chart degree (make-vector (binomial n degree) 0))))

(doc k-form-basis 'type '(-> Point Chart MultiIndex KForm))
(doc k-form-basis 'description "Create the basis k-form dx^I for multi-index I")
(define (k-form-basis point chart idx)
  (doc 'export #t)
  (let* ([n (chart-dim chart)]
         [k (length idx)]
         [num-components (binomial n k)]
         [components (make-vector num-components 0)]
         [offset (multi-index->offset n idx)])
    (vector-set! components offset 1)
    (make-k-form point chart k components)))

;;; ============================================================================
;;; Conversion: 1-forms ↔ CotangentVector
;;; ============================================================================

(doc cotangent->1-form 'type '(-> CotangentVector KForm))
(doc cotangent->1-form 'description "Convert a cotangent vector to a 1-form")
(define (cotangent->1-form cv)
  (doc 'export #t)
  (make-k-form
   (cotangent-vector-point cv)
   (cotangent-vector-chart cv)
   1
   (cotangent-vector-components cv)))

(doc 1-form->cotangent 'type '(-> KForm CotangentVector))
(doc 1-form->cotangent 'description "Convert a 1-form to a cotangent vector")
(define (1-form->cotangent kf)
  (doc 'export #t)
  (if (not (= (k-form-degree kf) 1))
      `(error not-a-1-form degree ,(k-form-degree kf))
      (make-cotangent-vector
       (k-form-point kf)
       (k-form-chart kf)
       (k-form-components kf))))

;;; ============================================================================
;;; k-Form Arithmetic
;;; ============================================================================

(doc k-form-add 'type '(-> KForm KForm KForm))
(doc k-form-add 'description "Add two k-forms (must have same degree, point, chart)")
(define (k-form-add kf1 kf2)
  (doc 'export #t)
  (cond
   [(not (= (k-form-degree kf1) (k-form-degree kf2)))
    `(error degree-mismatch ,(k-form-degree kf1) ,(k-form-degree kf2))]
   [(not (equal? (k-form-point kf1) (k-form-point kf2)))
    `(error different-base-points)]
   [(not (eq? (chart-name (k-form-chart kf1)) (chart-name (k-form-chart kf2))))
    `(error different-charts)]
   [else
    (make-k-form
     (k-form-point kf1)
     (k-form-chart kf1)
     (k-form-degree kf1)
     (vec-add (k-form-components kf1) (k-form-components kf2)))]))

(doc k-form-sub 'type '(-> KForm KForm KForm))
(define (k-form-sub kf1 kf2)
  (doc 'export #t)
  (cond
   [(not (= (k-form-degree kf1) (k-form-degree kf2)))
    `(error degree-mismatch ,(k-form-degree kf1) ,(k-form-degree kf2))]
   [(not (equal? (k-form-point kf1) (k-form-point kf2)))
    `(error different-base-points)]
   [(not (eq? (chart-name (k-form-chart kf1)) (chart-name (k-form-chart kf2))))
    `(error different-charts)]
   [else
    (make-k-form
     (k-form-point kf1)
     (k-form-chart kf1)
     (k-form-degree kf1)
     (vec-sub (k-form-components kf1) (k-form-components kf2)))]))

(doc k-form-scale 'type '(-> Num KForm KForm))
(define (k-form-scale c kf)
  (doc 'export #t)
  (make-k-form
   (k-form-point kf)
   (k-form-chart kf)
   (k-form-degree kf)
   (vec-scale c (k-form-components kf))))

(doc k-form-negate 'type '(-> KForm KForm))
(define (k-form-negate kf)
  (doc 'export #t)
  (k-form-scale -1 kf))

;;; ============================================================================
;;; Chart Change for k-Forms
;;; ============================================================================

(doc 'section 'k-form-chart-change)
(doc 'note "k-form components transform via the k-th exterior power of the inverse Jacobian")
(doc 'note "ω'_J = Σ_I ω_I det(J⁻¹_{I,J}) where J⁻¹ is the Jacobian of transition new→old")

(doc k-form-change-chart 'type '(-> KForm Chart KForm))
(doc k-form-change-chart 'description "Express a k-form in a different chart")
(define (k-form-change-chart kf new-chart)
  (doc 'export #t)
  (let* ([point (k-form-point kf)]
         [old-chart (k-form-chart kf)]
         [k (k-form-degree kf)]
         [old-comps (k-form-components kf)])
    (cond
     [(not (chart-contains? new-chart point))
      `(error point-not-in-target-chart ,(chart-name new-chart))]
     [(eq? (chart-name old-chart) (chart-name new-chart))
      kf]  ; Same chart, no transformation needed
     [(= k 0)
      ;; 0-forms (scalars) don't transform
      (make-k-form point new-chart 0 old-comps)]
     [else
      (let* ([n (chart-dim old-chart)]
             ;; J⁻¹ = Jacobian of transition from new-chart to old-chart
             ;; This is the "inverse" because tangent vectors transform with J,
             ;; and covectors (like 1-forms) transform with J⁻¹ᵀ
             [new-coords (chart-apply new-chart point)]
             [J-inv (transition-jacobian new-chart old-chart new-coords)]
             ;; Transform components: ω'_J = Σ_I ω_I det(J⁻¹_{I,J})
             [indices-k (generate-multi-indices n k)]
             [num-k (binomial n k)]
             [new-comps (make-vector num-k 0)])
        (for-each
         (lambda (idx-J)
           (let ([offset-J (multi-index->offset n idx-J)]
                 [sum 0])
             (for-each
              (lambda (idx-I)
                (let ([omega-I (vector-ref old-comps (multi-index->offset n idx-I))])
                  (when (not (= omega-I 0))
                    ;; Build k×k submatrix J⁻¹_{I,J}: rows from I, cols from J
                    (let ([submat (make-matrix k k 0)])
                      (do ([row 0 (+ row 1)])
                          ((= row k))
                          (let ([i (list-ref idx-I row)])
                            (do ([col 0 (+ col 1)])
                                ((= col k))
                                (let ([j (list-ref idx-J col)])
                                  (matrix-set! submat row col (matrix-ref J-inv i j))))))
                      (let ([det (matrix-det-small submat)])
                        (set! sum (+ sum (* omega-I det))))))))
              indices-k)
             (vector-set! new-comps offset-J sum)))
         indices-k)
        (make-k-form point new-chart k new-comps))])))

;;; ============================================================================
;;; Wedge Product
;;; ============================================================================

(doc 'section 'wedge-product)
(doc 'note "The wedge product ∧ is an associative, bilinear operation")
(doc 'note "For α ∈ Λ^p, β ∈ Λ^q: α ∧ β ∈ Λ^{p+q}")
(doc 'note "Antisymmetry: α ∧ β = (-1)^{pq} β ∧ α")
(doc 'note "For basis forms: dx^I ∧ dx^J = sign(I,J) dx^{I∪J} if I∩J=∅, else 0")

(doc wedge 'type '(-> KForm KForm KForm))
(doc wedge 'description "Wedge product of two k-forms")
(define (wedge kf1 kf2)
  (doc 'export #t)
  (cond
   [(not (equal? (k-form-point kf1) (k-form-point kf2)))
    `(error different-base-points)]
   [(not (eq? (chart-name (k-form-chart kf1)) (chart-name (k-form-chart kf2))))
    `(error different-charts)]
   [else
    (let* ([point (k-form-point kf1)]
           [chart (k-form-chart kf1)]
           [n (chart-dim chart)]
           [p (k-form-degree kf1)]
           [q (k-form-degree kf2)]
           [r (+ p q)])
      ;; If r > n, result is zero (no (p+q)-forms on n-manifold when p+q > n)
      (if (> r n)
          (k-form-zero point chart r)
          (let* ([comp1 (k-form-components kf1)]
                 [comp2 (k-form-components kf2)]
                 [indices-p (generate-multi-indices n p)]
                 [indices-q (generate-multi-indices n q)]
                 [indices-r (generate-multi-indices n r)]
                 [num-r (binomial n r)]
                 [result (make-vector num-r 0)])
            ;; For each pair of basis forms, compute wedge and add
            (for-each
             (lambda (idx-p)
               (let ([val-p (vector-ref comp1 (multi-index->offset n idx-p))])
                 (when (not (= val-p 0))
                   (for-each
                    (lambda (idx-q)
                      (let ([val-q (vector-ref comp2 (multi-index->offset n idx-q))])
                        (when (not (= val-q 0))
                          (let ([sign (shuffle-sign idx-p idx-q)])
                            (when sign  ; sign is #f if indices overlap
                              (let* ([idx-r (merge-indices idx-p idx-q)]
                                     [offset-r (multi-index->offset n idx-r)]
                                     [contribution (* sign val-p val-q)])
                                (vector-set! result offset-r
                                             (+ (vector-ref result offset-r)
                                                contribution))))))))
                    indices-q))))
             indices-p)
            (make-k-form point chart r result))))]))

(doc wedge* 'type '(-> KForm ... KForm))
(doc wedge* 'description "Wedge product of multiple forms (left-associative)")
(define (wedge* . forms)
  (doc 'export #t)
  (if (null? forms)
      `(error no-forms-provided)
      (let loop ([result (car forms)] [rest (cdr forms)])
        (if (null? rest)
            result
            (loop (wedge result (car rest)) (cdr rest))))))

;;; ============================================================================
;;; Interior Product (Contraction)
;;; ============================================================================

(doc 'section 'interior-product)
(doc 'note "The interior product ι_v(ω) contracts a k-form with a vector")
(doc 'note "ι_v : Λ^k → Λ^{k-1}")
(doc 'note "(ι_v ω)(v₂,...,vₖ) = ω(v, v₂,...,vₖ)")

(doc interior-product 'type '(-> TangentVector KForm KForm))
(doc interior-product 'description "Contract a k-form with a tangent vector (ι_v ω)")
(define (interior-product tv kf)
  (doc 'export #t)
  (let ([k (k-form-degree kf)])
    (cond
     [(= k 0)
      (k-form-zero (k-form-point kf) (k-form-chart kf) 0)]
     [(not (equal? (tangent-vector-point tv) (k-form-point kf)))
      `(error different-base-points)]
     [else
      (let* ([point (k-form-point kf)]
             [chart (k-form-chart kf)]
             [n (chart-dim chart)]
             ;; Ensure tv is in the same chart
             [tv-in-chart (if (eq? (chart-name (tangent-vector-chart tv)) (chart-name chart))
                              tv
                              (tangent-change-chart tv chart))]
             [v-comps (tangent-vector-components tv-in-chart)]
             [kf-comps (k-form-components kf)]
             [indices-k (generate-multi-indices n k)]
             [indices-k-1 (generate-multi-indices n (- k 1))]
             [num-k-1 (binomial n (- k 1))]
             [result (make-vector num-k-1 0)])
        ;; For each basis k-form dx^I, the interior product removes one index
        ;; ι_v(dx^{i₁} ∧ ... ∧ dx^{iₖ}) = Σⱼ (-1)^{j-1} v^{iⱼ} dx^{i₁} ∧ ... ∧ dx̂^{iⱼ} ∧ ... ∧ dx^{iₖ}
        (for-each
         (lambda (idx-k)
           (let ([val (vector-ref kf-comps (multi-index->offset n idx-k))])
             (when (not (= val 0))
               ;; Remove each index in turn
               (do ([j 0 (+ j 1)])
                   ((= j k))
                   (let* ([removed-idx (list-ref idx-k j)]
                          [sign (if (even? j) 1 -1)]
                          [v-comp (vector-ref v-comps removed-idx)]
                          [remaining (remove-at idx-k j)]
                          [offset-k-1 (multi-index->offset n remaining)]
                          [contribution (* sign v-comp val)])
                     (vector-set! result offset-k-1
                                  (+ (vector-ref result offset-k-1)
                                     contribution)))))))
         indices-k)
        (make-k-form point chart (- k 1) result))])))

(doc remove-at 'type '(-> (List a) Nat (List a)))
(define (remove-at lst idx)
  (let loop ([lst lst] [i 0] [acc '()])
    (cond
     [(null? lst) (reverse acc)]
     [(= i idx) (append (reverse acc) (cdr lst))]
     [else (loop (cdr lst) (+ i 1) (cons (car lst) acc))])))

;;; ============================================================================
;;; Exterior Derivative
;;; ============================================================================

(doc 'section 'exterior-derivative)
(doc 'note "The exterior derivative d : Λ^k → Λ^{k+1}")
(doc 'note "Fundamental property: d(d(ω)) = 0 (nilpotent)")
(doc 'note "Leibniz rule: d(α ∧ β) = dα ∧ β + (-1)^{deg α} α ∧ dβ")
(doc 'note "For a k-form ω = Σ_I ω_I dx^I:")
(doc 'note "  dω = Σ_I Σⱼ (∂ω_I/∂x^j) dx^j ∧ dx^I")

(doc exterior-derivative 'type '(-> (Point → KForm) Point Chart KForm))
(doc exterior-derivative 'description "Compute the exterior derivative of a k-form field at a point")
(doc exterior-derivative 'note "omega-field is a function from points to k-forms")
(define (exterior-derivative omega-field point chart . epsilon-arg)
  (doc 'export #t)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [omega-at-p (omega-field point)]
         [k (k-form-degree omega-at-p)]
         [n (chart-dim chart)]
         [coords (chart-apply chart point)])
    ;; Special case: if k >= n, derivative is zero (no (n+1)-forms)
    (if (>= k n)
        (k-form-zero point chart (+ k 1))
        (let* ([indices-k (generate-multi-indices n k)]
               [indices-k+1 (generate-multi-indices n (+ k 1))]
               [num-k+1 (binomial n (+ k 1))]
               [result (make-vector num-k+1 0)]
               ;; Scratch for perturbed coordinates
               [coords-plus (vec-copy coords)]
               [coords-minus (vec-copy coords)])
          ;; For each coordinate direction j, compute ∂ω/∂x^j
          (do ([j 0 (+ j 1)])
              ((= j n))
              ;; Perturb in direction j
              (let ([cj (vector-ref coords j)])
                (vector-set! coords-plus j (+ cj eps))
                (vector-set! coords-minus j (- cj eps)))
              ;; Evaluate form at perturbed points and transform to target chart
              (let* ([p-plus (chart-apply-inverse chart coords-plus)]
                     [p-minus (chart-apply-inverse chart coords-minus)]
                     [omega-plus-raw (omega-field p-plus)]
                     [omega-minus-raw (omega-field p-minus)]
                     ;; Transform to target chart if needed (fixes chart mismatch)
                     [omega-plus (k-form-change-chart omega-plus-raw chart)]
                     [omega-minus (k-form-change-chart omega-minus-raw chart)]
                     [comps-plus (k-form-components omega-plus)]
                     [comps-minus (k-form-components omega-minus)])
                ;; For each basis k-form dx^I, compute ∂ω_I/∂x^j and add dx^j ∧ dx^I
                (for-each
                 (lambda (idx-I)
                   (let* ([offset-I (multi-index->offset n idx-I)]
                          [val-plus (vector-ref comps-plus offset-I)]
                          [val-minus (vector-ref comps-minus offset-I)]
                          [deriv (/ (- val-plus val-minus) (* 2 eps))])
                     (when (not (= deriv 0))
                       ;; Compute dx^j ∧ dx^I
                       ;; If j ∈ I, this is zero
                       (when (not (member j idx-I))
                         ;; Insert j into I and compute sign
                         (let* ([idx-jI (insert-sorted j idx-I)]
                                [sign (insertion-sign j idx-I)]
                                [offset-jI (multi-index->offset n idx-jI)]
                                [contribution (* sign deriv)])
                           (vector-set! result offset-jI
                                        (+ (vector-ref result offset-jI)
                                           contribution)))))))
                 indices-k))
              ;; Reset coords
              (let ([cj (vector-ref coords j)])
                (vector-set! coords-plus j cj)
                (vector-set! coords-minus j cj)))
          (make-k-form point chart (+ k 1) result)))))

(doc insert-sorted 'type '(-> Int MultiIndex MultiIndex))
(define (insert-sorted x lst)
  (cond
   [(null? lst) (list x)]
   [(< x (car lst)) (cons x lst)]
   [else (cons (car lst) (insert-sorted x (cdr lst)))]))

(doc insertion-sign 'type '(-> Int MultiIndex Int))
(doc insertion-sign 'description "Sign from inserting j into sorted list I: (-1)^{position}")
(define (insertion-sign j idx)
  (let loop ([lst idx] [pos 0])
    (cond
     [(null? lst) (if (even? pos) 1 -1)]
     [(< j (car lst)) (if (even? pos) 1 -1)]
     [else (loop (cdr lst) (+ pos 1))])))

;;; ============================================================================
;;; Scalar (0-form) Differential
;;; ============================================================================

(doc d-scalar 'type '(-> (Point → Num) Point Chart KForm))
(doc d-scalar 'description "Compute df for a scalar function f (0-form → 1-form)")
(define (d-scalar f point chart . epsilon-arg)
  (doc 'export #t)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [coords (chart-apply chart point)]
         [n (chart-dim chart)]
         [grad (make-vector n 0)]
         [coords-plus (vec-copy coords)]
         [coords-minus (vec-copy coords)])
    ;; Compute gradient
    (do ([i 0 (+ i 1)])
        ((= i n))
        (let ([ci (vector-ref coords i)])
          (vector-set! coords-plus i (+ ci eps))
          (vector-set! coords-minus i (- ci eps)))
        (let* ([p-plus (chart-apply-inverse chart coords-plus)]
               [p-minus (chart-apply-inverse chart coords-minus)]
               [deriv (/ (- (f p-plus) (f p-minus)) (* 2 eps))])
          (vector-set! grad i deriv))
        (let ([ci (vector-ref coords i)])
          (vector-set! coords-plus i ci)
          (vector-set! coords-minus i ci)))
    (make-k-form point chart 1 grad)))

;;; ============================================================================
;;; Evaluation: Apply k-form to k tangent vectors
;;; ============================================================================

(doc 'section 'evaluation)
(doc 'note "A k-form applied to k tangent vectors yields a scalar")
(doc 'note "ω(v₁, ..., vₖ) = Σ_I ω_I det([v₁,...,vₖ]_I)")

(doc k-form-apply 'type '(-> KForm (List TangentVector) Num))
(doc k-form-apply 'description "Apply a k-form to k tangent vectors")
(define (k-form-apply kf vectors)
  (doc 'export #t)
  (let ([k (k-form-degree kf)]
        [num-vecs (length vectors)])
    (cond
     [(not (= k num-vecs))
      `(error wrong-number-of-vectors expected ,k got ,num-vecs)]
     [(= k 0)
      ;; 0-form is just a scalar
      (vector-ref (k-form-components kf) 0)]
     [else
      (let* ([point (k-form-point kf)]
             [chart (k-form-chart kf)]
             [n (chart-dim chart)]
             [comps (k-form-components kf)]
             [indices (generate-multi-indices n k)]
             ;; Get vector components (transform to chart if needed)
             [vec-comps (map (lambda (tv)
                               (tangent-vector-components
                                (if (eq? (chart-name (tangent-vector-chart tv))
                                         (chart-name chart))
                                    tv
                                    (tangent-change-chart tv chart))))
                             vectors)]
             [result 0])
        ;; For each basis k-form dx^I with component ω_I,
        ;; compute ω_I * det of k×k submatrix formed by rows I
        (for-each
         (lambda (idx)
           (let ([omega-I (vector-ref comps (multi-index->offset n idx))])
             (when (not (= omega-I 0))
               ;; Build k×k matrix: row j is [v_j^{i₁}, v_j^{i₂}, ...]
               (let ([submat (make-matrix k k 0)])
                 (do ([j 0 (+ j 1)])
                     ((= j k))
                     (let ([vj (list-ref vec-comps j)])
                       (do ([col 0 (+ col 1)])
                           ((= col k))
                           (let ([row-idx (list-ref idx col)])
                             (matrix-set! submat j col (vector-ref vj row-idx))))))
                 (let ([det (matrix-det-small submat)])
                   (set! result (+ result (* omega-I det))))))))
         indices)
        result)])))

(doc matrix-det-small 'type '(-> Matrix Num))
(doc matrix-det-small 'description "Determinant for small matrices (up to 4x4)")
(define (matrix-det-small M)
  (let ([n (matrix-rows M)])
    (cond
     [(= n 1) (matrix-ref M 0 0)]
     [(= n 2)
      (- (* (matrix-ref M 0 0) (matrix-ref M 1 1))
         (* (matrix-ref M 0 1) (matrix-ref M 1 0)))]
     [(= n 3)
      (let ([a (matrix-ref M 0 0)] [b (matrix-ref M 0 1)] [c (matrix-ref M 0 2)]
            [d (matrix-ref M 1 0)] [e (matrix-ref M 1 1)] [f (matrix-ref M 1 2)]
            [g (matrix-ref M 2 0)] [h (matrix-ref M 2 1)] [i (matrix-ref M 2 2)])
        (+ (* a (- (* e i) (* f h)))
           (- (* b (- (* d i) (* f g))))
           (* c (- (* d h) (* e g)))))]
     [else
      ;; Use cofactor expansion for 4x4 and up
      (matrix-det M)])))

;;; ============================================================================
;;; Pullback of k-forms
;;; ============================================================================

(doc 'section 'pullback-forms)
(doc 'note "The pullback f* : Λ^k(N) → Λ^k(M) for f : M → N")
(doc 'note "(f*ω)(v₁,...,vₖ) = ω(f_*v₁,...,f_*vₖ)")
(doc 'note "f*(α ∧ β) = (f*α) ∧ (f*β)")

(doc pullback-k-form 'type '(-> (Vec → Vec) KForm Point Chart Chart KForm))
(doc pullback-k-form 'description "Pull back a k-form through a map f: source → target")
(define (pullback-k-form f kf source-point source-chart target-chart . epsilon-arg)
  (doc 'export #t)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [k (k-form-degree kf)])
    ;; Special case: 0-forms (scalars) pull back trivially
    (if (= k 0)
        (make-k-form source-point source-chart 0 (k-form-components kf))
        (let* ([n-source (chart-dim source-chart)]
               [n-target (chart-dim target-chart)]
               [source-coords (chart-apply source-chart source-point)]
               ;; Compute Jacobian of f
               [J (jacobian-numerical f source-coords eps)]
               ;; Pullback transforms components using exterior power of J^T
               [kf-comps (k-form-components kf)])
          (if (> k n-source)
              ;; If k > source dimension, pullback is zero
              (k-form-zero source-point source-chart k)
              (let* ([indices-k-source (generate-multi-indices n-source k)]
                     [indices-k-target (generate-multi-indices n-target k)]
                     [num-source (binomial n-source k)]
                     [result (make-vector num-source 0)])
                ;; (f*ω)_I = Σ_J ω_J det(J^T_{I,J}) = Σ_J ω_J det(J_{J,I})
                ;; where J_{J,I} is the k×k submatrix with rows J and columns I
                (for-each
                 (lambda (idx-I)
                   (let ([offset-I (multi-index->offset n-source idx-I)]
                         [sum 0])
                     (for-each
                      (lambda (idx-J)
                        (let ([omega-J (vector-ref kf-comps (multi-index->offset n-target idx-J))])
                          (when (not (= omega-J 0))
                            ;; Build k×k submatrix J_{J,I}
                            (let ([submat (make-matrix k k 0)])
                              (do ([row 0 (+ row 1)])
                                  ((= row k))
                                  (let ([j (list-ref idx-J row)])
                                    (do ([col 0 (+ col 1)])
                                        ((= col k))
                                        (let ([i (list-ref idx-I col)])
                                          (matrix-set! submat row col (matrix-ref J j i))))))
                              (let ([det (matrix-det-small submat)])
                                (set! sum (+ sum (* omega-J det))))))))
                      indices-k-target)
                     (vector-set! result offset-I sum)))
                 indices-k-source)
                (make-k-form source-point source-chart k result)))))))

;;; ============================================================================
;;; Hodge Star (for metrics)
;;; ============================================================================

(doc 'section 'hodge-star)
(doc 'note "The Hodge star ★ : Λ^k → Λ^{n-k} requires a metric")
(doc 'note "For Euclidean metric: ★(dx^I) = sign(I,Iᶜ) dx^{Iᶜ}")

(doc hodge-star-euclidean 'type '(-> KForm KForm))
(doc hodge-star-euclidean 'description "Hodge star operator with Euclidean metric")
(define (hodge-star-euclidean kf)
  (doc 'export #t)
  (let* ([point (k-form-point kf)]
         [chart (k-form-chart kf)]
         [n (chart-dim chart)]
         [k (k-form-degree kf)]
         [m (- n k)]  ; degree of result
         [comps (k-form-components kf)]
         [indices-k (generate-multi-indices n k)]
         [indices-m (generate-multi-indices n m)]
         [num-m (binomial n m)]
         [result (make-vector num-m 0)])
    ;; For each basis k-form dx^I, compute ★(dx^I) = ε_{I,Iᶜ} dx^{Iᶜ}
    (for-each
     (lambda (idx-I)
       (let ([omega-I (vector-ref comps (multi-index->offset n idx-I))])
         (when (not (= omega-I 0))
           (let* ([idx-Ic (complement-index n idx-I)]
                  [sign (levi-civita-sign n (append idx-I idx-Ic))]
                  [offset-Ic (multi-index->offset n idx-Ic)]
                  [contribution (* sign omega-I)])
             (vector-set! result offset-Ic contribution)))))
     indices-k)
    (make-k-form point chart m result)))

(doc complement-index 'type '(-> Nat MultiIndex MultiIndex))
(define (complement-index n idx)
  (let loop ([i 0] [remaining idx] [acc '()])
    (cond
     [(= i n) (reverse acc)]
     [(and (not (null? remaining)) (= i (car remaining)))
      (loop (+ i 1) (cdr remaining) acc)]
     [else
      (loop (+ i 1) remaining (cons i acc))])))

(doc levi-civita-sign 'type '(-> Nat (List Int) Int))
(define (levi-civita-sign n perm)
  (permutation-sign perm))

;;; ============================================================================
;;; Volume Form
;;; ============================================================================

(doc volume-form 'type '(-> Point Chart KForm))
(doc volume-form 'description "The volume n-form dx¹ ∧ dx² ∧ ... ∧ dxⁿ at a point")
(define (volume-form point chart)
  (doc 'export #t)
  (let* ([n (chart-dim chart)]
         [idx (range 0 n)])
    (k-form-basis point chart idx)))

;;; ============================================================================
;;; Integration (Simple Cases)
;;; ============================================================================

(doc 'section 'integration)
(doc 'note "Integration of k-forms over k-dimensional domains")
(doc 'note "∫_D ω where D is a parameterized domain")

(doc integrate-1-form-line 'type '(-> (Point → KForm) (Num → Point) Num Num Nat Num))
(doc integrate-1-form-line 'description "Integrate a 1-form along a parameterized curve")
(doc integrate-1-form-line 'param "omega-field: point → 1-form")
(doc integrate-1-form-line 'param "gamma: t → point (curve parameterization)")
(doc integrate-1-form-line 'param "t0, t1: parameter range")
(doc integrate-1-form-line 'param "steps: number of integration steps")
(define (integrate-1-form-line omega-field gamma t0 t1 steps)
  (doc 'export #t)
  (let* ([dt (/ (- t1 t0) steps)]
         [result 0])
    (do ([i 0 (+ i 1)])
        ((= i steps) result)
        (let* ([t (+ t0 (* i dt))]
               [t-mid (+ t (/ dt 2))]
               [p (gamma t-mid)]
               [omega (omega-field p)]
               ;; Compute tangent vector to curve: dγ/dt
               [p-plus (gamma (+ t-mid (* 0.5 dt)))]
               [p-minus (gamma (- t-mid (* 0.5 dt)))]
               [chart (k-form-chart omega)]
               [c-plus (chart-apply chart p-plus)]
               [c-minus (chart-apply chart p-minus)]
               [tangent-comps (vec-scale (/ 1.0 dt) (vec-sub c-plus c-minus))]
               [tangent (make-tangent-vector p chart tangent-comps)]
               ;; Evaluate ω(dγ/dt) and multiply by dt
               [integrand (k-form-apply omega (list tangent))])
          (set! result (+ result (* integrand dt)))))))

(doc integrate-2-form-surface 'type '(-> (Point → KForm) (Num Num → Point) Num Num Num Num Nat Nat Num))
(doc integrate-2-form-surface 'description "Integrate a 2-form over a parameterized surface")
(doc integrate-2-form-surface 'param "omega-field: point → 2-form")
(doc integrate-2-form-surface 'param "sigma: (u,v) → point (surface parameterization)")
(doc integrate-2-form-surface 'param "u0, u1, v0, v1: parameter ranges")
(doc integrate-2-form-surface 'param "u-steps, v-steps: integration resolution")
(doc integrate-2-form-surface 'param "epsilon: optional step size for numerical differentiation")
(define (integrate-2-form-surface omega-field sigma u0 u1 v0 v1 u-steps v-steps . epsilon-arg)
  (doc 'export #t)
  (let* ([du (/ (- u1 u0) u-steps)]
         [dv (/ (- v1 v0) v-steps)]
         [eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [result 0])
    (do ([i 0 (+ i 1)])
        ((= i u-steps) result)
        (do ([j 0 (+ j 1)])
            ((= j v-steps))
            (let* ([u (+ u0 (* i du) (/ du 2))]
                   [v (+ v0 (* j dv) (/ dv 2))]
                   [p (sigma u v)]
                   [omega (omega-field p)]
                   [chart (k-form-chart omega)]
                   ;; Compute tangent vectors ∂σ/∂u and ∂σ/∂v
                   [p-u+ (sigma (+ u eps) v)]
                   [p-u- (sigma (- u eps) v)]
                   [p-v+ (sigma u (+ v eps))]
                   [p-v- (sigma u (- v eps))]
                   [c-u+ (chart-apply chart p-u+)]
                   [c-u- (chart-apply chart p-u-)]
                   [c-v+ (chart-apply chart p-v+)]
                   [c-v- (chart-apply chart p-v-)]
                   [du-vec (vec-scale (/ 1.0 (* 2 eps)) (vec-sub c-u+ c-u-))]
                   [dv-vec (vec-scale (/ 1.0 (* 2 eps)) (vec-sub c-v+ c-v-))]
                   [tangent-u (make-tangent-vector p chart du-vec)]
                   [tangent-v (make-tangent-vector p chart dv-vec)]
                   ;; Evaluate ω(∂σ/∂u, ∂σ/∂v) and multiply by du dv
                   [integrand (k-form-apply omega (list tangent-u tangent-v))])
              (set! result (+ result (* integrand du dv))))))))

;;; ============================================================================
;;; Utility
;;; ============================================================================

(define (for-all pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (for-all pred (cdr lst))]))

(define (vec-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (vector-ref v i)))))

;;; ============================================================================
;;; REPL Interface
;;; ============================================================================

(printf "forms.ss loaded — Differential Forms and Exterior Calculus\n")
(printf "  Construction:\n")
(printf "    (make-k-form point chart degree components)\n")
(printf "    (k-form-basis point chart multi-index)  - basis form dx^I\n")
(printf "    (cotangent->1-form cv)                  - 1-form from cotangent\n")
(printf "    (k-form-change-chart kf new-chart)      - express in different chart\n")
(printf "  Wedge Product:\n")
(printf "    (wedge α β)                            - α ∧ β\n")
(printf "    (wedge* α β γ ...)                     - multiple wedge\n")
(printf "  Interior Product:\n")
(printf "    (interior-product v ω)                 - ι_v(ω)\n")
(printf "  Exterior Derivative:\n")
(printf "    (exterior-derivative ω-field p chart)  - dω at point\n")
(printf "    (d-scalar f p chart)                   - df for scalar f\n")
(printf "  Evaluation:\n")
(printf "    (k-form-apply ω (list v₁ ... vₖ))      - ω(v₁,...,vₖ)\n")
(printf "  Hodge Star:\n")
(printf "    (hodge-star-euclidean ω)               - ★ω (Euclidean)\n")
(printf "  Integration:\n")
(printf "    (integrate-1-form-line ω γ t0 t1 n)    - ∫_γ ω\n")
(printf "    (integrate-2-form-surface ω σ ...)     - ∫_σ ω\n")
