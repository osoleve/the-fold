;;; @module tropical
;;; @requires prelude matrix

(require 'prelude)
(require 'matrix)

(doc 'module 'tropical)
(doc 'description "Tropical algebra: semirings, matrix operations, eigenvalues, polynomials, Newton polygons")
(doc 'layer 'lattice)
(doc 'purity 'observably-pure)

(doc 'note "Tropical semirings reveal algorithmic unity:")
(doc 'note "- Floyd-Warshall = tropical matrix closure (A* over min-plus)")
(doc 'note "- LP duality = tropical linear algebra")
(doc 'note "- Newton polygons = tropical algebraic geometry")
(doc 'note "")
(doc 'note "NOT a ring — tropical semirings lack additive inverses.")
(doc 'note "We define a lighter semiring abstraction locally.")

;;; ====
;;; Section 1: Semiring Abstraction
;;; ====

(doc 'section 'semiring-abstraction)

(doc 'note "A semiring (S, ⊕, ⊗, 0̄, 1̄) satisfies:")
(doc 'note "- (S, ⊕, 0̄) is a commutative monoid")
(doc 'note "- (S, ⊗, 1̄) is a monoid")
(doc 'note "- ⊗ distributes over ⊕")
(doc 'note "- 0̄ annihilates: a ⊗ 0̄ = 0̄ ⊗ a = 0̄")

;;; make-semiring : (a a → a) × (a a → a) × a × a × (a a → Boolean) → Semiring
(define (make-semiring add mul zero one eq?)
  (doc 'export #t)
  (list 'semiring add mul zero one eq?))

;;; semiring? : Any → Boolean
(define (semiring? x)
  (doc 'export #t)
  (and (list? x) (= (length x) 6) (eq? (car x) 'semiring)))

;;; semiring-add : Semiring → (a a → a)
(define (semiring-add sr) (list-ref sr 1))
(doc 'export #t)

;;; semiring-mul : Semiring → (a a → a)
(define (semiring-mul sr) (list-ref sr 2))
(doc 'export #t)

;;; semiring-zero : Semiring → a
(define (semiring-zero sr) (list-ref sr 3))
(doc 'export #t)

;;; semiring-one : Semiring → a
(define (semiring-one sr) (list-ref sr 4))
(doc 'export #t)

;;; semiring-eq? : Semiring → (a a → Boolean)
(define (semiring-eq? sr) (list-ref sr 5))
(doc 'export #t)

;;; ====
;;; Section 2: Standard Instances
;;; ====

(doc 'section 'standard-instances)

;;; min-plus-semiring : Semiring
;;; The tropical semiring (ℝ ∪ {+∞}, min, +, +∞, 0).
;;; This is the "standard" tropical semiring for shortest-path problems.
(define min-plus-semiring
  (make-semiring min + +inf.0 0 =))
(doc 'export #t)

;;; max-plus-semiring : Semiring
;;; The max-plus tropical semiring (ℝ ∪ {-∞}, max, +, -∞, 0).
;;; Used for longest-path, critical-path, and scheduling problems.
(define max-plus-semiring
  (make-semiring max + -inf.0 0 =))
(doc 'export #t)

;;; ====
;;; Section 3: Scalar Operations
;;; ====

(doc 'section 'scalar-operations)

;;; tropical-add : Semiring × a × a → a
(define (tropical-add sr a b)
  (doc 'export #t)
  ((semiring-add sr) a b))

;;; tropical-mul : Semiring × a × a → a
(define (tropical-mul sr a b)
  (doc 'export #t)
  ((semiring-mul sr) a b))

;;; tropical-power : Semiring × a × Nat → a
;;; Repeated ⊗ via binary exponentiation.
(define (tropical-power sr a n)
  (doc 'export #t)
  (cond [(= n 0) (semiring-one sr)]
        [(= n 1) a]
        [(even? n)
         (let ([half (tropical-power sr a (quotient n 2))])
           (tropical-mul sr half half))]
        [else
         (tropical-mul sr a (tropical-power sr a (- n 1)))]))

;;; ====
;;; Section 4: Matrix Operations
;;; ====

(doc 'section 'matrix-operations)

;;; tropical-matrix-identity : Semiring × Nat → Matrix
;;; Identity matrix: 1̄ on diagonal, 0̄ off-diagonal.
(define (tropical-matrix-identity sr n)
  (doc 'export #t)
  (let ([data (make-vector (* n n) (semiring-zero sr))]
        [one (semiring-one sr)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! data (+ (* i n) i) one))
    (list 'matrix n n data)))

;;; tropical-matrix-mul : Semiring × Matrix × Matrix → Matrix
;;; C[i,j] = ⊕_k (A[i,k] ⊗ B[k,j])
(define (tropical-matrix-mul sr A B)
  (doc 'export #t)
  (let* ([m (matrix-rows A)]
         [p (matrix-cols A)]  ; = (matrix-rows B)
         [n (matrix-cols B)]
         [zero (semiring-zero sr)]
         [data (make-vector (* m n) zero)])
    (do ([i 0 (+ i 1)])
        ((= i m))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let loop ([k 0] [acc zero])
          (if (= k p)
              (vector-set! data (+ (* i n) j) acc)
              (loop (+ k 1)
                    (tropical-add sr acc
                                  (tropical-mul sr
                                                (matrix-ref A i k)
                                                (matrix-ref B k j))))))))
    (list 'matrix m n data)))

;;; tropical-matrix-add : Semiring × Matrix × Matrix → Matrix
;;; Entrywise ⊕.
(define (tropical-matrix-add sr A B)
  (doc 'export #t)
  (let* ([m (matrix-rows A)]
         [n (matrix-cols A)]
         [data (make-vector (* m n) (semiring-zero sr))])
    (do ([i 0 (+ i 1)])
        ((= i m))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)])
          (vector-set! data idx
                       (tropical-add sr
                                     (matrix-ref A i j)
                                     (matrix-ref B i j))))))
    (list 'matrix m n data)))

;;; tropical-matrix-power : Semiring × Matrix × Nat → Matrix
;;; A^k via repeated squaring over ⊗.
(define (tropical-matrix-power sr A k)
  (doc 'export #t)
  (let ([n (matrix-rows A)])
    (cond [(= k 0) (tropical-matrix-identity sr n)]
          [(= k 1) A]
          [(even? k)
           (let ([half (tropical-matrix-power sr A (quotient k 2))])
             (tropical-matrix-mul sr half half))]
          [else
           (tropical-matrix-mul sr A (tropical-matrix-power sr A (- k 1)))])))

;;; tropical-matrix-closure : Semiring × Matrix → Matrix
;;; A* = I ⊕ A ⊕ A² ⊕ A³ ⊕ ...
;;; Computed via Floyd-Warshall DP in O(n³).
;;;
;;; This IS the key insight: Floyd-Warshall is computing the Kleene star
;;; (reflexive-transitive closure) over a tropical semiring.
(define (tropical-matrix-closure sr A)
  (doc 'export #t)
  (let* ([n (matrix-rows A)]
         [⊕ (semiring-add sr)]
         [⊗ (semiring-mul sr)]
         [zero (semiring-zero sr)]
         [one (semiring-one sr)]
         [data (make-vector (* n n) zero)])
    ;; Initialize: D = I ⊕ A (identity absorbs into the closure)
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [a-ij (matrix-ref A i j)])
          (if (= i j)
              (vector-set! data idx (⊕ one a-ij))
              (vector-set! data idx a-ij)))))
    ;; Floyd-Warshall: D[i,j] ← D[i,j] ⊕ (D[i,k] ⊗ D[k,j])
    (do ([k 0 (+ k 1)])
        ((= k n))
      (do ([i 0 (+ i 1)])
          ((= i n))
        (do ([j 0 (+ j 1)])
            ((= j n))
          (let* ([ij (+ (* i n) j)]
                 [d-ij (vector-ref data ij)]
                 [d-ik (vector-ref data (+ (* i n) k))]
                 [d-kj (vector-ref data (+ (* k n) j))]
                 [via-k (⊗ d-ik d-kj)])
            (vector-set! data ij (⊕ d-ij via-k))))))
    (list 'matrix n n data)))

;;; ====
;;; Section 5: Eigenvalue / Critical Circuits
;;; ====

(doc 'section 'eigenvalue)

(doc 'note "The tropical eigenvalue of an n×n matrix A is the minimum")
(doc 'note "cycle mean: λ = min over all cycles C of (weight(C) / length(C)).")
(doc 'note "Computed via Karp's algorithm in O(n³).")
(doc 'note "")
(doc 'note "These functions are min-plus-specific: Karp's formula uses")
(doc 'note "conventional subtraction and division, which only make sense")
(doc 'note "when ⊗ = +. Max-plus eigenvalue (maximum cycle mean) would")
(doc 'note "need the min/max in the extraction phase swapped.")

;;; tropical-eigenvalue : Matrix → Number | #f
;;; Minimum cycle mean over the min-plus semiring.
;;; Returns #f if no cycles exist.
;;; Uses Karp's algorithm: O(n³) time, O(n²) space.
(define (tropical-eigenvalue A)
  (doc 'export #t)
  (let* ([n (matrix-rows A)]
         ;; F[k,j] = min-plus cost of shortest k-step walk from virtual source to j.
         ;; Virtual source has zero-cost edges to all nodes.
         ;;
         ;; Karp's formula: λ = min_j max_{0≤k<n} (F[n,j] - F[k,j]) / (n - k)
         ;;
         ;; We compute F as a (n+1) × n table.
         [F (make-vector (* (+ n 1) n) +inf.0)])
    ;; F[0,j] = 0 for all j — virtual source
    (do ([j 0 (+ j 1)])
        ((= j n))
      (vector-set! F j 0))
    ;; F[k+1,j] = min_i (F[k,i] + A[i,j])
    (do ([k 0 (+ k 1)])
        ((= k n))
      (let ([k-off (* k n)]
            [k1-off (* (+ k 1) n)])
        (do ([j 0 (+ j 1)])
            ((= j n))
          (let loop ([i 0] [acc +inf.0])
            (if (= i n)
                (vector-set! F (+ k1-off j) acc)
                (loop (+ i 1)
                      (min acc (+ (vector-ref F (+ k-off i))
                                  (matrix-ref A i j)))))))))
    ;; Karp extraction: λ = min_j max_{0≤k<n} (F[n,j] - F[k,j]) / (n - k)
    (let ([n-off (* n n)])
      (let loop-j ([j 0] [best #f])
        (if (= j n)
            best
            (let ([fn-j (vector-ref F (+ n-off j))])
              (if (not (finite? fn-j))
                  (loop-j (+ j 1) best)
                  ;; Inner: max over k of (F[n,j] - F[k,j]) / (n - k)
                  (let loop-k ([k 0] [worst #f])
                    (if (= k n)
                        (loop-j (+ j 1)
                                (if worst
                                    (if best (min best worst) worst)
                                    best))
                        (let ([fk-j (vector-ref F (+ (* k n) j))])
                          (if (finite? fk-j)
                              (let ([ratio (/ (- fn-j fk-j) (- n k))])
                                (loop-k (+ k 1)
                                        (if worst (max worst ratio) ratio)))
                              (loop-k (+ k 1) worst))))))))))))

;;; tropical-critical-edges : Matrix × Number → List
;;; Edges (i, j) on critical circuits (those achieving the eigenvalue).
;;; Min-plus-specific: uses conventional arithmetic on edge weights.
;;;
;;; Algorithm: subtract eigenval from every finite edge weight to get
;;; the reduced matrix B. Compute B's closure. Edge (i,j) is critical
;;; iff B[i,j] + B*[j,i] = 0 (the cycle through (i,j) in the reduced
;;; graph has zero total weight, meaning the original cycle had mean = eigenval).
(define (tropical-critical-edges A eigenval)
  (doc 'export #t)
  (let* ([n (matrix-rows A)]
         ;; Reduced matrix: B[i,j] = A[i,j] - eigenval for finite entries
         [B-data (make-vector (* n n) +inf.0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([a-ij (matrix-ref A i j)])
          (when (finite? a-ij)
            (vector-set! B-data (+ (* i n) j) (- a-ij eigenval))))))
    (let* ([B (list 'matrix n n B-data)]
           [B* (tropical-matrix-closure min-plus-semiring B)])
      ;; Edge (i,j) is critical iff B[i,j] + B*[j,i] = 0
      (let loop-i ([i 0] [result '()])
        (if (= i n)
            (reverse result)
            (let loop-j ([j 0] [result result])
              (if (= j n)
                  (loop-i (+ i 1) result)
                  (let ([b-ij (matrix-ref B i j)]
                        [d-ji (matrix-ref B* j i)])
                    (if (and (finite? b-ij) (finite? d-ji)
                             (= (+ b-ij d-ji) 0))
                        (loop-j (+ j 1) (cons (list i j) result))
                        (loop-j (+ j 1) result))))))))))

;;; ====
;;; Section 6: Tropical Polynomials
;;; ====

(doc 'section 'tropical-polynomials)

(doc 'note "A tropical polynomial ⊕_i (a_i ⊗ x^i) evaluates to a")
(doc 'note "piecewise-linear function. Its 'roots' are the breakpoints")
(doc 'note "where the maximum/minimum switches between terms — equivalently,")
(doc 'note "the slopes of the Newton polygon.")

;;; tropical-poly-eval : Semiring × (List Number) × Number → Number
;;; Evaluate ⊕_i (a_i ⊗ x^i) where coeffs = (a_0 a_1 ... a_d).
(define (tropical-poly-eval sr coeffs x)
  (doc 'export #t)
  (let ([⊕ (semiring-add sr)]
        [⊗ (semiring-mul sr)]
        [zero (semiring-zero sr)])
    (let loop ([cs coeffs] [i 0] [acc zero])
      (if (null? cs)
          acc
          (let ([term (⊗ (car cs) (tropical-power sr x i))])
            (loop (cdr cs) (+ i 1) (⊕ acc term)))))))

;;; newton-polygon : (List Number) → (List (Number . Number))
;;; Lower convex hull of points {(i, a_i)} where a_i ≠ +inf.0.
;;; Returns vertices as (x . y) pairs in increasing x order.
(define (newton-polygon coeffs)
  (doc 'export #t)
  ;; Collect finite points
  (let ([points (let loop ([cs coeffs] [i 0] [acc '()])
                  (if (null? cs)
                      (reverse acc)
                      (if (finite? (car cs))
                          (loop (cdr cs) (+ i 1) (cons (cons i (car cs)) acc))
                          (loop (cdr cs) (+ i 1) acc))))])
    (if (< (length points) 2)
        points
        ;; Graham-scan-like for lower convex hull on sorted-by-x points.
        ;; Points are already sorted by x (index i is ascending).
        (let loop ([pts (cdr points)]
                   [hull (list (car points))])
          (if (null? pts)
              (reverse hull)
              (let ([p (car pts)])
                (loop (cdr pts)
                      (trim-hull hull p))))))))

;;; trim-hull : (List Point) × Point → (List Point)
;;; Remove points from the top of the hull stack that would create
;;; a non-left-turn (for lower convex hull: remove if not turning right).
(define (trim-hull hull p)
  (if (< (length hull) 2)
      (cons p hull)
      (let* ([b (car hull)]
             [a (cadr hull)]
             ;; Cross product: (b-a) × (p-a)
             ;; For lower hull, we remove b if the turn is not clockwise,
             ;; i.e., if (b.x-a.x)(p.y-a.y) - (b.y-a.y)(p.x-a.x) >= 0
             [cross (- (* (- (car b) (car a)) (- (cdr p) (cdr a)))
                       (* (- (cdr b) (cdr a)) (- (car p) (car a))))])
        (if (<= cross 0)
            ;; Clockwise or collinear — remove b (not on lower hull)
            (trim-hull (cdr hull) p)
            ;; Counterclockwise — b is below the line, keep it
            (cons p hull)))))

;;; tropical-poly-roots : (List Number) → (List Number)
;;; Breakpoints of the piecewise-linear function defined by a min-plus
;;; tropical polynomial. These are the slopes of the Newton polygon edges.
;;; Coefficients are (a_0 a_1 ... a_d).
(define (tropical-poly-roots coeffs)
  (doc 'export #t)
  (let ([hull (newton-polygon coeffs)])
    (if (< (length hull) 2)
        '()
        (let loop ([pts (cdr hull)] [prev (car hull)] [acc '()])
          (if (null? pts)
              (reverse acc)
              (let* ([cur (car pts)]
                     ;; Slope = (y2 - y1) / (x2 - x1)
                     [slope (/ (- (cdr cur) (cdr prev))
                               (- (car cur) (car prev)))])
                (loop (cdr pts) cur (cons slope acc))))))))

;;; ====
;;; Section 7: Conversion Utilities
;;; ====

(doc 'section 'conversion-utilities)

(doc 'note "Bridge between graph-matrix (0 = no edge, *infinity* = 1e308)")
(doc 'note "and tropical algebra (IEEE +inf.0 for additive identity).")

;;; adjacency->tropical : Semiring × Matrix → Matrix
;;; Convert graph adjacency matrix to tropical form.
;;; In graph-matrix: 0 means no edge (except diagonal).
;;; In tropical: no edge → +inf.0 (the ⊕-identity for min-plus).
(define (adjacency->tropical sr adj)
  (doc 'export #t)
  (let* ([n (matrix-rows adj)]
         [zero (semiring-zero sr)]
         [data (make-vector (* n n) zero)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [val (matrix-ref adj i j)])
          (cond
           ;; Diagonal: preserve (typically 0 = self-loop cost)
           [(= i j) (vector-set! data idx val)]
           ;; Zero means no edge → tropical zero (additive identity)
           [(and (number? val) (zero? val))
            (vector-set! data idx zero)]
           ;; The graph-matrix sentinel 1e308 → proper tropical zero
           [(and (number? val) (>= val 1e308))
            (vector-set! data idx zero)]
           ;; Actual weight → preserve
           [else (vector-set! data idx val)]))))
    (list 'matrix n n data)))

;;; tropical->adjacency : Matrix → Matrix
;;; Convert tropical distance matrix back to graph adjacency form.
;;; +inf.0 → 0 (no edge).
(define (tropical->adjacency dist)
  (doc 'export #t)
  (let* ([n (matrix-rows dist)]
         [data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [val (matrix-ref dist i j)])
          (if (and (number? val) (finite? val))
              (vector-set! data idx val)
              (vector-set! data idx 0)))))
    (list 'matrix n n data)))
