;;; lattice/algebra/test-tropical.ss — Tests for Tropical Algebra
;;; Run with: scheme --script lattice/algebra/test-tropical.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/algebra/tropical.ss")
(load "lattice/data/graph/graph-matrix.ss")

;;; ============================================================
;;; Helper: numeric equality (exact/inexact agnostic)
;;; ============================================================

;;; assert-num= : Number × Number → void
;;; Like assert-equal but uses = instead of equal?, so 3 = 3.0.
;;; Tropical operations produce flonums due to IEEE infinity contagion
;;; in min/max, so we need this for numeric comparisons.
(define-syntax assert-num=
  (syntax-rules ()
    [(_ expected actual)
     (let ([e expected] [a actual])
       (assert-true (= e a)))]))

;;; ============================================================
;;; Section 1: Semiring Axioms
;;; ============================================================

(test-group "semiring-axioms"
  (define-test "semiring? recognizes valid semirings"
    (assert-true (semiring? min-plus-semiring))
    (assert-true (semiring? max-plus-semiring))
    (assert-false (semiring? 42))
    (assert-false (semiring? '(not a semiring))))

  (define-test "min-plus: additive identity (⊕-identity is +inf.0)"
    (assert-num= 3 (tropical-add min-plus-semiring 3 +inf.0))
    (assert-num= 3 (tropical-add min-plus-semiring +inf.0 3))
    (assert-num= +inf.0 (tropical-add min-plus-semiring +inf.0 +inf.0)))

  (define-test "min-plus: multiplicative identity (⊗-identity is 0)"
    (assert-num= 5 (tropical-mul min-plus-semiring 5 0))
    (assert-num= 5 (tropical-mul min-plus-semiring 0 5)))

  (define-test "min-plus: associativity of ⊕"
    (let ([sr min-plus-semiring])
      (assert-num= (tropical-add sr (tropical-add sr 1 2) 3)
                    (tropical-add sr 1 (tropical-add sr 2 3)))))

  (define-test "min-plus: commutativity of ⊕"
    (let ([sr min-plus-semiring])
      (assert-num= (tropical-add sr 2 5)
                    (tropical-add sr 5 2))))

  (define-test "min-plus: associativity of ⊗"
    (let ([sr min-plus-semiring])
      (assert-num= (tropical-mul sr (tropical-mul sr 1 2) 3)
                    (tropical-mul sr 1 (tropical-mul sr 2 3)))))

  (define-test "min-plus: annihilation (0̄ ⊗ a = 0̄)"
    (let ([sr min-plus-semiring])
      (assert-num= +inf.0 (tropical-mul sr +inf.0 5))
      (assert-num= +inf.0 (tropical-mul sr 5 +inf.0))))

  (define-test "min-plus: distributivity (a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c))"
    (let ([sr min-plus-semiring])
      ;; a ⊗ (b ⊕ c) = 2 + min(3, 5) = 2 + 3 = 5
      ;; (a ⊗ b) ⊕ (a ⊗ c) = min(2+3, 2+5) = min(5, 7) = 5
      (assert-num= (tropical-mul sr 2 (tropical-add sr 3 5))
                    (tropical-add sr (tropical-mul sr 2 3)
                                     (tropical-mul sr 2 5)))))

  (define-test "max-plus: additive identity (⊕-identity is -inf.0)"
    (assert-num= 3 (tropical-add max-plus-semiring 3 -inf.0))
    (assert-num= 3 (tropical-add max-plus-semiring -inf.0 3)))

  (define-test "max-plus: annihilation"
    (assert-num= -inf.0 (tropical-mul max-plus-semiring -inf.0 5))
    (assert-num= -inf.0 (tropical-mul max-plus-semiring 5 -inf.0)))

  (define-test "max-plus: distributivity"
    (let ([sr max-plus-semiring])
      ;; a ⊗ (b ⊕ c) = 2 + max(3, 5) = 2 + 5 = 7
      ;; (a ⊗ b) ⊕ (a ⊗ c) = max(2+3, 2+5) = max(5, 7) = 7
      (assert-num= (tropical-mul sr 2 (tropical-add sr 3 5))
                    (tropical-add sr (tropical-mul sr 2 3)
                                     (tropical-mul sr 2 5))))))

;;; ============================================================
;;; Section 2: Scalar Power
;;; ============================================================

(test-group "tropical-power"
  (define-test "power 0 returns one"
    (assert-equal 0 (tropical-power min-plus-semiring 7 0)))

  (define-test "power 1 returns the element"
    (assert-equal 7 (tropical-power min-plus-semiring 7 1)))

  (define-test "power n computes repeated ⊗ (addition)"
    ;; In min-plus: x^3 = x + x + x = 3x
    (assert-equal 21 (tropical-power min-plus-semiring 7 3))
    (assert-equal 15 (tropical-power min-plus-semiring 5 3))))

;;; ============================================================
;;; Section 3: Tropical Matrix Multiplication
;;; ============================================================

(test-group "tropical-matrix-mul"
  (define-test "identity matrix"
    (let* ([sr min-plus-semiring]
           [I (tropical-matrix-identity sr 3)])
      ;; Diagonal = 0, off-diagonal = +inf.0
      (assert-equal 0 (matrix-ref I 0 0))
      (assert-equal 0 (matrix-ref I 1 1))
      (assert-equal +inf.0 (matrix-ref I 0 1))))

  (define-test "multiply by identity"
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((0 3 +inf.0)
                                   (+inf.0 0 2)
                                   (4 +inf.0 0)))]
           [I (tropical-matrix-identity sr 3)]
           [AI (tropical-matrix-mul sr A I)])
      ;; A ⊗ I = A (values become flonums via min with +inf.0)
      (assert-num= 0 (matrix-ref AI 0 0))
      (assert-num= 3 (matrix-ref AI 0 1))
      (assert-num= +inf.0 (matrix-ref AI 0 2))
      (assert-num= 2 (matrix-ref AI 1 2))))

  (define-test "2x2 hand-computed multiplication"
    ;; A = ((0 1) (+inf.0 0)), B = ((0 3) (2 0))
    ;; C[0,0] = min(0+0, 1+2) = min(0, 3) = 0
    ;; C[0,1] = min(0+3, 1+0) = min(3, 1) = 1
    ;; C[1,0] = min(+inf+0, 0+2) = min(+inf, 2) = 2
    ;; C[1,1] = min(+inf+3, 0+0) = min(+inf, 0) = 0
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((0 1) (+inf.0 0)))]
           [B (matrix-from-lists '((0 3) (2 0)))]
           [C (tropical-matrix-mul sr A B)])
      (assert-num= 0 (matrix-ref C 0 0))
      (assert-num= 1 (matrix-ref C 0 1))
      (assert-num= 2 (matrix-ref C 1 0))
      (assert-num= 0 (matrix-ref C 1 1)))))

;;; ============================================================
;;; Section 4: Matrix Power (k-step shortest paths)
;;; ============================================================

(test-group "tropical-matrix-power"
  (define-test "A^2 gives 2-step shortest paths"
    ;; Triangle: 0→1 (cost 2), 1→2 (cost 3), 0→2 (cost 10)
    ;; A^2[0,2] should be min(direct=10, via 1=2+3=5) = 5
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((0 2 10)
                                   (+inf.0 0 3)
                                   (+inf.0 +inf.0 0)))]
           [A2 (tropical-matrix-power sr A 2)])
      ;; A^2[0,2] = min over k of (A[0,k] + A[k,2])
      ;; k=0: 0 + 10 = 10
      ;; k=1: 2 + 3 = 5
      ;; k=2: 10 + 0 = 10
      ;; = min(10, 5, 10) = 5
      (assert-num= 5 (matrix-ref A2 0 2)))))

;;; ============================================================
;;; Section 5: Floyd-Warshall Equivalence
;;; ============================================================

(test-group "floyd-warshall-equivalence"
  (define-test "tropical closure matches floyd-warshall on 4-node graph"
    ;; Build a weighted digraph:
    ;; 0→1: 3, 0→2: 8, 1→2: 2, 1→3: 5, 2→3: 1
    ;; Adjacency (graph-matrix style): 0 = no edge
    (let* ([adj-graph (matrix-from-lists '((0 3 8 0)
                                           (0 0 2 5)
                                           (0 0 0 1)
                                           (0 0 0 0)))]
           ;; Floyd-Warshall from graph-matrix
           [fw-result (floyd-warshall adj-graph)]
           ;; Convert to tropical and compute closure
           [sr min-plus-semiring]
           [adj-trop (adjacency->tropical sr adj-graph)]
           [tc-result (tropical-matrix-closure sr adj-trop)]
           [n 4])
      ;; Compare all pairs
      (do ([i 0 (+ i 1)])
          ((= i n))
        (do ([j 0 (+ j 1)])
            ((= j n))
          (let ([fw-val (matrix-ref fw-result i j)]
                [tc-val (matrix-ref tc-result i j)])
            ;; Convert: graph-matrix uses 1e308 for infinity, tropical uses +inf.0
            (let ([fw-norm (if (>= fw-val 1e308) +inf.0 fw-val)]
                  [tc-norm tc-val])
              (assert-true (or (and (not (finite? fw-norm)) (not (finite? tc-norm)))
                               (and (finite? fw-norm) (finite? tc-norm)
                                    (= fw-norm tc-norm))))))))))

  (define-test "closure gives correct shortest paths"
    ;; Known shortest paths for the graph above:
    ;; 0→0: 0, 0→1: 3, 0→2: 5 (via 1), 0→3: 6 (via 1→2→3)
    (let* ([sr min-plus-semiring]
           [adj (adjacency->tropical sr
                  (matrix-from-lists '((0 3 8 0)
                                       (0 0 2 5)
                                       (0 0 0 1)
                                       (0 0 0 0))))]
           [dist (tropical-matrix-closure sr adj)])
      (assert-num= 0 (matrix-ref dist 0 0))
      (assert-num= 3 (matrix-ref dist 0 1))
      (assert-num= 5 (matrix-ref dist 0 2))
      (assert-num= 6 (matrix-ref dist 0 3)))))

;;; ============================================================
;;; Section 6: Tropical Eigenvalue
;;; ============================================================

(test-group "tropical-eigenvalue"
  (define-test "eigenvalue of 2-node cycle"
    ;; 0→1: 4, 1→0: 6. One cycle with weight 10, length 2.
    ;; Cycle mean = 10/2 = 5.
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((+inf.0 4)
                                   (6 +inf.0)))]
           [ev (tropical-eigenvalue sr A)])
      (assert-num= 5 ev)))

  (define-test "eigenvalue of 3-node cycle"
    ;; 0→1: 1, 1→2: 2, 2→0: 3. Cycle weight = 6, length = 3.
    ;; Mean = 2.
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((+inf.0 1 +inf.0)
                                   (+inf.0 +inf.0 2)
                                   (3 +inf.0 +inf.0)))]
           [ev (tropical-eigenvalue sr A)])
      (assert-num= 2 ev)))

  (define-test "eigenvalue with competing cycles"
    ;; Two cycles: 0→1→0 with weight 3+5=8 (mean 4)
    ;;             0→1→2→0 with weight 3+1+2=6 (mean 2)
    ;; Min cycle mean = 2.
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((+inf.0 3 +inf.0)
                                   (5 +inf.0 1)
                                   (2 +inf.0 +inf.0)))]
           [ev (tropical-eigenvalue sr A)])
      (assert-num= 2 ev)))

  (define-test "eigenvalue of DAG is #f (no cycles)"
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((+inf.0 1 +inf.0)
                                   (+inf.0 +inf.0 2)
                                   (+inf.0 +inf.0 +inf.0)))]
           [ev (tropical-eigenvalue sr A)])
      (assert-false ev))))

;;; ============================================================
;;; Section 7: Newton Polygon and Tropical Polynomial Roots
;;; ============================================================

(test-group "newton-polygon"
  (define-test "linear polynomial has single vertex pair"
    ;; coeffs (a_0 a_1) = (3 1) → points (0,3) and (1,1)
    ;; Lower hull = both points
    (let ([hull (newton-polygon '(3 1))])
      (assert-equal 2 (length hull))
      (assert-equal '(0 . 3) (car hull))
      (assert-equal '(1 . 1) (cadr hull))))

  (define-test "quadratic: three collinear points collapse"
    ;; coeffs (6 4 2) → points (0,6), (1,4), (2,2)
    ;; These are collinear (slope -2). Lower hull = endpoints only.
    (let ([hull (newton-polygon '(6 4 2))])
      (assert-equal 2 (length hull))
      (assert-equal '(0 . 6) (car hull))
      (assert-equal '(2 . 2) (cadr hull))))

  (define-test "convex polygon with 3 vertices"
    ;; coeffs (10 2 8) → points (0,10), (1,2), (2,8)
    ;; (1,2) is below the line from (0,10) to (2,8), so all three are on the hull.
    (let ([hull (newton-polygon '(10 2 8))])
      (assert-equal 3 (length hull))
      (assert-equal '(0 . 10) (car hull))
      (assert-equal '(1 . 2) (cadr hull))
      (assert-equal '(2 . 8) (caddr hull))))

  (define-test "skip infinite coefficients"
    ;; coeffs (3 +inf.0 1) → points (0,3), (2,1). Index 1 skipped.
    (let ([hull (newton-polygon '(3 +inf.0 1))])
      (assert-equal 2 (length hull))
      (assert-equal '(0 . 3) (car hull))
      (assert-equal '(2 . 1) (cadr hull)))))

(test-group "tropical-poly-roots"
  (define-test "roots of quadratic are Newton polygon slopes"
    ;; coeffs (10 2 8) → hull: (0,10), (1,2), (2,8)
    ;; Slopes: (2-10)/(1-0) = -8, (8-2)/(2-1) = 6
    ;; Roots (breakpoints) = (-8, 6)
    (let ([roots (tropical-poly-roots '(10 2 8))])
      (assert-equal 2 (length roots))
      (assert-equal -8 (car roots))
      (assert-equal 6 (cadr roots))))

  (define-test "roots of linear polynomial"
    ;; coeffs (3 1) → slope = (1-3)/(1-0) = -2
    (let ([roots (tropical-poly-roots '(3 1))])
      (assert-equal 1 (length roots))
      (assert-equal -2 (car roots))))

  (define-test "empty roots for constant polynomial"
    (assert-equal '() (tropical-poly-roots '(5))))

  (define-test "tropical-poly-eval matches piecewise-linear"
    ;; For min-plus: ⊕_i (a_i + i*x) = min_i (a_i + i*x)
    ;; coeffs (10 2 8): min(10, 2+x, 8+2x)
    ;; At x = -8: min(10, 2+(-8), 8+(-16)) = min(10, -6, -8) = -8
    ;; At x = 0: min(10, 2, 8) = 2
    ;; At x = 6: min(10, 2+6, 8+12) = min(10, 8, 20) = 8
    (let ([sr min-plus-semiring])
      (assert-num= -8 (tropical-poly-eval sr '(10 2 8) -8))
      (assert-num= 2 (tropical-poly-eval sr '(10 2 8) 0))
      (assert-num= 8 (tropical-poly-eval sr '(10 2 8) 6)))))

;;; ============================================================
;;; Section 8: Conversion Utilities
;;; ============================================================

(test-group "conversion-utilities"
  (define-test "adjacency->tropical converts zeros to +inf.0"
    (let* ([sr min-plus-semiring]
           [adj (matrix-from-lists '((0 3 0)
                                     (0 0 2)
                                     (0 0 0)))]
           [trop (adjacency->tropical sr adj)])
      ;; Diagonal stays 0
      (assert-equal 0 (matrix-ref trop 0 0))
      ;; Edge 0→1 with weight 3 preserved
      (assert-equal 3 (matrix-ref trop 0 1))
      ;; No edge 0→2: 0 → +inf.0
      (assert-equal +inf.0 (matrix-ref trop 0 2))))

  (define-test "tropical->adjacency converts +inf.0 to 0"
    (let* ([trop (matrix-from-lists '((0 3 +inf.0)
                                      (+inf.0 0 2)
                                      (+inf.0 +inf.0 0)))]
           [adj (tropical->adjacency trop)])
      (assert-equal 0 (matrix-ref adj 0 0))
      (assert-equal 3 (matrix-ref adj 0 1))
      (assert-equal 0 (matrix-ref adj 0 2))))

  (define-test "round-trip: adjacency->tropical->adjacency preserves structure"
    (let* ([sr min-plus-semiring]
           [orig (matrix-from-lists '((0 5 0 0)
                                      (0 0 3 0)
                                      (0 0 0 7)
                                      (2 0 0 0)))]
           [trop (adjacency->tropical sr orig)]
           [back (tropical->adjacency trop)])
      ;; Edges preserved
      (assert-equal 5 (matrix-ref back 0 1))
      (assert-equal 3 (matrix-ref back 1 2))
      (assert-equal 7 (matrix-ref back 2 3))
      (assert-equal 2 (matrix-ref back 3 0))
      ;; Non-edges are 0
      (assert-equal 0 (matrix-ref back 0 2))
      (assert-equal 0 (matrix-ref back 1 0)))))

;;; ============================================================
;;; Section 9: Matrix Add
;;; ============================================================

(test-group "tropical-matrix-add"
  (define-test "entrywise min"
    (let* ([sr min-plus-semiring]
           [A (matrix-from-lists '((1 5) (3 +inf.0)))]
           [B (matrix-from-lists '((2 3) (+inf.0 4)))]
           [C (tropical-matrix-add sr A B)])
      (assert-num= 1 (matrix-ref C 0 0))
      (assert-num= 3 (matrix-ref C 0 1))
      (assert-num= 3 (matrix-ref C 1 0))
      (assert-num= 4 (matrix-ref C 1 1)))))

(run-all-tests-and-exit)
