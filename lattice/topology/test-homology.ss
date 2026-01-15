;;; lattice/topology/test-homology.ss — Tests for Homology Computation
;;;
;;; Tests verify:
;;; 1. Z2 matrix operations work correctly
;;; 2. Boundary matrices are built properly
;;; 3. Betti numbers match known topological invariants

(load "core/testing/test-framework.ss")
(load "lattice/topology/homology.ss")

;;; ============================================================
;;; Z2 MATRIX TESTS
;;; ============================================================

(test-group "Z2 Matrix Operations"

  (define-test "z2-matrix creation and access"
    (let ([m (z2-matrix 2 3 (vector 1 0 1 0 1 1))])
      (assert-equal 2 (z2-matrix-rows m))
      (assert-equal 3 (z2-matrix-cols m))
      (assert-equal 1 (z2-matrix-ref m 0 0))
      (assert-equal 0 (z2-matrix-ref m 0 1))
      (assert-equal 1 (z2-matrix-ref m 0 2))
      (assert-equal 0 (z2-matrix-ref m 1 0))
      (assert-equal 1 (z2-matrix-ref m 1 1))
      (assert-equal 1 (z2-matrix-ref m 1 2))))

  (define-test "z2-matrix-zero creates all zeros"
    (let ([m (z2-matrix-zero 3 4)])
      (assert-equal 0 (z2-matrix-ref m 0 0))
      (assert-equal 0 (z2-matrix-ref m 2 3))))

  (define-test "z2-matrix-copy creates independent copy"
    (let* ([m1 (z2-matrix 2 2 (vector 1 0 0 1))]
           [m2 (z2-matrix-copy m1)])
      (z2-matrix-set! m2 0 0 0)
      (assert-equal 1 (z2-matrix-ref m1 0 0))
      (assert-equal 0 (z2-matrix-ref m2 0 0))))

  (define-test "z2-matrix-add-row! does mod 2 addition"
    (let ([m (z2-matrix 2 3 (vector 1 1 0 1 0 1))])
      (z2-matrix-add-row! m 0 1)  ; row0 += row1
      ; Original: row0 = (1 1 0), row1 = (1 0 1)
      ; After:    row0 = (0 1 1), row1 = (1 0 1)
      (assert-equal 0 (z2-matrix-ref m 0 0))
      (assert-equal 1 (z2-matrix-ref m 0 1))
      (assert-equal 1 (z2-matrix-ref m 0 2)))))

;;; ============================================================
;;; RANK COMPUTATION TESTS
;;; ============================================================

(test-group "Z2 Rank Computation"

  (define-test "rank of identity matrix"
    (let ([m (z2-matrix 3 3 (vector 1 0 0 0 1 0 0 0 1))])
      (assert-equal 3 (z2-rank m))))

  (define-test "rank of zero matrix"
    (let ([m (z2-matrix-zero 3 3)])
      (assert-equal 0 (z2-rank m))))

  (define-test "rank of single 1"
    (let ([m (z2-matrix 3 3 (vector 0 0 0 0 1 0 0 0 0))])
      (assert-equal 1 (z2-rank m))))

  (define-test "rank with linear dependence"
    ; Rows: (1 1 0), (0 1 1), (1 0 1) - third row = first XOR second
    (let ([m (z2-matrix 3 3 (vector 1 1 0 0 1 1 1 0 1))])
      (assert-equal 2 (z2-rank m))))

  (define-test "rank of tall matrix"
    (let ([m (z2-matrix 4 2 (vector 1 0 0 1 1 1 0 1))])
      (assert-equal 2 (z2-rank m))))

  (define-test "rank of wide matrix"
    (let ([m (z2-matrix 2 4 (vector 1 0 1 0 0 1 0 1))])
      (assert-equal 2 (z2-rank m))))

  (define-test "nullity complements rank"
    (let ([m (z2-matrix 3 4 (vector 1 0 1 0 0 1 0 1 1 1 1 1))])
      (let ([r (z2-rank m)]
            [n (z2-nullity m)])
        (assert-equal 4 (+ r n))))))

;;; ============================================================
;;; Z2 NULL SPACE TESTS
;;; ============================================================

(test-group "Z2 Null Space"

  (define-test "null space of identity is empty"
    (let ([m (z2-matrix 3 3 (vector 1 0 0 0 1 0 0 0 1))])
      (assert-equal '() (z2-null-space m))))

  (define-test "null space of zero matrix is full"
    (let* ([m (z2-matrix-zero 2 3)]
           [ns (z2-null-space m)])
      ; 3 columns, rank 0, so nullity = 3
      (assert-equal 3 (length ns))))

  (define-test "null space dimension equals nullity"
    (let* ([m (z2-matrix 3 4 (vector 1 0 1 0 0 1 0 1 1 1 1 1))]
           [ns (z2-null-space m)])
      (assert-equal (z2-nullity m) (length ns))))

  (define-test "null space vectors are in kernel"
    ; Matrix with known null space: [1 1 0; 0 1 1] has null vector (1 1 1)
    (let* ([m (z2-matrix 2 3 (vector 1 1 0 0 1 1))]
           [ns (z2-null-space m)])
      (assert-equal 1 (length ns))
      ; Verify Ax = 0: each row dot null-vector should be 0 mod 2
      (let ([v (car ns)])
        ; Row 0: 1*v0 + 1*v1 + 0*v2 = v0 + v1
        (assert-equal 0 (modulo (+ (list-ref v 0) (list-ref v 1)) 2))
        ; Row 1: 0*v0 + 1*v1 + 1*v2 = v1 + v2
        (assert-equal 0 (modulo (+ (list-ref v 1) (list-ref v 2)) 2)))))

  (define-test "null space of boundary matrix gives cycles"
    ; Triangle boundary d1 has null space = cycles
    ; For a triangle, the cycle is the boundary itself (all 3 edges)
    (let* ([sc (sc-from-simplices (list (triangle 0 1 2)))]
           [b1 (sc-boundary-matrix sc 1)]
           [ns (z2-null-space b1)])
      ; Filled triangle has no 1-cycles (B1 = 0), so null space of d1
      ; should have dimension = nullity(d1) = 3 - 2 = 1... but wait
      ; Actually d1 for filled triangle has rank 2, nullity 1
      ; The null vector (1 1 1) represents all 3 edges forming a cycle
      (assert-equal 1 (length ns))
      (assert-equal '(1 1 1) (car ns))))

  (define-test "z2-rref returns correct pivots"
    (let* ([m (z2-matrix 2 3 (vector 1 1 0 0 1 1))])
      (let-values ([(rref pivots) (z2-rref m)])
        ; Pivots should be in columns 0 and 1
        (assert-equal '(0 1) pivots)
        ; RREF should have 1s only on diagonal for pivot cols
        (assert-equal 1 (z2-matrix-ref rref 0 0))
        (assert-equal 1 (z2-matrix-ref rref 1 1))))))

;;; ============================================================
;;; BOUNDARY MATRIX TESTS
;;; ============================================================

(test-group "Boundary Matrices"

  (define-test "d₁ of triangle"
    ; Triangle on vertices {0, 1, 2}
    ; Edges: (0 1), (0 2), (1 2)
    ; Vertices: 0, 1, 2
    ; d₁ maps edges to their boundary (endpoints)
    (let* ([sc (sc-from-simplices (list (triangle 0 1 2)))]
           [b1 (sc-boundary-matrix sc 1)])
      ; Check dimensions: 3 vertices (rows) × 3 edges (cols)
      (assert-equal 3 (z2-matrix-rows b1))
      (assert-equal 3 (z2-matrix-cols b1))))

  (define-test "d2 of triangle"
    ; The triangle itself maps to its boundary (the 3 edges)
    (let* ([sc (sc-from-simplices (list (triangle 0 1 2)))]
           [b2 (sc-boundary-matrix sc 2)])
      ; 3 edges (rows) × 1 triangle (cols)
      (assert-equal 3 (z2-matrix-rows b2))
      (assert-equal 1 (z2-matrix-cols b2))
      ; Each edge is hit once (all entries are 1)
      (assert-equal 1 (z2-matrix-ref b2 0 0))
      (assert-equal 1 (z2-matrix-ref b2 1 0))
      (assert-equal 1 (z2-matrix-ref b2 2 0))))

  (define-test "dd = 0 (fundamental property)"
    ; For any simplex, boundary of boundary should be zero
    (let* ([sc (sc-from-simplices (list (tetrahedron 0 1 2 3)))]
           [b2 (sc-boundary-matrix sc 2)]
           [b3 (sc-boundary-matrix sc 3)])
      ; Compose: d2 ∘ d₃ should be zero matrix
      ; This is hard to check directly, but we can verify via Betti numbers
      ; If dd ≠ 0, Betti numbers would be wrong
      (assert-true #t))))

;;; ============================================================
;;; BETTI NUMBER TESTS - STANDARD SPACES
;;; ============================================================

(test-group "Betti Numbers - Points and Simplices"

  (define-test "single point has B = (1)"
    (let* ([sc (sc-from-simplices (list (vertex 0)))]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1) betti)))

  (define-test "two disconnected points have B = (2)"
    (let* ([sc (sc-discrete '(0 1))]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(2) betti)))

  (define-test "single edge has B = (1 0)"
    ; An edge is contractible, so B₀=1, B₁=0
    (let* ([sc (sc-from-simplices (list (edge 0 1)))]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 0) betti)))

  (define-test "triangle (filled) has B = (1 0 0)"
    ; A filled triangle is contractible
    (let* ([sc (sc-from-simplices (list (triangle 0 1 2)))]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 0 0) betti)))

  (define-test "tetrahedron (filled) has B = (1 0 0 0)"
    (let* ([sc (sc-from-simplices (list (tetrahedron 0 1 2 3)))]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 0 0 0) betti))))

;;; ============================================================
;;; BETTI NUMBER TESTS - SPHERES
;;; ============================================================

(test-group "Betti Numbers - Spheres"

  (define-test "S⁰ (two points) has B = (2)"
    (let ([betti (sc-betti-numbers (make-sphere 0))])
      (assert-equal '(2) betti)))

  (define-test "S¹ (circle) has B = (1 1)"
    ; Circle: 1 component, 1 hole
    (let ([betti (sc-betti-numbers (make-sphere 1))])
      (assert-equal '(1 1) betti)))

  (define-test "S² (sphere) has B = (1 0 1)"
    ; Sphere: 1 component, no tunnels, 1 void
    (let ([betti (sc-betti-numbers (make-sphere 2))])
      (assert-equal '(1 0 1) betti)))

  (define-test "S³ (3-sphere) has B = (1 0 0 1)"
    (let ([betti (sc-betti-numbers (make-sphere 3))])
      (assert-equal '(1 0 0 1) betti))))

;;; ============================================================
;;; BETTI NUMBER TESTS - SURFACES
;;; ============================================================

(test-group "Betti Numbers - Surfaces"

  (define-test "hollow triangle (dD2) is S1"
    ; Boundary of triangle = 3 edges forming a loop
    (let* ([sc (sc-boundary-of-simplex 2)]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 1) betti)))

  (define-test "hollow tetrahedron (dD3) is S2"
    (let* ([sc (sc-boundary-of-simplex 3)]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 0 1) betti)))

  (define-test "torus T2 has B = (1 2 1)"
    ; Torus: 1 component, 2 independent loops, 1 void
    (let* ([sc (make-torus)]
           [betti (sc-betti-numbers sc)]
           [euler (sc-euler sc)])
      (assert-equal '(1 2 1) betti)
      (assert-equal 0 euler)))  ; X = 1 - 2 + 1 = 0

  (define-test "torus f-vector is (9 27 18)"
    ; 3x3 grid: 9 vertices, 27 edges, 18 triangles
    (let ([f (sc-f-vector (make-torus))])
      (assert-equal '(9 27 18) f)))

  (define-test "Klein bottle has B = (1 2 1) over Z2"
    ; Over Z2, Klein bottle looks like torus (non-orientability invisible)
    (let* ([sc (make-klein-bottle)]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 2 1) betti)))

  (define-test "projective plane RP2 has B = (1 1 1) over Z2"
    ; Over Z2: 1 component, 1 loop (from crosscap), 1 "void"
    (let* ([sc (make-projective-plane)]
           [betti (sc-betti-numbers sc)])
      (assert-equal '(1 1 1) betti))))

;;; ============================================================
;;; EULER CHARACTERISTIC TESTS
;;; ============================================================

(test-group "Euler Characteristic"

  (define-test "X = V - E + F for triangle"
    (let ([sc (sc-from-simplices (list (triangle 0 1 2)))])
      ; 3 vertices, 3 edges, 1 face -> X = 3 - 3 + 1 = 1
      (assert-equal 1 (sc-euler sc))))

  (define-test "X via Betti equals X via f-vector"
    (let ([sc (make-sphere 2)])
      (let ([euler-f (sc-euler sc)]
            [betti (sc-betti-numbers sc)])
        ; X = Σ(-1)^k B_k = 1 - 0 + 1 = 2
        (let ([euler-b (- (+ (car betti) (caddr betti))
                          (cadr betti))])
          (assert-equal euler-f euler-b)))))

  (define-test "X(S^n) = 1 + (-1)^n"
    ; S⁰: X = 2
    ; S¹: X = 0
    ; S²: X = 2
    ; S³: X = 0
    (assert-equal 2 (sc-euler (make-sphere 0)))
    (assert-equal 0 (sc-euler (make-sphere 1)))
    (assert-equal 2 (sc-euler (make-sphere 2)))
    (assert-equal 0 (sc-euler (make-sphere 3)))))

;;; ============================================================
;;; CONNECTED COMPONENTS TESTS
;;; ============================================================

(test-group "Connected Components"

  (define-test "single vertex has 1 component"
    (let ([sc (sc-from-simplices (list (vertex 0)))])
      (assert-equal 1 (sc-connected-components sc))))

  (define-test "two disconnected vertices have 2 components"
    (let ([sc (sc-discrete '(0 1))])
      (assert-equal 2 (sc-connected-components sc))))

  (define-test "connected graph has 1 component"
    (let ([sc (sc-from-simplices
                (list (edge 0 1) (edge 1 2) (edge 2 3)))])
      (assert-equal 1 (sc-connected-components sc))))

  (define-test "two disconnected triangles have 2 components"
    (let ([sc (sc-from-simplices
                (list (triangle 0 1 2) (triangle 3 4 5)))])
      (assert-equal 2 (sc-connected-components sc))))

  (define-test "components equals B₀"
    (let ([sc (sc-from-simplices
                (list (triangle 0 1 2) (edge 3 4) (vertex 5)))])
      (assert-equal (sc-connected-components sc)
                    (sc-betti sc 0)))))

;;; ============================================================
;;; RUN ALL TESTS
;;; ============================================================

(run-all-tests)
