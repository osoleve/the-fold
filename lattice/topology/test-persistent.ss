;;; lattice/topology/test-persistent.ss — Tests for Persistent Homology

(load "core/testing/test-framework.ss")
(load "lattice/topology/persistent.ss")

;;; ============================================================
;;; Filtration Tests
;;; ============================================================

(test-group 'filtration

  (define-test "filtration orders by birth time"
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [s01 (make-simplex '(0 1))]
           [f (make-filtration (list (cons s01 2.0)
                                     (cons s0 0.0)
                                     (cons s1 1.0)))]
           [simplices (filtration-simplices f)])
      ; Should be ordered: s0 at 0, s1 at 1, s01 at 2
      (assert-equal 3 (length simplices))
      (assert-true (simplex-equal? s0 (car simplices)))
      (assert-true (simplex-equal? s1 (cadr simplices)))
      (assert-true (simplex-equal? s01 (caddr simplices)))))

  (define-test "filtration orders dimension before coface at same time"
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [s01 (make-simplex '(0 1))]
           ; All at same birth time
           [f (make-filtration (list (cons s01 0.0)
                                     (cons s1 0.0)
                                     (cons s0 0.0)))]
           [simplices (filtration-simplices f)])
      ; Vertices should come before edge
      (assert-equal 0 (simplex-dim (car simplices)))
      (assert-equal 0 (simplex-dim (cadr simplices)))
      (assert-equal 1 (simplex-dim (caddr simplices))))))

;;; ============================================================
;;; Vietoris-Rips Construction Tests
;;; ============================================================

(test-group 'vietoris-rips

  (define-test "rips of two close points has edge"
    (let* ([pts '((0.0 0.0) (0.5 0.0))]  ; distance = 0.5
           [f (rips-filtration pts 1 1.0 euclidean-distance)]
           [simplices (filtration-simplices f)])
      ; Should have: 2 vertices + 1 edge = 3 simplices
      (assert-equal 3 (length simplices))))

  (define-test "rips of far points has no edge"
    (let* ([pts '((0.0 0.0) (5.0 0.0))]  ; distance = 5
           [f (rips-filtration pts 1 1.0 euclidean-distance)]
           [simplices (filtration-simplices f)])
      ; Should have: 2 vertices only (edge too far)
      (assert-equal 2 (length simplices))))

  (define-test "rips triangle forms at max pairwise distance"
    (let* ([pts '((0.0 0.0) (1.0 0.0) (0.5 0.866))]  ; equilateral, side ≈ 1
           [f (rips-filtration pts 2 2.0 euclidean-distance)]
           [pairs (filtration-pairs f)])
      ; Find the triangle's birth time (should be ~1.0)
      (let ([triangle-pair (find (lambda (p)
                                   (= 2 (simplex-dim (car p))))
                                 pairs)])
        (assert-true (pair? triangle-pair))
        ; Birth time should be approximately 1.0 (edge length)
        (assert-true (< (abs (- (cdr triangle-pair) 1.0)) 0.01))))))

(define (find pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) (car lst)]
    [else (find pred (cdr lst))]))

;;; ============================================================
;;; Persistence Reduction Tests
;;; ============================================================

(test-group 'persistence-reduction

  (define-test "single point has one essential H_0"
    (let* ([s0 (make-simplex '(0))]
           [f (make-filtration (list (cons s0 0.0)))]
           [result (persistence-reduce f)]
           [essential (persistence-essential result)])
      ; One essential 0-cycle (the connected component)
      (assert-equal 1 (length essential))
      (assert-equal 0 (caar essential))))  ; dimension 0

  (define-test "edge kills one component"
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [s01 (make-simplex '(0 1))]
           [f (make-filtration (list (cons s0 0.0)
                                     (cons s1 1.0)
                                     (cons s01 2.0)))]
           [result (persistence-reduce f)]
           [pairs (persistence-pairs result)]
           [essential (persistence-essential result)])
      ; The edge (s01) kills one of the two components
      (assert-equal 1 (length pairs))
      ; One essential H_0 remains (one connected component)
      (assert-true (pair? (member 0 (map car essential))))))

  (define-test "triangle boundary creates and kills cycle"
    ; Triangle: vertices at 0, edges at 1, face at 2
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [s2 (make-simplex '(2))]
           [e01 (make-simplex '(0 1))]
           [e02 (make-simplex '(0 2))]
           [e12 (make-simplex '(1 2))]
           [t (make-simplex '(0 1 2))]
           [f (make-filtration (list (cons s0 0.0) (cons s1 0.0) (cons s2 0.0)
                                     (cons e01 1.0) (cons e02 1.0) (cons e12 1.0)
                                     (cons t 2.0)))]
           [result (persistence-reduce f)]
           [diagram (make-persistence-diagram result)]
           [h1-points (diagram-points-dim diagram 1)])
      ; Should have one H_1 feature born at 1, dying at 2
      (assert-equal 1 (length h1-points))
      (let ([pt (car h1-points)])
        (assert-equal 1.0 (car pt))   ; birth
        (assert-equal 2.0 (cadr pt))))))  ; death

;;; ============================================================
;;; Persistence Diagram Tests
;;; ============================================================

(test-group 'persistence-diagram

  (define-test "betti number at time"
    ; Two points, edge at time 1
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [e (make-simplex '(0 1))]
           [f (make-filtration (list (cons s0 0.0)
                                     (cons s1 0.0)
                                     (cons e 1.0)))]
           [result (persistence-reduce f)]
           [diagram (make-persistence-diagram result)])
      ; Before edge: B_0 = 2 (two components)
      (assert-equal 2 (diagram-betti diagram 0 0.5))
      ; After edge: B_0 = 1 (one component)
      (assert-equal 1 (diagram-betti diagram 0 1.5))))

  (define-test "diagram from point cloud"
    ; Three points forming a right angle
    (let* ([pts '((0.0 0.0) (1.0 0.0) (0.0 1.0))]
           [diagram (compute-persistence pts 2 2.0)])
      ; At time 0: 3 components (B_0 = 3)
      (assert-equal 3 (diagram-betti diagram 0 0.0))
      ; Eventually: 1 component (B_0 = 1)
      (assert-equal 1 (diagram-betti diagram 0 1.5)))))

;;; ============================================================
;;; Barcode Tests
;;; ============================================================

(test-group 'barcode

  (define-test "barcode groups by dimension"
    (let* ([s0 (make-simplex '(0))]
           [s1 (make-simplex '(1))]
           [e (make-simplex '(0 1))]
           [f (make-filtration (list (cons s0 0.0)
                                     (cons s1 0.5)
                                     (cons e 1.0)))]
           [result (persistence-reduce f)]
           [diagram (make-persistence-diagram result)]
           [bc (diagram->barcode diagram)]
           [h0-intervals (barcode-intervals bc 0)])
      ; Should have H_0 intervals
      (assert-true (> (length h0-intervals) 0)))))

;;; ============================================================
;;; Known Topology Tests
;;; ============================================================

(test-group 'known-topologies

  (define-test "circle persistence: one H_0, one H_1"
    ; Four points in a square, max-dim=1 (edges only, no triangles)
    (let* ([pts '((0.0 0.0) (1.0 0.0) (1.0 1.0) (0.0 1.0))]
           [diagram (compute-persistence pts 1 2.0)])
      ; Should have exactly 1 essential H_0 (connected)
      (let ([essential-h0 (filter (lambda (p)
                                    (and (= 0 (caddr p))
                                         (infinite? (cadr p))))
                                  (diagram-points diagram))])
        (assert-equal 1 (length essential-h0)))
      ; Should have 1 H_1 feature (the cycle)
      ; (may be essential or killed depending on epsilon)
      (let ([h1-pts (diagram-points-dim diagram 1)])
        (assert-true (>= (length h1-pts) 1)))))

  (define-test "filled triangle has no H_1"
    ; Three close points, dim 2 to allow filling
    (let* ([pts '((0.0 0.0) (0.5 0.0) (0.25 0.4))]
           [diagram (compute-persistence pts 2 1.0)])
      ; Any H_1 that appears should die (be filled by triangle)
      (let ([h1-essential (filter (lambda (p)
                                    (and (= 1 (caddr p))
                                         (infinite? (cadr p))))
                                  (diagram-points diagram))])
        (assert-equal 0 (length h1-essential))))))

;;; ============================================================
;;; Distance Metric Tests
;;; ============================================================

(test-group 'distances

  (define-test "bottleneck distance is zero for identical diagrams"
    (let* ([pts '((0.0 0.0) (1.0 0.0) (0.5 0.5))]
           [d1 (compute-persistence pts 1 2.0)]
           [d2 (compute-persistence pts 1 2.0)]
           [dist (diagram-bottleneck d1 d2 0)])
      ; Identical diagrams should have distance 0
      (assert-equal 0.0 dist)))

  (define-test "bottleneck distance positive for different diagrams"
    (let* ([pts1 '((0.0 0.0) (1.0 0.0))]
           [pts2 '((0.0 0.0) (2.0 0.0))]  ; edge at different scale
           [d1 (compute-persistence pts1 1 3.0)]
           [d2 (compute-persistence pts2 1 3.0)])
      ; Edge birth times differ (1.0 vs 2.0)
      (assert-true (> (diagram-bottleneck d1 d2 0) 0)))))

;;; ============================================================
;;; High-Dimension Tests
;;; ============================================================

(test-group 'high-dimensions

  (define-test "rips supports dimension 4 (5-simplex)"
    ; 5 points in a small cluster should form a 4-simplex
    (let* ([pts '((0.0 0.0) (0.1 0.0) (0.0 0.1) (0.1 0.1) (0.05 0.05))]
           [f (rips-filtration pts 4 1.0 euclidean-distance)]
           [simplices (filtration-simplices f)]
           [dims (map simplex-dim simplices)]
           [max-dim-found (apply max dims)])
      ; Should have simplices up to dimension 4
      (assert-equal 4 max-dim-found)))

  (define-test "tetrahedra generated for 4 points"
    ; 4 points close together should form a tetrahedron (dim 3)
    (let* ([pts '((0.0 0.0 0.0) (1.0 0.0 0.0) (0.5 0.866 0.0) (0.5 0.289 0.816))]
           [f (rips-filtration pts 3 2.0 euclidean-distance)]
           [simplices (filtration-simplices f)])
      ; Should have exactly one tetrahedron (dim 3)
      (assert-equal 1 (length (filter (lambda (s) (= 3 (simplex-dim s))) simplices))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "\n=== Persistent Homology Tests ===\n\n")
(run-all-tests)
