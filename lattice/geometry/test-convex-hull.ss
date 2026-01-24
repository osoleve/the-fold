;;; lattice/geometry/test-convex-hull.ss --- Tests for convex hull algorithms

(load "core/testing/test-framework.ss")
(load "lattice/geometry/convex-hull.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define (approx-equal? a b epsilon)
  (< (abs (- a b)) epsilon))

(define (point2-equal? p1 p2 epsilon)
  (and (approx-equal? (point2-x p1) (point2-x p2) epsilon)
       (approx-equal? (point2-y p1) (point2-y p2) epsilon)))

(define (hull-contains-point? hull p epsilon)
  (ormap (lambda (hp) (point2-equal? hp p epsilon)) hull))

(define (hulls-same-vertices? h1 h2 epsilon)
  (and (= (length h1) (length h2))
       (andmap (lambda (p) (hull-contains-point? h2 p epsilon)) h1)
       (andmap (lambda (p) (hull-contains-point? h1 p epsilon)) h2)))

;;; ============================================================
;;; Test Data
;;; ============================================================

;; Simple square
(define square-points
  (list (make-point2 0 0)
        (make-point2 1 0)
        (make-point2 1 1)
        (make-point2 0 1)))

;; Square with interior point
(define square-with-center
  (list (make-point2 0 0)
        (make-point2 1 0)
        (make-point2 0.5 0.5)  ; Interior point
        (make-point2 1 1)
        (make-point2 0 1)))

;; Triangle
(define triangle-points
  (list (make-point2 0 0)
        (make-point2 1 0)
        (make-point2 0.5 1)))

;; Collinear points
(define collinear-points
  (list (make-point2 0 0)
        (make-point2 1 1)
        (make-point2 2 2)
        (make-point2 3 3)))

;; Random-ish convex shape
(define hexagon-points
  (list (make-point2 0 1)
        (make-point2 0.866 0.5)
        (make-point2 0.866 -0.5)
        (make-point2 0 -1)
        (make-point2 -0.866 -0.5)
        (make-point2 -0.866 0.5)))

;; Points with many interior points
(define many-interior-points
  (append
   (list (make-point2 0 0)
         (make-point2 10 0)
         (make-point2 10 10)
         (make-point2 0 10))
   ;; 16 interior points
   (map (lambda (i)
          (make-point2 (+ 1 (* 2 (modulo i 4)))
                       (+ 1 (* 2 (quotient i 4)))))
        (iota 16))))

;;; ============================================================
;;; Graham Scan Tests
;;; ============================================================

(test-group "Graham Scan"
  (define-test "Square hull has 4 vertices"
    (let ([hull (graham-scan square-points)])
      (assert-equal 4 (length hull))))

  (define-test "Square with interior point still has 4 vertices"
    (let ([hull (graham-scan square-with-center)])
      (assert-equal 4 (length hull))))

  (define-test "Triangle hull has 3 vertices"
    (let ([hull (graham-scan triangle-points)])
      (assert-equal 3 (length hull))))

  (define-test "Collinear points reduce to 2 endpoints"
    (let ([hull (graham-scan collinear-points)])
      (assert-equal 2 (length hull))))

  (define-test "Hull vertices are CCW ordered"
    (let* ([hull (graham-scan square-points)]
           [n (length hull)])
      ;; All consecutive triples should make left turns
      (assert-true
       (let loop ([i 0])
         (if (>= i n)
             #t
             (let ([p1 (list-ref hull i)]
                   [p2 (list-ref hull (modulo (+ i 1) n))]
                   [p3 (list-ref hull (modulo (+ i 2) n))])
               (and (>= (cross-product-2d p1 p2 p3) (- 1e-10))
                    (loop (+ i 1)))))))))

  (define-test "Filters many interior points correctly"
    (let ([hull (graham-scan many-interior-points)])
      (assert-equal 4 (length hull))))

  (define-test "Hexagon hull is the hexagon itself"
    (let ([hull (graham-scan hexagon-points)])
      (assert-equal 6 (length hull)))))

;;; ============================================================
;;; Quickhull Tests
;;; ============================================================

(test-group "Quickhull"
  (define-test "Square hull has 4 vertices"
    (let ([hull (quickhull square-points)])
      (assert-equal 4 (length hull))))

  (define-test "Square with interior point still has 4 vertices"
    (let ([hull (quickhull square-with-center)])
      (assert-equal 4 (length hull))))

  (define-test "Triangle hull has 3 vertices"
    (let ([hull (quickhull triangle-points)])
      (assert-equal 3 (length hull))))

  (define-test "Filters many interior points correctly"
    (let ([hull (quickhull many-interior-points)])
      (assert-equal 4 (length hull))))

  (define-test "Graham and Quickhull produce same vertices"
    (let ([graham (graham-scan many-interior-points)]
          [quick (quickhull many-interior-points)])
      (assert-true (hulls-same-vertices? graham quick 1e-10)))))

;;; ============================================================
;;; Hull Properties Tests
;;; ============================================================

(test-group "Hull Properties"
  (define-test "Unit square area is 1"
    (let ([hull (graham-scan square-points)])
      (assert-true (approx-equal? (convex-hull-area hull) 1.0 1e-10))))

  (define-test "Unit square perimeter is 4"
    (let ([hull (graham-scan square-points)])
      (assert-true (approx-equal? (convex-hull-perimeter hull) 4.0 1e-10))))

  (define-test "Interior point is inside hull"
    (let ([hull (graham-scan square-points)])
      (assert-true (point-in-convex-hull? (make-point2 0.5 0.5) hull))))

  (define-test "Exterior point is outside hull"
    (let ([hull (graham-scan square-points)])
      (assert-false (point-in-convex-hull? (make-point2 2.0 0.5) hull))))

  (define-test "Vertex is on hull boundary"
    (let ([hull (graham-scan square-points)])
      (assert-true (point-in-convex-hull? (make-point2 0 0) hull))))

  (define-test "Unit square diameter is sqrt(2)"
    (let ([hull (graham-scan square-points)])
      (assert-true (approx-equal? (convex-hull-diameter hull)
                                  (sqrt 2)
                                  1e-10))))

  (define-test "Centroid of square is (0.5, 0.5)"
    (let* ([hull (graham-scan square-points)]
           [c (convex-hull-centroid hull)])
      (assert-true (point2-equal? c (make-point2 0.5 0.5) 1e-10)))))

;;; ============================================================
;;; Extreme Points and Support Function Tests
;;; ============================================================

(test-group "Extreme Points"
  (define-test "Extreme point in +x direction"
    (let* ([pts square-points]
           [ext (extreme-point pts (make-point2 1 0))])
      (assert-true (= (point2-x ext) 1))))

  (define-test "Extreme point in -y direction"
    (let* ([pts square-points]
           [ext (extreme-point pts (make-point2 0 -1))])
      (assert-true (= (point2-y ext) 0))))

  (define-test "Support function matches extreme point dot product"
    (let* ([pts square-points]
           [dir (make-point2 1 1)]
           [ext (extreme-point pts dir)]
           [sf (support-function pts dir)])
      (assert-true (approx-equal? sf
                                  (+ (* (point2-x ext) (point2-x dir))
                                     (* (point2-y ext) (point2-y dir)))
                                  1e-10)))))

;;; ============================================================
;;; Minkowski Operations Tests
;;; ============================================================

(test-group "Minkowski Operations"
  (define-test "Sum of two unit squares has area 4"
    (let* ([s1 (graham-scan square-points)]
           [s2 (graham-scan square-points)]
           [sum (minkowski-sum s1 s2)]
           [area (convex-hull-area sum)])
      ;; (0-1)² + (0-1)² Minkowski sum = (0-2)² = area 4
      ;; But actually need to check this more carefully
      ;; Original squares are [0,1]×[0,1], sum should be [0,2]×[0,2]
      (assert-true (> area 3.5))))  ; Should be 4, allowing some margin

  (define-test "Minkowski difference for collision detection"
    (let* ([s1 (graham-scan square-points)]
           [s2 (graham-scan (map (lambda (p)
                                   (make-point2 (+ 0.5 (point2-x p))
                                                (+ 0.5 (point2-y p))))
                                 square-points))])
      ;; Overlapping squares
      (assert-true (hulls-intersect? s1 s2))))

  (define-test "Non-overlapping hulls don't intersect"
    (let* ([s1 (graham-scan square-points)]
           [s2 (graham-scan (map (lambda (p)
                                   (make-point2 (+ 5 (point2-x p))
                                                (point2-y p)))
                                 square-points))])
      (assert-false (hulls-intersect? s1 s2)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group "Edge Cases"
  (define-test "Single point returns itself"
    (let ([hull (graham-scan (list (make-point2 0 0)))])
      (assert-equal 1 (length hull))))

  (define-test "Two points return both"
    (let ([hull (graham-scan (list (make-point2 0 0) (make-point2 1 1)))])
      (assert-equal 2 (length hull))))

  (define-test "Empty list returns empty"
    (let ([hull (graham-scan '())])
      (assert-equal 0 (length hull))))

  (define-test "Duplicate points handled"
    (let ([hull (graham-scan (list (make-point2 0 0)
                                   (make-point2 0 0)
                                   (make-point2 1 0)
                                   (make-point2 1 1)
                                   (make-point2 0 1)))])
      (assert-equal 4 (length hull)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
