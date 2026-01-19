;;; boundary/ffi/bench-bvh.ss — BVH Benchmark: Scheme vs Rust
;;;
;;; Benchmarks BVH operations before and after Rust acceleration.
;;; Run with: scheme --script boundary/ffi/bench-bvh.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/mesh-sdf.ss")
(load "lattice/geometry/bvh.ss")

;;; ====
;;; Test Scene: Cube mesh (12 triangles)
;;; ====

(define (make-cube-mesh size)
  (let* ([s (/ size 2.0)]
         ;; 8 vertices
         [v0 (vec3 (- s) (- s) (- s))]
         [v1 (vec3    s  (- s) (- s))]
         [v2 (vec3    s     s  (- s))]
         [v3 (vec3 (- s)    s  (- s))]
         [v4 (vec3 (- s) (- s)    s)]
         [v5 (vec3    s  (- s)    s)]
         [v6 (vec3    s     s     s)]
         [v7 (vec3 (- s)    s     s)])
        ;; 12 triangles (2 per face)
        (list
         ;; Front face
         (triangle3 v4 v5 v6) (triangle3 v4 v6 v7)
         ;; Back face
         (triangle3 v1 v0 v3) (triangle3 v1 v3 v2)
         ;; Left face
         (triangle3 v0 v4 v7) (triangle3 v0 v7 v3)
         ;; Right face
         (triangle3 v5 v1 v2) (triangle3 v5 v2 v6)
         ;; Top face
         (triangle3 v7 v6 v2) (triangle3 v7 v2 v3)
         ;; Bottom face
         (triangle3 v0 v1 v5) (triangle3 v0 v5 v4))))

;;; Larger mesh: sphere approximation (more triangles)
(define (make-sphere-mesh radius subdivisions)
  (let ([triangles '()])
       ;; Simple UV sphere
       (do ([lat 0 (+ lat 1)])
           ((>= lat subdivisions) triangles)
           (do ([lon 0 (+ lon 1)])
               ((>= lon (* 2 subdivisions)))
               (let* ([lat0 (* 3.14159 (/ lat subdivisions))]
                      [lat1 (* 3.14159 (/ (+ lat 1) subdivisions))]
                      [lon0 (* 2 3.14159 (/ lon (* 2 subdivisions)))]
                      [lon1 (* 2 3.14159 (/ (+ lon 1) (* 2 subdivisions)))]
                      [p00 (vec3 (* radius (sin lat0) (cos lon0))
                                 (* radius (cos lat0))
                                 (* radius (sin lat0) (sin lon0)))]
                      [p01 (vec3 (* radius (sin lat0) (cos lon1))
                                 (* radius (cos lat0))
                                 (* radius (sin lat0) (sin lon1)))]
                      [p10 (vec3 (* radius (sin lat1) (cos lon0))
                                 (* radius (cos lat1))
                                 (* radius (sin lat1) (sin lon0)))]
                      [p11 (vec3 (* radius (sin lat1) (cos lon1))
                                 (* radius (cos lat1))
                                 (* radius (sin lat1) (sin lon1)))])
                     (set! triangles (cons (triangle3 p00 p10 p11) triangles))
                     (set! triangles (cons (triangle3 p00 p11 p01) triangles)))))))

;;; ====
;;; Timing Utilities
;;; ====

(define (time-thunk thunk)
  (let* ([start (current-time 'time-monotonic)]
         [result (thunk)]
         [end (current-time 'time-monotonic)]
         [secs (- (time-second end) (time-second start))]
         [nsecs (- (time-nanosecond end) (time-nanosecond start))]
         [total-ms (+ (* secs 1000.0) (/ nsecs 1000000.0))])
        (cons result total-ms)))

;;; ====
;;; Benchmark: Pure Scheme BVH
;;; ====

(define (bench-scheme-closest-point bvh points iterations)
  (display "  Pure Scheme closest-point: ")
  (let ([result (time-thunk
                 (lambda ()
                         (do ([i 0 (+ i 1)])
                             ((>= i iterations))
                             (for-each (lambda (p)
                                               (bvh-closest-point bvh p))
                                       points))))])
       (display (cdr result))
       (display " ms")
       (newline)
       (cdr result)))

(define (bench-scheme-intersect-ray bvh rays iterations)
  (display "  Pure Scheme intersect-ray: ")
  (let ([result (time-thunk
                 (lambda ()
                         (do ([i 0 (+ i 1)])
                             ((>= i iterations))
                             (for-each (lambda (r)
                                               (bvh-intersect-ray bvh (ray3 (car r) (cdr r))))
                                       rays))))])
       (display (cdr result))
       (display " ms")
       (newline)
       (cdr result)))

;;; ====
;;; Test Data Generation
;;; ====

(define (random-points n range)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           acc
           (loop (+ i 1)
                 (cons (vec3 (- (* 2 range (random 1.0)) range)
                             (- (* 2 range (random 1.0)) range)
                             (- (* 2 range (random 1.0)) range))
                       acc)))))

(define (random-rays n range)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           acc
           (let* ([origin (vec3 (- (* 2 range (random 1.0)) range)
                                (- (* 2 range (random 1.0)) range)
                                (* range 2))]  ; Start above
                  [target (vec3 (- (* range (random 1.0)) (/ range 2))
                                (- (* range (random 1.0)) (/ range 2))
                                0)]
                  [dir (vec3-normalize (vec3-sub target origin))])
                 (loop (+ i 1)
                       (cons (cons origin dir) acc))))))

;;; ====
;;; Main Benchmark
;;; ====

(define (run-benchmark)
  (display "====\n")
  (display "BVH Benchmark: Pure Scheme (Baseline)\n")
  (display "====\n\n")
  
  ;; Small mesh (cube - 12 triangles)
  (display "--- Small Mesh (12 triangles) ---\n")
  (let* ([triangles (make-cube-mesh 2.0)]
         [bvh (bvh-build triangles 4)]
         [points (random-points 100 3.0)]
         [rays (random-rays 100 3.0)]
         [iterations 10])
        (display (format "  BVH built: ~a triangles\n" (length triangles)))
        (bench-scheme-closest-point bvh points iterations)
        (bench-scheme-intersect-ray bvh rays iterations))
  
  (newline)
  
  ;; Medium mesh (sphere - ~200 triangles)
  (display "--- Medium Mesh (~200 triangles) ---\n")
  (let* ([triangles (make-sphere-mesh 1.0 8)]
         [bvh (bvh-build triangles 4)]
         [points (random-points 100 2.0)]
         [rays (random-rays 100 2.0)]
         [iterations 5])
        (display (format "  BVH built: ~a triangles\n" (length triangles)))
        (bench-scheme-closest-point bvh points iterations)
        (bench-scheme-intersect-ray bvh rays iterations))
  
  (newline)
  
  ;; Larger mesh (~800 triangles)
  (display "--- Large Mesh (~800 triangles) ---\n")
  (let* ([triangles (make-sphere-mesh 1.0 16)]
         [bvh (bvh-build triangles 4)]
         [points (random-points 50 2.0)]
         [rays (random-rays 50 2.0)]
         [iterations 3])
        (display (format "  BVH built: ~a triangles\n" (length triangles)))
        (bench-scheme-closest-point bvh points iterations)
        (bench-scheme-intersect-ray bvh rays iterations))
  
  (display "\n====\n")
  (display "Benchmark complete.\n")
  (display "====\n"))

;; Run if executed as script
(run-benchmark)
