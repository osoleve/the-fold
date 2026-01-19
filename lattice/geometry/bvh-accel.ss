;;; lattice/geometry/bvh-accel.ss — Transparent Rust-Accelerated BVH Operations
;;;
;;; Provides BVH operations that transparently use Rust acceleration if available,
;;; with fallback to pure Scheme implementation.
;;;
;;; API matches the pure Scheme BVH operations but adds fuel tracking:
;;;   (bvh-closest-point/accel bvh point fuel) → (ok result fuel) | (suspended ...)
;;;   (bvh-intersect-ray/accel bvh ray fuel) → (ok result fuel) | (suspended ...)
;;;
;;; This is Lattice code with Shell dependencies for acceleration.

(load "lattice/geometry/bvh.ss")

;;; Try to load acceleration, but don't fail if unavailable
(define *accel-enabled* #f)
(guard (ex [else (set! *accel-enabled* #f)])
       (load "boundary/ffi/bvh-cache.ss")
       (when (accel-load!)
             (bind-bvh-procedures!)
             (set! *accel-enabled* #t)))

;;; ====
;;; Acceleration Status
;;; ====

;;; accel-enabled? : → Boolean
;;; Check if Rust acceleration is available
(define (accel-enabled?)
  *accel-enabled*)

;;; ====
;;; Fuel Cost Estimation (for Scheme fallback)
;;; ====

;;; estimate-closest-point-fuel : BVH → Nat
;;; Estimate fuel cost for closest-point query on this BVH
(define (estimate-closest-point-fuel bvh)
  (let* ([nodes (bvh-count-nodes bvh)]
         [tris (bvh-count-triangles bvh)]
         ;; Match Rust costs: BASE=5, NODE=2, AABB=3, TRI=10
         [base 5]
         [per-node (+ 2 3)]  ; node visit + aabb test
         [per-tri 10]
         ;; Estimate: visit all nodes, test log(n) triangles on average
         [avg-tris-tested (max 1 (ceiling (* (log (+ tris 1)) 2)))])
        (+ base
           (* nodes per-node)
           (* avg-tris-tested per-tri))))

;;; estimate-ray-intersect-fuel : BVH → Nat
;;; Estimate fuel cost for ray-intersect query on this BVH
(define (estimate-ray-intersect-fuel bvh)
  (let* ([nodes (bvh-count-nodes bvh)]
         [tris (bvh-count-triangles bvh)]
         ;; Match Rust costs: BASE=5, NODE=2, AABB=3, TRI_RAY=8
         [base 5]
         [per-node (+ 2 3)]
         [per-tri 8]
         [avg-tris-tested (max 1 (ceiling (* (log (+ tris 1)) 2)))])
        (+ base
           (* nodes per-node)
           (* avg-tris-tested per-tri))))

;;; ====
;;; Accelerated Operations
;;; ====

;;; bvh-closest-point/accel : BVH × Point3 × Fuel → Result
;;; Find closest point on BVH surface with fuel tracking.
;;; Returns:
;;;   (ok (Point3 Distance Triangle3) Fuel) - success
;;;   (ok #f Fuel) - miss (no triangles)
;;;   (suspended (bvh-closest-point bvh point)) - out of fuel
(define (bvh-closest-point/accel bvh point fuel)
  (if *accel-enabled*
      ;; Use Rust acceleration
      (let* ([handle (get-rust-handle bvh)]
             [result (rust-bvh-closest-point/raw handle point fuel)])
            (case (car result)
                  [(ok)
                   ;; (ok (vec3 dist) fuel-out) → (ok (vec3 dist #f) fuel-out)
                   ;; Note: Rust doesn't return the triangle, so we return #f
                   (let ([data (cadr result)]
                         [fuel-out (caddr result)])
                        `(ok (,(car data) ,(cadr data) #f) ,fuel-out))]
                  [(miss)
                   `(ok #f ,(cadr result))]
                  [(out-of-fuel)
                   `(suspended (bvh-closest-point ,bvh ,point))]
                  [else result]))
      
      ;; Fallback to pure Scheme
      (let ([estimated (estimate-closest-point-fuel bvh)])
           (if (>= fuel estimated)
               ;; Have enough fuel
               (let ([result (bvh-closest-point bvh point)])
                    `(ok ,result ,(- fuel estimated)))
               ;; Not enough fuel
               `(suspended (bvh-closest-point ,bvh ,point))))))

;;; bvh-intersect-ray/accel : BVH × Ray3 × Fuel → Result
;;; Find ray intersection with BVH with fuel tracking.
;;; Returns:
;;;   (ok (Triangle3 Distance) Fuel) - hit
;;;   (ok #f Fuel) - miss
;;;   (suspended (bvh-intersect-ray bvh ray)) - out of fuel
(define (bvh-intersect-ray/accel bvh ray fuel)
  (let ([origin (ray3-origin ray)]
        [direction (ray3-direction ray)])
       (if *accel-enabled*
           ;; Use Rust acceleration
           (let* ([handle (get-rust-handle bvh)]
                  [result (rust-bvh-intersect-ray/raw handle origin direction fuel)])
                 (case (car result)
                       [(ok)
                        ;; (ok (t normal) fuel-out) → (ok (#f t) fuel-out)
                        ;; Note: Rust returns normal, Scheme expects triangle - return #f for tri
                        (let* ([data (cadr result)]
                               [t (car data)]
                               [fuel-out (caddr result)])
                              `(ok (#f ,t) ,fuel-out))]
                       [(miss)
                        `(ok #f ,(cadr result))]
                       [(out-of-fuel)
                        `(suspended (bvh-intersect-ray ,bvh ,ray))]
                       [else result]))
           
           ;; Fallback to pure Scheme
           (let ([estimated (estimate-ray-intersect-fuel bvh)])
                (if (>= fuel estimated)
                    (let ([result (bvh-intersect-ray bvh ray)])
                         `(ok ,result ,(- fuel estimated)))
                    `(suspended (bvh-intersect-ray ,bvh ,ray)))))))

;;; ====
;;; Convenience: Auto-detect and use best implementation
;;; ====

;;; with-bvh-accel : (→ α) → α
;;; Run thunk with BVH acceleration enabled (if available)
;;; Ensures cleanup after execution
(define (with-bvh-accel thunk)
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda ()
           (when *accel-enabled*
                 (cleanup-stale-handles!)))))

;;; ====
;;; Tests
;;; ====

(define (run-accel-tests)
  (display "BVH Acceleration Tests\n")
  (display "====\n")
  
  (display (format "Acceleration enabled: ~a\n" (accel-enabled?)))
  
  ;; Build test BVH
  (let* ([triangles (list
                     (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
                     (triangle3 (vec3 1 0 0) (vec3 2 0 0) (vec3 1 1 0))
                     (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0)))]
         [bvh (bvh-build triangles 2)])
        
        ;; Test 1: Closest point with plenty of fuel
        (display "1. Closest point (fuel=1000)... ")
        (let ([result (bvh-closest-point/accel bvh (vec3 0.5 0.5 1.0) 1000)])
             (display result)
             (newline))
        
        ;; Test 2: Closest point with low fuel
        (display "2. Closest point (fuel=2)... ")
        (let ([result (bvh-closest-point/accel bvh (vec3 0.5 0.5 1.0) 2)])
             (display result)
             (newline))
        
        ;; Test 3: Ray intersection
        (display "3. Ray intersection (fuel=1000)... ")
        (let ([ray (ray3 (vec3 0.25 0.25 1.0) (vec3 0 0 -1))])
             (let ([result (bvh-intersect-ray/accel bvh ray 1000)])
                  (display result)
                  (newline)))
        
        ;; Test 4: Compare with pure Scheme
        (display "4. Pure Scheme result... ")
        (let ([scheme-result (bvh-closest-point bvh (vec3 0.5 0.5 1.0))])
             (display scheme-result)
             (newline)))
  
  (display "====\n")
  #t)
