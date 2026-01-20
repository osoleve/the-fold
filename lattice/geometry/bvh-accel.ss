(load "lattice/geometry/bvh.ss")

(doc 'module 'bvh-accel)
(doc 'description "Transparent Rust-accelerated BVH operations with fallback to pure Scheme")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'provides "BVH operations that transparently use Rust acceleration if available; API matches pure Scheme BVH but adds fuel tracking")
(doc 'note "Functions: (bvh-closest-point/accel bvh point fuel) → (ok result fuel) | (suspended ...), (bvh-intersect-ray/accel bvh ray fuel)")

(doc 'note "Try to load acceleration, but don't fail if unavailable")
(define *accel-enabled* #f)
(guard (ex [else (set! *accel-enabled* #f)])
       (load "boundary/ffi/bvh-cache.ss")
       (when (accel-load!)
             (bind-bvh-procedures!)
             (set! *accel-enabled* #t)))

(doc 'section 'acceleration-status)

(define (accel-enabled?)
  (doc 'type '(-> Boolean))
  (doc 'description "Check if Rust acceleration is available")
  *accel-enabled*)

(doc 'section 'fuel-cost-estimation)
(doc 'note "For Scheme fallback")

(define (estimate-closest-point-fuel bvh)
  (doc 'type '(-> BVH Nat))
  (doc 'description "Estimate fuel cost for closest-point query on this BVH")
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

(define (estimate-ray-intersect-fuel bvh)
  (doc 'type '(-> BVH Nat))
  (doc 'description "Estimate fuel cost for ray-intersect query on this BVH")
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

(doc 'section 'accelerated-operations)

(define (bvh-closest-point/accel bvh point fuel)
  (doc 'type '(-> BVH Point3 Fuel Result))
  (doc 'description "Find closest point on BVH surface with fuel tracking")
  (doc 'returns "(ok (Point3 Distance Triangle3) Fuel) | (ok #f Fuel) | (suspended ...)")
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

(define (bvh-intersect-ray/accel bvh ray fuel)
  (doc 'type '(-> BVH Ray3 Fuel Result))
  (doc 'description "Find ray intersection with BVH with fuel tracking")
  (doc 'returns "(ok (Triangle3 Distance) Fuel) | (ok #f Fuel) | (suspended ...)")
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

(doc 'section 'convenience-wrappers)
(doc 'note "Auto-detect and use best implementation")

(define (with-bvh-accel thunk)
  (doc 'type '(-> (-> α) α))
  (doc 'description "Run thunk with BVH acceleration enabled (if available); ensures cleanup after execution")
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda ()
           (when *accel-enabled*
                 (cleanup-stale-handles!)))))

(doc 'section 'tests)

(define (run-accel-tests)
  (doc 'description "Run BVH acceleration test suite")
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
