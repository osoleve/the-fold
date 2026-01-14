;;; lattice/geometry/raymarch-accel.ss — Transparent Rust-Accelerated Raymarching
;;;
;;; Provides mesh raymarching that transparently uses Rust acceleration if available,
;;; with fallback to pure Scheme implementation.
;;;
;;; API matches the pure Scheme raymarching but adds fuel tracking:
;;;   (raymarch-mesh/accel mesh ray params fuel) → (ok result fuel) | (suspended ...)
;;;
;;; This is Lattice code with Shell dependencies for acceleration.

(load "lattice/geometry/raymarch.ss")

;;; Try to load acceleration, but don't fail if unavailable
(define *raymarch-accel-enabled* #f)
(guard (ex [else (set! *raymarch-accel-enabled* #f)])
       (load "shell/ffi/raymarch-ffi.ss")
       (load "shell/ffi/bvh-cache.ss")
       (when (accel-available?)
             (bind-bvh-procedures!)
             (bind-raymarch-procedures!)
             (set! *raymarch-accel-enabled* #t)))

;;; ====
;;; Acceleration Status
;;; ====

;;; raymarch-accel-enabled? : → Boolean
;;; Check if Rust raymarching acceleration is available
(define (raymarch-accel-enabled?)
  *raymarch-accel-enabled*)

;;; ====
;;; Fuel Cost Estimation (for Scheme fallback)
;;; ====

;;; estimate-raymarch-fuel : Mesh × RaymarchParams → Nat
;;; Estimate fuel cost for raymarching on this mesh
(define (estimate-raymarch-fuel mesh params)
  (let* ([bvh (mesh-bvh mesh)]
         [nodes (bvh-count-nodes bvh)]
         [tris (bvh-count-triangles bvh)]
         [max-steps (raymarch-params-max-steps params)]
         ;; Estimate per-BVH-query cost
         [base 5]
         [per-node (+ 2 3)]  ; node visit + aabb test
         [per-tri 10]
         [avg-tris-tested (max 1 (ceiling (* (log (+ tris 1)) 2)))]
         [query-cost (+ base (* nodes per-node) (* avg-tris-tested per-tri))]
         ;; Raymarching: base + (avg_steps * query_cost) + normal_cost
         [avg-steps (/ max-steps 2)]  ; assume ~half of max steps
         [normal-cost (* 6 query-cost)])  ; 6 SDF queries for gradient
        (+ 10 ; base
           (* avg-steps query-cost)
           normal-cost)))

;;; ====
;;; Accelerated Operations
;;; ====

;;; raymarch-mesh/accel : Mesh × Ray3 × RaymarchParams × Fuel → Result
;;; Raymarching with fuel tracking.
;;; Returns:
;;;   (ok (hit-point normal t steps tri-idx) fuel) - hit
;;;   (miss steps fuel) - no intersection
;;;   (suspended (raymarch-mesh mesh ray params)) - out of fuel
(define (raymarch-mesh/accel mesh ray params fuel)
  (if *raymarch-accel-enabled*
      ;; Use Rust acceleration
      (let* ([bvh (mesh-bvh mesh)]
             [handle (get-rust-handle bvh)]
             [origin (ray3-origin ray)]
             [direction (ray3-direction ray)]
             [max-steps (raymarch-params-max-steps params)]
             [max-dist (raymarch-params-max-distance params)]
             [threshold (raymarch-params-hit-threshold params)]
             [result (rust-raymarch-mesh/raw handle origin direction
                                             max-steps max-dist threshold fuel)])
            (case (car result)
                  [(ok)
                   ;; (ok (hit-point normal t steps tri-idx) fuel-out)
                   (let* ([data (cadr result)]
                          [fuel-out (caddr result)]
                          [hit-point (car data)]
                          [normal (cadr data)]
                          [t (caddr data)]
                          [steps (cadddr data)]
                          [tri-idx (car (cddddr data))])
                         `(ok (,hit-point ,normal ,t ,steps ,tri-idx) ,fuel-out))]
                  [(miss)
                   ;; (miss steps fuel-out)
                   (let ([steps (cadr result)]
                         [fuel-out (caddr result)])
                        `(miss ,steps ,fuel-out))]
                  [(out-of-fuel)
                   ;; (out-of-fuel steps)
                   `(suspended (raymarch-mesh ,mesh ,ray ,params))]
                  [else result]))
      
      ;; Fallback to pure Scheme
      (let ([estimated (estimate-raymarch-fuel mesh params)])
           (if (>= fuel estimated)
               ;; Have enough fuel
               (let ([result (raymarch-mesh mesh ray params)])
                    (if result
                        ;; Hit: (hit-point t steps)
                        (let ([hit-point (car result)]
                              [t (cadr result)]
                              [steps (caddr result)])
                             ;; Compute normal using Scheme fallback
                             (let* ([sdf-fn (lambda (p) (mesh-sdf mesh p))]
                                    [normal (sdf-normal sdf-fn hit-point)])
                                   `(ok (,hit-point ,normal ,t ,steps 0) ,(- fuel estimated))))
                        ;; Miss
                        `(miss 0 ,(- fuel estimated))))
               ;; Not enough fuel
               `(suspended (raymarch-mesh ,mesh ,ray ,params))))))

;;; mesh-sdf-normal/accel : Mesh × Vec3 × Fuel → Result
;;; Compute mesh SDF normal with fuel tracking.
;;; Returns:
;;;   (ok normal fuel) - success
;;;   (suspended (mesh-sdf-gradient mesh point)) - out of fuel
(define (mesh-sdf-normal/accel mesh point fuel)
  (if *raymarch-accel-enabled*
      ;; Use Rust acceleration
      (let* ([bvh (mesh-bvh mesh)]
             [handle (get-rust-handle bvh)]
             [result (rust-mesh-sdf-normal/raw handle point fuel)])
            (case (car result)
                  [(ok)
                   (let ([normal (cadr result)]
                         [fuel-out (caddr result)])
                        `(ok ,normal ,fuel-out))]
                  [(out-of-fuel)
                   `(suspended (mesh-sdf-gradient ,mesh ,point))]
                  [else result]))
      
      ;; Fallback to pure Scheme
      (let ([normal (mesh-sdf-gradient mesh point)])
           `(ok ,normal ,fuel))))

;;; ====
;;; Convenience Wrappers
;;; ====

;;; with-raymarch-accel : (→ α) → α
;;; Run thunk with raymarching acceleration enabled (if available)
;;; Ensures cleanup after execution
(define (with-raymarch-accel thunk)
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda ()
           (when *raymarch-accel-enabled*
                 (cleanup-stale-handles!)))))

;;; render-pixel/accel : Mesh × Ray3 × Vec3 × RaymarchParams × Fuel → Result
;;; Render a single pixel with acceleration
;;; Returns: (ok shading-value fuel) | (miss fuel) | (suspended ...)
(define (render-pixel/accel mesh ray light-pos params fuel)
  (let ([result (raymarch-mesh/accel mesh ray params fuel)])
       (case (car result)
             [(ok)
              (let* ([data (cadr result)]
                     [fuel-out (caddr result)]
                     [hit-point (car data)]
                     [normal (cadr data)])
                    ;; Compute simple shading
                    (let* ([to-light (vec3-normalize (vec3-sub light-pos hit-point))]
                           [to-camera (vec3-normalize (vec3-scale (ray3-direction ray) -1.0))]
                           [shading (simple-shading normal to-light to-camera)])
                          `(ok ,shading ,fuel-out)))]
             [(miss)
              `(ok 0.0 ,(caddr result))]  ; background = 0
             [else result])))

;;; ====
;;; Tests
;;; ====

(define (run-raymarch-accel-tests)
  (display "Raymarch Acceleration Tests\n")
  (display "====\n")
  
  (display (format "Acceleration enabled: ~a\n" (raymarch-accel-enabled?)))
  
  ;; Build test mesh (cube)
  (display "1. Building test mesh (cube)... ")
  (let ([mesh (make-mesh-cube 1.0)])
       (display (format "~a triangles\n" (mesh-triangle-count mesh)))
       
       ;; Test 2: Raymarch hit
       (display "2. Raymarch hit (ray from above)... ")
       (let* ([ray (ray3 (vec3 0 0 3) (vec3 0 0 -1))]
              [params (raymarch-params 100 10.0 0.001)]
              [result (raymarch-mesh/accel mesh ray params 100000)])
             (display (format "~a\n"
                              (if (eq? (car result) 'ok)
                                  (format "HIT at t=~a, ~a steps"
                                          (caddr (cadr result))   ; t
                                          (cadddr (cadr result))) ; steps
                                  result))))
       
       ;; Test 3: Raymarch miss
       (display "3. Raymarch miss (ray away from mesh)... ")
       (let* ([ray (ray3 (vec3 10 10 10) (vec3 1 1 1))]
              [params (raymarch-params 100 10.0 0.001)]
              [result (raymarch-mesh/accel mesh ray params 100000)])
             (display (format "~a\n"
                              (if (eq? (car result) 'miss)
                                  "MISS as expected"
                                  result))))
       
       ;; Test 4: Compare with pure Scheme
       (display "4. Pure Scheme result... ")
       (let* ([ray (ray3 (vec3 0 0 3) (vec3 0 0 -1))]
              [params (raymarch-params 100 10.0 0.001)]
              [scheme-result (raymarch-mesh mesh ray params)])
             (display (format "~a\n"
                              (if scheme-result
                                  (format "HIT at t=~a" (cadr scheme-result))
                                  "MISS"))))
       
       ;; Test 5: Fuel exhaustion
       (display "5. Fuel exhaustion (fuel=10)... ")
       (let* ([ray (ray3 (vec3 0 0 3) (vec3 0 0 -1))]
              [params (raymarch-params 100 10.0 0.001)]
              [result (raymarch-mesh/accel mesh ray params 10)])
             (display (format "~a\n"
                              (if (eq? (car result) 'suspended)
                                  "SUSPENDED as expected"
                                  result)))))
  
  (display "====\n")
  #t)
