(load "lattice/geometry/raymarch.ss")

(doc 'module 'raymarch-accel)
(doc 'description "Transparent Rust-accelerated raymarching with fallback to pure Scheme")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'provides "Mesh raymarching that transparently uses Rust acceleration if available; API matches pure Scheme but adds fuel tracking")
(doc 'note "Function: (raymarch-mesh/accel mesh ray params fuel) → (ok result fuel) | (suspended ...)")

(doc 'note "Try to load acceleration, but don't fail if unavailable")
(define *raymarch-accel-enabled* #f)
(guard (ex [else (set! *raymarch-accel-enabled* #f)])
       (load "boundary/ffi/raymarch-ffi.ss")
       (load "boundary/ffi/bvh-cache.ss")
       (when (accel-available?)
             (bind-bvh-procedures!)
             (bind-raymarch-procedures!)
             (set! *raymarch-accel-enabled* #t)))

(doc 'section 'acceleration-status)

(define (raymarch-accel-enabled?)
  (doc 'type '(-> Boolean))
  (doc 'description "Check if Rust raymarching acceleration is available")
  *raymarch-accel-enabled*)

(doc 'section 'fuel-cost-estimation)
(doc 'note "For Scheme fallback")

(define (estimate-raymarch-fuel mesh params)
  (doc 'type '(-> Mesh RaymarchParams Nat))
  (doc 'description "Estimate fuel cost for raymarching on this mesh")
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

(doc 'section 'accelerated-operations)

(define (raymarch-mesh/accel mesh ray params fuel)
  (doc 'type '(-> Mesh Ray3 RaymarchParams Fuel Result))
  (doc 'description "Raymarching with fuel tracking")
  (doc 'returns "(ok (hit-point normal t steps tri-idx) fuel) | (miss steps fuel) | (suspended ...)")
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

(define (mesh-sdf-normal/accel mesh point fuel)
  (doc 'type '(-> Mesh Vec3 Fuel Result))
  (doc 'description "Compute mesh SDF normal with fuel tracking")
  (doc 'returns "(ok normal fuel) | (suspended ...)")
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

(doc 'section 'convenience-wrappers)

(define (with-raymarch-accel thunk)
  (doc 'type '(-> (-> α) α))
  (doc 'description "Run thunk with raymarching acceleration enabled (if available); ensures cleanup after execution")
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda ()
           (when *raymarch-accel-enabled*
                 (cleanup-stale-handles!)))))

(define (render-pixel/accel mesh ray light-pos params fuel)
  (doc 'type '(-> Mesh Ray3 Vec3 RaymarchParams Fuel Result))
  (doc 'description "Render a single pixel with acceleration")
  (doc 'returns "(ok shading-value fuel) | (miss fuel) | (suspended ...)")
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

(doc 'section 'tests)

(define (run-raymarch-accel-tests)
  (doc 'description "Run raymarch acceleration test suite")
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
