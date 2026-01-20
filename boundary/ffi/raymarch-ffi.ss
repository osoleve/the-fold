(load "boundary/ffi/ffi-core.ss")
(load "boundary/ffi/bvh-ffi.ss")

(doc 'module 'raymarch-ffi)
(doc 'description "FFI Bindings for Rust Raymarcher Acceleration — Provides Scheme wrappers for Rust-accelerated mesh raymarching")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Uses define-ftype for C-compatible structs with out-pointers")

(doc 'section 'result-structs)
(doc 'note "Result structs matching Rust #[repr(C)]")

;;; Raymarching result struct
;;; Fields ordered for natural alignment (f64 first, then u64, u32, u8)
(define-ftype raymarch-result-t
  (struct
   [px            double]          ; hit point x
   [py            double]          ; hit point y
   [pz            double]          ; hit point z
   [nx            double]          ; normal x
   [ny            double]          ; normal y
   [nz            double]          ; normal z
   [t             double]          ; ray parameter at hit
   [fuel          unsigned-64]     ; remaining fuel
   [steps         unsigned-32]     ; steps taken
   [triangle-idx  unsigned-32]     ; triangle index (for texturing)
   [status        unsigned-8]      ; 0=miss, 1=hit, 2=out-of-fuel
   [pad0          unsigned-8]      ; explicit padding
   [pad1          unsigned-8]
   [pad2          unsigned-8]
   [pad3          unsigned-8]
   [pad4          unsigned-8]
   [pad5          unsigned-8]
   [pad6          unsigned-8]))

;;; Normal computation result struct
(define-ftype normal-result-t
  (struct
   [nx     double]
   [ny     double]
   [nz     double]
   [fuel   unsigned-64]
   [status unsigned-8]
   [pad0   unsigned-8]
   [pad1   unsigned-8]
   [pad2   unsigned-8]
   [pad3   unsigned-8]
   [pad4   unsigned-8]
   [pad5   unsigned-8]
   [pad6   unsigned-8]))

;;; ====
;;; Foreign Procedures (bound lazily)
;;; ====

(define rust-raymarch-mesh #f)
(define rust-mesh-sdf-normal #f)

;;; bind-raymarch-procedures! : → void
;;; Bind raymarching foreign procedures (call after library load)
(define (bind-raymarch-procedures!)
  ;; Complete mesh raymarching in one FFI call
  (set! rust-raymarch-mesh
        (foreign-procedure "fold_raymarch_mesh"
                           (uptr                         ; BVH handle
                                 double double double         ; ray origin
                                 double double double         ; ray direction
                                 unsigned-32                  ; max_steps
                                 double                       ; max_distance
                                 double                       ; threshold
                                 unsigned-64                  ; fuel_in
                                 (* raymarch-result-t))       ; out pointer
                           void))
  
  ;; Compute mesh SDF normal at a point
  (set! rust-mesh-sdf-normal
        (foreign-procedure "fold_mesh_sdf_normal"
                           (uptr                         ; BVH handle
                                 double double double         ; point
                                 unsigned-64                  ; fuel_in
                                 (* normal-result-t))         ; out pointer
                           void)))

;;; ====
;;; High-Level Wrappers
;;; ====

;;; rust-raymarch-mesh/raw : RustBVHHandle × Vec3 × Vec3 × RaymarchParams × Nat → Result
;;; Perform raymarching using Rust acceleration
;;; Returns: (ok (hit-point normal t steps tri-idx) fuel) | (miss fuel) | (out-of-fuel)
(define (rust-raymarch-mesh/raw handle origin direction max-steps max-distance threshold fuel)
  (unless (accel-available?)
          (error 'rust-raymarch-mesh/raw "Acceleration library not loaded"))
  (unless (rust-bvh-handle? handle)
          (error 'rust-raymarch-mesh/raw "Invalid handle"))
  
  ;; Allocate result struct
  (let* ([result-ptr (make-ftype-pointer raymarch-result-t
                                         (foreign-alloc (ftype-sizeof raymarch-result-t)))]
         [ptr (rust-bvh-handle-ptr handle)]
         [ox (inexact (vec3-x origin))]
         [oy (inexact (vec3-y origin))]
         [oz (inexact (vec3-z origin))]
         [dx (inexact (vec3-x direction))]
         [dy (inexact (vec3-y direction))]
         [dz (inexact (vec3-z direction))])
        
        ;; Use dynamic-wind to ensure cleanup even if exception occurs
        (dynamic-wind
         (lambda () #f)
         (lambda ()
                 ;; Call Rust function
                 (rust-raymarch-mesh ptr
                                     ox oy oz
                                     dx dy dz
                                     max-steps
                                     (inexact max-distance)
                                     (inexact threshold)
                                     fuel
                                     result-ptr)
                 
                 ;; Extract results
                 (let ([status (ftype-ref raymarch-result-t (status) result-ptr)]
                       [px (ftype-ref raymarch-result-t (px) result-ptr)]
                       [py (ftype-ref raymarch-result-t (py) result-ptr)]
                       [pz (ftype-ref raymarch-result-t (pz) result-ptr)]
                       [nx (ftype-ref raymarch-result-t (nx) result-ptr)]
                       [ny (ftype-ref raymarch-result-t (ny) result-ptr)]
                       [nz (ftype-ref raymarch-result-t (nz) result-ptr)]
                       [t (ftype-ref raymarch-result-t (t) result-ptr)]
                       [steps (ftype-ref raymarch-result-t (steps) result-ptr)]
                       [tri-idx (ftype-ref raymarch-result-t (triangle-idx) result-ptr)]
                       [fuel-out (ftype-ref raymarch-result-t (fuel) result-ptr)])
                      
                      ;; Return appropriate result
                      (case status
                            [(0) `(miss ,steps ,fuel-out)]
                            [(1) `(ok (,(vec3 px py pz) ,(vec3 nx ny nz) ,t ,steps ,tri-idx) ,fuel-out)]
                            [(2) `(out-of-fuel ,steps)]
                            [else `(error unknown-status ,status)])))
         (lambda ()
                 ;; Cleanup - always runs even on exception
                 (foreign-free (ftype-pointer-address result-ptr))))))

;;; rust-mesh-sdf-normal/raw : RustBVHHandle × Vec3 × Nat → Result
;;; Compute mesh SDF normal at a point
;;; Returns: (ok normal fuel) | (out-of-fuel)
(define (rust-mesh-sdf-normal/raw handle point fuel)
  (unless (accel-available?)
          (error 'rust-mesh-sdf-normal/raw "Acceleration library not loaded"))
  (unless (rust-bvh-handle? handle)
          (error 'rust-mesh-sdf-normal/raw "Invalid handle"))
  
  ;; Allocate result struct
  (let* ([result-ptr (make-ftype-pointer normal-result-t
                                         (foreign-alloc (ftype-sizeof normal-result-t)))]
         [ptr (rust-bvh-handle-ptr handle)]
         [px (inexact (vec3-x point))]
         [py (inexact (vec3-y point))]
         [pz (inexact (vec3-z point))])
        
        ;; Use dynamic-wind to ensure cleanup even if exception occurs
        (dynamic-wind
         (lambda () #f)
         (lambda ()
                 ;; Call Rust function
                 (rust-mesh-sdf-normal ptr px py pz fuel result-ptr)
                 
                 ;; Extract results
                 (let ([status (ftype-ref normal-result-t (status) result-ptr)]
                       [nx (ftype-ref normal-result-t (nx) result-ptr)]
                       [ny (ftype-ref normal-result-t (ny) result-ptr)]
                       [nz (ftype-ref normal-result-t (nz) result-ptr)]
                       [fuel-out (ftype-ref normal-result-t (fuel) result-ptr)])
                      
                      ;; Return appropriate result
                      (case status
                            [(1) `(ok ,(vec3 nx ny nz) ,fuel-out)]
                            [(2) '(out-of-fuel)]
                            [else `(error unknown-status ,status)])))
         (lambda ()
                 ;; Cleanup - always runs even on exception
                 (foreign-free (ftype-pointer-address result-ptr))))))

;;; ====
;;; Convenience Wrappers
;;; ====

;;; raymarch-mesh/rust : Mesh × Ray3 × RaymarchParams × Nat → Result
;;; Raymarching with automatic BVH caching
;;; Returns: (ok (hit-point normal t steps tri-idx) fuel) | (miss fuel) | (out-of-fuel)
(define (raymarch-mesh/rust mesh ray params fuel)
  (unless (accel-available?)
          (error 'raymarch-mesh/rust "Acceleration library not loaded"))
  
  ;; Get cached rust handle for the mesh's BVH
  (let* ([bvh (mesh-bvh mesh)]
         [handle (get-rust-handle bvh)]
         [origin (ray3-origin ray)]
         [direction (ray3-direction ray)]
         [max-steps (raymarch-params-max-steps params)]
         [max-dist (raymarch-params-max-distance params)]
         [threshold (raymarch-params-hit-threshold params)])
        (rust-raymarch-mesh/raw handle origin direction max-steps max-dist threshold fuel)))

;;; ====
;;; Tests
;;; ====

(define (run-raymarch-ffi-tests)
  (display "Raymarch FFI Tests\n")
  (display "====\n")
  
  ;; Ensure library is loaded
  (unless (accel-available?)
          (accel-load!))
  
  (unless (accel-available?)
          (display "SKIP: No acceleration library\n")
          (display "====\n")
          #f)
  
  ;; Bind procedures
  (bind-bvh-procedures!)
  (bind-raymarch-procedures!)
  
  ;; Create test mesh (triangle in XY plane at z=0)
  (display "1. Build test mesh BVH... ")
  (let* ([triangles (list
                     (triangle3 (vec3 0 0 0) (vec3 2 0 0) (vec3 1 2 0)))]
         [scheme-bvh (bvh-build triangles 2)]
         [rust-handle (scheme-bvh->rust-handle scheme-bvh)])
        (if rust-handle
            (begin
             (display "OK\n")
             
             ;; Test 2: Raymarch hit
             (display "2. Raymarch hit (ray from above)... ")
             (let ([result (rust-raymarch-mesh/raw rust-handle
                                                   (vec3 1.0 1.0 5.0)    ; origin above
                                                   (vec3 0.0 0.0 -1.0)   ; pointing down
                                                   100                    ; max steps
                                                   100.0                  ; max distance
                                                   0.001                  ; threshold
                                                   100000)])              ; fuel
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'ok))
                          (display "   FAIL: expected (ok ...)\n")))
             
             ;; Test 3: Raymarch miss
             (display "3. Raymarch miss (ray away from mesh)... ")
             (let ([result (rust-raymarch-mesh/raw rust-handle
                                                   (vec3 10.0 10.0 5.0)   ; far from mesh
                                                   (vec3 0.0 0.0 -1.0)
                                                   100
                                                   100.0
                                                   0.001
                                                   100000)])
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'miss))
                          (display "   FAIL: expected (miss ...)\n")))
             
             ;; Test 4: Normal computation
             (display "4. Normal computation at point... ")
             (let ([result (rust-mesh-sdf-normal/raw rust-handle
                                                     (vec3 1.0 1.0 0.1)   ; just above triangle
                                                     100000)])
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'ok))
                          (display "   FAIL: expected (ok ...)\n")))
             
             ;; Test 5: Fuel exhaustion
             (display "5. Fuel exhaustion... ")
             (let ([result (rust-raymarch-mesh/raw rust-handle
                                                   (vec3 1.0 1.0 5.0)
                                                   (vec3 0.0 0.0 -1.0)
                                                   100
                                                   100.0
                                                   0.001
                                                   10)])   ; very low fuel
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'out-of-fuel))
                          (display "   FAIL: expected (out-of-fuel ...)\n")))
             
             ;; Cleanup
             (display "6. Free handle... ")
             (free-rust-bvh! rust-handle)
             (display "OK\n"))
            
            (display "FAIL: Could not build BVH\n")))
  
  (display "====\n")
  #t)
