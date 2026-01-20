(load "boundary/ffi/ffi-core.ss")
(load "boundary/ffi/serialize.ss")

(doc 'module 'bvh-ffi)
(doc 'description "FFI Bindings for Rust BVH Acceleration — Provides Scheme wrappers for Rust-accelerated BVH operations")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Uses define-ftype for C-compatible structs with out-pointers")

(doc 'section 'result-structs)
(doc 'note "Result structs matching Rust #[repr(C)]")

;;; Closest point query result
(define-ftype closest-point-result-t
  (struct
   [status   unsigned-8]     ; 0=miss, 1=hit, 2=out-of-fuel
   [px       double]         ; closest point x
   [py       double]         ; closest point y
   [pz       double]         ; closest point z
   [distance double]         ; distance to surface
   [fuel     unsigned-64]))  ; remaining fuel

;;; Ray intersection query result
(define-ftype ray-intersect-result-t
  (struct
   [status unsigned-8]       ; 0=miss, 1=hit, 2=out-of-fuel
   [t      double]           ; distance along ray
   [nx     double]           ; normal x
   [ny     double]           ; normal y
   [nz     double]           ; normal z
   [fuel   unsigned-64]))    ; remaining fuel

(doc 'section 'foreign-procedures)
(doc 'note "Foreign procedures bound lazily")

(define rust-bvh-build #f)
(define rust-bvh-drop #f)
(define rust-bvh-closest-point #f)
(define rust-bvh-intersect-ray #f)

(doc bind-bvh-procedures! 'type '(-> Void))
(doc bind-bvh-procedures! 'description "Bind BVH foreign procedures (call after library load)")
(define (bind-bvh-procedures!)
  ;; Build BVH from bytevector, returns handle pointer
  (set! rust-bvh-build
        (foreign-procedure "fold_bvh_build"
                           (u8* size_t)
                           uptr))
  
  ;; Free BVH handle
  (set! rust-bvh-drop
        (foreign-procedure "fold_bvh_drop"
                           (uptr)
                           void))
  
  ;; Closest point query
  (set! rust-bvh-closest-point
        (foreign-procedure "fold_bvh_closest_point"
                           (uptr double double double unsigned-64 (* closest-point-result-t))
                           void))
  
  ;; Ray intersection query
  (set! rust-bvh-intersect-ray
        (foreign-procedure "fold_bvh_intersect_ray"
                           (uptr double double double double double double unsigned-64 (* ray-intersect-result-t))
                           void)))

;;; ====
;;; High-Level Wrappers
;;; ====

;;; rust-bvh-handle : Record type for BVH handles with cleanup tracking
(define-record-type rust-bvh-handle
  (fields ptr))

;;; build-rust-bvh : Bytevector → RustBVHHandle | #f
;;; Build Rust BVH from serialized bytevector
(define (build-rust-bvh bv)
  (unless (accel-available?)
          (error 'build-rust-bvh "Acceleration library not loaded"))
  
  (let ([ptr (rust-bvh-build bv (bytevector-length bv))])
       (if (zero? ptr)
           #f
           (make-rust-bvh-handle ptr))))

;;; free-rust-bvh! : RustBVHHandle → void
;;; Free Rust BVH handle
(define (free-rust-bvh! handle)
  (when (and handle (rust-bvh-handle? handle))
        (rust-bvh-drop (rust-bvh-handle-ptr handle))))

;;; rust-bvh-closest-point/raw : RustBVHHandle × Vec3 × Nat → Result
;;; Query closest point using Rust acceleration
;;; Returns: (ok (Vec3 Real) Fuel) | (miss Fuel) | (out-of-fuel)
(define (rust-bvh-closest-point/raw handle point fuel)
  (unless (accel-available?)
          (error 'rust-bvh-closest-point/raw "Acceleration library not loaded"))
  (unless (rust-bvh-handle? handle)
          (error 'rust-bvh-closest-point/raw "Invalid handle"))

  ;; Allocate result struct
  ;; BUGFIX: Use dynamic-wind to ensure cleanup on exception
  (let* ([result-addr (foreign-alloc (ftype-sizeof closest-point-result-t))]
         [result-ptr (make-ftype-pointer closest-point-result-t result-addr)]
         [ptr (rust-bvh-handle-ptr handle)])

        (dynamic-wind
         (lambda () #f)  ; no setup needed
         (lambda ()
           ;; Call Rust function
           (rust-bvh-closest-point ptr
                                   (vec3-x point)
                                   (vec3-y point)
                                   (vec3-z point)
                                   fuel
                                   result-ptr)

           ;; Extract results
           (let ([status (ftype-ref closest-point-result-t (status) result-ptr)]
                 [px (ftype-ref closest-point-result-t (px) result-ptr)]
                 [py (ftype-ref closest-point-result-t (py) result-ptr)]
                 [pz (ftype-ref closest-point-result-t (pz) result-ptr)]
                 [dist (ftype-ref closest-point-result-t (distance) result-ptr)]
                 [fuel-out (ftype-ref closest-point-result-t (fuel) result-ptr)])

                ;; Return appropriate result
                (case status
                      [(0) `(miss ,fuel-out)]
                      [(1) `(ok (,(vec3 px py pz) ,dist) ,fuel-out)]
                      [(2) '(out-of-fuel)]
                      [else `(error unknown-status ,status)])))
         (lambda ()
           ;; Always free allocated memory
           (foreign-free result-addr)))))

;;; rust-bvh-intersect-ray/raw : RustBVHHandle × Vec3 × Vec3 × Nat → Result
;;; Query ray intersection using Rust acceleration
;;; Returns: (ok (Real Vec3) Fuel) | (miss Fuel) | (out-of-fuel)
(define (rust-bvh-intersect-ray/raw handle origin direction fuel)
  (unless (accel-available?)
          (error 'rust-bvh-intersect-ray/raw "Acceleration library not loaded"))
  (unless (rust-bvh-handle? handle)
          (error 'rust-bvh-intersect-ray/raw "Invalid handle"))

  ;; Allocate result struct
  ;; BUGFIX: Use dynamic-wind to ensure cleanup on exception
  (let* ([result-addr (foreign-alloc (ftype-sizeof ray-intersect-result-t))]
         [result-ptr (make-ftype-pointer ray-intersect-result-t result-addr)]
         [ptr (rust-bvh-handle-ptr handle)]
         [ox (inexact (vec3-x origin))]
         [oy (inexact (vec3-y origin))]
         [oz (inexact (vec3-z origin))]
         [dx (inexact (vec3-x direction))]
         [dy (inexact (vec3-y direction))]
         [dz (inexact (vec3-z direction))])

        (dynamic-wind
         (lambda () #f)  ; no setup needed
         (lambda ()
           ;; Call Rust function
           (rust-bvh-intersect-ray ptr ox oy oz dx dy dz fuel result-ptr)

           ;; Extract results
           (let ([status (ftype-ref ray-intersect-result-t (status) result-ptr)]
                 [t (ftype-ref ray-intersect-result-t (t) result-ptr)]
                 [nx (ftype-ref ray-intersect-result-t (nx) result-ptr)]
                 [ny (ftype-ref ray-intersect-result-t (ny) result-ptr)]
                 [nz (ftype-ref ray-intersect-result-t (nz) result-ptr)]
                 [fuel-out (ftype-ref ray-intersect-result-t (fuel) result-ptr)])

                ;; Return appropriate result
                (case status
                      [(0) `(miss ,fuel-out)]
                      [(1) `(ok (,t ,(vec3 nx ny nz)) ,fuel-out)]
                      [(2) '(out-of-fuel)]
                      [else `(error unknown-status ,status)])))
         (lambda ()
           ;; Always free allocated memory
           (foreign-free result-addr)))))

;;; ====
;;; Convenience: Build from Scheme BVH
;;; ====

;;; scheme-bvh->rust-handle : BVH → RustBVHHandle | #f
;;; Convert Scheme BVH to Rust handle
(define (scheme-bvh->rust-handle bvh)
  (let ([bv (bvh->bytes bvh)])
       (build-rust-bvh bv)))

;;; ====
;;; Tests
;;; ====

(define (run-bvh-ffi-tests)
  (display "BVH FFI Tests\n")
  (display "====\n")
  
  ;; Ensure library is loaded
  (unless (accel-available?)
          (accel-load!))
  
  (unless (accel-available?)
          (display "SKIP: No acceleration library\n")
          (display "====\n")
          #f)
  
  ;; Bind BVH procedures
  (bind-bvh-procedures!)
  
  ;; Test 1: Build BVH from Scheme
  (display "1. Build Rust BVH from Scheme... ")
  (let* ([triangles (list
                     (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
                     (triangle3 (vec3 1 0 0) (vec3 2 0 0) (vec3 1 1 0))
                     (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0)))]
         [scheme-bvh (bvh-build triangles 2)]
         [rust-handle (scheme-bvh->rust-handle scheme-bvh)])
        (if rust-handle
            (begin
             (display "OK\n")
             
             ;; Test 2: Closest point query
             (display "2. Closest point query... ")
             (let ([result (rust-bvh-closest-point/raw rust-handle (vec3 0.5 0.5 1.0) 1000)])
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'ok))
                          (display "   FAIL: expected (ok ...)\n")))
             
             ;; Test 3: Ray intersection query
             (display "3. Ray intersection query... ")
             (let ([result (rust-bvh-intersect-ray/raw rust-handle
                                                       (vec3 0.25 0.25 1.0)
                                                       (vec3 0.0 0.0 -1.0)
                                                       1000)])
                  (display result)
                  (newline)
                  (unless (and (pair? result) (eq? (car result) 'ok))
                          (display "   FAIL: expected (ok ...)\n")))
             
             ;; Test 4: Fuel exhaustion
             (display "4. Fuel exhaustion... ")
             (let ([result (rust-bvh-closest-point/raw rust-handle (vec3 0.5 0.5 1.0) 2)])
                  (display result)
                  (newline)
                  (unless (equal? result '(out-of-fuel))
                          (display "   FAIL: expected (out-of-fuel)\n")))
             
             ;; Test 5: Miss (point far from triangles)
             (display "5. Miss query... ")
             (let ([result (rust-bvh-intersect-ray/raw rust-handle
                                                       (vec3 100 100 100)
                                                       (vec3 1 0 0)
                                                       1000)])
                  (display result)
                  (newline))
             
             ;; Cleanup
             (display "6. Free handle... ")
             (free-rust-bvh! rust-handle)
             (display "OK\n"))
            
            (display "FAIL\n")))
  
  (display "====\n")
  #t)
