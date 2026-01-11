;;; Benchmark: Bytevector FFI vs Element-Copy FFI
;;;
;;; Tests whether bytevector pass-through eliminates marshaling overhead.

(load "core/base/prelude.ss")
(load "shell/ffi/ffi-core.ss")
(load "shell/ffi/bytevector-ffi.ss")

(display "=== Bytevector FFI Benchmark ===\n\n")

;;; ============================================================
;;; Load Library
;;; ============================================================

(unless (accel-load!)
        (error 'bench "Cannot load acceleration library"))

;;; ============================================================
;;; Result struct for mat4 (must match Rust)
;;; ============================================================

(define-ftype mat4-result-t
  (struct
   [status unsigned-8]
   [m0 double] [m1 double] [m2 double] [m3 double]
   [m4 double] [m5 double] [m6 double] [m7 double]
   [m8 double] [m9 double] [m10 double] [m11 double]
   [m12 double] [m13 double] [m14 double] [m15 double]
   [fuel unsigned-64]))

;;; ============================================================
;;; FFI Bindings - Both Approaches
;;; ============================================================

;; Approach 1: void* (requires manual pointer passing)
(define rust-mat4-mul-ptr
  (foreign-procedure "fold_mat4_mul"
                     (void* void* unsigned-64 void*)
                     void))

;; Approach 2: u8* (accepts bytevectors directly!)
;; Chez Scheme automatically passes bytevector data pointer
(define rust-mat4-mul-bv
  (foreign-procedure "fold_mat4_mul"
                     (u8* u8* unsigned-64 void*)
                     void))

;; Batch transform with bytevectors
(define rust-mat4-transform-bv
  (foreign-procedure "fold_mat4_transform_points"
                     (u8* u8* unsigned-64 u8* unsigned-64 void* void*)
                     void))

;;; ============================================================
;;; Test Data as Bytevectors
;;; ============================================================

(define mat-a-bv (make-mat4-bytevector))
(define mat-b-bv (make-mat4-bytevector))
(define result-bv (make-mat4-bytevector))

;; Fill mat-a with test values
(do ([i 0 (+ i 1)])
    ((= i 16))
    (f64-bv-set! mat-a-bv i (+ 1.0 i)))

;; Fill mat-b with test values
(do ([i 0 (+ i 1)])
    ((= i 16))
    (f64-bv-set! mat-b-bv i (+ 17.0 i)))

;; Identity for comparison
(define identity-bv (make-mat4-bytevector))
(mat4-identity! identity-bv)

;;; ============================================================
;;; Timing Utilities
;;; ============================================================

(define (time-it name iterations thunk)
  (collect)
  (let* ([start (current-time)]
         [_ (let loop ([i iterations])
                 (when (> i 0)
                       (thunk)
                       (loop (- i 1))))]
         [end (current-time)]
         [elapsed-ns (+ (* (- (time-second end) (time-second start)) 1000000000)
                        (- (time-nanosecond end) (time-nanosecond start)))]
         [ns-per-call (/ elapsed-ns iterations)])
        (printf "~a: ~,0fns/call\n" name ns-per-call)
        ns-per-call))

;;; ============================================================
;;; Test 1: Correctness
;;; ============================================================

(display "1. Correctness Test (A * I = A)\n")

;; Allocate result struct
(define result-ptr (make-ftype-pointer mat4-result-t
                                       (foreign-alloc (ftype-sizeof mat4-result-t))))

;; Call with bytevectors directly
(rust-mat4-mul-bv mat-a-bv identity-bv 10000 (ftype-pointer-address result-ptr))

(let ([status (ftype-ref mat4-result-t (status) result-ptr)])
     (if (= status 1)
         (begin
          (display "   Status: OK\n")
          (display "   Result[0..3]: ")
          (display (list (ftype-ref mat4-result-t (m0) result-ptr)
                         (ftype-ref mat4-result-t (m1) result-ptr)
                         (ftype-ref mat4-result-t (m2) result-ptr)
                         (ftype-ref mat4-result-t (m3) result-ptr)))
          (newline)
          (display "   Expected:     ")
          (display (list (f64-bv-ref mat-a-bv 0)
                         (f64-bv-ref mat-a-bv 1)
                         (f64-bv-ref mat-a-bv 2)
                         (f64-bv-ref mat-a-bv 3)))
          (newline))
         (printf "   Status: FAIL (~a)\n" status)))

;;; ============================================================
;;; Test 2: Performance - Bytevector vs Element Copy
;;; ============================================================

(display "\n2. Performance: Bytevector Direct Pass\n")

(define iterations 100000)
(printf "   Running ~a iterations...\n\n" iterations)

;; Bytevector direct pass (u8*)
(define bv-direct-ns
  (time-it "   Bytevector direct (u8*)" iterations
           (lambda ()
                   (rust-mat4-mul-bv mat-a-bv mat-b-bv 10000
                                     (ftype-pointer-address result-ptr)))))

;; Compare to old approach with foreign-alloc + copy
(define mat-a-vec (f64-bytevector->vector mat-a-bv))
(define mat-b-vec (f64-bytevector->vector mat-b-bv))

(define (old-approach-call a-vec b-vec fuel)
  (let* ([a-ptr (foreign-alloc (* 16 8))]
         [b-ptr (foreign-alloc (* 16 8))])
        ;; Copy in
        (do ([i 0 (+ i 1)])
            ((= i 16))
            (foreign-set! 'double a-ptr (* i 8) (vector-ref a-vec i))
            (foreign-set! 'double b-ptr (* i 8) (vector-ref b-vec i)))
        ;; Call
        (rust-mat4-mul-ptr a-ptr b-ptr fuel (ftype-pointer-address result-ptr))
        ;; Free
        (foreign-free a-ptr)
        (foreign-free b-ptr)))

(define old-approach-ns
  (time-it "   Element copy (foreign-alloc)" iterations
           (lambda ()
                   (old-approach-call mat-a-vec mat-b-vec 10000))))

(printf "\n   Speedup: ~,2fx\n" (/ old-approach-ns bv-direct-ns))

;;; ============================================================
;;; Test 3: Batch Transform with Bytevectors
;;; ============================================================

(display "\n3. Batch Transform (1000 points)\n\n")

(define batch-size 1000)
(define batch-iterations 1000)

;; Create point buffers as bytevectors
(define points-in-bv (make-points-bytevector batch-size))
(define points-out-bv (make-points-bytevector batch-size))

;; Output params need foreign-alloc (written by Rust, not bytevector-safe)
(define fuel-out-ptr (foreign-alloc 8))
(define status-ptr (foreign-alloc 1))

;; Fill input points
(do ([i 0 (+ i 1)])
    ((= i batch-size))
    (points-bv-set! points-in-bv i
                    (exact->inexact i)
                    (exact->inexact (* i 2))
                    (exact->inexact (* i 3))
                    1.0))

;; Bytevector batch transform
;; Lock bytevectors during FFI call for GC safety (best practice)
(define bv-batch-ns
  (time-it "   Rust bytevector batch" batch-iterations
           (lambda ()
                   (with-locked-bytevectors
                    (list mat-a-bv points-in-bv points-out-bv)
                    (lambda ()
                            (rust-mat4-transform-bv
                             mat-a-bv
                             points-in-bv
                             batch-size
                             points-out-bv
                             1000000
                             fuel-out-ptr
                             status-ptr))))))

(printf "   Per-point: ~,1fns\n" (/ bv-batch-ns batch-size))

;; Compare to Scheme
;; NOTE: Must write results for fair comparison with Rust!
(define (scheme-batch-transform mat points-in points-out n)
  (do ([i 0 (+ i 1)])
      ((= i n))
      (let* ([base (* i 4)]
             [x (f64-bv-ref points-in base)]
             [y (f64-bv-ref points-in (+ base 1))]
             [z (f64-bv-ref points-in (+ base 2))]
             [w (f64-bv-ref points-in (+ base 3))])
            ;; Write all 4 components to output (same work as Rust)
            (f64-bv-set! points-out base
                         (+ (* (f64-bv-ref mat 0) x) (* (f64-bv-ref mat 1) y)
                            (* (f64-bv-ref mat 2) z) (* (f64-bv-ref mat 3) w)))
            (f64-bv-set! points-out (+ base 1)
                         (+ (* (f64-bv-ref mat 4) x) (* (f64-bv-ref mat 5) y)
                            (* (f64-bv-ref mat 6) z) (* (f64-bv-ref mat 7) w)))
            (f64-bv-set! points-out (+ base 2)
                         (+ (* (f64-bv-ref mat 8) x) (* (f64-bv-ref mat 9) y)
                            (* (f64-bv-ref mat 10) z) (* (f64-bv-ref mat 11) w)))
            (f64-bv-set! points-out (+ base 3)
                         (+ (* (f64-bv-ref mat 12) x) (* (f64-bv-ref mat 13) y)
                            (* (f64-bv-ref mat 14) z) (* (f64-bv-ref mat 15) w))))))

;; Allocate Scheme output buffer for fair comparison
(define scheme-points-out-bv (make-points-bytevector batch-size))

(define scheme-batch-ns
  (time-it "   Scheme bytevector loop" batch-iterations
           (lambda ()
                   (scheme-batch-transform mat-a-bv points-in-bv scheme-points-out-bv batch-size))))

(printf "   Per-point: ~,1fns\n\n" (/ scheme-batch-ns batch-size))
(printf "   Speedup: ~,2fx\n" (/ scheme-batch-ns bv-batch-ns))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n=== Summary ===\n")
(printf "Single mat4 mul:\n")
(printf "  Bytevector direct: ~,0fns\n" bv-direct-ns)
(printf "  Element copy:      ~,0fns\n" old-approach-ns)
(printf "  Improvement:       ~,2fx\n\n" (/ old-approach-ns bv-direct-ns))

(printf "Batch 1000 points:\n")
(printf "  Rust (bytevector): ~,0fns total, ~,1fns/point\n"
        bv-batch-ns (/ bv-batch-ns batch-size))
(printf "  Scheme (bytevector): ~,0fns total, ~,1fns/point\n"
        scheme-batch-ns (/ scheme-batch-ns batch-size))

(if (< bv-batch-ns scheme-batch-ns)
    (printf "  Winner: Rust (~,2fx faster)\n" (/ scheme-batch-ns bv-batch-ns))
    (printf "  Winner: Scheme (~,2fx faster)\n" (/ bv-batch-ns scheme-batch-ns)))

;; Cleanup
(foreign-free (ftype-pointer-address result-ptr))
(foreign-free fuel-out-ptr)
(foreign-free status-ptr)

(display "\n=== Benchmark Complete ===\n")
