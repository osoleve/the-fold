;;; Benchmark: Layer 2 Mat4 Rust FFI vs Scheme
;;;
;;; Tests whether matrix operations (112+ ops) amortize FFI overhead.

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "shell/ffi/ffi-core.ss")

(display "=== Layer 2 Benchmark: Mat4 Multiply ===\n\n")

;;; ====
;;; Mat4 Rust FFI Binding
;;; ====

;; Result struct: { status: u8, m: [f64; 16], fuel: u64 }
;; Total size: 1 + 128 + 8 = 137 bytes (plus alignment padding)
(define-ftype mat4-result-t
  (struct
   [status unsigned-8]
   [m0 double] [m1 double] [m2 double] [m3 double]
   [m4 double] [m5 double] [m6 double] [m7 double]
   [m8 double] [m9 double] [m10 double] [m11 double]
   [m12 double] [m13 double] [m14 double] [m15 double]
   [fuel unsigned-64]))

;; Load library
(unless (accel-load!)
        (error 'bench-mat4 "Cannot load acceleration library"))

;; Bind fold_mat4_mul
;; Signature: fn(a: *const f64, b: *const f64, fuel: u64, out: *mut Mat4Result)
;; Note: out is passed as raw void* since we manage the struct manually
(define rust-mat4-mul
  (foreign-procedure "fold_mat4_mul"
                     (void* void* unsigned-64 void*)
                     void))

;;; ====
;;; Scheme Mat4 Implementation (for comparison)
;;; ====

;; Matrix as flat vector (row-major)
(define (scheme-mat4-mul a b)
  (let ([c (make-vector 16 0.0)])
       ;; C[i][j] = sum(k, A[i][k] * B[k][j])
       ;; Fully unrolled for fair comparison
       
       ;; Row 0
       (vector-set! c 0  (+ (* (vector-ref a 0) (vector-ref b 0))
                            (* (vector-ref a 1) (vector-ref b 4))
                            (* (vector-ref a 2) (vector-ref b 8))
                            (* (vector-ref a 3) (vector-ref b 12))))
       (vector-set! c 1  (+ (* (vector-ref a 0) (vector-ref b 1))
                            (* (vector-ref a 1) (vector-ref b 5))
                            (* (vector-ref a 2) (vector-ref b 9))
                            (* (vector-ref a 3) (vector-ref b 13))))
       (vector-set! c 2  (+ (* (vector-ref a 0) (vector-ref b 2))
                            (* (vector-ref a 1) (vector-ref b 6))
                            (* (vector-ref a 2) (vector-ref b 10))
                            (* (vector-ref a 3) (vector-ref b 14))))
       (vector-set! c 3  (+ (* (vector-ref a 0) (vector-ref b 3))
                            (* (vector-ref a 1) (vector-ref b 7))
                            (* (vector-ref a 2) (vector-ref b 11))
                            (* (vector-ref a 3) (vector-ref b 15))))
       
       ;; Row 1
       (vector-set! c 4  (+ (* (vector-ref a 4) (vector-ref b 0))
                            (* (vector-ref a 5) (vector-ref b 4))
                            (* (vector-ref a 6) (vector-ref b 8))
                            (* (vector-ref a 7) (vector-ref b 12))))
       (vector-set! c 5  (+ (* (vector-ref a 4) (vector-ref b 1))
                            (* (vector-ref a 5) (vector-ref b 5))
                            (* (vector-ref a 6) (vector-ref b 9))
                            (* (vector-ref a 7) (vector-ref b 13))))
       (vector-set! c 6  (+ (* (vector-ref a 4) (vector-ref b 2))
                            (* (vector-ref a 5) (vector-ref b 6))
                            (* (vector-ref a 6) (vector-ref b 10))
                            (* (vector-ref a 7) (vector-ref b 14))))
       (vector-set! c 7  (+ (* (vector-ref a 4) (vector-ref b 3))
                            (* (vector-ref a 5) (vector-ref b 7))
                            (* (vector-ref a 6) (vector-ref b 11))
                            (* (vector-ref a 7) (vector-ref b 15))))
       
       ;; Row 2
       (vector-set! c 8  (+ (* (vector-ref a 8) (vector-ref b 0))
                            (* (vector-ref a 9) (vector-ref b 4))
                            (* (vector-ref a 10) (vector-ref b 8))
                            (* (vector-ref a 11) (vector-ref b 12))))
       (vector-set! c 9  (+ (* (vector-ref a 8) (vector-ref b 1))
                            (* (vector-ref a 9) (vector-ref b 5))
                            (* (vector-ref a 10) (vector-ref b 9))
                            (* (vector-ref a 11) (vector-ref b 13))))
       (vector-set! c 10 (+ (* (vector-ref a 8) (vector-ref b 2))
                            (* (vector-ref a 9) (vector-ref b 6))
                            (* (vector-ref a 10) (vector-ref b 10))
                            (* (vector-ref a 11) (vector-ref b 14))))
       (vector-set! c 11 (+ (* (vector-ref a 8) (vector-ref b 3))
                            (* (vector-ref a 9) (vector-ref b 7))
                            (* (vector-ref a 10) (vector-ref b 11))
                            (* (vector-ref a 11) (vector-ref b 15))))
       
       ;; Row 3
       (vector-set! c 12 (+ (* (vector-ref a 12) (vector-ref b 0))
                            (* (vector-ref a 13) (vector-ref b 4))
                            (* (vector-ref a 14) (vector-ref b 8))
                            (* (vector-ref a 15) (vector-ref b 12))))
       (vector-set! c 13 (+ (* (vector-ref a 12) (vector-ref b 1))
                            (* (vector-ref a 13) (vector-ref b 5))
                            (* (vector-ref a 14) (vector-ref b 9))
                            (* (vector-ref a 15) (vector-ref b 13))))
       (vector-set! c 14 (+ (* (vector-ref a 12) (vector-ref b 2))
                            (* (vector-ref a 13) (vector-ref b 6))
                            (* (vector-ref a 14) (vector-ref b 10))
                            (* (vector-ref a 15) (vector-ref b 14))))
       (vector-set! c 15 (+ (* (vector-ref a 12) (vector-ref b 3))
                            (* (vector-ref a 13) (vector-ref b 7))
                            (* (vector-ref a 14) (vector-ref b 11))
                            (* (vector-ref a 15) (vector-ref b 15))))
       c))

;;; ====
;;; FFI Helper
;;; ====

;; Call Rust mat4_mul with flat vectors
(define (rust-mat4-mul-call a b fuel)
  ;; Allocate result struct
  (let* ([result-ptr (make-ftype-pointer mat4-result-t
                                         (foreign-alloc (ftype-sizeof mat4-result-t)))]
         ;; Allocate input arrays
         [a-ptr (foreign-alloc (* 16 8))]
         [b-ptr (foreign-alloc (* 16 8))])
        
        ;; Copy input data
        (do ([i 0 (+ i 1)])
            ((= i 16))
            (foreign-set! 'double a-ptr (* i 8) (vector-ref a i))
            (foreign-set! 'double b-ptr (* i 8) (vector-ref b i)))
        
        ;; Call Rust (pass raw addresses)
        (rust-mat4-mul a-ptr b-ptr fuel (ftype-pointer-address result-ptr))
        
        ;; Extract result
        (let* ([status (ftype-ref mat4-result-t (status) result-ptr)]
               [fuel-out (ftype-ref mat4-result-t (fuel) result-ptr)]
               [c (make-vector 16 0.0)])
              (when (= status 1)
                    (vector-set! c 0 (ftype-ref mat4-result-t (m0) result-ptr))
                    (vector-set! c 1 (ftype-ref mat4-result-t (m1) result-ptr))
                    (vector-set! c 2 (ftype-ref mat4-result-t (m2) result-ptr))
                    (vector-set! c 3 (ftype-ref mat4-result-t (m3) result-ptr))
                    (vector-set! c 4 (ftype-ref mat4-result-t (m4) result-ptr))
                    (vector-set! c 5 (ftype-ref mat4-result-t (m5) result-ptr))
                    (vector-set! c 6 (ftype-ref mat4-result-t (m6) result-ptr))
                    (vector-set! c 7 (ftype-ref mat4-result-t (m7) result-ptr))
                    (vector-set! c 8 (ftype-ref mat4-result-t (m8) result-ptr))
                    (vector-set! c 9 (ftype-ref mat4-result-t (m9) result-ptr))
                    (vector-set! c 10 (ftype-ref mat4-result-t (m10) result-ptr))
                    (vector-set! c 11 (ftype-ref mat4-result-t (m11) result-ptr))
                    (vector-set! c 12 (ftype-ref mat4-result-t (m12) result-ptr))
                    (vector-set! c 13 (ftype-ref mat4-result-t (m13) result-ptr))
                    (vector-set! c 14 (ftype-ref mat4-result-t (m14) result-ptr))
                    (vector-set! c 15 (ftype-ref mat4-result-t (m15) result-ptr)))
              
              ;; Free memory
              (foreign-free (ftype-pointer-address result-ptr))
              (foreign-free a-ptr)
              (foreign-free b-ptr)
              
              (list status c fuel-out))))

;;; ====
;;; Test Data
;;; ====

(define identity-mat
  (vector 1.0 0.0 0.0 0.0
          0.0 1.0 0.0 0.0
          0.0 0.0 1.0 0.0
          0.0 0.0 0.0 1.0))

(define test-mat-a
  (vector 1.0 2.0 3.0 4.0
          5.0 6.0 7.0 8.0
          9.0 10.0 11.0 12.0
          13.0 14.0 15.0 16.0))

(define test-mat-b
  (vector 17.0 18.0 19.0 20.0
          21.0 22.0 23.0 24.0
          25.0 26.0 27.0 28.0
          29.0 30.0 31.0 32.0))

;;; ====
;;; Timing Utilities
;;; ====

(define (time-it name iterations thunk)
  (collect)  ;; GC before timing
  (let* ([start (current-time)]
         [_ (let loop ([i iterations])
                 (when (> i 0)
                       (thunk)
                       (loop (- i 1))))]
         [end (current-time)]
         [elapsed-ns (+ (* (- (time-second end) (time-second start)) 1000000000)
                        (- (time-nanosecond end) (time-nanosecond start)))]
         [elapsed-ms (/ elapsed-ns 1000000.0)]
         [ns-per-call (/ elapsed-ns iterations)])
        (printf "~a: ~,2fms total, ~,0fns/call\n" name elapsed-ms ns-per-call)
        ns-per-call))

;;; ====
;;; Correctness Test
;;; ====

(display "1. Correctness Test\n")

(let* ([rust-result (rust-mat4-mul-call test-mat-a identity-mat 10000)]
       [scheme-result (scheme-mat4-mul test-mat-a identity-mat)])
      (display "   A * I (Rust):   ")
      (display (cadr rust-result))
      (newline)
      (display "   A * I (Scheme): ")
      (display scheme-result)
      (newline)
      
      ;; Check they match
      (let ([rust-c (cadr rust-result)])
           (let check ([i 0] [ok #t])
                (if (= i 16)
                    (if ok
                        (display "   MATCH: Rust and Scheme agree\n")
                        (display "   MISMATCH!\n"))
                    (check (+ i 1)
                           (and ok (< (abs (- (vector-ref rust-c i)
                                              (vector-ref scheme-result i)))
                                      1e-10)))))))

;; Also test A * B
(let* ([rust-result (rust-mat4-mul-call test-mat-a test-mat-b 10000)]
       [scheme-result (scheme-mat4-mul test-mat-a test-mat-b)])
      (display "   A * B (Rust):   ")
      (display (cadr rust-result))
      (newline)
      (display "   A * B (Scheme): ")
      (display scheme-result)
      (newline))

;;; ====
;;; Performance Benchmark
;;; ====

(display "\n2. Performance Benchmark\n")

(define iterations 100000)
(printf "   Running ~a iterations each...\n\n" iterations)

;; Benchmark Rust FFI (includes marshaling overhead)
(define rust-ns
  (time-it "   Rust FFI (full overhead)" iterations
           (lambda () (rust-mat4-mul-call test-mat-a test-mat-b 10000))))

;; Benchmark Scheme
(define scheme-ns
  (time-it "   Scheme native            " iterations
           (lambda () (scheme-mat4-mul test-mat-a test-mat-b))))

;; Calculate speedup
(newline)
(printf "   Speedup: ~,2fx\n" (/ scheme-ns rust-ns))
(printf "   FFI overhead: ~,0fns per call\n" rust-ns)

;;; ====
;;; Analysis
;;; ====

(display "\n3. Analysis\n")
(display "   Mat4 multiply: 64 muls + 48 adds = 112 ops\n")
(display "   FFI overhead estimate: ~200-500ns\n")
(printf "   Measured Rust: ~,0fns/call\n" rust-ns)
(printf "   Measured Scheme: ~,0fns/call\n" scheme-ns)

(if (> (/ scheme-ns rust-ns) 1.0)
    (display "\n   RESULT: Layer 2 FFI WINS for mat4 multiply!\n")
    (display "\n   RESULT: FFI overhead still dominates - need more ops\n"))

;;; ====
;;; Batch Transform Benchmark (where Layer 2 really shines)
;;; ====

(display "\n4. Batch Transform Benchmark\n")
(display "   (This is where FFI really wins - amortize overhead)\n\n")

;; Simulate batch by doing N sequential transforms
(define (scheme-batch-transform mat points)
  (map (lambda (p)
               (let ([x (vector-ref p 0)]
                     [y (vector-ref p 1)]
                     [z (vector-ref p 2)]
                     [w (vector-ref p 3)])
                    (vector
                     (+ (* (vector-ref mat 0) x) (* (vector-ref mat 1) y)
                        (* (vector-ref mat 2) z) (* (vector-ref mat 3) w))
                     (+ (* (vector-ref mat 4) x) (* (vector-ref mat 5) y)
                        (* (vector-ref mat 6) z) (* (vector-ref mat 7) w))
                     (+ (* (vector-ref mat 8) x) (* (vector-ref mat 9) y)
                        (* (vector-ref mat 10) z) (* (vector-ref mat 11) w))
                     (+ (* (vector-ref mat 12) x) (* (vector-ref mat 13) y)
                        (* (vector-ref mat 14) z) (* (vector-ref mat 15) w)))))
       points))

;; Generate test points
(define (make-test-points n)
  (let loop ([i 0] [pts '()])
       (if (= i n)
           pts
           (loop (+ i 1)
                 (cons (vector (exact->inexact i)
                               (exact->inexact (* i 2))
                               (exact->inexact (* i 3))
                               1.0)
                       pts)))))

(define batch-size 1000)
(define batch-iterations 1000)
(define test-points (make-test-points batch-size))

(printf "   Transforming ~a points, ~a iterations\n\n" batch-size batch-iterations)

(define scheme-batch-ns
  (time-it "   Scheme batch     " batch-iterations
           (lambda () (scheme-batch-transform test-mat-a test-points))))

;; For comparison: same work as N individual mat-vec multiplies
(define scheme-individual-ns
  (time-it "   Scheme individual" batch-iterations
           (lambda ()
                   (do ([i 0 (+ i 1)])
                       ((= i batch-size))
                       (scheme-mat4-mul test-mat-a identity-mat)))))

(printf "\n   Per-point cost (batch): ~,0fns\n" (/ scheme-batch-ns batch-size))
(printf "   Per-point cost (individual muls): ~,0fns\n" (/ scheme-individual-ns batch-size))

(display "\n   NOTE: Rust batch (fold_mat4_transform_points) would do all\n")
(display "         1000 transforms in a single FFI call, amortizing\n")
(display "         the ~200-500ns overhead across all points.\n")

;;; ====
;;; 5. Rust Batch Transform Benchmark
;;; ====

(display "\n5. Rust Batch Transform Benchmark\n")

;; Bind fold_mat4_transform_points
;; Signature: fn(m: *const f64, points_in: *const f64, n: u64,
;;               points_out: *mut f64, fuel_in: u64, fuel_out: *mut u64, status: *mut u8)
(define rust-mat4-transform-points
  (foreign-procedure "fold_mat4_transform_points"
                     (void* void* unsigned-64 void* unsigned-64 void* void*)
                     void))

(define (rust-batch-transform mat points)
  (let* ([n (length points)]
         ;; Allocate buffers
         [mat-ptr (foreign-alloc (* 16 8))]
         [in-ptr (foreign-alloc (* 4 n 8))]
         [out-ptr (foreign-alloc (* 4 n 8))]
         [fuel-out-ptr (foreign-alloc 8)]
         [status-ptr (foreign-alloc 1)])
        
        ;; Copy matrix
        (do ([i 0 (+ i 1)])
            ((= i 16))
            (foreign-set! 'double mat-ptr (* i 8) (vector-ref mat i)))
        
        ;; Copy input points
        (let copy-pts ([pts points] [idx 0])
             (when (pair? pts)
                   (let ([p (car pts)])
                        (foreign-set! 'double in-ptr (* idx 8) (vector-ref p 0))
                        (foreign-set! 'double in-ptr (* (+ idx 1) 8) (vector-ref p 1))
                        (foreign-set! 'double in-ptr (* (+ idx 2) 8) (vector-ref p 2))
                        (foreign-set! 'double in-ptr (* (+ idx 3) 8) (vector-ref p 3)))
                   (copy-pts (cdr pts) (+ idx 4))))
        
        ;; Call Rust
        (rust-mat4-transform-points mat-ptr in-ptr n out-ptr 1000000 fuel-out-ptr status-ptr)
        
        ;; Extract status
        (let ([status (foreign-ref 'unsigned-8 status-ptr 0)])
             ;; Free memory
             (foreign-free mat-ptr)
             (foreign-free in-ptr)
             (foreign-free out-ptr)
             (foreign-free fuel-out-ptr)
             (foreign-free status-ptr)
             status)))

;; Benchmark Rust batch transform
(define rust-batch-ns
  (time-it "   Rust batch (1000 pts)" batch-iterations
           (lambda () (rust-batch-transform test-mat-a test-points))))

(printf "\n   Per-point cost (Rust batch): ~,0fns\n" (/ rust-batch-ns batch-size))

(let ([rust-per-point (/ rust-batch-ns batch-size)]
      [scheme-per-point (/ scheme-batch-ns batch-size)])
     (printf "   Rust vs Scheme per-point: ~,2fx speedup\n" (/ scheme-per-point rust-per-point)))

;;; ====
;;; 6. Pre-allocated Buffer Benchmark (isolate FFI call cost)
;;; ====

(display "\n6. Pre-allocated Buffer Benchmark\n")
(display "   (Isolates FFI call from marshaling overhead)\n\n")

;; Pre-allocate all buffers once
(define cached-mat-ptr (foreign-alloc (* 16 8)))
(define cached-in-ptr (foreign-alloc (* 4 batch-size 8)))
(define cached-out-ptr (foreign-alloc (* 4 batch-size 8)))
(define cached-fuel-out-ptr (foreign-alloc 8))
(define cached-status-ptr (foreign-alloc 1))

;; Copy matrix once
(do ([i 0 (+ i 1)])
    ((= i 16))
    (foreign-set! 'double cached-mat-ptr (* i 8) (vector-ref test-mat-a i)))

;; Copy points once
(let copy-pts ([pts test-points] [idx 0])
     (when (pair? pts)
           (let ([p (car pts)])
                (foreign-set! 'double cached-in-ptr (* idx 8) (vector-ref p 0))
                (foreign-set! 'double cached-in-ptr (* (+ idx 1) 8) (vector-ref p 1))
                (foreign-set! 'double cached-in-ptr (* (+ idx 2) 8) (vector-ref p 2))
                (foreign-set! 'double cached-in-ptr (* (+ idx 3) 8) (vector-ref p 3)))
           (copy-pts (cdr pts) (+ idx 4))))

;; Benchmark JUST the FFI call (no alloc/copy)
(define rust-cached-ns
  (time-it "   Rust (cached buffers)" batch-iterations
           (lambda ()
                   (rust-mat4-transform-points
                    cached-mat-ptr
                    cached-in-ptr
                    batch-size
                    cached-out-ptr
                    1000000
                    cached-fuel-out-ptr
                    cached-status-ptr))))

(printf "\n   Per-point cost (cached): ~,0fns\n" (/ rust-cached-ns batch-size))
(printf "   Speedup vs Scheme batch: ~,2fx\n" (/ scheme-batch-ns rust-cached-ns))

;; Cleanup
(foreign-free cached-mat-ptr)
(foreign-free cached-in-ptr)
(foreign-free cached-out-ptr)
(foreign-free cached-fuel-out-ptr)
(foreign-free cached-status-ptr)

(display "\n=== Layer 2 Summary ===\n")
(printf "   Single mat4 mul: Scheme wins (~,2fx faster)\n" (/ rust-ns scheme-ns))
(printf "   Batch 1000 points: ")
(if (< (/ rust-batch-ns batch-size) (/ scheme-batch-ns batch-size))
    (printf "Rust wins (~,2fx faster per point)\n" (/ (/ scheme-batch-ns batch-size) (/ rust-batch-ns batch-size)))
    (printf "Scheme wins (~,2fx faster per point)\n" (/ (/ rust-batch-ns batch-size) (/ scheme-batch-ns batch-size))))

(display "\n=== Benchmark Complete ===\n")
