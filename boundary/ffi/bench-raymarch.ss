;;; boundary/ffi/bench-raymarch.ss — Raymarching Benchmark: Scheme vs Rust
;;;
;;; Benchmarks raymarching operations before and after Rust acceleration.
;;; Run with: scheme --script boundary/ffi/bench-raymarch.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/mesh-sdf.ss")
(load "lattice/geometry/bvh.ss")
(load "lattice/geometry/raymarch.ss")
(load "boundary/ffi/ffi-core.ss")
(load "boundary/ffi/serialize.ss")
(load "boundary/ffi/bvh-ffi.ss")
(load "boundary/ffi/bvh-cache.ss")
(load "boundary/ffi/raymarch-ffi.ss")

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
;;; Test Ray Generation
;;; ====

(define (make-camera-rays width height fov center)
  ;; Generate rays from camera at (0,0,5) looking at origin
  (let* ([aspect (/ width height)]
         [scale (tan (* fov 0.5 (/ 3.14159 180.0)))]
         [camera-pos (vec3 0 0 5)]
         [rays '()])
        (do ([y 0 (+ y 1)])
            ((>= y height) rays)
            (do ([x 0 (+ x 1)])
                ((>= x width))
                (let* ([px (- (* 2.0 (/ (+ x 0.5) width)) 1.0)]
                       [py (- (* 2.0 (/ (+ y 0.5) height)) 1.0)]
                       [dir (vec3-normalize (vec3 (* px aspect scale)
                                                  (* py scale)
                                                  -1.0))])
                      (set! rays (cons (ray3 camera-pos dir) rays)))))))

;;; ====
;;; Benchmark: Pure Scheme Raymarching
;;; ====

(define (bench-scheme-raymarch mesh rays params iterations)
  (display "  Pure Scheme raymarch:   ")
  (let ([result (time-thunk
                 (lambda ()
                         (do ([i 0 (+ i 1)])
                             ((>= i iterations))
                             (for-each (lambda (ray)
                                               (raymarch-mesh mesh ray params))
                                       rays))))])
       (display (cdr result))
       (display " ms")
       (newline)
       (cdr result)))

;;; ====
;;; Benchmark: Rust-Accelerated Raymarching
;;; ====

(define *benchmark-fuel* 1000000)  ; Plenty of fuel for benchmarks

(define (bench-rust-raymarch rust-handle rays params iterations)
  (display "  Rust accel raymarch:    ")
  (let ([max-steps (raymarch-params-max-steps params)]
        [max-dist (raymarch-params-max-distance params)]
        [threshold (raymarch-params-hit-threshold params)])
       (let ([result (time-thunk
                      (lambda ()
                              (do ([i 0 (+ i 1)])
                                  ((>= i iterations))
                                  (for-each (lambda (ray)
                                                    (rust-raymarch-mesh/raw
                                                     rust-handle
                                                     (ray3-origin ray)
                                                     (ray3-direction ray)
                                                     max-steps
                                                     max-dist
                                                     threshold
                                                     *benchmark-fuel*))
                                            rays))))])
            (display (cdr result))
            (display " ms")
            (newline)
            (cdr result))))

;;; ====
;;; Main Benchmark
;;; ====

(define (run-raymarch-benchmark)
  (display "====\n")
  (display "Raymarching Benchmark: Scheme vs Rust Acceleration\n")
  (display "====\n\n")
  
  ;; Load Rust library
  (display "Loading Rust acceleration library...\n")
  (unless (accel-load!)
          (display "ERROR: Could not load Rust library!\n")
          (display "Build with: cd boundary/ffi/rust-accel && cargo build --release\n")
          (exit 1))
  (bind-bvh-procedures!)
  (bind-raymarch-procedures!)
  (display "Rust library loaded. Version: ")
  (display (accel-version))
  (newline)
  (newline)
  
  ;; Default raymarch params
  (let ([params (raymarch-params 100 100.0 0.001)])
       
       ;; Small mesh (cube - 12 triangles)
       (display "--- Small Mesh (12 triangles) ---\n")
       (let* ([mesh (make-mesh-cube 1.0)]
              [bvh (mesh-bvh mesh)]
              [rust-handle (scheme-bvh->rust-handle bvh)]
              [rays (make-camera-rays 8 8 60 (vec3 0 0 0))]
              [iterations 5])
             (display (format "  Mesh: ~a triangles, ~a rays per frame\n"
                              (mesh-triangle-count mesh) (length rays)))
             (let ([scheme-time (bench-scheme-raymarch mesh rays params iterations)]
                   [rust-time (bench-rust-raymarch rust-handle rays params iterations)])
                  (display (format "  Speedup: ~ax\n" (/ scheme-time rust-time))))
             (free-rust-bvh! rust-handle))
       
       (newline)
       
       ;; Medium mesh (icosphere subdivision 1 - ~80 triangles)
       (display "--- Medium Mesh (~80 triangles) ---\n")
       (let* ([mesh (make-mesh-sphere-ico 1.0 1)]
              [bvh (mesh-bvh mesh)]
              [rust-handle (scheme-bvh->rust-handle bvh)]
              [rays (make-camera-rays 8 8 60 (vec3 0 0 0))]
              [iterations 3])
             (display (format "  Mesh: ~a triangles, ~a rays per frame\n"
                              (mesh-triangle-count mesh) (length rays)))
             (let ([scheme-time (bench-scheme-raymarch mesh rays params iterations)]
                   [rust-time (bench-rust-raymarch rust-handle rays params iterations)])
                  (display (format "  Speedup: ~ax\n" (/ scheme-time rust-time))))
             (free-rust-bvh! rust-handle))
       
       (newline)
       
       ;; Large mesh (icosphere subdivision 2 - ~320 triangles)
       (display "--- Large Mesh (~320 triangles) ---\n")
       (let* ([mesh (make-mesh-sphere-ico 1.0 2)]
              [bvh (mesh-bvh mesh)]
              [rust-handle (scheme-bvh->rust-handle bvh)]
              [rays (make-camera-rays 8 8 60 (vec3 0 0 0))]
              [iterations 2])
             (display (format "  Mesh: ~a triangles, ~a rays per frame\n"
                              (mesh-triangle-count mesh) (length rays)))
             (let ([scheme-time (bench-scheme-raymarch mesh rays params iterations)]
                   [rust-time (bench-rust-raymarch rust-handle rays params iterations)])
                  (display (format "  Speedup: ~ax\n" (/ scheme-time rust-time))))
             (free-rust-bvh! rust-handle))
       
       (newline)
       
       ;; Verify correctness
       (display "--- Correctness Check ---\n")
       (let* ([mesh (make-mesh-cube 1.0)]
              [bvh (mesh-bvh mesh)]
              [rust-handle (scheme-bvh->rust-handle bvh)]
              [ray (ray3 (vec3 0 0 3) (vec3 0 0 -1))]
              [scheme-result (raymarch-mesh mesh ray params)]
              [rust-result (rust-raymarch-mesh/raw rust-handle
                                                   (vec3 0 0 3)
                                                   (vec3 0 0 -1)
                                                   100 100.0 0.001
                                                   *benchmark-fuel*)])
             (display "  Scheme result: ")
             (if scheme-result
                 (display (format "HIT at t=~a\n" (cadr scheme-result)))
                 (display "MISS\n"))
             (display "  Rust result:   ")
             (if (and (pair? rust-result) (eq? (car rust-result) 'ok))
                 (let* ([data (cadr rust-result)]
                        [t (caddr data)])
                       (display (format "HIT at t=~a\n" t)))
                 (display (format "~a\n" rust-result)))
             
             ;; Verify t-values are close
             (when (and scheme-result (eq? (car rust-result) 'ok))
                   (let* ([scheme-t (cadr scheme-result)]
                          [rust-t (caddr (cadr rust-result))]
                          [diff (abs (- scheme-t rust-t))])
                         (display (format "  Difference: ~a (should be < 0.1)\n" diff))
                         (if (< diff 0.1)
                             (display "  PASS: Results match!\n")
                             (display "  WARN: Results differ significantly\n"))))
             
             (free-rust-bvh! rust-handle)))
  
  (newline)
  
  ;; Cleanup
  (display "Cleaning up cache...\n")
  (clear-cache!)
  
  (display "\n====\n")
  (display "Benchmark complete.\n")
  (display "====\n"))

;; Run if executed as script
(run-raymarch-benchmark)
