;;; user/bench-rust-vs-scheme.ss — Benchmark Rust FFI vs Scheme interpreter

(load "core/lang/rust-codegen.ss")
(load "boundary/ffi/rust-compile.ss")
(load "boundary/ffi/rust-loader.ss")
(load "core/lang/eval.ss")

(display "=== Rust vs Scheme Benchmark ===\n\n")

;; Ensure library is loaded
(accel-load!)

;; Load the functions we generated
(rust-load-fn! "fold_multiply_add" '(i64 i64 i64) 'i64)
(rust-load-fn! "fold_distance" '(f64 f64 f64 f64) 'f64)
(rust-load-fn! "fold_sum5" '(i64 i64 i64 i64 i64) 'i64)

;; Benchmark helper
(define (time-it name iterations thunk)
  (let* ([start (current-time)]
         [_ (let loop ([i iterations])
              (when (> i 0)
                (thunk)
                (loop (- i 1))))]
         [end (current-time)]
         [elapsed-ns (+ (* (- (time-second end) (time-second start)) 1000000000)
                        (- (time-nanosecond end) (time-nanosecond start)))]
         [elapsed-ms (/ elapsed-ns 1000000.0)]
         [per-call-ns (/ elapsed-ns iterations)])
    (display name)
    (display ": ")
    (display (round elapsed-ms))
    (display "ms total, ")
    (display (round per-call-ns))
    (display "ns/call\n")
    per-call-ns))

(define iterations 100000)

(display "Iterations: ")
(display iterations)
(display "\n\n")

;; Benchmark 1: multiply_add (a*b + c) - Rust FFI vs The Fold eval
(display "--- multiply_add(3, 4, 5) ---\n")
(define rust-ma-time
  (time-it "Rust FFI    " iterations
           (lambda () (rust-call "fold_multiply_add" 3 4 5 1000))))

(define fold-ma-time
  (time-it "Fold eval   " iterations
           (lambda () (eval-expr '(+ (* 3 4) 5) '() 1000))))

(display "Speedup: ")
(display (inexact (/ fold-ma-time rust-ma-time)))
(display "x\n\n")

;; Benchmark 2: distance (sqrt of sum of squares)
(display "--- distance(0, 0, 3, 4) ---\n")
(define rust-dist-time
  (time-it "Rust FFI    " iterations
           (lambda () (rust-call "fold_distance" 0.0 0.0 3.0 4.0 1000))))

(define fold-dist-time
  (time-it "Fold eval   " iterations
           (lambda () (eval-expr '(sqrt (+ (* (- 3.0 0.0) (- 3.0 0.0))
                                           (* (- 4.0 0.0) (- 4.0 0.0))))
                                 '() 1000))))

(display "Speedup: ")
(display (inexact (/ fold-dist-time rust-dist-time)))
(display "x\n\n")

;; Benchmark 3: sum5 (variadic add)
(display "--- sum5(1, 2, 3, 4, 5) ---\n")
(define rust-sum-time
  (time-it "Rust FFI    " iterations
           (lambda () (rust-call "fold_sum5" 1 2 3 4 5 1000))))

(define fold-sum-time
  (time-it "Fold eval   " iterations
           (lambda () (eval-expr '(+ 1 2 3 4 5) '() 1000))))

(display "Speedup: ")
(display (inexact (/ fold-sum-time rust-sum-time)))
(display "x\n\n")

;; Benchmark 4: Raw Chez Scheme (no eval, no FFI - baseline)
(display "--- Raw Chez Scheme (no eval/FFI) ---\n")
(define rust-raw-time
  (time-it "Rust FFI    " iterations
           (lambda () (rust-call "fold_multiply_add" 3 4 0 1000))))

(define chez-raw-time
  (time-it "Chez native " iterations
           (lambda () (+ (* 3 4) 0))))

(display "Overhead: ")
(display (inexact (/ rust-raw-time chez-raw-time)))
(display "x (FFI call overhead vs native)\n\n")

(display "=== Summary ===\n")
(display "Rust FFI accelerates The Fold's eval-based interpreter.\n")
(display "Raw Chez Scheme is faster than FFI due to zero crossing overhead.\n")
(display "Best use case: hot paths in interpreted/verified code.\n")
