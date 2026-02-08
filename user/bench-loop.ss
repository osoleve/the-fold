;;; Benchmark: Loop-heavy computation

(load "core/lang/rust-codegen.ss")
(load "boundary/ffi/rust-compile.ss")
(load "boundary/ffi/rust-loader.ss")

(display "=== Loop Computation Benchmark ===\n\n")

;; Create a function that does N iterations of work
;; This amortizes FFI overhead

;; Generate Rust code for: sum from 1 to n (using formula n*(n+1)/2)
(define sum-formula-ir
  '(R-Fn sum_formula ((n i64)) i64
         (R-Call div
                 (R-Call mul (R-Var n)
                         (R-Call add (R-Var n) (R-Literal 1)))
                 (R-Literal 2))))

(display "1. Compiling sum_formula to Rust...\n")
(compile-to-crate "sum_formula" sum-formula-ir)
(rust-build!)

(accel-load!)
(rust-load-fn! "fold_sum_formula" '(i64) 'i64)

;; Scheme equivalent
(define (scheme-sum-formula n)
  (quotient (* n (+ n 1)) 2))

(define (time-it name iterations thunk)
  (let* ([start (current-time)]
         [_ (let loop ([i iterations])
              (when (> i 0)
                (thunk)
                (loop (- i 1))))]
         [end (current-time)]
         [elapsed-ns (+ (* (- (time-second end) (time-second start)) 1000000000)
                        (- (time-nanosecond end) (time-nanosecond start)))]
         [elapsed-ms (/ elapsed-ns 1000000.0)])
    (display name)
    (display ": ")
    (display (round elapsed-ms))
    (display "ms\n")
    elapsed-ms))

(display "\n2. Running benchmarks...\n\n")

;; Test with increasing N to see where FFI wins
(for-each
 (lambda (n)
   (display "--- N = ")
   (display n)
   (display " ---\n")
   (let* ([iterations (quotient 1000000 n)]
          [rust-time (time-it "Rust FFI    " iterations
                              (lambda () (rust-call "fold_sum_formula" n 10000)))]
          [scheme-time (time-it "Scheme      " iterations
                                (lambda () (scheme-sum-formula n)))])
     (display "Ratio: ")
     (display (inexact (/ scheme-time rust-time)))
     (display "x\n\n")))
 '(10 100 1000 10000))

;; The real win: batch operations
(display "=== Batch Operation Simulation ===\n")
(display "(Simulating what a vectorized operation would look like)\n\n")

;; Call Rust once with large n vs Scheme loop
(define big-n 1000000)
(display "Computing sum(1..1000000):\n")

(define rust-batch-time
  (time-it "Rust single call" 1000
           (lambda () (rust-call "fold_sum_formula" big-n 10000))))

(define scheme-batch-time
  (time-it "Scheme function " 1000
           (lambda () (scheme-sum-formula big-n))))

(display "Rust/Scheme ratio: ")
(display (inexact (/ rust-batch-time scheme-batch-time)))
(display "x\n\n")

;; Verify correctness
(display "Correctness check:\n")
(display "  Rust:   ")
(display (rust-call "fold_sum_formula" 100 10000))
(newline)
(display "  Scheme: ")
(display (scheme-sum-formula 100))
(newline)
