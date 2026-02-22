;;; lattice/linalg/bench-blocked.ss — Benchmarks for Blocked/Parallel Matrix Operations

(load "boundary/parallel/matrix-parallel.ss")
(load "lattice/data/sort.ss")

;;; ====
;;; Benchmark Harness
;;; ====

(define (time-ms thunk)
  (let ([t0 (current-time 'time-monotonic)])
    (thunk)
    (let ([t1 (current-time 'time-monotonic)])
      (+ (* 1000 (- (time-second t1) (time-second t0)))
         (/ (- (time-nanosecond t1) (time-nanosecond t0)) 1000000.0)))))

(define (bench label thunk . args)
  (let ([runs (if (null? args) 3 (car args))])
  (let loop ([i 0] [times '()])
    (if (= i runs)
        (let* ([sorted (sort-by < times)]
               [median (list-ref sorted (quotient runs 2))])
          (printf "  ~a: ~,1fms (median of ~a runs)\n" label median runs)
          median)
        (begin
          (collect) ; GC before each run
          (loop (+ i 1) (cons (time-ms thunk) times)))))))

(define (make-test-matrix rows cols)
  (let ([data (make-vector (* rows cols) 0)])
    (do ([i 0 (+ i 1)])
        ((= i rows))
      (do ([j 0 (+ j 1)])
          ((= j cols))
        (vector-set! data (+ (* i cols) j) (+ (* i cols) j 1.0))))
    (list 'matrix rows cols data)))

;;; ====
;;; Multiplication Benchmarks
;;; ====

(define (bench-mul-at-size n)
  (printf "\n=== Matrix Multiply ~ax~a ===\n" n n)
  (let ([a (make-test-matrix n n)]
        [b (make-test-matrix n n)])
    (let ([t-naive    (bench "naive (i-k-j)"  (lambda () (matrix-mul a b)))]
          [t-blocked  (bench "blocked (bs=32)" (lambda () (matrix-mul-blocked a b)))]
          [t-strassen (if (>= n 16)
                         (bench "strassen (th=64)" (lambda () (matrix-mul-strassen a b 64)))
                         #f)]
          [t-parallel (bench "parallel"        (lambda () (parallel-matrix-mul a b)))])
      (printf "  speedup blocked/naive: ~,2fx\n" (/ t-naive t-blocked))
      (when t-strassen
        (printf "  speedup strassen/naive: ~,2fx\n" (/ t-naive t-strassen)))
      (printf "  speedup parallel/naive: ~,2fx\n" (/ t-naive t-parallel)))))

(printf "======= Blocked/Parallel Matrix Benchmarks =========\n")
(printf "\nWorker count: ~a\n" (pool-worker-count (ensure-pool!)))

(for-each bench-mul-at-size '(32 64 128 256))

;;; ====
;;; Transpose Benchmarks
;;; ====

(define (bench-transpose-at-size n)
  (printf "\n=== Matrix Transpose ~ax~a ===\n" n n)
  (let ([m (make-test-matrix n n)])
    (let ([t-naive   (bench "naive"     (lambda () (matrix-transpose m)))]
          [t-blocked (bench "blocked"   (lambda () (matrix-transpose-blocked m)))]
          [t-parallel (bench "parallel" (lambda () (parallel-matrix-transpose m)))])
      (printf "  speedup blocked/naive: ~,2fx\n" (/ t-naive t-blocked))
      (printf "  speedup parallel/naive: ~,2fx\n" (/ t-naive t-parallel)))))

(for-each bench-transpose-at-size '(64 128 256 512))

;;; ====
;;; Decomposition Benchmarks
;;; ====

(define (make-dd-matrix n)
  (let ([data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (vector-set! data (+ (* i n) j)
          (if (= i j)
              (+ n 1.0)
              (/ 1.0 (+ 1.0 (abs (- i j))))))))
    (list 'matrix n n data)))

(define (bench-decomp-at-size n)
  (printf "\n=== Decompositions ~ax~a ===\n" n n)
  (let ([a (make-dd-matrix n)])
    ;; Load matrix-decomp for sequential baseline
    (bench "parallel-lu" (lambda () (parallel-lu a)))
    (bench "parallel-qr" (lambda () (parallel-qr a)))))

(for-each bench-decomp-at-size '(64 128 256))

;;; ====
;;; Scaling Analysis
;;; ====

(printf "\n=== Scaling: Parallel Multiply at 256x256 ===\n")
(let ([a (make-test-matrix 256 256)]
      [b (make-test-matrix 256 256)])
  (let ([t-seq (bench "sequential blocked" (lambda () (matrix-mul-blocked a b)))])
    (printf "  (parallel speedup measured against sequential blocked)\n")
    (let ([t-par (bench "parallel" (lambda () (parallel-matrix-mul a b)))])
      (printf "  parallel efficiency: ~,2fx speedup on ~a workers\n"
              (/ t-seq t-par)
              (pool-worker-count (ensure-pool!))))))

(printf "\nDone.\n")
(pool-shutdown-global!)
