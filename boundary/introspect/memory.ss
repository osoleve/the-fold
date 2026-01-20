(load "core/base/prelude.ss")

(doc 'module 'memory)
(doc 'description "Memory measurement and analysis for operations and allocations")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Chez-specific memory primitives: bytes-allocated, current-memory-bytes, statistics")

(doc 'section 'memory-snapshot)

(define-record-type memory-snapshot
  (fields
   bytes-allocated   ; Total bytes allocated since startup
   bytes-in-use      ; Approximate bytes currently live
   gc-count          ; Number of garbage collections so far
   timestamp-ns))    ; Nanosecond timestamp when snapshot was taken

(define (current-memory-snapshot)
  (doc 'description "Capture current memory state snapshot")
  (let ([stats (statistics)])
       (make-memory-snapshot
        (bytes-allocated)
        (current-memory-bytes)
        (+ (sstats-gc-count stats))
        (let ([t (current-time 'time-monotonic)])
             (+ (* (time-second t) 1000000000)
                (time-nanosecond t))))))

(doc 'section 'memory-delta)

(define-record-type memory-delta
  (fields
   bytes-allocated   ; Bytes allocated during operation
   bytes-freed       ; Bytes freed (may be negative if growth)
   gc-triggered))    ; Number of GCs triggered during operation

(define (compute-memory-delta before after)
  (doc 'description "Compute memory delta between two snapshots")
  (make-memory-delta
   (- (memory-snapshot-bytes-allocated after)
      (memory-snapshot-bytes-allocated before))
   (- (memory-snapshot-bytes-in-use before)
      (memory-snapshot-bytes-in-use after))
   (- (memory-snapshot-gc-count after)
      (memory-snapshot-gc-count before))))

(doc 'section 'memory-measurement)

(define-record-type memory-result
  (fields
   before      ; MemorySnapshot before execution
   after       ; MemorySnapshot after execution
   delta       ; MemoryDelta
   value))     ; Result of the computation

(define (memory-thunk thunk)
  (doc 'description "Execute thunk and measure memory impact")
  (let* ([before (current-memory-snapshot)]
         [result (thunk)]
         [after (current-memory-snapshot)]
         [delta (compute-memory-delta before after)])
        (make-memory-result before after delta result)))

(define (gc-and-measure)
  (doc 'description "Force garbage collection then measure")
  (doc 'note "Useful for getting a clean baseline")
  (collect)
  (collect)  ; Second pass for generational GC
  (current-memory-snapshot))

(doc 'section 'memory-benchmark)

(define-record-type memory-stats
  (fields
   min-allocated     ; Minimum bytes allocated per iteration
   max-allocated     ; Maximum bytes allocated per iteration
   mean-allocated    ; Mean allocation
   total-allocated   ; Total bytes allocated across all iterations
   gc-count))        ; Total GCs triggered

;;; compute-memory-stats : (List MemoryDelta) → MemoryStats
(define (compute-memory-stats deltas)
  (let* ([allocations (map memory-delta-bytes-allocated deltas)]
         [n (length allocations)]
         [sorted (list-sort < allocations)]
         [total (fold-left + 0 allocations)]
         [gc-total (fold-left + 0 (map memory-delta-gc-triggered deltas))])
        (make-memory-stats
         (if (> n 0) (car sorted) 0)
         (if (> n 0) (list-ref sorted (- n 1)) 0)
         (if (> n 0) (/ total n) 0)
         total
         gc-total)))

(define-record-type memory-benchmark-result
  (fields
   name          ; Benchmark name
   iterations    ; Number of iterations
   results       ; List of MemoryResult
   stats))       ; Computed MemoryStats

;;; memory-benchmark : String × Nat × (→ A) → MemoryBenchmarkResult
;;; Run a memory benchmark for n iterations.
(define (memory-benchmark name iterations thunk)
  (let* ([results (let loop ([i 0] [acc '()])
                       (if (>= i iterations)
                           (reverse acc)
                           (begin
                            ;; Optional: GC between iterations for cleaner data
                            ;; (collect)
                            (loop (+ i 1)
                                  (cons (memory-thunk thunk) acc)))))]
         [deltas (map memory-result-delta results)]
         [stats (compute-memory-stats deltas)])
        (make-memory-benchmark-result name iterations results stats)))

;;; memory-benchmark-with-gc : String × Nat × (→ A) → MemoryBenchmarkResult
;;; Run memory benchmark with GC between iterations for cleaner measurements.
(define (memory-benchmark-with-gc name iterations thunk)
  (let* ([results (let loop ([i 0] [acc '()])
                       (if (>= i iterations)
                           (reverse acc)
                           (begin
                            (collect)
                            (loop (+ i 1)
                                  (cons (memory-thunk thunk) acc)))))]
         [deltas (map memory-result-delta results)]
         [stats (compute-memory-stats deltas)])
        (make-memory-benchmark-result name iterations results stats)))

(doc 'section 'formatting)

(define (format-bytes bytes)
  (doc 'description "Format bytes in human-readable form (KB, MB, GB)")
  (cond
   [(>= (abs bytes) (* 1024 1024 1024))
    (format "~,2fGB" (/ bytes (* 1024.0 1024.0 1024.0)))]
   [(>= (abs bytes) (* 1024 1024))
    (format "~,2fMB" (/ bytes (* 1024.0 1024.0)))]
   [(>= (abs bytes) 1024)
    (format "~,2fKB" (/ bytes 1024.0))]
   [else (format "~aB" bytes)]))

;;; format-memory-snapshot : MemorySnapshot → String
(define (format-memory-snapshot snap)
  (format "allocated: ~a, in-use: ~a, gc-count: ~a"
          (format-bytes (memory-snapshot-bytes-allocated snap))
          (format-bytes (memory-snapshot-bytes-in-use snap))
          (memory-snapshot-gc-count snap)))

;;; format-memory-delta : MemoryDelta → String
(define (format-memory-delta delta)
  (format "allocated: ~a, freed: ~a, gc: ~a"
          (format-bytes (memory-delta-bytes-allocated delta))
          (format-bytes (memory-delta-bytes-freed delta))
          (memory-delta-gc-triggered delta)))

;;; format-memory-stats : MemoryStats → String
(define (format-memory-stats stats)
  (format "min: ~a, max: ~a, mean: ~a, total: ~a, gc: ~a"
          (format-bytes (memory-stats-min-allocated stats))
          (format-bytes (memory-stats-max-allocated stats))
          (format-bytes (inexact->exact (round (memory-stats-mean-allocated stats))))
          (format-bytes (memory-stats-total-allocated stats))
          (memory-stats-gc-count stats)))

;;; format-memory-benchmark : MemoryBenchmarkResult → String
(define (format-memory-benchmark mbr)
  (format "~a (~a iterations):\n  ~a"
          (memory-benchmark-result-name mbr)
          (memory-benchmark-result-iterations mbr)
          (format-memory-stats (memory-benchmark-result-stats mbr))))

;;; ====
;;; Display Utilities
;;; ====

;;; print-memory-snapshot : MemorySnapshot → void
(define (print-memory-snapshot snap)
  (display (format-memory-snapshot snap))
  (newline))

;;; print-memory-result : MemoryResult → void
(define (print-memory-result mr)
  (display "Memory measurement:\n")
  (display (format "  Before: ~a\n" (format-memory-snapshot (memory-result-before mr))))
  (display (format "  After:  ~a\n" (format-memory-snapshot (memory-result-after mr))))
  (display (format "  Delta:  ~a\n" (format-memory-delta (memory-result-delta mr)))))

;;; print-memory-benchmark : MemoryBenchmarkResult → void
(define (print-memory-benchmark mbr)
  (display (format-memory-benchmark mbr))
  (newline))

(doc 'section 'allocation-tracking)

(define-record-type allocation-profile
  (fields
   bytes-allocated
   gc-real-time-ns     ; Real time spent in GC
   gc-cpu-time-ns      ; CPU time spent in GC
   gc-count
   gc-bytes-reclaimed))

(define (track-allocations thunk)
  (let* ([stats-before (statistics)]
         [alloc-before (bytes-allocated)]
         [result (thunk)]
         [alloc-after (bytes-allocated)]
         [stats-after (statistics)])
        (values
         result
         (make-allocation-profile
          (- alloc-after alloc-before)
          (- (sstats-gc-real (+ stats-after)) (sstats-gc-real (+ stats-before)))
          (- (sstats-gc-cpu (+ stats-after)) (sstats-gc-cpu (+ stats-before)))
          (- (sstats-gc-count stats-after) (sstats-gc-count stats-before))
          (- (sstats-gc-bytes stats-after) (sstats-gc-bytes stats-before))))))

(doc 'section 'memory-pressure)

(define (allocate-bytes n)
  (doc 'description "Allocate exactly n bytes for testing memory behavior")
  (make-bytevector n 0))

;;; measure-under-pressure : Nat × (→ A) → MemoryResult
;;; Measure thunk's memory behavior after allocating pressure bytes.
;;; Useful for testing behavior under memory pressure.
(define (measure-under-pressure pressure-bytes thunk)
  (let ([_ (allocate-bytes pressure-bytes)])  ; Create pressure
       (collect)  ; Force the pressure into older generation
       (memory-thunk thunk)))
