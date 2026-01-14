;;; core/autodiff/profiling.ss --- Performance Profiling for Autodiff
;;;
;;; Provides utilities for profiling and debugging autodiff performance:
;;;   - tape-stats: Operation counts and tape size
;;;   - graph-stats: Node/edge counts, depth
;;;   - time-gradient: Measure forward/backward pass time
;;;   - memory-estimate: Estimate memory usage
;;;
;;; This is Core code: pure (except for timing), assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - autodiff/comp-graph.ss
;;;   - autodiff/reverse-diff.ss

(load "core/base/prelude.ss")
(load "core/autodiff/comp-graph.ss")
(load "core/autodiff/reverse-diff.ss")

;;; ====
;;; Tape Statistics
;;; ====

;;; tape-stats : MutableTape → Stats
(define (tape-stats tape)
  (let* ([entries (reverse-tape-entries tape)]
         [size (length entries)]
         [op-counts (make-hashtable symbol-hash eq?)])
        ;; Count operations by type
        (for-each
         (lambda (entry)
                 (let* ([op-name (cadr entry)]
                        [current (hashtable-ref op-counts op-name 0)])
                       (hashtable-set! op-counts op-name (+ current 1))))
         entries)
        ;; Build result
        (let-values ([(keys vals) (hashtable-entries op-counts)])
                    (list (cons 'size size)
                          (cons 'ops (map cons
                                          (vector->list keys)
                                          (vector->list vals)))))))

;;; tape-size : MutableTape → Nat
(define (tape-size tape)
  (length (reverse-tape-entries tape)))

;;; tape-op-count : MutableTape × Symbol → Nat
(define (tape-op-count tape op-name)
  (length (filter (lambda (entry) (eq? (cadr entry) op-name))
                  (reverse-tape-entries tape))))

;;; ====
;;; Computational Graph Statistics
;;; ====

;;; graph-stats : CompGraph → Stats
(define (graph-stats g)
  (let* ([nodes (comp-graph-nodes g)]
         [node-count (length nodes)]
         [edge-count (graph-edge-count g)]
         [depth (graph-depth g)]
         [var-names (graph-variables g)]
         [op-types (graph-operations g)])
        (list (cons 'nodes node-count)
              (cons 'edges edge-count)
              (cons 'depth depth)
              (cons 'variables var-names)
              (cons 'op-distribution (tally-ops op-types)))))

;;; graph-edge-count : CompGraph → Nat
(define (graph-edge-count g)
  (graph-fold-nodes
   (lambda (acc id node)
           (+ acc (length (node-inputs node))))
   0 g))

;;; graph-depth : CompGraph → Nat
(define (graph-depth g)
  (if (null? (comp-graph-nodes g))
      0
      (let ([depths (make-hashtable equal-hash equal?)])
           ;; Compute depth for each node (memoized)
           (letrec ([compute-depth
                     (lambda (id)
                             (or (hashtable-ref depths id #f)
                                 (let* ([node (graph-get-node g id)]
                                        [input-ids (extract-input-ids node g)]
                                        [d (if (null? input-ids)
                                               0
                                               (+ 1 (apply max
                                                           (map compute-depth input-ids))))])
                                       (hashtable-set! depths id d)
                                       d)))])
                   ;; Compute depth for all nodes, return max
                   (graph-fold-nodes
                    (lambda (acc id node)
                            (max acc (compute-depth id)))
                    0 g)))))

;;; extract-input-ids : Node × CompGraph → (List NodeId)
(define (extract-input-ids node g)
  (if (node-op? node)
      ;; For op nodes, we need to find the IDs of input nodes
      ;; This is a simplification - in a proper implementation,
      ;; nodes would store IDs directly
      (let ([inputs (node-op-inputs node)])
           (filter-map
            (lambda (input)
                    (find-node-id input g))
            inputs))
      '()))

;;; find-node-id : Node × CompGraph → (Option NodeId)
(define (find-node-id node g)
  (let ([found (find (lambda (entry) (equal? (cdr entry) node))
                     (comp-graph-nodes g))])
       (if found (car found) #f)))

;;; tally-ops : (List Symbol) → (AList Symbol Nat)
(define (tally-ops ops)
  (let ([counts (make-hashtable symbol-hash eq?)])
       (for-each
        (lambda (op)
                (hashtable-set! counts op
                                (+ 1 (hashtable-ref counts op 0))))
        ops)
       (let-values ([(keys vals) (hashtable-entries counts)])
                   (map cons
                        (vector->list keys)
                        (vector->list vals)))))

;;; ====
;;; Timing Utilities
;;; ====

;;; current-microseconds : → Integer
(define (current-microseconds)
  (let ([t (current-time 'time-monotonic)])
       (+ (* (time-second t) 1000000)
          (quotient (time-nanosecond t) 1000))))

;;; time-thunk : (→ α) → (Values α Integer)
(define (time-thunk thunk)
  (let* ([start (current-microseconds)]
         [result (thunk)]
         [end (current-microseconds)])
        (values result (- end start))))

;;; time-gradient : ((Traced ...) → Traced) × (List Number) → TimingResult
(define (time-gradient f args)
  (reset-traced-ids!)
  (let* ([tape (make-reverse-tape)]
         [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
         ;; Time forward pass
         [forward-start (current-microseconds)]
         [result (apply f traced-args)]
         [forward-end (current-microseconds)]
         [forward-time (- forward-end forward-start)])
        (if (traced? result)
            (let* ([val (traced-value result)]
                   [result-id (traced-id result)]
                   [arg-ids (map traced-id traced-args)]
                   ;; Time backward pass
                   [backward-start (current-microseconds)]
                   [grads-table (backward tape result-id 1)]
                   [backward-end (current-microseconds)]
                   [backward-time (- backward-end backward-start)]
                   [grads (map (lambda (id)
                                       (if (= id result-id)
                                           1
                                           (hashtable-ref grads-table id 0)))
                               arg-ids)])
                  (list (cons 'value val)
                        (cons 'gradient grads)
                        (cons 'forward-us forward-time)
                        (cons 'backward-us backward-time)
                        (cons 'total-us (+ forward-time backward-time))
                        (cons 'tape-size (tape-size tape))))
            ;; Constant function
            (list (cons 'value result)
                  (cons 'gradient (map (lambda (_) 0) args))
                  (cons 'forward-us forward-time)
                  (cons 'backward-us 0)
                  (cons 'total-us forward-time)
                  (cons 'tape-size 0)))))

;;; ====
;;; Memory Estimation
;;; ====

;;; Estimated sizes in bytes (Chez Scheme on 64-bit)
(define *ptr-size* 8)       ; pointer
(define *fixnum-size* 8)    ; fixnum
(define *flonum-size* 16)   ; flonum (header + 64-bit double)
(define *pair-size* 16)     ; cons cell (car + cdr pointers)
(define *vector-header* 16) ; vector header

;;; tape-memory-estimate : MutableTape → Nat
(define (tape-memory-estimate tape)
  (let* ([entries (reverse-tape-entries tape)]
         [n (length entries)])
        (if (zero? n)
            0
            ;; Each entry: 4 cons cells (list structure)
            ;; + result-id (fixnum)
            ;; + op-name (symbol, shared)
            ;; + input-ids list (2-3 pairs + fixnums typically)
            ;; + local-grads list (2-3 pairs + flonums typically)
            (let ([entry-overhead (* 4 *pair-size*)]        ; list structure
                  [id-size *fixnum-size*]                   ; result-id
                  [avg-inputs 2]                            ; typical binary op
                  [input-list-size (* 2 (+ *pair-size* *fixnum-size*))]
                  [grad-list-size (* 2 (+ *pair-size* *flonum-size*))]
                  [tape-overhead (* 2 *pair-size*)])        ; tape structure
                 (+ tape-overhead
                    (* n (+ entry-overhead
                            id-size
                            input-list-size
                            grad-list-size)))))))

;;; graph-memory-estimate : CompGraph → Nat
(define (graph-memory-estimate g)
  (let* ([nodes (comp-graph-nodes g)]
         [n (length nodes)])
        (if (zero? n)
            0
            (graph-fold-nodes
             (lambda (acc id node)
                     (+ acc (node-memory-estimate node)))
             ;; Graph overhead: (comp-graph nodes output next-id)
             (* 4 *pair-size*)
             g))))

;;; node-memory-estimate : Node → Nat
(define (node-memory-estimate node)
  (cond
   [(node-var? node)
    ;; (var name): 2 pairs + symbol (shared)
    (* 2 *pair-size*)]
   [(node-const? node)
    ;; (const value): 2 pairs + number
    (+ (* 2 *pair-size*) *flonum-size*)]
   [(node-op? node)
    ;; (op name inputs): 3 pairs + symbol + input list
    (+ (* 3 *pair-size*)
       (* (length (node-op-inputs node))
          (+ *pair-size* ; list cell
             *pair-size*)))]  ; node reference
   [else 0]))

;;; traced-memory-estimate : Traced → Nat
(define (traced-memory-estimate t)
  (if (traced? t)
      ;; (traced value id tape-ref): 4 pairs + flonum + fixnum + ptr
      (+ (* 4 *pair-size*)
         *flonum-size*
         *fixnum-size*
         *ptr-size*)
      ;; Plain number
      *flonum-size*))

;;; ====
;;; Profile Report
;;; ====

;;; profile-computation : ((Traced ...) → Traced) × (List Number) → Report
(define (profile-computation f args)
  (reset-traced-ids!)
  (let* ([tape (make-reverse-tape)]
         [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
         ;; Forward pass with timing
         [forward-start (current-microseconds)]
         [result (apply f traced-args)]
         [forward-end (current-microseconds)])
        (if (traced? result)
            (let* ([val (traced-value result)]
                   [result-id (traced-id result)]
                   [arg-ids (map traced-id traced-args)]
                   ;; Backward pass with timing
                   [backward-start (current-microseconds)]
                   [grads-table (backward tape result-id 1)]
                   [backward-end (current-microseconds)]
                   [grads (map (lambda (id)
                                       (if (= id result-id)
                                           1
                                           (hashtable-ref grads-table id 0)))
                               arg-ids)]
                   [stats (tape-stats tape)]
                   [mem (tape-memory-estimate tape)])
                  (list
                   (cons 'value val)
                   (cons 'gradient grads)
                   (cons 'timing
                         (list (cons 'forward-us (- forward-end forward-start))
                               (cons 'backward-us (- backward-end backward-start))
                               (cons 'total-us (- backward-end forward-start))))
                   (cons 'tape stats)
                   (cons 'memory-bytes mem)
                   (cons 'num-inputs (length args))))
            ;; Constant function
            (list
             (cons 'value result)
             (cons 'gradient (map (lambda (_) 0) args))
             (cons 'timing
                   (list (cons 'forward-us (- forward-end forward-start))
                         (cons 'backward-us 0)
                         (cons 'total-us (- forward-end forward-start))))
             (cons 'tape '((size . 0) (ops . ())))
             (cons 'memory-bytes 0)
             (cons 'num-inputs (length args))))))

;;; print-profile : Report → Void
(define (print-profile report)
  (let ([val (cdr (assq 'value report))]
        [grad (cdr (assq 'gradient report))]
        [timing (cdr (assq 'timing report))]
        [tape-info (cdr (assq 'tape report))]
        [mem (cdr (assq 'memory-bytes report))]
        [n-inputs (cdr (assq 'num-inputs report))])
       (printf "~n=== Autodiff Profile ====~n")
       (printf "Inputs:     ~a variables~n" n-inputs)
       (printf "Value:      ~a~n" val)
       (printf "Gradient:   ~a~n" grad)
       (printf "~n--- Timing ----~n")
       (printf "Forward:    ~a us~n" (cdr (assq 'forward-us timing)))
       (printf "Backward:   ~a us~n" (cdr (assq 'backward-us timing)))
       (printf "Total:      ~a us~n" (cdr (assq 'total-us timing)))
       (printf "~n--- Tape ----~n")
       (printf "Size:       ~a entries~n" (cdr (assq 'size tape-info)))
       (printf "Operations: ~a~n" (cdr (assq 'ops tape-info)))
       (printf "~n--- Memory ----~n")
       (printf "Estimated:  ~a bytes (~a KB)~n"
               mem (quotient mem 1024))
       (printf "====~n")))

;;; ====
;;; Benchmark Utilities
;;; ====

;;; benchmark-gradient : ((Traced ...) → Traced) × (List Number) × Nat → BenchResult
(define (benchmark-gradient f args iterations)
  (let* ([times (let loop ([i 0] [acc '()])
                     (if (= i iterations)
                         (reverse acc)
                         (let-values ([(_ us) (time-thunk
                                               (lambda ()
                                                       (gradient f args)))])
                                     (loop (+ i 1) (cons us acc)))))]
         [total (apply + times)]
         [avg (/ total iterations)]
         [sorted (list-sort < times)]
         [min-t (car sorted)]
         [max-t (car (reverse sorted))]
         [median (list-ref sorted (quotient iterations 2))])
        (list (cons 'iterations iterations)
              (cons 'total-us total)
              (cons 'avg-us avg)
              (cons 'min-us min-t)
              (cons 'max-us max-t)
              (cons 'median-us median))))

;;; print-benchmark : BenchResult → Void
(define (print-benchmark result)
  (printf "~n=== Benchmark Results ====~n")
  (printf "Iterations: ~a~n" (cdr (assq 'iterations result)))
  (printf "Total:      ~a us~n" (cdr (assq 'total-us result)))
  (printf "Average:    ~a us~n" (inexact (cdr (assq 'avg-us result))))
  (printf "Min:        ~a us~n" (cdr (assq 'min-us result)))
  (printf "Max:        ~a us~n" (cdr (assq 'max-us result)))
  (printf "Median:     ~a us~n" (cdr (assq 'median-us result)))
  (printf "====~n"))
