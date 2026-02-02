(load "core/base/prelude.ss")
(load "boundary/parallel/thread-pool.ss")

(doc 'module 'scheduler)
(doc 'description "Public API for parallel task execution.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; Global pool (lazy singleton)
(define *global-pool* (box #f))

(define (ensure-pool!)
  (unless (unbox *global-pool*)
    (let ([pool (make-thread-pool (default-worker-count))])
      (pool-start! pool)
      (set-box! *global-pool* pool)))
  (unbox *global-pool*))

(define (spawn thunk . args)
  (doc 'type '(-> (-> Any) [Nat] Future))
  (doc 'description "Spawn a task, return future. Optional fuel argument.")
  (let* ([fuel (if (null? args) #f (car args))]
         [task (make-task thunk fuel)]
         [pool (ensure-pool!)])
    (pool-submit! pool task)
    (make-future task)))

(define (await future)
  (doc 'type '(-> Future Any))
  (doc 'description "Wait for future result with work-helping.")
  (let* ([task (future-task future)]
         [pool (ensure-pool!)])
    (await-with-helping task pool)))

;;; Work-helping await - run other tasks while waiting
(define (await-with-helping task pool)
  (let loop ([backoff 1])
    (cond
      [(task-done? task)
       (when (task-failed? task)
         (let ([p (task-promise task)])
           (error 'await
                  (if (and (pair? (cdr p)) (eq? (cadr p) 'task-failed))
                      (format "Task failed: ~a" (caddr p))
                      (format "Task failed: ~a" (cdr p))))))
       (task-result task)]
      [else
       ;; Try to help by running other tasks
       (let ([workers (pool-workers pool)])
         ;; Try to steal from any worker's deque
         (let help-loop ([i 0])
           (if (>= i (vector-length workers))
               ;; No work found, backoff and retry
               (begin
                 (sleep (make-time 'time-duration (* backoff 1000000) 0))
                 (loop (min (* backoff 2) 100)))
               (let ([w (vector-ref workers i)])
                 (let ([stolen (deque-steal! (worker-deque w))])
                   (cond
                     [(and stolen (not (eq? stolen 'empty)) (not (eq? stolen 'abort)))
                      (run-task stolen)
                      (loop 1)]  ; Reset backoff after work
                     [else
                      (help-loop (+ i 1))]))))))])))

;;; Pool management
(define (pool-stats)
  (doc 'type '(-> Alist))
  (doc 'description "Get pool statistics.")
  (let ([pool (ensure-pool!)])
    `((worker-count . ,(pool-worker-count pool))
      (running . ,(pool-running? pool))
      (queued . ,(let loop ([i 0] [total 0])
                   (if (>= i (pool-worker-count pool))
                       total
                       (loop (+ i 1)
                             (+ total (deque-size
                                       (worker-deque
                                        (vector-ref (pool-workers pool) i)))))))))))

(define (pool-shutdown-global!)
  (doc 'type '(-> Void))
  (let ([pool (unbox *global-pool*)])
    (when pool
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)
      (set-box! *global-pool* #f))))

;;; Bulk parallel operations

(define (parallel-invoke . thunks)
  (doc 'type '(-> (-> Any) ... (List Any)))
  (doc 'description "Run thunks in parallel, return results in order.")
  (if (null? thunks)
      '()
      (let ([futures (map spawn thunks)])
        (map await futures))))

(define *parallel-chunk-threshold* 64)

(define (parallel-map f xs . args)
  (doc 'type '(-> (-> a b) (List a) [Nat] (List b)))
  (doc 'description "Parallel map with adaptive chunking.")
  (let ([chunk-size (if (null? args)
                        #f
                        (car args))]
        [n (length xs)])
    (cond
      [(null? xs) '()]
      [(or (< n *parallel-chunk-threshold*)
           (not (pool-running? (ensure-pool!))))
       ;; Sequential fallback for small lists or no pool
       (map f xs)]
      [else
       ;; Parallel execution
       (let* ([num-workers (pool-worker-count (ensure-pool!))]
              [actual-chunk-size (or chunk-size
                                     (max 1 (quotient n num-workers)))]
              [chunks (split-into-chunks xs actual-chunk-size)]
              [futures (map (lambda (chunk)
                              (spawn (lambda () (map f chunk))))
                            chunks)])
         (apply append (map await futures)))])))

(define (split-into-chunks lst chunk-size)
  (doc 'type '(-> (List a) Nat (List (List a))))
  (doc 'description "Split list into chunks of given size.")
  (if (null? lst)
      '()
      (let loop ([remaining lst] [acc '()])
        (if (null? remaining)
            (reverse acc)
            (let-values ([(chunk rest) (split-at-most remaining chunk-size)])
              (loop rest (cons chunk acc)))))))

(define (split-at-most lst n)
  (doc 'type '(-> (List a) Nat (Values (List a) (List a))))
  (let loop ([lst lst] [n n] [acc '()])
    (if (or (null? lst) (= n 0))
        (values (reverse acc) lst)
        (loop (cdr lst) (- n 1) (cons (car lst) acc)))))

(define (parallel-for-each f xs)
  (doc 'type '(-> (-> a Void) (List a) Void))
  (doc 'description "Parallel for-each (side effects).")
  (parallel-map f xs)
  (void))
