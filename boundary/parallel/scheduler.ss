;;; boundary/parallel/scheduler.ss
;;; @module scheduler
;;; @requires thread-pool task

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
