(load "core/base/prelude.ss")
(load "lattice/data/chase-lev-deque.ss")
(load "boundary/parallel/task.ss")

(doc 'module 'thread-pool)
(doc 'description "Work-stealing thread pool with Chase-Lev deques.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; CPU count detection
(define (cpu-count)
  (doc 'type '(-> Nat))
  (doc 'description "Get number of CPU cores.")
  ;; Chez doesn't have built-in cpu-count, use reasonable default or env var
  (let ([env-val (getenv "FOLD_CPU_COUNT")])
    (if env-val
        (string->number env-val)
        4)))  ; Conservative default

(define (default-worker-count)
  (doc 'type '(-> Nat))
  (max 1 (- (cpu-count) 1)))

;;; Worker record
(define (make-worker id)
  (list 'worker
        id                              ; worker id
        #f                              ; thread handle (set on start)
        (make-chase-lev-deque 256)      ; work deque
        (box 'idle)))                   ; state: idle|running|stealing|shutdown

(define (worker? x) (and (pair? x) (eq? (car x) 'worker)))
(define (worker-id w) (list-ref w 1))
(define (worker-thread w) (list-ref w 2))
(define (worker-thread-set! w t) (set-car! (list-tail w 2) t))
(define (worker-deque w) (list-ref w 3))
(define (worker-state w) (unbox (list-ref w 4)))
(define (worker-state-set! w s) (set-box! (list-ref w 4) s))

;;; Thread-safe injection queue for external submissions
;;; Uses a mutex since external submissions are infrequent
(define (make-injection-queue)
  (list 'injection-queue
        (box '())      ; queue (list of tasks)
        (make-mutex)))  ; mutex for thread safety

(define (injection-queue-push! q task)
  "Thread-safe push to injection queue."
  (with-mutex (caddr q)
    (let ([queue-box (cadr q)])
      (set-box! queue-box (cons task (unbox queue-box))))))

(define (injection-queue-take-all! q)
  "Atomically take all tasks from queue. Returns list (may be empty)."
  (with-mutex (caddr q)
    (let* ([queue-box (cadr q)]
           [tasks (reverse (unbox queue-box))])  ; FIFO order
      (set-box! queue-box '())
      tasks)))

;;; Thread pool record
(define (make-thread-pool num-workers)
  (doc 'type '(-> Nat ThreadPool))
  (let ([workers (list->vector
                   (map make-worker (iota num-workers)))]
        [running-box (box #f)]
        [shutdown-box (box #f)]
        [injection-queue (make-injection-queue)])
    (list 'thread-pool workers running-box shutdown-box injection-queue)))

(define (thread-pool? x) (and (pair? x) (eq? (car x) 'thread-pool)))
(define (pool-workers p) (list-ref p 1))
(define (pool-running? p) (unbox (list-ref p 2)))
(define (pool-running-set! p v) (set-box! (list-ref p 2) v))
(define (pool-shutdown? p) (unbox (list-ref p 3)))
(define (pool-shutdown-set! p v) (set-box! (list-ref p 3) v))
(define (pool-injection-queue p) (list-ref p 4))

(define (pool-worker-count p)
  (vector-length (pool-workers p)))

(define (pool-shutdown! p)
  (doc 'type '(-> ThreadPool Void))
  (doc 'description "Signal pool to shut down.")
  (pool-shutdown-set! p #t)
  (pool-running-set! p #f))

(define (pool-submit! p task)
  (doc 'type '(-> ThreadPool Task Void))
  (doc 'description "Submit task to pool for execution.
Uses thread-safe injection queue to avoid multi-producer race on deques.")
  (injection-queue-push! (pool-injection-queue p) task))

;;; Random number generator for victim selection
(define *random-state* (box (current-time)))

(define (pool-random n)
  "Simple LCG random number in [0, n)"
  (let* ([s (unbox *random-state*)]
         [next (mod (+ (* 1103515245 (if (time? s) (time-nanosecond s) s)) 12345)
                    (expt 2 31))])
    (set-box! *random-state* next)
    (mod next n)))

;;; Try to steal from another worker
(define (try-steal pool worker)
  (let* ([workers (pool-workers pool)]
         [n (vector-length workers)]
         [my-id (worker-id worker)]
         [start (pool-random n)])
    (let loop ([i 0])
      (if (>= i n)
          #f
          (let ([victim-idx (mod (+ start i) n)])
            (if (= victim-idx my-id)
                (loop (+ i 1))
                (let* ([victim (vector-ref workers victim-idx)]
                       [stolen (deque-steal! (worker-deque victim))])
                  (cond
                    [(eq? stolen 'abort) (loop (+ i 1))]
                    [(eq? stolen 'empty) (loop (+ i 1))]
                    [else stolen]))))))))

;;; Run a single task
(define (run-task task)
  (guard (exn [else (task-fail! task (format "~a" exn))])
    (let ([result ((task-thunk task))])
      (task-complete! task 'ok result))))

;;; Check injection queue and move tasks to worker's deque
(define (drain-injection-queue! worker pool)
  "Move tasks from injection queue to this worker's deque."
  (let ([tasks (injection-queue-take-all! (pool-injection-queue pool))])
    (for-each (lambda (task)
                (deque-push! (worker-deque worker) task))
              tasks)
    (not (null? tasks))))  ; Return #t if we got any tasks

;;; Worker main loop
(define (worker-loop worker pool)
  (let loop ([backoff 1])
    (cond
      [(pool-shutdown? pool)
       (worker-state-set! worker 'shutdown)]
      [else
       (worker-state-set! worker 'running)
       (let ([task (deque-pop! (worker-deque worker))])
         (cond
           [(and (not (eq? task 'empty)) task)
            (run-task task)
            (loop 1)]
           [else
            ;; No local work - check injection queue first (all workers share this)
            (if (drain-injection-queue! worker pool)
                (loop 1)  ; Got tasks from injection queue, try again
                (begin
                  (worker-state-set! worker 'stealing)
                  (let ([stolen (try-steal pool worker)])
                    (cond
                      [stolen
                       (run-task stolen)
                       (loop 1)]
                      [else
                       ;; Exponential backoff
                       (sleep (make-time 'time-duration (* backoff 1000000) 0))
                       (loop (min (* backoff 2) 100))]))))]))])))

;;; Start the pool
(define (pool-start! p)
  (doc 'type '(-> ThreadPool Void))
  (doc 'description "Start worker threads.")
  (unless (pool-running? p)
    (pool-running-set! p #t)
    (vector-for-each
     (lambda (worker)
       (worker-thread-set!
        worker
        (fork-thread (lambda () (worker-loop worker p)))))
     (pool-workers p))))

;;; Wait for all workers to finish
(define (pool-wait-shutdown! p)
  (doc 'type '(-> ThreadPool Void))
  (doc 'description "Wait for all workers to finish after shutdown.")
  (vector-for-each
   (lambda (worker)
     (let ([t (worker-thread worker)])
       (when t (thread-join t))))
   (pool-workers p)))
