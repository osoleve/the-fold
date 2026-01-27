;;; boundary/parallel/thread-pool.ss
;;; @module thread-pool
;;; @requires prelude task chase-lev-deque

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

;;; Thread pool record
(define (make-thread-pool num-workers)
  (doc 'type '(-> Nat ThreadPool))
  (let ([workers (list->vector
                   (map make-worker (iota num-workers)))]
        [running-box (box #f)]
        [shutdown-box (box #f)])
    (list 'thread-pool workers running-box shutdown-box)))

(define (thread-pool? x) (and (pair? x) (eq? (car x) 'thread-pool)))
(define (pool-workers p) (list-ref p 1))
(define (pool-running? p) (unbox (list-ref p 2)))
(define (pool-running-set! p v) (set-box! (list-ref p 2) v))
(define (pool-shutdown? p) (unbox (list-ref p 3)))
(define (pool-shutdown-set! p v) (set-box! (list-ref p 3) v))

(define (pool-worker-count p)
  (vector-length (pool-workers p)))

(define (pool-shutdown! p)
  (doc 'type '(-> ThreadPool Void))
  (doc 'description "Signal pool to shut down.")
  (pool-shutdown-set! p #t)
  (pool-running-set! p #f))

;;; Global submit deque for external task submission
(define (pool-submit-deque p)
  ;; Use worker 0's deque for submissions (simple approach)
  (worker-deque (vector-ref (pool-workers p) 0)))

(define (pool-submit! p task)
  (doc 'type '(-> ThreadPool Task Void))
  (doc 'description "Submit task to pool for execution.")
  (deque-push! (pool-submit-deque p) task))

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
            (worker-state-set! worker 'stealing)
            (let ([stolen (try-steal pool worker)])
              (cond
                [stolen
                 (run-task stolen)
                 (loop 1)]
                [else
                 ;; Exponential backoff
                 (sleep (make-time 'time-duration (* backoff 1000000) 0))
                 (loop (min (* backoff 2) 100))]))]))])))

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
