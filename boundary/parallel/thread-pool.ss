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
