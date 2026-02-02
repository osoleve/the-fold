(load "core/base/prelude.ss")

(doc 'module 'task)
(doc 'description "Task and Future abstractions for parallel scheduler.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; Task states:
;;; - pending: promise = #f
;;; - completed: promise = (ok . value)
;;; - failed: promise = (error . message)
;;; - suspended: promise = (suspended . continuation-data)

(define (make-task thunk fuel)
  (doc 'type '(-> (-> Any) (U Nat #f) Task))
  (doc 'description "Create a new task. fuel=#f means unlimited.")
  (list 'task
        thunk                        ; thunk to execute
        fuel                         ; fuel budget
        (box #f)                     ; promise: #f | (ok . val) | (error . msg)
        (make-mutex)                 ; done-mutex
        (make-condition)             ; done-condition
        '()))                        ; captured-env (set later)

(define (task? x)
  (and (pair? x) (eq? (car x) 'task)))

(define (task-thunk t) (list-ref t 1))
(define (task-fuel t) (list-ref t 2))
(define (task-promise-box t) (list-ref t 3))
(define (task-promise t) (unbox (task-promise-box t)))
(define (task-mutex t) (list-ref t 4))
(define (task-condition t) (list-ref t 5))
(define (task-captured-env t) (list-ref t 6))

(define (task-set-captured-env! t env)
  (set-car! (list-tail t 6) env))

(define (task-done? t)
  (doc 'type '(-> Task Boolean))
  (not (eq? #f (task-promise t))))

(define (task-failed? t)
  (doc 'type '(-> Task Boolean))
  (let ([p (task-promise t)])
    (and (pair? p) (eq? (car p) 'error))))

(define (task-suspended? t)
  (doc 'type '(-> Task Boolean))
  (let ([p (task-promise t)])
    (and (pair? p) (eq? (car p) 'suspended))))

(define (task-result t)
  (doc 'type '(-> Task Any))
  (doc 'description "Get task result. Only valid if task-done? is true.")
  (let ([p (task-promise t)])
    (if (and (pair? p) (eq? (car p) 'ok))
        (cdr p)
        p)))  ; Return error/suspended as-is

(define (task-complete! t status value)
  (doc 'type '(-> Task Symbol Any Void))
  (doc 'description "Mark task complete with result.")
  (set-box! (task-promise-box t) (cons status value))
  (with-mutex (task-mutex t)
    (condition-broadcast (task-condition t))))

(define (task-fail! t message)
  (doc 'type '(-> Task String Void))
  (task-complete! t 'error `(task-failed ,message)))

;;; Future - external handle to a task

(define (make-future task)
  (doc 'type '(-> Task Future))
  (list 'future task))

(define (future? x)
  (and (pair? x) (eq? (car x) 'future)))

(define (future-task f) (cadr f))

(define (future-done? f)
  (doc 'type '(-> Future Boolean))
  (task-done? (future-task f)))

(define (future-result f)
  (doc 'type '(-> Future Any))
  (task-result (future-task f)))
