# Work-Stealing Scheduler Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a work-stealing scheduler with Chase-Lev deques for parallel evaluation in The Fold.

**Architecture:** Lock-free Chase-Lev deque in lattice/data/ (pure), thread pool and scheduler API in boundary/parallel/ (impure). Workers steal from random victims when idle, use work-helping during await to prevent deadlock.

**Tech Stack:** Chez Scheme threading (`fork-thread`, `thread-join`, `box-cas!`, `make-mutex`, `make-condition`)

**Design Doc:** `docs/plans/2026-01-27-work-stealing-scheduler-design.md`

---

## Task 1: Chase-Lev Deque - Structure and Constructor

**Files:**
- Create: `lattice/data/chase-lev-deque.ss`
- Create: `lattice/data/test-chase-lev-deque.ss`

**Step 1: Create test file with basic constructor test**

```scheme
;;; lattice/data/test-chase-lev-deque.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "lattice/data/chase-lev-deque.ss")

(test-group "chase-lev-deque"

  (define-test "make-chase-lev-deque creates empty deque"
    (let ([d (make-chase-lev-deque 16)])
      (assert-true (chase-lev-deque? d))
      (assert-equal 0 (deque-size d))))

  (define-test "deque-empty? returns true for new deque"
    (let ([d (make-chase-lev-deque 16)])
      (assert-true (deque-empty? d)))))

(run-all-tests)
```

**Step 2: Run test to verify it fails**

Run: `cd /home/oso/fold/.worktrees/work-stealing && scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: FAIL - "chase-lev-deque.ss" not found

**Step 3: Write minimal implementation**

```scheme
;;; lattice/data/chase-lev-deque.ss
;;; @module chase-lev-deque
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'chase-lev-deque)
(doc 'description "Lock-free Chase-Lev work-stealing deque.
Owner pushes/pops from bottom (LIFO), thieves steal from top (FIFO).
Uses monotonic 64-bit indices to avoid ABA problem.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)  ; Uses mutation for lock-free operations
(doc 'references '("Chase & Lev 2005: Dynamic Circular Work-Stealing Deque"))

;;; Structure: buffer (vector), bottom (box), top (box)
;;; No separate capacity field - thieves compute from (vector-length buffer)

(define (make-chase-lev-deque initial-capacity)
  (doc 'type '(-> Nat ChaselevDeque))
  (doc 'description "Create empty deque with given initial capacity.")
  (list 'chase-lev-deque
        (box (make-vector initial-capacity #f))  ; buffer
        (box 0)                                   ; bottom (monotonic)
        (box 0)))                                 ; top (monotonic)

(define (chase-lev-deque? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'chase-lev-deque)))

(define (deque-buffer d) (unbox (cadr d)))
(define (deque-buffer-box d) (cadr d))
(define (deque-bottom d) (unbox (caddr d)))
(define (deque-bottom-box d) (caddr d))
(define (deque-top d) (unbox (cadddr d)))
(define (deque-top-box d) (cadddr d))

(define (deque-size d)
  (doc 'type '(-> ChaselevDeque Nat))
  (doc 'description "Approximate size (racy but useful for debugging).")
  (max 0 (- (deque-bottom d) (deque-top d))))

(define (deque-empty? d)
  (doc 'type '(-> ChaselevDeque Boolean))
  (doc 'description "Check if deque appears empty.")
  (<= (deque-bottom d) (deque-top d)))
```

**Step 4: Run test to verify it passes**

Run: `cd /home/oso/fold/.worktrees/work-stealing && scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: PASS

**Step 5: Commit**

```bash
cd /home/oso/fold/.worktrees/work-stealing
git add lattice/data/chase-lev-deque.ss lattice/data/test-chase-lev-deque.ss
git commit -m "feat(deque): Add Chase-Lev deque structure and constructor

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 2: Chase-Lev Deque - Push Operation

**Files:**
- Modify: `lattice/data/chase-lev-deque.ss`
- Modify: `lattice/data/test-chase-lev-deque.ss`

**Step 1: Add push test**

Add to test file after existing tests:

```scheme
  (define-test "deque-push! adds element"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'task-1)
      (assert-equal 1 (deque-size d))
      (assert-false (deque-empty? d))))

  (define-test "deque-push! multiple elements"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'a)
      (deque-push! d 'b)
      (deque-push! d 'c)
      (assert-equal 3 (deque-size d))))
```

**Step 2: Run test to verify it fails**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: FAIL - "deque-push!" not defined

**Step 3: Implement push**

Add to chase-lev-deque.ss:

```scheme
(define (deque-push! d task)
  (doc 'type '(-> ChaselevDeque Task Void))
  (doc 'description "Owner pushes task to bottom. Resizes if full.")
  (let* ([b (deque-bottom d)]
         [t (deque-top d)]
         [buf (deque-buffer d)]
         [cap (vector-length buf)])
    ;; Resize if full
    (when (>= (- b t) cap)
      (deque-resize! d))
    ;; Write task and increment bottom
    (let ([buf (deque-buffer d)]  ; Re-read after potential resize
          [cap (vector-length (deque-buffer d))])
      (vector-set! buf (mod b cap) task)
      (set-box! (deque-bottom-box d) (+ b 1)))))

(define (deque-resize! d)
  (doc 'type '(-> ChaselevDeque Void))
  (doc 'description "Double buffer capacity, copying valid elements.")
  (let* ([old-buf (deque-buffer d)]
         [old-cap (vector-length old-buf)]
         [new-cap (* old-cap 2)]
         [new-buf (make-vector new-cap #f)]
         [t (deque-top d)]
         [b (deque-bottom d)])
    ;; Copy valid elements [t, b) to new buffer
    (let loop ([i t])
      (when (< i b)
        (vector-set! new-buf (mod i new-cap)
                     (vector-ref old-buf (mod i old-cap)))
        (loop (+ i 1))))
    (set-box! (deque-buffer-box d) new-buf)))
```

**Step 4: Run test to verify it passes**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add lattice/data/chase-lev-deque.ss lattice/data/test-chase-lev-deque.ss
git commit -m "feat(deque): Add deque-push! with auto-resize

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 3: Chase-Lev Deque - Pop Operation

**Files:**
- Modify: `lattice/data/chase-lev-deque.ss`
- Modify: `lattice/data/test-chase-lev-deque.ss`

**Step 1: Add pop tests**

```scheme
  (define-test "deque-pop! returns last pushed (LIFO)"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'first)
      (deque-push! d 'second)
      (deque-push! d 'third)
      (assert-equal 'third (deque-pop! d))
      (assert-equal 'second (deque-pop! d))
      (assert-equal 'first (deque-pop! d))))

  (define-test "deque-pop! on empty returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (assert-equal 'empty (deque-pop! d))))

  (define-test "deque-pop! after emptying returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'x)
      (deque-pop! d)
      (assert-equal 'empty (deque-pop! d))))
```

**Step 2: Run test to verify it fails**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: FAIL - "deque-pop!" not defined

**Step 3: Implement pop with CAS for contention zone**

```scheme
(define (deque-pop! d)
  (doc 'type '(-> ChaselevDeque (U Task 'empty)))
  (doc 'description "Owner pops from bottom. Returns 'empty if deque is empty.
Uses CAS when contending with thieves on last element.")
  (let* ([b (- (deque-bottom d) 1)]
         [_ (set-box! (deque-bottom-box d) b)]  ; Decrement first
         [t (deque-top d)])
    (cond
      [(< b t)
       ;; Queue was empty, restore bottom
       (set-box! (deque-bottom-box d) t)
       'empty]
      [(> b t)
       ;; Multiple elements, safe to take
       (let* ([buf (deque-buffer d)]
              [cap (vector-length buf)]
              [task (vector-ref buf (mod b cap))])
         (vector-set! buf (mod b cap) #f)  ; Clear for GC
         task)]
      [else
       ;; b == t: Last element, race with thieves
       (let* ([buf (deque-buffer d)]
              [cap (vector-length buf)]
              [task (vector-ref buf (mod b cap))])
         (if (box-cas! (deque-top-box d) t (+ t 1))
             ;; Won the race
             (begin
               (set-box! (deque-bottom-box d) (+ t 1))
               (vector-set! buf (mod b cap) #f)
               task)
             ;; Lost to thief
             (begin
               (set-box! (deque-bottom-box d) (+ t 1))
               'empty)))])))
```

**Step 4: Run test to verify it passes**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add lattice/data/chase-lev-deque.ss lattice/data/test-chase-lev-deque.ss
git commit -m "feat(deque): Add deque-pop! with CAS for contention

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 4: Chase-Lev Deque - Steal Operation

**Files:**
- Modify: `lattice/data/chase-lev-deque.ss`
- Modify: `lattice/data/test-chase-lev-deque.ss`

**Step 1: Add steal tests**

```scheme
  (define-test "deque-steal! returns oldest (FIFO from top)"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'first)
      (deque-push! d 'second)
      (deque-push! d 'third)
      ;; Steal takes from top (oldest)
      (assert-equal 'first (deque-steal! d))
      (assert-equal 'second (deque-steal! d))))

  (define-test "deque-steal! on empty returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (assert-equal 'empty (deque-steal! d))))

  (define-test "deque-steal! and deque-pop! work together"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'a)
      (deque-push! d 'b)
      (deque-push! d 'c)
      ;; Steal oldest, pop newest
      (assert-equal 'a (deque-steal! d))
      (assert-equal 'c (deque-pop! d))
      (assert-equal 'b (deque-pop! d))
      (assert-equal 'empty (deque-pop! d))))
```

**Step 2: Run test to verify it fails**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: FAIL - "deque-steal!" not defined

**Step 3: Implement steal**

```scheme
(define (deque-steal! d)
  (doc 'type '(-> ChaselevDeque (U Task 'empty 'abort)))
  (doc 'description "Thief steals from top. Returns task, 'empty, or 'abort (CAS failed).")
  (let* ([t (deque-top d)]
         [b (deque-bottom d)])
    (if (>= t b)
        'empty
        (let* ([buf (deque-buffer d)]
               [cap (vector-length buf)]
               [task (vector-ref buf (mod t cap))])
          (if (box-cas! (deque-top-box d) t (+ t 1))
              (begin
                (vector-set! buf (mod t cap) #f)  ; Clear for GC
                task)
              'abort)))))  ; Another thief won, caller should retry elsewhere
```

**Step 4: Run test to verify it passes**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add lattice/data/chase-lev-deque.ss lattice/data/test-chase-lev-deque.ss
git commit -m "feat(deque): Add deque-steal! with CAS

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 5: Chase-Lev Deque - Resize Under Load Test

**Files:**
- Modify: `lattice/data/test-chase-lev-deque.ss`

**Step 1: Add resize stress test**

```scheme
  (define-test "deque handles resize correctly"
    (let ([d (make-chase-lev-deque 4)])  ; Small initial capacity
      ;; Push more than initial capacity
      (deque-push! d 1)
      (deque-push! d 2)
      (deque-push! d 3)
      (deque-push! d 4)
      (deque-push! d 5)  ; Triggers resize
      (deque-push! d 6)
      (assert-equal 6 (deque-size d))
      ;; Verify all elements still accessible
      (assert-equal 6 (deque-pop! d))
      (assert-equal 5 (deque-pop! d))
      (assert-equal 1 (deque-steal! d))
      (assert-equal 2 (deque-steal! d))
      (assert-equal 3 (deque-pop! d))
      (assert-equal 4 (deque-pop! d))
      (assert-equal 'empty (deque-pop! d))))

  (define-test "deque handles many resizes"
    (let ([d (make-chase-lev-deque 2)])
      ;; Push 100 elements (will resize multiple times)
      (let loop ([i 0])
        (when (< i 100)
          (deque-push! d i)
          (loop (+ i 1))))
      (assert-equal 100 (deque-size d))
      ;; Pop all and verify LIFO order
      (let loop ([i 99])
        (when (>= i 0)
          (assert-equal i (deque-pop! d))
          (loop (- i 1))))))
```

**Step 2: Run test to verify it passes**

Run: `scheme --script lattice/data/test-chase-lev-deque.ss`
Expected: PASS (tests existing implementation)

**Step 3: Commit**

```bash
git add lattice/data/test-chase-lev-deque.ss
git commit -m "test(deque): Add resize stress tests

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 6: Task and Future Records

**Files:**
- Create: `boundary/parallel/task.ss`
- Create: `boundary/parallel/test-task.ss`

**Step 1: Create test file**

```scheme
;;; boundary/parallel/test-task.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/task.ss")

(test-group "task-and-future"

  (define-test "make-task creates pending task"
    (let ([t (make-task (lambda () 42) 1000)])
      (assert-true (task? t))
      (assert-false (task-done? t))))

  (define-test "task-complete! sets result"
    (let ([t (make-task (lambda () 42) 1000)])
      (task-complete! t 'success 42)
      (assert-true (task-done? t))
      (assert-equal 42 (task-result t))))

  (define-test "task-fail! sets error"
    (let ([t (make-task (lambda () (error 'oops)) 1000)])
      (task-fail! t "something went wrong")
      (assert-true (task-done? t))
      (assert-true (task-failed? t))))

  (define-test "make-future wraps task"
    (let* ([t (make-task (lambda () 42) 1000)]
           [f (make-future t)])
      (assert-true (future? f))
      (assert-equal t (future-task f)))))

(run-all-tests)
```

**Step 2: Run test to verify it fails**

Run: `scheme --script boundary/parallel/test-task.ss`
Expected: FAIL - file not found

**Step 3: Create task.ss**

```scheme
;;; boundary/parallel/task.ss
;;; @module task
;;; @requires prelude

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
```

**Step 4: Run test to verify it passes**

Run: `mkdir -p boundary/parallel && scheme --script boundary/parallel/test-task.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add boundary/parallel/task.ss boundary/parallel/test-task.ss
git commit -m "feat(parallel): Add Task and Future abstractions

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 7: Thread Pool - Basic Structure

**Files:**
- Create: `boundary/parallel/thread-pool.ss`
- Create: `boundary/parallel/test-thread-pool.ss`

**Step 1: Create test file**

```scheme
;;; boundary/parallel/test-thread-pool.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/thread-pool.ss")

(test-group "thread-pool"

  (define-test "cpu-count returns positive integer"
    (let ([n (cpu-count)])
      (assert-true (and (integer? n) (> n 0)))))

  (define-test "default-worker-count is (max 1 (- cpu-count 1))"
    (let ([n (default-worker-count)])
      (assert-true (>= n 1))
      (assert-true (<= n (cpu-count)))))

  (define-test "make-thread-pool creates pool with workers"
    (let ([pool (make-thread-pool 2)])
      (assert-true (thread-pool? pool))
      (assert-equal 2 (pool-worker-count pool))
      (pool-shutdown! pool))))

(run-all-tests)
```

**Step 2: Run test to verify it fails**

Run: `scheme --script boundary/parallel/test-thread-pool.ss`
Expected: FAIL - file not found

**Step 3: Create thread-pool.ss skeleton**

```scheme
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
```

**Step 4: Run test to verify it passes**

Run: `scheme --script boundary/parallel/test-thread-pool.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add boundary/parallel/thread-pool.ss boundary/parallel/test-thread-pool.ss
git commit -m "feat(parallel): Add thread pool structure

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 8: Thread Pool - Worker Loop and Stealing

**Files:**
- Modify: `boundary/parallel/thread-pool.ss`
- Modify: `boundary/parallel/test-thread-pool.ss`

**Step 1: Add worker execution tests**

```scheme
  (define-test "pool-start! launches workers"
    (let ([pool (make-thread-pool 2)])
      (pool-start! pool)
      (assert-true (pool-running? pool))
      (sleep (make-time 'time-duration 0 0))  ; Yield
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)))

  (define-test "pool executes submitted task"
    (let ([pool (make-thread-pool 2)]
          [result-box (box #f)])
      (pool-start! pool)
      (pool-submit! pool (make-task (lambda ()
                                      (set-box! result-box 42)
                                      42)
                                    #f))
      ;; Wait for result
      (let loop ([n 100])
        (when (and (> n 0) (not (unbox result-box)))
          (sleep (make-time 'time-duration 10000000 0))  ; 10ms
          (loop (- n 1))))
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)
      (assert-equal 42 (unbox result-box))))
```

**Step 2: Run test to verify it fails**

Run: `scheme --script boundary/parallel/test-thread-pool.ss`
Expected: FAIL - pool-start! not defined

**Step 3: Implement worker loop and stealing**

Add to thread-pool.ss:

```scheme
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
                (let ([victim (vector-ref workers victim-idx)])
                  (case (deque-steal! (worker-deque victim))
                    [(abort) (loop (+ i 1))]
                    [(empty) (loop (+ i 1))]
                    [else => identity]))))))))

;;; Run a single task
(define (run-task task)
  (guard (exn [else (task-fail! task (format "~a" exn))])
    (let ([result ((task-thunk task))])
      (task-complete! t 'ok result))))

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
```

**Step 4: Run test to verify it passes**

Run: `scheme --script boundary/parallel/test-thread-pool.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add boundary/parallel/thread-pool.ss boundary/parallel/test-thread-pool.ss
git commit -m "feat(parallel): Add worker loop with stealing and backoff

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 9: Public API - spawn and await

**Files:**
- Create: `boundary/parallel/scheduler.ss`
- Create: `boundary/parallel/test-scheduler.ss`

**Step 1: Create test file**

```scheme
;;; boundary/parallel/test-scheduler.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/scheduler.ss")

(test-group "scheduler-api"

  (define-test "spawn returns future"
    (let ([f (spawn (lambda () 42))])
      (assert-true (future? f))))

  (define-test "await returns result"
    (let* ([f (spawn (lambda () (+ 1 2 3)))]
           [result (await f)])
      (assert-equal 6 result)))

  (define-test "spawn/await with multiple tasks"
    (let* ([f1 (spawn (lambda () 10))]
           [f2 (spawn (lambda () 20))]
           [f3 (spawn (lambda () 30))])
      (assert-equal 10 (await f1))
      (assert-equal 20 (await f2))
      (assert-equal 30 (await f3))))

  (define-test "await captures errors"
    (let* ([f (spawn (lambda () (error 'test "boom")))]
           [result (await f)])
      (assert-true (and (pair? result)
                        (eq? (car result) 'error))))))

(run-all-tests)
```

**Step 2: Run test to verify it fails**

Run: `scheme --script boundary/parallel/test-scheduler.ss`
Expected: FAIL - file not found

**Step 3: Create scheduler.ss**

```scheme
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
         ;; Try to pop from any worker's deque
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
```

**Step 4: Run test to verify it passes**

Run: `scheme --script boundary/parallel/test-scheduler.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add boundary/parallel/scheduler.ss boundary/parallel/test-scheduler.ss
git commit -m "feat(parallel): Add spawn/await with work-helping

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 10: Public API - parallel-invoke and parallel-map

**Files:**
- Modify: `boundary/parallel/scheduler.ss`
- Modify: `boundary/parallel/test-scheduler.ss`

**Step 1: Add tests**

```scheme
  (define-test "parallel-invoke runs multiple thunks"
    (let ([results (parallel-invoke
                    (lambda () 1)
                    (lambda () 2)
                    (lambda () 3))])
      (assert-equal '(1 2 3) results)))

  (define-test "parallel-map applies function to list"
    (let ([results (parallel-map (lambda (x) (* x x)) '(1 2 3 4 5))])
      (assert-equal '(1 4 9 16 25) results)))

  (define-test "parallel-map handles empty list"
    (assert-equal '() (parallel-map (lambda (x) x) '())))

  (define-test "parallel-for-each executes side effects"
    (let ([sum-box (box 0)]
          [lock (make-mutex)])
      (parallel-for-each
       (lambda (x)
         (with-mutex lock
           (set-box! sum-box (+ (unbox sum-box) x))))
       '(1 2 3 4 5))
      (assert-equal 15 (unbox sum-box))))
```

**Step 2: Run test to verify it fails**

Run: `scheme --script boundary/parallel/test-scheduler.ss`
Expected: FAIL - parallel-invoke not defined

**Step 3: Implement bulk operations**

Add to scheduler.ss:

```scheme
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
```

**Step 4: Run test to verify it passes**

Run: `scheme --script boundary/parallel/test-scheduler.ss`
Expected: PASS

**Step 5: Commit**

```bash
git add boundary/parallel/scheduler.ss boundary/parallel/test-scheduler.ss
git commit -m "feat(parallel): Add parallel-invoke, parallel-map, parallel-for-each

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 11: Integration with par/pseq

**Files:**
- Modify: `core/lang/eval.ss`
- Verify: `core/lang/test-par-pseq.ss`

**Step 1: Verify existing par tests still pass**

Run: `scheme --script core/lang/test-par-pseq.ss`
Expected: PASS (baseline)

**Step 2: Update eval-par-parallel to use scheduler**

In `core/lang/eval.ss`, modify `eval-par-parallel`:

```scheme
;; At top of file, add conditional load
(when (file-exists? "boundary/parallel/scheduler.ss")
  (load "boundary/parallel/scheduler.ss"))

;; Modify eval-par-parallel to use scheduler when available
(define (eval-par-parallel a-expr b-expr env fuel ctx)
  (doc 'type (-> Expr Expr Env Fuel Context Result))
  (doc 'description "Parallel evaluation using work-stealing scheduler.")
  (if (top-level-bound? 'spawn)
      ;; Use work-stealing scheduler
      (let* ([a-future (spawn (lambda () (eval-core a-expr env fuel #f)) fuel)]
             [b-result (eval-core b-expr env fuel ctx)]
             [a-result (await a-future)])
        (cond
          [(and (pair? a-result) (result-error? a-result))
           a-result]
          [(and (pair? a-result) (result-suspended? a-result))
           `(error par-suspended "Background expression suspended (out of fuel)" ,a-expr)]
          [else b-result]))
      ;; Fallback to thread-based parallel
      (eval-par-parallel-threads a-expr b-expr env fuel ctx)))

;; Rename old implementation
(define (eval-par-parallel-threads a-expr b-expr env fuel ctx)
  ;; ... existing fork-thread implementation ...
  )
```

**Step 3: Run par/pseq tests**

Run: `scheme --script core/lang/test-par-pseq.ss`
Expected: PASS

**Step 4: Commit**

```bash
git add core/lang/eval.ss
git commit -m "feat(eval): Integrate work-stealing scheduler with par/pseq

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 12: Stress Tests and Verification

**Files:**
- Create: `boundary/parallel/test-stress.ss`

**Step 1: Create stress test file**

```scheme
;;; boundary/parallel/test-stress.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/scheduler.ss")

(test-group "stress-tests"

  (define-test "parallel-map 1000 elements"
    (let* ([xs (iota 1000)]
           [results (parallel-map (lambda (x) (* x 2)) xs)])
      (assert-equal (map (lambda (x) (* x 2)) xs) results)))

  (define-test "nested parallel-invoke"
    (let ([result (parallel-invoke
                   (lambda () (parallel-invoke
                               (lambda () 1)
                               (lambda () 2)))
                   (lambda () (parallel-invoke
                               (lambda () 3)
                               (lambda () 4))))])
      (assert-equal '((1 2) (3 4)) result)))

  (define-test "many concurrent spawns"
    (let ([futures (map (lambda (i) (spawn (lambda () i)))
                        (iota 100))])
      (assert-equal (iota 100) (map await futures))))

  (define-test "error isolation"
    (let ([results (parallel-invoke
                    (lambda () 1)
                    (lambda () (error 'test "boom"))
                    (lambda () 3))])
      (assert-equal 1 (car results))
      (assert-true (and (pair? (cadr results))
                        (eq? (car (cadr results)) 'error)))
      (assert-equal 3 (caddr results)))))

(run-all-tests)

;; Cleanup
(pool-shutdown-global!)
```

**Step 2: Run stress tests**

Run: `scheme --script boundary/parallel/test-stress.ss`
Expected: PASS

**Step 3: Commit**

```bash
git add boundary/parallel/test-stress.ss
git commit -m "test(parallel): Add stress tests for scheduler

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 13: Update BBS and Documentation

**Files:**
- Update BBS issue fold-0a9
- Create manifest for parallel module

**Step 1: Create manifest**

```scheme
;;; boundary/parallel/manifest.sexp
(module parallel
  (version "0.1.0")
  (layer boundary)
  (purity partial)
  (stability experimental)
  (description "Work-stealing parallel scheduler with Chase-Lev deques")
  (deps (data))
  (exports
    (scheduler spawn await parallel-invoke parallel-map parallel-for-each
               pool-stats pool-shutdown-global!)
    (task make-task task? task-done? task-result make-future future?
          future-done? future-result)
    (thread-pool make-thread-pool pool-start! pool-shutdown!
                 cpu-count default-worker-count)))
```

**Step 2: Close BBS issue**

```scheme
(bbs-update 'fold-0a9 'status 'completed)
(bbs-comment 'fold-0a9 "Implemented work-stealing scheduler with Chase-Lev deques.
- Lock-free deque in lattice/data/chase-lev-deque.ss
- Thread pool in boundary/parallel/thread-pool.ss
- Public API in boundary/parallel/scheduler.ss
- Integrated with par/pseq evaluation")
```

**Step 3: Commit**

```bash
git add boundary/parallel/manifest.sexp
git commit -m "docs(parallel): Add manifest and close fold-0a9

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 14: Final Integration Test and Merge

**Step 1: Run full test suite**

Run: `scheme --script test-all.ss quick`
Expected: PASS (or pre-existing failures only)

**Step 2: Merge to main**

```bash
cd /home/oso/fold
git merge feature/work-stealing-scheduler
git push
```

**Step 3: Clean up worktree**

```bash
git worktree remove .worktrees/work-stealing
```

---

## Summary

| Task | Component | Estimated Complexity |
|------|-----------|---------------------|
| 1-5 | Chase-Lev Deque | Medium (lock-free) |
| 6 | Task/Future | Simple |
| 7-8 | Thread Pool | Medium |
| 9-10 | Public API | Simple |
| 11 | par/pseq Integration | Simple |
| 12 | Stress Tests | Simple |
| 13-14 | Docs & Merge | Simple |

Total: 14 tasks, ~30 steps
