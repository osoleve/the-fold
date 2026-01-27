# Work-Stealing Scheduler Design

**Issue:** fold-0a9
**Date:** 2026-01-27
**Status:** Approved
**Reviewed by:** Claude Opus, Gemini Pro

## Overview

A work-stealing scheduler for The Fold's parallel evaluation. Provides efficient load balancing across CPU cores using Chase-Lev deques and Cilk-style work helping.

## Goals

- Enable true parallel execution for `par`/`pseq` forms
- Unblock: fold-h4l (parallel runtime) → fold-cpt (parallel strategies) → fold-wz94 (simulation SDK)
- Reusable lock-free deque for other concurrent algorithms

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Threading | Native OS threads (`fork-thread`) | True parallelism, multiple cores |
| Pool size | `(max 1 (- (cpu-count) 1))` | Leave core for main thread/system |
| Pool lifecycle | Global singleton, lazy init | Simple API, matches Rayon/Go |
| Chunking | Adaptive split-on-steal | Balances overhead vs load balancing |
| Fuel | Per-task budget | Scheduler stays semantics-agnostic |
| Errors | Captured as values | Matches Fold's error philosophy |
| Victim selection | Random | Industry standard, proven efficient |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Public API                           │
│  spawn, await, parallel-invoke, parallel-map, etc.     │
├─────────────────────────────────────────────────────────┤
│                   Thread Pool                           │
│  Workers, stealing protocol, work-helping               │
├─────────────────────────────────────────────────────────┤
│                 Chase-Lev Deque                         │
│  Lock-free, monotonic indices, resizable               │
└─────────────────────────────────────────────────────────┘
```

## Component 1: Chase-Lev Deque

**Location:** `lattice/data/chase-lev-deque.ss`

### Structure

```scheme
(define-record-type chase-lev-deque
  (fields
    (mutable buffer)    ; Vector of tasks (thieves compute capacity from vector-length)
    bottom              ; Box: monotonic 64-bit index (owner writes)
    top))               ; Box: monotonic 64-bit index (thieves CAS)
```

**Note:** No separate `capacity` field. Thieves compute `(vector-length buffer)` to avoid race with resize.

### Operations

```scheme
(make-chase-lev-deque initial-capacity) → deque
(deque-push! deque task)                → void (resizes if needed)
(deque-pop! deque)                      → task | 'empty
(deque-steal! deque)                    → task | 'empty | 'abort
(deque-size deque)                      → approximate count
```

### Algorithm: `deque-pop!`

```
1. Decrement bottom
2. Read top (t)
3. S = bottom - top
4. If S < 0: Reset bottom = top, return 'empty
5. If S > 0: Return buffer[bottom % len], no CAS needed
6. If S == 0 (last element, race zone):
   - CAS(top, t, t+1)
   - Success: return buffer[bottom % len]
   - Fail: reset bottom = top + 1, return 'empty
```

### Algorithm: `deque-steal!`

```
1. Read top (t)
2. Read bottom (b)
3. If t >= b: return 'empty
4. Read buffer, compute len = (vector-length buffer)
5. Read task from buffer[t % len]
6. CAS(top, t, t+1)
   - Success: return task
   - Fail: return 'abort (retry elsewhere)
```

### Algorithm: `deque-push!` with Resize

```
1. Read bottom (b), top (t)
2. If (b - t) >= (vector-length buffer):
   - Allocate new buffer (2x size)
   - Copy valid elements [t, b) with correct modular indexing
   - Set buffer to new buffer
3. Write task to buffer[b % len]
4. Increment bottom
```

## Component 2: Thread Pool

**Location:** `boundary/parallel/thread-pool.ss`

### Structure

```scheme
(define-record-type worker
  (fields
    id                  ; Worker index 0..N-1
    thread              ; OS thread handle
    deque               ; This worker's Chase-Lev deque
    (mutable state)))   ; 'idle | 'running | 'stealing | 'helping | 'shutdown

(define-record-type thread-pool
  (fields
    workers             ; Vector of workers
    (mutable running?)  ; #t when pool is active
    submit-deque        ; Deque for externally submitted tasks
    shutdown-box))      ; Box for coordinated shutdown
```

### Worker Loop

```scheme
(define (worker-loop worker pool)
  (let loop ([backoff 1])
    (cond
      [(not (pool-running? pool)) (void)]  ; Shutdown
      [(deque-pop! (worker-deque worker))
       => (lambda (task)
            (run-task task)
            (loop 1))]  ; Reset backoff on success
      [else
       (let ([stolen (try-steal pool worker)])
         (if stolen
             (begin (run-task stolen) (loop 1))
             (begin (sleep-ms (min backoff 100))
                    (loop (min (* backoff 2) 100)))))])))
```

### Stealing Protocol

```scheme
(define (try-steal pool worker)
  (let* ([workers (pool-workers pool)]
         [n (vector-length workers)]
         [start (random n)])
    (let loop ([i 0])
      (if (>= i n)
          #f  ; No work found
          (let* ([victim-idx (mod (+ start i) n)]
                 [victim (vector-ref workers victim-idx)])
            (if (= victim-idx (worker-id worker))
                (loop (+ i 1))  ; Skip self
                (case (deque-steal! (worker-deque victim))
                  [(abort) (loop (+ i 1))]  ; CAS failed, try next
                  [(empty) (loop (+ i 1))]  ; Victim empty
                  [else => identity])))))))  ; Got task
```

### Work-Helping (Deadlock Prevention)

When `await` is called, the worker doesn't block. Instead:

```scheme
(define (await-with-helping future worker pool)
  (let loop ([backoff 1])
    (let ([result (future-try-get future)])
      (if result
          result
          ;; Help while waiting
          (let ([task (or (deque-pop! (worker-deque worker))
                          (try-steal pool worker))])
            (if task
                (begin (run-task task) (loop 1))
                (begin (sleep-ms (min backoff 100))
                       (loop (min (* backoff 2) 100)))))))))
```

**Important:** Backoff applies to helping loop too, preventing CPU spin when blocked on I/O.

## Component 3: Task & Future

**Location:** `boundary/parallel/task.ss`

### Structure

```scheme
(define-record-type task
  (fields
    thunk               ; () → result
    fuel                ; Fuel budget (or #f for unlimited)
    captured-env        ; Captured dynamic environment
    promise             ; Box: #f | result | (error ...) | (suspended ...)
    done-condition      ; Condition variable
    done-mutex))        ; Mutex for condition

(define-record-type future
  (fields task))
```

### Task Execution

```scheme
(define (run-task task)
  (guard (exn [else
               (set-box! (task-promise task)
                         `(error task-failed ,(format "~a" exn)))])
    (let ([result (with-dynamic-env (task-captured-env task)
                    (task-thunk task))])
      (set-box! (task-promise task) result)))
  (with-mutex (task-done-mutex task)
    (condition-broadcast (task-done-condition task))))
```

### Suspended Task Handling

When a task exhausts fuel and returns `(suspended ...)`:
- Re-push to bottom of owner's deque (LIFO for cache locality)
- Will be retried when popped again

## Component 4: Dynamic Environment Capture

**Location:** `boundary/parallel/env-capture.ss`

```scheme
(define *tracked-parameters* '())

(define (register-parallel-parameter! param)
  (set! *tracked-parameters* (cons param *tracked-parameters*)))

(define (capture-dynamic-env)
  (map (lambda (p) (cons p (p))) *tracked-parameters*))

(define (with-dynamic-env captured thunk)
  (if (null? captured)
      (thunk)
      (parameterize ([(caar captured) (cdar captured)])
        (with-dynamic-env (cdr captured) thunk))))
```

**Warning:** Tasks using FFI are pinned to their worker thread. `dynamic-wind` handlers may run on unexpected threads.

## Component 5: Public API

**Location:** `boundary/parallel/scheduler.ss`

```scheme
;; Pool management (usually implicit)
(pool-start!)                         → void
(pool-shutdown!)                      → void
(pool-stats)                          → alist

;; Core primitives
(spawn thunk [fuel])                  → future
(await future)                        → result

;; Bulk operations
(parallel-invoke thunk ...)           → (list result ...)
(parallel-map f xs [chunk-size])      → (list (f x) ...)
(parallel-for-each f xs)              → void
```

### Adaptive Chunking for `parallel-map`

```scheme
(define *parallel-chunk-threshold* 64)

(define (parallel-map f xs)
  (let* ([n (length xs)]
         [num-workers (vector-length (pool-workers *pool*))]
         [chunk-size (max 1 (quotient n num-workers))]
         [chunks (split-into-chunks xs chunk-size)])
    (parallel-invoke
      (map (lambda (chunk)
             (lambda () (map f chunk)))
           chunks))))
```

Split-on-steal: If a chunk is stolen and has >64 elements, the thief splits it and keeps half.

## Component 6: Integration with par/pseq

**Location:** Update `core/lang/eval.ss`

```scheme
(define (eval-par-parallel a-expr b-expr env fuel ctx)
  (let* ([a-future (spawn (lambda () (eval-core a-expr env fuel #f))
                          fuel)]
         [b-result (eval-core b-expr env fuel ctx)]
         [a-result (await a-future)])
    (cond
      [(result-error? a-result) a-result]
      [(result-suspended? a-result)
       `(error par-suspended "Background suspended" ,a-expr)]
      [else b-result])))
```

## File Structure

```
lattice/data/
  chase-lev-deque.ss          # Lock-free deque
  test-chase-lev-deque.ss     # Deque tests

boundary/parallel/
  thread-pool.ss              # Worker pool
  task.ss                     # Task/Future records
  env-capture.ss              # Dynamic environment
  scheduler.ss                # Public API
  test-scheduler.ss           # Integration tests
```

## Testing Strategy

1. **Unit tests:** Deque operations, single-threaded correctness
2. **Stress tests:** Multiple workers, high contention, resize under load
3. **Correctness tests:** Results match sequential execution
4. **Deadlock tests:** Nested parallel-invoke, recursive parallel-map
5. **Fuel tests:** Suspension and resumption

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Memory ordering bugs | Use `box-cas!` (full barrier), test on weak memory systems |
| Resize race | Thieves compute capacity from `vector-length`, not separate field |
| Deadlock in nested parallelism | Work-helping in await |
| FFI pinning | Document restriction, detect and warn |

## Future Work

- Affinity hints (pin related tasks to same worker)
- Priority queues for task scheduling
- Distributed work-stealing across processes
- Integration with CUDA codegen pipeline

## References

- Chase & Lev, "Dynamic Circular Work-Stealing Deque" (2005)
- Blumofe & Leiserson, "Scheduling Multithreaded Computations by Work Stealing" (1999)
- Chez Scheme Threading Documentation
