;;; chase-lev-deque.sexp — Development tasks for lattice/data/chase-lev-deque.ss
;;;
;;; Module: chase-lev-deque (tier 0, depends on prelude)
;;; 159 lines, ~10 exported functions
;;; Lock-free Chase-Lev work-stealing deque. Owner pushes/pops from bottom
;;; (LIFO), thieves steal from top (FIFO). Uses monotonic indices and
;;; generation counters to avoid ABA problems.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/data/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement deque-empty?
  ;; ──────────────────────────────────────────────
  ;; The simplest query — is the deque empty?

  (task
    (id "data/chase-lev-deque/deque-empty-impl")
    (module chase-lev-deque)
    (tier 0)
    (type implement)
    (description "Implement deque-empty?: check if the deque contains no elements. The deque tracks two monotonic indices: bottom (where the owner pushes/pops) and top (where thieves steal). The deque is empty when bottom <= top. Use the accessor functions deque-bottom and deque-top to read the indices. This is a racy check (another thread could modify the deque concurrently), but it's useful for debugging and single-threaded contexts.")
    (context "Available: deque-bottom (returns bottom index), deque-top (returns top index), <=. One comparison.")
    (before "(define (deque-empty? d)
  ;; TODO: check if bottom <= top
  #t)")
    (test-file "lattice/data/test-chase-lev-deque.ss")
    (test-forms (";; New deque is empty
(let ([d (make-chase-lev-deque 16)])
  (assert-true (deque-empty? d)))"
                 ";; After push, not empty
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'task-1)
  (assert-false (deque-empty? d)))"
                 ";; After push then pop, empty again
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'x)
  (deque-pop! d)
  (assert-true (deque-empty? d)))"))
    (available-modules (prelude))
    (hints ("Read bottom = (deque-bottom d)"
            "Read top = (deque-top d)"
            "Return (<= bottom top)"))
    (reference-solution "(define (deque-empty? d)
  (<= (deque-bottom d) (deque-top d)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement deque-size
  ;; ──────────────────────────────────────────────
  ;; Approximate element count from monotonic indices.

  (task
    (id "data/chase-lev-deque/deque-size-impl")
    (module chase-lev-deque)
    (tier 0)
    (type implement)
    (description "Implement deque-size: compute the approximate number of elements in the deque. Size = bottom - top, but clamped to at least 0 (since indices are read non-atomically, bottom < top is transiently possible during concurrent operations). Use max to ensure non-negative result. This is an approximation — the true size may change between reading bottom and top — but it's useful for load-balancing decisions in work-stealing schedulers.")
    (context "Available: deque-bottom, deque-top, max, -. One subtraction with a clamp.")
    (before "(define (deque-size d)
  ;; TODO: approximate size = max(0, bottom - top)
  0)")
    (test-file "lattice/data/test-chase-lev-deque.ss")
    (test-forms (";; Empty deque has size 0
(let ([d (make-chase-lev-deque 16)])
  (assert-equal 0 (deque-size d)))"
                 ";; After pushes
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'a)
  (deque-push! d 'b)
  (deque-push! d 'c)
  (assert-equal 3 (deque-size d)))"
                 ";; After push and pop
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'x)
  (deque-push! d 'y)
  (deque-pop! d)
  (assert-equal 1 (deque-size d)))"))
    (available-modules (prelude))
    (hints ("Compute (- (deque-bottom d) (deque-top d))"
            "Clamp: (max 0 ...)"))
    (reference-solution "(define (deque-size d)
  (max 0 (- (deque-bottom d) (deque-top d))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement deque-push!
  ;; ──────────────────────────────────────────────
  ;; Owner pushes to bottom — the core mutating operation.

  (task
    (id "data/chase-lev-deque/deque-push-impl")
    (module chase-lev-deque)
    (tier 0)
    (type implement)
    (description "Implement deque-push!: the owner pushes a task to the bottom of the deque. Steps: (1) Read current bottom (b) and top (t) indices, and the buffer. (2) If the deque is full (b - t >= capacity), call deque-resize! to double the buffer. (3) Re-read the buffer after potential resize. (4) Write the task into buf[b mod capacity]. (5) Atomically increment bottom using a CAS loop: read current bottom, attempt box-cas! to set it to b+1, retry if CAS fails. The CAS on bottom acts as a memory barrier ensuring the task write is visible before bottom is updated.")
    (context "Available: deque-bottom, deque-top, deque-buffer, deque-bottom-box, deque-resize!, vector-length, vector-set!, mod, >=, -, +, box-cas!, let*, let, when, unless. CAS loop for atomic increment.")
    (before "(define (deque-push! d task)
  ;; TODO: push task to bottom, resize if full, CAS increment bottom
  (void))")
    (test-file "lattice/data/test-chase-lev-deque.ss")
    (test-forms (";; Push adds element
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'task-1)
  (assert-equal 1 (deque-size d))
  (assert-false (deque-empty? d)))"
                 ";; Push multiple
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'a)
  (deque-push! d 'b)
  (deque-push! d 'c)
  (assert-equal 3 (deque-size d)))"
                 ";; Push triggers resize
(let ([d (make-chase-lev-deque 4)])
  (deque-push! d 1)
  (deque-push! d 2)
  (deque-push! d 3)
  (deque-push! d 4)
  (deque-push! d 5)
  (assert-equal 5 (deque-size d)))"))
    (available-modules (prelude))
    (hints ("Read b, t, buf, cap"
            "Resize check: (when (>= (- b t) cap) (deque-resize! d))"
            "Re-read buf after resize"
            "Write: (vector-set! buf (mod b cap) task)"
            "CAS loop: retry (box-cas! (deque-bottom-box d) cur-b (+ b 1)) until success"))
    (reference-solution "(define (deque-push! d task)
  (let* ([b (deque-bottom d)]
         [t (deque-top d)]
         [buf (deque-buffer d)]
         [cap (vector-length buf)])
    (when (>= (- b t) cap)
      (deque-resize! d))
    (let ([buf (deque-buffer d)]
          [cap (vector-length (deque-buffer d))])
      (vector-set! buf (mod b cap) task)
      (let loop ()
        (let ([cur-b (deque-bottom d)])
          (unless (box-cas! (deque-bottom-box d) cur-b (+ b 1))
            (loop)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement deque-pop!
  ;; ──────────────────────────────────────────────
  ;; Owner pops from bottom — must handle contention with thieves.

  (task
    (id "data/chase-lev-deque/deque-pop-impl")
    (module chase-lev-deque)
    (tier 0)
    (type implement)
    (description "Implement deque-pop!: the owner pops from the bottom of the deque (LIFO). Three cases after decrementing bottom to b = bottom-1: (1) b < top: deque was empty, restore bottom to top, return 'empty. (2) b > top: multiple elements remain, safe to take buf[b mod cap], clear the slot for GC. (3) b = top: last element, race with thieves — use CAS on top to claim it. If CAS succeeds, set bottom to top+1, clear slot, return task. If CAS fails (thief won), set bottom to top+1, return 'empty. The bottom decrement uses a CAS loop as a memory barrier.")
    (context "Available: deque-bottom, deque-top, deque-buffer, deque-bottom-box, deque-top-box, vector-length, vector-ref, vector-set!, mod, <, >, -, +, box-cas!, set-box!, let*, let, cond, unless. Three-case dispatch with CAS.")
    (before "(define (deque-pop! d)
  ;; TODO: pop from bottom, handle empty and last-element race
  'empty)")
    (test-file "lattice/data/test-chase-lev-deque.ss")
    (test-forms (";; Pop returns LIFO order
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'first)
  (deque-push! d 'second)
  (deque-push! d 'third)
  (assert-equal 'third (deque-pop! d))
  (assert-equal 'second (deque-pop! d))
  (assert-equal 'first (deque-pop! d)))"
                 ";; Pop on empty returns 'empty
(let ([d (make-chase-lev-deque 16)])
  (assert-equal 'empty (deque-pop! d)))"
                 ";; Pop after emptying returns 'empty
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'x)
  (deque-pop! d)
  (assert-equal 'empty (deque-pop! d)))"))
    (available-modules (prelude))
    (hints ("Decrement: b = (- (deque-bottom d) 1)"
            "CAS loop to set bottom to b (memory barrier)"
            "Read t = (deque-top d)"
            "If (< b t): restore (set-box! (deque-bottom-box d) t), return 'empty"
            "If (> b t): read buf[b mod cap], clear slot, return task"
            "If (= b t): CAS on top-box from t to t+1. Win → return task, lose → return 'empty. Either way set bottom to t+1"))
    (reference-solution "(define (deque-pop! d)
  (let* ([b (- (deque-bottom d) 1)])
    (let loop ()
      (let ([cur-b (deque-bottom d)])
        (unless (box-cas! (deque-bottom-box d) cur-b b)
          (loop))))
    (let ([t (deque-top d)])
      (cond
        [(< b t)
         (set-box! (deque-bottom-box d) t)
         'empty]
        [(> b t)
         (let* ([buf (deque-buffer d)]
                [cap (vector-length buf)]
                [task (vector-ref buf (mod b cap))])
           (vector-set! buf (mod b cap) #f)
           task)]
        [else
         (let* ([buf (deque-buffer d)]
                [cap (vector-length buf)]
                [task (vector-ref buf (mod b cap))])
           (if (box-cas! (deque-top-box d) t (+ t 1))
               (begin
                 (set-box! (deque-bottom-box d) (+ t 1))
                 (vector-set! buf (mod b cap) #f)
                 task)
               (begin
                 (set-box! (deque-bottom-box d) (+ t 1))
                 'empty)))]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement deque-steal!
  ;; ──────────────────────────────────────────────
  ;; Thief steals from top — FIFO, generation-checked.

  (task
    (id "data/chase-lev-deque/deque-steal-impl")
    (module chase-lev-deque)
    (tier 0)
    (type implement)
    (description "Implement deque-steal!: a thief steals from the top of the deque (FIFO). Returns the task, 'empty, or 'abort. Steps: (1) Read generation counter gen1. (2) Read top and bottom indices. (3) If top >= bottom, return 'empty. (4) Read task from buf[top mod cap]. (5) Read generation gen2. If gen1 != gen2, a resize happened — return 'abort (task may be in wrong buffer). (6) CAS on top from t to t+1. If CAS succeeds, clear the slot and return task. If CAS fails (another thief won), return 'abort. The generation check prevents reading stale data from the old buffer after a resize.")
    (context "Available: deque-generation, deque-top, deque-bottom, deque-buffer, deque-top-box, vector-length, vector-ref, vector-set!, mod, >=, =, not, +, box-cas!, let*, if, begin. Generation-guarded CAS.")
    (before "(define (deque-steal! d)
  ;; TODO: steal from top, check generation, CAS on top
  'empty)")
    (test-file "lattice/data/test-chase-lev-deque.ss")
    (test-forms (";; Steal returns FIFO order (oldest first)
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'first)
  (deque-push! d 'second)
  (deque-push! d 'third)
  (assert-equal 'first (deque-steal! d))
  (assert-equal 'second (deque-steal! d)))"
                 ";; Steal on empty returns 'empty
(let ([d (make-chase-lev-deque 16)])
  (assert-equal 'empty (deque-steal! d)))"
                 ";; Steal and pop work together
(let ([d (make-chase-lev-deque 16)])
  (deque-push! d 'a)
  (deque-push! d 'b)
  (deque-push! d 'c)
  (assert-equal 'a (deque-steal! d))
  (assert-equal 'c (deque-pop! d))
  (assert-equal 'b (deque-pop! d)))"))
    (available-modules (prelude))
    (hints ("Read gen1 = (deque-generation d)"
            "Read t = (deque-top d), b = (deque-bottom d)"
            "If (>= t b) → 'empty"
            "Read task from buf[t mod cap]"
            "Read gen2 = (deque-generation d)"
            "If (not (= gen1 gen2)) → 'abort"
            "CAS: (box-cas! (deque-top-box d) t (+ t 1))"
            "Success → clear slot, return task. Failure → 'abort"))
    (reference-solution "(define (deque-steal! d)
  (let* ([gen1 (deque-generation d)]
         [t (deque-top d)]
         [b (deque-bottom d)])
    (if (>= t b)
        'empty
        (let* ([buf (deque-buffer d)]
               [cap (vector-length buf)]
               [task (vector-ref buf (mod t cap))]
               [gen2 (deque-generation d)])
          (if (not (= gen1 gen2))
              'abort
              (if (box-cas! (deque-top-box d) t (+ t 1))
                  (begin
                    (vector-set! buf (mod t cap) #f)
                    task)
                  'abort))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
