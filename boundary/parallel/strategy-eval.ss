(load "core/base/prelude.ss")
(load "lattice/parallel/strategies.ss")
(load "boundary/parallel/scheduler.ss")

(doc 'module 'strategy-eval)
(doc 'description "Plan Execution Engine

Interprets plan data structures (from lattice/parallel/strategies.ss)
against the work-stealing thread pool. This is the boundary between
pure strategy computation and impure parallel execution.

Key insight for speculation: No thread cancellation in Chez.
Fuel IS cancellation — speculative branches with limited fuel
suspend naturally. The race plan just ignores loser results.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude.ss
                     lattice/parallel/strategies.ss
                     boundary/parallel/scheduler.ss))
(doc 'exports '(run-plan run-with-strategy))

(doc 'section 'thunk-registry)
(doc 'note "Plans reference thunks by ID. The executor needs a mapping from
thunk IDs to actual closures. This registry is passed through run-plan.")

(define (make-thunk-registry entries)
  (doc 'type '(-> (List (Pair Any (-> Any))) ThunkRegistry))
  (doc 'description "Create a registry mapping thunk IDs to closures.
entries is a list of (id . closure) pairs.")
  entries)

(define (registry-lookup registry id)
  (doc 'type '(-> ThunkRegistry Any (U (-> Any) #f)))
  (doc 'description "Look up a closure by thunk ID.")
  (let ([entry (assoc id registry)])
    (if entry (cdr entry) #f)))

(doc 'section 'plan-execution)

(define (run-plan plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Execute a plan using the work-stealing thread pool.
Dispatches on plan type:
  plan:seq   → sequential map
  plan:par   → spawn all, await all
  plan:chunk → split into chunks, spawn per chunk
  plan:tree  → recursive run-plan on sub-plans
  plan:race  → spawn all, return first completed
  plan:pipe  → sequential stages, parallel within stages

Returns a list of results in thunk order.")
  (case (plan-type plan)
    [(plan:seq)   (run-plan-seq plan registry)]
    [(plan:par)   (run-plan-par plan registry)]
    [(plan:chunk) (run-plan-chunk plan registry)]
    [(plan:tree)  (run-plan-tree plan registry)]
    [(plan:race)  (run-plan-race plan registry)]
    [(plan:pipe)  (run-plan-pipe plan registry)]
    [else (error 'run-plan "unknown plan type" (plan-type plan))]))

(define (run-thunk registry thunk)
  (doc 'type '(-> ThunkRegistry Thunk Any))
  (doc 'description "Execute a single thunk by looking up its closure in the registry.")
  (let ([closure (registry-lookup registry (thunk-id thunk))])
    (if closure
        (closure)
        (error 'run-thunk "thunk not found in registry" (thunk-id thunk)))))

(define (run-plan-seq plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Sequential execution: run thunks one at a time.")
  (map (lambda (t) (run-thunk registry t)) (plan-thunks plan)))

(define (run-plan-par plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Parallel execution: spawn all thunks, await results in order.")
  (let* ([thunks (plan-thunks plan)]
         [futures (map (lambda (t)
                         (spawn (lambda () (run-thunk registry t))))
                       thunks)])
    (map await futures)))

(define (run-plan-chunk plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Chunked parallel: group thunks, run groups in parallel,
sequential within each group. Clamps chunk-size to minimum 1.")
  (let* ([raw-size (plan-chunk-size plan)]
         [chunk-size (max 1 raw-size)]
         [thunks (plan-thunks plan)]
         [chunks (split-list thunks chunk-size)]
         [futures (map (lambda (chunk)
                         (spawn (lambda ()
                                  (map (lambda (t) (run-thunk registry t)) chunk))))
                       chunks)])
    (apply append (map await futures))))

(define (run-plan-tree plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Recursive parallel: spawn sub-plans in parallel, collect results.")
  (let* ([sub-plans (plan-sub-plans plan)]
         [futures (map (lambda (sp)
                         (spawn (lambda () (run-plan sp registry))))
                       sub-plans)])
    (apply append (map await futures))))

(define (run-plan-race plan registry)
  (doc 'type '(-> Plan ThunkRegistry Any))
  (doc 'description "Speculative execution: spawn all, return first to complete.
Other branches are left to exhaust their fuel naturally.
Returns a single result (the winner), not a list.
Empty race returns (void). If all thunks error, returns the first error.")
  (let ([thunks (plan-thunks plan)])
    (if (null? thunks)
        (void)
        (let* ([result-box (box #f)]
               [error-count (box 0)]
               [first-error (box #f)]
               [n (length thunks)]
               [done-mutex (make-mutex)]
               [done-cond (make-condition)]
               [done-flag (box #f)])
          ;; Spawn all thunks
          (for-each
           (lambda (t)
             (spawn (lambda ()
                      (guard (e [#t
                                 (with-mutex done-mutex
                                   (unless (unbox done-flag)
                                     (unless (unbox first-error)
                                       (set-box! first-error e))
                                     (set-box! error-count (+ 1 (unbox error-count)))
                                     (when (= (unbox error-count) n)
                                       ;; All failed — unblock waiter
                                       (set-box! done-flag #t)
                                       (condition-broadcast done-cond))))])
                        (let ([result (run-thunk registry t)])
                          (with-mutex done-mutex
                            (unless (unbox done-flag)
                              (set-box! result-box result)
                              (set-box! done-flag #t)
                              (condition-broadcast done-cond)))
                          result)))))
           thunks)
          ;; Wait for first completion or all-failure
          (with-mutex done-mutex
            (let loop ()
              (if (unbox done-flag)
                  (if (unbox result-box)
                      (unbox result-box)
                      ;; All errored — re-raise first error
                      (if (unbox first-error)
                          (raise (unbox first-error))
                          (void)))
                  (begin
                    (condition-wait done-cond done-mutex)
                    (loop)))))))))

(define (run-plan-pipe plan registry)
  (doc 'type '(-> Plan ThunkRegistry (List Any)))
  (doc 'description "Pipeline execution: stages run sequentially,
thunks within each stage run in parallel.
Returns results from the final stage.")
  (let loop ([stages (plan-stages plan)] [prev-results '()])
    (if (null? stages)
        prev-results
        (let* ([stage (car stages)]
               [stage-thunks (cdr stage)]
               [stage-plan (plan:par stage-thunks)]
               [results (run-plan-par stage-plan registry)])
          (loop (cdr stages) results)))))

(doc 'section 'convenience-api)

(define (run-with-strategy strategy thunk-specs fuel)
  (doc 'type '(-> Strategy (List (Pair Any (-> Any))) Nat (List Any)))
  (doc 'description "Convenience: create thunks from specs, plan with strategy, execute.

thunk-specs is a list of (id . closure) pairs. Work is estimated as 100
for each thunk (a reasonable default when static analysis isn't available).

Returns a list of results in thunk order.")
  (let* ([thunks (map (lambda (spec)
                        (make-thunk (car spec) 100))
                      thunk-specs)]
         [plan (using strategy thunks fuel)]
         [registry (make-thunk-registry thunk-specs)]
         [result (run-plan plan registry)])
    ;; Normalize: race returns scalar, wrap it for consistent list contract
    (if (plan:race? plan)
        (list result)
        result)))

(doc 'section 'utilities)

(define (split-list lst n)
  (doc 'type '(-> (List a) Nat (List (List a))))
  (doc 'description "Split list into sub-lists of at most n elements.")
  (if (null? lst)
      '()
      (let loop ([remaining lst] [acc '()])
        (if (null? remaining)
            (reverse acc)
            (let-values ([(chunk rest) (take-at-most remaining n)])
              (loop rest (cons chunk acc)))))))

(define (take-at-most lst n)
  (doc 'type '(-> (List a) Nat (Values (List a) (List a))))
  (let loop ([lst lst] [n n] [acc '()])
    (if (or (null? lst) (zero? n))
        (values (reverse acc) lst)
        (loop (cdr lst) (- n 1) (cons (car lst) acc)))))
