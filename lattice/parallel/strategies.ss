(load "core/base/prelude.ss")
(load "lattice/fp/analysis/cost-analysis.ss")

(doc 'module 'strategies)
(doc 'description "Parallel Evaluation Strategies

Strategies describe task decomposition: how to split work into parallelizable
units. A strategy takes thunk descriptors and fuel, producing an execution
plan (pure S-expression data). The boundary layer interprets plans against
the work-stealing pool.

This separation keeps strategies pure, composable, and content-addressable.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(core/base/prelude.ss lattice/fp/analysis/cost-analysis.ss))
(doc 'exports '(;; Thunk type
                make-thunk thunk? thunk-id thunk-work
                ;; Plan types
                plan:seq plan:par plan:chunk plan:tree plan:race plan:pipe
                plan:seq? plan:par? plan:chunk? plan:tree? plan:race? plan:pipe?
                plan-thunks plan-chunk-size plan-combiner plan-sub-plans
                plan-stages plan? plan-type collect-plan-thunks
                ;; Strategy type
                make-strategy strategy? strategy-name strategy-transform
                ;; Core strategies
                strategy/seq strategy/par strategy/par-chunk strategy/par-buffer
                strategy/speculate
                ;; Combinators
                strategy-compose strategy-when-beneficial using
                ;; Fuel budgeting
                fuel-budget-full fuel-budget-proportional
                ;; List strategy
                strategy/par-list))

(doc 'section 'thunk-type)
(doc 'note "A Thunk is a descriptor for a suspended computation.
No actual closures here — those live in boundary. The thunk carries
an ID (for matching results back to submissions) and a cost estimate
for planning.

  (thunk <id> <estimated-work>)")

(define (make-thunk id estimated-work)
  (doc 'type '(-> Any Nat Thunk))
  (doc 'description "Create a thunk descriptor with ID and work estimate.")
  `(thunk ,id ,estimated-work))

(define (thunk? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'thunk)
       (pair? (cdr x)) (pair? (cddr x))))

(define (thunk-id t)
  (doc 'type '(-> Thunk Any))
  (cadr t))

(define (thunk-work t)
  (doc 'type '(-> Thunk Nat))
  (caddr t))

(doc 'section 'plan-types)
(doc 'note "Plans are pure S-expression data describing execution topology.
Each plan type carries the thunks it governs plus any type-specific metadata.

  (plan:seq <thunks>)                — sequential execution
  (plan:par <thunks>)                — parallel execution
  (plan:chunk <size> <thunks>)       — chunked parallel
  (plan:tree <combiner> <sub-plans>) — recursive parallel (D&C)
  (plan:race <thunks>)               — speculative: run all, take first success
  (plan:pipe <stages>)               — pipeline: producer-consumer chain

Plans compose: a plan:tree's sub-plans can be any plan type.")

(define (plan:seq thunks)
  (doc 'type '(-> (List Thunk) Plan))
  (doc 'description "Sequential execution plan.")
  `(plan:seq ,thunks))

(define (plan:par thunks)
  (doc 'type '(-> (List Thunk) Plan))
  (doc 'description "Parallel execution plan.")
  `(plan:par ,thunks))

(define (plan:chunk size thunks)
  (doc 'type '(-> Nat (List Thunk) Plan))
  (doc 'description "Chunked parallel plan: partition into size-element chunks, run chunks in parallel.")
  `(plan:chunk ,size ,thunks))

(define (plan:tree combiner sub-plans)
  (doc 'type '(-> Symbol (List Plan) Plan))
  (doc 'description "Recursive parallel plan for divide-and-conquer.
combiner names the function that merges sub-results.")
  `(plan:tree ,combiner ,sub-plans))

(define (plan:race thunks)
  (doc 'type '(-> (List Thunk) Plan))
  (doc 'description "Speculative execution: run all thunks, take first to succeed.")
  `(plan:race ,thunks))

(define (plan:pipe stages)
  (doc 'type '(-> (List (Pair Symbol (List Thunk))) Plan))
  (doc 'description "Pipeline plan: each stage feeds the next.
stages is a list of (stage-name . thunks) pairs.")
  `(plan:pipe ,stages))

;;; Plan predicates

(define (plan? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (memq (car x) '(plan:seq plan:par plan:chunk plan:tree plan:race plan:pipe))
       #t))

(define (plan-type p)
  (doc 'type '(-> Plan Symbol))
  (car p))

(define (plan:seq? p) (and (pair? p) (eq? (car p) 'plan:seq)))
(define (plan:par? p) (and (pair? p) (eq? (car p) 'plan:par)))
(define (plan:chunk? p) (and (pair? p) (eq? (car p) 'plan:chunk)))
(define (plan:tree? p) (and (pair? p) (eq? (car p) 'plan:tree)))
(define (plan:race? p) (and (pair? p) (eq? (car p) 'plan:race)))
(define (plan:pipe? p) (and (pair? p) (eq? (car p) 'plan:pipe)))

;;; Plan accessors

(define (plan-thunks p)
  (doc 'type '(-> Plan (List Thunk)))
  (doc 'description "Get thunks from a flat plan (seq/par/chunk/race).")
  (case (plan-type p)
    [(plan:seq plan:par plan:race) (cadr p)]
    [(plan:chunk) (caddr p)]
    [else (error 'plan-thunks "not a flat plan" p)]))

(define (plan-chunk-size p)
  (doc 'type '(-> Plan Nat))
  (doc 'description "Get chunk size from a plan:chunk.")
  (if (plan:chunk? p)
      (cadr p)
      (error 'plan-chunk-size "not a plan:chunk" p)))

(define (plan-combiner p)
  (doc 'type '(-> Plan Symbol))
  (doc 'description "Get combiner name from a plan:tree.")
  (if (plan:tree? p)
      (cadr p)
      (error 'plan-combiner "not a plan:tree" p)))

(define (plan-sub-plans p)
  (doc 'type '(-> Plan (List Plan)))
  (doc 'description "Get sub-plans from a plan:tree.")
  (if (plan:tree? p)
      (caddr p)
      (error 'plan-sub-plans "not a plan:tree" p)))

(define (plan-stages p)
  (doc 'type '(-> Plan (List (Pair Symbol (List Thunk)))))
  (doc 'description "Get stages from a plan:pipe.")
  (if (plan:pipe? p)
      (cadr p)
      (error 'plan-stages "not a plan:pipe" p)))

(doc 'section 'strategy-type)
(doc 'note "A Strategy wraps a transform function:
  transform : (List Thunk) × Fuel → Plan

  (strategy <name> <transform-fn>)")

(define (make-strategy name transform)
  (doc 'type '(-> Symbol (-> (List Thunk) Nat Plan) Strategy))
  (doc 'description "Create a named strategy with a transform function.")
  `(strategy ,name ,transform))

(define (strategy? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'strategy)))

(define (strategy-name s)
  (doc 'type '(-> Strategy Symbol))
  (cadr s))

(define (strategy-transform s)
  (doc 'type '(-> Strategy (-> (List Thunk) Nat Plan)))
  (caddr s))

(doc 'section 'fuel-budgeting)
(doc 'note "How to distribute fuel across parallel branches.
  - full: each branch gets the full fuel budget (matches core par semantics)
  - proportional: divide fuel by estimated work weight")

(define (fuel-budget-full fuel thunks)
  (doc 'type '(-> Nat (List Thunk) (List Nat)))
  (doc 'description "Each branch gets the full fuel budget.
Matches core eval-par semantics where both branches get full fuel.")
  (map (lambda (_) fuel) thunks))

(define (fuel-budget-proportional fuel thunks)
  (doc 'type '(-> Nat (List Thunk) (List Nat)))
  (doc 'description "Divide fuel proportionally by estimated work.")
  (let* ([total-work (fold-left + 0 (map thunk-work thunks))])
    (if (zero? total-work)
        (map (lambda (_) fuel) thunks)
        (map (lambda (t)
               (max 1 (quotient (* fuel (thunk-work t)) total-work)))
             thunks))))

(doc 'section 'core-strategies)

(doc strategy/seq 'type 'Strategy)
(doc strategy/seq 'description "Always sequential. Baseline strategy.")
(define strategy/seq
  (make-strategy 'seq
    (lambda (thunks fuel)
      (plan:seq thunks))))

(doc strategy/par 'type 'Strategy)
(doc strategy/par 'description "Parallel execution. Falls back to sequential if total work
is below *min-parallel-work* or fewer than 2 thunks.")
(define strategy/par
  (make-strategy 'par
    (lambda (thunks fuel)
      (let ([total-work (fold-left + 0 (map thunk-work thunks))])
        (if (or (< (length thunks) 2)
                (< total-work *min-parallel-work*))
            (plan:seq thunks)
            (plan:par thunks))))))

(define (strategy/par-chunk n)
  (doc 'type '(-> Nat Strategy))
  (doc 'description "Split thunks into chunks of size n, run chunks in parallel.
Within each chunk, execution is sequential.")
  (make-strategy 'par-chunk
    (lambda (thunks fuel)
      (let ([total-work (fold-left + 0 (map thunk-work thunks))])
        (if (or (< (length thunks) 2)
                (< total-work *min-parallel-work*))
            (plan:seq thunks)
            (plan:chunk n thunks))))))

(define (strategy/par-buffer max-in-flight)
  (doc 'type '(-> Nat Strategy))
  (doc 'description "Bounded parallelism: at most max-in-flight thunks executing concurrently.
Produces chunked plan where chunk-size = max-in-flight.")
  (make-strategy 'par-buffer
    (lambda (thunks fuel)
      (if (< (length thunks) 2)
          (plan:seq thunks)
          (plan:chunk max-in-flight thunks)))))

(define (strategy/speculate deadline)
  (doc 'type '(-> Nat Strategy))
  (doc 'description "Speculative execution: race all thunks with a fuel deadline.
First to complete wins. Fuel IS cancellation — branches that exhaust
their fuel budget suspend naturally.")
  (make-strategy 'speculate
    (lambda (thunks fuel)
      (let ([effective-fuel (min fuel deadline)])
        ;; Annotate thunks with deadline fuel
        (plan:race (map (lambda (t)
                          (make-thunk (thunk-id t) (min (thunk-work t) effective-fuel)))
                        thunks))))))

(define (strategy/par-list s)
  (doc 'type '(-> Strategy Strategy))
  (doc 'description "Apply strategy s to each element of a thunk list.
The resulting plan wraps the inner strategy's output for each thunk.")
  (make-strategy 'par-list
    (lambda (thunks fuel)
      (let ([inner-transform (strategy-transform s)])
        ;; Apply the inner strategy to each singleton, then combine
        (let ([sub-plans (map (lambda (t) (inner-transform (list t) fuel)) thunks)])
          (if (< (length thunks) 2)
              (if (null? sub-plans)
                  (plan:seq '())
                  (car sub-plans))
              (plan:tree 'list sub-plans)))))))

(doc 'section 'combinators)

(define (strategy-compose s1 s2)
  (doc 'type '(-> Strategy Strategy Strategy))
  (doc 'description "Compose two strategies: apply s1 first, then refine with s2.
If s1 produces a tree, s2 is applied to each leaf plan's thunks.")
  (make-strategy
   (string->symbol (format "~a+~a" (strategy-name s1) (strategy-name s2)))
   (lambda (thunks fuel)
     (let ([plan1 ((strategy-transform s1) thunks fuel)])
       (refine-plan plan1 s2 fuel)))))

(define (collect-plan-thunks plan)
  (doc 'type '(-> Plan (List Thunk)))
  (doc 'description "Collect all leaf thunks from a plan, recursing into sub-plans.
Used by refine-plan to flatten refined plans back into thunk lists.")
  (case (plan-type plan)
    [(plan:seq plan:par plan:race) (plan-thunks plan)]
    [(plan:chunk) (plan-thunks plan)]
    [(plan:tree) (apply append (map collect-plan-thunks (plan-sub-plans plan)))]
    [(plan:pipe) (apply append (map cdr (plan-stages plan)))]
    [else '()]))

(define (refine-plan plan s fuel)
  (doc 'type '(-> Plan Strategy Nat Plan))
  (doc 'description "Apply strategy s to refine leaf plans within a composite plan.")
  (let ([transform (strategy-transform s)])
    (case (plan-type plan)
      ;; Leaf plans: refine their thunks
      [(plan:seq plan:par plan:race)
       (transform (plan-thunks plan) fuel)]
      [(plan:chunk)
       (transform (plan-thunks plan) fuel)]
      ;; Tree: recurse into sub-plans
      [(plan:tree)
       (plan:tree (plan-combiner plan)
                  (map (lambda (sp) (refine-plan sp s fuel))
                       (plan-sub-plans plan)))]
      ;; Pipe: refine each stage's thunks. Since stages hold flat thunk lists
      ;; but refinement can produce any plan shape, we collect all leaf thunks
      ;; from the refined plan to reconstruct the stage.
      [(plan:pipe)
       (plan:pipe (map (lambda (stage)
                         (let ([refined (transform (cdr stage) fuel)])
                           (cons (car stage) (collect-plan-thunks refined))))
                       (plan-stages plan)))]
      [else plan])))

(define (strategy-when-beneficial s)
  (doc 'type '(-> Strategy Strategy))
  (doc 'description "Apply strategy s only when cost analysis indicates parallelization
is worthwhile. Falls back to sequential otherwise.
Uses *min-parallel-work* and *parallel-overhead* thresholds.")
  (make-strategy
   (string->symbol (format "when-beneficial:~a" (strategy-name s)))
   (lambda (thunks fuel)
     (let* ([total-work (fold-left + 0 (map thunk-work thunks))]
            [n (length thunks)]
            [overhead (* n *parallel-overhead*)]
            [min-per-task (quotient *min-parallel-work* 2)])
       (if (and (>= n 2)
                (> total-work *min-parallel-work*)
                (andmap (lambda (t) (> (thunk-work t) min-per-task)) thunks))
           ((strategy-transform s) thunks fuel)
           (plan:seq thunks))))))

(doc 'section 'main-entry-point)

(define (using strategy thunks fuel)
  (doc 'type '(-> Strategy (List Thunk) Nat Plan))
  (doc 'description "Main entry point: apply a strategy to thunks with given fuel budget.
Returns a plan suitable for execution by the boundary layer.")
  ((strategy-transform strategy) thunks fuel))
