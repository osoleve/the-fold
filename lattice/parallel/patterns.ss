(load "core/base/prelude.ss")
(load "lattice/parallel/strategies.ss")

(doc 'module 'patterns)
(doc 'description "Parallel Algorithm Skeletons

Higher-level patterns that produce strategies and plans for common
parallel computation shapes:
  - Divide and conquer (D&C)
  - Parallel associative reduction
  - Blelloch-style parallel prefix scan
  - Multi-stage streaming pipeline
  - Parallel merge sort

Each skeleton is a pure function that generates plan data.
The boundary layer interprets these plans against the thread pool.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(core/base/prelude.ss lattice/parallel/strategies.ss))
(doc 'exports '(par-divide-and-conquer par-reduce par-scan par-pipeline par-sort))

(doc 'section 'divide-and-conquer)

(define (par-divide-and-conquer divisible? divide combine base-case thunks fuel)
  (doc 'type '(-> (-> (List Thunk) Boolean)
                  (-> (List Thunk) (Values (List Thunk) (List Thunk)))
                  Symbol
                  (-> Thunk Plan)
                  (List Thunk)
                  Nat
                  Plan))
  (doc 'description "Generic divide-and-conquer skeleton.

  divisible?  : Can this thunk list be split further?
  divide      : Split thunks into two halves (returns pair of lists)
  combine     : Symbol naming the combiner function
  base-case   : Strategy for leaf thunks (thunk -> plan)
  thunks      : The work units
  fuel        : Recursion fuel (prevents infinite splitting)

Returns a plan:tree of nested plan:par and base-case plans.")
  (let dc ([ts thunks] [f fuel])
    (cond
      [(<= f 0) (plan:seq ts)]
      [(null? ts) (plan:seq '())]
      [(not (divisible? ts))
       ;; Base case: apply base-case to each thunk
       (if (= (length ts) 1)
           (base-case (car ts))
           ;; Multiple non-divisible thunks: wrap base-case plans in a tree
           (plan:tree combine (map base-case ts)))]
      [else
       (let-values ([(left right) (divide ts)])
         (plan:tree combine
                    (list (dc left (- f 1))
                          (dc right (- f 1)))))])))

(doc 'section 'parallel-reduction)

(define (par-reduce combiner thunks fuel)
  (doc 'type '(-> Symbol (List Thunk) Nat Plan))
  (doc 'description "Parallel associative reduction.

Builds a balanced binary tree of parallel reductions.
combiner names the associative binary function.
Requires >= 2 thunks for parallelism; single thunk returns plan:seq.

  (par-reduce 'add [(thunk 'a 100) (thunk 'b 100) (thunk 'c 100) (thunk 'd 100)] 1000)
  =>  (plan:tree add
        [(plan:tree add [(plan:seq [(thunk 'a 100)]) (plan:seq [(thunk 'b 100)])])
         (plan:tree add [(plan:seq [(thunk 'c 100)]) (plan:seq [(thunk 'd 100)])])])")
  (let reduce ([ts thunks] [f fuel])
    (cond
      [(<= f 0) (plan:seq ts)]
      [(null? ts) (plan:seq '())]
      [(= (length ts) 1) (plan:seq ts)]
      [(= (length ts) 2)
       (plan:tree combiner
                  (list (plan:seq (list (car ts)))
                        (plan:seq (list (cadr ts)))))]
      [else
       (let* ([mid (quotient (length ts) 2)]
              [left (list-head ts mid)]
              [right (list-tail ts mid)])
         (plan:tree combiner
                    (list (reduce left (- f 1))
                          (reduce right (- f 1)))))])))

(doc 'section 'parallel-prefix-scan)

(define (par-scan combiner thunks fuel)
  (doc 'type '(-> Symbol (List Thunk) Nat Plan))
  (doc 'description "Blelloch-style parallel prefix scan skeleton.

Produces a two-phase plan:
  Phase 1 (up-sweep):  Parallel pairwise reduction from leaves to root
  Phase 2 (down-sweep): Parallel distribution of partial sums back down

The plan is represented as a pipeline of two stages:
  (plan:pipe ((up-sweep . <reduce-plan>) (down-sweep . <distribute-plan>)))

Each stage is a balanced tree matching par-reduce's shape.")
  (if (< (length thunks) 2)
      (plan:seq thunks)
      (let* ([up-plan (par-reduce combiner thunks fuel)]
             ;; Down-sweep mirrors the up-sweep structure with reversed thunks
             [reversed-ts (reverse thunks)]
             [down-plan (par-reduce combiner reversed-ts fuel)])
        (plan:pipe (list (cons 'up-sweep (plan-thunks-deep up-plan))
                         (cons 'down-sweep (plan-thunks-deep down-plan)))))))

(define (plan-thunks-deep plan)
  (doc 'type '(-> Plan (List Thunk)))
  (doc 'description "Collect all thunks from a plan, recursing into sub-plans.")
  (case (plan-type plan)
    [(plan:seq plan:par plan:race) (plan-thunks plan)]
    [(plan:chunk) (plan-thunks plan)]
    [(plan:tree)
     (apply append (map plan-thunks-deep (plan-sub-plans plan)))]
    [(plan:pipe)
     (apply append (map cdr (plan-stages plan)))]
    [else '()]))

(doc 'section 'parallel-pipeline)

(define (par-pipeline stages fuel)
  (doc 'type '(-> (List (Pair Symbol (List Thunk))) Nat Plan))
  (doc 'description "Multi-stage streaming pipeline.

stages is a list of (name . thunks) pairs. Each stage can process
its thunks in parallel, with inter-stage synchronization.

  (par-pipeline '((parse . [t1 t2]) (transform . [t3 t4]) (emit . [t5]))
                1000)
  => (plan:pipe ((parse t1 t2) (transform t3 t4) (emit t5)))")
  (if (null? stages)
      (plan:seq '())
      (plan:pipe stages)))

(doc 'section 'parallel-sort)

(define (par-sort thunks fuel)
  (doc 'type '(-> (List Thunk) Nat Plan))
  (doc 'description "Parallel merge sort skeleton.

Built on divide-and-conquer with:
  - divisible? : more than one thunk
  - divide     : split at midpoint
  - combine    : merge
  - base-case  : single-element plan:seq

Produces a plan:tree of nested merges.")
  (par-divide-and-conquer
   ;; divisible?: more than 1 element
   (lambda (ts) (> (length ts) 1))
   ;; divide: split at midpoint
   (lambda (ts)
     (let ([mid (quotient (length ts) 2)])
       (values (list-head ts mid) (list-tail ts mid))))
   ;; combine: merge
   'merge
   ;; base-case: single thunk as seq plan
   (lambda (t) (plan:seq (list t)))
   thunks
   fuel))

;;; Utility: list-head (take first n elements)
(define (list-head lst n)
  (doc 'type '(-> (List a) Nat (List a)))
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

;;; Utility: list-tail is already built-in to Chez
