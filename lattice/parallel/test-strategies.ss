(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/parallel/strategies.ss")

;;; ============================================================
;;; Test Suite: Parallel Evaluation Strategies (Pure)
;;; ============================================================

(test-group "thunk-construction"

  (define-test "make-thunk creates valid thunk"
    (let ([t (make-thunk 'a 100)])
      (assert-true (thunk? t))
      (assert-equal 'a (thunk-id t))
      (assert-equal 100 (thunk-work t))))

  (define-test "thunk? rejects non-thunks"
    (assert-false (thunk? '()))
    (assert-false (thunk? 42))
    (assert-false (thunk? '(not-a-thunk)))
    (assert-false (thunk? '(thunk))))

  (define-test "thunk with zero work"
    (let ([t (make-thunk 0 0)])
      (assert-true (thunk? t))
      (assert-equal 0 (thunk-work t))))

  (define-test "thunk with symbolic id"
    (let ([t (make-thunk '(compute matrix 3) 500)])
      (assert-equal '(compute matrix 3) (thunk-id t))
      (assert-equal 500 (thunk-work t)))))

(test-group "plan-construction"

  (define-test "plan:seq creates sequential plan"
    (let* ([ts (list (make-thunk 'a 10) (make-thunk 'b 20))]
           [p (plan:seq ts)])
      (assert-true (plan? p))
      (assert-true (plan:seq? p))
      (assert-equal 'plan:seq (plan-type p))
      (assert-equal 2 (length (plan-thunks p)))))

  (define-test "plan:par creates parallel plan"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 200))]
           [p (plan:par ts)])
      (assert-true (plan:par? p))
      (assert-equal 'plan:par (plan-type p))))

  (define-test "plan:chunk carries size and thunks"
    (let* ([ts (list (make-thunk 'a 10) (make-thunk 'b 20)
                     (make-thunk 'c 30) (make-thunk 'd 40))]
           [p (plan:chunk 2 ts)])
      (assert-true (plan:chunk? p))
      (assert-equal 2 (plan-chunk-size p))
      (assert-equal 4 (length (plan-thunks p)))))

  (define-test "plan:tree with sub-plans"
    (let* ([left (plan:par (list (make-thunk 'a 100)))]
           [right (plan:par (list (make-thunk 'b 200)))]
           [p (plan:tree 'merge (list left right))])
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))
      (assert-equal 2 (length (plan-sub-plans p)))))

  (define-test "plan:race creates speculative plan"
    (let* ([ts (list (make-thunk 'primary 300) (make-thunk 'fallback 100))]
           [p (plan:race ts)])
      (assert-true (plan:race? p))
      (assert-equal 2 (length (plan-thunks p)))))

  (define-test "plan:pipe creates pipeline plan"
    (let* ([stages (list (cons 'produce (list (make-thunk 'p1 50)))
                         (cons 'transform (list (make-thunk 't1 100)))
                         (cons 'consume (list (make-thunk 'c1 30))))]
           [p (plan:pipe stages)])
      (assert-true (plan:pipe? p))
      (assert-equal 3 (length (plan-stages p)))
      (assert-equal 'produce (caar (plan-stages p)))))

  (define-test "plan predicates are mutually exclusive"
    (let ([p-seq (plan:seq '())]
          [p-par (plan:par '())]
          [p-chunk (plan:chunk 2 '())]
          [p-tree (plan:tree 'merge '())]
          [p-race (plan:race '())]
          [p-pipe (plan:pipe '())])
      (assert-true (plan:seq? p-seq))
      (assert-false (plan:par? p-seq))
      (assert-true (plan:par? p-par))
      (assert-false (plan:seq? p-par))
      (assert-true (plan:chunk? p-chunk))
      (assert-true (plan:tree? p-tree))
      (assert-true (plan:race? p-race))
      (assert-true (plan:pipe? p-pipe))))

  (define-test "plan? rejects non-plans"
    (assert-false (plan? '()))
    (assert-false (plan? 42))
    (assert-false (plan? '(not-a-plan foo)))))

(test-group "strategy-construction"

  (define-test "make-strategy creates valid strategy"
    (let ([s (make-strategy 'test (lambda (ts fuel) (plan:seq ts)))])
      (assert-true (strategy? s))
      (assert-equal 'test (strategy-name s))))

  (define-test "strategy? rejects non-strategies"
    (assert-false (strategy? '()))
    (assert-false (strategy? '(not-strategy))))

  (define-test "strategy-transform is callable"
    (let* ([s (make-strategy 'identity (lambda (ts fuel) (plan:seq ts)))]
           [ts (list (make-thunk 'a 10))]
           [p ((strategy-transform s) ts 1000)])
      (assert-true (plan:seq? p)))))

(test-group "strategy/seq"

  (define-test "always produces plan:seq"
    (let* ([ts (list (make-thunk 'a 500) (make-thunk 'b 500))]
           [p (using strategy/seq ts 1000)])
      (assert-true (plan:seq? p))
      (assert-equal 2 (length (plan-thunks p)))))

  (define-test "handles empty thunks"
    (let ([p (using strategy/seq '() 1000)])
      (assert-true (plan:seq? p))
      (assert-equal 0 (length (plan-thunks p)))))

  (define-test "handles single thunk"
    (let* ([ts (list (make-thunk 'only 1000))]
           [p (using strategy/seq ts 500)])
      (assert-true (plan:seq? p))
      (assert-equal 1 (length (plan-thunks p))))))

(test-group "strategy/par"

  (define-test "parallel for substantial work"
    (let* ([ts (list (make-thunk 'a 500) (make-thunk 'b 500))]
           [p (using strategy/par ts 1000)])
      (assert-true (plan:par? p))))

  (define-test "sequential fallback for small work"
    (let* ([ts (list (make-thunk 'a 10) (make-thunk 'b 10))]
           [p (using strategy/par ts 1000)])
      ;; total-work=20 < *min-parallel-work*=200, so sequential
      (assert-true (plan:seq? p))))

  (define-test "sequential fallback for single thunk"
    (let* ([ts (list (make-thunk 'a 5000))]
           [p (using strategy/par ts 1000)])
      (assert-true (plan:seq? p))))

  (define-test "preserves thunk order"
    (let* ([ts (list (make-thunk 'first 300) (make-thunk 'second 300))]
           [p (using strategy/par ts 1000)]
           [result-thunks (plan-thunks p)])
      (assert-equal 'first (thunk-id (car result-thunks)))
      (assert-equal 'second (thunk-id (cadr result-thunks))))))

(test-group "strategy/par-chunk"

  (define-test "produces chunk plan for substantial work"
    (let* ([ts (map (lambda (i) (make-thunk i 100)) (iota 8))]
           [s (strategy/par-chunk 3)]
           [p (using s ts 1000)])
      (assert-true (plan:chunk? p))
      (assert-equal 3 (plan-chunk-size p))
      (assert-equal 8 (length (plan-thunks p)))))

  (define-test "sequential fallback for tiny work"
    (let* ([ts (list (make-thunk 'a 5) (make-thunk 'b 5))]
           [s (strategy/par-chunk 2)]
           [p (using s ts 1000)])
      (assert-true (plan:seq? p))))

  (define-test "chunk size 1 means fully parallel"
    (let* ([ts (map (lambda (i) (make-thunk i 200)) (iota 4))]
           [s (strategy/par-chunk 1)]
           [p (using s ts 1000)])
      (assert-true (plan:chunk? p))
      (assert-equal 1 (plan-chunk-size p)))))

(test-group "strategy/par-buffer"

  (define-test "produces chunk plan with buffer size"
    (let* ([ts (map (lambda (i) (make-thunk i 100)) (iota 10))]
           [s (strategy/par-buffer 3)]
           [p (using s ts 1000)])
      (assert-true (plan:chunk? p))
      (assert-equal 3 (plan-chunk-size p))))

  (define-test "single thunk stays sequential"
    (let* ([ts (list (make-thunk 'only 100))]
           [s (strategy/par-buffer 4)]
           [p (using s ts 1000)])
      (assert-true (plan:seq? p)))))

(test-group "strategy/speculate"

  (define-test "produces race plan"
    (let* ([ts (list (make-thunk 'primary 300) (make-thunk 'fallback 100))]
           [s (strategy/speculate 500)]
           [p (using s ts 1000)])
      (assert-true (plan:race? p))
      (assert-equal 2 (length (plan-thunks p)))))

  (define-test "clamps thunk work to deadline"
    (let* ([ts (list (make-thunk 'expensive 10000) (make-thunk 'cheap 50))]
           [s (strategy/speculate 200)]
           [p (using s ts 1000)]
           [race-thunks (plan-thunks p)])
      ;; Work should be clamped to min(original, effective-fuel)
      ;; effective-fuel = min(1000, 200) = 200
      (assert-true (<= (thunk-work (car race-thunks)) 200))
      (assert-true (<= (thunk-work (cadr race-thunks)) 200))))

  (define-test "deadline respects fuel budget"
    (let* ([ts (list (make-thunk 'a 100))]
           [s (strategy/speculate 5000)]
           [p (using s ts 300)]
           [race-thunks (plan-thunks p)])
      ;; effective-fuel = min(300, 5000) = 300
      (assert-true (<= (thunk-work (car race-thunks)) 300)))))

(test-group "strategy/par-list"

  (define-test "wraps single thunk without tree"
    (let* ([ts (list (make-thunk 'a 100))]
           [s (strategy/par-list strategy/par)]
           [p (using s ts 1000)])
      ;; Single element: returns inner strategy result directly
      (assert-true (plan? p))))

  (define-test "wraps multiple thunks in tree"
    (let* ([ts (list (make-thunk 'a 500) (make-thunk 'b 500)
                     (make-thunk 'c 500))]
           [s (strategy/par-list strategy/seq)]
           [p (using s ts 1000)])
      (assert-true (plan:tree? p))
      (assert-equal 'list (plan-combiner p))
      (assert-equal 3 (length (plan-sub-plans p))))))

(test-group "strategy-compose"

  (define-test "compose applies both strategies"
    (let* ([ts (list (make-thunk 'a 300) (make-thunk 'b 300))]
           ;; First strategy: try par. Second: if it was par, this refines.
           [composed (strategy-compose strategy/par strategy/seq)]
           [p (using composed ts 1000)])
      ;; s1 produces plan:par, s2 refines to plan:seq
      (assert-true (plan:seq? p))))

  (define-test "compose preserves tree structure"
    (let* ([ts (list (make-thunk 'a 500) (make-thunk 'b 500))]
           [tree-strategy (make-strategy 'make-tree
                            (lambda (thunks fuel)
                              (plan:tree 'merge
                                (map (lambda (t) (plan:seq (list t))) thunks))))]
           [composed (strategy-compose tree-strategy strategy/par)]
           [p (using composed ts 1000)])
      ;; Tree structure preserved, leaves refined
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))))

  (define-test "compose name reflects both strategies"
    (let ([composed (strategy-compose strategy/seq strategy/par)])
      (assert-equal 'seq+par (strategy-name composed)))))

(test-group "strategy-when-beneficial"

  (define-test "applies strategy when work is substantial"
    (let* ([ts (list (make-thunk 'a 500) (make-thunk 'b 500))]
           [s (strategy-when-beneficial strategy/par)]
           [p (using s ts 1000)])
      ;; total-work=1000 > threshold, both thunks > min-per-task
      (assert-true (plan:par? p))))

  (define-test "falls back to seq for tiny work"
    (let* ([ts (list (make-thunk 'a 5) (make-thunk 'b 5))]
           [s (strategy-when-beneficial strategy/par)]
           [p (using s ts 1000)])
      (assert-true (plan:seq? p))))

  (define-test "falls back when one thunk is too small"
    (let* ([ts (list (make-thunk 'big 1000) (make-thunk 'tiny 10))]
           [s (strategy-when-beneficial strategy/par)]
           [p (using s ts 1000)])
      ;; tiny.work=10 < min-per-task=100, so falls back
      (assert-true (plan:seq? p))))

  (define-test "falls back for single thunk"
    (let* ([ts (list (make-thunk 'only 5000))]
           [s (strategy-when-beneficial strategy/par)]
           [p (using s ts 1000)])
      (assert-true (plan:seq? p)))))

(test-group "fuel-budgeting"

  (define-test "fuel-budget-full gives everyone full budget"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 200) (make-thunk 'c 300))]
           [budgets (fuel-budget-full 1000 ts)])
      (assert-equal '(1000 1000 1000) budgets)))

  (define-test "fuel-budget-proportional distributes by weight"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 300))]
           [budgets (fuel-budget-proportional 1000 ts)])
      ;; a gets 100/400 * 1000 = 250, b gets 300/400 * 1000 = 750
      (assert-equal 250 (car budgets))
      (assert-equal 750 (cadr budgets))))

  (define-test "proportional handles zero-work thunks"
    (let* ([ts (list (make-thunk 'a 0) (make-thunk 'b 0))]
           [budgets (fuel-budget-proportional 1000 ts)])
      ;; zero total work → everyone gets full fuel
      (assert-equal '(1000 1000) budgets)))

  (define-test "proportional gives at least 1 fuel"
    (let* ([ts (list (make-thunk 'a 1) (make-thunk 'b 999))]
           [budgets (fuel-budget-proportional 10 ts)])
      ;; a gets max(1, 1/1000*10) = max(1,0) = 1
      (assert-true (>= (car budgets) 1)))))

(test-group "using-entry-point"

  (define-test "using applies strategy and returns plan"
    (let* ([ts (list (make-thunk 'a 100))]
           [p (using strategy/seq ts 500)])
      (assert-true (plan? p))))

  (define-test "using with empty thunks"
    (let ([p (using strategy/par '() 1000)])
      (assert-true (plan? p))))

  (define-test "using passes fuel to strategy"
    ;; Verify fuel reaches the strategy by using speculate which uses it
    (let* ([ts (list (make-thunk 'a 500))]
           [s (strategy/speculate 100)]
           [p (using s ts 200)]
           [race-thunks (plan-thunks p)])
      ;; effective-fuel = min(200, 100) = 100
      (assert-true (<= (thunk-work (car race-thunks)) 100)))))

(run-all-tests)
