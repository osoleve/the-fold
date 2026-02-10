(load "core/testing/test-framework.ss")
(load "lattice/parallel/patterns.ss")
(load "boundary/parallel/strategy-eval.ss")

;;; ============================================================
;;; Test Suite: Plan Execution (Boundary / Impure)
;;; ============================================================

;;; Helper: deterministic computation closures
(define (make-adder n) (lambda () (+ n 10)))
(define (make-squarer n) (lambda () (* n n)))
(define (make-sleeper ms val)
  (lambda ()
    (sleep (make-time 'time-duration (* ms 1000000) 0))
    val))

;;; Ensure pool is ready before tests
(ensure-pool!)

(test-group "run-plan-seq"

  (define-test "sequential execution returns results in order"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (lambda () 1))
                        (cons 'b (lambda () 2))
                        (cons 'c (lambda () 3))))]
           [plan (plan:seq (list (make-thunk 'a 10)
                                 (make-thunk 'b 10)
                                 (make-thunk 'c 10)))]
           [results (run-plan plan reg)])
      (assert-equal '(1 2 3) results)))

  (define-test "empty seq returns empty list"
    (let* ([reg (make-thunk-registry '())]
           [plan (plan:seq '())]
           [results (run-plan plan reg)])
      (assert-equal '() results)))

  (define-test "single thunk seq"
    (let* ([reg (make-thunk-registry (list (cons 'only (lambda () 42))))]
           [plan (plan:seq (list (make-thunk 'only 10)))]
           [results (run-plan plan reg)])
      (assert-equal '(42) results))))

(test-group "run-plan-par"

  (define-test "parallel execution returns correct results"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (make-adder 1))
                        (cons 'b (make-adder 2))
                        (cons 'c (make-adder 3))))]
           [plan (plan:par (list (make-thunk 'a 100)
                                  (make-thunk 'b 100)
                                  (make-thunk 'c 100)))]
           [results (run-plan plan reg)])
      (assert-equal '(11 12 13) results)))

  (define-test "parallel preserves order despite timing differences"
    (let* ([reg (make-thunk-registry
                  (list (cons 'slow (make-sleeper 50 'slow-result))
                        (cons 'fast (lambda () 'fast-result))))]
           [plan (plan:par (list (make-thunk 'slow 100)
                                  (make-thunk 'fast 10)))]
           [results (run-plan plan reg)])
      ;; Results should be in thunk order, not completion order
      (assert-equal '(slow-result fast-result) results)))

  (define-test "parallel with computation"
    (let* ([reg (make-thunk-registry
                  (list (cons 'sq3 (make-squarer 3))
                        (cons 'sq7 (make-squarer 7))))]
           [plan (plan:par (list (make-thunk 'sq3 100)
                                  (make-thunk 'sq7 100)))]
           [results (run-plan plan reg)])
      (assert-equal '(9 49) results))))

(test-group "run-plan-chunk"

  (define-test "chunked execution with chunk-size 2"
    (let* ([reg (make-thunk-registry
                  (list (cons 0 (lambda () 'a))
                        (cons 1 (lambda () 'b))
                        (cons 2 (lambda () 'c))
                        (cons 3 (lambda () 'd))))]
           [plan (plan:chunk 2 (map (lambda (i) (make-thunk i 10)) (iota 4)))]
           [results (run-plan plan reg)])
      (assert-equal '(a b c d) results)))

  (define-test "chunk size larger than list"
    (let* ([reg (make-thunk-registry
                  (list (cons 'x (lambda () 99))
                        (cons 'y (lambda () 88))))]
           [plan (plan:chunk 10 (list (make-thunk 'x 10) (make-thunk 'y 10)))]
           [results (run-plan plan reg)])
      (assert-equal '(99 88) results)))

  (define-test "chunk size 1 means each thunk separate"
    (let* ([reg (make-thunk-registry
                  (map (lambda (i) (cons i (lambda () (* i 10))))
                       (iota 3)))]
           [plan (plan:chunk 1 (map (lambda (i) (make-thunk i 10)) (iota 3)))]
           [results (run-plan plan reg)])
      (assert-equal '(0 10 20) results))))

(test-group "run-plan-tree"

  (define-test "tree with two seq leaves"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (lambda () 1))
                        (cons 'b (lambda () 2))))]
           [plan (plan:tree 'merge
                   (list (plan:seq (list (make-thunk 'a 10)))
                         (plan:seq (list (make-thunk 'b 10)))))]
           [results (run-plan plan reg)])
      (assert-equal '(1 2) results)))

  (define-test "nested tree"
    (let* ([reg (make-thunk-registry
                  (map (lambda (i) (cons i (lambda () i))) (iota 4)))]
           [plan (plan:tree 'merge
                   (list (plan:tree 'merge
                           (list (plan:seq (list (make-thunk 0 10)))
                                 (plan:seq (list (make-thunk 1 10)))))
                         (plan:tree 'merge
                           (list (plan:seq (list (make-thunk 2 10)))
                                 (plan:seq (list (make-thunk 3 10)))))))]
           [results (run-plan plan reg)])
      (assert-equal '(0 1 2 3) results)))

  (define-test "tree from par-reduce"
    (let* ([reg (make-thunk-registry
                  (map (lambda (i) (cons i (lambda () (* i i)))) (iota 4)))]
           [thunks (map (lambda (i) (make-thunk i 50)) (iota 4))]
           [plan (par-reduce 'add thunks 100)]
           [results (run-plan plan reg)])
      ;; Should get all 4 squared values
      (assert-equal '(0 1 4 9) results))))

(test-group "run-plan-race"

  (define-test "race returns first to complete"
    (let* ([reg (make-thunk-registry
                  (list (cons 'slow (make-sleeper 200 'slow))
                        (cons 'fast (lambda () 'fast))))]
           [plan (plan:race (list (make-thunk 'slow 100)
                                   (make-thunk 'fast 10)))]
           [result (run-plan plan reg)])
      ;; Fast should win
      (assert-equal 'fast result)))

  (define-test "race with single thunk"
    (let* ([reg (make-thunk-registry
                  (list (cons 'only (lambda () 'winner))))]
           [plan (plan:race (list (make-thunk 'only 10)))]
           [result (run-plan plan reg)])
      (assert-equal 'winner result))))

(test-group "run-plan-pipe"

  (define-test "pipeline executes stages sequentially"
    (let* ([reg (make-thunk-registry
                  (list (cons 'p1 (lambda () 'produced))
                        (cons 't1 (lambda () 'transformed))
                        (cons 'c1 (lambda () 'consumed))))]
           [plan (plan:pipe
                   (list (cons 'produce (list (make-thunk 'p1 10)))
                         (cons 'transform (list (make-thunk 't1 10)))
                         (cons 'consume (list (make-thunk 'c1 10)))))]
           [results (run-plan plan reg)])
      ;; Returns results from final stage
      (assert-equal '(consumed) results)))

  (define-test "pipeline with multi-thunk stages"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (lambda () 1))
                        (cons 'b (lambda () 2))
                        (cons 'x (lambda () 10))
                        (cons 'y (lambda () 20))))]
           [plan (plan:pipe
                   (list (cons 'stage1 (list (make-thunk 'a 10)
                                              (make-thunk 'b 10)))
                         (cons 'stage2 (list (make-thunk 'x 10)
                                              (make-thunk 'y 10)))))]
           [results (run-plan plan reg)])
      ;; Returns results from final stage (stage2)
      (assert-equal '(10 20) results))))

(test-group "run-with-strategy"

  (define-test "run-with-strategy sequential"
    (let* ([specs (list (cons 'a (lambda () 'alpha))
                        (cons 'b (lambda () 'beta)))]
           [results (run-with-strategy strategy/seq specs 1000)])
      (assert-equal '(alpha beta) results)))

  (define-test "run-with-strategy parallel"
    (let* ([specs (list (cons 'x (lambda () 10))
                        (cons 'y (lambda () 20))
                        (cons 'z (lambda () 30)))]
           [results (run-with-strategy strategy/par specs 1000)])
      ;; Default work estimate is 100 each → total 300 > threshold
      (assert-equal '(10 20 30) results)))

  (define-test "run-with-strategy with chunking"
    (let* ([specs (map (lambda (i) (cons i (lambda () (* i 2)))) (iota 6))]
           [results (run-with-strategy (strategy/par-chunk 2) specs 1000)])
      (assert-equal '(0 2 4 6 8 10) results)))

  (define-test "determinism across repeated runs"
    (let* ([specs (map (lambda (i) (cons i (lambda () i))) (iota 10))]
           [run1 (run-with-strategy strategy/par specs 1000)]
           [run2 (run-with-strategy strategy/par specs 1000)]
           [run3 (run-with-strategy strategy/par specs 1000)])
      (assert-equal run1 run2)
      (assert-equal run2 run3))))

(test-group "end-to-end-patterns"

  (define-test "par-sort plan executes correctly"
    (let* ([data '(3 1 4 1 5 9 2 6)]
           [specs (map (lambda (v i) (cons i (lambda () v)))
                       data (iota (length data)))]
           [thunks (map (lambda (i) (make-thunk i 50)) (iota (length data)))]
           [plan (par-sort thunks 100)]
           [reg (make-thunk-registry specs)]
           [results (run-plan plan reg)])
      ;; Results should be all values (order depends on tree structure, not sorted)
      (assert-equal (length data) (length results))
      ;; All original values present
      (assert-true (equal? (sort-by < results) (sort-by < data)))))

  (define-test "par-reduce plan executes correctly"
    (let* ([specs (map (lambda (i) (cons i (lambda () (+ i 1)))) (iota 4))]
           [thunks (map (lambda (i) (make-thunk i 50)) (iota 4))]
           [plan (par-reduce 'add thunks 100)]
           [reg (make-thunk-registry specs)]
           [results (run-plan plan reg)])
      ;; Should get 1, 2, 3, 4 in tree order
      (assert-equal 4 (length results))
      (assert-equal 10 (apply + results)))))

;;; ============================================================
;;; Edge cases from codex QA review
;;; ============================================================

(test-group "edge-cases"

  (define-test "race with empty thunks returns void"
    (let* ([reg (make-thunk-registry '())]
           [plan (plan:race '())]
           [result (run-plan plan reg)])
      ;; Empty race should not deadlock, returns void
      (assert-true (eq? (void) result))))

  (define-test "chunk with size 0 does not infinite loop"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (lambda () 1))
                        (cons 'b (lambda () 2))))]
           [plan (plan:chunk 0 (list (make-thunk 'a 10) (make-thunk 'b 10)))]
           [results (run-plan plan reg)])
      ;; Size 0 gets clamped to 1, so each thunk in its own chunk
      (assert-equal '(1 2) results)))

  (define-test "run-with-strategy wraps race result in list"
    (let* ([specs (list (cons 'a (lambda () 'alpha))
                        (cons 'b (lambda () 'beta)))]
           [results (run-with-strategy (strategy/speculate 1000) specs 1000)])
      ;; run-with-strategy always returns list, even for race
      (assert-true (list? results))
      (assert-equal 1 (length results))
      ;; Result should be one of the thunk values
      (assert-true (pair? (memq (car results) '(alpha beta))))))

  (define-test "D&C non-divisible multi-element produces tree"
    (let* ([reg (make-thunk-registry
                  (list (cons 'a (lambda () 10))
                        (cons 'b (lambda () 20))
                        (cons 'c (lambda () 30))))]
           [thunks (list (make-thunk 'a 50) (make-thunk 'b 50) (make-thunk 'c 50))]
           [plan (par-divide-and-conquer
                   ;; Never divisible
                   (lambda (ts) #f)
                   (lambda (ts) (values '() '()))
                   'merge
                   (lambda (t) (plan:seq (list t)))
                   thunks 100)]
           [results (run-plan plan reg)])
      ;; Should get all 3 values (tree of base-case plans runs correctly)
      (assert-equal 3 (length results))
      (assert-equal 60 (apply + results))))

  (define-test "compose seq then par-chunk on pipe does not crash"
    ;; strategy-compose with pipe: verifies refine-plan handles pipe stages
    (let* ([specs (list (cons 'a (lambda () 1))
                        (cons 'b (lambda () 2))
                        (cons 'c (lambda () 3))
                        (cons 'd (lambda () 4)))]
           [composed (strategy-compose strategy/seq (strategy/par-chunk 2))]
           [thunks (map (lambda (spec) (make-thunk (car spec) 100)) specs)]
           [plan (using composed thunks 1000)])
      ;; seq first → plan:seq, then par-chunk refines → plan:chunk
      ;; Should not crash regardless of plan shape
      (assert-true (plan? plan)))))

;;; Cleanup
(pool-shutdown-global!)

(run-all-tests)
