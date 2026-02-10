(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/parallel/patterns.ss")

;;; ============================================================
;;; Test Suite: Parallel Algorithm Skeletons (Pure)
;;; ============================================================

(test-group "par-divide-and-conquer"

  (define-test "base case: single thunk"
    (let* ([ts (list (make-thunk 'a 100))]
           [p (par-divide-and-conquer
                (lambda (ts) (> (length ts) 1))
                (lambda (ts)
                  (let ([mid (quotient (length ts) 2)])
                    (values (list-head ts mid) (list-tail ts mid))))
                'combine
                (lambda (t) (plan:seq (list t)))
                ts 100)])
      ;; Single thunk, not divisible → base-case returns plan:seq
      (assert-true (plan:seq? p))
      (assert-equal 1 (length (plan-thunks p)))))

  (define-test "splits into balanced tree"
    (let* ([ts (map (lambda (i) (make-thunk i 100)) (iota 4))]
           [p (par-divide-and-conquer
                (lambda (ts) (> (length ts) 1))
                (lambda (ts)
                  (let ([mid (quotient (length ts) 2)])
                    (values (list-head ts mid) (list-tail ts mid))))
                'merge
                (lambda (t) (plan:seq (list t)))
                ts 100)])
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))
      (assert-equal 2 (length (plan-sub-plans p)))))

  (define-test "recursion depth bounded by fuel"
    (let* ([ts (map (lambda (i) (make-thunk i 10)) (iota 16))]
           [p (par-divide-and-conquer
                (lambda (ts) (> (length ts) 1))
                (lambda (ts)
                  (let ([mid (quotient (length ts) 2)])
                    (values (list-head ts mid) (list-tail ts mid))))
                'merge
                (lambda (t) (plan:seq (list t)))
                ts 1)])  ;; Only 1 fuel — can only split once
      ;; Top level splits, but children become plan:seq due to fuel exhaustion
      (assert-true (plan:tree? p))
      (let ([children (plan-sub-plans p)])
        ;; Children should be seq (fuel exhausted at depth 1)
        (assert-true (plan:seq? (car children)))
        (assert-true (plan:seq? (cadr children))))))

  (define-test "empty thunks produce empty seq"
    (let ([p (par-divide-and-conquer
               (lambda (ts) #t)
               (lambda (ts) (values '() '()))
               'merge
               (lambda (t) (plan:seq (list t)))
               '() 100)])
      (assert-true (plan:seq? p))
      (assert-equal 0 (length (plan-thunks p)))))

  (define-test "non-divisible multi-thunk list"
    (let* ([ts (list (make-thunk 'a 10) (make-thunk 'b 10))]
           [p (par-divide-and-conquer
                ;; Never divisible
                (lambda (ts) #f)
                (lambda (ts) (values '() '()))
                'merge
                (lambda (t) (plan:seq (list t)))
                ts 100)])
      ;; Not divisible, multi-thunk → plan:tree wrapping base-case plans
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))
      (assert-equal 2 (length (plan-sub-plans p)))
      ;; Each sub-plan is a base-case result
      (assert-true (plan:seq? (car (plan-sub-plans p)))))))

(test-group "par-reduce"

  (define-test "single thunk returns seq"
    (let* ([ts (list (make-thunk 'a 100))]
           [p (par-reduce 'add ts 100)])
      (assert-true (plan:seq? p))
      (assert-equal 1 (length (plan-thunks p)))))

  (define-test "two thunks produce balanced tree"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 200))]
           [p (par-reduce 'add ts 100)])
      (assert-true (plan:tree? p))
      (assert-equal 'add (plan-combiner p))
      (assert-equal 2 (length (plan-sub-plans p)))
      ;; Each sub-plan wraps a single thunk
      (assert-true (plan:seq? (car (plan-sub-plans p))))
      (assert-true (plan:seq? (cadr (plan-sub-plans p))))))

  (define-test "four thunks produce nested tree"
    (let* ([ts (map (lambda (i) (make-thunk i 100)) (iota 4))]
           [p (par-reduce 'multiply ts 100)])
      (assert-true (plan:tree? p))
      (assert-equal 'multiply (plan-combiner p))
      ;; Left and right are both trees (each has 2 elements)
      (assert-true (plan:tree? (car (plan-sub-plans p))))
      (assert-true (plan:tree? (cadr (plan-sub-plans p))))))

  (define-test "empty thunks return empty seq"
    (let ([p (par-reduce 'add '() 100)])
      (assert-true (plan:seq? p))))

  (define-test "preserves thunk identity through tree"
    (let* ([ts (list (make-thunk 'x 50) (make-thunk 'y 50))]
           [p (par-reduce 'add ts 100)]
           [left-thunks (plan-thunks (car (plan-sub-plans p)))]
           [right-thunks (plan-thunks (cadr (plan-sub-plans p)))])
      (assert-equal 'x (thunk-id (car left-thunks)))
      (assert-equal 'y (thunk-id (car right-thunks))))))

(test-group "par-scan"

  (define-test "single thunk returns seq"
    (let* ([ts (list (make-thunk 'a 100))]
           [p (par-scan 'add ts 100)])
      (assert-true (plan:seq? p))))

  (define-test "multiple thunks produce pipeline"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 100)
                     (make-thunk 'c 100) (make-thunk 'd 100))]
           [p (par-scan 'add ts 100)])
      (assert-true (plan:pipe? p))
      (assert-equal 2 (length (plan-stages p)))
      (assert-equal 'up-sweep (caar (plan-stages p)))
      (assert-equal 'down-sweep (caadr (plan-stages p)))))

  (define-test "scan preserves all thunks"
    (let* ([ts (map (lambda (i) (make-thunk i 50)) (iota 4))]
           [p (par-scan 'add ts 100)]
           [stages (plan-stages p)]
           [up-thunks (cdar stages)]
           [down-thunks (cdadr stages)])
      ;; Both stages should have all 4 thunks (collected from tree)
      (assert-equal 4 (length up-thunks))
      (assert-equal 4 (length down-thunks)))))

(test-group "par-pipeline"

  (define-test "empty stages produce seq"
    (let ([p (par-pipeline '() 1000)])
      (assert-true (plan:seq? p))))

  (define-test "single stage pipeline"
    (let* ([stages (list (cons 'compute (list (make-thunk 'a 100))))]
           [p (par-pipeline stages 1000)])
      (assert-true (plan:pipe? p))
      (assert-equal 1 (length (plan-stages p)))))

  (define-test "multi-stage pipeline preserves order"
    (let* ([stages (list (cons 'read (list (make-thunk 'r1 50)))
                         (cons 'process (list (make-thunk 'p1 200)
                                              (make-thunk 'p2 200)))
                         (cons 'write (list (make-thunk 'w1 50))))]
           [p (par-pipeline stages 1000)])
      (assert-true (plan:pipe? p))
      (assert-equal 3 (length (plan-stages p)))
      (assert-equal 'read (caar (plan-stages p)))
      (assert-equal 'process (car (cadr (plan-stages p))))
      (assert-equal 'write (car (caddr (plan-stages p))))))

  (define-test "stage thunks accessible"
    (let* ([t1 (make-thunk 'work 100)]
           [stages (list (cons 'stage-a (list t1)))]
           [p (par-pipeline stages 1000)]
           [stage-thunks (cdar (plan-stages p))])
      (assert-equal 1 (length stage-thunks))
      (assert-equal 'work (thunk-id (car stage-thunks))))))

(test-group "par-sort"

  (define-test "single thunk returns seq"
    (let* ([ts (list (make-thunk 'a 100))]
           [p (par-sort ts 100)])
      ;; Single element, not divisible → base-case
      (assert-true (plan:seq? p))))

  (define-test "two thunks produce merge tree"
    (let* ([ts (list (make-thunk 'a 100) (make-thunk 'b 100))]
           [p (par-sort ts 100)])
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))
      (assert-equal 2 (length (plan-sub-plans p)))))

  (define-test "four thunks produce nested merge tree"
    (let* ([ts (map (lambda (i) (make-thunk i 50)) (iota 4))]
           [p (par-sort ts 100)])
      (assert-true (plan:tree? p))
      (assert-equal 'merge (plan-combiner p))
      ;; Each half should also be a tree (2 elements each)
      (assert-true (plan:tree? (car (plan-sub-plans p))))
      (assert-true (plan:tree? (cadr (plan-sub-plans p))))))

  (define-test "empty produces seq"
    (let ([p (par-sort '() 100)])
      (assert-true (plan:seq? p)))))

(test-group "plan-thunks-deep"

  (define-test "flat plan returns thunks directly"
    (let* ([ts (list (make-thunk 'a 10) (make-thunk 'b 20))]
           [p (plan:par ts)])
      (assert-equal 2 (length (plan-thunks-deep p)))))

  (define-test "tree plan collects all leaf thunks"
    (let* ([ts (map (lambda (i) (make-thunk i 10)) (iota 4))]
           [p (par-reduce 'add ts 100)]
           [all-thunks (plan-thunks-deep p)])
      (assert-equal 4 (length all-thunks))))

  (define-test "nested tree collects recursively"
    (let* ([ts (map (lambda (i) (make-thunk i 10)) (iota 8))]
           [p (par-sort ts 100)]
           [all-thunks (plan-thunks-deep p)])
      (assert-equal 8 (length all-thunks)))))

(run-all-tests)
