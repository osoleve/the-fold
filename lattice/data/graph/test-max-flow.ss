;;; test-max-flow.ss — Tests for max-flow / min-cut module
;;;
;;; Run with: scheme --script lattice/data/graph/test-max-flow.ss

(load "core/lang/module.ss")
(load "lattice/data/graph/max-flow.ss")
(load "core/testing/test-framework.ss")

(printf "\n=== Max-Flow / Min-Cut Tests ===\n\n")

;;; ====
;;; Test Networks
;;; ====

;;; Network 1: Classic two-path network
;;;
;;;   s(0) --10--> 1 --10--> t(3)
;;;   s(0) --10--> 2 --10--> t(3)
;;;
;;; Max flow = 20 (two disjoint paths, each carrying 10)

(define (make-net1)
  (let ([net (make-flow-network 4)])
    (flow-network-add-edge! net 0 1 10)
    (flow-network-add-edge! net 1 3 10)
    (flow-network-add-edge! net 0 2 10)
    (flow-network-add-edge! net 2 3 10)
    net))

;;; Network 2: Bottleneck network
;;;
;;;   s(0) --100--> 1 --1--> 2 --100--> t(3)
;;;
;;; Max flow = 1 (bottleneck at 1->2)

(define (make-net2)
  (let ([net (make-flow-network 4)])
    (flow-network-add-edge! net 0 1 100)
    (flow-network-add-edge! net 1 2 1)
    (flow-network-add-edge! net 2 3 100)
    net))

;;; Network 3: Diamond with shared bottleneck
;;;
;;;   s(0) --10--> 1 --5--> 3(t)
;;;   s(0) --10--> 2 --5--> 3(t)
;;;   1 --15--> 2
;;;
;;; Max flow = 10 (limited by total capacity into sink)

(define (make-net3)
  (let ([net (make-flow-network 4)])
    (flow-network-add-edge! net 0 1 10)
    (flow-network-add-edge! net 0 2 10)
    (flow-network-add-edge! net 1 3 5)
    (flow-network-add-edge! net 2 3 5)
    (flow-network-add-edge! net 1 2 15)
    net))

;;; Network 4: CLRS-style 6-node network
;;;
;;;   s(0) --16--> 1
;;;   s(0) --13--> 2
;;;   1 --12--> 3
;;;   2 --14--> 4
;;;   1 --10--> 2 (cross edge)
;;;   3 --20--> t(5)
;;;   4 --4---> 3
;;;   2 --9---> 3 (alternate path)  <-- not in original, let's use a simpler variant
;;;   4 --7---> t(5)
;;;
;;; Simplified CLRS network:
;;;   s=0, a=1, b=2, c=3, d=4, t=5
;;;   s->a:16, s->b:13, a->b:4, a->c:12, b->a:10, b->d:14, c->b:9, c->t:20, d->c:7, d->t:4
;;;
;;; Max flow = 23

(define (make-net4)
  (let ([net (make-flow-network 6)])
    (flow-network-add-edge! net 0 1 16)   ; s->a
    (flow-network-add-edge! net 0 2 13)   ; s->b
    (flow-network-add-edge! net 1 2 4)    ; a->b
    (flow-network-add-edge! net 1 3 12)   ; a->c
    (flow-network-add-edge! net 2 1 10)   ; b->a
    (flow-network-add-edge! net 2 4 14)   ; b->d
    (flow-network-add-edge! net 3 2 9)    ; c->b
    (flow-network-add-edge! net 3 5 20)   ; c->t
    (flow-network-add-edge! net 4 3 7)    ; d->c
    (flow-network-add-edge! net 4 5 4)    ; d->t
    net))

;;; Network 5: Single edge
;;;
;;;   s(0) --42--> t(1)
;;;
;;; Max flow = 42

(define (make-net5)
  (let ([net (make-flow-network 2)])
    (flow-network-add-edge! net 0 1 42)
    net))

;;; Network 6: No path from source to sink
;;;
;;;   s(0) --10--> 1    2 --10--> t(3)
;;;
;;; Max flow = 0

(define (make-net6)
  (let ([net (make-flow-network 4)])
    (flow-network-add-edge! net 0 1 10)
    (flow-network-add-edge! net 2 3 10)
    net))

;;; Network 7: Parallel edges (multiple edges between same nodes)
;;;
;;;   s(0) ==={5,3}==> t(1)
;;;
;;; Max flow = 8

(define (make-net7)
  (let ([net (make-flow-network 2)])
    (flow-network-add-edge! net 0 1 5)
    (flow-network-add-edge! net 0 1 3)
    net))

;;; ====
;;; Construction Tests
;;; ====

(test-group "flow-network-construction"

  (define-test "make-flow-network creates valid network"
    (let ([net (make-flow-network 4)])
      (assert-true (flow-network? net))
      (assert-equal 4 (flow-network-size net))))

  (define-test "flow-network? rejects non-networks"
    (assert-false (flow-network? 42))
    (assert-false (flow-network? '()))
    (assert-false (flow-network? "hello")))

  (define-test "add-edge creates forward and reverse edges"
    (let ([net (make-flow-network 3)])
      (flow-network-add-edge! net 0 1 10)
      (let ([edges (flow-network-edges net)])
        ;; Should have exactly one forward edge
        (assert-equal 1 (length edges))
        (assert-equal '(0 1 10 0) (car edges)))))

  (define-test "edges reports all forward edges"
    (let ([net (make-net1)])
      (let ([edges (flow-network-edges net)])
        (assert-equal 4 (length edges)))))

  (define-test "flow-on-edge returns 0 before flow"
    (let ([net (make-net1)])
      (assert-equal 0 (flow-network-flow-on-edge net 0 1))
      (assert-equal 0 (flow-network-flow-on-edge net 1 3)))))

;;; ====
;;; Max-Flow Basic Tests
;;; ====

(test-group "max-flow-basic"

  (define-test "simple two-path network"
    (let ([net (make-net1)])
      (assert-equal 20 (max-flow net 0 3))))

  (define-test "bottleneck network"
    (let ([net (make-net2)])
      (assert-equal 1 (max-flow net 0 3))))

  (define-test "diamond with shared bottleneck"
    (let ([net (make-net3)])
      (assert-equal 10 (max-flow net 0 3))))

  (define-test "CLRS-style network"
    (let ([net (make-net4)])
      (assert-equal 23 (max-flow net 0 5))))

  (define-test "single edge"
    (let ([net (make-net5)])
      (assert-equal 42 (max-flow net 0 1))))

  (define-test "no path gives zero flow"
    (let ([net (make-net6)])
      (assert-equal 0 (max-flow net 0 3))))

  (define-test "parallel edges"
    (let ([net (make-net7)])
      (assert-equal 8 (max-flow net 0 1))))

  (define-test "source equals sink gives zero"
    (let ([net (make-flow-network 2)])
      (flow-network-add-edge! net 0 1 10)
      (assert-equal 0 (max-flow net 0 0)))))

;;; ====
;;; Flow Conservation Tests
;;; ====

(test-group "flow-conservation"

  (define-test "flow on edges consistent with max-flow value"
    (let ([net (make-net1)])
      (max-flow net 0 3)
      ;; Flow out of source should equal max-flow
      (let ([out-source (+ (flow-network-flow-on-edge net 0 1)
                           (flow-network-flow-on-edge net 0 2))])
        (assert-equal 20 out-source))))

  (define-test "flow conservation at intermediate nodes"
    (let ([net (make-net1)])
      (max-flow net 0 3)
      ;; Flow into node 1 should equal flow out of node 1
      (let ([in-1 (flow-network-flow-on-edge net 0 1)]
            [out-1 (flow-network-flow-on-edge net 1 3)])
        (assert-equal in-1 out-1))
      ;; Same for node 2
      (let ([in-2 (flow-network-flow-on-edge net 0 2)]
            [out-2 (flow-network-flow-on-edge net 2 3)])
        (assert-equal in-2 out-2))))

  (define-test "flow does not exceed capacity"
    (let ([net (make-net4)])
      (max-flow net 0 5)
      (let ([edges (flow-network-edges net)])
        ;; Every edge: 0 <= flow <= capacity
        (let check ([es edges])
          (unless (null? es)
            (let ([e (car es)])
              (assert-true (>= (cadddr e) 0))     ; flow >= 0
              (assert-true (<= (cadddr e) (caddr e))))  ; flow <= cap
            (check (cdr es))))))))

;;; ====
;;; Min-Cut Tests
;;; ====

(test-group "min-cut"

  (define-test "min-cut equals max-flow (two-path)"
    (let ([net (make-net1)])
      (let ([flow-val (max-flow net 0 3)])
        (let ([cut (min-cut net 0 3)])
          (assert-equal flow-val (car cut))
          (assert-equal flow-val (cadr cut))))))

  (define-test "min-cut equals max-flow (CLRS)"
    (let ([net (make-net4)])
      (let ([flow-val (max-flow net 0 5)])
        (let ([cut (min-cut net 0 5)])
          (assert-equal 23 (car cut))
          (assert-equal 23 (cadr cut))))))

  (define-test "min-cut edges cross partition"
    (let ([net (make-net2)])
      (max-flow net 0 3)
      (let ([cut (min-cut net 0 3)])
        ;; Cut edges should include the bottleneck 1->2
        (let ([cut-edges (caddr cut)])
          (assert-true (pair? cut-edges))
          ;; Bottleneck is 1->2 with capacity 1
          (assert-equal 1 (car cut))))))

  (define-test "no-path network: source isolated"
    (let ([net (make-net6)])
      (max-flow net 0 3)
      (let ([cut (min-cut net 0 3)])
        (assert-equal 0 (car cut))))))

;;; ====
;;; Min-Cut Partition Tests
;;; ====

(test-group "min-cut-partition"

  (define-test "partition covers all nodes"
    (let ([net (make-net4)])
      (max-flow net 0 5)
      (let* ([parts (min-cut-partition net 0 5)]
             [s-side (car parts)]
             [t-side (cadr parts)]
             [total (+ (length s-side) (length t-side))])
        (assert-equal 6 total))))

  (define-test "source in S-side, sink in T-side"
    (let ([net (make-net1)])
      (max-flow net 0 3)
      (let* ([parts (min-cut-partition net 0 3)]
             [s-side (car parts)]
             [t-side (cadr parts)])
        (assert-true (pair? (memv 0 s-side)))
        (assert-true (pair? (memv 3 t-side)))))))

;;; ====
;;; Bipartite Matching Tests
;;; ====

(test-group "bipartite-matching"

  (define-test "perfect matching exists"
    ;; Left: {0,1,2}, Right: {3,4,5}
    ;; Edges: 0-3, 1-4, 2-5 (perfect matching)
    (let ([result (bipartite-max-matching '(0 1 2) '(3 4 5)
                    '((0 3) (1 4) (2 5)))])
      (assert-equal 3 (car result))
      (assert-equal 3 (length (cadr result)))))

  (define-test "partial matching"
    ;; Left: {0,1,2}, Right: {3,4}
    ;; Edges: 0-3, 1-3, 2-4
    ;; Node 3 can only match one of {0,1}
    (let ([result (bipartite-max-matching '(0 1 2) '(3 4)
                    '((0 3) (1 3) (2 4)))])
      (assert-equal 2 (car result))))

  (define-test "complete bipartite K3,3"
    ;; Every left connects to every right
    (let ([result (bipartite-max-matching '(0 1 2) '(3 4 5)
                    '((0 3) (0 4) (0 5)
                      (1 3) (1 4) (1 5)
                      (2 3) (2 4) (2 5)))])
      (assert-equal 3 (car result))
      (assert-equal 3 (length (cadr result)))))

  (define-test "no edges means no matching"
    (let ([result (bipartite-max-matching '(0 1) '(2 3) '())])
      (assert-equal 0 (car result))
      (assert-true (null? (cadr result)))))

  (define-test "single pair"
    (let ([result (bipartite-max-matching '(0) '(1) '((0 1)))])
      (assert-equal 1 (car result))
      (assert-equal '((0 1)) (cadr result))))

  (define-test "augmenting path needed"
    ;; Left: {0,1}, Right: {2,3}
    ;; Edges: 0-2, 0-3, 1-2
    ;; Greedy might match 0-2, leaving 1 unmatched.
    ;; Augmenting path: 1-2-0-3 gives matching {1-2, 0-3}
    (let ([result (bipartite-max-matching '(0 1) '(2 3)
                    '((0 2) (0 3) (1 2)))])
      (assert-equal 2 (car result)))))

;;; ====
;;; Copy and Reset Tests
;;; ====

(test-group "copy-and-reset"

  (define-test "copy preserves structure"
    (let* ([net (make-net1)]
           [copy (copy-flow-network net)])
      (assert-equal (flow-network-size net) (flow-network-size copy))
      (assert-true (flow-network? copy))))

  (define-test "copy is independent"
    (let* ([net (make-net1)]
           [copy (copy-flow-network net)])
      ;; Mutate original
      (max-flow net 0 3)
      ;; Copy should still have zero flow
      (assert-equal 0 (flow-network-flow-on-edge copy 0 1))))

  (define-test "reset clears flow"
    (let ([net (make-net1)])
      (max-flow net 0 3)
      ;; Should have non-zero flow
      (assert-equal 10 (flow-network-flow-on-edge net 0 1))
      ;; Reset
      (reset-flow! net)
      (assert-equal 0 (flow-network-flow-on-edge net 0 1))
      ;; Can re-run max-flow
      (assert-equal 20 (max-flow net 0 3)))))

;;; ====
;;; Edge Cases
;;; ====

(test-group "edge-cases"

  (define-test "single node network"
    (let ([net (make-flow-network 1)])
      (assert-equal 0 (max-flow net 0 0))))

  (define-test "chain of unit edges"
    ;; 0 --1--> 1 --1--> 2 --1--> 3
    (let ([net (make-flow-network 4)])
      (flow-network-add-edge! net 0 1 1)
      (flow-network-add-edge! net 1 2 1)
      (flow-network-add-edge! net 2 3 1)
      (assert-equal 1 (max-flow net 0 3))))

  (define-test "wide fan-out fan-in"
    ;; s -> {1,2,3,4,5} -> t, each edge capacity 1
    ;; Max flow = 5
    (let ([net (make-flow-network 7)])
      (flow-network-add-edge! net 0 1 1)
      (flow-network-add-edge! net 0 2 1)
      (flow-network-add-edge! net 0 3 1)
      (flow-network-add-edge! net 0 4 1)
      (flow-network-add-edge! net 0 5 1)
      (flow-network-add-edge! net 1 6 1)
      (flow-network-add-edge! net 2 6 1)
      (flow-network-add-edge! net 3 6 1)
      (flow-network-add-edge! net 4 6 1)
      (flow-network-add-edge! net 5 6 1)
      (assert-equal 5 (max-flow net 0 6)))))

(run-all-tests)
