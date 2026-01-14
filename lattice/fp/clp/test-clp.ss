;;; lattice/fp/clp/test-clp.ss — Integration Tests for CLP(FD)
;;;
;;; Tests the complete CLP system with classic constraint problems.

(load "core/testing/test-framework.ss")
(load "lattice/fp/clp/clp.ss")

;;; Helper
(define (truthy? x) (if x #t #f))

;;; ====
;;; Basic Constraint Tests
;;; ====

(test-group "clp-basic"
            
            (define-test "in-range constrains domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 10) cs)])
                    (assert-true (cstore? cs2))
                    (assert-equal 10 (domain-size (cstore-get-domain cs2 x)))))
            
            (define-test "=fd with constant narrows to singleton"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 10) cs)]
                     [cs3 ((=fd x 5) cs2)])
                    (assert-true (cstore? cs3))
                    (assert-true (domain-singleton? (cstore-get-domain cs3 x)))
                    (assert-equal 5 (domain-min (cstore-get-domain cs3 x)))))
            
            (define-test "=fd fails when value not in domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 10) cs)])
                    (assert-false ((=fd x 15) cs2))))
            
            (define-test "<fd narrows domains"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 ((in-range x 1 10) cs)]
                     [cs3 ((in-range y 1 10) cs2)]
                     [cs4 ((<fd x y) cs3)])
                    (assert-true (cstore? cs4))
                    ;; x can be 1..9, y can be 2..10
                    (assert-equal 9 (domain-max (cstore-get-domain cs4 x)))
                    (assert-equal 2 (domain-min (cstore-get-domain cs4 y)))))
            
            (define-test "=/=fd removes value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 10) cs)]
                     [cs3 ((=/=fd x 5) cs2)])
                    (assert-true (cstore? cs3))
                    (assert-false (domain-contains? (cstore-get-domain cs3 x) 5))))
            )

;;; ====
;;; Arithmetic Constraint Tests
;;; ====

(test-group "clp-arithmetic"
            
            (define-test "+fd with known values"
              (let* ([cs (make-cstore)]
                     [z (make-lvar 'z)]
                     [cs2 ((+fd 3 4 z) cs)])
                    (assert-true (cstore? cs2))
                    (assert-equal 7 (cstore-get-value cs2 z))))
            
            (define-test "+fd bounds propagation"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [z (make-lvar 'z)]
                     [cs2 ((in-range x 1 5) cs)]
                     [cs3 ((in-range y 1 5) cs2)]
                     [cs4 ((in-range z 1 20) cs3)]
                     [cs5 ((+fd x y z) cs4)])
                    (assert-true (cstore? cs5))
                    ;; z = x + y, so z in [2, 10]
                    (assert-equal 2 (domain-min (cstore-get-domain cs5 z)))
                    (assert-equal 10 (domain-max (cstore-get-domain cs5 z)))))
            
            (define-test "*fd with known values"
              (let* ([cs (make-cstore)]
                     [z (make-lvar 'z)]
                     [cs2 ((*fd 3 4 z) cs)])
                    (assert-true (cstore? cs2))
                    (assert-equal 12 (cstore-get-value cs2 z))))
            )

;;; ====
;;; All-Different Tests
;;; ====

(test-group "clp-all-different"
            
            (define-test "all-different removes singleton values"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [z (make-lvar 'z)]
                     [cs2 ((in-range x 1 3) cs)]
                     [cs3 ((in-range y 1 3) cs2)]
                     [cs4 ((in-range z 1 3) cs3)]
                     [cs5 ((=fd x 2) cs4)]
                     [cs6 ((all-different (list x y z)) cs5)])
                    (assert-true (cstore? cs6))
                    ;; y and z should not contain 2
                    (assert-false (domain-contains? (cstore-get-domain cs6 y) 2))
                    (assert-false (domain-contains? (cstore-get-domain cs6 z) 2))))
            
            (define-test "all-different fails on duplicate singletons"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 ((=fd x 5) cs)]
                     [cs3 ((=fd y 5) cs2)])
                    (assert-false ((all-different (list x y)) cs3))))
            )

;;; ====
;;; Labeling Tests
;;; ====

(test-group "clp-labeling"
            
            (define-test "label finds solutions"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 3) cs)]
                     [solutions (label cs2 (list x))])
                    (assert-equal 3 (stream-count solutions 10))))
            
            (define-test "first-fail selects smallest domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 ((in-range x 1 10) cs)]
                     [cs3 ((in-range y 1 3) cs2)]
                     [selected (first-fail cs3 (list x y))])
                    ;; y has smaller domain, should be selected
                    (assert-equal (lvar-id y) (lvar-id selected))))
            
            (define-test "label-first returns first solution"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 5) cs)]
                     [solution (label-first cs2 (list x))])
                    (assert-true (cstore? solution))
                    (assert-equal 1 (cstore-get-value solution x))))
            )

;;; ====
;;; Goal Combinator Tests
;;; ====

(test-group "clp-goals"
            
            (define-test "clp-conj combines goals"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [goal (clp-conj
                            (goal-in-range x 1 10)
                            (goal-<fd x 5))]
                     [solutions (goal cs)])
                    (assert-true (truthy? (not (stream-nil? solutions))))
                    (let ([cs1 (stream-head solutions)])
                         (assert-equal 4 (domain-max (cstore-get-domain cs1 x))))))
            
            (define-test "clp-conj* combines multiple goals"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [goal (clp-conj*
                            (list (goal-in-range x 1 5)
                                  (goal-in-range y 1 5)
                                  (goal-<fd x y)
                                  (goal-label (list x y))))]
                     [solutions (goal cs)]
                     [count (stream-count solutions 100)])
                    ;; x < y with both in [1,5]: (1,2), (1,3), ..., (4,5) = 10 solutions
                    (assert-equal 10 count)))
            )

;;; ====
;;; N-Queens Tests
;;; ====

(test-group "clp-n-queens"
            
            (define-test "4-queens has 2 solutions"
              (let* ([cs (make-cstore)]
                     [goal (n-queens 4)]
                     [solutions (goal cs)]
                     [count (stream-count solutions 100)])
                    (assert-equal 2 count)))
            
            ;; 8-queens is slower, so we just check it finds at least one solution
            (define-test "8-queens has solutions"
              (let* ([cs (make-cstore)]
                     [goal (n-queens 8)]
                     [solutions (goal cs)])
                    (assert-false (stream-nil? solutions))))
            )

;;; ====
;;; SEND + MORE = MONEY Tests
;;; ====

(test-group "clp-cryptarithmetic"
            
            (define-test "SEND+MORE=MONEY has solution"
              (let* ([cs (make-cstore)]
                     [goal (send-more-money)]
                     [solutions (goal cs)])
                    (assert-false (stream-nil? solutions))
                    ;; Verify the solution
                    (let ([sol (stream-head solutions)])
                         ;; Just check we got a valid solution
                         (assert-true (cstore? sol)))))
            )

;;; ====
;;; Sum Constraint Tests
;;; ====

(test-group "clp-sum"
            
            (define-test "sum-fd constrains result"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [z (make-lvar 'z)]
                     [cs2 ((in-range x 1 5) cs)]
                     [cs3 ((in-range y 1 5) cs2)]
                     [cs4 ((sum-fd (list x y) z) cs3)])
                    (assert-true (cstore? cs4))
                    ;; z = x + y, so z in [2, 10]
                    (assert-equal 2 (domain-min (cstore-get-domain cs4 z)))
                    (assert-equal 10 (domain-max (cstore-get-domain cs4 z)))))
            
            (define-test "sum-fd with fixed result"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 ((in-range x 1 5) cs)]
                     [cs3 ((in-range y 1 5) cs2)]
                     [cs4 ((sum-fd (list x y) 6) cs3)])
                    (assert-true (cstore? cs4))
                    ;; x + y = 6, so x in [1,5], y in [1,5]
                    ;; x can be 1..5, y can be 1..5
                    ;; But x + y = 6 means valid pairs: (1,5), (2,4), (3,3), (4,2), (5,1)
                    (assert-true (domain-contains? (cstore-get-domain cs4 x) 1))
                    (assert-true (domain-contains? (cstore-get-domain cs4 x) 5))))
            )

;;; ====
;;; Edge Cases
;;; ====

(test-group "clp-edge-cases"
            
            (define-test "empty domain fails"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 5) cs)]
                     [cs3 ((=fd x 10) cs2)])
                    (assert-false cs3)))
            
            (define-test "unsatisfiable constraints fail"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 5) cs)]
                     [cs3 ((<fd x 1) cs2)])
                    (assert-false cs3)))
            
            (define-test "single variable labels correctly"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 ((in-range x 1 1) cs)]
                     [solutions (label cs2 (list x))])
                    (assert-equal 1 (stream-count solutions 10))))
            )

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
