;;; lattice/fp/clp/test-store.ss — Tests for Constraint Store
;;;
;;; Tests constraint store operations, domain integration, and unification.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/clp/store.ss")

;;; Helper to convert truthy values to #t for assert-true
(define (truthy? x) (if x #t #f))

;;; ====
;;; Basic Store Tests
;;; ====

(test-group "store-basics"
            
            (define-test "make-cstore creates empty store"
              (let ([cs (make-cstore)])
                   (assert-true (cstore? cs))
                   (assert-equal empty-subst (cstore-subst cs))
                   (assert-equal '() (cstore-domains cs))
                   (assert-equal '() (cstore-constraints cs))
                   (assert-equal '() (cstore-pending cs))))
            
            (define-test "cstore-set-subst updates substitution"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [new-subst (extend-subst x 5 empty-subst)]
                     [cs2 (cstore-set-subst cs new-subst)])
                    (assert-equal new-subst (cstore-subst cs2))))
            )

;;; ====
;;; Domain Tests
;;; ====

(test-group "store-domains"
            
            (define-test "cstore-get-domain returns #f for unconstrained var"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)])
                    (assert-false (cstore-get-domain cs x))))
            
            (define-test "cstore-set-domain sets domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [dom (make-domain 1 10)]
                     [cs2 (cstore-set-domain cs x dom)])
                    (assert-true (cstore? cs2))
                    (assert-equal dom (cstore-get-domain cs2 x))))
            
            (define-test "cstore-set-domain fails on empty domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)])
                    (assert-false (cstore-set-domain cs x '()))))
            
            (define-test "cstore-set-domain adds var to pending"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))])
                    (assert-true (truthy? (member (lvar-id x) (cstore-pending cs2))))))
            
            (define-test "cstore-narrow-domain intersects domains"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))]
                     [cs3 (cstore-narrow-domain cs2 x (make-domain 5 15))])
                    (assert-equal (make-domain 5 10) (cstore-get-domain cs3 x))))
            
            (define-test "cstore-narrow-domain fails on empty intersection"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 5))])
                    (assert-false (cstore-narrow-domain cs2 x (make-domain 10 15)))))
            
            (define-test "cstore-remove-value removes single value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))]
                     [cs3 (cstore-remove-value cs2 x 5)])
                    (assert-false (domain-contains? (cstore-get-domain cs3 x) 5))
                    (assert-true (domain-contains? (cstore-get-domain cs3 x) 4))
                    (assert-true (domain-contains? (cstore-get-domain cs3 x) 6))))
            )

;;; ====
;;; Binding Tests
;;; ====

(test-group "store-binding"
            
            (define-test "cstore-bind-var binds variable to value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-bind-var cs x 5)])
                    (assert-true (cstore? cs2))
                    (assert-equal 5 (walk x (cstore-subst cs2)))))
            
            (define-test "cstore-bind-var respects domain constraint"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))]
                     [cs3 (cstore-bind-var cs2 x 5)])
                    (assert-true (cstore? cs3))
                    (assert-equal 5 (walk x (cstore-subst cs3)))))
            
            (define-test "cstore-bind-var fails if value not in domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))])
                    (assert-false (cstore-bind-var cs2 x 15))))
            
            (define-test "cstore-bind-var sets singleton domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))]
                     [cs3 (cstore-bind-var cs2 x 5)])
                    (assert-true (domain-singleton? (cstore-get-domain cs3 x)))))
            )

;;; ====
;;; Unification Tests
;;; ====

(test-group "store-unification"
            
            (define-test "cstore-unify unifies variable with value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-unify cs x 5)])
                    (assert-true (cstore? cs2))
                    (assert-equal 5 (cstore-walk cs2 x))))
            
            (define-test "cstore-unify unifies two variables"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 (cstore-unify cs x y)]
                     [cs3 (cstore-unify cs2 y 5)])
                    (assert-true (cstore? cs3))
                    (assert-equal 5 (cstore-walk cs3 x))
                    (assert-equal 5 (cstore-walk cs3 y))))
            
            (define-test "cstore-unify merges domains"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))]
                     [cs3 (cstore-set-domain cs2 y (make-domain 5 15))]
                     [cs4 (cstore-unify cs3 x y)])
                    (assert-true (cstore? cs4))
                    (assert-equal (make-domain 5 10) (cstore-get-domain cs4 x))
                    (assert-equal (make-domain 5 10) (cstore-get-domain cs4 y))))
            
            (define-test "cstore-unify fails on disjoint domains"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 5))]
                     [cs3 (cstore-set-domain cs2 y (make-domain 10 15))])
                    (assert-false (cstore-unify cs3 x y))))
            
            (define-test "cstore-unify handles pairs"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 (cstore-unify cs (cons x y) (cons 1 2))])
                    (assert-true (cstore? cs2))
                    (assert-equal 1 (cstore-walk cs2 x))
                    (assert-equal 2 (cstore-walk cs2 y))))
            
            (define-test "cstore-unify fails on structural mismatch"
              (let* ([cs (make-cstore)])
                    (assert-false (cstore-unify cs (cons 1 2) (cons 1 3)))))
            )

;;; ====
;;; Constraint Tests
;;; ====

(test-group "store-constraints"
            
            (define-test "make-constraint creates constraint"
              (let* ([x (make-lvar 'x)]
                     [c (make-constraint 'test (list x) (lambda (cs) cs))])
                    (assert-true (constraint? c))
                    (assert-equal 'test (constraint-type c))
                    (assert-equal (list x) (constraint-vars c))))
            
            (define-test "cstore-add-constraint adds constraint"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [c (make-constraint 'test (list x) (lambda (cs) cs))]
                     [cs2 (cstore-add-constraint cs c)])
                    (assert-equal 1 (length (cstore-constraints cs2)))))
            
            (define-test "cstore-constraints-for-var finds relevant constraints"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [c1 (make-constraint 'c1 (list x) (lambda (cs) cs))]
                     [c2 (make-constraint 'c2 (list y) (lambda (cs) cs))]
                     [c3 (make-constraint 'c3 (list x y) (lambda (cs) cs))]
                     [cs2 (cstore-add-constraint cs c1)]
                     [cs3 (cstore-add-constraint cs2 c2)]
                     [cs4 (cstore-add-constraint cs3 c3)])
                    (assert-equal 2 (length (cstore-constraints-for-var cs4 x)))
                    (assert-equal 2 (length (cstore-constraints-for-var cs4 y)))))
            )

;;; ====
;;; Pending Queue Tests
;;; ====

(test-group "store-pending"
            
            (define-test "cstore-add-pending adds to queue"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-add-pending cs x)])
                    (assert-true (truthy? (member (lvar-id x) (cstore-pending cs2))))))
            
            (define-test "cstore-add-pending is idempotent"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-add-pending cs x)]
                     [cs3 (cstore-add-pending cs2 x)])
                    (assert-equal 1 (length (cstore-pending cs3)))))
            
            (define-test "cstore-pop-pending removes from queue"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-add-pending cs x)]
                     [result (cstore-pop-pending cs2)])
                    (assert-true (truthy? result))
                    (assert-equal (lvar-id x) (car result))
                    (assert-equal '() (cstore-pending (cdr result)))))
            
            (define-test "cstore-pop-pending returns #f on empty"
              (let ([cs (make-cstore)])
                   (assert-false (cstore-pop-pending cs))))
            
            (define-test "cstore-clear-pending empties queue"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [y (make-lvar 'y)]
                     [cs2 (cstore-add-pending cs x)]
                     [cs3 (cstore-add-pending cs2 y)]
                     [cs4 (cstore-clear-pending cs3)])
                    (assert-equal '() (cstore-pending cs4))))
            )

;;; ====
;;; Ground/Singleton Detection Tests
;;; ====

(test-group "store-ground-detection"
            
            (define-test "cstore-ground? false for unbound var"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)])
                    (assert-false (cstore-ground? cs x))))
            
            (define-test "cstore-ground? true for bound var"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-bind-var cs x 5)])
                    (assert-true (cstore-ground? cs2 x))))
            
            (define-test "cstore-singleton? false for range domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (make-domain 1 10))])
                    (assert-false (cstore-singleton? cs2 x))))
            
            (define-test "cstore-singleton? true for singleton domain"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (domain-singleton 5))])
                    (assert-true (cstore-singleton? cs2 x))))
            
            (define-test "cstore-get-value returns bound value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-bind-var cs x 5)])
                    (assert-equal 5 (cstore-get-value cs2 x))))
            
            (define-test "cstore-get-value returns singleton domain value"
              (let* ([cs (make-cstore)]
                     [x (make-lvar 'x)]
                     [cs2 (cstore-set-domain cs x (domain-singleton 7))])
                    (assert-equal 7 (cstore-get-value cs2 x))))
            )

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
