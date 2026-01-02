;;; fabric/stitches/test-state.ss — Tests for the State Monad
;;;
;;; Verifies the State monad implementation.

(load "core/prelude.ss")
(load "core/test-framework.ss")
(load "core/eval.ss")
(load "core/util/state.ss")

;;; ============================================================
;;; Build State Environment
;;; ============================================================

;;; Extend the base prelude with state combinators
(define (build-state-env fuel)
  (let ([base-env (build-prelude-env fuel)])
       (let loop ([defs state-prelude-defs] [env base-env] [remaining fuel])
            (if (null? defs)
                env
                (let* ([def (car defs)]
                       [name (car def)]
                       [expr (cdr def)]
                       [result (eval-expr expr env remaining)])
                      (if (eq? (car result) 'ok)
                          (loop (cdr defs)
                                (env-extend env name (cadr result))
                                (caddr result))
                          (begin
                           (display "Failed to build state env at: ")
                           (display name)
                           (newline)
                           env)))))))

(define state-env (build-state-env 1000))

;;; Helper to run tests with state env
(define (run-with-state expr fuel)
  (eval-expr expr state-env fuel))

;;; ============================================================
;;; Basic Tests
;;; ============================================================

(display "State Return and Run
")

;;; (state-return 42) applied to initial state 0 should give (42 0)
(test "return preserves state"
      #t
      (let ([result (run-with-state
                     '((run-state (state-return 42)) 0)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(42 0)))))

(test "return with different init state"
      #t
      (let ([result (run-with-state
                     '((run-state (state-return 'hello)) 999)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(hello 999)))))

;;; ============================================================
;;; Get and Put Tests
;;; ============================================================

(display "
Get and Put
")

(test "state-get returns current state"
      #t
      (let ([result (run-with-state
                     '((run-state state-get) 42)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(42 42)))))

(test "state-put updates state"
      #t
      (let ([result (run-with-state
                     '((run-state (state-put 100)) 0)
                     100)])
           (and (eq? (car result) 'ok)
                ;; Returns unit () and new state 100
                (equal? (cadr result) '(() 100)))))

;;; ============================================================
;;; Bind Tests
;;; ============================================================

(display "
State Bind (Sequencing)
")

(test "bind sequences computations"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-bind state-get)
                         (fn (x) (state-return (prim 'add x 1)))))
                       10)
                     200)])
           (and (eq? (car result) 'ok)
                ;; get returns 10, then we add 1, state unchanged
                (equal? (cadr result) '(11 10)))))

(test "bind threads state through"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-bind (state-put 50))
                         (fn (u) state-get)))
                       0)
                     200)])
           (and (eq? (car result) 'ok)
                ;; put 50, then get -> returns 50 with state 50
                (equal? (cadr result) '(50 50)))))

;;; ============================================================
;;; Modify Tests
;;; ============================================================

(display "
State Modify
")

(test "modify applies function to state"
      #t
      (let ([result (run-with-state
                     '((run-state
                        (state-modify (fn (x) (prim 'mul x 2))))
                       5)
                     100)])
           (and (eq? (car result) 'ok)
                ;; Returns unit, state doubled to 10
                (equal? (cadr result) '(() 10)))))

(test "modify then get"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-bind (state-modify (fn (x) (prim 'add x 10))))
                         (fn (u) state-get)))
                       7)
                     200)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(17 17)))))

;;; ============================================================
;;; Counter Tests
;;; ============================================================

(display "
Counter Combinators
")

(test "inc-counter returns old value"
      #t
      (let ([result (run-with-state
                     '((run-state inc-counter) 5)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(5 6)))))

(test "dec-counter returns old value"
      #t
      (let ([result (run-with-state
                     '((run-state dec-counter) 10)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(10 9)))))

(test "multiple increments"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-bind inc-counter)
                         (fn (a)
                             ((state-bind inc-counter)
                              (fn (b)
                                  ((state-bind inc-counter)
                                   (fn (c)
                                       (state-return (prim 'list a b c)))))))))
                       0)
                     500)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '((0 1 2) 3)))))

;;; ============================================================
;;; Eval and Exec Tests
;;; ============================================================

(display "
Eval and Exec State
")

(test "eval-state extracts value"
      #t
      (let ([result (run-with-state
                     '((eval-state (state-return 42)) 100)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) 42))))

(test "exec-state extracts final state"
      #t
      (let ([result (run-with-state
                     '((exec-state (state-put 999)) 0)
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) 999))))

;;; ============================================================
;;; Map and Ap Tests
;;; ============================================================

(display "
Functor and Applicative
")

(test "state-map applies function"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-map (fn (x) (prim 'mul x 10)))
                         (state-return 5)))
                       0)
                     200)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(50 0)))))

(test "state-ap applies wrapped function"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-ap (state-return (fn (x) (prim 'add x 1))))
                         (state-return 10)))
                       0)
                     200)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(11 0)))))

;;; ============================================================
;;; Position/Game State Tests
;;; ============================================================

(display "
Game State (Position)
")

(test "move-right updates x"
      #t
      (let ([result (run-with-state
                     '((run-state (move-right 5)) (prim 'list 0 0))
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(() (5 0))))))

(test "move-up updates y"
      #t
      (let ([result (run-with-state
                     '((run-state (move-up 3)) (prim 'list 10 20))
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(() (10 23))))))

(test "get-x reads x coordinate"
      #t
      (let ([result (run-with-state
                     '((run-state get-x) (prim 'list 42 99))
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(42 (42 99))))))

(test "get-y reads y coordinate"
      #t
      (let ([result (run-with-state
                     '((run-state get-y) (prim 'list 42 99))
                     100)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '(99 (42 99))))))

(test "move sequence"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-bind (move-right 10))
                         (fn (u)
                             ((state-bind (move-up 5))
                              (fn (v)
                                  ((state-bind get-x)
                                   (fn (x)
                                       ((state-bind get-y)
                                        (fn (y)
                                            (state-return (prim 'list x y)))))))))))
                       (prim 'list 0 0))
                     500)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '((10 5) (10 5))))))

;;; ============================================================
;;; Monad Laws Tests
;;; ============================================================

(display "
Monad Laws
")

;;; Left identity: (bind (return a) f) ≡ (f a)
(test "left identity"
      #t
      (let ([left (run-with-state
                   '((run-state
                      ((state-bind (state-return 5))
                       (fn (x) (state-return (prim 'mul x 2)))))
                     0)
                   200)]
            [right (run-with-state
                    '((run-state
                       ((fn (x) (state-return (prim 'mul x 2))) 5))
                      0)
                    100)])
           (and (eq? (car left) 'ok)
                (eq? (car right) 'ok)
                (equal? (cadr left) (cadr right)))))

;;; Right identity: (bind m return) ≡ m
(test "right identity"
      #t
      (let ([left (run-with-state
                   '((run-state
                      ((state-bind (state-return 42))
                       state-return))
                     0)
                   200)]
            [right (run-with-state
                    '((run-state (state-return 42)) 0)
                    100)])
           (and (eq? (car left) 'ok)
                (eq? (car right) 'ok)
                (equal? (cadr left) (cadr right)))))

;;; Associativity: (bind (bind m f) g) ≡ (bind m (λx. bind (f x) g))
(test "associativity"
      #t
      (let ([left (run-with-state
                   '((run-state
                      ((state-bind
                        ((state-bind (state-return 2))
                         (fn (x) (state-return (prim 'mul x 3)))))
                       (fn (y) (state-return (prim 'add y 1)))))
                     0)
                   300)]
            [right (run-with-state
                    '((run-state
                       ((state-bind (state-return 2))
                        (fn (x)
                            ((state-bind (state-return (prim 'mul x 3)))
                             (fn (y) (state-return (prim 'add y 1)))))))
                      0)
                    300)])
           (and (eq? (car left) 'ok)
                (eq? (car right) 'ok)
                (equal? (cadr left) (cadr right)))))

;;; ============================================================
;;; Replicate Test
;;; ============================================================

(display "
State Replicate
")

(test "replicate counter"
      #t
      (let ([result (run-with-state
                     '((run-state
                        ((state-replicate 4) inc-counter))
                       0)
                     500)])
           (and (eq? (car result) 'ok)
                (equal? (cadr result) '((0 1 2 3) 4)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "✓ All state monad tests complete.
")
