;;; lattice/fp/test-state-properties.ss — QuickCheck properties for State monad
;;; 
;;; Tests the fundamental State monad laws:
;;; 1. Left identity:  (return a) >>= f  ==  f a
;;; 2. Right identity: m >>= return      ==  m
;;; 3. Associativity:  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
;;; 4. get/put laws:   put s >> get      ==  put s >> return s
;;;                    get >>= put       ==  return ()
;;;                    put s >> put s'   ==  put s'

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'fp/control/state)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -100 100))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group state-monad-laws

  ;; Left identity: return a >>= f == f a
  (define-property "state left identity law"
    gen-int-small
    (lambda (a)
      (let* ([f (lambda (x) (state-pure (+ x 10)))]
             [lhs (run-state (state-bind (state-pure a) f) 0)]
             [rhs (run-state (f a) 0)])
        (equal? lhs rhs)))
    'tests 100)

  ;; Right identity: m >>= return == m
  (define-property "state right identity law"
    gen-int-small
    (lambda (x)
      (let* ([m (state-pure x)]
             [lhs (run-state (state-bind m state-pure) 0)]
             [rhs (run-state m 0)])
        (equal? lhs rhs)))
    'tests 100)

  ;; Associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)
  (define-property "state associativity law"
    gen-int-small
    (lambda (x)
      (let* ([m (state-pure x)]
             [f (lambda (a) (state-pure (+ a 1)))]
             [g (lambda (b) (state-pure (* b 2)))]
             ;; LHS: (m >>= f) >>= g
             [lhs (run-state (state-bind (state-bind m f) g) 0)]
             ;; RHS: m >>= (\x -> f x >>= g)
             [rhs (run-state (state-bind m 
                                        (lambda (a) 
                                          (state-bind (f a) g))) 
                             0)])
        (equal? lhs rhs)))
    'tests 100)

  ;; get >>= put == return ()
  (define-property "get-put law"
    (gen-pure #t)
    (lambda (_)
      (let* ([result (run-state (state-bind state-get state-put) 42)]
             [expected (run-state (state-pure '()) 42)])
        (equal? result expected)))
    'tests 50)

  ;; put s >> get == put s >> return s
  (define-property "put-get law"
    gen-int-small
    (lambda (s)
      (let* ([lhs (run-state (state-then (state-put s) state-get) 0)]
             [rhs (run-state (state-then (state-put s) (state-pure s)) 0)])
        (equal? lhs rhs)))
    'tests 100)

  ;; put s >> put s' == put s'
  (define-property "put-put law"
    (gen-bind gen-int-small
      (lambda (s)
        (gen-map (lambda (s-prime) (cons s s-prime)) gen-int-small)))
    (lambda (ss-prime)
      (let* ([s (car ss-prime)]
             [s-prime (cdr ss-prime)]
             [lhs (run-state (state-then (state-put s) (state-put s-prime)) 0)]
             [rhs (run-state (state-put s-prime) 0)])
        (equal? lhs rhs)))
    'tests 100)

  ;; eval-state returns the result, not the state
  (define-property "eval-state returns result"
    gen-int-small
    (lambda (x)
      (let* ([result (eval-state (state-pure x) 0)])
        (equal? result x)))
    'tests 100)

  ;; exec-state returns the state, not the result
  (define-property "exec-state returns final state"
    gen-int-small
    (lambda (s)
      (let* ([result (exec-state (state-put s) 0)])
        (equal? result s)))
    'tests 100)

  ;; state-gets is equivalent to fmap
  (define-property "state-gets applies function to state"
    gen-int-small
    (lambda (s)
      (let* ([f (lambda (x) (* x 2))]
             [result (eval-state (state-gets f) s)])
        (equal? result (f s))))
    'tests 100)

  ;; state-modify composes state changes
  (define-property "state-modify composes changes"
    (gen-bind gen-int-small
      (lambda (s)
        (gen-map (lambda (delta) (cons s delta)) gen-int-small)))
    (lambda (sdelta)
      (let* ([s (car sdelta)]
             [delta (cdr sdelta)]
             [result (exec-state (state-modify (lambda (x) (+ x delta))) s)])
        (equal? result (+ s delta))))
    'tests 100)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
