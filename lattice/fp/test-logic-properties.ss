;;; lattice/fp/test-logic-properties.ss — QuickCheck properties for logic programming
;;;
;;; Tests core logic programming operations:
;;; 1. Unification: (walk (unify a b s) a) == (walk (unify a b s) b)
;;; 2. Substitution extension preserves existing bindings
;;; 3. Walk follows chains of substitutions
;;; 4. Conjunction/disjunction semantics

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'logic)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -20 20))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group logic-variable-laws

  ;; make-lvar creates logic variables
  (define-property "make-lvar creates distinct variables"
    (gen-pure #t)
    (lambda (_)
      (let* ([v1 (make-lvar "x")]
             [v2 (make-lvar "x")])
        ;; Different calls create different variables (by id)
        (not (lvar=? v1 v2))))
    'tests 50)

  ;; lvar? recognizes logic variables
  (define-property "lvar? recognizes logic variables"
    (gen-pure #t)
    (lambda (_)
      (let* ([v (make-lvar "test")]
             [ground 42])
        (and (lvar? v)
             (not (lvar? ground)))))
    'tests 50)

  ;; lvar=? is reflexive
  (define-property "lvar=? is reflexive"
    (gen-pure #t)
    (lambda (_)
      (let ([v (make-lvar "x")])
        (lvar=? v v)))
    'tests 50)
)

(test-group logic-substitution-laws

  ;; Walk with empty substitution returns term unchanged for ground terms
  (define-property "walk with empty substitution returns ground term"
    gen-int-small
    (lambda (x)
      (equal? (walk x '()) x))
    'tests 100)

  ;; Walk follows direct binding
  ;; Note: extend-subst adds (var . val) pair, walk uses lookup-subst
  (define-property "walk follows direct binding"
    gen-int-small
    (lambda (x)
      (let* ([v (make-lvar "v")]
             [subst (extend-subst v x '())])
        (equal? (walk v subst) x)))
    'tests 100)

  ;; Extend substitution preserves existing bindings
  (define-property "extend-subst preserves existing bindings"
    gen-int-small
    (lambda (x)
      (let* ([v1 (make-lvar "v1")]
             [v2 (make-lvar "v2")]
             [y (+ x 1)]
             [subst1 (extend-subst v1 x '())]
             [subst2 (extend-subst v2 y subst1)])
        (and (equal? (walk v1 subst2) x)
             (equal? (walk v2 subst2) y))))
    'tests 100)

  ;; Unification binds variable to value (returns just)
  (define-property "unification binds var to ground term"
    gen-int-small
    (lambda (x)
      (let* ([v (make-lvar "v")]
             [result (unify v x '())])
        ;; Unify returns (just subst) on success
        (and (pair? result)
             (eq? (car result) 'just))))
    'tests 100)

  ;; Unification is symmetric for var/term
  (define-property "unification is symmetric for var/term"
    gen-int-small
    (lambda (x)
      (let* ([v (make-lvar "v")]
             [result1 (unify v x '())]
             [result2 (unify x v '())])
        ;; Both should succeed or both fail
        (equal? (pair? result1) (pair? result2))))
    'tests 100)

  ;; Self-unification succeeds for ground terms
  (define-property "self-unification succeeds"
    gen-int-small
    (lambda (x)
      (let ([result (unify x x '())])
        ;; Should return (just subst)
        (and (pair? result)
             (eq? (car result) 'just))))
    'tests 100)

  ;; Unification of distinct ground terms fails
  (define-property "unification of distinct ground terms fails"
    (gen-pure #t)
    (lambda (_)
      (let ([result (unify 1 2 '())])
        ;; unify returns #f on failure
        (eq? result #f)))
    'tests 50)
)

(test-group logic-goal-laws

  ;; succeed returns a stream with the substitution
  (define-property "succeed returns non-empty stream"
    (gen-pure #t)
    (lambda (_)
      (let ([result (succeed '())])
        ;; succeed returns (stream-cons subst thunk)
        (pair? result)))
    'tests 50)

  ;; fail returns empty stream (stream-nil)
  (define-property "fail returns empty stream"
    (gen-pure #t)
    (lambda (_)
      (let ([result (fail '())])
        ;; fail returns stream-nil which is a marker
        (equal? result '(stream-nil))))
    'tests 50)

  ;; == goal succeeds when unification succeeds
  (define-property "== goal succeeds when terms unify"
    gen-int-small
    (lambda (x)
      (let* ([v (make-lvar "v")]
             [goal (== v x)]
             [result-stream (goal '())])
        ;; Goal should return a non-empty stream
        (pair? result-stream)))
    'tests 100)

  ;; =/= succeeds when terms cannot unify
  (define-property "=/= succeeds when terms differ"
    (gen-pure #t)
    (lambda (_)
      (let* ([goal (=/= 1 2)]
             [result-stream (goal '())])
        ;; Should return non-empty stream
        (pair? result-stream)))
    'tests 50)

  ;; =/= fails when terms unify
  (define-property "=/= fails when terms unify"
    (gen-pure #t)
    (lambda (_)
      (let* ([goal (=/= 1 1)]
             [result-stream (goal '())])
        ;; Should return empty stream (stream-nil)
        (equal? result-stream '(stream-nil))))
    'tests 50)

  ;; disj tries alternatives (at least one succeeds)
  (define-property "disj tries alternatives"
    (gen-pure #t)
    (lambda (_)
      (let* ([v (make-lvar "v")]
             [goal (disj (== v 5) (== v 6))]
             [result-stream (goal '())])
        ;; Should return non-empty stream
        (pair? result-stream)))
    'tests 50)
)

(test-group logic-walk-star-laws

  ;; walk* fully dereferences
  (define-property "walk* fully dereferences"
    (gen-pure #t)
    (lambda (_)
      (let* ([v1 (make-lvar "v1")]
             [v2 (make-lvar "v2")]
             [subst (extend-subst v1 v2 
                     (extend-subst v2 42 '()))])
        (equal? (walk* v1 subst) 42)))
    'tests 50)

  ;; walk* handles improper lists (cons pairs)
  (define-property "walk* handles cons pairs"
    (gen-pure #t)
    (lambda (_)
      (let* ([v (make-lvar "v")]
             [subst (extend-subst v 3 '())]
             [term (cons 1 (cons 2 (cons v 4)))]  ; improper list: (1 2 v . 4)
             [result (walk* term subst)])
        (equal? result '(1 2 3 . 4))))
    'tests 50)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
