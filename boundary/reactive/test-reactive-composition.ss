;;; test-reactive-composition.ss --- Test inter-derivation dependency tracking
;;;
;;; Tests for fold-zxpg fix: derivations depending on other derivations
;;; should be invalidated when their dependencies change.

(load "core/testing/test-framework.ss")
(load "boundary/reactive/reactive.ss")
(load "lattice/fp/optics/optics.ss")

;;; Reset state before tests
(set! *derivations* (make-hashtable symbol-hash eq?))
(set! *optic-to-derivations* (make-hashtable symbol-hash eq?))
(set! *derivation-to-derivations* (make-hashtable symbol-hash eq?))

;;; Test fixtures
(define test-source '((x . 10) (y . 20)))

(define lens-x
  (make-lens
    (lambda (s) (cdr (assq 'x s)))
    (lambda (v s) (cons (cons 'x v) (filter (lambda (p) (not (eq? 'x (car p)))) s)))))

(define lens-y
  (make-lens
    (lambda (s) (cdr (assq 'y s)))
    (lambda (v s) (cons (cons 'y v) (filter (lambda (p) (not (eq? 'y (car p)))) s)))))

;;; Register optics for tracking
(register-optic! 'lens-x lens-x)
(register-optic! 'lens-y lens-y)

(test-group "derivation-composition"

  (define-test "base derivation tracks optic dependency"
    (define-reactive 'base-x test-source
      (lambda (s) (reactive-view s lens-x)))
    (assert-equal '(lens-x) (reactive-dependencies 'base-x))
    (assert-equal 10 (reactive-value 'base-x)))

  (define-test "derived derivation tracks derivation dependency"
    (define-reactive 'doubled test-source
      (lambda (s)
        (* 2 (reactive-value 'base-x))))
    ;; Should have no optic deps (doesn't use reactive-view)
    (assert-equal '() (reactive-dependencies 'doubled))
    ;; Should have derivation dep on base-x
    (assert-equal '(base-x) (reactive-derivation-dependencies 'doubled))
    ;; Value should be correct
    (assert-equal 20 (reactive-value 'doubled)))

  (define-test "invalidating optic cascades through derivation chain"
    ;; Start fresh
    (assert-false (reactive-stale? 'base-x))
    (assert-false (reactive-stale? 'doubled))
    ;; Invalidate the optic that base-x depends on
    (invalidate-optic! 'lens-x)
    ;; Both should now be stale
    (assert-true (reactive-stale? 'base-x))
    (assert-true (reactive-stale? 'doubled)))

  (define-test "recomputation works through chain"
    ;; Update source
    (reactive-update-source! 'base-x '((x . 50) (y . 20)))
    (reactive-update-source! 'doubled '((x . 50) (y . 20)))
    ;; Force recompute
    (assert-equal 50 (reactive-value 'base-x))
    (assert-equal 100 (reactive-value 'doubled)))

  (define-test "three-level derivation chain"
    (define-reactive 'level-1 test-source
      (lambda (s) (reactive-view s lens-x)))
    (define-reactive 'level-2 test-source
      (lambda (s) (+ 5 (reactive-value 'level-1))))
    (define-reactive 'level-3 test-source
      (lambda (s) (* 2 (reactive-value 'level-2))))
    ;; Check dependencies
    (assert-equal '(level-1) (reactive-derivation-dependencies 'level-2))
    (assert-equal '(level-2) (reactive-derivation-dependencies 'level-3))
    ;; Check values: 10 -> 15 -> 30
    (assert-equal 10 (reactive-value 'level-1))
    (assert-equal 15 (reactive-value 'level-2))
    (assert-equal 30 (reactive-value 'level-3))
    ;; Invalidate and check cascade
    (invalidate-optic! 'lens-x)
    (assert-true (reactive-stale? 'level-1))
    (assert-true (reactive-stale? 'level-2))
    (assert-true (reactive-stale? 'level-3)))

  (define-test "derivation-dependency-graph shows relationships"
    (let ([graph (derivation-dependency-graph)])
      ;; base-x should have doubled as dependent
      (let ([base-x-dependents (cdr (assq 'base-x graph))])
        (assert-true (pair? (memq 'doubled base-x-dependents))))
      ;; level-1 should have level-2 as dependent
      (let ([level-1-dependents (cdr (assq 'level-1 graph))])
        (assert-true (pair? (memq 'level-2 level-1-dependents))))
      ;; level-2 should have level-3 as dependent
      (let ([level-2-dependents (cdr (assq 'level-2 graph))])
        (assert-true (pair? (memq 'level-3 level-2-dependents))))))

  (define-test "derivation-info includes both dependency types"
    (let ([info (derivation-info 'doubled)])
      (assert-true (pair? (assq 'optic-dependencies info)))
      (assert-true (pair? (assq 'derivation-dependencies info)))
      (assert-equal '(base-x) (cdr (assq 'derivation-dependencies info)))))

  (define-test "undefine-reactive cleans up derivation deps"
    (undefine-reactive 'level-3)
    ;; level-2's dependents should no longer include level-3
    (let ([graph (derivation-dependency-graph)])
      (let ([level-2-entry (assq 'level-2 graph)])
        (or (not level-2-entry)
            (not (memq 'level-3 (cdr level-2-entry)))))))

  (define-test "circular dependency detection"
    ;; Create two derivations that depend on each other
    ;; A depends on B, B depends on A
    (define-reactive 'circular-a test-source
      (lambda (s)
        ;; This will try to access circular-b, but circular-b doesn't exist yet
        ;; So we need to define them both first, then trigger the cycle
        (+ 1 (reactive-view s lens-x))))

    (define-reactive 'circular-b test-source
      (lambda (s)
        (+ 1 (reactive-value 'circular-a))))

    ;; Now redefine circular-a to depend on circular-b
    (define-reactive 'circular-a test-source
      (lambda (s)
        (+ 1 (reactive-value 'circular-b))))

    ;; Invalidate to make both stale
    (invalidate-optic! 'lens-x)

    ;; Accessing should raise an error about circular dependency
    ;; Use guard to catch the error
    (let ([caught-error #f])
      (guard (ex [else (set! caught-error #t)])
        (reactive-value 'circular-a))
      (assert-true caught-error))))

(run-all-tests)
