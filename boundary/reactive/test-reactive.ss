;;; boundary/reactive/test-reactive.ss — Tests for Reactive Derivations
;;;
;;; Tests for:
;;;   - Access tracking during computation
;;;   - Derivation definition and value retrieval
;;;   - Automatic invalidation on optic writes
;;;   - Dependency graph management
;;;   - Batch operations
;;;
;;; Run: scheme --script boundary/reactive/test-reactive.ss

(load "core/testing/test-framework.ss")
(load "boundary/reactive/reactive.ss")

;;; ============================================================
;;; Test Setup
;;; ============================================================

;; Ensure provenance tracking is enabled
(provenance-enable!)

;; Clear any existing derivations
(for-each undefine-reactive (list-derivations))

;;; ============================================================
;;; Part 1: Access Tracking Tests
;;; ============================================================

(test-group "access-tracking"
  (define-test "with-access-tracking captures optic names"
    (call-with-values
      (lambda ()
        (with-access-tracking
         (lambda ()
           (reactive-view '(1 . 2) lens-fst)
           (reactive-view '(a . b) lens-snd)
           'done)))
      (lambda (result deps)
        (assert-equal 'done result)
        (assert-true (pair? (memq 'lens-fst deps)))
        (assert-true (pair? (memq 'lens-snd deps))))))

  (define-test "no duplicates in access log"
    (call-with-values
      (lambda ()
        (with-access-tracking
         (lambda ()
           (reactive-view '(1 . 2) lens-fst)
           (reactive-view '(3 . 4) lens-fst)  ; Same optic
           (reactive-view '(5 . 6) lens-fst)  ; Same optic again
           'done)))
      (lambda (result deps)
        (assert-equal 1 (length (filter (lambda (x) (eq? x 'lens-fst)) deps))))))

  (define-test "tracking is scoped"
    ;; Outside of tracking, log should be empty
    (reactive-view '(1 . 2) lens-fst)
    (assert-equal '() *access-log*)))

;;; ============================================================
;;; Part 2: Derivation Definition Tests
;;; ============================================================

(test-group "derivation-definition"
  (define-test "define-reactive creates derivation"
    (define-reactive 'test-sum
      '(10 . 20)
      (lambda (pair)
        (+ (reactive-view pair lens-fst)
           (reactive-view pair lens-snd))))
    (assert-true (pair? (memq 'test-sum (list-derivations)))))

  (define-test "reactive-value returns computed value"
    (define-reactive 'pair-product
      '(3 . 4)
      (lambda (pair)
        (* (reactive-view pair lens-fst)
           (reactive-view pair lens-snd))))
    (assert-equal 12 (reactive-value 'pair-product)))

  (define-test "derivation captures dependencies"
    (define-reactive 'fst-only
      '(100 . 200)
      (lambda (pair)
        (reactive-view pair lens-fst)))
    (let ([deps (reactive-dependencies 'fst-only)])
      (assert-true (pair? (memq 'lens-fst deps)))
      ;; Should NOT depend on lens-snd since we only viewed fst
      (assert-false (pair? (memq 'lens-snd deps))))))

  (define-test "undefine-reactive removes derivation"
    (define-reactive 'temp-derivation
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-true (pair? (memq 'temp-derivation (list-derivations))))
    (undefine-reactive 'temp-derivation)
    (assert-false (pair? (memq 'temp-derivation (list-derivations)))))

;;; ============================================================
;;; Part 3: Caching Tests
;;; ============================================================

(test-group "caching"
  (define-test "value is cached (not recomputed)"
    (let ([call-count 0])
      (define-reactive 'cached-test
        '(5 . 5)
        (lambda (pair)
          (set! call-count (+ call-count 1))
          (+ (reactive-view pair lens-fst)
             (reactive-view pair lens-snd))))
      ;; First access computes
      (reactive-value 'cached-test)
      (let ([count-after-first call-count])
        ;; Second access should use cache
        (reactive-value 'cached-test)
        (assert-equal count-after-first call-count))))

  (define-test "reactive-stale? is false initially"
    (define-reactive 'fresh-derivation
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-false (reactive-stale? 'fresh-derivation)))

  (define-test "reactive-refresh! forces recomputation"
    (let ([call-count 0])
      (define-reactive 'refreshable
        '(10 . 20)
        (lambda (pair)
          (set! call-count (+ call-count 1))
          (reactive-view pair lens-fst)))
      (reactive-value 'refreshable)
      (let ([count-before call-count])
        (reactive-refresh! 'refreshable)
        (assert-equal (+ count-before 1) call-count)))))

;;; ============================================================
;;; Part 4: Invalidation Tests
;;; ============================================================

(test-group "invalidation"
  (define-test "invalidate-optic! marks derivation stale"
    (define-reactive 'invalidation-test
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-false (reactive-stale? 'invalidation-test))
    (invalidate-optic! 'lens-fst)
    (assert-true (reactive-stale? 'invalidation-test)))

  (define-test "stale derivation recomputes on access"
    (define-reactive 'stale-recompute
      '(100 . 200)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-equal 100 (reactive-value 'stale-recompute))
    (assert-false (reactive-stale? 'stale-recompute))
    ;; Mark stale
    (invalidate-optic! 'lens-fst)
    (assert-true (reactive-stale? 'stale-recompute))
    ;; Access should recompute and clear stale flag
    (reactive-value 'stale-recompute)
    (assert-false (reactive-stale? 'stale-recompute)))

  (define-test "unrelated optic does not invalidate"
    (define-reactive 'fst-dep
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-false (reactive-stale? 'fst-dep))
    ;; Invalidate lens-snd should not affect lens-fst derivation
    (invalidate-optic! 'lens-snd)
    (assert-false (reactive-stale? 'fst-dep))))

;;; ============================================================
;;; Part 5: Reactive Write Tests
;;; ============================================================

(test-group "reactive-writes"
  (define-test "reactive-set! invalidates dependent derivations"
    (define-reactive 'write-test
      '(50 . 60)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-false (reactive-stale? 'write-test))
    ;; This should invalidate because write-test depends on lens-fst
    (reactive-set! lens-fst 999 '(1 . 2))
    (assert-true (reactive-stale? 'write-test)))

  (define-test "reactive-over! invalidates dependent derivations"
    (define-reactive 'over-test
      '(10 . 20)
      (lambda (p) (reactive-view p lens-snd)))
    (assert-false (reactive-stale? 'over-test))
    (reactive-over! lens-snd add1 '(1 . 2))
    (assert-true (reactive-stale? 'over-test))))

;;; ============================================================
;;; Part 6: Multiple Derivation Tests
;;; ============================================================

(test-group "multiple-derivations"
  (define-test "multiple derivations can share dependencies"
    (define-reactive 'multi-a
      '(1 . 2)
      (lambda (p) (* 2 (reactive-view p lens-fst))))
    (define-reactive 'multi-b
      '(3 . 4)
      (lambda (p) (* 3 (reactive-view p lens-fst))))
    ;; Both depend on lens-fst
    (invalidate-optic! 'lens-fst)
    (assert-true (reactive-stale? 'multi-a))
    (assert-true (reactive-stale? 'multi-b)))

  (define-test "derivations with different dependencies"
    (define-reactive 'dep-fst
      '(10 . 20)
      (lambda (p) (reactive-view p lens-fst)))
    (define-reactive 'dep-snd
      '(10 . 20)
      (lambda (p) (reactive-view p lens-snd)))
    ;; Invalidate only lens-fst
    (invalidate-optic! 'lens-fst)
    (assert-true (reactive-stale? 'dep-fst))
    (assert-false (reactive-stale? 'dep-snd))))

;;; ============================================================
;;; Part 7: Source Update Tests
;;; ============================================================

(test-group "source-updates"
  (define-test "reactive-update-source! changes source and marks stale"
    (define-reactive 'source-test
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (assert-equal 1 (reactive-value 'source-test))
    ;; Update source
    (reactive-update-source! 'source-test '(999 . 888))
    (assert-true (reactive-stale? 'source-test))
    ;; Should now compute with new source
    (assert-equal 999 (reactive-value 'source-test))))

;;; ============================================================
;;; Part 8: Batch Operation Tests
;;; ============================================================

(test-group "batch-operations"
  (define-test "with-batch defers invalidation"
    (define-reactive 'batch-test
      '(1 . 2)
      (lambda (p)
        (+ (reactive-view p lens-fst)
           (reactive-view p lens-snd))))
    (assert-equal 3 (reactive-value 'batch-test))
    (let ([stale-during-batch #f])
      (with-batch
       (lambda ()
         ;; These should not immediately invalidate
         (reactive-set! lens-fst 10 '(0 . 0))
         (set! stale-during-batch (reactive-stale? 'batch-test))
         (reactive-set! lens-snd 20 '(0 . 0))))
      ;; During batch, should NOT be stale (deferral working)
      (assert-false stale-during-batch)
      ;; After batch completes, should be stale
      (assert-true (reactive-stale? 'batch-test))))

  (define-test "with-batch collects multiple invalidations"
    (define-reactive 'multi-batch-a
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (define-reactive 'multi-batch-b
      '(3 . 4)
      (lambda (p) (reactive-view p lens-snd)))
    ;; Get initial values
    (reactive-value 'multi-batch-a)
    (reactive-value 'multi-batch-b)
    ;; Both depend on different optics
    (with-batch
     (lambda ()
       ;; Invalidate both optics in one batch
       (reactive-set! lens-fst 100 '(0 . 0))
       (reactive-set! lens-snd 200 '(0 . 0))))
    ;; Both should be stale after batch
    (assert-true (reactive-stale? 'multi-batch-a))
    (assert-true (reactive-stale? 'multi-batch-b))))

;;; ============================================================
;;; Part 9: Introspection Tests
;;; ============================================================

(test-group "introspection"
  (define-test "list-derivations returns all names"
    (define-reactive 'intro-a '(1 . 2) (lambda (p) 1))
    (define-reactive 'intro-b '(3 . 4) (lambda (p) 2))
    (let ([all (list-derivations)])
      (assert-true (pair? (memq 'intro-a all)))
      (assert-true (pair? (memq 'intro-b all)))))

  (define-test "derivation-info returns alist"
    (define-reactive 'info-test
      '(5 . 6)
      (lambda (p) (reactive-view p lens-fst)))
    (let ([info (derivation-info 'info-test)])
      (assert-true (list? info))
      (assert-equal 'info-test (cdr (assq 'name info)))
      (assert-true (list? (cdr (assq 'dependencies info))))
      (assert-true (boolean? (cdr (assq 'stale? info))))))

  (define-test "dependency-graph shows optic -> derivation mapping"
    (define-reactive 'graph-test
      '(1 . 2)
      (lambda (p) (reactive-view p lens-fst)))
    (let ([graph (dependency-graph)])
      (assert-true (list? graph))
      ;; Should have lens-fst in the graph
      (let ([fst-entry (assq 'lens-fst graph)])
        (assert-true (pair? fst-entry))
        (assert-true (pair? (memq 'graph-test (cdr fst-entry))))))))

;;; ============================================================
;;; Part 10: Dynamic Dependency Tests
;;; ============================================================

(test-group "dynamic-dependencies"
  (define-test "dependencies update on recomputation"
    ;; A derivation that conditionally accesses different optics
    (let ([use-fst? #t])
      (define-reactive 'dynamic-dep
        '(10 . 20)
        (lambda (p)
          (if use-fst?
              (reactive-view p lens-fst)
              (reactive-view p lens-snd))))
      ;; Initially depends on lens-fst
      (assert-true (pair? (memq 'lens-fst (reactive-dependencies 'dynamic-dep))))
      ;; Change the branch and force recompute
      (set! use-fst? #f)
      (reactive-refresh! 'dynamic-dep)
      ;; Now should depend on lens-snd instead
      (assert-true (pair? (memq 'lens-snd (reactive-dependencies 'dynamic-dep)))))))

;;; ============================================================
;;; Part 11: Unregistered Optic Handling Tests (fold-zxpe fix)
;;; ============================================================

(test-group "unregistered-optic-handling"
  ;; Create an anonymous optic that is NOT registered
  ;; Use make-lens from the optics tower for proper structure
  (define anon-fst (make-lens car (lambda (b s) (cons b (cdr s)))))

  (define-test "anonymous optic is not registered"
    ;; Verify our test optic is truly unregistered
    (assert-false (lookup-optic-name anon-fst)))

  (define-test "warning emitted for unregistered optic (default mode)"
    ;; Reset warnings and configuration
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Use in a tracked context - should warn but not error
    (call-with-values
      (lambda ()
        (with-access-tracking
         (lambda ()
           ;; This should emit a warning (to stderr)
           ;; but still work and return the value
           (reactive-view '(1 . 2) anon-fst))))
      (lambda (result deps)
        ;; Result should still work
        (assert-equal 1 result)
        ;; But deps should be empty (no registration)
        (assert-equal '() deps))))

  (define-test "warning only emitted once per optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; First use - should set warning flag
    (with-access-tracking
     (lambda () (reactive-view '(1 . 2) anon-fst)))
    ;; Check that optic is now in warned cache
    (assert-true (hashtable-ref *warned-optics* anon-fst #f)))

  (define-test "reset-optic-warnings! clears cache"
    (reset-optic-warnings!)
    ;; After reset, optic should not be in cache
    (assert-false (hashtable-ref *warned-optics* anon-fst #f)))

  (define-test "strict mode raises error for unregistered optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #t)
    ;; Should raise an error - assert-error takes a thunk
    (assert-error
     (lambda ()
       (with-access-tracking
        (lambda ()
          (reactive-view '(1 . 2) anon-fst)))))
    ;; Reset to safe defaults
    (set! *strict-optic-registration?* #f))

  (define-test "no warning when not tracking accesses"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Outside of tracking context, no warning should be logged
    (reactive-view '(1 . 2) anon-fst)
    ;; The optic should NOT be in the warned cache
    ;; (warning is only for tracked context where deps matter)
    (assert-false (hashtable-ref *warned-optics* anon-fst #f)))

  (define-test "silent mode when warnings disabled"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #f)
    (set! *strict-optic-registration?* #f)
    ;; Should work silently without warning
    (call-with-values
      (lambda ()
        (with-access-tracking
         (lambda () (reactive-view '(42 . 0) anon-fst))))
      (lambda (result deps)
        (assert-equal 42 result)
        ;; Still no deps tracked
        (assert-equal '() deps)
        ;; And no warning recorded
        (assert-false (hashtable-ref *warned-optics* anon-fst #f)))))

  (define-test "reactive-preview warns for unregistered optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Define an anonymous prism for testing preview
    (let ([anon-prism (make-prism
                        (lambda (x) (if (pair? x) (just (car x)) nothing))
                        (lambda (a) (list a)))])
      (with-access-tracking
       (lambda () (reactive-preview '(1 2 3) anon-prism)))
      (assert-true (hashtable-ref *warned-optics* anon-prism #f))))

  (define-test "reactive-to-list warns for unregistered optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Use traversal-each which is registered, but compose with anonymous lens
    (let ([anon-traversal (make-traversal
                            (lambda (f xs) (map f xs))
                            (lambda (xs) xs))])
      (with-access-tracking
       (lambda () (reactive-to-list '(1 2 3) anon-traversal)))
      (assert-true (hashtable-ref *warned-optics* anon-traversal #f))))

  (define-test "reactive-set! warns for unregistered optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; reactive-set! should also warn (always, not just during tracking)
    (reactive-set! anon-fst 99 '(1 . 2))
    ;; Should be in warned cache now
    (assert-true (hashtable-ref *warned-optics* anon-fst #f)))

  (define-test "reactive-over! warns for unregistered optic"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Clear from previous test
    (reset-optic-warnings!)
    (reactive-over! anon-fst add1 '(1 . 2))
    (assert-true (hashtable-ref *warned-optics* anon-fst #f)))

  (define-test "distinct anonymous optics trigger separate warnings"
    (reset-optic-warnings!)
    (set! *warn-unregistered-optics?* #t)
    (set! *strict-optic-registration?* #f)
    ;; Create two distinct anonymous lenses
    (let ([opt1 (make-lens car (lambda (b s) (cons b (cdr s))))]
          [opt2 (make-lens car (lambda (b s) (cons b (cdr s))))])
      (with-access-tracking (lambda () (reactive-view '(1 . 2) opt1)))
      (with-access-tracking (lambda () (reactive-view '(1 . 2) opt2)))
      ;; Should have 2 separate entries (eq? identity is different)
      (assert-equal 2 (hashtable-size *warned-optics*))))

  ;; Restore safe defaults after test group
  (set! *warn-unregistered-optics?* #t)
  (set! *strict-optic-registration?* #f)
  (reset-optic-warnings!))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
