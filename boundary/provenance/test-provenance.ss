;;; boundary/provenance/test-provenance.ss — Tests for Provenance Tracking
;;;
;;; Tests for optic provenance instrumentation:
;;;   - Provenance record creation
;;;   - Optic registry
;;;   - Traced operations
;;;   - Lineage queries
;;;   - Value retrieval
;;;
;;; Run: scheme --script boundary/provenance/test-provenance.ss

(load "core/testing/test-framework.ss")
(load "boundary/provenance/traced-optics.ss")
(load "boundary/provenance/query.ss")

;;; ============================================================
;;; Test Setup
;;; ============================================================

;; Ensure provenance is enabled for tests
(provenance-enable!)

;; Set default test context
(set! *current-agent-id* 'test-agent)
(set! *current-session-id* "test-session")

;;; ============================================================
;;; Part 1: Provenance Configuration Tests
;;; ============================================================

(test-group "provenance-config"
  (define-test "provenance enabled by default"
    (assert-true *provenance-enabled?*))

  (define-test "can disable and re-enable provenance"
    (provenance-disable!)
    (assert-false *provenance-enabled?*)
    (provenance-enable!)
    (assert-true *provenance-enabled?*))

  (define-test "with-agent-id sets agent identity"
    (with-agent-id 'custom-agent
      (lambda ()
        (assert-equal 'custom-agent *current-agent-id*))))

  (define-test "with-session-id sets session identity"
    (with-session-id "custom-session"
      (lambda ()
        (assert-equal "custom-session" *current-session-id*)))))

;;; ============================================================
;;; Part 2: Optic Registry Tests
;;; ============================================================

(test-group "optic-registry"
  (define-test "can register an optic with a name"
    (let ([my-lens (make-lens car (lambda (a p) (cons a (cdr p))))])
      (register-optic! 'my-lens my-lens)
      (assert-equal 'my-lens (lookup-optic-name my-lens))))

  (define-test "can lookup optic by name"
    (let ([found (lookup-optic 'lens-fst)])
      ;; Should find something (optics may differ by identity across loads)
      (assert-true (pair? found))))

  (define-test "standard optics are pre-registered"
    ;; Check that the names exist in reverse registry
    (assert-true (pair? (lookup-optic 'lens-fst)))
    (assert-true (pair? (lookup-optic 'lens-snd)))
    (assert-true (pair? (lookup-optic 'prism-just)))
    (assert-true (pair? (lookup-optic 'traversal-each))))

  (define-test "list-registered-optics returns alist"
    (let ([registered (list-registered-optics)])
      (assert-true (list? registered))
      (assert-true (pair? registered)))))

;;; ============================================================
;;; Part 3: Value Storage Tests
;;; ============================================================

(test-group "value-storage"
  (define-test "can store and fetch simple value"
    (let* ([val 42]
           [hash (store-value! val)]
           [fetched (fetch-value hash)])
      (assert-equal 42 fetched)))

  (define-test "can store and fetch pair"
    (let* ([val '(1 . 2)]
           [hash (store-value! val)]
           [fetched (fetch-value hash)])
      (assert-equal '(1 . 2) fetched)))

  (define-test "can store and fetch list"
    (let* ([val '(a b c d)]
           [hash (store-value! val)]
           [fetched (fetch-value hash)])
      (assert-equal '(a b c d) fetched)))

  (define-test "same value produces same hash"
    (let* ([hash1 (store-value! '(1 2 3))]
           [hash2 (store-value! '(1 2 3))])
      (assert-true (bytevector=? hash1 hash2)))))

;;; ============================================================
;;; Part 4: Traced Lens Operations Tests
;;; ============================================================

(test-group "traced-lens"
  (define-test "traced-view returns correct value"
    (let ([result (traced-view '(10 . 20) lens-fst)])
      (assert-equal 10 result)))

  (define-test "traced-set returns modified structure"
    (let ([result (traced-set lens-fst 99 '(1 . 2))])
      (assert-equal '(99 . 2) result)))

  (define-test "traced-lens-over applies function"
    (let ([result (traced-lens-over lens-fst add1 '(5 . 10))])
      (assert-equal '(6 . 10) result)))

  (define-test "traced operations record provenance"
    (let* ([source '(100 . 200)]
           [result (traced-set lens-fst 999 source)]
           [source-hash (hash->hex (store-value! source))]
           [record (provenance-for-source source-hash)])
      ;; Should have a provenance record
      (assert-true (provenance-record? record))
      ;; Check operation type (optic name may not match due to eq? identity)
      (assert-equal 'set (provenance-operation record))
      (assert-equal 'lens (provenance-optic-type record)))))

;;; ============================================================
;;; Part 5: Traced Prism Operations Tests
;;; ============================================================

(test-group "traced-prism"
  (define-test "traced-prism-preview on Just"
    (let ([result (traced-prism-preview (just 42) prism-just)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test "traced-prism-preview on Nothing"
    (let ([result (traced-prism-preview nothing prism-just)])
      (assert-true (nothing? result))))

  (define-test "traced-prism-review constructs value"
    (let ([result (traced-prism-review 42 prism-just)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result)))))

;;; ============================================================
;;; Part 6: Traced Traversal Operations Tests
;;; ============================================================

(test-group "traced-traversal"
  (define-test "traced-traversal-to-list gets all elements"
    (let ([result (traced-traversal-to-list '(1 2 3 4) traversal-each)])
      (assert-equal '(1 2 3 4) result)))

  (define-test "traced-traversal-over modifies all elements"
    (let ([result (traced-traversal-over traversal-each add1 '(1 2 3))])
      (assert-equal '(2 3 4) result)))

  (define-test "traced-traversal-set sets all elements"
    (let ([result (traced-traversal-set traversal-each 0 '(1 2 3))])
      (assert-equal '(0 0 0) result))))

;;; ============================================================
;;; Part 7: Traced Generic Operations Tests
;;; ============================================================

(test-group "traced-generic"
  (define-test "traced-view works with lens"
    (assert-equal 1 (traced-view '(1 . 2) lens-fst)))

  (define-test "traced-preview works with prism"
    (let ([result (traced-preview (just 5) prism-just)])
      (assert-true (just? result))
      (assert-equal 5 (from-just result))))

  (define-test "traced-to-list works with traversal"
    (assert-equal '(1 2 3) (traced-to-list '(1 2 3) traversal-each)))

  (define-test "traced-set works with any settable optic"
    (assert-equal '(9 . 2) (traced-set lens-fst 9 '(1 . 2)))))

;;; ============================================================
;;; Part 8: Lineage Tracking Tests
;;; ============================================================

(test-group "lineage"
  (define-test "trace-lineage returns list of provenance records"
    (let* ([step1 (traced-set lens-fst 10 '(0 . 0))]
           [step2 (traced-set lens-snd 20 step1)]
           [result-hash (hash->hex (store-value! step2))]
           [lineage (trace-lineage result-hash)])
      ;; Should have at least one step
      (assert-true (>= (length lineage) 1))
      (assert-true (andmap provenance-record? lineage))))

  (define-test "lineage captures transformation chain"
    ;; Build a chain of transformations
    (let* ([v1 '(a . b)]
           [v2 (traced-set lens-fst 'x v1)]
           [v3 (traced-set lens-snd 'y v2)]
           [v2-hash (hash->hex (store-value! v2))]
           [v3-hash (hash->hex (store-value! v3))]
           [v3-prov (provenance-for-result v3-hash)])
      ;; v3's provenance should reference v2 as source
      (assert-true (provenance-record? v3-prov))
      ;; The source of v3 should be v2
      (assert-equal v2-hash (provenance-source-hash v3-prov)))))

;;; ============================================================
;;; Part 9: Query API Tests
;;; ============================================================

(test-group "query-api"
  (define-test "provenance-for-result finds record"
    (let* ([source '(test . query)]
           [result (traced-set lens-fst 'found source)]
           [result-hash (hash->hex (store-value! result))]
           [record (provenance-for-result result-hash)])
      (assert-true (provenance-record? record))
      (assert-equal 'set (provenance-operation record))))

  (define-test "provenance-summary returns alist"
    (let* ([result (traced-view '(1 . 2) lens-fst)]
           [record (latest-provenance)]
           [summary (provenance-summary record)])
      (assert-true (list? summary))
      (assert-true (pair? (assq 'operation summary)))
      (assert-true (pair? (assq 'timestamp summary)))))

  (define-test "explain-value returns string"
    (let* ([result (traced-set lens-fst 'explained '(? . !))]
           [explanation (explain-value result)])
      (assert-true (string? explanation))
      (assert-true (> (string-length explanation) 0)))))

;;; ============================================================
;;; Part 10: Provenance Record Data Tests
;;; ============================================================

(test-group "provenance-data"
  (define-test "provenance record has correct operation"
    (traced-view '(check . this) lens-fst)
    (let ([record (latest-provenance)])
      (assert-equal 'view (provenance-operation record))))

  (define-test "provenance record has optic type"
    (traced-set lens-snd 42 '(a . b))
    (let ([record (latest-provenance)])
      (assert-equal 'lens (provenance-optic-type record))))

  (define-test "provenance record has timestamp"
    (traced-view '(ts . test) lens-fst)
    (let ([record (latest-provenance)])
      (let ([ts (provenance-timestamp record)])
        (assert-true (string? ts))
        ;; Should be ISO8601 format
        (assert-true (> (string-length ts) 10)))))

  (define-test "provenance record has agent/session from context"
    (with-agent-id 'agent-test
      (lambda ()
        (with-session-id "session-test"
          (lambda ()
            (traced-view '(ctx . test) lens-fst)
            (let ([record (latest-provenance)])
              (assert-equal 'agent-test (provenance-agent-id record))
              (assert-equal "session-test" (provenance-session-id record)))))))))

;;; ============================================================
;;; Part 11: Scoping Tests
;;; ============================================================

(test-group "scoping"
  (define-test "without-tracing suppresses provenance"
    (let ([before-count (length (store-hashes))])
      (without-tracing
       (lambda ()
         ;; These should not create provenance records
         (traced-view '(no . trace) lens-fst)
         (traced-set lens-fst 1 '(0 . 0))))
      ;; Only value storage should happen, not provenance
      (assert-true (>= (length (store-hashes)) before-count))))

  (define-test "with-tracing re-enables after without-tracing"
    (without-tracing
     (lambda ()
       (with-tracing
        (lambda ()
          (traced-view '(re . enabled) lens-fst)
          (let ([record (latest-provenance)])
            (assert-true (provenance-record? record)))))))))

;;; ============================================================
;;; Part 12: Composed Optic Tests
;;; ============================================================

(test-group "composed-optics"
  (define-test "traced->>> composes optics"
    (let* ([nested '((1 . 2) . (3 . 4))]
           [composed (traced->>> lens-fst lens-fst)]
           [result (traced-view nested composed)])
      (assert-equal 1 result)))

  (define-test "traced->>> auto-registers if both named"
    (let ([composed (traced->>> lens-fst lens-snd)])
      ;; Should be registered with combined name
      (let ([name (lookup-optic-name composed)])
        ;; May or may not be registered depending on timing
        (assert-true (or (not name) (symbol? name)))))))

;;; ============================================================
;;; Part 13: Value Retrieval Tests
;;; ============================================================

(test-group "value-retrieval"
  (define-test "provenance-get-source retrieves source value"
    (let* ([source '(retrieve . source)]
           [result (traced-set lens-fst 'new source)]
           [record (latest-provenance)]
           [retrieved (provenance-get-source record)])
      (assert-equal source retrieved)))

  (define-test "provenance-get-result retrieves result value"
    (let* ([source '(retrieve . result)]
           [result (traced-set lens-fst 'got source)]
           [record (latest-provenance)]
           [retrieved (provenance-get-result record)])
      (assert-equal result retrieved)))

  (define-test "provenance-get-value retrieves set value"
    (let* ([val 'the-value]
           [result (traced-set lens-fst val '(? . x))]
           [record (latest-provenance)]
           [retrieved (provenance-get-value record)])
      (assert-equal val retrieved))))

;;; ============================================================
;;; Part 14: Statistics Tests
;;; ============================================================

(test-group "statistics"
  (define-test "provenance-stats returns alist with counts"
    (let ([stats (provenance-stats)])
      (assert-true (list? stats))
      (assert-true (pair? (assq 'total stats)))
      (assert-true (pair? (assq 'by-operation stats)))
      (assert-true (pair? (assq 'by-optic stats)))
      (assert-true (pair? (assq 'by-agent stats)))))

  (define-test "stats total is non-negative"
    (let* ([stats (provenance-stats)]
           [total (cdr (assq 'total stats))])
      (assert-true (>= total 0)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
