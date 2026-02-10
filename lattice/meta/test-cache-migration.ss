;;; lattice/meta/test-cache-migration.ss — Tests for Cache Migration System
;;;
;;; Tests bidirectional cache format migrations.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/meta/cache-migration.ss")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define sample-cache-v1
  '(lattice-cache
    (version 1)
    (fingerprint "abc123")
    (kg-state
     (skill-names (foo bar baz))
     (export-names (x y z)))
    (docstrings ((foo . "Foo function")))))

(define sample-cache-v2
  '(lattice-cache
    (version 2)
    (fingerprint "abc123")
    (kg-state
     (exports-count 3)
     (skill-names (foo bar baz))
     (export-names (x y z)))
    (docstrings ((foo . "Foo function")))))

;;; ============================================================
;;; Tests
;;; ============================================================

(test-group "Cache Version Detection"

  (define-test "cache-version extracts version"
    (assert-equal 1 (cache-version sample-cache-v1)))

  (define-test "cache-version returns 0 for invalid cache"
    (assert-equal 0 (cache-version '(not-a-cache))))

  (define-test "cache-set-version updates version"
    (let ([updated (cache-set-version sample-cache-v1 2)])
      (assert-equal 2 (cache-version updated)))))

(test-group "Cache Migration Registration"

  (define-test "register and retrieve migration"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (assert-true (migration? (get-cache-migration 1 2))))

  (define-test "list-cache-migrations shows registered"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (assert-true (pair? (member '(1 . 2) (list-cache-migrations))))))

(test-group "Cache Migration Application"

  (define-test "migrate-cache applies v1->v2"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (let ([migrated (migrate-cache sample-cache-v1 2)])
      (assert-equal 2 (cache-version migrated))
      ;; Check exports-count was added
      (let* ([kg-state (cdr (assq 'kg-state (cdr migrated)))]
             [count-pair (assq 'exports-count kg-state)])
        (assert-true (pair? count-pair))
        (assert-equal 3 (cadr count-pair)))))

  (define-test "rollback-cache applies v2->v1"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (let ([rolled-back (rollback-cache sample-cache-v2 1)])
      (assert-equal 1 (cache-version rolled-back))
      ;; Check exports-count was removed
      (let* ([kg-state (cdr (assq 'kg-state (cdr rolled-back)))]
             [count-pair (assq 'exports-count kg-state)])
        (assert-false count-pair))))

  (define-test "migrate-cache is no-op when already at target"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (let ([result (migrate-cache sample-cache-v2 2)])
      (assert-equal 2 (cache-version result)))))

(test-group "Migration Path Finding"

  (define-test "find path for direct migration"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (let ([path (find-cache-migration-path 1 2)])
      (assert-true (list? path))
      (assert-equal 1 (length path))))

  (define-test "find path returns empty for same version"
    (assert-equal '() (find-cache-migration-path 1 1)))

  (define-test "cache-migrations-available checks path existence"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (assert-true (cache-migrations-available 1 2))
    (assert-false (cache-migrations-available 5 6))))

(test-group "Integration"

  (define-test "try-migrate-cache succeeds for valid path"
    (register-cache-migration! 1 2 cache-v1->v2-iso)
    (let ([result (try-migrate-cache sample-cache-v1 2)])
      (assert-true (and result (= 2 (cache-version result))))))

  (define-test "try-migrate-cache returns #f for invalid path"
    (let ([result (try-migrate-cache sample-cache-v1 99)])
      (assert-false result))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
