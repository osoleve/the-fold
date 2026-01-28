;;; core/lang/test-module.ss — Tests for Module System
;;; Standardized to use test-framework.ss
;;;
;;; Tests the module loader and dependency tracking.
;;; Updated to test header-based auto-discovery of dependencies.

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

;; Note: Header parsing tests use files that have ;;; @module annotations.
;; Files like parse.ss and prim.ss use this format, while block.ss and eval.ss
;; use the newer (doc 'module ...) style which parse-module-header doesn't parse.

(test-group header-parsing
  (define-test "parse parse.ss header"
    (let ([header (parse-module-header "core/lang/parse.ss")])
      (assert-true (and (pair? header) (eq? (car header) 'parse)))))

  (define-test "parse prim.ss header"
    (let ([header (parse-module-header "core/lang/prim.ss")])
      (assert-true (and (pair? header) (eq? (car header) 'prim)))))

  (define-test "prim.ss has requires"
    (let ([header (parse-module-header "core/lang/prim.ss")])
      (assert-true (and header (pair? (cdr header)))))))

(test-group module-loading
  (define-test "prelude already loaded"
    (assert-true (module-loaded? 'prelude)))

  (define-test "prelude has load time"
    (let ([time (module-load-time 'prelude)])
      (assert-true (and time (>= time 0))))))

(test-group circular-dependency-detection
  (define-test "loading-stack empty when idle"
    ;; Clear any stale state first
    (set! *loading-stack* '())
    (assert-equal '() (loading-stack)))

  (define-test "detect-cycle prelude (no cycle)"
    (assert-false (detect-cycle 'prelude)))

  (define-test "validate-deps finds no cycles in production modules"
    ;; Ensure no test modules are in the deps table
    (hashtable-delete! *module-deps* 'test-a)
    (hashtable-delete! *module-deps* 'test-b)
    (hashtable-delete! *module-deps* 'test-c)
    (hashtable-delete! *module-deps* 'self-dep)
    (assert-equal '() (validate-deps)))

  ;; Self-contained cycle detection test
  (define-test "detect-cycle finds synthetic cycle"
    ;; Setup: create a cycle test-a -> test-b -> test-c -> test-a
    (hashtable-set! *module-deps* 'test-a '(test-b))
    (hashtable-set! *module-deps* 'test-b '(test-c))
    (hashtable-set! *module-deps* 'test-c '(test-a))
    (let ([cycle (detect-cycle 'test-a)])
      ;; Cleanup before assertion (in case of failure)
      (hashtable-delete! *module-deps* 'test-a)
      (hashtable-delete! *module-deps* 'test-b)
      (hashtable-delete! *module-deps* 'test-c)
      (assert-true (and (list? cycle) (> (length cycle) 0)))))

  (define-test "cycle contains all three nodes"
    ;; Setup
    (hashtable-set! *module-deps* 'test-a '(test-b))
    (hashtable-set! *module-deps* 'test-b '(test-c))
    (hashtable-set! *module-deps* 'test-c '(test-a))
    (let ([cycle (detect-cycle 'test-a)])
      ;; Cleanup
      (hashtable-delete! *module-deps* 'test-a)
      (hashtable-delete! *module-deps* 'test-b)
      (hashtable-delete! *module-deps* 'test-c)
      ;; memq returns list tail, convert to boolean
      (assert-true (and cycle
                        (if (memq 'test-a cycle) #t #f)
                        (if (memq 'test-b cycle) #t #f)
                        (if (memq 'test-c cycle) #t #f)))))

  (define-test "validate-deps finds cycles in test modules"
    ;; Setup
    (hashtable-set! *module-deps* 'test-a '(test-b))
    (hashtable-set! *module-deps* 'test-b '(test-c))
    (hashtable-set! *module-deps* 'test-c '(test-a))
    (let ([cycles (validate-deps)])
      ;; Cleanup
      (hashtable-delete! *module-deps* 'test-a)
      (hashtable-delete! *module-deps* 'test-b)
      (hashtable-delete! *module-deps* 'test-c)
      (assert-true (> (length cycles) 0))))

  (define-test "format-cycle produces non-empty string"
    (let ([formatted (format-cycle 'test-a '(test-c test-b test-a))])
      (assert-true (and (string? formatted)
                        (> (string-length formatted) 0)))))

  (define-test "require-one raises error for cycle"
    ;; Setup
    (hashtable-set! *module-deps* 'test-a '(test-b))
    (hashtable-set! *module-deps* 'test-b '(test-c))
    (hashtable-set! *module-deps* 'test-c '(test-a))
    (let ([error-raised #f])
      (guard (e [else (set! error-raised #t)])
             (require-one 'test-a))
      ;; Cleanup
      (hashtable-delete! *module-deps* 'test-a)
      (hashtable-delete! *module-deps* 'test-b)
      (hashtable-delete! *module-deps* 'test-c)
      ;; Clear loading stack that require-one may have modified
      (set! *loading-stack* '())
      (assert-true error-raised)))

  (define-test "detect-cycle finds self-dependency"
    ;; Setup
    (hashtable-set! *module-deps* 'self-dep '(self-dep))
    (let ([cycle (detect-cycle 'self-dep)])
      ;; Cleanup
      (hashtable-delete! *module-deps* 'self-dep)
      ;; memq returns list tail, convert to boolean
      (assert-true (and (list? cycle) (if (memq 'self-dep cycle) #t #f))))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
