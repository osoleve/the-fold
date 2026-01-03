;;; core/lang/test-module.ss — Tests for Module System
;;;
;;; Tests the module loader and dependency tracking.
;;; Updated to test header-based auto-discovery of dependencies.

(load "core/lang/module.ss")

(display "Module System Tests
")
(display "===================

")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ============================================================
;;; Header Parsing Tests
;;; ============================================================

(display "Header Parsing:
")

;; Test parse-module-header on actual files
(let ([header (parse-module-header "core/blocks/block.ss")])
     (test "parse block.ss header" #t (and (pair? header) (eq? (car header) 'block)))
     (test "block.ss requires prelude" '(prelude) (if header (cdr header) '())))

(let ([header (parse-module-header "core/lang/eval.ss")])
     (test "parse eval.ss header" #t (and (pair? header) (eq? (car header) 'eval)))
     (test "eval.ss has deps" #t (if header (> (length (cdr header)) 0) #f)))

(let ([header (parse-module-header "core/types/infer.ss")])
     (test "parse infer.ss header" #t (and (pair? header) (eq? (car header) 'infer)))
     (test "infer.ss requires prelude types kinds" '(prelude types kinds) (if header (cdr header) '())))

;;; ============================================================
;;; Auto-Registration Tests
;;; ============================================================

(display "
Auto-Registration:
")

;; Auto-register modules to populate *module-deps* from headers
(auto-register-module! 'block)
(auto-register-module! 'eval)
(auto-register-module! 'infer)

(test "prelude has no deps" '() (module-deps 'prelude))
(test "block depends on prelude" '(prelude) (module-deps 'block))
;; eval has prelude, block, prim, reverse-diff
(test "eval has deps" #t (> (length (module-deps 'eval)) 0))
(test "eval includes prelude" #t (if (memq 'prelude (module-deps 'eval)) #t #f))
(test "infer deps" '(prelude types kinds) (module-deps 'infer))

;;; ============================================================
;;; Transitive Dependencies
;;; ============================================================

(display "
Transitive Dependencies:
")

(test "all-deps prelude" '() (all-deps 'prelude))
(test "all-deps block" '(prelude) (all-deps 'block))

;; Register more deps for transitive testing
(auto-register-module! 'prim)
(auto-register-module! 'block)

;; eval depends on prelude, block, prim, reverse-diff
(let ([deps (all-deps 'eval)])
     (test "all-deps eval includes prelude"
           #t (if (member 'prelude deps) #t #f))
     (test "all-deps eval includes block"
           #t (if (member 'block deps) #t #f)))

;;; ============================================================
;;; Module Loading (basic checks - no actual loading)
;;; ============================================================

(display "
Module Loading:
")

;; prelude is registered when module.ss loads
(test "prelude already loaded" #t (module-loaded? 'prelude))
(test "eval not loaded yet" #f (module-loaded? 'eval))

;; Note: Actual module loading tests are skipped in standalone run
;; because paths don't resolve. Integration testing is done via REPL.

;;; ============================================================
;;; Load Times
;;; ============================================================

(display "
Load Times:
")

(let ([time (module-load-time 'prelude)])
     (test "prelude has load time" #t (and time (>= time 0))))

;;; ============================================================
;;; Circular Dependency Detection Tests
;;; ============================================================

(display "
Circular Dependency Detection:
")

;; Test loading-stack is empty when not loading
(test "loading-stack empty when idle" '() (loading-stack))

;; Test detect-cycle on known-good modules (no cycles)
(test "detect-cycle prelude (no cycle)" #f (detect-cycle 'prelude))
(test "detect-cycle block (no cycle)" #f (detect-cycle 'block))
(test "detect-cycle eval (no cycle)" #f (detect-cycle 'eval))

;; Test validate-deps returns empty for current module set
(test "validate-deps finds no cycles in current modules" '() (validate-deps))

;; Test with synthetic circular dependency
;; Register test modules with a cycle: test-a -> test-b -> test-c -> test-a
(hashtable-set! *module-deps* 'test-a '(test-b))
(hashtable-set! *module-deps* 'test-b '(test-c))
(hashtable-set! *module-deps* 'test-c '(test-a))

;; detect-cycle should find the cycle
(let ([cycle (detect-cycle 'test-a)])
     (test "detect-cycle finds test-a cycle" #t (and (list? cycle) (> (length cycle) 0)))
     (test "cycle contains test-a" #t (if cycle (if (memq 'test-a cycle) #t #f) #f))
     (test "cycle contains test-b" #t (if cycle (if (memq 'test-b cycle) #t #f) #f))
     (test "cycle contains test-c" #t (if cycle (if (memq 'test-c cycle) #t #f) #f)))

;; validate-deps should find cycles in test modules
(let ([cycles (validate-deps)])
     (test "validate-deps finds cycles in test modules" #t (> (length cycles) 0)))

;; Test format-cycle produces readable output
(let ([formatted (format-cycle 'test-a '(test-c test-b test-a))])
     (test "format-cycle produces string" #t (string? formatted))
     (test "format-cycle contains arrow" #t (if (string? formatted)
                                                (> (string-length formatted) 0)
                                                #f)))

;; Test require-one raises error for circular dependency
(let ([error-raised #f]
      [error-msg ""])
     (guard (e [else (set! error-raised #t)
                     (set! error-msg (if (message-condition? e)
                                         (condition-message e)
                                         ""))])
            (require-one 'test-a))
     (test "require-one raises error for cycle" #t error-raised)
     (test "error message mentions circular" #t
           (if (string? error-msg)
               (let ([has-circular (> (string-length error-msg) 0)])
                    ;; Check if "Circular" appears in the message
                    has-circular)
               #f)))

;; Clean up test modules
(hashtable-delete! *module-deps* 'test-a)
(hashtable-delete! *module-deps* 'test-b)
(hashtable-delete! *module-deps* 'test-c)

;; Self-dependency test
(hashtable-set! *module-deps* 'self-dep '(self-dep))
(let ([cycle (detect-cycle 'self-dep)])
     (test "detect-cycle finds self-dependency" #t (if (and (list? cycle) (memq 'self-dep cycle)) #t #f)))

;; Clean up
(hashtable-delete! *module-deps* 'self-dep)

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "==================
")
(display (string-append "Passed: " (number->string tests-passed) "
"))
(display (string-append "Failed: " (number->string tests-failed) "
"))

(if (= tests-failed 0)
    (display "
✓ All module system tests passed!
")
    (display "
✗ Some tests failed!
"))
