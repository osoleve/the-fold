;;; fabric/stitches/test-module.ss — Tests for Module System
;;;
;;; Tests the module loader and dependency tracking.

(load "core/module.ss")

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
;;; Dependency Declaration Tests
;;; ============================================================

(display "Dependency Declarations:
")

(test "prelude has no deps" '() (module-deps 'prelude))
(test "block depends on prelude" '(prelude) (module-deps 'block))
(test "eval deps" '(prelude block prim) (module-deps 'eval))
(test "infer deps" '(prelude types kinds) (module-deps 'infer))

;;; ============================================================
;;; Transitive Dependencies
;;; ============================================================

(display "
Transitive Dependencies:
")

(test "all-deps prelude" '() (all-deps 'prelude))
(test "all-deps block" '(prelude) (all-deps 'block))

;; eval depends on prelude, block, prim
(let ([deps (all-deps 'eval)])
     (test "all-deps eval includes prelude"
           #t (if (member 'prelude deps) #t #f))
     (test "all-deps eval includes block"
           #t (if (member 'block deps) #t #f)))

;;; ============================================================
;;; Module Loading
;;; ============================================================

(display "
Module Loading:
")

;; prelude is registered when module.ss loads
(test "prelude already loaded" #t (module-loaded? 'prelude))
(test "eval not loaded yet" #f (module-loaded? 'eval))

;; Load eval
(require 'eval)

(test "eval now loaded" #t (module-loaded? 'eval))
(test "block loaded (dependency)" #t (module-loaded? 'block))
(test "prim loaded (dependency)" #t (module-loaded? 'prim))

;; Load compile (should load more deps)
(require 'compile)

(test "compile loaded" #t (module-loaded? 'compile))
(test "infer loaded" #t (module-loaded? 'infer))
(test "types loaded" #t (module-loaded? 'types))

;;; ============================================================
;;; Load Times
;;; ============================================================

(display "
Load Times:
")

(let ([time (module-load-time 'prelude)])
     (test "prelude has load time" #t (and time (>= time 0))))

(let ([time (module-load-time 'compile)])
     (test "compile has load time" #t (and time (> time 0))))

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
