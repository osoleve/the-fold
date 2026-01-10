;;; core/testing/README.sexp — Test Framework and Infrastructure
;;;
;;; Unified test infrastructure for The Fold.

((name . testing)
 (purpose . "Test framework and test runners for core modules")
 (tier-access . shepherd)
 (purity . pure)

 (description . "
Provides the unified test framework used by all core/ tests.
Tests self-register when defined and can be run as groups.

Key features:
- Named test registration with define-test
- Test grouping with test-group
- Standard assertions (equal, true, false, error, ok)
- Pass/fail counting and reporting
- Group-based or full-suite test runs
")

 (modules
  ((file . test-framework.ss)
   (purpose . "Core test framework with assertions and registry")
   (api . (define-test test-group assert-equal assert-true
           assert-false assert-error assert-ok
           run-all-tests run-tests)))

  ((file . run-tests.ss)
   (purpose . "Master test runner for all core modules"))

  ((file . quick-test.ss)
   (purpose . "Quick validation script for CI/CD")))

 (documentation
  ((file . TEST-FRAMEWORK.md)
   (purpose . "Detailed framework documentation")))

 (dependencies
  (internal . (core/base/prelude.ss)))

 (usage . "
Load test framework:
  (load \"core/testing/test-framework.ss\")

Or via legacy path (forwarding stub):
  (load \"core/test-framework.ss\")

Define tests:
  (test-group \"my-module\"
    (define-test \"basic functionality\"
      (assert-equal 4 (+ 2 2)))
    (define-test \"error handling\"
      (assert-error (/ 1 0))))

Run tests:
  (run-all-tests)        ; All registered tests
  (run-tests 'my-module) ; Specific group

Run from command line:
  scheme --script core/testing/run-tests.ss
")

 (notes . "
Backwards compatibility: core/test-framework.ss forwards to this location.
New code should use core/testing/test-framework.ss directly.
")

 (see-also . (
   "core/testing/TEST-FRAMEWORK.md"
   "shell/tests/README.sexp")))
