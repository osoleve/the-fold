(load "core/testing/test-framework.ss")
(load "boundary/testing/ansi-test-output.ss")

(doc 'module 'test-ansi-output)
(doc 'description "Test the ANSI test output - Quick demo tests")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/testing/test-framework boundary/testing/ansi-test-output))

(doc 'section 'demo-tests)

(define-test "passing test"
  (assert-equal 4 (+ 2 2)))

(define-test "another pass"
  (assert-true #t))

(define-test "string test"
  (assert-equal "hello" "hello"))

(doc 'note "Uncomment to see failure output:")
(doc 'fixme "intentional failure for demo - kept commented")

(doc 'section 'run-tests)

(run-all-tests)
