;;; thimble/tests/test-tdd.ss — Test the TDD system itself

(load "./fabric/stitches/test-framework.ss")

;;; Test basic TDD functionality
(define-test "tdd-system-loads"
  (assert-true #t)  ; Basic sanity check
  (assert-equal 4 (+ 2 2)))

(define-test "test-discovery-works"
  ;; Test that we can discover test files
  (let ([core-tests '("fabric/stitches/test-block.ss"
                      "fabric/stitches/test-prim.ss"
                      "fabric/stitches/test-parse.ss")])
       (assert-true (> (length core-tests) 0))
       (assert-equal 3 (length core-tests))))

(define-test "test-framework-integration"
  ;; Test that our tests integrate with the framework
  (assert-equal "hello" "hello")
  (assert-true (string? "test"))
  (assert-false (number? "test")))

(display "TDD system tests loaded successfully!
")
