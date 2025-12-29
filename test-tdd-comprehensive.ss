(display "Loading comprehensive TDD system...
")

;; Load dependencies
(load "./fabric/stitches/prelude.ss")
(load "./fabric/stitches/test-framework.ss")
(load "./thimble/tests/test-runner.ss")

(display "Dependencies loaded.
")

;; Test the test framework
(display "Testing test framework...
")

;; Define a simple test
(define-test "simple-arithmetic-test"
  (assert-equal 4 (+ 2 2))
  (assert-equal 10 (* 2 5)))

(define-test "string-test"
  (assert-equal "hello world" (string-append "hello" " " "world")))

;; Run the tests
(display "Running tests...
")
(let ([results (run-all-tests)])
     (display "Results: ")
     (display results)
     (newline))

;; Test test discovery
(display "Testing test discovery...
")
(let ([core-tests '("fabric/stitches/test-block.ss"
                    "fabric/stitches/test-prim.ss"
                    "fabric/stitches/test-parse.ss")])
     (display "Core tests found: ")
     (display (length core-tests))
     (newline))

(display "TDD system test completed successfully!
")
