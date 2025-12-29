(load "./fabric/stitches/prelude.ss")
(load "./fabric/stitches/test-framework.ss")
(load "./thimble/tests/test-tdd.ss")

(display "Running TDD system tests...
")
(run-all-tests)
