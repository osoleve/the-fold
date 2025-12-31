;;; test-persona-dsl.ss - Tests for Enhanced Persona DSL
;;;
;;; Run from project root:
;;;   scheme --script agents/lib/test-persona-dsl.ss

(load "fabric/stitches/test-framework.ss")
(load "agents/lib/persona-dsl.ss")

;;; ============================================================
;;; Helper Functions (must be defined before tests)
;;; ============================================================

;;; string-contains : String -> String -> Boolean
(define (string-contains haystack needle)
  (let ([hay-len (string-length haystack)]
        [needle-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i needle-len) hay-len) #f]
             [(string=? (substring haystack i (+ i needle-len)) needle) #t]
             [else (loop (+ i 1))]))))

(display "\n")
(display "==============================================================\n")
(display "         PERSONA DSL TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Basic Emission Tests
;;; ============================================================

(test-group basic-emit
            (define-test emit-simple-test
              (let ([result (run-persona (emit "Hello"))])
                   (assert-equal "Hello" result)))
            
            (define-test emit-line-test
              (let ([result (run-persona (emit-line "Hello" " World"))])
                   (assert-equal "Hello World\n" result)))
            
            (define-test emit-multiple-test
              (let ([result (run-persona
                             (prompt-do
                              (emit "Line 1")
                              (emit-line "Line 2")
                              (emit "Line 3")
                              (done)))])
                   (assert-true (string? result))
                   (assert-true (> (string-length result) 0))))
            
            (define-test emit-blank-test
              (let ([result (run-persona
                             (prompt-do
                              (emit "Before")
                              (emit-blank)
                              (emit "After")
                              (done)))])
                   (assert-true (string-contains result "\n"))))
            
            (define-test emit-section-test
              (let ([result (run-persona (emit-section "My Section"))])
                   (assert-true (string-contains result "My Section"))
                   (assert-true (string-contains result "─")))))

;;; ============================================================
;;; Choice Tests
;;; ============================================================

(test-group choices
            (define-test pick-returns-option-test
              (let ([result (run-persona
                             (prompt-do
                              (x <- (pick '("A" "B" "C")))
                              (emit x)
                              (done)))])
                   (assert-true (or (string=? result "A")
                                    (string=? result "B")
                                    (string=? result "C")))))
            
            (define-test pick-n-returns-correct-count-test
              (let* ([result (run-persona-traced
                              (prompt-do
                               (items <- (pick-n 2 '("A" "B" "C" "D")))
                               (emit (car items))
                               (emit (cadr items))
                               (done)))]
                     [choices (cdr result)])
                    ;; Should have recorded a pick-n choice
                    (assert-true (> (length choices) 0))
                    (let ([picked (cdar choices)])
                         (assert-equal 2 (length picked)))))
            
            (define-test pick-weighted-test
              ;; With weight 100 vs 0, should always pick first option
              (let ([result (run-persona
                             (prompt-do
                              (x <- (pick-weighted '((100 . "Always") (0 . "Never"))))
                              (emit x)
                              (done)))])
                   (assert-equal "Always" result))))

;;; ============================================================
;;; Do-Notation Tests
;;; ============================================================

(test-group do-notation
            (define-test prompt-do-basic-test
              (let ([result (run-persona
                             (prompt-do
                              (emit "Start")
                              (emit-line " End")
                              (done)))])
                   (assert-equal "Start End\n" result)))
            
            (define-test prompt-do-binding-test
              (let ([result (run-persona
                             (prompt-do
                              (name <- (pick '("Alice")))
                              (emit "Hello ")
                              (emit name)
                              (done)))])
                   (assert-equal "Hello Alice" result)))
            
            (define-test prompt-do-nested-test
              (let ([result (run-persona
                             (prompt-do
                              (x <- (prompt-do
                                     (a <- (pick '("Inner")))
                                     (dsl-pure a)))
                              (emit x)
                              (done)))])
                   (assert-equal "Inner" result))))

;;; ============================================================
;;; Choice Tracking Tests
;;; ============================================================

(test-group tracking
            (define-test run-traced-returns-choices-test
              (let* ([result (run-persona-traced
                              (prompt-do
                               (style <- (pick '("A" "B")))
                               (emit style)
                               (done)))]
                     [prompt (car result)]
                     [choices (cdr result)])
                    (assert-true (string? prompt))
                    (assert-true (list? choices))
                    (assert-true (> (length choices) 0))))
            
            (define-test choices-recorded-in-order-test
              (let* ([result (run-persona-traced
                              (prompt-do
                               (x <- (pick '("First")))
                               (y <- (pick '("Second")))
                               (emit x)
                               (emit y)
                               (done)))]
                     [choices (cdr result)])
                    (assert-equal 2 (length choices)))))

;;; ============================================================
;;; List Emission Tests
;;; ============================================================

(test-group lists
            (define-test emit-list-test
              (let ([result (run-persona
                             (emit-list "Items:" '("One" "Two" "Three")))])
                   (assert-true (string-contains result "Items:"))
                   (assert-true (string-contains result "One"))
                   (assert-true (string-contains result "Two"))
                   (assert-true (string-contains result "Three"))
                   (assert-true (string-contains result "•"))))
            
            (define-test emit-numbered-test
              (let ([result (run-persona
                             (emit-numbered "Steps:" '("First" "Second")))])
                   (assert-true (string-contains result "Steps:"))
                   (assert-true (string-contains result "1."))
                   (assert-true (string-contains result "2.")))))

;;; ============================================================
;;; Convenience Constructor Tests
;;; ============================================================

(test-group convenience
            (define-test persona-header-test
              (let ([result (run-persona
                             (persona-header "TestAgent" "A test persona"))])
                   (assert-true (string-contains result "TestAgent"))
                   (assert-true (string-contains result "Your Role"))))
            
            (define-test persona-voice-test
              (let ([result (run-persona
                             (persona-voice '("Style A" "Style B")))])
                   (assert-true (string-contains result "Your Voice"))
                   (assert-true (or (string-contains result "Style A")
                                    (string-contains result "Style B")))))
            
            (define-test persona-approaches-test
              (let ([result (run-persona
                             (persona-approaches 2 '("Approach 1" "Approach 2" "Approach 3")))])
                   (assert-true (string-contains result "Your Approaches")))))

;;; ============================================================
;;; Error Handling Tests
;;; ============================================================

(test-group error-handling
            (define-test run-safe-success-test
              (let ([result (run-persona-safe (emit "Test"))])
                   (assert-true (dsl-ok? result))))
            
            (define-test try-load-fragment-missing-test
              ;; Loading a non-existent fragment should return error result
              (let ([result (run-persona
                             (prompt-do
                              (frag-result <- (try-load-fragment 'nonexistent-fragment-xyz))
                              (if (dsl-ok? frag-result)
                                  (emit "Found")
                                  (emit "Not found"))
                              (done)))])
                   (assert-equal "Not found" result))))

;;; ============================================================
;;; Composition Tests
;;; ============================================================

(test-group composition
            (define-test compose-prompts-test
              (let ([result (run-persona
                             (compose-prompts
                              (emit "First")
                              (emit " Second")))])
                   (assert-equal "First Second" result)))
            
            (define-test merge-personas-test
              (let ([result (run-persona
                             (merge-personas
                              (list (emit "A")
                                    (emit "B")
                                    (emit "C"))))])
                   (assert-equal "ABC" result))))

;;; ============================================================
;;; Real Persona Test
;;; ============================================================

(test-group real-persona
            (define-test sentinel-v2-loads-test
              ;; Just verify the sentinel-dsl-v2 file loads and runs
              (load "agents/personas/sentinel-dsl-v2.ss")
              (let ([result (run-persona sentinel-persona)])
                   (assert-true (string? result))
                   (assert-true (> (string-length result) 100))
                   (assert-true (string-contains result "Sentinel"))
                   (assert-true (string-contains result "Your Role"))))
            
            (define-test sentinel-v2-traced-test
              (let* ([result (run-persona-traced sentinel-persona)]
                     [prompt (car result)]
                     [choices (cdr result)])
                    ;; Should have choices for: style, 2 approaches, 2 triggers, signoff
                    (assert-true (>= (length choices) 4))
                    (assert-true (string-contains prompt "Sentinel")))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All persona DSL tests passed.\n")
    (display "\n[FAILURE] Some persona DSL tests failed.\n"))
