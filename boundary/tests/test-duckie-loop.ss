(load "boundary/assistants/duckie-loop.ss")

(doc 'module 'test-duckie-loop)
(doc 'description "Tests for DUCKIE Main Loop")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Validates command parsing and state transitions")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal name actual expected)
(doc assert-equal 'description "Test assertion helper")
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(doc 'section 'command-parsing-tests)

(printf "\n=== Testing parse-command ===\n")

(assert-equal "parse pet" (parse-command "pet") '(pet))
(assert-equal "parse feed" (parse-command "feed") '(feed))
(assert-equal "parse play" (parse-command "play") '(play))
(assert-equal "parse sleep" (parse-command "sleep") '(sleep))
(assert-equal "parse wake" (parse-command "wake") '(wake))
(assert-equal "parse quit" (parse-command "quit") '(quit))

(assert-equal "parse talk hello" (parse-command "talk hello") '(talk "hello"))
(assert-equal "parse talk with spaces" (parse-command "talk  hello world ") '(talk "hello world"))

(assert-equal "parse unknown" (parse-command "jump") #f)
(assert-equal "parse empty" (parse-command "") #f)

(doc 'section 'loop-state-tests)

(printf "\n=== Testing loop state ===\n")

(let* ([duckie (make-duckie "Tester")]
       [canvas (make-canvas 10 10)]
       [particles (make-particle-system)]
       [state (make-loop-state duckie canvas 0 #t 'happy 0 particles)])
      
      (assert-equal "loop-state-frame" (loop-state-frame state) 0)
      (assert-equal "loop-state-running" (loop-state-running state) #t)
      
      (let ([state2 (loop-state-set-frame state 10)])
           (assert-equal "loop-state-set-frame" (loop-state-frame state2) 10))
      
      (let ([state3 (loop-state-set-running state #f)])
           (assert-equal "loop-state-set-running" (loop-state-running state3) #f)))

(doc 'section 'tick-tests)

(printf "\n=== Testing tick ===\n")

(let* ([duckie (make-duckie "Tester")]
       [canvas (make-canvas 10 10)]
       [particles (make-particle-system)]
       [state (make-loop-state duckie canvas 0 #t 'happy 0 particles)])
      
      (let* ([state1 (tick state)]
             [duckie1 (loop-state-duckie state1)])
            (assert-equal "tick increments frame" (loop-state-frame state1) 1)
            ;; Energy should NOT drain at frame 1 (only every 10)
            (assert-equal "tick energy no change" (duckie-energy duckie1) (duckie-energy duckie)))
      
      (let* ([state9 (loop-state-set-frame state 9)]
             [state10 (tick state9)]
             [duckie10 (loop-state-duckie state10)])
            (assert-equal "tick energy drains at frame 10" (duckie-energy duckie10) (- (duckie-energy duckie) 1))))

(doc 'section 'test-summary)

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

(= fail-count 0)
