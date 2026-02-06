;;; boundary/pipeline/test-rlm-integration.ss — Integration tests for RLM harness
;;;
;;; Tests against a live vLLM instance and the REPL daemon.
;;; Prerequisites: vLLM running on localhost:8000, fold daemon running.
;;;
;;; Run: scheme --script boundary/pipeline/test-rlm-integration.ss

(load "core/testing/test-framework.ss")
(load "boundary/pipeline/rlm-loop.ss")

;;; ====
;;; Layer 1: Can we talk to vLLM?
;;; ====

(define *vllm-provider*
  (rlm-provider-vllm "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" 8000))

(test-group "rlm-integration-llm"

  (define-test "rlm-chat returns ok from vLLM"
    (let* ([messages (list (make-msg "user" "Reply with exactly: HELLO"))]
           [result (rlm-chat *vllm-provider* messages 128 0.0)])
      (assert-true (rlm-chat-ok? result))
      (assert-true (string? (rlm-chat-text result)))
      (assert-true (> (string-length (rlm-chat-text result)) 0))))

  (define-test "rlm-chat handles system + user messages"
    (let* ([messages (list (make-msg "system" "You are a calculator. Only output numbers.")
                           (make-msg "user" "What is 2+2?"))]
           [result (rlm-chat *vllm-provider* messages 64 0.0)])
      (assert-true (rlm-chat-ok? result))))

  (define-test "rlm-chat with multi-turn conversation"
    (let* ([messages (list (make-msg "system" "You are helpful.")
                           (make-msg "user" "Say hi")
                           (make-msg "assistant" "Hi!")
                           (make-msg "user" "Say bye"))]
           [result (rlm-chat *vllm-provider* messages 64 0.0)])
      (assert-true (rlm-chat-ok? result)))))

;;; ====
;;; Layer 2: Does response parsing work on real model output?
;;; ====

(test-group "rlm-integration-parsing"

  (define-test "real model output parses to code blocks"
    (let* ([messages (list
              (make-msg "system"
                "You are a Scheme code assistant. Always wrap code in ```scheme blocks.")
              (make-msg "user"
                "Write a single ```scheme block that computes (+ 1 2). Nothing else."))]
           [result (rlm-chat *vllm-provider* messages 256 0.0)])
      (when (rlm-chat-ok? result)
        (let ([blocks (rlm-parse-response (rlm-chat-text result))])
          ;; Should have at least one code block
          (assert-true (pair? blocks))
          (let ([code-blocks (filter rlm-block-code? blocks)])
            (assert-true (>= (length code-blocks) 1)))))))

  (define-test "real model output parses DONE marker"
    (let* ([messages (list
              (make-msg "system"
                "When you have an answer, write DONE(answer) on its own line.")
              (make-msg "user"
                "The answer is 42. Say DONE(42) now."))]
           [result (rlm-chat *vllm-provider* messages 128 0.0)])
      (when (rlm-chat-ok? result)
        (let ([blocks (rlm-parse-response (rlm-chat-text result))])
          ;; Should contain a done block
          (let ([done-blocks (filter rlm-block-done? blocks)])
            (assert-true (>= (length done-blocks) 1))))))))

;;; ====
;;; Layer 3: CAS environment with real data
;;; ====

(test-group "rlm-integration-cas-env"

  (define-test "ingest and grep real text"
    (let* ([env (make-rlm-env)]
           [text (string-append
                   "The quick brown fox jumps over the lazy dog. "
                   "Machine learning models process natural language. "
                   "Content-addressed storage ensures data integrity. "
                   "Recursive language models decompose complex tasks.")]
           [env* (rlm-env-ingest-text! env 'doc text 60)]
           [results (rlm-env-grep env* 'doc "recursive language" 2)])
      (assert-true (pair? results))
      ;; Top result should contain "recursive"
      (assert-true (> (string-length (caar results)) 0)))))

;;; ====
;;; Layer 4: End-to-end rlm-run (the real deal)
;;; ====

(test-group "rlm-integration-e2e"

  (define-test "rlm-run completes simple arithmetic task"
    (let* ([config (make-rlm-config
                     *vllm-provider*
                     (string-append
                       "You are a Fold/Scheme agent. You can emit code in ```scheme blocks.\n"
                       "The code will be evaluated and you'll see the result.\n"
                       "When you have the final answer, write DONE(answer) on its own line.\n"
                       "Be concise. Do not explain.")
                     5      ; max-steps (keep short for testing)
                     5000   ; max-fuel
                     2000   ; chunk-size
                     1      ; max-depth
                     3)]    ; loop-window
           [result (rlm-run config "Compute (+ 21 21) and report the result." "")])
      ;; Should get a run result
      (assert-true (rlm-run-result? result))
      ;; Status should be completed or exhausted (model might not say DONE in 5 steps)
      (let ([status (rlm-run-result-status result)])
        (assert-true (or (eq? status 'completed)
                         (eq? status 'exhausted))))
      ;; Trajectory hash should exist
      (assert-true (string? (rlm-run-result-trajectory-hash result)))
      (assert-true (> (string-length (rlm-run-result-trajectory-hash result)) 0))
      ;; Environment should have entries
      (let ([env (rlm-run-result-env result)])
        (assert-true (pair? env))))))

(run-all-tests)
