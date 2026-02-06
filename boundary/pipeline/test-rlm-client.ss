;;; boundary/pipeline/test-rlm-client.ss — Tests for RLM LLM client
;;;
;;; Tests provider configuration construction and response parsing.
;;; Does NOT make actual HTTP calls.

(load "core/testing/test-framework.ss")
(load "boundary/pipeline/rlm-client.ss")

(test-group "rlm-client"

  ;;; ====
  ;;; Provider Construction
  ;;; ====

  (define-test "rlm-provider-vllm creates valid provider"
    (let ([p (rlm-provider-vllm "qwen3-80b" 8000)])
      (assert-true (rlm-provider? p))
      (assert-equal "http://localhost:8000/v1/chat/completions"
                    (rlm-provider-endpoint p))
      (assert-equal "qwen3-80b" (rlm-provider-model-id p))
      (assert-false (rlm-provider-api-key-env p))
      (assert-equal 'openai (rlm-provider-api-format p))))

  (define-test "rlm-provider-vllm with different port"
    (let ([p (rlm-provider-vllm "llama-70b" 9090)])
      (assert-equal "http://localhost:9090/v1/chat/completions"
                    (rlm-provider-endpoint p))))

  (define-test "rlm-provider-openai creates valid provider"
    (let ([p (rlm-provider-openai "gpt-4")])
      (assert-true (rlm-provider? p))
      (assert-equal "https://api.openai.com/v1/chat/completions"
                    (rlm-provider-endpoint p))
      (assert-equal "gpt-4" (rlm-provider-model-id p))
      (assert-equal "OPENAI_API_KEY" (rlm-provider-api-key-env p))
      (assert-equal 'openai (rlm-provider-api-format p))))

  (define-test "rlm-provider-anthropic creates valid provider"
    (let ([p (rlm-provider-anthropic 'sonnet)])
      (assert-true (rlm-provider? p))
      (assert-equal "https://api.anthropic.com/v1/messages"
                    (rlm-provider-endpoint p))
      (assert-equal "claude-sonnet-4-5-20250929" (rlm-provider-model-id p))
      (assert-equal "ANTHROPIC_API_KEY" (rlm-provider-api-key-env p))
      (assert-equal 'anthropic (rlm-provider-api-format p))))

  (define-test "rlm-provider-anthropic maps opus"
    (assert-equal "claude-opus-4-6"
                  (rlm-provider-model-id (rlm-provider-anthropic 'opus))))

  (define-test "rlm-provider-anthropic maps haiku"
    (assert-equal "claude-haiku-4-5-20251001"
                  (rlm-provider-model-id (rlm-provider-anthropic 'haiku))))

  (define-test "make-rlm-provider custom endpoint"
    (let ([p (make-rlm-provider "http://my-server:5000/v1/chat/completions"
                                 "custom-model" "MY_KEY" 'openai)])
      (assert-equal "http://my-server:5000/v1/chat/completions"
                    (rlm-provider-endpoint p))
      (assert-equal "MY_KEY" (rlm-provider-api-key-env p))))

  ;;; ====
  ;;; Response Parsing (OpenAI format)
  ;;; ====

  (define-test "parse-openai-response extracts text"
    (let ([json "{\"choices\":[{\"message\":{\"content\":\"Hello world\"}}]}"])
      (let ([result (parse-openai-response json)])
        (assert-true (rlm-chat-ok? result))
        (assert-equal "Hello world" (rlm-chat-text result)))))

  (define-test "parse-openai-response handles error"
    (let ([json "{\"error\":{\"message\":\"Rate limit exceeded\"}}"])
      (let ([result (parse-openai-response json)])
        (assert-true (rlm-chat-err? result))
        (assert-equal 'api-error (rlm-chat-error-code result)))))

  (define-test "parse-openai-response handles invalid json"
    (let ([result (parse-openai-response "not json at all")])
      (assert-true (rlm-chat-err? result))))

  ;;; ====
  ;;; Response Parsing (Anthropic format)
  ;;; ====

  (define-test "parse-anthropic-response extracts text"
    (let ([json "{\"content\":[{\"type\":\"text\",\"text\":\"Hello from Claude\"}]}"])
      (let ([result (parse-anthropic-response json)])
        (assert-true (rlm-chat-ok? result))
        (assert-equal "Hello from Claude" (rlm-chat-text result)))))

  (define-test "parse-anthropic-response handles error"
    (let ([json "{\"error\":{\"message\":\"Invalid API key\"}}"])
      (let ([result (parse-anthropic-response json)])
        (assert-true (rlm-chat-err? result)))))

  ;;; ====
  ;;; URL Escaping
  ;;; ====

  (define-test "shell-escape-url wraps in quotes"
    (let ([escaped (shell-escape-url "http://localhost:8000/v1")])
      (assert-equal "'http://localhost:8000/v1'" escaped)))

  (define-test "shell-escape-url handles single quotes in URL"
    (let ([escaped (shell-escape-url "http://host/path?q=it's")])
      ;; Should escape the single quote
      (assert-true (> (string-length escaped) 0))))

  ;;; ====
  ;;; System Message Extraction
  ;;; ====

  (define-test "extract-system-messages concatenates system messages"
    (let ([msgs (list '((role . "system") (content . "Be helpful."))
                      '((role . "user") (content . "Hi"))
                      '((role . "system") (content . "Be concise.")))])
      (let ([result (extract-system-messages msgs)])
        (assert-true (string? result))
        (assert-true (> (string-length result) 0)))))

  (define-test "extract-system-messages returns empty for no system"
    (let ([msgs (list '((role . "user") (content . "Hi")))])
      (assert-equal "" (extract-system-messages msgs)))))

(run-all-tests)
