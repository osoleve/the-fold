;;; boundary/pipeline/rlm-client.ss — Provider-Agnostic LLM Client
;;;
;;; Wraps OpenAI-compatible /v1/chat/completions AND Anthropic /v1/messages
;;; endpoints behind a uniform (rlm-chat provider messages max-tokens temperature)
;;; interface. Reuses shell-exec-with-env-stdin for secure API key handling.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "boundary/io/json.ss")
(load "boundary/pipeline/effects/llm.ss")  ; for shell-exec-with-env-stdin, shell-result-*

(doc 'module 'rlm-client)
(doc 'description "Provider-agnostic LLM HTTP client for RLM harness")
(doc 'layer 'boundary)
(doc 'purity 'impure)
(doc 'dependencies '(boundary/io/json.ss boundary/pipeline/effects/llm.ss))

;;; Monotonic counter for unique temp file names (concurrent-safe).
(define *rlm-req-counter* 0)
(define (rlm-request-file!)
  (set! *rlm-req-counter* (+ *rlm-req-counter* 1))
  (format "/tmp/rlm-req-~a-~a.json"
          (get-process-id) *rlm-req-counter*))

;;; ====
;;; Provider Configuration (pure data)
;;; ====

;;; make-rlm-provider : String -> String -> String -> Symbol -> RlmProvider
;;;   endpoint   : full URL (e.g. "http://localhost:8000/v1/chat/completions")
;;;   model-id   : model string sent in request body
;;;   api-key-env : env var name to read key from .env.agents (or #f for no auth)
;;;   api-format  : 'openai or 'anthropic (determines request/response shape)
(define (make-rlm-provider endpoint model-id api-key-env api-format)
  (list 'rlm-provider endpoint model-id api-key-env api-format))

(define (rlm-provider? x)
  (and (pair? x) (eq? (car x) 'rlm-provider)))

(define (rlm-provider-endpoint p)    (list-ref p 1))
(define (rlm-provider-model-id p)    (list-ref p 2))
(define (rlm-provider-api-key-env p) (list-ref p 3))
(define (rlm-provider-api-format p)  (list-ref p 4))

;;; ====
;;; Pre-Built Providers
;;; ====

;;; rlm-provider-vllm : String -> Nat -> RlmProvider
;;; Local vLLM instance. No auth required.
(define (rlm-provider-vllm model-id port)
  (make-rlm-provider
    (format "http://localhost:~a/v1/chat/completions" port)
    model-id
    #f       ; no api key
    'openai))

;;; rlm-provider-openai : String -> RlmProvider
;;; OpenAI API. Reads OPENAI_API_KEY from .env.agents.
(define (rlm-provider-openai model-id)
  (make-rlm-provider
    "https://api.openai.com/v1/chat/completions"
    model-id
    "OPENAI_API_KEY"
    'openai))

;;; rlm-provider-anthropic : Symbol -> RlmProvider
;;; Anthropic API. Reads ANTHROPIC_API_KEY from .env.agents.
;;; model-sym: 'opus, 'sonnet, 'haiku (mapped to API model IDs)
(define (rlm-provider-anthropic model-sym)
  (let ([model-id (case model-sym
                    [(opus)   "claude-opus-4-6"]
                    [(sonnet) "claude-sonnet-4-5-20250929"]
                    [(haiku)  "claude-haiku-4-5-20251001"]
                    [else     (symbol->string model-sym)])])
    (make-rlm-provider
      "https://api.anthropic.com/v1/messages"
      model-id
      "ANTHROPIC_API_KEY"
      'anthropic)))

;;; ====
;;; Core Chat Function
;;; ====

;;; rlm-chat : RlmProvider -> (List Message) -> Nat -> Float -> (ok Text) | (err Code Message)
;;;
;;; messages: list of ((role . "user"|"assistant"|"system") (content . "..."))
;;; Returns: (list 'ok text) or (list 'err code message)
(define (rlm-chat provider messages max-tokens temperature)
  (guard (ex [else
              (list 'err 'exception
                    (format "rlm-chat error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
    (let* ([endpoint (rlm-provider-endpoint provider)]
           [model-id (rlm-provider-model-id provider)]
           [api-key-env (rlm-provider-api-key-env provider)]
           [api-format (rlm-provider-api-format provider)]
           [api-key (if api-key-env (read-env-key api-key-env) #f)])
      ;; Check auth if required
      (if (and api-key-env (not api-key))
          (list 'err 'no-api-key
                (format "No ~a found in .env.agents" api-key-env))
          (case api-format
            [(openai) (rlm-chat-openai endpoint model-id api-key
                                       messages max-tokens temperature)]
            [(anthropic) (rlm-chat-anthropic endpoint model-id api-key
                                             messages max-tokens temperature)]
            [else (list 'err 'unknown-format
                        (format "Unknown API format: ~a" api-format))])))))

;;; ====
;;; OpenAI-Compatible Implementation
;;; ====

(define (rlm-chat-openai endpoint model-id api-key messages max-tokens temperature)
  (let* ([request-body (json->string
                         `((model . ,model-id)
                           (messages . ,(map format-openai-message messages))
                           (max_tokens . ,max-tokens)
                           (temperature . ,temperature)
                           (chat_template_kwargs . ((enable_thinking . #f)))))]
         [auth-header (if api-key
                         (format "-H 'Authorization: Bearer '\"$RLM_API_KEY\"")
                         "")]
         ;; Write request body to temp file to avoid pipe fd accumulation
         ;; in long-running processes with many sequential subprocess calls.
         [request-file (rlm-request-file!)]
         [_ (call-with-output-file request-file
              (lambda (p) (display request-body p))
              'replace)]
         [cmd (format "curl -sS --max-time 300 -X POST ~a -H 'Content-Type: application/json' ~a -d @~a"
                      (shell-escape-url endpoint)
                      auth-header
                      request-file)]
         [env (if api-key `(("RLM_API_KEY" . ,api-key)) '())])
    (let ([result (shell-exec-with-env-no-stdin env cmd)])
      (delete-file request-file)
      (if (shell-result-ok? result)
          (parse-openai-response (shell-result-stdout result))
          (list 'err 'http-error (shell-result-stderr result))))))

(define (format-openai-message msg)
  `((role . ,(cdr (assq 'role msg)))
    (content . ,(cdr (assq 'content msg)))))

(define (parse-openai-response json-str)
  (let ([parsed (parse-json-string json-str)])
    (if (not parsed)
        (list 'err 'json-parse-error
              (format "Failed to parse response: ~a"
                      (if (> (string-length json-str) 200)
                          (string-append (substring json-str 0 200) "...")
                          json-str)))
        ;; Extract choices[0].message.content
        (let ([choices (assq 'choices parsed)])
          (if (and choices (pair? (cdr choices)) (pair? (cadr choices)))
              (let* ([first-choice (cadr choices)]
                     [message (assq 'message first-choice)])
                (if message
                    (let ([content (assq 'content (cdr message))])
                      (if content
                          (list 'ok (cdr content))
                          (list 'err 'no-content "No content in response message")))
                    (list 'err 'no-message "No message in response choice")))
              ;; Check for error response
              (let ([error (assq 'error parsed)])
                (if error
                    (let ([msg (assq 'message (cdr error))])
                      (list 'err 'api-error
                            (if msg (cdr msg) "Unknown API error")))
                    (list 'err 'unexpected-format
                          (format "Unexpected response format: ~a"
                                  (if (> (string-length json-str) 200)
                                      (string-append (substring json-str 0 200) "...")
                                      json-str))))))))))

;;; ====
;;; Anthropic Implementation
;;; ====

(define (rlm-chat-anthropic endpoint model-id api-key messages max-tokens temperature)
  (let* ([system-content (extract-system-messages messages)]
         [non-system (filter (lambda (m) (not (string=? (cdr (assq 'role m)) "system")))
                             messages)]
         [request-body (json->string
                         `((model . ,model-id)
                           (max_tokens . ,max-tokens)
                           ,@(if (string=? system-content "")
                                 '()
                                 `((system . ,system-content)))
                           (messages . ,(map format-openai-message non-system))))]
         [request-file (rlm-request-file!)]
         [_ (call-with-output-file request-file
              (lambda (p) (display request-body p))
              'replace)]
         [cmd (format "curl -sS --max-time 300 -X POST ~a -H 'Content-Type: application/json' -H 'x-api-key: '\"$RLM_API_KEY\" -H 'anthropic-version: 2023-06-01' -d @~a"
                      (shell-escape-url endpoint)
                      request-file)]
         [env `(("RLM_API_KEY" . ,api-key))]
         [result (shell-exec-with-env-no-stdin env cmd)])
    (delete-file request-file)
    (if (shell-result-ok? result)
        (parse-anthropic-response (shell-result-stdout result))
        (list 'err 'http-error (shell-result-stderr result)))))

(define (extract-system-messages messages)
  (let ([system-msgs (filter (lambda (m) (string=? (cdr (assq 'role m)) "system"))
                              messages)])
    (if (null? system-msgs)
        ""
        (apply string-append
               (map (lambda (m)
                      (let ([content (cdr (assq 'content m))])
                        (if (string=? content "") "" (string-append content "\n"))))
                    system-msgs)))))

(define (parse-anthropic-response json-str)
  (let ([parsed (parse-json-string json-str)])
    (if (not parsed)
        (list 'err 'json-parse-error
              (format "Failed to parse Anthropic response: ~a"
                      (if (> (string-length json-str) 200)
                          (string-append (substring json-str 0 200) "...")
                          json-str)))
        ;; Extract content[0].text
        (let ([content (assq 'content parsed)])
          (if (and content (pair? (cdr content)))
              (let ([first-block (cadr content)])
                (let ([text (assq 'text first-block)])
                  (if text
                      (list 'ok (cdr text))
                      (list 'err 'no-text "No text in content block"))))
              ;; Check for error
              (let ([error (assq 'error parsed)])
                (if error
                    (let ([msg (assq 'message (cdr error))])
                      (list 'err 'api-error
                            (if msg (cdr msg) "Unknown Anthropic API error")))
                    (list 'err 'unexpected-format
                          (format "Unexpected Anthropic response: ~a"
                                  (if (> (string-length json-str) 200)
                                      (string-append (substring json-str 0 200) "...")
                                      json-str))))))))))

;;; ====
;;; Utilities
;;; ====

;;; read-env-key : String -> String | #f
;;; Read a key from .env.agents file (KEY=value format)
(define (read-env-key key-name)
  (let ([prefix (string-append key-name "=")])
    (guard (ex [else #f])
      (if (file-exists? ".env.agents")
          (call-with-input-file ".env.agents"
            (lambda (p)
              (let loop ()
                (let ([line (get-line p)])
                  (cond
                    [(eof-object? line) #f]
                    [(and (>= (string-length line) (string-length prefix))
                          (string=? (substring line 0 (string-length prefix)) prefix))
                     (substring line (string-length prefix) (string-length line))]
                    [else (loop)])))))
          #f))))

;;; shell-escape-url : String -> String
;;; Single-quote a URL for shell safety
(define (shell-escape-url url)
  ;; Use single quotes to prevent shell expansion
  (string-append "'" (rlm-escape-single-quotes url) "'"))

(define (rlm-escape-single-quotes s)
  (let loop ([chars (string->list s)] [result '()])
    (cond
      [(null? chars) (list->string (reverse result))]
      [(char=? (car chars) #\')
       (loop (cdr chars) (append (reverse (string->list "'\\''")) result))]
      [else
       (loop (cdr chars) (cons (car chars) result))])))

;;; rlm-chat response predicates
(define (rlm-chat-ok? r) (eq? (car r) 'ok))
(define (rlm-chat-err? r) (eq? (car r) 'err))
(define (rlm-chat-text r) (cadr r))       ; for ok response
(define (rlm-chat-error-code r) (cadr r)) ; for err response
(define (rlm-chat-error-msg r) (caddr r)) ; for err response
