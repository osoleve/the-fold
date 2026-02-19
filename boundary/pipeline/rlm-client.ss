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
;;; Persistent HTTP Relay
;;; ====
;;; Long-lived Python subprocess that maintains keep-alive HTTP sessions.
;;; Eliminates ~2-5s subprocess spawn overhead per LLM call.
;;; Falls back to curl subprocess if relay is unavailable.

(define *rlm-relay-to*   #f)  ; output port (we write to relay stdin)
(define *rlm-relay-from* #f)  ; input port  (we read from relay stdout)
(define *rlm-relay-err*  #f)  ; error port  (must keep ref to prevent GC/SIGPIPE)
(define *rlm-relay-pid*  #f)  ; process id

(define (rlm-relay-script-path)
  (string-append (current-directory) "/boundary/pipeline/http-relay.py"))

(define (rlm-relay-alive?)
  (and *rlm-relay-to* *rlm-relay-from*
       (not (port-closed? *rlm-relay-to*))
       (not (port-closed? *rlm-relay-from*))))

(define (rlm-relay-start!)
  (guard (ex [else
              (set! *rlm-relay-to* #f)
              (set! *rlm-relay-from* #f)
              (set! *rlm-relay-err* #f)
              (set! *rlm-relay-pid* #f)
              #f])
    (let-values ([(to-relay from-relay pid err-port)
                  (open-process-ports
                    (format "python3 ~a" (rlm-relay-script-path))
                    (buffer-mode line)
                    (native-transcoder))])
      ;; Cache ALL ports — GC collecting stderr closes the pipe fd,
      ;; sending SIGPIPE to the child process.
      (set! *rlm-relay-to*   to-relay)
      (set! *rlm-relay-from* from-relay)
      (set! *rlm-relay-err*  err-port)
      (set! *rlm-relay-pid*  pid)
      #t)))

(define (rlm-relay-ensure!)
  (unless (rlm-relay-alive?)
    (rlm-relay-stop!)  ; clean up any stale state
    (rlm-relay-start!)))

(define (rlm-relay-stop!)
  (when *rlm-relay-to*
    (guard (ex [else (void)])
      (close-port *rlm-relay-to*)))
  (set! *rlm-relay-to* #f)
  (set! *rlm-relay-from* #f)
  (set! *rlm-relay-err* #f)
  (set! *rlm-relay-pid* #f))

;;; rlm-relay-request! : String -> Alist -> Alist -> Nat -> (ok String) | (err Symbol String)
;;; Send a request through the persistent relay. Returns raw response body string.
(define (rlm-relay-request! url headers body timeout)
  (rlm-relay-ensure!)
  (if (not *rlm-relay-to*)
      (list 'err 'relay-unavailable "HTTP relay failed to start")
      (guard (ex [else
                  (rlm-relay-stop!)
                  (list 'err 'relay-error
                        (format "Relay error: ~a"
                                (if (message-condition? ex)
                                    (condition-message ex)
                                    "unknown")))])
        (let* ([req-obj `((url . ,url)
                          (headers . ,headers)
                          (body . ,body)
                          (timeout . ,timeout))]
               [req-line (json->string req-obj)])
          (display req-line *rlm-relay-to*)
          (newline *rlm-relay-to*)
          (flush-output-port *rlm-relay-to*)
          (let ([resp-line (get-line *rlm-relay-from*)])
            (cond
              [(eof-object? resp-line)
               (rlm-relay-stop!)
               (list 'err 'relay-eof "Relay process exited")]
              [else
               (rlm-parse-relay-response resp-line)]))))))

;;; Parse a JSON-lines response from the relay subprocess
(define (rlm-parse-relay-response resp-line)
  (let ([parsed (parse-json-string resp-line)])
    (cond
      [(not parsed)
       (list 'err 'relay-parse "Failed to parse relay response")]
      [else
       (let ([status (assq 'status parsed)]
             [resp-body (assq 'body parsed)]
             [err (assq 'error parsed)])
         (cond
           ;; Relay-level error (timeout, connection refused, etc.)
           [(and err (cdr err))
            (list 'err 'relay-upstream (cdr err))]
           ;; Success (2xx) with body
           [(and status (>= (cdr status) 200) (< (cdr status) 300) resp-body)
            (list 'ok (cdr resp-body))]
           ;; Non-2xx but has body (API error responses — let caller parse)
           [resp-body
            (list 'ok (cdr resp-body))]
           ;; No body at all
           [else
            (list 'err 'relay-http
                  (format "HTTP ~a, no body"
                          (if status (cdr status) "unknown")))]))])))


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
  (let* ([body `((model . ,model-id)
                 (messages . ,(map format-openai-message messages))
                 (max_tokens . ,max-tokens)
                 (temperature . ,temperature)
                 (chat_template_kwargs . ((enable_thinking . #f))))]
         [headers (if api-key
                      `((Authorization . ,(string-append "Bearer " api-key)))
                      '())]
         ;; Try persistent relay first
         [relay-result (rlm-relay-request! endpoint headers body 300)])
    (if (rlm-chat-ok? relay-result)
        ;; Relay succeeded — parse the response body
        (parse-openai-response (rlm-chat-text relay-result))
        ;; Relay failed — fall back to curl subprocess
        (rlm-chat-openai-curl endpoint model-id api-key
                              messages max-tokens temperature))))

;;; Curl fallback for when relay is unavailable
(define (rlm-chat-openai-curl endpoint model-id api-key
                               messages max-tokens temperature)
  (let* ([request-body (json->string
                         `((model . ,model-id)
                           (messages . ,(map format-openai-message messages))
                           (max_tokens . ,max-tokens)
                           (temperature . ,temperature)
                           (chat_template_kwargs . ((enable_thinking . #f)))))]
         [auth-header (if api-key
                         (format "-H 'Authorization: Bearer '\"$RLM_API_KEY\"")
                         "")]
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
  (unless (string? json-str)
    (set! json-str (format "~a" json-str)))
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
                    (let* ([content (assq 'content (cdr message))]
                           [content-val (and content (cdr content))]
                           ;; Some models (e.g. GPT-OSS-120B) put output in
                           ;; reasoning_content and leave content null.
                           [reasoning (assq 'reasoning_content (cdr message))]
                           [reasoning-val (and reasoning
                                              (string? (cdr reasoning))
                                              (cdr reasoning))])
                      (cond
                        [(and content-val (string? content-val))
                         (list 'ok content-val reasoning-val)]
                        [reasoning-val
                         (list 'ok reasoning-val #f)]
                        [else
                         (list 'err 'no-content "No content in response message")]))
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
         [body `((model . ,model-id)
                 (max_tokens . ,max-tokens)
                 ,@(if (string=? system-content "")
                       '()
                       `((system . ,system-content)))
                 (messages . ,(map format-openai-message non-system)))]
         [headers `((x-api-key . ,api-key)
                    (anthropic-version . "2023-06-01"))]
         ;; Try persistent relay first
         [relay-result (rlm-relay-request! endpoint headers body 300)])
    (if (rlm-chat-ok? relay-result)
        (parse-anthropic-response (rlm-chat-text relay-result))
        ;; Fall back to curl
        (rlm-chat-anthropic-curl endpoint model-id api-key
                                  messages max-tokens temperature))))

;;; Curl fallback for Anthropic
(define (rlm-chat-anthropic-curl endpoint model-id api-key
                                  messages max-tokens temperature)
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
(define (rlm-chat-reasoning r)            ; reasoning_content or #f
  (and (> (length r) 2) (caddr r)))
(define (rlm-chat-error-code r) (cadr r)) ; for err response
(define (rlm-chat-error-msg r) (caddr r)) ; for err response
