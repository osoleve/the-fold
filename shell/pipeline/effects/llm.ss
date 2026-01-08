;;; shell/pipeline/effects/llm.ss — LLM Effect Handler
;;;
;;; Handles LLM API calls (Anthropic Claude).
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")

;;; ============================================================
;;; LLM Effect Interpretation
;;; ============================================================

;;; interpret-llm-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-llm-effect payload ctx state input)
  (let ([op (car payload)]
        [model (cadr payload)]
        [prompt-template (caddr payload)])
       (case op
             [(call)
              (let* ([prompt (expand-template-with-ctx prompt-template ctx input)]
                     [system-prompt (get-system-prompt ctx)]
                     [response (call-llm-api model system-prompt prompt)])
                    (if (llm-response-ok? response)
                        (let ([new-state (state-add-log state
                                                        (make-log-entry 'debug
                                                                        (format "LLM ~a called" model)
                                                                        '()))])
                             (cons (stage-ok (llm-response-text response)) new-state))
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [(call-with-system)
              (let* ([system-prompt (caddr payload)]
                     [user-prompt (cadddr payload)]
                     [expanded-user (expand-template-with-ctx user-prompt ctx input)]
                     [response (call-llm-api model system-prompt expanded-user)])
                    (if (llm-response-ok? response)
                        (cons (stage-ok (llm-response-text response)) state)
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [(call-json)
              (let* ([prompt (expand-template-with-ctx prompt-template ctx input)]
                     [system-prompt (string-append (get-system-prompt ctx)
                                                   "\n\nRespond with valid JSON only.")]
                     [response (call-llm-api model system-prompt prompt)])
                    (if (llm-response-ok? response)
                        (let ([parsed (parse-json-string (llm-response-text response))])
                             (if parsed
                                 (cons (stage-ok parsed) state)
                                 (cons (stage-err 'json-parse-error
                                                  "Failed to parse LLM response as JSON"
                                                  (llm-response-text response))
                                       state)))
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [else
              (cons (stage-err 'unknown-llm-op
                               (format "Unknown LLM operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

;;; get-system-prompt : Context -> String
(define (get-system-prompt ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (persona-system-prompt persona)
           "")))

;;; ============================================================
;;; LLM API Implementation
;;; ============================================================

;;; *llm-api-key-file* : String
;;; Path to file containing Anthropic API key
(define *llm-api-key-file* ".env.agents")

;;; get-anthropic-api-key : -> String | #f
;;; Read the Anthropic API key from .env.agents file.
(define (get-anthropic-api-key)
  (guard (ex [else #f])
         (if (file-exists? *llm-api-key-file*)
             (call-with-input-file *llm-api-key-file*
                                   (lambda (p)
                                           (let loop ()
                                                (let ([line (get-line p)])
                                                     (cond
                                                      [(eof-object? line) #f]
                                                      [(and (>= (string-length line) 18)
                                                            (string=? (substring line 0 18) "ANTHROPIC_API_KEY="))
                                                       (substring line 18 (string-length line))]
                                                      [else (loop)])))))
             #f)))

;;; model-symbol->api-model : Symbol -> String
;;; Convert model symbol to Anthropic API model ID.
(define (model-symbol->api-model sym)
  (case sym
        [(opus) "claude-opus-4-20250514"]
        [(sonnet) "claude-sonnet-4-20250514"]
        [(haiku) "claude-3-5-haiku-20241022"]
        [(claude-3-opus) "claude-3-opus-20240229"]
        [(claude-3-sonnet) "claude-3-5-sonnet-20241022"]
        [(claude-3-haiku) "claude-3-5-haiku-20241022"]
        [else (symbol->string sym)]))

;;; call-llm-api : Symbol -> String -> String -> LLMResponse
;;; Call the Anthropic Claude API with given model, system prompt, and user prompt.
;;; Returns LLMResponse: (llm-response ok? text error)
(define (call-llm-api model system-prompt user-prompt)
  (let ([api-key (get-anthropic-api-key)])
       (if (not api-key)
           (list 'llm-response #f #f "No ANTHROPIC_API_KEY found in .env.agents")
           (guard (ex [else
                       (list 'llm-response #f #f
                             (format "LLM API error: ~a"
                                     (if (message-condition? ex)
                                         (condition-message ex)
                                         "unknown error")))])
                  (let* ([api-model (model-symbol->api-model model)]
                         ;; Build JSON request body
                         [request-body (format "{\"model\": ~s, \"max_tokens\": 4096, \"system\": ~s, \"messages\": [{\"role\": \"user\", \"content\": ~s}]}"
                                               api-model
                                               (json-escape system-prompt)
                                               (json-escape user-prompt))]
                         ;; Call curl with the request
                         [result (shell-exec-with-stdin
                                  (format "curl -sS -X POST https://api.anthropic.com/v1/messages -H 'Content-Type: application/json' -H 'x-api-key: ~a' -H 'anthropic-version: 2023-06-01' -d @-"
                                          api-key)
                                  request-body)])
                        (if (shell-result-ok? result)
                            (let ([response-json (shell-result-stdout result)])
                                 ;; Extract the text from the response
                                 (let ([text (extract-llm-response-text response-json)])
                                      (if text
                                          (list 'llm-response #t text #f)
                                          ;; Try to extract error
                                          (let ([error (extract-llm-error response-json)])
                                               (list 'llm-response #f #f (or error "Failed to parse response"))))))
                            (list 'llm-response #f #f (shell-result-stderr result))))))))

;;; json-escape : String -> String
;;; Escape a string for JSON (handle quotes, newlines, backslashes).
(define (json-escape s)
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) result))]
                 [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) result))]
                 [(char=? c #\newline) (loop (cdr chars) (append (list #\n #\\) result))]
                 [(char=? c #\return) (loop (cdr chars) (append (list #\r #\\) result))]
                 [(char=? c #\tab) (loop (cdr chars) (append (list #\t #\\) result))]
                 [else (loop (cdr chars) (cons c result))])))))

;;; extract-llm-response-text : String -> String | #f
;;; Extract the text content from an Anthropic API response JSON.
;;; Looks for: "content": [{"type": "text", "text": "..."}]
(define (extract-llm-response-text json-str)
  (let ([text-start (string-search json-str "\"text\": \"")])
       (if text-start
           (let* ([content-start (+ text-start 9)]
                  [content-end (find-json-string-end json-str content-start)])
                 (if content-end
                     (json-unescape (substring json-str content-start content-end))
                     #f))
           #f)))

;;; extract-llm-error : String -> String | #f
;;; Extract error message from API response.
(define (extract-llm-error json-str)
  (let ([error-start (string-search json-str "\"message\": \"")])
       (if error-start
           (let* ([msg-start (+ error-start 12)]
                  [msg-end (find-json-string-end json-str msg-start)])
                 (if msg-end
                     (json-unescape (substring json-str msg-start msg-end))
                     #f))
           #f)))

;;; string-search : String -> String -> Integer | #f
;;; Find first occurrence of needle in haystack.
(define (string-search haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) i]
             [else (loop (+ i 1))]))))

;;; find-json-string-end : String -> Integer -> Integer | #f
;;; Find the end of a JSON string starting at pos (after opening quote).
;;; Handles escape sequences.
(define (find-json-string-end str start)
  (let ([len (string-length str)])
       (let loop ([i start])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) #\\)
              ;; Skip escaped character
              (loop (+ i 2))]
             [(char=? (string-ref str i) #\")
              i]
             [else (loop (+ i 1))]))))

;;; json-unescape : String -> String
;;; Unescape a JSON string (handle \\, \", \n, \r, \t).
(define (json-unescape s)
  (let ([len (string-length s)])
       (let loop ([i 0]
                  [result '()])
            (cond
             [(>= i len) (list->string (reverse result))]
             [(and (char=? (string-ref s i) #\\) (< (+ i 1) len))
              (let ([next (string-ref s (+ i 1))])
                   (case next
                         [(#\n) (loop (+ i 2) (cons #\newline result))]
                         [(#\r) (loop (+ i 2) (cons #\return result))]
                         [(#\t) (loop (+ i 2) (cons #\tab result))]
                         [(#\" #\\) (loop (+ i 2) (cons next result))]
                         [else (loop (+ i 2) (cons next result))]))]
             [else (loop (+ i 1) (cons (string-ref s i) result))]))))

(define (llm-response-ok? r) (list-ref r 1))
(define (llm-response-text r) (list-ref r 2))
(define (llm-response-error r) (list-ref r 3))

;;; ============================================================
;;; Shell Execution Dependency
;;; ============================================================

;;; These need to be provided by the shell execution module or loaded
;;; from shell/pipeline/effects/shell.ss

(define (shell-exec-with-stdin cmd stdin-content)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-stdin error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         (let-values ([(to-stdin from-stdout from-stderr process-id)
                       (open-process-ports cmd
                                           (buffer-mode block)
                                           (native-transcoder))])
                     ;; Write stdin content
                     (put-string to-stdin stdin-content)
                     (close-port to-stdin)
                     (let ([stdout-all (get-string-all from-stdout)]
                           [stderr-all (get-string-all from-stderr)])
                          (close-port from-stdout)
                          (close-port from-stderr)
                          (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                               (list 'shell-result
                                     (string=? stderr-str "")
                                     stdout-str
                                     stderr-str))))))

(define (shell-result-ok? r) (list-ref r 1))
(define (shell-result-stdout r) (list-ref r 2))
(define (shell-result-stderr r) (list-ref r 3))

;;; ============================================================
;;; JSON Parsing Dependency
;;; ============================================================

;;; parse-json-string : String -> Any
;;; Parse a JSON string into Scheme data structures.
;;; Objects become alists, arrays become lists.
(define (parse-json-string s)
  (guard (ex [else #f])
         (if (or (not s) (string=? s ""))
             #f
             (let ([trimmed (string-trim s)])
                  (if (string=? trimmed "")
                      #f
                      (let-values ([(result rest) (parse-json-value trimmed 0)])
                                  result))))))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace.
(define (string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len) (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i (- len 1)])
                   (if (and (>= i start) (char-whitespace? (string-ref s i)))
                       (loop (- i 1))
                       (+ i 1)))])
        (if (>= start end)
            ""
            (substring s start end))))

;;; parse-json-value : String -> Integer -> (Values Any Integer)
;;; Parse a JSON value starting at position, return value and end position.
(define (parse-json-value s pos)
  (let ([pos (skip-whitespace s pos)])
       (if (>= pos (string-length s))
           (values #f pos)
           (let ([c (string-ref s pos)])
                (cond
                 [(char=? c #\{) (parse-json-object s pos)]
                 [(char=? c #\[) (parse-json-array s pos)]
                 [(char=? c #\") (parse-json-string-value s pos)]
                 [(or (char=? c #\-) (char-numeric? c)) (parse-json-number s pos)]
                 [(char=? c #\t) (parse-json-true s pos)]
                 [(char=? c #\f) (parse-json-false s pos)]
                 [(char=? c #\n) (parse-json-null s pos)]
                 [else (values #f pos)])))))

;;; skip-whitespace : String -> Integer -> Integer
(define (skip-whitespace s pos)
  (let ([len (string-length s)])
       (let loop ([i pos])
            (if (and (< i len) (char-whitespace? (string-ref s i)))
                (loop (+ i 1))
                i))))

;;; parse-json-object : String -> Integer -> (Values Alist Integer)
(define (parse-json-object s pos)
  (let ([pos (+ pos 1)])  ; skip '{'
       (let loop ([pos (skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\}))
                (values (reverse result) (+ pos 1))
                (let-values ([(key pos2) (parse-json-string-value s pos)])
                            (let ([pos3 (skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\:))
                                     (let-values ([(val pos4) (parse-json-value s (+ pos3 1))])
                                                 (let ([pos5 (skip-whitespace s pos4)])
                                                      (if (and (< pos5 (string-length s)) (char=? (string-ref s pos5) #\,))
                                                          (loop (skip-whitespace s (+ pos5 1))
                                                                (cons (cons (string->symbol key) val) result))
                                                          (loop pos5 (cons (cons (string->symbol key) val) result)))))
                                     (values (reverse result) pos3))))))))

;;; parse-json-array : String -> Integer -> (Values List Integer)
(define (parse-json-array s pos)
  (let ([pos (+ pos 1)])  ; skip '['
       (let loop ([pos (skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\]))
                (values (reverse result) (+ pos 1))
                (let-values ([(val pos2) (parse-json-value s pos)])
                            (let ([pos3 (skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\,))
                                     (loop (skip-whitespace s (+ pos3 1)) (cons val result))
                                     (loop pos3 (cons val result)))))))))

;;; parse-json-string-value : String -> Integer -> (Values String Integer)
(define (parse-json-string-value s pos)
  (let ([pos (+ pos 1)])  ; skip opening quote
       (let loop ([i pos]
                  [chars '()])
            (if (>= i (string-length s))
                (values (list->string (reverse chars)) i)
                (let ([c (string-ref s i)])
                     (cond
                      [(char=? c #\")
                       (values (list->string (reverse chars)) (+ i 1))]
                      [(char=? c #\\)
                       (if (< (+ i 1) (string-length s))
                           (let ([next (string-ref s (+ i 1))])
                                (case next
                                      [(#\n) (loop (+ i 2) (cons #\newline chars))]
                                      [(#\r) (loop (+ i 2) (cons #\return chars))]
                                      [(#\t) (loop (+ i 2) (cons #\tab chars))]
                                      [(#\" #\\ #\/) (loop (+ i 2) (cons next chars))]
                                      [else (loop (+ i 2) (cons next chars))]))
                           (values (list->string (reverse chars)) i))]
                      [else (loop (+ i 1) (cons c chars))]))))))

;;; parse-json-number : String -> Integer -> (Values Number Integer)
(define (parse-json-number s pos)
  (let ([len (string-length s)])
       (let loop ([i pos]
                  [chars '()])
            (if (and (< i len)
                     (let ([c (string-ref s i)])
                          (or (char-numeric? c)
                              (char=? c #\-)
                              (char=? c #\+)
                              (char=? c #\.)
                              (char=? c #\e)
                              (char=? c #\E))))
                (loop (+ i 1) (cons (string-ref s i) chars))
                (values (string->number (list->string (reverse chars))) i)))))

;;; parse-json-true : String -> Integer -> (Values #t Integer)
(define (parse-json-true s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "true"))
      (values #t (+ pos 4))
      (values #f pos)))

;;; parse-json-false : String -> Integer -> (Values #f Integer)
(define (parse-json-false s pos)
  (if (and (<= (+ pos 5) (string-length s))
           (string=? (substring s pos (+ pos 5)) "false"))
      (values #f (+ pos 5))
      (values #f pos)))

;;; parse-json-null : String -> Integer -> (Values '() Integer)
(define (parse-json-null s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "null"))
      (values '() (+ pos 4))
      (values #f pos)))
