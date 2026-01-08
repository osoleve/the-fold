;;; shell/pipeline/effects/http.ss — HTTP Effect Handler
;;;
;;; Handles HTTP requests using curl.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")
(load "shell/pipeline/effects/shell.ss")
(load "shell/json.ss")

;;; ============================================================
;;; HTTP Effect Interpretation
;;; ============================================================

;;; interpret-http-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-http-effect payload ctx state input)
  (let ([op (car payload)]
        [url-template (cadr payload)])
       (case op
             [(get)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (cons (stage-ok (http-result-body result)) state)
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [(get-json)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (let ([parsed (parse-json-string (http-result-body result))])
                             (if parsed
                                 (cons (stage-ok parsed) state)
                                 (cons (stage-err 'json-parse-error
                                                  "Failed to parse HTTP response as JSON"
                                                  (http-result-body result))
                                       state)))
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-http-op
                               (format "Unknown HTTP op: ~a" op)
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

;;; ============================================================
;;; HTTP Implementation
;;; ============================================================

;;; http-fetch-get : String -> HTTPResult
;;; Fetch content from a URL using curl.
;;; Returns HTTPResult with body on success, error on failure.
(define (http-fetch-get url)
  (guard (ex [else
              (list 'http-result #f #f
                    (format "http-fetch-get error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Use curl with silent mode and fail on HTTP errors
         (let ([result (shell-exec (format "curl -sS -f ~s" url))])
              (if (shell-result-ok? result)
                  (list 'http-result #t (shell-result-stdout result) #f)
                  (list 'http-result #f #f (shell-result-stderr result))))))

(define (http-result-ok? r) (list-ref r 1))
(define (http-result-body r) (list-ref r 2))
(define (http-result-error r) (list-ref r 3))
