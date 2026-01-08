;;; shell/pipeline/effects/http.ss — HTTP Effect Handler
;;;
;;; Handles HTTP requests using curl.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")
(load "shell/pipeline/effects/shell.ss")

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

;;; ============================================================
;;; JSON Parsing (Minimal - shared with LLM)
;;; ============================================================
;;; Note: This duplicates code from llm.ss. In a future refactor,
;;; this should be extracted to a shared json.ss module.

;;; parse-json-string : String -> Any
(define (parse-json-string s)
  (guard (ex [else #f])
         (if (or (not s) (string=? s ""))
             #f
             (let ([trimmed (string-trim s)])
                  (if (string=? trimmed "")
                      #f
                      (let-values ([(result rest) (parse-json-value trimmed 0)])
                                  result))))))

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

(define (skip-whitespace s pos)
  (let ([len (string-length s)])
       (let loop ([i pos])
            (if (and (< i len) (char-whitespace? (string-ref s i)))
                (loop (+ i 1))
                i))))

(define (parse-json-object s pos)
  (let ([pos (+ pos 1)])
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

(define (parse-json-array s pos)
  (let ([pos (+ pos 1)])
       (let loop ([pos (skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\]))
                (values (reverse result) (+ pos 1))
                (let-values ([(val pos2) (parse-json-value s pos)])
                            (let ([pos3 (skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\,))
                                     (loop (skip-whitespace s (+ pos3 1)) (cons val result))
                                     (loop pos3 (cons val result)))))))))

(define (parse-json-string-value s pos)
  (let ([pos (+ pos 1)])
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

(define (parse-json-true s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "true"))
      (values #t (+ pos 4))
      (values #f pos)))

(define (parse-json-false s pos)
  (if (and (<= (+ pos 5) (string-length s))
           (string=? (substring s pos (+ pos 5)) "false"))
      (values #f (+ pos 5))
      (values #f pos)))

(define (parse-json-null s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "null"))
      (values '() (+ pos 4))
      (values #f pos)))
