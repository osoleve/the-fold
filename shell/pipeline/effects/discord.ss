;;; shell/pipeline/effects/discord.ss — Discord Effect Handler
;;;
;;; Handles Discord integration via outbox queue.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")

;;; ============================================================
;;; Discord Configuration
;;; ============================================================

;;; Discord outbox path relative to project root
(define *discord-outbox-dir* ".fold-repl/discord-outbox")

;;; ============================================================
;;; Discord Effect Interpretation
;;; ============================================================

;;; interpret-discord-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-discord-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(post)
              (let* ([channel (cadr payload)]
                     [title (caddr payload)]
                     [body (cadddr payload)]
                     [expanded-body (expand-template-with-ctx body ctx input)])
                    (discord-queue-post channel
                                        title
                                        expanded-body
                                        ctx)
                    (cons (stage-ok '()) state))]
             [(post-embed)
              (let* ([channel (cadr payload)]
                     [embed-spec (caddr payload)])
                    (discord-queue-embed channel embed-spec ctx)
                    (cons (stage-ok '()) state))]
             [(chat)
              (let* ([channel (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-post channel #f body ctx)
                    (cons (stage-ok '()) state))]
             [(reply)
              (let* ([message-id (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-reply message-id body ctx)
                    (cons (stage-ok '()) state))]
             [(react)
              (let* ([message-id (cadr payload)]
                     [emoji (caddr payload)])
                    (discord-queue-react message-id emoji ctx)
                    (cons (stage-ok '()) state))]
             [(thread)
              (let* ([message-id (cadr payload)]
                     [thread-name (caddr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-thread message-id thread-name body ctx)
                    ;; Thread creation returns thread-id (mock for now)
                    (cons (stage-ok (format "thread-~a" message-id)) state))]
             [(dm)
              (let* ([user-id (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-dm user-id body ctx)
                    (cons (stage-ok '()) state))]
             [else
              (cons (stage-err 'unknown-discord-op
                               (format "Unknown Discord operation: ~a" op)
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
;;; Discord Queue Helpers
;;; ============================================================
;;; These write JSON files to the outbox for bridge.js to pick up.

;;; discord-queue-post : Symbol -> Maybe String -> String -> Context -> ()
;;; Queue a post (titled or chat) for Discord.
(define (discord-queue-post channel title body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [tier (get-agent-tier ctx)]
         [post-data `((channel . ,(symbol->string channel))
                      (title . ,title)
                      (body . ,body)
                      (author . ,author)
                      (tier . ,tier)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-embed : Symbol -> Alist -> Context -> ()
;;; Queue an embed post for Discord.
(define (discord-queue-embed channel embed-spec ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [tier (get-agent-tier ctx)]
         [post-data `((channel . ,(symbol->string channel))
                      (embed . ,embed-spec)
                      (author . ,author)
                      (tier . ,tier)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-reply : String -> String -> Context -> ()
;;; Queue a reply to a Discord message.
(define (discord-queue-reply message-id body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((reply_to . ,message-id)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-react : String -> String -> Context -> ()
;;; Queue a reaction to a Discord message.
(define (discord-queue-react message-id emoji ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [post-data `((react_to . ,message-id)
                      (emoji . ,emoji)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-thread : String -> String -> String -> Context -> ()
;;; Queue thread creation from a message.
(define (discord-queue-thread message-id thread-name body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((create_thread_from . ,message-id)
                      (thread_name . ,thread-name)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-dm : String -> String -> Context -> ()
;;; Queue a direct message.
(define (discord-queue-dm user-id body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((dm_to . ,user-id)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; make-outbox-filename : -> String
;;; Generate unique filename for outbox JSON.
(define (make-outbox-filename)
  (let ([timestamp (current-milliseconds)]
        [random-suffix (random 100000)])
       (format "~a/~a-~a.json" *discord-outbox-dir* timestamp random-suffix)))

;;; write-outbox-json : String -> Alist -> ()
;;; Write alist as JSON to outbox file.
(define (write-outbox-json path data)
  ;; Ensure outbox directory exists
  (let ([dir (path-directory path)])
       (unless (file-exists? dir)
               (make-directories dir)))
  ;; Write JSON
  (with-output-to-file path
                       (lambda ()
                               (display (alist->json data)))))

;;; ============================================================
;;; JSON Serialization
;;; ============================================================

;;; alist->json : Alist -> String
;;; Convert alist to JSON string (simple implementation).
(define (alist->json alist)
  (string-append
   "{\n"
   (apply string-append
          (intersperse
           ",\n"
           (map (lambda (pair)
                        (format "  ~s: ~a"
                                (symbol->string (car pair))
                                (json-value (cdr pair))))
                alist)))
   "\n}"))

;;; json-value : Any -> String
;;; Convert value to JSON representation.
(define (json-value v)
  (cond
   [(string? v) (format "~s" v)]
   [(number? v) (format "~a" v)]
   [(boolean? v) (if v "true" "false")]
   [(null? v) "null"]
   [(symbol? v) (format "~s" (symbol->string v))]
   [(pair? v)
    (if (and (pair? (car v)) (symbol? (caar v)))
        ;; Nested alist
        (alist->json v)
        ;; List/array
        (string-append
         "["
         (apply string-append
                (intersperse ", " (map json-value v)))
         "]"))]
   [else (format "~s" (format "~a" v))]))

;;; intersperse : String -> List String -> List String
(define (intersperse sep lst)
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst)
               (cons sep (intersperse sep (cdr lst))))]))

;;; ============================================================
;;; Context Accessors
;;; ============================================================

;;; get-agent-name : Context -> String
;;; Get agent name from context.
(define (get-agent-name ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (persona-name persona)
           "pipeline")))

;;; get-agent-tier : Context -> String
;;; Get agent tier from context.
(define (get-agent-tier ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (symbol->string (persona-tier persona))
           "builder")))

;;; ============================================================
;;; Time Utilities
;;; ============================================================

;;; current-iso-timestamp : -> String
;;; Get current time in ISO 8601 format.
(define (current-iso-timestamp)
  (let* ([t (current-time)]
         [d (time-utc->date t)])
        (format "~a-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                (date-year d)
                (date-month d)
                (date-day d)
                (date-hour d)
                (date-minute d)
                (date-second d))))

;;; current-milliseconds : -> Integer
;;; Get current time in milliseconds since epoch.
(define (current-milliseconds)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

;;; ============================================================
;;; Path Utilities
;;; ============================================================

;;; path-directory : String -> String
;;; Get directory portion of path.
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String -> Char -> Maybe Integer
;;; Find last occurrence of char in string.
(define (string-rindex str ch)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))

;;; make-directories : String -> ()
;;; Create directory and all parent directories.
(define (make-directories path)
  (ensure-directory! path))

;;; ensure-directory! : String -> ()
;;; Create directory and all parent directories if they don't exist.
(define (ensure-directory! path)
  (unless (file-exists? path)
          (let ([parent (path-directory path)])
               (when (and (not (string=? parent "."))
                          (not (string=? parent path)))
                     (ensure-directory! parent)))
          (guard (ex [else (void)])  ; Ignore if already exists
                 (mkdir path))))
