;;; boundary/pipeline/effects/misc.ss — Miscellaneous Effect Handlers
;;;
;;; Handles log, store, bbs, git, and await effects.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")
(load "boundary/pipeline/effects/shell.ss")
(load "boundary/bbs/bbs.ss")

;;; ====
;;; Configuration
;;; ====

;;; *pipeline-log-dir* : String
;;; Directory for pipeline logs.
(define *pipeline-log-dir* "logs/pipelines")

;;; *cas-store-dir* : String
;;; Directory for content-addressed storage.
(define *cas-store-dir* ".store")

;;; ====
;;; Log Effect Interpretation
;;; ====

;;; interpret-log-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-log-effect payload ctx state input)
  (let ([level (car payload)]
        [message (cadr payload)])
       (let* ([expanded (expand-template-with-ctx message ctx input)]
              [entry (make-log-entry level expanded input)]
              [new-state (state-add-log state entry)])
             ;; Also write to pipeline log file
             (write-pipeline-log entry)
             (cons (stage-ok input) new-state))))

;;; write-pipeline-log : LogEntry -> ()
;;; Append a log entry to the pipeline log file.
(define (write-pipeline-log entry)
  (guard (ex [else (void)])  ; Silently fail if logging fails
         (ensure-directory! *pipeline-log-dir*)
         (let* ([timestamp (get-timestamp)]
                [log-file (string-append *pipeline-log-dir* "/pipeline.log")]
                [level (log-entry-level entry)]
                [message (log-entry-message entry)])
               (call-with-output-file log-file
                                      (lambda (p)
                                              (display (format "[~a] [~a] ~a\n" timestamp level message) p))
                                      '(append)))))

;;; get-timestamp : -> String
;;; Get current time as a human-readable timestamp.
(define (get-timestamp)
  (let* ([t (current-time)]
         [seconds (time-second t)])
        ;; Return as ISO-style timestamp
        (format "~a" seconds)))

;;; ====
;;; Store (CAS) Effect Interpretation
;;; ====

;;; interpret-store-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-store-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(put)
              ;; Store input in CAS, return hash
              (guard (ex [else
                          (cons (stage-err 'store-error
                                           (format "Failed to store: ~a"
                                                   (if (message-condition? ex)
                                                       (condition-message ex)
                                                       "unknown error"))
                                           input)
                                state)])
                     (let ([hash (cas-persist input)])
                          (cons (stage-ok hash) state)))]
             [(get)
              (let* ([hash (cadr payload)]
                     [value (cas-load hash)])
                    (if value
                        (cons (stage-ok value) state)
                        (cons (stage-err 'store-not-found
                                         (format "Hash not found: ~a" hash)
                                         hash)
                              state)))]
             [(has)
              (let ([hash (cadr payload)])
                   (cons (stage-ok (cas-has? hash)) state))]
             [(pin)
              ;; Pin is a no-op for now (no garbage collection)
              (cons (stage-ok '()) state)]
             [else
              (cons (stage-err 'unknown-store-op
                               (format "Unknown store op: ~a" op)
                               payload)
                    state)])))

;;; cas-hash : Any -> String
;;; Compute a hash of a value for CAS storage.
;;; Uses a simple string hash for now.
(define (cas-hash value)
  (let* ([str (format "~s" value)]
         [hash (simple-string-hash str)])
        (format "~16,'0x" hash)))

;;; simple-string-hash : String -> Integer
;;; A simple hash function for strings.
(define (simple-string-hash str)
  (let ([len (string-length str)])
       (let loop ([i 0]
                  [hash 5381])
            (if (>= i len)
                (modulo hash (expt 2 64))
                (loop (+ i 1)
                      (+ (* hash 33) (char->integer (string-ref str i))))))))

;;; cas-path-for-hash : String -> String
;;; Get the file path for a hash in the CAS.
(define (cas-path-for-hash hash)
  (let ([prefix (substring hash 0 2)])
       (string-append *cas-store-dir* "/" prefix "/" hash)))

;;; cas-persist : Any -> String
;;; Store a value in CAS and return its hash.
(define (cas-persist value)
  (let* ([hash (cas-hash value)]
         [path (cas-path-for-hash hash)]
         [dir (path-directory path)])
        (ensure-directory! dir)
        (call-with-output-file path
                               (lambda (p)
                                       (pretty-print value p)))
        hash))

;;; cas-load : String -> Any | #f
;;; Load a value from CAS by hash.
(define (cas-load hash)
  (let ([path (cas-path-for-hash hash)])
       (if (file-exists? path)
           (guard (ex [else #f])
                  (call-with-input-file path read))
           #f)))

;;; cas-has? : String -> Boolean
;;; Check if a hash exists in CAS.
(define (cas-has? hash)
  (file-exists? (cas-path-for-hash hash)))

;;; cas-get : String -> Any | #f
;;; Alias for cas-load.
(define (cas-get hash)
  (cas-load hash))

;;; ====
;;; BBS Effect Interpretation
;;; ====

;;; *valid-bbs-types* : List of Symbol
;;; Allowed values for issue type.
(define *valid-bbs-types* '(task bug feature epic chore))

;;; *valid-bbs-priorities* : List of Number
;;; Allowed values for priority (0-4).
(define *valid-bbs-priorities* '(0 1 2 3 4))

;;; safe-bbs-id? : String -> Boolean
;;; Check if a string is a valid BBS ID (alphanumeric and hyphens only).
(define (safe-bbs-id? id)
  (and (string? id)
       (> (string-length id) 0)
       (let loop ([i 0])
            (if (>= i (string-length id))
                #t
                (let ([c (string-ref id i)])
                     (if (or (char-alphabetic? c)
                             (char-numeric? c)
                             (char=? c #\-))
                         (loop (+ i 1))
                         #f))))))

;;; interpret-bbs-effect : Payload -> Context -> State -> Input -> (Result . State)
;;; Handle BBS (issue tracker) effects using native Scheme API.
(define (interpret-bbs-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(create)
              ;; (create title)
              (guard (ex [else (cons (stage-err 'bbs-error
                                                (format "BBS create failed: ~a" ex)
                                                payload)
                                     state)])
                (let* ([title (cadr payload)]
                       [id (bbs-create title)])
                      (cons (stage-ok id) state)))]

             [(create-full)
              ;; (create-full title description type priority)
              (let* ([title (cadr payload)]
                     [description (caddr payload)]
                     [type (cadddr payload)]
                     [type-sym (if (symbol? type) type (string->symbol type))]
                     [priority (list-ref payload 4)]
                     [priority-num (if (number? priority)
                                       priority
                                       (string->number priority))])
                    ;; Validate type and priority
                    (cond
                     [(not (memq type-sym *valid-bbs-types*))
                      (cons (stage-err 'bbs-error
                                       (format "Invalid issue type: ~a (allowed: ~a)"
                                               type-sym *valid-bbs-types*)
                                       payload)
                            state)]
                     [(not (memv priority-num *valid-bbs-priorities*))
                      (cons (stage-err 'bbs-error
                                       (format "Invalid priority: ~a (allowed: ~a)"
                                               priority-num *valid-bbs-priorities*)
                                       payload)
                            state)]
                     [else
                      (guard (ex [else (cons (stage-err 'bbs-error
                                                        (format "BBS create failed: ~a" ex)
                                                        payload)
                                             state)])
                        (let ([id (bbs-create title
                                              'description description
                                              'type type-sym
                                              'priority priority-num)])
                             (cons (stage-ok id) state)))]))]

             [(update)
              ;; (update id updates-alist)
              (let* ([id (if (symbol? (cadr payload))
                             (symbol->string (cadr payload))
                             (cadr payload))]
                     [updates (caddr payload)])
                    (if (not (safe-bbs-id? id))
                        (cons (stage-err 'bbs-error
                                         (format "Invalid issue ID format: ~a" id)
                                         payload)
                              state)
                        (guard (ex [else (cons (stage-err 'bbs-error
                                                          (format "BBS update failed: ~a" ex)
                                                          payload)
                                               state)])
                          ;; Apply updates from alist
                          (let ([status (assq 'status updates)]
                                [priority (assq 'priority updates)]
                                [title (assq 'title updates)]
                                [description (assq 'description updates)]
                                [labels (assq 'labels updates)])
                               (apply bbs-update id
                                      (append (if status (list 'status (cdr status)) '())
                                              (if priority (list 'priority (cdr priority)) '())
                                              (if title (list 'title (cdr title)) '())
                                              (if description (list 'description (cdr description)) '())
                                              (if labels (list 'labels (cdr labels)) '()))))
                          (cons (stage-ok '()) state))))]

             [(close)
              ;; (close id)
              (let* ([id (if (symbol? (cadr payload))
                             (symbol->string (cadr payload))
                             (cadr payload))])
                    (if (not (safe-bbs-id? id))
                        (cons (stage-err 'bbs-error
                                         (format "Invalid issue ID format: ~a" id)
                                         payload)
                              state)
                        (guard (ex [else (cons (stage-err 'bbs-error
                                                          (format "BBS close failed: ~a" ex)
                                                          payload)
                                               state)])
                          (bbs-close id)
                          (cons (stage-ok '()) state))))]

             [(ready)
              ;; (ready) - get list of unblocked issues
              (guard (ex [else (cons (stage-err 'bbs-error
                                                (format "BBS ready failed: ~a" ex)
                                                payload)
                                     state)])
                (let* ([ids (bbs-ready-issues)]
                       [issues (map (lambda (id)
                                      (let ([data (bbs-fetch-issue-data id)])
                                           (if data data `((id . ,id)))))
                                    ids)])
                      (cons (stage-ok issues) state)))]

             [(show)
              ;; (show id) - get issue details
              (let* ([id (if (symbol? (cadr payload))
                             (symbol->string (cadr payload))
                             (cadr payload))])
                    (if (not (safe-bbs-id? id))
                        (cons (stage-err 'bbs-error
                                         (format "Invalid issue ID format: ~a" id)
                                         payload)
                              state)
                        (let ([data (bbs-fetch-issue-data id)])
                             (if data
                                 (cons (stage-ok data) state)
                                 (cons (stage-err 'bbs-error
                                                  (format "Issue not found: ~a" id)
                                                  payload)
                                       state)))))]

             [else
              (cons (stage-err 'unknown-bbs-op
                               (format "Unknown BBS op: ~a" op)
                               payload)
                    state)])))

;;; Backwards compatibility alias
(define interpret-beads-effect interpret-bbs-effect)

;;; ====
;;; Git Effect Interpretation
;;; ====

;;; interpret-git-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-git-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(status)
              (let ([result (shell-exec "git status --porcelain")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok (shell-result-stdout result)) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [(diff)
              (let ([result (shell-exec "git diff")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok (shell-result-stdout result)) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [(commit)
              (let* ([message (cadr payload)]
                     [result (shell-exec (format "git add -A && git commit -m ~s" message))])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'git-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(push)
              (let ([result (shell-exec "git push")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok '()) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [else
              (cons (stage-err 'unknown-git-op
                               (format "Unknown git op: ~a" op)
                               payload)
                    state)])))

;;; ====
;;; Await Effect Interpretation
;;; ====

;;; interpret-await-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-await-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(timeout)
              ;; Pause execution for specified milliseconds
              ;; FIX: Split into seconds and nanoseconds to avoid overflow
              ;; (make-time requires nanoseconds < 1e9)
              (let* ([ms (cadr payload)]
                     [total-ns (* ms 1000000)]
                     [seconds (quotient total-ns 1000000000)]
                     [ns (remainder total-ns 1000000000)])
                    (sleep (make-time 'time-duration ns seconds))
                    (cons (stage-ok input) state))]
             [(forum-tag)
              ;; TODO: Poll forum for tag
              (cons (stage-await (cadr payload)) state)]
             [(file)
              ;; TODO: Wait for file
              (cons (stage-await (list 'file (cadr payload))) state)]
             [(signal)
              (cons (stage-await (list 'signal (cadr payload))) state)]
             ;; Discord await operations
             [(discord-mention)
              ;; Wait for @agent mention in Discord
              ;; The daemon polls for trigger files written by bot.js
              (let* ([agent-name (cadr payload)]
                     [trigger-pattern (format "~a-discord-trigger" agent-name)])
                    ;; Return await result for daemon to poll
                    (cons (stage-await (list 'discord-mention agent-name)) state))]
             [(discord-reaction)
              ;; Wait for specific reaction on a message
              (let ([message-id (cadr payload)]
                    [emoji (caddr payload)])
                   (cons (stage-await (list 'discord-reaction message-id emoji)) state))]
             [(discord-reply)
              ;; Wait for reply to a message
              (let ([message-id (cadr payload)])
                   (cons (stage-await (list 'discord-reply message-id)) state))]
             [else
              (cons (stage-err 'unknown-await-op
                               (format "Unknown await op: ~a" op)
                               payload)
                    state)])))

;;; ====
;;; Helper Functions
;;; ====

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

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

;;; ====
;;; JSON Parsing (for beads effect)
;;; ====

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
