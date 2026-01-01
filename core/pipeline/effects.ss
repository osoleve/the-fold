;;; fabric/stitches/pipeline/effects.ss — Pipeline Effect Definitions
;;;
;;; Effects are staged operations that require interpretation by the shell.
;;; This module defines the effect types; thimble/pipeline/interpreter.ss
;;; provides the actual implementations.
;;;
;;; This is Core code: pure, describes effects without executing them.
;;;
;;; Effect Categories:
;;;   - LLM: Call language models with prompts
;;;   - Fold: Execute Fold/Scheme expressions
;;;   - Shell: Run shell commands
;;;   - Store: Read/write content-addressed storage
;;;   - Log: Emit log entries
;;;   - Checkpoint: Save/restore pipeline state
;;;   - HTTP: Fetch URLs (for external APIs)
;;;   - Await: Wait for external signals
;;;
;;; Dependencies:
;;;   - pipeline/stage.ss

(load "core/pipeline/stage.ss")

;;; ============================================================
;;; Effect Construction Helpers
;;; ============================================================

;;; effect : Symbol -> Any -> Stage ctx i o
;;; Create an effect stage with type and payload.
(define (effect type payload)
  (make-effect-stage type payload))

;;; ============================================================
;;; LLM Effects
;;; ============================================================

;;; llm : Symbol -> String -> Stage ctx String String
;;; Call a language model with a prompt.
;;; Model symbols: 'opus, 'sonnet, 'haiku, 'gemini-3, 'kimi, etc.
(define (llm model prompt-template)
  (effect 'llm
          (list 'call model prompt-template)))

;;; llm-with-system : Symbol -> String -> String -> Stage ctx String String
;;; Call LLM with both system and user prompts.
(define (llm-with-system model system-prompt user-prompt)
  (effect 'llm
          (list 'call-with-system model system-prompt user-prompt)))

;;; llm-json : Symbol -> String -> Stage ctx String Any
;;; Call LLM expecting JSON response.
(define (llm-json model prompt-template)
  (effect 'llm
          (list 'call-json model prompt-template)))

;;; llm-structured : Symbol -> String -> Any -> Stage ctx String Any
;;; Call LLM with schema for structured output.
(define (llm-structured model prompt-template schema)
  (effect 'llm
          (list 'call-structured model prompt-template schema)))

;;; ============================================================
;;; Fold Effects
;;; ============================================================

;;; fold-eval : String -> Stage ctx i Any
;;; Evaluate a Fold expression.
(define (fold-eval expr-template)
  (effect 'fold
          (list 'eval expr-template)))

;;; fold-call : Symbol -> List Any -> Stage ctx i Any
;;; Call a Fold function by name with arguments.
(define (fold-call fn-name args)
  (effect 'fold
          (list 'call fn-name args)))

;;; fold-load : String -> Stage ctx i ()
;;; Load a Fold file.
(define (fold-load path)
  (effect 'fold
          (list 'load path)))

;;; forum-post : Symbol -> String -> String -> Stage ctx i Hash
;;; Post to forum channel. Returns post hash.
(define (forum-post channel title body)
  (effect 'fold
          (list 'forum-post channel title body)))

;;; forum-digest : Stage ctx i String
;;; Get forum digest.
(define forum-digest
  (effect 'fold
          (list 'call 'digest-posts '())))

;;; forum-browse : Symbol -> Nat -> Stage ctx i String
;;; Browse a channel.
(define (forum-browse channel n)
  (effect 'fold
          (list 'call 'browse (list channel n))))

;;; ============================================================
;;; Shell Effects
;;; ============================================================

;;; shell : String -> Stage ctx i String
;;; Run a shell command, return stdout.
(define (shell cmd-template)
  (effect 'shell
          (list 'run cmd-template)))

;;; shell-with-stdin : String -> Stage ctx String String
;;; Run shell command with input piped to stdin.
(define (shell-with-stdin cmd-template)
  (effect 'shell
          (list 'run-stdin cmd-template)))

;;; shell-env : Alist (String . String) -> String -> Stage ctx i String
;;; Run shell command with environment variables.
(define (shell-env env-vars cmd-template)
  (effect 'shell
          (list 'run-env env-vars cmd-template)))

;;; shell-bg : String -> Stage ctx i Symbol
;;; Run shell command in background, return job id.
(define (shell-bg cmd-template)
  (effect 'shell
          (list 'run-bg cmd-template)))

;;; ============================================================
;;; Store Effects (Content-Addressed Storage)
;;; ============================================================

;;; store-put : Stage ctx Any Hash
;;; Store value in CAS, return hash.
(define store-put
  (effect 'store
          (list 'put)))

;;; store-get : Hash -> Stage ctx i Any
;;; Retrieve value from CAS by hash.
(define (store-get hash)
  (effect 'store
          (list 'get hash)))

;;; store-has : Hash -> Stage ctx i Boolean
;;; Check if hash exists in store.
(define (store-has hash)
  (effect 'store
          (list 'has hash)))

;;; store-pin : Hash -> Stage ctx i ()
;;; Pin a hash to prevent garbage collection.
(define (store-pin hash)
  (effect 'store
          (list 'pin hash)))

;;; ============================================================
;;; Log Effects
;;; ============================================================

;;; log-info : String -> Stage ctx i i
;;; Log info message, pass input through.
(define (log-info message)
  (effect 'log
          (list 'info message)))

;;; log-warn : String -> Stage ctx i i
;;; Log warning.
(define (log-warn message)
  (effect 'log
          (list 'warn message)))

;;; log-error : String -> Stage ctx i i
;;; Log error.
(define (log-error message)
  (effect 'log
          (list 'error message)))

;;; log-debug : String -> Stage ctx i i
;;; Log debug message.
(define (log-debug message)
  (effect 'log
          (list 'debug message)))

;;; log-metric : Symbol -> Number -> Stage ctx i i
;;; Log a metric.
(define (log-metric name value)
  (effect 'log
          (list 'metric name value)))

;;; ============================================================
;;; Checkpoint Effects
;;; ============================================================

;;; checkpoint : Symbol -> Stage ctx a a
;;; Save current value as named checkpoint.
(define (checkpoint name)
  (effect 'checkpoint
          (list 'save name)))

;;; checkpoint-with : Symbol -> Any -> Stage ctx i i
;;; Save specific value as checkpoint.
(define (checkpoint-with name value)
  (effect 'checkpoint
          (list 'save-value name value)))

;;; restore : Symbol -> Stage ctx i Any
;;; Restore from named checkpoint.
(define (restore name)
  (effect 'checkpoint
          (list 'restore name)))

;;; checkpoint-exists : Symbol -> Stage ctx i Boolean
;;; Check if checkpoint exists.
(define (checkpoint-exists name)
  (effect 'checkpoint
          (list 'exists name)))

;;; checkpoint-clear : Symbol -> Stage ctx i ()
;;; Clear a checkpoint.
(define (checkpoint-clear name)
  (effect 'checkpoint
          (list 'clear name)))

;;; ============================================================
;;; HTTP Effects
;;; ============================================================

;;; http-get : String -> Stage ctx i String
;;; GET request, return body.
(define (http-get url-template)
  (effect 'http
          (list 'get url-template)))

;;; http-post : String -> Stage ctx String String
;;; POST request with body from input.
(define (http-post url-template)
  (effect 'http
          (list 'post url-template)))

;;; http-json : String -> Stage ctx i Any
;;; GET and parse JSON.
(define (http-json url-template)
  (effect 'http
          (list 'get-json url-template)))

;;; http-with-headers : Alist (String . String) -> String -> Stage ctx i String
;;; GET with custom headers.
(define (http-with-headers headers url-template)
  (effect 'http
          (list 'get-headers headers url-template)))

;;; ============================================================
;;; Await Effects (External Signals)
;;; ============================================================

;;; await-tag : String -> Stage ctx i String
;;; Wait for a forum post with specific tag pattern.
(define (await-tag tag-pattern)
  (effect 'await
          (list 'forum-tag tag-pattern)))

;;; await-file : String -> Stage ctx i String
;;; Wait for a file to exist, return contents.
(define (await-file path)
  (effect 'await
          (list 'file path)))

;;; await-timeout : Nat -> Stage ctx i () -> Stage ctx i ()
;;; Wait for duration (milliseconds).
(define (await-timeout ms)
  (effect 'await
          (list 'timeout ms)))

;;; await-signal : Symbol -> Stage ctx i Any
;;; Wait for named signal.
(define (await-signal name)
  (effect 'await
          (list 'signal name)))

;;; ============================================================
;;; Beads Integration Effects
;;; ============================================================

;;; beads-create : String -> Stage ctx i String
;;; Create a bead issue, return ID.
(define (beads-create title)
  (effect 'beads
          (list 'create title)))

;;; beads-create-full : String -> String -> Symbol -> Nat -> Stage ctx i String
;;; Create bead with full details.
(define (beads-create-full title description type priority)
  (effect 'beads
          (list 'create-full title description type priority)))

;;; beads-update : String -> Alist -> Stage ctx i ()
;;; Update a bead by ID.
(define (beads-update id updates)
  (effect 'beads
          (list 'update id updates)))

;;; beads-close : String -> Stage ctx i ()
;;; Close a bead.
(define (beads-close id)
  (effect 'beads
          (list 'close id)))

;;; beads-ready : Stage ctx i (List Bead)
;;; Get ready (unblocked) beads.
(define beads-ready
  (effect 'beads
          (list 'ready)))

;;; beads-show : String -> Stage ctx i Bead
;;; Get bead details.
(define (beads-show id)
  (effect 'beads
          (list 'show id)))

;;; ============================================================
;;; Git Effects
;;; ============================================================

;;; git-status : Stage ctx i String
;;; Get git status.
(define git-status
  (effect 'git
          (list 'status)))

;;; git-diff : Stage ctx i String
;;; Get git diff.
(define git-diff
  (effect 'git
          (list 'diff)))

;;; git-commit : String -> Stage ctx i String
;;; Commit with message, return hash.
(define (git-commit message)
  (effect 'git
          (list 'commit message)))

;;; git-push : Stage ctx i ()
;;; Push to remote.
(define git-push
  (effect 'git
          (list 'push)))

;;; ============================================================
;;; Sub-Pipeline Effects
;;; ============================================================

;;; invoke-pipeline : Pipeline -> Stage ctx i o
;;; Invoke another pipeline as a stage.
(define (invoke-pipeline pipeline-or-name)
  (effect 'pipeline
          (list 'invoke pipeline-or-name)))

;;; invoke-with-fuel : Nat -> Pipeline -> Stage ctx i o
;;; Invoke pipeline with specific fuel limit.
(define (invoke-with-fuel fuel pipeline-or-name)
  (effect 'pipeline
          (list 'invoke-fuel fuel pipeline-or-name)))

;;; spawn-pipeline : Pipeline -> Stage ctx i Symbol
;;; Spawn pipeline asynchronously, return run ID.
(define (spawn-pipeline pipeline-or-name)
  (effect 'pipeline
          (list 'spawn pipeline-or-name)))

;;; await-pipeline : Symbol -> Stage ctx i Any
;;; Wait for spawned pipeline to complete.
(define (await-pipeline run-id)
  (effect 'pipeline
          (list 'await run-id)))

;;; ============================================================
;;; Template Expansion
;;; ============================================================
;;;
;;; Many effects take "templates" - strings with ${...} placeholders.
;;; The interpreter expands these against:
;;;   - ${input} - current stage input
;;;   - ${ctx.field} - context fields
;;;   - ${env.var} - environment variables
;;;   - ${checkpoint.name} - checkpoint values

;;; expand-template : String -> Alist -> String
;;; Expand template with bindings (pure helper).
(define (expand-template template bindings)
  (let loop ([chars (string->list template)]
             [result '()]
             [in-var #f]
             [var-chars '()])
       (cond
        [(null? chars)
         (list->string (reverse result))]
        [(and (not in-var) (char=? (car chars) #\$)
              (not (null? (cdr chars)))
              (char=? (cadr chars) #\{))
         (loop (cddr chars) result #t '())]
        [(and in-var (char=? (car chars) #\}))
         (let* ([var-name (list->string (reverse var-chars))]
                [value (assoc var-name bindings)])
               (if value
                   (loop (cdr chars)
                         (append (reverse (string->list (format "~a" (cdr value))))
                                 result)
                         #f '())
                   (loop (cdr chars)
                         (append (reverse (string->list (string-append "${" var-name "}")))
                                 result)
                         #f '())))]
        [in-var
         (loop (cdr chars) result #t (cons (car chars) var-chars))]
        [else
         (loop (cdr chars) (cons (car chars) result) #f '())])))

;;; ============================================================
;;; Effect Predicates
;;; ============================================================

;;; llm-effect? : Effect -> Boolean
(define (llm-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'llm)))

;;; fold-effect? : Effect -> Boolean
(define (fold-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'fold)))

;;; shell-effect? : Effect -> Boolean
(define (shell-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'shell)))

;;; store-effect? : Effect -> Boolean
(define (store-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'store)))

;;; log-effect? : Effect -> Boolean
(define (log-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'log)))

;;; checkpoint-effect? : Effect -> Boolean
(define (checkpoint-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'checkpoint)))

;;; http-effect? : Effect -> Boolean
(define (http-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'http)))

;;; await-effect? : Effect -> Boolean
(define (await-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'await)))

;;; beads-effect? : Effect -> Boolean
(define (beads-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'beads)))

;;; git-effect? : Effect -> Boolean
(define (git-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'git)))

;;; pipeline-effect? : Effect -> Boolean
(define (pipeline-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'pipeline)))

;;; ============================================================
;;; Discord Effects
;;; ============================================================
;;;
;;; Discord integration effects. These write to the discord-outbox
;;; for the bridge.js watcher to pick up and post to Discord.
;;;
;;; The bot/bridge architecture:
;;;   - Discord bot watches messages, logs to Fold via REPL IPC
;;;   - Fold agents write to .fold-repl/discord-outbox/*.json
;;;   - Bridge.js watches outbox, posts to Discord via webhooks

;;; discord-post : Symbol -> String -> String -> Stage ctx i ()
;;; Post a titled message to a Discord channel.
;;; Channel: 'engineering, 'philosophy, 'design, 'art, 'poetry,
;;;          'requests, 'wishlist, 'chat, 'news, 'bugs, 'arena, 'consult
(define (discord-post channel title body)
  (effect 'discord
          (list 'post channel title body)))

;;; discord-post-embed : Symbol -> Alist -> Stage ctx i ()
;;; Post an embed with full control over fields.
;;; Embed alist: ((title . "...") (description . "...") (color . 0x...)
;;;               (fields . ((name . "...") (value . "...") ...)))
(define (discord-post-embed channel embed-spec)
  (effect 'discord
          (list 'post-embed channel embed-spec)))

;;; discord-chat : Symbol -> Stage ctx String ()
;;; Post input as chat message (no title, plain text).
(define (discord-chat channel)
  (effect 'discord
          (list 'chat channel)))

;;; discord-reply : String -> Stage ctx String ()
;;; Reply to a specific Discord message ID.
(define (discord-reply message-id)
  (effect 'discord
          (list 'reply message-id)))

;;; discord-react : String -> String -> Stage ctx i ()
;;; Add emoji reaction to a Discord message.
(define (discord-react message-id emoji)
  (effect 'discord
          (list 'react message-id emoji)))

;;; discord-thread : String -> String -> Stage ctx String ()
;;; Create a thread from a message, return thread ID.
;;; Input becomes the first message in the thread.
(define (discord-thread message-id thread-name)
  (effect 'discord
          (list 'thread message-id thread-name)))

;;; discord-dm : String -> Stage ctx String ()
;;; Send direct message to user (by Discord user ID).
(define (discord-dm user-id)
  (effect 'discord
          (list 'dm user-id)))

;;; ============================================================
;;; Discord Await Effects
;;; ============================================================

;;; await-discord-mention : Symbol -> Stage ctx i DiscordContext
;;; Wait for an @agent mention in Discord.
;;; Returns context with message-id, channel, author, body.
(define (await-discord-mention agent-name)
  (effect 'await
          (list 'discord-mention agent-name)))

;;; await-discord-reaction : String -> String -> Stage ctx i String
;;; Wait for a specific reaction on a message.
(define (await-discord-reaction message-id emoji)
  (effect 'await
          (list 'discord-reaction message-id emoji)))

;;; await-discord-reply : String -> Stage ctx i DiscordContext
;;; Wait for a reply to a specific message.
(define (await-discord-reply message-id)
  (effect 'await
          (list 'discord-reply message-id)))

;;; ============================================================
;;; Discord Effect Predicate
;;; ============================================================

;;; discord-effect? : Effect -> Boolean
(define (discord-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'discord)))
