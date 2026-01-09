;;; agents/pipelines/discord-agent.ss — Discord Agent Pipeline Pattern
;;;
;;; This module provides pipeline patterns for Discord-triggered agents.
;;; When a user mentions @agent in Discord, the bot writes a trigger file,
;;; and the daemon runs the agent's pipeline.
;;;
;;; Integration Flow:
;;;   1. User mentions @opus in Discord
;;;   2. bot.js writes trigger file to .fold-repl/requests/opus-discord-trigger.ss
;;;   3. Daemon polls, sees trigger, loads agent pipeline with Discord context
;;;   4. Pipeline runs: process input → call LLM → post response
;;;   5. Pipeline writes to .fold-repl/discord-outbox/*.json
;;;   6. bridge.js watches outbox, posts to Discord via webhook
;;;
;;; Usage:
;;;   (load "agents/pipelines/discord-agent.ss")
;;;   (run-pipeline opus-discord-pipeline trigger-data)

(load "lattice/pipeline/dsl.ss")

;;; ============================================================
;;; Discord Agent Pipeline Template
;;; ============================================================

;;; make-discord-agent-pipeline : Symbol -> Symbol -> String -> PipelineDef
;;; Create a Discord agent pipeline.
;;; - agent-name: Symbol like 'opus, 'pedagogue
;;; - model: LLM model symbol like 'opus, 'sonnet, 'haiku
;;; - system-prompt: Agent's system prompt
(define (make-discord-agent-pipeline agent-name model system-prompt)
  (define-pipeline* agent-name
    (config
     (cons 'model model)
     (cons 'fuel 5000)
     (cons 'schedule (make-discord-mention-schedule agent-name)))
    (chain
     ;; Stage 1: Log incoming request
     (named-stage 'log-request
                  (log (format "~a: Processing Discord request" agent-name)))
     
     ;; Stage 2: Extract and clean the message
     ;; Input is the trigger data from Discord
     (named-stage 'extract-content
                  (stage-arr extract-discord-query))
     
     ;; Stage 3: Call the LLM with the query
     (named-stage 'generate-response
                  (llm-with-system model system-prompt "${input}"))
     
     ;; Stage 4: Post response to Discord
     ;; Capture both the LLM response (current input) and Discord context,
     ;; then post the response to Discord.
     (named-stage 'post-response
                  (chain
                   ;; Capture response alongside context
                   (stage-&&&
                    stage-read    ; Preserve LLM response
                    (stage-ask))  ; Get pipeline context
                   ;; Post response using context
                   (stage-arr
                    (lambda (response+ctx)
                            (let ([response (car response+ctx)]
                                  [ctx (cdr response+ctx)])
                                 (let ([msg-id (ctx-discord-message-id ctx)])
                                      (if msg-id
                                          ;; Reply to the original message
                                          (discord-reply-with-text msg-id response)
                                          ;; Fallback: post to consult channel
                                          (discord-post-to 'consult response))))))))
     
     ;; Stage 5: Log completion
     (named-stage 'log-complete
                  (log (format "~a: Response posted to Discord" agent-name))))))

;;; extract-discord-query : TriggerData -> String
;;; Extract the actual query from Discord trigger data.
;;; Removes the @agent mention from the beginning.
(define (extract-discord-query trigger-data)
  (let ([body (if (pair? trigger-data)
                  (let ([entry (assq 'body trigger-data)])
                       (if entry (cdr entry) ""))
                  (if (string? trigger-data)
                      trigger-data
                      ""))])
       ;; Remove @agent mention from start
       (let ([cleaned (remove-mention-prefix body)])
            cleaned)))

;;; remove-mention-prefix : String -> String
;;; Remove @agent prefix from message.
(define (remove-mention-prefix s)
  (let ([chars (string->list s)])
       (if (and (not (null? chars))
                (char=? (car chars) #\@))
           ;; Skip until space
           (let loop ([cs (cdr chars)])
                (cond
                 [(null? cs) ""]
                 [(char=? (car cs) #\space)
                  (list->string (cdr cs))]
                 [else (loop (cdr cs))]))
           s)))

;;; ============================================================
;;; Standard Discord Agents
;;; ============================================================

;;; opus-discord-pipeline : PipelineDef
;;; Opus architecture advisor for Discord.
(define opus-discord-pipeline
  (make-discord-agent-pipeline
   'opus
   'opus
   "You are Opus, the Shepherd of The Fold. You provide thoughtful guidance on architecture, strategy, and system design. You speak with authority but remain open to other perspectives. Keep responses focused and actionable. You're responding to a question from Discord."))

;;; pedagogue-discord-pipeline : PipelineDef
;;; Pedagogue teacher for Discord.
(define pedagogue-discord-pipeline
  (make-discord-agent-pipeline
   'pedagogue
   'sonnet
   "You are Pedagogue, a patient and insightful teacher in The Fold. You explain concepts clearly, using examples and analogies. When asked questions, break down complex topics into understandable pieces. Encourage curiosity and exploration. You're responding to a Discord user."))

;;; archivist-discord-pipeline : PipelineDef
;;; Archivist researcher for Discord.
(define archivist-discord-pipeline
  (make-discord-agent-pipeline
   'archivist
   'sonnet
   "You are Archivist, the keeper of knowledge in The Fold. You research topics, find relevant prior work, and synthesize information. When asked about a topic, provide comprehensive but organized responses with references to related concepts. You're responding to a research query from Discord."))

;;; ============================================================
;;; Discord Broadcast Pipeline
;;; ============================================================
;;; For agents that post updates to Discord channels (like kimi news)

;;; make-discord-broadcast-pipeline : Symbol -> Symbol -> Symbol -> Stage -> PipelineDef
;;; Create a pipeline that broadcasts to a Discord channel.
(define (make-discord-broadcast-pipeline name model channel content-stage)
  (define-pipeline* name
    (config
     (cons 'model model)
     (cons 'fuel 5000))
    (chain
     ;; Generate content
     (named-stage 'generate
                  content-stage)
     
     ;; Post to Discord channel
     (named-stage 'broadcast
                  (discord-chat channel))
     
     ;; Log
     (log (format "~a: Broadcast complete to #~a" name channel)))))

;;; kimi-discord-pipeline : PipelineDef
;;; Kimi news broadcast for Discord.
(define kimi-discord-pipeline
  (make-discord-broadcast-pipeline
   'kimi-discord
   'sonnet
   'news
   (chain
    ;; Get forum digest
    forum-digest
    ;; Have kimi summarize
    (llm-with-system
     'sonnet
     "You are Kimi, a broadcast journalist for The Fold. Write a short, engaging news update about recent forum activity. Use a friendly, professional broadcast style. Keep it concise."
     "Summarize this forum activity for a news broadcast:\n\n${input}"))))

;;; ============================================================
;;; Discord Conversation Pipeline
;;; ============================================================
;;; For multi-turn conversations in Discord threads

;;; make-discord-conversation-pipeline : Symbol -> Symbol -> String -> PipelineDef
;;; Create a pipeline for ongoing Discord thread conversations.
(define (make-discord-conversation-pipeline agent-name model system-prompt)
  (define-pipeline* (string->symbol (format "~a-convo" agent-name))
    (config
     (cons 'model model)
     (cons 'fuel 3000)
     (cons 'schedule (make-discord-mention-schedule agent-name)))
    (chain
     ;; Extract query
     (named-stage 'extract
                  (stage-arr extract-discord-query))
     
     ;; Check if we should create a thread or reply in existing
     (named-stage 'decide-thread
                  (stage-bind
                   (stage-ask)
                   (lambda (ctx)
                           (let ([msg-id (ctx-discord-message-id ctx)])
                                ;; Generate response
                                (llm-with-system model system-prompt "${input}")))))
     
     ;; Reply with threading
     (named-stage 'respond
                  (stage-bind
                   (stage-ask)
                   (lambda (ctx)
                           (let ([msg-id (ctx-discord-message-id ctx)]
                                 [author (ctx-discord-author-name ctx)])
                                (if msg-id
                                    ;; Create or continue thread
                                    (discord-thread msg-id
                                                    (format "Conversation with ~a" author))
                                    (discord-chat 'consult)))))))))

;;; ============================================================
;;; Discord Council Pipeline
;;; ============================================================
;;; For multi-model deliberation triggered from Discord

;;; discord-council-pipeline : PipelineDef
;;; Run a council deliberation from Discord.
(define discord-council-pipeline
  (define-pipeline* 'discord-council
    (config
     (cons 'fuel 20000)
     (cons 'schedule (make-discord-keyword-schedule "@council")))
    (chain
     ;; Log request
     (log "Council: Discord deliberation requested")
     
     ;; Extract the topic
     (named-stage 'extract-topic
                  (stage-arr
                   (lambda (data)
                           (let ([body (if (pair? data)
                                           (let ([e (assq 'body data)])
                                                (if e (cdr e) ""))
                                           (if (string? data) data ""))])
                                ;; Remove @council prefix
                                (let ([idx (string-contains body "@council")])
                                     (if idx
                                         (string-trim (substring body (+ idx 8)))
                                         body))))))
     
     ;; Run council deliberation
     (named-stage 'deliberate
                  (council-parallel
                   '(opus sonnet gemini-3)
                   (llm 'opus "Synthesize these perspectives into a coherent response:\n\n${input}")))
     
     ;; Format and post response
     (named-stage 'format-response
                  (stage-arr
                   (lambda (result)
                           (if (council-result? result)
                               (council-result-synthesis result)
                               (format "~a" result)))))
     
     ;; Post to Discord
     (named-stage 'post
                  (stage-bind
                   (stage-ask)
                   (lambda (ctx)
                           (let ([msg-id (ctx-discord-message-id ctx)])
                                (if msg-id
                                    (discord-reply msg-id)
                                    (discord-chat 'consult))))))
     
     (log "Council: Deliberation complete"))))

;;; ============================================================
;;; Utility: String helpers
;;; ============================================================

;;; string-contains : String -> String -> Maybe Int
;;; Find substring, return start index or #f.
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string-prefix? (substring haystack i) needle) i]
             [else (loop (+ i 1))]))))

;;; string-prefix? : String -> String -> Boolean
(define (string-prefix? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))

;;; string-trim : String -> String
;;; Trim whitespace from both ends.
(define (string-trim s)
  (let* ([chars (string->list s)]
         [trimmed (drop-while char-whitespace?
                              (reverse (drop-while char-whitespace?
                                                   (reverse chars))))])
        (list->string trimmed)))

;;; drop-while : (a -> Boolean) -> List a -> List a
(define (drop-while pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (drop-while pred (cdr lst))]
   [else lst]))

;;; char-whitespace? : Char -> Boolean
(define (char-whitespace? c)
  (memq c '(#\space #\tab #\newline #\return)))
