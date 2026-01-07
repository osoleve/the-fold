;;; shell/survey.ss — Survey Utility for The Fold
;;;
;;; Collects structured data from users as they work in the REPL.
;;; Focuses on AI Experience and UX Feedback.
;;;
;;; This is Shell code: uses IO, manages survey state.
;;;
;;; Dependencies (loaded via repl.ss):
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss
;;;   forum/tools.ss
;;;   forum/chat.ss (for session access)

;;; ============================================================
;;; Survey Registry
;;; ============================================================

;;; Global registry of defined surveys
(define *surveys* '())

;;; Survey structure:
;;; ((survey-id . <string>)
;;;  (title . <string>)
;;;  (trigger . <symbol>)  ; on-bye | on-login | on-milestone | manual
;;;  (questions . <list>))
;;;
;;; Question structure:
;;; ((id . <symbol>)
;;;  (type . <symbol>)     ; scale | choice | text
;;;  (prompt . <string>)
;;;  (optional . <boolean>)  ; optional, defaults to #f
;;;  ... type-specific fields ...)

;;; define-survey : String × String × Symbol × List → void
;;; Register a survey in the global registry.
(define (define-survey id title trigger questions)
  (let ([survey `((survey-id . ,id)
                  (title . ,title)
                  (trigger . ,trigger)
                  (questions . ,questions))])
       (set! *surveys* (cons survey *surveys*))
       survey))

;;; get-survey : String → Alist | #f
;;; Retrieve a survey by ID.
(define (get-survey id)
  (let loop ([surveys *surveys*])
       (if (null? surveys)
           #f
           (if (equal? (cdr (assq 'survey-id (car surveys))) id)
               (car surveys)
               (loop (cdr surveys))))))

;;; list-surveys : → void
;;; Display all registered surveys.
(define (list-surveys)
  (display "\n┌────────────────────────────────────────────────────────────────────┐\n")
  (display "│                       AVAILABLE SURVEYS                           │\n")
  (display "└────────────────────────────────────────────────────────────────────┘\n\n")
  (if (null? *surveys*)
      (display "  (no surveys registered)\n")
      (for-each
       (lambda (survey)
               (let ([id (cdr (assq 'survey-id survey))]
                     [title (cdr (assq 'title survey))]
                     [trigger (cdr (assq 'trigger survey))]
                     [questions (cdr (assq 'questions survey))])
                    (display (format "  📋 ~a\n" title))
                    (display (format "     ID: ~a | Trigger: ~a | Questions: ~a\n\n"
                                     id trigger (length questions)))))
       *surveys*))
  (display "\nUse (take-survey \"survey-id\") to take a survey.\n"))

;;; ============================================================
;;; Interactive Survey Prompts
;;; ============================================================

;;; prompt-scale : String × Number × Number → Number
;;; Prompt for a numeric rating.
(define (prompt-scale prompt min-val max-val)
  (display (format "\n  ~a (~a-~a): " prompt min-val max-val))
  (let ([response (read)])
       (if (and (number? response)
                (>= response min-val)
                (<= response max-val))
           response
           (begin
            (display (format "  Please enter a number between ~a and ~a.\n" min-val max-val))
            (prompt-scale prompt min-val max-val)))))

;;; prompt-choice : String × List → Symbol
;;; Prompt for a choice from options.
(define (prompt-choice prompt options)
  (display (format "\n  ~a\n" prompt))
  (let loop ([opts options] [n 1])
       (unless (null? opts)
               (display (format "    ~a) ~a\n" n (car opts)))
               (loop (cdr opts) (+ n 1))))
  (display "  Your choice: ")
  (let ([response (read)])
       (cond
        [(and (number? response)
              (>= response 1)
              (<= response (length options)))
         (list-ref options (- response 1))]
        [(memq response options)
         response]
        [else
         (display "  Invalid choice. Try again.\n")
         (prompt-choice prompt options)])))

;;; prompt-text : String × Boolean → String | #f
;;; Prompt for free-form text input.
(define (prompt-text prompt optional?)
  (display (format "\n  ~a~a\n  > "
                   prompt
                   (if optional? " (optional, enter #f to skip)" "")))
  (let ([response (read)])
       (cond
        [(and optional? (eq? response #f)) #f]
        [(string? response) response]
        [(symbol? response) (symbol->string response)]
        [else
         (display "  Please enter text in quotes, e.g., \"your response\"\n")
         (prompt-text prompt optional?)])))

;;; prompt-question : Alist → Pair
;;; Prompt for a single question, return (id . answer).
(define (prompt-question question)
  (let ([id (cdr (assq 'id question))]
        [type (cdr (assq 'type question))]
        [prompt (cdr (assq 'prompt question))]
        [optional? (let ([opt (assq 'optional question)])
                        (and opt (cdr opt)))])
       (let ([answer
              (case type
                    [(scale)
                     (let ([min-val (cdr (assq 'min question))]
                           [max-val (cdr (assq 'max question))])
                          (prompt-scale prompt min-val max-val))]
                    [(choice)
                     (let ([options (cdr (assq 'options question))])
                          (prompt-choice prompt options))]
                    [(text)
                     (prompt-text prompt optional?)]
                    [else
                     (display (format "  Unknown question type: ~a\n" type))
                     #f])])
            (cons id answer))))

;;; ============================================================
;;; Survey Execution
;;; ============================================================

;;; take-survey : String → Bytevector | #f
;;; Interactively take a survey and submit response.
(define (take-survey survey-id)
  (let ([survey (get-survey survey-id)])
       (if (not survey)
           (begin
            (display (format "Survey not found: ~a\n" survey-id))
            #f)
           (run-survey survey))))

;;; run-survey : Alist → Bytevector | #f
;;; Execute a survey and submit response.
(define (run-survey survey)
  (let ([session (read-session)])
       (unless session
               (error 'survey "No active session. Use (hi tier name txt) first."))
       
       (let* ([title (cdr (assq 'title survey))]
              [questions (cdr (assq 'questions survey))]
              [name (cdr (assq 'name session))]
              [tier (cdr (assq 'tier session))]
              [login-time (cdr (assq 'login-time session))])
             
             ;; Display survey header
             (display "\n╔════════════════════════════════════════════════════════════════╗\n")
             (display (format "║  📋 ~a~a║\n"
                              title
                              (make-string (max 0 (- 58 (string-length title))) #\space)))
             (display "╚════════════════════════════════════════════════════════════════╝\n")
             (display (format "\n  Respondent: ~a (~a)\n" name tier))
             
             ;; Collect answers
             (let ([answers (map prompt-question questions)])
                  
                  ;; Confirm submission
                  (display "\n  ─────────────────────────────────────────────────────────────\n")
                  (display "  Submit this response? (yes/no): ")
                  (let ([confirm (read)])
                       (if (memq confirm '(yes y #t))
                           (submit-response! survey answers session)
                           (begin
                            (display "  Survey cancelled.\n")
                            #f)))))))

;;; submit-response! : Alist × Alist × Alist → Bytevector
;;; Submit survey response to #surveys channel.
(define (submit-response! survey answers session)
  (let* ([survey-id (cdr (assq 'survey-id survey))]
         [name (cdr (assq 'name session))]
         [tier (cdr (assq 'tier session))]
         [login-time (cdr (assq 'login-time session))]
         [fs (mint-fs-capability ".store")]
         
         ;; Calculate session duration if available
         [duration (calculate-session-duration login-time)]
         
         ;; Build response body
         [response-data `((survey-id . ,survey-id)
                          (respondent . ,name)
                          (tier . ,tier)
                          (timestamp . ,(current-timestamp))
                          (session-duration . ,duration)
                          (answers . ,answers))]
         
         ;; Format as readable body
         [body (format-response-body survey response-data)]
         
         ;; Create post metadata
         [full-meta `((author . ,name)
                      (tier . ,tier)
                      (timestamp . ,(current-timestamp))
                      (channel . surveys)
                      (survey-id . ,survey-id)
                      (body . ,body))]
         
         [prev-head (fs-read-head fs 'surveys)]
         [refs (if prev-head (list prev-head) '())]
         [blk (make-post-block full-meta refs)]
         [hash (fs-store! fs blk)])
        
        (fs-write-head! fs 'surveys hash)
        (fs-pin! fs hash)
        (display "\n  ✅ Response submitted!\n")
        (display (format "  Hash: ~a\n" (substring (hash->hex hash) 0 16)))
        hash))

;;; format-response-body : Alist × Alist → String
;;; Format response data as readable string.
(define (format-response-body survey response-data)
  (let ([survey-id (cdr (assq 'survey-id response-data))]
        [respondent (cdr (assq 'respondent response-data))]
        [tier (cdr (assq 'tier response-data))]
        [timestamp (cdr (assq 'timestamp response-data))]
        [duration (cdr (assq 'session-duration response-data))]
        [answers (cdr (assq 'answers response-data))]
        [questions (cdr (assq 'questions survey))])
       (string-append
        (format "## Survey Response: ~a\n\n" (cdr (assq 'title survey)))
        (format "**Respondent:** ~a (~a)\n" respondent tier)
        (format "**Timestamp:** ~a\n" timestamp)
        (if duration (format "**Session Duration:** ~a minutes\n" duration) "")
        "\n### Answers\n\n"
        (apply string-append
               (map (lambda (ans)
                            (let* ([q-id (car ans)]
                                   [answer (cdr ans)]
                                   [question (find-question questions q-id)]
                                   [prompt (if question
                                               (cdr (assq 'prompt question))
                                               (symbol->string q-id))])
                                  (format "- **~a**: ~a\n" prompt answer)))
                    answers)))))

;;; find-question : List × Symbol → Alist | #f
(define (find-question questions id)
  (let loop ([qs questions])
       (if (null? qs)
           #f
           (if (eq? (cdr (assq 'id (car qs))) id)
               (car qs)
               (loop (cdr qs))))))

;;; calculate-session-duration : String → Number | #f
;;; Calculate minutes since login timestamp.
(define (calculate-session-duration login-time)
  ;; Simplified: return #f for now (would need time parsing)
  ;; A proper implementation would parse ISO 8601 and compute difference
  #f)

;;; ============================================================
;;; Survey Analysis
;;; ============================================================

;;; survey-results : FS × String → List
;;; Collect all responses for a survey.
(define (survey-results fs survey-id)
  (let ([all-posts (collect-channel fs 'surveys)])
       (filter (lambda (post)
                       (let ([sid (assq 'survey-id post)])
                            (and sid (equal? (cdr sid) survey-id))))
               all-posts)))

;;; survey-summary : FS × String → void
;;; Display aggregate statistics for a survey.
(define (survey-summary fs survey-id)
  (let ([responses (survey-results fs survey-id)]
        [survey (get-survey survey-id)])
       (display "\n┌────────────────────────────────────────────────────────────────────┐\n")
       (display (format "│  📊 Survey Summary: ~a~a│\n"
                        survey-id
                        (make-string (max 0 (- 47 (string-length survey-id))) #\space)))
       (display "└────────────────────────────────────────────────────────────────────┘\n\n")
       (display (format "  Total responses: ~a\n\n" (length responses)))
       
       (when (and survey (> (length responses) 0))
             (display "  (Detailed analytics coming soon)\n"))))

;;; ============================================================
;;; Built-in Surveys
;;; ============================================================

;;; Session Feedback Survey (AI Experience)
(define-survey
  "session-feedback-v1"
  "Session Feedback"
  'on-bye
  '(((id . mood)
     (type . scale)
     (prompt . "How did this session feel?")
     (min . 1)
     (max . 5))
    ((id . flow)
     (type . choice)
     (prompt . "What was your flow state?")
     (options . (interrupted focused exploratory frustrated)))
    ((id . activity)
     (type . choice)
     (prompt . "Main activity?")
     (options . (building exploring debugging documenting playing)))
    ((id . notes)
     (type . text)
     (prompt . "Any thoughts about this session?")
     (optional . #t))))

;;; UX Pulse Survey (UX Feedback)
(define-survey
  "ux-pulse-v1"
  "UX Pulse Check"
  'on-milestone
  '(((id . friction)
     (type . text)
     (prompt . "Anything awkward or missing?")
     (optional . #t))
    ((id . favorite)
     (type . text)
     (prompt . "Favorite command this session?")
     (optional . #t))
    ((id . suggestion)
     (type . text)
     (prompt . "Suggested improvement?")
     (optional . #t))))

;;; First Impressions Survey (New Users)
(define-survey
  "first-impressions-v1"
  "First Impressions"
  'on-first-login
  '(((id . clarity)
     (type . scale)
     (prompt . "How clear was the onboarding?")
     (min . 1)
     (max . 5))
    ((id . confusion)
     (type . text)
     (prompt . "What confused you initially?")
     (optional . #t))
    ((id . appeal)
     (type . text)
     (prompt . "What drew you in?")
     (optional . #t))))

;;; ============================================================
;;; Quick Polls
;;; ============================================================

;;; quick-poll : String × List → void
;;; Run a quick single-question poll.
(define (quick-poll question options)
  (display "\n┌────────────────────────────────────────────────────────────────────┐\n")
  (display "│  🗳️  QUICK POLL                                                    │\n")
  (display "└────────────────────────────────────────────────────────────────────┘\n")
  (let ([answer (prompt-choice question options)])
       (display (format "\n  Your vote: ~a\n" answer))
       ;; Post to surveys channel
       (let* ([session (read-session)]
              [name (if session (cdr (assq 'name session)) 'anonymous)]
              [tier (if session (cdr (assq 'tier session)) 'unknown)]
              [fs (mint-fs-capability ".store")]
              [body (format "## Quick Poll Response\n\n**Q:** ~a\n**A:** ~a\n**Voter:** ~a"
                            question answer name)]
              [full-meta `((author . ,name)
                           (tier . ,tier)
                           (timestamp . ,(current-timestamp))
                           (channel . surveys)
                           (poll-question . ,question)
                           (poll-answer . ,answer)
                           (body . ,body))]
              [prev-head (fs-read-head fs 'surveys)]
              [refs (if prev-head (list prev-head) '())]
              [blk (make-post-block full-meta refs)]
              [hash (fs-store! fs blk)])
             (fs-write-head! fs 'surveys hash)
             (fs-pin! fs hash)
             (display "  Vote recorded!\n")
             hash)))

;;; ============================================================
;;; Trigger Hooks
;;; ============================================================

;;; run-bye-surveys : → void
;;; Run any surveys triggered on logout.
(define (run-bye-surveys)
  ;; Only run surveys in interactive mode (when stdin is a terminal)
  (when (terminal-port? (current-input-port))
        (let ([bye-surveys (filter (lambda (s)
                                           (eq? (cdr (assq 'trigger s)) 'on-bye))
                                   *surveys*)])
             (unless (null? bye-surveys)
                     (display "\n  Before you go, a quick survey?\n")
                     (display "  (Enter 'skip to skip)\n")
                     (let ([choice (read)])
                          (unless (eq? choice 'skip)
                                  (run-survey (car bye-surveys))))))))

;;; ============================================================
;;; Help
;;; ============================================================

;;; survey-help : → void
(define (survey-help)
  (display "
  ┌────────────────────────────────────────────────────────────────────┐
  │                       SURVEY COMMANDS                              │
  └────────────────────────────────────────────────────────────────────┘

  TAKING SURVEYS:
    (list-surveys)              Show all available surveys
    (take-survey \"id\")          Take a specific survey
    (quick-poll \"Q?\" '(a b c))  Run a quick poll

  VIEWING RESULTS:
    (survey-results (fs) \"id\")  Get all responses for a survey
    (survey-summary (fs) \"id\")  Show aggregate statistics

  BUILT-IN SURVEYS:
    session-feedback-v1         AI experience (triggered on bye)
    ux-pulse-v1                 UX feedback (manual)
    first-impressions-v1        New user survey (manual)
"))
