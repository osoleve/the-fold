(load "boundary/flashmob/triage.ss")
(load "boundary/flashmob/history.ss")

(doc 'module 'flashmob-main)
(doc 'description "Game-theoretic QA triage system for The Fold. Coordinates multiple agents to prioritize and attribute findings.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'usage "
  (load \"boundary/flashmob/flashmob.ss\")
  (flashmob-start '(\"file1.ss\" \"file2.ss\"))
  (flashmob-add-finding 'file \"file1.ss\" 'line 10 'severity 'high ...)
  (flashmob-triage)
  (flashmob-ranking)
  (flashmob-credits)
")

(doc 'section 'current-session-state)

(doc *flashmob-current-session* 'type '(U String #f))
(doc *flashmob-current-session* 'description "ID of the currently active session")
(define *flashmob-current-session* #f)

(doc *flashmob-session-agents* 'type '(List Symbol))
(doc *flashmob-session-agents* 'description "Agents participating in the current session")
(define *flashmob-session-agents* '())

(doc *flashmob-session-findings* 'type '(List Alist))
(doc *flashmob-session-findings* 'description "Findings collected in the current session (in memory for quick access)")
(define *flashmob-session-findings* '())

(doc *flashmob-session-triage* 'type '(U Alist #f))
(doc *flashmob-session-triage* 'description "Most recent triage result")
(define *flashmob-session-triage* #f)

(doc 'section 'session-management)

(doc flashmob-start 'type '(-> (List String) String))
(doc flashmob-start 'description "Start a new QA session for the given files. Returns the session ID.")
(define (flashmob-start files)
  (when *flashmob-current-session*
    (printf "Warning: Closing previous session ~a~n" *flashmob-current-session*))
  (let ([session-id (flashmob-create-session! files)])
    (set! *flashmob-current-session* session-id)
    (set! *flashmob-session-agents* '())
    (set! *flashmob-session-findings* '())
    (set! *flashmob-session-triage* #f)
    (printf "Started session: ~a~n" session-id)
    (printf "Files under review: ~a~n" files)
    session-id))

(doc flashmob-status 'type '(-> Void))
(doc flashmob-status 'description "Show current session status")
(define (flashmob-status)
  (if (not *flashmob-current-session*)
      (printf "No active session. Use (flashmob-start files) to begin.~n")
      (let ([stats (flashmob-session-stats *flashmob-current-session*)])
        (printf "~a~n" (make-string 60 #\-))
        (printf "Session:    ~a~n" *flashmob-current-session*)
        (printf "Status:     ~a~n" (if stats (cdr (assq 'status stats)) "unknown"))
        (printf "Files:      ~a~n" (if stats (cdr (assq 'files stats)) '()))
        (printf "Agents:     ~a (~a registered)~n"
                (length *flashmob-session-agents*) *flashmob-session-agents*)
        (printf "Findings:   ~a~n" (length *flashmob-session-findings*))
        (printf "Triaged:    ~a~n" (if *flashmob-session-triage* "yes" "no"))
        (when *flashmob-session-triage*
          (printf "Strategy:   ~a~n"
                  (cdr (assq 'strategy *flashmob-session-triage*)))
          (printf "Consensus:  ~a~n"
                  (flashmob-format-pct (cdr (assq 'consensus *flashmob-session-triage*)))))
        (printf "~a~n" (make-string 60 #\-)))))

(doc flashmob-stop 'type '(-> Void))
(doc flashmob-stop 'description "End the current session and persist final state")
(define (flashmob-stop)
  (if (not *flashmob-current-session*)
      (printf "No active session to stop.~n")
      (begin
        ;; Update session in CAS with final state
        (flashmob-persist-session!)
        (printf "Session ~a stopped.~n" *flashmob-current-session*)
        (set! *flashmob-current-session* #f)
        (set! *flashmob-session-agents* '())
        (set! *flashmob-session-findings* '())
        (set! *flashmob-session-triage* #f))))

(doc 'section 'finding-management)

(doc flashmob-add-finding 'type '(-> . args Bytevector))
(doc flashmob-add-finding 'description "Add a finding to the current session")
(doc flashmob-add-finding 'param "'file - File path (required)")
(doc flashmob-add-finding 'param "'line - Line number (required)")
(doc flashmob-add-finding 'param "'severity - 'critical | 'high | 'medium | 'low | 'info (required)")
(doc flashmob-add-finding 'param "'category - 'security | 'performance | 'correctness | 'style | 'documentation (required)")
(doc flashmob-add-finding 'param "'confidence - 0.0-1.0 (required)")
(doc flashmob-add-finding 'param "'agent - Agent ID that found this (required)")
(doc flashmob-add-finding 'param "'title - Short description (optional)")
(doc flashmob-add-finding 'param "'description - Detailed description (optional)")
(define (flashmob-add-finding . args)
  (unless *flashmob-current-session*
    (error 'flashmob-add-finding "No active session"))
  (let* ([file (fm-get-keyword args 'file #f)]
         [line (fm-get-keyword args 'line 0)]
         [severity (fm-get-keyword args 'severity 'medium)]
         [category (fm-get-keyword args 'category 'correctness)]
         [confidence (fm-get-keyword args 'confidence 0.5)]
         [agent-id (fm-get-keyword args 'agent 'general-agent)]
         [title (fm-get-keyword args 'title (format "Finding at ~a:~a" file line))]
         [description (fm-get-keyword args 'description "")])
    ;; Validate required fields
    (unless file
      (error 'flashmob-add-finding "Missing required 'file argument"))
    ;; Ensure agent is registered
    (unless (memq agent-id *flashmob-session-agents*)
      (flashmob-ensure-agent! agent-id)
      (set! *flashmob-session-agents* (cons agent-id *flashmob-session-agents*)))
    ;; Create finding block
    (let* ([session-hash (flashmob-read-head *flashmob-current-session*)]
           [blk (make-finding-block file line severity category confidence agent-id
                                    title description session-hash)]
           [hash (flashmob-store! blk)]
           [data (finding-block-data blk)])
      ;; Add to in-memory list
      (set! *flashmob-session-findings*
            (cons data *flashmob-session-findings*))
      (printf "Added finding: ~a [~a ~a]~n" title severity category)
      hash)))

(doc flashmob-list-findings 'type '(-> Void))
(doc flashmob-list-findings 'description "List all findings in current session")
(define (flashmob-list-findings)
  (if (null? *flashmob-session-findings*)
      (printf "No findings yet.~n")
      (begin
        (printf "~a findings:~n" (length *flashmob-session-findings*))
        (printf "~a~n" (make-string 60 #\-))
        (for-each
         (lambda (f)
           (printf "~a:~a  [~a ~a]  ~a~n"
                   (cdr (assq 'file f))
                   (cdr (assq 'line f))
                   (cdr (assq 'severity f))
                   (cdr (assq 'category f))
                   (fm-truncate (cdr (assq 'title f)) 40)))
         *flashmob-session-findings*))))

(doc flashmob-show-finding 'type '(-> (U Int Bytevector) Void))
(doc flashmob-show-finding 'description "Show details of a specific finding")
(define (flashmob-show-finding id-or-hash)
  (let ([finding (if (number? id-or-hash)
                    (and (< id-or-hash (length *flashmob-session-findings*))
                         (list-ref *flashmob-session-findings* id-or-hash))
                    (flashmob-fetch-finding-data id-or-hash))])
    (if (not finding)
        (printf "Finding not found.~n")
        (begin
          (printf "~a~n" (make-string 60 #\-))
          (printf "File:        ~a~n" (cdr (assq 'file finding)))
          (printf "Line:        ~a~n" (cdr (assq 'line finding)))
          (printf "Severity:    ~a~n" (cdr (assq 'severity finding)))
          (printf "Category:    ~a~n" (cdr (assq 'category finding)))
          (printf "Confidence:  ~a~n" (cdr (assq 'confidence finding)))
          (printf "Agent:       ~a~n" (cdr (assq 'agent-id finding)))
          (printf "Title:       ~a~n" (cdr (assq 'title finding)))
          (let ([desc (cdr (assq 'description finding))])
            (unless (string=? desc "")
              (printf "~nDescription:~n~a~n" desc)))
          (printf "~a~n" (make-string 60 #\-))))))

(doc 'section 'triage-operations)

(doc flashmob-triage 'type '(-> . args Alist))
(doc flashmob-triage 'description "Run triage on current session")
(doc flashmob-triage 'param "'strategy - 'simple | 'game (default: auto-recommended)")
(doc flashmob-triage 'param "'k - Number of findings to select (default: 10)")
(define (flashmob-triage . args)
  (unless *flashmob-current-session*
    (error 'flashmob-triage "No active session"))
  (when (null? *flashmob-session-findings*)
    (error 'flashmob-triage "No findings to triage"))
  (let* ([strategy (fm-get-keyword args 'strategy #f)]
         [k (fm-get-keyword args 'k 10)]
         [effective-strategy (or strategy
                                 (flashmob-recommend-strategy
                                  *flashmob-session-agents*
                                  *flashmob-session-findings*))]
         [result (flashmob-triage-run *flashmob-session-agents*
                                      *flashmob-session-findings*
                                      'strategy effective-strategy
                                      'k k)])
    (set! *flashmob-session-triage* result)
    ;; Save triage to CAS
    (flashmob-save-triage! result)
    (printf "Triage complete (~a strategy)~n" effective-strategy)
    (printf "Consensus: ~a~n" (flashmob-format-pct (cdr (assq 'consensus result))))
    result))

(doc flashmob-ranking 'type '(-> Void))
(doc flashmob-ranking 'description "Display the current ranking of findings")
(define (flashmob-ranking)
  (if (not *flashmob-session-triage*)
      (printf "No triage yet. Run (flashmob-triage) first.~n")
      (let ([ranking (cdr (assq 'ranking *flashmob-session-triage*))])
        (printf "Finding ranking (~a strategy):~n"
                (cdr (assq 'strategy *flashmob-session-triage*)))
        (printf "~a~n" (make-string 60 #\-))
        (let loop ([r ranking] [i 1])
          (unless (null? r)
            (let ([f (car r)])
              (printf "~a. ~a:~a  [~a]  ~a~n"
                      (fm-pad-left (number->string i) 2)
                      (cdr (assq 'file f))
                      (cdr (assq 'line f))
                      (cdr (assq 'severity f))
                      (fm-truncate (cdr (assq 'title f)) 40))
              (loop (cdr r) (+ i 1))))))))

(doc flashmob-consensus 'type '(-> Real))
(doc flashmob-consensus 'description "Get the consensus confidence score")
(define (flashmob-consensus)
  (if (not *flashmob-session-triage*)
      (begin (printf "No triage yet.~n") 0)
      (let ([score (cdr (assq 'consensus *flashmob-session-triage*))])
        (printf "Consensus: ~a~n" (flashmob-format-pct score))
        score)))

(doc flashmob-compare 'type '(-> Alist))
(doc flashmob-compare 'description "Run both strategies and compare results")
(define (flashmob-compare)
  (unless *flashmob-current-session*
    (error 'flashmob-compare "No active session"))
  (when (null? *flashmob-session-findings*)
    (error 'flashmob-compare "No findings to compare"))
  (let ([result (flashmob-triage-compare *flashmob-session-agents*
                                          *flashmob-session-findings*)])
    (printf "~a~n" (make-string 60 #\=))
    (printf "Strategy Comparison~n")
    (printf "~a~n" (make-string 60 #\=))
    (let ([comp (cdr (assq 'comparison result))])
      (printf "Rank correlation:  ~a~n"
              (flashmob-format-pct (cdr (assq 'rank-correlation comp))))
      (printf "Top-K overlap:     ~a~n"
              (flashmob-format-pct (cdr (assq 'top-k-overlap comp))))
      (printf "Consensus diff:    ~a~n"
              (flashmob-format-signed (cdr (assq 'consensus-diff comp))))
      (printf "Runtime ratio:     ~a~n"
              (flashmob-format-ratio (cdr (assq 'runtime-ratio comp)))))
    (printf "~a~n" (make-string 60 #\-))
    (printf "Simple consensus:  ~a~n"
            (flashmob-format-pct
             (cdr (assq 'consensus (cdr (assq 'simple result))))))
    (printf "Game consensus:    ~a~n"
            (flashmob-format-pct
             (cdr (assq 'consensus (cdr (assq 'game result))))))
    (printf "~a~n" (make-string 60 #\=))
    result))

(doc 'section 'attribution)

(doc flashmob-credits 'type '(-> Void))
(doc flashmob-credits 'description "Display agent attribution credits")
(define (flashmob-credits)
  (if (not *flashmob-session-triage*)
      (printf "No triage yet. Run (flashmob-triage) first.~n")
      (let ([credits (cdr (assq 'credits *flashmob-session-triage*))])
        (printf "Agent Credits (~a strategy):~n"
                (cdr (assq 'strategy *flashmob-session-triage*)))
        (printf "~a~n" (make-string 40 #\-))
        (for-each
         (lambda (c)
           (printf "  ~a: ~a~n"
                   (fm-pad-right (symbol->string (car c)) 20)
                   (flashmob-format-pct (cdr c))))
         (sort (lambda (a b) (> (cdr a) (cdr b))) credits)))))

(doc flashmob-agent-power 'type '(-> Void))
(doc flashmob-agent-power 'description "Display agent power indices")
(define (flashmob-agent-power)
  (if (not *flashmob-session-triage*)
      (printf "No triage yet. Run (flashmob-triage) first.~n")
      (let ([power (cdr (assq 'power *flashmob-session-triage*))])
        (printf "Agent Power Indices (~a strategy):~n"
                (cdr (assq 'strategy *flashmob-session-triage*)))
        (printf "~a~n" (make-string 40 #\-))
        (for-each
         (lambda (p)
           (printf "  ~a: ~a~n"
                   (fm-pad-right (symbol->string (car p)) 20)
                   (flashmob-format-pct (cdr p))))
         (sort (lambda (a b) (> (cdr a) (cdr b))) power)))))

(doc 'section 'history)

(doc flashmob-history 'type '(-> Void))
(doc flashmob-history 'description "Show session history")
(define (flashmob-history)
  (let ([sessions (flashmob-all-sessions)])
    (printf "~a sessions:~n" (length sessions))
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([stats (flashmob-session-stats id)])
         (when stats
           (printf "~a  [~a]  ~a findings~n"
                   (fm-pad-right id 20)
                   (cdr (assq 'status stats))
                   (cdr (assq 'finding-count stats))))))
     sessions)))

(doc 'section 'persistence-helpers)

(doc flashmob-persist-session! 'type '(-> Void))
(doc flashmob-persist-session! 'description "Persist current session state to CAS")
(define (flashmob-persist-session!)
  (when *flashmob-current-session*
    (let* ([agent-hashes (list->vector
                         (map flashmob-agent-hash *flashmob-session-agents*))]
           [finding-hashes (vector)]  ; TODO: store finding hashes
           [triage-hash (if *flashmob-session-triage*
                           (flashmob-save-triage! *flashmob-session-triage*)
                           #f)])
      (flashmob-update-session! *flashmob-current-session*
                                agent-hashes
                                finding-hashes
                                triage-hash))))

(doc flashmob-save-triage! 'type '(-> Alist Bytevector))
(doc flashmob-save-triage! 'description "Save triage result to CAS")
(define (flashmob-save-triage! result)
  (let* ([session-hash (flashmob-read-head *flashmob-current-session*)]
         [strategy (cdr (assq 'strategy result))]
         [ranking (cdr (assq 'ranking result))]
         [consensus (cdr (assq 'consensus result))]
         [credits (cdr (assq 'credits result))]
         [power (cdr (assq 'power result))]
         ;; Convert ranking to serializable form
         [ranking-titles (map (lambda (f) (cdr (assq 'title f))) ranking)]
         [blk (make-triage-block strategy ranking-titles consensus
                                 credits power session-hash)]
         [hash (flashmob-store! blk)])
    hash))

(doc 'section 'formatting-utilities)

(doc flashmob-format-pct 'type '(-> Real String))
(define (flashmob-format-pct n)
  (format "~,1f%" (* 100 n)))

(doc flashmob-format-signed 'type '(-> Real String))
(define (flashmob-format-signed n)
  (if (>= n 0)
      (format "+~,1f%" (* 100 n))
      (format "~,1f%" (* 100 n))))

(doc flashmob-format-ratio 'type '(-> Real String))
(define (flashmob-format-ratio n)
  (format "~,2fx" n))

(doc fm-get-keyword 'type '(-> (List Any) Symbol Any Any))
(define (fm-get-keyword args key default)
  (let loop ([lst args])
    (cond
     [(null? lst) default]
     [(null? (cdr lst)) default]
     [(eq? (car lst) key) (cadr lst)]
     [else (loop (cdr lst))])))

(doc fm-truncate 'type '(-> String Int String))
(define (fm-truncate str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(doc fm-pad-right 'type '(-> String Int String))
(define (fm-pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

(doc fm-pad-left 'type '(-> String Int String))
(define (fm-pad-left str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append (make-string (- width len) #\space) str))))

(doc 'section 'print-help)

(printf "flashmob.ss loaded.~n")
(printf "  (flashmob-start files)          - Start QA session~n")
(printf "  (flashmob-status)               - Show session status~n")
(printf "  (flashmob-add-finding ...)      - Add finding~n")
(printf "  (flashmob-list-findings)        - List findings~n")
(printf "  (flashmob-triage)               - Run triage~n")
(printf "  (flashmob-triage 'strategy 'game) - Force game strategy~n")
(printf "  (flashmob-ranking)              - Show ranking~n")
(printf "  (flashmob-compare)              - Compare strategies~n")
(printf "  (flashmob-credits)              - Show attribution~n")
(printf "  (flashmob-agent-power)          - Show power indices~n")
(printf "  (flashmob-stop)                 - End session~n")
