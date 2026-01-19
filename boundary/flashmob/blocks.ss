;;; boundary/flashmob/blocks.ss — Flashmob Block Creation Helpers
;;;
;;; Creates CAS blocks for the QA triage system.
;;;
;;; Block Tags:
;;;   flashmob/finding  - Individual finding from an agent
;;;   flashmob/session  - QA session with refs to agents, findings, triage
;;;   flashmob/agent    - Agent profile with expertise weights
;;;   flashmob/triage   - Triage game results (ranking, consensus, credits)
;;;   flashmob/report   - Aggregated final report
;;;
;;; This is Shell code: uses Core block primitives.

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ====
;;; Block Tags
;;; ====

(define FLASHMOB-FINDING 'flashmob/finding)
(define FLASHMOB-SESSION 'flashmob/session)
(define FLASHMOB-AGENT 'flashmob/agent)
(define FLASHMOB-TRIAGE 'flashmob/triage)
(define FLASHMOB-REPORT 'flashmob/report)

;;; ====
;;; Finding Block
;;; ====

;;; make-finding-block : String Int Symbol Symbol Real Symbol String String (Option Bytevector) -> Block
;;; Create a finding block.
;;;
;;; Arguments:
;;;   file        - File path where finding was identified
;;;   line        - Line number
;;;   severity    - 'critical | 'high | 'medium | 'low | 'info
;;;   category    - 'security | 'performance | 'correctness | 'style | 'documentation
;;;   confidence  - Confidence score (0.0 - 1.0)
;;;   agent-id    - ID of the agent that found this
;;;   title       - Short description of the finding
;;;   description - Detailed description
;;;   session-ref - Hash of the session this finding belongs to (or #f)
(define (make-finding-block file line severity category confidence agent-id
                            title description session-ref)
  (let* ([payload-data `((file . ,file)
                         (line . ,line)
                         (severity . ,severity)
                         (category . ,category)
                         (confidence . ,confidence)
                         (agent-id . ,agent-id)
                         (title . ,title)
                         (description . ,description)
                         (timestamp . ,(flashmob-timestamp)))]
         [payload (string->utf8 (format "~s" payload-data))]
         [refs (if session-ref
                   (vector session-ref)
                   (vector))])
    (make-block FLASHMOB-FINDING payload refs)))

;;; finding-block-data : Block -> Alist | #f
;;; Extract finding data from a block.
(define (finding-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) FLASHMOB-FINDING))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; finding-block-session : Block -> Bytevector | #f
;;; Get the session reference from a finding block.
(define (finding-block-session blk)
  (let ([refs (block-refs blk)])
    (if (> (vector-length refs) 0)
        (vector-ref refs 0)
        #f)))

;;; ====
;;; Agent Block
;;; ====

;;; make-agent-block : Symbol String Alist -> Block
;;; Create an agent profile block.
;;;
;;; Arguments:
;;;   agent-id    - Unique identifier for the agent
;;;   agent-type  - Type description (e.g., "security-agent", "perf-agent")
;;;   expertise   - Alist of (category . weight) pairs, e.g., ((security . 0.9) (performance . 0.3))
(define (make-agent-block agent-id agent-type expertise)
  (let* ([payload-data `((agent-id . ,agent-id)
                         (agent-type . ,agent-type)
                         (expertise . ,expertise)
                         (created-at . ,(flashmob-timestamp)))]
         [payload (string->utf8 (format "~s" payload-data))])
    (make-block FLASHMOB-AGENT payload (vector))))

;;; agent-block-data : Block -> Alist | #f
;;; Extract agent data from a block.
(define (agent-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) FLASHMOB-AGENT))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; ====
;;; Session Block
;;; ====

;;; make-session-block : String (List String) (List Bytevector) (List Bytevector) (Option Bytevector) (Option Bytevector) Int -> Block
;;; Create a session block.
;;;
;;; Arguments:
;;;   session-id    - Human-readable session ID
;;;   files         - List of files under review
;;;   agent-refs    - Vector of agent block hashes
;;;   finding-refs  - Vector of finding block hashes
;;;   triage-ref    - Hash of triage results (or #f if not yet triaged)
;;;   prev-ref      - Hash of previous session (for history chain)
;;;   version       - Session version number
(define (make-session-block session-id files agent-refs finding-refs
                            triage-ref prev-ref version)
  (let* ([payload-data `((session-id . ,session-id)
                         (files . ,files)
                         (status . ,(if triage-ref 'triaged 'collecting))
                         (version . ,version)
                         (created-at . ,(flashmob-timestamp)))]
         [payload (string->utf8 (format "~s" payload-data))]
         ;; refs layout: [agent-refs..., finding-refs..., triage-ref?, prev-ref?]
         ;; We store counts in payload to know how to parse refs
         [refs-list (append (vector->list agent-refs)
                            (vector->list finding-refs)
                            (if triage-ref (list triage-ref) '())
                            (if prev-ref (list prev-ref) '()))]
         ;; Update payload with ref counts for parsing
         [payload-data-full `((session-id . ,session-id)
                              (files . ,files)
                              (status . ,(if triage-ref 'triaged 'collecting))
                              (version . ,version)
                              (created-at . ,(flashmob-timestamp))
                              (agent-count . ,(vector-length agent-refs))
                              (finding-count . ,(vector-length finding-refs))
                              (has-triage . ,(if triage-ref #t #f))
                              (has-prev . ,(if prev-ref #t #f)))]
         [payload-final (string->utf8 (format "~s" payload-data-full))])
    (make-block FLASHMOB-SESSION payload-final (list->vector refs-list))))

;;; session-block-data : Block -> Alist | #f
;;; Extract session data from a block.
(define (session-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) FLASHMOB-SESSION))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; session-block-agents : Block -> (Vector Bytevector)
;;; Get agent references from a session block.
(define (session-block-agents blk)
  (let* ([data (session-block-data blk)]
         [agent-count (cdr (assq 'agent-count data))]
         [refs (block-refs blk)])
    (let ([result (make-vector agent-count)])
      (do ([i 0 (+ i 1)])
          ((>= i agent-count) result)
        (vector-set! result i (vector-ref refs i))))))

;;; session-block-findings : Block -> (Vector Bytevector)
;;; Get finding references from a session block.
(define (session-block-findings blk)
  (let* ([data (session-block-data blk)]
         [agent-count (cdr (assq 'agent-count data))]
         [finding-count (cdr (assq 'finding-count data))]
         [refs (block-refs blk)])
    (let ([result (make-vector finding-count)])
      (do ([i 0 (+ i 1)])
          ((>= i finding-count) result)
        (vector-set! result i (vector-ref refs (+ agent-count i)))))))

;;; session-block-triage : Block -> Bytevector | #f
;;; Get triage reference from a session block.
(define (session-block-triage blk)
  (let* ([data (session-block-data blk)]
         [agent-count (cdr (assq 'agent-count data))]
         [finding-count (cdr (assq 'finding-count data))]
         [has-triage (cdr (assq 'has-triage data))]
         [refs (block-refs blk)])
    (if has-triage
        (vector-ref refs (+ agent-count finding-count))
        #f)))

;;; session-block-prev : Block -> Bytevector | #f
;;; Get previous session reference from a session block.
(define (session-block-prev blk)
  (let* ([data (session-block-data blk)]
         [agent-count (cdr (assq 'agent-count data))]
         [finding-count (cdr (assq 'finding-count data))]
         [has-triage (cdr (assq 'has-triage data))]
         [has-prev (cdr (assq 'has-prev data))]
         [refs (block-refs blk)]
         [offset (+ agent-count finding-count (if has-triage 1 0))])
    (if has-prev
        (vector-ref refs offset)
        #f)))

;;; ====
;;; Triage Block
;;; ====

;;; make-triage-block : Symbol (List Any) Real Alist Alist Bytevector -> Block
;;; Create a triage results block.
;;;
;;; Arguments:
;;;   strategy    - 'simple | 'game
;;;   ranking     - Ranked list of finding hashes/ids
;;;   consensus   - Consensus confidence score (0.0 - 1.0)
;;;   credits     - Alist of (agent-id . credit-value) for attribution
;;;   power       - Alist of (agent-id . power-index) for voting power
;;;   session-ref - Hash of the session this triage belongs to
(define (make-triage-block strategy ranking consensus credits power session-ref)
  (let* ([payload-data `((strategy . ,strategy)
                         (ranking . ,ranking)
                         (consensus . ,consensus)
                         (credits . ,credits)
                         (power . ,power)
                         (timestamp . ,(flashmob-timestamp)))]
         [payload (string->utf8 (format "~s" payload-data))])
    (make-block FLASHMOB-TRIAGE payload (vector session-ref))))

;;; triage-block-data : Block -> Alist | #f
;;; Extract triage data from a block.
(define (triage-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) FLASHMOB-TRIAGE))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; triage-block-session : Block -> Bytevector
;;; Get the session reference from a triage block.
(define (triage-block-session blk)
  (vector-ref (block-refs blk) 0))

;;; ====
;;; Report Block
;;; ====

;;; make-report-block : String Alist Bytevector (Option Bytevector) -> Block
;;; Create an aggregated report block.
;;;
;;; Arguments:
;;;   format      - Report format (e.g., "json", "markdown")
;;;   content     - Report content as an alist
;;;   session-ref - Hash of the session this report is for
;;;   prev-ref    - Hash of previous report (for iterative reports)
(define (make-report-block format content session-ref prev-ref)
  (let* ([payload-data `((format . ,format)
                         (content . ,content)
                         (timestamp . ,(flashmob-timestamp)))]
         [payload (string->utf8 (format "~s" payload-data))]
         [refs (if prev-ref
                   (vector session-ref prev-ref)
                   (vector session-ref))])
    (make-block FLASHMOB-REPORT payload refs)))

;;; report-block-data : Block -> Alist | #f
;;; Extract report data from a block.
(define (report-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) FLASHMOB-REPORT))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; report-block-session : Block -> Bytevector
;;; Get the session reference from a report block.
(define (report-block-session blk)
  (vector-ref (block-refs blk) 0))

;;; ====
;;; Timestamp Utility
;;; ====

;;; flashmob-timestamp : -> String
;;; Generate an ISO 8601 timestamp.
(define (flashmob-timestamp)
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))
