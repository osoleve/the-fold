;;; shell/flashmob/agents.ss — Flashmob Agent Profile Management
;;;
;;; Manages agent profiles with expertise weights.
;;; Agents have category-specific expertise that affects their voting power
;;; in triage decisions.
;;;
;;; Expertise weights are normalized to [0, 1]:
;;;   - 1.0 = Expert in this category
;;;   - 0.5 = Moderate expertise
;;;   - 0.0 = No expertise
;;;
;;; Categories:
;;;   - security      - Security vulnerabilities
;;;   - performance   - Performance issues
;;;   - correctness   - Logic/correctness bugs
;;;   - style         - Code style issues
;;;   - documentation - Documentation gaps
;;;
;;; This is Shell code: impure (CAS operations, mutable registry).

(load "shell/flashmob/store.ss")

;;; ====
;;; Agent Registry
;;; ====

;;; *flashmob-agents* : Hashtable Symbol -> Bytevector
;;; Maps agent-id to its block hash for quick lookup.
(define *flashmob-agents* (make-eq-hashtable))

;;; ====
;;; Default Agent Profiles
;;; ====

;;; Standard agent types with their expertise profiles.

(define *default-agent-profiles*
  '((security-agent
     . ((agent-type . "security-agent")
        (expertise . ((security . 0.95)
                      (correctness . 0.4)
                      (performance . 0.2)
                      (style . 0.1)
                      (documentation . 0.1)))))
    (perf-agent
     . ((agent-type . "performance-agent")
        (expertise . ((performance . 0.95)
                      (correctness . 0.4)
                      (security . 0.2)
                      (style . 0.1)
                      (documentation . 0.1)))))
    (correctness-agent
     . ((agent-type . "correctness-agent")
        (expertise . ((correctness . 0.95)
                      (security . 0.3)
                      (performance . 0.3)
                      (style . 0.2)
                      (documentation . 0.2)))))
    (style-agent
     . ((agent-type . "style-agent")
        (expertise . ((style . 0.95)
                      (documentation . 0.5)
                      (correctness . 0.2)
                      (performance . 0.1)
                      (security . 0.1)))))
    (docs-agent
     . ((agent-type . "documentation-agent")
        (expertise . ((documentation . 0.95)
                      (style . 0.4)
                      (correctness . 0.2)
                      (performance . 0.1)
                      (security . 0.1)))))
    (general-agent
     . ((agent-type . "general-agent")
        (expertise . ((security . 0.5)
                      (performance . 0.5)
                      (correctness . 0.5)
                      (style . 0.5)
                      (documentation . 0.5)))))))

;;; ====
;;; Agent Creation
;;; ====

;;; flashmob-create-agent! : Symbol -> Bytevector
;;; Create an agent from a default profile.
;;; Returns the agent block hash.
(define (flashmob-create-agent! agent-id)
  (let ([profile (assq agent-id *default-agent-profiles*)])
    (if profile
        (flashmob-create-agent-custom! agent-id
                                       (cdr (assq 'agent-type (cdr profile)))
                                       (cdr (assq 'expertise (cdr profile))))
        ;; Unknown agent type - use general profile
        (flashmob-create-agent-custom! agent-id
                                       (symbol->string agent-id)
                                       '((security . 0.5)
                                         (performance . 0.5)
                                         (correctness . 0.5)
                                         (style . 0.5)
                                         (documentation . 0.5))))))

;;; flashmob-create-agent-custom! : Symbol String Alist -> Bytevector
;;; Create a custom agent with specified expertise.
;;; Returns the agent block hash.
(define (flashmob-create-agent-custom! agent-id agent-type expertise)
  (let* ([blk (make-agent-block agent-id agent-type expertise)]
         [hash (flashmob-store! blk)])
    ;; Register in hashtable
    (hashtable-set! *flashmob-agents* agent-id hash)
    hash))

;;; ====
;;; Agent Lookup
;;; ====

;;; flashmob-get-agent : Symbol -> Alist | #f
;;; Get agent data by ID.
(define (flashmob-get-agent agent-id)
  (let ([hash (hashtable-ref *flashmob-agents* agent-id #f)])
    (if hash
        (flashmob-fetch-agent-data hash)
        ;; Try to create from defaults
        (let ([profile (assq agent-id *default-agent-profiles*)])
          (if profile
              (begin
                (flashmob-create-agent! agent-id)
                (flashmob-get-agent agent-id))
              #f)))))

;;; flashmob-agent-hash : Symbol -> Bytevector | #f
;;; Get the hash of an agent block.
(define (flashmob-agent-hash agent-id)
  (hashtable-ref *flashmob-agents* agent-id #f))

;;; flashmob-ensure-agent! : Symbol -> Bytevector
;;; Ensure an agent exists, creating it if needed.
;;; Returns the agent block hash.
(define (flashmob-ensure-agent! agent-id)
  (let ([existing (flashmob-agent-hash agent-id)])
    (if existing
        existing
        (flashmob-create-agent! agent-id))))

;;; ====
;;; Expertise Queries
;;; ====

;;; flashmob-agent-expertise : Symbol Symbol -> Real
;;; Get an agent's expertise weight for a category.
;;; Returns 0.5 (moderate) if agent or category not found.
(define (flashmob-agent-expertise agent-id category)
  (let ([data (flashmob-get-agent agent-id)])
    (if data
        (let* ([expertise (cdr (assq 'expertise data))]
               [cat-weight (assq category expertise)])
          (if cat-weight
              (cdr cat-weight)
              0.5))  ; Default moderate expertise
        0.5)))       ; Unknown agent - moderate expertise

;;; flashmob-agent-expertise-vector : Symbol -> (Vector Real)
;;; Get an agent's expertise as a vector [sec, perf, corr, style, doc].
(define (flashmob-agent-expertise-vector agent-id)
  (let ([data (flashmob-get-agent agent-id)])
    (if data
        (let ([expertise (cdr (assq 'expertise data))])
          (vector (or (cdr (assq 'security expertise)) 0.5)
                  (or (cdr (assq 'performance expertise)) 0.5)
                  (or (cdr (assq 'correctness expertise)) 0.5)
                  (or (cdr (assq 'style expertise)) 0.5)
                  (or (cdr (assq 'documentation expertise)) 0.5)))
        (vector 0.5 0.5 0.5 0.5 0.5))))

;;; ====
;;; Agent Scoring
;;; ====

;;; flashmob-agent-score-finding : Symbol Alist -> Real
;;; Score how relevant an agent is for a finding.
;;; Uses agent expertise × finding category match.
(define (flashmob-agent-score-finding agent-id finding-data)
  (let* ([category (cdr (assq 'category finding-data))]
         [confidence (cdr (assq 'confidence finding-data))]
         [agent-weight (flashmob-agent-expertise agent-id category)])
    ;; Score = agent expertise × finding confidence
    (* agent-weight confidence)))

;;; flashmob-agent-rank-findings : Symbol (List Alist) -> (List (Cons Alist Real))
;;; Rank findings by an agent's expertise.
;;; Returns findings paired with their scores, sorted high to low.
(define (flashmob-agent-rank-findings agent-id findings)
  (let ([scored (map (lambda (f)
                       (cons f (flashmob-agent-score-finding agent-id f)))
                     findings)])
    (sort (lambda (a b) (> (cdr a) (cdr b))) scored)))

;;; ====
;;; Collective Expertise
;;; ====

;;; flashmob-agents-total-expertise : (List Symbol) Symbol -> Real
;;; Total expertise of agents for a category.
(define (flashmob-agents-total-expertise agent-ids category)
  (apply + (map (lambda (id) (flashmob-agent-expertise id category))
                agent-ids)))

;;; flashmob-agents-best-for-category : (List Symbol) Symbol -> Symbol | #f
;;; Find the agent with highest expertise for a category.
(define (flashmob-agents-best-for-category agent-ids category)
  (if (null? agent-ids)
      #f
      (let loop ([ids (cdr agent-ids)]
                 [best-id (car agent-ids)]
                 [best-exp (flashmob-agent-expertise (car agent-ids) category)])
        (if (null? ids)
            best-id
            (let ([exp (flashmob-agent-expertise (car ids) category)])
              (if (> exp best-exp)
                  (loop (cdr ids) (car ids) exp)
                  (loop (cdr ids) best-id best-exp)))))))

;;; ====
;;; Agent Registry Management
;;; ====

;;; flashmob-list-agents : -> (List Symbol)
;;; List all registered agent IDs.
(define (flashmob-list-agents)
  (let-values ([(keys vals) (hashtable-entries *flashmob-agents*)])
    (vector->list keys)))

;;; flashmob-clear-agents! : -> Void
;;; Clear the agent registry (for testing).
(define (flashmob-clear-agents!)
  (set! *flashmob-agents* (make-eq-hashtable)))

;;; flashmob-agent-count : -> Int
;;; Number of registered agents.
(define (flashmob-agent-count)
  (hashtable-size *flashmob-agents*))
