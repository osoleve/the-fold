(load "boundary/flashmob/store.ss")

(doc 'module 'flashmob/agents)
(doc 'description "Manages agent profiles with expertise weights for QA triage. Agents have category-specific expertise that affects their voting power in triage decisions.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'expertise-model)
(doc 'description "Expertise weights are normalized to [0, 1]: 1.0 = Expert, 0.5 = Moderate, 0.0 = No expertise. Categories: security, performance, correctness, style, documentation.")

(doc 'section 'agent-registry)

(define *flashmob-agents* (make-eq-hashtable))
(doc *flashmob-agents* 'type '(Hashtable Symbol Bytevector))
(doc *flashmob-agents* 'description "Maps agent-id to its block hash for quick lookup")

(doc 'section 'default-profiles)

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
(doc *default-agent-profiles* 'description "Standard agent types with their expertise profiles")

(doc 'section 'agent-creation)

(define (flashmob-create-agent! agent-id)
  (doc 'type '(-> Symbol Bytevector))
  (doc 'description "Create an agent from a default profile")
  (doc 'returns "The agent block hash")
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

(define (flashmob-create-agent-custom! agent-id agent-type expertise)
  (doc 'type '(-> Symbol String Alist Bytevector))
  (doc 'description "Create a custom agent with specified expertise")
  (doc 'returns "The agent block hash")
  (let* ([blk (make-agent-block agent-id agent-type expertise)]
         [hash (flashmob-store! blk)])
    ;; Register in hashtable
    (hashtable-set! *flashmob-agents* agent-id hash)
    hash))

(doc 'section 'agent-lookup)

(define (flashmob-get-agent agent-id)
  (doc 'type '(-> Symbol (Maybe Alist)))
  (doc 'description "Get agent data by ID")
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

(define (flashmob-agent-hash agent-id)
  (doc 'type '(-> Symbol (Maybe Bytevector)))
  (doc 'description "Get the hash of an agent block")
  (hashtable-ref *flashmob-agents* agent-id #f))

(define (flashmob-ensure-agent! agent-id)
  (doc 'type '(-> Symbol Bytevector))
  (doc 'description "Ensure an agent exists, creating it if needed")
  (doc 'returns "The agent block hash")
  (let ([existing (flashmob-agent-hash agent-id)])
    (if existing
        existing
        (flashmob-create-agent! agent-id))))

(doc 'section 'expertise-queries)

(define (flashmob-agent-expertise agent-id category)
  (doc 'type '(-> Symbol Symbol Real))
  (doc 'description "Get an agent's expertise weight for a category")
  (doc 'returns "0.5 (moderate) if agent or category not found")
  (let ([data (flashmob-get-agent agent-id)])
    (if data
        (let* ([expertise (cdr (assq 'expertise data))]
               [cat-weight (assq category expertise)])
          (if cat-weight
              (cdr cat-weight)
              0.5))  ; Default moderate expertise
        0.5)))       ; Unknown agent - moderate expertise

(define (flashmob-agent-expertise-vector agent-id)
  (doc 'type '(-> Symbol (Vector Real)))
  (doc 'description "Get an agent's expertise as a vector [security, performance, correctness, style, documentation]")
  (let ([data (flashmob-get-agent agent-id)])
    (if data
        (let ([expertise (cdr (assq 'expertise data))])
          (vector (or (cdr (assq 'security expertise)) 0.5)
                  (or (cdr (assq 'performance expertise)) 0.5)
                  (or (cdr (assq 'correctness expertise)) 0.5)
                  (or (cdr (assq 'style expertise)) 0.5)
                  (or (cdr (assq 'documentation expertise)) 0.5)))
        (vector 0.5 0.5 0.5 0.5 0.5))))

(doc 'section 'agent-scoring)

(define (flashmob-agent-score-finding agent-id finding-data)
  (doc 'type '(-> Symbol Alist Real))
  (doc 'description "Score how relevant an agent is for a finding using agent expertise × finding category match")
  (let* ([category (cdr (assq 'category finding-data))]
         [confidence (cdr (assq 'confidence finding-data))]
         [agent-weight (flashmob-agent-expertise agent-id category)])
    ;; Score = agent expertise × finding confidence
    (* agent-weight confidence)))

(define (flashmob-agent-rank-findings agent-id findings)
  (doc 'type '(-> Symbol (List Alist) (List (Pair Alist Real))))
  (doc 'description "Rank findings by an agent's expertise")
  (doc 'returns "Findings paired with their scores, sorted high to low")
  (let ([scored (map (lambda (f)
                       (cons f (flashmob-agent-score-finding agent-id f)))
                     findings)])
    (sort (lambda (a b) (> (cdr a) (cdr b))) scored)))

(doc 'section 'collective-expertise)

(define (flashmob-agents-total-expertise agent-ids category)
  (doc 'type '(-> (List Symbol) Symbol Real))
  (doc 'description "Total expertise of agents for a category")
  (apply + (map (lambda (id) (flashmob-agent-expertise id category))
                agent-ids)))

(define (flashmob-agents-best-for-category agent-ids category)
  (doc 'type '(-> (List Symbol) Symbol (Maybe Symbol)))
  (doc 'description "Find the agent with highest expertise for a category")
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

(doc 'section 'registry-management)

(define (flashmob-list-agents)
  (doc 'type '(-> (List Symbol)))
  (doc 'description "List all registered agent IDs")
  (let-values ([(keys vals) (hashtable-entries *flashmob-agents*)])
    (vector->list keys)))

(define (flashmob-clear-agents!)
  (doc 'type '(-> Void))
  (doc 'description "Clear the agent registry (for testing)")
  (set! *flashmob-agents* (make-eq-hashtable)))

(define (flashmob-agent-count)
  (doc 'type '(-> Int))
  (doc 'description "Number of registered agents")
  (hashtable-size *flashmob-agents*))
