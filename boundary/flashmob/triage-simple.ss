;;; boundary/flashmob/triage-simple.ss — Simple Triage Strategy
;;;
;;; Implements the simple A/B test strategy:
;;;   - Selection: Approval voting (top K by count)
;;;   - Ranking: Borda count
;;;   - Power: Equal weights
;;;   - Attribution: Proportional share
;;;   - Consensus: Agreement ratio
;;;
;;; Complexity: O(N*M) where N = agents, M = findings
;;;
;;; This is Shell code: impure (agent lookups).

(load "boundary/flashmob/agents.ss")

;;; ====
;;; Preference Building
;;; ====

;;; Agents "vote" by ranking findings.
;;; Each agent ranks findings based on expertise-weighted scoring.

;;; simple-agent-rankings : (List Symbol) (List Alist) -> (List (List Alist))
;;; Build preference profile: each agent's ranking of findings.
;;; Returns list of rankings (one per agent), each ranking is list of finding-data.
(define (simple-agent-rankings agent-ids findings)
  (map (lambda (agent-id)
         (let ([ranked (flashmob-agent-rank-findings agent-id findings)])
           ;; Extract just the finding data (without scores)
           (map car ranked)))
       agent-ids))

;;; ====
;;; Approval Voting Selection
;;; ====

;;; simple-approval-threshold : (List Alist) -> Real
;;; Calculate the approval threshold for findings.
;;; Findings with confidence above this are "approved".
(define (simple-approval-threshold findings)
  (if (null? findings)
      0.5
      ;; Use median confidence as threshold
      (let* ([confidences (map (lambda (f) (cdr (assq 'confidence f))) findings)]
             [sorted (sort < confidences)]
             [mid (quotient (length sorted) 2)])
        (list-ref sorted mid))))

;;; simple-agent-approvals : Symbol (List Alist) Real -> (List Alist)
;;; Get the findings an agent "approves" (scores above threshold).
(define (simple-agent-approvals agent-id findings threshold)
  (let ([ranked (flashmob-agent-rank-findings agent-id findings)])
    ;; Approve findings where agent score >= threshold
    (map car (filter (lambda (pair) (>= (cdr pair) threshold)) ranked))))

;;; simple-approval-counts : (List Symbol) (List Alist) -> (List (Cons Alist Int))
;;; Count how many agents approve each finding.
(define (simple-approval-counts agent-ids findings)
  (let ([threshold (simple-approval-threshold findings)])
    (map (lambda (finding)
           (cons finding
                 (length (filter (lambda (agent-id)
                                   (let ([approvals (simple-agent-approvals agent-id findings threshold)])
                                     (member finding approvals)))
                                 agent-ids))))
         findings)))

;;; simple-approval-select : (List Symbol) (List Alist) Int -> (List Alist)
;;; Select top K findings by approval count.
(define (simple-approval-select agent-ids findings k)
  (let* ([counts (simple-approval-counts agent-ids findings)]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) counts)]
         [top-k (take-up-to k sorted)])
    (map car top-k)))

;;; ====
;;; Borda Count Ranking
;;; ====

;;; simple-borda-score : Alist (List (List Alist)) -> Int
;;; Compute Borda score for a finding.
;;; Each agent gives m-1 points to 1st, m-2 to 2nd, etc.
(define (simple-borda-score finding rankings)
  (let ([m (if (null? rankings) 0 (length (car rankings)))])
    (apply + (map (lambda (ranking)
                    (let ([pos (simple-position-of finding ranking)])
                      (if pos
                          (- m 1 pos)
                          0)))
                  rankings))))

;;; simple-position-of : Alist (List Alist) -> Int | #f
;;; Find position of finding in ranking (0-indexed).
(define (simple-position-of finding ranking)
  (let loop ([r ranking] [i 0])
    (cond
      [(null? r) #f]
      [(equal? (car r) finding) i]
      [else (loop (cdr r) (+ i 1))])))

;;; simple-borda-ranking : (List Alist) (List (List Alist)) -> (List Alist)
;;; Rank findings by Borda count.
(define (simple-borda-ranking findings rankings)
  (let ([scored (map (lambda (f) (cons f (simple-borda-score f rankings)))
                     findings)])
    (map car (sort (lambda (a b) (> (cdr a) (cdr b))) scored))))

;;; simple-borda-scores-all : (List Alist) (List (List Alist)) -> (List (Cons Alist Int))
;;; Get all Borda scores (for reporting).
(define (simple-borda-scores-all findings rankings)
  (map (lambda (f) (cons f (simple-borda-score f rankings)))
       findings))

;;; ====
;;; Consensus Measurement
;;; ====

;;; simple-pairwise-agreement : (List (List Alist)) Alist Alist -> Real
;;; Measure agreement on whether finding A should rank above B.
;;; Returns ratio of agents who agree.
(define (simple-pairwise-agreement rankings a b)
  (let* ([n (length rankings)]
         [a-over-b (length (filter (lambda (r)
                                     (let ([pa (simple-position-of a r)]
                                           [pb (simple-position-of b r)])
                                       (and pa pb (< pa pb))))
                                   rankings))])
    (if (= n 0) 0.5 (/ (max a-over-b (- n a-over-b)) n))))

;;; simple-consensus-score : (List Alist) (List (List Alist)) -> Real
;;; Overall consensus score (0-1).
;;; Average pairwise agreement across all finding pairs.
(define (simple-consensus-score findings rankings)
  (if (or (< (length findings) 2) (null? rankings))
      1.0  ; Perfect consensus if 0-1 findings
      (let* ([pairs (simple-all-pairs findings)]
             [agreements (map (lambda (pair)
                               (simple-pairwise-agreement rankings (car pair) (cdr pair)))
                             pairs)])
        (if (null? agreements)
            1.0
            (/ (apply + agreements) (length agreements))))))

;;; simple-all-pairs : (List a) -> (List (Cons a a))
;;; Generate all unordered pairs.
(define (simple-all-pairs lst)
  (if (null? lst)
      '()
      (append (map (lambda (x) (cons (car lst) x)) (cdr lst))
              (simple-all-pairs (cdr lst)))))

;;; ====
;;; Attribution (Proportional Share)
;;; ====

;;; simple-proportional-credits : (List Symbol) (List Alist) -> Alist
;;; Calculate proportional credit for each agent.
;;; Credit is proportional to their contribution to approved findings.
(define (simple-proportional-credits agent-ids findings)
  (if (null? agent-ids)
      '()
      (let* ([n (length agent-ids)]
             ;; Each agent gets 1/n of the credit for each finding they contributed to
             [base-share (/ 1.0 n)]
             ;; Calculate contribution per agent
             [contributions
              (map (lambda (agent-id)
                     (let* ([ranked (flashmob-agent-rank-findings agent-id findings)]
                            [total-score (apply + (map cdr ranked))])
                       (cons agent-id total-score)))
                   agent-ids)]
             [total-contrib (apply + (map cdr contributions))])
        (if (= total-contrib 0)
            ;; Equal split if no contributions
            (map (lambda (id) (cons id base-share)) agent-ids)
            ;; Proportional to contribution
            (map (lambda (c)
                   (cons (car c) (/ (cdr c) total-contrib)))
                 contributions)))))

;;; ====
;;; Power Indices (Equal for Simple Strategy)
;;; ====

;;; simple-power-indices : (List Symbol) -> Alist
;;; Power indices for simple strategy - all agents have equal power.
(define (simple-power-indices agent-ids)
  (if (null? agent-ids)
      '()
      (let ([equal-power (/ 1.0 (length agent-ids))])
        (map (lambda (id) (cons id equal-power)) agent-ids))))

;;; ====
;;; Main Triage Function
;;; ====

;;; simple-triage : (List Symbol) (List Alist) Int -> Alist
;;; Run the simple triage strategy.
;;;
;;; Arguments:
;;;   agent-ids - List of participating agent IDs
;;;   findings  - List of finding data alists
;;;   k         - Number of top findings to select (0 = all)
;;;
;;; Returns:
;;;   ((strategy . simple)
;;;    (ranking . <sorted list of findings>)
;;;    (selected . <top-k findings>)
;;;    (consensus . <0-1 score>)
;;;    (credits . <agent attribution>)
;;;    (power . <power indices>)
;;;    (scores . <borda scores for all>))
(define (simple-triage agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      `((strategy . simple)
        (ranking . ())
        (selected . ())
        (consensus . 1.0)
        (credits . ())
        (power . ())
        (scores . ()))
      (let* ([rankings (simple-agent-rankings agent-ids findings)]
             [borda-ranking (simple-borda-ranking findings rankings)]
             [selected (if (= k 0)
                          borda-ranking
                          (take-up-to k borda-ranking))]
             [consensus (simple-consensus-score findings rankings)]
             [credits (simple-proportional-credits agent-ids findings)]
             [power (simple-power-indices agent-ids)]
             [scores (simple-borda-scores-all findings rankings)])
        `((strategy . simple)
          (ranking . ,borda-ranking)
          (selected . ,selected)
          (consensus . ,consensus)
          (credits . ,credits)
          (power . ,power)
          (scores . ,scores)))))

;;; ====
;;; Utility Functions
;;; ====

;;; take-up-to : Int (List a) -> (List a)
;;; Take up to n elements from list.
(define (take-up-to n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))
