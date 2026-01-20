(load "boundary/flashmob/flashmob.ss")
(load "boundary/flashmob/report.ss")

(doc 'module 'flashmob-dogfood)
(doc 'description "Dogfood test of flashmob on itself")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'usage "scheme --script boundary/flashmob/dogfood-run.ss")

(doc 'section 'setup-session)

(printf "~n=== FLASHMOB DOGFOOD TEST ===~n")
(printf "Testing flashmob on its own codebase~n~n")

;; Start session on flashmob modules
(flashmob-start '("boundary/flashmob/flashmob.ss"
                  "boundary/flashmob/triage-game.ss"
                  "boundary/flashmob/triage-simple.ss"
                  "boundary/flashmob/report.ss"
                  "boundary/flashmob/agents.ss"))

;; Register QA agents with different expertise profiles
(flashmob-create-agent! 'security-agent)
(flashmob-create-agent! 'perf-agent)
(flashmob-create-agent! 'correctness-agent)
(flashmob-create-agent! 'style-agent)

(printf "~nRegistered 4 QA agents~n")

(doc 'section 'add-findings)
(doc 'note "Simulating agent reports")

;; Security findings
(flashmob-add-finding
  'file "report.ss"
  'line 154
  'severity 'medium
  'category 'security
  'confidence 0.85
  'agent 'security-agent
  'title "JSON escape incomplete for Unicode"
  'description "json-escape-string only handles ASCII control chars, not full Unicode escapes")

(flashmob-add-finding
  'file "flashmob.ss"
  'line 89
  'severity 'low
  'category 'security
  'confidence 0.6
  'agent 'security-agent
  'title "File path not validated"
  'description "flashmob-start accepts file paths without validation")

;; Performance findings
(flashmob-add-finding
  'file "triage-game.ss"
  'line 280
  'severity 'critical
  'category 'performance
  'confidence 0.95
  'agent 'perf-agent
  'title "Shapley value O(2^N) complexity"
  'description "game-shapley-attribution iterates all 2^N coalitions - exponential blowup with >15 agents")

(flashmob-add-finding
  'file "triage-game.ss"
  'line 180
  'severity 'medium
  'category 'performance
  'confidence 0.8
  'agent 'perf-agent
  'title "Schulze matrix O(N^3) for large candidate sets"
  'description "Floyd-Warshall in game-schulze-ranking is cubic in number of findings")

(flashmob-add-finding
  'file "triage-simple.ss"
  'line 45
  'severity 'low
  'category 'performance
  'confidence 0.7
  'agent 'perf-agent
  'title "Redundant list traversals in scoring"
  'description "simple-score-all-findings traverses findings multiple times per agent")

;; Correctness findings
(flashmob-add-finding
  'file "triage-game.ss"
  'line 220
  'severity 'high
  'category 'correctness
  'confidence 0.88
  'agent 'correctness-agent
  'title "Schulze fallback not triggered on sparse data"
  'description "When <50% pairwise preferences exist, should fall back to Borda but threshold not checked")

(flashmob-add-finding
  'file "agents.ss"
  'line 75
  'severity 'medium
  'category 'correctness
  'confidence 0.75
  'agent 'correctness-agent
  'title "Default expertise may not sum to 1"
  'description "flashmob-default-expertise returns values that may not be normalized")

(flashmob-add-finding
  'file "triage-simple.ss"
  'line 120
  'severity 'low
  'category 'correctness
  'confidence 0.65
  'agent 'correctness-agent
  'title "Division by zero possible in consensus"
  'description "simple-consensus divides by total without checking for empty agent list")

;; Style findings
(flashmob-add-finding
  'file "flashmob.ss"
  'line 1
  'severity 'info
  'category 'style
  'confidence 0.9
  'agent 'style-agent
  'title "Inconsistent module header format"
  'description "Some files use ;;; header, others use different conventions")

(flashmob-add-finding
  'file "triage-game.ss"
  'line 50
  'severity 'info
  'category 'style
  'confidence 0.85
  'agent 'style-agent
  'title "Long functions exceed 50 lines"
  'description "game-schulze-ranking and game-shapley-attribution are >100 lines each")

(printf "Added 10 findings across 4 categories~n~n")

(doc 'section 'run-and-compare)

(printf "~n========================================~n")
(printf "SIMPLE STRATEGY RESULTS~n")
(printf "========================================~n~n")

(let ([simple-result (flashmob-triage-run
                       *flashmob-session-agents*
                       *flashmob-session-findings*
                       'strategy 'simple
                       'k 5)])
  (set! *flashmob-session-triage* simple-result)

  (printf "Strategy: ~a~n" (cdr (assq 'strategy simple-result)))
  (printf "Consensus: ~,2f~n~n" (cdr (assq 'consensus simple-result)))

  (printf "Top 5 Findings (by Borda count):~n")
  (printf "~a~n" (make-string 50 #\-))
  (let ([selected (cdr (assq 'selected simple-result))])
    (for-each
      (lambda (f i)
        (printf "~a. [~a] ~a - ~a~n"
                (+ i 1)
                (cdr (assq 'severity f))
                (cdr (assq 'file f))
                (cdr (assq 'title f))))
      selected
      (iota (length selected))))

  (printf "~nPower Indices (equal for simple):~n")
  (for-each
    (lambda (p)
      (printf "  ~a: ~,3f~n" (car p) (cdr p)))
    (cdr (assq 'power simple-result)))

  (printf "~nCredit Attribution (proportional):~n")
  (for-each
    (lambda (c)
      (printf "  ~a: ~,1f%~n" (car c) (* 100 (cdr c))))
    (cdr (assq 'credits simple-result))))

(printf "~n~n========================================~n")
(printf "GAME-THEORETIC STRATEGY RESULTS~n")
(printf "========================================~n~n")

(let ([game-result (flashmob-triage-run
                     *flashmob-session-agents*
                     *flashmob-session-findings*
                     'strategy 'game
                     'k 5)])
  (set! *flashmob-session-triage* game-result)

  (printf "Strategy: ~a~n" (cdr (assq 'strategy game-result)))
  (printf "Consensus: ~,2f~n~n" (cdr (assq 'consensus game-result)))

  (printf "Top 5 Findings (by PAV + Schulze):~n")
  (printf "~a~n" (make-string 50 #\-))
  (let ([selected (cdr (assq 'selected game-result))])
    (for-each
      (lambda (f i)
        (printf "~a. [~a] ~a - ~a~n"
                (+ i 1)
                (cdr (assq 'severity f))
                (cdr (assq 'file f))
                (cdr (assq 'title f))))
      selected
      (iota (length selected))))

  (printf "~nPower Indices (Banzhaf, expertise-weighted):~n")
  (for-each
    (lambda (p)
      (printf "  ~a: ~,3f~n" (car p) (cdr p)))
    (cdr (assq 'power game-result)))

  (printf "~nCredit Attribution (Shapley value):~n")
  (for-each
    (lambda (c)
      (printf "  ~a: ~,1f%~n" (car c) (* 100 (cdr c))))
    (cdr (assq 'credits game-result))))

(doc 'section 'direct-comparison)

(printf "~n~n========================================~n")
(printf "STRATEGY COMPARISON~n")
(printf "========================================~n~n")

(let* ([comparison (flashmob-triage-compare
                     *flashmob-session-agents*
                     *flashmob-session-findings*
                     'k 5)]
       [simple (cdr (assq 'simple comparison))]
       [game (cdr (assq 'game comparison))]
       [comp (cdr (assq 'comparison comparison))])

  (printf "Rank Correlation (Kendall's tau): ~,3f~n"
          (cdr (assq 'rank-correlation comp)))
  (printf "Top-5 Overlap: ~,1f%~n"
          (* 100 (cdr (assq 'top-k-overlap comp))))

  (printf "~nConsensus Difference:~n")
  (printf "  Simple: ~,2f~n" (cdr (assq 'consensus simple)))
  (printf "  Game:   ~,2f~n" (cdr (assq 'consensus game)))

  (printf "~nRanking Differences:~n")
  (printf "~a~n" (make-string 60 #\-))

  (let ([simple-ranking (cdr (assq 'ranking simple))]
        [game-ranking (cdr (assq 'ranking game))])
    (printf "~nSimple Ranking (full):~n")
    (for-each
      (lambda (f i)
        (printf "  ~a. [~a ~a] ~a~n"
                (+ i 1)
                (cdr (assq 'severity f))
                (cdr (assq 'category f))
                (cdr (assq 'title f))))
      simple-ranking
      (iota (length simple-ranking)))

    (printf "~nGame-Theoretic Ranking (full):~n")
    (for-each
      (lambda (f i)
        (printf "  ~a. [~a ~a] ~a~n"
                (+ i 1)
                (cdr (assq 'severity f))
                (cdr (assq 'category f))
                (cdr (assq 'title f))))
      game-ranking
      (iota (length game-ranking)))))

(printf "~n=== DOGFOOD TEST COMPLETE ===~n")

(doc 'section 'helpers)
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))
