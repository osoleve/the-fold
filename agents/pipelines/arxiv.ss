;;; agents/pipelines/arxiv.ss — ArXiv Paper Ingestion Pipeline
;;;
;;; Fetches recent papers from ArXiv, filters for relevance,
;;; summarizes key contributions, and posts digest to forum.
;;;
;;; Schedule: Daily at 6am UTC
;;; Trigger: (run-pipeline arxiv-pipeline '((category . "cs.AI") (days . 1)))

(load "fabric/stitches/pipeline/dsl.ss")

;;; ============================================================
;;; Pipeline Configuration
;;; ============================================================

(define arxiv-config
  (config
   (cons 'model 'sonnet)
   (cons 'fuel 15000)
   (cons 'schedule (make-cron-schedule "0 6 * * *"))
   (cons 'retry-policy retry-exponential)
   (cons 'tags '(research automation daily))))

;;; ============================================================
;;; Individual Stages
;;; ============================================================

;;; Stage 1: Fetch papers from ArXiv API
(define fetch-papers
  (named-stage 'fetch
               (chain
                ;; Get parameters from input
                (stage-arr (lambda (params)
                                   (let ([category (or (assq-ref params 'category) "cs.AI")]
                                         [days (or (assq-ref params 'days) 1)])
                                        (format "arxiv-fetch --category ~a --days ~a --format json"
                                                category days))))
                ;; Execute fetch
                (run-shell "${input}")
                ;; Parse JSON response
                parse-json
                ;; Checkpoint raw data
                (save 'raw-papers))))

;;; Stage 2: Score papers for relevance
(define score-papers
  (named-stage 'score
               (chain
                ;; For each paper, get relevance scores
                (map-stage
                 (chain
                  ;; Prepare paper context
                  (stage-arr (lambda (paper)
                                     (format "Title: ~a\nAbstract: ~a"
                                             (assq-ref paper 'title)
                                             (assq-ref paper 'abstract))))
                  ;; Get parallel relevance scores
                  (stage-&&&
                   (ask-llm 'haiku
                            "Rate this paper 1-10 for relevance to AI safety/alignment research. Respond with just a number.\n\n${input}")
                   (ask-llm 'haiku
                            "Rate this paper 1-10 for technical depth and novelty. Respond with just a number.\n\n${input}"))
                  ;; Parse scores
                  (stage-arr (lambda (scores)
                                     (let ([safety-score (string->number (car scores))]
                                           [tech-score (string->number (cdr scores))])
                                          (list (cons 'safety-score (or safety-score 0))
                                                (cons 'tech-score (or tech-score 0))
                                                (cons 'total (+ (or safety-score 0)
                                                                (or tech-score 0)))))))
                  ;; Reattach to original paper
                  (stage-arr (lambda (scores)
                                     ;; Input is available via closure
                                     scores))))
                ;; Checkpoint scored papers
                (save 'scored-papers))))

;;; Stage 3: Filter to high-relevance papers
(define filter-relevant
  (named-stage 'filter
               (chain
                ;; Keep papers with combined score > 12
                (filter-stage
                 (lambda (paper-with-scores)
                         (> (or (assq-ref paper-with-scores 'total) 0) 12)))
                ;; Log count
                (stage-tap (lambda (papers)
                                   (display (format "~a papers passed filter\n" (length papers)))))
                ;; Take top 5 if too many
                (stage-if (lambda (papers) (> (length papers) 5))
                          (take-stage 5)
                          stage-read))))

;;; Stage 4: Summarize each paper
(define summarize-papers
  (named-stage 'summarize
               (chain
                (map-stage
                 (chain
                  ;; Build summary prompt
                  (stage-arr (lambda (paper)
                                     (format "Summarize the key contributions of this paper in 2-3 sentences:\n\nTitle: ~a\n\nAbstract: ~a"
                                             (assq-ref paper 'title)
                                             (assq-ref paper 'abstract))))
                  ;; Get summary
                  (ask-llm 'sonnet "${input}")
                  ;; Structure result
                  (stage-arr (lambda (summary)
                                     summary))))
                ;; Checkpoint summaries
                (save 'summaries))))

;;; Stage 5: Synthesize into forum post
(define synthesize-digest
  (named-stage 'synthesize
               (chain
                ;; Build synthesis prompt
                (stage-arr (lambda (summaries)
                                   (format "Write a forum post digest of today's notable AI research papers. For each paper, include the title and your summary. Focus on what makes each paper interesting or important.\n\nPapers:\n~a"
                                           (apply string-append
                                                  (map (lambda (s) (format "- ~a\n" s))
                                                       summaries)))))
                ;; Synthesize with Opus for quality
                (ask-llm 'opus "${input}")
                ;; Checkpoint synthesis
                (save 'synthesis))))

;;; Stage 6: Post to forum
(define post-digest
  (named-stage 'post
               (chain
                ;; Post to engineering channel
                (forum-post 'engineering "ArXiv Digest" "${input}")
                ;; Log post hash
                (log "Posted ArXiv digest")
                ;; Checkpoint completion
                (save 'completed))))

;;; ============================================================
;;; Full Pipeline
;;; ============================================================

(define arxiv-pipeline
  (define-pipeline* 'arxiv-ingest arxiv-config
    (chain
     fetch-papers
     score-papers
     filter-relevant
     ;; Only continue if we have papers
     (stage-if (lambda (papers) (> (length papers) 0))
               (chain
                summarize-papers
                synthesize-digest
                post-digest)
               (chain
                (log "No relevant papers found today")
                (stage-skip-with "No papers"))))))

;;; ============================================================
;;; Pipeline Variants
;;; ============================================================

;;; Quick version for testing
(define arxiv-quick-pipeline
  (define-pipeline* 'arxiv-quick
    (config (cons 'model 'haiku) (cons 'fuel 5000))
    (chain
     ;; Use mock data for testing
     (stage-pure '((title . "Test Paper")
                   (abstract . "This is a test abstract about AI.")))
     ;; Just summarize and log
     (map-stage (ask-llm 'haiku "Summarize: ${input}"))
     (log "Quick test complete"))))

;;; Multi-category version
(define (arxiv-multi-category-pipeline categories)
  (define-pipeline* 'arxiv-multi arxiv-config
    (chain
     ;; Run arxiv-pipeline for each category
     (stage-pure categories)
     (map-stage
      (lambda (cat)
              (invoke-pipeline arxiv-pipeline)))
     ;; Combine results
     (stage-arr (lambda (results)
                        (apply append results))))))

;;; ============================================================
;;; Manual Invocation Helpers
;;; ============================================================

;;; arxiv-now : Category -> Days -> PipelineResult
;;; Run arxiv pipeline immediately with given parameters.
(define (arxiv-now category days)
  (run-pipeline arxiv-pipeline
                (list (cons 'category category)
                      (cons 'days days))))

;;; arxiv-test : -> PipelineResult
;;; Run quick test pipeline.
(define (arxiv-test)
  (run-pipeline arxiv-quick-pipeline '()))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(define (assq-ref alist key)
  (let ([entry (assq key alist)])
       (if entry (cdr entry) #f)))
