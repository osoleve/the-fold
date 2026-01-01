;;; agents/pipelines/tech-debt.ss — Tech Debt Review Pipeline
;;;
;;; Periodic review of codebase health and technical debt.
;;; Uses council deliberation for thorough analysis.
;;;
;;; Schedule: Weekly on Sunday at midnight UTC

(load "core/pipeline/dsl.ss")

;;; ============================================================
;;; Pipeline Configuration
;;; ============================================================

(define tech-debt-config
  (config
   (cons 'model 'opus)
   (cons 'fuel 30000)
   (cons 'schedule (make-cron-schedule "0 0 * * 0"))  ; Weekly Sunday
   (cons 'tags '(maintenance debt review))))

;;; ============================================================
;;; Stage: Gather Metrics
;;; ============================================================

(define gather-metrics-stage
  (named-stage 'gather-metrics
               (chain
                (log "Gathering codebase metrics")
                
                ;; Parallel metric collection
                (stage-pure '())  ; Start with empty context
                
                (stage-&&&
                 ;; Line counts
                 (run-shell "find fabric thimble -name '*.ss' -exec wc -l {} + | tail -1")
                 ;; File counts
                 (run-shell "find fabric thimble -name '*.ss' | wc -l"))
                
                (stage-&&&
                 ;; TODO count
                 (run-shell "grep -r 'TODO' fabric thimble --include='*.ss' | wc -l")
                 ;; FIXME count
                 (run-shell "grep -r 'FIXME' fabric thimble --include='*.ss' | wc -l"))
                
                (stage-&&&
                 ;; Test count
                 (run-shell "find . -name 'test-*.ss' | wc -l")
                 ;; Open issues
                 (run-shell "bd list --status=open | wc -l"))
                
                ;; Structure results
                (stage-arr (lambda (metrics)
                                   (list (cons 'line-count (caar metrics))
                                         (cons 'file-count (cdar metrics))
                                         (cons 'todo-count (caadr metrics))
                                         (cons 'fixme-count (cdadr metrics))
                                         (cons 'test-count (caaddr metrics))
                                         (cons 'open-issues (cdaddr metrics)))))
                
                (save 'metrics))))

;;; ============================================================
;;; Stage: Identify Hot Spots
;;; ============================================================

(define identify-hotspots-stage
  (named-stage 'hotspots
               (chain
                (log "Identifying code hot spots")
                
                ;; Get recent git activity
                (run-shell "git log --oneline --since='1 week ago' --name-only | grep '.ss$' | sort | uniq -c | sort -rn | head -10")
                
                ;; Ask LLM to analyze
                (ask-llm 'sonnet
                         "These files have been most frequently modified recently. Which might indicate technical debt or stability issues?\n\n${input}")
                
                (save 'hotspots))))

;;; ============================================================
;;; Stage: Code Quality Analysis
;;; ============================================================

(define code-quality-stage
  (named-stage 'quality
               (chain
                (log "Analyzing code quality")
                
                ;; Sample some files for review
                (run-shell "find fabric/stitches -name '*.ss' | shuf | head -3")
                
                ;; Read and analyze each
                (stage-arr (lambda (files)
                                   (string-split files "\n")))
                
                (filter-stage (lambda (f) (> (string-length f) 0)))
                
                (map-stage
                 (chain
                  (run-shell "head -100 ${input}")
                  (ask-llm 'sonnet
                           "Review this Scheme code for quality issues (naming, complexity, documentation). Be specific.\n\n${input}")))
                
                (save 'quality-reviews))))

;;; ============================================================
;;; Stage: Council Deliberation
;;; ============================================================

(define deliberation-stage
  (named-stage 'deliberate
               (chain
                (log "Council deliberation on tech debt")
                
                ;; Load gathered data
                (stage-&&&
                 (load-checkpoint 'metrics)
                 (stage-&&&
                  (load-checkpoint 'hotspots)
                  (load-checkpoint 'quality-reviews)))
                
                ;; Format for council
                (stage-arr (lambda (data)
                                   (format "Tech Debt Review Data:

Metrics:
~a

Hot Spots:
~a

Quality Reviews:
~a

What are the top 3 technical debt items we should address?"
                                           (car data)
                                           (cadr data)
                                           (cddr data))))
                
                ;; Council discussion
                (council-sequential '(opus gemini-3) 2 'opus)
                
                ;; Extract recommendations
                (stage-arr (lambda (result)
                                   (if (council-result? result)
                                       (result-synthesis result)
                                       result)))
                
                (save 'recommendations))))

;;; ============================================================
;;; Stage: Create Issues
;;; ============================================================

(define create-issues-stage
  (named-stage 'create-issues
               (chain
                (log "Creating issues for debt items")
                
                ;; Load recommendations
                (load-checkpoint 'recommendations)
                
                ;; Parse into actionable items
                (ask-llm 'sonnet
                         "Extract the top 3 actionable technical debt items from this analysis. For each, provide a title and brief description suitable for an issue tracker.\n\nFormat as:\n1. TITLE: description\n2. TITLE: description\n3. TITLE: description\n\n${input}")
                
                ;; Create beads for each
                (stage-arr (lambda (items)
                                   ;; Parse items (simplified)
                                   (string-split items "\n")))
                
                (filter-stage (lambda (line)
                                      (and (> (string-length line) 3)
                                           (char-numeric? (string-ref line 0)))))
                
                (map-stage
                 (chain
                  ;; Extract title
                  (stage-arr (lambda (line)
                                     ;; Skip "1. " prefix
                                     (if (> (string-length line) 3)
                                         (substring line 3)
                                         line)))
                  ;; Create bead
                  (beads-create "${input}")))
                
                (save 'issues-created))))

;;; ============================================================
;;; Stage: Post Report
;;; ============================================================

(define post-report-stage
  (named-stage 'report
               (chain
                (log "Posting tech debt report")
                
                ;; Load all data
                (stage-&&&
                 (load-checkpoint 'metrics)
                 (stage-&&&
                  (load-checkpoint 'recommendations)
                  (load-checkpoint 'issues-created)))
                
                ;; Format report
                (stage-arr (lambda (data)
                                   (format "# Weekly Tech Debt Report

## Metrics
~a

## Recommendations
~a

## Issues Created
~a items created in issue tracker"
                                           (car data)
                                           (cadr data)
                                           (length (cddr data)))))
                
                ;; Post to forum
                (forum-post 'engineering "Tech Debt Report" "${input}")
                
                (save 'report-posted))))

;;; ============================================================
;;; Full Pipeline
;;; ============================================================

(define tech-debt-pipeline
  (define-pipeline* 'tech-debt-review tech-debt-config
    (chain
     gather-metrics-stage
     identify-hotspots-stage
     code-quality-stage
     deliberation-stage
     create-issues-stage
     post-report-stage)))

;;; ============================================================
;;; Variants
;;; ============================================================

;;; Quick review without issue creation
(define tech-debt-quick-pipeline
  (define-pipeline* 'tech-debt-quick
    (config (cons 'model 'sonnet) (cons 'fuel 10000))
    (chain
     gather-metrics-stage
     identify-hotspots-stage
     deliberation-stage
     post-report-stage)))

;;; Metrics only
(define tech-debt-metrics-pipeline
  (define-pipeline* 'tech-debt-metrics
    (config (cons 'model 'haiku) (cons 'fuel 3000))
    gather-metrics-stage))

;;; ============================================================
;;; Manual Invocation
;;; ============================================================

(define (tech-debt-review)
  (run-pipeline tech-debt-pipeline '()))

(define (tech-debt-quick)
  (run-pipeline tech-debt-quick-pipeline '()))

(define (tech-debt-metrics)
  (run-pipeline tech-debt-metrics-pipeline '()))
