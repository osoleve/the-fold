;;; agents/pipelines/feature-dev.ss — Feature Development Pipeline
;;;
;;; Multi-stage workflow for feature development:
;;; plan → develop → QA → test → dogfood → ship
;;;
;;; Each stage auto-advances on success.
;;; Uses FSM pattern for complex state transitions.

(load "fabric/stitches/pipeline/dsl.ss")

;;; ============================================================
;;; Pipeline Configuration
;;; ============================================================

(define feature-dev-config
  (config
   (cons 'model 'opus)
   (cons 'fuel 50000)  ; Large budget for full cycle
   (cons 'retry-policy retry-exponential)
   (cons 'tags '(development workflow feature))))

;;; ============================================================
;;; Stage: Planning
;;; ============================================================

;;; Council deliberation on approach
(define plan-stage
  (named-stage 'plan
               (chain
                ;; Log start
                (log "Starting planning phase")
                
                ;; Gather context
                (stage-&&&
                 ;; Get codebase context
                 (run-shell "find fabric thimble -name '*.ss' | head -20")
                 ;; Get related issues
                 (run-shell "bd list --status=open --json | head -5"))
                
                ;; Council discussion on approach
                (stage-arr (lambda (context)
                                   (format "Feature request: ${input}\n\nRelevant files:\n~a\n\nOpen issues:\n~a"
                                           (car context) (cdr context))))
                
                ;; Architectural council
                (council-sequential '(opus gemini-3) 2 'opus)
                
                ;; Extract plan from council result
                (stage-arr (lambda (result)
                                   (if (council-result? result)
                                       (result-synthesis result)
                                       result)))
                
                ;; Save plan
                (save 'plan)
                
                ;; Post plan for visibility
                (forum-post 'engineering "Feature Plan" "${input}"))))

;;; ============================================================
;;; Stage: Development
;;; ============================================================

(define develop-stage
  (named-stage 'develop
               (chain
                ;; Log phase
                (log "Starting development phase")
                
                ;; Load plan
                (load-checkpoint 'plan)
                
                ;; Generate implementation
                (with-persona (make-persona 'builder "" '((tier . builder)))
                              (ask-llm 'sonnet
                                       "Based on this plan, provide the implementation. Include file paths and complete code.\n\n${input}"))
                
                ;; Parse into file operations (interpreter will handle)
                (stage-arr (lambda (impl)
                                   (list 'implementation impl)))
                
                ;; Save implementation
                (save 'implementation)
                
                ;; Log completion
                (log "Development phase complete"))))

;;; ============================================================
;;; Stage: QA Review
;;; ============================================================

(define qa-stage
  (named-stage 'qa
               (chain
                ;; Log phase
                (log "Starting QA phase")
                
                ;; Load implementation
                (load-checkpoint 'implementation)
                
                ;; Parallel review
                (stage-&&&
                 ;; Code review
                 (ask-llm 'sonnet
                          "Review this code for bugs, edge cases, and code quality issues. Be specific.\n\n${input}")
                 ;; Security review
                 (ask-llm 'sonnet
                          "Review this code for security vulnerabilities (injection, XSS, etc.). List any concerns.\n\n${input}"))
                
                ;; Combine reviews
                (stage-arr (lambda (reviews)
                                   (list (cons 'code-review (car reviews))
                                         (cons 'security-review (cdr reviews)))))
                
                ;; Check for blockers
                (stage-if
                 (lambda (reviews)
                         ;; Simple heuristic: look for "critical" or "vulnerability"
                         (or (string-contains? (cdr (assq 'code-review reviews)) "critical")
                             (string-contains? (cdr (assq 'security-review reviews)) "vulnerability")))
                 ;; Has blockers - return to development
                 (chain
                  (log "QA found blockers, returning to development")
                  (stage-err 'qa-blocked "QA found critical issues" '()))
                 ;; Passed
                 (chain
                  (save 'qa-passed)
                  (log "QA passed"))))))

;;; ============================================================
;;; Stage: Testing
;;; ============================================================

(define test-stage
  (named-stage 'test
               (chain
                ;; Log phase
                (log "Starting test phase")
                
                ;; Run test suite
                (run-shell "scheme --script test-all.ss 2>&1")
                
                ;; Parse results
                (stage-if
                 (lambda (output)
                         (string-contains? output "All tests passed"))
                 (chain
                  (save 'tests-passed)
                  (log "All tests passed"))
                 (chain
                  (log "Tests failed")
                  (stage-err 'tests-failed "Test suite failed" '()))))))

;;; ============================================================
;;; Stage: Dogfooding
;;; ============================================================

(define dogfood-stage
  (named-stage 'dogfood
               (chain
                ;; Log phase
                (log "Starting dogfood phase")
                
                ;; Sample from multiple models for diverse testing
                (council-parallel
                 '(opus sonnet haiku gemini-3 kimi)
                 'opus)
                
                ;; Check for issues
                (stage-arr (lambda (result)
                                   (if (council-result? result)
                                       (let ([responses (result-responses result)])
                                            ;; Look for issues in any response
                                            (filter (lambda (r)
                                                            (or (string-contains? (cdr r) "issue")
                                                                (string-contains? (cdr r) "problem")
                                                                (string-contains? (cdr r) "bug")))
                                                    responses))
                                       '())))
                
                ;; Handle issues
                (stage-if
                 (lambda (issues) (> (length issues) 0))
                 (chain
                  (log "Dogfooding found issues")
                  (stage-arr (lambda (issues)
                                     (list 'dogfood-issues issues)))
                  (stage-err 'dogfood-issues "Dogfooding found issues" '()))
                 (chain
                  (save 'dogfood-passed)
                  (log "Dogfooding passed"))))))

;;; ============================================================
;;; Stage: Ship
;;; ============================================================

(define ship-stage
  (named-stage 'ship
               (chain
                ;; Log phase
                (log "Shipping feature")
                
                ;; Load plan for commit message
                (load-checkpoint 'plan)
                
                ;; Create commit
                (stage-arr (lambda (plan)
                                   ;; Extract first line of plan as summary
                                   (let ([lines (string-split plan "\n")])
                                        (if (pair? lines)
                                            (car lines)
                                            "Feature implementation"))))
                
                ;; Commit and push
                (run-shell "git add -A && git commit -m '${input}' && git push")
                
                ;; Close related beads
                (run-shell "bd list --status=in_progress --json | jq -r '.[].id' | xargs -I{} bd close {}")
                
                ;; Post announcement
                (forum-post 'engineering "Feature Shipped" "Feature has been implemented and deployed.")
                
                ;; Final checkpoint
                (save 'shipped))))

;;; ============================================================
;;; FSM Definition
;;; ============================================================

;;; State transitions:
;;; plan → (ok → develop, err → halt)
;;; develop → (ok → qa, err → halt)
;;; qa → (ok → test, qa-blocked → develop)
;;; test → (ok → dogfood, tests-failed → develop)
;;; dogfood → (ok → ship, dogfood-issues → develop)
;;; ship → (ok → accept)

(define feature-dev-fsm
  (fsm-pipeline
   ;; States and their stages with transitions
   (list
    (list 'plan plan-stage
          (cons 'ok 'develop))
    
    (list 'develop develop-stage
          (cons 'ok 'qa))
    
    (list 'qa qa-stage
          (cons 'ok 'test)
          (cons 'qa-blocked 'develop))
    
    (list 'test test-stage
          (cons 'ok 'dogfood)
          (cons 'tests-failed 'develop))
    
    (list 'dogfood dogfood-stage
          (cons 'ok 'ship)
          (cons 'dogfood-issues 'develop))
    
    (list 'ship ship-stage
          (cons 'ok 'accept)))
   
   ;; Initial state
   'plan
   
   ;; Accepting states
   '(accept halt)))

;;; ============================================================
;;; Full Pipeline
;;; ============================================================

(define feature-dev-pipeline
  (define-pipeline* 'feature-dev feature-dev-config
    feature-dev-fsm))

;;; ============================================================
;;; Pipeline Variants
;;; ============================================================

;;; Skip dogfooding for small changes
(define feature-dev-quick-pipeline
  (define-pipeline* 'feature-dev-quick
    (config (cons 'model 'sonnet) (cons 'fuel 20000))
    (chain plan-stage develop-stage qa-stage test-stage ship-stage)))

;;; Planning only (for design discussion)
(define feature-plan-only-pipeline
  (define-pipeline* 'feature-plan
    (config (cons 'model 'opus) (cons 'fuel 10000))
    plan-stage))

;;; ============================================================
;;; Manual Invocation Helpers
;;; ============================================================

;;; feature-dev : String -> PipelineResult
;;; Run full feature development cycle.
(define (feature-dev description)
  (run-pipeline feature-dev-pipeline description))

;;; feature-plan : String -> PipelineResult
;;; Just do planning phase.
(define (feature-plan description)
  (run-pipeline feature-plan-only-pipeline description))

;;; feature-quick : String -> PipelineResult
;;; Quick feature dev without dogfooding.
(define (feature-quick description)
  (run-pipeline feature-dev-quick-pipeline description))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(define (string-contains? str substr)
  ;; Simple substring search
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

(define (string-split str delim)
  ;; Simple split on delimiter character
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) (string-ref delim 0))
         (loop (cdr chars)
               '()
               (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))
