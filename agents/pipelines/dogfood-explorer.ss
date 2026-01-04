;;; agents/pipelines/dogfood-explorer.ss — Varied Exploratory Dogfooding
;;;
;;; Provides diverse exploration paths through The Fold for testing.
;;; Each session randomly selects exploration behaviors and depths.
;;;
;;; Exploration Paths:
;;;   1. Block Explorer - Navigate the content-addressed store
;;;   2. Lambda Kombat - Play pattern matching puzzles
;;;   3. Forum Interaction - Browse, search, post
;;;   4. Tutorial System - Start and navigate tutorials
;;;   5. Duckie Chat - Interact with the learning companion
;;;   6. Turtle Graphics - Create and test drawings
;;;   7. Command Discovery - Explore help and commands
;;;
;;; Usage:
;;;   (dogfood-session)           ; Run a varied exploration session
;;;   (dogfood-session 'deep)     ; Run with deep exploration
;;;   (dogfood-session 'quick)    ; Run quick smoke tests
;;;   (dogfood-report)            ; Show findings from last session

;;; ============================================================
;;; Session State
;;; ============================================================

(define *dogfood-findings* '())
(define *dogfood-session-start* #f)
(define *dogfood-exploration-path* #f)

;;; Finding types: issue, observation, success, suggestion
(define (record-finding type category message)
  (set! *dogfood-findings*
        (cons `((type . ,type)
                (category . ,category)
                (message . ,message)
                (timestamp . ,(current-timestamp)))
              *dogfood-findings*)))

(define (clear-findings!)
  (set! *dogfood-findings* '()))

;;; ============================================================
;;; Exploration Path Definitions
;;; ============================================================

;;; Each path is: (name weight quick-actions deep-actions)
;;; Weight affects selection probability
(define *exploration-paths*
  '((block-explorer    3 (bx-stats bx-popular)
                    (bx-popular bx-view-random bx-search-random bx-orphans bx-navigate-depth))
    (lambda-kombat     2 (lk-start-quit)
                   (lk-play-rounds lk-check-leaderboard))
    (forum-interaction 4 (digest channels)
                       (digest-deep browse-channels search-posts reply-test))
    (tutorial-system   2 (list-tutorials)
                     (start-tutorial tutorial-navigation))
    (duckie-chat       2 (duckie-greeting)
                 (duckie-conversation duckie-question))
    (turtle-graphics   2 (turtle-basic)
                     (turtle-shapes turtle-colors turtle-save))
    (command-discovery 3 (help commands)
                       (help-deep command-exploration typo-recovery))))

;;; ============================================================
;;; Random Selection Utilities
;;; ============================================================

(define (weighted-random-select paths)
  "Select a path based on weights"
  (let* ([total-weight (apply + (map cadr paths))]
         [r (random total-weight)])
        (let loop ([paths paths] [acc 0])
             (if (null? paths)
                 (car (last-pair *exploration-paths*))
                 (let ([p (car paths)]
                       [new-acc (+ acc (cadr (car paths)))])
                      (if (< r new-acc)
                          p
                          (loop (cdr paths) new-acc)))))))

(define (pick-random lst)
  "Pick random element from list"
  (if (null? lst)
      #f
      (list-ref lst (random (length lst)))))

(define (pick-n-random n lst)
  "Pick n random elements from list"
  (let loop ([remaining lst] [count n] [result '()])
       (if (or (= count 0) (null? remaining))
           (reverse result)
           (let ([idx (random (length remaining))])
                (loop (append (list-head remaining idx)
                              (list-tail remaining (+ idx 1)))
                      (- count 1)
                      (cons (list-ref remaining idx) result))))))

;;; ============================================================
;;; Block Explorer Exploration
;;; ============================================================

(define (explore-block-explorer depth)
  (display "\n=== Block Explorer Exploration ===\n\n")
  (record-finding 'observation 'block-explorer "Starting block explorer exploration")
  
  ;; Always start with stats
  (guard (ex [else (record-finding 'issue 'block-explorer
                                   (format "bx-stats failed: ~a" ex))])
         (bx-stats)
         (record-finding 'success 'block-explorer "bx-stats executed successfully"))
  
  (when (eq? depth 'deep)
        ;; Try popular blocks
        (guard (ex [else (record-finding 'issue 'block-explorer
                                         (format "bx-popular failed: ~a" ex))])
               (bx-popular)
               (record-finding 'success 'block-explorer "bx-popular shows referenced blocks"))
        
        ;; Try viewing a random entry if available
        (guard (ex [else (record-finding 'observation 'block-explorer
                                         "Could not navigate into block")])
               (let ([blocks (current-block-list)])
                    (when (and blocks (not (null? blocks)))
                          (let ([idx (random (min 5 (length blocks)))])
                               (bx-view idx)
                               (record-finding 'success 'block-explorer
                                               (format "Navigated to block ~a" idx))))))
        
        ;; Try search with common term
        (let ([search-terms '("post" "message" "lambda" "test" "chat")])
             (guard (ex [else (record-finding 'issue 'block-explorer
                                              (format "bx-search failed: ~a" ex))])
                    (bx-search (pick-random search-terms))
                    (record-finding 'success 'block-explorer "Search functionality works")))
        
        ;; Check orphans
        (guard (ex [else (record-finding 'issue 'block-explorer
                                         (format "bx-orphans failed: ~a" ex))])
               (bx-orphans)
               (let ([orphan-count (length (current-block-list))])
                    (record-finding 'observation 'block-explorer
                                    (format "Found ~a orphan blocks" orphan-count)))))
  
  (bx-home))

;;; ============================================================
;;; Lambda Kombat Exploration
;;; ============================================================

(define (explore-lambda-kombat depth)
  (display "\n=== Lambda Kombat Exploration ===\n\n")
  (record-finding 'observation 'lambda-kombat "Starting Lambda Kombat exploration")
  
  ;; Start a game
  (guard (ex [else (record-finding 'issue 'lambda-kombat
                                   (format "lambda-kombat failed to start: ~a" ex))])
         (lambda-kombat)
         (record-finding 'success 'lambda-kombat "Game started successfully"))
  
  (when (eq? depth 'deep)
        ;; Play a few rounds
        (let play-rounds ([rounds-left (+ 2 (random 4))])
             (when (> rounds-left 0)
                   (guard (ex [else (record-finding 'observation 'lambda-kombat
                                                    "Round play interrupted")])
                          ;; Answer randomly (testing UI, not correctness)
                          (let ([answer (+ 1 (random 4))])
                               (lk-answer answer)
                               (record-finding 'success 'lambda-kombat
                                               (format "Answered round with ~a" answer)))
                          (lk-next)
                          (play-rounds (- rounds-left 1)))))
        
        ;; Check leaderboard
        (guard (ex [else (record-finding 'issue 'lambda-kombat
                                         (format "Leaderboard failed: ~a" ex))])
               (lk-leaderboard)
               (record-finding 'success 'lambda-kombat "Leaderboard displays correctly")))
  
  ;; Quit game
  (guard (ex [else #f])
         (lk-quit)
         (record-finding 'success 'lambda-kombat "Game quit cleanly")))

;;; ============================================================
;;; Forum Interaction Exploration
;;; ============================================================

(define (explore-forum-interaction depth)
  (display "\n=== Forum Interaction Exploration ===\n\n")
  (record-finding 'observation 'forum "Starting forum exploration")
  
  ;; Digest
  (guard (ex [else (record-finding 'issue 'forum
                                   (format "digest failed: ~a" ex))])
         (digest)
         (record-finding 'success 'forum "Digest displays correctly"))
  
  ;; Channels list
  (guard (ex [else (record-finding 'issue 'forum
                                   (format "channels failed: ~a" ex))])
         (channels)
         (record-finding 'success 'forum "Channel list works"))
  
  (when (eq? depth 'deep)
        ;; Browse specific channels
        (let ([test-channels '(engineering philosophy design bugs arena)])
             (for-each
              (lambda (ch)
                      (guard (ex [else (record-finding 'observation 'forum
                                                       (format "Channel ~a browse issue" ch))])
                             (browse ch 3)
                             (record-finding 'success 'forum
                                             (format "Browsed ~a channel" ch))))
              (pick-n-random 2 test-channels)))
        
        ;; Search
        (let ([search-terms '("feature" "bug" "test" "question")])
             (guard (ex [else (record-finding 'issue 'forum "Search failed")])
                    (search-posts (fs) 'engineering (pick-random search-terms))
                    (record-finding 'success 'forum "Search in channel works")))
        
        ;; Forum summary
        (guard (ex [else (record-finding 'issue 'forum "Forum summary failed")])
               (forum-summary (fs))
               (record-finding 'success 'forum "Forum summary displays"))))

;;; ============================================================
;;; Tutorial System Exploration
;;; ============================================================

(define (explore-tutorial-system depth)
  (display "\n=== Tutorial System Exploration ===\n\n")
  (record-finding 'observation 'tutorial "Starting tutorial exploration")
  
  ;; List tutorials
  (guard (ex [else (record-finding 'issue 'tutorial
                                   (format "list-tutorials failed: ~a" ex))])
         (list-tutorials)
         (record-finding 'success 'tutorial "Tutorial listing works"))
  
  (when (eq? depth 'deep)
        ;; Try starting a tutorial
        (guard (ex [else (record-finding 'observation 'tutorial
                                         "Tutorial start interrupted")])
               (start-tutorial)
               (record-finding 'success 'tutorial "Tutorial started"))
        
        ;; Try navigation commands
        (guard (ex [else #f])
               (tutorial-status)
               (record-finding 'success 'tutorial "Tutorial status works"))
        
        (guard (ex [else #f])
               (tutorial-help)
               (record-finding 'success 'tutorial "Tutorial help works"))))

;;; ============================================================
;;; Duckie Chat Exploration
;;; ============================================================

(define (explore-duckie-chat depth)
  (display "\n=== Duckie Chat Exploration ===\n\n")
  (record-finding 'observation 'duckie "Starting Duckie exploration")
  
  ;; Basic greeting
  (guard (ex [else (record-finding 'issue 'duckie
                                   (format "to-duckie failed: ~a" ex))])
         (to-duckie "Hello DUCKIE!")
         (record-finding 'success 'duckie "Duckie responds to greeting"))
  
  (when (eq? depth 'deep)
        ;; Ask questions
        (let ([questions '("What can you teach me?"
                           "How does The Fold work?"
                           "What are blocks?"
                           "Tell me about lambda calculus")])
             (for-each
              (lambda (q)
                      (guard (ex [else (record-finding 'observation 'duckie
                                                       (format "Question response issue: ~a" q))])
                             (to-duckie q)
                             (record-finding 'success 'duckie
                                             (format "Duckie answered: ~a" (string-truncate q 30)))))
              (pick-n-random 2 questions)))))

;;; ============================================================
;;; Turtle Graphics Exploration
;;; ============================================================

(define (explore-turtle-graphics depth)
  (display "\n=== Turtle Graphics Exploration ===\n\n")
  (record-finding 'observation 'turtle "Starting turtle graphics exploration")
  
  ;; Load turtle module if not already loaded
  (guard (ex [else #f])
         (load "shell/ui/turtle.ss"))
  
  ;; Create turtle
  (guard (ex [else (record-finding 'issue 'turtle
                                   (format "make-turtle failed: ~a" ex))])
         (define t (make-turtle))
         (record-finding 'success 'turtle "Turtle created successfully")
         
         (when (eq? depth 'deep)
               ;; Basic movements
               (guard (ex [else (record-finding 'issue 'turtle "Movement failed")])
                      (set! t (fd t 50))
                      (set! t (rt t 90))
                      (set! t (fd t 50))
                      (record-finding 'success 'turtle "Basic movement works"))
               
               ;; Draw a shape
               (guard (ex [else (record-finding 'issue 'turtle "Shape drawing failed")])
                      (let loop ([i 0] [turtle t])
                           (if (< i 4)
                               (loop (+ i 1) (rt (fd turtle 30) 90))
                               (set! t turtle)))
                      (record-finding 'success 'turtle "Square drawing works"))
               
               ;; Check position
               (guard (ex [else #f])
                      (let ([x (xcor t)] [y (ycor t)])
                           (record-finding 'observation 'turtle
                                           (format "Final position: (~a, ~a)" x y)))))))

;;; ============================================================
;;; Command Discovery Exploration
;;; ============================================================

(define (explore-command-discovery depth)
  (display "\n=== Command Discovery Exploration ===\n\n")
  (record-finding 'observation 'commands "Starting command discovery exploration")
  
  ;; Basic help
  (guard (ex [else (record-finding 'issue 'commands "help failed")])
         (help)
         (record-finding 'success 'commands "Help command works"))
  
  ;; Commands list
  (guard (ex [else (record-finding 'issue 'commands "commands list failed")])
         (commands)
         (record-finding 'success 'commands "Commands list works"))
  
  (when (eq? depth 'deep)
        ;; Help for specific commands
        (let ([test-commands '(digest chat msg browse who bye)])
             (for-each
              (lambda (cmd)
                      (guard (ex [else (record-finding 'observation 'commands
                                                       (format "help '~a issue" cmd))])
                             (help cmd)
                             (record-finding 'success 'commands
                                             (format "Help for ~a works" cmd))))
              (pick-n-random 3 test-commands)))
        
        ;; Test typo recovery
        (guard (ex [else #f])
               (cmd 'digestt)  ; Intentional typo
               (record-finding 'observation 'commands "Typo suggestion tested"))))

;;; ============================================================
;;; Main Session Runner
;;; ============================================================

(define (dogfood-session . args)
  "Run a varied dogfood exploration session.
   Optional arg: 'deep for thorough exploration, 'quick for smoke tests"
  (let ([depth (if (null? args)
                   (if (< (random 10) 3) 'quick 'deep)
                   (car args))])
       
       ;; Initialize session
       (clear-findings!)
       (set! *dogfood-session-start* (current-timestamp))
       
       (display "\n")
       (display "╔══════════════════════════════════════════════════════════════════╗\n")
       (display "║                    DOGFOOD EXPLORATION SESSION                   ║\n")
       (display "╠══════════════════════════════════════════════════════════════════╣\n")
       (display (format "║  Mode: ~a~a║\n"
                        depth
                        (make-string (- 55 (string-length (symbol->string depth))) #\space)))
       (display (format "║  Start: ~a~a║\n"
                        *dogfood-session-start*
                        (make-string (- 50 (string-length *dogfood-session-start*)) #\space)))
       (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
       
       ;; Select exploration paths (1-3 for quick, 2-5 for deep)
       (let* ([num-paths (if (eq? depth 'quick)
                             (+ 1 (random 2))
                             (+ 2 (random 4)))]
              [selected-paths (pick-n-random num-paths *exploration-paths*)])
             
             (set! *dogfood-exploration-path* (map car selected-paths))
             
             (display (format "Selected exploration paths: ~a\n\n"
                              (map car selected-paths)))
             
             ;; Run each exploration
             (for-each
              (lambda (path-spec)
                      (let ([path-name (car path-spec)])
                           (display (format "\n>>> Exploring: ~a\n" path-name))
                           (guard (ex [else
                                       (record-finding 'issue 'session
                                                       (format "Path ~a crashed: ~a" path-name ex))])
                                  (case path-name
                                        [(block-explorer) (explore-block-explorer depth)]
                                        [(lambda-kombat) (explore-lambda-kombat depth)]
                                        [(forum-interaction) (explore-forum-interaction depth)]
                                        [(tutorial-system) (explore-tutorial-system depth)]
                                        [(duckie-chat) (explore-duckie-chat depth)]
                                        [(turtle-graphics) (explore-turtle-graphics depth)]
                                        [(command-discovery) (explore-command-discovery depth)]
                                        [else (record-finding 'observation 'session
                                                              (format "Unknown path: ~a" path-name))]))))
              selected-paths))
       
       ;; Show summary
       (dogfood-report)))

;;; ============================================================
;;; Report Generation
;;; ============================================================

(define (dogfood-report)
  "Display findings from the last dogfood session"
  (let* ([issues (filter (lambda (f) (eq? (cdr (assq 'type f)) 'issue))
                         *dogfood-findings*)]
         [successes (filter (lambda (f) (eq? (cdr (assq 'type f)) 'success))
                            *dogfood-findings*)]
         [observations (filter (lambda (f) (eq? (cdr (assq 'type f)) 'observation))
                               *dogfood-findings*)])
        
        (display "\n")
        (display "╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║                    DOGFOOD SESSION REPORT                        ║\n")
        (display "╠══════════════════════════════════════════════════════════════════╣\n")
        (display (format "║  Paths explored: ~a~a║\n"
                         (if *dogfood-exploration-path*
                             (length *dogfood-exploration-path*)
                             0)
                         (make-string 48 #\space)))
        (display (format "║  Issues: ~a  │  Successes: ~a  │  Observations: ~a~a║\n"
                         (length issues)
                         (length successes)
                         (length observations)
                         (make-string 20 #\space)))
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        
        (when (not (null? issues))
              (display "ISSUES FOUND:\n")
              (for-each
               (lambda (f)
                       (display (format "  ⚠ [~a] ~a\n"
                                        (cdr (assq 'category f))
                                        (cdr (assq 'message f)))))
               issues)
              (display "\n"))
        
        (when (not (null? observations))
              (display "OBSERVATIONS:\n")
              (for-each
               (lambda (f)
                       (display (format "  • [~a] ~a\n"
                                        (cdr (assq 'category f))
                                        (cdr (assq 'message f)))))
               (list-head observations (min 10 (length observations))))
              (when (> (length observations) 10)
                    (display (format "  ... and ~a more\n" (- (length observations) 10))))
              (display "\n"))
        
        (display (format "Total findings: ~a\n" (length *dogfood-findings*)))
        
        ;; Return structured result for workflow integration
        `((issues . ,issues)
          (successes . ,successes)
          (observations . ,observations)
          (paths . ,*dogfood-exploration-path*)
          (session-start . ,*dogfood-session-start*))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(define (string-truncate str max-len)
  (if (> (string-length str) max-len)
      (string-append (substring str 0 max-len) "...")
      str))

(define (list-head lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;;; ============================================================
;;; Workflow Integration
;;; ============================================================

;;; For use in feedback.yaml workflow
(define (dogfood-run-and-report)
  "Run dogfood session and return report suitable for workflow"
  (dogfood-session)
  (let ([report (dogfood-report)])
       (if (null? (cdr (assq 'issues report)))
           (display "\n✓ No issues found in this session.\n")
           (display "\n⚠ Issues found - see report above.\n"))
       report))

;;; Check if issues warrant a forum post
(define (dogfood-report-worthy?)
  (let ([issues (filter (lambda (f) (eq? (cdr (assq 'type f)) 'issue))
                        *dogfood-findings*)])
       (> (length issues) 0)))

;;; Generate forum post body from findings
(define (dogfood-generate-post)
  (let* ([issues (filter (lambda (f) (eq? (cdr (assq 'type f)) 'issue))
                         *dogfood-findings*)]
         [paths *dogfood-exploration-path*])
        (string-append
         "## Dogfood Session Report\n\n"
         (format "**Paths Explored:** ~a\n\n" paths)
         "### Issues Found\n\n"
         (apply string-append
                (map (lambda (f)
                             (format "- **~a**: ~a\n"
                                     (cdr (assq 'category f))
                                     (cdr (assq 'message f))))
                     issues))
         "\n---\n*Automated dogfood exploration session*")))

;;; ============================================================
;;; Load Message
;;; ============================================================

(display "Dogfood Explorer loaded.\n")
(display "  (dogfood-session)        - Run varied exploration\n")
(display "  (dogfood-session 'deep)  - Deep exploration\n")
(display "  (dogfood-session 'quick) - Quick smoke test\n")
(display "  (dogfood-report)         - Show last session findings\n")
