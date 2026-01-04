;;; agents/pipelines/dogfood-explorer.ss — Varied Exploratory Dogfooding
;;;
;;; Provides diverse exploration paths through The Fold for testing.
;;; Each session randomly selects exploration behaviors and depths.
;;;
;;; Exploration Paths:
;;;   1. Block Store - Navigate and query content-addressed storage
;;;   2. Forum System - Browse, search, post, channels
;;;   3. Session Management - Login, logout, identity, tiers
;;;   4. Tutorial Infrastructure - Tutorial system mechanics
;;;   5. Graphics Primitives - Turtle, canvas, rendering
;;;   6. Command System - Help, discovery, error recovery
;;;   7. Query DSL - Search, filters, block queries
;;;   8. Creative Tools - User creations, templates, patches
;;;
;;; Usage:
;;;   (dogfood-session)           ; Run a varied exploration session
;;;   (dogfood-session 'deep)     ; Run with deep exploration
;;;   (dogfood-session 'quick)    ; Run quick smoke tests
;;;   (dogfood-report)            ; Show findings from last session
;;;   (show-trajectory-stats)     ; View trajectory diversity stats
;;;
;;; Trajectory Tracking:
;;;   Sessions record their exploration path order to promote temporal
;;;   diversity. The system generates candidate orderings and selects
;;;   the one least similar to recent trajectories (both exact matches
;;;   and pairwise transitions are penalized).

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
;;; Trajectory Tracking
;;; ============================================================

;;; Store trajectories to promote temporal diversity
(define *trajectory-file* ".fold-repl/dogfood-trajectories.ss")
(define *max-trajectories* 50)  ; Keep last N trajectories

;;; load-trajectories : -> (List Trajectory)
;;; Each trajectory is: ((timestamp . T) (paths . (p1 p2 ...)) (depth . D))
(define (load-trajectories)
  (guard (ex [else '()])
         (if (file-exists? *trajectory-file*)
             (call-with-input-file *trajectory-file* read)
             '())))

;;; save-trajectory! : (List Symbol) × Symbol -> void
;;; Record a trajectory for future diversity calculations
(define (save-trajectory! paths depth)
  (let* ([existing (load-trajectories)]
         [new-entry `((timestamp . ,(current-timestamp))
                      (paths . ,paths)
                      (depth . ,depth))]
         [updated (cons new-entry
                        (if (>= (length existing) *max-trajectories*)
                            (list-head existing (- *max-trajectories* 1))
                            existing))])
        (guard (ex [else #f])
               ;; Delete and recreate to avoid Chez Scheme file-exists error
               (when (file-exists? *trajectory-file*)
                     (delete-file *trajectory-file*))
               (call-with-output-file *trajectory-file*
                                      (lambda (port)
                                              (write updated port)
                                              (newline port))))))

;;; trajectory-signature : (List Symbol) -> Symbol
;;; Create a canonical signature for a path ordering
(define (trajectory-signature paths)
  (string->symbol
   (apply string-append
          (map (lambda (p) (string-append (symbol->string p) ">"))
               paths))))

;;; count-trajectory-occurrences : (List Trajectory) -> Hashtable
;;; Count how many times each trajectory signature has occurred
(define (count-trajectory-occurrences trajectories)
  (let ([counts (make-eq-hashtable)])
       (for-each
        (lambda (traj)
                (let* ([paths (cdr (assq 'paths traj))]
                       [sig (trajectory-signature paths)]
                       [current (hashtable-ref counts sig 0)])
                      (hashtable-set! counts sig (+ current 1))))
        trajectories)
       counts))

;;; path-pair-counts : (List Trajectory) -> Hashtable
;;; Count transitions between paths (for Markov-style diversity)
(define (path-pair-counts trajectories)
  (let ([counts (make-eq-hashtable)])
       (for-each
        (lambda (traj)
                (let ([paths (cdr (assq 'paths traj))])
                     (let loop ([remaining paths])
                          (when (>= (length remaining) 2)
                                (let* ([pair (cons (car remaining) (cadr remaining))]
                                       [key (string->symbol
                                             (format "~a->~a" (car pair) (cdr pair)))]
                                       [current (hashtable-ref counts key 0)])
                                      (hashtable-set! counts key (+ current 1))
                                      (loop (cdr remaining)))))))
        trajectories)
       counts))

;;; diversity-score : (List Symbol) × Hashtable × Hashtable -> Number
;;; Score a candidate trajectory (lower = more novel)
(define (diversity-score candidate-paths traj-counts pair-counts)
  (let* ([sig (trajectory-signature candidate-paths)]
         [sig-count (hashtable-ref traj-counts sig 0)]
         [transition-score
          (let loop ([paths candidate-paths] [score 0])
               (if (< (length paths) 2)
                   score
                   (let* ([key (string->symbol
                                (format "~a->~a" (car paths) (cadr paths)))]
                          [pair-count (hashtable-ref pair-counts key 0)])
                         (loop (cdr paths) (+ score pair-count)))))])
        ;; Base score: how many times this exact sequence occurred
        ;; Plus penalty for common transitions
        (+ (* sig-count 10) transition-score)))

;;; select-diverse-paths : Nat × (List PathSpec) -> (List PathSpec)
;;; Select paths with diversity bias based on trajectory history
(define (select-diverse-paths n all-paths)
  (let* ([trajectories (load-trajectories)]
         [traj-counts (count-trajectory-occurrences trajectories)]
         [pair-counts (path-pair-counts trajectories)])
        (if (null? trajectories)
            ;; No history: pure random selection
            (pick-n-random n all-paths)
            ;; Generate candidates and pick most diverse
            (let ([candidates (map (lambda (_) (pick-n-random n all-paths))
                                   (iota 5))]  ; Generate 5 candidates
                  [score-path (lambda (paths)
                                      (diversity-score (map car paths)
                                                       traj-counts
                                                       pair-counts))])
                 ;; Pick candidate with lowest score (most novel)
                 (car (list-sort (lambda (a b)
                                         (< (score-path a) (score-path b)))
                                 candidates))))))

;;; show-trajectory-stats : -> void
;;; Display trajectory diversity statistics
(define (show-trajectory-stats)
  (let* ([trajectories (load-trajectories)]
         [traj-counts (count-trajectory-occurrences trajectories)]
         [pair-counts (path-pair-counts trajectories)])
        (display "\n")
        (display "╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║                  TRAJECTORY DIVERSITY STATS                      ║\n")
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        (display (format "Total recorded trajectories: ~a\n" (length trajectories)))
        (display (format "Unique trajectory signatures: ~a\n"
                         (hashtable-size traj-counts)))
        (display (format "Unique path transitions: ~a\n\n"
                         (hashtable-size pair-counts)))
        
        ;; Show most common trajectories
        (when (> (hashtable-size traj-counts) 0)
              (display "Most common trajectories:\n")
              (let* ([entries (let-values ([(keys vals) (hashtable-entries traj-counts)])
                                          (map cons (vector->list keys) (vector->list vals)))]
                     [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) entries)]
                     [top-5 (list-head sorted (min 5 (length sorted)))])
                    (for-each
                     (lambda (entry)
                             (display (format "  ~a: ~a times\n" (car entry) (cdr entry))))
                     top-5)))
        
        ;; Show recent trajectories
        (when (not (null? trajectories))
              (display "\nRecent trajectories:\n")
              (for-each
               (lambda (traj)
                       (display (format "  ~a: ~a\n"
                                        (cdr (assq 'timestamp traj))
                                        (cdr (assq 'paths traj)))))
               (list-head trajectories (min 5 (length trajectories)))))))

;;; iota helper if not available
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Exploration Path Definitions
;;; ============================================================

;;; Each path is: (name weight quick-actions deep-actions)
;;; Weight affects selection probability
;;; Focus on infrastructure and tools, not specific user creations
(define *exploration-paths*
  '((block-store       4 (bx-stats bx-popular)
                 (bx-popular bx-view-random bx-search-random bx-orphans bx-navigate-depth))
    (forum-system      4 (digest channels)
                  (digest-deep browse-channels search-posts post-test))
    (session-mgmt      3 (who session-check)
                  (tier-verification session-persistence survey-system))
    (tutorial-infra    2 (list-tutorials tutorial-status)
                    (tutorial-mechanics step-navigation progress-tracking))
    (graphics-prims    2 (turtle-create turtle-move)
                    (turtle-paths canvas-render drawing-export))
    (command-system    3 (help commands)
                    (help-deep command-routing typo-recovery))
    (query-dsl         3 (query-basic store-search)
               (query-complex filter-chains tag-queries))
    (creative-tools    2 (patch-system templates)
                    (user-creations playpen-loading))))

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
;;; Block Store Exploration
;;; ============================================================

(define (explore-block-store depth)
  (display "\n=== Block Store Exploration ===\n\n")
  (record-finding 'observation 'block-store "Starting block store exploration")
  
  ;; Always start with stats
  (guard (ex [else (record-finding 'issue 'block-store
                                   (format "bx-stats failed: ~a" ex))])
         (bx-stats)
         (record-finding 'success 'block-store "bx-stats executed successfully"))
  
  (when (eq? depth 'deep)
        ;; Try popular blocks
        (guard (ex [else (record-finding 'issue 'block-store
                                         (format "bx-popular failed: ~a" ex))])
               (bx-popular)
               (record-finding 'success 'block-store "bx-popular shows referenced blocks"))
        
        ;; Try viewing a random entry if available
        (guard (ex [else (record-finding 'observation 'block-store
                                         "Could not navigate into block")])
               (let ([blocks (current-block-list)])
                    (when (and blocks (not (null? blocks)))
                          (let ([idx (random (min 5 (length blocks)))])
                               (bx-view idx)
                               (record-finding 'success 'block-store
                                               (format "Navigated to block ~a" idx))))))
        
        ;; Try search with common term
        (let ([search-terms '("post" "message" "lambda" "test" "chat")])
             (guard (ex [else (record-finding 'issue 'block-store
                                              (format "bx-search failed: ~a" ex))])
                    (bx-search (pick-random search-terms))
                    (record-finding 'success 'block-store "Search functionality works")))
        
        ;; Check orphans
        (guard (ex [else (record-finding 'issue 'block-store
                                         (format "bx-orphans failed: ~a" ex))])
               (bx-orphans)
               (let ([orphan-count (length (current-block-list))])
                    (record-finding 'observation 'block-store
                                    (format "Found ~a orphan blocks" orphan-count)))))
  
  (bx-home))

;;; ============================================================
;;; Session Management Exploration
;;; ============================================================

(define (explore-session-mgmt depth)
  (display "\n=== Session Management Exploration ===\n\n")
  (record-finding 'observation 'session "Starting session management exploration")
  
  ;; Check current session
  (guard (ex [else (record-finding 'issue 'session
                                   (format "who command failed: ~a" ex))])
         (who)
         (record-finding 'success 'session "Session info displays correctly"))
  
  ;; Check session persistence
  (guard (ex [else (record-finding 'observation 'session "Session read failed")])
         (let ([session (read-session)])
              (if session
                  (record-finding 'success 'session
                                  (format "Session persists: ~a (~a)"
                                          (cdr (assq 'name session))
                                          (cdr (assq 'tier session))))
                  (record-finding 'observation 'session "No active session"))))
  
  (when (eq? depth 'deep)
        ;; Test survey system
        (guard (ex [else (record-finding 'observation 'session "Survey list unavailable")])
               (list-surveys)
               (record-finding 'success 'session "Survey system accessible"))
        
        ;; Check tier-based access
        (guard (ex [else #f])
               (let ([session (read-session)])
                    (when session
                          (let ([tier (cdr (assq 'tier session))])
                               (record-finding 'observation 'session
                                               (format "Current tier: ~a" tier))))))))

;;; ============================================================
;;; Forum System Exploration
;;; ============================================================

(define (explore-forum-system depth)
  (display "\n=== Forum System Exploration ===\n\n")
  (record-finding 'observation 'forum "Starting forum system exploration")
  
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
;;; Tutorial Infrastructure Exploration
;;; ============================================================

(define (explore-tutorial-infra depth)
  (display "\n=== Tutorial Infrastructure Exploration ===\n\n")
  (record-finding 'observation 'tutorial "Starting tutorial infrastructure exploration")
  
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
;;; Query DSL Exploration
;;; ============================================================

(define (explore-query-dsl depth)
  (display "\n=== Query DSL Exploration ===\n\n")
  (record-finding 'observation 'query "Starting query DSL exploration")
  
  ;; Basic store search
  (guard (ex [else (record-finding 'issue 'query
                                   (format "store-find-by-tag failed: ~a" ex))])
         (let ([results (store-find-by-tag (fs) 'post)])
              (record-finding 'success 'query
                              (format "Tag query returned ~a results" (length results)))))
  
  (when (eq? depth 'deep)
        ;; Test payload search
        (guard (ex [else (record-finding 'observation 'query "Payload search unavailable")])
               (let ([results (store-find-by-payload-contains (fs) "test")])
                    (record-finding 'success 'query
                                    (format "Payload search returned ~a results" (length results)))))
        
        ;; Test query DSL if available
        (guard (ex [else (record-finding 'observation 'query "Query DSL not loaded")])
               (query (fs) '(tag . post))
               (record-finding 'success 'query "Query DSL executes"))
        
        ;; Test graph queries
        (guard (ex [else (record-finding 'observation 'query "Graph queries unavailable")])
               (let ([all-hashes (fs-all-hashes (fs))])
                    (when (not (null? all-hashes))
                          (let ([refs (store-get-refs (fs) (car all-hashes))])
                               (record-finding 'success 'query
                                               (format "Graph query: block has ~a refs"
                                                       (if refs (length refs) 0)))))))))

;;; ============================================================
;;; Graphics Primitives Exploration
;;; ============================================================

(define (explore-graphics-prims depth)
  (display "\n=== Graphics Primitives Exploration ===\n\n")
  (record-finding 'observation 'graphics "Starting graphics primitives exploration")
  
  ;; Load turtle module if not already loaded
  (guard (ex [else #f])
         (load "shell/ui/turtle.ss"))
  
  ;; Create turtle
  (guard (ex [else (record-finding 'issue 'graphics
                                   (format "make-turtle failed: ~a" ex))])
         (define t (make-turtle))
         (record-finding 'success 'graphics "Turtle created successfully")
         
         (when (eq? depth 'deep)
               ;; Basic movements
               (guard (ex [else (record-finding 'issue 'graphics "Movement failed")])
                      (set! t (fd t 50))
                      (set! t (rt t 90))
                      (set! t (fd t 50))
                      (record-finding 'success 'graphics "Basic movement works"))
               
               ;; Draw a shape
               (guard (ex [else (record-finding 'issue 'graphics "Shape drawing failed")])
                      (let loop ([i 0] [turtle t])
                           (if (< i 4)
                               (loop (+ i 1) (rt (fd turtle 30) 90))
                               (set! t turtle)))
                      (record-finding 'success 'graphics "Square drawing works"))
               
               ;; Check position
               (guard (ex [else #f])
                      (let ([x (xcor t)] [y (ycor t)])
                           (record-finding 'observation 'graphics
                                           (format "Final position: (~a, ~a)" x y)))))))

;;; ============================================================
;;; Command System Exploration
;;; ============================================================

(define (explore-command-system depth)
  (display "\n=== Command System Exploration ===\n\n")
  (record-finding 'observation 'commands "Starting command system exploration")
  
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
;;; Creative Tools Exploration
;;; ============================================================

(define (explore-creative-tools depth)
  (display "\n=== Creative Tools Exploration ===\n\n")
  (record-finding 'observation 'creative "Starting creative tools exploration")
  
  ;; Check if patches are available
  (guard (ex [else (record-finding 'observation 'creative "Patch system not loaded")])
         (when (defined? 'apply-patch)
               (record-finding 'success 'creative "Patch system available")))
  
  ;; Check for playpen templates
  (guard (ex [else (record-finding 'observation 'creative "Templates not accessible")])
         (let ([template-files (directory-list "user/templates")])
              (record-finding 'success 'creative
                              (format "Found ~a templates" (length template-files)))))
  
  (when (eq? depth 'deep)
        ;; Test loading a user creation
        (guard (ex [else (record-finding 'observation 'creative
                                         "User creations directory not accessible")])
               (let ([creations (directory-list "user/creations")])
                    (when (not (null? creations))
                          (record-finding 'success 'creative
                                          (format "Found ~a user creations" (length creations))))))
        
        ;; Test playpen loading mechanism
        (guard (ex [else (record-finding 'observation 'creative "Playpen loader not available")])
               (when (defined? 'load-playpen)
                     (record-finding 'success 'creative "Playpen loading infrastructure present")))
        
        ;; Check for creative content in store
        (guard (ex [else #f])
               (let ([art-posts (store-find-by-tag (fs) 'art)])
                    (record-finding 'observation 'creative
                                    (format "Store contains ~a art blocks" (length art-posts)))))))

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
       
       ;; Select exploration paths with diversity bias (1-3 for quick, 2-5 for deep)
       (let* ([num-paths (if (eq? depth 'quick)
                             (+ 1 (random 2))
                             (+ 2 (random 4)))]
              [selected-paths (select-diverse-paths num-paths *exploration-paths*)])
             
             (set! *dogfood-exploration-path* (map car selected-paths))
             
             (display (format "Selected exploration paths: ~a\n" (map car selected-paths)))
             (display "(diversity-optimized based on trajectory history)\n\n")
             
             ;; Run each exploration
             (for-each
              (lambda (path-spec)
                      (let ([path-name (car path-spec)])
                           (display (format "\n>>> Exploring: ~a\n" path-name))
                           (guard (ex [else
                                       (record-finding 'issue 'session
                                                       (format "Path ~a crashed: ~a" path-name ex))])
                                  (case path-name
                                        [(block-store) (explore-block-store depth)]
                                        [(forum-system) (explore-forum-system depth)]
                                        [(session-mgmt) (explore-session-mgmt depth)]
                                        [(tutorial-infra) (explore-tutorial-infra depth)]
                                        [(graphics-prims) (explore-graphics-prims depth)]
                                        [(command-system) (explore-command-system depth)]
                                        [(query-dsl) (explore-query-dsl depth)]
                                        [(creative-tools) (explore-creative-tools depth)]
                                        [else (record-finding 'observation 'session
                                                              (format "Unknown path: ~a" path-name))]))))
              selected-paths)
             
             ;; Save trajectory for future diversity calculations
             (save-trajectory! (map car selected-paths) depth))
       
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

(display "Dogfood Explorer loaded (with trajectory tracking).\n")
(display "  (dogfood-session)        - Run varied exploration\n")
(display "  (dogfood-session 'deep)  - Deep exploration\n")
(display "  (dogfood-session 'quick) - Quick smoke test\n")
(display "  (dogfood-report)         - Show last session findings\n")
(display "  (show-trajectory-stats)  - View trajectory diversity stats\n")
