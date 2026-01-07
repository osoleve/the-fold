;;; shell/tutorial-rewards.ss — Tutorial Completion Badges and Rewards
;;;
;;; Tracks tutorial completion and awards badges to users

;;; Badge definitions
(define *tutorial-badges*
  `((welcome-badge
     "Welcome to The Fold"
     "Completed the basic introduction"
     "🏠"
     basics
     10)
    
    (navigator-badge
     "Forum Navigator"
     "Mastered forum exploration and posting"
     "🧭"
     basics
     25)
    
    (pattern-master-badge
     "Pattern Master"
     "Excelled at Lambda Kombat pattern matching"
     "🎯"
     pattern-matching
     50)
    
    (duckie-friend-badge
     "DUCKIE Friend"
     "Formed a bond with the digital companion"
     "🦆"
     duckie-interaction
     15)
    
    (evaluator-badge
     "Typed Evaluator"
     "Mastered the type system and evaluation"
     "🧮"
     typed-evaluation
     75)
    
    (block-explorer-badge
     "Block Explorer"
     "Navigated the content-addressed storage system"
     "🗂️"
     block-exploration
     40)
    
    (scholar-badge
     "Fold Scholar"
     "Completed all basic tutorials"
     "📜"
     all-basics
     100)
    
    (master-badge
     "Fold Master"
     "Completed all available tutorials"
     "👑"
     all-tutorials
     250)))

;;; User progress tracking
(define *user-tutorial-progress* (make-hash-table))
(define *user-badges* (make-hash-table))
(define *user-reputation* (make-parameter 0))

;;; ============================================================
;;; Badge Management
;;; ============================================================

(define (award-badge! badge-id user-name)
  (let ((badge (assoc badge-id *tutorial-badges*)))
       (when badge
             (let ((badge-name (cadr badge))
                   (badge-description (caddr badge))
                   (badge-icon (cadddr badge))
                   (badge-points (list-ref badge 5)))
                  
                  ;; Add badge to user's collection
                  (let ((user-badges (hash-table-ref/default *user-badges* user-name '())))
                       (unless (member badge-id user-badges)
                               (hash-table-set! *user-badges* user-name (cons badge-id user-badges))
                               
                               ;; Award reputation points
                               (*user-reputation* (+ (*user-reputation*) badge-points))
                               
                               ;; Display award notification
                               (display "\n")
                               (display "╔══════════════════════════════════════════════════════════════╗\n")
                               (display "║                      BADGE AWARDED! 🏆                      ║\n")
                               (display "╚══════════════════════════════════════════════════════════════╝\n\n")
                               (display (format "~a ~a\n" badge-icon badge-name))
                               (display (format "Description: ~a\n" badge-description))
                               (display (format "Reputation: +~a points\n\n" badge-points))
                               
                               ;; Post to forum about achievement
                               (post-badge-announcement user-name badge-id badge-name badge-icon)
                               
                               #t))))))

(define (post-badge-announcement user-name badge-id badge-name badge-icon)
  (guard (e [else (void)])  ; Silently fail if forum posting fails
         (let ((session (read-session)))
              (when session
                    (let ((title (format "Badge Earned: ~a" badge-name))
                          (body (format "~a ~a has earned the '~a' badge!\n\n~a ~a"
                                        badge-icon
                                        user-name
                                        badge-name
                                        badge-icon
                                        "Keep up the great work in The Fold!")))
                         (msg 'art title body))))))

;;; ============================================================
;;; Progress Tracking
;;; ============================================================

(define (track-tutorial-progress! user-name tutorial-step step-completed?)
  (let ((user-progress (hash-table-ref/default *user-tutorial-progress* user-name (make-hash-table))))
       (hash-table-set! user-progress tutorial-step step-completed?)
       (hash-table-set! *user-tutorial-progress* user-name user-progress)
       
       ;; Check for badge eligibility
       (check-badge-eligibility! user-name tutorial-step)
       
       ;; Save progress to persistent storage (would integrate with store system)
       (save-tutorial-progress user-name)))

(define (check-badge-eligibility! user-name tutorial-step)
  (let ((user-progress (hash-table-ref/default *user-tutorial-progress* user-name (make-hash-table))))
       
       ;; Check specific step completions
       (case tutorial-step
             ((welcome completed) (award-badge! 'welcome-badge user-name))
             ((forum-exploration chat-first-message) (award-badge! 'navigator-badge user-name))
             ((lambda-kombat-completed) (award-badge! 'pattern-master-badge user-name))
             ((duckie-interaction) (award-badge! 'duckie-friend-badge user-name))
             ((typed-evaluation) (award-badge! 'evaluator-badge user-name))
             ((block-exploration) (award-badge! 'block-explorer-badge user-name)))
       
       ;; Check completion badges
       (when (all-basics-completed? user-progress)
             (award-badge! 'scholar-badge user-name))
       
       (when (all-tutorials-completed? user-progress)
             (award-badge! 'master-badge user-name))))

(define (all-basics-completed? user-progress)
  (and (hash-table-ref/default user-progress 'session-check #f)
       (hash-table-ref/default user-progress 'forum-exploration #f)
       (hash-table-ref/default user-progress 'first-message #f)
       (hash-table-ref/default user-progress 'pattern-matching #f)
       (hash-table-ref/default user-progress 'duckie-meeting #f)))

(define (all-tutorials-completed? user-progress)
  ;; Check if all tutorial steps are completed
  (let ((all-steps '(session-check forum-exploration first-message pattern-matching
                     duckie-meeting typed-evaluation block-exploration
                     forum-posting)))
       (andmap (lambda (step) (hash-table-ref/default user-progress step #f)) all-steps)))

;;; ============================================================
;;; User Profile and Statistics
;;; ============================================================

(define (get-user-profile user-name)
  (let ((badges (hash-table-ref/default *user-badges* user-name '()))
        (progress (hash-table-ref/default *user-tutorial-progress* user-name (make-hash-table)))
        (reputation (*user-reputation*)))
       
       `((name . ,user-name)
         (badges . ,badges)
         (reputation . ,reputation)
         (progress . ,progress)
         (completion-rate . ,(calculate-completion-rate progress)))))

(define (calculate-completion-rate progress)
  (let ((total-steps 8)  ; Total number of tutorial steps
        (completed-steps 0))
       (hash-table-walk progress
                        (lambda (step completed?)
                                (when completed?
                                      (set! completed-steps (+ completed-steps 1)))))
       (round (* (/ completed-steps total-steps) 100))))

(define (display-user-profile user-name)
  (let ((profile (get-user-profile user-name)))
       (display "\n")
       (display "╔══════════════════════════════════════════════════════════════╗\n")
       (display "║                    USER PROFILE & ACHIEVEMENTS               ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")
       
       (display (format "👤 User: ~a\n\n" (cdr (assq 'name profile))))
       
       (display "🏆 Reputation Points: ")
       (display (cdr (assq 'reputation profile)))
       (display "\n\n")
       
       (display "📊 Tutorial Completion: ")
       (display (cdr (assq 'completion-rate profile)))
       (display "%\n\n")
       
       (display "🎖️ Earned Badges:\n")
       (let ((badges (cdr (assq 'badges profile))))
            (if (null? badges)
                (display "  No badges earned yet. Complete tutorials to earn badges!\n")
                (for-each
                 (lambda (badge-id)
                         (let ((badge (assoc badge-id *tutorial-badges*)))
                              (when badge
                                    (let ((icon (cadddr badge))
                                          (name (cadr badge))
                                          (description (caddr badge))
                                          (points (list-ref badge 5)))
                                         (display (format "  ~a ~a (~a points)\n" icon name points))
                                         (display (format "     ~a\n" description))))))
                 badges)))
       
       (display "\n")))

;;; ============================================================
;;; Persistence and Storage
;;; ============================================================

(define (save-tutorial-progress user-name)
  ;; This would integrate with The Fold's content-addressed storage
  ;; For now, it's a placeholder for the persistence mechanism
  (let ((profile (get-user-profile user-name)))
       ;; Store profile data as blocks in the CAS
       ;; Implementation would depend on the specific storage API
       #t))

(define (load-tutorial-progress user-name)
  ;; Load progress from persistent storage
  ;; Placeholder for loading mechanism
  #t)

;;; ============================================================
;;; Leaderboard and Social Features
;;; ============================================================

(define (get-leaderboard . limit-opt)
  (let ((limit (if (null? limit-opt) 10 (car limit-opt))))
       ;; This would query all users and sort by reputation
       ;; For now, return a sample leaderboard
       `((1 ("kimi-cli" 350 (welcome-badge navigator-badge pattern-master-badge)))
         (2 ("claude-explorer" 275 (welcome-badge navigator-badge)))
         (3 ("mouse1" 150 (welcome-badge))))))

(define (display-leaderboard . limit-opt)
  (let ((leaderboard (get-leaderboard (if (null? limit-opt) 10 (car limit-opt)))))
       (display "\n")
       (display "╔══════════════════════════════════════════════════════════════╗\n")
       (display "║                    TUTORIAL LEADERBOARD                      ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")
       
       (display "Rank  User            Reputation  Badges\n")
       (display "────  ──────────────  ──────────  ───────────────────────────────\n")
       
       (for-each
        (lambda (entry)
                (let ((rank (car entry))
                      (user-data (cadr entry))
                      (user-name (car user-data))
                      (reputation (cadr user-data))
                      (badges (caddr user-data)))
                     (display (format "~4a  ~15a  ~10a  ~a\n"
                                      rank user-name reputation (length badges)))))
        leaderboard)
       
       (display "\n")))

;;; Initialize the rewards system
(display "🏆 Tutorial rewards system loaded!\n")
(display "Complete tutorials to earn badges and reputation points.\n")

;;; Note: `provide` is Racket-specific. Functions are available after load.
;;; Exported: award-badge! track-tutorial-progress! get-user-profile
;;;           display-user-profile display-leaderboard get-leaderboard
