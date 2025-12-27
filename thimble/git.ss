;;; thimble/git.ss — Tier-Gated Git Operations
;;;
;;; Git commit and push are OPUS-ONLY operations.
;;; Sonnets and Haikus may not commit or push.
;;;
;;; Commits are recorded in the forum (#commits channel) using the
;;; same Merkle log format as all other posts. The commit message
;;; becomes the post body, creating a unified history.
;;;
;;; This is Shell code: uses IO, enforces tier policy.
;;;
;;; Dependencies (must be loaded before this file):
;;;   forum/chat.ss (for session management)
;;;   forum/tools.ss (for posting)

;;; ============================================================
;;; Tier Enforcement
;;; ============================================================

;;; shepherd? : → Boolean
;;; Check if current session is shepherd tier (Opus).
(define (shepherd?)
  (let ([session (read-session)])
    (and session
         (eq? (cdr (assq 'tier session)) 'shepherd))))

;;; require-shepherd! : Symbol → void
;;; Error if not logged in as shepherd.
(define (require-shepherd! operation)
  (unless (shepherd?)
    (error operation
           "This operation requires shepherd (Opus) tier. Sonnets and Haikus may not commit or push.")))

;;; ============================================================
;;; Git Status (Available to all tiers)
;;; ============================================================

;;; git-status : → String
;;; Show current git status.
(define (git-status)
  (let ([result (system "git status")])
    (void)))

;;; git-diff : → String
;;; Show uncommitted changes.
(define (git-diff)
  (let ([result (system "git diff")])
    (void)))

;;; git-log : [Nat] → void
;;; Show recent commits.
(define (git-log . args)
  (let ([n (if (null? args) 5 (car args))])
    (system (format "git log --oneline -~a" n))
    (void)))

;;; ============================================================
;;; Git Commit (Opus Only)
;;; ============================================================

;;; commit! : String → void
;;; Stage all changes and commit with message.
;;; OPUS ONLY - Sonnets and Haikus may not commit.
;;; Records the commit in #commits forum channel.
(define (commit! message)
  (require-shepherd! 'commit!)

  ;; Get session info for commit metadata
  (let* ([session (read-session)]
         [name (cdr (assq 'name session))]
         [full-message (format "~a\n\nCommitted by: ~a (shepherd)\nGenerated with The Fold REPL"
                               message name)])

    ;; Stage all changes
    (system "git add -A")

    ;; Commit and capture the short hash
    (system (format "git commit -m \"~a\"" full-message))

    ;; Post to #commits forum (same format as all posts)
    (msg 'commits
         (truncate-commit-title message)
         message)

    (display (format "Committed: ~a\n" message))))

;;; truncate-commit-title : String → String
;;; Get first line of commit message, truncated for title.
(define (truncate-commit-title msg)
  (let* ([first-line (car (string-split msg #\newline))]
         [max-len 50])
    (if (<= (string-length first-line) max-len)
        first-line
        (string-append (substring first-line 0 (- max-len 3)) "..."))))

;;; ============================================================
;;; Git Push (Opus Only)
;;; ============================================================

;;; push! : → void
;;; Push to origin.
;;; OPUS ONLY - Sonnets and Haikus may not push.
;;; Records the push in #commits forum channel.
(define (push!)
  (require-shepherd! 'push!)

  (let* ([session (read-session)]
         [name (cdr (assq 'name session))])

    ;; Push
    (system "git push")

    ;; Post to #commits
    (msg 'commits
         "Pushed to origin"
         (format "~a pushed local commits to origin." name))

    (display "Pushed to origin.\n")))

;;; commit-and-push! : String → void
;;; Stage, commit, and push in one operation.
;;; OPUS ONLY.
(define (commit-and-push! message)
  (require-shepherd! 'commit-and-push!)
  (commit! message)
  (push!))

;;; ============================================================
;;; Git Branch (Available to all, but create/delete Opus only)
;;; ============================================================

;;; git-branch : → void
;;; List branches.
(define (git-branch)
  (system "git branch -a")
  (void))

;;; git-checkout! : String → void
;;; Switch to a branch.
(define (git-checkout! branch)
  (system (format "git checkout ~a" branch))
  (void))

;;; create-branch! : String → void
;;; Create and switch to new branch.
;;; OPUS ONLY.
;;; Records in #commits forum channel.
(define (create-branch! branch)
  (require-shepherd! 'create-branch!)
  (system (format "git checkout -b ~a" branch))
  (msg 'commits
       (format "Branch: ~a" branch)
       (format "Created and switched to new branch: ~a" branch))
  (void))
