;;; shell/git.ss — Git Operations
;;;
;;; Git operations for the Fold.
;;;
;;; This is Shell code: uses IO for git commands.

;;; ============================================================
;;; Git Status
;;; ============================================================

;;; git-status : → void
;;; Show current git status.
(define (git-status)
  (system "git status")
  (void))

;;; git-diff : → void
;;; Show uncommitted changes.
(define (git-diff)
  (system "git diff")
  (void))

;;; git-log : [Nat] → void
;;; Show recent commits.
(define (git-log . args)
  (let ([n (if (null? args) 5 (car args))])
       (system (format "git log --oneline -~a" n))
       (void)))

;;; ============================================================
;;; Git Commit
;;; ============================================================

;;; commit! : String → void
;;; Stage all changes and commit with message.
(define (commit! message)
  ;; Stage all changes
  (system "git add -A")
  ;; Commit
  (system (format "git commit -m \"~a\"" message))
  (display (format "Committed: ~a\n" message)))

;;; truncate-commit-title : String → String
;;; Get first line of commit message, truncated for title.
(define (truncate-commit-title msg)
  (let* ([first-line (car (string-split msg #\newline))]
         [max-len 50])
        (if (<= (string-length first-line) max-len)
            first-line
            (string-append (substring first-line 0 (- max-len 3)) "..."))))

;;; ============================================================
;;; Git Push
;;; ============================================================

;;; push! : → void
;;; Push to origin.
(define (push!)
  (system "git push")
  (display "Pushed to origin.\n"))

;;; commit-and-push! : String → void
;;; Stage, commit, and push in one operation.
(define (commit-and-push! message)
  (commit! message)
  (push!))

;;; ============================================================
;;; Git Branch
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
(define (create-branch! branch)
  (system (format "git checkout -b ~a" branch))
  (void))
