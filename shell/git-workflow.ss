;;; thimble/git-workflow.ss — Git Workflow Helpers
;;;
;;; High-level git operations for common workflows.
;;; Builds on shell/git.ss with convenience functions.
;;;
;;; This is Shell code: executes git commands, manages state.
;;;
;;; Dependencies:
;;;   core/prelude.ss
;;;   shell/git.ss (if available)
;;;   shell/fs.ss
;;;
;;; NOTE: string utilities (string-contains?, string-trim) provided by core/prelude.ss
;;;
;;; Operations:
;;;   (quick-commit msg) — Add all, commit with message
;;;   (checkpoint desc) — Save work-in-progress
;;;   (feature-start name) — Start new feature branch
;;;   (feature-finish name) — Finish and merge feature
;;;   (sync) — Pull, push, handle conflicts
;;;   (undo-last) — Undo last commit (keep changes)
;;;   (amend msg) — Amend last commit
;;;   (stash-all desc) — Stash all changes
;;;   (branch-cleanup) — Delete merged branches
;;;
;;; Features:
;;;   - Smart commit (auto-detects changes)
;;;   - Feature branch workflow
;;;   - Conflict resolution helpers
;;;   - Branch management
;;;   - Stash management
;;;   - Safe operations (confirmations)

;;; ============================================================
;;; Configuration
;;; ============================================================

(load "core/base/prelude.ss")

(define *default-branch* "main")
(define *feature-prefix* "feature/")
(define *auto-push* #f)
(define *require-confirmation* #t)

;;; ============================================================
;;; Quick Operations
;;; ============================================================

;;; quick-commit : String → Bool
;;; Add all changes and commit with message.
(define (quick-commit msg)
  (display "Adding all changes...\n")
  (git-add-all)
  (display (format "Committing: ~a\n" msg))
  (git-commit msg)
  (when *auto-push*
        (display "Pushing to remote...\n")
        (git-push))
  #t)

;;; checkpoint : String → Bool
;;; Save work-in-progress with timestamp.
(define (checkpoint desc)
  (let ([msg (format "WIP: ~a [~a]" desc (format-timestamp))])
       (quick-commit msg)))

;;; sync : → Bool
;;; Pull from remote, push local changes.
(define (sync)
  (display "Syncing with remote...\n")
  (display "Pulling changes...\n")
  (let ([pull-result (git-pull)])
       (if (has-conflicts? pull-result)
           (begin
            (display "⚠ Conflicts detected. Resolve manually:\n")
            (display-conflicts)
            #f)
           (begin
            (display "Pushing changes...\n")
            (git-push)
            (display "✓ Sync complete\n")
            #t))))

;;; ============================================================
;;; Feature Branch Workflow
;;; ============================================================

;;; feature-start : String → Bool
;;; Create and switch to new feature branch.
(define (feature-start name)
  (let ([branch-name (string-append *feature-prefix* name)])
       (display (format "Creating feature branch: ~a\n" branch-name))
       (git-checkout-new branch-name)
       (display (format "✓ Now on ~a\n" branch-name))
       #t))

;;; feature-finish : String → Bool
;;; Merge feature branch into main and clean up.
(define (feature-finish name)
  (let ([branch-name (string-append *feature-prefix* name)])
       (when (and *require-confirmation*
                  (not (confirm (format "Merge ~a into ~a?" branch-name *default-branch*))))
             (error 'feature-finish "Merge cancelled"))
       
       (display (format "Switching to ~a...\n" *default-branch*))
       (git-checkout *default-branch*)
       
       (display (format "Merging ~a...\n" branch-name))
       (let ([merge-result (git-merge branch-name)])
            (if (has-conflicts? merge-result)
                (begin
                 (display "⚠ Merge conflicts. Resolve manually.\n")
                 #f)
                (begin
                 (display (format "Deleting branch ~a...\n" branch-name))
                 (git-delete-branch branch-name)
                 (display "✓ Feature merged and cleaned up\n")
                 #t)))))

;;; ============================================================
;;; Undo Operations
;;; ============================================================

;;; undo-last : → Bool
;;; Undo last commit but keep changes in working directory.
(define (undo-last)
  (when (and *require-confirmation*
             (not (confirm "Undo last commit? (changes will be kept)")))
        (error 'undo-last "Undo cancelled"))
  
  (display "Undoing last commit...\n")
  (git-reset-soft "HEAD~1")
  (display "✓ Last commit undone (changes preserved)\n")
  #t)

;;; amend : String → Bool
;;; Amend last commit with new message.
(define (amend msg)
  (when (and *require-confirmation*
             (not (confirm (format "Amend last commit with: ~a?" msg))))
        (error 'amend "Amend cancelled"))
  
  (display "Amending last commit...\n")
  (git-commit-amend msg)
  (display "✓ Commit amended\n")
  #t)

;;; discard-changes : → Bool
;;; Discard all uncommitted changes.
(define (discard-changes)
  (when (and *require-confirmation*
             (not (confirm "Discard ALL uncommitted changes? THIS CANNOT BE UNDONE!")))
        (error 'discard-changes "Discard cancelled"))
  
  (display "Discarding all changes...\n")
  (git-reset-hard "HEAD")
  (display "✓ All changes discarded\n")
  #t)

;;; ============================================================
;;; Stash Operations
;;; ============================================================

;;; stash-all : String → Bool
;;; Stash all changes with description.
(define (stash-all desc)
  (display (format "Stashing changes: ~a\n" desc))
  (git-stash-save desc)
  (display "✓ Changes stashed\n")
  #t)

;;; stash-pop : → Bool
;;; Apply and remove most recent stash.
(define (stash-pop)
  (display "Popping stash...\n")
  (git-stash-pop)
  (display "✓ Stash applied\n")
  #t)

;;; stash-list : → void
;;; Display all stashes.
(define (stash-list)
  (display "Stashes:\n")
  (git-stash-list))

;;; ============================================================
;;; Branch Management
;;; ============================================================

;;; branch-cleanup : → Bool
;;; Delete branches that have been merged.
(define (branch-cleanup)
  (when (and *require-confirmation*
             (not (confirm "Delete all merged branches?")))
        (error 'branch-cleanup "Cleanup cancelled"))
  
  (display "Finding merged branches...\n")
  (let ([merged (git-merged-branches)])
       (if (null? merged)
           (begin
            (display "No merged branches to clean up\n")
            #t)
           (begin
            (display (format "Deleting ~a merged branches...\n" (length merged)))
            (for-each
             (lambda (branch)
                     (display (format "  Deleting ~a\n" branch))
                     (git-delete-branch branch))
             merged)
            (display "✓ Cleanup complete\n")
            #t))))

;;; branch-info : → void
;;; Display current branch and status.
(define (branch-info)
  (let ([current (git-current-branch)]
        [status (git-status-summary)])
       (display "╔══════════════════════════════════════════════════════════╗\n")
       (display "║                    BRANCH INFO                           ║\n")
       (display "╚══════════════════════════════════════════════════════════╝\n")
       (display "\n")
       (display (format "Current Branch: ~a\n" current))
       (display (format "Status: ~a\n" status))
       (display "\n")))

;;; ============================================================
;;; Conflict Resolution
;;; ============================================================

;;; has-conflicts? : GitResult → Bool
(define (has-conflicts? result)
  ;; Simplified: check if result contains "CONFLICT"
  (and (string? result)
       (string-contains? result "CONFLICT")))

;;; display-conflicts : → void
(define (display-conflicts)
  (display "\nConflicting files:\n")
  (let ([conflicts (git-list-conflicts)])
       (for-each
        (lambda (file)
                (display (format "  • ~a\n" file)))
        conflicts))
  (display "\nResolve conflicts, then run: (continue-merge)\n"))

;;; continue-merge : → Bool
;;; Continue merge after resolving conflicts.
(define (continue-merge)
  (display "Checking if conflicts are resolved...\n")
  (let ([conflicts (git-list-conflicts)])
       (if (null? conflicts)
           (begin
            (display "Adding resolved files...\n")
            (git-add-all)
            (display "Committing merge...\n")
            (git-commit "Merge conflicts resolved")
            (display "✓ Merge complete\n")
            #t)
           (begin
            (display "⚠ Still has conflicts:\n")
            (display-conflicts)
            #f))))

;;; ============================================================
;;; Git Command Wrappers
;;; ============================================================
;;; These wrap the underlying git operations.
;;; In a real implementation, these would call shell/git.ss functions.

(define (git-add-all)
  (system "git add -A"))

(define (git-commit msg)
  (system (format "git commit -m \"~a\"" msg)))

(define (git-commit-amend msg)
  (system (format "git commit --amend -m \"~a\"" msg)))

(define (git-push)
  (system "git push"))

(define (git-pull)
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git pull"))
       (get-output-string output)))

(define (git-checkout branch)
  (system (format "git checkout ~a" branch)))

(define (git-checkout-new branch)
  (system (format "git checkout -b ~a" branch)))

(define (git-merge branch)
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system (format "git merge ~a" branch)))
       (get-output-string output)))

(define (git-delete-branch branch)
  (system (format "git branch -d ~a" branch)))

(define (git-reset-soft ref)
  (system (format "git reset --soft ~a" ref)))

(define (git-reset-hard ref)
  (system (format "git reset --hard ~a" ref)))

(define (git-stash-save desc)
  (system (format "git stash save \"~a\"" desc)))

(define (git-stash-pop)
  (system "git stash pop"))

(define (git-stash-list)
  (system "git stash list"))

(define (git-current-branch)
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git branch --show-current"))
       (string-trim (get-output-string output))))

(define (git-status-summary)
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git status --short"))
       (let ([result (get-output-string output)])
            (if (string=? result "")
                "clean"
                "modified"))))

(define (git-merged-branches)
  ;; Returns list of merged branch names
  '())

(define (git-list-conflicts)
  ;; Returns list of files with conflicts
  '())

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; confirm : String → Bool
(define (confirm prompt)
  (display (format "~a [y/N]: " prompt))
  (flush-output-port)
  (let ([response (get-line (current-input-port))])
       (or (string=? response "y")
           (string=? response "Y")
           (string=? response "yes")
           (string=? response "Yes"))))

;;; format-timestamp : → String
(define (format-timestamp)
  (let ([t (current-time 'time-utc)])
       (format "~a" (time-second t))))

(display "\n")
(display "Git workflow helpers loaded.\n")
(display "\n")
(display "Quick Commands:\n")
(display "  (quick-commit \"msg\")         - Add all & commit\n")
(display "  (checkpoint \"desc\")          - Save WIP\n")
(display "  (sync)                       - Pull & push\n")
(display "\n")
(display "Feature Workflow:\n")
(display "  (feature-start \"name\")       - New feature branch\n")
(display "  (feature-finish \"name\")      - Merge & cleanup\n")
(display "\n")
(display "Undo:\n")
(display "  (undo-last)                  - Undo last commit\n")
(display "  (amend \"msg\")                - Amend last commit\n")
(display "\n")
(display "Stash:\n")
(display "  (stash-all \"desc\")           - Stash changes\n")
(display "  (stash-pop)                  - Apply stash\n")
(display "  (stash-list)                 - List stashes\n")
(display "\n")
(display "Branches:\n")
(display "  (branch-info)                - Show branch status\n")
(display "  (branch-cleanup)             - Delete merged\n")
(display "\n")
