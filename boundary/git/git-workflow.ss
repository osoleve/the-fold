(load "core/base/prelude.ss")

(doc 'module 'git-workflow)
(doc 'description "Git Workflow Helpers — High-level git operations for common workflows. Builds on boundary/git.ss with convenience functions.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(prelude))
(doc 'note "Features: Smart commit (auto-detects changes), feature branch workflow, conflict resolution helpers, branch management, stash management, safe operations (confirmations).")

(doc 'section 'configuration)

(define *default-branch* "main")
(define *feature-prefix* "feature/")
(define *auto-push* #f)
(define *require-confirmation* #t)

(doc 'section 'quick-operations)

(define (quick-commit msg)
  (doc 'type (-> String Bool))
  (doc 'description "Add all changes and commit with message.")
  (doc 'export #t)
  (display "Adding all changes...\n")
  (git-add-all)
  (display (format "Committing: ~a\n" msg))
  (git-commit msg)
  (when *auto-push*
        (display "Pushing to remote...\n")
        (git-push))
  #t)

(define (checkpoint desc)
  (doc 'type (-> String Bool))
  (doc 'description "Save work-in-progress with timestamp.")
  (doc 'export #t)
  (let ([msg (format "WIP: ~a [~a]" desc (format-timestamp))])
       (quick-commit msg)))

(define (sync)
  (doc 'type (-> Bool))
  (doc 'description "Pull from remote, push local changes.")
  (doc 'export #t)
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

(doc 'section 'feature-branch-workflow)

(define (feature-start name)
  (doc 'type (-> String Bool))
  (doc 'description "Create and switch to new feature branch.")
  (doc 'export #t)
  (let ([branch-name (string-append *feature-prefix* name)])
       (display (format "Creating feature branch: ~a\n" branch-name))
       (git-checkout-new branch-name)
       (display (format "✓ Now on ~a\n" branch-name))
       #t))

(define (feature-finish name)
  (doc 'type (-> String Bool))
  (doc 'description "Merge feature branch into main and clean up.")
  (doc 'export #t)
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

(doc 'section 'undo-operations)

(define (undo-last)
  (doc 'type (-> Bool))
  (doc 'description "Undo last commit but keep changes in working directory.")
  (doc 'export #t)
  (when (and *require-confirmation*
             (not (confirm "Undo last commit? (changes will be kept)")))
        (error 'undo-last "Undo cancelled"))

  (display "Undoing last commit...\n")
  (git-reset-soft "HEAD~1")
  (display "✓ Last commit undone (changes preserved)\n")
  #t)

(define (amend msg)
  (doc 'type (-> String Bool))
  (doc 'description "Amend last commit with new message.")
  (doc 'export #t)
  (when (and *require-confirmation*
             (not (confirm (format "Amend last commit with: ~a?" msg))))
        (error 'amend "Amend cancelled"))

  (display "Amending last commit...\n")
  (git-commit-amend msg)
  (display "✓ Commit amended\n")
  #t)

(define (discard-changes)
  (doc 'type (-> Bool))
  (doc 'description "Discard all uncommitted changes.")
  (doc 'export #t)
  (when (and *require-confirmation*
             (not (confirm "Discard ALL uncommitted changes? THIS CANNOT BE UNDONE!")))
        (error 'discard-changes "Discard cancelled"))

  (display "Discarding all changes...\n")
  (git-reset-hard "HEAD")
  (display "✓ All changes discarded\n")
  #t)

(doc 'section 'stash-operations)

(define (stash-all desc)
  (doc 'type (-> String Bool))
  (doc 'description "Stash all changes with description.")
  (doc 'export #t)
  (display (format "Stashing changes: ~a\n" desc))
  (git-stash-save desc)
  (display "✓ Changes stashed\n")
  #t)

(define (stash-pop)
  (doc 'type (-> Bool))
  (doc 'description "Apply and remove most recent stash.")
  (doc 'export #t)
  (display "Popping stash...\n")
  (git-stash-pop)
  (display "✓ Stash applied\n")
  #t)

(define (stash-list)
  (doc 'type (-> Void))
  (doc 'description "Display all stashes.")
  (doc 'export #t)
  (display "Stashes:\n")
  (git-stash-list))

(doc 'section 'branch-management)

(define (branch-cleanup)
  (doc 'type (-> Bool))
  (doc 'description "Delete branches that have been merged.")
  (doc 'export #t)
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

(define (branch-info)
  (doc 'type (-> Void))
  (doc 'description "Display current branch and status.")
  (doc 'export #t)
  (let ([current (git-current-branch)]
        [status (git-status-summary)])
       (display "================== BRANCH INFO ============================\n")
       (display "\n")
       (display (format "Current Branch: ~a\n" current))
       (display (format "Status: ~a\n" status))
       (display "\n")))

(doc 'section 'conflict-resolution)

(define (has-conflicts? result)
  (doc 'type (-> String Bool))
  (doc 'description "Check if git result contains conflicts.")
  ;; Simplified: check if result contains "CONFLICT"
  (and (string? result)
       (string-contains? result "CONFLICT")))

(define (display-conflicts)
  (doc 'type (-> Void))
  (doc 'description "Display list of conflicting files.")
  (display "\nConflicting files:\n")
  (let ([conflicts (git-list-conflicts)])
       (for-each
        (lambda (file)
                (display (format "  • ~a\n" file)))
        conflicts))
  (display "\nResolve conflicts, then run: (continue-merge)\n"))

(define (continue-merge)
  (doc 'type (-> Bool))
  (doc 'description "Continue merge after resolving conflicts.")
  (doc 'export #t)
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

(doc 'section 'git-command-wrappers)
(doc 'note "These wrap the underlying git operations. In a real implementation, these would call boundary/git/git.ss functions.")

(define (git-add-all)
  (doc 'type (-> Void))
  (system "git add -A"))

(define (git-commit msg)
  (doc 'type (-> String Void))
  (system (format "git commit -m \"~a\"" msg)))

(define (git-commit-amend msg)
  (doc 'type (-> String Void))
  (system (format "git commit --amend -m \"~a\"" msg)))

(define (git-push)
  (doc 'type (-> Void))
  (system "git push"))

(define (git-pull)
  (doc 'type (-> String))
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git pull"))
       (get-output-string output)))

(define (git-checkout branch)
  (doc 'type (-> String Void))
  (system (format "git checkout ~a" branch)))

(define (git-checkout-new branch)
  (doc 'type (-> String Void))
  (system (format "git checkout -b ~a" branch)))

(define (git-merge branch)
  (doc 'type (-> String String))
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system (format "git merge ~a" branch)))
       (get-output-string output)))

(define (git-delete-branch branch)
  (doc 'type (-> String Void))
  (system (format "git branch -d ~a" branch)))

(define (git-reset-soft ref)
  (doc 'type (-> String Void))
  (system (format "git reset --soft ~a" ref)))

(define (git-reset-hard ref)
  (doc 'type (-> String Void))
  (system (format "git reset --hard ~a" ref)))

(define (git-stash-save desc)
  (doc 'type (-> String Void))
  (system (format "git stash save \"~a\"" desc)))

(define (git-stash-pop)
  (doc 'type (-> Void))
  (system "git stash pop"))

(define (git-stash-list)
  (doc 'type (-> Void))
  (system "git stash list"))

(define (git-current-branch)
  (doc 'type (-> String))
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git branch --show-current"))
       (string-trim (get-output-string output))))

(define (git-status-summary)
  (doc 'type (-> String))
  (let ([output (open-output-string)])
       (parameterize ([current-output-port output])
                     (system "git status --short"))
       (let ([result (get-output-string output)])
            (if (string=? result "")
                "clean"
                "modified"))))

(define (git-merged-branches)
  (doc 'type (-> (List String)))
  (doc 'description "Returns list of merged branch names.")
  ;; Returns list of merged branch names
  '())

(define (git-list-conflicts)
  (doc 'type (-> (List String)))
  (doc 'description "Returns list of files with conflicts.")
  ;; Returns list of files with conflicts
  '())

(doc 'section 'utility-functions)

(define (confirm prompt)
  (doc 'type (-> String Bool))
  (doc 'description "Prompt user for confirmation.")
  (display (format "~a [y/N]: " prompt))
  (flush-output-port)
  (let ([response (get-line (current-input-port))])
       (or (string=? response "y")
           (string=? response "Y")
           (string=? response "yes")
           (string=? response "Yes"))))

(define (format-timestamp)
  (doc 'type (-> String))
  (doc 'description "Format current time as timestamp.")
  (let ([t (current-time 'time-utc)])
       (format "~a" (time-second t))))

(doc 'section 'help)

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
