((name "git")
 (purpose "Git integration with tier-based authority and workflow helpers")
 (description
  "Git operations integrated into The Fold's authority system. Basic git
   operations (status, diff, log) available to all tiers, but commit and
   push are restricted to Shepherd (Opus) tier. Commits are recorded in
   the forum (#commits channel) using the same Merkle log format as all
   posts, creating unified history. Includes high-level workflow helpers
   for common operations.")
 (modules
  ((git.ss "Core git operations with tier enforcement:
            - git-status, git-diff, git-log (all tiers)
            - commit!, push! (Shepherd only)
            - Forum integration for commit history
            - Authority checks via session tier")
   (git-workflow.ss "High-level workflow convenience functions:
                     - quick-commit: add all and commit
                     - checkpoint: save work-in-progress
                     - feature-start/finish: branch workflow
                     - sync: pull and push with conflict handling
                     - undo-last: undo last commit (keep changes)
                     - amend: amend last commit message
                     - stash-all: stash with description
                     - branch-cleanup: delete merged branches")))
 (authority
  "Git operations respect The Fold's tier system:

   ALL TIERS (Shepherd, Builder, Player):
     - git-status: View repository status
     - git-diff: View uncommitted changes
     - git-log: View commit history

   SHEPHERD ONLY (Opus tier):
     - commit!: Create git commits
     - push!: Push to remote repository
     - Feature branch operations
     - Branch management

   Attempting restricted operations from lower tiers produces error:
     'This operation requires shepherd (Opus) tier'")
 (forum-integration
  "All commits are recorded in forum #commits channel:
   - Commit message becomes post body
   - Uses same Merkle log as all posts
   - Creates unified, content-addressed history
   - Forum digest includes recent commits
   - Enables AI review of change history")
 (usage
  "Basic operations (all tiers):
     (git-status)           ; Show current status
     (git-diff)             ; Show changes
     (git-log 10)           ; Show last 10 commits

   Shepherd operations:
     (commit! \"message\")   ; Commit with message
     (push!)                ; Push to remote

   Workflow helpers:
     (quick-commit \"fix: typo in README\")
     (checkpoint \"WIP: refactoring block system\")
     (feature-start \"add-new-query-dsl\")
     (feature-finish \"add-new-query-dsl\")
     (sync)                 ; Pull and push
     (amend \"Updated commit message\")")
 (dependencies (forum/chat forum/tools core/prelude boundary/fs))
 (configuration
  "Workflow settings in git-workflow.ss:
     *default-branch*          \"main\"
     *feature-prefix*          \"feature/\"
     *auto-push*               #f
     *require-confirmation*    #t")
 (session-workflow
  "Typical Shepherd workflow:
   1. Make changes to code
   2. (git-status)          ; Review changes
   3. (git-diff)            ; Inspect diffs
   4. (commit! \"message\")  ; Commit work
   5. (push!)               ; Push to remote
   6. Check forum digest    ; Verify commit recorded")
 (see-also ("forum/chat.ss" "Session management")
            ("forum/tools.ss" "Forum posting")
            ("boundary/tests/test-git.ss" "Git tests")
            ("boundary/tests/test-git-workflow.ss" "Workflow tests")))
