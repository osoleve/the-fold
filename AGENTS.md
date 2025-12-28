# Agent Instructions

This project uses **bd** (beads) for issue tracking — a dependency-aware issue tracker designed for AI-supervised workflows.

## What is beads?

Beads chains issues together like beads on a string, with dependency relationships that prevent agents from duplicating effort. It's specifically designed for AI agents working collaboratively.

## Quick Reference

```bash
# Finding and claiming work
bd ready                        # Show unblocked work ready to claim
bd show <id>                    # View issue details
bd update <id> --status in_progress  # Claim work (mark as in progress)

# Creating and managing issues
bd create "Issue title"         # Create new issue
bd create "Title" -d "Description" -p 0 -t feature  # Create with details
bd list                         # List all issues
bd list --status open           # List open issues
bd list --priority 0            # List by priority (0-4, 0=highest)

# Dependencies
bd dep add <id1> <id2>          # Make id2 block id1 (id2 must complete first)
bd dep tree <id>                # Visualize dependency tree
bd dep cycles                   # Detect circular dependencies

# Completing work
bd close <id>                   # Complete work
bd close <id> --reason "Fixed in PR #42"  # Close with reason
bd sync                         # Sync with git (auto-export/import)

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

## Using beads for AI Collaboration

### Dependency-Aware Workflows

Beads prevents work conflicts through dependencies:
- **blocks**: Task B must complete before task A can start
- **related**: Soft connection, doesn't block progress  
- **parent-child**: Epic/subtask hierarchical relationship
- **discovered-from**: Auto-created when AI discovers related work

### Agent Integration Features

- **Ready Work Detection**: `bd ready` shows only unblocked work
- **JSON Output**: Use `--json` flags for programmatic parsing
- **Auto-sync**: Git integration keeps issues synced across machines
- **Database Extension**: Applications can extend SQLite for custom workflows

### Best Practices for Agents

1. **Create issues for discovered work**: When you find something that needs follow-up
2. **Check dependencies before starting**: Use `bd dep tree <id>` to understand blockers
3. **Claim work immediately**: Update status to `in_progress` to prevent conflicts
4. **File issues for incomplete work**: Before ending session, create issues for remaining tasks
5. **Use descriptive titles**: Help other agents understand the work context

### Database and Sync

Beads auto-discovers your database:
1. `--db /path/to/db.db` flag
2. `$BEADS_DB` environment variable  
3. `.beads/*.db` in current directory or ancestors
4. `~/.beads/default.db` as fallback

Git workflow (auto-sync):
- ✓ Export to JSONL after CRUD operations (5s debounce)
- ✓ Import from JSONL when newer than DB (after git pull)
- ✓ Works seamlessly across machines and team members
- No manual export/import needed!

Disable with: `--no-auto-flush` or `--no-auto-import`

