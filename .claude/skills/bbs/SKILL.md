---
name: bbs
description: Manage issues with The Fold's CAS-native BBS (Bulletin Board System). Use for creating, updating, searching, and tracking issues. Invoke when the user wants to create tickets, find work, check issue status, or manage dependencies.
allowed-tools: Bash(./fold:*), Read, Grep, Glob
---

# BBS (Bulletin Board System) Skill

## Overview

BBS is The Fold's CAS-native issue tracker built on block primitives. All issues are content-addressed, with full history preserved in the store.

The API has two layers:
- **Data functions** return values (alists, lists, booleans) — use these programmatically
- **Print functions** (suffixed with `!`) display formatted output — use these at the REPL

## Quick Reference

### Data API (returns values)

| Function | Returns | Example |
|----------|---------|---------|
| `bbs-get` | Issue alist or `#f` | `(bbs-get "fold-001")` |
| `bbs-ready` | List of unblocked IDs | `(bbs-ready)` |
| `bbs-blocked` | List of blocked IDs | `(bbs-blocked)` |
| `bbs-blockers` | `(id . status)` pairs | `(bbs-blockers "fold-001")` |
| `bbs-blocker-ids` | List of blocker IDs | `(bbs-blocker-ids "fold-001")` |
| `bbs-blocking` | List of IDs this blocks | `(bbs-blocking "fold-001")` |
| `bbs-is-blocked?` | Boolean | `(bbs-is-blocked? "fold-001")` |
| `bbs-by-status` | List of IDs | `(bbs-by-status 'open)` |
| `bbs-by-priority` | List of IDs | `(bbs-by-priority 0)` |
| `bbs-by-label` | List of IDs | `(bbs-by-label 'sft)` |
| `bbs-by-type` | List of IDs | `(bbs-by-type 'epic)` |
| `bbs-all-ids` | All issue IDs | `(bbs-all-ids)` |
| `bbs-labels` | All unique labels | `(bbs-labels)` |
| `bbs-stats` | Statistics alist | `(bbs-stats)` |
| `bbs-get-history` | List of version alists | `(bbs-get-history "fold-001")` |
| `bbs-get-block` | Raw issue Block or `#f` | `(bbs-get-block "fold-001")` |
| `bbs-issue-count` | Integer | `(bbs-issue-count)` |
| `bbs-issue-exists?` | Boolean | `(bbs-issue-exists? "fold-001")` |

### Mutations (return hashes/IDs)

| Function | Returns | Example |
|----------|---------|---------|
| `bbs-create` | Issue ID string | `(bbs-create "Title" 'priority 2 'type 'task)` |
| `bbs-update` | New hash | `(bbs-update 'fold-001 'status 'in_progress)` |
| `bbs-close` | Hash or `#f` | `(bbs-close 'fold-001 'reason "Done")` |
| `bbs-reopen` | Hash or `#f` | `(bbs-reopen 'fold-001)` |
| `bbs-comment` | Hash | `(bbs-comment "fold-001" "Note text")` |
| `bbs-dep` | Void | `(bbs-dep 'blocker 'blocked)` |
| `bbs-undep` | Void | `(bbs-undep 'blocker 'blocked)` |

### Display (print to stdout, return void)

| Function | Example |
|----------|---------|
| `bbs-show!` | `(bbs-show! 'fold-001)` |
| `bbs-list!` | `(bbs-list! 'status 'closed)` |
| `bbs-ready!` | `(bbs-ready!)` |
| `bbs-blocked!` | `(bbs-blocked!)` |
| `bbs-history!` | `(bbs-history! 'fold-001)` |
| `bbs-blockers!` | `(bbs-blockers! 'fold-001)` |
| `bbs-find!` | `(bbs-find! "query")` |
| `bbs-search!` | `(bbs-search! "query")` |
| `bbs-list-by-label!` | `(bbs-list-by-label! 'topology)` |
| `bbs-list-by-type!` | `(bbs-list-by-type! 'epic)` |
| `bbs-label-report!` | `(bbs-label-report!)` |
| `bbs-gc!` | `(bbs-gc!)` |

## Instructions

### Getting Issue Data

```bash
# Get issue as alist (programmatic access)
./fold "(bbs-get \"fold-001\")"
# Returns: ((id . "fold-001") (title . "...") (status . open) (priority . 2) ...)

# Get unblocked issue IDs
./fold "(bbs-ready)"

# Get blockers with their status
./fold "(bbs-blockers \"fold-002\")"
# Returns: (("fold-001" . open) ("fold-003" . closed))
```

### Displaying Issues (REPL)

```bash
# Display formatted issue
./fold "(bbs-show! 'fold-001)"

# List open issues
./fold "(bbs-list!)"

# List closed issues
./fold "(bbs-list! 'status 'closed)"

# Search titles + descriptions
./fold "(bbs-search! \"authentication\")"

# Show unblocked work
./fold "(bbs-ready!)"
```

### Creating Issues

```bash
# Basic create (just title)
./fold "(bbs-create \"Fix authentication bug\")"

# With priority (0=critical, 2=medium, 4=backlog)
./fold "(bbs-create \"Urgent fix\" 'priority 0)"

# With type (task, bug, feature, epic)
./fold "(bbs-create \"Add dark mode\" 'type 'feature)"

# Full creation with all options
./fold "(bbs-create \"Refactor auth\" 'description \"Detailed description here\" 'priority 1 'type 'task 'labels '(core security))"
```

### Updating and Closing Issues

```bash
# Update status
./fold "(bbs-update 'fold-001 'status 'in_progress)"

# Change priority
./fold "(bbs-update 'fold-001 'priority 0)"

# Close with reason
./fold "(bbs-close 'fold-001 'reason \"Fixed in commit abc123\")"
```

### Managing Dependencies

```bash
# fold-001 blocks fold-002
./fold "(bbs-dep 'fold-001 'fold-002)"

# What blocks this issue?
./fold "(bbs-blockers \"fold-002\")"

# What does this issue block?
./fold "(bbs-blocking 'fold-001)"
```

## Field Reference

### Priority Levels

| Priority | Meaning |
|----------|---------|
| 0 | Critical - drop everything |
| 1 | High - do soon |
| 2 | Medium (default) |
| 3 | Low - when time permits |
| 4 | Backlog - someday maybe |

### Issue Types

| Type | Use For |
|------|---------|
| `task` | General work items |
| `bug` | Defects to fix |
| `feature` | New functionality |
| `epic` | Large multi-issue efforts |

### Status Values

| Status | Meaning |
|--------|---------|
| `open` | Not yet started |
| `in_progress` | Currently being worked |
| `closed` | Completed or won't fix |

## Notes

- **Issue IDs**: Can be symbols (`'fold-001`) or strings (`"fold-001"`)
- **Auto-initialization**: BBS initializes automatically when the REPL starts
- **CAS Storage**: All issues are content-addressed in `.store/`
- **Head files**: Current issue state tracked in `.store/heads/bbs/`
- **Counter**: Issue counter in `.bbs/counter`

## Pipeline Integration

BBS effects are available in agent pipelines (`lattice/pipeline/effects.ss`):

```scheme
(bbs-create "title")              ; Create issue, return ID
(bbs-create-full title desc type priority)
(bbs-update id updates-alist)     ; Update issue fields
(bbs-close id)                    ; Close issue
(bbs-ready)                       ; Get unblocked issues
(bbs-show id)                     ; Get issue details
```

Note: Pipeline effect names match the data API, not the display API.

## File Locations

| Path | Purpose |
|------|---------|
| `.store/heads/bbs/` | Issue head files (current hash per issue) |
| `.bbs/counter` | Next issue number |
| `.bbs/deps` | Dependency relationships |
| `boundary/bbs/bbs.ss` | Entry point and display functions |
| `boundary/bbs/store.ss` | Data accessors (bbs-get, bbs-get-history) |
| `boundary/bbs/ops.ss` | Mutations (create, update, close) |
| `boundary/bbs/index.ss` | Query index (by-status, ready, blocked) |
