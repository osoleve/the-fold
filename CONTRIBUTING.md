# Contributing to The Fold

## Development Philosophy

The Fold follows a **forum-based progress tracking** approach using the TodoWrite tool rather than proliferating markdown documentation files.

### ✅ DO: Use the Forum (TodoWrite)

**When working on features or fixes:**

1. **Create tasks** using TodoWrite at the start of work
2. **Update task status** as you progress (pending → in_progress → completed)
3. **Post progress updates** via TodoWrite instead of creating docs
4. **Share summaries** in the forum when work is complete

Example workflow:
```scheme
;; Start work
(TodoWrite [
  {"content": "Implement feature X", "status": "in_progress"},
  {"content": "Add tests for feature X", "status": "pending"}
])

;; ... do work ...

;; Update progress
(TodoWrite [
  {"content": "Implement feature X", "status": "completed"},
  {"content": "Add tests for feature X", "status": "in_progress"}
])
```

### ❌ DON'T: Create Documentation Explosions

**Avoid creating:**
- Multiple markdown files in the project root
- Redundant API documentation
- Implementation summaries as separate files
- Quick-start guides as standalone docs
- Architecture diagrams as text files

**Instead:**
- Use inline code comments for API documentation
- Use the forum (TodoWrite) for progress summaries
- Create ONE module-level doc in the module directory (e.g., `shell/LAYERS.md`)
- Keep root directory clean

### Documentation Guidelines

**When documentation IS appropriate:**

1. **Module API References** - ONE file per module in its directory
   - Example: `shell/color.ss` → `shell/COLOR.md` (if needed)
   - Keep it concise, focus on public API
   - Include type signatures and examples

2. **Architecture Decisions** - Update existing `ARCHITECTURE.md` (if it exists)
   - Don't create new files for every decision
   - Use inline comments for most architectural notes

3. **README** - Only for project overview
   - How to run the project
   - High-level architecture
   - Links to key modules

**Examples:**

✅ GOOD:
```
the-fold/
├─ shell/
│  ├─ color.ss
│  └─ LAYERS.md         # ONE doc for layers module
├─ CONTRIBUTING.md      # This file
└─ ARCHITECTURE.md      # High-level only
```

❌ BAD:
```
the-fold/
├─ COLOR-SYSTEM.md
├─ COLOR-API-REFERENCE.md
├─ COLOR-IMPLEMENTATION-SUMMARY.md
├─ INTEGRATION-GUIDE.md
├─ QUICKSTART-LAYERS.md
├─ LAYERING-ARCHITECTURE.txt
├─ LAYERING-SYSTEM-SUMMARY.md
└─ ... (documentation explosion!)
```

### Code Style

The Fold follows functional programming principles:

1. **Pure Functions** - No side effects in core logic
2. **Immutable Data** - All data structures are immutable
3. **Clear Separation** - Core (pure) / Shell (impure) / Playpen (experimental)

See inline documentation in source files for specific conventions.

### Git Workflow

1. Work on feature branches named `claude/<feature>-<session-id>`
2. Use descriptive commit messages following conventional commits
3. Push to remote before requesting reviews
4. Create PRs when features are complete

### Agent/Sonnet Instructions

If you're a Claude agent working on this project:

1. **ALWAYS use TodoWrite** for progress tracking
2. **Post summaries to the forum** instead of creating markdown files
3. **Only create docs** if absolutely necessary (API references in module dirs)
4. **Ask the user** if you're unsure whether documentation is needed
5. **Clean up after yourself** - don't leave temporary files

### Questions?

The forum (TodoWrite) is your friend. When in doubt, use it to:
- Ask questions
- Share progress
- Document decisions
- Track work

Keep the repository clean and the forum active! 🎉
