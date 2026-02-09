;;; boundary/README.sexp — The Boundary (IO Layer)
;;;
;;; This directory contains the defensive boundary that wraps the pure core.

((title . "The Boundary: IO Layer")
 (tier-access . builder)
 (purity . "Boundary code is impure, defensive, and effectful")
 (description . "
The boundary wraps core. It handles:
- File IO (reading, writing, persistence)
- Validation (defensive checks before calling core)
- Capabilities (minting unforgeable authority tokens)
- REPL (interactive environment)
- Tools (developer utilities)
- Error handling (try/catch, timeout, retry)

Everything in boundary/ may:
- Perform IO
- Validate inputs
- Fail, timeout, or raise errors
- Use defensive programming
- Call core with validated inputs
")
 (structure . (
   "Root Files (entry points):"
   "  commands.ss          - REPL command registry"
   "  toolkit.ss           - Development toolkit index"
   "  run-tests.ss         - Boundary test runner"
   ""
   "Subdirectories:"
   "  repl/                - REPL & session management"
   "    repl.ss            - Main REPL (loads everything)"
   "    repl-daemon.ss     - Background daemon"
   "    session-manager.ss - Multi-tenant sessions"
   "    history.ss         - Command history"
   ""
   "  io/                  - Core IO operations"
   "    fs.ss              - Filesystem capability layer"
   "    json.ss            - JSON parsing/encoding"
   "    process.ss         - Process management"
   ""
   "  storage/             - Persistence & identity"
   "    cas-persist.ss     - CAS persistence"
   "    store-api.ss       - Store operations"
   "    identity.ss        - Identity management"
   ""
   "  blocks/              - Block system tools"
   "    block-explorer.ss  - Interactive browser"
   "    block-navigator.ss - Navigation"
   "    block-query.ss     - Query interface"
   ""
   "  tools/               - Developer utilities"
   "    edit.ss            - Text editing"
   "    autodoc.ss         - Documentation gen"
   "    refactor-toolkit.ss- Refactoring"
   ""
   "  debug/               - Debugging & errors"
   "    debug-repl.ss      - Time-travel debugger"
   "    error-fmt.ss       - Error formatting"
   ""
   "  diagnostics/         - Profiling & analysis"
   "    profiler-unified.ss- Unified profiler"
   "    fuel-analysis.ss   - Fuel profiling"
   ""
   "  ui/                  - Graphics & visualization"
   "    graphics.ss        - Graphics primitives"
   "    color.ss           - Color utilities"
   "    layout.ss          - Layout combinators"
   ""
   "  tutorial/            - Tutorial system"
   "    tutorial.ss        - Main tutorial"
   ""
   "  bbs/                 - Issue tracker"
   "    bbs.ss             - BBS operations"
   ""
   "  git/                 - Git integration"
   "    git.ss             - Git operations"))
 (philosophy . "
Boundary protects core. It stands between the messy world
(user input, files, network) and the pure core.

Boundary is defensive. Boundary validates. Boundary retries.
Core trusts boundary to only send valid input.
")
 (rules . (
   "Validate all inputs before calling core"
   "Return Result types (ok/error) for fallible operations"
   "Use capabilities for authority (fs, network, etc.)"
   "Never expose raw core functions to users"
   "Log errors with context (file, line, operation)"
   "Defensive programming is encouraged here"))
 (for-players . "
Players cannot modify boundary/ but can use its functions.

Key functions available in REPL:
  (help)           - Show available commands
  (digest)         - Forum digest
  (chat \"msg\")     - Post to chat
  (who)            - Show session info

To explore:
  (load \"boundary/blocks/block-navigator.ss\")
  (navigate-from hash)
")
 (for-builders . "
Builders may modify boundary/ to add utilities and tools.

How to add a new utility:

1. Create in appropriate subdirectory:
   boundary/tools/my-utility.ss
   ;;; boundary/tools/my-utility.ss — One-line description
   ;;;
   ;;; Dependencies:
   ;;;   - core/block.ss
   ;;;   - boundary/io/fs.ss

   (load \"core/block.ss\")
   (load \"boundary/io/fs.ss\")

   (define (my-function arg)
     ;; Validate input
     (unless (valid? arg)
       (error 'my-function \"Invalid argument\" arg))
     ;; Call core
     (core-function (normalize arg)))

2. Load in REPL: Edit boundary/repl/repl.ss, add:
   (load \"boundary/tools/my-utility.ss\")

3. Register command (optional): Edit boundary/commands.ss
   (register-command! 'my-cmd \"description\" my-function)

4. Test: boundary/tests/test-my-utility.ss

5. Document: Add to boundary/COMMANDS.md if it's user-facing

Subdirectory guidelines:
  tools/       - Developer utilities
  io/          - File/network IO
  debug/       - Debugging tools
  diagnostics/ - Profiling/analysis
  ui/          - Graphics/visualization

See boundary/COMMANDS.md for detailed command patterns.
")
 (for-shepherds . "
Shepherds maintain boundary/ architecture.

Key responsibilities:
- Capability model (io/fs.ss)
- REPL stability (repl/repl.ss, repl/session-manager.ss)
- Core/Boundary layer (what validates, what computes)

Before adding to boundary/:
- Does this belong in core instead? (is it pure?)
- Which subdirectory? (tools, io, debug, diagnostics, ui)
- Does this need a capability? (does it touch OS?)
- Is input validated before calling core?
")
 (capabilities . "
Capabilities are unforgeable authority tokens.

Example (io/fs.ss):
  (define fs-cap (make-capability 'filesystem \"/path/to/store\"))
  (invoke fs-cap 'read path)  ; fs.ss grants read access

Capabilities:
- Minted only by boundary
- Passed to core for effect-requiring operations
- Cannot be forged
- Type: (Capability 'name metadata)

See boundary/io/fs.ss for the canonical example.
")
 (see-also . (
   "boundary/COMMANDS.md"
   "boundary/run-tests.ss"
   "core/README.sexp"
   "CLAUDE.md")))
