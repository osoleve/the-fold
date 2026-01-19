;;; boundary/README.sexp — The Shell (IO Layer)
;;;
;;; This directory contains the defensive shell that wraps the pure core.

((title . "The Shell: IO Layer")
 (tier-access . builder)
 (purity . "Shell code is impure, defensive, and effectful")
 (description . "
The shell wraps core. It handles:
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
   "REPL & Session Management:"
   "  repl.ss              - Main REPL (loads everything)"
   "  repl-daemon.ss       - Background REPL daemon"
   "  repl-daemon-mcp.ss   - Session broker daemon"
   "  repl-worker.ss       - Per-session worker process"
   "  session-manager.ss   - Multi-tenant session handling"
   ""
   "Core IO:"
   "  fs.ss                - Filesystem capability layer"
   "  text.ss              - Encoding hygiene, Glitchling quarantine"
   "  cas-persist.ss       - Persist CAS to disk"
   ""
   "Developer Tools:"
   "  commands.ss          - Command registry"
   "  block-navigator.ss   - Block exploration"
   "  block-explorer.ss    - Interactive block browser"
   "  type-inspect.ss      - Type system introspection"
   "  benchmark.ss         - Performance measurement"
   "  coverage.ss          - Code coverage analysis"
   "  fuel-profile.ss      - Fuel consumption profiling"
   ""
   "Graphics & Visualization:"
   "  graphics.ss          - Graphics primitives"
   "  turtle.ss            - Turtle graphics"
   "  color.ss             - Color utilities"
   "  layers.ss            - Layer composition"
   ""
   "Testing & Quality:"
   "  test-runner.ss       - Shell test runner"
   "  validate.ss          - Input validation"
   ""
   "Utilities:"
   "  format.ss            - Code formatting"
   "  scaffold.ss          - Project scaffolding"
   "  git.ss               - Git integration"
   "  export.ss            - Data export"))
 (philosophy . "
Shell protects core. It stands between the messy world
(user input, files, network) and the pure core.

Shell is defensive. Shell validates. Shell retries.
Core trusts shell to only send valid input.
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
  (load \"boundary/block-navigator.ss\")
  (navigate-from hash)
")
 (for-builders . "
Builders may modify boundary/ to add utilities and tools.

How to add a new utility:

1. Create: boundary/my-utility.ss
   ;;; boundary/my-utility.ss — One-line description
   ;;;
   ;;; Dependencies:
   ;;;   - core/block.ss
   ;;;   - boundary/fs.ss

   (load \"core/block.ss\")
   (load \"boundary/fs.ss\")

   (define (my-function arg)
     ;; Validate input
     (unless (valid? arg)
       (error 'my-function \"Invalid argument\" arg))
     ;; Call core
     (core-function (normalize arg)))

2. Load in REPL: Edit boundary/repl.ss, add:
   (load \"boundary/my-utility.ss\")

3. Register command (optional): Edit boundary/commands.ss
   (register-command! 'my-cmd \"description\" my-function)

4. Test: boundary/test-my-utility.ss

5. Document: Add to boundary/COMMANDS.md if it's user-facing

See boundary/COMMANDS.md for detailed command patterns.
")
 (for-shepherds . "
Shepherds maintain boundary/ architecture.

Key responsibilities:
- Capability model (fs.ss, capability.ss)
- REPL stability (repl.ss, session-manager.ss)
- Core/Shell boundary (what validates, what computes)

Before adding to boundary/:
- Does this belong in core instead? (is it pure?)
- Does this need a capability? (does it touch OS?)
- Is input validated before calling core?
")
 (capabilities . "
Capabilities are unforgeable authority tokens.

Example (fs.ss):
  (define fs-cap (make-capability 'filesystem \"/path/to/store\"))
  (invoke fs-cap 'read path)  ; fs.ss grants read access

Capabilities:
- Minted only by shell
- Passed to core for effect-requiring operations
- Cannot be forged
- Type: (Capability 'name metadata)

See boundary/fs.ss for the canonical example.
")
 (see-also . (
   "boundary/COMMANDS.md"
   "boundary/run-tests.ss"
   "core/README.sexp"
   "CLAUDE.md")))
