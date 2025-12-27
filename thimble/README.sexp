;;; thimble/README.sexp — The Thimble (Shell/IO Layer)
;;;
;;; This directory contains the defensive shell that wraps the pure core.

((title . "Thimble: The IO Layer")
 (tier-access . builder)
 (purity . "Thimble code is impure, defensive, and effectful")
 (description . "
The Thimble is the shell around the Fabric. It handles:
- File IO (reading, writing, persistence)
- Validation (defensive checks before calling Fabric)
- Capabilities (minting unforgeable authority tokens)
- REPL (interactive environment)
- Tools (developer utilities)
- Error handling (try/catch, timeout, retry)

Everything in thimble/ may:
- Perform IO
- Validate inputs
- Fail, timeout, or raise errors
- Use defensive programming
- Call Fabric with validated inputs
")
 (structure . (
   "REPL & Session Management:"
   "  repl.ss              - Main REPL (loads everything)"
   "  repl-daemon.ss       - Background REPL daemon"
   "  repl-daemon-mcp.ss   - Multi-session IPC daemon"
   "  namespace.ss         - Session isolation"
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
   "  test-runner.ss       - Thimble test runner"
   "  validate.ss          - Input validation"
   ""
   "Utilities:"
   "  format.ss            - Code formatting"
   "  scaffold.ss          - Project scaffolding"
   "  git.ss               - Git integration"
   "  export.ss            - Data export"))
 (philosophy . "
Thimble protects Fabric. It stands between the messy world
(user input, files, network) and the pure core.

Thimble is defensive. Thimble validates. Thimble retries.
Fabric trusts Thimble to only send valid input.

Metaphor:
A thimble protects the finger that pushes the needle through fabric.
The thimble bears the bruises. The needle stays sharp.
")
 (rules . (
   "Validate all inputs before calling Fabric"
   "Return Result types (ok/error) for fallible operations"
   "Use capabilities for authority (fs, network, etc.)"
   "Never expose raw Fabric functions to users"
   "Log errors with context (file, line, operation)"
   "Defensive programming is encouraged here"))
 (for-players . "
Players cannot modify thimble/ but can use its functions.

Key functions available in REPL:
  (help)           - Show available commands
  (digest)         - Forum digest
  (chat \"msg\")     - Post to chat
  (who)            - Show session info

To explore:
  (load \"thimble/block-navigator.ss\")
  (navigate-from hash)
")
 (for-builders . "
Builders may modify thimble/ to add utilities and tools.

How to add a new utility:

1. Create: thimble/my-utility.ss
   ;;; thimble/my-utility.ss — One-line description
   ;;;
   ;;; Dependencies:
   ;;;   - fabric/stitches/block.ss
   ;;;   - thimble/fs.ss

   (load \"fabric/stitches/block.ss\")
   (load \"thimble/fs.ss\")

   (define (my-function arg)
     ;; Validate input
     (unless (valid? arg)
       (error 'my-function \"Invalid argument\" arg))
     ;; Call Fabric
     (fabric-function (normalize arg)))

2. Load in REPL: Edit thimble/repl.ss, add:
   (load \"thimble/my-utility.ss\")

3. Register command (optional): Edit thimble/commands.ss
   (register-command! 'my-cmd \"description\" my-function)

4. Test: thimble/test-my-utility.ss

5. Document: Add to thimble/COMMANDS.md if it's user-facing

See thimble/COMMANDS.md for detailed command patterns.
")
 (for-shepherds . "
Shepherds maintain thimble/ architecture.

Key responsibilities:
- Capability model (fs.ss, capability.ss)
- REPL stability (repl.ss, session-manager.ss)
- Core/Shell boundary (what validates, what computes)

Before adding to thimble/:
- Does this belong in Fabric instead? (is it pure?)
- Does this need a capability? (does it touch OS?)
- Is input validated before calling Fabric?
")
 (capabilities . "
Capabilities are unforgeable authority tokens.

Example (fs.ss):
  (define fs-cap (make-capability 'filesystem \"/path/to/store\"))
  (invoke fs-cap 'read path)  ; fs.ss grants read access

Capabilities:
- Minted only by Thimble
- Passed to Fabric for effect-requiring operations
- Cannot be forged
- Type: (Capability 'name metadata)

See thimble/fs.ss for the canonical example.
")
 (see-also . (
   "thimble/COMMANDS.md"
   "thimble/run-tests.ss"
   "fabric/README.sexp"
   "CLAUDE.md")))
