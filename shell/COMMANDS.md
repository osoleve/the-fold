# Structured REPL Command Subsystem

## Overview

The Structured REPL Command Subsystem provides a unified framework for command discovery, routing, and error recovery in The Fold REPL. Commands are registered in a central registry with metadata including help text and handler functions.

## Location

- **Implementation**: `shell/commands.ss`
- **Integration**: `shell/repl.ss`

## Features

### 1. Command Registry

Commands are registered with metadata:
```scheme
(register-command! 'name "Short help" "Long help" handler-fn)
(unregister-command! 'name)
```

### 2. Command Discovery

```scheme
(commands)          ; List all registered commands with short help
(help)              ; Show general help
(help 'cmd-name)    ; Show detailed help for specific command
```

### 3. Command Routing

```scheme
(cmd 'name args...) ; Invoke command by name
```

Commands return:
- Success: `(ok result)`
- Failure: `(error 'command-error "message")`

### 4. Error Recovery

- Commands that fail return error results instead of crashing REPL
- Typo detection with "did you mean?" suggestions using edit distance
- Helpful error messages for unknown commands

### 5. Core Commands

The following commands are registered by default:

| Command   | Description           | Usage                    |
|-----------|-----------------------|--------------------------|
| `digest`  | Show forum digest     | `(cmd 'digest)`          |
| `chat`    | Post to chat          | `(cmd 'chat "message")`  |
| `who`     | Show session info     | `(cmd 'who)`             |
| `bye`     | Logout                | `(cmd 'bye)`             |
| `clear`   | Clear screen          | `(cmd 'clear)`           |
| `version` | Show system version   | `(cmd 'version)`         |

## Usage Examples

### List all commands
```scheme
(commands)
```

### Get help
```scheme
(help)              ; General help
(help 'digest)      ; Help on digest command
```

### Invoke commands
```scheme
(cmd 'version)                    ; => (ok #<void>)
(cmd 'chat "Hello, world!")       ; => (ok <hash>)
(cmd 'unknown)                    ; => (error 'command-error "...")
(cmd 'chatt "test")               ; => (error 'command-error "Did you mean: chat?")
```

### Register custom command
```scheme
(register-command!
 'greet
 "Greet the user"
 "Display a friendly greeting.\n  Usage: (cmd 'greet [name])"
 (lambda args
   (if (null? args)
       (display "Hello!\n")
       (display (format "Hello, ~a!\n" (car args))))
   (void)))
```

### Direct function calls

All registered commands are also available as direct Scheme functions:
```scheme
(digest)            ; Same as (cmd 'digest)
(chat "Hello!")     ; Same as (cmd 'chat "Hello!")
(version)           ; Same as (cmd 'version)
```

## Architecture

### Command Record Structure
```scheme
((name . Symbol)
 (short-help . String)
 (long-help . String)
 (handler . Procedure))
```

### Registry Implementation
- Hash table mapping symbol → command record
- `*command-registry*` global variable
- R6RS hashtable with `symbol-hash` and `eq?`

### Error Handling
- All handler invocations wrapped in `guard`
- Converts exceptions to `(error 'command-error msg)`
- Preserves REPL stability

### Typo Detection
- Levenshtein edit distance algorithm
- Suggests commands within distance ≤ 2
- Returns first matching candidate

## Integration with REPL

The command system is loaded automatically by `shell/repl.ss`:

1. `commands.ss` is loaded after all dependencies
2. Core commands are auto-registered on load
3. `help` function enhanced to support command-specific help
4. Convenience wrappers added for direct function calls

## Testing

Run the comprehensive demo:
```bash
scheme --quiet --script test-commands-demo.ss
```

Run tests:
```bash
scheme --quiet --script test-commands.ss
scheme --quiet --script test-commands-advanced.ss
```

## Future Enhancements

- Command aliases (e.g., `h` for `help`)
- Command categories for organized help display
- Command completion hints
- Command history and favorites
- Permission-based command access (tier-specific commands)
- Async command execution for long-running operations
