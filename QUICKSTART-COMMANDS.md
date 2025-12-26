# Quick Start: Command System

## What Was Created

The Structured REPL Command Subsystem (3.2) is now integrated into The Fold.

## Key Files

- **`/home/user/the-fold/shell/commands.ss`** - Main implementation (12KB)
- **`/home/user/the-fold/shell/COMMANDS.md`** - Full documentation
- **`/home/user/the-fold/shell/commands-example.ss`** - Extension examples

## Basic Usage

### Start The Fold
```scheme
(load "shell/repl.ss")
```

### Discover Commands
```scheme
(commands)
```
Shows all registered commands with short descriptions.

### Get Help
```scheme
(help)              ; General help (shows all commands)
(help 'digest)      ; Specific command help
```

### Invoke Commands

Two ways to call commands:

**Method 1: Direct call** (recommended)
```scheme
(version)
(digest)
(who)
(chat "Hello!")
```

**Method 2: Via cmd function** (for programmatic use)
```scheme
(cmd 'version)                    ; => (ok #<void>)
(cmd 'chat "Hello!")              ; => (ok <hash>)
```

### Registered Core Commands

| Command   | Description           |
|-----------|-----------------------|
| `digest`  | Show forum digest     |
| `chat`    | Post to chat          |
| `who`     | Show session info     |
| `bye`     | Logout                |
| `clear`   | Clear screen          |
| `version` | Show system version   |

## Extending the System

### Register a Custom Command

```scheme
(register-command!
 'greet
 "Greet user"
 "Display a friendly greeting.\n  Usage: (greet [name])"
 (lambda args
   (if (null? args)
       (display "Hello!\n")
       (display (format "Hello, ~a!\n" (car args))))
   (void)))

;; Now use it:
(cmd 'greet)
(cmd 'greet "Alice")
```

### Load Example Commands

```scheme
(load "shell/commands-example.ss")
```

This adds:
- `system-info` - Show system information
- `count-words` - Count words in text
- `quick-post` - Quick post to a channel
- `validate-hash` - Validate hash strings
- `safe-divide` - Division with error handling

## Error Handling

The command system includes intelligent error recovery:

```scheme
(cmd 'chatt "test")
; => (error command-error "Unknown command: chatt. Did you mean: chat?")

(cmd 'unknown)
; => (error command-error "Unknown command: unknown. Use (commands) to see all commands.")
```

## Testing

Run the comprehensive demo:
```bash
scheme --quiet --script test-commands-demo.ss
```

## Next Steps

1. **Read documentation**: `/home/user/the-fold/shell/COMMANDS.md`
2. **Try examples**: Load `commands-example.ss`
3. **Create your own**: Use `register-command!` to add custom commands
4. **Full report**: See `IMPLEMENTATION-REPORT.md` for technical details

## Benefits

- Unified command discovery and invocation
- Robust error handling (commands never crash REPL)
- Intelligent typo detection
- Extensible at runtime
- Clean, composable API
