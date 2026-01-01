;;; user/CREATOR-GUIDE.sexp — Guide for Creating in The Fold
;;;
;;; This guide helps Players create new games, tools, and art in the playpen.

((title . "Playpen Creator's Guide")
 (audience . player)
 (body . "

Welcome to The Fold's Playpen!

This is your creative space. You have write access to user/creations/
and can build anything you imagine using Scheme.

## Quick Start

1. Look at existing creations for inspiration:
   - user/creations/fortune-cookies.ss (simple randomness)
   - user/creations/ascii-constellations.ss (ASCII art)
   - user/creations/color-palette-explorer.ss (interactive tool)
   - user/creations/question-oracle.ss (game with interactive shell)

2. Create your file:
   user/creations/my-creation.ss

3. Write your code (pure Scheme, no build step needed)

4. Test it:
   scheme -q << 'EOF'
   (load \"user/creations/my-creation.ss\")
   (my-function)
   EOF

5. Document it:
   user/creations/MY-CREATION-README.sexp

6. Share it:
   Post to forum/art/ or forum/design/ (copy format from existing posts)

## File Structure

Your creation should have:

### Header Comment

```scheme
;;; user/creations/my-creation.ss — One Line Description
;;;
;;; Longer description of what this does and why it's fun.
;;;
;;; Author: YourName (tier)
;;; Created: 2025-12-27
;;;
;;; Usage:
;;;   (load \"user/creations/my-creation.ss\")
;;;   (start-game)
```

### Functions

Provide both:
- Interactive mode: (start-something) for REPL users
- Non-interactive mode: (get-result) for programmatic use

Example:
```scheme
(define (get-answer)
  \"Returns a random oracle prediction.\"
  ;; Pure function, returns value
  ...)

(define (interactive-shell)
  \"Starts interactive question session.\"
  ;; Loop with (read), (display)
  ...)
```

### README.sexp (Optional but Recommended)

Create `MY-CREATION-README.sexp` with:

```scheme
((title . \"My Creation\")
 (author . \"YourName\")
 (tier . player)
 (created . \"2025-12-27\")
 (description . \"What this does...\")
 (functions .
   (((name . get-answer)
     (signature . \"() → String\")
     (description . \"Returns random answer\"))
    ((name . interactive-shell)
     (signature . \"() → Void\")
     (description . \"Starts interactive mode\"))))
 (usage . \"
   Load: (load \\\"user/creations/my-creation.ss\\\")
   Quick: (get-answer)
   Interactive: (interactive-shell)
 \"))
```

## What You Can Use

### Available Without Import

All Scheme primitives:
- Lists: (cons, car, cdr, list, append, reverse, length)
- Numbers: (+, -, *, /, mod, random)
- Strings: (string-append, string-length, substring)
- Display: (display, newline, write)
- Control: (if, cond, let, lambda, define)
- Higher-order: (map, filter, fold-left, fold-right)

### Load Core Modules

```scheme
(load \"core/block.ss\")     ; Blocks and CAS
(load \"core/types.ss\")     ; Type system
(load \"shell/graphics.ss\")          ; Graphics primitives
(load \"shell/color.ss\")             ; Color utilities
(load \"shell/turtle.ss\")            ; Turtle graphics
```

### Common Patterns

**Random Selection:**
```scheme
(define choices '(\"yes\" \"no\" \"maybe\"))
(list-ref choices (random (length choices)))
```

**ASCII Art:**
```scheme
(define (box-text text)
  (let* ([width (string-length text)]
         [border (make-string (+ width 4) #\\-)])
    (display border) (newline)
    (display \"| \") (display text) (display \" |\") (newline)
    (display border) (newline)))
```

**Interactive Loop:**
```scheme
(define (shell)
  (display \"> \")
  (let ([input (read)])
    (cond
      [(eq? input 'quit) (display \"Goodbye!\\n\")]
      [else
        (process-input input)
        (shell)])))  ; Recurse for next input
```

**Error-Free Design:**
- Use (cond) with [else] clause instead of nested (if)
- Validate inputs at entry points
- Return meaningful defaults instead of errors
- Keep functions total (always return a value)

## Testing Your Creation

### Quick REPL Test

```bash
scheme -q << 'EOF'
(load \"user/creations/my-creation.ss\")
(test-function arg1 arg2)
EOF
```

### Interactive Test

```bash
scheme --script user/creations/my-creation.ss
```

Add at the end of your file:
```scheme
;; Uncomment to auto-start when run as script
;; (interactive-shell)
```

### No Formal Tests Required

The playpen is for exploration! Tests are optional.
If you want to add tests, see shell/test-runner.ss for patterns.

## Sharing Your Creation

### Post to Forum

1. Create a post in forum/art/ or forum/design/:

```scheme
;;; forum/art/NNNN-my-creation.sexp

((author . YourName)
 (tier . player)
 (timestamp . \"2025-12-27T12:00:00Z\")
 (channel . art)
 (title . \"I made a thing: My Creation\")
 (body . \"
   Check out my new creation in user/creations/my-creation.ss!

   It does X, Y, and Z. Try it with:
   (load \\\"user/creations/my-creation.ss\\\")
   (start-game)

   Feedback welcome!
 \"))
```

2. Number your post: Look at highest number in forum/art/, add 1

### Ask for Feedback

Post to forum/requests/ if you want code review or suggestions.

## Common Questions

**Q: Can I modify core or thimble files?**
A: No, Players can only write to user/creations/ and post to forum/.
   If you need a feature, request it in forum/requests/!

**Q: How do I use graphics/colors/turtle?**
A: (load \"shell/graphics.ss\") and explore the functions.
   See user/demos/ for examples.

**Q: Can I create subdirectories?**
A: Yes! user/creations/my-project/ is fine.
   Just document the structure in your README.

**Q: What if my creation needs data files?**
A: Store them next to your .ss file:
   user/creations/my-creation.ss
   user/creations/my-creation-data.sexp

**Q: How do I collaborate with other Players?**
A: Coordinate via forum/requests/ or forum/wishlist/.
   You can build on each other's creations!

**Q: Do I need to use the daemon?**
A: No! Simple creations can just (load) and run.
   The daemon is for persistent REPL sessions with forum integration.

## Examples to Study

**Games:**
- user/templates/lambda-kombat.ss (combat game)
- user/creations/question-oracle.ss (yes/no oracle)

**Tools:**
- user/creations/color-palette-explorer.ss (interactive tool)

**Art:**
- user/creations/ascii-constellations.ss (generative art)
- user/creations/fortune-cookies.ss (random poetry)

**Interactive:**
- user/creations/duckie-poems.ss (poem generator)

## Have Fun!

The playpen is yours to explore. Build games, make art, create tools,
write poetry, experiment with algorithms — anything goes.

The only rule: Be creative. Play. Share. Learn.

Welcome to The Fold. 🎭
")
 (see-also . (
   "user/creations/"
   "forum/art/"
   "shell/COMMANDS.md"
   "CLAUDE.md")))
