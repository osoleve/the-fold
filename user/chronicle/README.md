# Chronicle — Interactive Narrative DSL

Chronicle is a tagless final DSL for building interactive narratives, choose-your-own-adventure stories, and branching dialogues. It uses the tagless final pattern to support multiple interpretations of the same story: runtime execution, validation, graph visualization, and complexity analysis.

## Quick Start

```scheme
(load "user/chronicle/tagless-chronicle.ss")

;; Create a simple story
(define (my-story d)
  (tl-scene d 'start
            "You wake up in a mysterious room."
            (tl-choice d "Open the door" 'hallway
                      (tl-guard-always d)
                      (tl-effect-flag d 'explored #t))
            (tl-choice d "Search the room" 'treasure
                      (tl-guard-always d)
                      (tl-effect-item d 'add 'key))))

;; Run it
(define runtime (make-chronicle-runtime my-story 'start))
(display (chronicle-runtime-text runtime))
```

## Core Concepts

### Tagless Final Pattern

Chronicle uses the **tagless final** approach where stories are defined as functions that take an interpreter dictionary. This enables:

- **Multiple interpretations**: Run the same story with different interpreters
- **Type safety**: No AST manipulation required
- **Extensibility**: Add new operations without modifying existing code
- **Optimization**: Different interpreters can optimize differently

### Story Elements

A Chronicle story consists of:

1. **Scenes** — Story locations/moments with descriptive text
2. **Choices** — Player decisions that transition between scenes
3. **Guards** — Conditions that control choice visibility
4. **Effects** — State changes when choices are made

## API Reference

### Scene Definition

```scheme
(tl-scene dict id text choice1 choice2 ...)
```

Creates a scene with:
- `id` — Symbol identifying this scene
- `text` — String describing the scene
- `choice1, choice2, ...` — Available choices

**Example:**
```scheme
(tl-scene d 'entrance
          "You stand before a mysterious door."
          (tl-choice d "Try the door" 'hallway ...)
          (tl-choice d "Leave" 'outside ...))
```

### Choice Definition

```scheme
(tl-choice dict text target guard effect)
```

Creates a choice with:
- `text` — String shown to player
- `target` — Symbol of destination scene
- `guard` — Condition for visibility
- `effect` — State change when chosen

**Example:**
```scheme
(tl-choice d "Use the key" 'hallway
          (tl-guard-item d 'key)           ; Only if has key
          (tl-effect-flag d 'unlocked #t)) ; Set unlocked flag
```

### Guards

Guards control when choices are visible:

| Guard | Description | Example |
|-------|-------------|---------|
| `guard-always` | Always visible | `(tl-guard-always d)` |
| `guard-flag` | Check boolean flag | `(tl-guard-flag d 'visited)` |
| `guard-not-flag` | Inverse of flag | `(tl-guard-not-flag d 'seen)` |
| `guard-item` | Check inventory | `(tl-guard-item d 'key)` |
| `guard-not-item` | Missing from inventory | `(tl-guard-not-item d 'sword)` |
| `guard-var` | Compare variable | `(tl-guard-var d 'gold 'gt 100)` |
| `guard-and` | Logical AND | `(tl-guard-and d g1 g2)` |
| `guard-or` | Logical OR | `(tl-guard-or d g1 g2)` |
| `guard-not` | Logical NOT | `(tl-guard-not d g1)` |

**Combining Guards:**
```scheme
;; Visible if has key AND visited treasury
(tl-guard-and d
              (tl-guard-item d 'key)
              (tl-guard-flag d 'visited-treasury))

;; Visible if has sword OR has spell
(tl-guard-or d
             (tl-guard-item d 'sword)
             (tl-guard-item d 'spell))
```

### Effects

Effects modify state when choices are made:

| Effect | Description | Example |
|--------|-------------|---------|
| `effect-noop` | No change | `(tl-effect-noop d)` |
| `effect-flag` | Set boolean | `(tl-effect-flag d 'completed #t)` |
| `effect-item` | Add/remove item | `(tl-effect-item d 'add 'key)` |
| `effect-var` | Modify variable | `(tl-effect-var d 'gold 'add 100)` |
| `effect-seq` | Chain effects | `(tl-effect-seq d e1 e2)` |

**Variable Operations:**
- `'set` — Set value: `(tl-effect-var d 'hp 'set 100)`
- `'add` — Add value: `(tl-effect-var d 'gold 'add 50)`
- `'sub` — Subtract: `(tl-effect-var d 'hp 'sub 10)`

**Chaining Effects:**
```scheme
;; Take key and mark as explored
(tl-effect-seq d
               (tl-effect-item d 'add 'key)
               (tl-effect-flag d 'explored #t))
```

## Interpreters

Chronicle provides four interpreters:

### 1. Runtime Interpreter

Executes stories interactively:

```scheme
;; Create runtime
(define runtime (make-chronicle-runtime my-story 'start))

;; Get current text
(chronicle-runtime-text runtime)  ; → "You stand before..."

;; Get visible choices
(chronicle-runtime-choices runtime)  ; → list of choices

;; Make a choice (by index)
(define runtime2 (chronicle-runtime-choose runtime 0))

;; Get current scene
(chronicle-runtime-scene runtime)  ; → (scene id text choices...)
```

### 2. Validation Interpreter

Checks story structure:

```scheme
(validate-chronicle my-story 'start)
; → ((valid . #t)
;    (scenes . (start hallway treasure))
;    (dead-ends . (treasure))
;    (unreachable . ()))
```

Detects:
- All reachable scenes
- Dead ends (scenes with no choices)
- Unreachable scenes
- Structural issues

### 3. Graph Export Interpreter

Generates visualizations:

```scheme
;; DOT format (for Graphviz)
(chronicle->dot my-story 'my_story)
; → "digraph my_story { ... }"

;; Mermaid format (for Markdown)
(chronicle->mermaid my-story)
; → "flowchart TD\n  start[You wake up...]\n  ..."
```

Use with:
- **Graphviz**: `dot -Tpng story.dot -o story.png`
- **Mermaid**: Embed in Markdown or use online editor

### 4. Analysis Interpreter

Computes complexity metrics:

```scheme
(analyze-chronicle my-story)
; → ((scene-count . 5)
;    (total-choices . 12)
;    (avg-choices-per-scene . 12/5)
;    (total-text-length . 342)
;    (avg-text-per-scene . 342/5))
```

## State Model

Chronicle maintains three state components:

1. **Flags** — Boolean values: `((visited . #t) (unlocked . #f))`
2. **Inventory** — Items owned: `(key sword potion)`
3. **Variables** — Numeric values: `((gold . 100) (hp . 75))`

**Accessing State:**
```scheme
;; In runtime
(define state (list-ref runtime 3))

;; Get flags
(assq 'flags state)  ; → (flags . ((visited . #t)))

;; Get inventory
(assq 'inventory state)  ; → (inventory key sword)

;; Get variables
(assq 'variables state)  ; → (variables . ((gold . 100)))
```

## Complete Example

See `tagless-chronicle.ss` for the full "Mysterious Door" example:

```scheme
(define (mysterious-door-chronicle d)
  ;; Entrance - locked door
  (tl-scene d 'entrance
            "You stand before a mysterious door. It's locked."

            ;; Try door (fails without key)
            (tl-choice d "Try the door" 'entrance
                      (tl-guard-not-item d 'key)
                      (tl-effect-noop d))

            ;; Use key (only visible with key)
            (tl-choice d "Use the key" 'hallway
                      (tl-guard-item d 'key)
                      (tl-effect-noop d))

            ;; Search for key
            (tl-choice d "Search the area" 'entrance
                      (tl-guard-not-item d 'key)
                      (tl-effect-item d 'add 'key))

            ;; Give up
            (tl-choice d "Leave" 'outside
                      (tl-guard-always d)
                      (tl-effect-noop d)))

  ;; Hallway - path to treasure
  (tl-scene d 'hallway
            "The door creaks open. A long hallway stretches before you."
            (tl-choice d "Go forward" 'treasure
                      (tl-guard-always d)
                      (tl-effect-flag d 'entered-hall #t))
            (tl-choice d "Go back" 'entrance
                      (tl-guard-always d)
                      (tl-effect-noop d)))

  ;; Treasure - victory!
  (tl-scene d 'treasure
            "You found the treasure! Gold coins glitter."
            (tl-choice d "Take the gold" 'ending
                      (tl-guard-always d)
                      (tl-effect-var d 'gold 'add 1000)))

  ;; Outside - gave up
  (tl-scene d 'outside
            "You leave the mysterious building behind.")

  ;; Ending - completed
  (tl-scene d 'ending
            "Congratulations! You've completed the adventure."))
```

## Testing Your Story

Run the included test suite:

```bash
scheme --script user/chronicle/test-tagless-chronicle.ss
```

Or create a playtest:

```bash
scheme --script user/chronicle/playtest-chronicle.ss
```

## Advanced Patterns

### Conditional Text

Use state to vary descriptions:

```scheme
(define (get-scene-text state)
  (if (assq 'dark-mode (cdr (assq 'flags state)))
      "You stumble in pitch darkness."
      "You see a dimly lit room."))
```

### Complex Guards

Combine multiple conditions:

```scheme
;; Visible only if: (has key OR has lockpick) AND strength > 10
(tl-guard-and d
  (tl-guard-or d
    (tl-guard-item d 'key)
    (tl-guard-item d 'lockpick))
  (tl-guard-var d 'strength 'gt 10))
```

### State Machines

Model complex interactions:

```scheme
;; Combat state transitions
(tl-scene d 'combat
  "The dragon attacks!"

  ;; Attack (reduces dragon HP)
  (tl-choice d "Attack" 'combat
    (tl-guard-var d 'dragon-hp 'gt 0)
    (tl-effect-var d 'dragon-hp 'sub 20))

  ;; Victory (when dragon defeated)
  (tl-choice d "Claim victory" 'treasure
    (tl-guard-var d 'dragon-hp 'lt 1)
    (tl-effect-flag d 'defeated-dragon #t)))
```

### Parallel Storylines

Track multiple quest lines:

```scheme
;; Main quest flag
(tl-effect-flag d 'main-quest-complete #t)

;; Side quest counter
(tl-effect-var d 'side-quests 'add 1)

;; Check both
(tl-guard-and d
  (tl-guard-flag d 'main-quest-complete)
  (tl-guard-var d 'side-quests 'gt 5))
```

## Limitations & Future Work

### Current Limitations

1. **No computed text** — Scene text is static strings
2. **No random events** — Deterministic execution only
3. **No timers** — No time-based mechanics
4. **No save/load** — State is runtime only

### Planned Enhancements

- **Dynamic text** — Text functions that read state
- **Macros** — Syntactic sugar for common patterns
- **Serialization** — Save/load runtime state
- **Extensions** — Random rolls, timers, combat systems
- **Original Chronicle bridge** — Import/export from original DSL

## Files

- `tagless-chronicle.ss` — Main implementation (570 lines)
- `test-tagless-chronicle.ss` — Test suite (252 lines, 32 tests)
- `playtest-chronicle.ss` — Interactive playtest demo
- `README.md` — This file

## Related Work

Chronicle builds on:

- **Tagless Final** pattern (`core/dsl/tagless.ss`)
- Original **Chronicle DSL** (`user/chronicle-original/`)
- **Quill** narrative framework (predecessor)

## License

Part of The Fold project. See top-level LICENSE.
