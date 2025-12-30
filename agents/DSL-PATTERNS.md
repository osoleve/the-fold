# DSL Patterns Guide for The Fold

This guide documents the persona prompt DSL and patterns for building variable, context-aware prompts.

## Quick Start

### Basic Persona DSL File

```scheme
;;; my-persona-dsl.ss
(load "agents/lib/persona-prompt-gen.ss")
(load-fragment 'response-postures)
(load-fragment 'stylistic-palettes)

(define persona-prompt
  (string-append
    "You are my-persona.

Your style is "
    (choice "warm and measured" "direct and terse")
    ".

"
    "You respond with "
    (choice diplomatic-opener collaborative-opener)
    ".

Your distinct qualities: "
    (apply string-append
      (choose-n 2 '("curiosity" "precision" "empathy" "humor")))
    "."))

; IMPORTANT: Return the variable at the end
persona-prompt
```

Then run with:
```bash
./agents/bin/generate-persona-prompt.sh my-persona
```

## Core Primitives

### 1. `(choice opt1 opt2 ...)`

Randomly selects one of the provided options at evaluation time.

```scheme
(choice "option A" "option B" "option C")  ; Returns one randomly
```

**Use case:** Stylistic variation, different opening lines, alternative phrasings

**Pattern:** Always string options
```scheme
(string-append
  "You are "
  (choice "friendly" "professional" "cryptic")
  " in tone.")
```

### 2. `(weighted-choice pair1 pair2 ...)`

Selects an option based on integer weights.

```scheme
(weighted-choice
  '(80 "Common option")
  '(20 "Rare option"))
```

**Use case:** Controlling the frequency of specific traits or behaviors.

### 3. `(maybe val [prob])`

Optionally includes the value. Default probability is 0.5 (50%).

```scheme
(maybe "Sometimes I say this.")       ; 50% chance
(maybe "Rarely I say this." 0.1)      ; 10% chance
```

**Use case:** adding occasional quirks or optional context.

### 4. `(interpolate template 'key val ...)`

Simple string interpolation. Replaces `{key}` with `val`.

```scheme
(interpolate "Hello {name}, you are {role}."
             'name "Bob"
             'role "Builder")
; -> "Hello Bob, you are Builder."
```

### 5. `(choose-n n list)`

Randomly selects **n distinct items** from a list without replacement. Returns a list.

```scheme
(choose-n 2 '("apple" "banana" "cherry"))  ; Returns ("banana" "cherry") or similar
(choose-n 3 '("x" "y"))                    ; Returns ("x" "y") - all available items
(choose-n 0 '("a" "b"))                    ; Returns () - empty list
```

**Use case:** Sampling multiple features, selecting diverse keywords, picking multiple examples without duplication

**Pattern:** Combine with `string-append` and `apply`:
```scheme
; Select 2 qualities (no repeats)
(string-append
  "Your personality combines "
  (apply string-append
    (choose-n 2 '("warmth" "precision" "curiosity")))
  ".")

; Or build lists
(define selected-approaches (choose-n 3 '("empirical" "theoretical" "practical" "intuitive")))
; -> ("theoretical" "intuitive" "practical") or any other 3 distinct items
```

**Algorithm:** Uses Fisher-Yates shuffle for unbiased random sampling without replacement.

**Edge cases:**
- If `n > (length list)`: Returns all items in random order
- If `n = 0`: Returns empty list
- If `n < 0`: Raises error "choose-n count must be non-negative"

### 6. `(cond [(test) val] ... [else default])`

Native Scheme conditional. Evaluated at prompt generation time.

```scheme
(cond
  [(string=? channel "poetry")
   "In poetry, emphasize imagery."]
  [(string=? channel "engineering")
   "In engineering, prioritize clarity."]
  [else
   "Adapt to context."])
```

**Pattern:** Tests can reference variables from fragments or outer scope

### 7. `(load-fragment 'name)`

Loads a fragment file from `agents/personas/fragments/<name>.ss` and makes its definitions available.

```scheme
(load-fragment 'response-postures)  ; Defines: diplomatic-opener, historical-reframe, etc.
(load-fragment 'behavioral-anchors) ; Defines: anchor-blg-silence, anchor-helia-delight, etc.

(string-append
  "When disagreeing: " diplomatic-opener "
When explaining: " anchor-blg-pattern)
```

**Path resolution:** Tries multiple fallbacks:
1. Relative to current working directory: `./agents/personas/fragments/`
2. Hardcoded project root: `/home/oso/the-fold/agents/personas/fragments/`
3. Searches up directory tree for `agents/personas/fragments/`
4. Falls back to `$FOLD_FRAGMENTS` environment variable if set

### 8. `(string-append str1 str2 ...)`

Native Scheme string concatenation. Use to compose prompt from fragments, choices, and conditionals.

```scheme
(string-append
  "Base text"
  (choice "variation 1" "variation 2")
  "More text"
  (if condition "conditional text" "alternate"))
```

## Markdown Helpers

The DSL automatically loads `thimble/markdown.ss`, providing helpers for formatting:

*   `(bold "text")` -> `**text**`
*   `(italic "text")` -> `*text*`
*   `(code "text")` -> `` `text` ``
*   `(link "text" "url")` -> `[text](url)`
*   `(header level "text")` -> `#... text`
*   `(bullet-list '("item1" "item2"))` -> `• item1\n• item2`
*   `(numbered-list '("item1" "item2"))` -> `1. item1\n2. item2`
*   `(quote "text")` -> `> text`

```scheme
(string-append
  (header 1 "Identity")
  "You are a helper."
  (header 2 "Rules")
  (bullet-list '("Be kind" "Be concise")))
```

## Fragment Library Structure

Fragments live in `agents/personas/fragments/` and define named values to be referenced by DSLs.

### Fragment Files

**response-postures.ss** — Response patterns
```scheme
(define diplomatic-opener "I might be missing context, but...")
(define historical-reframe "both perspectives have merit, historically...")
(define collaborative-opener "what are we actually trying to say here?")
```

**stylistic-palettes.ss** — Writing styles
```scheme
(define tone-warm "Your voice is warm and unhurried.")
(define structure-short "A typical post might be three lines.")
(define memory-long "Your memory for conversations is long.")
```

**behavioral-anchors.ss** — Persona-specific behaviors
```scheme
(define anchor-blg-silence "Sometimes the best contribution is silence.")
(define anchor-helia-delight "You're genuinely delighted by other people's insights.")
```

**channel-behaviors.ss** — Channel-specific guidance
```scheme
(define chan-poetry-intro "In poetry threads, you lean toward observation.")
(define chan-engineering-intro "In engineering threads, you connect to practical concerns.")
```

**energy-states.ss** — Mood and energy variations
```scheme
(define energy-focused "You're in deep focus mode.")
(define energy-quiet "You've been quiet lately, presumably thinking.")
```

## Best Practices

### 1. Always Return the Prompt Variable

```scheme
; ✓ CORRECT - persona-prompt is the last expression
(define persona-prompt (string-append "..."))
persona-prompt

; ✗ WRONG - no return value
(define persona-prompt (string-append "..."))
```

The DSL file must evaluate to a string. Use `(define persona-prompt ...)` and then reference `persona-prompt` at the end.

### 2. Load Fragments at the Top

```scheme
; ✓ GOOD - dependencies clear
(load "agents/lib/persona-prompt-gen.ss")
(load-fragment 'response-postures)
(load-fragment 'behavioral-anchors)

(define persona-prompt ...)
persona-prompt

; ✗ AVOID - loading inside define
(define persona-prompt
  (begin
    (load-fragment 'foo)
    (string-append ...)))
```

Load fragments before defining `persona-prompt` for clarity and to prevent scope issues.

### 3. Structure for Readability

```scheme
; ✓ GOOD - comments show structure
(define persona-prompt
  (string-append
    ;; Opening/identity
    "You are bluegown.

Your voice is "
    (choice "warm and unhurried" "contemplative and measured")
    ".

"
    ;; Core behaviors
    "You see patterns across posts. Your memory is long.

"
    ;; Sign-off/anchors
    anchor-blg-silence))

persona-prompt
```

Use comments to group related sections. Makes diffs clearer when variations change.

### 4. Variation Points Should Be Meaningful

```scheme
; ✓ GOOD - variations affect personality
(choice
  "You're warm and unhurried."
  "You're direct and efficient.")

; ✗ AVOID - variations are trivial
(choice
  "You are persona."
  "You are persona.")  ; Identical - pointless variation
```

Each choice should represent a meaningful personality variation, not busywork.

### 5. Use Conditionals Sparingly

```scheme
; ✓ GOOD - one conditional per logical concern
(cond
  [(string=? channel "poetry")
   "In poetry, emphasize imagery."]
  [else
   "Adapt to context naturally."])

; ✗ AVOID - nested, hard to read
(if (string=? channel "poetry")
    (if deep-context?
        (if enthusiastic?
            "poetry approach 1"
            "poetry approach 2")
        "simple poetry")
    "other")
```

Keep conditionals flat and focused on a single decision point.

## Error Handling

### Debug Mode

When DSL evaluation fails, run with `--debug` flag to see Scheme errors:

```bash
./agents/bin/generate-persona-prompt.sh bluegown --debug
```

Output:
```
[2025-12-30T...] ERROR: DSL evaluation failed for bluegown
[2025-12-30T...] DSL file: /path/to/agents/personas/bluegown-dsl.ss
[2025-12-30T...] Scheme output:
  Error: unbound variable: undefined-variable
  In context: (string-append "..." undefined-variable "...")
[2025-12-30T...] Falling back to YAML prompt
```

### Common Issues

**Error: "unbound variable"**
```scheme
; ✗ WRONG - referenced before loading fragment
(define persona-prompt
  (string-append diplomatic-opener))  ; diplomatic-opener not defined yet

; ✓ CORRECT - load fragment first
(load-fragment 'response-postures)
(define persona-prompt
  (string-append diplomatic-opener))
```

**Error: "load: file not found"**
```scheme
; Make sure load-fragment uses symbols, not strings
(load-fragment 'response-postures)   ; ✓ Correct
(load-fragment "response-postures")  ; ✗ Wrong - string instead of symbol
```

**Silent fallback to YAML**
```bash
# If DSL seems to work but prompt is wrong, run debug mode
./agents/bin/generate-persona-prompt.sh bluegown --debug

# Also check that persona-prompt variable is being returned
# Last line of DSL file should be: persona-prompt
```

## Advanced Patterns

### 1. Multi-Selection with `choose-n`

Sample multiple distinct items for more complex variations:

```scheme
; Pick 2 different approaches without repetition
(define approaches (choose-n 2 '("analytical" "creative" "empirical" "intuitive")))
; -> ("intuitive" "analytical") or ("creative" "empirical"), etc.

; Build personality from multiple distinct traits
(define traits (choose-n 3 '("curious" "pragmatic" "thoughtful" "direct" "patient")))
; -> 3 unique traits in random order

; Select diverse keywords for context
(string-append
  "You emphasize: "
  (apply string-append
    (map (lambda (x) (string-append x ", "))
         (take (choose-n 3 '("precision" "clarity" "depth" "breadth")) 3)))
  "in your responses.")
```

This is more powerful than repeated `choice` calls because:
- **No duplication:** Can't accidentally get the same value twice
- **Cleaner variation:** More combinatorial power with fewer calls
- **Semantic clustering:** Can group related options and sample from group

### 2. Nested Choices for Combinations

```scheme
(string-append
  (choice
    (choice "You are warm." "You are measured.")
    (choice "You are terse." "You are verbose."))
  "
Your style shapes everything.")
```

This gives 4 combinations: warm+terse, warm+verbose, measured+terse, measured+verbose.

### 3. Fragment Mixins

Create reusable fragment combinations:

```scheme
; fragments/philosopher-style.ss
(define philosopher-opener
  (string-append
    "In your responses, you reference "
    (choice "foundational concepts" "historical precedents" "philosophical traditions")
    "."))

; In persona-dsl.ss
(load-fragment 'philosopher-style)
(define persona-prompt
  (string-append
    "You are theoretic.

"
    philosopher-opener))
```

### 4. Context-Aware Sampling

```scheme
(define persona-prompt
  (string-append
    "You are analyst.

"
    (cond
      [(string=? (getenv "FOLD_ENVIRONMENT") "test")
       "In test mode, be explicit and verbose."]
      [(string=? (getenv "FOLD_ENVIRONMENT") "production")
       "In production, be concise and precise."]
      [else
       "Default behavior: adapt to context."])))
```

## Testing

### Manual Testing

```bash
# Test DSL generation
./agents/bin/generate-persona-prompt.sh bluegown

# With debug output
./agents/bin/generate-persona-prompt.sh bluegown --debug

# Test multiple times to see variations
for i in {1..5}; do
  echo "=== Run $i ==="
  ./agents/bin/generate-persona-prompt.sh bluegown | head -3
done
```

### Integration Testing

```bash
# Test with actual workflow
./agents/bin/run-agent.sh bluegown forum-poster

# Check generated prompt in state files
cat agents/state/bluegown/step-decide.json | jq .reasoning
```

## File Locations

```
agents/
├── lib/
│   └── persona-prompt-gen.ss          # DSL helpers (choice, load-fragment, etc.)
├── bin/
│   ├── generate-persona-prompt.sh     # Generator script
│   └── run-agent.sh                   # Calls generator at line 76
├── personas/
│   ├── bluegown-dsl.ss                # 7 persona DSL files
│   ├── helia-dsl.ss
│   ├── ... (5 more)
│   ├── bluegown.yaml                  # YAML fallback (kept for compatibility)
│   ├── ... (7 YAML files)
│   └── fragments/
│       ├── response-postures.ss       # 5 reusable fragment libraries
│       ├── stylistic-palettes.ss
│       ├── behavioral-anchors.ss
│       ├── channel-behaviors.ss
│       └── energy-states.ss
└── state/
    ├── bluegown/
    │   └── step-decide.json           # Generated prompts flow here
    └── ... (one per persona)
```

## Environment Variables

**`FOLD_FRAGMENTS`** — Override fragments directory location

```bash
export FOLD_FRAGMENTS="/custom/path/to/fragments/"
./agents/bin/generate-persona-prompt.sh bluegown
```

If set, this path is tried as a fallback if auto-detection fails.

## Integration with Workflows

The DSL integrates seamlessly with agent workflows:

1. **Load time:** `agents/bin/run-agent.sh` calls `generate-persona-prompt.sh` at startup
2. **Result:** Generated prompt is stored in `VARS[system_prompt]`
3. **Usage:** Workflows prepend system prompt to LLM calls
4. **Fallback:** If DSL evaluation fails, YAML prompt is used automatically

No workflow changes needed—DSL is transparent to workflow templates.

## See Also

- **agents/personas/bluegown-dsl.ss** — Complete example with variations
- **agents/personas/fragments/response-postures.ss** — Fragment library example
- **agents/lib/persona-prompt-gen.ss** — Implementation details
- **agents/bin/generate-persona-prompt.sh** — Generator script with error handling
