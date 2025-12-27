# String Utilities

**Location:** `thimble/string-utils.ss`
**Status:** ✅ Complete (2025-12-27)
**Tests:** 67/67 passing
**Wishlist:** Item #3 (High Priority Tools)

## Overview

Canonical string utility functions for The Fold. This library eliminates the need for local reimplementations of common string operations that were previously scattered across 23+ files in the codebase.

## Quick Start

```scheme
(load "thimble/string-utils.ss")

;; Split and join
(string-split "foo,bar,baz" ",")  ;=> ("foo" "bar" "baz")
(string-join '("a" "b" "c") "-")  ;=> "a-b-c"

;; Search
(string-contains? "hello world" "world")  ;=> #t
(string-starts-with? "hello" "hel")       ;=> #t
(string-ends-with? "hello" "lo")          ;=> #t

;; Transform
(string-trim "  hello  ")                 ;=> "hello"
(string-replace "foo foo" "foo" "bar")    ;=> "bar bar"

;; Predicates
(string-empty? "")                        ;=> #t
(string-blank? "   ")                     ;=> #t
```

## Functions

### String Splitting & Joining
- `string-split` - Split by delimiter
- `string-split-lines` - Split by newlines (handles Unix/Windows)
- `string-join` - Join with separator

### String Search & Pattern Matching
- `string-contains?` - Substring search
- `string-starts-with?` - Prefix check
- `string-ends-with?` - Suffix check
- `string-index` - Find character position
- `string-index-of` - Find substring position

### String Replacement
- `string-replace` - Replace all occurrences
- `string-replace-first` - Replace first occurrence only

### String Trimming & Whitespace
- `string-trim` - Remove leading/trailing whitespace
- `string-trim-left` - Remove leading whitespace
- `string-trim-right` - Remove trailing whitespace
- `whitespace?` - Character predicate

### String Predicates
- `string-empty?` - Check if empty
- `string-blank?` - Check if empty or whitespace only

## Design Principles

1. **Pure functions** - No mutation
2. **Tier 5-6 operations** - Built on Scheme string primitives
3. **UTF-8 aware** - Proper Unicode handling
4. **Consistent naming** - Follows Scheme conventions
5. **Clear error handling** - Graceful with edge cases

## Examples

See:
- `thimble/string-utils-example.ss` - 8 practical examples
- `playpen/string-art.ss` - Stress tests and creative uses
- `playpen/string-puzzle.ss` - Word games and puzzles

## Tests

```bash
scheme --script thimble/test-string-utils.ss
```

**Coverage:**
- Basic operations (split, join, trim)
- Edge cases (empty strings, delimiters at boundaries)
- Unicode handling (Greek, Chinese, emoji)
- Pattern matching
- Replacement operations
- Whitespace handling

## Impact

This library eliminates duplicate implementations in:
- `fabric/stitches/error.ss`
- `thimble/concept-map.ss`
- `thimble/format.ss`
- `thimble/history.ss`
- `thimble/coverage.ss`
- ...and 18+ more files

## Future Work

When The Fold has a module system, convert to a proper module with explicit exports.

## Forum Posts

- Announcement: `forum/wishlist/0008-implementing-string-utilities.sexp`
- Completion: `forum/engineering/0019-string-utils-complete.sexp`

## Related Wishlist Items

**High Priority Tools (Status):**
- ✅ String Utilities - COMPLETE
- ☐ Persistent Store API
- ☐ Block Navigation Library
- ☐ Query Language
- ☐ Collection Utilities

---

*Implementation of wishlist item #3, responding to the Knowledge Engine development experience*
*Root hash: dc857f05c0d58dcb417b9631a064161abac9956b5c75f0e49a4d739329e7075f*
