# Playpen Experiments

This directory contains exploratory experiments and creative demonstrations.

## String Utilities Experiments (2025-12-27)

A series of experiments created while building and testing the string utilities library.

### Files

#### `string-art.ss` - Stress Testing & Creative Playground
10 comprehensive tests demonstrating string utility capabilities:
- ASCII art manipulation (duck indentation)
- Emoji round-trip testing (🔥💧🌍⚡)
- Multilingual Unicode (Arabic, Chinese, Russian, Hebrew, Japanese)
- Nested replacement chains
- Whitespace pattern detection
- Pattern matching validation
- Split/join identity verification
- Template engine implementation
- Performance testing (100+ words)
- Trim edge case handling

**Run:** `scheme --script playpen/string-art.ss`

#### `string-puzzle.ss` - Interactive Word Games
Fun text-based puzzles using string utilities:
- Message decoder (XXX markers + reverse words)
- Caesar cipher decryption
- Pattern matching challenges
- Word frequency analyzer
- Palindrome detector
- Haiku formatter

**Run:** `scheme --script playpen/string-puzzle.ss`

#### `block-playground.ss` - Content-Addressed Blocks Explorer
Deep dive into The Fold's block system:
- Block creation and inspection
- Content addressing verification (same content = same hash)
- Block references and Merkle DAG construction
- Serialization round-trip testing
- Mini knowledge graph demonstration (Alan Turing example)

**Run:** `scheme --script playpen/block-playground.ss`

#### `session-summary.ss` - Development Session Report
Comprehensive summary of the string utilities implementation project:
- Deliverables list
- Experiments overview
- Key insights and discoveries
- Impact metrics
- Next steps and remaining wishlist items

**Run:** `scheme --script playpen/session-summary.ss`

## Purpose

These experiments serve multiple purposes:
1. **Testing** - Real-world validation of new libraries
2. **Documentation** - Examples of how to use the tools
3. **Learning** - Understanding the system architecture
4. **Fun** - Making development enjoyable!

## Related

- String utilities implementation: `thimble/string-utils.ss`
- String utilities tests: `thimble/test-string-utils.ss`
- String utilities examples: `thimble/string-utils-example.ss`
- Forum announcement: `forum/wishlist/0008-implementing-string-utilities.sexp`
- Completion report: `forum/engineering/0019-string-utils-complete.sexp`

---

*Created during the implementation of wishlist item #3 (High Priority Tools: String Utilities)*
