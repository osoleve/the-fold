"""
agents/harness/parse.py — Response parsing for agent harness

Extracts S-expressions from LLM completions, handling:
- Code fences (```scheme ... ```)
- Single backticks (`...`)
- Bare expressions (+ 1 2) → ((+ 1 2))

Note: This parser is designed for LLM outputs, not arbitrary Scheme code.
It does NOT handle:
- Comments (; ...)
- Character literals (#\\x)
These are rarely/never produced by LLMs in our use case.
"""

import re
from dataclasses import dataclass
from typing import Optional

# --- Constants ---

MAX_COMPLETION_LENGTH = 100_000  # 100KB - reasonable for any LLM output

# Pre-compiled regex patterns (performance)
# These patterns search within text (not anchored) to find code fences
# even when surrounded by prose explanation
_TRIPLE_FENCE_PATTERN = re.compile(r'```[\w-]*\s*(.*?)\s*```', re.DOTALL)
_SINGLE_BACKTICK_PATTERN = re.compile(r'`([^`]+)`')


@dataclass(frozen=True)
class ParseResult:
    """Result of parsing an LLM completion."""
    success: bool
    expression: str           # The cleaned S-expression (if success)
    error: Optional[str]      # Error message (if failure)
    raw_input: str            # Original input for debugging
    transformations: tuple    # What we did: ("strip_fences", "add_parens", ...)


def strip_code_fences(text: str) -> tuple[str, bool]:
    """
    Extract code from code fences, searching within the text.

    Handles:
    - Triple backticks with optional language: ```scheme\n(+ 1 2)\n```
    - Single backticks: `(+ 1 2)`

    IMPORTANT: This searches for code fences anywhere in the text, not just
    when the entire text is wrapped. This prevents intro prose like
    "I (the agent) will run:" from being mistakenly parsed as S-expressions.

    Returns (cleaned_text, was_transformed).
    """
    cleaned = text.strip()

    # Search for triple backticks anywhere in the text (prefer this over prose)
    match = _TRIPLE_FENCE_PATTERN.search(cleaned)
    if match:
        return (match.group(1).strip(), True)

    # Search for single backticks (but only if content looks like code)
    match = _SINGLE_BACKTICK_PATTERN.search(cleaned)
    if match:
        content = match.group(1).strip()
        # Only use backtick content if it looks like an S-expression
        if content.startswith('(') or not any(c in content for c in ' \n\t'):
            return (content, True)

    # No fences found - return stripped version
    return (cleaned, cleaned != text)


def ensure_parenthesized(text: str) -> tuple[str, bool]:
    """
    Wrap expression in parentheses if not already parenthesized.

    Allows: + 1 2 → (+ 1 2)
    Preserves: (+ 1 2) → (+ 1 2)

    Returns (result, was_transformed).
    """
    trimmed = text.strip()

    if not trimmed:
        return (trimmed, False)

    if trimmed.startswith('('):
        return (trimmed, False)

    return (f'({trimmed})', True)


def extract_first_sexp(text: str) -> tuple[Optional[str], Optional[str]]:
    """
    Extract the first complete S-expression from text.

    Handles cases where LLM includes explanation before/after code.

    Returns (sexp, remainder) or (None, error_message).
    """
    trimmed = text.strip()

    if not trimmed:
        return (None, "Empty input")

    # Find the first opening paren
    start = trimmed.find('(')
    if start == -1:
        # No parens - this is a bare expression like "help" or "+ 1 2"
        # Return the whole thing (will be wrapped in parens later)
        return (trimmed, '')

    # Count parens to find matching close
    # (iterate by index to avoid string slice copy)
    depth = 0
    in_string = False
    escape_next = False

    for i in range(start, len(trimmed)):
        char = trimmed[i]

        if escape_next:
            escape_next = False
            continue

        if char == '\\' and in_string:
            escape_next = True
            continue

        if char == '"' and not escape_next:
            in_string = not in_string
            continue

        if in_string:
            continue

        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth == 0:
                # Found complete sexp
                sexp = trimmed[start:i+1]
                remainder = trimmed[i+1:].strip()
                return (sexp, remainder)

    # Unbalanced parens
    return (None, f"Unbalanced parentheses (depth {depth} at end)")


def validate_sexp_syntax(expr: str) -> Optional[str]:
    """
    Basic syntax validation for S-expressions.

    Returns None if valid, error message if invalid.
    Does NOT evaluate - just checks structure.
    """
    if not expr:
        return "Empty expression"

    # Check balanced parens (already done in extract, but double-check)
    depth = 0
    in_string = False
    escape_next = False

    for char in expr:
        if escape_next:
            escape_next = False
            continue
        if char == '\\' and in_string:
            escape_next = True
            continue
        if char == '"':
            in_string = not in_string
            continue
        if in_string:
            continue
        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth < 0:
                return "Unexpected closing parenthesis"

    if depth != 0:
        return f"Unbalanced parentheses (missing {depth} closing)"

    if in_string:
        return "Unterminated string literal"

    return None


def parse_completion(raw_completion: str) -> ParseResult:
    """
    Parse an LLM completion into an S-expression.

    Pipeline:
    0. Length check (DoS protection)
    1. Strip code fences
    2. Extract first S-expression (handles prose around code)
    3. Ensure parenthesized (for bare expressions)
    4. Validate syntax

    Returns ParseResult with success/failure and transformations applied.
    """
    transformations = []

    # Step 0: Length check (DoS protection)
    if len(raw_completion) > MAX_COMPLETION_LENGTH:
        return ParseResult(
            success=False,
            expression="",
            error=f"Input too large: {len(raw_completion)} bytes (max {MAX_COMPLETION_LENGTH})",
            raw_input=raw_completion[:1000] + "...[truncated]",
            transformations=tuple(transformations)
        )

    current = raw_completion

    # Step 1: Strip code fences
    current, did_strip = strip_code_fences(current)
    if did_strip:
        transformations.append("strip_fences")

    # Step 2: Extract first S-expression
    extracted, remainder_or_error = extract_first_sexp(current)
    if extracted is None:
        return ParseResult(
            success=False,
            expression="",
            error=f"Extraction failed: {remainder_or_error}",
            raw_input=raw_completion,
            transformations=tuple(transformations)
        )

    if remainder_or_error:  # Had to extract from prose
        transformations.append("extract_from_prose")

    current = extracted

    # Step 3: Ensure parenthesized
    current, did_wrap = ensure_parenthesized(current)
    if did_wrap:
        transformations.append("add_parens")

    # Step 4: Validate syntax
    # Optimization: If the extracted expression originally started with '(',
    # extract_first_sexp has already validated it (balanced parens, terminated strings).
    # We only need to validate if it was a bare expression that we wrapped.
    if not extracted.startswith('('):
        error = validate_sexp_syntax(current)
        if error:
            return ParseResult(
                success=False,
                expression=current,
                error=f"Syntax error: {error}",
                raw_input=raw_completion,
                transformations=tuple(transformations)
            )

    return ParseResult(
        success=True,
        expression=current,
        error=None,
        raw_input=raw_completion,
        transformations=tuple(transformations)
    )


# --- Tests ---

def _test_parse():
    """Self-test for parse module."""

    test_cases = [
        # (input, expected_success, expected_expr_or_error_substring)

        # Basic cases
        ("(+ 1 2)", True, "(+ 1 2)"),
        ("```scheme\n(+ 1 2)\n```", True, "(+ 1 2)"),
        ("```\n(define x 42)\n```", True, "(define x 42)"),
        ("`(help)`", True, "(help)"),

        # Bare expressions
        ("+ 1 2", True, "(+ 1 2)"),
        ("help", True, "(help)"),

        # Prose around code
        ("I'll browse the forum:\n\n```scheme\n(browse 'engineering 3)\n```\n\nThis should show recent posts.",
         True, "(browse 'engineering 3)"),

        # CRITICAL: Intro prose with parentheses should NOT be extracted
        # This was a security vulnerability - "I (the agent)" was being extracted
        ("I (the agent) will run this code:\n\n```scheme\n(help)\n```",
         True, "(help)"),
        ("Here's the code (simple example):\n```\n(+ 1 2)\n```",
         True, "(+ 1 2)"),

        # Nested parens
        ("(define (square x) (* x x))", True, "(define (square x) (* x x))"),

        # Strings
        ('(msg \'chat "Hello world")', True, '(msg \'chat "Hello world")'),

        # Error cases
        ("", False, "Empty"),
        ("(()", False, "Unbalanced"),
        ("(+ 1 2", False, "Unbalanced"),

        # String with parens inside (should not confuse parser)
        ('(display "(not a real paren)")', True, '(display "(not a real paren)")'),

        # Escaped quotes in strings
        ('(display "He said \\"hello\\"")', True, '(display "He said \\"hello\\"")'),

        # Multiple expressions (should take first)
        ("(+ 1 2) (+ 3 4)", True, "(+ 1 2)"),

        # Inline code fence without newlines
        ("```scheme(help)```", True, "(help)"),

        # Whitespace variations
        ("  (help)  ", True, "(help)"),
        ("\n\n(digest)\n\n", True, "(digest)"),

        # DoS protection - huge input
        ("x" * 200_000, False, "Input too large"),
    ]

    passed = 0
    failed = 0

    for input_text, expect_success, expect_content in test_cases:
        result = parse_completion(input_text)

        if result.success != expect_success:
            print(f"FAIL: {input_text[:40]!r}")
            print(f"  Expected success={expect_success}, got {result.success}")
            print(f"  Error: {result.error}")
            failed += 1
            continue

        if expect_success:
            if expect_content not in result.expression:
                print(f"FAIL: {input_text[:40]!r}")
                print(f"  Expected {expect_content!r} in {result.expression!r}")
                failed += 1
                continue
        else:
            if expect_content not in (result.error or ""):
                print(f"FAIL: {input_text[:40]!r}")
                print(f"  Expected {expect_content!r} in error {result.error!r}")
                failed += 1
                continue

        passed += 1

    print(f"\nparse.py: {passed} passed, {failed} failed")
    return failed == 0


if __name__ == "__main__":
    import sys
    success = _test_parse()
    sys.exit(0 if success else 1)
