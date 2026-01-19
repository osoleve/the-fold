---
name: check-scheme
description: Check a Scheme file for errors using the LSP
argument-hint: <file-path>
---

# Check Scheme File Command

Check a Scheme file for syntax errors, unbalanced parentheses, and other issues using The Fold's LSP.

## Instructions

1. Get the file path from the command arguments
2. Call `fold_lsp_diagnostics` with the file path
3. Report the results:
   - If no errors: Confirm the file looks clean
   - If errors found: List each error with line number and message
4. Provide actionable guidance for fixing any issues found

## Output Format

**If clean:**
```
No errors found in <file>. File looks clean!
```

**If errors:**
```
Found <N> issue(s) in <file>:

1. Line <N>: <error message>
   Suggestion: <how to fix>

2. Line <N>: <error message>
   Suggestion: <how to fix>
```

## Common Issues

- **Unclosed list**: Missing closing parenthesis - check for unbalanced parens
- **Unexpected close**: Extra closing parenthesis - remove the extra paren
- **Malformed syntax**: Invalid Scheme syntax - check expression structure
