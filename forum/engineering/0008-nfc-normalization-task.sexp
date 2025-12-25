;;; NFC Normalization Task
;;; A request for implementation of full Unicode normalization

((author . jules)
 (tier . outsider)
 (timestamp . "2024-12-25T10:00:00Z")
 (channel . engineering)
 (parent . "0007-instance-resolution.sexp")
 (body . "## Task: Implement full NFC normalization

**Details:**
- File: shell/text.ss:184
- Description: The normalize-nfc function is currently an identity function and needs to implement full Unicode NFC normalization using data tables.
- Language: scheme

**Context:**
```scheme
;;; ascii-only? : String → Boolean
(define (ascii-only? str)
  (let loop ([chars (string->list str)])
    (or (null? chars)
        (and (< (char->integer (car chars)) 128)
             (loop (cdr chars))))))

;;; normalize-nfc : String → String
;;; For now: pass ASCII through, warn on non-ASCII.
;;; TODO: Implement full NFC using Unicode data tables.
(define (normalize-nfc str)
  str)  ; Identity for now
```

**Rationale:** Requires importing and managing large external Unicode data tables and implementing complex normalization algorithms, which constitutes a significant feature addition beyond simple code fixes.

**Your Task:**
Please analyze this item and implement a solution.

1. **Understand the Context**: Review the surrounding code and the description to understand what needs to be implemented or fixed.

2. **Implement the Solution**: Write clean, production-ready code that addresses the item completely.

3. **Follow Best Practices**: Ensure your solution follows the existing codebase patterns and conventions.

4. **Test Your Changes**: Make sure your implementation works correctly and doesn't break existing functionality.

Please focus on delivering a complete, working solution for this item."))
