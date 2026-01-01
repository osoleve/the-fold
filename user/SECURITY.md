# Playpen Security Hardening Guide

## Overview

This document describes the security improvements made to the playpen codebase to address input validation vulnerabilities, path traversal issues, and other common security concerns.

## Security Vulnerabilities Addressed

### 1. Path Traversal Vulnerabilities
**Issue**: Files used `load` statements with relative paths that could be hijacked by malicious actors.
**Fix**: 
- Created `sanitize-path` and `sanitize-filename` functions
- All file paths are now validated before use
- Directory traversal sequences (`..`, `~`, `/`, `\`) are removed
- Empty or invalid paths return `#f` for proper error handling

### 2. Input Validation Issues
**Issue**: Functions accepted arbitrary user input without validation, leading to potential crashes or unexpected behavior.
**Fix**:
- Created validation functions: `valid-string?`, `valid-integer?`, `valid-symbol?`
- All user inputs are validated before processing
- Bounds checking prevents buffer overflow-style issues
- Type validation ensures expected data types

### 3. Code Injection Risks
**Issue**: Template rendering and string operations could be exploited for code injection.
**Fix**:
- Created `safe-template-render` with recursion depth limits
- HTML escaping function `escape-html` prevents XSS attacks
- String operations have length limits and validation
- Block deserialization validates content before processing

### 4. Resource Exhaustion
**Issue**: No limits on string lengths, list sizes, or recursion depth could lead to denial of service.
**Fix**:
- Implemented maximum length constants: `MAX_STRING_LENGTH`, `MAX_LIST_LENGTH`
- Safe string operations with built-in limits
- Recursion depth tracking in template rendering
- Memory-efficient list operations with `safe-list-take`

## Security Utilities Module

The `security-utils.ss` module provides comprehensive security functions:

### Core Functions

#### Input Validation
```scheme
(valid-string? str min-len max-len)     ; Validate string length
(valid-integer? val min-val max-val)    ; Validate integer range
(valid-symbol? sym allowed-symbols)     ; Validate symbol membership
```

#### String Sanitization
```scheme
(sanitize-path path-str)                ; Remove path traversal sequences
(sanitize-filename filename)            ; Clean dangerous filename characters
(escape-html str)                       ; Basic HTML entity escaping
```

#### Safe Operations
```scheme
(safe-string-replace str old new max-len)  ; Bounded string replacement
(safe-string-split str delim max-parts)    ; Bounded string splitting
(safe-template-render template bindings)   ; Secure template rendering
(safe-list-take lst n)                     ; Bounded list operations
```

#### Security Logging
```scheme
(log-security-event event-type details)   ; Log security events
(log-invalid-input context input)         ; Log invalid input attempts
```

### Configuration Constants

```scheme
MAX_STRING_LENGTH     ; 10000 characters
MAX_LIST_LENGTH       ; 1000 elements  
MAX_FILENAME_LENGTH   ; 255 characters
MAX_PATH_LENGTH       ; 4096 characters
MAX_RECURSION_DEPTH   ; 100 levels
```

## File-Specific Security Fixes

### string-art.ss
- **Template Rendering**: Replaced unsafe `render` function with `safe-template-render`
- **String Operations**: Added bounds checking for large string operations
- **Input Validation**: Validates emoji and Unicode input lengths
- **Security Logging**: Logs template rendering failures

### block-playground.ss
- **Block Creation**: Validates block content and tags before creation
- **Content Validation**: Ensures block payloads are within safe limits
- **Symbol Validation**: Restricts block tags to known safe values
- **String Splitting**: Uses secure string splitting with limits

### duckie.ss
- **Point Validation**: Validates coordinates are within reasonable bounds
- **Mood Validation**: Ensures mood states are from allowed set
- **Name Sanitization**: Sanitizes duckie names to prevent injection
- **Memory Creation**: Validates memory parameters before creation
- **Block Deserialization**: Safely deserializes duckie data with validation

### string-puzzle.ss
- **Cipher Validation**: Validates Caesar cipher shift ranges (-25 to 25)
- **String Operations**: All string operations have length validation
- **Input Sanitization**: Validates puzzle inputs and sample text
- **Palindrome Checking**: Secure palindrome validation with bounds checking

### environment.ss
- **Time Validation**: Validates time-of-day symbols
- **Coordinate Validation**: Ensures drawing coordinates are within bounds
- **Recursion Limits**: Prevents infinite recursion in drawing functions
- **Bounds Checking**: All drawing operations validate parameters

## Security Testing

Run the security test suite to validate all security functions:

```bash
scheme --script user/test-security.ss
```

The test suite validates:
- Input validation functions
- String sanitization
- Bounds checking
- Safe string operations
- Template rendering security
- Block content validation
- Security logging
- Path traversal protection

## Best Practices for Future Development

### 1. Always Validate Input
```scheme
;; Good
(define (process-user-input input)
  (if (valid-string? input 1 1000)
      (process-safe input)
      (handle-invalid-input input)))

;; Bad  
(define (process-user-input input)
  (process-unsafe input))  ; No validation!
```

### 2. Use Safe Operations
```scheme
;; Good
(safe-string-replace text "old" "new" MAX_STRING_LENGTH)

;; Bad
(string-replace text "old" "new")  ; No length limits!
```

### 3. Sanitize External Data
```scheme
;; Good
(let ([safe-filename (sanitize-filename user-filename)])
  (when safe-filename
    (process-file safe-filename)))

;; Bad
(process-file user-filename)  ; Path traversal risk!
```

### 4. Log Security Events
```scheme
;; Good
(when (not (valid-input? input))
  (log-invalid-input "my-function" input)
  (error "Invalid input"))

;; Bad
(when (not (valid-input? input))
  (error "Invalid input"))  ; No logging!
```

### 5. Set Reasonable Limits
```scheme
;; Good
(define (process-list lst)
  (let ([safe-lst (safe-list-take lst MAX_LIST_LENGTH)])
    (map process-item safe-lst)))

;; Bad
(define (process-list lst)
  (map process-item lst))  ; Could exhaust memory!
```

## Security Considerations

### Threat Model
The playpen codebase is designed to be safe for:
- User-generated content processing
- File system operations
- Network input handling
- Template rendering with user data
- Block chain operations

### Attack Vectors Mitigated
- **Path Traversal**: `../../../etc/passwd` → sanitized to safe filename
- **Code Injection**: `<script>alert('xss')</script>` → HTML escaped
- **Resource Exhaustion**: Infinite lists/strings → bounded by limits
- **Type Confusion**: Wrong data types → validated before use
- **Buffer Overflow**: Oversized inputs → length validation

### Limitations
- Security functions add computational overhead
- Some legitimate inputs may be rejected (false positives)
- Cannot protect against all possible attack vectors
- Requires ongoing maintenance and updates

## Monitoring and Maintenance

### Security Event Monitoring
All security events are logged to console with `[SECURITY]` prefix:
- Invalid input attempts
- Template rendering failures
- Block validation failures
- Path sanitization events

### Regular Security Tasks
1. Review security logs for suspicious activity
2. Update validation rules as needed
3. Test with new attack vectors
4. Update maximum limits based on usage patterns
5. Review and update sanitization rules

### Incident Response
If security events are detected:
1. Identify the source of invalid input
2. Review the context of the security event
3. Update validation rules if necessary
4. Consider additional security measures
5. Document the incident and response

## Conclusion

The security hardening of the playpen codebase provides robust protection against common vulnerabilities while maintaining functionality. The modular security utilities can be reused across the entire Fold ecosystem.

Remember: Security is an ongoing process, not a one-time fix. Regular reviews and updates are essential for maintaining security posture.