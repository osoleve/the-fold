# Gemini 3 Pro Review: LSP Implementation

**Date:** 2026-01-08
**Reviewer:** Gemini 3 Pro
**Scope:** core/lsp/, shell/lsp/

## Executive Summary

The implementation provides a functional foundation for an LSP server, with a clean separation of concerns (transport, protocol, logic). However, there is a **critical protocol violation** in the transport layer regarding `Content-Length` handling that will cause desynchronization with any client sending non-ASCII characters.

## 1. Protocol Correctness

* **CRITICAL BUG (`shell/lsp/lsp-transport.ss`)**: `read-n-chars` uses `read-char` to read the message body based on `Content-Length`. LSP defines `Content-Length` in **bytes**, but `read-char` consumes **characters**.
    * **Impact**: If a message contains multi-byte UTF-8 sequences (e.g., emojis, or even non-Latin characters), the server will read fewer bytes than specified, leaving the remaining bytes to be misread as the start of the next header. This breaks the request/response loop.
    * **Fix**: The transport must read `N` *bytes* into a bytevector, then decode that bytevector to a string using UTF-8.
* **JSON-RPC**: The structure of requests, responses, and notifications (`core/lsp/protocol.ss`) adheres to the JSON-RPC 2.0 specification.
* **Capabilities**: The server correctly advertises `textDocumentSync` as `Full` (1), which matches the implementation in `handle-did-change`.

## 2. UTF-16 Handling

* **Correct**: `core/lsp/documents.ss` implements `utf16-offset->char-offset` and `char-offset->utf16-offset` correctly.
* **Logic**: It correctly identifies non-BMP characters (`> #xFFFF`) as requiring 2 UTF-16 code units (surrogate pairs). This ensures that column positions sent by VS Code (or other clients) map correctly to Scheme string indices.

## 3. Code Quality & Style

* **Structure**: The code is well-modularized. `protocol.ss` handles definitions, `json.ss` handles parsing, and `server.ss` ties it together.
* **Idiomatic**: The Scheme code is generally clean and readable.
* **Manual Parsing**: The manual JSON parser (`core/lsp/json.ss`) is a valid choice for zero-dependency environments, though `list->string` accumulation is inefficient (see Performance).
* **Global State**: `lsp-server.ss` relies on global variables (`*server-initialized*`, `*documents*`). While acceptable for a simple script, it hampers testability.

## 4. Error Handling

* **Resilience**: `lsp-server.ss` uses `guard` (exception handling) around request dispatch. This is excellent; it prevents a single malformed request from crashing the entire server.
* **Diagnostics**: `core/lsp/diagnostics.ss` has a robust mechanism to convert internal `fold` errors (parse/infer/eval) into LSP Diagnostics with correct ranges and severities.

## 5. Performance Concerns

* **JSON Parsing**: `core/lsp/json.ss` constructs strings by accumulating characters in a list (`(cons c chars)`) and then reversing. This generates high garbage collection pressure. **Recommendation**: Use string ports (`open-output-string`) or a buffer.
* **Document Symbols**: `core/lsp/capabilities.ss` uses `(string-split content #\newline)` in `extract-definitions`. For large files, this allocates a massive list of strings just to regex-match `(define ...`. **Recommendation**: Iterate over the string content directly without splitting.
* **Full Sync**: The server uses "Full" text sync. For very large files, transferring and re-parsing the entire JSON payload on every keystroke (or debounce) will be sluggish.

## 6. Security

* **DoS Risk**: `read-lsp-message` reads `Content-Length` bytes into memory without a limit. A malicious client could send a header `Content-Length: 2147483647` and exhaust server memory. **Recommendation**: Enforce a reasonable maximum message size (e.g., 10MB).
* **Path Traversal**: `uri->path` naively strips `file://`. Ensure that operations are confined to the workspace root where appropriate, though this is less critical for a local dev tool.

---

## Specific Fix Recommendations

### 1. Fix Transport (Priority: High)

Modify `shell/lsp/lsp-transport.ss`:

```scheme
;; Current (Broken)
(define (read-n-chars port n)
  (let ([buf (make-string n)])
       ;; ... reads chars ...
       ))

;; Proposed Fix
(define (read-n-bytes-as-string port n)
  (let ([bv (get-bytevector-n port n)])
    (utf8->string bv)))
```

### 2. Optimize Symbol Extraction (Priority: Medium)

In `core/lsp/capabilities.ss`, replace `string-split` with a line-iterator that scans the string for `(define` forms without allocating substrings for every line.

### 3. Optimize JSON String Parsing (Priority: Low)

In `core/lsp/json.ss`, use `call-with-string-output-port` for building strings instead of list accumulation.
