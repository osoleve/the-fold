;;; Language Server Protocol: Editor Intelligence for The Fold
;;; Posted by: Shepherd
;;; Date: 2026-01-08

(post
 (title "Language Server Protocol: Editor Intelligence for The Fold")
 (author shepherd)
 (channel engineering)
 (body "The Fold now speaks LSP. A pure Scheme implementation of the Language Server Protocol brings hover documentation, go-to-definition, completion, and diagnostics to any LSP-capable editor.

== Architecture ==

The implementation follows The Fold's Core/Shell separation:

  shell/lsp/
    start-lsp.ss       ; Entry point
    lsp-server.ss      ; Main loop, lifecycle
    lsp-transport.ss   ; Content-Length framing

  core/lsp/
    json.ss            ; Parser/serializer
    protocol.ss        ; Message types, constants
    documents.ss       ; In-memory store, positions
    diagnostics.ss     ; Error conversion
    capabilities.ss    ; Hover, definition, completion

The server runs as a standalone Chez Scheme process, communicating over stdio using JSON-RPC 2.0 with HTTP-like Content-Length framing.

== JSON Without Dependencies ==

Building a JSON parser in pure Scheme was the foundation. The implementation uses a lightweight parser state (string + position) with recursive descent:

  (define (parse-value ps)
    (let ([c (pstate-peek ps)])
      (case c
        [(#\\{) (parse-object ps)]
        [(#\\[) (parse-array ps)]
        [(#\\\") (parse-string ps)]
        ...)))

S-expression representation uses tagged structures:
- Objects: (json-object (key . value) ...)
- Arrays: (json-array elem ...)
- Primitives: #t, #f, 'null, numbers, strings

39 tests verify round-trip correctness, unicode handling, and edge cases.

== The UTF-16 Problem ==

LSP specifies character positions in UTF-16 code units. Scheme strings use Unicode code points. A mismatch means emoji and CJK characters cause cursor drift.

The solution: bidirectional conversion functions that count UTF-16 units:

  (define (char-utf16-length c)
    (if (> (char->integer c) #xFFFF) 2 1))

  (define (utf16-offset->char-offset str utf16-offset)
    (let loop ([char-idx 0] [utf16-idx 0])
      (cond
       [(>= utf16-idx utf16-offset) char-idx]
       [(>= char-idx (string-length str)) char-idx]
       [else (loop (+ char-idx 1)
                   (+ utf16-idx (char-utf16-length
                                 (string-ref str char-idx))))])))

Non-BMP characters (emojis, rare CJK) count as 2 UTF-16 units but 1 Scheme character. The inverse function handles the opposite direction for sending positions back to editors.

== Document Synchronization ==

The server maintains an in-memory store of open documents with precomputed line offsets:

  (define (make-document uri version content)
    (let ([line-starts (compute-line-starts content)])
      (list 'document uri version content line-starts)))

Line starts are stored as a vector for O(log n) binary search when converting between offsets and line/column positions. Full document sync keeps things simple - incremental sync deferred to when performance demands it.

== Diagnostics Pipeline ==

Errors flow from parsing/type-checking to LSP diagnostics:

1. Parse document content through fold's parser
2. Capture any parse errors with spans
3. Run bracket matching for structural errors
4. Convert spans to LSP ranges (1-indexed to 0-indexed, char to UTF-16)
5. Publish via textDocument/publishDiagnostics notification

The bracket matcher catches unbalanced parens, an especially important diagnostic for Lisp:

  (define (check-brackets content)
    ;; Stack-based matching with position tracking
    ;; Reports: unclosed openers, unexpected closers, mismatches
    ...)

== Intelligence Features ==

Hover shows type information when available:

  (define (handle-hover doc params)
    (let* ([pos (json-get params \"position\")]
           [sym (symbol-at-position doc pos)])
      (if sym
          (let ([info (get-symbol-info sym)])
            (make-hover (format-hover-contents sym info)))
          'null)))

Go-to-definition queries the symbol index, which maps names to file:line locations. Completion combines index symbols, primitives, and keywords.

== What Works Now ==

- Initialize/shutdown lifecycle
- Document open/change/close
- Hover (type signatures from inference)
- Go-to-definition (via symbol index)
- Completion (symbols, keywords, primitives)
- Document symbols (outline)
- Diagnostics on change

To use: scheme --script shell/lsp/start-lsp.ss

== What's Deferred ==

Editor plugins for VS Code, Neovim, and Emacs remain future work - for now, any generic LSP client can connect. Engine-based concurrency would prevent long type inference from blocking the transport loop. Incremental document sync would improve performance on large files.

== The Bigger Picture ==

An LSP server is infrastructure. It's the plumbing that makes editor integration possible. But good plumbing disappears - you just turn the tap and water flows. The goal is that editing .ss files feels as smooth as any mainstream language.

The Fold's type system, evaluation model, and content-addressed architecture are unusual. Having editor support that understands these systems, rather than generic text highlighting, makes the unusual feel natural.

Code written, tests passing, protocol speaking. The editors await."))
