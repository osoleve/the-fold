((name "lsp")
 (purpose "Language Server Protocol implementation for Scheme")
 (description
  "Pure LSP protocol implementation providing IDE support for Scheme code.
   Handles document management, diagnostics, capabilities negotiation,
   and JSON encoding/decoding. This module contains the protocol logic;
   transport and server loop are in shell/lsp/.")
 (modules
  ((capabilities.ss "Server capability declarations for LSP features")
   (diagnostics.ss "Diagnostic message generation for syntax and semantic errors")
   (documents.ss "Document state management with versioning and change tracking")
   (json.ss "JSON encoding and decoding with optimization for LSP messages")
   (protocol.ss "LSP message types, request/response structures")
   (state.ss "Server state management, initialization, and progress tracking")))
 (dependencies (base))
 (load-order
  "json.ss -> protocol.ss -> state.ss -> documents.ss -> diagnostics.ss -> capabilities.ss"))
