((name "lsp")
 (purpose "Language Server Protocol implementation with real type inference")
 (description
  "Shell-level Language Server Protocol implementation providing IDE features.
   Uses HTTP-like Content-Length headers over stdio. Implements hover with
   real type inference from core/types/infer.ss, completion, diagnostics,
   document symbols, and go-to-definition.")
 (modules
  ((lsp-transport.ss "Message framing: header parsing, buffered reading, response writing")
   (lsp-server.ss "Server main loop, request routing, lifecycle management")
   (protocol.ss "LSP message parsing, request/response construction")
   (capabilities.ss "Feature implementations: hover, completion, diagnostics, symbols")
   (documents.ss "Document state management, incremental sync")
   (diagnostics.ss "Error/warning reporting and tracking")
   (state.ss "Server state: documents, diagnostics, symbol index")
   (json.ss "JSON parsing and serialization")
   (start-lsp.ss "Entry point script to launch the LSP server")))
 (key-concepts
  ((hover-inference "Real type inference via parse-definitions, build-tenv-from-defs")
   (fallback-types "Layered fallback: inference -> primitives -> symbol index")
   (incremental-sync "Document changes applied incrementally via text edits")))
 (dependencies (core/types core/lsp base))
 (tests
  ((test-hover-inference.ss "Type inference integration tests")
   (test-capabilities.ss "Feature implementation tests")
   (test-diagnostics.ss "Error reporting tests")
   (test-documents.ss "Document sync tests")
   (test-protocol.ss "Message parsing tests")
   (test-json.ss "JSON handling tests")))
 (load-order "json.ss -> protocol.ss -> state.ss -> documents.ss -> diagnostics.ss -> capabilities.ss -> lsp-transport.ss -> lsp-server.ss"))
