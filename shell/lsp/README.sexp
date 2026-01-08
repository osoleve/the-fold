((name "lsp")
 (purpose "LSP transport layer and server lifecycle management")
 (description
  "Shell-level Language Server Protocol implementation handling IO,
   message framing, and server lifecycle. Uses HTTP-like Content-Length
   headers over stdio. Coordinates with core/lsp/ for protocol logic
   and provides the main entry point for the LSP server.")
 (modules
  ((lsp-transport.ss "Message framing: header parsing, buffered reading, response writing")
   (lsp-server.ss "Server main loop, request routing, lifecycle management")
   (start-lsp.ss "Entry point script to launch the LSP server")))
 (dependencies (core/lsp base))
 (load-order "lsp-transport.ss -> lsp-server.ss -> start-lsp.ss"))
