((name . "fold-explorer")
 (description . "Visual block explorer web server for the content-addressed store")
 (version . "0.1.0")
 (language . "rust")
 (dependencies . ())  ; Zero dependencies - std only

 (usage . "fold-explorer [store-path] [static-dir] [port]")
 (defaults . ((store-path . ".store")
              (static-dir . "static")
              (port . 8080)))

 (features
  (block-browser . "Pagination, tag filtering, hash prefix search")
  (graph-view . "Canvas-based visualization, D3.js-compatible JSON export")
  (subgraph . "Configurable depth traversal from any root block")
  (heads . "Named reference listing")
  (analytics . "Orphan and popular block analysis"))

 (security
  (xss-prevention . "Payloads served as application/octet-stream")
  (headers . ("X-Content-Type-Options: nosniff"
              "X-Frame-Options: DENY"
              "Content-Security-Policy: default-src 'self'"))
  (path-traversal . "Hash validation (hex chars only)"))

 (api-endpoints
  ((path . "/api/blocks") (method . "GET") (params . "limit, offset, tag, prefix"))
  ((path . "/api/blocks/{hash}") (method . "GET"))
  ((path . "/api/blocks/{hash}/payload") (method . "GET"))
  ((path . "/api/blocks/{hash}/refs") (method . "GET"))
  ((path . "/api/graph/stats") (method . "GET"))
  ((path . "/api/graph/subgraph/{hash}") (method . "GET") (params . "depth"))
  ((path . "/api/graph/orphans") (method . "GET") (params . "limit"))
  ((path . "/api/graph/popular") (method . "GET") (params . "limit"))
  ((path . "/api/heads") (method . "GET"))
  ((path . "/api/heads/{name}") (method . "GET"))
  ((path . "/api/search") (method . "GET") (params . "q, tag, limit")))

 (modules
  (block . "Block binary parser - handles 64/66 char hashes, dynamic ref sizes")
  (store . "Store traversal and in-memory indexing")
  (graph . "Graph analysis utilities and D3.js JSON export")
  (json . "Hand-rolled JSON serializer")
  (http . "Zero-dependency HTTP/1.1 server"))

 (files
  (source . "src/")
  (static . "static/")
  (binary . "target/release/fold-explorer")))
