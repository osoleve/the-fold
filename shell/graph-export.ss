;;; thimble/graph-export.ss — Graph Visualization Export Tool
;;;
;;; Exports the block store graph to various visualization formats:
;;;   - DOT (Graphviz) for static graph rendering
;;;   - JSON (D3.js compatible) for interactive web visualization
;;;   - ASCII for terminal-based tree views
;;;
;;; This is Shell code: uses filesystem operations, formatting.
;;;
;;; Dependencies:
;;;   fabric/stitches/block.ss
;;;   fabric/stitches/sha256.ss
;;;   thimble/fs.ss
;;;   thimble/store-api.ss
;;;
;;; Usage:
;;;   (load "shell/graph-export.ss")
;;;
;;;   ;; DOT export (Graphviz)
;;;   (export-dot fs "graph.dot")                    ; Export entire store
;;;   (export-dot-subset fs hashes "subset.dot")    ; Export specific blocks
;;;   (export-dot-string fs)                         ; Return DOT as string
;;;
;;;   ;; JSON export (D3.js)
;;;   (export-json fs "graph.json")                  ; Export entire store
;;;   (export-json-subset fs hashes "subset.json")  ; Export specific blocks
;;;   (export-json-string fs)                        ; Return JSON as string
;;;
;;;   ;; ASCII visualization
;;;   (render-ascii fs hash 3)                       ; Tree view with depth 3
;;;   (render-ascii-refs fs hash)                    ; Immediate refs only

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; hash->short : Bytevector -> String
;;; Truncate hash to first 8 hex characters for display.
(define (hash->short hash)
  (let ([hex (hash->hex hash)])
       (if (> (string-length hex) 8)
           (substring hex 0 8)
           hex)))

;;; block->label : Block -> String
;;; Generate a readable label for a block.
;;; Format: "tag: payload-preview" (truncated)
(define (block->label blk)
  (let* ([tag (symbol->string (block-tag blk))]
         [payload (block-payload blk)]
         [payload-str (guard (e [else "[binary]"])
                             (utf8->string payload))]
         [preview (truncate-label payload-str 30)])
        (string-append tag ": " preview)))

;;; truncate-label : String x Nat -> String
;;; Truncate string for labels, adding ellipsis if needed.
(define (truncate-label str max-len)
  (let ([cleaned (string-trim-newlines str)])
       (if (> (string-length cleaned) max-len)
           (string-append (substring cleaned 0 max-len) "...")
           cleaned)))

;;; string-trim-newlines : String -> String
;;; Replace newlines with spaces for single-line labels.
(define (string-trim-newlines str)
  (let ([len (string-length str)])
       (let loop ([i 0] [chars '()])
            (if (>= i len)
                (list->string (reverse chars))
                (let ([c (string-ref str i)])
                     (loop (+ i 1)
                           (cons (if (or (char=? c #\newline)
                                         (char=? c #\return)
                                         (char=? c #\tab))
                                     #\space
                                     c)
                                 chars)))))))

;;; escape-dot-string : String -> String
;;; Escape special characters for DOT format.
;;; Escapes: backslash, double quotes, newlines.
(define (escape-dot-string str)
  (let ([len (string-length str)])
       (let loop ([i 0] [result '()])
            (if (>= i len)
                (list->string (reverse result))
                (let ([c (string-ref str i)])
                     (cond
                      [(char=? c #\\)
                       (loop (+ i 1) (cons #\\ (cons #\\ result)))]
                      [(char=? c #\")
                       (loop (+ i 1) (cons #\" (cons #\\ result)))]
                      [(char=? c #\newline)
                       (loop (+ i 1) (cons #\n (cons #\\ result)))]
                      [(char=? c #\return)
                       (loop (+ i 1) result)]  ; Skip carriage returns
                      [(char=? c #\tab)
                       (loop (+ i 1) (cons #\space result))]
                      [else
                       (loop (+ i 1) (cons c result))]))))))

;;; escape-json-string : String -> String
;;; Escape special characters for JSON format.
;;; Escapes: backslash, double quotes, control characters.
(define (escape-json-string str)
  (let ([len (string-length str)])
       (let loop ([i 0] [result '()])
            (if (>= i len)
                (list->string (reverse result))
                (let ([c (string-ref str i)])
                     (cond
                      [(char=? c #\\)
                       (loop (+ i 1) (cons #\\ (cons #\\ result)))]
                      [(char=? c #\")
                       (loop (+ i 1) (cons #\" (cons #\\ result)))]
                      [(char=? c #\newline)
                       (loop (+ i 1) (cons #\n (cons #\\ result)))]
                      [(char=? c #\return)
                       (loop (+ i 1) (cons #\r (cons #\\ result)))]
                      [(char=? c #\tab)
                       (loop (+ i 1) (cons #\t (cons #\\ result)))]
                      [(char<? c #\space)
                       ;; Skip other control characters
                       (loop (+ i 1) result)]
                      [else
                       (loop (+ i 1) (cons c result))]))))))

;;; ============================================================
;;; DOT Format Export (Graphviz)
;;; ============================================================

;;; build-dot-string : FS x (List Bytevector) -> String
;;; Build DOT format string from a list of block hashes.
(define (build-dot-string fs hashes)
  (let ([port (open-output-string)])
       ;; Header
       (put-string port "digraph G {\n")
       (put-string port "  rankdir=TB;\n")
       (put-string port "  node [shape=box, fontname=\"monospace\", fontsize=10];\n")
       (put-string port "  edge [color=\"#666666\"];\n")
       (put-string port "\n")
       
       ;; Track which nodes we've defined
       (let ([defined (make-hashtable equal-hash equal?)])
            
            ;; Define nodes
            (put-string port "  // Nodes\n")
            (for-each
             (lambda (hash)
                     (let ([blk (store-get fs hash)])
                          (when blk
                                (let* ([hash-hex (hash->hex hash)]
                                       [short (hash->short hash)]
                                       [label (escape-dot-string (block->label blk))])
                                      (hashtable-set! defined hash #t)
                                      (put-string port
                                                  (format "  \"~a\" [label=\"~a\\n(~a)\"];\n"
                                                          hash-hex label short))))))
             hashes)
            
            (put-string port "\n")
            
            ;; Define edges
            (put-string port "  // Edges\n")
            (for-each
             (lambda (hash)
                     (let ([blk (store-get fs hash)])
                          (when blk
                                (let* ([hash-hex (hash->hex hash)]
                                       [refs (block-refs blk)])
                                      (let loop ([i 0])
                                           (when (< i (vector-length refs))
                                                 (let* ([ref-hash (vector-ref refs i)]
                                                        [ref-hex (hash->hex ref-hash)])
                                                       ;; Add edge
                                                       (put-string port
                                                                   (format "  \"~a\" -> \"~a\";\n" hash-hex ref-hex))
                                                       ;; Add placeholder node for missing refs
                                                       (unless (hashtable-ref defined ref-hash #f)
                                                               (let ([ref-blk (store-get fs ref-hash)])
                                                                    (if ref-blk
                                                                        (let ([label (escape-dot-string (block->label ref-blk))]
                                                                              [short (hash->short ref-hash)])
                                                                             (hashtable-set! defined ref-hash #t)
                                                                             (put-string port
                                                                                         (format "  \"~a\" [label=\"~a\\n(~a)\", style=dashed];\n"
                                                                                                 ref-hex label short)))
                                                                        (begin
                                                                         (hashtable-set! defined ref-hash #t)
                                                                         (put-string port
                                                                                     (format "  \"~a\" [label=\"[missing]\\n(~a)\", style=dotted, color=red];\n"
                                                                                             ref-hex (hash->short ref-hash))))))))
                                                 (loop (+ i 1))))))))
             hashes))
       
       ;; Footer
       (put-string port "}\n")
       (get-output-string port)))

;;; export-dot-string : FS -> String
;;; Export entire store to DOT format string.
(define (export-dot-string fs)
  (build-dot-string fs (store-all-hashes fs)))

;;; export-dot : FS x String -> void
;;; Export entire store to DOT file.
(define (export-dot fs output-path)
  (let ([dot-str (export-dot-string fs)])
       (call-with-output-file output-path
                              (lambda (port)
                                      (put-string port dot-str)))
       (display (format "Exported graph to ~a\n" output-path))))

;;; export-dot-subset : FS x (List Bytevector) x String -> void
;;; Export specific blocks to DOT file.
(define (export-dot-subset fs hashes output-path)
  (let ([dot-str (build-dot-string fs hashes)])
       (call-with-output-file output-path
                              (lambda (port)
                                      (put-string port dot-str)))
       (display (format "Exported ~a blocks to ~a\n" (length hashes) output-path))))

;;; ============================================================
;;; JSON Format Export (D3.js compatible)
;;; ============================================================

;;; build-json-string : FS x (List Bytevector) -> String
;;; Build JSON format string from a list of block hashes.
(define (build-json-string fs hashes)
  (let ([port (open-output-string)]
        [nodes '()]
        [links '()]
        [defined (make-hashtable equal-hash equal?)])
       
       ;; Collect nodes and links
       (for-each
        (lambda (hash)
                (let ([blk (store-get fs hash)])
                     (when blk
                           (let* ([hash-hex (hash->hex hash)]
                                  [tag (symbol->string (block-tag blk))]
                                  [payload (block-payload blk)]
                                  [payload-str (guard (e [else "[binary]"])
                                                      (utf8->string payload))]
                                  [label (truncate-label payload-str 50)]
                                  [refs (block-refs blk)])
                                 
                                 ;; Add node
                                 (unless (hashtable-ref defined hash #f)
                                         (hashtable-set! defined hash #t)
                                         (set! nodes (cons (list hash-hex tag label) nodes)))
                                 
                                 ;; Add edges and referenced nodes
                                 (let loop ([i 0])
                                      (when (< i (vector-length refs))
                                            (let* ([ref-hash (vector-ref refs i)]
                                                   [ref-hex (hash->hex ref-hash)])
                                                  ;; Add link
                                                  (set! links (cons (cons hash-hex ref-hex) links))
                                                  ;; Add referenced node if not already defined
                                                  (unless (hashtable-ref defined ref-hash #f)
                                                          (let ([ref-blk (store-get fs ref-hash)])
                                                               (hashtable-set! defined ref-hash #t)
                                                               (if ref-blk
                                                                   (let* ([ref-tag (symbol->string (block-tag ref-blk))]
                                                                          [ref-payload (block-payload ref-blk)]
                                                                          [ref-payload-str (guard (e [else "[binary]"])
                                                                                                  (utf8->string ref-payload))]
                                                                          [ref-label (truncate-label ref-payload-str 50)])
                                                                         (set! nodes (cons (list ref-hex ref-tag ref-label) nodes)))
                                                                   (set! nodes (cons (list ref-hex "missing" "[missing block]") nodes))))))
                                            (loop (+ i 1))))))))
        hashes)
       
       ;; Build JSON
       (put-string port "{\n")
       
       ;; Nodes array
       (put-string port "  \"nodes\": [\n")
       (let loop ([node-list (reverse nodes)] [first? #t])
            (when (not (null? node-list))
                  (let* ([node (car node-list)]
                         [id (car node)]
                         [tag (cadr node)]
                         [label (caddr node)])
                        (unless first?
                                (put-string port ",\n"))
                        (put-string port
                                    (format "    {\"id\": \"~a\", \"tag\": \"~a\", \"label\": \"~a\"}"
                                            (escape-json-string id)
                                            (escape-json-string tag)
                                            (escape-json-string label)))
                        (loop (cdr node-list) #f))))
       (put-string port "\n  ],\n")
       
       ;; Links array
       (put-string port "  \"links\": [\n")
       (let loop ([link-list (reverse links)] [first? #t])
            (when (not (null? link-list))
                  (let* ([link (car link-list)]
                         [source (car link)]
                         [target (cdr link)])
                        (unless first?
                                (put-string port ",\n"))
                        (put-string port
                                    (format "    {\"source\": \"~a\", \"target\": \"~a\"}"
                                            (escape-json-string source)
                                            (escape-json-string target)))
                        (loop (cdr link-list) #f))))
       (put-string port "\n  ]\n")
       
       (put-string port "}\n")
       (get-output-string port)))

;;; export-json-string : FS -> String
;;; Export entire store to JSON format string.
(define (export-json-string fs)
  (build-json-string fs (store-all-hashes fs)))

;;; export-json : FS x String -> void
;;; Export entire store to JSON file.
(define (export-json fs output-path)
  (let ([json-str (export-json-string fs)])
       (call-with-output-file output-path
                              (lambda (port)
                                      (put-string port json-str)))
       (display (format "Exported graph to ~a\n" output-path))))

;;; export-json-subset : FS x (List Bytevector) x String -> void
;;; Export specific blocks to JSON file.
(define (export-json-subset fs hashes output-path)
  (let ([json-str (build-json-string fs hashes)])
       (call-with-output-file output-path
                              (lambda (port)
                                      (put-string port json-str)))
       (display (format "Exported ~a blocks to ~a\n" (length hashes) output-path))))

;;; ============================================================
;;; ASCII Visualization (Terminal)
;;; ============================================================

;;; render-ascii : FS x Bytevector x Nat -> void
;;; Render a tree view of a block and its references up to given depth.
(define (render-ascii fs hash depth)
  (display "Block Tree:\n")
  (display "===========\n")
  (render-ascii-tree fs hash "" depth 0 (make-hashtable equal-hash equal?)))

;;; render-ascii-tree : FS x Bytevector x String x Nat x Nat x Hashtable -> void
;;; Recursively render ASCII tree.
(define (render-ascii-tree fs hash prefix max-depth current-depth visited)
  (let ([hash-hex (hash->hex hash)]
        [short (hash->short hash)])
       
       ;; Check for cycles
       (if (hashtable-ref visited hash #f)
           (display (format "~a[cycle: ~a...]\n" prefix short))
           (begin
            (hashtable-set! visited hash #t)
            
            (let ([blk (store-get fs hash)])
                 (if (not blk)
                     (display (format "~a[missing] (~a...)\n" prefix short))
                     (let* ([tag (symbol->string (block-tag blk))]
                            [payload (block-payload blk)]
                            [payload-str (guard (e [else "[binary]"])
                                                (utf8->string payload))]
                            [preview (truncate-label payload-str 30)])
                           
                           ;; Print this node
                           (display (format "~a~a: ~a (~a...)\n" prefix tag preview short))
                           
                           ;; Recurse if not at max depth
                           (when (< current-depth max-depth)
                                 (let ([refs (block-refs blk)])
                                      (let loop ([i 0])
                                           (when (< i (vector-length refs))
                                                 (let* ([ref-hash (vector-ref refs i)]
                                                        [is-last (= i (- (vector-length refs) 1))]
                                                        [connector (if is-last
                                                                       (string-append prefix "    ")
                                                                       (string-append prefix "|   "))]
                                                        [branch (if is-last
                                                                    (string-append prefix "`-- ")
                                                                    (string-append prefix "|-- "))])
                                                       (render-ascii-tree fs ref-hash branch max-depth
                                                                          (+ current-depth 1) visited)
                                                       (loop (+ i 1))))))))))))))

;;; render-ascii-refs : FS x Bytevector -> void
;;; Show a block and its immediate references (depth 1).
(define (render-ascii-refs fs hash)
  (let ([blk (store-get fs hash)])
       (if (not blk)
           (display (format "Block not found: ~a\n" (hash->hex hash)))
           (let* ([tag (symbol->string (block-tag blk))]
                  [short (hash->short hash)]
                  [payload (block-payload blk)]
                  [payload-str (guard (e [else "[binary]"])
                                      (utf8->string payload))]
                  [preview (truncate-label payload-str 40)]
                  [refs (block-refs blk)])
                 
                 ;; Print this node
                 (display (format "~a: ~a (~a...)\n" tag preview short))
                 
                 ;; Print immediate refs
                 (if (= (vector-length refs) 0)
                     (display "  (no references)\n")
                     (let loop ([i 0])
                          (when (< i (vector-length refs))
                                (let* ([ref-hash (vector-ref refs i)]
                                       [ref-short (hash->short ref-hash)]
                                       [ref-blk (store-get fs ref-hash)]
                                       [is-last (= i (- (vector-length refs) 1))]
                                       [branch (if is-last "`-- " "|-- ")])
                                      (if ref-blk
                                          (let* ([ref-tag (symbol->string (block-tag ref-blk))]
                                                 [ref-payload (block-payload ref-blk)]
                                                 [ref-payload-str (guard (e [else "[binary]"])
                                                                         (utf8->string ref-payload))]
                                                 [ref-preview (truncate-label ref-payload-str 35)])
                                                (display (format "~a~a: ~a (~a...)\n"
                                                                 branch ref-tag ref-preview ref-short)))
                                          (display (format "~a[missing] (~a...)\n" branch ref-short))))
                                (loop (+ i 1)))))))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; export-graph : FS x String x Symbol -> void
;;; Export graph in specified format (:dot or :json).
(define (export-graph fs output-path format)
  (case format
        [(dot) (export-dot fs output-path)]
        [(json) (export-json fs output-path)]
        [else (display (format "Unknown format: ~a. Use 'dot or 'json.\n" format))]))

;;; graph-summary : FS -> void
;;; Print a summary of the graph structure.
(define (graph-summary fs)
  (let* ([hashes (store-all-hashes fs)]
         [total-blocks (length hashes)]
         [total-edges 0]
         [orphan-count 0]
         [referenced (make-hashtable equal-hash equal?)])
        
        ;; Count edges and mark referenced blocks
        (for-each
         (lambda (hash)
                 (let ([blk (store-get fs hash)])
                      (when blk
                            (let ([refs (block-refs blk)])
                                 (set! total-edges (+ total-edges (vector-length refs)))
                                 (let loop ([i 0])
                                      (when (< i (vector-length refs))
                                            (hashtable-set! referenced (vector-ref refs i) #t)
                                            (loop (+ i 1))))))))
         hashes)
        
        ;; Count orphans
        (set! orphan-count
              (length (filter (lambda (h) (not (hashtable-ref referenced h #f))) hashes)))
        
        (display "Graph Summary:\n")
        (display "==============\n")
        (display (format "  Total nodes: ~a\n" total-blocks))
        (display (format "  Total edges: ~a\n" total-edges))
        (display (format "  Orphan nodes: ~a\n" orphan-count))
        (display (format "  Avg edges/node: ~a\n"
                         (if (> total-blocks 0)
                             (inexact (/ total-edges total-blocks))
                             0)))))

;;; ============================================================
;;; Module Load Message
;;; ============================================================

(printf "Graph Export loaded.\n")
(printf "  DOT:   (export-dot fs path), (export-dot-string fs)\n")
(printf "  JSON:  (export-json fs path), (export-json-string fs)\n")
(printf "  ASCII: (render-ascii fs hash depth), (render-ascii-refs fs hash)\n")
(printf "  Info:  (graph-summary fs)\n")
