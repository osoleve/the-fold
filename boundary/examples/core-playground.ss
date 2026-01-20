(doc 'module 'core-playground)
(doc 'description "Interactive Core Experimentation Tools - Created by Sauna (sonnet) during Deep Heat session. Makes it easy to explore normalization, expansion, hashing, and blocks without writing test files.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Features:")
(doc 'note "- Normalization experiments (S-expr → de Bruijn)")
(doc 'note "- Expansion experiments (canonical → S-expr)")
(doc 'note "- Round-trip testing (normalize + expand)")
(doc 'note "- Expression hashing (content-addressed expressions)")
(doc 'note "- Block construction and inspection")
(doc 'note "- Free variable analysis")

(doc 'usage "
  (try-normalize '(lambda (x) (+ x 1)))
  (try-roundtrip '(lambda (x) x))
  (hash-expr '(lambda (x) x))
  (try-free-vars '(+ x y))
  (try-block 'my-tag #vu8(1 2 3) '())
")

(doc 'section 'dependencies)
(doc 'note "These should be loaded by repl.ss or load-core:")
(doc 'note "core/block.ss, core/sha256.ss, core/normalize.ss, core/expand.ss")

(doc 'section 'pretty-printing-utilities)

(doc print-boxed 'type '(-> String String void))
(doc print-boxed 'description "Print content in a nice box with a title")
(define (print-boxed title content)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║ ")
  (display title)
  (display (make-string (max 0 (- 60 (string-length title))) #\space))
  (display "║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display content)
  (display "\n"))

;;; print-section : String String → void
;;; Print a labeled section.
(define (print-section label content)
  (display (format "~a:\n  " label))
  (display content)
  (display "\n\n"))

(doc 'section 'normalization-experiments)

(doc try-normalize 'type '(-> Expr void))
(doc try-normalize 'description "Show the normalized (de Bruijn) form of an expression")
(define (try-normalize expr)
  (print-boxed "NORMALIZE" "")
  (print-section "Input" (sexpr->string expr))
  (let ([normalized (normalize expr)])
       (print-section "Normalized (de Bruijn)" (sexpr->string normalized)))
  (display "Note: Variable names become indices, alpha-equivalent forms hash the same.\n"))

;;; try-free-vars : Expr → void
;;; Show the free variables in an expression.
(define (try-free-vars expr)
  (print-boxed "FREE VARIABLES" "")
  (print-section "Expression" (sexpr->string expr))
  (let ([fv (free-vars expr)])
       (if (null? fv)
           (display "  No free variables (closed expression)\n\n")
           (print-section "Free variables" (sexpr->string fv)))))

(doc 'section 'expansion-experiments)

;;; try-expand : Expr × List<Symbol> → void
;;; Expand a normalized expression with specific variable names.
(define (try-expand canonical symbols)
  (print-boxed "EXPAND" "")
  (print-section "Canonical form" (sexpr->string canonical))
  (print-section "Symbol supply" (sexpr->string symbols))
  (let ([expanded (expand canonical symbols)])
       (print-section "Expanded" (sexpr->string expanded))
       (display "Note: Same canonical form can be spelled many ways.\n")))

;;; try-expand-fresh : Expr → void
;;; Expand a normalized expression with fresh generated symbols.
(define (try-expand-fresh canonical)
  (print-boxed "EXPAND (FRESH SYMBOLS)" "")
  (print-section "Canonical form" (sexpr->string canonical))
  (let ([expanded (expand-fresh canonical)])
       (print-section "Expanded with fresh symbols" (sexpr->string expanded))))

(doc 'section 'round-trip-testing)

;;; try-roundtrip : Expr × [List<Symbol>] → void
;;; Normalize an expression then expand it back.
;;; Shows the full normalize → expand cycle.
(define (try-roundtrip expr . symbol-args)
  (print-boxed "ROUND-TRIP (NORMALIZE → EXPAND)" "")
  (print-section "Original" (sexpr->string expr))
  
  (let ([normalized (normalize expr)])
       (print-section "After normalize" (sexpr->string normalized))
       
       (let ([expanded (if (null? symbol-args)
                           (expand-fresh normalized)
                           (expand normalized (car symbol-args)))])
            (print-section "After expand" (sexpr->string expanded))
            
            ;; Check if they're alpha-equivalent by re-normalizing
            (let ([renormalized (normalize expanded)])
                 (if (equal? normalized renormalized)
                     (display "✓ Round-trip successful: normalized forms match!\n")
                     (begin
                      (display "✗ Round-trip failed: normalized forms differ!\n")
                      (print-section "Re-normalized" (sexpr->string renormalized))))))))

(doc 'section 'hashing-experiments)

;;; hash-expr : Expr → Bytevector
;;; Normalize an expression and return its hash.
(define (hash-expr expr)
  (let* ([normalized (normalize expr)]
         [str (sexpr->string normalized)]
         [bytes (string->utf8 str)])
        (sha256 bytes)))

;;; hash-expr-hex : Expr → String
;;; Normalize an expression and return its hash as hex string.
(define (hash-expr-hex expr)
  (hash->hex (hash-expr expr)))

;;; try-hash : Expr → void
;;; Show the hash of a normalized expression.
(define (try-hash expr)
  (print-boxed "EXPRESSION HASH" "")
  (print-section "Expression" (sexpr->string expr))
  
  (let ([normalized (normalize expr)])
       (print-section "Normalized" (sexpr->string normalized))
       
       (let ([hash-hex (hash-expr-hex expr)])
            (display (format "Hash: ~a\n\n" hash-hex))
            (display "Note: Alpha-equivalent expressions have the same hash.\n"))))

;;; try-hash-compare : Expr × Expr → void
;;; Compare hashes of two expressions.
(define (try-hash-compare expr1 expr2)
  (print-boxed "HASH COMPARISON" "")
  (print-section "Expression 1" (sexpr->string expr1))
  (let ([hash1 (hash-expr-hex expr1)])
       (display (format "  Hash: ~a\n\n" hash1))
       
       (print-section "Expression 2" (sexpr->string expr2))
       (let ([hash2 (hash-expr-hex expr2)])
            (display (format "  Hash: ~a\n\n" hash2))
            
            (if (string=? hash1 hash2)
                (display "✓ Hashes match! Expressions are alpha-equivalent.\n")
                (display "✗ Hashes differ. Expressions are NOT alpha-equivalent.\n")))))

(doc 'section 'block-experiments)

;;; try-block : Symbol × Bytevector × List<Bytevector> → void
;;; Build a block and show its structure and hash.
(define (try-block tag payload refs)
  (print-boxed "BLOCK CONSTRUCTION" "")
  (let ([blk (make-block tag payload (list->vector refs))])
       (print-section "Tag" (symbol->string tag))
       (print-section "Payload size" (format "~a bytes" (bytevector-length payload)))
       (print-section "Ref count" (format "~a" (length refs)))
       
       (let ([hash (hash-block blk)])
            (display (format "Block hash: ~a\n\n" (hash->hex hash)))
            
            ;; Show serialization size
            (let ([serialized (block->bytes blk)])
                 (display (format "Serialized size: ~a bytes\n" (bytevector-length serialized)))))))

;;; inspect-block : Block → void
;;; Show detailed information about a block structure.
(define (inspect-block blk)
  (print-boxed "BLOCK INSPECTION" "")
  (let ([tag (block-tag blk)]
        [payload (block-payload blk)]
        [refs (block-refs blk)])
       (print-section "Tag" (symbol->string tag))
       (print-section "Payload" (format "~a bytes" (bytevector-length payload)))
       (print-section "References" (format "~a refs" (vector-length refs)))
       
       (let ([hash (hash-block blk)])
            (display (format "Hash: ~a\n\n" (hash->hex hash))))
       
       ;; Show first 64 bytes of payload if available
       (when (> (bytevector-length payload) 0)
             (let* ([preview-len (min 64 (bytevector-length payload))]
                    [preview (make-bytevector preview-len)])
                   (bytevector-copy! payload 0 preview 0 preview-len)
                   (display "Payload preview (first 64 bytes):\n  ")
                   (display preview)
                   (display "\n")
                   (when (> (bytevector-length payload) 64)
                         (display "  ... (truncated)\n"))))))

;;; try-block-roundtrip : Block → void
;;; Serialize and deserialize a block, verify they match.
(define (try-block-roundtrip blk)
  (print-boxed "BLOCK ROUND-TRIP (SERIALIZE → DESERIALIZE)" "")
  (display "Original block:\n")
  (let ([tag (block-tag blk)]
        [payload (block-payload blk)]
        [refs (block-refs blk)])
       (display (format "  Tag: ~a\n" tag))
       (display (format "  Payload: ~a bytes\n" (bytevector-length payload)))
       (display (format "  Refs: ~a\n\n" (vector-length refs)))
       
       (let* ([serialized (block->bytes blk)]
              [deserialized (bytes->block serialized)])
             (display (format "Serialized to ~a bytes\n\n" (bytevector-length serialized)))
             
             (let ([tag2 (block-tag deserialized)]
                   [payload2 (block-payload deserialized)]
                   [refs2 (block-refs deserialized)])
                  (display "Deserialized block:\n")
                  (display (format "  Tag: ~a\n" tag2))
                  (display (format "  Payload: ~a bytes\n" (bytevector-length payload2)))
                  (display (format "  Refs: ~a\n\n" (vector-length refs2)))
                  
                  (if (block-equal? blk deserialized)
                      (display "✓ Round-trip successful! Blocks match.\n")
                      (display "✗ Round-trip failed! Blocks differ.\n"))))))

(doc 'section 'quick-demos)

;;; playground-demo : → void
;;; Run a quick demo of all playground features.
(define (playground-demo)
  (display "\n")
  (display "════════════════════════════════════════════════════════════════\n")
  (display "           CORE PLAYGROUND DEMO\n")
  (display "════════════════════════════════════════════════════════════════\n")
  (display "\n")
  
  ;; Normalization
  (try-normalize '(lambda (x) (+ x 1)))
  (display "────────────────────────────────────────────────────────────────\n\n")
  
  ;; Hash comparison
  (try-hash-compare '(lambda (x) x) '(lambda (y) y))
  (display "────────────────────────────────────────────────────────────────\n\n")
  
  ;; Free variables
  (try-free-vars '(+ x y z))
  (display "────────────────────────────────────────────────────────────────\n\n")
  
  ;; Block construction
  (try-block 'example (string->utf8 "Hello, Fold!") '())
  (display "────────────────────────────────────────────────────────────────\n\n")
  
  (display "Demo complete! Try the commands yourself.\n")
  (display "Use (playground-help) for command reference.\n\n"))

;;; playground-help : → void
;;; Show available playground commands.
(define (playground-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────┐\n")
  (display "  │              CORE PLAYGROUND COMMANDS                          │\n")
  (display "  └────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  NORMALIZATION:\n")
  (display "    (try-normalize expr)       Show de Bruijn form\n")
  (display "    (try-free-vars expr)       Show free variables\n")
  (display "\n")
  (display "  EXPANSION:\n")
  (display "    (try-expand canonical syms) Expand with specific symbols\n")
  (display "    (try-expand-fresh canon)    Expand with fresh symbols\n")
  (display "\n")
  (display "  ROUND-TRIP:\n")
  (display "    (try-roundtrip expr [syms]) Normalize then expand\n")
  (display "\n")
  (display "  HASHING:\n")
  (display "    (try-hash expr)             Show expression hash\n")
  (display "    (hash-expr expr)            Get hash as bytevector\n")
  (display "    (hash-expr-hex expr)        Get hash as hex string\n")
  (display "    (try-hash-compare e1 e2)    Compare two expression hashes\n")
  (display "\n")
  (display "  BLOCKS:\n")
  (display "    (try-block tag payload refs) Build and inspect a block\n")
  (display "    (inspect-block blk)          Show block details\n")
  (display "    (try-block-roundtrip blk)    Test serialize/deserialize\n")
  (display "\n")
  (display "  UTILITIES:\n")
  (display "    (playground-demo)            Run a quick demo\n")
  (display "    (playground-help)            Show this help\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (try-normalize '(lambda (x) (+ x 1)))\n")
  (display "    (try-hash-compare '(lambda (x) x) '(lambda (y) y))\n")
  (display "    (try-free-vars '(+ x y))\n")
  (display "    (try-block 'test (string->utf8 \"data\") '())\n")
  (display "\n"))
