(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'turtle-block)
(doc 'description "CAS Integration for Turtle Graphics - Stores turtle drawings in the content-addressed store (CAS). Same drawing = same hash, forever.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Block Structure:")
(doc 'note "  tag: 'turtle-drawing")
(doc 'note "  payload: S-expression serialization of drawing")
(doc 'note "  refs: [] (self-contained)")

(doc 'note "Dependencies:")
(doc 'note "  - core/block.ss (make-block, block-tag, block-payload)")
(doc 'note "  - core/cas.ss (store!, fetch)")
(doc 'note "  - boundary/turtle-color.ss (color12->list, list->color12)")
(doc 'note "  - boundary/turtle-path.ss (path-cmd->sexpr, sexpr->path-cmd)")
(doc 'note "  - boundary/ui/turtle.ss (drawing record)")

(doc 'section 'drawing-serialization)

(define (drawing->sexpr drawing)
  (doc 'type (-> Drawing S-expr))
  (doc 'description "Convert a drawing to a pure S-expression for storage.")
  `((version . 1)
    (width . ,(drawing-width drawing))
    (height . ,(drawing-height drawing))
    (bg-color . ,(color12->list (drawing-bg-color drawing)))
    (paths . ,(map path-cmd->sexpr (drawing-paths drawing)))
    (metadata . ,(drawing-metadata drawing))))

(define (sexpr->drawing sexpr)
  (doc 'type (-> S-expr Drawing))
  (doc 'description "Reconstruct a drawing from an S-expression.")
  (let* ([width (cdr (assq 'width sexpr))]
         [height (cdr (assq 'height sexpr))]
         [bg-color (list->color12 (cdr (assq 'bg-color sexpr)))]
         [paths-sexpr (cdr (assq 'paths sexpr))]
         [paths (map sexpr->path-cmd paths-sexpr)]
         [metadata (cdr (assq 'metadata sexpr))])
        (make-drawing% width height bg-color paths metadata)))

(doc 'section 'block-conversion)

(define (drawing->block drawing)
  (doc 'type (-> Drawing Block))
  (doc 'description "Serialize a turtle drawing to a content-addressed block.")
  (let* ([sexpr (drawing->sexpr drawing)]
         [payload (string->utf8 (sexpr->string sexpr))])
        (make-block 'turtle-drawing payload '#())))

(define (block->drawing blk)
  (doc 'type (-> Block (U Drawing Bool)))
  (doc 'description "Deserialize a turtle drawing from a block. Returns #f if the block is not a valid turtle drawing.")
  (if (not (eq? (block-tag blk) 'turtle-drawing))
      #f
      (let* ([payload (block-payload blk)]
             [str (utf8->string payload)]
             [sexpr (read (open-input-string str))])
            (sexpr->drawing sexpr))))

(doc 'section 'cas-operations)

(define (store-drawing! drawing)
  (doc 'type (-> Drawing Bytevector))
  (doc 'description "Store a drawing in the CAS and return its hash.")
  (store! (drawing->block drawing)))

(define (fetch-drawing hash)
  (doc 'type (-> Bytevector (U Drawing Bool)))
  (doc 'description "Fetch a drawing by its hash from the CAS.")
  (let ([blk (fetch hash)])
       (if blk
           (block->drawing blk)
           #f)))

(define (store-turtle! t)
  (doc 'type (-> Turtle Bytevector))
  (doc 'description "Store a turtle's current drawing state in the CAS.")
  (store-drawing! (turtle->drawing t)))

(doc 'section 'svg-block-storage)

(define (store-drawing-svg! drawing)
  (doc 'type (-> Drawing Bytevector))
  (doc 'description "Store the SVG representation of a drawing as a separate block. This caches the rendered output for quick retrieval.")
  (let* ([svg (drawing->svg drawing)]
         [payload (string->utf8 svg)])
        (store! (make-block 'turtle-svg payload '#()))))

(define (fetch-svg hash)
  (doc 'type (-> Bytevector (U String Bool)))
  (doc 'description "Fetch an SVG string by its hash from the CAS.")
  (let ([blk (fetch hash)])
       (if (and blk (eq? (block-tag blk) 'turtle-svg))
           (utf8->string (block-payload blk))
           #f)))

(doc 'section 'convenience-functions)

(define (hash-drawing drawing)
  (doc 'type (-> Drawing Bytevector))
  (doc 'description "Compute the hash of a drawing without storing it.")
  (hash-block (drawing->block drawing)))

(define (drawing-stored? drawing)
  (doc 'type (-> Drawing Bool))
  (doc 'description "Check if a drawing is already in the store.")
  (stored? (hash-drawing drawing)))

(doc 'section 'helper-functions)

(doc 'note "sexpr->string may already be in prelude")

(define (sexpr->string obj)
  (doc 'type (-> Any String))
  (doc 'description "Convert any S-expression to a string.")
  (let ([port (open-output-string)])
       (write obj port)
       (get-output-string port)))
