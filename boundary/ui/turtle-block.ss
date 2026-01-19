;;; boundary/turtle-block.ss — CAS Integration for Turtle Graphics
;;;
;;; Stores turtle drawings in the content-addressed store (CAS).
;;; Same drawing = same hash, forever.
;;;
;;; Block Structure:
;;;   tag: 'turtle-drawing
;;;   payload: S-expression serialization of drawing
;;;   refs: [] (self-contained)
;;;
;;; This is Shell code: integrates with CAS for persistence.
;;;
;;; Dependencies:
;;;   - core/block.ss (make-block, block-tag, block-payload)
;;;   - core/cas.ss (store!, fetch)
;;;   - boundary/turtle-color.ss (color12->list, list->color12)
;;;   - boundary/turtle-path.ss (path-cmd->sexpr, sexpr->path-cmd)
;;;   - boundary/ui/turtle.ss (drawing record)

;;; ====
;;; Drawing Serialization
;;; ====

;;; drawing->sexpr : Drawing -> S-expr
;;; Convert a drawing to a pure S-expression for storage.
(define (drawing->sexpr drawing)
  `((version . 1)
    (width . ,(drawing-width drawing))
    (height . ,(drawing-height drawing))
    (bg-color . ,(color12->list (drawing-bg-color drawing)))
    (paths . ,(map path-cmd->sexpr (drawing-paths drawing)))
    (metadata . ,(drawing-metadata drawing))))

;;; sexpr->drawing : S-expr -> Drawing
;;; Reconstruct a drawing from an S-expression.
(define (sexpr->drawing sexpr)
  (let* ([width (cdr (assq 'width sexpr))]
         [height (cdr (assq 'height sexpr))]
         [bg-color (list->color12 (cdr (assq 'bg-color sexpr)))]
         [paths-sexpr (cdr (assq 'paths sexpr))]
         [paths (map sexpr->path-cmd paths-sexpr)]
         [metadata (cdr (assq 'metadata sexpr))])
        (make-drawing% width height bg-color paths metadata)))

;;; ====
;;; Block Conversion
;;; ====

;;; drawing->block : Drawing -> Block
;;; Serialize a turtle drawing to a content-addressed block.
(define (drawing->block drawing)
  (let* ([sexpr (drawing->sexpr drawing)]
         [payload (string->utf8 (sexpr->string sexpr))])
        (make-block 'turtle-drawing payload '#())))

;;; block->drawing : Block -> Drawing | #f
;;; Deserialize a turtle drawing from a block.
;;; Returns #f if the block is not a valid turtle drawing.
(define (block->drawing blk)
  (if (not (eq? (block-tag blk) 'turtle-drawing))
      #f
      (let* ([payload (block-payload blk)]
             [str (utf8->string payload)]
             [sexpr (read (open-input-string str))])
            (sexpr->drawing sexpr))))

;;; ====
;;; CAS Store/Fetch Operations
;;; ====

;;; store-drawing! : Drawing -> Bytevector
;;; Store a drawing in the CAS and return its hash.
(define (store-drawing! drawing)
  (store! (drawing->block drawing)))

;;; fetch-drawing : Bytevector -> Drawing | #f
;;; Fetch a drawing by its hash from the CAS.
(define (fetch-drawing hash)
  (let ([blk (fetch hash)])
       (if blk
           (block->drawing blk)
           #f)))

;;; store-turtle! : Turtle -> Bytevector
;;; Store a turtle's current drawing state in the CAS.
(define (store-turtle! t)
  (store-drawing! (turtle->drawing t)))

;;; ====
;;; SVG Block Storage
;;; ====

;;; store-drawing-svg! : Drawing -> Bytevector
;;; Store the SVG representation of a drawing as a separate block.
;;; This caches the rendered output for quick retrieval.
(define (store-drawing-svg! drawing)
  (let* ([svg (drawing->svg drawing)]
         [payload (string->utf8 svg)])
        (store! (make-block 'turtle-svg payload '#()))))

;;; fetch-svg : Bytevector -> String | #f
;;; Fetch an SVG string by its hash from the CAS.
(define (fetch-svg hash)
  (let ([blk (fetch hash)])
       (if (and blk (eq? (block-tag blk) 'turtle-svg))
           (utf8->string (block-payload blk))
           #f)))

;;; ====
;;; Convenience Functions
;;; ====

;;; hash-drawing : Drawing -> Bytevector
;;; Compute the hash of a drawing without storing it.
(define (hash-drawing drawing)
  (hash-block (drawing->block drawing)))

;;; drawing-stored? : Drawing -> Bool
;;; Check if a drawing is already in the store.
(define (drawing-stored? drawing)
  (stored? (hash-drawing drawing)))

;;; ====
;;; Helper: sexpr->string (may already be in prelude)
;;; ====

;;; sexpr->string : Any -> String
;;; Convert any S-expression to a string.
(define (sexpr->string obj)
  (let ([port (open-output-string)])
       (write obj port)
       (get-output-string port)))
