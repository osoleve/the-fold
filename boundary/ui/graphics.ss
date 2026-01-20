(doc 'module 'graphics)
(doc 'description "Graphics engine foundation - unified visual primitives with block substrate")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "The unified graphics engine for The Fold, integrating all visual primitives")
(doc 'note "with the block substrate. Provides the rendering pipeline for DUCKIE and")
(doc 'note "other visual entities")

(doc 'note "Architecture:")
(doc 'note "- Canvas Layer: 2D character grids (from boundary/ui/layout.ss)")
(doc 'note "- Color Layer: RGB/palette colors + ANSI output (from boundary/ui/color.ss)")
(doc 'note "- Primitive Layer: Shapes, lines, circles (from boundary/ui/graphics-primitives.ss)")
(doc 'note "- Composition Layer: Transparency, z-ordering (from boundary/ui/layers.ss)")
(doc 'note "- Animation Layer: Easing functions (from boundary/ui/animation.ss)")
(doc 'note "- Block Layer: Content-addressed storage of graphics (THIS FILE)")

(doc 'note "Block Integration:")
(doc 'note "- Canvases can be stored as blocks")
(doc 'note "- Layers can be stored as blocks")
(doc 'note "- Complete scenes (layer stacks) can be stored as blocks")
(doc 'note "- Rendering pipelines reference graphics by hash")

(doc 'note "Rendering Pipeline:")
(doc 'note "- Render targets: terminal, string, file, block")
(doc 'note "- Render modes: ASCII-only, ANSI color, Unicode")
(doc 'note "- Frame buffering for animation")

(doc 'section 'dependencies)

(doc 'dependencies "This file expects the following to already be loaded:")
(doc 'dependencies "- core/block.ss")
(doc 'dependencies "- core/sha256.ss")
(doc 'dependencies "- core/cas.ss")
(doc 'dependencies "- boundary/io/fs.ss")
(doc 'dependencies "- boundary/ui/layout.ss")
(doc 'dependencies "- boundary/ui/color.ss")
(doc 'dependencies "Load them via boundary/repl/repl.ss or manually before loading this file")
(doc 'dependencies "Note: graphics-primitives.ss, layers.ss, and animation.ss are libraries")
(doc 'dependencies "They will be loaded when needed via import")

(doc 'section 'canvas-block-storage)

(doc canvas->block 'type (-> Canvas Block))
(doc canvas->block 'description "Convert a canvas to a content-addressed block")
(doc canvas->block 'note "Block structure:")
(doc canvas->block 'note "tag: 'canvas")
(doc canvas->block 'note "payload: Serialized canvas data")
(doc canvas->block 'note "[width : 4 bytes (u32 little-endian)]")
(doc canvas->block 'note "[height : 4 bytes (u32 little-endian)]")
(doc canvas->block 'note "[cells : width×height bytes (UTF-8 characters)]")
(doc canvas->block 'note "refs: []")
(doc canvas->block 'note "This is a simplified ASCII-only format. For color support,")
(doc canvas->block 'note "use colored-canvas->block which stores Cell data")
(define (canvas->block canvas)
  (let* ([w (canvas-width canvas)]
         [h (canvas-height canvas)]
         [cells (canvas-cells canvas)]
         [cell-count (* w h)])
        ;; Serialize canvas: [width][height][cells...]
        (let* ([payload-parts
                (list
                 (u32->bytes-le w)
                 (u32->bytes-le h)
                 ;; Convert character vector to UTF-8 bytes
                 (string->utf8
                  (list->string (vector->list cells))))]
               [payload (bytevector-concat payload-parts)])
              (make-block 'canvas payload empty-refs))))

(doc block->canvas 'type (-> Block (Union Canvas Bool)))
(doc block->canvas 'description "Reconstruct a canvas from a block")
(doc block->canvas 'returns "#f if block is not a valid canvas block")
(define (block->canvas blk)
  (if (not (eq? (block-tag blk) 'canvas))
      #f
      (let* ([payload (block-payload blk)]
             [w (bytes-le->u32 payload 0)]
             [h (bytes-le->u32 payload 4)]
             [cells-bytes (make-bytevector (- (bytevector-length payload) 8))])
            (bytevector-copy! payload 8 cells-bytes 0 (bytevector-length cells-bytes))
            (let* ([cell-str (utf8->string cells-bytes)]
                   [cell-list (string->list cell-str)]
                   [cells (list->vector cell-list)])
                  (make-canvas% w h cells)))))

(doc store-canvas! 'type (-> FS Canvas Bytevector))
(doc store-canvas! 'description "Store a canvas in the CAS and return its hash")
(define (store-canvas! fs canvas)
  (fs-store! fs (canvas->block canvas)))

(doc fetch-canvas 'type (-> FS Bytevector (Union Canvas Bool)))
(doc fetch-canvas 'description "Fetch a canvas by its hash from the CAS")
(define (fetch-canvas fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->canvas blk)
           #f)))

(doc 'section 'colored-canvas-block-storage)

(doc 'note "Color type codes for serialization")
(define COLOR-TYPE-DEFAULT 0)
(define COLOR-TYPE-RGB 1)
(define COLOR-TYPE-PALETTE 2)

(doc clamp-byte 'type (-> Integer Byte))
(doc clamp-byte 'description "Clamp an integer to valid byte range [0, 255]")
(define (clamp-byte n)
  (cond
   [(not (integer? n)) 0]
   [(< n 0) 0]
   [(> n 255) 255]
   [else n]))

(doc serialize-color 'type (-> Color Bytevector))
(doc serialize-color 'description "Serialize a color to bytes")
(doc serialize-color 'note "Format: [type : 1 byte][data : 0-3 bytes]")
(doc serialize-color 'note "Defensive: Clamps RGB/palette values to valid byte range")
(define (serialize-color c)
  (cond
   [(color-default? c)
    (let ([bv (make-bytevector 1)])
         (bytevector-u8-set! bv 0 COLOR-TYPE-DEFAULT)
         bv)]
   [(color-rgb? c)
    (let ([bv (make-bytevector 4)]
          [r (clamp-byte (cadr c))]
          [g (clamp-byte (caddr c))]
          [b (clamp-byte (cadddr c))])
         (bytevector-u8-set! bv 0 COLOR-TYPE-RGB)
         (bytevector-u8-set! bv 1 r)
         (bytevector-u8-set! bv 2 g)
         (bytevector-u8-set! bv 3 b)
         bv)]
   [(color-palette? c)
    (let ([bv (make-bytevector 2)]
          [n (clamp-byte (cadr c))])
         (bytevector-u8-set! bv 0 COLOR-TYPE-PALETTE)
         (bytevector-u8-set! bv 1 n)
         bv)]
   [else
    ;; Fallback to default
    (let ([bv (make-bytevector 1)])
         (bytevector-u8-set! bv 0 COLOR-TYPE-DEFAULT)
         bv)]))

(doc deserialize-color 'type (-> Bytevector Nat (Union (Pair Color Nat) Bool)))
(doc deserialize-color 'description "Deserialize a color from bytes at given offset")
(doc deserialize-color 'returns "(color . new-offset) or #f if bytevector is too short")
(doc deserialize-color 'note "Defensive: Validates bytevector bounds before reading")
(define (deserialize-color bv offset)
  (let ([len (bytevector-length bv)])
       ;; Need at least 1 byte for type
       (if (>= offset len)
           #f
           (let ([type (bytevector-u8-ref bv offset)])
                (cond
                 [(= type COLOR-TYPE-DEFAULT)
                  (cons color-default (+ offset 1))]
                 [(= type COLOR-TYPE-RGB)
                  ;; Need 4 bytes total (type + 3 RGB)
                  (if (> (+ offset 4) len)
                      #f
                      (let ([r (bytevector-u8-ref bv (+ offset 1))]
                            [g (bytevector-u8-ref bv (+ offset 2))]
                            [b (bytevector-u8-ref bv (+ offset 3))])
                           (cons (make-color-rgb r g b) (+ offset 4))))]
                 [(= type COLOR-TYPE-PALETTE)
                  ;; Need 2 bytes total (type + palette index)
                  (if (> (+ offset 2) len)
                      #f
                      (let ([n (bytevector-u8-ref bv (+ offset 1))])
                           (cons (make-color-palette n) (+ offset 2))))]
                 [else
                  ;; Unknown type, treat as default
                  (cons color-default (+ offset 1))])))))

(doc colored-canvas->block 'type (-> Canvas (Vector Cell) Block))
(doc colored-canvas->block 'description "Store a canvas with full color information")
(doc colored-canvas->block 'note "Block structure:")
(doc colored-canvas->block 'note "tag: 'colored-canvas")
(doc colored-canvas->block 'note "payload: Serialized colored canvas data")
(doc colored-canvas->block 'note "[width : 4 bytes]")
(doc colored-canvas->block 'note "[height : 4 bytes]")
(doc colored-canvas->block 'note "For each cell (row-major order):")
(doc colored-canvas->block 'note "[char : 4 bytes (UTF-32 character code)]")
(doc colored-canvas->block 'note "[fg-type : 1 byte (0=default, 1=rgb, 2=palette)]")
(doc colored-canvas->block 'note "[fg-data : variable (0 bytes for default, 3 for rgb, 1 for palette)]")
(doc colored-canvas->block 'note "[bg-type : 1 byte]")
(doc colored-canvas->block 'note "[bg-data : variable]")
(doc colored-canvas->block 'note "refs: []")
(doc colored-canvas->block 'note "This is more complex but preserves full color data")
(doc colored-canvas->block 'note "For simple ASCII rendering, use canvas->block instead")
(define (colored-canvas->block canvas color-cells)
  (let* ([w (canvas-width canvas)]
         [h (canvas-height canvas)]
         [cell-count (* w h)]
         ;; Build list of payload parts
         [header (list (u32->bytes-le w) (u32->bytes-le h))]
         ;; Serialize each cell
         [cell-parts
          (let loop ([i 0] [acc '()])
               (if (>= i cell-count)
                   (reverse acc)
                   (let* ([cell (vector-ref color-cells i)]
                          [ch (cell%-char cell)]
                          [fg (cell%-fg cell)]
                          [bg (cell%-bg cell)]
                          ;; Character as UTF-32 (4 bytes, little-endian)
                          [char-bv (u32->bytes-le (char->integer ch))]
                          ;; Foreground color
                          [fg-bv (serialize-color fg)]
                          ;; Background color
                          [bg-bv (serialize-color bg)])
                         (loop (+ i 1)
                               (cons bg-bv
                                     (cons fg-bv
                                           (cons char-bv acc)))))))]
         [payload (bytevector-concat (append header cell-parts))])
        (make-block 'colored-canvas payload empty-refs)))

(doc valid-char-code? 'type (-> Integer Boolean))
(doc valid-char-code? 'description "Check if an integer is a valid Unicode code point")
(doc valid-char-code? 'note "Excludes surrogates (0xD800-0xDFFF) and values > 0x10FFFF")
(define (valid-char-code? n)
  (and (integer? n)
       (>= n 0)
       (<= n #x10FFFF)
       (not (and (>= n #xD800) (<= n #xDFFF)))))

(doc safe-integer->char 'type (-> Integer Char))
(doc safe-integer->char 'description "Convert integer to char, returning space for invalid codes")
(define (safe-integer->char n)
  (if (valid-char-code? n)
      (integer->char n)
      #\space))


(doc block->colored-canvas 'type (-> Block (Union (Pair Canvas (Vector Cell)) Bool)))
(doc block->colored-canvas 'description "Reconstruct a colored canvas from a block")
(doc block->colored-canvas 'returns "(canvas . color-cells) or #f if block is invalid")
(doc block->colored-canvas 'note "Defensive: Validates payload bounds and character codes")
(define (block->colored-canvas blk)
  (if (not (eq? (block-tag blk) 'colored-canvas))
      #f
      (parse-colored-canvas-payload (block-payload blk))))

(doc parse-colored-canvas-payload 'type (-> Bytevector (Union (Pair Canvas (Vector Cell)) Bool)))
(doc parse-colored-canvas-payload 'description "Internal: Parse colored canvas from payload bytes")
(define (parse-colored-canvas-payload payload)
  (let ([payload-len (bytevector-length payload)])
       ;; Need at least 8 bytes for width and height
       (if (< payload-len 8)
           #f
           (let ([w (bytes-le->u32 payload 0)]
                 [h (bytes-le->u32 payload 4)])
                ;; Sanity check dimensions (max 10000x10000 to prevent DoS)
                (if (or (> w 10000) (> h 10000) (= w 0) (= h 0))
                    #f
                    (parse-colored-canvas-cells payload payload-len w h))))))

(doc parse-colored-canvas-cells 'type (-> Bytevector Nat Nat Nat (Union (Pair Canvas (Vector Cell)) Bool)))
(doc parse-colored-canvas-cells 'description "Internal: Parse cell data from payload")
(define (parse-colored-canvas-cells payload payload-len w h)
  (let* ([cell-count (* w h)]
         [canvas-cells (make-vector cell-count #\space)]
         [color-cells (make-vector cell-count #f)])
        (let loop ([i 0] [offset 8])
             (cond
              ;; Done - build and return result
              [(>= i cell-count)
               (cons (make-canvas% w h canvas-cells) color-cells)]
              ;; Not enough bytes for char code
              [(> (+ offset 4) payload-len)
               #f]
              ;; Parse one cell
              [else
               (let ([result (parse-one-cell payload offset)])
                    (if (not result)
                        #f
                        (let ([ch (car result)]
                              [cell (cadr result)]
                              [next-offset (cddr result)])
                             (vector-set! canvas-cells i ch)
                             (vector-set! color-cells i cell)
                             (loop (+ i 1) next-offset))))]))))

(doc parse-one-cell 'type (-> Bytevector Nat (Union (Cons Char (Cons Cell Nat)) Bool)))
(doc parse-one-cell 'description "Internal: Parse one cell from payload at offset")
(doc parse-one-cell 'returns "(char cell . new-offset) or #f on failure")
(define (parse-one-cell payload offset)
  (let* ([char-code (bytes-le->u32 payload offset)]
         [ch (safe-integer->char char-code)]
         [fg-result (deserialize-color payload (+ offset 4))])
        (if (not fg-result)
            #f
            (let ([bg-result (deserialize-color payload (cdr fg-result))])
                 (if (not bg-result)
                     #f
                     (let ([cell (make-cell ch (car fg-result) (car bg-result))])
                          (cons ch (cons cell (cdr bg-result)))))))))

(doc store-colored-canvas! 'type (-> FS Canvas (Vector Cell) Bytevector))
(doc store-colored-canvas! 'description "Store a colored canvas in the CAS and return its hash")
(define (store-colored-canvas! fs canvas color-cells)
  (fs-store! fs (colored-canvas->block canvas color-cells)))

(doc fetch-colored-canvas 'type (-> FS Bytevector (Union (Pair Canvas (Vector Cell)) Bool)))
(doc fetch-colored-canvas 'description "Fetch a colored canvas by its hash from the CAS")
(define (fetch-colored-canvas fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->colored-canvas blk)
           #f)))

(doc 'section 'scene-block-storage)

(doc 'note "A Scene is a complete visual composition ready to render")
(doc 'note "It contains:")
(doc 'note "- A layer stack (ordered layers with z-depth)")
(doc 'note "- Metadata (name, dimensions, timestamp)")
(doc 'note "Block structure:")
(doc 'note "tag: 'scene")
(doc 'note "payload: Serialized metadata")
(doc 'note "[width : 4 bytes]")
(doc 'note "[height : 4 bytes]")
(doc 'note "[name : length-prefixed UTF-8 string]")
(doc 'note "[timestamp : length-prefixed UTF-8 string]")
(doc 'note "[layer-count : 4 bytes]")
(doc 'note "refs: [layer-hash₀, layer-hash₁, ..., layer-hashₙ]")
(doc 'note "Each layer is stored as a separate canvas block")

(define-record-type scene%
  (fields width height name timestamp layer-hashes))

(doc make-scene 'type (-> Nat Nat String String (List Bytevector) Scene))
(define make-scene make-scene%)

(doc 'note "Re-export accessors")
(define scene-width scene%-width)
(define scene-height scene%-height)
(define scene-name scene%-name)
(define scene-timestamp scene%-timestamp)
(define scene-layer-hashes scene%-layer-hashes)

(doc scene->block 'type (-> Scene Block))
(doc scene->block 'description "Convert a scene to a block")
(define (scene->block scene)
  (let* ([w (scene-width scene)]
         [h (scene-height scene)]
         [name (scene-name scene)]
         [timestamp (scene-timestamp scene)]
         [layer-hashes (scene-layer-hashes scene)]
         [layer-count (length layer-hashes)]
         [name-bytes (string->utf8 name)]
         [timestamp-bytes (string->utf8 timestamp)]
         [payload-parts
          (list
           (u32->bytes-le w)
           (u32->bytes-le h)
           (u32->bytes-le (bytevector-length name-bytes))
           name-bytes
           (u32->bytes-le (bytevector-length timestamp-bytes))
           timestamp-bytes
           (u32->bytes-le layer-count))]
         [payload (bytevector-concat payload-parts)]
         [refs (list->vector layer-hashes)])
        (make-block 'scene payload refs)))

(doc block->scene 'type (-> Block (Union Scene Bool)))
(doc block->scene 'description "Reconstruct a scene from a block")
(define (block->scene blk)
  (if (not (eq? (block-tag blk) 'scene))
      #f
      (let* ([payload (block-payload blk)]
             [pos 0]
             ;; Width
             [w (bytes-le->u32 payload pos)]
             [_ (set! pos (+ pos 4))]
             ;; Height
             [h (bytes-le->u32 payload pos)]
             [_ (set! pos (+ pos 4))]
             ;; Name
             [name-len (bytes-le->u32 payload pos)]
             [_ (set! pos (+ pos 4))]
             [name-bytes (make-bytevector name-len)]
             [_ (bytevector-copy! payload pos name-bytes 0 name-len)]
             [_ (set! pos (+ pos name-len))]
             [name (utf8->string name-bytes)]
             ;; Timestamp
             [timestamp-len (bytes-le->u32 payload pos)]
             [_ (set! pos (+ pos 4))]
             [timestamp-bytes (make-bytevector timestamp-len)]
             [_ (bytevector-copy! payload pos timestamp-bytes 0 timestamp-len)]
             [_ (set! pos (+ pos timestamp-len))]
             [timestamp (utf8->string timestamp-bytes)]
             ;; Layer count
             [layer-count (bytes-le->u32 payload pos)]
             [_ (set! pos (+ pos 4))]
             ;; Refs
             [refs (block-refs blk)]
             [layer-hashes (vector->list refs)])
            (make-scene w h name timestamp layer-hashes))))

(doc store-scene! 'type (-> FS Scene Bytevector))
(doc store-scene! 'description "Store a scene in the CAS and return its hash")
(define (store-scene! fs scene)
  (fs-store! fs (scene->block scene)))

(doc fetch-scene 'type (-> FS Bytevector (Union Scene Bool)))
(doc fetch-scene 'description "Fetch a scene by its hash")
(define (fetch-scene fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->scene blk)
           #f)))

(doc 'section 'rendering-pipeline)

(doc 'note "Render Mode: Controls output format")
(doc 'note "'ascii     — ASCII-only (0x20-0x7E), no color")
(doc 'note "'ansi      — ANSI color codes, basic characters")
(doc 'note "'unicode   — Full Unicode, ANSI color")
(doc 'note "'block     — Store to CAS instead of display")
(define render-mode-ascii 'ascii)
(define render-mode-ansi 'ansi)
(define render-mode-unicode 'unicode)
(define render-mode-block 'block)

(doc 'note "Render Target: Where output goes")
(doc 'note "(terminal)          — Print to stdout")
(doc 'note "(string)            — Return as string")
(doc 'note "(file path)         — Write to file")
(doc 'note "(block fs)          — Store in CAS, return hash")
(define-record-type render-target%
  (fields type value))

(define (make-render-target type . args)
  (make-render-target% type (if (null? args) #f (car args))))

(define render-target-type render-target%-type)
(define render-target-value render-target%-value)

(doc render-canvas 'type (-> Canvas RenderMode RenderTarget Any))
(doc render-canvas 'description "Render a canvas to the specified target")
(doc render-canvas 'returns "- (void) for terminal output")
(doc render-canvas 'returns "- String for string target")
(doc render-canvas 'returns "- Path for file target")
(doc render-canvas 'returns "- Bytevector (hash) for block target")
(define (render-canvas canvas mode target)
  (case (render-target-type target)
        [(terminal)
         (display (canvas->string canvas))
         (newline)]
        
        [(string)
         (canvas->string canvas)]
        
        [(file)
         (let ([path (render-target-value target)])
              (call-with-output-file path
                                     (lambda (port)
                                             (display (canvas->string canvas) port)
                                             (newline port)))
              path)]
        
        [(block)
         (let ([fs (render-target-value target)])
              (store-canvas! fs canvas))]
        
        [else
         (error 'render-canvas "Unknown render target" target)]))

(doc render-scene 'type (-> Scene FS RenderMode RenderTarget Any))
(doc render-scene 'description "Render a complete scene by fetching and compositing all layers")
(doc render-scene 'note "This is a simplified version that assumes layers are stored as canvases")
(doc render-scene 'note "In the future, this should handle layer metadata (visibility, offset, etc.)")
(define (render-scene scene fs mode target)
  (let* ([w (scene-width scene)]
         [h (scene-height scene)]
         [layer-hashes (scene-layer-hashes scene)]
         ;; Fetch all layer canvases
         [layers (map (lambda (hash) (fetch-canvas fs hash))
                      layer-hashes)]
         ;; Create base canvas
         [base (make-canvas w h)])
        ;; Composite layers (simplified: just overlay in order)
        (let composite-loop ([layers layers] [result base])
             (if (null? layers)
                 (render-canvas result mode target)
                 (let ([layer (car layers)])
                      (if layer
                          (composite-loop (cdr layers)
                                          (composite result layer (point 0 0)))
                          (composite-loop (cdr layers) result)))))))

(doc 'section 'graphics-primitives-api)

(doc 'note "These functions provide a convenient API for common graphics operations")
(doc 'note "They integrate with the existing layout.ss primitives but add block storage")

(doc make-graphics-canvas 'type (-> Nat Nat Canvas))
(doc make-graphics-canvas 'description "Create a new blank canvas for drawing")
(define make-graphics-canvas make-canvas)

(doc graphics-draw-box 'type (-> Canvas Nat Nat Nat Nat Symbol Canvas))
(doc graphics-draw-box 'description "Draw a box on the canvas")
(doc graphics-draw-box 'param "style: 'ascii, 'light, 'heavy, 'double")
(define (graphics-draw-box canvas x y width height style)
  (draw-box canvas
            (make-rect (point x y) width height)
            style))

(doc graphics-draw-text 'type (-> Canvas Nat Nat String Canvas))
(doc graphics-draw-text 'description "Draw text on the canvas")
(define (graphics-draw-text canvas x y text)
  (draw-string canvas (point x y) text))

(doc graphics-fill 'type (-> Canvas Nat Nat Nat Nat Char Canvas))
(doc graphics-fill 'description "Fill a rectangular region with a character")
(define (graphics-fill canvas x y width height ch)
  (fill-rect canvas (make-rect (point x y) width height) ch))

(doc 'section 'frame-buffer-system)

(doc 'note "A frame buffer manages double-buffering for animation")
(doc 'note "It maintains front and back buffers, supporting smooth animation")

(define-record-type frame-buffer%
  (fields width height front back))

(doc make-frame-buffer 'type (-> Nat Nat FrameBuffer))
(doc make-frame-buffer 'description "Create a new frame buffer with the given dimensions")
(define (make-frame-buffer width height)
  (make-frame-buffer%
   width
   height
   (make-canvas width height)
   (make-canvas width height)))

(doc frame-buffer-swap 'type (-> FrameBuffer FrameBuffer))
(doc frame-buffer-swap 'description "Swap front and back buffers (returns new frame buffer with swapped buffers)")
(define (frame-buffer-swap fb)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-back fb)   ; back becomes front
   (frame-buffer%-front fb))) ; front becomes back

(doc frame-buffer-clear-back 'type (-> FrameBuffer FrameBuffer))
(doc frame-buffer-clear-back 'description "Clear the back buffer (returns new frame buffer with cleared back)")
(define (frame-buffer-clear-back fb)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-front fb)
   (make-canvas (frame-buffer%-width fb) (frame-buffer%-height fb))))

(doc frame-buffer-get-front 'type (-> FrameBuffer Canvas))
(doc frame-buffer-get-front 'description "Get the front buffer for rendering")
(define frame-buffer-get-front frame-buffer%-front)

(doc frame-buffer-get-back 'type (-> FrameBuffer Canvas))
(doc frame-buffer-get-back 'description "Get the back buffer for drawing")
(define frame-buffer-get-back frame-buffer%-back)

(doc frame-buffer-set-back 'type (-> FrameBuffer Canvas FrameBuffer))
(doc frame-buffer-set-back 'description "Set the back buffer to a new canvas")
(define (frame-buffer-set-back fb canvas)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-front fb)
   canvas))

(doc 'section 'export-summary)

(doc 'note "This file provides:")
(doc 'note "Block Integration:")
(doc 'note "- canvas->block, block->canvas")
(doc 'note "- store-canvas!, fetch-canvas")
(doc 'note "- scene->block, block->scene")
(doc 'note "- store-scene!, fetch-scene")
(doc 'note "Rendering Pipeline:")
(doc 'note "- render-canvas, render-scene")
(doc 'note "- make-render-target")
(doc 'note "- render-mode-* constants")
(doc 'note "Graphics API:")
(doc 'note "- make-graphics-canvas")
(doc 'note "- graphics-draw-box, graphics-draw-text, graphics-fill")
(doc 'note "Frame Buffer:")
(doc 'note "- make-frame-buffer")
(doc 'note "- frame-buffer-swap!, frame-buffer-clear-back!")
(doc 'note "- frame-buffer-get-front, frame-buffer-get-back")
(doc 'note "- frame-buffer-set-back")
(doc 'note "Integration with existing systems:")
(doc 'note "- Uses boundary/ui/layout.ss for canvas primitives")
(doc 'note "- Uses boundary/ui/color.ss for color representation")
(doc 'note "- Uses core/block.ss + core/cas.ss for storage")
(doc 'note "- Uses boundary/io/fs.ss for persistence")
