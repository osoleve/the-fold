;;; boundary/graphics.ss — Graphics Engine Foundation
;;;
;;; The unified graphics engine for The Fold, integrating all visual primitives
;;; with the block substrate. Provides the rendering pipeline for DUCKIE and
;;; other visual entities.
;;;
;;; This is Shell code: handles rendering and IO, integrates with CAS.
;;;
;;; Architecture:
;;;   - Canvas Layer: 2D character grids (from boundary/ui/layout.ss)
;;;   - Color Layer: RGB/palette colors + ANSI output (from boundary/ui/color.ss)
;;;   - Primitive Layer: Shapes, lines, circles (from boundary/ui/graphics-primitives.ss)
;;;   - Composition Layer: Transparency, z-ordering (from boundary/ui/layers.ss)
;;;   - Animation Layer: Easing functions (from boundary/ui/animation.ss)
;;;   - Block Layer: Content-addressed storage of graphics (THIS FILE)
;;;
;;; Block Integration:
;;;   - Canvases can be stored as blocks
;;;   - Layers can be stored as blocks
;;;   - Complete scenes (layer stacks) can be stored as blocks
;;;   - Rendering pipelines reference graphics by hash
;;;
;;; Rendering Pipeline:
;;;   - Render targets: terminal, string, file, block
;;;   - Render modes: ASCII-only, ANSI color, Unicode
;;;   - Frame buffering for animation

;;; ====
;;; Dependencies
;;; ====

;;; This file expects the following to already be loaded:
;;;   - core/block.ss
;;;   - core/sha256.ss
;;;   - core/cas.ss
;;;   - boundary/io/fs.ss
;;;   - boundary/ui/layout.ss
;;;   - boundary/ui/color.ss
;;;
;;; Load them via boundary/repl/repl.ss or manually before loading this file.
;;;
;;; Note: graphics-primitives.ss, layers.ss, and animation.ss are libraries.
;;; They will be loaded when needed via import.

;;; ====
;;; Canvas Block Storage
;;; ====

;;; canvas->block : Canvas → Block
;;; Convert a canvas to a content-addressed block.
;;;
;;; Block structure:
;;;   tag: 'canvas
;;;   payload: Serialized canvas data
;;;     [width : 4 bytes (u32 little-endian)]
;;;     [height : 4 bytes (u32 little-endian)]
;;;     [cells : width×height bytes (UTF-8 characters)]
;;;   refs: []
;;;
;;; Note: This is a simplified ASCII-only format. For color support,
;;; use colored-canvas->block which stores Cell data.
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

;;; block->canvas : Block → Canvas | #f
;;; Reconstruct a canvas from a block.
;;; Returns #f if block is not a valid canvas block.
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

;;; store-canvas! : FS × Canvas → Bytevector
;;; Store a canvas in the CAS and return its hash.
(define (store-canvas! fs canvas)
  (fs-store! fs (canvas->block canvas)))

;;; fetch-canvas : FS × Bytevector → Canvas | #f
;;; Fetch a canvas by its hash from the CAS.
(define (fetch-canvas fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->canvas blk)
           #f)))

;;; ====
;;; Colored Canvas Block Storage
;;; ====

;;; Color type codes for serialization
(define COLOR-TYPE-DEFAULT 0)
(define COLOR-TYPE-RGB 1)
(define COLOR-TYPE-PALETTE 2)

;;; clamp-byte : Integer → Byte
;;; Clamp an integer to valid byte range [0, 255].
(define (clamp-byte n)
  (cond
   [(not (integer? n)) 0]
   [(< n 0) 0]
   [(> n 255) 255]
   [else n]))

;;; serialize-color : Color → Bytevector
;;; Serialize a color to bytes.
;;; Format: [type : 1 byte][data : 0-3 bytes]
;;; Defensive: Clamps RGB/palette values to valid byte range.
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

;;; deserialize-color : Bytevector × Nat → (Color . Nat) | #f
;;; Deserialize a color from bytes at given offset.
;;; Returns (color . new-offset) or #f if bytevector is too short.
;;; Defensive: Validates bytevector bounds before reading.
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

;;; colored-canvas->block : Canvas × (Vector Cell) → Block
;;; Store a canvas with full color information.
;;;
;;; Block structure:
;;;   tag: 'colored-canvas
;;;   payload: Serialized colored canvas data
;;;     [width : 4 bytes]
;;;     [height : 4 bytes]
;;;     For each cell (row-major order):
;;;       [char : 4 bytes (UTF-32 character code)]
;;;       [fg-type : 1 byte (0=default, 1=rgb, 2=palette)]
;;;       [fg-data : variable (0 bytes for default, 3 for rgb, 1 for palette)]
;;;       [bg-type : 1 byte]
;;;       [bg-data : variable]
;;;   refs: []
;;;
;;; Note: This is more complex but preserves full color data.
;;; For simple ASCII rendering, use canvas->block instead.
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

;;; valid-char-code? : Integer → Boolean
;;; Check if an integer is a valid Unicode code point.
;;; Excludes surrogates (0xD800-0xDFFF) and values > 0x10FFFF.
(define (valid-char-code? n)
  (and (integer? n)
       (>= n 0)
       (<= n #x10FFFF)
       (not (and (>= n #xD800) (<= n #xDFFF)))))

;;; safe-integer->char : Integer → Char
;;; Convert integer to char, returning space for invalid codes.
(define (safe-integer->char n)
  (if (valid-char-code? n)
      (integer->char n)
      #\space))


;;; block->colored-canvas : Block → (Canvas . Vector Cell) | #f
;;; Reconstruct a colored canvas from a block.
;;; Returns (canvas . color-cells) or #f if block is invalid.
;;; Defensive: Validates payload bounds and character codes.
(define (block->colored-canvas blk)
  (if (not (eq? (block-tag blk) 'colored-canvas))
      #f
      (parse-colored-canvas-payload (block-payload blk))))

;;; parse-colored-canvas-payload : Bytevector → (Canvas . Vector Cell) | #f
;;; Internal: Parse colored canvas from payload bytes.
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

;;; parse-colored-canvas-cells : Bytevector × Nat × Nat × Nat → (Canvas . Vector Cell) | #f
;;; Internal: Parse cell data from payload.
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

;;; parse-one-cell : Bytevector × Nat → (Char Cell . Nat) | #f
;;; Internal: Parse one cell from payload at offset.
;;; Returns (char cell . new-offset) or #f on failure.
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

;;; store-colored-canvas! : FS × Canvas × (Vector Cell) → Bytevector
;;; Store a colored canvas in the CAS and return its hash.
(define (store-colored-canvas! fs canvas color-cells)
  (fs-store! fs (colored-canvas->block canvas color-cells)))

;;; fetch-colored-canvas : FS × Bytevector → (Canvas . Vector Cell) | #f
;;; Fetch a colored canvas by its hash from the CAS.
(define (fetch-colored-canvas fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->colored-canvas blk)
           #f)))

;;; ====
;;; Scene Block Storage
;;; ====

;;; A Scene is a complete visual composition ready to render.
;;; It contains:
;;;   - A layer stack (ordered layers with z-depth)
;;;   - Metadata (name, dimensions, timestamp)
;;;
;;; Block structure:
;;;   tag: 'scene
;;;   payload: Serialized metadata
;;;     [width : 4 bytes]
;;;     [height : 4 bytes]
;;;     [name : length-prefixed UTF-8 string]
;;;     [timestamp : length-prefixed UTF-8 string]
;;;     [layer-count : 4 bytes]
;;;   refs: [layer-hash₀, layer-hash₁, ..., layer-hashₙ]
;;;
;;; Each layer is stored as a separate canvas block.

(define-record-type scene%
  (fields width height name timestamp layer-hashes))

;;; make-scene : Nat × Nat × String × String × (List Bytevector) → Scene
(define make-scene make-scene%)

;;; Re-export accessors
(define scene-width scene%-width)
(define scene-height scene%-height)
(define scene-name scene%-name)
(define scene-timestamp scene%-timestamp)
(define scene-layer-hashes scene%-layer-hashes)

;;; scene->block : Scene → Block
;;; Convert a scene to a block.
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

;;; block->scene : Block → Scene | #f
;;; Reconstruct a scene from a block.
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

;;; store-scene! : FS × Scene → Bytevector
;;; Store a scene in the CAS and return its hash.
(define (store-scene! fs scene)
  (fs-store! fs (scene->block scene)))

;;; fetch-scene : FS × Bytevector → Scene | #f
;;; Fetch a scene by its hash.
(define (fetch-scene fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (block->scene blk)
           #f)))

;;; ====
;;; Rendering Pipeline
;;; ====

;;; Render Mode: Controls output format
;;;   'ascii     — ASCII-only (0x20-0x7E), no color
;;;   'ansi      — ANSI color codes, basic characters
;;;   'unicode   — Full Unicode, ANSI color
;;;   'block     — Store to CAS instead of display
(define render-mode-ascii 'ascii)
(define render-mode-ansi 'ansi)
(define render-mode-unicode 'unicode)
(define render-mode-block 'block)

;;; Render Target: Where output goes
;;;   (terminal)          — Print to stdout
;;;   (string)            — Return as string
;;;   (file path)         — Write to file
;;;   (block fs)          — Store in CAS, return hash
(define-record-type render-target%
  (fields type value))

(define (make-render-target type . args)
  (make-render-target% type (if (null? args) #f (car args))))

(define render-target-type render-target%-type)
(define render-target-value render-target%-value)

;;; render-canvas : Canvas × RenderMode × RenderTarget → Any
;;; Render a canvas to the specified target.
;;;
;;; Returns:
;;;   - (void) for terminal output
;;;   - String for string target
;;;   - Path for file target
;;;   - Bytevector (hash) for block target
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

;;; render-scene : Scene × FS × RenderMode × RenderTarget → Any
;;; Render a complete scene by fetching and compositing all layers.
;;;
;;; This is a simplified version that assumes layers are stored as canvases.
;;; In the future, this should handle layer metadata (visibility, offset, etc.)
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

;;; ====
;;; Graphics Primitives API
;;; ====

;;; These functions provide a convenient API for common graphics operations.
;;; They integrate with the existing layout.ss primitives but add block storage.

;;; make-graphics-canvas : Nat × Nat → Canvas
;;; Create a new blank canvas for drawing.
(define make-graphics-canvas make-canvas)

;;; graphics-draw-box : Canvas × Nat × Nat × Nat × Nat × Symbol → Canvas
;;; Draw a box on the canvas.
;;; Style: 'ascii, 'light, 'heavy, 'double
(define (graphics-draw-box canvas x y width height style)
  (draw-box canvas
            (make-rect (point x y) width height)
            style))

;;; graphics-draw-text : Canvas × Nat × Nat × String → Canvas
;;; Draw text on the canvas.
(define (graphics-draw-text canvas x y text)
  (draw-string canvas (point x y) text))

;;; graphics-fill : Canvas × Nat × Nat × Nat × Nat × Char → Canvas
;;; Fill a rectangular region with a character.
(define (graphics-fill canvas x y width height ch)
  (fill-rect canvas (make-rect (point x y) width height) ch))

;;; ====
;;; Frame Buffer System
;;; ====

;;; A frame buffer manages double-buffering for animation.
;;; It maintains front and back buffers, supporting smooth animation.

(define-record-type frame-buffer%
  (fields width height front back))

;;; make-frame-buffer : Nat × Nat → FrameBuffer
;;; Create a new frame buffer with the given dimensions.
(define (make-frame-buffer width height)
  (make-frame-buffer%
   width
   height
   (make-canvas width height)
   (make-canvas width height)))

;;; frame-buffer-swap! : FrameBuffer → FrameBuffer
;;; Swap front and back buffers (returns new frame buffer with swapped buffers).
(define (frame-buffer-swap fb)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-back fb)   ; back becomes front
   (frame-buffer%-front fb))) ; front becomes back

;;; frame-buffer-clear-back! : FrameBuffer → FrameBuffer
;;; Clear the back buffer (returns new frame buffer with cleared back).
(define (frame-buffer-clear-back fb)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-front fb)
   (make-canvas (frame-buffer%-width fb) (frame-buffer%-height fb))))

;;; frame-buffer-get-front : FrameBuffer → Canvas
;;; Get the front buffer for rendering.
(define frame-buffer-get-front frame-buffer%-front)

;;; frame-buffer-get-back : FrameBuffer → Canvas
;;; Get the back buffer for drawing.
(define frame-buffer-get-back frame-buffer%-back)

;;; frame-buffer-set-back : FrameBuffer × Canvas → FrameBuffer
;;; Set the back buffer to a new canvas.
(define (frame-buffer-set-back fb canvas)
  (make-frame-buffer%
   (frame-buffer%-width fb)
   (frame-buffer%-height fb)
   (frame-buffer%-front fb)
   canvas))

;;; ====
;;; Export Summary
;;; ====

;;; This file provides:
;;;
;;; Block Integration:
;;;   - canvas->block, block->canvas
;;;   - store-canvas!, fetch-canvas
;;;   - scene->block, block->scene
;;;   - store-scene!, fetch-scene
;;;
;;; Rendering Pipeline:
;;;   - render-canvas, render-scene
;;;   - make-render-target
;;;   - render-mode-* constants
;;;
;;; Graphics API:
;;;   - make-graphics-canvas
;;;   - graphics-draw-box, graphics-draw-text, graphics-fill
;;;
;;; Frame Buffer:
;;;   - make-frame-buffer
;;;   - frame-buffer-swap!, frame-buffer-clear-back!
;;;   - frame-buffer-get-front, frame-buffer-get-back
;;;   - frame-buffer-set-back
;;;
;;; Integration with existing systems:
;;;   - Uses boundary/ui/layout.ss for canvas primitives
;;;   - Uses boundary/ui/color.ss for color representation
;;;   - Uses core/block.ss + core/cas.ss for storage
;;;   - Uses boundary/io/fs.ss for persistence
