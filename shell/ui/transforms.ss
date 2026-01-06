;;; thimble/transforms.ss — 2D Transformations for Graphics
;;;
;;; Provides affine transformations for canvas-based graphics:
;;; translate, rotate, scale, and general matrix transforms.
;;;
;;; This is Shell code: pure functions for geometric transformations.
;;;
;;; Features:
;;;   - Translate (move) a canvas by dx, dy
;;;   - Rotate a canvas by angle (in radians)
;;;   - Scale a canvas by sx, sy factors
;;;   - General 2x3 affine matrix transforms
;;;   - Clipping (render only within bounds)
;;;   - Masking (use one canvas to mask another)
;;;   - Transform composition (combine multiple transforms)
;;;
;;; Dependencies:
;;;   - shell/layout.ss — Canvas primitives
;;;
;;; Coordinate System:
;;;   - Origin (0,0) is top-left
;;;   - +X goes right, +Y goes down
;;;   - Rotations are clockwise

(library (shell transforms)
         (export
          ;; Basic transforms
          canvas-translate
          canvas-rotate
          canvas-scale
          canvas-flip-horizontal
          canvas-flip-vertical
          
          ;; Matrix transforms
          make-transform-matrix
          matrix-translate
          matrix-rotate
          matrix-scale
          matrix-multiply
          matrix-invert
          canvas-transform
          
          ;; Clipping and masking
          canvas-clip
          canvas-mask
          
          ;; Groups (transformed layer composition)
          make-transform-group
          group-add-transform
          group-apply
          transform-group-canvas
          transform-group-offset)
         
         (import (chezscheme))
         
         ;;; ============================================================
         ;;; Transform Matrix (2x3 Affine)
         ;;; ============================================================
         
         ;;; 2D Affine transformation matrix:
         ;;;   | a  b  tx |
         ;;;   | c  d  ty |
         ;;;
         ;;; Transforms point (x, y) to:
         ;;;   x' = a*x + b*y + tx
         ;;;   y' = c*x + d*y + ty
         
         (define-record-type transform-matrix%
           (fields a b c d tx ty))
         
         ;;; make-transform-matrix : Real × Real × Real × Real × Real × Real → Matrix
         (define make-transform-matrix make-transform-matrix%)
         
         ;;; Identity matrix
         (define identity-matrix
           (make-transform-matrix 1.0 0.0 0.0 1.0 0.0 0.0))
         
         ;;; matrix-translate : Real × Real → Matrix
         ;;; Create translation matrix.
         (define (matrix-translate dx dy)
           (make-transform-matrix 1.0 0.0
                                  0.0 1.0
                                  (exact->inexact dx)
                                  (exact->inexact dy)))
         
         ;;; matrix-rotate : Real → Matrix
         ;;; Create rotation matrix (angle in radians, clockwise).
         (define (matrix-rotate angle)
           (let ([cos-a (cos angle)]
                 [sin-a (sin angle)])
                (make-transform-matrix cos-a sin-a
                                       (- sin-a) cos-a
                                       0.0 0.0)))
         
         ;;; matrix-scale : Real × Real → Matrix
         ;;; Create scale matrix.
         (define (matrix-scale sx sy)
           (make-transform-matrix (exact->inexact sx) 0.0
                                  0.0 (exact->inexact sy)
                                  0.0 0.0))
         
         ;;; matrix-multiply : Matrix × Matrix → Matrix
         ;;; Multiply two transformation matrices (compose transforms).
         (define (matrix-multiply m1 m2)
           (let ([a1 (transform-matrix%-a m1)]
                 [b1 (transform-matrix%-b m1)]
                 [c1 (transform-matrix%-c m1)]
                 [d1 (transform-matrix%-d m1)]
                 [tx1 (transform-matrix%-tx m1)]
                 [ty1 (transform-matrix%-ty m1)]
                 [a2 (transform-matrix%-a m2)]
                 [b2 (transform-matrix%-b m2)]
                 [c2 (transform-matrix%-c m2)]
                 [d2 (transform-matrix%-d m2)]
                 [tx2 (transform-matrix%-tx m2)]
                 [ty2 (transform-matrix%-ty m2)])
                (make-transform-matrix
                 (+ (* a1 a2) (* b1 c2))
                 (+ (* a1 b2) (* b1 d2))
                 (+ (* c1 a2) (* d1 c2))
                 (+ (* c1 b2) (* d1 d2))
                 (+ (* a1 tx2) (* b1 ty2) tx1)
                 (+ (* c1 tx2) (* d1 ty2) ty1))))
         
         ;;; matrix-invert : Matrix → Matrix
         ;;; Invert an affine transformation matrix.
         ;;; Returns identity matrix if determinant is zero (singular matrix).
         (define (matrix-invert matrix)
           (let ([a (transform-matrix%-a matrix)]
                 [b (transform-matrix%-b matrix)]
                 [c (transform-matrix%-c matrix)]
                 [d (transform-matrix%-d matrix)]
                 [tx (transform-matrix%-tx matrix)]
                 [ty (transform-matrix%-ty matrix)])
                (let ([det (- (* a d) (* b c))])
                     (if (< (abs det) 1e-10)
                         ;; Singular matrix, return identity
                         identity-matrix
                         (let ([inv-det (/ 1.0 det)])
                              (make-transform-matrix
                               (* inv-det d)
                               (* inv-det (- b))
                               (* inv-det (- c))
                               (* inv-det a)
                               (* inv-det (- (* d tx) (* b ty)))
                               (* inv-det (- (* c tx) (* a ty)))))))))
         
         ;;; transform-point : Matrix × Nat × Nat → (Nat . Nat)
         ;;; Apply transform matrix to a point.
         (define (transform-point matrix x y)
           (let ([a (transform-matrix%-a matrix)]
                 [b (transform-matrix%-b matrix)]
                 [c (transform-matrix%-c matrix)]
                 [d (transform-matrix%-d matrix)]
                 [tx (transform-matrix%-tx matrix)]
                 [ty (transform-matrix%-ty matrix)]
                 [fx (exact->inexact x)]
                 [fy (exact->inexact y)])
                (let ([x' (+ (* a fx) (* b fy) tx)]
                      [y' (+ (* c fx) (* d fy) ty)])
                     (cons (inexact->exact (round x'))
                           (inexact->exact (round y'))))))
         
         ;;; ============================================================
         ;;; Canvas Import (assume layout.ss loaded)
         ;;; ============================================================
         
         ;;; Note: These are assumed to be available from shell/layout.ss:
         ;;;   - make-canvas, canvas-width, canvas-height
         ;;;   - canvas-ref, canvas-set
         ;;;   - point, point-x, point-y
         ;;;   - make-rect, rect-origin, rect-width, rect-height
         
         ;;; For the library version, we need to import from layout
         ;;; But layout.ss isn't a library yet, so these will be undefined
         ;;; until layout is converted to a library or this is loaded via load.
         
         ;;; Fallback definitions for when running standalone:
         (define (make-canvas% w h cells)
           (vector 'canvas w h cells))
         
         (define (canvas-width canvas)
           (vector-ref canvas 1))
         
         (define (canvas-height canvas)
           (vector-ref canvas 2))
         
         (define (canvas-cells canvas)
           (vector-ref canvas 3))
         
         (define (canvas-ref canvas x y)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)]
                 [cells (canvas-cells canvas)])
                (if (or (< x 0) (>= x w) (< y 0) (>= y h))
                    #\space
                    (vector-ref cells (+ (* y w) x)))))
         
         (define (canvas-set canvas x y ch)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)]
                 [cells (canvas-cells canvas)])
                (if (or (< x 0) (>= x w) (< y 0) (>= y h))
                    canvas
                    (let ([new-cells (vector-copy cells)])
                         (vector-set! new-cells (+ (* y w) x) ch)
                         (make-canvas% w h new-cells)))))
         
         (define (make-canvas w h)
           (make-canvas% w h (make-vector (* w h) #\space)))
         
         (define (point x y) (cons x y))
         (define (point-x pt) (car pt))
         (define (point-y pt) (cdr pt))
         
         ;;; ============================================================
         ;;; Basic Transform Operations
         ;;; ============================================================
         
         ;;; canvas-translate : Canvas × Int × Int → Canvas
         ;;; Translate canvas by dx, dy.
         (define (canvas-translate canvas dx dy)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (canvas-transform canvas (matrix-translate dx dy) w h)))
         
         ;;; canvas-rotate : Canvas × Real × Point → Canvas
         ;;; Rotate canvas around center point by angle (radians).
         (define (canvas-rotate canvas angle center)
           (let* ([cx (point-x center)]
                  [cy (point-y center)]
                  [w (canvas-width canvas)]
                  [h (canvas-height canvas)]
                  ;; Rotate around center: translate to origin, rotate, translate back
                  [m1 (matrix-translate (- cx) (- cy))]
                  [m2 (matrix-rotate angle)]
                  [m3 (matrix-translate cx cy)]
                  [combined (matrix-multiply (matrix-multiply m3 m2) m1)])
                 (canvas-transform canvas combined w h)))
         
         ;;; canvas-scale : Canvas × Real × Real × Point → Canvas
         ;;; Scale canvas by sx, sy around center point.
         (define (canvas-scale canvas sx sy center)
           (let* ([cx (point-x center)]
                  [cy (point-y center)]
                  [w (inexact->exact (round (* (canvas-width canvas) sx)))]
                  [h (inexact->exact (round (* (canvas-height canvas) sy)))]
                  ;; Scale around center
                  [m1 (matrix-translate (- cx) (- cy))]
                  [m2 (matrix-scale sx sy)]
                  [m3 (matrix-translate cx cy)]
                  [combined (matrix-multiply (matrix-multiply m3 m2) m1)])
                 (canvas-transform canvas combined w h)))
         
         ;;; canvas-flip-horizontal : Canvas → Canvas
         ;;; Flip canvas horizontally (mirror across vertical axis).
         (define (canvas-flip-horizontal canvas)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (canvas-transform canvas
                                  (matrix-multiply
                                   (matrix-translate w 0)
                                   (matrix-scale -1.0 1.0))
                                  w h)))
         
         ;;; canvas-flip-vertical : Canvas → Canvas
         ;;; Flip canvas vertically (mirror across horizontal axis).
         (define (canvas-flip-vertical canvas)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (canvas-transform canvas
                                  (matrix-multiply
                                   (matrix-translate 0 h)
                                   (matrix-scale 1.0 -1.0))
                                  w h)))
         
         ;;; ============================================================
         ;;; General Transform Application
         ;;; ============================================================
         
         ;;; canvas-transform : Canvas × Matrix × Nat × Nat → Canvas
         ;;; Apply arbitrary affine transform to canvas.
         ;;; Creates new canvas of size width×height.
         ;;; Uses inverse matrix: for each dest pixel, find which source pixel to sample.
         ;;; This ensures transforms move content in the expected direction.
         (define (canvas-transform src matrix width height)
           (let ([dest (make-canvas width height)]
                 [inv-matrix (matrix-invert matrix)])
                (let loop-y ([y 0])
                     (if (>= y height)
                         dest
                         (let loop-x ([x 0])
                              (if (>= x width)
                                  (loop-y (+ y 1))
                                  (let* ([src-pt (transform-point inv-matrix x y)]
                                         [sx (car src-pt)]
                                         [sy (cdr src-pt)]
                                         [ch (canvas-ref src sx sy)])
                                        (canvas-set! dest x y ch)
                                        (loop-x (+ x 1)))))))))
         
         ;;; ============================================================
         ;;; Clipping
         ;;; ============================================================
         
         ;;; canvas-clip : Canvas × Rect → Canvas
         ;;; Extract rectangular region from canvas (crop).
         (define (canvas-clip canvas rect)
           (let* ([ox (point-x (rect-origin rect))]
                  [oy (point-y (rect-origin rect))]
                  [w (rect-width rect)]
                  [h (rect-height rect)]
                  [result (make-canvas w h)])
                 (let loop-y ([y 0])
                      (if (>= y h)
                          result
                          (let loop-x ([x 0])
                               (if (>= x w)
                                   (loop-y (+ y 1))
                                   (let ([ch (canvas-ref canvas (+ ox x) (+ oy y))])
                                        (canvas-set! result x y ch)
                                        (loop-x (+ x 1)))))))))
         
         ;;; Helper: Since rect functions might not be defined
         (define (rect-origin rect) (vector-ref rect 1))
         (define (rect-width rect) (vector-ref rect 2))
         (define (rect-height rect) (vector-ref rect 3))
         
         ;;; ============================================================
         ;;; Masking
         ;;; ============================================================
         
         ;;; canvas-mask : Canvas × Canvas × Char → Canvas
         ;;; Apply mask to canvas. Only show canvas where mask is not mask-char.
         ;;; Both canvases must have same dimensions.
         (define (canvas-mask canvas mask mask-char)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (let loop-y ([y 0])
                     (if (>= y h)
                         canvas
                         (let loop-x ([x 0])
                              (if (>= x w)
                                  (loop-y (+ y 1))
                                  (let ([mask-ch (canvas-ref mask x y)])
                                       (when (char=? mask-ch mask-char)
                                             ;; Masked out - replace with space
                                             (canvas-set! canvas x y #\space))
                                       (loop-x (+ x 1)))))))))
         
         ;;; ============================================================
         ;;; Transform Groups
         ;;; ============================================================
         
         ;;; A transform group applies a sequence of transforms to a canvas.
         ;;; Useful for complex composed transforms.
         
         (define-record-type transform-group%
           (fields canvas transforms offset))
         
         ;;; make-transform-group : Canvas → TransformGroup
         (define (make-transform-group canvas)
           (make-transform-group% canvas '() (point 0 0)))
         
         ;;; group-add-transform : TransformGroup × Matrix → TransformGroup
         (define (group-add-transform group matrix)
           (make-transform-group%
            (transform-group%-canvas group)
            (cons matrix (transform-group%-transforms group))
            (transform-group%-offset group)))
         
         ;;; group-apply : TransformGroup × Nat × Nat → Canvas
         ;;; Apply all transforms in the group and return final canvas.
         (define (group-apply group width height)
           (let ([canvas (transform-group%-canvas group)]
                 [transforms (reverse (transform-group%-transforms group))])
                ;; Compose all transform matrices
                (let ([combined (fold-left matrix-multiply
                                           identity-matrix
                                           transforms)])
                     (canvas-transform canvas combined width height))))
         
         (define transform-group-canvas transform-group%-canvas)
         (define transform-group-offset transform-group%-offset)
         
         ) ; end library
