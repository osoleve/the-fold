;;; shell/turtle.ss — Turtle Graphics Core
;;;
;;; A vector-based turtle graphics system for a 640x480 viewport.
;;; All operations are pure functional (return new state).
;;;
;;; Coordinate System:
;;;   - Origin (0,0) is top-left
;;;   - Center is (320, 240)
;;;   - Heading 0 = North (up), clockwise rotation
;;;   - Angles in degrees
;;;
;;; This is Shell code: pure functions for turtle state manipulation.
;;;
;;; Dependencies:
;;;   - shell/turtle-color.ss
;;;   - shell/turtle-path.ss

;;; ============================================================
;;; Constants
;;; ============================================================

(define *turtle-viewport-width* 640)
(define *turtle-viewport-height* 480)
(define *turtle-center-x* 320.0)
(define *turtle-center-y* 240.0)

;;; Degrees to radians conversion
(define pi 3.141592653589793)
(define (deg->rad deg) (* deg (/ pi 180.0)))
(define (rad->deg rad) (* rad (/ 180.0 pi)))

;;; ============================================================
;;; Turtle State Record
;;; ============================================================

;;; Turtle State:
;;;   x, y       : Real        - Current position
;;;   heading    : Real        - Angle in degrees (0=North, clockwise)
;;;   pen-down?  : Bool        - Is the pen drawing?
;;;   pen-color  : Color12     - Current pen color
;;;   pen-width  : Nat         - Stroke width (1-10)
;;;   bg-color   : Color12     - Background color
;;;   paths      : List        - Accumulated path commands (reversed)
;;;   visible?   : Bool        - Show turtle cursor?

(define-record-type turtle%
  (fields x y heading pen-down? pen-color pen-width bg-color paths visible?))

;;; ============================================================
;;; Turtle Constructor
;;; ============================================================

;;; make-turtle : -> Turtle
;;; Create a turtle with default state.
(define (make-turtle)
  (make-turtle%
    *turtle-center-x*     ; x: center
    *turtle-center-y*     ; y: center
    0.0                   ; heading: north (up)
    #t                    ; pen-down: yes
    color12-black         ; pen-color: black
    1                     ; pen-width: 1
    color12-white         ; bg-color: white
    '()                   ; paths: empty
    #t))                  ; visible: yes

;;; make-default-turtle : -> Turtle
;;; Alias for make-turtle.
(define make-default-turtle make-turtle)

;;; ============================================================
;;; State Accessors (aliases for readability)
;;; ============================================================

(define turtle-x turtle%-x)
(define turtle-y turtle%-y)
(define turtle-heading turtle%-heading)
(define turtle-pen-down? turtle%-pen-down?)
(define turtle-pen-color turtle%-pen-color)
(define turtle-pen-width turtle%-pen-width)
(define turtle-bg-color turtle%-bg-color)
(define turtle-paths turtle%-paths)
(define turtle-visible? turtle%-visible?)

;;; ============================================================
;;; State Updaters (functional updates)
;;; ============================================================

;;; Copy turtle with new position and paths
(define (turtle-with-position t x y paths)
  (make-turtle% x y
                (turtle-heading t)
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                paths
                (turtle-visible? t)))

;;; Copy turtle with new heading
(define (turtle-with-heading t heading)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                heading
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                (turtle-paths t)
                (turtle-visible? t)))

;;; Copy turtle with new pen state
(define (turtle-with-pen-down t down?)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                down?
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                (turtle-paths t)
                (turtle-visible? t)))

;;; Copy turtle with new pen color
(define (turtle-with-pen-color t color)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                (turtle-pen-down? t)
                color
                (turtle-pen-width t)
                (turtle-bg-color t)
                (turtle-paths t)
                (turtle-visible? t)))

;;; Copy turtle with new pen width
(define (turtle-with-pen-width t width)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (max 1 (min 10 width))
                (turtle-bg-color t)
                (turtle-paths t)
                (turtle-visible? t)))

;;; Copy turtle with new background color
(define (turtle-with-bg-color t color)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (turtle-pen-width t)
                color
                (turtle-paths t)
                (turtle-visible? t)))

;;; Copy turtle with new paths
(define (turtle-with-paths t paths)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                paths
                (turtle-visible? t)))

;;; Copy turtle with new visibility
(define (turtle-with-visible t visible?)
  (make-turtle% (turtle-x t)
                (turtle-y t)
                (turtle-heading t)
                (turtle-pen-down? t)
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                (turtle-paths t)
                visible?))

;;; ============================================================
;;; Movement Commands
;;; ============================================================

;;; fd : Turtle x Real -> Turtle
;;; Move forward by distance units in the current heading direction.
(define (fd t distance)
  (let* ([heading-rad (deg->rad (turtle-heading t))]
         ;; Note: sin for x because 0 deg = North (up)
         ;; cos for y, negated because screen y increases downward
         [dx (* distance (sin heading-rad))]
         [dy (* (- distance) (cos heading-rad))]
         [new-x (+ (turtle-x t) dx)]
         [new-y (+ (turtle-y t) dy)]
         [current-paths (turtle-paths t)]
         ;; If pen is down and this is the first line, add a move-to first
         [needs-initial-move? (and (turtle-pen-down? t)
                                   (or (null? current-paths)
                                       (not (or (move-to? (car current-paths))
                                                (line-to? (car current-paths))))))]
         [paths-with-move (if needs-initial-move?
                              (cons (make-move-to (turtle-x t) (turtle-y t))
                                    current-paths)
                              current-paths)]
         [new-path (if (turtle-pen-down? t)
                       (make-line-to new-x new-y
                                     (turtle-pen-color t)
                                     (turtle-pen-width t))
                       (make-move-to new-x new-y))]
         [new-paths (cons new-path paths-with-move)])
    (turtle-with-position t new-x new-y new-paths)))

;;; forward : Turtle x Real -> Turtle
;;; Alias for fd.
(define forward fd)

;;; bk : Turtle x Real -> Turtle
;;; Move backward by distance units.
(define (bk t distance)
  (fd t (- distance)))

;;; back : Turtle x Real -> Turtle
;;; Alias for bk.
(define back bk)

;;; rt : Turtle x Real -> Turtle
;;; Turn right (clockwise) by angle degrees.
(define (rt t angle)
  (let ([new-heading (mod (+ (turtle-heading t) angle) 360.0)])
    (turtle-with-heading t new-heading)))

;;; right : Turtle x Real -> Turtle
;;; Alias for rt.
(define right rt)

;;; lt : Turtle x Real -> Turtle
;;; Turn left (counter-clockwise) by angle degrees.
(define (lt t angle)
  (rt t (- angle)))

;;; left : Turtle x Real -> Turtle
;;; Alias for lt.
(define left lt)

;;; mod helper for positive modulo
(define (mod x y)
  (let ([r (remainder x y)])
    (if (< r 0) (+ r y) r)))

;;; ============================================================
;;; Position Commands
;;; ============================================================

;;; setxy : Turtle x Real x Real -> Turtle
;;; Move to absolute position (x, y).
(define (setxy t x y)
  (let* ([current-paths (turtle-paths t)]
         ;; If pen is down and this is the first line, add a move-to first
         [needs-initial-move? (and (turtle-pen-down? t)
                                   (or (null? current-paths)
                                       (not (or (move-to? (car current-paths))
                                                (line-to? (car current-paths))))))]
         [paths-with-move (if needs-initial-move?
                              (cons (make-move-to (turtle-x t) (turtle-y t))
                                    current-paths)
                              current-paths)]
         [new-path (if (turtle-pen-down? t)
                       (make-line-to x y
                                     (turtle-pen-color t)
                                     (turtle-pen-width t))
                       (make-move-to x y))]
         [new-paths (cons new-path paths-with-move)])
    (turtle-with-position t x y new-paths)))

;;; setx : Turtle x Real -> Turtle
;;; Move to absolute x, keeping y.
(define (setx t x)
  (setxy t x (turtle-y t)))

;;; sety : Turtle x Real -> Turtle
;;; Move to absolute y, keeping x.
(define (sety t y)
  (setxy t (turtle-x t) y))

;;; home : Turtle -> Turtle
;;; Return to center (320, 240) facing north.
(define (home t)
  (let* ([current-paths (turtle-paths t)]
         ;; If pen is down and this is the first line, add a move-to first
         [needs-initial-move? (and (turtle-pen-down? t)
                                   (or (null? current-paths)
                                       (not (or (move-to? (car current-paths))
                                                (line-to? (car current-paths))))))]
         [paths-with-move (if needs-initial-move?
                              (cons (make-move-to (turtle-x t) (turtle-y t))
                                    current-paths)
                              current-paths)]
         [new-path (if (turtle-pen-down? t)
                       (make-line-to *turtle-center-x* *turtle-center-y*
                                     (turtle-pen-color t)
                                     (turtle-pen-width t))
                       (make-move-to *turtle-center-x* *turtle-center-y*))]
         [new-paths (cons new-path paths-with-move)])
    (turtle-with-heading
      (turtle-with-position t *turtle-center-x* *turtle-center-y* new-paths)
      0.0)))

;;; setheading : Turtle x Real -> Turtle
;;; Set absolute heading (0 = north, clockwise).
(define (setheading t angle)
  (turtle-with-heading t (mod angle 360.0)))

;;; seth : Turtle x Real -> Turtle
;;; Alias for setheading.
(define seth setheading)

;;; ============================================================
;;; Pen Commands
;;; ============================================================

;;; pu : Turtle -> Turtle
;;; Pen up - stop drawing.
(define (pu t)
  (turtle-with-pen-down t #f))

;;; penup : Turtle -> Turtle
;;; Alias for pu.
(define penup pu)

;;; pd : Turtle -> Turtle
;;; Pen down - start drawing.
(define (pd t)
  (turtle-with-pen-down t #t))

;;; pendown : Turtle -> Turtle
;;; Alias for pd.
(define pendown pd)

;;; cs : Turtle -> Turtle
;;; Clear screen - reset to initial state, clear all paths.
(define (cs t)
  (make-turtle% *turtle-center-x* *turtle-center-y*
                0.0
                #t
                (turtle-pen-color t)
                (turtle-pen-width t)
                (turtle-bg-color t)
                '()
                (turtle-visible? t)))

;;; clearscreen : Turtle -> Turtle
;;; Alias for cs.
(define clearscreen cs)

;;; clean : Turtle -> Turtle
;;; Clear paths but keep position and heading.
(define (clean t)
  (turtle-with-paths t '()))

;;; ============================================================
;;; Pen Attribute Commands
;;; ============================================================

;;; setpc : Turtle x ColorSpec -> Turtle
;;; Set pen color. Accepts Color12, (r g b), integer, or symbol.
(define (setpc t color-spec)
  (let ([color (parse-color12 color-spec)])
    (if color
        (turtle-with-pen-color t color)
        t)))  ; Invalid color, no change

;;; setpencolor : Turtle x ColorSpec -> Turtle
;;; Alias for setpc.
(define setpencolor setpc)

;;; pen-color : Turtle x ColorSpec -> Turtle
;;; Alias for setpc.
(define pen-color setpc)

;;; setpw : Turtle x Nat -> Turtle
;;; Set pen width (1-10).
(define (setpw t width)
  (turtle-with-pen-width t width))

;;; setpensize : Turtle x Nat -> Turtle
;;; Alias for setpw.
(define setpensize setpw)

;;; setbg : Turtle x ColorSpec -> Turtle
;;; Set background color.
(define (setbg t color-spec)
  (let ([color (parse-color12 color-spec)])
    (if color
        (turtle-with-bg-color t color)
        t)))

;;; setbackground : Turtle x ColorSpec -> Turtle
;;; Alias for setbg.
(define setbackground setbg)

;;; ============================================================
;;; Visibility Commands
;;; ============================================================

;;; st : Turtle -> Turtle
;;; Show turtle cursor.
(define (st t)
  (turtle-with-visible t #t))

;;; showturtle : Turtle -> Turtle
;;; Alias for st.
(define showturtle st)

;;; ht : Turtle -> Turtle
;;; Hide turtle cursor.
(define (ht t)
  (turtle-with-visible t #f))

;;; hideturtle : Turtle -> Turtle
;;; Alias for ht.
(define hideturtle ht)

;;; ============================================================
;;; Query Functions
;;; ============================================================

;;; xcor : Turtle -> Real
;;; Get current x coordinate.
(define (xcor t) (turtle-x t))

;;; ycor : Turtle -> Real
;;; Get current y coordinate.
(define (ycor t) (turtle-y t))

;;; heading : Turtle -> Real
;;; Get current heading in degrees.
(define (heading t) (turtle-heading t))

;;; pos : Turtle -> (Pair Real Real)
;;; Get current position as (x . y) pair.
(define (pos t) (cons (turtle-x t) (turtle-y t)))

;;; pendown? : Turtle -> Bool
;;; Test if pen is down.
(define (pendown? t) (turtle-pen-down? t))

;;; shown? : Turtle -> Bool
;;; Test if turtle is visible.
(define (shown? t) (turtle-visible? t))

;;; ============================================================
;;; Drawing Record (for SVG/CAS output)
;;; ============================================================

;;; Drawing: Complete turtle graphics output
;;;   width, height : Nat        - Viewport dimensions
;;;   bg-color      : Color12    - Background color
;;;   paths         : List       - Path commands (chronological order)
;;;   metadata      : Alist      - Optional metadata

(define-record-type drawing%
  (fields width height bg-color paths metadata))

;;; Accessors
(define drawing-width drawing%-width)
(define drawing-height drawing%-height)
(define drawing-bg-color drawing%-bg-color)
(define drawing-paths drawing%-paths)
(define drawing-metadata drawing%-metadata)

;;; turtle->drawing : Turtle -> Drawing
;;; Extract a drawing from turtle state.
(define (turtle->drawing t)
  (make-drawing% *turtle-viewport-width*
                 *turtle-viewport-height*
                 (turtle-bg-color t)
                 (reverse (turtle-paths t))  ; Chronological order
                 '()))

;;; turtle->drawing/meta : Turtle x Alist -> Drawing
;;; Extract a drawing with metadata.
(define (turtle->drawing/meta t metadata)
  (make-drawing% *turtle-viewport-width*
                 *turtle-viewport-height*
                 (turtle-bg-color t)
                 (reverse (turtle-paths t))
                 metadata))

;;; ============================================================
;;; Extended Commands: Shapes
;;; ============================================================

;;; arc : Turtle x Real x Real -> Turtle
;;; Draw an arc with given angle (degrees) and radius.
;;; The arc curves to the right of the turtle's direction.
;;; After drawing, the turtle is at the arc endpoint facing tangent.
(define (arc t angle radius)
  (if (turtle-pen-down? t)
      (let* ([heading-rad (deg->rad (turtle-heading t))]
             [start-x (turtle-x t)]
             [start-y (turtle-y t)]
             ;; Center is 90 degrees to the right of heading
             [cx (+ start-x (* radius (cos heading-rad)))]
             [cy (+ start-y (* radius (sin heading-rad)))]
             ;; Start angle in standard coords (from center)
             [start-angle (- (turtle-heading t) 90.0)]
             [end-angle (+ start-angle angle)]
             ;; End position
             [end-rad (deg->rad end-angle)]
             [end-x (+ cx (* radius (cos end-rad)))]
             [end-y (+ cy (* radius (sin end-rad)))]
             ;; New heading is tangent to arc at end
             [new-heading (mod (+ (turtle-heading t) angle) 360.0)]
             ;; Create arc command
             [arc-cmd (make-arc cx cy radius start-angle end-angle
                                (turtle-pen-color t)
                                (turtle-pen-width t))]
             [new-paths (cons arc-cmd (turtle-paths t))])
        (turtle-with-heading
          (turtle-with-position t end-x end-y new-paths)
          new-heading))
      ;; Pen up: just move to where we would end up
      (let* ([heading-rad (deg->rad (turtle-heading t))]
             [start-x (turtle-x t)]
             [start-y (turtle-y t)]
             [cx (+ start-x (* radius (cos heading-rad)))]
             [cy (+ start-y (* radius (sin heading-rad)))]
             [start-angle (- (turtle-heading t) 90.0)]
             [end-angle (+ start-angle angle)]
             [end-rad (deg->rad end-angle)]
             [end-x (+ cx (* radius (cos end-rad)))]
             [end-y (+ cy (* radius (sin end-rad)))]
             [new-heading (mod (+ (turtle-heading t) angle) 360.0)]
             [move-cmd (make-move-to end-x end-y)]
             [new-paths (cons move-cmd (turtle-paths t))])
        (turtle-with-heading
          (turtle-with-position t end-x end-y new-paths)
          new-heading))))

;;; circle : Turtle x Real -> Turtle
;;; Draw a circle centered at current position with given radius.
;;; The turtle does not move.
(define (circle t radius)
  (circle/fill t radius #f))

;;; circle/fill : Turtle x Real x Bool -> Turtle
;;; Draw a circle, optionally filled.
(define (circle/fill t radius fill?)
  (if (turtle-pen-down? t)
      (let* ([circle-cmd (make-circle (turtle-x t) (turtle-y t) radius
                                      (turtle-pen-color t)
                                      (turtle-pen-width t)
                                      fill?)]
             [new-paths (cons circle-cmd (turtle-paths t))])
        (turtle-with-paths t new-paths))
      t))  ; Pen up, do nothing

;;; filled-circle : Turtle x Real -> Turtle
;;; Draw a filled circle.
(define (filled-circle t radius)
  (circle/fill t radius #t))

;;; dot : Turtle x Real -> Turtle
;;; Draw a filled circle (alias for filled-circle).
(define dot filled-circle)

;;; polygon : Turtle x Nat x Real -> Turtle
;;; Draw a regular polygon with n sides, each of given length.
;;; The polygon is drawn starting from current position/heading.
;;; After drawing, turtle returns to starting position with same heading.
(define (polygon t n side-length)
  (polygon/fill t n side-length #f))

;;; polygon/fill : Turtle x Nat x Real x Bool -> Turtle
;;; Draw a regular polygon, optionally filled.
(define (polygon/fill t n side-length fill?)
  (if (and (turtle-pen-down? t) (>= n 3))
      (let* ([points (compute-polygon-points
                       (turtle-x t)
                       (turtle-y t)
                       (turtle-heading t)
                       n
                       side-length)]
             [polygon-cmd (make-polygon points
                                        (turtle-pen-color t)
                                        (turtle-pen-width t)
                                        fill?)]
             [new-paths (cons polygon-cmd (turtle-paths t))])
        (turtle-with-paths t new-paths))
      t))  ; Pen up or invalid n, do nothing

;;; filled-polygon : Turtle x Nat x Real -> Turtle
;;; Draw a filled regular polygon.
(define (filled-polygon t n side-length)
  (polygon/fill t n side-length #t))

;;; compute-polygon-points : Real x Real x Real x Nat x Real -> (List (Pair Real Real))
;;; Calculate the vertices of a regular polygon.
(define (compute-polygon-points start-x start-y heading n side-length)
  (let ([turn-angle (/ 360.0 n)])
    (let loop ([i 0]
               [x start-x]
               [y start-y]
               [h heading]
               [acc '()])
      (if (= i n)
          (reverse acc)
          (let* ([h-rad (deg->rad h)]
                 [next-x (+ x (* side-length (sin h-rad)))]
                 [next-y (- y (* side-length (cos h-rad)))]
                 [next-h (+ h turn-angle)])
            (loop (+ i 1)
                  next-x
                  next-y
                  next-h
                  (cons (cons x y) acc)))))))

;;; ============================================================
;;; Repeat Helper
;;; ============================================================

;;; repeat : Nat x (Turtle -> Turtle) x Turtle -> Turtle
;;; Apply a procedure n times to a turtle.
(define (repeat n proc t)
  (if (<= n 0)
      t
      (repeat (- n 1) proc (proc t))))

;;; ============================================================
;;; Convenience Procedures for Common Shapes
;;; ============================================================

;;; square : Turtle x Real -> Turtle
;;; Draw a square with given side length.
(define (square t side)
  (repeat 4 (lambda (t) (rt (fd t side) 90)) t))

;;; triangle : Turtle x Real -> Turtle
;;; Draw an equilateral triangle.
(define (triangle t side)
  (repeat 3 (lambda (t) (rt (fd t side) 120)) t))

;;; star : Turtle x Real -> Turtle
;;; Draw a 5-pointed star.
(define (star t size)
  (repeat 5 (lambda (t) (rt (fd t size) 144)) t))

;;; spiral : Turtle x Real x Real x Nat -> Turtle
;;; Draw a spiral with initial size, growth factor, and number of sides.
(define (spiral t size growth n)
  (if (<= n 0)
      t
      (spiral (rt (fd t size) 90) (+ size growth) growth (- n 1))))
