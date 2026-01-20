(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'turtle-path)
(doc 'description "Path Command Representation for Turtle Graphics - Defines the path command types used to record turtle movements. Path commands are stored as tagged lists (S-expressions) for easy serialization to CAS blocks and conversion to SVG.")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'note "Command Types:")
(doc 'note "  (move-to x y)                           - Pen-up move")
(doc 'note "  (line-to x y color width)               - Line segment")
(doc 'note "  (arc cx cy r start end color width)     - Arc")
(doc 'note "  (circle cx cy r color width fill?)      - Circle")
(doc 'note "  (polygon points color width fill?)      - Polygon")

(doc 'note "Dependencies: None (color values passed through opaquely)")

(doc 'section 'move-to-command)

(define (make-move-to x y)
  (doc 'type (-> Real Real PathCmd))
  (doc 'description "Create a pen-up move command.")
  (list 'move-to x y))

(define (move-to? cmd)
  (doc 'type (-> Any Bool))
  (and (pair? cmd) (eq? (car cmd) 'move-to)))

(doc 'note "Accessors")
(define (move-to-x cmd) (list-ref cmd 1))
(define (move-to-y cmd) (list-ref cmd 2))

(doc 'section 'line-to-command)

(define (make-line-to x y color width)
  (doc 'type (-> Real Real Color12 Nat PathCmd))
  (doc 'description "Create a line drawing command.")
  (list 'line-to x y color width))

(define (line-to? cmd)
  (doc 'type (-> Any Bool))
  (and (pair? cmd) (eq? (car cmd) 'line-to)))

(doc 'note "Accessors")
(define (line-to-x cmd) (list-ref cmd 1))
(define (line-to-y cmd) (list-ref cmd 2))
(define (line-to-color cmd) (list-ref cmd 3))
(define (line-to-width cmd) (list-ref cmd 4))

(doc 'section 'arc-command)

(define (make-arc cx cy radius start-angle end-angle color width)
  (doc 'type (-> Real Real Real Real Real Color12 Nat PathCmd))
  (doc 'description "Create an arc drawing command. Parameters: center-x, center-y, radius, start-angle, end-angle, color, width. Angles are in degrees.")
  (list 'arc cx cy radius start-angle end-angle color width))

(define (arc? cmd)
  (doc 'type (-> Any Bool))
  (and (pair? cmd) (eq? (car cmd) 'arc)))

(doc 'note "Accessors")
(define (arc-cx cmd) (list-ref cmd 1))
(define (arc-cy cmd) (list-ref cmd 2))
(define (arc-radius cmd) (list-ref cmd 3))
(define (arc-start-angle cmd) (list-ref cmd 4))
(define (arc-end-angle cmd) (list-ref cmd 5))
(define (arc-color cmd) (list-ref cmd 6))
(define (arc-width cmd) (list-ref cmd 7))

(doc 'section 'circle-command)

(define (make-circle cx cy radius color width fill?)
  (doc 'type (-> Real Real Real Color12 Nat Bool PathCmd))
  (doc 'description "Create a circle drawing command. Parameters: center-x, center-y, radius, color, stroke-width, filled?")
  (list 'circle cx cy radius color width fill?))

(define (circle? cmd)
  (doc 'type (-> Any Bool))
  (and (pair? cmd) (eq? (car cmd) 'circle)))

(doc 'note "Accessors")
(define (circle-cx cmd) (list-ref cmd 1))
(define (circle-cy cmd) (list-ref cmd 2))
(define (circle-radius cmd) (list-ref cmd 3))
(define (circle-color cmd) (list-ref cmd 4))
(define (circle-width cmd) (list-ref cmd 5))
(define (circle-fill? cmd) (list-ref cmd 6))

(doc 'section 'polygon-command)

(define (make-polygon points color width fill?)
  (doc 'type (-> (List (Pair Real Real)) Color12 Nat Bool PathCmd))
  (doc 'description "Create a polygon drawing command. Points is a list of (x . y) pairs.")
  (list 'polygon points color width fill?))

(define (polygon? cmd)
  (doc 'type (-> Any Bool))
  (and (pair? cmd) (eq? (car cmd) 'polygon)))

(doc 'note "Accessors")
(define (polygon-points cmd) (list-ref cmd 1))
(define (polygon-color cmd) (list-ref cmd 2))
(define (polygon-width cmd) (list-ref cmd 3))
(define (polygon-fill? cmd) (list-ref cmd 4))

(doc 'section 'path-command-utilities)

(define (path-cmd? x)
  (doc 'type (-> Any Bool))
  (doc 'description "Test if value is any path command.")
  (or (move-to? x)
      (line-to? x)
      (arc? x)
      (circle? x)
      (polygon? x)))

(define (path-cmd-type cmd)
  (doc 'type (-> PathCmd Symbol))
  (doc 'description "Get the type tag of a path command.")
  (if (pair? cmd) (car cmd) #f))

(define (path-cmd-endpoint cmd)
  (doc 'type (-> PathCmd (U (Pair Real Real) Bool)))
  (doc 'description "Get the endpoint of a command if applicable. Returns #f for shapes without clear endpoints.")
  (cond
   [(move-to? cmd) (cons (move-to-x cmd) (move-to-y cmd))]
   [(line-to? cmd) (cons (line-to-x cmd) (line-to-y cmd))]
   [else #f]))

(doc 'section 'path-serialization)

(define (path-cmd->sexpr cmd)
  (doc 'type (-> PathCmd S-expr))
  (doc 'description "Convert path command to a pure S-expression for storage. Colors are converted to lists for serialization.")
  (case (path-cmd-type cmd)
        [(move-to)
         `(move-to ,(move-to-x cmd) ,(move-to-y cmd))]
        
        [(line-to)
         `(line-to ,(line-to-x cmd) ,(line-to-y cmd)
           ,(color12->list (line-to-color cmd))
           ,(line-to-width cmd))]
        
        [(arc)
         `(arc ,(arc-cx cmd) ,(arc-cy cmd) ,(arc-radius cmd)
           ,(arc-start-angle cmd) ,(arc-end-angle cmd)
           ,(color12->list (arc-color cmd))
           ,(arc-width cmd))]
        
        [(circle)
         `(circle ,(circle-cx cmd) ,(circle-cy cmd) ,(circle-radius cmd)
           ,(color12->list (circle-color cmd))
           ,(circle-width cmd)
           ,(circle-fill? cmd))]
        
        [(polygon)
         `(polygon ,(polygon-points cmd)
           ,(color12->list (polygon-color cmd))
           ,(polygon-width cmd)
           ,(polygon-fill? cmd))]

        [else cmd]))

(define (sexpr->path-cmd sexpr)
  (doc 'type (-> S-expr PathCmd))
  (doc 'description "Reconstruct path command from S-expression. Requires color12 functions from turtle-color.ss.")
  (case (car sexpr)
        [(move-to)
         (make-move-to (list-ref sexpr 1) (list-ref sexpr 2))]
        
        [(line-to)
         (make-line-to (list-ref sexpr 1)
                       (list-ref sexpr 2)
                       (list->color12 (list-ref sexpr 3))
                       (list-ref sexpr 4))]
        
        [(arc)
         (make-arc (list-ref sexpr 1) (list-ref sexpr 2) (list-ref sexpr 3)
                   (list-ref sexpr 4) (list-ref sexpr 5)
                   (list->color12 (list-ref sexpr 6))
                   (list-ref sexpr 7))]
        
        [(circle)
         (make-circle (list-ref sexpr 1) (list-ref sexpr 2) (list-ref sexpr 3)
                      (list->color12 (list-ref sexpr 4))
                      (list-ref sexpr 5)
                      (list-ref sexpr 6))]
        
        [(polygon)
         (make-polygon (list-ref sexpr 1)
                       (list->color12 (list-ref sexpr 2))
                       (list-ref sexpr 3)
                       (list-ref sexpr 4))]

        [else sexpr]))

(doc 'note "color12->list and list->color12 are defined in turtle-color.ss")
(doc 'note "This file forward-references them; they must be loaded first.")
