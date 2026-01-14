;;; shell/turtle-path.ss — Path Command Representation for Turtle Graphics
;;;
;;; Defines the path command types used to record turtle movements.
;;; Path commands are stored as tagged lists (S-expressions) for easy
;;; serialization to CAS blocks and conversion to SVG.
;;;
;;; Command Types:
;;;   (move-to x y)                           - Pen-up move
;;;   (line-to x y color width)               - Line segment
;;;   (arc cx cy r start end color width)     - Arc
;;;   (circle cx cy r color width fill?)      - Circle
;;;   (polygon points color width fill?)      - Polygon
;;;
;;; This is Shell code: pure functions for path construction.
;;;
;;; Dependencies: None (color values passed through opaquely)

;;; ====
;;; Move-To Command
;;; ====

;;; make-move-to : Real x Real -> PathCmd
;;; Create a pen-up move command.
(define (make-move-to x y)
  (list 'move-to x y))

;;; move-to? : Any -> Bool
(define (move-to? cmd)
  (and (pair? cmd) (eq? (car cmd) 'move-to)))

;;; Accessors
(define (move-to-x cmd) (list-ref cmd 1))
(define (move-to-y cmd) (list-ref cmd 2))

;;; ====
;;; Line-To Command
;;; ====

;;; make-line-to : Real x Real x Color12 x Nat -> PathCmd
;;; Create a line drawing command.
(define (make-line-to x y color width)
  (list 'line-to x y color width))

;;; line-to? : Any -> Bool
(define (line-to? cmd)
  (and (pair? cmd) (eq? (car cmd) 'line-to)))

;;; Accessors
(define (line-to-x cmd) (list-ref cmd 1))
(define (line-to-y cmd) (list-ref cmd 2))
(define (line-to-color cmd) (list-ref cmd 3))
(define (line-to-width cmd) (list-ref cmd 4))

;;; ====
;;; Arc Command
;;; ====

;;; make-arc : Real x Real x Real x Real x Real x Color12 x Nat -> PathCmd
;;; Create an arc drawing command.
;;; Parameters: center-x, center-y, radius, start-angle, end-angle, color, width
;;; Angles are in degrees.
(define (make-arc cx cy radius start-angle end-angle color width)
  (list 'arc cx cy radius start-angle end-angle color width))

;;; arc? : Any -> Bool
(define (arc? cmd)
  (and (pair? cmd) (eq? (car cmd) 'arc)))

;;; Accessors
(define (arc-cx cmd) (list-ref cmd 1))
(define (arc-cy cmd) (list-ref cmd 2))
(define (arc-radius cmd) (list-ref cmd 3))
(define (arc-start-angle cmd) (list-ref cmd 4))
(define (arc-end-angle cmd) (list-ref cmd 5))
(define (arc-color cmd) (list-ref cmd 6))
(define (arc-width cmd) (list-ref cmd 7))

;;; ====
;;; Circle Command
;;; ====

;;; make-circle : Real x Real x Real x Color12 x Nat x Bool -> PathCmd
;;; Create a circle drawing command.
;;; Parameters: center-x, center-y, radius, color, stroke-width, filled?
(define (make-circle cx cy radius color width fill?)
  (list 'circle cx cy radius color width fill?))

;;; circle? : Any -> Bool
(define (circle? cmd)
  (and (pair? cmd) (eq? (car cmd) 'circle)))

;;; Accessors
(define (circle-cx cmd) (list-ref cmd 1))
(define (circle-cy cmd) (list-ref cmd 2))
(define (circle-radius cmd) (list-ref cmd 3))
(define (circle-color cmd) (list-ref cmd 4))
(define (circle-width cmd) (list-ref cmd 5))
(define (circle-fill? cmd) (list-ref cmd 6))

;;; ====
;;; Polygon Command
;;; ====

;;; make-polygon : (List (Pair Real Real)) x Color12 x Nat x Bool -> PathCmd
;;; Create a polygon drawing command.
;;; Points is a list of (x . y) pairs.
(define (make-polygon points color width fill?)
  (list 'polygon points color width fill?))

;;; polygon? : Any -> Bool
(define (polygon? cmd)
  (and (pair? cmd) (eq? (car cmd) 'polygon)))

;;; Accessors
(define (polygon-points cmd) (list-ref cmd 1))
(define (polygon-color cmd) (list-ref cmd 2))
(define (polygon-width cmd) (list-ref cmd 3))
(define (polygon-fill? cmd) (list-ref cmd 4))

;;; ====
;;; Path Command Utilities
;;; ====

;;; path-cmd? : Any -> Bool
;;; Test if value is any path command.
(define (path-cmd? x)
  (or (move-to? x)
      (line-to? x)
      (arc? x)
      (circle? x)
      (polygon? x)))

;;; path-cmd-type : PathCmd -> Symbol
;;; Get the type tag of a path command.
(define (path-cmd-type cmd)
  (if (pair? cmd) (car cmd) #f))

;;; path-cmd-endpoint : PathCmd -> (Pair Real Real) | #f
;;; Get the endpoint of a command if applicable.
;;; Returns #f for shapes without clear endpoints.
(define (path-cmd-endpoint cmd)
  (cond
   [(move-to? cmd) (cons (move-to-x cmd) (move-to-y cmd))]
   [(line-to? cmd) (cons (line-to-x cmd) (line-to-y cmd))]
   [else #f]))

;;; ====
;;; Path Serialization (for CAS blocks)
;;; ====

;;; path-cmd->sexpr : PathCmd -> S-expr
;;; Convert path command to a pure S-expression for storage.
;;; Colors are converted to lists for serialization.
(define (path-cmd->sexpr cmd)
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

;;; sexpr->path-cmd : S-expr -> PathCmd
;;; Reconstruct path command from S-expression.
;;; Requires color12 functions from turtle-color.ss.
(define (sexpr->path-cmd sexpr)
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

;;; Note: color12->list and list->color12 are defined in turtle-color.ss
;;; This file forward-references them; they must be loaded first.
