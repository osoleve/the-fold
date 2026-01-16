;;; lattice/fp/test-protocol.ss — Tests for Open Protocol System
;;;
;;; Run with: scheme --script lattice/fp/test-protocol.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/protocol.ss")

;;; ====
;;; Test Fixtures: Simple Shape Types
;;; ====

;;; Circle: (list 'circle x y radius)
(define (make-circle x y r)
  (list 'circle x y r))

(define (circle-x c) (cadr c))
(define (circle-y c) (caddr c))
(define (circle-radius c) (cadddr c))

;;; Rectangle: (list 'rectangle x y w h)
(define (make-rectangle x y w h)
  (list 'rectangle x y w h))

(define (rect-x r) (cadr r))
(define (rect-y r) (caddr r))
(define (rect-w r) (cadddr r))
(define (rect-h r) (car (cddddr r)))

;;; Triangle: (list 'triangle x1 y1 x2 y2 x3 y3)
(define (make-triangle x1 y1 x2 y2 x3 y3)
  (list 'triangle x1 y1 x2 y2 x3 y3))

;;; ====
;;; Test Protocol Definition
;;; ====

;;; Define test protocols
(define-protocol (shape-area s) "Calculate area of shape")
(define-protocol (shape-perimeter s) "Calculate perimeter of shape")
(define-protocol (shape-center s) "Get center point of shape")
(define-protocol (shape-scale s factor) "Scale shape by factor")

;;; ====
;;; Protocol Implementations
;;; ====

;;; Circle implementations
(implement-protocol! 'shape-area 'circle
  (lambda (c)
    (let ([r (circle-radius c)])
      (* 3.14159265 r r))))

(implement-protocol! 'shape-perimeter 'circle
  (lambda (c)
    (* 2 3.14159265 (circle-radius c))))

(implement-protocol! 'shape-center 'circle
  (lambda (c)
    (list (circle-x c) (circle-y c))))

(implement-protocol! 'shape-scale 'circle
  (lambda (c factor)
    (make-circle (circle-x c) (circle-y c)
                 (* (circle-radius c) factor))))

;;; Rectangle implementations
(implement-protocol! 'shape-area 'rectangle
  (lambda (r)
    (* (rect-w r) (rect-h r))))

(implement-protocol! 'shape-perimeter 'rectangle
  (lambda (r)
    (* 2 (+ (rect-w r) (rect-h r)))))

(implement-protocol! 'shape-center 'rectangle
  (lambda (r)
    (list (+ (rect-x r) (/ (rect-w r) 2))
          (+ (rect-y r) (/ (rect-h r) 2)))))

(implement-protocol! 'shape-scale 'rectangle
  (lambda (r factor)
    (make-rectangle (rect-x r) (rect-y r)
                    (* (rect-w r) factor)
                    (* (rect-h r) factor))))

;;; ====
;;; Tests: Basic Protocol Dispatch
;;; ====

(test-group "Basic Protocol Dispatch"
  (define-test "dispatch to circle implementation"
    (let ([c (make-circle 0 0 10)])
      (assert-true (> (shape-area c) 314))
      (assert-true (< (shape-area c) 315))))

  (define-test "dispatch to rectangle implementation"
    (let ([r (make-rectangle 0 0 10 5)])
      (assert-equal 50 (shape-area r))))

  (define-test "dispatch perimeter - circle"
    (let ([c (make-circle 0 0 1)])
      (assert-true (> (shape-perimeter c) 6.28))
      (assert-true (< (shape-perimeter c) 6.29))))

  (define-test "dispatch perimeter - rectangle"
    (let ([r (make-rectangle 0 0 10 5)])
      (assert-equal 30 (shape-perimeter r))))

  (define-test "dispatch center - circle"
    (let ([c (make-circle 5 10 3)])
      (assert-equal '(5 10) (shape-center c))))

  (define-test "dispatch center - rectangle"
    (let ([r (make-rectangle 0 0 10 10)])
      (assert-equal '(5 5) (shape-center r)))))

;;; ====
;;; Tests: Multi-argument Protocols
;;; ====

(test-group "Multi-argument Protocols"
  (define-test "scale circle"
    (let* ([c (make-circle 0 0 10)]
           [scaled (shape-scale c 2)])
      (assert-equal 20 (circle-radius scaled))))

  (define-test "scale rectangle"
    (let* ([r (make-rectangle 0 0 10 5)]
           [scaled (shape-scale r 3)])
      (assert-equal 30 (rect-w scaled))
      (assert-equal 15 (rect-h scaled)))))

;;; ====
;;; Tests: Error Handling
;;; ====

(test-group "Error Handling"
  (define-test "error on unimplemented type"
    ;; Triangle has no implementations
    (let ([t (make-triangle 0 0 10 0 5 8)])
      (assert-error (lambda () (shape-area t)))))

  (define-test "error on non-tagged value"
    (assert-error (lambda () (shape-area 42))))

  (define-test "error on non-symbol-tagged list"
    (assert-error (lambda () (shape-area '(123 data))))))

;;; ====
;;; Tests: Type Tag Extraction
;;; ====

(test-group "Type Tag Extraction"
  (define-test "get-type-tag from circle"
    (assert-equal 'circle (get-type-tag (make-circle 0 0 5))))

  (define-test "get-type-tag from rectangle"
    (assert-equal 'rectangle (get-type-tag (make-rectangle 0 0 10 5))))

  (define-test "get-type-tag returns #f for number"
    (assert-false (get-type-tag 42)))

  (define-test "get-type-tag returns #f for string"
    (assert-false (get-type-tag "hello")))

  (define-test "get-type-tag returns #f for non-symbol-headed list"
    (assert-false (get-type-tag '(1 2 3)))))

;;; ====
;;; Tests: Introspection
;;; ====

(test-group "Introspection"
  (define-test "protocol-exists? for registered protocol"
    (assert-true (protocol-exists? 'shape-area)))

  (define-test "protocol-exists? for unregistered protocol"
    (assert-false (protocol-exists? 'nonexistent-protocol)))

  (define-test "type-implements? true case"
    (assert-true (type-implements? 'circle 'shape-area)))

  (define-test "type-implements? false case"
    (assert-false (type-implements? 'triangle 'shape-area)))

  (define-test "protocol-implementations lists types"
    (let ([impls (protocol-implementations 'shape-area)])
      (assert-true (if (memq 'circle impls) #t #f))
      (assert-true (if (memq 'rectangle impls) #t #f))))

  (define-test "list-protocols includes registered"
    (let ([protos (list-protocols)])
      (assert-true (if (memq 'shape-area protos) #t #f))
      (assert-true (if (memq 'shape-perimeter protos) #t #f)))))

;;; ====
;;; Tests: Protocol with Default
;;; ====

;;; Define a protocol with default behavior
(define-protocol/default (shape-name s)
  (lambda (s) "unknown-shape"))

(implement-protocol! 'shape-name 'circle
  (lambda (c) "circle"))

(implement-protocol! 'shape-name 'rectangle
  (lambda (r) "rectangle"))

(test-group "Protocol with Default"
  (define-test "uses implementation when available"
    (assert-equal "circle" (shape-name (make-circle 0 0 5))))

  (define-test "uses default for unimplemented type"
    (assert-equal "unknown-shape" (shape-name (make-triangle 0 0 1 0 0 1))))

  (define-test "uses default for non-tagged values"
    (assert-equal "unknown-shape" (shape-name 42))))

;;; ====
;;; Tests: Polymorphic Usage Pattern
;;; ====

(test-group "Polymorphic Usage"
  (define-test "process mixed shape list"
    (let* ([shapes (list (make-circle 0 0 10)
                         (make-rectangle 0 0 10 5)
                         (make-circle 0 0 5))]
           [areas (map shape-area shapes)])
      ;; Circle r=10: ~314, Rect: 50, Circle r=5: ~78
      (assert-equal 3 (length areas))
      (assert-true (> (car areas) 300))
      (assert-equal 50 (cadr areas))
      (assert-true (< (caddr areas) 80))))

  (define-test "total area of mixed shapes"
    (let* ([shapes (list (make-rectangle 0 0 10 10)
                         (make-rectangle 0 0 5 5))]
           [total (apply + (map shape-area shapes))])
      (assert-equal 125 total))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
