#!/usr/bin/env scheme --script
;;; test-layers.ss — Unit Tests for Layering System
;;;
;;; Tests basic functionality of the layering system.
;;; Run with: scheme --script test-layers.ss

(load "shell/layout.ss")
(load "shell/layers.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test-start name)
  (display (string-append "\nTest: " name "\n")))

(define (assert-equal expected actual test-name)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display (string-append "  ✓ " test-name "\n")))
      (begin
        (set! fail-count (+ fail-count 1))
        (display (string-append "  ✗ " test-name "\n"))
        (display (string-append "    Expected: " (format "~a" expected) "\n"))
        (display (string-append "    Actual:   " (format "~a" actual) "\n")))))

(define (assert-true value test-name)
  (assert-equal #t value test-name))

(define (assert-false value test-name)
  (assert-equal #f value test-name))

(define (test-summary)
  (display "\n═══════════════════════════════════════\n")
  (display (string-append "Total tests: " (number->string test-count) "\n"))
  (display (string-append "Passed: " (number->string pass-count) "\n"))
  (display (string-append "Failed: " (number->string fail-count) "\n"))
  (display "═══════════════════════════════════════\n")
  (if (= fail-count 0)
      (display "All tests passed! ✓\n")
      (display "Some tests failed! ✗\n")))

;;; ============================================================
;;; Tests: Transparency
;;; ============================================================

(define (test-transparency)
  (test-start "Transparency")

  (assert-true (transparent? transparent-char)
               "transparent-char should be transparent")

  (assert-false (transparent? #\space)
                "space should not be transparent")

  (assert-false (transparent? #\x)
                "regular char should not be transparent")

  (let ([canvas (make-transparent-canvas 10 5)])
    (assert-equal 10 (canvas-width canvas)
                  "transparent canvas width")
    (assert-equal 5 (canvas-height canvas)
                  "transparent canvas height")
    (assert-true (transparent? (canvas-ref canvas 0 0))
                 "transparent canvas cells should be transparent")))

;;; ============================================================
;;; Tests: Layer Construction
;;; ============================================================

(define (test-layer-construction)
  (test-start "Layer Construction")

  (let* ([canvas (make-canvas 20 10)]
         [layer (make-layer 'test canvas)])
    (assert-equal 'test (layer-name layer)
                  "layer name")
    (assert-equal 0 (layer-depth layer)
                  "default depth is 0")
    (assert-equal 0 (point-x (layer-offset layer))
                  "default offset x is 0")
    (assert-equal 0 (point-y (layer-offset layer))
                  "default offset y is 0")
    (assert-true (layer-visible layer)
                 "layer visible by default"))

  (let* ([canvas (make-canvas 20 10)]
         [layer (make-layer 'test canvas 50)])
    (assert-equal 50 (layer-depth layer)
                  "custom depth"))

  (let* ([canvas (make-canvas 20 10)]
         [offset (point 5 3)]
         [layer (make-layer 'test canvas 50 offset)])
    (assert-equal 5 (point-x (layer-offset layer))
                  "custom offset x")
    (assert-equal 3 (point-y (layer-offset layer))
                  "custom offset y")))

;;; ============================================================
;;; Tests: Layer Operations
;;; ============================================================

(define (test-layer-operations)
  (test-start "Layer Operations")

  (let* ([canvas (make-canvas 20 10)]
         [layer (make-layer 'test canvas 50)]
         [layer2 (layer-set-visible layer #f)])
    (assert-false (layer-visible layer2)
                  "layer-set-visible should hide layer")
    (assert-true (layer-visible layer)
                 "original layer unchanged"))

  (let* ([canvas (make-canvas 20 10)]
         [layer (make-layer 'test canvas 50)]
         [layer2 (layer-set-depth layer 100)])
    (assert-equal 100 (layer-depth layer2)
                  "layer-set-depth should change depth")
    (assert-equal 50 (layer-depth layer)
                  "original layer unchanged"))

  (let* ([canvas (make-canvas 20 10)]
         [layer (make-layer 'test canvas)]
         [offset (point 10 5)]
         [layer2 (layer-set-offset layer offset)])
    (assert-equal 10 (point-x (layer-offset layer2))
                  "layer-set-offset should change offset")
    (assert-equal 0 (point-x (layer-offset layer))
                  "original layer unchanged")))

;;; ============================================================
;;; Tests: Layer Stack
;;; ============================================================

(define (test-layer-stack)
  (test-start "Layer Stack")

  (let ([stack (make-layer-stack)])
    (assert-equal 0 (length (stack-layers stack))
                  "empty stack has no layers"))

  (let* ([layer1 (make-layer 'a (make-canvas 10 10) 10)]
         [layer2 (make-layer 'b (make-canvas 10 10) 20)]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer2)]  ; Add higher depth first
         [stack (stack-add-layer stack layer1)]) ; Add lower depth second
    (assert-equal 2 (length (stack-layers stack))
                  "stack has 2 layers")
    ;; Layers should be sorted by depth
    (assert-equal 'a (layer-name (car (stack-layers stack)))
                  "first layer should be lowest depth (a)")
    (assert-equal 'b (layer-name (cadr (stack-layers stack)))
                  "second layer should be highest depth (b)"))

  (let* ([layer (make-layer 'test (make-canvas 10 10))]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer)]
         [found (stack-find-layer stack 'test)])
    (assert-true (layer? found)
                 "stack-find-layer should find layer")
    (assert-equal 'test (layer-name found)
                  "found layer has correct name"))

  (let* ([layer (make-layer 'test (make-canvas 10 10))]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer)]
         [stack2 (stack-remove-layer stack 'test)])
    (assert-equal 0 (length (stack-layers stack2))
                  "stack-remove-layer should remove layer")
    (assert-equal 1 (length (stack-layers stack))
                  "original stack unchanged")))

;;; Helper to check if value is a layer record
(define (layer? obj)
  (and (record? obj)
       (eq? (record-type-descriptor obj)
            (record-type-descriptor (make-layer 'test (make-canvas 1 1))))))

;;; ============================================================
;;; Tests: Stack Operations
;;; ============================================================

(define (test-stack-operations)
  (test-start "Stack Operations")

  (let* ([layer (make-layer 'test (make-canvas 10 10))]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer)]
         [stack2 (stack-hide-layer stack 'test)]
         [found (stack-find-layer stack2 'test)])
    (assert-false (layer-visible found)
                  "stack-hide-layer should hide layer"))

  (let* ([layer (make-layer 'test (make-canvas 10 10))]
         [layer (layer-set-visible layer #f)]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer)]
         [stack2 (stack-show-layer stack 'test)]
         [found (stack-find-layer stack2 'test)])
    (assert-true (layer-visible found)
                 "stack-show-layer should show layer"))

  (let* ([layer (make-layer 'test (make-canvas 10 10) 10)]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer)]
         [stack2 (stack-reorder stack 'test 100)]
         [found (stack-find-layer stack2 'test)])
    (assert-equal 100 (layer-depth found)
                  "stack-reorder should change depth")))

;;; ============================================================
;;; Tests: Composition
;;; ============================================================

(define (test-composition)
  (test-start "Composition")

  ;; Test transparent composition
  (let* ([dest (make-canvas 10 5)]
         [dest (fill-rect dest (make-rect (point 0 0) 10 5) #\A)]
         [src (make-transparent-canvas 5 3)]
         [src (canvas-set src 2 1 #\B)]
         [result (composite-transparent dest src (point 2 1))])
    (assert-equal #\A (canvas-ref result 0 0)
                  "destination unchanged where no source")
    (assert-equal #\B (canvas-ref result 4 2)
                  "source overlays at offset")
    (assert-equal #\A (canvas-ref result 1 1)
                  "transparent cells don't overwrite"))

  ;; Test flattening
  (let* ([layer1 (make-layer 'bg (make-canvas 10 5) 0)]
         [layer1 (layer-fill-rect layer1 (make-rect (point 0 0) 10 5) #\.)]
         [layer2 (make-layer 'fg (make-transparent-canvas 5 3) 10 (point 2 1))]
         [layer2 (layer-set-canvas layer2
                                   (canvas-set (layer-canvas layer2) 2 1 #\X))]
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer1)]
         [stack (stack-add-layer stack layer2)]
         [result (flatten-layers stack 10 5)])
    (assert-equal #\. (canvas-ref result 0 0)
                  "background layer renders")
    (assert-equal #\X (canvas-ref result 4 2)
                  "foreground layer composites on top")))

;;; ============================================================
;;; Tests: Helper Constructors
;;; ============================================================

(define (test-helper-constructors)
  (test-start "Helper Constructors")

  (let ([layer (make-background-layer 40 20)])
    (assert-equal 'background (layer-name layer)
                  "background layer name")
    (assert-equal 0 (layer-depth layer)
                  "background layer depth")
    (assert-equal 40 (canvas-width (layer-canvas layer))
                  "background layer width"))

  (let ([layer (make-sprite-layer 'hero 10 8 (point 15 10))])
    (assert-equal 'hero (layer-name layer)
                  "sprite layer name")
    (assert-equal 50 (layer-depth layer)
                  "sprite layer depth")
    (assert-true (transparent? (canvas-ref (layer-canvas layer) 0 0))
                 "sprite layer is transparent"))

  (let ([layer (make-ui-layer 60 25)])
    (assert-equal 'ui (layer-name layer)
                  "ui layer name")
    (assert-equal 100 (layer-depth layer)
                  "ui layer depth")
    (assert-true (transparent? (canvas-ref (layer-canvas layer) 0 0))
                 "ui layer is transparent")))

;;; ============================================================
;;; Main Test Runner
;;; ============================================================

(define (main)
  (display "\n")
  (display "═══════════════════════════════════════\n")
  (display "  Layering System Unit Tests\n")
  (display "═══════════════════════════════════════\n")

  (test-transparency)
  (test-layer-construction)
  (test-layer-operations)
  (test-layer-stack)
  (test-stack-operations)
  (test-composition)
  (test-helper-constructors)

  (test-summary))

(main)
