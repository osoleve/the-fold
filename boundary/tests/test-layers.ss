(doc 'note "Run from project root: scheme --script boundary/tests/test-layers.ss")

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "lattice/ui/layout.ss")
(load "boundary/ui/layers.ss")

(doc 'module 'test-layers)
(doc 'description "Tests for canvas layering and depth composition system")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "\n")
(display "====\n")
(display "         CANVAS LAYERING SYSTEM TESTS\n")
(display "====\n")

(doc 'section 'transparency-tests)

(test-group transparency
            (define-test transparent-char-exists
              ;; transparent-char should be defined
              (assert-true (char? transparent-char)))
            
            (define-test transparent-predicate
              ;; transparent? should identify transparent char
              (assert-true (transparent? transparent-char))
              (assert-false (transparent? #\space))
              (assert-false (transparent? #\X)))
            
            (define-test make-transparent-canvas-basic
              ;; Create transparent canvas
              (let ([c (make-transparent-canvas 10 5)])
                   (assert-equal 10 (canvas-width c))
                   (assert-equal 5 (canvas-height c))
                   ;; All cells should be transparent
                   (assert-true (transparent? (canvas-ref c 0 0)))
                   (assert-true (transparent? (canvas-ref c 5 2)))
                   (assert-true (transparent? (canvas-ref c 9 4))))))

(doc 'section 'layer-construction-tests)

(test-group layer-construction
            (define-test make-layer-basic
              ;; Create a simple layer
              (let* ([c (make-canvas 10 5)]
                     [layer (make-layer 'test c)])
                    (assert-equal 'test (layer-name layer))
                    (assert-true (layer-visible layer))
                    (assert-equal 0 (layer-depth layer))
                    (assert-equal 0 (point-x (layer-offset layer)))
                    (assert-equal 0 (point-y (layer-offset layer)))))
            
            (define-test make-layer-with-depth
              ;; Create layer with custom depth
              (let* ([c (make-canvas 10 5)]
                     [layer (make-layer 'foreground c 100)])
                    (assert-equal 100 (layer-depth layer))))
            
            (define-test make-layer-with-offset
              ;; Create layer with depth and offset
              (let* ([c (make-canvas 10 5)]
                     [layer (make-layer 'sprite c 50 (point 10 20))])
                    (assert-equal 50 (layer-depth layer))
                    (assert-equal 10 (point-x (layer-offset layer)))
                    (assert-equal 20 (point-y (layer-offset layer))))))

(doc 'section 'layer-setters-tests)

(test-group layer-setters
            (define-test layer-set-visible-test
              ;; Toggle layer visibility
              (let* ([c (make-canvas 5 5)]
                     [layer (make-layer 'test c)]
                     [hidden (layer-set-visible layer #f)]
                     [shown (layer-set-visible hidden #t)])
                    (assert-true (layer-visible layer))
                    (assert-false (layer-visible hidden))
                    (assert-true (layer-visible shown))))
            
            (define-test layer-set-canvas-test
              ;; Update layer canvas
              (let* ([c1 (make-canvas 5 5)]
                     [c2 (make-canvas 10 10)]
                     [layer (make-layer 'test c1)]
                     [updated (layer-set-canvas layer c2)])
                    (assert-equal 5 (canvas-width (layer-canvas layer)))
                    (assert-equal 10 (canvas-width (layer-canvas updated)))))
            
            (define-test layer-set-offset-test
              ;; Move layer position
              (let* ([c (make-canvas 5 5)]
                     [layer (make-layer 'test c)]
                     [moved (layer-set-offset layer (point 15 25))])
                    (assert-equal 0 (point-x (layer-offset layer)))
                    (assert-equal 15 (point-x (layer-offset moved)))
                    (assert-equal 25 (point-y (layer-offset moved)))))
            
            (define-test layer-set-depth-test
              ;; Change layer z-order
              (let* ([c (make-canvas 5 5)]
                     [layer (make-layer 'test c 10)]
                     [reordered (layer-set-depth layer 99)])
                    (assert-equal 10 (layer-depth layer))
                    (assert-equal 99 (layer-depth reordered)))))

(doc 'section 'layer-stack-tests)

(test-group layer-stack
            (define-test make-empty-stack
              ;; Create empty stack
              (let ([stack (make-layer-stack)])
                   (assert-true (null? (stack-layers stack)))))
            
            (define-test make-stack-with-layers
              ;; Create stack with initial layers
              (let* ([l1 (make-layer 'bg (make-canvas 10 10) 0)]
                     [l2 (make-layer 'fg (make-canvas 10 10) 100)]
                     [stack (make-layer-stack (list l2 l1))])  ; Order shouldn't matter
                    (assert-equal 2 (length (stack-layers stack)))
                    ;; Should be sorted by depth
                    (assert-equal 'bg (layer-name (car (stack-layers stack))))
                    (assert-equal 'fg (layer-name (cadr (stack-layers stack))))))
            
            (define-test stack-add-layer-test
              ;; Add layer maintains depth order
              (let* ([bg (make-layer 'bg (make-canvas 10 10) 0)]
                     [ui (make-layer 'ui (make-canvas 10 10) 100)]
                     [stack (make-layer-stack)]
                     [stack (stack-add-layer stack ui)]
                     [stack (stack-add-layer stack bg)])  ; Add bg after ui
                    ;; bg should come first (lower depth)
                    (assert-equal 'bg (layer-name (car (stack-layers stack))))
                    (assert-equal 'ui (layer-name (cadr (stack-layers stack))))))
            
            (define-test stack-remove-layer-test
              ;; Remove layer by name
              (let* ([l1 (make-layer 'keep (make-canvas 5 5) 0)]
                     [l2 (make-layer 'remove (make-canvas 5 5) 10)]
                     [stack (make-layer-stack (list l1 l2))]
                     [stack (stack-remove-layer stack 'remove)])
                    (assert-equal 1 (length (stack-layers stack)))
                    (assert-equal 'keep (layer-name (car (stack-layers stack))))))
            
            (define-test stack-find-layer-test
              ;; Find layer by name
              (let* ([l1 (make-layer 'first (make-canvas 5 5) 0)]
                     [l2 (make-layer 'second (make-canvas 5 5) 10)]
                     [stack (make-layer-stack (list l1 l2))])
                    (assert-equal 'first (layer-name (stack-find-layer stack 'first)))
                    (assert-equal 'second (layer-name (stack-find-layer stack 'second)))
                    (assert-false (stack-find-layer stack 'nonexistent)))))

(doc 'section 'stack-update-operations-tests)

(test-group stack-operations
            (define-test stack-update-layer-test
              ;; Update layer by name
              (let* ([layer (make-layer 'test (make-canvas 5 5) 10)]
                     [stack (make-layer-stack (list layer))]
                     [stack (stack-update-layer stack 'test
                                                (lambda (l) (layer-set-depth l 99)))])
                    (assert-equal 99 (layer-depth (stack-find-layer stack 'test)))))
            
            (define-test stack-show-hide-layer-test
              ;; Show and hide layers
              (let* ([layer (make-layer 'test (make-canvas 5 5))]
                     [stack (make-layer-stack (list layer))]
                     [hidden (stack-hide-layer stack 'test)]
                     [shown (stack-show-layer hidden 'test)])
                    (assert-false (layer-visible (stack-find-layer hidden 'test)))
                    (assert-true (layer-visible (stack-find-layer shown 'test)))))
            
            (define-test stack-reorder-test
              ;; Change layer depth via stack
              (let* ([l1 (make-layer 'first (make-canvas 5 5) 0)]
                     [l2 (make-layer 'second (make-canvas 5 5) 10)]
                     [stack (make-layer-stack (list l1 l2))]
                     [stack (stack-reorder stack 'first 100)])
                    ;; first should now be after second
                    (assert-equal 'second (layer-name (car (stack-layers stack))))
                    (assert-equal 'first (layer-name (cadr (stack-layers stack)))))))

(doc 'section 'transparent-composition-tests)

(test-group transparent-composition
            (define-test composite-transparent-basic
              ;; Transparent cells don't overwrite
              (let* ([dest (make-canvas 5 5)]
                     [dest (draw-string dest (point 0 0) "DEST")]
                     [src (make-transparent-canvas 5 5)]
                     [src (canvas-set src 2 0 #\X)]  ; Draw X at position 2
                     [result (composite-transparent dest src (point 0 0))])
                    ;; D and E should be preserved (src is transparent there)
                    (assert-equal #\D (canvas-ref result 0 0))
                    (assert-equal #\E (canvas-ref result 1 0))
                    ;; X should overwrite S
                    (assert-equal #\X (canvas-ref result 2 0))
                    ;; T should be preserved
                    (assert-equal #\T (canvas-ref result 3 0))))
            
            (define-test composite-transparent-offset
              ;; Composite at offset position
              (let* ([dest (make-canvas 10 10)]
                     [dest (draw-string dest (point 0 0) "BASE")]
                     [src (make-transparent-canvas 3 3)]
                     [src (canvas-set src 1 1 #\*)]
                     [result (composite-transparent dest src (point 5 5))])
                    ;; BASE should be untouched
                    (assert-equal #\B (canvas-ref result 0 0))
                    ;; * should appear at (6, 6) = (5+1, 5+1)
                    (assert-equal #\* (canvas-ref result 6 6)))))

(doc 'section 'layer-flattening-tests)

(test-group layer-flattening
            (define-test flatten-single-layer
              ;; Flatten a single layer
              (let* ([c (make-canvas 10 5)]
                     [c (draw-string c (point 0 0) "Hello")]
                     [layer (make-layer 'text c)]
                     [stack (make-layer-stack (list layer))]
                     [result (flatten-layers stack 10 5)])
                    (assert-equal #\H (canvas-ref result 0 0))
                    (assert-equal #\e (canvas-ref result 1 0))
                    (assert-equal #\l (canvas-ref result 2 0))))
            
            (define-test flatten-multiple-layers
              ;; Flatten background and foreground
              (let* ([bg (make-canvas 10 5)]
                     [bg (fill-rect bg (make-rect (point 0 0) 10 5) #\.)]
                     [fg (make-transparent-canvas 10 5)]
                     [fg (draw-string fg (point 2 2) "FG")]
                     [bg-layer (make-layer 'background bg 0)]
                     [fg-layer (make-layer 'foreground fg 10)]
                     [stack (make-layer-stack (list bg-layer fg-layer))]
                     [result (flatten-layers stack 10 5)])
                    ;; Background should show through
                    (assert-equal #\. (canvas-ref result 0 0))
                    ;; Foreground text should overlay
                    (assert-equal #\F (canvas-ref result 2 2))
                    (assert-equal #\G (canvas-ref result 3 2))))
            
            (define-test flatten-skips-invisible
              ;; Hidden layers should not appear
              (let* ([visible-canvas (make-canvas 10 5)]
                     [visible-canvas (draw-string visible-canvas (point 0 0) "VISIBLE")]
                     [hidden-canvas (make-canvas 10 5)]
                     [hidden-canvas (draw-string hidden-canvas (point 0 0) "HIDDEN")]
                     [vis-layer (make-layer 'visible visible-canvas 0)]
                     [hid-layer (make-layer% 'hidden hidden-canvas #f 10 (point 0 0))]
                     [stack (make-layer-stack (list vis-layer hid-layer))]
                     [result (flatten-layers stack 10 5)])
                    ;; Should see VISIBLE, not HIDDEN
                    (assert-equal #\V (canvas-ref result 0 0)))))

(doc 'section 'helper-constructor-tests)

(test-group helper-constructors
            (define-test make-background-layer-test
              ;; Background layer at depth 0
              (let ([layer (make-background-layer 20 10)])
                   (assert-equal 'background (layer-name layer))
                   (assert-equal 0 (layer-depth layer))
                   (assert-equal 20 (canvas-width (layer-canvas layer)))
                   (assert-equal 10 (canvas-height (layer-canvas layer)))))
            
            (define-test make-sprite-layer-test
              ;; Sprite layer at depth 50, transparent
              (let ([layer (make-sprite-layer 'player 8 8 (point 10 20))])
                   (assert-equal 'player (layer-name layer))
                   (assert-equal 50 (layer-depth layer))
                   (assert-equal 10 (point-x (layer-offset layer)))
                   (assert-equal 20 (point-y (layer-offset layer)))
                   ;; Canvas should be transparent
                   (assert-true (transparent? (canvas-ref (layer-canvas layer) 0 0)))))
            
            (define-test make-ui-layer-test
              ;; UI layer at depth 100
              (let ([layer (make-ui-layer 30 10)])
                   (assert-equal 'ui (layer-name layer))
                   (assert-equal 100 (layer-depth layer)))))

(doc 'section 'layer-drawing-operations-tests)

(test-group layer-drawing
            (define-test layer-draw-string-test
              ;; Draw string on layer
              (let* ([layer (make-layer 'text (make-canvas 15 5))]
                     [layer (layer-draw-string layer (point 2 1) "Hello")])
                    (assert-equal #\H (canvas-ref (layer-canvas layer) 2 1))
                    (assert-equal #\o (canvas-ref (layer-canvas layer) 6 1))))
            
            (define-test layer-draw-char-test
              ;; Draw single char on layer
              (let* ([layer (make-layer 'marker (make-canvas 5 5))]
                     [layer (layer-draw-char layer (point 2 2) #\X)])
                    (assert-equal #\X (canvas-ref (layer-canvas layer) 2 2))))
            
            (define-test layer-fill-rect-test
              ;; Fill rectangle on layer
              (let* ([layer (make-layer 'fill (make-canvas 10 10))]
                     [layer (layer-fill-rect layer (make-rect (point 1 1) 5 3) #\#)])
                    (assert-equal #\# (canvas-ref (layer-canvas layer) 2 2))
                    (assert-equal #\# (canvas-ref (layer-canvas layer) 4 3))))
            
            (define-test draw-sprite-to-layer-test
              ;; Draw multiline sprite
              (let* ([layer (make-sprite-layer 'duck 10 10 (point 0 0))]
                     [sprite '("  __" " (o>" " (()")]
                     [layer (draw-sprite-to-layer layer (point 1 1) sprite)])
                    ;; Check first line
                    (assert-equal #\_ (canvas-ref (layer-canvas layer) 3 1))
                    ;; Check second line
                    (assert-equal #\( (canvas-ref (layer-canvas layer) 2 2)))))

(doc 'section 'alpha-blending-tests)

(test-group alpha-blending
            (define-test blend-chars-full-transparent
              ;; Alpha 0 returns dest
              (assert-equal #\D (blend-chars #\D #\S 0.0)))
            
            (define-test blend-chars-full-opaque
              ;; Alpha 1 returns src
              (assert-equal #\S (blend-chars #\D #\S 1.0)))
            
            (define-test blend-chars-medium
              ;; Alpha 0.5 should return a blend character
              (let ([result (blend-chars #\D #\S 0.5)])
                   (assert-true (char? result))))
            
            (define-test composite-with-alpha-test
              ;; Composite with 50% alpha
              (let* ([dest (make-canvas 5 5)]
                     [dest (fill-rect dest (make-rect (point 0 0) 5 5) #\D)]
                     [src (make-canvas 5 5)]
                     [src (fill-rect src (make-rect (point 0 0) 5 5) #\S)]
                     [result (composite-with-alpha dest src (point 0 0) 0.5)])
                    ;; Result should have blended characters
                    (assert-true (char? (canvas-ref result 2 2)))))
            
            (define-test layer-with-opacity-test
              ;; Create layer-opacity pair
              (let* ([layer (make-layer 'test (make-canvas 5 5))]
                     [pair (layer-with-opacity layer 0.75)])
                    (assert-equal layer (car pair))
                    (assert-equal 0.75 (cdr pair)))))

(doc 'section 'debug-string-tests)

(test-group debug-strings
            (define-test layer-to-string-test
              ;; layer->string should produce readable output
              (let* ([layer (make-layer 'test (make-canvas 10 5) 25 (point 3 7))]
                     [str (layer->string layer)])
                    (assert-true (string? str))
                    (assert-true (string-contains? str "test"))
                    (assert-true (string-contains? str "25"))))
            
            (define-test stack-to-string-test
              ;; stack->string should list all layers
              (let* ([l1 (make-layer 'first (make-canvas 5 5) 0)]
                     [l2 (make-layer 'second (make-canvas 5 5) 10)]
                     [stack (make-layer-stack (list l1 l2))]
                     [str (stack->string stack)])
                    (assert-true (string? str))
                    (assert-true (string-contains? str "first"))
                    (assert-true (string-contains? str "second")))))

(doc 'section 'summary)

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All layers tests passed.\n")
    (display "\n[FAILURE] Some layers tests failed.\n"))
