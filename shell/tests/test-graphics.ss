;;; shell/tests/test-graphics.ss — Tests for graphics.ss color serialization
;;;
;;; NOTE: Run from project root: scheme --script shell/tests/test-graphics.ss

(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")
(load "shell/layout.ss")
(load "shell/ui/color.ss")
(load "shell/ui/graphics.ss")

(display "\n")
(display "====\n")
(display "         GRAPHICS COLOR SERIALIZATION TESTS\n")
(display "====\n")

;;; ====
;;; Color Serialization Tests
;;; ====

(test-group color-serialization
            (define-test serialize-default-color
              (let* ([bv (serialize-color color-default)])
                    (assert-equal 1 (bytevector-length bv))
                    (assert-equal 0 (bytevector-u8-ref bv 0))))
            
            (define-test serialize-rgb-color
              (let* ([c (make-color-rgb 255 128 0)]
                     [bv (serialize-color c)])
                    (assert-equal 4 (bytevector-length bv))
                    (assert-equal 1 (bytevector-u8-ref bv 0))  ; RGB type
                    (assert-equal 255 (bytevector-u8-ref bv 1))
                    (assert-equal 128 (bytevector-u8-ref bv 2))
                    (assert-equal 0 (bytevector-u8-ref bv 3))))
            
            (define-test serialize-palette-color
              (let* ([c (make-color-palette 42)]
                     [bv (serialize-color c)])
                    (assert-equal 2 (bytevector-length bv))
                    (assert-equal 2 (bytevector-u8-ref bv 0))  ; Palette type
                    (assert-equal 42 (bytevector-u8-ref bv 1))))
            
            (define-test deserialize-default-color
              (let* ([bv (make-bytevector 1)])
                    (bytevector-u8-set! bv 0 0)  ; Default type
                    (let ([result (deserialize-color bv 0)])
                         (assert-true (color-default? (car result)))
                         (assert-equal 1 (cdr result)))))
            
            (define-test deserialize-rgb-color
              (let* ([bv (make-bytevector 4)])
                    (bytevector-u8-set! bv 0 1)   ; RGB type
                    (bytevector-u8-set! bv 1 100)
                    (bytevector-u8-set! bv 2 150)
                    (bytevector-u8-set! bv 3 200)
                    (let ([result (deserialize-color bv 0)])
                         (assert-true (color-rgb? (car result)))
                         (assert-equal '(rgb 100 150 200) (car result))
                         (assert-equal 4 (cdr result)))))
            
            (define-test deserialize-palette-color
              (let* ([bv (make-bytevector 2)])
                    (bytevector-u8-set! bv 0 2)   ; Palette type
                    (bytevector-u8-set! bv 1 123)
                    (let ([result (deserialize-color bv 0)])
                         (assert-true (color-palette? (car result)))
                         (assert-equal '(palette 123) (car result))
                         (assert-equal 2 (cdr result)))))
            
            (define-test color-roundtrip-default
              (let* ([c color-default]
                     [bv (serialize-color c)]
                     [result (deserialize-color bv 0)])
                    (assert-true (color-default? (car result)))))
            
            (define-test color-roundtrip-rgb
              (let* ([c (make-color-rgb 50 100 150)]
                     [bv (serialize-color c)]
                     [result (deserialize-color bv 0)])
                    (assert-equal c (car result))))
            
            (define-test color-roundtrip-palette
              (let* ([c (make-color-palette 200)]
                     [bv (serialize-color c)]
                     [result (deserialize-color bv 0)])
                    (assert-equal c (car result)))))

;;; ====
;;; Colored Canvas Block Tests
;;; ====

(test-group colored-canvas-block
            (define-test colored-canvas-roundtrip-simple
              ;; Create a 2x2 canvas with colored cells
              (let* ([cells (make-vector 4 #\space)]
                     [canvas (make-canvas% 2 2 cells)]
                     [color-cells (vector
                                   (make-cell #\A color-red color-default)
                                   (make-cell #\B color-green color-default)
                                   (make-cell #\C color-blue color-default)
                                   (make-cell #\D color-default color-black))]
                     ;; Serialize to block
                     [blk (colored-canvas->block canvas color-cells)]
                     ;; Deserialize back
                     [result (block->colored-canvas blk)])
                    (assert-true (pair? result))
                    (let ([restored-canvas (car result)]
                          [restored-cells (cdr result)])
                         ;; Check dimensions
                         (assert-equal 2 (canvas-width restored-canvas))
                         (assert-equal 2 (canvas-height restored-canvas))
                         ;; Check characters
                         (assert-equal #\A (cell%-char (vector-ref restored-cells 0)))
                         (assert-equal #\B (cell%-char (vector-ref restored-cells 1)))
                         (assert-equal #\C (cell%-char (vector-ref restored-cells 2)))
                         (assert-equal #\D (cell%-char (vector-ref restored-cells 3)))
                         ;; Check colors
                         (assert-equal color-red (cell%-fg (vector-ref restored-cells 0)))
                         (assert-equal color-green (cell%-fg (vector-ref restored-cells 1)))
                         (assert-equal color-blue (cell%-fg (vector-ref restored-cells 2)))
                         (assert-equal color-default (cell%-fg (vector-ref restored-cells 3)))
                         (assert-equal color-black (cell%-bg (vector-ref restored-cells 3))))))
            
            (define-test colored-canvas-roundtrip-rgb
              ;; Test RGB colors specifically
              (let* ([cells (make-vector 1 #\space)]
                     [canvas (make-canvas% 1 1 cells)]
                     [fg (make-color-rgb 255 0 128)]
                     [bg (make-color-rgb 0 255 64)]
                     [color-cells (vector (make-cell #\X fg bg))]
                     [blk (colored-canvas->block canvas color-cells)]
                     [result (block->colored-canvas blk)])
                    (assert-true (pair? result))
                    (let ([restored-cells (cdr result)])
                         (assert-equal fg (cell%-fg (vector-ref restored-cells 0)))
                         (assert-equal bg (cell%-bg (vector-ref restored-cells 0))))))
            
            (define-test colored-canvas-block-tag
              ;; Verify the block has correct tag
              (let* ([cells (make-vector 1 #\space)]
                     [canvas (make-canvas% 1 1 cells)]
                     [color-cells (vector (make-cell #\X color-default color-default))]
                     [blk (colored-canvas->block canvas color-cells)])
                    (assert-equal 'colored-canvas (block-tag blk))))
            
            (define-test block->colored-canvas-wrong-tag
              ;; Verify wrong tag returns #f
              (let* ([blk (make-block 'wrong-tag (make-bytevector 0) empty-refs)])
                    (assert-false (block->colored-canvas blk)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All graphics tests passed.\n")
    (display "\n[FAILURE] Some graphics tests failed.\n"))
