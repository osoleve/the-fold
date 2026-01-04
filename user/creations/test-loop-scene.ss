;;; test-loop-scene.ss --- Test the loop-scene DSL
;;;
;;; Quick test to verify the DSL works.

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  Testing Loop-Scene DSL\n")
(display "============================================================\n\n")

;;; Test 1: Build the gyroscope scene using the DSL
(display "Test 1: Building gyroscope scene with DSL...\n")

(define gyro-test
  (build-loop-scene
   `((duration . 2.0) (fps . 15) (width . 60) (height . 26)
     (camera . ,(look-at '(0 1.8 -6.5) '(0 0 0))))
   (ring :R 2.5 :r 0.10 :axis '(0.2 1 0.3) :cycles 2)
   (ring :R 2.0 :r 0.09 :axis '(0.5 1 0.2) :cycles 3)
   (ring :R 1.5 :r 0.08 :axis '(1 0.5 0) :cycles 5)
   (ring :R 1.0 :r 0.07 :axis '(0.3 1 0.5) :cycles 7)
   (core :r 0.22 :pulse 0.04 :cycles 4)))

(display "  ✓ Scene built successfully\n\n")

;;; Test 2: Preview a frame
(display "Test 2: Previewing frame at t=0...\n\n")
(preview-scene gyro-test)

;;; Test 3: Estimate render time
(display "\nTest 3: Estimating render time...\n")
(estimate-render-time gyro-test)

;;; Test 4: Suggest rates
(display "\nTest 4: Suggesting coprime rates...\n")
(suggest-rates 5)

;;; Test 5: Test validation (should fail)
(display "\nTest 5: Validation test (expecting error for non-integer cycles)...\n")
(guard (ex [else (display "  ✓ Correctly rejected non-integer cycles\n")])
       (ring :R 1.0 :r 0.1 :axis '(0 1 0) :cycles 2.5)
       (display "  ✗ Should have raised an error!\n"))

;;; Test 6: Test preset scene
(display "\nTest 6: Building preset scene...\n")
(define blob-test (blob-scene))
(display "  ✓ blob-scene built successfully\n")

;;; Test 7: Test icosahedron mesh
(display "\nTest 7: Building icosahedron mesh...\n")
(define ico-test (icosahedron-scene))
(display "  ✓ icosahedron-scene built successfully\n")
(display "\nPreview icosahedron at t=0:\n\n")
(preview-scene ico-test)

(display "\n")
(display "============================================================\n")
(display "  All tests passed!\n")
(display "============================================================\n\n")

(display "To render a full GIF, run:\n")
(display "  (render-loop! gyro-test \"user/creations/gyro-dsl-test.gif\")\n\n")
