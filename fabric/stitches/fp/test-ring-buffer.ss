;;; fabric/stitches/fp/test-ring-buffer.ss — Tests for Ring Buffer Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/ring-buffer.ss")

(display "\n")
(display "==============================================================\n")
(display "         RING BUFFER TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Basic Ring Buffer Tests
;;; ============================================================

(test-group ring-buffer-basics
  (define-test empty-buffer-test
    (let ([rb (make-ring-buffer 5)])
      (assert-true (ring-buffer? rb))
      (assert-true (rb-empty? rb))
      (assert-false (rb-full? rb))
      (assert-equal 0 (rb-size rb))
      (assert-equal 5 (rb-capacity rb))))

  (define-test push-back-test
    (let* ([rb (make-ring-buffer 5)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)]
           [rb (rb-push-back rb 3)])
      (assert-equal 3 (rb-size rb))
      (assert-equal '(1 2 3) (ring-buffer->list rb))))

  (define-test push-front-test
    (let* ([rb (make-ring-buffer 5)]
           [rb (rb-push-front rb 3)]
           [rb (rb-push-front rb 2)]
           [rb (rb-push-front rb 1)])
      (assert-equal 3 (rb-size rb))
      (assert-equal '(1 2 3) (ring-buffer->list rb))))

  (define-test full-buffer-test
    (let* ([rb (make-ring-buffer 3)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)]
           [rb (rb-push-back rb 3)])
      (assert-true (rb-full? rb))
      (assert-equal 3 (rb-size rb)))))

;;; ============================================================
;;; Overflow Tests
;;; ============================================================

(test-group overflow
  (define-test overflow-drop-oldest-test
    (let* ([rb (make-ring-buffer 3)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)]
           [rb (rb-push-back rb 3)]
           [rb (rb-push-back rb 4)])  ; Should drop 1
      (assert-equal 3 (rb-size rb))
      (assert-equal '(2 3 4) (ring-buffer->list rb))))

  (define-test safe-push-test
    (let* ([rb (make-ring-buffer 2)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)])
      (assert-true (nothing? (rb-push-back-safe rb 3)))
      (assert-equal '(1 2) (ring-buffer->list rb)))))

;;; ============================================================
;;; Pop Tests
;;; ============================================================

(test-group pop-operations
  (define-test pop-front-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [result (rb-pop-front rb)])
      (assert-equal 1 (car result))
      (assert-equal '(2 3) (ring-buffer->list (cdr result)))))

  (define-test pop-back-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [result (rb-pop-back rb)])
      (assert-equal 3 (car result))
      (assert-equal '(1 2) (ring-buffer->list (cdr result)))))

  (define-test pop-empty-test
    (let ([rb (make-ring-buffer 5)])
      (assert-false (rb-pop-front rb))
      (assert-false (rb-pop-back rb))))

  (define-test pop-all-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [r1 (rb-pop-front rb)]
           [r2 (rb-pop-front (cdr r1))]
           [r3 (rb-pop-front (cdr r2))])
      (assert-equal 1 (car r1))
      (assert-equal 2 (car r2))
      (assert-equal 3 (car r3))
      (assert-true (rb-empty? (cdr r3))))))

;;; ============================================================
;;; Peek Tests
;;; ============================================================

(test-group peek-operations
  (define-test peek-front-test
    (let ([rb (list->ring-buffer 5 '(1 2 3))])
      (assert-equal 1 (from-just (rb-peek-front rb)))))

  (define-test peek-back-test
    (let ([rb (list->ring-buffer 5 '(1 2 3))])
      (assert-equal 3 (from-just (rb-peek-back rb)))))

  (define-test peek-empty-test
    (let ([rb (make-ring-buffer 5)])
      (assert-true (nothing? (rb-peek-front rb)))
      (assert-true (nothing? (rb-peek-back rb))))))

;;; ============================================================
;;; Rotation Tests
;;; ============================================================

(test-group rotation
  (define-test rotate-left-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [rotated (rb-rotate-left rb)])
      (assert-equal '(2 3 1) (ring-buffer->list rotated))))

  (define-test rotate-right-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [rotated (rb-rotate-right rb)])
      (assert-equal '(3 1 2) (ring-buffer->list rotated))))

  (define-test rotate-left-n-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3 4))]
           [rotated (rb-rotate-left-n rb 2)])
      (assert-equal '(3 4 1 2) (ring-buffer->list rotated))))

  (define-test rotate-right-n-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3 4))]
           [rotated (rb-rotate-right-n rb 2)])
      (assert-equal '(3 4 1 2) (ring-buffer->list rotated))))

  (define-test rotate-empty-test
    (let ([rb (make-ring-buffer 5)])
      (assert-true (rb-empty? (rb-rotate-left rb)))
      (assert-true (rb-empty? (rb-rotate-right rb))))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
  (define-test list-to-rb-test
    (let ([rb (list->ring-buffer 5 '(1 2 3))])
      (assert-equal '(1 2 3) (ring-buffer->list rb))))

  (define-test list-truncation-test
    (let ([rb (list->ring-buffer 3 '(1 2 3 4 5))])
      (assert-equal 3 (rb-size rb))
      (assert-equal '(1 2 3) (ring-buffer->list rb))))

  (define-test empty-list-test
    (let ([rb (list->ring-buffer 5 '())])
      (assert-true (rb-empty? rb)))))

;;; ============================================================
;;; Index Tests
;;; ============================================================

(test-group index-operations
  (define-test rb-ref-test
    (let ([rb (list->ring-buffer 5 '(10 20 30))])
      (assert-equal 10 (from-just (rb-ref rb 0)))
      (assert-equal 20 (from-just (rb-ref rb 1)))
      (assert-equal 30 (from-just (rb-ref rb 2)))))

  (define-test rb-ref-bounds-test
    (let ([rb (list->ring-buffer 5 '(10 20 30))])
      (assert-true (nothing? (rb-ref rb -1)))
      (assert-true (nothing? (rb-ref rb 3)))))

  (define-test rb-set-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [updated (rb-set rb 1 99)])
      (assert-equal '(1 99 3) (ring-buffer->list updated)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
  (define-test rb-map-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [doubled (rb-map (lambda (x) (* x 2)) rb)])
      (assert-equal '(2 4 6) (ring-buffer->list doubled))))

  (define-test rb-filter-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3 4 5))]
           [evens (rb-filter even? rb)])
      (assert-equal '(2 4) (ring-buffer->list evens))))

  (define-test rb-fold-left-test
    (let ([rb (list->ring-buffer 5 '(1 2 3 4 5))])
      (assert-equal 15 (rb-fold-left + 0 rb))))

  (define-test rb-fold-right-test
    (let ([rb (list->ring-buffer 5 '(1 2 3))])
      (assert-equal '(1 2 3) (rb-fold-right cons '() rb)))))

;;; ============================================================
;;; Sliding Window Tests
;;; ============================================================

(test-group sliding-window
  (define-test slide-test
    (let* ([rb (list->ring-buffer 3 '(1 2 3))]
           [result (rb-slide rb 4)])  ; Should return 1 and push 4
      (assert-equal 1 (car result))
      (assert-equal '(2 3 4) (ring-buffer->list (cdr result)))))

  (define-test slide-not-full-test
    (let* ([rb (list->ring-buffer 5 '(1 2))]
           [result (rb-slide rb 3)])
      (assert-false (car result))  ; Nothing dropped
      (assert-equal '(1 2 3) (ring-buffer->list (cdr result)))))

  (define-test last-n-test
    (let ([rb (list->ring-buffer 5 '(1 2 3 4 5))])
      (assert-equal '(3 4 5) (rb-last-n rb 3))))

  (define-test first-n-test
    (let ([rb (list->ring-buffer 5 '(1 2 3 4 5))])
      (assert-equal '(1 2 3) (rb-first-n rb 3)))))

;;; ============================================================
;;; Statistics Tests
;;; ============================================================

(test-group statistics
  (define-test rb-sum-test
    (let ([rb (list->ring-buffer 5 '(1 2 3 4 5))])
      (assert-equal 15 (rb-sum rb))))

  (define-test rb-average-test
    (let ([rb (list->ring-buffer 5 '(2 4 6 8 10))])
      (assert-equal 6 (rb-average rb))))

  (define-test rb-min-test
    (let ([rb (list->ring-buffer 5 '(5 3 7 1 9))])
      (assert-equal 1 (from-just (rb-min rb)))))

  (define-test rb-max-test
    (let ([rb (list->ring-buffer 5 '(5 3 7 1 9))])
      (assert-equal 9 (from-just (rb-max rb)))))

  (define-test stats-empty-test
    (let ([rb (make-ring-buffer 5)])
      (assert-equal 0 (rb-sum rb))
      (assert-equal 0 (rb-average rb))
      (assert-true (nothing? (rb-min rb)))
      (assert-true (nothing? (rb-max rb))))))

;;; ============================================================
;;; Management Tests
;;; ============================================================

(test-group management
  (define-test rb-clear-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [cleared (rb-clear rb)])
      (assert-true (rb-empty? cleared))
      (assert-equal 5 (rb-capacity cleared))))

  (define-test rb-resize-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3))]
           [resized (rb-resize rb 10)])
      (assert-equal 10 (rb-capacity resized))
      (assert-equal '(1 2 3) (ring-buffer->list resized))))

  (define-test rb-resize-truncate-test
    (let* ([rb (list->ring-buffer 5 '(1 2 3 4 5))]
           [resized (rb-resize rb 3)])
      (assert-equal 3 (rb-capacity resized))
      (assert-equal '(1 2 3) (ring-buffer->list resized)))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparison
  (define-test rb-equal-test
    (let ([rb1 (list->ring-buffer 5 '(1 2 3))]
          [rb2 (list->ring-buffer 10 '(1 2 3))]
          [rb3 (list->ring-buffer 5 '(1 2 4))])
      (assert-true (rb-equal? rb1 rb2))  ; Same content, different capacity
      (assert-false (rb-equal? rb1 rb3))))

  (define-test rb-to-string-test
    (let ([rb (list->ring-buffer 5 '(1 2 3))])
      (assert-true (string? (rb->string rb))))))

;;; ============================================================
;;; Bulk Operation Tests
;;; ============================================================

(test-group bulk-operations
  (define-test push-all-test
    (let* ([rb (make-ring-buffer 10)]
           [rb (rb-push-all rb '(1 2 3 4 5))])
      (assert-equal '(1 2 3 4 5) (ring-buffer->list rb))))

  (define-test pop-n-test
    (let* ([rb (list->ring-buffer 10 '(1 2 3 4 5))]
           [result (rb-pop-n rb 3)])
      (assert-equal '(1 2 3) (car result))
      (assert-equal '(4 5) (ring-buffer->list (cdr result))))))

;;; ============================================================
;;; History Buffer Tests
;;; ============================================================

(test-group history-buffer
  (define-test history-basic-test
    (let* ([h (make-history-buffer 10)]
           [h (history-push h 'state1)]
           [h (history-push h 'state2)]
           [h (history-push h 'state3)])
      (assert-equal 'state3 (from-just (history-current h)))))

  (define-test history-back-test
    (let* ([h (make-history-buffer 10)]
           [h (history-push h 'state1)]
           [h (history-push h 'state2)]
           [h (history-push h 'state3)]
           [h (history-back h)])
      (assert-equal 'state2 (from-just (history-current h)))))

  (define-test history-forward-test
    (let* ([h (make-history-buffer 10)]
           [h (history-push h 'state1)]
           [h (history-push h 'state2)]
           [h (history-push h 'state3)]
           [h (history-back h)]
           [h (history-forward h)])
      (assert-equal 'state3 (from-just (history-current h)))))

  (define-test history-clear-future-test
    (let* ([h (make-history-buffer 10)]
           [h (history-push h 'state1)]
           [h (history-push h 'state2)]
           [h (history-push h 'state3)]
           [h (history-back h)]
           [h (history-push h 'new-state)])  ; Should clear future
      (assert-equal 'new-state (from-just (history-current h))))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test capacity-one-test
    (let* ([rb (make-ring-buffer 1)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)])
      (assert-equal '(2) (ring-buffer->list rb))))

  (define-test mixed-push-pop-test
    (let* ([rb (make-ring-buffer 5)]
           [rb (rb-push-back rb 1)]
           [rb (rb-push-back rb 2)]
           [result (rb-pop-front rb)]
           [rb (rb-push-back (cdr result) 3)]
           [rb (rb-push-back rb 4)])
      (assert-equal '(2 3 4) (ring-buffer->list rb)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All ring buffer tests passed.\n")
    (display "\n[FAILURE] Some ring buffer tests failed.\n"))
