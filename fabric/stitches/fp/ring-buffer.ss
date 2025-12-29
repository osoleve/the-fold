;;; fabric/stitches/fp/ring-buffer.ss — Functional Ring Buffer Library
;;;
;;; A purely functional ring buffer (circular buffer) implementation.
;;; Provides O(1) access to front and back, efficient rotation,
;;; and fixed-capacity semantics with overflow policies.
;;;
;;; Features:
;;; - O(1) push/pop from both ends
;;; - O(1) rotation left/right
;;; - Fixed capacity with overflow handling
;;; - Drop-oldest or drop-newest policies
;;; - Window/sliding operations
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Ring Buffer Type
;;; ============================================================

;;; Ring buffer represented as:
;;; (ring-buffer capacity front-list back-list size)
;;; Front is in normal order, back is reversed (for efficient push)

(define (make-ring-buffer capacity)
  (list 'ring-buffer capacity '() '() 0))

(define (ring-buffer? x)
  (and (pair? x) (eq? (car x) 'ring-buffer)))

(define (rb-capacity rb)
  (list-ref rb 1))

(define (rb-front rb)
  (list-ref rb 2))

(define (rb-back rb)
  (list-ref rb 3))

(define (rb-size rb)
  (list-ref rb 4))

(define (make-rb cap front back size)
  (list 'ring-buffer cap front back size))

;;; ============================================================
;;; Core Predicates
;;; ============================================================

(define (rb-empty? rb)
  (= (rb-size rb) 0))

(define (rb-full? rb)
  (= (rb-size rb) (rb-capacity rb)))

;;; ============================================================
;;; Internal Helpers
;;; ============================================================

;;; Rebalance: when front is empty, reverse back to front
(define (rb-rebalance rb)
  (let ([cap (rb-capacity rb)]
        [front (rb-front rb)]
        [back (rb-back rb)]
        [size (rb-size rb)])
    (if (and (null? front) (not (null? back)))
        (make-rb cap (reverse back) '() size)
        rb)))

;;; ============================================================
;;; Push Operations
;;; ============================================================

;;; Push to back (newest position) - O(1)
;;; With drop-oldest overflow policy
(define (rb-push-back rb x)
  (let* ([cap (rb-capacity rb)]
         [front (rb-front rb)]
         [back (rb-back rb)]
         [size (rb-size rb)])
    (if (< size cap)
        ;; Room available
        (make-rb cap front (cons x back) (+ size 1))
        ;; Buffer full - drop oldest (from front)
        (let ([balanced (rb-rebalance rb)])
          (let* ([new-front (cdr (rb-front balanced))]
                 [new-back (cons x (rb-back balanced))])
            (rb-rebalance (make-rb cap new-front new-back size)))))))

;;; Push to front (oldest position) - O(1)
(define (rb-push-front rb x)
  (let* ([cap (rb-capacity rb)]
         [front (rb-front rb)]
         [back (rb-back rb)]
         [size (rb-size rb)])
    (if (< size cap)
        ;; Room available
        (make-rb cap (cons x front) back (+ size 1))
        ;; Buffer full - drop newest (from back)
        (let* ([new-back (if (null? back)
                            (cdr (reverse front))
                            (cdr back))]
               [new-front (if (null? back)
                             (cons x '())
                             (cons x front))])
          (make-rb cap new-front new-back size)))))

;;; Push without overflow (returns nothing if full)
(define (rb-push-back-safe rb x)
  (if (rb-full? rb)
      nothing
      (just (rb-push-back rb x))))

;;; ============================================================
;;; Pop Operations
;;; ============================================================

;;; Pop from front (oldest element) - O(1) amortized
(define (rb-pop-front rb)
  (if (rb-empty? rb)
      #f
      (let ([balanced (rb-rebalance rb)])
        (let ([front (rb-front balanced)]
              [back (rb-back balanced)]
              [cap (rb-capacity balanced)]
              [size (rb-size balanced)])
          (cons (car front)
                (make-rb cap (cdr front) back (- size 1)))))))

;;; Pop from back (newest element) - O(1) amortized
(define (rb-pop-back rb)
  (if (rb-empty? rb)
      #f
      (let ([front (rb-front rb)]
            [back (rb-back rb)]
            [cap (rb-capacity rb)]
            [size (rb-size rb)])
        (if (null? back)
            ;; Need to get from front (reversed)
            (let ([rev-front (reverse front)])
              (cons (car rev-front)
                    (make-rb cap (reverse (cdr rev-front)) '() (- size 1))))
            ;; Pop from back directly
            (cons (car back)
                  (make-rb cap front (cdr back) (- size 1)))))))

;;; ============================================================
;;; Peek Operations
;;; ============================================================

;;; Peek front (oldest) - O(1) amortized
(define (rb-peek-front rb)
  (if (rb-empty? rb)
      nothing
      (let ([balanced (rb-rebalance rb)])
        (just (car (rb-front balanced))))))

;;; Peek back (newest) - O(1) amortized
(define (rb-peek-back rb)
  (if (rb-empty? rb)
      nothing
      (let ([back (rb-back rb)])
        (if (null? back)
            (just (car (reverse (rb-front rb))))
            (just (car back))))))

;;; ============================================================
;;; Rotation Operations
;;; ============================================================

;;; Rotate left: move front to back - O(1) amortized
(define (rb-rotate-left rb)
  (if (rb-empty? rb)
      rb
      (let ([result (rb-pop-front rb)])
        (rb-push-back (cdr result) (car result)))))

;;; Rotate right: move back to front - O(1) amortized
(define (rb-rotate-right rb)
  (if (rb-empty? rb)
      rb
      (let ([result (rb-pop-back rb)])
        (rb-push-front (cdr result) (car result)))))

;;; Rotate left n times
(define (rb-rotate-left-n rb n)
  (if (<= n 0)
      rb
      (rb-rotate-left-n (rb-rotate-left rb) (- n 1))))

;;; Rotate right n times
(define (rb-rotate-right-n rb n)
  (if (<= n 0)
      rb
      (rb-rotate-right-n (rb-rotate-right rb) (- n 1))))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert list to ring buffer (truncates if too long)
(define (list->ring-buffer cap lst)
  (let* ([truncated (take-n cap lst)]
         [len (length truncated)])
    (make-rb cap truncated '() len)))

;;; Helper: take up to n elements
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; Convert ring buffer to list (front to back order)
(define (ring-buffer->list rb)
  (let ([balanced (rb-rebalance rb)])
    (append (rb-front balanced) (reverse (rb-back balanced)))))

;;; ============================================================
;;; Index Operations
;;; ============================================================

;;; Get element at index (0 = front/oldest)
(define (rb-ref rb i)
  (let ([lst (ring-buffer->list rb)])
    (if (and (>= i 0) (< i (length lst)))
        (just (list-ref lst i))
        nothing)))

;;; Set element at index
(define (rb-set rb i x)
  (let* ([lst (ring-buffer->list rb)]
         [len (length lst)])
    (if (and (>= i 0) (< i len))
        (list->ring-buffer (rb-capacity rb)
                           (append (take-n i lst)
                                   (list x)
                                   (drop-n (+ i 1) lst)))
        rb)))

;;; Helper: drop first n elements
(define (drop-n n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-n (- n 1) (cdr lst))))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; Map function over buffer
(define (rb-map f rb)
  (list->ring-buffer (rb-capacity rb)
                     (map f (ring-buffer->list rb))))

;;; Filter elements (may reduce size)
(define (rb-filter pred rb)
  (list->ring-buffer (rb-capacity rb)
                     (filter pred (ring-buffer->list rb))))

;;; Fold left over buffer (front to back)
(define (rb-fold-left f init rb)
  (fold-left f init (ring-buffer->list rb)))

;;; Fold right over buffer
(define (rb-fold-right f init rb)
  (fold-right f init (ring-buffer->list rb)))

;;; ============================================================
;;; Sliding Window Operations
;;; ============================================================

;;; Push value and return oldest (for streaming)
(define (rb-slide rb x)
  (if (rb-full? rb)
      (let ([oldest (from-just (rb-peek-front rb))])
        (cons oldest (rb-push-back rb x)))
      (cons #f (rb-push-back rb x))))

;;; Get last n elements (most recent n)
(define (rb-last-n rb n)
  (let ([lst (ring-buffer->list rb)])
    (drop-n (max 0 (- (length lst) n)) lst)))

;;; Get first n elements (oldest n)
(define (rb-first-n rb n)
  (take-n n (ring-buffer->list rb)))

;;; ============================================================
;;; Statistics (for numeric buffers)
;;; ============================================================

;;; Sum of all elements
(define (rb-sum rb)
  (rb-fold-left + 0 rb))

;;; Average of elements
(define (rb-average rb)
  (if (rb-empty? rb)
      0
      (/ (rb-sum rb) (rb-size rb))))

;;; Min element
(define (rb-min rb)
  (if (rb-empty? rb)
      nothing
      (just (rb-fold-left min (from-just (rb-peek-front rb)) rb))))

;;; Max element
(define (rb-max rb)
  (if (rb-empty? rb)
      nothing
      (just (rb-fold-left max (from-just (rb-peek-front rb)) rb))))

;;; ============================================================
;;; Clear and Reset
;;; ============================================================

;;; Clear buffer (keep capacity)
(define (rb-clear rb)
  (make-ring-buffer (rb-capacity rb)))

;;; Resize buffer (may truncate)
(define (rb-resize rb new-cap)
  (list->ring-buffer new-cap (ring-buffer->list rb)))

;;; ============================================================
;;; Comparison
;;; ============================================================

(define (rb-equal? rb1 rb2)
  (equal? (ring-buffer->list rb1) (ring-buffer->list rb2)))

;;; ============================================================
;;; Display
;;; ============================================================

(define (rb->string rb)
  (format "RingBuffer[cap=~a, size=~a, ~a]"
          (rb-capacity rb)
          (rb-size rb)
          (ring-buffer->list rb)))

;;; ============================================================
;;; Fixed-Size Buffer Constructors
;;; ============================================================

;;; Common sizes
(define (ring-buffer-8) (make-ring-buffer 8))
(define (ring-buffer-16) (make-ring-buffer 16))
(define (ring-buffer-32) (make-ring-buffer 32))
(define (ring-buffer-64) (make-ring-buffer 64))
(define (ring-buffer-128) (make-ring-buffer 128))
(define (ring-buffer-256) (make-ring-buffer 256))

;;; ============================================================
;;; Bulk Operations
;;; ============================================================

;;; Push multiple elements
(define (rb-push-all rb lst)
  (fold-left rb-push-back rb lst))

;;; Pop n elements from front
(define (rb-pop-n rb n)
  (let loop ([rb rb] [n n] [acc '()])
    (if (or (<= n 0) (rb-empty? rb))
        (cons (reverse acc) rb)
        (let ([result (rb-pop-front rb)])
          (loop (cdr result) (- n 1) (cons (car result) acc))))))

;;; ============================================================
;;; History Buffer (specialized ring buffer)
;;; ============================================================

;;; History buffer tracks undo/redo style history
(define (make-history-buffer max-size)
  (list 'history (make-ring-buffer max-size) '()))

(define (history? x)
  (and (pair? x) (eq? (car x) 'history)))

(define (history-buffer h)
  (cadr h))

(define (history-future h)
  (caddr h))

;;; Add to history (clears future)
(define (history-push h x)
  (list 'history (rb-push-back (history-buffer h) x) '()))

;;; Go back in history
(define (history-back h)
  (let ([result (rb-pop-back (history-buffer h))])
    (if result
        (list 'history (cdr result) (cons (car result) (history-future h)))
        h)))

;;; Go forward in history
(define (history-forward h)
  (let ([future (history-future h)])
    (if (null? future)
        h
        (list 'history
              (rb-push-back (history-buffer h) (car future))
              (cdr future)))))

;;; Get current item (most recent)
(define (history-current h)
  (rb-peek-back (history-buffer h)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Types:
;;;   ring-buffer?, make-ring-buffer, rb-capacity, rb-size,
;;;   rb-empty?, rb-full?
;;;
;;; Push/pop:
;;;   rb-push-back, rb-push-front, rb-push-back-safe,
;;;   rb-pop-front, rb-pop-back
;;;
;;; Peek:
;;;   rb-peek-front, rb-peek-back
;;;
;;; Rotation:
;;;   rb-rotate-left, rb-rotate-right,
;;;   rb-rotate-left-n, rb-rotate-right-n
;;;
;;; Conversions:
;;;   list->ring-buffer, ring-buffer->list
;;;
;;; Index:
;;;   rb-ref, rb-set
;;;
;;; Transformations:
;;;   rb-map, rb-filter, rb-fold-left, rb-fold-right
;;;
;;; Sliding window:
;;;   rb-slide, rb-last-n, rb-first-n
;;;
;;; Statistics:
;;;   rb-sum, rb-average, rb-min, rb-max
;;;
;;; Management:
;;;   rb-clear, rb-resize
;;;
;;; Comparison:
;;;   rb-equal?, rb->string
;;;
;;; Bulk:
;;;   rb-push-all, rb-pop-n
;;;
;;; History buffer:
;;;   make-history-buffer, history?, history-push,
;;;   history-back, history-forward, history-current
