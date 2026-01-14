;;; lattice/data/queue.ss — FIFO Queue
;;;
;;; Immutable functional queue using Okasaki's two-list implementation.
;;; Provides amortized O(1) enqueue and dequeue.
;;;
;;; Queue α = (front (List α) × back (List α))
;;;
;;; TIER: 0 (no lattice dependencies)

;;; make-queue : (List α) × (List α) → Queue
;;; Internal constructor maintaining invariant.
(define (make-queue front back)
  (if (null? front)
      (cons (reverse back) '())
      (cons front back)))

;;; queue-empty : Queue
;;; The empty queue.
(define queue-empty (cons '() '()))

;;; queue-empty? : Queue → Boolean
;;; Check if queue is empty.
(define (queue-empty? queue)
  (and (null? (car queue))
       (null? (cdr queue))))

;;; queue-enqueue : α Queue → Queue
;;; Add element to back of queue. Returns new queue.
(define (queue-enqueue elem queue)
  (let ([front (car queue)]
        [back (cdr queue)])
       (make-queue front (cons elem back))))

;;; queue-dequeue : Queue → (Values Queue α)
;;; Remove element from front of queue. Returns (new-queue, element).
;;; Error if queue is empty.
(define (queue-dequeue queue)
  (let ([front (car queue)]
        [back (cdr queue)])
       (if (null? front)
           (error 'queue-dequeue "Cannot dequeue from empty queue")
           (values (make-queue (cdr front) back)
                   (car front)))))

;;; queue-peek : Queue → α
;;; Get front element without removing. Error if empty.
(define (queue-peek queue)
  (let ([front (car queue)])
       (if (null? front)
           (error 'queue-peek "Cannot peek empty queue")
           (car front))))

;;; queue-size : Queue → Nat
;;; Get number of elements in queue.
(define (queue-size queue)
  (+ (length (car queue))
     (length (cdr queue))))

;;; queue->list : (Queue α) → (List α)
;;; Convert queue to list (front to back order).
(define (queue->list queue)
  (append (car queue) (reverse (cdr queue))))

;;; list->queue : (List α) → (Queue α)
;;; Convert list to queue (first element at front).
(define (list->queue lst)
  (cons lst '()))
