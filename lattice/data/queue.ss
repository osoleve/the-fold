;;; lattice/data/queue.ss — FIFO Queue
;;; @module queue
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'queue)
(doc 'description "FIFO Queue")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)
(doc 'note "Immutable functional queue using Okasaki's two-list implementation")
(doc 'complexity "Amortized O(1) enqueue and dequeue")

(define (make-queue front back)
  (doc 'type (-> (List α) (List α) Queue))
  (doc 'description "Internal constructor maintaining invariant")
  (if (null? front)
      (cons (reverse back) '())
      (cons front back)))

(define queue-empty (cons '() '()))
(doc queue-empty 'type 'Queue)
(doc queue-empty 'description "The empty queue")

(define (queue-empty? queue)
  (doc 'type (-> Queue Boolean))
  (doc 'description "Check if queue is empty")
  (and (null? (car queue))
       (null? (cdr queue))))

(define (queue-enqueue elem queue)
  (doc 'type (-> α Queue Queue))
  (doc 'description "Add element to back of queue")
  (let ([front (car queue)]
        [back (cdr queue)])
       (make-queue front (cons elem back))))

(define (queue-dequeue queue)
  (doc 'type (-> Queue (Values Queue α)))
  (doc 'description "Remove element from front of queue")
  (let ([front (car queue)]
        [back (cdr queue)])
       (if (null? front)
           (error 'queue-dequeue "Cannot dequeue from empty queue")
           (values (make-queue (cdr front) back)
                   (car front)))))

(define (queue-peek queue)
  (doc 'type (-> Queue α))
  (doc 'description "Get front element without removing")
  (let ([front (car queue)])
       (if (null? front)
           (error 'queue-peek "Cannot peek empty queue")
           (car front))))

(define (queue-size queue)
  (doc 'type (-> Queue Nat))
  (doc 'description "Get number of elements in queue")
  (+ (length (car queue))
     (length (cdr queue))))

(define (queue->list queue)
  (doc 'type (-> Queue (List α)))
  (doc 'description "Convert queue to list (front to back order)")
  (append (car queue) (reverse (cdr queue))))

(define (list->queue lst)
  (doc 'type (-> (List α) Queue))
  (doc 'description "Convert list to queue (first element at front)")
  (cons lst '()))
