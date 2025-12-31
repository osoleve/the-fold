; Data structure operations - queue, stack, deque, priority queue
; All implemented using lists for simplicity

; ============================================
; Queue Operations (FIFO, list-based)
; ============================================

; queue-new: Create empty queue
(queue-new (fn () '()))

; queue-empty: Create empty queue (alias)
(queue-empty '())

; queue-empty?: Check if queue is empty
(queue-empty? null?)

; queue-enqueue: Add element to back of queue
(queue-enqueue (fn (q x) (append q (list x))))

; queue-dequeue: Remove and return front element (returns (element . rest))
(queue-dequeue (fn (q)
                   (if (null? q)
                       (cons #f '())
                       (cons (car q) (cdr q)))))

; queue-front: Peek at front element
(queue-front (fn (q) (if (null? q) #f (car q))))

; queue-size: Get queue size
(queue-size length)

; queue-to-list: Convert queue to list
(queue-to-list id)

; queue-normalize: Normalize queue (no-op for list-based queue)
(queue-normalize id)

; ============================================
; Stack Operations (LIFO, list-based)
; ============================================

; stack-new: Create empty stack
(stack-new (fn () '()))

; stack-empty: Create empty stack (alias)
(stack-empty '())

; stack-empty?: Check if stack is empty
(stack-empty? null?)

; stack-push: Push element onto stack (stack first, then element)
(stack-push (fn (s x) (cons x s)))

; stack-pop: Pop element from stack (returns (element . rest))
(stack-pop (fn (s)
               (if (null? s)
                   (cons #f '())
                   (cons (car s) (cdr s)))))

; stack-top: Peek at top element
(stack-top (fn (s) (if (null? s) #f (car s))))

; stack-size: Get stack size
(stack-size length)

; ============================================
; Deque Operations (double-ended queue)
; ============================================

; deque-new: Create empty deque
(deque-new (fn () '()))

; deque-empty?: Check if deque is empty
(deque-empty? null?)

; deque-push-front: Add to front (deque first, then element)
(deque-push-front (fn (d x) (cons x d)))

; deque-push-back: Add to back
(deque-push-back (fn (d x) (append d (list x))))

; deque-pop-front: Remove from front
(deque-pop-front (fn (d)
                     (if (null? d)
                         (cons #f '())
                         (cons (car d) (cdr d)))))

; deque-pop-back: Remove from back
(deque-pop-back (fn (d)
                    (if (null? d)
                        (cons #f '())
                        (cons (last d) (init d)))))

; deque-front: Peek at front
(deque-front (fn (d) (if (null? d) #f (car d))))

; deque-back: Peek at back
(deque-back (fn (d) (if (null? d) #f (last d))))

; deque-front-back: Get both front and back (for test compatibility)
; Note: The test actually calls deque-front and deque-back separately
; This is just a placeholder in case it's needed
(deque-front-back (fn (d)
                      (if (null? d)
                          (cons #f #f)
                          (cons (car d) (last d)))))

; ============================================
; Priority Queue (using sorted list)
; ============================================

; pq-new: Create empty priority queue
(pq-new (fn () '()))

; pq-empty?: Check if priority queue is empty
(pq-empty? null?)

; pq-insert: Insert with priority (lower = higher priority)
(pq-insert (fn (pq priority value)
               (insert-sorted-by car (list priority value) pq)))

; pq-peek: Get highest priority element
(pq-peek (fn (pq)
             (if (null? pq) #f (cadar pq))))

; pq-pop: Remove and return highest priority element
(pq-pop (fn (pq)
            (if (null? pq)
                (cons #f '())
                (cons (cadar pq) (cdr pq)))))
