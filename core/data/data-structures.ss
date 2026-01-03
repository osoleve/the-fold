;;; fabric/patterns/data-structures.ss — Persistent Data Structures
;;;
;;; Immutable functional data structures: Queue, Stack, Set, Dictionary.
;;; All operations return new structures without modifying originals.
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 5-6: Pure functional data structures

;;; ============================================================
;;; Stack — Last In, First Out (LIFO)
;;; ============================================================

;;; Stack is represented as a simple list.
;;; Implementation: Direct list operations (cons, car, cdr).
;;;
;;; Stack = (List α)

;;; stack-empty : Stack
;;; The empty stack.
(define stack-empty '())

;;; stack-empty? : Stack → Boolean
;;; Check if stack is empty.
(define (stack-empty? stack)
  (null? stack))

;;; stack-push : α Stack → Stack
;;; Push element onto stack. Returns new stack.
(define (stack-push elem stack)
  (cons elem stack))

;;; stack-pop : Stack → (Values Stack α)
;;; Pop top element from stack. Returns (new-stack, element).
;;; Error if stack is empty.
(define (stack-pop stack)
  (if (null? stack)
      (error 'stack-pop "Cannot pop from empty stack")
      (values (cdr stack) (car stack))))

;;; stack-peek : Stack → α
;;; Get top element without removing. Error if empty.
(define (stack-peek stack)
  (if (null? stack)
      (error 'stack-peek "Cannot peek empty stack")
      (car stack)))

;;; stack-size : Stack → Nat
;;; Get number of elements in stack.
(define (stack-size stack)
  (length stack))

;;; stack->list : Stack → (List α)
;;; Convert stack to list (top to bottom).
(define (stack->list stack)
  stack)

;;; list->stack : (List α) → Stack
;;; Convert list to stack (first element becomes top).
(define (list->stack lst)
  lst)

;;; ============================================================
;;; Queue — First In, First Out (FIFO)
;;; ============================================================

;;; Queue is represented using Okasaki's two-list implementation:
;;;   - front: Elements to dequeue (reversed)
;;;   - back: Elements to enqueue (normal order)
;;;
;;; When front is empty and we dequeue, reverse back into front.
;;; This gives amortized O(1) enqueue and dequeue.
;;;
;;; Queue α = (front (List α) × back (List α))

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

;;; queue->list : Queue → (List α)
;;; Convert queue to list (front to back order).
(define (queue->list queue)
  (append (car queue) (reverse (cdr queue))))

;;; list->queue : (List α) → Queue
;;; Convert list to queue (first element at front).
(define (list->queue lst)
  (cons lst '()))

;;; ============================================================
;;; Set — Unordered Collection (No Duplicates)
;;; ============================================================

;;; Set is represented as a list with no duplicates.
;;; Uses equal? for membership comparison.
;;;
;;; Set α = (List α)
;;;
;;; Note: This is a simple implementation. For large sets,
;;; consider balanced trees or hash-based structures.

;;; set-empty : Set
;;; The empty set.
(define set-empty '())

;;; set-empty? : Set → Boolean
;;; Check if set is empty.
(define (set-empty? set)
  (null? set))

;;; set-member? : α Set → Boolean
;;; Check if element is in set.
(define (set-member? elem set)
  (cond
   [(null? set) #f]
   [(equal? elem (car set)) #t]
   [else (set-member? elem (cdr set))]))

;;; set-add : α Set → Set
;;; Add element to set. Returns new set.
;;; If element already exists, returns original set.
(define (set-add elem set)
  (if (set-member? elem set)
      set
      (cons elem set)))

;;; set-remove : α Set → Set
;;; Remove element from set. Returns new set.
(define (set-remove elem set)
  (let loop ([remaining set]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? elem (car remaining))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; set-union : Set Set → Set
;;; Union of two sets.
(define (set-union set1 set2)
  (let loop ([remaining set1]
             [result set2])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (set-add (car remaining) result)))))

;;; set-intersection : Set Set → Set
;;; Intersection of two sets.
(define (set-intersection set1 set2)
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

;;; set-difference : Set Set → Set
;;; Elements in set1 but not in set2.
(define (set-difference set1 set2)
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) acc)]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; set-subset? : Set Set → Boolean
;;; Check if set1 is a subset of set2.
(define (set-subset? set1 set2)
  (let loop ([remaining set1])
       (cond
        [(null? remaining) #t]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining))]
        [else #f])))

;;; set-size : Set → Nat
;;; Get number of elements in set.
(define (set-size set)
  (length set))

;;; set->list : Set → (List α)
;;; Convert set to list (arbitrary order).
(define (set->list set)
  set)

;;; list->set : (List α) → Set
;;; Convert list to set (removes duplicates).
(define (list->set lst)
  (let loop ([remaining lst]
             [acc set-empty])
       (if (null? remaining)
           acc
           (loop (cdr remaining) (set-add (car remaining) acc)))))

;;; ============================================================
;;; Dictionary/Map — Key-Value Pairs
;;; ============================================================

;;; Dictionary is represented as an association list.
;;; Uses equal? for key comparison.
;;;
;;; Dict κ ν = (List (Pair κ ν))
;;;
;;; Note: This is a simple implementation. For large dictionaries,
;;; consider balanced trees or hash tables.

;;; dict-empty : Dict
;;; The empty dictionary.
(define dict-empty '())

;;; dict-empty? : Dict → Boolean
;;; Check if dictionary is empty.
(define (dict-empty? dict)
  (null? dict))

;;; dict-lookup : κ Dict → (Maybe ν)
;;; Look up value by key. Returns #f if not found.
(define (dict-lookup key dict)
  (let ([pair (assoc key dict)])
       (if pair
           (cdr pair)
           #f)))

;;; dict-has-key? : κ Dict → Boolean
;;; Check if key exists in dictionary.
(define (dict-has-key? key dict)
  (if (assoc key dict) #t #f))

;;; dict-assoc : κ ν Dict → Dict
;;; Associate key with value. Returns new dictionary.
;;; If key exists, updates value. Otherwise adds new entry.
(define (dict-assoc key value dict)
  (cons (cons key value)
        (dict-dissoc key dict)))

;;; dict-dissoc : κ Dict → Dict
;;; Remove key from dictionary. Returns new dictionary.
(define (dict-dissoc key dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? key (car (car remaining)))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; dict-keys : Dict → (List κ)
;;; Get list of all keys.
(define (dict-keys dict)
  (map car dict))

;;; dict-values : Dict → (List ν)
;;; Get list of all values.
(define (dict-values dict)
  (map cdr dict))

;;; dict-entries : Dict → (List (Pair κ ν))
;;; Get list of all key-value pairs.
(define (dict-entries dict)
  dict)

;;; dict-merge : Dict Dict → Dict
;;; Merge two dictionaries. If keys overlap, dict2 wins.
(define (dict-merge dict1 dict2)
  (let loop ([remaining dict2]
             [result dict1])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (dict-assoc (car (car remaining))
                             (cdr (car remaining))
                             result)))))

;;; dict-map-values : (ν → μ) Dict → Dict
;;; Apply function to all values, keeping keys the same.
(define (dict-map-values f dict)
  (map (lambda (pair)
               (cons (car pair) (f (cdr pair))))
       dict))

;;; dict-filter : (κ ν → Boolean) Dict → Dict
;;; Filter dictionary by predicate on key-value pairs.
(define (dict-filter pred dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(pred (car (car remaining)) (cdr (car remaining)))
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

;;; dict-size : Dict → Nat
;;; Get number of key-value pairs.
(define (dict-size dict)
  (length dict))

;;; dict->alist : Dict → (List (Pair κ ν))
;;; Convert dictionary to association list (identity).
(define (dict->alist dict)
  dict)

;;; alist->dict : (List (Pair κ ν)) → Dict
;;; Convert association list to dictionary (identity).
(define (alist->dict alist)
  alist)

(printf "✓ Data structures loaded (Stack, Queue, Set, Dict)\n")
