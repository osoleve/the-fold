;;; lattice/data/graph/graph-primitives.ss — Pure Graph Data Structures
;;; @module graph-primitives
;;; @requires prelude sort hamt

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'sort)
(require 'hamt)

(doc 'module 'graph-primitives)
(doc 'description "Pure graph data structures: visited sets, queues, stacks, hash utilities, cycle utilities.
  Composable building blocks for graph algorithms.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "DESIGN PRINCIPLES:
  - Pure functional — no I/O or store access
  - Efficient visited-set tracking to avoid cycles
  - Composable data structures for graph algorithms")

(doc 'note "Homology-based cycle analysis lives in lattice/data/graph/graph-homology.ss")
(doc 'note "Store-dependent traversal and analysis functions (BFS, DFS, pathfinding,
  connected components, centrality, etc.) live in boundary/blocks/graph-traversal.ss")

(require 'collection-utils)

(doc bytevector-hash 'type '(-> Bytevector Integer))
(doc bytevector-hash 'description "Hash function for bytevectors (FNV-1a inspired)")
(define (bytevector-hash bv)
  (let ([len (bytevector-length bv)])
       (let loop ([i 0] [h 2166136261])  ; FNV offset basis
            (if (>= i len)
                (bitwise-and h #xFFFFFFFF)  ; Keep 32-bit
                (loop (+ i 1)
                      (bitwise-and
                       (* (bitwise-xor h (bytevector-u8-ref bv i))
                          16777619)  ; FNV prime
                       #xFFFFFFFF))))))

;;; make-visited : → HAMT
;;; Create an empty visited set.
(define (make-visited)
  hamt-empty)

;;; visited-add : HAMT Hash → HAMT
;;; Add a hash to the visited set. Returns new HAMT.
(define (visited-add visited hash)
  (hamt-assoc hash #t visited))

;;; visited-contains? : HAMT Hash → Boolean
;;; Check if hash is in visited set. O(log32 n) lookup.
(define (visited-contains? visited hash)
  (hamt-lookup hash visited))

;;; visited-remove : HAMT Hash → HAMT
;;; Remove a hash from the visited set. Used for backtracking.
(define (visited-remove visited hash)
  (hamt-dissoc hash visited))

;;; --- Queue Operations (for BFS) ---
;;; Using Okasaki's two-list queue for amortized O(1) operations.
;;; Queue = (front-list . back-list)
;;; - Enqueue: cons to back
;;; - Dequeue: pop from front, reverse back if front empty

;;; make-queue : → Queue
;;; Create an empty queue.
(define (make-queue)
  (cons '() '()))

;;; queue-empty? : Queue → Boolean
;;; Check if queue is empty.
(define (queue-empty? q)
  (and (null? (car q)) (null? (cdr q))))

;;; queue-normalize : Queue → Queue
;;; Internal: ensure front is non-empty if possible.
(define (queue-normalize q)
  (if (null? (car q))
      (cons (reverse (cdr q)) '())
      q))

;;; queue-enqueue : Queue Any → Queue
;;; Add element to back of queue. O(1).
(define (queue-enqueue q item)
  (queue-normalize (cons (car q) (cons item (cdr q)))))

;;; queue-enqueue-all : Queue (List Any) → Queue
;;; Add all elements to back of queue. O(k) where k = length of items.
(define (queue-enqueue-all q items)
  (queue-normalize (cons (car q) (append (reverse items) (cdr q)))))

;;; queue-dequeue : Queue → (Values Any Queue)
;;; Remove and return front element. Amortized O(1).
(define (queue-dequeue q)
  (let ([nq (queue-normalize q)])
       (values (car (car nq))
               (queue-normalize (cons (cdr (car nq)) (cdr nq))))))

;;; --- Stack Operations (for DFS) ---

;;; make-stack : → (List Any)
;;; Create an empty stack (as a list).
(define (make-stack)
  '())

;;; stack-empty? : (List Any) → Boolean
;;; Check if stack is empty.
(define (stack-empty? s)
  (null? s))

;;; stack-push : (List Any) Any → (List Any)
;;; Push element onto stack.
(define (stack-push s item)
  (cons item s))

;;; stack-push-all : (List Any) (List Any) → (List Any)
;;; Push all elements onto stack (first in list will be on top).
(define (stack-push-all s items)
  (append items s))

;;; stack-pop : (List Any) → (Values Any (List Any))
;;; Pop and return top element.
(define (stack-pop s)
  (values (car s) (cdr s)))

;;; --- Hash Utilities ---

;;; hash-equal? : Hash Hash → Boolean
;;; Compare two hashes for equality.
(define (hash-equal? h1 h2)
  (bytevector=? h1 h2))

;;; hash-in-list? : Hash (List Hash) → Boolean
;;; Check if hash is in a list of hashes.
(define (hash-in-list? hash lst)
  (let loop ([l lst])
       (cond
        [(null? l) #f]
        [(bytevector=? (car l) hash) #t]
        [else (loop (cdr l))])))

;;; remove-hash : Hash (List Hash) → (List Hash)
;;; Remove a hash from a list.
(define (remove-hash hash lst)
  (filter (lambda (h) (not (bytevector=? h hash))) lst))

;;; unique-hashes : (List Hash) → (List Hash)
;;; Remove duplicate hashes from a list.
;;; Uses bytevector=? for hash comparison (not equal?), so cannot use prelude's unique.
(define (unique-hashes hashes)
  (let loop ([remaining hashes]
             [seen '()]
             [result '()])
       (if (null? remaining)
           (reverse result)
           (let ([h (car remaining)])
                (if (hash-in-list? h seen)
                    (loop (cdr remaining) seen result)
                    (loop (cdr remaining)
                          (cons h seen)
                          (cons h result)))))))

;;; --- List Utilities ---

;;; path-length : (List Hash) → Integer
;;; Get length of a path (number of edges).
(define (path-length path)
  (if (null? path)
      0
      (- (length path) 1)))

;;; --- Cycle Utilities ---

;;; unique-cycles : (List (List Hash)) → (List (List Hash))
;;; Remove duplicate cycles from list.
;;; Two cycles are duplicates if they contain the same nodes (set equality).
(define (unique-cycles cycles)
  (let loop ([remaining cycles]
             [result '()])
       (if (null? remaining)
           result
           (let ([cycle (car remaining)])
                (if (cycle-in-list? cycle result)
                    (loop (cdr remaining) result)
                    (loop (cdr remaining) (cons cycle result)))))))

;;; cycle-in-list? : (List Hash) (List (List Hash)) → Boolean
;;; Check if cycle is already in list (as same set of nodes).
(define (cycle-in-list? cycle cycles)
  (let ([cycle-set (cdr cycle)])  ; Remove duplicate endpoint
       (let loop ([cs cycles])
            (if (null? cs)
                #f
                (let ([other-set (cdr (car cs))])
                     (if (same-hash-set? cycle-set other-set)
                         #t
                         (loop (cdr cs))))))))

;;; same-hash-set? : (List Hash) (List Hash) → Boolean
;;; Check if two lists contain the same hashes (as sets).
;;; Uses hash table for O(N) complexity instead of O(N*M).
(define (same-hash-set? lst1 lst2)
  (and (= (length lst1) (length lst2))
       (let ([hash-set (fold-left (lambda (acc h) (hamt-assoc h #t acc)) hamt-empty lst2)])
            (let loop ([l lst1])
                 (if (null? l)
                     #t
                     (and (hamt-has-key? (car l) hash-set)
                          (loop (cdr l))))))))
