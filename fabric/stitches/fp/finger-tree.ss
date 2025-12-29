;;; fabric/stitches/fp/finger-tree.ss — Finger Tree Library
;;;
;;; Finger trees are a functional data structure providing:
;;; - O(1) amortized access to both ends
;;; - O(log n) concatenation
;;; - O(log n) splitting based on monotonic predicates
;;;
;;; They can be used to implement:
;;; - Deques
;;; - Priority queues
;;; - Interval trees
;;; - Ropes (for text editors)
;;;
;;; This is a simplified implementation focusing on deque operations.
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Digit Type (1-4 elements)
;;; ============================================================

;;; A digit holds 1-4 elements at each end of the tree
(define (make-digit elements)
  (list 'digit elements))

(define (digit? x)
  (and (pair? x) (eq? (car x) 'digit)))

(define (digit-elements d)
  (cadr d))

(define (digit-size d)
  (length (digit-elements d)))

(define (digit-one a)
  (make-digit (list a)))

(define (digit-two a b)
  (make-digit (list a b)))

(define (digit-three a b c)
  (make-digit (list a b c)))

(define (digit-four a b c d)
  (make-digit (list a b c d)))

;;; Convert digit to list
(define (digit->list d)
  (digit-elements d))

;;; ============================================================
;;; Node Type (2-3 elements for internal nodes)
;;; ============================================================

(define (make-node2 a b)
  (list 'node2 a b))

(define (make-node3 a b c)
  (list 'node3 a b c))

(define (node? x)
  (and (pair? x) (or (eq? (car x) 'node2) (eq? (car x) 'node3))))

(define (node2? x)
  (and (pair? x) (eq? (car x) 'node2)))

(define (node3? x)
  (and (pair? x) (eq? (car x) 'node3)))

(define (node->list n)
  (cond
    [(node2? n) (list (cadr n) (caddr n))]
    [(node3? n) (list (cadr n) (caddr n) (cadddr n))]
    [else (error 'node->list "not a node")]))

;;; ============================================================
;;; Finger Tree Type
;;; ============================================================

;;; Finger tree structure:
;;; - Empty: no elements
;;; - Single: one element
;;; - Deep: left digit + inner tree + right digit

(define ft-empty '(finger-tree-empty))

(define (ft-empty? ft)
  (and (pair? ft) (eq? (car ft) 'finger-tree-empty)))

(define (make-ft-single x)
  (list 'finger-tree-single x))

(define (ft-single? ft)
  (and (pair? ft) (eq? (car ft) 'finger-tree-single)))

(define (ft-single-elem ft)
  (cadr ft))

(define (make-ft-deep left middle right)
  (list 'finger-tree-deep left middle right))

(define (ft-deep? ft)
  (and (pair? ft) (eq? (car ft) 'finger-tree-deep)))

(define (ft-left ft)
  (list-ref ft 1))

(define (ft-middle ft)
  (list-ref ft 2))

(define (ft-right ft)
  (list-ref ft 3))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Add element to left - O(1) amortized
(define (ft-cons-left ft x)
  (cond
    [(ft-empty? ft) (make-ft-single x)]
    [(ft-single? ft) (make-ft-deep (digit-one x) ft-empty (digit-one (ft-single-elem ft)))]
    [(ft-deep? ft)
     (let ([left (ft-left ft)]
           [middle (ft-middle ft)]
           [right (ft-right ft)])
       (if (= (digit-size left) 4)
           ;; Left digit full - push node into middle
           (let ([elems (digit-elements left)])
             (make-ft-deep (digit-two x (car elems))
                           (ft-cons-left middle (make-node3 (cadr elems) (caddr elems) (cadddr elems)))
                           right))
           ;; Room in left digit
           (make-ft-deep (make-digit (cons x (digit-elements left)))
                         middle
                         right)))]))

;;; Add element to right - O(1) amortized
(define (ft-snoc-right ft x)
  (cond
    [(ft-empty? ft) (make-ft-single x)]
    [(ft-single? ft) (make-ft-deep (digit-one (ft-single-elem ft)) ft-empty (digit-one x))]
    [(ft-deep? ft)
     (let ([left (ft-left ft)]
           [middle (ft-middle ft)]
           [right (ft-right ft)])
       (if (= (digit-size right) 4)
           ;; Right digit full - push node into middle
           (let ([elems (digit-elements right)])
             (make-ft-deep left
                           (ft-snoc-right middle (make-node3 (car elems) (cadr elems) (caddr elems)))
                           (digit-two (cadddr elems) x)))
           ;; Room in right digit
           (make-ft-deep left
                         middle
                         (make-digit (append (digit-elements right) (list x))))))]))

;;; Get leftmost element - O(1)
(define (ft-head-left ft)
  (cond
    [(ft-empty? ft) nothing]
    [(ft-single? ft) (just (ft-single-elem ft))]
    [(ft-deep? ft) (just (car (digit-elements (ft-left ft))))]))

;;; Get rightmost element - O(1)
(define (ft-head-right ft)
  (cond
    [(ft-empty? ft) nothing]
    [(ft-single? ft) (just (ft-single-elem ft))]
    [(ft-deep? ft)
     (let ([elems (digit-elements (ft-right ft))])
       (just (list-ref elems (- (length elems) 1))))]))

;;; Convert digit or list to finger tree
(define (digit-or-list->ft items)
  (cond
    [(null? items) ft-empty]
    [(null? (cdr items)) (make-ft-single (car items))]
    [else
     (let* ([len (length items)]
            [mid (quotient len 2)]
            [left-items (take-n mid items)]
            [right-items (drop-n mid items)])
       (make-ft-deep (make-digit left-items)
                     ft-empty
                     (make-digit right-items)))]))

;;; Helper: take first n elements
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; Helper: drop first n elements
(define (drop-n n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-n (- n 1) (cdr lst))))

;;; Remove leftmost element - O(1) amortized
(define (ft-tail-left ft)
  (cond
    [(ft-empty? ft) ft-empty]
    [(ft-single? ft) ft-empty]
    [(ft-deep? ft)
     (let ([left (ft-left ft)]
           [middle (ft-middle ft)]
           [right (ft-right ft)])
       (if (= (digit-size left) 1)
           ;; Need to borrow from middle
           (if (ft-empty? middle)
               (digit-or-list->ft (digit-elements right))
               (let ([node-result (ft-head-left middle)])
                 (if (nothing? node-result)
                     (digit-or-list->ft (digit-elements right))
                     (make-ft-deep (make-digit (node->list (from-just node-result)))
                                   (ft-tail-left middle)
                                   right))))
           ;; Just remove first from left digit
           (make-ft-deep (make-digit (cdr (digit-elements left)))
                         middle
                         right)))]))

;;; Remove rightmost element - O(1) amortized
(define (ft-tail-right ft)
  (cond
    [(ft-empty? ft) ft-empty]
    [(ft-single? ft) ft-empty]
    [(ft-deep? ft)
     (let ([left (ft-left ft)]
           [middle (ft-middle ft)]
           [right (ft-right ft)])
       (if (= (digit-size right) 1)
           ;; Need to borrow from middle
           (if (ft-empty? middle)
               (digit-or-list->ft (digit-elements left))
               (let ([node-result (ft-head-right middle)])
                 (if (nothing? node-result)
                     (digit-or-list->ft (digit-elements left))
                     (make-ft-deep left
                                   (ft-tail-right middle)
                                   (make-digit (node->list (from-just node-result)))))))
           ;; Just remove last from right digit
           (let ([elems (digit-elements right)])
             (make-ft-deep left
                           middle
                           (make-digit (take-n (- (length elems) 1) elems))))))]))

;;; ============================================================
;;; Concatenation - O(log n)
;;; ============================================================

;;; Concatenate with middle list
(define (ft-append3 ft1 middle ft2)
  (cond
    [(ft-empty? ft1) (fold-left ft-cons-left ft2 (reverse middle))]
    [(ft-empty? ft2) (fold-left ft-snoc-right ft1 middle)]
    [(ft-single? ft1) (ft-cons-left (fold-left ft-cons-left ft2 (reverse middle)) (ft-single-elem ft1))]
    [(ft-single? ft2) (ft-snoc-right (fold-left ft-snoc-right ft1 middle) (ft-single-elem ft2))]
    [(and (ft-deep? ft1) (ft-deep? ft2))
     (make-ft-deep (ft-left ft1)
                   (ft-append3 (ft-middle ft1)
                               (nodes (append (digit-elements (ft-right ft1))
                                              middle
                                              (digit-elements (ft-left ft2))))
                               (ft-middle ft2))
                   (ft-right ft2))]
    [else (error 'ft-append3 "invalid finger tree")]))

;;; Convert list to nodes (groups of 2-3)
(define (nodes lst)
  (let ([len (length lst)])
    (cond
      [(= len 2) (list (make-node2 (car lst) (cadr lst)))]
      [(= len 3) (list (make-node3 (car lst) (cadr lst) (caddr lst)))]
      [(= len 4) (list (make-node2 (car lst) (cadr lst))
                       (make-node2 (caddr lst) (cadddr lst)))]
      [else
       (cons (make-node3 (car lst) (cadr lst) (caddr lst))
             (nodes (cdddr lst)))])))

;;; Simple concatenation
(define (ft-append ft1 ft2)
  (ft-append3 ft1 '() ft2))

;;; ============================================================
;;; Queries
;;; ============================================================

;;; Check if empty
(define (ft-null? ft)
  (ft-empty? ft))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert list to finger tree - O(n)
(define (list->ft lst)
  (fold-left ft-snoc-right ft-empty lst))

;;; Convert finger tree to list - O(n)
;;; This is a polymorphic traversal that works at any depth
(define (ft->list ft)
  (ft->list-at-depth ft 0))

;;; Convert at a specific depth where depth 0 = base elements
(define (ft->list-at-depth ft depth)
  (cond
    [(ft-empty? ft) '()]
    [(ft-single? ft)
     (if (= depth 0)
         (list (ft-single-elem ft))
         (flatten-node (ft-single-elem ft) (- depth 1)))]
    [(ft-deep? ft)
     (append (flatten-digit (ft-left ft) depth)
             (ft->list-at-depth (ft-middle ft) (+ depth 1))
             (flatten-digit (ft-right ft) depth))]))

;;; Flatten a digit at given depth
(define (flatten-digit d depth)
  (if (= depth 0)
      (digit-elements d)
      (apply append (map (lambda (n) (flatten-node n (- depth 1)))
                         (digit-elements d)))))

;;; Flatten a node at given depth
(define (flatten-node n depth)
  (if (= depth 0)
      (node->list n)
      (apply append (map (lambda (x) (flatten-node x (- depth 1)))
                         (node->list n)))))

;;; Get size - O(n) via conversion
(define (ft-size ft)
  (length (ft->list ft)))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; Map over elements
(define (ft-map f ft)
  (list->ft (map f (ft->list ft))))

;;; Filter elements
(define (ft-filter pred ft)
  (list->ft (filter pred (ft->list ft))))

;;; Fold left
(define (ft-fold-left f init ft)
  (fold-left f init (ft->list ft)))

;;; Fold right
(define (ft-fold-right f init ft)
  (fold-right f init (ft->list ft)))

;;; ============================================================
;;; Deque Interface
;;; ============================================================

;;; A deque is just a finger tree with a nicer API

(define (deque-empty)
  ft-empty)

(define (deque-empty? dq)
  (ft-empty? dq))

(define (deque-cons-front dq x)
  (ft-cons-left dq x))

(define (deque-cons-back dq x)
  (ft-snoc-right dq x))

(define (deque-front dq)
  (ft-head-left dq))

(define (deque-back dq)
  (ft-head-right dq))

(define (deque-pop-front dq)
  (let ([h (ft-head-left dq)])
    (if (nothing? h)
        #f
        (cons (from-just h) (ft-tail-left dq)))))

(define (deque-pop-back dq)
  (let ([h (ft-head-right dq)])
    (if (nothing? h)
        #f
        (cons (from-just h) (ft-tail-right dq)))))

(define (deque-size dq)
  (ft-size dq))

(define (list->deque lst)
  (list->ft lst))

(define (deque->list dq)
  (ft->list dq))

;;; ============================================================
;;; Sequence Interface
;;; ============================================================

;;; Finger trees as general sequences

(define (seq-empty)
  ft-empty)

(define (seq-empty? s)
  (ft-empty? s))

(define (seq-singleton x)
  (make-ft-single x))

(define (seq-cons x s)
  (ft-cons-left s x))

(define (seq-snoc s x)
  (ft-snoc-right s x))

(define (seq-head s)
  (ft-head-left s))

(define (seq-last s)
  (ft-head-right s))

(define (seq-tail s)
  (ft-tail-left s))

(define (seq-init s)
  (ft-tail-right s))

(define (seq-append s1 s2)
  (ft-append s1 s2))

(define (seq-length s)
  (ft-size s))

(define (list->seq lst)
  (list->ft lst))

(define (seq->list s)
  (ft->list s))

;;; ============================================================
;;; Split Operations
;;; ============================================================

;;; Split sequence at position n
(define (seq-split-at n s)
  (let ([lst (ft->list s)])
    (cons (list->ft (take-n n lst))
          (list->ft (drop-n n lst)))))

;;; Take first n elements
(define (seq-take n s)
  (car (seq-split-at n s)))

;;; Drop first n elements
(define (seq-drop n s)
  (cdr (seq-split-at n s)))

;;; Get element at index (0-based)
(define (seq-ref s n)
  (let loop ([s s] [n n])
    (if (ft-empty? s)
        nothing
        (if (= n 0)
            (ft-head-left s)
            (loop (ft-tail-left s) (- n 1))))))

;;; ============================================================
;;; Reversal
;;; ============================================================

(define (ft-reverse ft)
  (list->ft (reverse (ft->list ft))))

(define (seq-reverse s)
  (ft-reverse s))

(define (deque-reverse dq)
  (ft-reverse dq))

;;; ============================================================
;;; Comparison
;;; ============================================================

(define (ft-equal? ft1 ft2)
  (equal? (ft->list ft1) (ft->list ft2)))

;;; ============================================================
;;; Show/Display
;;; ============================================================

(define (ft->string ft)
  (format "FingerTree[~a]"
          (fold-left (lambda (acc x)
                       (if (string=? acc "")
                           (format "~a" x)
                           (string-append acc ", " (format "~a" x))))
                     ""
                     (ft->list ft))))

;;; ============================================================
;;; Monoid Operations
;;; ============================================================

(define ft-mempty ft-empty)
(define ft-mappend ft-append)

(define (ft-mconcat fts)
  (fold-left ft-append ft-empty fts))

;;; ============================================================
;;; Additional Utilities
;;; ============================================================

;;; Replicate element n times
(define (seq-replicate n x)
  (if (<= n 0)
      ft-empty
      (ft-cons-left (seq-replicate (- n 1) x) x)))

;;; Build from range [start, end)
(define (seq-range start end)
  (if (>= start end)
      ft-empty
      (ft-cons-left (seq-range (+ start 1) end) start)))

;;; Intersperse element
(define (seq-intersperse sep s)
  (let ([lst (ft->list s)])
    (if (or (null? lst) (null? (cdr lst)))
        s
        (list->ft (intersperse-list sep lst)))))

(define (intersperse-list sep lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons sep (intersperse-list sep (cdr lst))))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Finger Tree Core:
;;;   ft-empty, ft-empty?, ft-cons-left, ft-snoc-right,
;;;   ft-head-left, ft-head-right, ft-tail-left, ft-tail-right,
;;;   ft-append, ft-size, ft-null?, list->ft, ft->list,
;;;   ft-map, ft-filter, ft-fold-left, ft-fold-right,
;;;   ft-reverse, ft-equal?, ft->string
;;;
;;; Deque Interface:
;;;   deque-empty, deque-empty?, deque-cons-front, deque-cons-back,
;;;   deque-front, deque-back, deque-pop-front, deque-pop-back,
;;;   deque-size, list->deque, deque->list, deque-reverse
;;;
;;; Sequence Interface:
;;;   seq-empty, seq-empty?, seq-singleton, seq-cons, seq-snoc,
;;;   seq-head, seq-last, seq-tail, seq-init, seq-append,
;;;   seq-length, list->seq, seq->list, seq-split-at, seq-take,
;;;   seq-drop, seq-ref, seq-reverse, seq-replicate, seq-range,
;;;   seq-intersperse
;;;
;;; Monoid:
;;;   ft-mempty, ft-mappend, ft-mconcat
