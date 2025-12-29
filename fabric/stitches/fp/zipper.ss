;;; fabric/stitches/fp/zipper.ss — Zipper Data Structures
;;;
;;; Zippers provide efficient navigation and local updates to immutable
;;; data structures. A zipper "focuses" on one element while remembering
;;; the context, allowing O(1) movement and updates in that neighborhood.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - List zipper (traverse lists bidirectionally)
;;;   - Binary tree zipper (navigate trees with parent context)
;;;   - Generic zipper operations
;;;   - Path recording and replay
;;;   - Zipper comonad structure
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; List Zipper
;;; ============================================================
;;;
;;; A list zipper consists of:
;;;   - focus: the current element
;;;   - left: elements to the left (reversed for O(1) movement)
;;;   - right: elements to the right
;;;
;;; Example: [1, 2, |3|, 4, 5] where |3| is focus
;;;   focus = 3
;;;   left = (2 1)  ; reversed
;;;   right = (4 5)

;;; make-list-zipper : a -> List a -> List a -> ListZipper a
(define (make-list-zipper focus left right)
  (list 'list-zipper focus left right))

;;; list-zipper? : Any -> Boolean
(define (list-zipper? x)
  (and (pair? x) (eq? (car x) 'list-zipper)))

;;; list-zipper-focus : ListZipper a -> a
(define (list-zipper-focus z)
  (list-ref z 1))

;;; list-zipper-left : ListZipper a -> List a
(define (list-zipper-left z)
  (list-ref z 2))

;;; list-zipper-right : ListZipper a -> List a
(define (list-zipper-right z)
  (list-ref z 3))

;;; list-to-zipper : List a -> Maybe (ListZipper a)
;;; Create a zipper from a list, focusing on the first element.
(define (list-to-zipper lst)
  (if (null? lst)
      nothing
      (just (make-list-zipper (car lst) '() (cdr lst)))))

;;; zipper-to-list : ListZipper a -> List a
;;; Convert a zipper back to a list.
(define (zipper-to-list z)
  (append (reverse (list-zipper-left z))
          (cons (list-zipper-focus z)
                (list-zipper-right z))))

;;; list-zipper-move-right : ListZipper a -> Maybe (ListZipper a)
;;; Move focus one position to the right.
(define (list-zipper-move-right z)
  (let ([right (list-zipper-right z)])
       (if (null? right)
           nothing
           (just (make-list-zipper
                  (car right)
                  (cons (list-zipper-focus z) (list-zipper-left z))
                  (cdr right))))))

;;; list-zipper-move-left : ListZipper a -> Maybe (ListZipper a)
;;; Move focus one position to the left.
(define (list-zipper-move-left z)
  (let ([left (list-zipper-left z)])
       (if (null? left)
           nothing
           (just (make-list-zipper
                  (car left)
                  (cdr left)
                  (cons (list-zipper-focus z) (list-zipper-right z)))))))

;;; list-zipper-set : a -> ListZipper a -> ListZipper a
;;; Replace the focused element.
(define (list-zipper-set new-focus z)
  (make-list-zipper new-focus (list-zipper-left z) (list-zipper-right z)))

;;; list-zipper-modify : (a -> a) -> ListZipper a -> ListZipper a
;;; Apply a function to the focused element.
(define (list-zipper-modify f z)
  (list-zipper-set (f (list-zipper-focus z)) z))

;;; list-zipper-insert-left : a -> ListZipper a -> ListZipper a
;;; Insert an element to the left of focus.
(define (list-zipper-insert-left x z)
  (make-list-zipper
   (list-zipper-focus z)
   (cons x (list-zipper-left z))
   (list-zipper-right z)))

;;; list-zipper-insert-right : a -> ListZipper a -> ListZipper a
;;; Insert an element to the right of focus.
(define (list-zipper-insert-right x z)
  (make-list-zipper
   (list-zipper-focus z)
   (list-zipper-left z)
   (cons x (list-zipper-right z))))

;;; list-zipper-delete-right : ListZipper a -> Maybe (ListZipper a)
;;; Delete the element to the right of focus.
(define (list-zipper-delete-right z)
  (let ([right (list-zipper-right z)])
       (if (null? right)
           nothing
           (just (make-list-zipper
                  (list-zipper-focus z)
                  (list-zipper-left z)
                  (cdr right))))))

;;; list-zipper-start : ListZipper a -> ListZipper a
;;; Move to the start of the list.
(define (list-zipper-start z)
  (let ([left (list-zipper-left z)])
       (if (null? left)
           z
           (list-zipper-start
            (from-just (list-zipper-move-left z))))))

;;; list-zipper-end : ListZipper a -> ListZipper a
;;; Move to the end of the list.
(define (list-zipper-end z)
  (let ([right (list-zipper-right z)])
       (if (null? right)
           z
           (list-zipper-end
            (from-just (list-zipper-move-right z))))))

;;; list-zipper-length : ListZipper a -> Int
;;; Get the total length of the zipper.
(define (list-zipper-length z)
  (+ (length (list-zipper-left z))
     1
     (length (list-zipper-right z))))

;;; list-zipper-position : ListZipper a -> Int
;;; Get the current position (0-indexed from left).
(define (list-zipper-position z)
  (length (list-zipper-left z)))

;;; list-zipper-move-to : Int -> ListZipper a -> Maybe (ListZipper a)
;;; Move to a specific position.
(define (list-zipper-move-to n z)
  (let* ([start-z (list-zipper-start z)]
         [pos 0])
        (list-zipper-move-to-helper n start-z)))

(define (list-zipper-move-to-helper n z)
  (cond
   [(< n 0) nothing]
   [(= n 0) (just z)]
   [else
    (let ([moved (list-zipper-move-right z)])
         (if (nothing? moved)
             nothing
             (list-zipper-move-to-helper (- n 1) (from-just moved))))]))

;;; list-zipper-find-right : (a -> Boolean) -> ListZipper a -> Maybe (ListZipper a)
;;; Find the next element to the right matching the predicate.
(define (list-zipper-find-right pred z)
  (let ([moved (list-zipper-move-right z)])
       (cond
        [(nothing? moved) nothing]
        [(pred (list-zipper-focus (from-just moved))) moved]
        [else (list-zipper-find-right pred (from-just moved))])))

;;; list-zipper-find-left : (a -> Boolean) -> ListZipper a -> Maybe (ListZipper a)
;;; Find the next element to the left matching the predicate.
(define (list-zipper-find-left pred z)
  (let ([moved (list-zipper-move-left z)])
       (cond
        [(nothing? moved) nothing]
        [(pred (list-zipper-focus (from-just moved))) moved]
        [else (list-zipper-find-left pred (from-just moved))])))

;;; ============================================================
;;; Binary Tree Zipper
;;; ============================================================
;;;
;;; A binary tree is:
;;;   ('leaf value)
;;;   ('node value left right)
;;;
;;; A tree zipper maintains a context (crumb) for each step taken:
;;;   ('left parent-value sibling-right rest-of-context)
;;;   ('right parent-value sibling-left rest-of-context)

;;; Tree constructors
(define (tree-leaf value)
  (list 'leaf value))

(define (tree-node value left right)
  (list 'node value left right))

;;; Tree predicates
(define (tree-leaf? t)
  (and (pair? t) (eq? (car t) 'leaf)))

(define (tree-node? t)
  (and (pair? t) (eq? (car t) 'node)))

;;; Tree accessors
(define (tree-value t)
  (list-ref t 1))

(define (tree-left t)
  (list-ref t 2))

(define (tree-right t)
  (list-ref t 3))

;;; Crumb constructors
(define (crumb-left parent-value sibling)
  (list 'crumb-left parent-value sibling))

(define (crumb-right parent-value sibling)
  (list 'crumb-right parent-value sibling))

;;; Crumb predicates
(define (crumb-left? c)
  (and (pair? c) (eq? (car c) 'crumb-left)))

(define (crumb-right? c)
  (and (pair? c) (eq? (car c) 'crumb-right)))

;;; Crumb accessors
(define (crumb-value c)
  (list-ref c 1))

(define (crumb-sibling c)
  (list-ref c 2))

;;; make-tree-zipper : Tree a -> List Crumb -> TreeZipper a
(define (make-tree-zipper focus crumbs)
  (list 'tree-zipper focus crumbs))

;;; tree-zipper? : Any -> Boolean
(define (tree-zipper? x)
  (and (pair? x) (eq? (car x) 'tree-zipper)))

;;; tree-zipper-focus : TreeZipper a -> Tree a
(define (tree-zipper-focus z)
  (list-ref z 1))

;;; tree-zipper-crumbs : TreeZipper a -> List Crumb
(define (tree-zipper-crumbs z)
  (list-ref z 2))

;;; tree-to-zipper : Tree a -> TreeZipper a
;;; Create a zipper focused on the root.
(define (tree-to-zipper tree)
  (make-tree-zipper tree '()))

;;; zipper-to-tree : TreeZipper a -> Tree a
;;; Reconstruct the tree from the zipper (moves to root first).
(define (zipper-to-tree z)
  (tree-zipper-focus (tree-zipper-root z)))

;;; tree-zipper-go-left : TreeZipper a -> Maybe (TreeZipper a)
;;; Move focus to the left child.
(define (tree-zipper-go-left z)
  (let ([focus (tree-zipper-focus z)]
        [crumbs (tree-zipper-crumbs z)])
       (if (tree-leaf? focus)
           nothing
           (just (make-tree-zipper
                  (tree-left focus)
                  (cons (crumb-left (tree-value focus) (tree-right focus))
                        crumbs))))))

;;; tree-zipper-go-right : TreeZipper a -> Maybe (TreeZipper a)
;;; Move focus to the right child.
(define (tree-zipper-go-right z)
  (let ([focus (tree-zipper-focus z)]
        [crumbs (tree-zipper-crumbs z)])
       (if (tree-leaf? focus)
           nothing
           (just (make-tree-zipper
                  (tree-right focus)
                  (cons (crumb-right (tree-value focus) (tree-left focus))
                        crumbs))))))

;;; tree-zipper-go-up : TreeZipper a -> Maybe (TreeZipper a)
;;; Move focus to the parent.
(define (tree-zipper-go-up z)
  (let ([focus (tree-zipper-focus z)]
        [crumbs (tree-zipper-crumbs z)])
       (if (null? crumbs)
           nothing
           (let ([crumb (car crumbs)]
                 [rest (cdr crumbs)])
                (if (crumb-left? crumb)
                    (just (make-tree-zipper
                           (tree-node (crumb-value crumb) focus (crumb-sibling crumb))
                           rest))
                    (just (make-tree-zipper
                           (tree-node (crumb-value crumb) (crumb-sibling crumb) focus)
                           rest)))))))

;;; tree-zipper-root : TreeZipper a -> TreeZipper a
;;; Move to the root of the tree.
(define (tree-zipper-root z)
  (let ([up (tree-zipper-go-up z)])
       (if (nothing? up)
           z
           (tree-zipper-root (from-just up)))))

;;; tree-zipper-set : Tree a -> TreeZipper a -> TreeZipper a
;;; Replace the focused subtree.
(define (tree-zipper-set new-focus z)
  (make-tree-zipper new-focus (tree-zipper-crumbs z)))

;;; tree-zipper-modify : (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
;;; Apply a function to the focused subtree.
(define (tree-zipper-modify f z)
  (tree-zipper-set (f (tree-zipper-focus z)) z))

;;; tree-zipper-set-value : a -> TreeZipper a -> TreeZipper a
;;; Set the value at the current focus (works for both leaf and node).
(define (tree-zipper-set-value v z)
  (let ([focus (tree-zipper-focus z)])
       (if (tree-leaf? focus)
           (tree-zipper-set (tree-leaf v) z)
           (tree-zipper-set (tree-node v (tree-left focus) (tree-right focus)) z))))

;;; tree-zipper-is-leaf? : TreeZipper a -> Boolean
(define (tree-zipper-is-leaf? z)
  (tree-leaf? (tree-zipper-focus z)))

;;; tree-zipper-is-root? : TreeZipper a -> Boolean
(define (tree-zipper-is-root? z)
  (null? (tree-zipper-crumbs z)))

;;; tree-zipper-depth : TreeZipper a -> Int
;;; Get the depth of the current focus.
(define (tree-zipper-depth z)
  (length (tree-zipper-crumbs z)))

;;; ============================================================
;;; Path-Based Navigation
;;; ============================================================
;;;
;;; Paths are lists of directions that can be recorded and replayed.

;;; Path directions for trees
(define tree-path-left 'left)
(define tree-path-right 'right)
(define tree-path-up 'up)

;;; tree-zipper-follow-path : List Direction -> TreeZipper a -> Maybe (TreeZipper a)
;;; Follow a path from the current position.
(define (tree-zipper-follow-path path z)
  (if (null? path)
      (just z)
      (let* ([dir (car path)]
             [next (case dir
                         [(left) (tree-zipper-go-left z)]
                         [(right) (tree-zipper-go-right z)]
                         [(up) (tree-zipper-go-up z)]
                         [else nothing])])
            (if (nothing? next)
                nothing
                (tree-zipper-follow-path (cdr path) (from-just next))))))

;;; tree-zipper-path-from-root : TreeZipper a -> List Direction
;;; Get the path from root to current position.
(define (tree-zipper-path-from-root z)
  (let ([crumbs (tree-zipper-crumbs z)])
       (map (lambda (crumb)
                    (if (crumb-left? crumb) 'left 'right))
            (reverse crumbs))))

;;; ============================================================
;;; Zipper as Comonad
;;; ============================================================
;;;
;;; The list zipper forms a comonad, allowing us to:
;;;   - Extract the focused element (extract)
;;;   - Duplicate the structure with zippers at each position (duplicate)
;;;   - Extend a function over all positions (extend)

;;; list-zipper-extract : ListZipper a -> a
;;; Extract the focused element (comonad extract).
(define (list-zipper-extract z)
  (list-zipper-focus z))

;;; list-zipper-duplicate : ListZipper a -> ListZipper (ListZipper a)
;;; Create a zipper of zippers, each focused at a different position.
(define (list-zipper-duplicate z)
  (let ([lefts (list-zipper-generate-lefts z)]
        [rights (list-zipper-generate-rights z)])
       (make-list-zipper z lefts rights)))

(define (list-zipper-generate-lefts z)
  (let ([moved (list-zipper-move-left z)])
       (if (nothing? moved)
           '()
           (cons (from-just moved)
                 (list-zipper-generate-lefts (from-just moved))))))

(define (list-zipper-generate-rights z)
  (let ([moved (list-zipper-move-right z)])
       (if (nothing? moved)
           '()
           (cons (from-just moved)
                 (list-zipper-generate-rights (from-just moved))))))

;;; list-zipper-extend : (ListZipper a -> b) -> ListZipper a -> ListZipper b
;;; Apply a function to each position and collect the results.
(define (list-zipper-extend f z)
  (let ([dup (list-zipper-duplicate z)])
       (list-zipper-modify-all f dup)))

(define (list-zipper-modify-all f z)
  (make-list-zipper
   (f (list-zipper-focus z))
   (map f (list-zipper-left z))
   (map f (list-zipper-right z))))

;;; ============================================================
;;; Practical Examples
;;; ============================================================

;;; local-sum : ListZipper Int -> Int
;;; Sum the focus and its immediate neighbors.
(define (local-sum z)
  (let* ([focus (list-zipper-focus z)]
         [left-val (let ([m (list-zipper-move-left z)])
                        (if (nothing? m) 0 (list-zipper-focus (from-just m))))]
         [right-val (let ([m (list-zipper-move-right z)])
                         (if (nothing? m) 0 (list-zipper-focus (from-just m))))])
        (+ left-val focus right-val)))

;;; smooth : ListZipper Number -> ListZipper Number
;;; Apply a 3-point average smoothing filter.
(define (smooth z)
  (list-zipper-extend
   (lambda (z2)
           (let* ([focus (list-zipper-focus z2)]
                  [left-val (let ([m (list-zipper-move-left z2)])
                                 (if (nothing? m) focus (list-zipper-focus (from-just m))))]
                  [right-val (let ([m (list-zipper-move-right z2)])
                                  (if (nothing? m) focus (list-zipper-focus (from-just m))))])
                 (/ (+ left-val focus right-val) 3)))
   z))

;;; tree-sum : TreeZipper Int -> Int
;;; Sum all values in the tree.
(define (tree-sum z)
  (let loop ([z (tree-zipper-root z)])
       (let ([focus (tree-zipper-focus z)])
            (if (tree-leaf? focus)
                (tree-value focus)
                (+ (tree-value focus)
                   (let ([left (tree-zipper-go-left z)])
                        (if (nothing? left) 0 (loop (from-just left))))
                   (let ([right (tree-zipper-go-right z)])
                        (if (nothing? right) 0 (loop (from-just right)))))))))

;;; tree-find : (a -> Boolean) -> TreeZipper a -> Maybe (TreeZipper a)
;;; DFS search for an element matching the predicate.
(define (tree-find pred z)
  (let ([focus (tree-zipper-focus z)])
       (cond
        [(tree-leaf? focus)
         (if (pred (tree-value focus)) (just z) nothing)]
        [else
         (if (pred (tree-value focus))
             (just z)
             (let ([left-result (let ([l (tree-zipper-go-left z)])
                                     (if (nothing? l)
                                         nothing
                                         (tree-find pred (from-just l))))])
                  (if (just? left-result)
                      left-result
                      (let ([r (tree-zipper-go-right z)])
                           (if (nothing? r)
                               nothing
                               (tree-find pred (from-just r)))))))])))

;;; tree-map-zipper : (a -> b) -> TreeZipper a -> TreeZipper b
;;; Map a function over all values in the tree.
(define (tree-map-zipper f z)
  (let ([focus (tree-zipper-focus z)])
       (if (tree-leaf? focus)
           (tree-zipper-set (tree-leaf (f (tree-value focus))) z)
           (let* ([z1 (tree-zipper-set-value (f (tree-value focus)) z)]
                  [z2 (from-just (tree-zipper-go-left z1))]
                  [z3 (tree-map-zipper f z2)]
                  [z4 (from-just (tree-zipper-go-up z3))]
                  [z5 (from-just (tree-zipper-go-right z4))]
                  [z6 (tree-map-zipper f z5)])
                 (from-just (tree-zipper-go-up z6))))))

;;; ============================================================
;;; String Zipper (specialized list zipper)
;;; ============================================================

;;; string-to-zipper : String -> Maybe (ListZipper Char)
;;; Create a zipper from a string.
(define (string-to-zipper s)
  (list-to-zipper (string->list s)))

;;; zipper-to-string : ListZipper Char -> String
;;; Convert a char zipper back to a string.
(define (zipper-to-string z)
  (list->string (zipper-to-list z)))

;;; ============================================================
;;; Utility: Focused operations
;;; ============================================================

;;; with-focus : (a -> b) -> Zipper a -> (b, Zipper a)
;;; Apply a function to the focus and return the result with the zipper.
(define (with-focus f z)
  (cons (f (list-zipper-focus z)) z))

;;; focus-satisfies? : (a -> Boolean) -> Zipper a -> Boolean
;;; Check if the focus satisfies a predicate.
(define (focus-satisfies? pred z)
  (pred (list-zipper-focus z)))
