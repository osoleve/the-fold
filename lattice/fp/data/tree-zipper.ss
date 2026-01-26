(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

(doc 'module 'tree-zipper)
(doc 'description "Rose Tree Type A rose tree (multi-way tree) consists of a node value and a list of children (which are themselves trees). Tree a = Node a [Tree a] tree-tag : Symbol")
(doc 'layer 'lattice)
(define tree-tag 'rose-tree)

;;; make-tree : a x (List (Tree a)) -> (Tree a)
;;; Construct a rose tree node.
(define (make-tree value children)
  (list tree-tag value children))

;;; tree? : a -> Bool
;;; Check if value is a rose tree.
(define (tree? t)
  (and (pair? t)
       (eq? (car t) tree-tag)
       (= (length t) 3)))

;;; tree-value : (Tree a) -> a
;;; Get the node value.
(define (tree-value t)
  (if (tree? t)
      (list-ref t 1)
      (error 'tree-value "not a tree")))

;;; tree-children : (Tree a) -> (List (Tree a))
;;; Get the list of children.
(define (tree-children t)
  (if (tree? t)
      (list-ref t 2)
      (error 'tree-children "not a tree")))

;;; tree-leaf : a -> (Tree a)
;;; Create a leaf node (tree with no children).
(define (tree-leaf value)
  (make-tree value '()))

;;; tree-leaf? : (Tree a) -> Bool
;;; Check if tree is a leaf (no children).
(define (tree-leaf? t)
  (and (tree? t)
       (null? (tree-children t))))

;;; tree-node : a x (Tree a) ... -> (Tree a)
;;; Variadic constructor for tree with children.
(define (tree-node value . children)
  (make-tree value children))

;;; ====
;;; Tree Utilities
;;; ====

;;; tree-size : (Tree a) -> Nat
;;; Count all nodes in tree.
(define (tree-size t)
  (if (not (tree? t))
      0
      (+ 1 (apply + (map tree-size (tree-children t))))))

;;; tree-depth : (Tree a) -> Nat
;;; Compute maximum depth of tree (leaf = 1).
(define (tree-depth t)
  (if (not (tree? t))
      0
      (if (null? (tree-children t))
          1
          (+ 1 (apply max (map tree-depth (tree-children t)))))))

;;; tree-map : (a -> b) x (Tree a) -> (Tree b)
;;; Map function over all node values.
(define (tree-map f t)
  (if (not (tree? t))
      t
      (make-tree (f (tree-value t))
                 (map (lambda (child) (tree-map f child))
                      (tree-children t)))))

;;; tree-fold : (a x [b] -> b) x (Tree a) -> b
;;; Fold tree bottom-up. f receives node value and list of child results.
(define (tree-fold f t)
  (if (not (tree? t))
      (error 'tree-fold "not a tree")
      (let ([child-results (map (lambda (c) (tree-fold f c)) (tree-children t))])
        (f (tree-value t) child-results))))

;;; tree-flatten : (Tree a) -> (List a)
;;; Flatten tree to list via pre-order traversal.
;;; Uses accumulator-passing to avoid (apply append ...) which can hit
;;; argument limits for wide trees.
(define (tree-flatten t)
  (if (not (tree? t))
      '()
      (tree-flatten-acc t '())))

;;; tree-flatten-acc : (Tree a) x (List a) -> (List a)
;;; Helper using accumulator for efficient flattening.
(define (tree-flatten-acc t acc)
  (if (not (tree? t))
      acc
      (cons (tree-value t)
            (tree-flatten-children (tree-children t) acc))))

;;; tree-flatten-children : (List (Tree a)) x (List a) -> (List a)
;;; Flatten a list of trees, right-to-left for correct order.
(define (tree-flatten-children children acc)
  (if (null? children)
      acc
      (tree-flatten-acc (car children)
                        (tree-flatten-children (cdr children) acc))))

;;; ====
;;; Tree Zipper Type
;;; ====
;;;
;;; A tree zipper consists of:
;;;   - focus: The currently focused subtree
;;;   - crumbs: Path back to root (list of parent contexts)
;;;
;;; Each crumb contains:
;;;   - parent-value: Value of parent node
;;;   - left-siblings: Trees to the left (reversed, closest first)
;;;   - right-siblings: Trees to the right
;;;
;;; Example: For tree (1 (2) (3 (4) (5)) (6)) focused on (3 (4) (5)):
;;;   focus = (3 (4) (5))
;;;   crumbs = [(1, [(2)], [(6)])]
;;;     - parent value is 1
;;;     - left sibling is (2)
;;;     - right sibling is (6)

;;; tree-zipper-tag : Symbol
(define tree-zipper-tag 'tree-zipper)

;;; crumb-tag : Symbol
(define crumb-tag 'tree-crumb)

;;; make-crumb : a x (List (Tree a)) x (List (Tree a)) -> (Crumb a)
;;; Create a parent context crumb.
(define (make-crumb parent-value left-siblings right-siblings)
  (list crumb-tag parent-value left-siblings right-siblings))

;;; crumb? : a -> Bool
(define (crumb? c)
  (and (pair? c)
       (eq? (car c) crumb-tag)
       (= (length c) 4)))

;;; crumb-value : (Crumb a) -> a
(define (crumb-value c)
  (list-ref c 1))

;;; crumb-left : (Crumb a) -> (List (Tree a))
(define (crumb-left c)
  (list-ref c 2))

;;; crumb-right : (Crumb a) -> (List (Tree a))
(define (crumb-right c)
  (list-ref c 3))

;;; make-tree-zipper : (Tree a) x (List (Crumb a)) -> (TreeZipper a)
;;; Create a tree zipper from focus and crumbs.
(define (make-tree-zipper focus crumbs)
  (list tree-zipper-tag focus crumbs))

;;; tree-zipper? : a -> Bool
;;; Check if value is a tree zipper.
(define (tree-zipper? z)
  (and (pair? z)
       (eq? (car z) tree-zipper-tag)
       (= (length z) 3)))

;;; tree-zipper-focus : (TreeZipper a) -> (Tree a)
;;; Get the focused subtree.
(define (tree-zipper-focus z)
  (if (tree-zipper? z)
      (list-ref z 1)
      (error 'tree-zipper-focus "not a tree zipper")))

;;; tree-zipper-crumbs : (TreeZipper a) -> (List (Crumb a))
;;; Get the path (list of crumbs).
(define (tree-zipper-crumbs z)
  (if (tree-zipper? z)
      (list-ref z 2)
      (error 'tree-zipper-crumbs "not a tree zipper")))

;;; ====
;;; Constructors
;;; ====

;;; tree->zipper : (Tree a) -> (TreeZipper a)
;;; Create a zipper focused at the root.
(define (tree->zipper t)
  (if (not (tree? t))
      (error 'tree->zipper "not a tree")
      (make-tree-zipper t '())))

;;; zipper->tree : (TreeZipper a) -> (Tree a)
;;; Reconstruct tree from zipper by navigating to root.
(define (zipper->tree z)
  (let ([up-result (tree-zipper-up z)])
    (if (nothing? up-result)
        (tree-zipper-focus z)
        (zipper->tree (from-just up-result)))))

;;; tree-zipper-at-root? : (TreeZipper a) -> Bool
;;; Check if zipper is at the root.
(define (tree-zipper-at-root? z)
  (null? (tree-zipper-crumbs z)))

;;; tree-zipper-at-leaf? : (TreeZipper a) -> Bool
;;; Check if zipper is at a leaf (no children).
(define (tree-zipper-at-leaf? z)
  (tree-leaf? (tree-zipper-focus z)))

;;; tree-zipper-depth : (TreeZipper a) -> Nat
;;; Get depth in tree (0 = root).
(define (tree-zipper-depth z)
  (length (tree-zipper-crumbs z)))

;;; ====
;;; Navigation
;;; ====

;;; tree-zipper-up : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move focus to parent. Returns nothing if at root.
(define (tree-zipper-up z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing
        (let* ([crumb (car crumbs)]
               [parent-value (crumb-value crumb)]
               [left (crumb-left crumb)]
               [right (crumb-right crumb)]
               [focus (tree-zipper-focus z)]
               ;; Reconstruct parent with focus in correct position
               [siblings (append (reverse left) (list focus) right)])
          (just (make-tree-zipper
                 (make-tree parent-value siblings)
                 (cdr crumbs)))))))

;;; tree-zipper-down-left : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move focus to leftmost (first) child. Returns nothing if leaf.
(define (tree-zipper-down-left z)
  (let ([focus (tree-zipper-focus z)])
    (let ([children (tree-children focus)])
      (if (null? children)
          nothing
          (let ([crumb (make-crumb (tree-value focus)
                                   '()                    ; no left siblings
                                   (cdr children))])      ; rest are right siblings
            (just (make-tree-zipper
                   (car children)
                   (cons crumb (tree-zipper-crumbs z)))))))))

;;; tree-zipper-down-right : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move focus to rightmost (last) child. Returns nothing if leaf.
(define (tree-zipper-down-right z)
  (let ([focus (tree-zipper-focus z)])
    (let ([children (tree-children focus)])
      (if (null? children)
          nothing
          (let* ([rev-children (reverse children)]
                 [crumb (make-crumb (tree-value focus)
                                    (cdr rev-children)    ; all but last, reversed
                                    '())])                ; no right siblings
            (just (make-tree-zipper
                   (car rev-children)
                   (cons crumb (tree-zipper-crumbs z)))))))))

;;; tree-zipper-down : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Alias for tree-zipper-down-left.
(define tree-zipper-down tree-zipper-down-left)

;;; tree-zipper-left : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move focus to left sibling. Returns nothing if no left sibling.
(define (tree-zipper-left z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing  ; At root, no siblings
        (let* ([crumb (car crumbs)]
               [left (crumb-left crumb)])
          (if (null? left)
              nothing  ; No left sibling
              (let ([new-crumb (make-crumb
                                (crumb-value crumb)
                                (cdr left)                              ; new left
                                (cons (tree-zipper-focus z)            ; old focus becomes right
                                      (crumb-right crumb)))])
                (just (make-tree-zipper
                       (car left)
                       (cons new-crumb (cdr crumbs))))))))))

;;; tree-zipper-right : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move focus to right sibling. Returns nothing if no right sibling.
(define (tree-zipper-right z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing  ; At root, no siblings
        (let* ([crumb (car crumbs)]
               [right (crumb-right crumb)])
          (if (null? right)
              nothing  ; No right sibling
              (let ([new-crumb (make-crumb
                                (crumb-value crumb)
                                (cons (tree-zipper-focus z)    ; old focus becomes left
                                      (crumb-left crumb))
                                (cdr right))])                 ; new right
                (just (make-tree-zipper
                       (car right)
                       (cons new-crumb (cdr crumbs))))))))))

;;; tree-zipper-leftmost : (TreeZipper a) -> (TreeZipper a)
;;; Move to leftmost sibling.
(define (tree-zipper-leftmost z)
  (let ([left-result (tree-zipper-left z)])
    (if (nothing? left-result)
        z
        (tree-zipper-leftmost (from-just left-result)))))

;;; tree-zipper-rightmost : (TreeZipper a) -> (TreeZipper a)
;;; Move to rightmost sibling.
(define (tree-zipper-rightmost z)
  (let ([right-result (tree-zipper-right z)])
    (if (nothing? right-result)
        z
        (tree-zipper-rightmost (from-just right-result)))))

;;; tree-zipper-root : (TreeZipper a) -> (TreeZipper a)
;;; Move to root.
(define (tree-zipper-root z)
  (let ([up-result (tree-zipper-up z)])
    (if (nothing? up-result)
        z
        (tree-zipper-root (from-just up-result)))))

;;; tree-zipper-nth-child : (TreeZipper a) x Nat -> (Maybe (TreeZipper a))
;;; Move to nth child (0-indexed). Returns nothing if index out of bounds.
(define (tree-zipper-nth-child z n)
  (let loop ([current (tree-zipper-down-left z)] [i 0])
    (cond
      [(nothing? current) nothing]
      [(= i n) current]
      [else (loop (tree-zipper-right (from-just current)) (+ i 1))])))

;;; ====
;;; Navigation Predicates
;;; ====

;;; tree-zipper-can-go-up? : (TreeZipper a) -> Bool
(define (tree-zipper-can-go-up? z)
  (not (null? (tree-zipper-crumbs z))))

;;; tree-zipper-can-go-down? : (TreeZipper a) -> Bool
(define (tree-zipper-can-go-down? z)
  (not (null? (tree-children (tree-zipper-focus z)))))

;;; tree-zipper-can-go-left? : (TreeZipper a) -> Bool
(define (tree-zipper-can-go-left? z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (and (not (null? crumbs))
         (not (null? (crumb-left (car crumbs)))))))

;;; tree-zipper-can-go-right? : (TreeZipper a) -> Bool
(define (tree-zipper-can-go-right? z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (and (not (null? crumbs))
         (not (null? (crumb-right (car crumbs)))))))

;;; ====
;;; Focus Operations
;;; ====

;;; tree-zipper-get : (TreeZipper a) -> a
;;; Get the value at focus.
(define (tree-zipper-get z)
  (tree-value (tree-zipper-focus z)))

;;; tree-zipper-set : (TreeZipper a) x a -> (TreeZipper a)
;;; Set the value at focus.
(define (tree-zipper-set z value)
  (let ([focus (tree-zipper-focus z)])
    (make-tree-zipper
     (make-tree value (tree-children focus))
     (tree-zipper-crumbs z))))

;;; tree-zipper-modify : (TreeZipper a) x (a -> a) -> (TreeZipper a)
;;; Modify the value at focus.
(define (tree-zipper-modify z f)
  (tree-zipper-set z (f (tree-zipper-get z))))

;;; tree-zipper-set-tree : (TreeZipper a) x (Tree a) -> (TreeZipper a)
;;; Replace the entire focused subtree.
(define (tree-zipper-set-tree z new-tree)
  (make-tree-zipper new-tree (tree-zipper-crumbs z)))

;;; tree-zipper-modify-tree : (TreeZipper a) x ((Tree a) -> (Tree a)) -> (TreeZipper a)
;;; Modify the entire focused subtree.
(define (tree-zipper-modify-tree z f)
  (tree-zipper-set-tree z (f (tree-zipper-focus z))))

;;; tree-zipper-children-count : (TreeZipper a) -> Nat
;;; Get number of children of focused node.
(define (tree-zipper-children-count z)
  (length (tree-children (tree-zipper-focus z))))

;;; ====
;;; Insertion
;;; ====

;;; tree-zipper-insert-child-left : (TreeZipper a) x (Tree a) -> (TreeZipper a)
;;; Insert a tree as the new leftmost child of focused node.
(define (tree-zipper-insert-child-left z child)
  (let ([focus (tree-zipper-focus z)])
    (make-tree-zipper
     (make-tree (tree-value focus)
                (cons child (tree-children focus)))
     (tree-zipper-crumbs z))))

;;; tree-zipper-insert-child-right : (TreeZipper a) x (Tree a) -> (TreeZipper a)
;;; Insert a tree as the new rightmost child of focused node.
(define (tree-zipper-insert-child-right z child)
  (let ([focus (tree-zipper-focus z)])
    (make-tree-zipper
     (make-tree (tree-value focus)
                (append (tree-children focus) (list child)))
     (tree-zipper-crumbs z))))

;;; tree-zipper-insert-child : (TreeZipper a) x (Tree a) -> (TreeZipper a)
;;; Alias for insert-child-left.
(define tree-zipper-insert-child tree-zipper-insert-child-left)

;;; tree-zipper-insert-left : (TreeZipper a) x (Tree a) -> (Maybe (TreeZipper a))
;;; Insert a tree as left sibling of focus. Returns nothing if at root.
(define (tree-zipper-insert-left z sibling)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing  ; Can't insert sibling at root
        (let* ([crumb (car crumbs)]
               [new-crumb (make-crumb
                           (crumb-value crumb)
                           (cons sibling (crumb-left crumb))
                           (crumb-right crumb))])
          (just (make-tree-zipper
                 (tree-zipper-focus z)
                 (cons new-crumb (cdr crumbs))))))))

;;; tree-zipper-insert-right : (TreeZipper a) x (Tree a) -> (Maybe (TreeZipper a))
;;; Insert a tree as right sibling of focus. Returns nothing if at root.
(define (tree-zipper-insert-right z sibling)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing  ; Can't insert sibling at root
        (let* ([crumb (car crumbs)]
               [new-crumb (make-crumb
                           (crumb-value crumb)
                           (crumb-left crumb)
                           (cons sibling (crumb-right crumb)))])
          (just (make-tree-zipper
                 (tree-zipper-focus z)
                 (cons new-crumb (cdr crumbs))))))))

;;; ====
;;; Deletion
;;; ====

;;; tree-zipper-delete : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Delete focused subtree. Focus moves to:
;;;   1. Right sibling if exists
;;;   2. Left sibling if exists
;;;   3. Parent (with focus as leaf) if exists
;;;   Returns nothing if at root (can't delete root).
(define (tree-zipper-delete z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing  ; Can't delete root
        (let* ([crumb (car crumbs)]
               [left (crumb-left crumb)]
               [right (crumb-right crumb)])
          (cond
            ;; Has right sibling - focus moves right
            [(not (null? right))
             (let ([new-crumb (make-crumb (crumb-value crumb) left (cdr right))])
               (just (make-tree-zipper
                      (car right)
                      (cons new-crumb (cdr crumbs)))))]
            ;; Has left sibling - focus moves left
            [(not (null? left))
             (let ([new-crumb (make-crumb (crumb-value crumb) (cdr left) right)])
               (just (make-tree-zipper
                      (car left)
                      (cons new-crumb (cdr crumbs)))))]
            ;; No siblings - move to parent (now a leaf)
            [else
             (just (make-tree-zipper
                    (tree-leaf (crumb-value crumb))
                    (cdr crumbs)))])))))

;;; tree-zipper-delete-children : (TreeZipper a) -> (TreeZipper a)
;;; Delete all children of focused node (make it a leaf).
(define (tree-zipper-delete-children z)
  (let ([focus (tree-zipper-focus z)])
    (make-tree-zipper
     (tree-leaf (tree-value focus))
     (tree-zipper-crumbs z))))

;;; ====
;;; Traversal
;;; ====

;;; tree-zipper-next-preorder : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move to next node in pre-order traversal.
(define (tree-zipper-next-preorder z)
  ;; Try going down first
  (let ([down (tree-zipper-down z)])
    (if (just? down)
        down
        ;; Try going right
        (let try-right ([current z])
          (let ([right (tree-zipper-right current)])
            (if (just? right)
                right
                ;; Go up and try right again
                (let ([up (tree-zipper-up current)])
                  (if (nothing? up)
                      nothing  ; End of traversal
                      (try-right (from-just up))))))))))

;;; tree-zipper-prev-preorder : (TreeZipper a) -> (Maybe (TreeZipper a))
;;; Move to previous node in pre-order traversal.
(define (tree-zipper-prev-preorder z)
  ;; Try going to left sibling's rightmost descendant
  (let ([left (tree-zipper-left z)])
    (if (just? left)
        (just (rightmost-descendant (from-just left)))
        ;; Otherwise, go up
        (tree-zipper-up z))))

;;; rightmost-descendant : (TreeZipper a) -> (TreeZipper a)
;;; Navigate to rightmost, deepest descendant.
(define (rightmost-descendant z)
  (let ([down (tree-zipper-down-right z)])
    (if (nothing? down)
        z
        (rightmost-descendant (from-just down)))))

;;; tree-zipper-preorder : (TreeZipper a) -> (List (TreeZipper a))
;;; Get list of all zippers in pre-order from current position.
(define (tree-zipper-preorder z)
  (let loop ([current (just z)] [acc '()])
    (if (nothing? current)
        (reverse acc)
        (let ([curr (from-just current)])
          (loop (tree-zipper-next-preorder curr) (cons curr acc))))))

;;; ====
;;; Path Operations
;;; ====

;;; tree-zipper-path : (TreeZipper a) -> (List a)
;;; Get path from root to focus (list of values).
(define (tree-zipper-path z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (append (map crumb-value (reverse crumbs))
            (list (tree-zipper-get z)))))

;;; tree-zipper-sibling-index : (TreeZipper a) -> (Maybe Nat)
;;; Get index among siblings (0-indexed). Nothing if at root.
(define (tree-zipper-sibling-index z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing
        (just (length (crumb-left (car crumbs)))))))

;;; tree-zipper-index-path : (TreeZipper a) -> (List Nat)
;;; Get path from root to focus as list of child indices.
;;; Each index represents which child to descend into at each level.
;;; Empty list means we're at root.
(define (tree-zipper-index-path z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (reverse (map (lambda (crumb)
                    (length (crumb-left crumb)))
                  crumbs))))

;;; ====
;;; Functor Instance
;;; ====

;;; tree-zipper-map : (a -> b) x (TreeZipper a) -> (TreeZipper b)
;;; Map function over all values in zipper (focus and context).
(define (tree-zipper-map f z)
  (make-tree-zipper
   (tree-map f (tree-zipper-focus z))
   (map (lambda (crumb)
          (make-crumb
           (f (crumb-value crumb))
           (map (lambda (t) (tree-map f t)) (crumb-left crumb))
           (map (lambda (t) (tree-map f t)) (crumb-right crumb))))
        (tree-zipper-crumbs z))))

;;; tree-zipper-functor : (Functor TreeZipper)
(define tree-zipper-functor
  (list 'functor tree-zipper-map))

;;; ====
;;; Comonad Instance
;;; ====
;;;
;;; TreeZipper forms a Comonad with:
;;;   - extract: Get the focused node's value
;;;   - extend: Apply contextual function to all positions
;;;
;;; The Comonad instance enables operations that depend on
;;; local context, like tree differencing or local transformations.

;;; tree-zipper-extract : (TreeZipper a) -> a
;;; Extract the focused value (Comonad extract).
(define tree-zipper-extract tree-zipper-get)

;;; tree-zipper-extend : ((TreeZipper a) -> b) x (TreeZipper a) -> (TreeZipper b)
;;; Extend a contextual function to all positions.
;;; For each position, applies f to the zipper focused at that position.
;;;
;;; Comonad law: extend extract = id
;;; This implementation satisfies the law by using index-based navigation,
;;; which correctly handles trees with duplicate values among siblings.
(define (tree-zipper-extend f z)
  ;; Capture the index path BEFORE navigating to root
  ;; This is the structural position we need to return to
  (let ([index-path (tree-zipper-index-path z)])
    ;; Navigate to root, then build the result tree
    (let ([root-z (tree-zipper-root z)])
      (let ([result-tree (extend-tree f root-z)])
        ;; Navigate to the same structural position in the result
        ;; Uses indices, not values, ensuring correctness with duplicates
        (navigate-to-indices result-tree index-path)))))

;;; extend-tree : ((TreeZipper a) -> b) x (TreeZipper a) -> (Tree b)
;;; Build a tree by applying f at each position.
(define (extend-tree f z)
  (let ([value (f z)]
        [children (tree-children (tree-zipper-focus z))])
    (make-tree
     value
     (let loop ([child-zippers (tree-zipper-down z)] [idx 0] [acc '()])
       (if (nothing? child-zippers)
           (reverse acc)
           (let ([cz (from-just child-zippers)])
             (loop (tree-zipper-right cz)
                   (+ idx 1)
                   (cons (extend-tree f cz) acc))))))))

;;; navigate-to-indices : (Tree a) x (List Nat) -> (TreeZipper a)
;;; Navigate from root following child indices.
;;; This is the correct implementation for Comonad - uses structural position,
;;; not value matching, ensuring correct behavior with duplicate values.
(define (navigate-to-indices tree indices)
  (let ([z (tree->zipper tree)])
    (if (null? indices)
        z
        (let navigate ([z z] [remaining indices])
          (if (null? remaining)
              z
              ;; Go to nth child
              (let ([child-z (tree-zipper-nth-child z (car remaining))])
                (if (nothing? child-z)
                    z  ; Index out of bounds, return current
                    (navigate (from-just child-z) (cdr remaining)))))))))

;;; tree-zipper-duplicate : (TreeZipper a) -> (TreeZipper (TreeZipper a))
;;; Duplicate: zipper of all possible focuses.
(define (tree-zipper-duplicate z)
  (tree-zipper-extend (lambda (x) x) z))

;;; tree-zipper-comonad : (Comonad TreeZipper)
(define tree-zipper-comonad
  (list 'comonad
        tree-zipper-functor
        tree-zipper-extract
        tree-zipper-extend))

;;; ====
;;; Equality and Display
;;; ====

;;; tree-equal? : (Tree a) x (Tree a) -> Bool
;;; Check equality of two trees.
(define (tree-equal? t1 t2)
  (and (tree? t1) (tree? t2)
       (equal? (tree-value t1) (tree-value t2))
       (= (length (tree-children t1)) (length (tree-children t2)))
       (andmap tree-equal? (tree-children t1) (tree-children t2))))

;;; tree-zipper-equal? : (TreeZipper a) x (TreeZipper a) -> Bool
;;; Check equality of two tree zippers.
(define (tree-zipper-equal? z1 z2)
  (and (tree-equal? (tree-zipper-focus z1) (tree-zipper-focus z2))
       (= (length (tree-zipper-crumbs z1)) (length (tree-zipper-crumbs z2)))
       (andmap (lambda (c1 c2)
                 (and (equal? (crumb-value c1) (crumb-value c2))
                      (andmap tree-equal? (crumb-left c1) (crumb-left c2))
                      (andmap tree-equal? (crumb-right c1) (crumb-right c2))))
               (tree-zipper-crumbs z1)
               (tree-zipper-crumbs z2))))

;;; tree->string : (Tree a) -> String
;;; Convert tree to string representation.
(define (tree->string t)
  (if (not (tree? t))
      "#<not-a-tree>"
      (let ([val (tree-value t)]
            [children (tree-children t)])
        (if (null? children)
            (format "~a" val)
            (format "(~a ~a)"
                    val
                    (apply string-append
                           (map (lambda (c)
                                  (string-append (tree->string c) " "))
                                children)))))))

;;; tree-zipper->string : (TreeZipper a) -> String
;;; Convert tree zipper to string showing focus.
(define (tree-zipper->string z)
  (let ([focus (tree-zipper-focus z)]
        [depth (tree-zipper-depth z)])
    (format "[depth:~a focus:~a tree:~a]"
            depth
            (tree-value focus)
            (tree->string (zipper->tree z)))))

;;; ====
;;; Helper: andmap
;;; ====

;;; andmap : (a -> Bool) x (List a) -> Bool
;;; Check if predicate holds for all elements.
;;; Also works with two lists: (a x b -> Bool) x (List a) x (List b) -> Bool
(define (andmap pred . lists)
  (if (null? (car lists))
      #t
      (if (null? (cdr lists))
          ;; Single list version
          (and (pred (car (car lists)))
               (andmap pred (cdr (car lists))))
          ;; Two list version
          (and (apply pred (map car lists))
               (apply andmap pred (map cdr lists))))))
