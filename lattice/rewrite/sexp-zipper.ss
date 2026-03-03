;;; @module rewrite/sexp-zipper
;;; @requires prelude combinators tree-zipper

(require 'prelude)
(require 'combinators)
(require 'tree-zipper)

(doc 'module 'rewrite/sexp-zipper)
(doc 'purity 'total)
(doc 'description "Zipper for S-expressions")
(doc 'layer 'lattice)

(doc 'description "Provides efficient navigation and modification of S-expression trees.
Treats S-expressions as rose trees where:
  - Lists have their elements as children
  - Atoms (symbols, numbers, etc.) are leaves

This enables O(1) local modifications and efficient traversal
without rebuilding entire expression trees.

This is Lattice code: pure, depends on tree-zipper.")

(doc 'section "S-expression to Rose Tree Conversion")

(doc 'description "An S-expression is converted to a rose tree where:
  - The tree value at each node is the S-expression itself
  - For lists, children are the tree representations of elements
  - For atoms, children are empty (leaf nodes)

Example: (+ (* 2 3) 4)
  Tree value = (+ (* 2 3) 4)
  Children = [tree(+), tree((* 2 3)), tree(4)]

Note: We store the FULL expression as value, not just the head.
This enables pattern matching at each position.")

(define (sexp->tree expr)
  (doc 'type (-> Sexp (Tree Sexp)))
  (doc 'description "Convert an S-expression to rose tree format.")
  (if (pair? expr)
      (make-tree expr (map sexp->tree expr))
      (make-tree expr '())))

;;; tree->sexp : (Tree Sexp) -> Sexp
;;; Convert a rose tree back to S-expression.
;;; Reconstructs from children if present, otherwise returns value.
(define (tree->sexp t)
  (if (null? (tree-children t))
      (tree-value t)
      (map tree->sexp (tree-children t))))

;;; ====
;;; S-expression Zipper Type
;;; ====

;;; sexp-zipper-tag : Symbol
(define sexp-zipper-tag 'sexp-zipper)

;;; make-sexp-zipper : (TreeZipper Sexp) -> SexpZipper
(define (make-sexp-zipper tree-z)
  (list sexp-zipper-tag tree-z))

;;; sexp-zipper? : a -> Bool
(define (sexp-zipper? x)
  (and (pair? x)
       (eq? (car x) sexp-zipper-tag)))

;;; sexp-zipper-tree-z : SexpZipper -> (TreeZipper Sexp)
(define (sexp-zipper-tree-z sz)
  (list-ref sz 1))

;;; ====
;;; Constructors
;;; ====

;;; sexp->zipper : Sexp -> SexpZipper
;;; Create a zipper focused at the root of an S-expression.
(define (sexp->zipper expr)
  (make-sexp-zipper (tree->zipper (sexp->tree expr))))

;;; zipper->sexp : SexpZipper -> Sexp
;;; Reconstruct S-expression from zipper (navigates to root first).
(define (zipper->sexp sz)
  (tree->sexp (zipper->tree (sexp-zipper-tree-z sz))))

;;; ====
;;; Navigation Lifting
;;; ====
;;;
;;; Helper to lift tree-zipper operations into sexp-zipper.
;;; Eliminates boilerplate in navigation functions.

;;; lift-tree-nav : ((TreeZipper a) -> (Maybe (TreeZipper a))) -> (SexpZipper -> (Maybe SexpZipper))
;;; Lift a tree-zipper navigation function to work on sexp-zippers.
(define (lift-tree-nav tree-op)
  (lambda (sz)
    (let ([result (tree-op (sexp-zipper-tree-z sz))])
      (if (nothing? result)
          nothing
          (just (make-sexp-zipper (from-just result)))))))

;;; lift-tree-nav-indexed : ((TreeZipper a) x Nat -> (Maybe (TreeZipper a))) -> (SexpZipper x Nat -> (Maybe SexpZipper))
;;; Lift an indexed tree-zipper navigation function.
(define (lift-tree-nav-indexed tree-op)
  (lambda (sz n)
    (let ([result (tree-op (sexp-zipper-tree-z sz) n)])
      (if (nothing? result)
          nothing
          (just (make-sexp-zipper (from-just result)))))))

;;; ====
;;; Focus Operations
;;; ====

;;; sexp-zipper-get : SexpZipper -> Sexp
;;; Get the S-expression at focus.
(define (sexp-zipper-get sz)
  (tree-value (tree-zipper-focus (sexp-zipper-tree-z sz))))

;;; sexp-zipper-set : SexpZipper x Sexp -> SexpZipper
;;; Replace the S-expression at focus.
;;; Rebuilds children if the new value is a list.
(define (sexp-zipper-set sz new-expr)
  (let* ([tz (sexp-zipper-tree-z sz)]
         [new-tree (sexp->tree new-expr)]
         [new-tz (tree-zipper-set-tree tz new-tree)])
    (make-sexp-zipper new-tz)))

;;; sexp-zipper-modify : SexpZipper x (Sexp -> Sexp) -> SexpZipper
;;; Modify the S-expression at focus.
(define (sexp-zipper-modify sz f)
  (sexp-zipper-set sz (f (sexp-zipper-get sz))))

;;; ====
;;; Navigation
;;; ====
;;;
;;; Navigation functions lifted from tree-zipper operations.
;;; Uses lift-tree-nav to eliminate boilerplate.

;;; sexp-zipper-up : SexpZipper -> (Maybe SexpZipper)
;;; Move focus to parent.
(define sexp-zipper-up (lift-tree-nav tree-zipper-up))

;;; sexp-zipper-down : SexpZipper -> (Maybe SexpZipper)
;;; Move focus to first child (first element if focus is a list).
(define sexp-zipper-down (lift-tree-nav tree-zipper-down))

;;; sexp-zipper-left : SexpZipper -> (Maybe SexpZipper)
;;; Move focus to left sibling.
(define sexp-zipper-left (lift-tree-nav tree-zipper-left))

;;; sexp-zipper-right : SexpZipper -> (Maybe SexpZipper)
;;; Move focus to right sibling.
(define sexp-zipper-right (lift-tree-nav tree-zipper-right))

;;; sexp-zipper-root : SexpZipper -> SexpZipper
;;; Move focus to root.
(define (sexp-zipper-root sz)
  (make-sexp-zipper (tree-zipper-root (sexp-zipper-tree-z sz))))

;;; sexp-zipper-nth : SexpZipper x Nat -> (Maybe SexpZipper)
;;; Move to nth child (0-indexed).
(define sexp-zipper-nth (lift-tree-nav-indexed tree-zipper-nth-child))

;;; ====
;;; Navigation Predicates
;;; ====

;;; sexp-zipper-at-root? : SexpZipper -> Bool
(define (sexp-zipper-at-root? sz)
  (tree-zipper-at-root? (sexp-zipper-tree-z sz)))

;;; sexp-zipper-at-leaf? : SexpZipper -> Bool
;;; True if focus is an atom (no children).
(define (sexp-zipper-at-leaf? sz)
  (tree-zipper-at-leaf? (sexp-zipper-tree-z sz)))

;;; sexp-zipper-can-go-up? : SexpZipper -> Bool
(define (sexp-zipper-can-go-up? sz)
  (tree-zipper-can-go-up? (sexp-zipper-tree-z sz)))

;;; sexp-zipper-can-go-down? : SexpZipper -> Bool
(define (sexp-zipper-can-go-down? sz)
  (tree-zipper-can-go-down? (sexp-zipper-tree-z sz)))

;;; ====
;;; Position-Based Navigation
;;; ====
;;;
;;; Positions are lists of indices like '(1 2 0) meaning:
;;;   "go to child 1, then child 2, then child 0"
;;; This matches the existing engine.ss API.

;;; sexp-zipper-at : SexpZipper x Position -> (Maybe SexpZipper)
;;; Navigate to a position from current focus.
(define (sexp-zipper-at sz pos)
  (if (null? pos)
      (just sz)
      (let ([child (sexp-zipper-nth sz (car pos))])
        (if (nothing? child)
            nothing
            (sexp-zipper-at (from-just child) (cdr pos))))))

;;; sexp-zipper-goto : SexpZipper x Position -> (Maybe SexpZipper)
;;; Navigate to a position from root.
(define (sexp-zipper-goto sz pos)
  (sexp-zipper-at (sexp-zipper-root sz) pos))

;;; sexp-zipper-position : SexpZipper -> Position
;;; Get the current position as a list of indices.
(define (sexp-zipper-position sz)
  (tree-zipper-index-path (sexp-zipper-tree-z sz)))

;;; ====
;;; Expression Operations (Engine Compatibility)
;;; ====
;;;
;;; These functions provide the same API as engine.ss but use
;;; zippers internally for efficiency.

;;; sexp-at : Sexp x Position -> Sexp
;;; Get the subexpression at a position.
;;; Drop-in replacement for expr-at.
(define (sexp-at expr pos)
  (let* ([sz (sexp->zipper expr)]
         [result (sexp-zipper-goto sz pos)])
    (if (nothing? result)
        (error 'sexp-at "Invalid position")
        (sexp-zipper-get (from-just result)))))

;;; sexp-set-at : Sexp x Position x Sexp -> Sexp
;;; Replace the subexpression at a position.
;;; Drop-in replacement for expr-set-at.
(define (sexp-set-at expr pos new-subexpr)
  (let* ([sz (sexp->zipper expr)]
         [at-pos (sexp-zipper-goto sz pos)])
    (if (nothing? at-pos)
        (error 'sexp-set-at "Invalid position")
        (zipper->sexp (sexp-zipper-set (from-just at-pos) new-subexpr)))))

;;; ====
;;; Traversal
;;; ====

;;; sexp-zipper-next : SexpZipper -> (Maybe SexpZipper)
;;; Move to next position in preorder traversal.
(define sexp-zipper-next (lift-tree-nav tree-zipper-next-preorder))

;;; sexp-zipper-prev : SexpZipper -> (Maybe SexpZipper)
;;; Move to previous position in preorder traversal.
(define sexp-zipper-prev (lift-tree-nav tree-zipper-prev-preorder))

;;; sexp-zipper-preorder : SexpZipper -> (List SexpZipper)
;;; Get all positions in preorder.
(define (sexp-zipper-preorder sz)
  (map make-sexp-zipper
       (tree-zipper-preorder (sexp-zipper-tree-z (sexp-zipper-root sz)))))

;;; sexp-all-positions : Sexp -> (List Position)
;;; Get all positions in an S-expression.
;;; Drop-in replacement for find-all-positions.
(define (sexp-all-positions expr)
  (let ([sz (sexp->zipper expr)])
    (map sexp-zipper-position (sexp-zipper-preorder sz))))

;;; ====
;;; Depth Information
;;; ====

;;; sexp-zipper-depth : SexpZipper -> Nat
;;; Get depth in tree (0 = root).
(define (sexp-zipper-depth sz)
  (tree-zipper-depth (sexp-zipper-tree-z sz)))

;;; ====
;;; Zipper-Based Strategy Support
;;; ====
;;;
;;; These functions support building strategies that work
;;; directly with zippers instead of rebuilding trees.

;;; sexp-zipper-try-transform : SexpZipper x (Sexp -> Sexp | #f) -> SexpZipper | #f
;;; Try to transform the focused expression.
;;; Returns updated zipper on success, #f on failure.
(define (sexp-zipper-try-transform sz f)
  (let ([result (f (sexp-zipper-get sz))])
    (if result
        (sexp-zipper-set sz result)
        #f)))

;;; sexp-zipper-find-match : SexpZipper x (Sexp -> Bool) -> (Maybe SexpZipper)
;;; Find first position (preorder) where predicate is true.
(define (sexp-zipper-find-match sz pred)
  (let loop ([current (just (sexp-zipper-root sz))])
    (cond
      [(nothing? current) nothing]
      [(pred (sexp-zipper-get (from-just current)))
       current]
      [else
       (loop (sexp-zipper-next (from-just current)))])))

;;; sexp-zipper-collect-matches : SexpZipper x (Sexp -> Bool) -> (List SexpZipper)
;;; Collect all positions where predicate is true.
(define (sexp-zipper-collect-matches sz pred)
  (let loop ([current (just (sexp-zipper-root sz))] [acc '()])
    (cond
      [(nothing? current)
       (reverse acc)]
      [(pred (sexp-zipper-get (from-just current)))
       (loop (sexp-zipper-next (from-just current))
             (cons (from-just current) acc))]
      [else
       (loop (sexp-zipper-next (from-just current)) acc)])))
