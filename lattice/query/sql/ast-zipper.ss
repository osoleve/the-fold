(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude combinators tree-zipper zipper-lens
(require 'prelude)
(require 'combinators)
(require 'tree-zipper)
(require 'zipper-lens)

(doc 'module 'ast-zipper)
(doc 'description "Zipper for SQL AST Navigation")
(doc 'note "Provides efficient navigation and transformation of SQL AST trees. Key benefits over visitor pattern: immutable traversal and collection (no set!), edit-while-traversing patterns, current position tracking, context awareness (path to root)")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; AST to Rose Tree Conversion
;;; ====
;;;
;;; SQL AST nodes are converted to rose trees where:
;;;   - Value = the full AST node
;;;   - Children = AST nodes found in the data fields

;;; Forward declaration for sql-ast predicates
;;; (we avoid loading types.ss to prevent circular deps)

;;; sql-ast? : Any -> Bool
(define (sql-ast? x)
  (and (pair? x) (eq? (car x) 'sql-ast)))

;;; sql-tag : AST -> Symbol
(define (sql-tag node)
  (and (sql-ast? node) (list-ref node 1)))

;;; sql-span : AST -> Span
(define (sql-span node)
  (and (sql-ast? node) (list-ref node 2)))

;;; sql-data : AST -> List
(define (sql-data node)
  (and (sql-ast? node) (list-ref node 3)))

;;; extract-ast-children : AST -> (List AST)
;;; Extract all direct AST children from node's data.
(define (extract-ast-children node)
  (if (not (sql-ast? node))
      '()
      (let ([data (sql-data node)])
        (flatten-ast-children data))))

;;; flatten-ast-children : Any -> (List AST)
;;; Recursively find AST nodes in data structure.
(define (flatten-ast-children data)
  (cond
    [(sql-ast? data) (list data)]
    [(pair? data)
     (append (flatten-ast-children (car data))
             (flatten-ast-children (cdr data)))]
    [else '()]))

;;; ast->tree : AST -> (Tree AST)
;;; Convert SQL AST to rose tree for zipper navigation.
(define (ast->tree node)
  (if (not (sql-ast? node))
      (make-tree node '())
      (let ([children (extract-ast-children node)])
        (make-tree node (map ast->tree children)))))

;;; tree->ast : (Tree AST) -> AST
;;; Convert rose tree back to AST.
;;; Note: The tree value IS the AST node, so just return it.
;;; Children are already embedded in the node's data.
(define (tree->ast t)
  (tree-value t))

;;; ====
;;; AST Zipper Type
;;; ====

;;; ast-zipper-tag : Symbol
(define ast-zipper-tag 'ast-zipper)

;;; make-ast-zipper : (TreeZipper AST) -> AstZipper
(define (make-ast-zipper tree-z)
  (list ast-zipper-tag tree-z))

;;; ast-zipper? : a -> Bool
(define (ast-zipper? x)
  (and (pair? x)
       (eq? (car x) ast-zipper-tag)))

;;; ast-zipper-tree-z : AstZipper -> (TreeZipper AST)
(define (ast-zipper-tree-z az)
  (list-ref az 1))

;;; ====
;;; Navigation Lifting
;;; ====
;;;
;;; Helper to lift tree-zipper operations into ast-zipper.
;;; Eliminates boilerplate in navigation functions.

;;; lift-ast-nav : ((TreeZipper a) -> (Maybe (TreeZipper a))) -> (AstZipper -> (Maybe AstZipper))
;;; Lift a tree-zipper navigation function to work on ast-zippers.
(define (lift-ast-nav tree-op)
  (lambda (az)
    (let ([result (tree-op (ast-zipper-tree-z az))])
      (if (nothing? result)
          nothing
          (just (make-ast-zipper (from-just result)))))))

;;; lift-ast-nav-indexed : ((TreeZipper a) x Nat -> (Maybe (TreeZipper a))) -> (AstZipper x Nat -> (Maybe AstZipper))
;;; Lift an indexed tree-zipper navigation function.
(define (lift-ast-nav-indexed tree-op)
  (lambda (az n)
    (let ([result (tree-op (ast-zipper-tree-z az) n)])
      (if (nothing? result)
          nothing
          (just (make-ast-zipper (from-just result)))))))

;;; ====
;;; Constructors
;;; ====

;;; ast->zipper : AST -> AstZipper
;;; Create a zipper focused at the root of an AST.
(define (ast->zipper node)
  (make-ast-zipper (tree->zipper (ast->tree node))))

;;; zipper->ast : AstZipper -> AST
;;; Reconstruct AST from zipper (navigates to root first).
(define (zipper->ast az)
  (tree->ast (zipper->tree (ast-zipper-tree-z az))))

;;; ====
;;; Focus Operations
;;; ====

;;; ast-zipper-node : AstZipper -> AST
;;; Get the AST node at focus.
(define (ast-zipper-node az)
  (tree-value (tree-zipper-focus (ast-zipper-tree-z az))))

;;; ast-zipper-tag : AstZipper -> Symbol
;;; Get the tag of the focused AST node.
(define (ast-zipper-tag-of az)
  (sql-tag (ast-zipper-node az)))

;;; ast-zipper-span : AstZipper -> Span
;;; Get the span of the focused AST node.
(define (ast-zipper-span az)
  (sql-span (ast-zipper-node az)))

;;; ast-zipper-data : AstZipper -> List
;;; Get the data of the focused AST node.
(define (ast-zipper-data az)
  (sql-data (ast-zipper-node az)))

;;; ast-zipper-set : AstZipper x AST -> AstZipper
;;; Replace the focused AST node.
(define (ast-zipper-set az new-node)
  (let* ([tz (ast-zipper-tree-z az)]
         [new-tree (ast->tree new-node)]
         [new-tz (tree-zipper-set-tree tz new-tree)])
    (make-ast-zipper new-tz)))

;;; ast-zipper-modify : AstZipper x (AST -> AST) -> AstZipper
;;; Modify the focused AST node.
(define (ast-zipper-modify az f)
  (ast-zipper-set az (f (ast-zipper-node az))))

;;; ====
;;; Navigation
;;; ====
;;;
;;; Navigation functions lifted from tree-zipper operations.
;;; Uses lift-ast-nav to eliminate boilerplate.

;;; ast-zipper-up : AstZipper -> (Maybe AstZipper)
(define ast-zipper-up (lift-ast-nav tree-zipper-up))

;;; ast-zipper-down : AstZipper -> (Maybe AstZipper)
(define ast-zipper-down (lift-ast-nav tree-zipper-down))

;;; ast-zipper-left : AstZipper -> (Maybe AstZipper)
(define ast-zipper-left (lift-ast-nav tree-zipper-left))

;;; ast-zipper-right : AstZipper -> (Maybe AstZipper)
(define ast-zipper-right (lift-ast-nav tree-zipper-right))

;;; ast-zipper-root : AstZipper -> AstZipper
(define (ast-zipper-root az)
  (make-ast-zipper (tree-zipper-root (ast-zipper-tree-z az))))

;;; ast-zipper-nth-child : AstZipper x Nat -> (Maybe AstZipper)
(define ast-zipper-nth-child (lift-ast-nav-indexed tree-zipper-nth-child))

;;; ====
;;; Navigation Predicates
;;; ====

;;; ast-zipper-at-root? : AstZipper -> Bool
(define (ast-zipper-at-root? az)
  (tree-zipper-at-root? (ast-zipper-tree-z az)))

;;; ast-zipper-at-leaf? : AstZipper -> Bool
(define (ast-zipper-at-leaf? az)
  (tree-zipper-at-leaf? (ast-zipper-tree-z az)))

;;; ast-zipper-can-go-up? : AstZipper -> Bool
(define (ast-zipper-can-go-up? az)
  (tree-zipper-can-go-up? (ast-zipper-tree-z az)))

;;; ast-zipper-can-go-down? : AstZipper -> Bool
(define (ast-zipper-can-go-down? az)
  (tree-zipper-can-go-down? (ast-zipper-tree-z az)))

;;; ====
;;; Traversal
;;; ====

;;; ast-zipper-next : AstZipper -> (Maybe AstZipper)
;;; Move to next node in preorder traversal.
(define ast-zipper-next (lift-ast-nav tree-zipper-next-preorder))

;;; ast-zipper-prev : AstZipper -> (Maybe AstZipper)
;;; Move to previous node in preorder traversal.
(define ast-zipper-prev (lift-ast-nav tree-zipper-prev-preorder))

;;; ast-zipper-preorder : AstZipper -> (List AstZipper)
;;; Get all nodes in preorder traversal.
(define (ast-zipper-preorder az)
  (map make-ast-zipper
       (tree-zipper-preorder (ast-zipper-tree-z (ast-zipper-root az)))))

;;; ====
;;; Visitor Pattern Replacement
;;; ====
;;;
;;; These functions replace walk-ast and collect-nodes with
;;; immutable, zipper-based alternatives.

;;; ast-walk : (AST -> Unit) x AST -> Unit
;;; Walk AST depth-first, calling visitor on each node.
;;; Drop-in replacement for walk-ast (but still uses side effects in visitor).
(define (ast-walk visitor node)
  (let* ([az (ast->zipper node)]
         [all-zippers (ast-zipper-preorder az)])
    (for-each (lambda (z) (visitor (ast-zipper-node z))) all-zippers)))

;;; ast-collect : (AST -> Bool) x AST -> (List AST)
;;; Collect all nodes matching predicate.
;;; Drop-in replacement for collect-nodes (but immutable!).
(define (ast-collect pred node)
  (let* ([az (ast->zipper node)]
         [all-zippers (ast-zipper-preorder az)])
    (filter-map-ast
     (lambda (z)
       (let ([n (ast-zipper-node z)])
         (if (pred n) n #f)))
     all-zippers)))

;;; filter-map-ast : (a -> b | #f) x (List a) -> (List b)
(define (filter-map-ast f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (if result
              (loop (cdr lst) (cons result acc))
              (loop (cdr lst) acc))))))

;;; ====
;;; Advanced Traversal
;;; ====

;;; ast-find : (AST -> Bool) x AST -> (Maybe AstZipper)
;;; Find first node matching predicate, return zipper focused on it.
(define (ast-find pred node)
  (let* ([az (ast->zipper node)])
    (let loop ([current (just az)])
      (cond
        [(nothing? current) nothing]
        [(pred (ast-zipper-node (from-just current)))
         current]
        [else
         (loop (ast-zipper-next (from-just current)))]))))

;;; ast-find-by-tag : Symbol x AST -> (Maybe AstZipper)
;;; Find first node with given tag.
(define (ast-find-by-tag tag node)
  (ast-find (lambda (n) (and (sql-ast? n) (eq? (sql-tag n) tag))) node))

;;; ast-collect-by-tag : Symbol x AST -> (List AST)
;;; Collect all nodes with given tag.
(define (ast-collect-by-tag tag node)
  (ast-collect (lambda (n) (and (sql-ast? n) (eq? (sql-tag n) tag))) node))

;;; ====
;;; Transformation
;;; ====

;;; ast-transform : (AST -> AST) x AST -> AST
;;; Transform all nodes using function.
;;; Applies transformation bottom-up (children first).
(define (ast-transform f node)
  (let* ([az (ast->zipper node)]
         [all-zippers (reverse (ast-zipper-preorder az))])  ; bottom-up
    ;; Apply transformation to each node, rebuilding
    (let loop ([zippers all-zippers] [result node])
      (if (null? zippers)
          result
          (let* ([z (car zippers)]
                 [pos (tree-zipper-index-path (ast-zipper-tree-z z))]
                 [new-az (ast->zipper result)]
                 [at-pos (navigate-to-pos new-az pos)])
            (if (nothing? at-pos)
                (loop (cdr zippers) result)
                (let* ([transformed (f (ast-zipper-node (from-just at-pos)))]
                       [new-result (zipper->ast (ast-zipper-set (from-just at-pos) transformed))])
                  (loop (cdr zippers) new-result))))))))

;;; navigate-to-pos : AstZipper x Position -> (Maybe AstZipper)
(define (navigate-to-pos az pos)
  (if (null? pos)
      (just az)
      (let ([child (ast-zipper-nth-child az (car pos))])
        (if (nothing? child)
            nothing
            (navigate-to-pos (from-just child) (cdr pos))))))

;;; ast-transform-if : (AST -> Bool) x (AST -> AST) x AST -> AST
;;; Transform only nodes matching predicate.
(define (ast-transform-if pred f node)
  (ast-transform (lambda (n) (if (pred n) (f n) n)) node))

;;; ast-transform-tag : Symbol x (AST -> AST) x AST -> AST
;;; Transform only nodes with given tag.
(define (ast-transform-tag tag f node)
  (ast-transform-if
   (lambda (n) (and (sql-ast? n) (eq? (sql-tag n) tag)))
   f
   node))

;;; ====
;;; Context Information
;;; ====

;;; ast-zipper-depth : AstZipper -> Nat
;;; Get depth in tree (0 = root).
(define (ast-zipper-depth az)
  (tree-zipper-depth (ast-zipper-tree-z az)))

;;; ast-zipper-path : AstZipper -> (List AST)
;;; Get path from root to focus (list of parent nodes).
(define (ast-zipper-path az)
  (let ([crumbs (tree-zipper-crumbs (ast-zipper-tree-z az))])
    (map crumb-value crumbs)))

;;; ast-zipper-ancestors : AstZipper -> (List AST)
;;; Get ancestors from immediate parent to root.
(define (ast-zipper-ancestors az)
  (reverse (ast-zipper-path az)))

;;; ====
;;; Specific Node Finders
;;; ====

;;; ast-column-refs : AST -> (List AST)
;;; Get all column references in AST.
(define (ast-column-refs node)
  (ast-collect-by-tag 'column-ref node))

;;; ast-table-refs : AST -> (List AST)
;;; Get all table references in AST.
(define (ast-table-refs node)
  (ast-collect-by-tag 'table-ref node))

;;; ast-literals : AST -> (List AST)
;;; Get all literals in AST.
(define (ast-literals node)
  (ast-collect-by-tag 'literal node))

;;; ast-subqueries : AST -> (List AST)
;;; Get all subqueries in AST.
(define (ast-subqueries node)
  (ast-collect-by-tag 'subquery node))

;;; ast-function-calls : AST -> (List AST)
;;; Get all function calls in AST.
(define (ast-function-calls node)
  (ast-collect-by-tag 'function-call node))
