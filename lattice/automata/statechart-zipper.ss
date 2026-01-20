(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/data/tree-zipper.ss")

(doc 'module 'statechart-zipper)
(doc 'description "Efficient navigation and modification of statechart hierarchies using tree-zipper")
(doc 'benefits "O(1) parent navigation (vs O(n) find-state), free ancestor tracking from crumbs, efficient LCA computation, local modification without full tree rebuild")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'state-tree-conversion)
(doc 'description "Statechart states form a rose tree where node value = state record and children = substates. Convert to/from tree-zipper's rose tree format")

(define (state->tree state)
  (doc 'type '(-> State (Tree State)))
  (doc 'description "Convert a state hierarchy to a rose tree for zipper navigation. The tree value at each node is the full state record")
  (let ([substates (state-substates state)])
    (make-tree state
               (map state->tree substates))))

;;; tree->state : (Tree State) -> State
;;; Convert a rose tree back to a state hierarchy.
;;; Rebuilds parent references during conversion.
(define (tree->state t)
  (tree->state-with-parent t #f))

;;; tree->state-with-parent : (Tree State) x (Option Symbol) -> State
;;; Helper that rebuilds parent references.
(define (tree->state-with-parent t parent-id)
  (let* ([state (tree-value t)]
         [children (tree-children t)]
         [state-with-parent (set-state-parent state parent-id)]
         [new-substates (map (lambda (child)
                              (tree->state-with-parent child (state-id state)))
                            children)])
    ;; Rebuild state with converted substates
    (make-state (state-id state)
                (state-type state)
                (state-entry-action state)
                (state-exit-action state)
                new-substates
                (state-transitions state)
                parent-id
                (state-data state))))

;;; ====
;;; State Zipper Type
;;; ====
;;;
;;; A state-zipper wraps tree-zipper specialized for states.
;;; It provides state-specific operations on top of generic navigation.

;;; state-zipper-tag : Symbol
(define state-zipper-tag 'state-zipper)

;;; make-state-zipper : (TreeZipper State) -> StateZipper
(define (make-state-zipper tree-z)
  (list state-zipper-tag tree-z))

;;; state-zipper? : a -> Bool
(define (state-zipper? x)
  (and (pair? x)
       (eq? (car x) state-zipper-tag)))

;;; state-zipper-tree-z : StateZipper -> (TreeZipper State)
(define (state-zipper-tree-z sz)
  (list-ref sz 1))

;;; ====
;;; Constructors
;;; ====

;;; state->zipper : State -> StateZipper
;;; Create a zipper focused at the root of a state hierarchy.
(define (state->zipper state)
  (make-state-zipper (tree->zipper (state->tree state))))

;;; zipper->state : StateZipper -> State
;;; Reconstruct state hierarchy from zipper (navigates to root first).
(define (zipper->state sz)
  (tree->state (zipper->tree (state-zipper-tree-z sz))))

;;; ====
;;; Focus Operations
;;; ====

;;; state-zipper-state : StateZipper -> State
;;; Get the currently focused state.
(define (state-zipper-state sz)
  (tree-value (tree-zipper-focus (state-zipper-tree-z sz))))

;;; state-zipper-id : StateZipper -> Symbol
;;; Get the ID of the focused state.
(define (state-zipper-id sz)
  (state-id (state-zipper-state sz)))

;;; state-zipper-type : StateZipper -> Symbol
;;; Get the type of the focused state.
(define (state-zipper-type sz)
  (state-type (state-zipper-state sz)))

;;; state-zipper-set : StateZipper x State -> StateZipper
;;; Replace the focused state (keeps same position).
(define (state-zipper-set sz new-state)
  (let* ([tz (state-zipper-tree-z sz)]
         [focus (tree-zipper-focus tz)]
         [new-tree (make-tree new-state (tree-children focus))]
         [new-tz (tree-zipper-set-tree tz new-tree)])
    (make-state-zipper new-tz)))

;;; state-zipper-modify : StateZipper x (State -> State) -> StateZipper
;;; Modify the focused state.
(define (state-zipper-modify sz f)
  (state-zipper-set sz (f (state-zipper-state sz))))

;;; ====
;;; Navigation
;;; ====

;;; state-zipper-up : StateZipper -> (Maybe StateZipper)
;;; Move focus to parent state.
(define (state-zipper-up sz)
  (let ([result (tree-zipper-up (state-zipper-tree-z sz))])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; state-zipper-down : StateZipper -> (Maybe StateZipper)
;;; Move focus to first child state.
(define (state-zipper-down sz)
  (let ([result (tree-zipper-down (state-zipper-tree-z sz))])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; state-zipper-left : StateZipper -> (Maybe StateZipper)
;;; Move focus to left sibling.
(define (state-zipper-left sz)
  (let ([result (tree-zipper-left (state-zipper-tree-z sz))])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; state-zipper-right : StateZipper -> (Maybe StateZipper)
;;; Move focus to right sibling.
(define (state-zipper-right sz)
  (let ([result (tree-zipper-right (state-zipper-tree-z sz))])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; state-zipper-root : StateZipper -> StateZipper
;;; Move focus to root state.
(define (state-zipper-root sz)
  (make-state-zipper (tree-zipper-root (state-zipper-tree-z sz))))

;;; state-zipper-nth-child : StateZipper x Nat -> (Maybe StateZipper)
;;; Move to nth child (0-indexed).
(define (state-zipper-nth-child sz n)
  (let ([result (tree-zipper-nth-child (state-zipper-tree-z sz) n)])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; ====
;;; Navigation Predicates
;;; ====

;;; state-zipper-at-root? : StateZipper -> Bool
(define (state-zipper-at-root? sz)
  (tree-zipper-at-root? (state-zipper-tree-z sz)))

;;; state-zipper-at-leaf? : StateZipper -> Bool
(define (state-zipper-at-leaf? sz)
  (tree-zipper-at-leaf? (state-zipper-tree-z sz)))

;;; state-zipper-can-go-up? : StateZipper -> Bool
(define (state-zipper-can-go-up? sz)
  (tree-zipper-can-go-up? (state-zipper-tree-z sz)))

;;; state-zipper-can-go-down? : StateZipper -> Bool
(define (state-zipper-can-go-down? sz)
  (tree-zipper-can-go-down? (state-zipper-tree-z sz)))

;;; ====
;;; State-Specific Search
;;; ====

;;; state-zipper-find : StateZipper x Symbol -> (Maybe StateZipper)
;;; Find a state by ID, returning zipper focused on it.
;;; Performs preorder traversal from current position.
(define (state-zipper-find sz target-id)
  (state-zipper-find-from (state-zipper-root sz) target-id))

;;; state-zipper-find-from : StateZipper x Symbol -> (Maybe StateZipper)
;;; Find state by ID starting from current position (preorder).
(define (state-zipper-find-from sz target-id)
  (cond
    ;; Found it
    [(eq? (state-zipper-id sz) target-id)
     (just sz)]
    ;; Try children
    [else
     (let try-down ([child (state-zipper-down sz)])
       (if (nothing? child)
           ;; No children or exhausted children, try right sibling
           (let try-right ([current sz])
             (let ([right (state-zipper-right current)])
               (if (nothing? right)
                   ;; No right sibling, go up and try right
                   (let ([up (state-zipper-up current)])
                     (if (nothing? up)
                         nothing  ; Exhausted tree
                         (try-right (from-just up))))
                   (state-zipper-find-from (from-just right) target-id))))
           ;; Check this child
           (let ([result (state-zipper-find-from (from-just child) target-id)])
             (if (just? result)
                 result
                 ;; Try next sibling
                 (let try-siblings ([current (from-just child)])
                   (let ([right (state-zipper-right current)])
                     (if (nothing? right)
                         nothing
                         (let ([result (state-zipper-find-from (from-just right) target-id)])
                           (if (just? result)
                               result
                               (try-siblings (from-just right)))))))))))]))

;;; ====
;;; Ancestor Operations (O(depth) instead of O(n))
;;; ====

;;; state-zipper-ancestors : StateZipper -> (List State)
;;; Get all ancestor states from parent to root.
;;; O(depth) - extracted directly from zipper crumbs.
(define (state-zipper-ancestors sz)
  (let ([crumbs (tree-zipper-crumbs (state-zipper-tree-z sz))])
    (map (lambda (crumb) (crumb-value crumb)) crumbs)))

;;; state-zipper-ancestor-ids : StateZipper -> (List Symbol)
;;; Get IDs of all ancestors.
(define (state-zipper-ancestor-ids sz)
  (map state-id (state-zipper-ancestors sz)))

;;; state-zipper-path : StateZipper -> (List State)
;;; Get path from root to focus (inclusive).
(define (state-zipper-path sz)
  (append (reverse (state-zipper-ancestors sz))
          (list (state-zipper-state sz))))

;;; state-zipper-path-ids : StateZipper -> (List Symbol)
;;; Get IDs of states from root to focus.
(define (state-zipper-path-ids sz)
  (map state-id (state-zipper-path sz)))

;;; state-zipper-depth : StateZipper -> Nat
;;; Get depth in hierarchy (0 = root).
(define (state-zipper-depth sz)
  (tree-zipper-depth (state-zipper-tree-z sz)))

;;; ====
;;; LCA (Lowest Common Ancestor) - Efficient Implementation
;;; ====
;;;
;;; To find LCA of two states:
;;; 1. Find both states (get zippers focused on each)
;;; 2. Extract ancestor paths from crumbs
;;; 3. Find common prefix

;;; state-zipper-lca : StateZipper x Symbol x Symbol -> (Maybe State)
;;; Find lowest common ancestor of two states by ID.
;;; More efficient than repeated find-state calls.
(define (state-zipper-lca sz id1 id2)
  (let* ([root-sz (state-zipper-root sz)]
         [z1 (state-zipper-find root-sz id1)]
         [z2 (state-zipper-find root-sz id2)])
    (if (or (nothing? z1) (nothing? z2))
        nothing
        (let* ([path1 (state-zipper-path-ids (from-just z1))]
               [path2 (state-zipper-path-ids (from-just z2))]
               [common (common-prefix path1 path2)])
          (if (null? common)
              nothing
              ;; Find the LCA state
              (let ([lca-id (last common)])
                (let ([lca-z (state-zipper-find root-sz lca-id)])
                  (if (nothing? lca-z)
                      nothing
                      (just (state-zipper-state (from-just lca-z)))))))))))

;;; common-prefix : (List a) x (List a) -> (List a)
;;; Find common prefix of two lists.
(define (common-prefix xs ys)
  (cond
    [(or (null? xs) (null? ys)) '()]
    [(equal? (car xs) (car ys))
     (cons (car xs) (common-prefix (cdr xs) (cdr ys)))]
    [else '()]))

;;; last : (List a) -> a
;;; Get last element of non-empty list.
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; ====
;;; Descendant Check
;;; ====

;;; state-zipper-is-descendant? : StateZipper x Symbol x Symbol -> Bool
;;; Check if child-id is a descendant of parent-id.
(define (state-zipper-is-descendant? sz parent-id child-id)
  (let* ([root-sz (state-zipper-root sz)]
         [child-z (state-zipper-find root-sz child-id)])
    (if (nothing? child-z)
        #f
        (if (member parent-id (state-zipper-ancestor-ids (from-just child-z)))
            #t
            #f))))

;;; ====
;;; Exit/Entry Path Computation
;;; ====
;;;
;;; For transitions, we need:
;;; - States to exit (source up to LCA, exclusive)
;;; - States to enter (LCA down to target, exclusive of LCA)

;;; state-zipper-exit-path : StateZipper x Symbol x Symbol -> (List State)
;;; Get states to exit when transitioning from source to target.
;;; Returns states from source up to (but not including) LCA, in exit order
;;; (innermost first, i.e., source -> ... -> just-below-LCA).
(define (state-zipper-exit-path sz source-id target-id)
  (let* ([root-sz (state-zipper-root sz)]
         [source-z (state-zipper-find root-sz source-id)]
         [lca-result (state-zipper-lca sz source-id target-id)])
    (if (or (nothing? source-z) (nothing? lca-result))
        '()
        (let* ([lca-id (state-id (from-just lca-result))]
               [source-path (state-zipper-path (from-just source-z))])
          ;; source-path is (root ... source)
          ;; Reverse to get (source ... root), take until LCA
          (takewhile-not (lambda (s) (eq? (state-id s) lca-id))
                         (reverse source-path))))))

;;; state-zipper-entry-path : StateZipper x Symbol x Symbol -> (List State)
;;; Get states to enter when transitioning from source to target.
;;; Returns states from after LCA down to target (inclusive).
(define (state-zipper-entry-path sz source-id target-id)
  (let* ([root-sz (state-zipper-root sz)]
         [target-z (state-zipper-find root-sz target-id)]
         [lca-result (state-zipper-lca sz source-id target-id)])
    (if (or (nothing? target-z) (nothing? lca-result))
        '()
        (let* ([lca-id (state-id (from-just lca-result))]
               [target-path (state-zipper-path (from-just target-z))])
          ;; Take states after LCA down to target
          (cdr (dropwhile-not (lambda (s) (eq? (state-id s) lca-id))
                              target-path))))))

;;; takewhile-not : (a -> Bool) x (List a) -> (List a)
(define (takewhile-not pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) '()]
    [else (cons (car lst) (takewhile-not pred (cdr lst)))]))

;;; dropwhile-not : (a -> Bool) x (List a) -> (List a)
(define (dropwhile-not pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) lst]
    [else (dropwhile-not pred (cdr lst))]))

;;; ====
;;; Preorder Traversal
;;; ====

;;; state-zipper-preorder : StateZipper -> (List StateZipper)
;;; Get all state zippers in preorder traversal.
(define (state-zipper-preorder sz)
  (map make-state-zipper
       (tree-zipper-preorder (state-zipper-tree-z (state-zipper-root sz)))))

;;; state-zipper-next : StateZipper -> (Maybe StateZipper)
;;; Move to next state in preorder traversal.
(define (state-zipper-next sz)
  (let ([result (tree-zipper-next-preorder (state-zipper-tree-z sz))])
    (if (nothing? result)
        nothing
        (just (make-state-zipper (from-just result))))))

;;; ====
;;; Utility: Find with predicate
;;; ====

;;; state-zipper-find-if : StateZipper x (State -> Bool) -> (Maybe StateZipper)
;;; Find first state matching predicate (preorder).
(define (state-zipper-find-if sz pred)
  (let loop ([current (just (state-zipper-root sz))])
    (cond
      [(nothing? current) nothing]
      [(pred (state-zipper-state (from-just current)))
       current]
      [else
       (loop (state-zipper-next (from-just current)))])))

;;; ====
;;; Collect Operations
;;; ====

;;; state-zipper-collect : StateZipper x (State -> Bool) -> (List State)
;;; Collect all states matching predicate.
(define (state-zipper-collect sz pred)
  (let loop ([current (just (state-zipper-root sz))] [acc '()])
    (cond
      [(nothing? current) (reverse acc)]
      [(pred (state-zipper-state (from-just current)))
       (loop (state-zipper-next (from-just current))
             (cons (state-zipper-state (from-just current)) acc))]
      [else
       (loop (state-zipper-next (from-just current)) acc)])))

;;; state-zipper-collect-ids : StateZipper x (State -> Bool) -> (List Symbol)
;;; Collect IDs of states matching predicate.
(define (state-zipper-collect-ids sz pred)
  (map state-id (state-zipper-collect sz pred)))
