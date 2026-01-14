;;; lattice/fp/data/zipper-lens.ss — Zipper-Lens Integration
;;;
;;; Connects zippers with the lens library for unified navigation and
;;; modification patterns. Provides lenses for zipper components and
;;; lens-like traversals for navigation.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Integration points:
;;;   1. Lenses INTO zippers (focus on zipper components)
;;;   2. Zippers AS lenses (navigate and modify nested data)
;;;   3. Comonad-lens connection (extend with lens operations)
;;;
;;; References:
;;;   - "Lenses, Folds, and Traversals" (Edward Kmett)
;;;   - "The Essence of the Iterator Pattern" (Gibbons & Oliveira)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/meta/combinators.ss
;;;   - fp/templates.ss (lens operations)
;;;   - fp/data/zipper.ss
;;;   - fp/data/tree-zipper.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")
(load "lattice/fp/data/zipper.ss")
(load "lattice/fp/data/tree-zipper.ss")

;;; ============================================================
;;; Part 1: Lenses INTO List Zippers
;;; ============================================================
;;;
;;; These lenses allow you to view/modify the internal structure
;;; of a list zipper using standard lens operations.

;;; zipper-focus-lens : Lens (ListZipper α) α
;;; Lens focusing on the current element of a list zipper.
;;; Partial: errors if zipper has no focus.
(define zipper-focus-lens
  (make-lens
   zipper-focus
   (lambda (a z) (zipper-set z a))))

;;; zipper-focus-maybe-lens : Lens (ListZipper α) (Maybe α)
;;; Lens focusing on the Maybe-wrapped focus.
;;; Total: works even when zipper has no focus.
(define zipper-focus-maybe-lens
  (make-lens
   zipper-focus-maybe
   (lambda (maybe-a z)
     (make-zipper (zipper-left z) maybe-a (zipper-right z)))))

;;; zipper-left-lens : Lens (ListZipper α) (List α)
;;; Lens focusing on the left context (reversed, closest first).
(define zipper-left-lens
  (make-lens
   zipper-left
   (lambda (left z)
     (make-zipper left (zipper-focus-maybe z) (zipper-right z)))))

;;; zipper-right-lens : Lens (ListZipper α) (List α)
;;; Lens focusing on the right context.
(define zipper-right-lens
  (make-lens
   zipper-right
   (lambda (right z)
     (make-zipper (zipper-left z) (zipper-focus-maybe z) right))))

;;; zipper-position-lens : Lens (ListZipper α) Nat
;;; Lens for reading/setting position. Setting moves the zipper.
(define zipper-position-lens
  (make-lens
   zipper-position
   (lambda (pos z) (zipper-goto z pos))))

;;; ============================================================
;;; Part 2: Lenses INTO Tree Zippers
;;; ============================================================

;;; tree-zipper-focus-lens : Lens (TreeZipper α) (Tree α)
;;; Lens focusing on the entire focused subtree.
(define tree-zipper-focus-lens
  (make-lens
   tree-zipper-focus
   (lambda (new-tree z) (tree-zipper-set-tree z new-tree))))

;;; tree-zipper-value-lens : Lens (TreeZipper α) α
;;; Lens focusing on just the value at the current node.
(define tree-zipper-value-lens
  (make-lens
   tree-zipper-get
   (lambda (v z) (tree-zipper-set z v))))

;;; tree-zipper-children-lens : Lens (TreeZipper α) (List (Tree α))
;;; Lens focusing on the children of the focused node.
(define tree-zipper-children-lens
  (make-lens
   (lambda (z) (tree-children (tree-zipper-focus z)))
   (lambda (children z)
     (let ([focus (tree-zipper-focus z)])
       (tree-zipper-set-tree z
         (make-tree (tree-value focus) children))))))

;;; tree-zipper-crumbs-lens : Lens (TreeZipper α) (List Crumb)
;;; Lens focusing on the navigation path (crumbs).
(define tree-zipper-crumbs-lens
  (make-lens
   tree-zipper-crumbs
   (lambda (crumbs z)
     (make-tree-zipper (tree-zipper-focus z) crumbs))))

;;; tree-zipper-depth-lens : Lens (TreeZipper α) Nat
;;; Read-only lens for depth (setting is a no-op).
(define tree-zipper-depth-lens
  (make-lens
   tree-zipper-depth
   (lambda (_depth z) z)))  ; Can't meaningfully set depth

;;; ============================================================
;;; Part 3: Navigation as Affine Traversals
;;; ============================================================
;;;
;;; Navigation operations can be viewed as affine traversals
;;; (they may fail to find a target). We model these as functions
;;; returning Maybe.

;;; Affine : (s → Maybe a) × (a → s → s) → Affine
;;; An affine is like a lens that may not have a focus.
(define (make-affine getter setter)
  (list 'affine getter setter))

(define (affine? x)
  (and (pair? x) (eq? (car x) 'affine)))

(define (affine-getter a) (cadr a))
(define (affine-setter a) (caddr a))

;;; preview-affine : Affine × s → Maybe a
;;; Try to get the focus.
(define (preview-affine affine s)
  ((affine-getter affine) s))

;;; set-affine : Affine × a × s → s
;;; Set if focus exists, otherwise return unchanged.
(define (set-affine affine a s)
  (let ([result ((affine-getter affine) s)])
    (if (nothing? result)
        s
        ((affine-setter affine) a s))))

;;; over-affine : Affine × (a → a) × s → s
;;; Modify if focus exists.
(define (over-affine affine f s)
  (let ([result ((affine-getter affine) s)])
    (if (nothing? result)
        s
        ((affine-setter affine) (f (from-just result)) s))))

;;; ============================================================
;;; Part 4: List Zipper Navigation Affines
;;; ============================================================

;;; zipper-left-affine : Affine (ListZipper α) (ListZipper α)
;;; Navigate left (returns new zipper position).
(define zipper-left-affine
  (make-affine
   zipper-left!
   (lambda (new-z _old-z) new-z)))

;;; zipper-right-affine : Affine (ListZipper α) (ListZipper α)
;;; Navigate right.
(define zipper-right-affine
  (make-affine
   zipper-right!
   (lambda (new-z _old-z) new-z)))

;;; zipper-nth-affine : Nat → Affine (ListZipper α) α
;;; Focus on the nth element relative to current position.
(define (zipper-nth-affine n)
  (make-affine
   (lambda (z)
     (let* ([current-pos (zipper-position z)]
            [target-pos (+ current-pos n)]
            [len (zipper-length z)])
       (if (and (>= target-pos 0) (< target-pos len))
           (let ([z2 (zipper-goto z target-pos)])
             (if (zipper-has-focus? z2)
                 (just (zipper-focus z2))
                 nothing))
           nothing)))
   (lambda (a z)
     (let* ([current-pos (zipper-position z)]
            [target-pos (+ current-pos n)])
       (zipper-set (zipper-goto z target-pos) a)))))

;;; ============================================================
;;; Part 5: Tree Zipper Navigation Affines
;;; ============================================================

;;; tree-up-affine : Affine (TreeZipper α) (TreeZipper α)
(define tree-up-affine
  (make-affine
   tree-zipper-up
   (lambda (new-z _old-z) new-z)))

;;; tree-down-affine : Affine (TreeZipper α) (TreeZipper α)
(define tree-down-affine
  (make-affine
   tree-zipper-down
   (lambda (new-z _old-z) new-z)))

;;; tree-left-affine : Affine (TreeZipper α) (TreeZipper α)
(define tree-left-affine
  (make-affine
   tree-zipper-left
   (lambda (new-z _old-z) new-z)))

;;; tree-right-affine : Affine (TreeZipper α) (TreeZipper α)
(define tree-right-affine
  (make-affine
   tree-zipper-right
   (lambda (new-z _old-z) new-z)))

;;; tree-nth-child-affine : Nat → Affine (TreeZipper α) (TreeZipper α)
(define (tree-nth-child-affine n)
  (make-affine
   (lambda (z) (tree-zipper-nth-child z n))
   (lambda (new-z _old-z) new-z)))

;;; tree-child-value-affine : Nat → Affine (TreeZipper α) α
;;; Focus on the value of the nth child without navigating there.
(define (tree-child-value-affine n)
  (make-affine
   (lambda (z)
     (let ([child-z (tree-zipper-nth-child z n)])
       (if (nothing? child-z)
           nothing
           (just (tree-zipper-get (from-just child-z))))))
   (lambda (a z)
     (let ([child-z (tree-zipper-nth-child z n)])
       (if (nothing? child-z)
           z
           (let* ([cz (from-just child-z)]
                  [modified (tree-zipper-set cz a)])
             ;; Navigate back up to maintain position
             (from-just (tree-zipper-up modified))))))))

;;; ============================================================
;;; Part 6: Zipper-to-Lens Adapter
;;; ============================================================
;;;
;;; Convert a zipper path into a composed lens for accessing
;;; deeply nested data.

;;; zipper-path->lens : (List (Affine s s)) → (s → Maybe Lens)
;;; Follow a path of navigation affines to create a lens.
;;; Returns nothing if any step fails.
(define (follow-path affines z)
  (let loop ([remaining affines] [current z])
    (if (null? remaining)
        (just current)
        (let ([result (preview-affine (car remaining) current)])
          (if (nothing? result)
              nothing
              (loop (cdr remaining) (from-just result)))))))

;;; zipper-to-lens : (ListZipper α) → Lens (List α) α
;;; Convert a list zipper to a lens that focuses on the same position.
;;; The zipper encodes "where" in the list; the lens accesses "that" position.
(define (zipper-to-lens z)
  (let ([pos (zipper-position z)])
    (lens-nth pos)))

;;; tree-path-to-lens : (List Nat) → Lens (Tree α) α
;;; Convert a list of child indices to a lens for tree access.
(define (tree-path-to-lens indices)
  (if (null? indices)
      ;; At root: lens on the value
      (make-lens tree-value
                 (lambda (v t) (make-tree v (tree-children t))))
      ;; Navigate down and compose
      (let* ([child-idx (car indices)]
             [rest-lens (tree-path-to-lens (cdr indices))]
             [child-lens
              (make-lens
               (lambda (t)
                 (let ([children (tree-children t)])
                   (if (< child-idx (length children))
                       (list-ref children child-idx)
                       (error 'tree-path-to-lens "child index out of bounds"))))
               (lambda (new-child t)
                 (let ([children (tree-children t)])
                   (make-tree (tree-value t)
                              (list-set children child-idx new-child)))))])
        (lens-compose child-lens rest-lens))))

;;; list-set : (List α) × Nat × α → (List α)
;;; Helper: set element at index.
(define (list-set lst idx val)
  (let loop ([i 0] [xs lst] [acc '()])
    (if (null? xs)
        (reverse acc)
        (if (= i idx)
            (loop (+ i 1) (cdr xs) (cons val acc))
            (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))

;;; ============================================================
;;; Part 7: Comonad-Lens Connection
;;; ============================================================
;;;
;;; The zipper comonad allows us to "extend" lens operations
;;; across all positions.

;;; extend-with-lens : Lens × ((ListZipper α) → β) → (ListZipper α) → (ListZipper β)
;;; Apply a contextual function that uses a lens, to all positions.
(define (extend-with-lens lens f z)
  (zipper-extend
   (lambda (z2)
     (if (zipper-has-focus? z2)
         (f (view lens z2) z2)
         (f #f z2)))  ; No focus, pass #f
   z))

;;; map-with-context : Lens × (α × Context → β) → (ListZipper α) → (ListZipper β)
;;; Map over elements with access to their local context via a lens.
(define (map-with-context lens f z)
  (zipper-extend
   (lambda (z2)
     (if (zipper-has-focus? z2)
         (f (view lens z2)
            (list (zipper-take-left z2 1)
                  (zipper-take-right z2 1)))
         #f))
   z))

;;; neighbor-lens : Lens (ListZipper α) (List α)
;;; Lens focusing on immediate neighbors (left and right).
(define neighbor-lens
  (make-lens
   (lambda (z)
     (append (zipper-take-left z 1)
             (zipper-take-right z 1)))
   (lambda (neighbors z)
     ;; Can't easily set neighbors, return unchanged
     z)))

;;; window-lens : Nat × Nat → Lens (ListZipper α) (List α)
;;; Lens focusing on a window around the focus.
(define (window-lens left-size right-size)
  (make-lens
   (lambda (z) (zipper-window z left-size right-size))
   (lambda (window z)
     ;; Setting a window is complex; return unchanged
     z)))

;;; ============================================================
;;; Part 8: Traversal Utilities
;;; ============================================================

;;; zipper-each : (α → β) → (ListZipper α) → (ListZipper β)
;;; Apply function to each element (like map, but preserves position).
(define (zipper-each f z)
  (zipper-map f z))

;;; zipper-all? : (α → Bool) → (ListZipper α) → Bool
;;; Check if predicate holds for all elements.
(define (zipper-all? pred z)
  (let ([as-list (zipper->list z)])
    (andmap pred as-list)))

;;; zipper-any? : (α → Bool) → (ListZipper α) → Bool
;;; Check if predicate holds for any element.
(define (zipper-any? pred z)
  (let ([as-list (zipper->list z)])
    (ormap pred as-list)))

;;; ormap : (α → Bool) × (List α) → Bool
(define (ormap pred xs)
  (if (null? xs)
      #f
      (or (pred (car xs)) (ormap pred (cdr xs)))))

;;; andmap : (α → Bool) × (List α) → Bool
(define (andmap-single pred xs)
  (if (null? xs)
      #t
      (and (pred (car xs)) (andmap-single pred (cdr xs)))))

;;; zipper-collect : (α → Maybe β) → (ListZipper α) → (List β)
;;; Collect results from a partial function applied to each element.
(define (zipper-collect f z)
  (let loop ([current (zipper-start z)] [acc '()])
    (if (not (zipper-has-focus? current))
        (reverse acc)
        (let* ([result (f (zipper-focus current))]
               [new-acc (if (just? result)
                           (cons (from-just result) acc)
                           acc)]
               [next (zipper-right! current)])
          (if (nothing? next)
              (reverse new-acc)
              (loop (from-just next) new-acc))))))

;;; ============================================================
;;; Part 9: Composed Lens Paths
;;; ============================================================
;;;
;;; Utilities for building lens paths through nested structures.

;;; at-focus : Lens (ListZipper α) α
;;; Alias for zipper-focus-lens.
(define at-focus zipper-focus-lens)

;;; at-left : Nat → Lens (ListZipper α) α
;;; Lens for the nth element to the left of focus (1 = immediate left).
(define (at-left n)
  (make-lens
   (lambda (z)
     (let ([left (zipper-left z)])
       (if (> n (length left))
           (error 'at-left "index out of bounds")
           (list-ref left (- n 1)))))
   (lambda (a z)
     (let ([left (zipper-left z)])
       (if (> n (length left))
           (error 'at-left "index out of bounds")
           (make-zipper (list-set left (- n 1) a)
                        (zipper-focus-maybe z)
                        (zipper-right z)))))))

;;; at-right : Nat → Lens (ListZipper α) α
;;; Lens for the nth element to the right of focus (1 = immediate right).
(define (at-right n)
  (make-lens
   (lambda (z)
     (let ([right (zipper-right z)])
       (if (> n (length right))
           (error 'at-right "index out of bounds")
           (list-ref right (- n 1)))))
   (lambda (a z)
     (let ([right (zipper-right z)])
       (if (> n (length right))
           (error 'at-right "index out of bounds")
           (make-zipper (zipper-left z)
                        (zipper-focus-maybe z)
                        (list-set right (- n 1) a)))))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Lenses into list zippers:
;;;   zipper-focus-lens, zipper-focus-maybe-lens
;;;   zipper-left-lens, zipper-right-lens, zipper-position-lens
;;;
;;; Lenses into tree zippers:
;;;   tree-zipper-focus-lens, tree-zipper-value-lens
;;;   tree-zipper-children-lens, tree-zipper-crumbs-lens
;;;   tree-zipper-depth-lens
;;;
;;; Affines (partial lenses) for navigation:
;;;   make-affine, affine?, affine-getter, affine-setter
;;;   preview-affine, set-affine, over-affine
;;;   zipper-left-affine, zipper-right-affine, zipper-nth-affine
;;;   tree-up-affine, tree-down-affine, tree-left-affine, tree-right-affine
;;;   tree-nth-child-affine, tree-child-value-affine
;;;
;;; Zipper-to-lens conversion:
;;;   follow-path, zipper-to-lens, tree-path-to-lens, list-set
;;;
;;; Comonad-lens integration:
;;;   extend-with-lens, map-with-context
;;;   neighbor-lens, window-lens
;;;
;;; Traversal utilities:
;;;   zipper-each, zipper-all?, zipper-any?, zipper-collect
;;;
;;; Composed lens paths:
;;;   at-focus, at-left, at-right
