; tree-util.ss - Nested structure and tree utilities
; Part of The Fold prelude

; ============================================
; Deep structure traversal
; ============================================

; deep-map: Apply function to all atoms in nested structure
; (deep-map inc '((1 2) (3 (4 5)))) => ((2 3) (4 (5 6)))
(define (deep-map f tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (cons (deep-map f (car tree))
                (deep-map f (cdr tree)))
          (f tree))))

; deep-filter: Keep atoms matching predicate (preserves structure)
; (deep-filter even? '((1 2) (3 (4 5)))) => ((() 2) (() (4 ())))
(define (deep-filter p tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (cons (deep-filter p (car tree))
                (deep-filter p (cdr tree)))
          (if (p tree) tree '()))))

; flatten-deep: Flatten all nested lists into single list
; (flatten-deep '((1 2) (3 (4 5)))) => (1 2 3 4 5)
(define (flatten-deep tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (append (flatten-deep (car tree))
                  (flatten-deep (cdr tree)))
          (list tree))))

; ============================================
; Tree metrics
; ============================================

; tree-depth: Maximum nesting depth of structure
; (tree-depth '((1 2) (3 (4 5)))) => 3
(define (tree-depth tree)
  (if (null? tree)
      0
      (if (pair? tree)
          (+ 1 (max (tree-depth (car tree))
                    (tree-depth (cdr tree))))
          0)))

; tree-height: Alias for tree-depth
(define tree-height tree-depth)

; tree-size: Count all atoms in nested structure
; (tree-size '((1 2) (3 (4 5)))) => 5
(define (tree-size tree)
  (if (null? tree)
      0
      (if (pair? tree)
          (+ (tree-size (car tree))
             (tree-size (cdr tree)))
          1)))

; tree-count: Alias for tree-size
(define tree-count tree-size)

; ============================================
; Tree search
; ============================================

; tree-find: Find first atom matching predicate in tree
; (tree-find even? '((1 3) (5 (6 7)))) => 6
(define (tree-find p tree)
  (if (null? tree)
      #f
      (if (pair? tree)
          (let ((left (tree-find p (car tree))))
            (if left
                left
                (tree-find p (cdr tree))))
          (if (p tree) tree #f))))

; tree-leaves: Get all leaves of tree (atoms)
; (tree-leaves '((1 2) (3 (4 5)))) => (1 2 3 4 5)
(define tree-leaves flatten-deep)

; ============================================
; Module exports
; ============================================

(module-exports
  deep-map
  deep-filter
  flatten-deep
  tree-depth
  tree-height
  tree-size
  tree-count
  tree-find
  tree-leaves)
