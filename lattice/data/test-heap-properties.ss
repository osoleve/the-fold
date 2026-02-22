;;; lattice/data/test-heap-properties.ss — QuickCheck property tests for heaps

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'heap)
(require 'sort)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-heap
  (gen-map list->heap (gen-list gen-int)))

(define gen-heap-max
  (gen-map list->heap-max (gen-list gen-int)))

;;; ============================================================================
;;; Helpers — verify leftist property
;;; ============================================================================

(define (heap-leftist? h)
  (if (heap-empty? h)
      #t
      (and (>= (heap-rank (heap-left h))
               (heap-rank (heap-right h)))
           (heap-leftist? (heap-left h))
           (heap-leftist? (heap-right h)))))

(define (heap-ordered? h)
  (if (heap-empty? h)
      #t
      (let ([v (heap-value h)]
            [l (heap-left h)]
            [r (heap-right h)])
        (and (or (heap-empty? l) (<= v (heap-value l)))
             (or (heap-empty? r) (<= v (heap-value r)))
             (heap-ordered? l)
             (heap-ordered? r)))))

(define (all-satisfy? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (all-satisfy? pred (cdr lst)))))

(define (same-elements? xs ys)
  (equal? (merge-sort xs) (merge-sort ys)))

;;; ============================================================================
;;; Structural invariants
;;; ============================================================================

(test-group heap-invariants

  (define-property "leftist property holds after building from list"
    gen-heap
    (lambda (h) (heap-leftist? h))
    'tests 300)

  (define-property "heap ordering holds after building from list"
    gen-heap
    (lambda (h) (heap-ordered? h))
    'tests 300)
)

;;; ============================================================================
;;; Insert properties
;;; ============================================================================

(test-group heap-insert-properties

  (define-property "insert preserves leftist property"
    (gen-pair gen-int gen-heap)
    (lambda (p)
      (heap-leftist? (heap-insert (car p) (cdr p))))
    'tests 300)

  (define-property "insert preserves heap ordering"
    (gen-pair gen-int gen-heap)
    (lambda (p)
      (heap-ordered? (heap-insert (car p) (cdr p))))
    'tests 300)

  (define-property "insert increases size by 1"
    (gen-pair gen-int gen-heap)
    (lambda (p)
      (= (heap-size (heap-insert (car p) (cdr p)))
         (+ 1 (heap-size (cdr p)))))
    'tests 200)

  (define-property "min after insert is <= inserted element"
    (gen-pair gen-int gen-heap)
    (lambda (p)
      (let ([elem (car p)]
            [h (cdr p)])
        (<= (heap-min (heap-insert elem h)) elem)))
    'tests 300)

  (define-property "min after insert is min of old min and new element"
    (gen-pair gen-int gen-heap)
    (lambda (p)
      (let ([elem (car p)]
            [h (cdr p)])
        (if (heap-empty? h)
            (= (heap-min (heap-insert elem h)) elem)
            (= (heap-min (heap-insert elem h))
               (min elem (heap-min h))))))
    'tests 300)
)

;;; ============================================================================
;;; Delete-min properties
;;; ============================================================================

(test-group heap-delete-properties

  (define-property "delete-min preserves leftist property"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (let ([h (list->heap lst)])
            (heap-leftist? (heap-delete-min h)))))
    'tests 300)

  (define-property "delete-min preserves heap ordering"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (let ([h (list->heap lst)])
            (heap-ordered? (heap-delete-min h)))))
    'tests 300)

  (define-property "delete-min decreases size by 1"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (let ([h (list->heap lst)])
            (= (heap-size (heap-delete-min h))
               (- (heap-size h) 1)))))
    'tests 200)
)

;;; ============================================================================
;;; Merge properties
;;; ============================================================================

(test-group heap-merge-properties

  (define-property "merge size = sum of sizes"
    (gen-pair gen-heap gen-heap)
    (lambda (p)
      (= (heap-size (heap-merge (car p) (cdr p)))
         (+ (heap-size (car p)) (heap-size (cdr p)))))
    'tests 200)

  (define-property "merge preserves leftist property"
    (gen-pair gen-heap gen-heap)
    (lambda (p)
      (heap-leftist? (heap-merge (car p) (cdr p))))
    'tests 200)

  (define-property "merge preserves heap ordering"
    (gen-pair gen-heap gen-heap)
    (lambda (p)
      (heap-ordered? (heap-merge (car p) (cdr p))))
    'tests 200)

  (define-property "merge min is min of both mins"
    (gen-pair gen-heap gen-heap)
    (lambda (p)
      (let ([h1 (car p)] [h2 (cdr p)])
        (cond
          [(and (heap-empty? h1) (heap-empty? h2)) #t]
          [(heap-empty? h1) (= (heap-min (heap-merge h1 h2)) (heap-min h2))]
          [(heap-empty? h2) (= (heap-min (heap-merge h1 h2)) (heap-min h1))]
          [else (= (heap-min (heap-merge h1 h2))
                   (min (heap-min h1) (heap-min h2)))])))
    'tests 200)
)

;;; ============================================================================
;;; Roundtrip / heapsort properties
;;; ============================================================================

(test-group heap-roundtrip

  (define-property "heap->list produces sorted output"
    gen-heap
    (lambda (h)
      (let ([lst (heap->list h)])
        (or (null? lst)
            (null? (cdr lst))
            (let loop ([prev (car lst)] [rest (cdr lst)])
              (or (null? rest)
                  (and (<= prev (car rest))
                       (loop (car rest) (cdr rest))))))))
    'tests 300)

  (define-property "list->heap->list is a sorted permutation"
    (gen-list gen-int)
    (lambda (lst)
      (let ([result (heap->list (list->heap lst))])
        (same-elements? lst result)))
    'tests 300)

  (define-property "heapsort agrees with merge-sort"
    (gen-list gen-int)
    (lambda (lst)
      (equal? (merge-sort lst) (heapsort lst)))
    'tests 200)

  (define-property "heap size equals input list length"
    (gen-list gen-int)
    (lambda (lst)
      (= (heap-size (list->heap lst)) (length lst)))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
