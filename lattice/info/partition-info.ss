(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module partition-info
;;; @requires prelude entropy

(require 'prelude)
(require 'entropy)

(doc 'module 'partition-info)
(doc 'description "Information-theoretic partition quality metrics for community detection")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Bridges info/ to data/graph/ by providing the standard info-theoretic
  quality metrics used in community detection evaluation.
  All functions operate on label vectors (vectors of integer labels).")

(doc 'section 'partition-entropy)

(define (partition-entropy labels)
  (doc 'export #t)
  (doc 'type '(-> Vector Number))
  (doc 'description "Shannon entropy of partition sizes H(C) = -sum(n_k/n * log2(n_k/n)).
   A partition with equally-sized communities has maximum entropy.
   A single-community partition has zero entropy.")
  (let* ([n (vector-length labels)]
         [sizes (partition-sizes labels)]
         [probs (map (lambda (s) (/ s n)) sizes)])
    (entropy probs)))

(define (partition-sizes labels)
  (doc 'export #t)
  (doc 'type '(-> Vector (List Nat)))
  (doc 'description "Count the size of each community in a label vector.
   Returns list of counts (one per distinct label), sorted by label.")
  (let ([counts (make-hashtable equal-hash equal?)])
    (let loop ([i 0])
      (if (= i (vector-length labels))
          ;; Extract counts sorted by label
          (let* ([keys (vector->list (hashtable-keys counts))]
                 [sorted-keys (sort-labels keys)])
            (map (lambda (k) (hashtable-ref counts k 0)) sorted-keys))
          (begin
            (let ([lbl (vector-ref labels i)])
              (hashtable-set! counts lbl
                              (+ 1 (hashtable-ref counts lbl 0))))
            (loop (+ i 1)))))))

(define (sort-labels keys)
  (doc 'export #f)
  (doc 'type '(-> (List Any) (List Any)))
  (doc 'description "Sort label keys for deterministic output")
  ;; Labels are integers from label-propagation
  (let loop ([remaining keys] [sorted '()])
    (if (null? remaining)
        sorted
        (loop (cdr remaining)
              (insert-sorted (car remaining) sorted)))))

(define (insert-sorted x sorted)
  (cond
    [(null? sorted) (list x)]
    [(< x (car sorted)) (cons x sorted)]
    [else (cons (car sorted) (insert-sorted x (cdr sorted)))]))

(doc 'section 'partition-mutual-information)

(define (partition-mi labels-a labels-b)
  (doc 'export #t)
  (doc 'type '(-> Vector Vector Number))
  (doc 'description "Mutual information I(A;B) between two partitions.
   Measures how much knowing one partition tells you about the other.
   I(A;B) = H(A) + H(B) - H(A,B).")
  (let* ([n (vector-length labels-a)]
         [joint (build-contingency-table labels-a labels-b)]
         [joint-probs (map (lambda (row)
                             (map (lambda (c) (/ c n)) row))
                           joint)]
         [marginal-a (map (lambda (row) (fold-left + 0 row)) joint-probs)]
         [num-cols (if (null? joint-probs) 0 (length (car joint-probs)))]
         [marginal-b (if (= num-cols 0)
                         '()
                         (map (lambda (j)
                                (fold-left + 0
                                           (map (lambda (row) (list-ref row j))
                                                joint-probs)))
                              (iota num-cols)))])
    (mutual-information joint-probs marginal-a marginal-b)))

(define (build-contingency-table labels-a labels-b)
  (doc 'export #f)
  (doc 'type '(-> Vector Vector (List (List Nat))))
  (doc 'description "Build contingency table from two label vectors.
   Rows correspond to communities in A, columns to communities in B.")
  (let* ([n (vector-length labels-a)]
         ;; Collect unique labels
         [labels-a-unique (unique-labels labels-a)]
         [labels-b-unique (unique-labels labels-b)]
         [na (length labels-a-unique)]
         [nb (length labels-b-unique)]
         ;; Map labels to indices
         [a-index (label-index-map labels-a-unique)]
         [b-index (label-index-map labels-b-unique)]
         ;; Count joint occurrences
         [counts (make-vector (* na nb) 0)])
    (let loop ([i 0])
      (if (= i n)
          ;; Convert to list-of-lists
          (let row-loop ([r 0] [rows '()])
            (if (= r na)
                (reverse rows)
                (let col-loop ([c 0] [cols '()])
                  (if (= c nb)
                      (row-loop (+ r 1) (cons (reverse cols) rows))
                      (col-loop (+ c 1)
                                (cons (vector-ref counts (+ (* r nb) c)) cols))))))
          (let ([ai (hashtable-ref a-index (vector-ref labels-a i) 0)]
                [bi (hashtable-ref b-index (vector-ref labels-b i) 0)])
            (let ([idx (+ (* ai nb) bi)])
              (vector-set! counts idx (+ 1 (vector-ref counts idx))))
            (loop (+ i 1)))))))

(define (unique-labels labels)
  (doc 'export #f)
  (let ([seen (make-hashtable equal-hash equal?)]
        [result '()])
    (let loop ([i 0])
      (if (= i (vector-length labels))
          (sort-labels result)
          (let ([lbl (vector-ref labels i)])
            (unless (hashtable-ref seen lbl #f)
              (hashtable-set! seen lbl #t)
              (set! result (cons lbl result)))
            (loop (+ i 1)))))))

(define (label-index-map labels-list)
  (doc 'export #f)
  (let ([ht (make-hashtable equal-hash equal?)])
    (let loop ([ls labels-list] [i 0])
      (if (null? ls)
          ht
          (begin
            (hashtable-set! ht (car ls) i)
            (loop (cdr ls) (+ i 1)))))))

(doc 'section 'normalized-mutual-information)

(define (partition-nmi labels-a labels-b)
  (doc 'export #t)
  (doc 'type '(-> Vector Vector Number))
  (doc 'description "Normalized Mutual Information between two partitions.
   NMI(A,B) = I(A;B) / sqrt(H(A) * H(B)), in [0, 1].
   NMI = 1 means identical partitions, NMI = 0 means independent.
   This is the standard metric for comparing community detection results
   against ground truth.")
  (let ([ha (partition-entropy labels-a)]
        [hb (partition-entropy labels-b)]
        [mi (partition-mi labels-a labels-b)])
    (if (or (<= ha 0) (<= hb 0))
        ;; If either partition has zero entropy (single community),
        ;; NMI is 1 if both are single-community, 0 otherwise
        (if (and (<= ha 0) (<= hb 0)) 1.0 0.0)
        (/ mi (sqrt (* ha hb))))))

(doc 'section 'variation-of-information)

(define (partition-vi labels-a labels-b)
  (doc 'export #t)
  (doc 'type '(-> Vector Vector Number))
  (doc 'description "Variation of Information between two partitions.
   VI(A,B) = H(A|B) + H(B|A) = H(A) + H(B) - 2*I(A;B).
   VI is a true metric on the space of partitions (satisfies triangle inequality).
   VI = 0 means identical partitions.")
  (let ([ha (partition-entropy labels-a)]
        [hb (partition-entropy labels-b)]
        [mi (partition-mi labels-a labels-b)])
    (max 0 (+ ha hb (* -2 mi)))))

(define (partition-vi-normalized labels-a labels-b)
  (doc 'export #t)
  (doc 'type '(-> Vector Vector Number))
  (doc 'description "Normalized Variation of Information: VI(A,B) / log2(n).
   Bounded in [0, 1], independent of partition size.")
  (let* ([n (vector-length labels-a)]
         [vi (partition-vi labels-a labels-b)])
    (if (<= n 1)
        0
        (/ vi (log2 n)))))
