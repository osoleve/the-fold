;;; lattice/fp/clp/domain.ss — Finite Domain Representation
;;;
;;; Implements finite domains for constraint logic programming using
;;; an interval-based representation for efficiency. Domains are
;;; represented as sorted, non-overlapping interval lists.
;;;
;;; This is Lattice code: pure, may use fuel for large operations.
;;;
;;; Representation:
;;;   Domain = ((lo1 . hi1) (lo2 . hi2) ...)
;;;   where lo1 <= hi1 < lo2 <= hi2 < ...
;;;   representing {lo1..hi1} ∪ {lo2..hi2} ∪ ...
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Domain Constructors
;;; ====

;;; make-domain : Int × Int → Domain
;;; Create a domain from a range [lo, hi].
;;; Returns empty domain if lo > hi.
(define (make-domain lo hi)
  (if (> lo hi)
      '()
      (list (cons lo hi))))

;;; domain-singleton : Int → Domain
;;; Create a domain containing a single value.
(define (domain-singleton n)
  (list (cons n n)))

;;; domain-from-list : (List Int) → Domain
;;; Create a domain from an explicit list of values.
;;; Values need not be sorted or unique.
(define (domain-from-list vals)
  (if (null? vals)
      '()
      (let ([sorted (list-sort < (list-uniq vals))])
           (intervals-from-sorted sorted))))

;;; intervals-from-sorted : (List Int) → Domain
;;; Convert sorted unique list to interval representation.
(define (intervals-from-sorted sorted)
  (if (null? sorted)
      '()
      (let loop ([vals (cdr sorted)]
                 [start (car sorted)]
                 [end (car sorted)]
                 [acc '()])
           (cond
            [(null? vals)
             (reverse (cons (cons start end) acc))]
            [(= (car vals) (+ end 1))
             (loop (cdr vals) start (car vals) acc)]
            [else
             (loop (cdr vals) (car vals) (car vals)
                   (cons (cons start end) acc))]))))

;;; list-uniq : (List α) → (List α)
;;; Remove duplicates from a list (preserves first occurrence).
(define (list-uniq lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; ====
;;; Domain Predicates
;;; ====

;;; domain? : α → Bool
;;; Check if value is a valid domain.
(define (domain? x)
  (and (list? x)
       (or (null? x)
           (and (pair? (car x))
                (integer? (caar x))
                (integer? (cdar x))
                (<= (caar x) (cdar x))))))

;;; domain-empty? : Domain → Bool
;;; Check if domain is empty.
(define (domain-empty? dom)
  (null? dom))

;;; domain-singleton? : Domain → Bool
;;; Check if domain contains exactly one value.
(define (domain-singleton? dom)
  (and (pair? dom)
       (null? (cdr dom))
       (= (caar dom) (cdar dom))))

;;; domain-contains? : Domain × Int → Bool
;;; Check if domain contains a specific value.
(define (domain-contains? dom n)
  (let loop ([intervals dom])
       (cond
        [(null? intervals) #f]
        [(< n (caar intervals)) #f]  ; n < lo, and intervals sorted
        [(<= n (cdar intervals)) #t]  ; lo <= n <= hi
        [else (loop (cdr intervals))])))

;;; ====
;;; Domain Accessors
;;; ====

;;; domain-size : Domain → Nat
;;; Count the number of values in the domain.
(define (domain-size dom)
  (let loop ([intervals dom] [count 0])
       (if (null? intervals)
           count
           (loop (cdr intervals)
                 (+ count (+ 1 (- (cdar intervals) (caar intervals))))))))

;;; domain-min : Domain → (Maybe Int)
;;; Get the minimum value in the domain.
(define (domain-min dom)
  (if (null? dom)
      #f
      (caar dom)))

;;; domain-max : Domain → (Maybe Int)
;;; Get the maximum value in the domain.
(define (domain-max dom)
  (if (null? dom)
      #f
      (cdr (list-last dom))))

;;; list-last : (List α) → α
;;; Get the last element of a non-empty list.
(define (list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (list-last (cdr lst))))

;;; domain-bounds : Domain → (Maybe (Int × Int))
;;; Get (min, max) bounds of domain.
(define (domain-bounds dom)
  (if (null? dom)
      #f
      (cons (domain-min dom) (domain-max dom))))

;;; ====
;;; Domain Operations
;;; ====

;;; domain-intersect : Domain × Domain → Domain
;;; Compute intersection of two domains.
(define (domain-intersect dom1 dom2)
  (let loop ([d1 dom1] [d2 dom2] [acc '()])
       (cond
        [(or (null? d1) (null? d2))
         (reverse acc)]
        [else
         (let* ([i1 (car d1)]
                [i2 (car d2)]
                [lo1 (car i1)] [hi1 (cdr i1)]
                [lo2 (car i2)] [hi2 (cdr i2)]
                [lo (max lo1 lo2)]
                [hi (min hi1 hi2)])
               (cond
                ;; No overlap, advance the one that ends first
                [(> lo hi)
                 (if (< hi1 hi2)
                     (loop (cdr d1) d2 acc)
                     (loop d1 (cdr d2) acc))]
                ;; Overlap found
                [else
                 (let ([new-acc (cons (cons lo hi) acc)])
                      (cond
                       [(< hi1 hi2) (loop (cdr d1) d2 new-acc)]
                       [(> hi1 hi2) (loop d1 (cdr d2) new-acc)]
                       [else (loop (cdr d1) (cdr d2) new-acc)]))]))])))

;;; domain-union : Domain × Domain → Domain
;;; Compute union of two domains.
(define (domain-union dom1 dom2)
  (merge-intervals (append dom1 dom2)))

;;; merge-intervals : (List Interval) → Domain
;;; Merge and normalize a list of intervals.
(define (merge-intervals intervals)
  (if (null? intervals)
      '()
      (let ([sorted (list-sort interval<? intervals)])
           (let loop ([rest (cdr sorted)]
                      [current (car sorted)]
                      [acc '()])
                (cond
                 [(null? rest)
                  (reverse (cons current acc))]
                 [else
                  (let* ([next (car rest)]
                         [cur-hi (cdr current)]
                         [next-lo (car next)]
                         [next-hi (cdr next)])
                        ;; If intervals overlap or are adjacent, merge them
                        (if (>= (+ cur-hi 1) next-lo)
                            (loop (cdr rest)
                                  (cons (car current) (max cur-hi next-hi))
                                  acc)
                            (loop (cdr rest)
                                  next
                                  (cons current acc))))])))))

;;; interval<? : Interval × Interval → Bool
;;; Compare intervals by their lower bound.
(define (interval<? i1 i2)
  (< (car i1) (car i2)))

;;; domain-subtract-value : Domain × Int → Domain
;;; Remove a single value from the domain.
(define (domain-subtract-value dom n)
  (let loop ([intervals dom] [acc '()])
       (cond
        [(null? intervals)
         (reverse acc)]
        [else
         (let* ([interval (car intervals)]
                [lo (car interval)]
                [hi (cdr interval)])
               (cond
                ;; n is before this interval
                [(< n lo)
                 (append (reverse acc) intervals)]
                ;; n is after this interval
                [(> n hi)
                 (loop (cdr intervals) (cons interval acc))]
                ;; n is exactly lo = hi (singleton interval)
                [(and (= n lo) (= n hi))
                 (loop (cdr intervals) acc)]
                ;; n is at lo boundary
                [(= n lo)
                 (loop (cdr intervals) (cons (cons (+ lo 1) hi) acc))]
                ;; n is at hi boundary
                [(= n hi)
                 (loop (cdr intervals) (cons (cons lo (- hi 1)) acc))]
                ;; n is in the middle - split interval
                [else
                 (loop (cdr intervals)
                       (cons (cons (+ n 1) hi)
                             (cons (cons lo (- n 1)) acc)))]))])))

;;; domain-subtract : Domain × Domain → Domain
;;; Remove all values in dom2 from dom1.
(define (domain-subtract dom1 dom2)
  (let loop ([d1 dom1] [d2 dom2] [acc '()])
       (cond
        [(null? d1) (reverse acc)]
        [(null? d2) (append (reverse acc) d1)]
        [else
         (let* ([i1 (car d1)]
                [i2 (car d2)]
                [lo1 (car i1)] [hi1 (cdr i1)]
                [lo2 (car i2)] [hi2 (cdr i2)])
               (cond
                ;; i2 is entirely before i1
                [(< hi2 lo1)
                 (loop d1 (cdr d2) acc)]
                ;; i1 is entirely before i2
                [(< hi1 lo2)
                 (loop (cdr d1) d2 (cons i1 acc))]
                ;; i2 covers all of i1
                [(and (<= lo2 lo1) (>= hi2 hi1))
                 (loop (cdr d1) d2 acc)]
                ;; i2 cuts off beginning of i1
                [(and (<= lo2 lo1) (< hi2 hi1))
                 (loop (cons (cons (+ hi2 1) hi1) (cdr d1)) (cdr d2) acc)]
                ;; i2 cuts off end of i1
                [(and (> lo2 lo1) (>= hi2 hi1))
                 (loop (cdr d1) d2 (cons (cons lo1 (- lo2 1)) acc))]
                ;; i2 is in the middle of i1 - split
                [else
                 (loop (cons (cons (+ hi2 1) hi1) (cdr d1))
                       (cdr d2)
                       (cons (cons lo1 (- lo2 1)) acc))]))])))

;;; domain-restrict-min : Domain × Int → Domain
;;; Keep only values >= n.
(define (domain-restrict-min dom n)
  (let loop ([intervals dom] [acc '()])
       (cond
        [(null? intervals)
         (reverse acc)]
        [else
         (let* ([interval (car intervals)]
                [lo (car interval)]
                [hi (cdr interval)])
               (cond
                ;; Entire interval is below n
                [(< hi n)
                 (loop (cdr intervals) acc)]
                ;; Entire interval is at or above n
                [(>= lo n)
                 (append (reverse acc) intervals)]
                ;; Partial overlap - truncate
                [else
                 (loop (cdr intervals) (cons (cons n hi) acc))]))])))

;;; domain-restrict-max : Domain × Int → Domain
;;; Keep only values <= n.
(define (domain-restrict-max dom n)
  (let loop ([intervals dom] [acc '()])
       (cond
        [(null? intervals)
         (reverse acc)]
        [else
         (let* ([interval (car intervals)]
                [lo (car interval)]
                [hi (cdr interval)])
               (cond
                ;; Entire interval is above n
                [(> lo n)
                 (reverse acc)]
                ;; Entire interval is at or below n
                [(<= hi n)
                 (loop (cdr intervals) (cons interval acc))]
                ;; Partial overlap - truncate
                [else
                 (reverse (cons (cons lo n) acc))]))])))

;;; ====
;;; Domain Enumeration
;;; ====

;;; domain->list : Domain → (List Int)
;;; Convert domain to explicit list of values.
;;; WARNING: Only use for small domains!
(define (domain->list dom)
  (let loop ([intervals dom] [acc '()])
       (if (null? intervals)
           (reverse acc)
           (let* ([interval (car intervals)]
                  [lo (car interval)]
                  [hi (cdr interval)])
                 (loop (cdr intervals)
                       (append (reverse (range lo (+ hi 1))) acc))))))

;;; range : Int × Int → (List Int)
;;; Generate list [lo, lo+1, ..., hi-1].
(define (range lo hi)
  (if (>= lo hi)
      '()
      (cons lo (range (+ lo 1) hi))))

;;; domain-for-each : Domain × (Int → ()) → ()
;;; Apply procedure to each value in domain.
(define (domain-for-each dom proc)
  (for-each
   (lambda (interval)
           (let loop ([n (car interval)])
                (when (<= n (cdr interval))
                      (proc n)
                      (loop (+ n 1)))))
   dom))

;;; domain-fold : Domain × α × (Int × α → α) → α
;;; Fold over domain values.
(define (domain-fold dom init f)
  (let loop ([intervals dom] [acc init])
       (if (null? intervals)
           acc
           (let* ([interval (car intervals)]
                  [lo (car interval)]
                  [hi (cdr interval)])
                 (let inner ([n lo] [acc acc])
                      (if (> n hi)
                          (loop (cdr intervals) acc)
                          (inner (+ n 1) (f n acc))))))))

;;; ====
;;; Domain Comparison
;;; ====

;;; domain=? : Domain × Domain → Bool
;;; Check if two domains are equal.
(define (domain=? dom1 dom2)
  (equal? dom1 dom2))

;;; domain-subset? : Domain × Domain → Bool
;;; Check if dom1 is a subset of dom2.
(define (domain-subset? dom1 dom2)
  (domain-empty? (domain-subtract dom1 dom2)))

;;; ====
;;; Domain Display
;;; ====

;;; domain->string : Domain → String
;;; Convert domain to readable string representation.
(define (domain->string dom)
  (if (null? dom)
      "{}"
      (string-append
       "{"
       (apply string-append
              (list-intersperse
               (map interval->string dom)
               ", "))
       "}")))

;;; interval->string : Interval → String
;;; Convert interval to string.
(define (interval->string interval)
  (let ([lo (car interval)]
        [hi (cdr interval)])
       (if (= lo hi)
           (number->string lo)
           (string-append (number->string lo) ".." (number->string hi)))))

;;; list-intersperse : (List α) × α → (List α)
;;; Intersperse separator between list elements.
(define (list-intersperse lst sep)
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst) (cons sep (list-intersperse (cdr lst) sep)))]))
