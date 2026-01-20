(load "core/base/prelude.ss")

(doc 'module 'domain)
(doc 'description "Finite Domain Representation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Implements finite domains using an interval-based representation for efficiency. Domains are sorted, non-overlapping interval lists.")
(doc 'note "Representation: Domain = ((lo1 . hi1) (lo2 . hi2) ...) where lo1 <= hi1 < lo2 <= hi2 < ... representing {lo1..hi1} ∪ {lo2..hi2} ∪ ...")
(doc 'exports "make-domain, domain-singleton, domain-from-list, domain predicates, domain operations, domain enumeration, domain comparison")
(doc 'dependencies "prelude.ss")

(doc 'section 'domain-constructors)

(define (make-domain lo hi)
  (doc 'type '(-> Int Int Domain))
  (doc 'description "Create a domain from a range [lo, hi]")
  (doc 'returns "Empty domain if lo > hi")
  (if (> lo hi)
      '()
      (list (cons lo hi))))

(define (domain-singleton n)
  (doc 'type '(-> Int Domain))
  (doc 'description "Create a domain containing a single value")
  (list (cons n n)))

(define (domain-from-list vals)
  (doc 'type '(-> (List Int) Domain))
  (doc 'description "Create a domain from an explicit list of values")
  (doc 'note "Values need not be sorted or unique")
  (if (null? vals)
      '()
      (let ([sorted (list-sort < (list-uniq vals))])
           (intervals-from-sorted sorted))))

(define (intervals-from-sorted sorted)
  (doc 'type '(-> (List Int) Domain))
  (doc 'description "Convert sorted unique list to interval representation")
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

(define (list-uniq lst)
  (doc 'type '(-> (List α) (List α)))
  (doc 'description "Remove duplicates from a list (preserves first occurrence)")
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

(doc 'section 'domain-predicates)

(define (domain? x)
  (doc 'type '(-> α Bool))
  (doc 'description "Check if value is a valid domain")
  (and (list? x)
       (or (null? x)
           (and (pair? (car x))
                (integer? (caar x))
                (integer? (cdar x))
                (<= (caar x) (cdar x))))))

(define (domain-empty? dom)
  (doc 'type '(-> Domain Bool))
  (doc 'description "Check if domain is empty")
  (null? dom))

(define (domain-singleton? dom)
  (doc 'type '(-> Domain Bool))
  (doc 'description "Check if domain contains exactly one value")
  (and (pair? dom)
       (null? (cdr dom))
       (= (caar dom) (cdar dom))))

(define (domain-contains? dom n)
  (doc 'type '(-> Domain Int Bool))
  (doc 'description "Check if domain contains a specific value")
  (let loop ([intervals dom])
       (cond
        [(null? intervals) #f]
        [(< n (caar intervals)) #f]  ; n < lo, and intervals sorted
        [(<= n (cdar intervals)) #t]  ; lo <= n <= hi
        [else (loop (cdr intervals))])))

(doc 'section 'domain-accessors)

(define (domain-size dom)
  (doc 'type '(-> Domain Nat))
  (doc 'description "Count the number of values in the domain")
  (let loop ([intervals dom] [count 0])
       (if (null? intervals)
           count
           (loop (cdr intervals)
                 (+ count (+ 1 (- (cdar intervals) (caar intervals))))))))

(define (domain-min dom)
  (doc 'type '(-> Domain (Maybe Int)))
  (doc 'description "Get the minimum value in the domain")
  (if (null? dom)
      #f
      (caar dom)))

(define (domain-max dom)
  (doc 'type '(-> Domain (Maybe Int)))
  (doc 'description "Get the maximum value in the domain")
  (if (null? dom)
      #f
      (cdr (list-last dom))))

(define (list-last lst)
  (doc 'type '(-> (List α) α))
  (doc 'description "Get the last element of a non-empty list")
  (if (null? (cdr lst))
      (car lst)
      (list-last (cdr lst))))

(define (domain-bounds dom)
  (doc 'type '(-> Domain (Maybe (Pair Int Int))))
  (doc 'description "Get (min, max) bounds of domain")
  (if (null? dom)
      #f
      (cons (domain-min dom) (domain-max dom))))

(doc 'section 'domain-operations)

(define (domain-intersect dom1 dom2)
  (doc 'type '(-> Domain Domain Domain))
  (doc 'description "Compute intersection of two domains")
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

(define (domain-union dom1 dom2)
  (doc 'type '(-> Domain Domain Domain))
  (doc 'description "Compute union of two domains")
  (merge-intervals (append dom1 dom2)))

(define (merge-intervals intervals)
  (doc 'type '(-> (List Interval) Domain))
  (doc 'description "Merge and normalize a list of intervals")
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

(define (interval<? i1 i2)
  (doc 'type '(-> Interval Interval Bool))
  (doc 'description "Compare intervals by their lower bound")
  (< (car i1) (car i2)))

(define (domain-subtract-value dom n)
  (doc 'type '(-> Domain Int Domain))
  (doc 'description "Remove a single value from the domain")
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

(define (domain-subtract dom1 dom2)
  (doc 'type '(-> Domain Domain Domain))
  (doc 'description "Remove all values in dom2 from dom1")
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

(define (domain-restrict-min dom n)
  (doc 'type '(-> Domain Int Domain))
  (doc 'description "Keep only values >= n")
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

(define (domain-restrict-max dom n)
  (doc 'type '(-> Domain Int Domain))
  (doc 'description "Keep only values <= n")
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

(doc 'section 'domain-enumeration)

(define (domain->list dom)
  (doc 'type '(-> Domain (List Int)))
  (doc 'description "Convert domain to explicit list of values")
  (doc 'note "WARNING: Only use for small domains!")
  (let loop ([intervals dom] [acc '()])
       (if (null? intervals)
           (reverse acc)
           (let* ([interval (car intervals)]
                  [lo (car interval)]
                  [hi (cdr interval)])
                 (loop (cdr intervals)
                       (append (reverse (range lo (+ hi 1))) acc))))))

(define (range lo hi)
  (doc 'type '(-> Int Int (List Int)))
  (doc 'description "Generate list [lo, lo+1, ..., hi-1]")
  (if (>= lo hi)
      '()
      (cons lo (range (+ lo 1) hi))))

(define (domain-for-each dom proc)
  (doc 'type '(-> Domain (-> Int Void) Void))
  (doc 'description "Apply procedure to each value in domain")
  (for-each
   (lambda (interval)
           (let loop ([n (car interval)])
                (when (<= n (cdr interval))
                      (proc n)
                      (loop (+ n 1)))))
   dom))

(define (domain-fold dom init f)
  (doc 'type '(-> Domain α (-> Int α α) α))
  (doc 'description "Fold over domain values")
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

(doc 'section 'domain-comparison)

(define (domain=? dom1 dom2)
  (doc 'type '(-> Domain Domain Bool))
  (doc 'description "Check if two domains are equal")
  (equal? dom1 dom2))

(define (domain-subset? dom1 dom2)
  (doc 'type '(-> Domain Domain Bool))
  (doc 'description "Check if dom1 is a subset of dom2")
  (domain-empty? (domain-subtract dom1 dom2)))

(doc 'section 'domain-display)

(define (domain->string dom)
  (doc 'type '(-> Domain String))
  (doc 'description "Convert domain to readable string representation")
  (if (null? dom)
      "{}"
      (string-append
       "{"
       (apply string-append
              (list-intersperse
               (map interval->string dom)
               ", "))
       "}")))

(define (interval->string interval)
  (doc 'type '(-> Interval String))
  (doc 'description "Convert interval to string")
  (let ([lo (car interval)]
        [hi (cdr interval)])
       (if (= lo hi)
           (number->string lo)
           (string-append (number->string lo) ".." (number->string hi)))))

(define (list-intersperse lst sep)
  (doc 'type '(-> (List α) α (List α)))
  (doc 'description "Intersperse separator between list elements")
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst) (cons sep (list-intersperse (cdr lst) sep)))]))
