;;; @module vec
;;; @requires prelude iteration

(require 'prelude)
(require 'iteration)

(doc 'module 'vec)
(doc 'description "Core vector operations for linear algebra")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Vectors are represented as Scheme vectors for efficient O(1) access")
(doc 'note "All operations are pure and total (return error values rather than throwing)")

(doc 'section 'vector-construction)

(define (make-vec n init)
  (doc 'type '(-> Nat a (Vec a)))
  (doc 'description "Create a vector of n elements, all initialized to init")
  (make-vector n init))

(define (vec-from-list lst)
  (doc 'type '(-> (List a) (Vec a)))
  (doc 'description "Convert a list to a vector")
  (list->vector lst))

(define (vec->list v)
  (doc 'type '(-> (Vec a) (List a)))
  (doc 'description "Convert a vector to a list")
  (vector->list v))

(define (vec . elems)
  (doc 'export #t)
  (doc 'type '(-> a ... (Vec a)))
  (doc 'description "Variadic vector construction")
  (list->vector elems))

(doc 'section 'vector-accessors)

(define (vec-ref v i)
  (doc 'type '(-> (Vec a) Nat (Or a Error)))
  (doc 'description "Get element at index (0-based)")
  (if (and (>= i 0) (< i (vector-length v)))
      (vector-ref v i)
      `(error out-of-bounds ,i ,(vector-length v))))

(define (vec-length v)
  (doc 'type '(-> (Vec a) Nat))
  (doc 'description "Get the length of a vector")
  (vector-length v))

(define (vec-empty? v)
  (doc 'type '(-> (Vec a) Bool))
  (doc 'description "Check if vector is empty")
  (= (vector-length v) 0))

(define (vec-first v)
  (doc 'type '(-> (Vec a) (Or a Error)))
  (doc 'description "Get first element")
  (if (> (vector-length v) 0)
      (vector-ref v 0)
      '(error empty-vector)))

(define (vec-last v)
  (doc 'type '(-> (Vec a) (Or a Error)))
  (doc 'description "Get last element")
  (let ([n (vector-length v)])
       (if (> n 0)
           (vector-ref v (- n 1))
           '(error empty-vector))))

(doc 'section 'vector-transformations)

(define (vec-map f v)
  (doc 'type '(-> (-> a b) (Vec a) (Vec b)))
  (doc 'description "Apply function to each element")
  (doc 'fuel-cost '(linear n))
  (vec-map-idx i v (f (vector-ref v i))))

(define (vec-fold f init v)
  (doc 'type '(-> (-> b a b) b (Vec a) b))
  (doc 'description "Left fold over vector elements")
  (doc 'fuel-cost '(linear n))
  (vec-fold-idx acc init i v (f acc (vector-ref v i))))

(define (vec-fold-right f init v)
  (doc 'type '(-> (-> a b b) b (Vec a) b))
  (doc 'description "Right fold over vector elements")
  (vec-fold-reverse acc init i v (f (vector-ref v i) acc)))

(define (vec-zip-with f v1 v2)
  (doc 'type '(-> (-> a b c) (Vec a) (Vec b) (Or (Vec c) Error)))
  (doc 'description "Zip two vectors with a combining function")
  (let ([n1 (vector-length v1)]
        [n2 (vector-length v2)])
       (if (not (= n1 n2))
           `(error dimension-mismatch ,n1 ,n2)
           (vec-zip-map-idx i v1 v2 (f (vector-ref v1 i) (vector-ref v2 i))))))

(define (vec-append v1 v2)
  (doc 'type '(-> (Vec a) (Vec a) (Vec a)))
  (doc 'description "Concatenate two vectors")
  (let* ([n1 (vector-length v1)]
         [n2 (vector-length v2)]
         [result (make-vector (+ n1 n2) 0)])
        (range-do! i 0 n1
                   (vector-set! result i (vector-ref v1 i)))
        (range-do! i 0 n2
                   (vector-set! result (+ n1 i) (vector-ref v2 i)))
        result))

(define (vec-take v n)
  (doc 'type '(-> (Vec a) Nat (Or (Vec a) Error)))
  (doc 'description "Take first n elements")
  (let ([len (vector-length v)])
       (if (> n len)
           `(error out-of-bounds ,n ,len)
           (let* ([result (make-vector n 0)]
                  [src v])
                 (range-do! i 0 n
                            (vector-set! result i (vector-ref src i)))
                 result))))

(define (vec-drop v n)
  (doc 'type '(-> (Vec a) Nat (Or (Vec a) Error)))
  (doc 'description "Drop first n elements")
  (let ([len (vector-length v)])
       (if (> n len)
           `(error out-of-bounds ,n ,len)
           (let* ([new-len (- len n)]
                  [result (make-vector new-len 0)]
                  [src v]
                  [offset n])
                 (range-do! i 0 new-len
                            (vector-set! result i (vector-ref src (+ offset i))))
                 result))))

(define (vec-slice v start end)
  (doc 'type '(-> (Vec a) Nat Nat (Or (Vec a) Error)))
  (doc 'description "Extract slice from start (inclusive) to end (exclusive)")
  (let ([len (vector-length v)])
       (cond
        [(< start 0) `(error out-of-bounds ,start ,len)]
        [(> end len) `(error out-of-bounds ,end ,len)]
        [(> start end) `(error invalid-range ,start ,end)]
        [else
         (let* ([new-len (- end start)]
                [result (make-vector new-len 0)]
                [src v]
                [offset start])
               (range-do! i 0 new-len
                          (vector-set! result i (vector-ref src (+ offset i))))
               result)])))

(doc 'section 'vector-arithmetic)

(define (vec-add v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or (Vec Num) Error)))
  (doc 'description "Element-wise addition")
  (doc 'fuel-cost '(linear n))
  (vec-zip-with + v1 v2))

(define (vec-sub v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or (Vec Num) Error)))
  (doc 'description "Element-wise subtraction")
  (vec-zip-with - v1 v2))

(define (vec-mul v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or (Vec Num) Error)))
  (doc 'description "Element-wise multiplication (Hadamard product)")
  (vec-zip-with * v1 v2))

(define (vec-div v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or (Vec Num) Error)))
  (doc 'description "Element-wise division")
  (vec-zip-with / v1 v2))

(define (vec-scale k v)
  (doc 'type '(-> Num (Vec Num) (Vec Num)))
  (doc 'description "Scalar multiplication")
  (vec-map (lambda (x) (* k x)) v))

(define (vec-negate v)
  (doc 'type '(-> (Vec Num) (Vec Num)))
  (doc 'description "Negate all elements")
  (vec-map - v))

(doc 'section 'vector-products-and-norms)

(define (vec-dot v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or Num Error)))
  (doc 'description "Dot product (inner product)")
  (doc 'fuel-cost '(linear n))
  (let ([n1 (vector-length v1)]
        [n2 (vector-length v2)])
       (if (not (= n1 n2))
           `(error dimension-mismatch ,n1 ,n2)
           (dot-product-loop i n1 (vector-ref v1 i) (vector-ref v2 i)))))

(define (vec-norm-squared v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "Squared L2 norm (sum of squares)")
  (vec-dot v v))

(define (vec-norm v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "L2 norm (Euclidean length)")
  (doc 'fuel-cost '(linear n))
  (sqrt (vec-norm-squared v)))

(define (vec-norm-l1 v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "L1 norm (Manhattan distance)")
  (vec-fold (lambda (acc x) (+ acc (abs x))) 0 v))

(define (vec-norm-linf v)
  (doc 'type '(-> (Vec Num) (Or Num Error)))
  (doc 'description "L-infinity norm (maximum absolute value)")
  (doc 'note "Returns error for empty vectors")
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold (lambda (acc x) (max acc (abs x))) 0 v)))

(define (vec-normalize v)
  (doc 'type '(-> (Vec Num) (Or (Vec Num) Error)))
  (doc 'description "Normalize to unit length")
  (doc 'fuel-cost '(linear n))
  (let ([n (vec-norm v)])
       (if (= n 0)
           '(error zero-vector)
           (vec-scale (/ 1 n) v))))

(define (vec-distance v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or Num Error)))
  (doc 'description "Euclidean distance between two vectors")
  (let ([diff (vec-sub v1 v2)])
       (if (and (pair? diff) (eq? (car diff) 'error))
           diff
           (vec-norm diff))))

(doc 'section 'vector-comparisons)

(define (vec-equal? v1 v2)
  (doc 'type '(-> (Vec a) (Vec a) Bool))
  (doc 'description "Exact equality (short-circuits on first mismatch)")
  (and (= (vector-length v1) (vector-length v2))
       (let ([n (vector-length v1)])
            (do ([i 0 (+ i 1)]
                 [eq #t (and eq (equal? (vector-ref v1 i)
                                        (vector-ref v2 i)))])
                ((or (= i n) (not eq)) eq)))))

(define (vec-approx-equal? v1 v2 . epsilon-arg)
  (doc 'type '(-> (Vec Num) (Vec Num) &opt Num Bool))
  (doc 'description "Approximate equality within tolerance")
  (doc 'param 'epsilon "Tolerance (default 1e-10)")
  (let ([epsilon (if (null? epsilon-arg) 1e-10 (car epsilon-arg))])
       (and (= (vector-length v1) (vector-length v2))
            (let ([n (vector-length v1)])
                 (do ([i 0 (+ i 1)]
                      [eq #t (and eq (< (abs (- (vector-ref v1 i)
                                                (vector-ref v2 i)))
                                        epsilon))])
                     ((or (= i n) (not eq)) eq))))))

(doc 'section 'vector-utilities)

(define (vec-sum v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "Sum of all elements")
  (doc 'fuel-cost '(linear n))
  (vec-fold + 0 v))

(define (vec-product v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "Product of all elements")
  (vec-fold * 1 v))

(define (vec-mean v)
  (doc 'type '(-> (Vec Num) Num))
  (doc 'description "Arithmetic mean")
  (let ([n (vector-length v)])
       (if (= n 0)
           0
           (/ (vec-sum v) n))))

(define (vec-min v)
  (doc 'type '(-> (Vec Num) (Or Num Error)))
  (doc 'description "Minimum element")
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold min (vector-ref v 0) v)))

(define (vec-max v)
  (doc 'type '(-> (Vec Num) (Or Num Error)))
  (doc 'description "Maximum element")
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold max (vector-ref v 0) v)))

(define (vec-argmin v)
  (doc 'type '(-> (Vec Num) (Or Nat Error)))
  (doc 'description "Index of minimum element")
  (if (vec-empty? v)
      '(error empty-vector)
      (let ([n (vector-length v)])
           (range-fold min-i 0 i 1 n
                       (if (< (vector-ref v i) (vector-ref v min-i)) i min-i)))))

(define (vec-argmax v)
  (doc 'type '(-> (Vec Num) (Or Nat Error)))
  (doc 'description "Index of maximum element")
  (if (vec-empty? v)
      '(error empty-vector)
      (let ([n (vector-length v)])
           (range-fold max-i 0 i 1 n
                       (if (> (vector-ref v i) (vector-ref v max-i)) i max-i)))))

(define (vec-reverse v)
  (doc 'type '(-> (Vec a) (Vec a)))
  (doc 'description "Reverse a vector")
  (let ([n (vector-length v)])
       (vec-map-idx i v (vector-ref v (- n 1 i)))))

(define (vec-copy v)
  (doc 'type '(-> (Vec a) (Vec a)))
  (doc 'description "Create a copy of a vector")
  (vec-map-idx i v (vector-ref v i)))

(doc 'section 'special-vectors)

(define (vec-zeros n)
  (doc 'type '(-> Nat (Vec Num)))
  (doc 'description "Vector of n zeros")
  (make-vector n 0))

(define (vec-ones n)
  (doc 'type '(-> Nat (Vec Num)))
  (doc 'description "Vector of n ones")
  (make-vector n 1))

(define (vec-range n)
  (doc 'type '(-> Nat (Vec Nat)))
  (doc 'description "Vector of [0, 1, 2, ..., n-1]")
  (let ([result (make-vector n 0)])
       (range-do! i 0 n
                  (vector-set! result i i))
       result))

(define (vec-linspace start end n)
  (doc 'type '(-> Num Num Nat (Vec Num)))
  (doc 'description "Vector of n evenly spaced values from start to end")
  (if (< n 2)
      (if (= n 1)
          (vector start)
          (vector))
      (let* ([step (/ (- end start) (- n 1))]
             [result (make-vector n 0)])
            (range-do! i 0 n
                       (vector-set! result i (+ start (* i step))))
            result)))

(define (vec-unit n k)
  (doc 'type '(-> Nat Nat (Vec Num)))
  (doc 'description "Unit vector: 1 at position k, 0 elsewhere")
  (let ([result (make-vector n 0)])
       (when (and (>= k 0) (< k n))
             (vector-set! result k 1))
       result))
