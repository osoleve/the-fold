;;; core/linalg/vec.ss — Vector Operations
;;; @module vec
;;; @requires prelude
;;;
;;; Core vector operations for linear algebra.
;;;
;;; Vectors are represented as Scheme vectors for efficient O(1) access.
;;; All operations are pure and total (return error values rather than throwing).
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Vector Construction
;;; ============================================================

;;; make-vec : Nat × a → Vec a
;;; Create a vector of n elements, all initialized to init.
(define (make-vec n init)
  (make-vector n init))

;;; vec-from-list : (List a) → Vec a
;;; Convert a list to a vector.
(define (vec-from-list lst)
  (list->vector lst))

;;; vec->list : Vec a → (List a)
;;; Convert a vector to a list.
(define (vec->list v)
  (vector->list v))

;;; vec : a ... → Vec a
;;; Variadic vector construction.
(define (vec . elems)
  (list->vector elems))

;;; ============================================================
;;; Vector Accessors
;;; ============================================================

;;; vec-ref : Vec a × Nat → a | Error
;;; Get element at index (0-based).
(define (vec-ref v i)
  (if (and (>= i 0) (< i (vector-length v)))
      (vector-ref v i)
      `(error out-of-bounds ,i ,(vector-length v))))

;;; vec-length : Vec a → Nat
(define (vec-length v)
  (vector-length v))

;;; vec-empty? : Vec a → Boolean
(define (vec-empty? v)
  (= (vector-length v) 0))

;;; vec-first : Vec a → a | Error
(define (vec-first v)
  (if (> (vector-length v) 0)
      (vector-ref v 0)
      '(error empty-vector)))

;;; vec-last : Vec a → a | Error
(define (vec-last v)
  (let ([n (vector-length v)])
       (if (> n 0)
           (vector-ref v (- n 1))
           '(error empty-vector))))

;;; ============================================================
;;; Vector Transformations
;;; ============================================================

;;; vec-map : (a → b) × Vec a → Vec b
;;; Apply function to each element.
(define (vec-map f v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (f (vector-ref v i))))))

;;; vec-fold : (b × a → b) × b × Vec a → b
;;; Left fold over vector elements.
(define (vec-fold f init v)
  (let ([n (vector-length v)])
       (do ([i 0 (+ i 1)]
            [acc init (f acc (vector-ref v i))])
           ((= i n) acc))))

;;; vec-fold-right : (a × b → b) × b × Vec a → b
;;; Right fold over vector elements.
(define (vec-fold-right f init v)
  (let ([n (vector-length v)])
       (do ([i (- n 1) (- i 1)]
            [acc init (f (vector-ref v i) acc)])
           ((< i 0) acc))))

;;; vec-zip-with : (a × b → c) × Vec a × Vec b → Vec c | Error
;;; Zip two vectors with a combining function.
(define (vec-zip-with f v1 v2)
  (let ([n1 (vector-length v1)]
        [n2 (vector-length v2)])
       (if (not (= n1 n2))
           `(error dimension-mismatch ,n1 ,n2)
           (let ([result (make-vector n1 0)])
                (do ([i 0 (+ i 1)])
                    ((= i n1) result)
                    (vector-set! result i
                                 (f (vector-ref v1 i)
                                    (vector-ref v2 i))))))))

;;; vec-append : Vec a × Vec a → Vec a
;;; Concatenate two vectors.
(define (vec-append v1 v2)
  (let* ([n1 (vector-length v1)]
         [n2 (vector-length v2)]
         [result (make-vector (+ n1 n2) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n1))
            (vector-set! result i (vector-ref v1 i)))
        (do ([i 0 (+ i 1)])
            ((= i n2) result)
            (vector-set! result (+ n1 i) (vector-ref v2 i)))))

;;; vec-take : Vec a × Nat → Vec a | Error
;;; Take first n elements.
(define (vec-take v n)
  (let ([len (vector-length v)])
       (if (> n len)
           `(error out-of-bounds ,n ,len)
           (let ([result (make-vector n 0)])
                (do ([i 0 (+ i 1)])
                    ((= i n) result)
                    (vector-set! result i (vector-ref v i)))))))

;;; vec-drop : Vec a × Nat → Vec a | Error
;;; Drop first n elements.
(define (vec-drop v n)
  (let ([len (vector-length v)])
       (if (> n len)
           `(error out-of-bounds ,n ,len)
           (let* ([new-len (- len n)]
                  [result (make-vector new-len 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i new-len) result)
                     (vector-set! result i (vector-ref v (+ n i))))))))

;;; vec-slice : Vec a × Nat × Nat → Vec a | Error
;;; Extract slice from start (inclusive) to end (exclusive).
(define (vec-slice v start end)
  (let ([len (vector-length v)])
       (cond
        [(< start 0) `(error out-of-bounds ,start ,len)]
        [(> end len) `(error out-of-bounds ,end ,len)]
        [(> start end) `(error invalid-range ,start ,end)]
        [else
         (let* ([new-len (- end start)]
                [result (make-vector new-len 0)])
               (do ([i 0 (+ i 1)])
                   ((= i new-len) result)
                   (vector-set! result i (vector-ref v (+ start i)))))])))

;;; ============================================================
;;; Vector Arithmetic
;;; ============================================================

;;; vec-add : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise addition.
(define (vec-add v1 v2)
  (vec-zip-with + v1 v2))

;;; vec-sub : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise subtraction.
(define (vec-sub v1 v2)
  (vec-zip-with - v1 v2))

;;; vec-mul : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise multiplication (Hadamard product).
(define (vec-mul v1 v2)
  (vec-zip-with * v1 v2))

;;; vec-div : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise division.
(define (vec-div v1 v2)
  (vec-zip-with / v1 v2))

;;; vec-scale : Num × Vec Num → Vec Num
;;; Scalar multiplication.
(define (vec-scale k v)
  (vec-map (lambda (x) (* k x)) v))

;;; vec-negate : Vec Num → Vec Num
;;; Negate all elements.
(define (vec-negate v)
  (vec-map - v))

;;; ============================================================
;;; Vector Products and Norms
;;; ============================================================

;;; vec-dot : Vec Num × Vec Num → Num | Error
;;; Dot product (inner product).
(define (vec-dot v1 v2)
  (let ([n1 (vector-length v1)]
        [n2 (vector-length v2)])
       (if (not (= n1 n2))
           `(error dimension-mismatch ,n1 ,n2)
           (do ([i 0 (+ i 1)]
                [sum 0 (+ sum (* (vector-ref v1 i)
                                 (vector-ref v2 i)))])
               ((= i n1) sum)))))

;;; vec-norm-squared : Vec Num → Num
;;; Squared L2 norm (sum of squares).
(define (vec-norm-squared v)
  (vec-dot v v))

;;; vec-norm : Vec Num → Num
;;; L2 norm (Euclidean length).
(define (vec-norm v)
  (sqrt (vec-norm-squared v)))

;;; vec-norm-l1 : Vec Num → Num
;;; L1 norm (Manhattan distance).
(define (vec-norm-l1 v)
  (vec-fold (lambda (acc x) (+ acc (abs x))) 0 v))

;;; vec-norm-linf : Vec Num → Num | Error
;;; L-infinity norm (maximum absolute value).
;;; Returns error for empty vectors (consistent with vec-max).
(define (vec-norm-linf v)
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold (lambda (acc x) (max acc (abs x))) 0 v)))

;;; vec-normalize : Vec Num → Vec Num | Error
;;; Normalize to unit length.
(define (vec-normalize v)
  (let ([n (vec-norm v)])
       (if (= n 0)
           '(error zero-vector)
           (vec-scale (/ 1 n) v))))

;;; vec-distance : Vec Num × Vec Num → Num | Error
;;; Euclidean distance between two vectors.
(define (vec-distance v1 v2)
  (let ([diff (vec-sub v1 v2)])
       (if (and (pair? diff) (eq? (car diff) 'error))
           diff
           (vec-norm diff))))

;;; ============================================================
;;; Vector Comparisons
;;; ============================================================

;;; vec-equal? : Vec a × Vec a → Boolean
;;; Exact equality.
(define (vec-equal? v1 v2)
  (and (= (vector-length v1) (vector-length v2))
       (let ([n (vector-length v1)])
            (do ([i 0 (+ i 1)]
                 [eq #t (and eq (equal? (vector-ref v1 i)
                                        (vector-ref v2 i)))])
                ((or (= i n) (not eq)) eq)))))

;;; vec-approx-equal? : Vec Num × Vec Num × [Num] → Boolean
;;; Approximate equality within tolerance.
(define (vec-approx-equal? v1 v2 . epsilon-arg)
  (let ([epsilon (if (null? epsilon-arg) 1e-10 (car epsilon-arg))])
       (and (= (vector-length v1) (vector-length v2))
            (let ([n (vector-length v1)])
                 (do ([i 0 (+ i 1)]
                      [eq #t (and eq (< (abs (- (vector-ref v1 i)
                                                (vector-ref v2 i)))
                                        epsilon))])
                     ((or (= i n) (not eq)) eq))))))

;;; ============================================================
;;; Vector Utilities
;;; ============================================================

;;; vec-sum : Vec Num → Num
;;; Sum of all elements.
(define (vec-sum v)
  (vec-fold + 0 v))

;;; vec-product : Vec Num → Num
;;; Product of all elements.
(define (vec-product v)
  (vec-fold * 1 v))

;;; vec-mean : Vec Num → Num
;;; Arithmetic mean.
(define (vec-mean v)
  (let ([n (vector-length v)])
       (if (= n 0)
           0
           (/ (vec-sum v) n))))

;;; vec-min : Vec Num → Num | Error
;;; Minimum element.
(define (vec-min v)
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold min (vector-ref v 0) v)))

;;; vec-max : Vec Num → Num | Error
;;; Maximum element.
(define (vec-max v)
  (if (vec-empty? v)
      '(error empty-vector)
      (vec-fold max (vector-ref v 0) v)))

;;; vec-argmin : Vec Num → Nat | Error
;;; Index of minimum element.
(define (vec-argmin v)
  (if (vec-empty? v)
      '(error empty-vector)
      (let ([n (vector-length v)])
           (do ([i 1 (+ i 1)]
                [min-i 0 (if (< (vector-ref v i) (vector-ref v min-i)) i min-i)])
               ((= i n) min-i)))))

;;; vec-argmax : Vec Num → Nat | Error
;;; Index of maximum element.
(define (vec-argmax v)
  (if (vec-empty? v)
      '(error empty-vector)
      (let ([n (vector-length v)])
           (do ([i 1 (+ i 1)]
                [max-i 0 (if (> (vector-ref v i) (vector-ref v max-i)) i max-i)])
               ((= i n) max-i)))))

;;; vec-reverse : Vec a → Vec a
;;; Reverse a vector.
(define (vec-reverse v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (vector-ref v (- n 1 i))))))

;;; vec-copy : Vec a → Vec a
;;; Create a copy of a vector.
(define (vec-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (vector-ref v i)))))

;;; ============================================================
;;; Special Vectors
;;; ============================================================

;;; vec-zeros : Nat → Vec Num
;;; Vector of n zeros.
(define (vec-zeros n)
  (make-vector n 0))

;;; vec-ones : Nat → Vec Num
;;; Vector of n ones.
(define (vec-ones n)
  (make-vector n 1))

;;; vec-range : Nat → Vec Nat
;;; Vector of [0, 1, 2, ..., n-1].
(define (vec-range n)
  (let ([result (make-vector n 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (vector-set! result i i))))

;;; vec-linspace : Num × Num × Nat → Vec Num
;;; Vector of n evenly spaced values from start to end.
(define (vec-linspace start end n)
  (if (< n 2)
      (if (= n 1)
          (vector start)
          (vector))
      (let* ([step (/ (- end start) (- n 1))]
             [result (make-vector n 0)])
            (do ([i 0 (+ i 1)])
                ((= i n) result)
                (vector-set! result i (+ start (* i step)))))))

;;; vec-unit : Nat × Nat → Vec Num
;;; Unit vector: 1 at position k, 0 elsewhere.
(define (vec-unit n k)
  (let ([result (make-vector n 0)])
       (when (and (>= k 0) (< k n))
             (vector-set! result k 1))
       result))
