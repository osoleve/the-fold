;;; fabric/stitches/fp/vec-matrix-instances.ss -- Type Class Instances for Vec and Matrix
;;;
;;; Provides Functor, Foldable, Traversable, and Applicative instances
;;; for Vec (vector) and Matrix types.
;;;
;;; This enables generic functional programming over numeric containers:
;;;   (fmap sqrt vec)           ; Apply function to each element
;;;   (traverse validate vec)   ; Traverse with effects
;;;   (fold + 0 matrix)         ; Reduce to single value
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss
;;;   - fp/traversable.ss

(load "core/prelude.ss")
(load "core/vec.ss")
(load "core/matrix.ss")
(load "core/fp/functors/traversable.ss")

;;; ============================================================
;;; Vec Functor Instance
;;; ============================================================

;;; vec-fmap : (a -> b) -> Vec a -> Vec b
;;; Apply a function to each element of a vector.
(define (vec-fmap f v)
  (let* ([n (vec-length v)]
         [result (make-vec n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (f (vec-ref v i))))))

;;; vec-functor : Functor Vec
;;; The Functor instance dictionary for Vec.
(define vec-functor
  (cons 'vec-functor vec-fmap))

;;; ============================================================
;;; Vec Foldable Instance
;;; ============================================================

;;; vec-foldr : (a -> b -> b) -> b -> Vec a -> b
;;; Right fold over a vector.
(define (vec-foldr f init v)
  (let ([n (vec-length v)])
       (let loop ([i (- n 1)] [acc init])
            (if (< i 0)
                acc
                (loop (- i 1) (f (vec-ref v i) acc))))))

;;; vec-foldl : (b -> a -> b) -> b -> Vec a -> b
;;; Left fold over a vector.
(define (vec-foldl f init v)
  (let ([n (vec-length v)])
       (let loop ([i 0] [acc init])
            (if (>= i n)
                acc
                (loop (+ i 1) (f acc (vec-ref v i)))))))

;;; vec-fold-map : Monoid m => (a -> m) -> Vec a -> m
;;; Map each element to a monoid and combine.
(define (vec-fold-map monoid f v)
  (vec-foldl (lambda (acc x) ((monoid-append monoid) acc (f x)))
             (monoid-empty monoid)
             v))

;;; vec-foldable : Foldable Vec
(define vec-foldable
  (list 'vec-foldable vec-foldr vec-foldl vec-fold-map))

;;; Derived Foldable operations for Vec

;;; vec-sum : Num a => Vec a -> a
(define (vec-sum v)
  (vec-foldl + 0 v))

;;; vec-product : Num a => Vec a -> a
(define (vec-product v)
  (vec-foldl * 1 v))

;;; vec-any : (a -> Bool) -> Vec a -> Bool
(define (vec-any pred v)
  (let ([n (vec-length v)])
       (let loop ([i 0])
            (cond
             [(>= i n) #f]
             [(pred (vec-ref v i)) #t]
             [else (loop (+ i 1))]))))

;;; vec-all : (a -> Bool) -> Vec a -> Bool
(define (vec-all pred v)
  (let ([n (vec-length v)])
       (let loop ([i 0])
            (cond
             [(>= i n) #t]
             [(not (pred (vec-ref v i))) #f]
             [else (loop (+ i 1))]))))

;;; vec-find : (a -> Bool) -> Vec a -> Maybe a
(define (vec-find pred v)
  (let ([n (vec-length v)])
       (let loop ([i 0])
            (cond
             [(>= i n) nothing]
             [(pred (vec-ref v i)) (just (vec-ref v i))]
             [else (loop (+ i 1))]))))

;;; vec-maximum : Ord a => Vec a -> Maybe a
(define (vec-maximum v)
  (if (vec-empty? v)
      nothing
      (just (vec-foldl (lambda (acc x) (if (> x acc) x acc))
                       (vec-ref v 0)
                       v))))

;;; vec-minimum : Ord a => Vec a -> Maybe a
(define (vec-minimum v)
  (if (vec-empty? v)
      nothing
      (just (vec-foldl (lambda (acc x) (if (< x acc) x acc))
                       (vec-ref v 0)
                       v))))

;;; ============================================================
;;; Vec Traversable Instance
;;; ============================================================

;;; vec-traverse : Applicative f => (a -> f b) -> Vec a -> f (Vec b)
;;; Traverse a vector with an effectful function.
(define (vec-traverse app f v)
  (let ([n (vec-length v)]
        [pure (app-pure app)]
        [ap (app-ap app)])
       (if (= n 0)
           (pure (vec))
           ;; Build up the result by traversing each element
           (let loop ([i 0] [acc (pure (vec))])
                (if (>= i n)
                    acc
                    (let ([fb (f (vec-ref v i))])
                         ;; Combine: append this element to accumulated vector
                         (loop (+ i 1)
                               (ap (ap (pure (lambda (vs)
                                                     (lambda (x)
                                                             (vec-append vs (vec x)))))
                                       acc)
                                   fb))))))))

;;; vec-sequence : Applicative f => Vec (f a) -> f (Vec a)
;;; Sequence effectful values in a vector.
(define (vec-sequence app v)
  (vec-traverse app identity v))

;;; vec-traversable : Traversable Vec
(define vec-traversable
  (list 'vec-traversable vec-traverse vec-sequence vec-foldable))

;;; Helper: vec-append
(define (vec-append v1 v2)
  (let* ([n1 (vec-length v1)]
         [n2 (vec-length v2)]
         [result (make-vec (+ n1 n2) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n1))
            (vector-set! result i (vec-ref v1 i)))
        (do ([i 0 (+ i 1)])
            ((= i n2) result)
            (vector-set! result (+ n1 i) (vec-ref v2 i)))))

;;; ============================================================
;;; Vec Applicative Instance
;;; ============================================================

;;; vec-pure : a -> Vec a
;;; Lift a value into a singleton vector.
(define (vec-pure x)
  (vec x))

;;; vec-ap : Vec (a -> b) -> Vec a -> Vec b
;;; Apply functions in a vector to values in another vector.
;;; Uses zipWith semantics (element-wise application).
(define (vec-ap vf va)
  (let ([n (min (vec-length vf) (vec-length va))])
       (let ([result (make-vec n 0)])
            (do ([i 0 (+ i 1)])
                ((= i n) result)
                (vector-set! result i ((vec-ref vf i) (vec-ref va i)))))))

;;; vec-applicative : Applicative Vec
(define vec-applicative
  (make-applicative vec-pure vec-ap))

;;; vec-lift2 : (a -> b -> c) -> Vec a -> Vec b -> Vec c
;;; Lift a binary function to vectors (zipWith).
(define (vec-lift2 f va vb)
  (let ([n (min (vec-length va) (vec-length vb))])
       (let ([result (make-vec n 0)])
            (do ([i 0 (+ i 1)])
                ((= i n) result)
                (vector-set! result i (f (vec-ref va i) (vec-ref vb i)))))))

;;; vec-zip : Vec a -> Vec b -> Vec (a, b)
(define (vec-zip va vb)
  (vec-lift2 cons va vb))

;;; ============================================================
;;; Matrix Functor Instance
;;; ============================================================

;;; matrix-fmap : (a -> b) -> Matrix a -> Matrix b
;;; Apply a function to each element of a matrix.
(define (matrix-fmap f m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [n (* rows cols)]
         [new-data (make-vec n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! new-data i (f (vector-ref data i))))
        (list 'matrix rows cols new-data)))

;;; matrix-functor : Functor Matrix
(define matrix-functor
  (cons 'matrix-functor matrix-fmap))

;;; ============================================================
;;; Matrix Foldable Instance
;;; ============================================================

;;; matrix-foldr : (a -> b -> b) -> b -> Matrix a -> b
;;; Right fold over all elements of a matrix (row-major order).
(define (matrix-foldr f init m)
  (vec-foldr f init (matrix-data m)))

;;; matrix-foldl : (b -> a -> b) -> b -> Matrix a -> b
;;; Left fold over all elements of a matrix.
(define (matrix-foldl f init m)
  (vec-foldl f init (matrix-data m)))

;;; matrix-fold-map : Monoid m => (a -> m) -> Matrix a -> m
(define (matrix-fold-map monoid f m)
  (vec-fold-map monoid f (matrix-data m)))

;;; matrix-foldable : Foldable Matrix
(define matrix-foldable
  (list 'matrix-foldable matrix-foldr matrix-foldl matrix-fold-map))

;;; Derived Foldable operations for Matrix

;;; matrix-sum : Num a => Matrix a -> a
(define (matrix-sum m)
  (vec-sum (matrix-data m)))

;;; matrix-product : Num a => Matrix a -> a
(define (matrix-product m)
  (vec-product (matrix-data m)))

;;; matrix-any : (a -> Bool) -> Matrix a -> Bool
(define (matrix-any pred m)
  (vec-any pred (matrix-data m)))

;;; matrix-all : (a -> Bool) -> Matrix a -> Bool
(define (matrix-all pred m)
  (vec-all pred (matrix-data m)))

;;; matrix-maximum : Ord a => Matrix a -> Maybe a
(define (matrix-maximum m)
  (vec-maximum (matrix-data m)))

;;; matrix-minimum : Ord a => Matrix a -> Maybe a
(define (matrix-minimum m)
  (vec-minimum (matrix-data m)))

;;; ============================================================
;;; Matrix Traversable Instance
;;; ============================================================

;;; matrix-traverse : Applicative f => (a -> f b) -> Matrix a -> f (Matrix b)
;;; Traverse all elements of a matrix with an effectful function.
(define (matrix-traverse app f m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [pure (app-pure app)]
         [ap (app-ap app)])
        ;; Traverse the underlying data vector, then wrap in matrix
        (ap (pure (lambda (new-data) (list 'matrix rows cols new-data)))
            (vec-traverse app f data))))

;;; matrix-sequence : Applicative f => Matrix (f a) -> f (Matrix a)
(define (matrix-sequence app m)
  (matrix-traverse app identity m))

;;; matrix-traversable : Traversable Matrix
(define matrix-traversable
  (list 'matrix-traversable matrix-traverse matrix-sequence matrix-foldable))

;;; ============================================================
;;; Matrix Applicative Instance
;;; ============================================================

;;; Note: Matrix Applicative uses element-wise operations (like NumPy broadcasting
;;; with same-shape matrices). This is the ZipMatrix applicative.

;;; matrix-pure : a -> Matrix a
;;; Create a 1x1 matrix containing a single value.
(define (matrix-pure x)
  (matrix-from-lists (list (list x))))

;;; matrix-ap : Matrix (a -> b) -> Matrix a -> Matrix b
;;; Element-wise application (requires same shape).
(define (matrix-ap mf ma)
  (let* ([rows (matrix-rows ma)]
         [cols (matrix-cols ma)]
         [df (matrix-data mf)]
         [da (matrix-data ma)]
         [n (* rows cols)]
         [result (make-vec n 0)])
        (if (and (= rows (matrix-rows mf))
                 (= cols (matrix-cols mf)))
            (begin
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (vector-set! result i ((vector-ref df i) (vector-ref da i))))
             (list 'matrix rows cols result))
            '(error shape-mismatch))))

;;; matrix-applicative : Applicative Matrix
(define matrix-applicative
  (make-applicative matrix-pure matrix-ap))

;;; matrix-lift2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
;;; Lift a binary function to matrices (element-wise).
(define (matrix-lift2 f ma mb)
  (let* ([rows (matrix-rows ma)]
         [cols (matrix-cols ma)]
         [da (matrix-data ma)]
         [db (matrix-data mb)]
         [n (* rows cols)]
         [result (make-vec n 0)])
        (if (and (= rows (matrix-rows mb))
                 (= cols (matrix-cols mb)))
            (begin
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (vector-set! result i (f (vector-ref da i) (vector-ref db i))))
             (list 'matrix rows cols result))
            '(error shape-mismatch))))

;;; ============================================================
;;; Row/Column Operations via Folds
;;; ============================================================

;;; matrix-fold-rows : (a -> b -> b) -> b -> Matrix a -> Vec b
;;; Fold each row independently.
(define (matrix-fold-rows f init m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vec rows 0)])
        (do ([r 0 (+ r 1)])
            ((= r rows) result)
            (let ([row-result
                   (let loop ([c (- cols 1)] [acc init])
                        (if (< c 0)
                            acc
                            (loop (- c 1) (f (vector-ref data (+ (* r cols) c)) acc))))])
                 (vector-set! result r row-result)))))

;;; matrix-fold-cols : (a -> b -> b) -> b -> Matrix a -> Vec b
;;; Fold each column independently.
(define (matrix-fold-cols f init m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vec cols 0)])
        (do ([c 0 (+ c 1)])
            ((= c cols) result)
            (let ([col-result
                   (let loop ([r (- rows 1)] [acc init])
                        (if (< r 0)
                            acc
                            (loop (- r 1) (f (vector-ref data (+ (* r cols) c)) acc))))])
                 (vector-set! result c col-result)))))

;;; matrix-row-sums : Num a => Matrix a -> Vec a
(define (matrix-row-sums m)
  (matrix-fold-rows + 0 m))

;;; matrix-col-sums : Num a => Matrix a -> Vec a
(define (matrix-col-sums m)
  (matrix-fold-cols + 0 m))

;;; matrix-row-maxes : Ord a => Matrix a -> Vec a
(define (matrix-row-maxes m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vec rows 0)])
        (do ([r 0 (+ r 1)])
            ((= r rows) result)
            (let ([first-elem (vector-ref data (* r cols))])
                 (vector-set! result r
                              (let loop ([c 1] [mx first-elem])
                                   (if (>= c cols)
                                       mx
                                       (let ([elem (vector-ref data (+ (* r cols) c))])
                                            (loop (+ c 1) (if (> elem mx) elem mx))))))))))

;;; matrix-col-maxes : Ord a => Matrix a -> Vec a
(define (matrix-col-maxes m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vec cols 0)])
        (do ([c 0 (+ c 1)])
            ((= c cols) result)
            (let ([first-elem (vector-ref data c)])
                 (vector-set! result c
                              (let loop ([r 1] [mx first-elem])
                                   (if (>= r rows)
                                       mx
                                       (let ([elem (vector-ref data (+ (* r cols) c))])
                                            (loop (+ r 1) (if (> elem mx) elem mx))))))))))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Vec Instances:
;;;   vec-functor, vec-fmap
;;;   vec-foldable, vec-foldr, vec-foldl, vec-fold-map
;;;   vec-traversable, vec-traverse, vec-sequence
;;;   vec-applicative, vec-pure, vec-ap, vec-lift2, vec-zip
;;;
;;; Vec Derived Operations:
;;;   vec-sum, vec-product, vec-any, vec-all, vec-find
;;;   vec-maximum, vec-minimum, vec-append
;;;
;;; Matrix Instances:
;;;   matrix-functor, matrix-fmap
;;;   matrix-foldable, matrix-foldr, matrix-foldl, matrix-fold-map
;;;   matrix-traversable, matrix-traverse, matrix-sequence
;;;   matrix-applicative, matrix-pure, matrix-ap, matrix-lift2
;;;
;;; Matrix Derived Operations:
;;;   matrix-sum, matrix-product, matrix-any, matrix-all
;;;   matrix-maximum, matrix-minimum
;;;   matrix-fold-rows, matrix-fold-cols
;;;   matrix-row-sums, matrix-col-sums
;;;   matrix-row-maxes, matrix-col-maxes
