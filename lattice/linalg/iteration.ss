;;; lattice/linalg/iteration.ss — Iteration Macros
;;; @module iteration
;;; @requires prelude

(require 'prelude)

(doc 'module 'iteration)
(doc 'description "Vector and matrix iteration macros for linalg")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Provides high-level iteration abstractions to replace repetitive do loops")
(doc 'note "These macros expand to efficient iterative code without abstraction overhead")
(doc 'note "No runtime overhead, just cleaner code")
(doc 'note "Pure macros (requires prelude for doc primitive)")

(doc 'section 'vector-iteration)

(doc vec-do! 'type '(syntax (idx-var vector body ...) unspecified))
(doc vec-do! 'description "Execute body for each index in vector, binding idx-var")
(doc vec-do! 'note "Use for side-effecting operations on external targets")
(doc vec-do! 'example "(vec-do! i v (display (vector-ref v i)))")
(define-syntax vec-do!
  (syntax-rules ()
                [(_ idx vec body ...)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([idx 0 (+ idx 1)])
                          ((= idx n))
                          body ...))]))

;;; vec-map-idx! : (idx-var src-vec result-vec body) -> result-vec
;;; Map over source vector with index, storing results in result vector.
;;; Body should evaluate to the value to store at each index.
;;;
;;; Example:
;;;   (vec-map-idx! i src result (+ (vector-ref src i) 1))
(define-syntax vec-map-idx!
  (syntax-rules ()
                [(_ idx src result body)
                 (let ([s src]
                       [r result]
                       [n (vector-length src)])
                      (do ([idx 0 (+ idx 1)])
                          ((= idx n) r)
                          (vector-set! r idx body)))]))

;;; vec-map-idx : (idx-var vec body) -> new-vector
;;; Map over vector with index access, creating a new result vector.
;;; More convenient when you don't need to pre-allocate.
;;;
;;; Example:
;;;   (vec-map-idx i v (+ (vector-ref v i) i))  ; add index to element
(define-syntax vec-map-idx
  (syntax-rules ()
                [(_ idx vec body)
                 (let* ([v vec]
                        [n (vector-length v)]
                        [result (make-vector n 0)])
                       (do ([idx 0 (+ idx 1)])
                           ((= idx n) result)
                           (vector-set! result idx body)))]))

;;; vec-fold-idx : (acc-var init idx-var vec body) -> final-acc
;;; Fold over vector with index access. Body computes new accumulator.
;;;
;;; Example:
;;;   (vec-fold-idx sum 0 i v (+ sum (* i (vector-ref v i))))
(define-syntax vec-fold-idx
  (syntax-rules ()
                [(_ acc init idx vec body)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([idx 0 (+ idx 1)]
                           [acc init body])
                          ((= idx n) acc)))]))

;;; vec-fold : (acc-var init elem-var vec body) -> final-acc
;;; Fold over vector elements (no index). Body computes new accumulator.
;;;
;;; Example:
;;;   (vec-fold sum 0 x v (+ sum x))
(define-syntax vec-fold
  (syntax-rules ()
                [(_ acc init elem vec body)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([i 0 (+ i 1)]
                           [acc init (let ([elem (vector-ref v i)]) body)])
                          ((= i n) acc)))]))

;;; vec-zip-do! : (idx-var vec1 vec2 body ...) -> unspecified
;;; Iterate over two vectors simultaneously by index.
;;;
;;; Example:
;;;   (vec-zip-do! i v1 v2
;;;     (vector-set! result i (+ (vector-ref v1 i) (vector-ref v2 i))))
(define-syntax vec-zip-do!
  (syntax-rules ()
                [(_ idx vec1 vec2 body ...)
                 (let ([v1 vec1]
                       [v2 vec2]
                       [n (vector-length vec1)])
                      (do ([idx 0 (+ idx 1)])
                          ((= idx n))
                          body ...))]))

;;; vec-zip-map-idx : (idx-var vec1 vec2 body) -> new-vector
;;; Map over two vectors by index, creating result.
;;;
;;; Example:
;;;   (vec-zip-map-idx i v1 v2 (+ (vector-ref v1 i) (vector-ref v2 i)))
(define-syntax vec-zip-map-idx
  (syntax-rules ()
                [(_ idx vec1 vec2 body)
                 (let* ([v1 vec1]
                        [v2 vec2]
                        [n (vector-length v1)]
                        [result (make-vector n 0)])
                       (do ([idx 0 (+ idx 1)])
                           ((= idx n) result)
                           (vector-set! result idx body)))]))

;;; vec-any? : (elem-var vec pred) -> boolean
;;; Check if any element satisfies predicate.
;;;
;;; Example:
;;;   (vec-any? x v (> x 0))
(define-syntax vec-any?
  (syntax-rules ()
                [(_ elem vec pred)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([i 0 (+ i 1)]
                           [found #f (or found (let ([elem (vector-ref v i)]) pred))])
                          ((or (= i n) found) found)))]))

;;; vec-all? : (elem-var vec pred) -> boolean
;;; Check if all elements satisfy predicate.
;;;
;;; Example:
;;;   (vec-all? x v (> x 0))
(define-syntax vec-all?
  (syntax-rules ()
                [(_ elem vec pred)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([i 0 (+ i 1)]
                           [all #t (and all (let ([elem (vector-ref v i)]) pred))])
                          ((or (= i n) (not all)) all)))]))

;;; ====
;;; Matrix Iteration Macros
;;; ====

;;; matrix-do! : (i-var j-var rows cols body ...) -> unspecified
;;; Iterate over matrix indices (row-major order).
;;;
;;; Example:
;;;   (matrix-do! i j rows cols
;;;     (vector-set! data (+ (* i cols) j) 0))
(define-syntax matrix-do!
  (syntax-rules ()
                [(_ i j rows cols body ...)
                 (let ([r rows]
                       [c cols])
                      (do ([i 0 (+ i 1)])
                          ((= i r))
                          (do ([j 0 (+ j 1)])
                              ((= j c))
                              body ...)))]))

;;; matrix-map-ij! : (i-var j-var rows cols data result body) -> result
;;; Map over matrix with (i, j) access, storing results.
;;; Body should reference data and evaluate to value for result.
;;;
;;; Example:
;;;   (matrix-map-ij! i j r c src-data dst-data
;;;     (+ (vector-ref src-data (+ (* i c) j)) 1))
(define-syntax matrix-map-ij!
  (syntax-rules ()
                [(_ i j rows cols data result body)
                 (let ([r rows]
                       [c cols]
                       [d data]
                       [res result])
                      (do ([i 0 (+ i 1)])
                          ((= i r) res)
                          (do ([j 0 (+ j 1)])
                              ((= j c))
                              (vector-set! res (+ (* i c) j) body))))]))

;;; matrix-fold-ij : (acc-var init i-var j-var rows cols data body) -> final-acc
;;; Fold over matrix with (i, j) access.
;;;
;;; Example:
;;;   (matrix-fold-ij sum 0 i j r c data
;;;     (+ sum (vector-ref data (+ (* i c) j))))
(define-syntax matrix-fold-ij
  (syntax-rules ()
                [(_ acc init i j rows cols data body)
                 (let ([r rows]
                       [c cols]
                       [d data])
                      (do ([i 0 (+ i 1)]
                           [acc init
                                (do ([j 0 (+ j 1)]
                                     [inner-acc acc
                                                (let () body)])
                                    ((= j c) inner-acc))])
                          ((= i r) acc)))]))

;;; matrix-row-do! : (j-var row-idx cols data body ...) -> unspecified
;;; Iterate over a single row of a matrix.
;;;
;;; Example:
;;;   (matrix-row-do! j 2 cols data
;;;     (vector-set! result j (vector-ref data (+ start j))))
(define-syntax matrix-row-do!
  (syntax-rules ()
                [(_ j row cols data body ...)
                 (let ([c cols]
                       [start (* row cols)]
                       [d data])
                      (do ([j 0 (+ j 1)])
                          ((= j c))
                          body ...))]))

;;; matrix-col-do! : (i-var col-idx rows cols data body ...) -> unspecified
;;; Iterate over a single column of a matrix.
;;;
;;; Example:
;;;   (matrix-col-do! i 0 rows cols data
;;;     (vector-set! result i (vector-ref data (+ (* i cols) col))))
(define-syntax matrix-col-do!
  (syntax-rules ()
                [(_ i col rows cols data body ...)
                 (let ([r rows]
                       [c cols]
                       [co col]
                       [d data])
                      (do ([i 0 (+ i 1)])
                          ((= i r))
                          body ...))]))

;;; ====
;;; Range Iteration
;;; ====

;;; range-do! : (var start end body ...) -> unspecified
;;; Execute body for each value from start to end-1.
;;;
;;; Example:
;;;   (range-do! i 0 10 (display i))
(define-syntax range-do!
  (syntax-rules ()
                [(_ var start end body ...)
                 (do ([var start (+ var 1)])
                     ((= var end))
                     body ...)]))

;;; range-fold : (acc-var init var start end body) -> final-acc
;;; Fold over a range of integers.
;;;
;;; Example:
;;;   (range-fold sum 0 i 1 11 (+ sum i))  ; sum of 1..10 = 55
(define-syntax range-fold
  (syntax-rules ()
                [(_ acc init var start end body)
                 (do ([var start (+ var 1)]
                      [acc init body])
                     ((= var end) acc))]))

;;; ====
;;; Reverse Iteration
;;; ====

;;; vec-do-reverse! : (idx-var vec body ...) -> unspecified
;;; Iterate over vector indices in reverse order (n-1 down to 0).
;;;
;;; Example:
;;;   (vec-do-reverse! i v (display (vector-ref v i)))
(define-syntax vec-do-reverse!
  (syntax-rules ()
                [(_ idx vec body ...)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([idx (- n 1) (- idx 1)])
                          ((< idx 0))
                          body ...))]))

;;; vec-fold-reverse : (acc-var init idx-var vec body) -> final-acc
;;; Fold over vector in reverse order (right fold).
;;;
;;; Example:
;;;   (vec-fold-reverse lst '() i v (cons (vector-ref v i) lst))
(define-syntax vec-fold-reverse
  (syntax-rules ()
                [(_ acc init idx vec body)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([idx (- n 1) (- idx 1)]
                           [acc init body])
                          ((< idx 0) acc)))]))

;;; ====
;;; Conditional Iteration with Early Exit
;;; ====

;;; vec-find-idx : (idx-var vec pred) -> index or #f
;;; Find first index where predicate holds.
;;;
;;; Example:
;;;   (vec-find-idx i v (> (vector-ref v i) 5))
(define-syntax vec-find-idx
  (syntax-rules ()
                [(_ idx vec pred)
                 (let ([v vec]
                       [n (vector-length vec)])
                      (do ([idx 0 (+ idx 1)]
                           [found #f (if pred idx #f)])
                          ((or (= idx n) found) found)))]))

;;; ====
;;; Triple-Nested Loops (Matrix Multiplication Pattern)
;;; ====

;;; matrix-mul-do! : (i-var j-var k-var r1 c1 c2 body ...) -> unspecified
;;; Triple nested loop for matrix multiplication pattern.
;;; i: 0..r1-1, j: 0..c2-1, k: 0..c1-1
;;;
;;; Example:
;;;   (matrix-mul-do! i j k r1 c1 c2
;;;     (set! sum (+ sum (* (matrix-ref A i k) (matrix-ref B k j)))))
(define-syntax matrix-mul-do!
  (syntax-rules ()
                [(_ i j k rows inner outer body ...)
                 (let ([r rows]
                       [m inner]
                       [c outer])
                      (do ([i 0 (+ i 1)])
                          ((= i r))
                          (do ([j 0 (+ j 1)])
                              ((= j c))
                              (do ([k 0 (+ k 1)])
                                  ((= k m))
                                  body ...))))]))

;;; ====
;;; Accumulating Inner Products
;;; ====

;;; dot-product-loop : (k-var len get-a get-b) -> sum
;;; Compute inner product using expressions for getting elements.
;;; This is a common pattern in matrix-vector and matrix-matrix multiply.
;;;
;;; Example:
;;;   (dot-product-loop k n
;;;     (vector-ref a (+ (* i cols) k))
;;;     (vector-ref b k))
(define-syntax dot-product-loop
  (syntax-rules ()
                [(_ k len get-a get-b)
                 (do ([k 0 (+ k 1)]
                      [sum 0 (+ sum (* get-a get-b))])
                     ((= k len) sum))]))

;;; ====
;;; Vector Construction Combinators
;;; ====
;;;
;;; These encapsulate the "make-vector + do + vector-set!" pattern
;;; behind a pure interface. The Rust codegen will pattern-match on
;;; these to emit optimized kernels.

(doc 'section 'vector-construction-combinators)

;;; vec-tabulate : (size idx-var body) -> vector
;;; Build a new vector of given size where each element is computed
;;; from its index.
;;;
;;; Replaces: (let ([r (make-vector n 0)]) (do ([i 0 (+ i 1)]) ((= i n) r) (vector-set! r i expr)))
;;;
;;; Example:
;;;   (vec-tabulate 5 i (* i i))          ; => #(0 1 4 9 16)
;;;   (vec-tabulate n i (vector-ref u i))  ; copy of u
(define-syntax vec-tabulate
  (syntax-rules ()
                [(_ size idx body)
                 (let* ([len size]
                        [result (make-vector len 0)])
                       (do ([idx 0 (+ idx 1)])
                           ((= idx len) result)
                           (vector-set! result idx body)))]))

;;; vec-scan : (size init idx-var acc-var body) -> vector
;;; Left-to-right accumulation producing a vector. Element 0 gets init,
;;; each subsequent element is computed from the previous accumulator.
;;; Body should compute the next accumulator value; it's also stored
;;; at position idx.
;;;
;;; Replaces: (vector-set! v 0 init) (do ([t 1 ...] [acc init expr]) ((= t n)) (vector-set! v t acc))
;;;
;;; Example:
;;;   (vec-scan 5 0 i acc (+ acc i))       ; prefix sums: #(0 1 3 6 10)
;;;   (vec-scan n x0 t prev (* alpha (vector-ref xs t) + (- 1 alpha) prev))
(define-syntax vec-scan
  (syntax-rules ()
                [(_ size init idx acc body)
                 (let* ([len size]
                        [result (make-vector len 0)])
                       (vector-set! result 0 init)
                       (let loop ([idx 1] [acc init])
                            (if (= idx len)
                                result
                                (let ([acc body])
                                     (vector-set! result idx acc)
                                     (loop (+ idx 1) acc)))))]))
