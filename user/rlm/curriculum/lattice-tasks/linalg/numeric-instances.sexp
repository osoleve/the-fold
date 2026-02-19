;;; numeric-instances.sexp — Development tasks for lattice/linalg/numeric-instances.ss
;;;
;;; Module: numeric-instances (tier 0, depends on prelude, vec, matrix)
;;; 480 lines, ~30 exported functions
;;; Numeric type class instances for Vec and Matrix: element-wise arithmetic,
;;; transcendental functions, scalar broadcasting, applicative operations.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement vec-signum
  ;; ──────────────────────────────────────────────
  ;; Element-wise signum function.

  (task
    (id "linalg/numeric-instances/vec-signum-impl")
    (module numeric-instances)
    (tier 0)
    (type implement)
    (description "Implement vec-signum: compute the element-wise signum of a vector. For each element x, return -1 if x < 0, 1 if x > 0, and 0 if x = 0. This is the sign function lifted to vectors. Use vec-map with a lambda that applies a cond expression to classify each element.")
    (context "Available: vec-map, cond, <, >, lambda. Single vec-map call with cond lambda.")
    (before "(define (vec-signum v)
  ;; TODO: element-wise signum (-1, 0, or 1)
  v)")
    (test-file "lattice/linalg/test-numeric-instances.ss")
    (test-forms (";; Mixed signs
(assert-equal '#(-1 0 1 -1 1) (vec-signum '#(-3 0 5 -1 7)))"
                 ";; All positive
(assert-equal '#(1 1 1) (vec-signum '#(1 2 3)))"
                 ";; All zeros
(assert-equal '#(0 0) (vec-signum '#(0 0)))"
                 ";; Single element
(assert-equal '#(-1) (vec-signum '#(-42)))"))
    (available-modules (prelude vec matrix))
    (hints ("Use vec-map with a lambda"
            "Inside the lambda: (cond [(< x 0) -1] [(> x 0) 1] [else 0])"
            "(vec-map (lambda (x) (cond ...)) v)"))
    (reference-solution "(define (vec-signum v)
  (vec-map (lambda (x)
             (cond [(< x 0) -1]
                   [(> x 0) 1]
                   [else 0]))
           v))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement vec-recip
  ;; ──────────────────────────────────────────────
  ;; Element-wise reciprocal.

  (task
    (id "linalg/numeric-instances/vec-recip-impl")
    (module numeric-instances)
    (tier 0)
    (type implement)
    (description "Implement vec-recip: compute the element-wise reciprocal (1/x) of a vector. This is the multiplicative inverse lifted to vectors, part of the Fractional type class instance. Use vec-map with a lambda that divides 1 by each element.")
    (context "Available: vec-map, /, lambda. Single vec-map call.")
    (before "(define (vec-recip v)
  ;; TODO: element-wise 1/x
  v)")
    (test-file "lattice/linalg/test-numeric-instances.ss")
    (test-forms (";; Integer reciprocals
(assert-equal '#(1/2 1/4 1/5) (vec-recip '#(2 4 5)))"
                 ";; Reciprocal of 1 is 1
(assert-equal '#(1 1 1) (vec-recip '#(1 1 1)))"
                 ";; Negative reciprocals
(assert-equal '#(-1/3 1/2) (vec-recip '#(-3 2)))"))
    (available-modules (prelude vec matrix))
    (hints ("Use vec-map: (vec-map (lambda (x) (/ 1 x)) v)"
            "Scheme exact arithmetic gives rationals: (/ 1 2) = 1/2"))
    (reference-solution "(define (vec-recip v)
  (vec-map (lambda (x) (/ 1 x)) v))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement scalar-vec+
  ;; ──────────────────────────────────────────────
  ;; Add scalar to each vector element.

  (task
    (id "linalg/numeric-instances/scalar-vec-plus-impl")
    (module numeric-instances)
    (tier 0)
    (type implement)
    (description "Implement scalar-vec+: add a scalar k to each element of a vector. This broadcasts a scalar across all elements, returning a new vector where each element is (+ k x_i). Broadcasting is essential for numeric computing — it lets scalars and vectors mix naturally in arithmetic expressions. Use vec-map with a lambda that adds k to each element.")
    (context "Available: vec-map, +, lambda. Single vec-map call with closure over k.")
    (before "(define (scalar-vec+ k v)
  ;; TODO: add k to each element
  v)")
    (test-file "lattice/linalg/test-numeric-instances.ss")
    (test-forms (";; Add 10 to each element
(assert-equal '#(11 12 13) (scalar-vec+ 10 '#(1 2 3)))"
                 ";; Add zero (identity)
(assert-equal '#(1 2 3) (scalar-vec+ 0 '#(1 2 3)))"
                 ";; Add negative
(assert-equal '#(-4 -3 -2) (scalar-vec+ -5 '#(1 2 3)))"))
    (available-modules (prelude vec matrix))
    (hints ("Use vec-map with a lambda that closes over k"
            "(vec-map (lambda (x) (+ k x)) v)"))
    (reference-solution "(define (scalar-vec+ k v)
  (vec-map (lambda (x) (+ k x)) v))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement matrix-hadamard
  ;; ──────────────────────────────────────────────
  ;; Element-wise matrix multiplication.

  (task
    (id "linalg/numeric-instances/matrix-hadamard-impl")
    (module numeric-instances)
    (tier 0)
    (type implement)
    (description "Implement matrix-hadamard: compute the Hadamard (element-wise) product of two matrices. For matrices A and B of the same dimensions, the result C has C_ij = A_ij * B_ij. This is NOT standard matrix multiplication — it multiplies corresponding elements. First check dimensions match, returning an error if they don't. Then allocate a result vector, loop over all elements multiplying corresponding pairs, and construct the result matrix. Use matrix-rows, matrix-cols, matrix-data to access matrix internals. A matrix is stored as (list 'matrix rows cols data-vector).")
    (context "Available: matrix-rows, matrix-cols, matrix-data, make-vector, vector-ref, vector-set!, *, =, or, not, do, list. Dimension check + single do loop.")
    (before "(define (matrix-hadamard m1 m2)
  ;; TODO: element-wise multiplication
  m1)")
    (test-file "lattice/linalg/test-numeric-instances.ss")
    (test-forms (";; 2x2 Hadamard product
(let ([m1 (list 'matrix 2 2 '#(1 2 3 4))]
      [m2 (list 'matrix 2 2 '#(5 6 7 8))])
  (assert-equal '(matrix 2 2 #(5 12 21 32)) (matrix-hadamard m1 m2)))"
                 ";; 1x3 Hadamard product
(let ([m1 (list 'matrix 1 3 '#(2 3 4))]
      [m2 (list 'matrix 1 3 '#(10 10 10))])
  (assert-equal '(matrix 1 3 #(20 30 40)) (matrix-hadamard m1 m2)))"
                 ";; Dimension mismatch
(let ([m1 (list 'matrix 2 2 '#(1 2 3 4))]
      [m2 (list 'matrix 2 3 '#(1 2 3 4 5 6))])
  (assert-true (pair? (matrix-hadamard m1 m2))))"))
    (available-modules (prelude vec matrix))
    (hints ("Check dimensions: (or (not (= r1 r2)) (not (= c1 c2))) → error"
            "Total elements: (* r1 c1)"
            "Loop: (do ([i 0 (+ i 1)]) ((= i total)) (vector-set! result i (* (vector-ref data1 i) (vector-ref data2 i))))"
            "Result: (list 'matrix r1 c1 result)"))
    (reference-solution "(define (matrix-hadamard m1 m2)
  (let ([r1 (matrix-rows m1)]
        [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)]
        [c2 (matrix-cols m2)])
    (if (or (not (= r1 r2)) (not (= c1 c2)))
        `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
        (let ([data1 (matrix-data m1)]
              [data2 (matrix-data m2)]
              [result (make-vector (* r1 c1) 0)])
          (do ([i 0 (+ i 1)])
              ((= i (* r1 c1)))
            (vector-set! result i (* (vector-ref data1 i)
                                     (vector-ref data2 i))))
          (list 'matrix r1 c1 result)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement vec-ap
  ;; ──────────────────────────────────────────────
  ;; Applicative functor with broadcasting.

  (task
    (id "linalg/numeric-instances/vec-ap-impl")
    (module numeric-instances)
    (tier 0)
    (type implement)
    (description "Implement vec-ap: apply a vector of functions to a vector of values, with broadcasting support. There are three cases: (1) Equal lengths — apply each function to its corresponding value element-wise. (2) Single function — broadcast the one function across all values. (3) Single value — broadcast the one value across all functions. If neither operand has length 1 and lengths don't match, return a dimension mismatch error. Use vec-length, cond with three matching clauses, and a do loop in each case to construct the result vector.")
    (context "Available: vec-length, vector-ref, make-vector, vector-set!, =, cond, do, let. Three-case cond with do loops.")
    (before "(define (vec-ap fs xs)
  ;; TODO: apply vector of functions to vector of values
  xs)")
    (test-file "lattice/linalg/test-numeric-instances.ss")
    (test-forms (";; Equal lengths: apply element-wise
(assert-equal '#(11 19) (vec-ap (vector add1 sub1) '#(10 20)))"
                 ";; Single function broadcasts
(assert-equal '#(11 21 31) (vec-ap (vector add1) '#(10 20 30)))"
                 ";; Single value broadcasts
(assert-equal '#(6 4 -5) (vec-ap (vector add1 sub1 -) '#(5)))"
                 ";; Dimension mismatch
(let ([result (vec-ap (vector add1 sub1) '#(1 2 3))])
  (assert-true (pair? result)))"))
    (available-modules (prelude vec matrix))
    (hints ("Get lengths: (let ([nf (vec-length fs)] [nx (vec-length xs)]) ...)"
            "Case 1: (= nf nx) — standard element-wise"
            "Case 2: (= nf 1) — extract single function, apply to all xs"
            "Case 3: (= nx 1) — extract single value, apply all fs to it"
            "Each case: allocate result, do loop, return result"))
    (reference-solution "(define (vec-ap fs xs)
  (let ([nf (vec-length fs)]
        [nx (vec-length xs)])
    (cond
      [(= nf nx)
       (let ([result (make-vector nf 0)])
         (do ([i 0 (+ i 1)])
             ((= i nf) result)
           (vector-set! result i ((vector-ref fs i) (vector-ref xs i)))))]
      [(= nf 1)
       (let ([f (vector-ref fs 0)]
             [result (make-vector nx 0)])
         (do ([i 0 (+ i 1)])
             ((= i nx) result)
           (vector-set! result i (f (vector-ref xs i)))))]
      [(= nx 1)
       (let ([x (vector-ref xs 0)]
             [result (make-vector nf 0)])
         (do ([i 0 (+ i 1)])
             ((= i nf) result)
           (vector-set! result i ((vector-ref fs i) x))))]
      [else `(error dimension-mismatch ,nf ,nx)])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
