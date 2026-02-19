;;; vec.sexp — Development tasks for lattice/linalg/vec.ss
;;;
;;; Module: vec (tier 0, depends on prelude, iteration)
;;; 362 lines, ~35 exported functions
;;; N-dimensional vectors using Scheme vectors.
;;; Well-tested: test-vec.ss (275 lines, ~40 tests).
;;;
;;; Git history:
;;;   2f69645d docs(linalg): Convert vec.ss to doc form annotations
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require
;;;   d1ae3197 fix: Patch 12 P1 algorithmic and mathematical bugs

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement vec-dot (dot product)
  ;; ──────────────────────────────────────────────
  ;; The inner product — foundation for norms, projections, and
  ;; everything else in linalg. Must handle dimension mismatch.

  (task
    (id "linalg/vec/dot-impl")
    (module vec)
    (tier 0)
    (type implement)
    (description "Implement vec-dot: compute the dot product (inner product) of two vectors. Sum the products of corresponding elements. Return an error value if the vectors have different lengths. Use (vector-ref v i), (vector-length v), and a loop to accumulate the sum.")
    (context "Available: prelude, vector-ref, vector-length, make-vector. Error convention: return (list 'error 'dimension-mismatch n1 n2) for mismatched lengths. Vectors are Scheme vectors (created with (vector 1 2 3)).")
    (before "(define (vec-dot v1 v2)
  (doc 'type '(-> (Vec Num) (Vec Num) (Or Num Error)))
  (doc 'description \"Dot product (inner product)\")
  ;; TODO: sum of element-wise products
  0)")
    (test-file "lattice/linalg/test-vec.ss")
    (test-forms ("(assert-equal 32 (vec-dot (vector 1 2 3) (vector 4 5 6)))"
                 "(assert-equal 14 (vec-dot (vector 1 2 3) (vector 1 2 3)))"
                 "(assert-equal 0 (vec-dot (vector 1 0) (vector 0 1)))"
                 "(assert-true (pair? (vec-dot (vector 1 2) (vector 1 2 3))))"))
    (available-modules (prelude))
    (hints ("Check lengths first: (= (vector-length v1) (vector-length v2))"
            "Use a do-loop with accumulator: (do ([i 0 (+ i 1)] [sum 0 (+ sum (* ...))]) ...)"
            "Error return: (list 'error 'dimension-mismatch n1 n2)"))
    (reference-solution "(define (vec-dot v1 v2)
  (let ([n1 (vector-length v1)]
        [n2 (vector-length v2)])
       (if (not (= n1 n2))
           (list 'error 'dimension-mismatch n1 n2)
           (do ([i 0 (+ i 1)]
                [sum 0 (+ sum (* (vector-ref v1 i) (vector-ref v2 i)))])
               ((= i n1) sum)))))")
    (alternatives ())
    (source-commit "2f69645d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement vec-normalize
  ;; ──────────────────────────────────────────────
  ;; Requires computing norm then scaling — composes dot, sqrt, scale.

  (task
    (id "linalg/vec/normalize-impl")
    (module vec)
    (tier 0)
    (type implement)
    (description "Implement vec-normalize: normalize a vector to unit length. Compute the L2 norm (Euclidean length) as sqrt(sum of squares), then divide each element by the norm. Return an error if the vector is the zero vector (norm = 0). Use vec-dot, sqrt, and vec-scale which are already defined.")
    (context "Available: prelude, vec-dot (dot product), vec-scale (scalar multiplication: (vec-scale k v) multiplies each element by k), vec-map (applies function to each element), sqrt. A vector is a Scheme vector. Error convention: return '(error zero-vector) for zero-length input.")
    (before "(define (vec-normalize v)
  (doc 'type '(-> (Vec Num) (Or (Vec Num) Error)))
  (doc 'description \"Normalize to unit length\")
  ;; TODO: compute norm, check for zero, scale by 1/norm
  v)")
    (test-file "lattice/linalg/test-vec.ss")
    (test-forms ("(let ([n (vec-normalize (vector 3 4 0))]) (assert-true (< (abs (- (vec-dot n n) 1.0)) 1e-10)))"
                 "(assert-true (pair? (vec-normalize (vector 0 0 0))))"
                 "(let ([n (vec-normalize (vector 1 0 0))]) (assert-equal 1 (vector-ref n 0)))"))
    (available-modules (prelude))
    (hints ("Norm = (sqrt (vec-dot v v))"
            "Check for zero: (= n 0)"
            "Scale: (vec-scale (/ 1 n) v)"))
    (reference-solution "(define (vec-normalize v)
  (let ([n (sqrt (vec-dot v v))])
       (if (= n 0)
           '(error zero-vector)
           (vec-scale (/ 1 n) v))))")
    (alternatives ())
    (source-commit "2f69645d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement vec-linspace
  ;; ──────────────────────────────────────────────
  ;; Create evenly-spaced vector. Tests numerical thinking.

  (task
    (id "linalg/vec/linspace-impl")
    (module vec)
    (tier 0)
    (type implement)
    (description "Implement vec-linspace: create a vector of n evenly spaced values from start to end (inclusive on both endpoints). For n=1, return a vector containing just start. For n=0, return an empty vector. For n>=2, the step size is (end - start) / (n - 1). Use make-vector and vector-set! to build the result.")
    (context "Available: prelude, make-vector, vector-set!, vector. Vectors are Scheme vectors.")
    (before "(define (vec-linspace start end n)
  (doc 'type '(-> Num Num Nat (Vec Num)))
  (doc 'description \"Vector of n evenly spaced values from start to end\")
  ;; TODO: implement
  (make-vector n 0))")
    (test-file "lattice/linalg/test-vec.ss")
    (test-forms ("(assert-equal (vector 0 1 2 3 4) (vec-linspace 0 4 5))"
                 "(assert-equal (vector 10) (vec-linspace 10 20 1))"
                 "(assert-equal '#() (vec-linspace 0 1 0))"
                 "(let ([ls (vec-linspace 0 1 5)]) (assert-true (< (abs (- (vector-ref ls 2) 0.5)) 1e-10)))"))
    (available-modules (prelude))
    (hints ("Handle edge cases first: n=0 → (vector), n=1 → (vector start)"
            "Step size = (/ (- end start) (- n 1))"
            "Fill: (vector-set! result i (+ start (* i step)))"))
    (reference-solution "(define (vec-linspace start end n)
  (if (< n 2)
      (if (= n 1)
          (vector start)
          (vector))
      (let* ([step (/ (- end start) (- n 1))]
             [result (make-vector n 0)])
            (do ([i 0 (+ i 1)])
                ((= i n) result)
                (vector-set! result i (+ start (* i step)))))))")
    (alternatives ())
    (source-commit "2f69645d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement vec-argmin
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/vec/argmin-impl")
    (module vec)
    (tier 0)
    (type implement)
    (description "Implement vec-argmin: return the index of the minimum element in a numeric vector. For an empty vector, return an error. Iterate through all elements, tracking the index of the smallest value seen so far. Use vector-ref and vector-length.")
    (context "Available: prelude, vector-ref, vector-length. Vectors are Scheme vectors. Error convention: return '(error empty-vector) for empty input.")
    (before "(define (vec-argmin v)
  (doc 'type '(-> (Vec Num) (Or Nat Error)))
  (doc 'description \"Index of minimum element\")
  ;; TODO: iterate, track index of smallest
  0)")
    (test-file "lattice/linalg/test-vec.ss")
    (test-forms ("(assert-equal 1 (vec-argmin (vector 3 1 4 1 5)))"
                 "(assert-equal 0 (vec-argmin (vector 0 1 2 3)))"
                 "(assert-equal 2 (vec-argmin (vector 5 3 -1 7)))"
                 "(assert-true (pair? (vec-argmin (vector))))"))
    (available-modules (prelude))
    (hints ("Handle empty vector: (= (vector-length v) 0)"
            "Track min-index and compare as you iterate"
            "Use a do-loop from index 1 to n, updating min-index when a smaller element is found"))
    (reference-solution "(define (vec-argmin v)
  (if (= (vector-length v) 0)
      '(error empty-vector)
      (let ([n (vector-length v)])
           (do ([i 1 (+ i 1)]
                [min-i 0 (if (< (vector-ref v i) (vector-ref v min-i)) i min-i)])
               ((= i n) min-i)))))")
    (alternatives ())
    (source-commit "2f69645d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Extend — vec-cosine-similarity
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/vec/cosine-similarity-extend")
    (module vec)
    (tier 0)
    (type extend)
    (description "Add vec-cosine-similarity: compute the cosine of the angle between two vectors. Formula: cos(theta) = dot(a,b) / (norm(a) * norm(b)). Return an error if either vector is zero. Use the existing vec-dot and vec-norm functions.")
    (context "Available: prelude, vec-dot (inner product — returns a number or error for dimension mismatch), vec-norm (L2 norm — returns a number). Error convention: return '(error zero-vector) if either vector has norm 0.")
    (before ";; Add to vec.ss — cosine similarity is not currently in the module.
;; cos(theta) = dot(a,b) / (||a|| * ||b||)")
    (test-file #f)
    (test-forms ("(assert-equal 1 (vec-cosine-similarity (vector 1 0 0) (vector 2 0 0)))"
                 "(assert-equal 0 (vec-cosine-similarity (vector 1 0) (vector 0 1)))"
                 "(assert-true (< (abs (+ (vec-cosine-similarity (vector 1 0) (vector -1 0)) 1)) 1e-10))"
                 "(assert-true (pair? (vec-cosine-similarity (vector 0 0) (vector 1 0))))"))
    (available-modules (prelude vec))
    (hints ("Compute dot product, then both norms"
            "Check if either norm is zero before dividing"
            "The result should be in [-1, 1]"))
    (reference-solution "(define (vec-cosine-similarity v1 v2)
  (let ([d (vec-dot v1 v2)]
        [n1 (vec-norm v1)]
        [n2 (vec-norm v2)])
       (if (or (= n1 0) (= n2 0))
           '(error zero-vector)
           (/ d (* n1 n2)))))")
    (alternatives ())
    (source-commit #f)
    (difficulty 3))

) ;; end tasks
