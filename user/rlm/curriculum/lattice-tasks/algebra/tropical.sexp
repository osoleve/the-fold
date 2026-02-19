;;; tropical.sexp — Development tasks for lattice/algebra/tropical.ss
;;;
;;; Module: tropical (tier 0, depends on prelude, matrix)
;;; 463 lines, ~25 exported functions
;;; Tropical algebra: semiring abstraction, min-plus and max-plus semirings,
;;; tropical matrix multiply/add/power/closure (Floyd-Warshall), tropical
;;; eigenvalues via Karp's algorithm, tropical polynomials, Newton polygons.
;;;
;;; The key insight: shortest-path algorithms ARE tropical linear algebra.
;;; Floyd-Warshall = Kleene star (A*) over the min-plus semiring.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement make-semiring
  ;; ──────────────────────────────────────────────
  ;; The semiring constructor — a lightweight algebraic abstraction.

  (task
    (id "algebra/tropical/make-semiring-impl")
    (module tropical)
    (tier 0)
    (type implement)
    (description "Implement make-semiring: construct a semiring from its five components — addition (the 'tropical plus'), multiplication (the 'tropical times'), additive identity (zero), multiplicative identity (one), and equality predicate. A semiring (S, +, *, 0, 1) satisfies: (S,+,0) is a commutative monoid, (S,*,1) is a monoid, * distributes over +, and 0 annihilates under *. Package these as a tagged list: (list 'semiring add mul zero one eq?). This is the foundation for all tropical operations.")
    (context "Available: list. Pure data packaging, no computation.")
    (before "(define (make-semiring add mul zero one eq?)
  (doc 'export #t)
  ;; TODO: package as tagged list
  '())")
    (test-file "lattice/algebra/test-tropical.ss")
    (test-forms (";; Constructor returns a list tagged 'semiring
(assert-true (pair? (make-semiring min + +inf.0 0 =)))"
                 ";; Tag is 'semiring
(assert-equal 'semiring (car (make-semiring min + +inf.0 0 =)))"
                 ";; Length is 6 (tag + 5 components)
(assert-equal 6 (length (make-semiring min + +inf.0 0 =)))"))
    (available-modules (prelude))
    (hints ("This is pure data packaging — no arithmetic"
            "Return (list 'semiring add mul zero one eq?)"
            "The 'semiring tag enables the semiring? predicate"))
    (reference-solution "(define (make-semiring add mul zero one eq?)
  (list 'semiring add mul zero one eq?))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement tropical-power
  ;; ──────────────────────────────────────────────
  ;; Binary exponentiation over any semiring.

  (task
    (id "algebra/tropical/tropical-power-impl")
    (module tropical)
    (tier 0)
    (type implement)
    (description "Implement tropical-power: compute a^n over a semiring using binary exponentiation (repeated squaring). In the min-plus semiring, multiplication is ordinary addition, so a^n = a+a+...+a = n*a. But this function works for ANY semiring. Base cases: n=0 returns the multiplicative identity (semiring-one), n=1 returns a. For even n, compute half = a^(n/2), then return half * half. For odd n, return a * a^(n-1). Use (semiring-one sr) for the identity and (tropical-mul sr x y) for multiplication.")
    (context "Available: semiring-one (gets multiplicative identity), tropical-mul (semiring multiplication), =, even?, quotient, -. Recursive algorithm.")
    (before "(define (tropical-power sr a n)
  (doc 'export #t)
  ;; TODO: binary exponentiation using semiring multiplication
  0)")
    (test-file "lattice/algebra/test-tropical.ss")
    (test-forms (";; a^0 = identity (which is 0 for min-plus)
(assert-equal 0 (tropical-power min-plus-semiring 5 0))"
                 ";; a^1 = a
(assert-equal 5 (tropical-power min-plus-semiring 5 1))"
                 ";; In min-plus, a^n = n*a (since multiplication is addition)
(assert-equal 12 (tropical-power min-plus-semiring 3 4))"
                 ";; Works for max-plus too
(assert-equal 15 (tropical-power max-plus-semiring 5 3))"))
    (available-modules (prelude matrix))
    (hints ("Four cases: n=0 → (semiring-one sr), n=1 → a"
            "Even n: half = recursive call with n/2, return (tropical-mul sr half half)"
            "Odd n: return (tropical-mul sr a (recursive call with n-1))"
            "Use (quotient n 2) for integer division"))
    (reference-solution "(define (tropical-power sr a n)
  (cond [(= n 0) (semiring-one sr)]
        [(= n 1) a]
        [(even? n)
         (let ([half (tropical-power sr a (quotient n 2))])
           (tropical-mul sr half half))]
        [else
         (tropical-mul sr a (tropical-power sr a (- n 1)))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement tropical-poly-eval
  ;; ──────────────────────────────────────────────
  ;; Evaluate a tropical polynomial — piecewise-linear in min-plus.

  (task
    (id "algebra/tropical/tropical-poly-eval-impl")
    (module tropical)
    (tier 0)
    (type implement)
    (description "Implement tropical-poly-eval: evaluate a tropical polynomial p(x) = a_0 (+) a_1 (*) x (+) a_2 (*) x^2 (+) ... where (+) and (*) are the semiring operations. The coefficients are given as a list (a_0 a_1 ... a_d). For each term, compute a_i (*) x^i using tropical-power for x^i and tropical-mul for the product, then combine all terms with tropical-add. Start with the accumulator set to the semiring zero (additive identity). In the min-plus semiring, this computes min(a_0 + 0, a_1 + x, a_2 + 2x, ...) — a piecewise-linear function whose breakpoints are the 'tropical roots'.")
    (context "Available: semiring-add, semiring-mul, semiring-zero, tropical-power, tropical-mul, tropical-add, car, cdr, null?. Use a tail-recursive loop with index i tracking the current exponent.")
    (before "(define (tropical-poly-eval sr coeffs x)
  (doc 'export #t)
  ;; TODO: accumulate terms a_i * x^i using semiring operations
  0)")
    (test-file "lattice/algebra/test-tropical.ss")
    (test-forms (";; Constant polynomial: p(x) = 5 → min(5) = 5
(assert-equal 5 (tropical-poly-eval min-plus-semiring '(5) 10))"
                 ";; p(x) = min(1, 3+x, 2+2x) at x=0 → min(1,3,2) = 1
(let ([r (tropical-poly-eval min-plus-semiring '(1 3 2) 0)])
  (assert-true (< (abs (- r 1.0)) 0.001)))"
                 ";; p(x) = min(5, 0+x, 10+2x) at x=3 → min(5,3,16) = 3
(let ([r (tropical-poly-eval min-plus-semiring '(5 0 10) 3)])
  (assert-true (< (abs (- r 3.0)) 0.001)))"))
    (available-modules (prelude matrix))
    (hints ("Extract operations: add = (semiring-add sr), mul = (semiring-mul sr), zero = (semiring-zero sr)"
            "Loop over coefficients with index i starting at 0"
            "Each term: (mul (car cs) (tropical-power sr x i))"
            "Accumulate: (add acc term)"
            "Return acc when coefficients are exhausted"))
    (reference-solution "(define (tropical-poly-eval sr coeffs x)
  (let ([add (semiring-add sr)]
        [mul (semiring-mul sr)]
        [zero (semiring-zero sr)])
    (let loop ([cs coeffs] [i 0] [acc zero])
      (if (null? cs)
          acc
          (let ([term (mul (car cs) (tropical-power sr x i))])
            (loop (cdr cs) (+ i 1) (add acc term)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement tropical-matrix-mul
  ;; ──────────────────────────────────────────────
  ;; Matrix multiplication over a semiring — the core of tropical LA.

  (task
    (id "algebra/tropical/tropical-matrix-mul-impl")
    (module tropical)
    (tier 0)
    (type implement)
    (description "Implement tropical-matrix-mul: multiply two matrices over a semiring. The entry C[i,j] = (+)_k (A[i,k] (*) B[k,j]), where (+) is the semiring addition and (*) is the semiring multiplication. For min-plus: C[i,j] = min over k of (A[i,k] + B[k,j]) — this is exactly one step of shortest-path relaxation. For an m*p matrix A and p*n matrix B, the result is m*n. Use matrix-rows, matrix-cols, and matrix-ref to access the input matrices. Build the result as (list 'matrix m n data) where data is a flat vector of m*n entries.")
    (context "Available: matrix-rows, matrix-cols, matrix-ref (from matrix module), tropical-add, tropical-mul, semiring-zero, make-vector, vector-set!, do loops. Triple-nested loop: i over rows, j over cols, k over inner dimension.")
    (before "(define (tropical-matrix-mul sr A B)
  (doc 'export #t)
  ;; TODO: C[i,j] = tropical-add over k of (A[i,k] tropical-mul B[k,j])
  A)")
    (test-file "lattice/algebra/test-tropical.ss")
    (test-forms (";; Identity * A = A (for tropical identity)
(let* ([I (tropical-matrix-identity min-plus-semiring 2)]
       [A (list 'matrix 2 2 (vector 1 3 2 4))]
       [C (tropical-matrix-mul min-plus-semiring I A)])
  (assert-equal 1 (matrix-ref C 0 0))
  (assert-equal 4 (matrix-ref C 1 1)))"
                 ";; 2x2 min-plus multiply
;; A = [[0, 3], [2, +inf]], B = [[0, 1], [+inf, 0]]
;; C[0,0] = min(0+0, 3+inf) = 0
;; C[0,1] = min(0+1, 3+0) = 1
(let* ([A (list 'matrix 2 2 (vector 0 3 2 +inf.0))]
       [B (list 'matrix 2 2 (vector 0 1 +inf.0 0))]
       [C (tropical-matrix-mul min-plus-semiring A B)])
  (assert-equal 0 (matrix-ref C 0 0))
  (assert-equal 1 (matrix-ref C 0 1)))"))
    (available-modules (prelude matrix))
    (hints ("Get dimensions: m = (matrix-rows A), p = (matrix-cols A), n = (matrix-cols B)"
            "Allocate: (make-vector (* m n) (semiring-zero sr))"
            "Triple loop: for i in 0..m, j in 0..n, accumulate over k in 0..p"
            "Inner: use a named let loop over k with accumulator starting at (semiring-zero sr)"
            "Each step: (tropical-add sr acc (tropical-mul sr (matrix-ref A i k) (matrix-ref B k j)))"
            "Store result: (vector-set! data (+ (* i n) j) acc)"
            "Return: (list 'matrix m n data)"))
    (reference-solution "(define (tropical-matrix-mul sr A B)
  (let* ([m (matrix-rows A)]
         [p (matrix-cols A)]
         [n (matrix-cols B)]
         [zero (semiring-zero sr)]
         [data (make-vector (* m n) zero)])
    (do ([i 0 (+ i 1)])
        ((= i m))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let loop ([k 0] [acc zero])
          (if (= k p)
              (vector-set! data (+ (* i n) j) acc)
              (loop (+ k 1)
                    (tropical-add sr acc
                                  (tropical-mul sr
                                                (matrix-ref A i k)
                                                (matrix-ref B k j))))))))
    (list 'matrix m n data)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement tropical-matrix-closure
  ;; ──────────────────────────────────────────────
  ;; Floyd-Warshall IS tropical Kleene star — the deepest insight.

  (task
    (id "algebra/tropical/tropical-matrix-closure-impl")
    (module tropical)
    (tier 0)
    (type implement)
    (description "Implement tropical-matrix-closure: compute A* = I (+) A (+) A^2 (+) A^3 (+) ... the Kleene star of matrix A over a semiring. This is computed via Floyd-Warshall dynamic programming in O(n^3). Steps: (1) Initialize D = I (+) A, where the diagonal gets (one (+) A[i,i]) and off-diagonal gets A[i,j]. (2) For each intermediate vertex k from 0 to n-1, for each pair (i,j): D[i,j] = D[i,j] (+) (D[i,k] (*) D[k,j]). This IS Floyd-Warshall's all-pairs shortest paths when the semiring is min-plus. The connection is profound: shortest-path computation is literally tropical matrix algebra.")
    (context "Available: matrix-rows, matrix-ref, semiring-add, semiring-mul, semiring-zero, semiring-one, make-vector, vector-ref, vector-set!, do loops. Triple-nested loop: k (intermediate), i (source), j (destination).")
    (before "(define (tropical-matrix-closure sr A)
  (doc 'export #t)
  ;; TODO: Floyd-Warshall as tropical Kleene star
  A)")
    (test-file "lattice/algebra/test-tropical.ss")
    (test-forms (";; Closure of zero matrix is identity (diagonal = 0, off = +inf)
(let* ([Z (list 'matrix 2 2 (vector +inf.0 +inf.0 +inf.0 +inf.0))]
       [C (tropical-matrix-closure min-plus-semiring Z)])
  (assert-equal 0 (matrix-ref C 0 0))
  (assert-equal 0 (matrix-ref C 1 1))
  (assert-equal +inf.0 (matrix-ref C 0 1)))"
                 ";; Simple graph: 0 --3--> 1, 1 --2--> 0
;; Shortest 0->1 = 3, shortest 1->0 = 2, 0->0 = 0
(let* ([A (list 'matrix 2 2 (vector +inf.0 3 2 +inf.0))]
       [C (tropical-matrix-closure min-plus-semiring A)])
  (assert-equal 0 (matrix-ref C 0 0))
  (assert-equal 3 (matrix-ref C 0 1))
  (assert-equal 2 (matrix-ref C 1 0))
  (assert-equal 0 (matrix-ref C 1 1)))"))
    (available-modules (prelude matrix))
    (hints ("Extract semiring operations into local variables for readability"
            "Initialize D[i,j]: if i=j then (add one A[i,i]), else A[i,j]"
            "Floyd-Warshall: for k, i, j: D[i,j] = (add D[i,j] (mul D[i,k] D[k,j]))"
            "Use flat vector with index (+ (* i n) j)"
            "Return (list 'matrix n n data)"))
    (reference-solution "(define (tropical-matrix-closure sr A)
  (let* ([n (matrix-rows A)]
         [add (semiring-add sr)]
         [mul (semiring-mul sr)]
         [zero (semiring-zero sr)]
         [one (semiring-one sr)]
         [data (make-vector (* n n) zero)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [a-ij (matrix-ref A i j)])
          (if (= i j)
              (vector-set! data idx (add one a-ij))
              (vector-set! data idx a-ij)))))
    (do ([k 0 (+ k 1)])
        ((= k n))
      (do ([i 0 (+ i 1)])
          ((= i n))
        (do ([j 0 (+ j 1)])
            ((= j n))
          (let* ([ij (+ (* i n) j)]
                 [d-ij (vector-ref data ij)]
                 [d-ik (vector-ref data (+ (* i n) k))]
                 [d-kj (vector-ref data (+ (* k n) j))]
                 [via-k (mul d-ik d-kj)])
            (vector-set! data ij (add d-ij via-k))))))
    (list 'matrix n n data)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 4))

) ;; end tasks
