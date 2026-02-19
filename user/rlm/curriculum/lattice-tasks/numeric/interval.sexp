;;; interval.sexp — Development tasks for lattice/numeric/interval.ss
;;;
;;; Module: interval (tier 0, depends on prelude)
;;; 1288 lines, ~60+ exported functions
;;; Verified interval arithmetic with rigorous bounds. Interval type,
;;; accessors, predicates, three-valued comparisons, arithmetic (add, sub,
;;; mul with 9-case sign dispatch), set operations, transcendentals,
;;; directed-rounding variants.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement interval-mid
  ;; ──────────────────────────────────────────────
  ;; Overflow-safe midpoint computation.

  (task
    (id "numeric/interval/interval-mid-impl")
    (module interval)
    (tier 0)
    (type implement)
    (description "Implement interval-mid: compute the midpoint of an interval. The naive formula (lo + hi) / 2 can overflow for large values. Instead use the overflow-safe formula: lo + (hi - lo) / 2. Extract lo and hi using interval-lo and interval-hi, compute (hi - lo), divide by 2, and add to lo.")
    (context "Available: interval-lo, interval-hi, +, -, /, let. One let binding with two accessors and arithmetic.")
    (before "(define (interval-mid iv)
  ;; TODO: overflow-safe midpoint
  0)")
    (test-file "lattice/numeric/test-interval.ss")
    (test-forms (";; Point interval: midpoint is the point
(assert-equal 5 (interval-mid (make-interval 5 5)))"
                 ";; Symmetric interval
(assert-equal 0 (interval-mid (make-interval -3 3)))"
                 ";; General case
(assert-equal 2.5 (interval-mid (make-interval 1 4)))"
                 ";; Negative interval
(assert-equal -5.0 (interval-mid (make-interval -7 -3)))"))
    (available-modules (prelude))
    (hints ("Use overflow-safe formula: lo + (hi - lo) / 2"
            "Extract bounds: (let ([lo (interval-lo iv)] [hi (interval-hi iv)]) ...)"
            "Compute: (+ lo (/ (- hi lo) 2))"))
    (reference-solution "(define (interval-mid iv)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (+ lo (/ (- hi lo) 2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement interval-contains?
  ;; ──────────────────────────────────────────────
  ;; Point containment test.

  (task
    (id "numeric/interval/interval-contains-impl")
    (module interval)
    (tier 0)
    (type implement)
    (description "Implement interval-contains?: test if an interval contains a given point x. A point x is in the interval [lo, hi] if lo <= x AND x <= hi. Use interval-lo and interval-hi to extract bounds, then combine two <= comparisons with and.")
    (context "Available: interval-lo, interval-hi, <=, and. Two comparisons combined with and.")
    (before "(define (interval-contains? iv x)
  ;; TODO: lo <= x <= hi
  #f)")
    (test-file "lattice/numeric/test-interval.ss")
    (test-forms (";; Midpoint is contained
(assert-true (interval-contains? (make-interval 1 5) 3))"
                 ";; Endpoints are contained (closed interval)
(assert-true (interval-contains? (make-interval 1 5) 1))
(assert-true (interval-contains? (make-interval 1 5) 5))"
                 ";; Outside: too low
(assert-false (interval-contains? (make-interval 1 5) 0))"
                 ";; Outside: too high
(assert-false (interval-contains? (make-interval 1 5) 6))"))
    (available-modules (prelude))
    (hints ("Extract: lo = (interval-lo iv), hi = (interval-hi iv)"
            "Test: (and (<= lo x) (<= x hi))"))
    (reference-solution "(define (interval-contains? iv x)
  (and (<= (interval-lo iv) x)
       (<= x (interval-hi iv))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement interval-add
  ;; ──────────────────────────────────────────────
  ;; Interval addition: [a,b] + [c,d] = [a+c, b+d].

  (task
    (id "numeric/interval/interval-add-impl")
    (module interval)
    (tier 0)
    (type implement)
    (description "Implement interval-add: add two intervals. The fundamental identity of interval arithmetic for addition is [a,b] + [c,d] = [a+c, b+d]. The lower bound of the sum is the sum of the lower bounds, and the upper bound is the sum of the upper bounds. This is tight — no overestimation. Use make-interval with the summed lower and upper bounds.")
    (context "Available: interval-lo, interval-hi, make-interval, +. Two additions and a make-interval call.")
    (before "(define (interval-add iv1 iv2)
  ;; TODO: [a+c, b+d]
  (make-interval 0 0))")
    (test-file "lattice/numeric/test-interval.ss")
    (test-forms (";; [1,3] + [2,4] = [3,7]
(let ([r (interval-add (make-interval 1 3) (make-interval 2 4))])
  (assert-equal 3 (interval-lo r))
  (assert-equal 7 (interval-hi r)))"
                 ";; Adding negative intervals: [-3,-1] + [-4,-2] = [-7,-3]
(let ([r (interval-add (make-interval -3 -1) (make-interval -4 -2))])
  (assert-equal -7 (interval-lo r))
  (assert-equal -3 (interval-hi r)))"
                 ";; Adding point interval: [2,5] + [3,3] = [5,8]
(let ([r (interval-add (make-interval 2 5) (interval-singleton 3))])
  (assert-equal 5 (interval-lo r))
  (assert-equal 8 (interval-hi r)))"))
    (available-modules (prelude))
    (hints ("Lower bound: (+ (interval-lo iv1) (interval-lo iv2))"
            "Upper bound: (+ (interval-hi iv1) (interval-hi iv2))"
            "Return: (make-interval lo-sum hi-sum)"))
    (reference-solution "(define (interval-add iv1 iv2)
  (make-interval (+ (interval-lo iv1) (interval-lo iv2))
                 (+ (interval-hi iv1) (interval-hi iv2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement interval-neg
  ;; ──────────────────────────────────────────────
  ;; Negation: -[a,b] = [-b, -a].

  (task
    (id "numeric/interval/interval-neg-impl")
    (module interval)
    (tier 0)
    (type implement)
    (description "Implement interval-neg: negate an interval. Negating every point in [a,b] gives [-b, -a]. The lower bound becomes the negation of the upper bound, and the upper bound becomes the negation of the lower bound. Note the swap: the new lower bound is (- hi) and the new upper bound is (- lo).")
    (context "Available: interval-lo, interval-hi, make-interval, -. Two negations and make-interval.")
    (before "(define (interval-neg iv)
  ;; TODO: -[a,b] = [-b, -a]
  iv)")
    (test-file "lattice/numeric/test-interval.ss")
    (test-forms (";; Negate positive: -[1,3] = [-3,-1]
(let ([r (interval-neg (make-interval 1 3))])
  (assert-equal -3 (interval-lo r))
  (assert-equal -1 (interval-hi r)))"
                 ";; Negate negative: -[-5,-2] = [2,5]
(let ([r (interval-neg (make-interval -5 -2))])
  (assert-equal 2 (interval-lo r))
  (assert-equal 5 (interval-hi r)))"
                 ";; Negate symmetric: -[-3,3] = [-3,3]
(let ([r (interval-neg (make-interval -3 3))])
  (assert-equal -3 (interval-lo r))
  (assert-equal 3 (interval-hi r)))"
                 ";; Double negation is identity
(let* ([iv (make-interval 1 4)]
       [r (interval-neg (interval-neg iv))])
  (assert-equal (interval-lo iv) (interval-lo r))
  (assert-equal (interval-hi iv) (interval-hi r)))"))
    (available-modules (prelude))
    (hints ("New lower bound: (- (interval-hi iv))"
            "New upper bound: (- (interval-lo iv))"
            "Return: (make-interval (- hi) (- lo))"))
    (reference-solution "(define (interval-neg iv)
  (make-interval (- (interval-hi iv))
                 (- (interval-lo iv))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement interval-mul
  ;; ──────────────────────────────────────────────
  ;; 9-case sign-optimized multiplication.

  (task
    (id "numeric/interval/interval-mul-impl")
    (module interval)
    (tier 0)
    (type implement)
    (description "Implement interval-mul: multiply two intervals using sign-based case analysis. Naive interval multiplication would compute all four endpoint products and take min/max, requiring 4 multiplications. The sign-optimized version uses 9 cases (P/N/M × P/N/M where P=positive, N=negative, M=mixed) to reduce most cases to 2 multiplications. Let a,b = lo,hi of iv1 and c,d = lo,hi of iv2. First check if iv1 is positive (a >= 0), negative (b <= 0), or mixed. Within each, check iv2 similarly. The key cases: P*P = [a*c, b*d], N*N = [b*d, a*c], P*N = [b*c, a*d]. The M*M case (both straddle zero) is the only one requiring 4 multiplications: [min(a*d, b*c), max(a*c, b*d)].")
    (context "Available: interval-lo, interval-hi, make-interval, *, >=, <=, min, max, cond, let. Nested cond with 9 branches.")
    (before "(define (interval-mul iv1 iv2)
  ;; TODO: sign-based case dispatch
  (make-interval 0 0))")
    (test-file "lattice/numeric/test-interval.ss")
    (test-forms (";; P*P: [1,3] * [2,4] = [2,12]
(let ([r (interval-mul (make-interval 1 3) (make-interval 2 4))])
  (assert-equal 2 (interval-lo r))
  (assert-equal 12 (interval-hi r)))"
                 ";; N*N: [-3,-1] * [-4,-2] = [2,12]
(let ([r (interval-mul (make-interval -3 -1) (make-interval -4 -2))])
  (assert-equal 2 (interval-lo r))
  (assert-equal 12 (interval-hi r)))"
                 ";; P*N: [1,3] * [-4,-2] = [-12,-2]
(let ([r (interval-mul (make-interval 1 3) (make-interval -4 -2))])
  (assert-equal -12 (interval-lo r))
  (assert-equal -2 (interval-hi r)))"
                 ";; M*M: [-1,2] * [-3,4] = [-6,8]
(let ([r (interval-mul (make-interval -1 2) (make-interval -3 4))])
  (assert-equal -6 (interval-lo r))
  (assert-equal 8 (interval-hi r)))"))
    (available-modules (prelude))
    (hints ("Extract: (let ([a (interval-lo iv1)] [b (interval-hi iv1)] [c (interval-lo iv2)] [d (interval-hi iv2)]) ...)"
            "Outer cond on iv1: (>= a 0) → positive, (<= b 0) → negative, else → mixed"
            "Inner cond on iv2: same sign dispatch"
            "P*P: [a*c, b*d], N*N: [b*d, a*c], P*N: [b*c, a*d], N*P: [a*d, b*c]"
            "P*M: [b*c, b*d], N*M: [a*d, a*c], M*P: [a*d, b*d], M*N: [b*c, a*c]"
            "M*M: [min(a*d, b*c), max(a*c, b*d)] — only case needing 4 muls"))
    (reference-solution "(define (interval-mul iv1 iv2)
  (let ([a (interval-lo iv1)] [b (interval-hi iv1)]
        [c (interval-lo iv2)] [d (interval-hi iv2)])
    (cond
      [(>= a 0)
       (cond
         [(>= c 0) (make-interval (* a c) (* b d))]
         [(<= d 0) (make-interval (* b c) (* a d))]
         [else     (make-interval (* b c) (* b d))])]
      [(<= b 0)
       (cond
         [(>= c 0) (make-interval (* a d) (* b c))]
         [(<= d 0) (make-interval (* b d) (* a c))]
         [else     (make-interval (* a d) (* a c))])]
      [else
       (cond
         [(>= c 0) (make-interval (* a d) (* b d))]
         [(<= d 0) (make-interval (* b c) (* a c))]
         [else     (make-interval (min (* a d) (* b c))
                                  (max (* a c) (* b d)))])])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
