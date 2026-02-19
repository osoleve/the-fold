;;; interval-integrate.sexp — Development tasks for lattice/numeric/interval-integrate.ss
;;;
;;; Module: interval-integrate (tier 0, depends on prelude, interval, heap)
;;; 233 lines, ~6 exported functions
;;; Verified numerical integration with rigorous enclosure bounds.
;;; Algorithms: uniform subdivision (naive), bisected (midpoint), adaptive
;;; refinement (heap-based), rigorous (directed rounding). All methods return
;;; intervals guaranteed to contain the true definite integral.
;;;
;;; Git history:
;;;   70f24841 feat(numeric): O(N log N) adaptive interval integration
;;;   6aa1612f feat(interval): Add verified interval integration with enclosure guarantees

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement adaptive-entry-wider?
  ;; ──────────────────────────────────────────────
  ;; Comparator for adaptive integration heap entries.

  (task
    (id "numeric/interval-integrate/adaptive-entry-wider-impl")
    (module interval-integrate)
    (tier 0)
    (type implement)
    (description "Implement adaptive-entry-wider?: comparator for the adaptive integration heap. Each heap entry is a pair (contribution-interval . domain-interval). The entry with the wider contribution should have higher priority (be extracted first). Compare the interval widths of the contribution (car) of each entry using >=. This drives the adaptive algorithm to refine where the enclosure is widest.")
    (context "Available: interval-width, car, >=. Two accessor calls and one comparison.")
    (before "(define (adaptive-entry-wider? a b)
  ;; TODO: is a's contribution wider than b's?
  #f)")
    (test-file "lattice/numeric/test-interval-integrate.ss")
    (test-forms (";; Wider contribution has priority
(let ([a (cons (make-interval 0 5) (make-interval 0 1))]
      [b (cons (make-interval 0 3) (make-interval 0 1))])
  (assert-true (adaptive-entry-wider? a b)))"
                 ";; Narrower does not
(let ([a (cons (make-interval 0 2) (make-interval 0 1))]
      [b (cons (make-interval 0 5) (make-interval 0 1))])
  (assert-false (adaptive-entry-wider? a b)))"
                 ";; Equal widths: >= returns #t
(let ([a (cons (make-interval 0 4) (make-interval 0 1))]
      [b (cons (make-interval 0 4) (make-interval 0 1))])
  (assert-true (adaptive-entry-wider? a b)))"))
    (available-modules (prelude interval heap))
    (hints ("Each entry is (contribution . domain)"
            "Get contribution: (car a)"
            "Get width: (interval-width (car a))"
            "(>= (interval-width (car a)) (interval-width (car b)))"))
    (reference-solution "(define (adaptive-entry-wider? a b)
  (>= (interval-width (car a))
      (interval-width (car b))))")
    (alternatives ())
    (source-commit "70f24841")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement interval-integrate-naive
  ;; ──────────────────────────────────────────────
  ;; Natural interval extension integration.

  (task
    (id "numeric/interval-integrate/naive-impl")
    (module interval-integrate)
    (tier 0)
    (type implement)
    (description "Implement interval-integrate-naive: integrate f over [a,b] using n uniform subdivisions with interval arithmetic. This is the simplest verified integration method. Compute step size h = (b-a)/n, then iterate i from 0 to n-1: for each subinterval [a+i*h, a+(i+1)*h], create an interval, evaluate f on it, scale the result by h, and add to the running sum. Start with sum = (interval-singleton 0). Error if n <= 0. Use a named let loop with index i and accumulator sum.")
    (context "Available: interval-singleton, make-interval, interval-scale, interval-add, let*, let loop, if, >=, <=, +, -, *, /. Named let with accumulator.")
    (before "(define (interval-integrate-naive f a b n)
  ;; TODO: uniform subdivision interval integration
  (interval-singleton 0))")
    (test-file "lattice/numeric/test-interval-integrate.ss")
    (test-forms (";; Constant function: integral of 1 over [0,1] = 1
(let ([r (interval-integrate-naive (lambda (iv) (interval-singleton 1)) 0 1 10)])
  (assert-true (and (>= 1.0 (interval-lo r)) (<= 1.0 (interval-hi r))))
  (assert-true (< (interval-width r) 1e-10)))"
                 ";; Linear function: integral of x over [0,1] = 0.5
(let ([r (interval-integrate-naive (lambda (iv) iv) 0 1 100)])
  (assert-true (and (>= 0.5 (interval-lo r)) (<= 0.5 (interval-hi r)))))"
                 ";; Error on n=0
(assert-error (lambda () (interval-integrate-naive (lambda (iv) iv) 0 1 0)))"))
    (available-modules (prelude interval heap))
    (hints ("Step size: (let* ([h (/ (- b a) n)]) ...)"
            "Loop: (let loop ([i 0] [sum (interval-singleton 0)]) ...)"
            "Base case: (>= i n) -> sum"
            "Each step: make sub-interval, evaluate f, scale by h, add to sum"
            "(let* ([xi (+ a (* i h))] [xi+1 (+ a (* (+ i 1) h))] [sub-iv (make-interval xi xi+1)] [f-iv (f sub-iv)] [contrib (interval-scale f-iv h)]) (loop (+ i 1) (interval-add sum contrib)))"))
    (reference-solution "(define (interval-integrate-naive f a b n)
  (if (<= n 0)
      (error 'interval-integrate-naive \"n must be positive\")
      (let* ([h (/ (- b a) n)])
    (let loop ([i 0] [sum (interval-singleton 0)])
      (if (>= i n)
          sum
          (let* ([xi (+ a (* i h))]
                 [xi+1 (+ a (* (+ i 1) h))]
                 [sub-iv (make-interval xi xi+1)]
                 [f-iv (f sub-iv)]
                 [contrib (interval-scale f-iv h)])
            (loop (+ i 1) (interval-add sum contrib))))))))")
    (alternatives ())
    (source-commit "6aa1612f")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement interval-integrate-midpoint
  ;; ──────────────────────────────────────────────
  ;; Bisected integration for tighter bounds.

  (task
    (id "numeric/interval-integrate/midpoint-impl")
    (module interval-integrate)
    (tier 0)
    (type implement)
    (description "Implement interval-integrate-midpoint: integrate f over [a,b] using bisected subintervals for tighter bounds. Like naive, subdivide into n equal subintervals. But for each subinterval, split it at its midpoint and evaluate f on both halves separately. This gives tighter enclosures because interval overestimation grows superlinearly with width. For each subinterval [xi, xi+1]: compute mid = (xi + xi+1)/2, half-h = h/2, evaluate f on [xi, mid] and [mid, xi+1], scale each by half-h, add both contributions. Error if n <= 0.")
    (context "Available: interval-singleton, make-interval, interval-scale, interval-add, let*, let loop, if, >=, <=, +, -, *, /. Same structure as naive but with midpoint split.")
    (before "(define (interval-integrate-midpoint f a b n)
  ;; TODO: bisected interval integration
  (interval-singleton 0))")
    (test-file "lattice/numeric/test-interval-integrate.ss")
    (test-forms (";; Quadratic: integral of x^2 over [0,1] = 1/3
(let ([r (interval-integrate-midpoint (lambda (iv) (interval-mul iv iv)) 0 1 100)])
  (assert-true (and (>= (/ 1.0 3) (interval-lo r)) (<= (/ 1.0 3) (interval-hi r)))))"
                 ";; Tighter than naive for same n
(let* ([f (lambda (iv) (interval-mul iv iv))]
       [naive-w (interval-width (interval-integrate-naive f 0 1 32))]
       [mid-w (interval-width (interval-integrate-midpoint f 0 1 32))])
  (assert-true (< mid-w naive-w)))"
                 ";; Error on n=0
(assert-error (lambda () (interval-integrate-midpoint (lambda (iv) iv) 0 1 0)))"))
    (available-modules (prelude interval heap))
    (hints ("Same outer loop as naive: h = (b-a)/n, iterate i from 0 to n-1"
            "For each subinterval: compute mid = (/ (+ xi xi+1) 2) and half-h = (/ h 2)"
            "Evaluate f on left half [xi, mid] and right half [mid, xi+1]"
            "Scale each by half-h, add both to get contribution"
            "(interval-add (interval-scale (f (make-interval xi mid)) half-h) (interval-scale (f (make-interval mid xi+1)) half-h))"))
    (reference-solution "(define (interval-integrate-midpoint f a b n)
  (if (<= n 0)
      (error 'interval-integrate-midpoint \"n must be positive\")
      (let* ([h (/ (- b a) n)])
        (let loop ([i 0] [sum (interval-singleton 0)])
          (if (>= i n)
              sum
              (let* ([xi (+ a (* i h))]
                     [xi+1 (+ a (* (+ i 1) h))]
                     [mid (/ (+ xi xi+1) 2)]
                     [half-h (/ h 2)]
                     [left-f (f (make-interval xi mid))]
                     [left-contrib (interval-scale left-f half-h)]
                     [right-f (f (make-interval mid xi+1))]
                     [right-contrib (interval-scale right-f half-h)]
                     [contrib (interval-add left-contrib right-contrib)])
                (loop (+ i 1) (interval-add sum contrib))))))))")
    (alternatives ())
    (source-commit "6aa1612f")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement interval-integrate-rigorous
  ;; ──────────────────────────────────────────────
  ;; Directed rounding for formal guarantees.

  (task
    (id "numeric/interval-integrate/rigorous-impl")
    (module interval-integrate)
    (tier 0)
    (type implement)
    (description "Implement interval-integrate-rigorous: integrate f over [a,b] using n uniform subdivisions with directed rounding for formal guarantees. Structurally identical to interval-integrate-naive, but uses interval-scale-rigorous and interval-add-rigorous instead of interval-scale and interval-add. These directed-rounding variants ensure the computed bounds are formally valid even with floating-point arithmetic. Slower but suitable for computer-aided proofs. Error if n <= 0.")
    (context "Available: interval-singleton, make-interval, interval-scale-rigorous, interval-add-rigorous, let*, let loop, if, >=, <=, +, -, *, /. Same structure as naive with rigorous operations.")
    (before "(define (interval-integrate-rigorous f a b n)
  ;; TODO: rigorous interval integration with directed rounding
  (interval-singleton 0))")
    (test-file "lattice/numeric/test-interval-integrate.ss")
    (test-forms (";; Constant: integral of 1 over [0,1] = 1
(let ([r (interval-integrate-rigorous (lambda (iv) (interval-singleton 1)) 0 1 10)])
  (assert-true (and (>= 1.0 (interval-lo r)) (<= 1.0 (interval-hi r)))))"
                 ";; Quadratic: integral of x^2 over [0,1] encloses 1/3
(let ([r (interval-integrate-rigorous (lambda (iv) (interval-mul iv iv)) 0 1 100)])
  (assert-true (and (>= (/ 1.0 3) (interval-lo r)) (<= (/ 1.0 3) (interval-hi r)))))"
                 ";; Error on n=0
(assert-error (lambda () (interval-integrate-rigorous (lambda (iv) iv) 0 1 0)))"))
    (available-modules (prelude interval heap))
    (hints ("Identical to naive except for two operation swaps"
            "Use interval-scale-rigorous instead of interval-scale"
            "Use interval-add-rigorous instead of interval-add"
            "Everything else (loop structure, subinterval creation, f evaluation) is the same"))
    (reference-solution "(define (interval-integrate-rigorous f a b n)
  (if (<= n 0)
      (error 'interval-integrate-rigorous \"n must be positive\")
      (let* ([h (/ (- b a) n)])
    (let loop ([i 0] [sum (interval-singleton 0)])
      (if (>= i n)
          sum
          (let* ([xi (+ a (* i h))]
                 [xi+1 (+ a (* (+ i 1) h))]
                 [sub-iv (make-interval xi xi+1)]
                 [f-iv (f sub-iv)]
                 [contrib (interval-scale-rigorous f-iv h)])
            (loop (+ i 1) (interval-add-rigorous sum contrib))))))))")
    (alternatives ())
    (source-commit "6aa1612f")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement interval-integrate
  ;; ──────────────────────────────────────────────
  ;; Convenience wrapper with method dispatch.

  (task
    (id "numeric/interval-integrate/convenience-impl")
    (module interval-integrate)
    (tier 0)
    (type implement)
    (description "Implement interval-integrate: convenience wrapper that dispatches to the appropriate integration method. Takes f, a, b, and optional arguments. Parse opts: if the first opt is a symbol, it's the method name (default 'midpoint); remaining opts give n (default 64). Dispatch on method with case: 'naive calls interval-integrate-naive, 'midpoint calls interval-integrate-midpoint, 'adaptive calls interval-integrate-adaptive with tolerance (default 1e-8) and max-subdivisions (default 1000). Error on unknown method.")
    (context "Available: pair?, symbol?, car, cdr, case, if, let*, string-append, symbol->string, error, interval-integrate-naive, interval-integrate-midpoint, interval-integrate-adaptive. Argument parsing + case dispatch.")
    (before "(define (interval-integrate f a b . opts)
  ;; TODO: convenience wrapper dispatching to integration methods
  (interval-integrate-midpoint f a b 64))")
    (test-file "lattice/numeric/test-interval-integrate.ss")
    (test-forms (";; Default method is midpoint with n=64
(let* ([f (lambda (iv) (interval-mul iv iv))]
       [default-result (interval-integrate f 0 1)]
       [midpoint-result (interval-integrate-midpoint f 0 1 64)])
  (assert-equal (interval-lo default-result) (interval-lo midpoint-result))
  (assert-equal (interval-hi default-result) (interval-hi midpoint-result)))"
                 ";; Naive method via convenience
(let ([r (interval-integrate (lambda (iv) (interval-singleton 1)) 0 1 'naive 10)])
  (assert-true (and (>= 1.0 (interval-lo r)) (<= 1.0 (interval-hi r)))))"
                 ";; Midpoint method via convenience
(let ([r (interval-integrate (lambda (iv) (interval-mul iv iv)) 0 1 'midpoint 100)])
  (assert-true (and (>= (/ 1.0 3) (interval-lo r)) (<= (/ 1.0 3) (interval-hi r)))))"))
    (available-modules (prelude interval heap))
    (hints ("Parse method: (if (and (pair? opts) (symbol? (car opts))) (car opts) 'midpoint)"
            "Parse rest: skip method symbol if present"
            "Parse n: first of rest, default 64"
            "case dispatch on method symbol"
            "For adaptive: parse tolerance (default 1e-8) and max-subdivisions (default 1000) from rest"))
    (reference-solution "(define (interval-integrate f a b . opts)
  (let* ([method (if (and (pair? opts) (symbol? (car opts)))
                     (car opts)
                     'midpoint)]
         [rest (if (and (pair? opts) (symbol? (car opts)))
                   (cdr opts)
                   opts)]
         [n (if (pair? rest) (car rest) 64)])
    (case method
      [(naive) (interval-integrate-naive f a b n)]
      [(midpoint) (interval-integrate-midpoint f a b n)]
      [(adaptive)
       (let ([tol (if (pair? rest) (car rest) 1e-8)]
             [max-subs (if (and (pair? rest) (pair? (cdr rest)))
                          (cadr rest) 1000)])
         (interval-integrate-adaptive f a b tol max-subs))]
      [else (error 'interval-integrate
                   (string-append \"Unknown method: \" (symbol->string method)))])))")
    (alternatives ())
    (source-commit "6aa1612f")
    (difficulty 1))

) ;; end tasks
