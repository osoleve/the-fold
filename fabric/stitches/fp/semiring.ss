;;; fabric/stitches/fp/semiring.ss — Semiring and Algebraic Structures Library
;;;
;;; Implements semirings and related algebraic structures:
;;; - Semirings: (S, +, *, 0, 1) with associativity, identity, distribution
;;; - Common instances: Boolean, Tropical, Probabilistic
;;; - Applications: Shortest paths, parsing, dataflow analysis
;;;
;;; A semiring has:
;;; - Additive monoid (S, +, 0)
;;; - Multiplicative monoid (S, *, 1)
;;; - Multiplication distributes over addition
;;; - 0 annihilates multiplication
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Semiring Interface
;;; ============================================================

;;; A semiring is represented as: (semiring name zero one plus times)
(define (make-semiring name zero one plus times)
  (list 'semiring name zero one plus times))

(define (semiring? x)
  (and (pair? x) (eq? (car x) 'semiring)))

(define (semiring-name sr)
  (list-ref sr 1))

(define (semiring-zero sr)
  (list-ref sr 2))

(define (semiring-one sr)
  (list-ref sr 3))

(define (semiring-plus sr)
  (list-ref sr 4))

(define (semiring-times sr)
  (list-ref sr 5))

;;; ============================================================
;;; Semiring Operations
;;; ============================================================

;;; Add two elements
(define (sr-add sr a b)
  ((semiring-plus sr) a b))

;;; Multiply two elements
(define (sr-mul sr a b)
  ((semiring-times sr) a b))

;;; Get zero element
(define (sr-zero sr)
  (semiring-zero sr))

;;; Get one element
(define (sr-one sr)
  (semiring-one sr))

;;; Sum a list of elements
(define (sr-sum sr lst)
  (fold-left (semiring-plus sr) (semiring-zero sr) lst))

;;; Product of a list of elements
(define (sr-product sr lst)
  (fold-left (semiring-times sr) (semiring-one sr) lst))

;;; ============================================================
;;; Boolean Semiring
;;; ============================================================

;;; Boolean semiring: (Boolean, OR, AND, #f, #t)
;;; Used for: reachability, satisfiability
(define boolean-semiring
  (make-semiring
   'boolean
   #f                            ; zero = false
   #t                            ; one = true
   (lambda (a b) (or a b))       ; plus = or
   (lambda (a b) (and a b))))    ; times = and

;;; ============================================================
;;; Natural Number Semiring
;;; ============================================================

;;; Natural numbers: (N, +, *, 0, 1)
;;; The classic semiring
(define natural-semiring
  (make-semiring
   'natural
   0
   1
   +
   *))

;;; ============================================================
;;; Tropical Semiring (Min-Plus)
;;; ============================================================

;;; Tropical (min-plus): (R ∪ {∞}, min, +, ∞, 0)
;;; Used for: shortest paths
(define tropical-infinity 'inf)

(define (tropical-min a b)
  (cond
    [(eq? a 'inf) b]
    [(eq? b 'inf) a]
    [else (min a b)]))

(define (tropical-plus a b)
  (cond
    [(eq? a 'inf) 'inf]
    [(eq? b 'inf) 'inf]
    [else (+ a b)]))

(define tropical-semiring
  (make-semiring
   'tropical
   'inf                          ; zero = infinity
   0                             ; one = 0
   tropical-min                  ; plus = min
   tropical-plus))               ; times = +

;;; ============================================================
;;; Arctic Semiring (Max-Plus)
;;; ============================================================

;;; Arctic (max-plus): (R ∪ {-∞}, max, +, -∞, 0)
;;; Used for: longest paths
(define arctic-neg-infinity '-inf)

(define (arctic-max a b)
  (cond
    [(eq? a '-inf) b]
    [(eq? b '-inf) a]
    [else (max a b)]))

(define (arctic-plus a b)
  (cond
    [(eq? a '-inf) '-inf]
    [(eq? b '-inf) '-inf]
    [else (+ a b)]))

(define arctic-semiring
  (make-semiring
   'arctic
   '-inf                         ; zero = -infinity
   0                             ; one = 0
   arctic-max                    ; plus = max
   arctic-plus))                 ; times = +

;;; ============================================================
;;; Probabilistic Semiring
;;; ============================================================

;;; Probability: ([0,1], max, *, 0, 1)
;;; Used for: Viterbi algorithm, most likely paths
(define probability-semiring
  (make-semiring
   'probability
   0                             ; zero
   1                             ; one
   max                           ; plus = max
   *))                           ; times = *

;;; Log-probability: (R ∪ {-∞}, max, +, -∞, 0)
;;; Used for: numerical stability in probability
(define log-probability-semiring
  (make-semiring
   'log-probability
   '-inf                         ; zero = log(0)
   0                             ; one = log(1)
   (lambda (a b)
     (cond
       [(eq? a '-inf) b]
       [(eq? b '-inf) a]
       [else (max a b)]))
   (lambda (a b)
     (cond
       [(eq? a '-inf) '-inf]
       [(eq? b '-inf) '-inf]
       [else (+ a b)]))))

;;; ============================================================
;;; Counting Semiring
;;; ============================================================

;;; Counting paths: (N, +, *, 0, 1)
;;; Count number of paths (same as natural, explicitly for paths)
(define counting-semiring natural-semiring)

;;; ============================================================
;;; Set Semiring
;;; ============================================================

;;; Power set semiring: (P(S), ∪, ∩, {}, S)
;;; Note: Requires a universal set S for the one element
(define (make-set-semiring universal-set)
  (make-semiring
   'set
   '()                           ; zero = empty set
   universal-set                 ; one = universal set
   (lambda (a b)                 ; plus = union
     (set-union-list a b))
   (lambda (a b)                 ; times = intersection
     (set-intersect-list a b))))

(define (set-union-list a b)
  (fold-left (lambda (acc x) (if (member x acc) acc (cons x acc)))
             a
             b))

(define (set-intersect-list a b)
  (filter (lambda (x) (member x b)) a))

;;; ============================================================
;;; Regular Expression Semiring
;;; ============================================================

;;; Regex semiring for parsing
;;; Elements are: 'empty (zero), 'epsilon (one), 'char, 'alt, 'seq, 'star
(define regex-semiring
  (make-semiring
   'regex
   'empty                        ; zero = empty language
   'epsilon                      ; one = empty string
   (lambda (a b)                 ; plus = alternation
     (cond
       [(eq? a 'empty) b]
       [(eq? b 'empty) a]
       [else (list 'alt a b)]))
   (lambda (a b)                 ; times = concatenation
     (cond
       [(eq? a 'empty) 'empty]
       [(eq? b 'empty) 'empty]
       [(eq? a 'epsilon) b]
       [(eq? b 'epsilon) a]
       [else (list 'seq a b)]))))

;;; ============================================================
;;; Matrix Operations over Semirings
;;; ============================================================

;;; Matrix represented as list of rows (list of lists)

(define (matrix-rows m) (length m))
(define (matrix-cols m) (if (null? m) 0 (length (car m))))
(define (matrix-ref m i j) (list-ref (list-ref m i) j))

;;; Create n x n identity matrix
(define (sr-identity-matrix sr n)
  (let ([zero (sr-zero sr)]
        [one (sr-one sr)])
    (map (lambda (i)
           (map (lambda (j)
                  (if (= i j) one zero))
                (iota n)))
         (iota n))))

;;; Create n x n zero matrix
(define (sr-zero-matrix sr n)
  (let ([zero (sr-zero sr)])
    (map (lambda (i) (map (lambda (j) zero) (iota n)))
         (iota n))))

;;; Helper: iota
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; Matrix addition
(define (sr-matrix-add sr m1 m2)
  (map (lambda (row1 row2)
         (map (lambda (a b) (sr-add sr a b)) row1 row2))
       m1 m2))

;;; Matrix multiplication
(define (sr-matrix-mul sr m1 m2)
  (let* ([n (matrix-rows m1)]
         [m (matrix-cols m2)]
         [k (matrix-cols m1)])
    (map (lambda (i)
           (map (lambda (j)
                  (sr-sum sr
                          (map (lambda (l)
                                 (sr-mul sr
                                         (matrix-ref m1 i l)
                                         (matrix-ref m2 l j)))
                               (iota k))))
                (iota m)))
         (iota n))))

;;; Matrix power (for transitive closure)
(define (sr-matrix-power sr m n)
  (if (<= n 0)
      (sr-identity-matrix sr (matrix-rows m))
      (sr-matrix-mul sr m (sr-matrix-power sr m (- n 1)))))

;;; ============================================================
;;; Transitive Closure (Kleene Star)
;;; ============================================================

;;; Compute A* = I + A + A^2 + A^3 + ... (truncated at n iterations)
(define (sr-transitive-closure sr m iterations)
  (let ([n (matrix-rows m)]
        [id (sr-identity-matrix sr (matrix-rows m))])
    (let loop ([i 0] [power id] [sum id])
      (if (>= i iterations)
          sum
          (let ([next-power (sr-matrix-mul sr power m)])
            (loop (+ i 1)
                  next-power
                  (sr-matrix-add sr sum next-power)))))))

;;; ============================================================
;;; Shortest Paths (Floyd-Warshall with Semiring)
;;; ============================================================

;;; Floyd-Warshall using tropical semiring
(define (shortest-paths distance-matrix)
  (sr-transitive-closure tropical-semiring
                         distance-matrix
                         (matrix-rows distance-matrix)))

;;; ============================================================
;;; Path Counting
;;; ============================================================

;;; Count paths using counting semiring
(define (count-paths adjacency-matrix length)
  (sr-matrix-power counting-semiring adjacency-matrix length))

;;; ============================================================
;;; Semiring Polynomials
;;; ============================================================

;;; Polynomial represented as list of coefficients [a0, a1, a2, ...]
;;; representing a0 + a1*x + a2*x^2 + ...

(define (sr-poly-add sr p1 p2)
  (cond
    [(null? p1) p2]
    [(null? p2) p1]
    [else (cons (sr-add sr (car p1) (car p2))
                (sr-poly-add sr (cdr p1) (cdr p2)))]))

(define (sr-poly-mul sr p1 p2)
  (if (null? p1)
      '()
      (sr-poly-add sr
                   (sr-poly-scale sr (car p1) p2)
                   (cons (sr-zero sr) (sr-poly-mul sr (cdr p1) p2)))))

(define (sr-poly-scale sr c p)
  (map (lambda (a) (sr-mul sr c a)) p))

;;; Evaluate polynomial at x
(define (sr-poly-eval sr p x)
  (let loop ([coeffs (reverse p)] [acc (sr-zero sr)])
    (if (null? coeffs)
        acc
        (loop (cdr coeffs)
              (sr-add sr (car coeffs) (sr-mul sr acc x))))))

;;; ============================================================
;;; Star Semiring (Closed Semirings)
;;; ============================================================

;;; Some semirings have a star operation: a* = 1 + a + a^2 + ...
;;; This is used for Kleene closure in regular expressions

(define (make-star-semiring name zero one plus times star)
  (list 'star-semiring name zero one plus times star))

(define (star-semiring? x)
  (and (pair? x) (eq? (car x) 'star-semiring)))

(define (star-semiring-star sr)
  (list-ref sr 6))

;;; Boolean star semiring
(define boolean-star-semiring
  (make-star-semiring
   'boolean-star
   #f #t
   (lambda (a b) (or a b))
   (lambda (a b) (and a b))
   (lambda (a) #t)))  ; a* = true for any a

;;; Tropical star (requires negative cycle check)
(define tropical-star-semiring
  (make-star-semiring
   'tropical-star
   'inf 0
   tropical-min
   tropical-plus
   (lambda (a)
     (if (and (number? a) (< a 0))
         '-inf  ; Negative cycle
         0))))  ; a* = 0 for non-negative

;;; ============================================================
;;; Verification Utilities
;;; ============================================================

;;; Check if semiring laws hold for given elements
(define (sr-verify-laws sr elements)
  (let ([zero (sr-zero sr)]
        [one (sr-one sr)]
        [plus (semiring-plus sr)]
        [times (semiring-times sr)])
    (define (check-all pred name)
      (let ([results (map pred elements)])
        (if (fold-left (lambda (a b) (and a b)) #t results)
            (format "~a: PASS" name)
            (format "~a: FAIL" name))))
    (list
     ;; Zero is additive identity
     (check-all (lambda (a) (equal? (plus a zero) a))
                "0 + a = a")
     (check-all (lambda (a) (equal? (plus zero a) a))
                "a + 0 = a")
     ;; One is multiplicative identity
     (check-all (lambda (a) (equal? (times a one) a))
                "1 * a = a")
     (check-all (lambda (a) (equal? (times one a) a))
                "a * 1 = a")
     ;; Zero annihilates
     (check-all (lambda (a) (equal? (times a zero) zero))
                "0 * a = 0")
     (check-all (lambda (a) (equal? (times zero a) zero))
                "a * 0 = 0"))))

;;; ============================================================
;;; Display
;;; ============================================================

(define (semiring->string sr)
  (format "Semiring[~a]" (semiring-name sr)))

(define (matrix->string m)
  (let ([rows (map (lambda (row)
                     (fold-left (lambda (acc x)
                                  (string-append acc " " (format "~a" x)))
                                ""
                                row))
                   m)])
    (fold-left (lambda (acc row)
                 (string-append acc "\n" row))
               ""
               rows)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Semiring interface:
;;;   make-semiring, semiring?, semiring-name, semiring-zero,
;;;   semiring-one, semiring-plus, semiring-times
;;;
;;; Operations:
;;;   sr-add, sr-mul, sr-zero, sr-one, sr-sum, sr-product
;;;
;;; Common semirings:
;;;   boolean-semiring, natural-semiring, tropical-semiring,
;;;   arctic-semiring, probability-semiring, log-probability-semiring,
;;;   counting-semiring, regex-semiring
;;;
;;; Set semiring:
;;;   make-set-semiring
;;;
;;; Matrix operations:
;;;   sr-identity-matrix, sr-zero-matrix, sr-matrix-add,
;;;   sr-matrix-mul, sr-matrix-power
;;;
;;; Algorithms:
;;;   sr-transitive-closure, shortest-paths, count-paths
;;;
;;; Polynomials:
;;;   sr-poly-add, sr-poly-mul, sr-poly-scale, sr-poly-eval
;;;
;;; Star semirings:
;;;   make-star-semiring, star-semiring?, boolean-star-semiring,
;;;   tropical-star-semiring
;;;
;;; Verification:
;;;   sr-verify-laws
;;;
;;; Display:
;;;   semiring->string, matrix->string
