;;; lattice/random/test-prng-properties.ss — QuickCheck properties for PRNG utilities

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'prng)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (count-occurrences x xs)
  (let loop ([rest xs] [n 0])
    (cond
      [(null? rest) n]
      [(equal? x (car rest)) (loop (cdr rest) (+ n 1))]
      [else (loop (cdr rest) n)])))

(define (same-elements? xs ys)
  (and (= (length xs) (length ys))
       (let loop ([rest xs])
         (or (null? rest)
             (and (= (count-occurrences (car rest) xs)
                     (count-occurrences (car rest) ys))
                  (loop (cdr rest)))))))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (all-distinct? xs)
  (let loop ([rest xs] [seen '()])
    (cond
      [(null? rest) #t]
      [(member (car rest) seen) #f]
      [else (loop (cdr rest) (cons (car rest) seen))])))

(define (bytevector-all-in-byte-range? bv)
  (let loop ([i 0])
    (if (>= i (bytevector-length bv))
        #t
        (let ([b (bytevector-u8-ref bv i)])
          (and (>= b 0)
               (<= b 255)
               (loop (+ i 1)))))))

(define (take-next next-fn gen n)
  (let loop ([g gen] [remaining n] [acc '()])
    (if (<= remaining 0)
        (reverse acc)
        (let ([r (next-fn g)])
          (loop (cdr r)
                (- remaining 1)
                (cons (car r) acc))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-seed
  (gen-int-range 0 1000000))

(define gen-seed-stream
  (gen-pair gen-seed (gen-int-range 0 1024)))

(define gen-seed-step-count
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (n) (cons seed n))
               (gen-int-range 0 40)))))

(define gen-rot32-args
  (gen-bind (gen-int-range 0 4294967295)
    (lambda (x)
      (gen-map (lambda (k) (list x k))
               (gen-int-range 0 31)))))

(define gen-rot64-args
  (gen-bind (gen-int-range 0 9223372036854775807)
    (lambda (x)
      (gen-map (lambda (k) (list x k))
               (gen-int-range 0 63)))))

(define gen-range-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range -1000 1000)
        (lambda (lo)
          (gen-map (lambda (width) (list seed lo (+ lo width)))
                   (gen-int-range 0 2000)))))))

(define gen-float-range-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range -500 500)
        (lambda (lo-int)
          (gen-map (lambda (w-int)
                     (list seed (/ lo-int 10.0) (/ (+ lo-int w-int) 10.0)))
                   (gen-int-range 1 400)))))))

(define gen-random-list-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 0 40)
        (lambda (n)
          (gen-bind (gen-int-range -50 50)
            (lambda (lo)
              (gen-map (lambda (width) (list seed n lo (+ lo width)))
                       (gen-int-range 0 50)))))))))

(define gen-shuffle-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (lst) (cons seed lst))
               (gen-list (gen-int-range -20 20))))))

(define gen-sample-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 0 25)
        (lambda (n)
          (gen-map (lambda (k) (list seed n k))
                   (gen-int-range 0 n)))))))

(define gen-random-element-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 20)
        (lambda (n)
          (gen-map (lambda (xs) (cons seed xs))
                   (gen-list-of n (gen-int-range -100 100))))))))

(define gen-random-bytes-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (n) (cons seed n))
               (gen-int-range 0 64)))))

;;; ============================================================================
;;; Bit operation laws
;;; ============================================================================

(test-group bit-laws

  (define-property "rotr32 undoes rotl32"
    gen-rot32-args
    (lambda (args)
      (let ([x (car args)] [k (cadr args)])
        (= (u32 x)
           (rotr32 (rotl32 x k) k))))
    'tests 250)

  (define-property "rotr64 undoes rotl64"
    gen-rot64-args
    (lambda (args)
      (let ([x (car args)] [k (cadr args)])
        (= (u64 x)
           (rotr64 (rotl64 x k) k))))
    'tests 250)

  (define-property "u32 is idempotent"
    (gen-int-range -1000000000000 1000000000000)
    (lambda (n)
      (= (u32 n)
         (u32 (u32 n))))
    'tests 200)

  (define-property "u64 is idempotent"
    (gen-int-range -1000000000000 1000000000000)
    (lambda (n)
      (= (u64 n)
         (u64 (u64 n))))
    'tests 200)
)

;;; ============================================================================
;;; Generator determinism
;;; ============================================================================

(test-group generator-determinism

  (define-property "splitmix is reproducible for a given seed"
    gen-seed
    (lambda (seed)
      (let* ([r1 (splitmix-next (make-splitmix seed))]
             [r2 (splitmix-next (make-splitmix seed))])
        (and (= (car r1) (car r2))
             (equal? (cdr r1) (cdr r2)))))
    'tests 250)

  (define-property "PCG is reproducible for a given (seed, stream)"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [g1 (make-pcg seed stream)]
             [g2 (make-pcg seed stream)])
        (equal? (take-next pcg-next g1 8)
                (take-next pcg-next g2 8))))
    'tests 250)

  (define-property "xorshift128+ is reproducible for a given seed"
    gen-seed
    (lambda (seed)
      (let ([g1 (make-xorshift128 seed)]
            [g2 (make-xorshift128 seed)])
        (equal? (take-next xorshift128-next g1 8)
                (take-next xorshift128-next g2 8))))
    'tests 250)
)

;;; ============================================================================
;;; Generator surface coverage
;;; ============================================================================

(test-group generator-surface-properties

  (define-property "mask constants are max unsigned values"
    gen-seed
    (lambda (_)
      (and (= mask-32 (- (expt 2 32) 1))
           (= mask-64 (- (expt 2 64) 1))))
    'tests 10)

  (define-property "splitmix predicate and state accessor agree with constructor"
    gen-seed
    (lambda (seed)
      (let ([g (make-splitmix seed)])
        (and (splitmix? g)
             (= (splitmix-state g) (u64 seed)))))
    'tests 220)

  (define-property "pcg predicate and accessors agree with constructor"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [g (make-pcg seed stream)])
        (and (pcg? g)
             (= (pcg-state g) (cadr g))
             (= (pcg-inc g) (caddr g))
             (odd? (pcg-inc g)))))
    'tests 220)

  (define-property "xorshift predicate and accessors agree with constructor"
    gen-seed
    (lambda (seed)
      (let ([g (make-xorshift128 seed)])
        (and (xorshift128? g)
             (= (xorshift128-s0 g) (cadr g))
             (= (xorshift128-s1 g) (caddr g))
             (not (= (xorshift128-s0 g) 0))
             (not (= (xorshift128-s1 g) 0)))))
    'tests 220)

  (define-property "splitmix-random matches splitmix-next"
    gen-seed
    (lambda (seed)
      (let ([g (make-splitmix seed)])
        (equal? (run-state splitmix-random g)
                (splitmix-next g))))
    'tests 220)

  (define-property "pcg-random matches pcg-next"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [g (make-pcg seed stream)])
        (equal? (run-state pcg-random g)
                (pcg-next g))))
    'tests 220)

  (define-property "xorshift128-random matches xorshift128-next"
    gen-seed
    (lambda (seed)
      (let ([g (make-xorshift128 seed)])
        (equal? (run-state xorshift128-random g)
                (xorshift128-next g))))
    'tests 220)

  (define-property "random-u32-from yields 32-bit value"
    gen-seed
    (lambda (seed)
      (let* ([g (make-splitmix seed)]
             [x (car (run-state (random-u32-from g) g))])
        (and (integer? x)
             (>= x 0)
             (<= x mask-32))))
    'tests 220)

  (define-property "random-u64-from yields 64-bit value"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [g (make-pcg seed stream)]
             [x (car (run-state (random-u64-from g) g))])
        (and (integer? x)
             (>= x 0)
             (<= x mask-64))))
    'tests 220)

  (define-property "gen-advance matches repeated prng-next"
    gen-seed-step-count
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [g (make-pcg seed 1)]
             [manual (let loop ([i n] [curr g])
                       (if (<= i 0)
                           curr
                           (loop (- i 1) (cdr (prng-next curr)))))]
             [advanced (gen-advance n g)])
        (equal? advanced manual)))
    'tests 220)

  (define-property "gen-split returns two generators of the same family"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [g (make-pcg seed stream)]
             [children (gen-split g)])
        (and (pair? children)
             (pcg? (car children))
             (pcg? (cdr children)))))
    'tests 220)

  (define-property "gen-serialize preserves generator tag"
    gen-seed-stream
    (lambda (pair)
      (let* ([seed (car pair)]
             [stream (cdr pair)]
             [sexpr (gen-serialize (make-pcg seed stream))])
        (and (pair? sexpr)
             (eq? (car sexpr) 'pcg))))
    'tests 220)
)

;;; ============================================================================
;;; Sampling utilities
;;; ============================================================================

(test-group sampling-properties

  (define-property "random-float stays in [0, 1)"
    gen-seed
    (lambda (seed)
      (let ([x (with-random seed random-float)])
        (and (>= x 0.0) (< x 1.0))))
    'tests 250)

  (define-property "random-float-range stays in requested interval"
    gen-float-range-args
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [x (with-random seed (random-float-range lo hi))])
        (and (<= lo x) (<= x hi))))
    'tests 220)

  (define-property "random-bool returns booleans"
    gen-seed
    (lambda (seed)
      (let ([x (with-random seed random-bool)])
        (or (eq? x #t) (eq? x #f))))
    'tests 220)

  (define-property "random-element returns an element from non-empty list"
    gen-random-element-args
    (lambda (args)
      (let* ([seed (car args)]
             [xs (cdr args)]
             [picked (with-random seed (random-element xs))])
        (if (member picked xs) #t #f)))
    'tests 220)

  (define-property "random-int-range respects bounds"
    gen-range-args
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [x (with-random seed (random-int-range lo hi))])
        (and (>= x lo) (<= x hi))))
    'tests 250)

  (define-property "random-list returns correct length and range"
    gen-random-list-args
    (lambda (args)
      (let* ([seed (car args)]
             [n (cadr args)]
             [lo (caddr args)]
             [hi (cadddr args)]
             [xs (with-random seed (random-list n (random-int-range lo hi)))])
        (and (= (length xs) n)
             (all-satisfy? (lambda (x) (and (>= x lo) (<= x hi))) xs))))
    'tests 200)

  (define-property "shuffle preserves element multiplicities"
    gen-shuffle-args
    (lambda (args)
      (let* ([seed (car args)]
             [xs (cdr args)]
             [shuffled (with-random seed (shuffle xs))])
        (same-elements? xs shuffled)))
    'tests 200)

  (define-property "sample returns distinct members from source list"
    gen-sample-args
    (lambda (args)
      (let* ([seed (car args)]
             [n (cadr args)]
             [k (caddr args)]
             [source (iota n)]
             [picked (with-random seed (sample k source))])
        (and (= (length picked) k)
             (all-distinct? picked)
             (all-satisfy? (lambda (x) (and (>= x 0) (< x n))) picked))))
    'tests 200)

  (define-property "weighted-choice returns one of the weighted keys"
    gen-seed
    (lambda (seed)
      (let* ([weighted '((a . 5) (b . 3) (c . 1))]
             [choice (with-random seed (weighted-choice weighted))])
        (if (memq choice '(a b c)) #t #f)))
    'tests 200)

  (define-property "random-bytes returns requested length and byte range"
    gen-random-bytes-args
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [bv (with-random seed (random-bytes n))])
        (and (= (bytevector-length bv) n)
             (bytevector-all-in-byte-range? bv))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
