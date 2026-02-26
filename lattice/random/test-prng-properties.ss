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
;;; Sampling utilities
;;; ============================================================================

(test-group sampling-properties

  (define-property "random-float stays in [0, 1)"
    gen-seed
    (lambda (seed)
      (let ([x (with-random seed random-float)])
        (and (>= x 0.0) (< x 1.0))))
    'tests 250)

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
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
