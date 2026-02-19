;;; lattice/quickcheck/generators.ss — Sized, composable random generators
;;; @module qc-generators
;;; @requires prelude state prng

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'state)
(require 'prng)

(doc 'module 'qc-generators)
(doc 'description "QuickCheck-style generators. Gen a = Size -> (State RNG a). Sized, composable, monadic.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Gen type
;;; ============================================================================

(doc 'section 'gen-type)

(define (make-gen fn)
  (doc 'export #t)
  (doc 'type '(-> (-> Size (State RNG a)) (Gen a)))
  (doc 'description "Create a generator from a sized state computation")
  (cons 'gen fn))

(define (gen? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'gen)))

(define (gen-fn g)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> Size (State RNG a))))
  (doc 'description "Extract the underlying function")
  (cdr g))

(define (run-gen gen size seed)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) Size RNG a))
  (doc 'description "Evaluate a generator with given size and RNG seed")
  (eval-state ((gen-fn gen) size) seed))

(define (run-gen-with-state gen size seed)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) Size RNG (Pair a RNG)))
  (doc 'description "Evaluate a generator, returning both value and final RNG state")
  (run-state ((gen-fn gen) size) seed))

;;; ============================================================================
;;; Monad operations
;;; ============================================================================

(doc 'section 'monad-ops)

(define (gen-pure x)
  (doc 'export #t)
  (doc 'type '(-> a (Gen a)))
  (doc 'description "Constant generator — always produces x regardless of size or randomness")
  (make-gen (lambda (size) (state-pure x))))

(define (gen-map f gen)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (Gen a) (Gen b)))
  (doc 'description "Map a function over a generator's output")
  (make-gen (lambda (size)
    (state-map f ((gen-fn gen) size)))))

(define (gen-bind gen f)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a (Gen b)) (Gen b)))
  (doc 'description "Monadic bind — generate a, then use it to pick the next generator")
  (make-gen (lambda (size)
    (state-bind ((gen-fn gen) size)
                (lambda (a)
                  ((gen-fn (f a)) size))))))

;;; ============================================================================
;;; Size control
;;; ============================================================================

(doc 'section 'size-control)

(define (gen-sized f)
  (doc 'export #t)
  (doc 'type '(-> (-> Size (Gen a)) (Gen a)))
  (doc 'description "Access the current size parameter to build a generator")
  (make-gen (lambda (size)
    ((gen-fn (f size)) size))))

(define (gen-resize n gen)
  (doc 'export #t)
  (doc 'type '(-> Size (Gen a) (Gen a)))
  (doc 'description "Override the size parameter for a generator")
  (make-gen (lambda (size)
    ((gen-fn gen) n))))

(define (gen-scale f gen)
  (doc 'export #t)
  (doc 'type '(-> (-> Size Size) (Gen a) (Gen a)))
  (doc 'description "Transform the size parameter before passing to generator")
  (make-gen (lambda (size)
    ((gen-fn gen) (f size)))))

;;; ============================================================================
;;; Primitive generators
;;; ============================================================================

(doc 'section 'primitive-generators)

(doc gen-bool 'export #t)
(doc gen-bool 'description "Uniform random boolean")
(define gen-bool
  (make-gen (lambda (size) random-bool)))

(doc gen-nat 'export #t)
(doc gen-nat 'description "Natural number in [0, size]")
(define gen-nat
  (gen-sized (lambda (size)
    (make-gen (lambda (s) (random-int-range 0 (max 0 size)))))))

(doc gen-int 'export #t)
(doc gen-int 'description "Integer in [-size, size]")
(define gen-int
  (gen-sized (lambda (size)
    (make-gen (lambda (s) (random-int-range (- 0 size) size))))))

(doc gen-float 'export #t)
(doc gen-float 'description "Uniform float in [0, 1)")
(define gen-float
  (make-gen (lambda (size) random-float)))

(define (gen-int-range lo hi)
  (doc 'export #t)
  (doc 'type '(-> Int Int (Gen Int)))
  (doc 'description "Integer in [lo, hi] — size-independent")
  (make-gen (lambda (size) (random-int-range lo hi))))

(doc gen-char 'export #t)
(doc gen-char 'description "Printable ASCII character [32, 126]")
(define gen-char
  (gen-map integer->char (gen-int-range 32 126)))

;;; ============================================================================
;;; Combinators
;;; ============================================================================

(doc 'section 'combinators)

(define (gen-one-of gens)
  (doc 'export #t)
  (doc 'type '(-> (List (Gen a)) (Gen a)))
  (doc 'description "Uniform random choice among generators")
  (if (null? gens)
      (error 'gen-one-of "empty generator list")
      (let ([n (length gens)])
        (make-gen (lambda (size)
          (state-bind (random-int-range 0 (- n 1))
                      (lambda (i)
                        ((gen-fn (list-ref gens i)) size))))))))

(define (gen-frequency weighted-gens)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Int (Gen a))) (Gen a)))
  (doc 'description "Weighted random choice — each entry is (weight . gen)")
  (if (null? weighted-gens)
      (error 'gen-frequency "empty weighted list")
      (let* ([total (fold-left + 0 (map car weighted-gens))])
        (if (<= total 0)
            (error 'gen-frequency "total weight must be positive" total)
            (make-gen (lambda (size)
              (state-bind (random-int-range 1 total)
                          (lambda (r)
                            (let pick ([lst weighted-gens] [acc 0])
                              (let ([new-acc (+ acc (caar lst))])
                                (if (or (<= r new-acc) (null? (cdr lst)))
                                    ((gen-fn (cdar lst)) size)
                                    (pick (cdr lst) new-acc))))))))))))

(define (gen-elements lst)
  (doc 'export #t)
  (doc 'type '(-> (List a) (Gen a)))
  (doc 'description "Uniform random choice from a list of values")
  (if (null? lst)
      (error 'gen-elements "empty element list")
      (make-gen (lambda (size) (random-element lst)))))

(doc gen-list 'export #t)
(doc gen-list 'description "List of random length [0, size] using given element generator")
(define gen-list
  (lambda (gen)
    (gen-sized (lambda (size)
      (gen-bind (gen-int-range 0 (max 0 size))
                (lambda (n)
                  (gen-list-of n gen)))))))

(define (gen-list-of n gen)
  (doc 'export #t)
  (doc 'type '(-> Nat (Gen a) (Gen (List a))))
  (doc 'description "List of exactly n elements from given generator")
  (make-gen (lambda (size)
    (let loop ([remaining n] [acc '()])
      (if (<= remaining 0)
          (state-pure (reverse acc))
          (state-bind ((gen-fn gen) size)
                      (lambda (x)
                        (loop (- remaining 1) (cons x acc)))))))))

(define (gen-vector gen)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (Gen (Vector a))))
  (doc 'description "Vector of random length [0, size]")
  (gen-map list->vector (gen-list gen)))

(define (gen-pair gen-a gen-b)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (Gen b) (Gen (Pair a b))))
  (doc 'description "Pair combinator — generate both components")
  (gen-bind gen-a
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       gen-b))))

(doc gen-string 'export #t)
(doc gen-string 'description "String of random printable ASCII characters, length bounded by size")
(define gen-string
  (gen-map list->string (gen-list gen-char)))

;;; ============================================================================
;;; Lifting from State RNG
;;; ============================================================================

(doc 'section 'lifting)

(define (gen-from-random state-computation)
  (doc 'export #t)
  (doc 'type '(-> (State RNG a) (Gen a)))
  (doc 'description "Lift an existing (State RNG a) into Gen (ignores size)")
  (make-gen (lambda (size) state-computation)))
