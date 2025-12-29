;;; fabric/stitches/fp/strategies.ss — Parallel Evaluation Strategies
;;;
;;; Implements Haskell-style evaluation strategies for controlling
;;; parallelism and evaluation order. Strategies are functions that
;;; describe how to evaluate values, composable with other strategies.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Key concepts:
;;;   - Strategy a = a → a (evaluation control)
;;;   - Strategies compose: (s1 `dot` s2) applies s2 then s1
;;;   - Parallel hints: rpar sparks parallel evaluation
;;;   - Sequential forcing: rseq evaluates to WHNF
;;;
;;; Currently, par/pseq are sequential (same behavior).
;;; These strategies provide hooks for future parallelization.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; Strategy Type
;;; ============================================================
;;;
;;; A Strategy is a function: a → a
;;; Strategies control how/when values are evaluated.
;;;
;;; In a lazy language, strategies force evaluation.
;;; In our strict language, strategies:
;;;   1. Document intent about parallelizability
;;;   2. Provide hooks for future parallel execution
;;;   3. Enable uniform composition patterns
;;;
;;; Strategy constructors are prefixed with 'r' for "reduction"
;;; following Haskell convention.

;;; ============================================================
;;; Core Strategies
;;; ============================================================

;;; r0 : Strategy a
;;; Do nothing — return value unchanged.
;;; The identity strategy.
(define (r0 x) x)

;;; rseq : Strategy a
;;; Evaluate to weak head normal form (WHNF).
;;; In strict Scheme, values are already WHNF, so this is identity.
;;; Provides a hook for future lazy evaluation support.
(define (rseq x) x)

;;; rpar : Strategy a
;;; Spark parallel evaluation hint.
;;; Currently behaves like rseq (sequential).
;;; Marks the value as safe to evaluate in parallel.
(define (rpar x) x)

;;; rdeepseq : Strategy a
;;; Fully evaluate to normal form (NF).
;;; For lists, recursively evaluates all elements.
;;; For pairs, evaluates both components.
;;; For other values, returns as-is.
(define (rdeepseq x)
  (cond
   [(null? x) x]
   [(pair? x)
    (let ([head (rdeepseq (car x))]
          [tail (rdeepseq (cdr x))])
         (cons head tail))]
   [(vector? x)
    (let ([len (vector-length x)])
         (let loop ([i 0])
              (when (< i len)
                    (rdeepseq (vector-ref x i))
                    (loop (+ i 1))))
         x)]
   [else x]))

;;; ============================================================
;;; Strategy Application
;;; ============================================================

;;; using : a × Strategy a → a
;;; Apply a strategy to a value.
;;; (using x strat) evaluates x according to strat.
(define (using x strat)
  (strat x))

;;; with-strategy : Strategy a × a → a
;;; Apply strategy to value (flipped argument order).
;;; Useful for threading: (with-strategy rpar value)
(define (with-strategy strat x)
  (strat x))

;;; ============================================================
;;; Strategy Composition
;;; ============================================================

;;; dot : Strategy a × Strategy a → Strategy a
;;; Compose two strategies.
;;; (dot s1 s2) applies s2 first, then s1.
(define (dot s1 s2)
  (lambda (x)
          (s1 (s2 x))))

;;; strat-seq : (List (Strategy a)) → Strategy a
;;; Sequence multiple strategies left-to-right.
(define (strat-seq strats)
  (if (null? strats)
      r0
      (fold-left dot r0 strats)))

;;; ============================================================
;;; List Strategies
;;; ============================================================

;;; eval-list : Strategy a → Strategy (List a)
;;; Apply a strategy to each element of a list.
;;; Evaluates the spine (list structure) first.
(define (eval-list strat)
  (lambda (xs)
          (map strat xs)))

;;; parList : Strategy a → Strategy (List a)
;;; Apply strategy to each list element in parallel.
;;; Currently sequential — provides parallel hint for future.
;;; Evaluates all elements, returning the list.
(define (parList strat)
  (lambda (xs)
          (map strat xs)))

;;; parMap : (a → b) × Strategy b → (List a) → (List b)
;;; Map function over list with parallel evaluation strategy.
;;; Each result is evaluated according to the strategy.
(define (parMap f strat)
  (lambda (xs)
          (map (lambda (x) (strat (f x))) xs)))

;;; parBuffer : Nat × Strategy a → Strategy (List a)
;;; Chunked parallel evaluation.
;;; Divides list into chunks of size n, evaluates each chunk.
;;; Provides better granularity control for parallelism.
(define (parBuffer n strat)
  (lambda (xs)
          (let loop ([remaining xs] [acc '()])
               (if (null? remaining)
                   (reverse acc)
                   (let ([chunk (take-up-to n remaining)]
                         [rest (drop-up-to n remaining)])
                        ;; Evaluate chunk elements with strategy
                        (let ([evaluated (map strat chunk)])
                             (loop rest (append (reverse evaluated) acc))))))))

;;; take-up-to : Nat × (List a) → (List a)
;;; Take up to n elements (handles short lists).
(define (take-up-to n xs)
  (if (or (= n 0) (null? xs))
      '()
      (cons (car xs) (take-up-to (- n 1) (cdr xs)))))

;;; drop-up-to : Nat × (List a) → (List a)
;;; Drop up to n elements (handles short lists).
(define (drop-up-to n xs)
  (if (or (= n 0) (null? xs))
      xs
      (drop-up-to (- n 1) (cdr xs))))

;;; ============================================================
;;; Pair/Tuple Strategies
;;; ============================================================

;;; eval-tuple2 : Strategy a × Strategy b → Strategy (Pair a b)
;;; Apply strategies to each component of a pair.
(define (eval-tuple2 strat-a strat-b)
  (lambda (p)
          (cons (strat-a (car p))
                (strat-b (cdr p)))))

;;; parTuple2 : Strategy a × Strategy b → Strategy (Pair a b)
;;; Parallel tuple evaluation.
;;; Both components can be evaluated in parallel.
(define (parTuple2 strat-a strat-b)
  (eval-tuple2 strat-a strat-b))

;;; eval-tuple3 : Strategy a × Strategy b × Strategy c → Strategy (List3 a b c)
;;; Apply strategies to a 3-tuple (represented as list).
(define (eval-tuple3 strat-a strat-b strat-c)
  (lambda (t)
          (list (strat-a (car t))
                (strat-b (cadr t))
                (strat-c (caddr t)))))

;;; ============================================================
;;; Conditional Strategies
;;; ============================================================

;;; seqList : Strategy (List a)
;;; Force evaluation of list spine only.
;;; Elements are not evaluated.
(define (seqList xs)
  (if (null? xs)
      xs
      (begin
       (seqList (cdr xs))
       xs)))

;;; seqListN : Nat → Strategy (List a)
;;; Force evaluation of first n elements of list spine.
(define (seqListN n)
  (lambda (xs)
          (let loop ([i n] [ys xs])
               (if (or (= i 0) (null? ys))
                   xs
                   (loop (- i 1) (cdr ys))))))

;;; ============================================================
;;; Demanding Combinators
;;; ============================================================

;;; These combinators force evaluation before returning.
;;; Useful for strict evaluation points.

;;; demanding : a × b → a
;;; Evaluate y (for effect), return x.
;;; (demanding result (rdeepseq data))
(define (demanding x y)
  y  ; Force y
  x) ; Return x

;;; spark : a → a
;;; Hint that this value could be evaluated in parallel.
;;; Returns the value unchanged.
;;; A lightweight way to mark parallelizable computations.
(define (spark x) x)

;;; ============================================================
;;; Strategy Utilities
;;; ============================================================

;;; rwhnf : Strategy a
;;; Alias for rseq — reduce to weak head normal form.
(define rwhnf rseq)

;;; rnf : Strategy a
;;; Alias for rdeepseq — reduce to normal form.
(define rnf rdeepseq)

;;; evalSeq : Sequence evaluation helper.
;;; Given a value and list of strategies, apply each in sequence.
(define (eval-seq x . strats)
  ((strat-seq strats) x))

;;; ============================================================
;;; Force Combinators (Strict Evaluation Points)
;;; ============================================================

;;; force : a → a
;;; Force evaluation of a value.
;;; In strict Scheme, values are already forced.
;;; Provides documentation of intent.
(define (force x) x)

;;; force-list : (List a) → (List a)
;;; Force evaluation of entire list including elements.
(define (force-list xs)
  (rdeepseq xs))

;;; force-spine : (List a) → (List a)
;;; Force evaluation of list spine only.
(define force-spine seqList)

;;; ============================================================
;;; Chunking Utilities
;;; ============================================================

;;; chunk : Nat × (List a) → (List (List a))
;;; Split list into chunks of size n.
;;; Last chunk may be smaller.
(define (chunk n xs)
  (if (null? xs)
      '()
      (cons (take-up-to n xs)
            (chunk n (drop-up-to n xs)))))

;;; parListChunk : Nat × Strategy a → Strategy (List a)
;;; Evaluate list in chunks, applying strategy to each element.
;;; Chunks are processed in parallel (future).
(define (parListChunk n strat)
  (lambda (xs)
          (let ([chunks (chunk n xs)])
               ;; Map over chunks, applying strategy to each element
               (apply append
                      (map (lambda (c) (map strat c)) chunks)))))

;;; ============================================================
;;; NFData-like Deep Evaluation
;;; ============================================================

;;; deep-seq : a × b → b
;;; Deeply evaluate first argument, return second.
;;; (deep-seq (expensive-computation) result)
(define (deep-seq x y)
  (rdeepseq x)
  y)

;;; ($!!) : (a → b) × a → b
;;; Strict function application with deep evaluation.
;;; Fully evaluates argument before applying function.
(define ($!! f x)
  (f (rdeepseq x)))

;;; ($!) : (a → b) × a → b
;;; Strict function application (WHNF).
;;; In strict Scheme, this is just regular application.
(define ($! f x)
  (f (rseq x)))

