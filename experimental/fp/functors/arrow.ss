;;; fabric/stitches/fp/arrow.ss — Arrow Abstraction
;;;
;;; Arrows are a generalization of functions that enable compositional
;;; programming with side effects, branching, and parallel computation.
;;;
;;; Where monads sequence computations, arrows wire them together.
;;; An Arrow a b represents a computation from a to b.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Arrow interface (arr, >>>, first)
;;;   - Derived combinators (second, ***, &&&)
;;;   - ArrowChoice (left, right, |||, +++)
;;;   - ArrowLoop (loop)
;;;   - Kleisli arrows (monad to arrow)
;;;   - Static arrows (analysis without execution)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Arrow Laws:
;;;   arr id >>> f      = f                      (identity)
;;;   f >>> arr id      = f
;;;   (f >>> g) >>> h   = f >>> (g >>> h)        (associativity)
;;;   arr (g . f)       = arr f >>> arr g        (composition)
;;;   first (arr f)     = arr (f *** id)         (extension)
;;;   first (f >>> g)   = first f >>> first g    (functor)
;;;   first f >>> arr fst = arr fst >>> f        (exchange)
;;;   first f >>> arr (id *** g) = arr (id *** g) >>> first f  (unit)
;;;   first (first f) >>> arr assoc = arr assoc >>> first f    (association)

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Arrow Representation
;;; ============================================================
;;;
;;; An arrow is represented as:
;;;   ('arrow type run-fn)
;;;
;;; Where type is one of:
;;;   'function   — Plain function arrow
;;;   'kleisli    — Monad-based arrow
;;;   'static     — Arrow with static analysis

;;; make-arrow : Symbol -> (a -> b) -> Arrow a b
(define (make-arrow type run-fn)
  (list 'arrow type run-fn))

;;; arrow? : Any -> Boolean
(define (arrow? x)
  (and (pair? x) (eq? (car x) 'arrow)))

;;; arrow-type : Arrow a b -> Symbol
(define (arrow-type arr)
  (list-ref arr 1))

;;; arrow-run : Arrow a b -> (a -> b)
(define (arrow-run arr)
  (list-ref arr 2))

;;; run-arrow : Arrow a b -> a -> b
;;; Execute an arrow on input.
(define (run-arrow arr input)
  ((arrow-run arr) input))

;;; ============================================================
;;; Function Arrows (simplest implementation)
;;; ============================================================

;;; fn-arrow : (a -> b) -> Arrow a b
;;; Lift a function into an arrow.
(define (fn-arrow f)
  (make-arrow 'function f))

;;; arr : (a -> b) -> Arrow a b
;;; Canonical arrow constructor (alias for fn-arrow).
(define arr fn-arrow)

;;; arr-id : Arrow a a
;;; Identity arrow.
(define arr-id (arr identity))

;;; ============================================================
;;; Core Arrow Operations
;;; ============================================================

;;; arrow-compose : Arrow a b -> Arrow b c -> Arrow a c
;;; Compose two arrows (left-to-right, like >>>).
(define (arrow-compose f g)
  (fn-arrow (lambda (x) (run-arrow g (run-arrow f x)))))

;;; >>> : Arrow a b -> Arrow b c -> Arrow a c
;;; Sequential composition (f then g).
(define (>>> f g)
  (arrow-compose f g))

;;; <<< : Arrow b c -> Arrow a b -> Arrow a c
;;; Sequential composition (g after f).
(define (<<< g f)
  (arrow-compose f g))

;;; arrow-first : Arrow a b -> Arrow (a, c) (b, c)
;;; Apply arrow to first component of a pair.
(define (arrow-first f)
  (fn-arrow (lambda (pair)
                    (let ([a (car pair)]
                          [c (cdr pair)])
                         (cons (run-arrow f a) c)))))

;;; first : Arrow a b -> Arrow (a, c) (b, c)
;;; Alias for arrow-first.
(define first arrow-first)

;;; ============================================================
;;; Derived Operations
;;; ============================================================

;;; arrow-second : Arrow a b -> Arrow (c, a) (c, b)
;;; Apply arrow to second component of a pair.
(define (arrow-second f)
  (fn-arrow (lambda (pair)
                    (let ([c (car pair)]
                          [a (cdr pair)])
                         (cons c (run-arrow f a))))))

;;; second : Arrow a b -> Arrow (c, a) (c, b)
(define second arrow-second)

;;; arrow-split : Arrow a b -> Arrow c d -> Arrow (a, c) (b, d)
;;; Apply two arrows in parallel (***).
(define (arrow-split f g)
  (>>> (first f) (second g)))

;;; *** : Arrow a b -> Arrow c d -> Arrow (a, c) (b, d)
;;; Parallel composition.
(define *** arrow-split)

;;; arrow-fanout : Arrow a b -> Arrow a c -> Arrow a (b, c)
;;; Apply two arrows to the same input and pair results (&&&).
(define (arrow-fanout f g)
  (>>> (arr (lambda (x) (cons x x)))
       (*** f g)))

;;; &&& : Arrow a b -> Arrow a c -> Arrow a (b, c)
;;; Fanout composition.
(define &&& arrow-fanout)

;;; ============================================================
;;; Arrow Utilities
;;; ============================================================

;;; arr-const : b -> Arrow a b
;;; Arrow that ignores input and returns constant.
(define (arr-const x)
  (arr (const x)))

;;; arr-dup : Arrow a (a, a)
;;; Duplicate input.
(define arr-dup (arr (lambda (x) (cons x x))))

;;; arr-swap : Arrow (a, b) (b, a)
;;; Swap pair components.
(define arr-swap (arr (lambda (p) (cons (cdr p) (car p)))))

;;; arr-fst : Arrow (a, b) a
;;; Extract first component.
(define arr-fst (arr car))

;;; arr-snd : Arrow (a, b) b
;;; Extract second component.
(define arr-snd (arr cdr))

;;; arr-assoc : Arrow ((a, b), c) (a, (b, c))
;;; Reassociate nested pairs.
(define arr-assoc
  (arr (lambda (p)
               (let ([ab (car p)]
                     [c (cdr p)])
                    (cons (car ab) (cons (cdr ab) c))))))

;;; arr-unassoc : Arrow (a, (b, c)) ((a, b), c)
;;; Reverse reassociation.
(define arr-unassoc
  (arr (lambda (p)
               (let ([a (car p)]
                     [bc (cdr p)])
                    (cons (cons a (car bc)) (cdr bc))))))

;;; ============================================================
;;; ArrowChoice
;;; ============================================================
;;;
;;; ArrowChoice extends arrows with branching capability.
;;; Uses Either (left/right) for conditional execution.

;;; arr-left : Arrow a b -> Arrow (Either a c) (Either b c)
;;; Apply arrow to left values, pass through right values.
(define (arr-left f)
  (fn-arrow (lambda (e)
                    (if (left? e)
                        (left (run-arrow f (from-left e)))
                        e))))

;;; arr-right : Arrow a b -> Arrow (Either c a) (Either c b)
;;; Apply arrow to right values, pass through left values.
(define (arr-right f)
  (fn-arrow (lambda (e)
                    (if (right? e)
                        (right (run-arrow f (from-right e)))
                        e))))

;;; arrow-choice : Arrow a c -> Arrow b c -> Arrow (Either a b) c
;;; Route either left or right input to appropriate arrow (|||).
(define (arrow-choice f g)
  (fn-arrow (lambda (e)
                    (if (left? e)
                        (run-arrow f (from-left e))
                        (run-arrow g (from-right e))))))

;;; arrow-fanin : Arrow a c -> Arrow b c -> Arrow (Either a b) c
;;; Fanin composition (|||).
(define arrow-fanin arrow-choice)

;;; arrow-choose : Arrow a b -> Arrow c d -> Arrow (Either a c) (Either b d)
;;; Apply left or right arrow based on input (+++)
(define (arrow-choose f g)
  (>>> (arr-left f) (arr-right g)))

;;; arrow-sum : Arrow a b -> Arrow c d -> Arrow (Either a c) (Either b d)
;;; Alias for arrow-choose (+++).
(define arrow-sum arrow-choose)

;;; ============================================================
;;; Conditional Arrows
;;; ============================================================

;;; arr-if : (a -> Boolean) -> Arrow a b -> Arrow a b -> Arrow a b
;;; Conditional arrow based on predicate.
(define (arr-if pred then-arr else-arr)
  (fn-arrow (lambda (x)
                    (if (pred x)
                        (run-arrow then-arr x)
                        (run-arrow else-arr x)))))

;;; arr-case : List (pred, Arrow a b) -> Arrow a b -> Arrow a b
;;; Pattern matching on arrows.
(define (arr-case cases default)
  (fn-arrow (lambda (x)
                    (let loop ([cs cases])
                         (if (null? cs)
                             (run-arrow default x)
                             (let ([pred (caar cs)]
                                   [arr (cdar cs)])
                                  (if (pred x)
                                      (run-arrow arr x)
                                      (loop (cdr cs)))))))))

;;; ============================================================
;;; ArrowApply
;;; ============================================================
;;;
;;; ArrowApply allows arrows to be computed and then applied.
;;; This gives full monad power within the arrow framework.

;;; arr-apply : Arrow (Arrow a b, a) b
;;; Apply an arrow to its argument.
(define arr-apply
  (fn-arrow (lambda (pair)
                    (let ([f (car pair)]
                          [x (cdr pair)])
                         (run-arrow f x)))))

;;; ============================================================
;;; ArrowLoop
;;; ============================================================
;;;
;;; ArrowLoop enables feedback loops.
;;; loop : Arrow (a, c) (b, c) -> Arrow a b
;;; The 'c' value is fed back as input.

;;; arrow-loop : Arrow (a, c) (b, c) -> c -> Arrow a b
;;; Create a loop with initial feedback value.
(define (arrow-loop f init-c)
  (fn-arrow (lambda (a)
                    (let loop ([c init-c] [fuel 1000])
                         (if (= fuel 0)
                             (error 'arrow-loop "Loop did not converge")
                             (let* ([result (run-arrow f (cons a c))]
                                    [b (car result)]
                                    [c-new (cdr result)])
                                   (if (equal? c c-new)
                                       b
                                       (loop c-new (- fuel 1)))))))))

;;; arrow-fix : Arrow (a, c) (b, c) -> (c -> c -> Boolean) -> c -> Arrow a b
;;; Loop with custom convergence test.
(define (arrow-fix f converged? init-c)
  (fn-arrow (lambda (a)
                    (let loop ([c init-c] [fuel 1000])
                         (if (= fuel 0)
                             (error 'arrow-fix "Loop did not converge")
                             (let* ([result (run-arrow f (cons a c))]
                                    [b (car result)]
                                    [c-new (cdr result)])
                                   (if (converged? c c-new)
                                       b
                                       (loop c-new (- fuel 1)))))))))

;;; ============================================================
;;; Kleisli Arrows
;;; ============================================================
;;;
;;; Kleisli arrows wrap monadic functions (a -> m b).
;;; They need the monad's bind and pure to compose.

;;; make-kleisli : (a -> m b) -> (m a -> (a -> m b) -> m b) -> (b -> m b) -> KleisliArrow a b
;;; Create a Kleisli arrow from a monadic function.
(define (make-kleisli f m-bind m-pure)
  (list 'kleisli f m-bind m-pure))

;;; kleisli? : Any -> Boolean
(define (kleisli? x)
  (and (pair? x) (eq? (car x) 'kleisli)))

;;; kleisli-fn : KleisliArrow a b -> (a -> m b)
(define (kleisli-fn k) (list-ref k 1))

;;; kleisli-bind : KleisliArrow a b -> (m a -> (a -> m b) -> m b)
(define (kleisli-bind k) (list-ref k 2))

;;; kleisli-pure : KleisliArrow a b -> (b -> m b)
(define (kleisli-pure k) (list-ref k 3))

;;; run-kleisli : KleisliArrow a b -> a -> m b
;;; Execute a Kleisli arrow.
(define (run-kleisli k a)
  ((kleisli-fn k) a))

;;; kleisli-arr : (b -> m b) -> (a -> b) -> KleisliArrow a b
;;; Lift a pure function into a Kleisli arrow.
(define (kleisli-arr m-bind m-pure f)
  (make-kleisli (lambda (a) (m-pure (f a)))
                m-bind
                m-pure))

;;; kleisli-compose : KleisliArrow a b -> KleisliArrow b c -> KleisliArrow a c
;;; Compose two Kleisli arrows.
(define (kleisli-compose k1 k2)
  (let ([f1 (kleisli-fn k1)]
        [m-bind (kleisli-bind k1)]
        [m-pure (kleisli-pure k1)]
        [f2 (kleisli-fn k2)])
       (make-kleisli (lambda (a)
                             (m-bind (f1 a) f2))
                     m-bind
                     m-pure)))

;;; kleisli-first : KleisliArrow a b -> KleisliArrow (a, c) (b, c)
;;; Apply Kleisli arrow to first component.
(define (kleisli-first k)
  (let ([f (kleisli-fn k)]
        [m-bind (kleisli-bind k)]
        [m-pure (kleisli-pure k)])
       (make-kleisli (lambda (pair)
                             (let ([a (car pair)]
                                   [c (cdr pair)])
                                  (m-bind (f a)
                                          (lambda (b)
                                                  (m-pure (cons b c))))))
                     m-bind
                     m-pure)))

;;; ============================================================
;;; Arrow Combinators
;;; ============================================================

;;; arrow-map : (b -> c) -> Arrow a b -> Arrow a c
;;; Post-compose with a function.
(define (arrow-map f arr)
  (>>> arr (fn-arrow f)))

;;; arrow-pre : (a -> b) -> Arrow b c -> Arrow a c
;;; Pre-compose with a function.
(define (arrow-pre f arr)
  (>>> (fn-arrow f) arr))

;;; arrow-sequence : List (Arrow a a) -> Arrow a a
;;; Sequence a list of arrows.
(define (arrow-sequence arrs)
  (if (null? arrs)
      arr-id
      (>>> (car arrs) (arrow-sequence (cdr arrs)))))

;;; arrow-pipe : a -> List (Arrow a a) -> a
;;; Pipe a value through a list of arrows.
(define (arrow-pipe x arrs)
  (run-arrow (arrow-sequence arrs) x))

;;; arrow-fold : (b -> Arrow a b) -> b -> List a -> b
;;; Fold over a list using an arrow-generating function.
(define (arrow-fold f init lst)
  (if (null? lst)
      init
      (let ([result (run-arrow (f init) (car lst))])
           (arrow-fold f result (cdr lst)))))

;;; ============================================================
;;; Static Arrows (for analysis)
;;; ============================================================
;;;
;;; Static arrows carry metadata that can be analyzed
;;; without running the actual computation.

;;; make-static : Any -> Arrow a b -> StaticArrow a b
;;; Wrap an arrow with static information.
(define (make-static info arr)
  (list 'static info arr))

;;; static? : Any -> Boolean
(define (static? x)
  (and (pair? x) (eq? (car x) 'static)))

;;; static-info : StaticArrow a b -> Any
(define (static-info s) (list-ref s 1))

;;; static-arrow : StaticArrow a b -> Arrow a b
(define (static-arrow s) (list-ref s 2))

;;; run-static : StaticArrow a b -> a -> b
(define (run-static s x)
  (run-arrow (static-arrow s) x))

;;; static-compose : StaticArrow a b -> StaticArrow b c -> (i -> j -> k) -> StaticArrow a c
;;; Compose static arrows, combining info.
(define (static-compose s1 s2 combine-info)
  (make-static (combine-info (static-info s1) (static-info s2))
               (>>> (static-arrow s1) (static-arrow s2))))

;;; ============================================================
;;; Example: Tracing Arrow
;;; ============================================================
;;;
;;; An arrow that traces its execution path.

;;; traced : String -> Arrow a b -> Arrow a (b, List String)
;;; Wrap an arrow to trace its execution.
(define (traced name f)
  (fn-arrow (lambda (input)
                    (let* ([result (run-arrow f input)]
                           [trace (if (pair? result)
                                      (cdr result)
                                      '())]
                           [value (if (pair? result)
                                      (car result)
                                      result)])
                          (cons value (cons name trace))))))

;;; ============================================================
;;; Example: Profiling Arrow
;;; ============================================================

;;; timed : Arrow a b -> Arrow a (b, Number)
;;; Measure execution time (simplified - actual timing would need IO).
;;; This version counts steps instead.
(define (counted f)
  (fn-arrow (lambda (input)
                    (let ([result (run-arrow f input)])
                         (cons result 1)))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Simple arrow composition
;;; (define double (arr (lambda (x) (* x 2))))
;;; (define add1 (arr (lambda (x) (+ x 1))))
;;; (define pipeline (>>> double add1))
;;; (run-arrow pipeline 5)  ; => 11
;;;
;;; ;; Parallel arrows
;;; (define split-add (&&& (arr add1) (arr (lambda (x) (+ x 10)))))
;;; (run-arrow split-add 5)  ; => (6 . 15)
;;;
;;; ;; Arrow choice
;;; (define handle-either (||| (arr add1) (arr (lambda (x) (* x 2)))))
;;; (run-arrow handle-either (left 5))   ; => 6
;;; (run-arrow handle-either (right 5))  ; => 10
;;;
;;; ;; Conditional arrow
;;; (define abs-arrow
;;;   (arr-if negative?
;;;           (arr (lambda (x) (- x)))
;;;           arr-id))
;;; (run-arrow abs-arrow -5)  ; => 5
;;; (run-arrow abs-arrow 5)   ; => 5
