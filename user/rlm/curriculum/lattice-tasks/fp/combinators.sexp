;;; combinators.sexp — Development tasks for lattice/fp/meta/combinators.ss
;;;
;;; Module: combinators (tier 0, depends on prelude, sort)
;;; 513 lines, ~40 exported functions
;;; FP combinators: composition, currying, partial application, classic
;;; combinators (id, const, flip, on), tuple ops, Maybe/Either, list
;;; utilities, iteration combinators, memoization, Y combinator.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/fp/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement compose2
  ;; ──────────────────────────────────────────────
  ;; Binary function composition.

  (task
    (id "fp/combinators/compose2-impl")
    (module combinators)
    (tier 0)
    (type implement)
    (description "Implement compose2: compose two functions g and f, producing a new function that applies f first, then g. This is (g ∘ f)(x) = g(f(x)). Function composition is the most fundamental combinator — it chains transformations without naming intermediate results. Return a lambda that takes x and returns (g (f x)).")
    (context "Available: lambda. Single lambda expression.")
    (before "(define (compose2 g f)
  ;; TODO: (g ∘ f)(x) = g(f(x))
  f)")
    (test-file "lattice/fp/meta/test-combinators.ss")
    (test-forms (";; Double after increment: (3+1)*2 = 8
(assert-equal 8 ((compose2 (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3))"
                 ";; String operations
(assert-equal 6 ((compose2 string-length symbol->string) 'hello!))"
                 ";; Identity compositions
(assert-equal 42 ((compose2 (lambda (x) x) (lambda (x) (* x 6))) 7))"))
    (available-modules (prelude sort))
    (hints ("Return a lambda: (lambda (x) (g (f x)))"
            "f is applied first (inner), g second (outer)"))
    (reference-solution "(define (compose2 g f)
  (lambda (x) (g (f x))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement flip
  ;; ──────────────────────────────────────────────
  ;; Swap first two arguments.

  (task
    (id "fp/combinators/flip-impl")
    (module combinators)
    (tier 0)
    (type implement)
    (description "Implement flip: given a binary function f, return a new function that takes the same two arguments in reversed order. So (flip f) x y = f y x. This is useful when you need a function's arguments in a different order for composition or partial application. For example, (flip -) 3 10 = (- 10 3) = 7.")
    (context "Available: lambda. Single lambda returning a lambda.")
    (before "(define (flip f)
  ;; TODO: swap first two args
  f)")
    (test-file "lattice/fp/meta/test-combinators.ss")
    (test-forms (";; Flip subtraction: (flip -) 3 10 = 10 - 3 = 7
(assert-equal 7 ((flip -) 3 10))"
                 ";; Flip cons: (flip cons) '(1 2) 3 = (cons 3 '(1 2)) = (3 1 2)
(assert-equal '(3 1 2) ((flip cons) '(1 2) 3))"
                 ";; Flip division: (flip /) 2 10 = 10/2 = 5
(assert-equal 5 ((flip /) 2 10))"))
    (available-modules (prelude sort))
    (hints ("Return: (lambda (x y) (f y x))"
            "The arguments x and y are swapped before calling f"))
    (reference-solution "(define (flip f)
  (lambda (x y) (f y x)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement curry2
  ;; ──────────────────────────────────────────────
  ;; Curry a binary function.

  (task
    (id "fp/combinators/curry2-impl")
    (module combinators)
    (tier 0)
    (type implement)
    (description "Implement curry2: convert a binary function f(x, y) into a curried form, returning a function that takes x and returns a function that takes y and returns f(x, y). Currying enables partial application: ((curry2 +) 3) produces a function that adds 3 to its argument. This is the Schönfinkel/Curry transformation for 2-argument functions.")
    (context "Available: lambda. Nested lambdas.")
    (before "(define (curry2 f)
  ;; TODO: f(x,y) → x → y → f(x,y)
  f)")
    (test-file "lattice/fp/meta/test-combinators.ss")
    (test-forms (";; Curry addition: ((curry2 +) 3) 4 = 7
(assert-equal 7 (((curry2 +) 3) 4))"
                 ";; Partial application: add3 = (curry2 +) 3
(let ([add3 ((curry2 +) 3)])
  (assert-equal 8 (add3 5))
  (assert-equal 3 (add3 0)))"
                 ";; Curry cons
(assert-equal '(a . b) (((curry2 cons) 'a) 'b))"))
    (available-modules (prelude sort))
    (hints ("Outer lambda takes x"
            "Inner lambda takes y, calls (f x y)"
            "(lambda (x) (lambda (y) (f x y)))"))
    (reference-solution "(define (curry2 f)
  (lambda (x)
          (lambda (y)
                  (f x y))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement intersperse
  ;; ──────────────────────────────────────────────
  ;; Place separator between list elements.

  (task
    (id "fp/combinators/intersperse-impl")
    (module combinators)
    (tier 0)
    (type implement)
    (description "Implement intersperse: place a separator element between each pair of consecutive elements in a list. For example, (intersperse 'x '(a b c d)) = (a x b x c x d). Empty and singleton lists are returned as-is. For lists with 2+ elements, keep the first element, then fold the rest, prepending the separator before each element.")
    (context "Available: null?, cdr, or, car, cons, fold-right, lambda. Guard + fold pattern.")
    (before "(define (intersperse sep xs)
  ;; TODO: place sep between each pair of elements
  xs)")
    (test-file "lattice/fp/meta/test-combinators.ss")
    (test-forms (";; Normal case
(assert-equal '(a x b x c x d) (intersperse 'x '(a b c d)))"
                 ";; Singleton: no separators needed
(assert-equal '(a) (intersperse 'x '(a)))"
                 ";; Empty list
(assert-equal '() (intersperse 'x '()))"
                 ";; Two elements
(assert-equal '(1 0 2) (intersperse 0 '(1 2)))"))
    (available-modules (prelude sort))
    (hints ("Guard: (or (null? xs) (null? (cdr xs))) → xs"
            "Keep first element, fold-right over rest"
            "(cons (car xs) (fold-right (lambda (x acc) (cons sep (cons x acc))) '() (cdr xs)))"))
    (reference-solution "(define (intersperse sep xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (cons (car xs)
            (fold-right (lambda (x acc) (cons sep (cons x acc)))
                        '()
                        (cdr xs)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement iterate-n
  ;; ──────────────────────────────────────────────
  ;; Generate list by repeated function application.

  (task
    (id "fp/combinators/iterate-n-impl")
    (module combinators)
    (tier 0)
    (type implement)
    (description "Implement iterate-n: generate the first n values of repeated function application starting from x. Returns (x, f(x), f(f(x)), ...) — n elements total. This is the discrete orbit of x under f. For example, (iterate-n double 1 5) = (1 2 4 8 16). Use a named let loop with counter i, current value x, and accumulator. Collect values in reverse, then reverse at the end.")
    (context "Available: >=, +, cons, reverse, let. Named let with three accumulators (i, x, acc).")
    (before "(define (iterate-n f x n)
  ;; TODO: first n values of repeated application
  '())")
    (test-file "lattice/fp/meta/test-combinators.ss")
    (test-forms (";; Doubling from 1: (1 2 4 8 16)
(assert-equal '(1 2 4 8 16) (iterate-n (lambda (x) (* x 2)) 1 5))"
                 ";; Zero iterations: empty
(assert-equal '() (iterate-n (lambda (x) (* x 2)) 1 0))"
                 ";; Single iteration: just the seed
(assert-equal '(7) (iterate-n (lambda (x) (* x 2)) 7 1))"
                 ";; Increment: (0 1 2 3 4)
(assert-equal '(0 1 2 3 4) (iterate-n (lambda (x) (+ x 1)) 0 5))"))
    (available-modules (prelude sort))
    (hints ("Named let: (let loop ([i 0] [x x] [acc '()]) ...)"
            "Base: (>= i n) → (reverse acc)"
            "Step: (loop (+ i 1) (f x) (cons x acc))"
            "Note: cons x (not f(x)) because we record BEFORE applying"))
    (reference-solution "(define (iterate-n f x n)
  (let loop ([i 0] [x x] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (f x) (cons x acc)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
