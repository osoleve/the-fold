;;; core/base/prelude.ss — Shared Utilities for Core Modules
;;; @module prelude
;;; @requires
;;;
;;; Common pure functions used across core/ modules.
;;; This is the ONLY place for these implementations.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies: NONE (this is the foundation)
;;;
;;; This is a BASE module — no internal core dependencies.

;;; ====
;;; List Predicates
;;; ====

;;; andmap : (α → Bool) × (List α) → Bool
;;; Apply predicate to all elements, return true if all pass.
;;; Returns #t for empty list (vacuous truth).
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

;;; ormap : (α → Bool) × (List α) → Bool
;;; Apply predicate to all elements, return true if any pass.
;;; Returns #f for empty list.
(define (ormap pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (ormap pred (cdr lst)))))

;;; ====
;;; List Utilities
;;; ====

;;; ====
;;; Duplicate Removal
;;; ====
;;;
;;; Three variants for different use cases:
;;;   - unique-simple: O(n^2) with memq - works on any list, symbols preferred
;;;   - unique-fast: O(n) with hashtable - best for large lists
;;;   - unique: Alias for unique-fast (recommended default)
;;;
;;; For custom equality, use distinct-by with a key function.

;;; unique-simple : (List α) → (List α)
;;; Remove duplicates using linear search. O(n^2) complexity.
;;; Uses eq? for comparison - best for symbols and small lists.
;;; Preserves first occurrence order.
(define (unique-simple lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(memq (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; unique-fast : (List α) → (List α)
;;; Remove duplicates using hash table. O(n) complexity.
;;; Uses equal? for comparison via equal-hash.
;;; Preserves first occurrence order.
(define (unique-fast lst)
  (let ([seen (make-hashtable equal-hash equal?)])
       (let loop ([items lst] [acc '()])
            (if (null? items)
                (reverse acc)
                (let ([x (car items)])
                     (if (hashtable-contains? seen x)
                         (loop (cdr items) acc)
                         (begin
                          (hashtable-set! seen x #t)
                          (loop (cdr items) (cons x acc)))))))))

;;; unique : (List α) → (List α)
;;; Remove duplicates, preserving first occurrence order.
;;; Alias for unique-fast (O(n) with hashtable).
;;; Use unique-simple if you need eq? semantics or very small lists.
(define unique unique-fast)

;;; remove-duplicates : (List α) → (List α)
;;; Alias for unique (common naming convention).
(define remove-duplicates unique)

;;; filter : (α → Bool) × (List α) → (List α)
;;; Keep only elements satisfying predicate.
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

;;; fold-left : (β × α → β) × β × (List α) → β
;;; Left-associative fold.
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

;;; foldl : (β × α → β) × β × (List α) → β
;;; Alias for fold-left (Haskell naming convention).
(define foldl fold-left)

;;; fold-right : (α × β → β) × β × (List α) → β
;;; Right-associative fold.
(define (fold-right f acc lst)
  (if (null? lst)
      acc
      (f (car lst) (fold-right f acc (cdr lst)))))

;;; foldr : (α × β → β) × β × (List α) → β
;;; Alias for fold-right (Haskell naming convention).
(define foldr fold-right)

;;; cons* : α × ... × β → (Improper List)
;;; Build an improper list from arguments, with the last as the tail.
;;; (cons* a) => a
;;; (cons* a b) => (cons a b)
;;; (cons* a b c) => (cons a (cons b c))
(define (cons* . args)
  (cond
   [(null? args) (error 'cons* "requires at least one argument")]
   [(null? (cdr args)) (car args)]
   [else (cons (car args) (apply cons* (cdr args)))]))

;;; zip : (List α) × (List β) → (List (Pair α β))
;;; Zip two lists together. Stops at shorter list.
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;;; iota : Nat → (List Nat)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; take : Nat × (List α) → (List α)
;;; Take first n elements.
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; drop : Nat × (List α) → (List α)
;;; Drop first n elements.
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

;;; find : (α → Bool) × (List α) → α | #f
;;; Find first element satisfying predicate. Returns #f if not found.
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; last : (List α) → α
;;; Get last element of non-empty list.
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; init : (List α) → (List α)
;;; Get all elements except the last.
(define (init lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (init (cdr lst)))))

;;; replicate : Nat × α → (List α)
;;; Create list of n copies of element.
(define (replicate n x)
  (if (<= n 0)
      '()
      (cons x (replicate (- n 1) x))))

;;; span : (α → Bool) × (List α) → (Values (List α) (List α))
;;; Split list at first element that fails predicate.
;;; Returns (prefix, suffix) where all elements in prefix satisfy pred.
(define (span pred lst)
  (cond
   [(null? lst) (values '() '())]
   [(pred (car lst))
    (let-values ([(pre suf) (span pred (cdr lst))])
                (values (cons (car lst) pre) suf))]
   [else (values '() lst)]))

;;; break : (α → Bool) × (List α) → (Values (List α) (List α))
;;; Split list at first element that satisfies predicate.
;;; Opposite of span.
(define (break pred lst)
  (span (lambda (x) (not (pred x))) lst))

;;; ====
;;; Collection Utilities (Missing Functional Primitives)
;;; ====

;;; identity : α → α
;;; The identity function - returns its argument unchanged.
(define (identity x) x)

;;; flatten : (List (List α)) → (List α)
;;; Flatten a list of lists into a single list.
(define (flatten lst-of-lists)
  (fold-right append '() lst-of-lists))

;;; partition : (α → Bool) × (List α) → (× (List α) (List α))
;;; Partition list into two lists: those satisfying predicate, and those that don't.
(define (partition pred lst)
  (let loop ([lst lst] [yes '()] [no '()])
       (cond
        [(null? lst) (list (reverse yes) (reverse no))]
        [(pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no)]
        [else (loop (cdr lst) yes (cons (car lst) no))])))

;;; group-by : (α → β) × (List α) → (List (Pair β (List α)))
;;; Group consecutive elements by key function result.
(define (group-by key-fn lst)
  (if (null? lst)
      '()
      (let ([first-key (key-fn (car lst))])
           (let loop ([remaining lst] [current-key first-key] [current-group '()] [result '()])
                (cond
                 [(null? remaining)
                  (reverse (cons (cons current-key (reverse current-group)) result))]
                 [(equal? (key-fn (car remaining)) current-key)
                  (loop (cdr remaining) current-key (cons (car remaining) current-group) result)]
                 [else
                  (let ([new-key (key-fn (car remaining))])
                       (loop (cdr remaining) new-key (list (car remaining))
                             (cons (cons current-key (reverse current-group)) result)))])))))

;;; distinct-by : (α → β) × (List α) → (List α)
;;; Remove duplicates based on key function, preserving first occurrence order.
(define (distinct-by key-fn lst)
  (let loop ([lst lst] [seen '()] [result '()])
       (cond
        [(null? lst) (reverse result)]
        [else
         (let* ([elem (car lst)]
                [key (key-fn elem)])
               (if (member key seen)
                   (loop (cdr lst) seen result)
                   (loop (cdr lst) (cons key seen) (cons elem result))))])))

;;; ====
;;; Result Type (Standardized Error Handling)
;;; ====

;;; All core modules use:
;;;   (ok value)           — success
;;;   (error tag details)  — failure
;;;
;;; This provides a standard way to handle errors functionally.

;;; ok? : Any → Bool
;;; Is this a successful result?
(define (ok? result)
  (and (pair? result) (eq? (car result) 'ok)))

;;; error? : Any → Bool
;;; Is this an error result?
(define (error? result)
  (and (pair? result) (eq? (car result) 'error)))

;;; unwrap-ok : (Result α ε) → α
;;; Extract value from (ok value). Assumes result is ok.
(define (unwrap-ok result)
  (cadr result))

;;; unwrap-error : Result → (tag . details)
;;; Extract error from (error tag ...). Assumes result is error.
(define (unwrap-error result)
  (cdr result))

;;; result-map : (α → β) × Result α → Result β
;;; Apply function to ok value, pass through errors.
(define (result-map f result)
  (if (ok? result)
      `(ok ,(f (unwrap-ok result)))
      result))

;;; result-bind : Result α × (α → Result β) → Result β
;;; Chain results, short-circuiting on error.
(define (result-bind result f)
  (if (ok? result)
      (f (unwrap-ok result))
      result))

;;; result-sequence : (List (Result α)) → Result (List α)
;;; Convert list of results to result of list.
;;; Short-circuits on first error.
(define (result-sequence results)
  (if (null? results)
      '(ok ())
      (let ([first (car results)])
           (if (error? first)
               first
               (let ([rest (result-sequence (cdr results))])
                    (if (error? rest)
                        rest
                        `(ok ,(cons (unwrap-ok first) (unwrap-ok rest)))))))))

;;; ====
;;; String Utilities
;;; ====
;;; Loaded from core/base/string/string.ss
;;; See submodules: string-core.ss, string-search.ss, string-format.ss
(load "core/base/string/string.ss")

;;; ====
;;; Debug Utilities
;;; ====

;;; trace : String × α → α
;;; Print a debug message and return the value unchanged.
;;; Useful for debugging without changing code structure.
(define (trace msg val)
  (display msg)
  (display ": ")
  (write val)
  (newline)
  val)

;;; errorf : Symbol × String × Any... → ⊥
;;; Raise an error with a formatted message.
;;; This prevents the common bug of using format placeholders (~a, ~s)
;;; directly in (error ...) which doesn't do substitution.
;;;
;;; WRONG: (error 'foo "Unknown value: ~a" value)  ; Shows literal ~a
;;; RIGHT: (errorf 'foo "Unknown value: ~a" value) ; Substitutes value
;;;
;;; Example:
;;;   (errorf 'fetch "Block not found: ~a" hash)
;;;   => Exception in fetch: Block not found: abc123...
(define (errorf who fmt-string . args)
  (error who (apply format fmt-string args)))

;;; ====
;;; Unicode Aliases — Mathematical Notation
;;; ====
;;;
;;; These are ALIASES, not replacements. Both forms work.
;;; The Fold embraces mathematical notation where it aids clarity.

;;; --- Core Forms ---

;;; λ : Lambda alias
;;; Usage: (λ (x) (* x x)) ≡ (lambda (x) (* x x))
(define-syntax λ
  (syntax-rules ()
                [(_ args body ...) (lambda args body ...)]))

;;; --- Logic ---

;;; ∧ : Logical conjunction (and)
(define-syntax ∧
  (syntax-rules ()
                [(_ e ...) (and e ...)]))

;;; ∨ : Logical disjunction (or)
(define-syntax ∨
  (syntax-rules ()
                [(_ e ...) (or e ...)]))

;;; doc : unevaluated metadata annotation
;;; Arguments are NOT evaluated. Returns void.
;;; Used for type annotations, todos, descriptions, etc.
;;; Syntax: (doc 'tag args...) for contextual
;;;         (doc target 'tag args...) for targeted
(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

;;; ¬ : Boolean → Boolean
;;; Logical negation (not).
(define ¬ not)

;;; --- Comparison ---

;;; ≠ : α × α → Boolean
;;; Not equal.
(define (≠ a b) (not (equal? a b)))

;;; ≤ : Nat × Nat → Boolean
;;; Less than or equal.
(define ≤ <=)

;;; ≥ : Nat × Nat → Boolean
;;; Greater than or equal.
(define ≥ >=)

;;; --- Arithmetic ---

;;; × : Nat* → Nat
;;; Multiplication (alternative to *).
(define × *)

;;; ÷ : Nat × Nat → Nat
;;; Division (alternative to /).
(define ÷ /)

;;; ² : Nat → Nat
;;; Square function.
(define (² x) (* x x))

;;; ³ : Nat → Nat
;;; Cube function.
(define (³ x) (* x x x))

;;; √ : Nat → Nat
;;; Square root.
(define √ sqrt)

;;; --- Collections ---

;;; ∈ : α × (List α) → Boolean
;;; Membership test (member, returns boolean).
(define (∈ x lst) (if (member x lst) #t #f))

;;; ∉ : α × (List α) → Boolean
;;; Non-membership test.
(define (∉ x lst) (not (∈ x lst)))

;;; ∅ : (List α)
;;; Empty list.
(define ∅ '())

;;; --- Function Composition ---

;;; ∘ : (β → γ) × (α → β) → (α → γ)
;;; Function composition (f ∘ g)(x) = f(g(x)).
(define (∘ f g) (λ (x) (f (g x))))

;;; --- Constants ---

;;; π : Nat
;;; Pi (3.14159...).
(define π 3.141592653589793)

;;; τ : Nat
;;; Tau (2π).
(define τ 6.283185307179586)

;;; 𝑒 : Nat
;;; Euler's number.
(define 𝑒 2.718281828459045)

;;; ∞ : Nat
;;; Infinity (largest flonum).
(define ∞ +inf.0)

;;; -∞ : Nat
;;; Negative infinity.
(define -∞ -inf.0)
