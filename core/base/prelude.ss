;;; doc : unevaluated metadata annotation
;;; MUST be defined FIRST, before any uses.
(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'prelude)
(doc 'description "Shared utilities for core modules. Common pure functions used across core/. This is the ONLY place for these implementations.")
(doc 'layer 'core)
(doc 'purity 'total)
(doc 'dependencies '())
(doc 'note "BASE module — no internal core dependencies.")

(doc 'section 'list-predicates)

(define (andmap pred lst)
  (doc 'type (-> (-> α Bool) (List α) Bool))
  (doc 'description "Apply predicate to all elements, return true if all pass. Returns #t for empty list (vacuous truth).")
  (doc 'export #t)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

(define (ormap pred lst)
  (doc 'type (-> (-> α Bool) (List α) Bool))
  (doc 'description "Apply predicate to all elements, return true if any pass. Returns #f for empty list.")
  (doc 'export #t)
  (and (pair? lst)
       (or (pred (car lst))
           (ormap pred (cdr lst)))))

;; Aliases for ormap/andmap (more discoverable names)
(define any? ormap)
(doc any? 'alias 'ormap)
(define all? andmap)
(doc all? 'alias 'andmap)

(doc 'section 'list-utilities)

(doc 'section 'duplicate-removal)
(doc 'note "Three variants for different use cases: unique-simple O(n^2) with memq works on any list symbols preferred, unique-fast O(n) with hashtable best for large lists, unique alias for unique-fast recommended default. For custom equality use distinct-by with a key function.")

(define (unique-simple lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Remove duplicates using linear search. O(n^2) complexity. Uses eq? for comparison - best for symbols and small lists. Preserves first occurrence order.")
  (doc 'export #t)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(memq (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

(define (unique-fast lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Remove duplicates using hash table. O(n) complexity. Uses equal? for comparison via equal-hash. Preserves first occurrence order.")
  (doc 'export #t)
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

(doc unique 'type (-> (List α) (List α)))
(doc unique 'description "Remove duplicates, preserving first occurrence order. Alias for unique-fast (O(n) with hashtable). Use unique-simple if you need eq? semantics or very small lists.")
(doc unique 'export #t)
(define unique unique-fast)

(doc remove-duplicates 'type (-> (List α) (List α)))
(doc remove-duplicates 'description "Alias for unique (common naming convention).")
(doc remove-duplicates 'export #t)
(define remove-duplicates unique)

(define (filter pred lst)
  (doc 'type (-> (-> α Bool) (List α) (List α)))
  (doc 'description "Keep only elements satisfying predicate.")
  (doc 'export #t)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

(define (filter-map f lst)
  (doc 'type (-> (-> α (Maybe β)) (List α) (List β)))
  (doc 'description "Map function over list, keeping only non-#f results.")
  (doc 'export #t)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (if result
              (loop (cdr lst) (cons result acc))
              (loop (cdr lst) acc))))))

(define (fold-left f acc lst)
  (doc 'type (-> (-> β α β) β (List α) β))
  (doc 'description "Left-associative fold.")
  (doc 'export #t)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

(doc foldl 'type (-> (-> β α β) β (List α) β))
(doc foldl 'description "Alias for fold-left (Haskell naming convention).")
(doc foldl 'export #t)
(define foldl fold-left)

(define (fold-right f acc lst)
  (doc 'type (-> (-> α β β) β (List α) β))
  (doc 'description "Right-associative fold.")
  (doc 'export #t)
  (if (null? lst)
      acc
      (f (car lst) (fold-right f acc (cdr lst)))))

(doc foldr 'type (-> (-> α β β) β (List α) β))
(doc foldr 'description "Alias for fold-right (Haskell naming convention).")
(doc foldr 'export #t)
(define foldr fold-right)

(define (cons* . args)
  (doc 'type (-> α ... β (Improper-List)))
  (doc 'description "Build an improper list from arguments, with the last as the tail. (cons* a) => a, (cons* a b) => (cons a b), (cons* a b c) => (cons a (cons b c)).")
  (doc 'export #t)
  (cond
   [(null? args) (error 'cons* "requires at least one argument")]
   [(null? (cdr args)) (car args)]
   [else (cons (car args) (apply cons* (cdr args)))]))

(define (zip xs ys)
  (doc 'type (-> (List α) (List β) (List (Pair α β))))
  (doc 'description "Zip two lists together. Stops at shorter list.")
  (doc 'export #t)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

(define (iota n)
  (doc 'type (-> Nat (List Nat)))
  (doc 'description "Generate list [0, 1, ..., n-1].")
  (doc 'export #t)
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

(define (range start end)
  (doc 'type (-> Int Int (List Int)))
  (doc 'description "Generate list [start, start+1, ..., end-1].")
  (doc 'export #t)
  (let loop ([i start] [acc '()])
    (if (>= i end)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(define (take n lst)
  (doc 'type (-> Nat (List α) (List α)))
  (doc 'description "Take first n elements.")
  (doc 'export #t)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (doc 'type (-> Nat (List α) (List α)))
  (doc 'description "Drop first n elements.")
  (doc 'export #t)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

(define (find pred lst)
  (doc 'type (-> (-> α Bool) (List α) (Maybe α)))
  (doc 'description "Find first element satisfying predicate. Returns #f if not found.")
  (doc 'export #t)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

(define (last lst)
  (doc 'type (-> (List α) α))
  (doc 'description "Get last element of non-empty list.")
  (doc 'export #t)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (init lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Get all elements except the last.")
  (doc 'export #t)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (init (cdr lst)))))

(define (replicate n x)
  (doc 'type (-> Nat α (List α)))
  (doc 'description "Create list of n copies of element.")
  (doc 'export #t)
  (if (<= n 0)
      '()
      (cons x (replicate (- n 1) x))))

;; Alias for replicate (Racket/SRFI naming)
(define make-list replicate)
(doc make-list 'alias 'replicate)

(define (span pred lst)
  (doc 'type (-> (-> α Bool) (List α) (Values (List α) (List α))))
  (doc 'description "Split list at first element that fails predicate. Returns (prefix, suffix) where all elements in prefix satisfy pred.")
  (doc 'export #t)
  (cond
   [(null? lst) (values '() '())]
   [(pred (car lst))
    (let-values ([(pre suf) (span pred (cdr lst))])
                (values (cons (car lst) pre) suf))]
   [else (values '() lst)]))

(define (break pred lst)
  (doc 'type (-> (-> α Bool) (List α) (Values (List α) (List α))))
  (doc 'description "Split list at first element that satisfies predicate. Opposite of span.")
  (doc 'export #t)
  (span (lambda (x) (not (pred x))) lst))

(doc 'section 'collection-utilities)
(doc 'note "Missing functional primitives.")

(define (identity x)
  (doc 'type (-> α α))
  (doc 'description "The identity function - returns its argument unchanged.")
  (doc 'export #t)
  x)

(define (flatten lst-of-lists)
  (doc 'type (-> (List (List α)) (List α)))
  (doc 'description "Flatten a list of lists into a single list.")
  (doc 'export #t)
  (fold-right append '() lst-of-lists))

(define (partition pred lst)
  (doc 'type (-> (-> α Bool) (List α) (List (List α) (List α))))
  (doc 'description "Partition list into two lists: those satisfying predicate, and those that don't.")
  (doc 'export #t)
  (let loop ([lst lst] [yes '()] [no '()])
       (cond
        [(null? lst) (list (reverse yes) (reverse no))]
        [(pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no)]
        [else (loop (cdr lst) yes (cons (car lst) no))])))

(define (group-by key-fn lst)
  (doc 'type (-> (-> α β) (List α) (List (Pair β (List α)))))
  (doc 'description "Group consecutive elements by key function result.")
  (doc 'export #t)
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

(define (distinct-by key-fn lst)
  (doc 'type (-> (-> α β) (List α) (List α)))
  (doc 'description "Remove duplicates based on key function, preserving first occurrence order.")
  (doc 'export #t)
  (let loop ([lst lst] [seen '()] [result '()])
       (cond
        [(null? lst) (reverse result)]
        [else
         (let* ([elem (car lst)]
                [key (key-fn elem)])
               (if (member key seen)
                   (loop (cdr lst) seen result)
                   (loop (cdr lst) (cons key seen) (cons elem result))))])))

(doc 'section 'result-type)
(doc 'note "Standardized error handling. All core modules use: (ok value) for success, (error tag details) for failure. This provides a standard way to handle errors functionally.")

(define (ok? result)
  (doc 'type (-> Any Bool))
  (doc 'description "Is this a successful result?")
  (doc 'export #t)
  (and (pair? result) (eq? (car result) 'ok)))

(define (error? result)
  (doc 'type (-> Any Bool))
  (doc 'description "Is this an error result?")
  (doc 'export #t)
  (and (pair? result) (eq? (car result) 'error)))

(define (unwrap-ok result)
  (doc 'type (-> (Result α ε) α))
  (doc 'description "Extract value from (ok value). Assumes result is ok.")
  (doc 'export #t)
  (cadr result))

(define (unwrap-error result)
  (doc 'type (-> Result (Pair Symbol Any)))
  (doc 'description "Extract error from (error tag ...). Assumes result is error.")
  (doc 'export #t)
  (cdr result))

(define (result-map f result)
  (doc 'type (-> (-> α β) (Result α) (Result β)))
  (doc 'description "Apply function to ok value, pass through errors.")
  (doc 'export #t)
  (if (ok? result)
      `(ok ,(f (unwrap-ok result)))
      result))

(define (result-bind result f)
  (doc 'type (-> (Result α) (-> α (Result β)) (Result β)))
  (doc 'description "Chain results, short-circuiting on error.")
  (doc 'export #t)
  (if (ok? result)
      (f (unwrap-ok result))
      result))

(define (result-sequence results)
  (doc 'type (-> (List (Result α)) (Result (List α))))
  (doc 'description "Convert list of results to result of list. Short-circuits on first error.")
  (doc 'export #t)
  (if (null? results)
      '(ok ())
      (let ([first (car results)])
           (if (error? first)
               first
               (let ([rest (result-sequence (cdr results))])
                    (if (error? rest)
                        rest
                        `(ok ,(cons (unwrap-ok first) (unwrap-ok rest)))))))))

(doc 'section 'string-utilities)
(doc 'note "Loaded from core/base/string/string.ss. See submodules: string-core.ss, string-search.ss, string-format.ss.")
(load "core/base/string/string.ss")

(doc 'section 'debug-utilities)

(define (trace msg val)
  (doc 'type (-> String α α))
  (doc 'description "Print a debug message and return the value unchanged. Useful for debugging without changing code structure.")
  (doc 'export #t)
  (display msg)
  (display ": ")
  (write val)
  (newline)
  val)

(define (errorf who fmt-string . args)
  (doc 'type (-> Symbol String Any ... ⊥))
  (doc 'description "Raise an error with a formatted message. This prevents the common bug of using format placeholders (~a ~s) directly in (error ...) which doesn't do substitution.")
  (doc 'note "WRONG: (error 'foo \"Unknown value: ~a\" value) shows literal ~a. RIGHT: (errorf 'foo \"Unknown value: ~a\" value) substitutes value.")
  (doc 'export #t)
  (error who (apply format fmt-string args)))

(doc 'section 'unicode-aliases)
(doc 'note "Mathematical notation. These are ALIASES not replacements. Both forms work. The Fold embraces mathematical notation where it aids clarity.")

(doc 'section 'core-forms)

(doc λ 'type Syntax)
(doc λ 'description "Lambda alias. Usage: (λ (x) (* x x)) ≡ (lambda (x) (* x x)).")
(doc λ 'export #t)
(define-syntax λ
  (syntax-rules ()
                [(_ args body ...) (lambda args body ...)]))

(doc 'section 'logic)

(doc ∧ 'type Syntax)
(doc ∧ 'description "Logical conjunction (and).")
(doc ∧ 'export #t)
(define-syntax ∧
  (syntax-rules ()
                [(_ e ...) (and e ...)]))

(doc ∨ 'type Syntax)
(doc ∨ 'description "Logical disjunction (or).")
(doc ∨ 'export #t)
(define-syntax ∨
  (syntax-rules ()
                [(_ e ...) (or e ...)]))

(doc doc 'type Syntax)
(doc doc 'description "Unevaluated metadata annotation. Arguments are NOT evaluated. Returns void. Used for type annotations todos descriptions etc.")
(doc doc 'note "Syntax: (doc 'tag args...) for contextual, (doc target 'tag args...) for targeted.")
(doc doc 'export #t)

(doc ¬ 'type (-> Bool Bool))
(doc ¬ 'description "Logical negation (not).")
(doc ¬ 'export #t)
(define ¬ not)

(doc 'section 'comparison)

(define (≠ a b)
  (doc 'type (-> α α Bool))
  (doc 'description "Not equal.")
  (doc 'export #t)
  (not (equal? a b)))

(doc ≤ 'type (-> Nat Nat Bool))
(doc ≤ 'description "Less than or equal.")
(doc ≤ 'export #t)
(define ≤ <=)

(doc ≥ 'type (-> Nat Nat Bool))
(doc ≥ 'description "Greater than or equal.")
(doc ≥ 'export #t)
(define ≥ >=)

(doc 'section 'arithmetic)

(doc × 'type (-> Nat ... Nat))
(doc × 'description "Multiplication (alternative to *).")
(doc × 'export #t)
(define × *)

(doc ÷ 'type (-> Nat Nat Nat))
(doc ÷ 'description "Division (alternative to /).")
(doc ÷ 'export #t)
(define ÷ /)

(define (² x)
  (doc 'type (-> Nat Nat))
  (doc 'description "Square function.")
  (doc 'export #t)
  (* x x))

(define (³ x)
  (doc 'type (-> Nat Nat))
  (doc 'description "Cube function.")
  (doc 'export #t)
  (* x x x))

(doc √ 'type (-> Nat Nat))
(doc √ 'description "Square root.")
(doc √ 'export #t)
(define √ sqrt)

(doc 'section 'collections)

(define (∈ x lst)
  (doc 'type (-> α (List α) Bool))
  (doc 'description "Membership test (member, returns boolean).")
  (doc 'export #t)
  (if (member x lst) #t #f))

(define (∉ x lst)
  (doc 'type (-> α (List α) Bool))
  (doc 'description "Non-membership test.")
  (doc 'export #t)
  (not (∈ x lst)))

(doc ∅ 'type (List α))
(doc ∅ 'description "Empty list.")
(doc ∅ 'export #t)
(define ∅ '())

(doc 'section 'function-composition)

(define (∘ f g)
  (doc 'type (-> (-> β γ) (-> α β) (-> α γ)))
  (doc 'description "Function composition (f ∘ g)(x) = f(g(x)).")
  (doc 'export #t)
  (λ (x) (f (g x))))

(doc 'section 'constants)

(doc π 'type Nat)
(doc π 'description "Pi (3.14159...).")
(doc π 'export #t)
(define π 3.141592653589793)

(doc τ 'type Nat)
(doc τ 'description "Tau (2π).")
(doc τ 'export #t)
(define τ 6.283185307179586)

(doc 𝑒 'type Nat)
(doc 𝑒 'description "Euler's number.")
(doc 𝑒 'export #t)
(define 𝑒 2.718281828459045)

(doc ∞ 'type Nat)
(doc ∞ 'description "Infinity (largest flonum).")
(doc ∞ 'export #t)
(define ∞ +inf.0)

(doc -∞ 'type Nat)
(doc -∞ 'description "Negative infinity.")
(doc -∞ 'export #t)
(define -∞ -inf.0)
