(until (fix until
            (fn (stop? f x)
                (if (stop? x)
                    x
                    (until stop? f (f x))))))

; converge: Apply function until result stops changing (fixed point)
; (converge (fn (x) (/ (+ x (/ 2 x)) 2)) 1.0) => ~1.414 (sqrt 2)
; (converge (fn (x) (if (> x 5) x (+ x 1))) 0) => 6
(converge (fix converge
               (fn (f x)
                   (let ((next (f x)))
                        (if (equal? x next)
                            x
                            (converge f next))))))

; fixed-point: Same as converge (alternative name)
(fixed-point converge)

; -- Predicate combinators --

; both: Combine two predicates with and
; ((both positive? even?) 4) => #t
(both (fn (p1 p2) (fn (x) (and (p1 x) (p2 x)))))

; either-pred: Combine two predicates with or
; ((either-pred positive? even?) -2) => #t
(either-pred (fn (p1 p2) (fn (x) (or (p1 x) (p2 x)))))

; neither: Combine two predicates with nor
(neither (fn (p1 p2) (fn (x) (not (or (p1 x) (p2 x))))))

; -- More list utilities --

; -- String higher-order functions --

; string-map: Apply function to each character of string
; (string-map char-upcase "hello") => "HELLO"
; string-filter: Keep characters matching predicate
; (string-filter char-alphabetic? "a1b2c3") => "abc"
; string-any: Check if any character satisfies predicate
; (string-any char-numeric? "abc123") => #t
; string-all: Check if all characters satisfy predicate
; (string-all char-alphabetic? "abc") => #t
; string-foldl: Left fold over string characters
; (string-foldl (fn (acc c) (+ acc 1)) 0 "hello") => 5
; string-foldr: Right fold over string characters
; words: Split string on whitespace
; (words "hello world  foo") => ("hello" "world" "foo")
(words (fn (str)
           (filter (fn (s) (not (string-empty? s))) (string-split str " "))))

; unwords: Join strings with spaces
; (unwords '("hello" "world")) => "hello world"
(unwords (fn (strs)
             (if (null? strs)
                 ""
                 (foldl (fn (acc s) (string-append acc " " s))
                        (car strs)
                        (cdr strs)))))

; lines: Split string on newlines
; (lines "a\nb\nc") => ("a" "b" "c")
(lines (fn (str)
           (string-split str "\n")))

; unlines: Join strings with newlines
; (unlines '("a" "b" "c")) => "a\nb\nc"
(unlines (fn (strs)
             (if (null? strs)
                 ""
                 (foldl (fn (acc s) (string-append acc "\n" s))
                        (car strs)
                        (cdr strs)))))

; join: Join strings with separator
; (join ", " '("a" "b" "c")) => "a, b, c"
(join (fn (sep strs)
          (if (null? strs)
              ""
              (foldl (fn (acc s) (string-append acc sep s))
                     (car strs)
                     (cdr strs)))))

; string-take-while: Take characters while predicate holds
; string-drop-while: Drop characters while predicate holds
; string-find: Find first character matching predicate
; string-count: Count characters matching predicate
; string-partition: Split string into matching and non-matching chars
; char-between?: Check if character code is in range
(char-between? (fn (lo hi c)
                   (let ((code (char->integer c)))
                        (and (>= code (char->integer lo))
                             (<= code (char->integer hi))))))

; char-alphabetic?: Check if character is a letter
(char-alphabetic? (fn (c)
                      (or (char-between? #\a #\z c)
                          (char-between? #\A #\Z c))))

; char-numeric?: Check if character is a digit
(char-numeric? (fn (c)
                   (char-between? #\0 #\9 c)))

; char-whitespace?: Check if character is whitespace
(char-whitespace? (fn (c)
                      (or (eq? c #\space)
                          (or (eq? c #\tab)
                              (eq? c #\newline)))))

; char-upcase: Convert character to uppercase
(char-upcase (fn (c)
                 (if (char-between? #\a #\z c)
                     (integer->char (- (char->integer c) 32))
                     c)))

; char-downcase: Convert character to lowercase
(char-downcase (fn (c)
                   (if (char-between? #\A #\Z c)
                       (integer->char (+ (char->integer c) 32))
                       c)))

; -- Set operations (lists as sets) --

; -- Association list utilities --

; alist-invert: Swap keys and values
; (alist-invert '((a . 1) (b . 2))) => ((1 . a) (2 . b))
; alist-group: Group list elements by key function into alist
; (alist-group even? '(1 2 3 4)) => ((#f 1 3) (#t 2 4))
(alist-group (fn (key-fn lst)
                 (let ((keys (nub (map key-fn lst))))
                      (map (fn (k)
                               (cons k (filter (fn (x) (eq? (key-fn x) k)) lst)))
                           keys))))

; -- Control flow utilities --

; when-let: Execute body if value is truthy, with value bound
; Like (let ((x expr)) (if x body #f))
; if-let: Like when-let but with else clause
; cond-fn: Create function that tests predicates in order
; ((cond-fn (list (cons even? "even") (cons odd? "odd"))) 3) => "odd"
(cond-fn (fix cond-fn
              (fn (clauses)
                  (fn (x)
                      (if (null? clauses)
                          #f
                          (if ((car (car clauses)) x)
                              (cdr (car clauses))
                              ((cond-fn (cdr clauses)) x)))))))

; thread-first: Thread value through functions (value as first arg)
; (thread-first 5 (list inc double)) => 12
(thread-first (fn (val fns)
                  (foldl (fn (acc f) (f acc)) val fns)))

; thread-last: Same as thread-first for single-arg functions
(thread-last thread-first)

; tap: Apply function for side effect, return original value
; Useful for debugging in pipelines
; -- More numeric utilities --

; clamp-list: Clamp all values in list to range
; (clamp-list 0 10 '(-5 5 15)) => (0 5 10)
; Note: clamp primitive is (clamp value lo hi)
(clamp-list (fn (lo hi lst)
                (map (fn (x) (clamp x lo hi)) lst)))

; normalize: Scale list values to 0-1 range
; (normalize '(0 50 100)) => (0 0.5 1)
(normalize (fn (lst)
               (if (null? lst)
                   '()
                   (let ((lo (foldl min (car lst) lst))
                         (hi (foldl max (car lst) lst)))
                        (if (= lo hi)
                            (map (const 0) lst)
                            (map (fn (x) (/ (- x lo) (- hi lo))) lst))))))

; running-sum: Cumulative sum
; (running-sum '(1 2 3 4)) => (1 3 6 10)
(running-sum (fn (lst)
                 (cdr (scanl + 0 lst))))

; running-product: Cumulative product
; (running-product '(1 2 3 4)) => (1 2 6 24)
(running-product (fn (lst)
                     (cdr (scanl * 1 lst))))

; differences: Consecutive differences
; (differences '(1 3 6 10)) => (2 3 4)
(differences (fn (lst)
                 (if (null? lst)
                     '()
                     (if (null? (cdr lst))
                         '()
                         (zip-with - (cdr lst) lst)))))

; -- List rotation and shuffling --

; -- Predicate utilities --

; all-equal?: Check if all elements are equal
; (all-equal? '(1 1 1)) => #t
; sorted?: Check if list is sorted (ascending)
; (sorted? '(1 2 3)) => #t
; sorted-by?: Check if list is sorted by key function
(sorted-by? (fn (key-fn lst)
                (sorted? (map key-fn lst))))

; palindrome?: Check if list is a palindrome
; (palindrome? '(1 2 1)) => #t
(palindrome? (fix palindrome?
                  (fn (lst)
                      (if (null? lst)
                          #t
                          (if (null? (cdr lst))
                              #t
                              (if (eq? (car lst) (last lst))
                                  (palindrome? (cdr (init lst)))
                                  #f))))))

; -- Tree/Nested structure utilities --

; deep-map: Apply function to all atoms in nested structure
; (deep-map inc '((1 2) (3 (4 5)))) => ((2 3) (4 (5 6)))
(deep-map (fix deep-map
               (fn (f tree)
                   (if (null? tree)
                       '()
                       (if (pair? tree)
                           (cons (deep-map f (car tree))
                                 (deep-map f (cdr tree)))
                           (f tree))))))

; deep-filter: Keep atoms matching predicate (preserves structure)
; (deep-filter even? '((1 2) (3 (4 5)))) => ((() 2) (() (4 ())))
(deep-filter (fix deep-filter
                  (fn (p tree)
                      (if (null? tree)
                          '()
                          (if (pair? tree)
                              (cons (deep-filter p (car tree))
                                    (deep-filter p (cdr tree)))
                              (if (p tree) tree '()))))))

; flatten-deep: Flatten all nested lists into single list
; (flatten-deep '((1 2) (3 (4 5)))) => (1 2 3 4 5)
(flatten-deep (fix flatten-deep
                   (fn (tree)
                       (if (null? tree)
                           '()
                           (if (pair? tree)
                               (append (flatten-deep (car tree))
                                       (flatten-deep (cdr tree)))
                               (list tree))))))

; tree-depth: Maximum nesting depth of structure
; (tree-depth '((1 2) (3 (4 5)))) => 3
; tree-size: Count all atoms in nested structure
; (tree-size '((1 2) (3 (4 5)))) => 5
; tree-find: Find first atom matching predicate in tree
; (tree-find even? '((1 3) (5 (6 7)))) => 6
; -- More function combinators --

; curry3: Curry a 3-argument function
; (((curry3 (fn (a b c) (+ a (+ b c)))) 1) 2) 3) => 6
(curry3 (fn (f)
            (fn (a) (fn (b) (fn (c) (f a b c))))))

; uncurry3: Uncurry to a 3-argument function
(uncurry3 (fn (f)
              (fn (a b c) (((f a) b) c))))

; partial1: Partially apply first argument (returns 1-arg function)
; ((partial1 + 10) 5) => 15
(partial1 (fn (f x)
              (fn (y) (f x y))))

; partial2: Partially apply first two arguments (returns 1-arg function)
; ((partial2 (fn (a b c) (+ a (+ b c))) 1 2) 3) => 6
; compose-n: Compose multiple functions right-to-left
; ((compose-n (list inc inc double)) 5) => 12
; pipe-n: Compose multiple functions left-to-right
; ((pipe-n (list double inc inc)) 5) => 12
; memoize-1: Stub for memoization (returns function as-is in pure context)
; Note: True memoization requires mutation which isn't well-supported
(memoize-1 (fn (f) f))

; -- Math utilities --

; fibonacci: nth Fibonacci number
; (fibonacci 10) => 55
(fibonacci (fix fibonacci
                (fn (n)
                    (if (<= n 1)
                        n
                        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

; gcd-list: GCD of a list of numbers
; (gcd-list '(12 18 24)) => 6
(gcd-list (fn (lst)
              (foldl gcd (car lst) (cdr lst))))

; lcm-list: LCM of a list of numbers
; (lcm-list '(4 6 8)) => 24
(lcm-list (fn (lst)
              (foldl lcm (car lst) (cdr lst))))

; prime?: Check if number is prime
; (prime? 17) => #t
; -- Validation utilities --

; validate: Apply validator, return value or error
; (validate positive? 5) => 5
; (validate positive? -5) => #f
; validate-all: Check all predicates pass
; (validate-all (list positive? even?) 4) => 4
; ensure: Like validate but with default
; (ensure positive? 0 -5) => 0
; -- Debug utilities --

; trace: Identity with side-effect printing (pure version just returns)
; spy: Like trace but shows both label and value
; assert-eq: Check equality (returns #t or #f)
; assert-pred: Check predicate holds
; -- Logical utilities --

; -- More list utilities --

; take-n: Take first n elements (HOF version)
; drop-n: Drop first n elements (HOF version)
; nth: Get nth element (0-indexed)
(nth (fix nth
          (fn (n lst)
              (if (null? lst)
                  '()
                  (if (= n 0)
                      (car lst)
                      (nth (- n 1) (cdr lst)))))))

; init: All but last element
(init (fix init
           (fn (lst)
               (if (null? lst)
                   '()
                   (if (null? (cdr lst))
                       '()
                       (cons (car lst) (init (cdr lst))))))))

; last: Last element of list
(last (fix last
           (fn (lst)
               (if (null? lst)
                   '()
                   (if (null? (cdr lst))
                       (car lst)
                       (last (cdr lst)))))))

; butlast: Remove last n elements
; count-eq: Count elements equal to x
(count-eq (fn (x lst)
              (foldl (fn (acc item) (if (= item x) (+ acc 1) acc)) 0 lst)))

; -- Association list utilities --

; assoc: Find pair with matching key
(assoc (fix assoc
            (fn (key alist)
                (if (null? alist)
                    #f
                    (if (eq? key (car (car alist)))
                        (car alist)
                        (assoc key (cdr alist)))))))

; assoc-ref: Get value for key (or #f)
; assoc-set: Set value for key (returns new alist)
; assoc-remove: Remove key from alist
; assoc-keys: Get all keys from alist
; assoc-values: Get all values from alist
; -- More string utilities --

; Note: string-contains? is a primitive (haystack needle) - uses Rust's contains()

; string-prefix?: Check if string starts with prefix
; string-suffix?: Check if string ends with suffix
; -- Numeric sequences --

; iota: Generate list of n integers starting from start
; range: Generate list from start to end (exclusive)
(range (fn (start end)
           (iota (- end start) start)))

; range-step: Generate list from start to end with step
(range-step (fix range-step
                 (fn (start end step)
                     (if (>= start end)
                         '()
                         (cons start (range-step (+ start step) end step))))))

; -- Zipping utilities --

; zip3: Zip three lists together
(zip3 (fix zip3
           (fn (xs ys zs)
               (if (null? xs)
                   '()
                   (if (null? ys)
                       '()
                       (if (null? zs)
                           '()
                           (cons (list (car xs) (car ys) (car zs))
                                 (zip3 (cdr xs) (cdr ys) (cdr zs)))))))))

; zip-with-index: Pair each element with its index
(zip-with-index (fn (lst)
                    (zip (iota (length lst) 0) lst)))

; -- Misc utilities --

; constantly: Return a function that always returns the same value
; identity: Identity function (same as id, but clearer name)
; first: Get first element (same as car)
; second: Get second element
; third: Get third element
; rest: Get rest of list (same as cdr)
; empty?: Check if list is empty (same as null?)
; singleton?: Check if list has exactly one element
(singleton? (fn (lst)
                (if (null? lst)
                    #f
                    (null? (cdr lst)))))

; has-pair?: Check if list has at least two elements
(has-pair? (fn (lst)
               (if (null? lst)
                   #f
                   (if (null? (cdr lst))
                       #f
                       #t))))

; -- car/cdr compositions (Scheme standard) --

; cadr: (car (cdr x))
; caar: (car (car x))
; cddr: (cdr (cdr x))
; cdar: (cdr (car x))
; caddr: (car (cdr (cdr x)))
; caaar: (car (car (car x)))
; caadr: (car (car (cdr x)))
; cadar: (car (cdr (car x)))
; cdaar: (cdr (car (car x)))
; cdadr: (cdr (car (cdr x)))
; cddar: (cdr (cdr (car x)))
; cdddr: (cdr (cdr (cdr x)))
; cadddr: (car (cdr (cdr (cdr x)))) - fourth element
; -- More list utilities --

; fourth: Get fourth element
; fifth: Get fifth element
; Note: list-ref is a primitive that takes (list idx) - different order from nth

; list-tail: Return list starting at index n
(list-tail drop-n)

; list-head: Return first n elements
(list-head take-n)

; memq: Check if x is in list (using eq?)
(memq (fix memq
           (fn (x lst)
               (if (null? lst)
                   #f
                   (if (eq? x (car lst))
                       lst
                       (memq x (cdr lst)))))))

; assq: Like assoc but uses eq?
(assq assoc)

; append-map: Map then flatten (flatMap)
(append-map flat-map)

; -- Boolean utilities --

; -- More numeric utilities --

; square: x^2
(square (fn (x) (* x x)))

; cube: x^3
(cube (fn (x) (* x x x)))

; abs-diff: Absolute difference
; in-range?: Check if value is in [lo, hi)
(in-range? (fn (lo hi x)
               (if (>= x lo)
                   (< x hi)
                   #f)))

; signum: Sign function (-1, 0, or 1)
(signum (fn (x)
            (if (< x 0) -1 (if (> x 0) 1 0))))

; -- More higher-order utilities --

; unless: Like if but for false condition
(unless (fn (test then)
            (if test '() then)))

; when: Execute if true (returns result or nil)
(when (fn (test then)
          (if test then '())))

; for-each: Map for side effects (returns nil)
; reduce: foldl with first element as initial
; reduce-right: foldr with last element as initial
; every?: Alias for all (Scheme naming)
(every? all)

; some?: Alias for any (Scheme naming)
(some? any)

; filter-not: Keep elements that don't match
(filter-not (fn (pred lst)
                (filter (fn (x) (not (pred x))) lst)))

; remove: Remove first occurrence
; remove-all: Remove all occurrences
(remove-all (fn (x lst)
                (filter (fn (y) (not (= y x))) lst)))

; -- List building utilities --

; cons*: Multi-argument cons
(cons* (fix cons*
            (fn (first rest)
                (if (null? rest)
                    first
                    (if (null? (cdr rest))
                        (cons first (car rest))
                        (cons first (cons* (car rest) (cdr rest))))))))

; list*: Like list but last element is tail
(list* cons*)

; build-list: Build list by calling f on each index
(build-list (fn (n f)
                (map f (iota n 0))))

; make-list-fn: Create list of n elements using function
(make-list-fn (fn (n f)
                  (map (fn (i) (f)) (iota n 0))))

; tabulate: Build list using index function
(tabulate (fn (n f)
              (map f (iota n 0))))

; -- Comparison utilities --

; max-of: Find maximum using comparison function
(max-of (fn (compare lst)
            (if (null? lst)
                '()
                (foldl (fn (acc x) (if (compare x acc) x acc))
                       (car lst)
                       (cdr lst)))))

; min-of: Find minimum using comparison function
(min-of (fn (compare lst)
            (if (null? lst)
                '()
                (foldl (fn (acc x) (if (compare x acc) acc x))
                       (car lst)
                       (cdr lst)))))

; argmax: Find index of maximum
(argmax (fn (f lst)
            (if (null? lst)
                -1
                (car (foldl (fn (acc xi)
                                (let ((idx (car xi))
                                      (x (cadr xi))
                                      (best-idx (car acc))
                                      (best-val (cdr acc)))
                                     (let ((val (f x)))
                                          (if (> val best-val)
                                              (cons idx val)
                                              acc))))
                            (cons 0 (f (car lst)))
                            (zip-with-index (cdr lst)))))))

; -- Utility aliases for common patterns --

; succ: Successor (add 1)
(succ (fn (x) (+ x 1)))

; pred-fn: Predecessor (subtract 1) - named to avoid conflict with pred primitive
(pred-fn (fn (x) (- x 1)))

; double: Multiply by 2
(double (fn (x) (* x 2)))

; halve: Divide by 2
(halve (fn (x) (/ x 2)))

; -- Scheme compatibility aliases --

; fold-left: Alias for foldl (Scheme naming)
; fold-right: Alias for foldr (Scheme naming)
; member: Check membership (like elem but returns tail or #f)
(member (fix member
             (fn (x lst)
                 (if (null? lst)
                     #f
                     (if (eq? x (car lst))
                         lst
                         (member x (cdr lst)))))))

; member?: Boolean membership check
; position: Find index of element (or #f)
(position (fix position
               (fn (x lst)
                   (let ((helper (fix helper
                                      (fn (lst idx)
                                          (if (null? lst)
                                              #f
                                              (if (eq? x (car lst))
                                                  idx
                                                  (helper (cdr lst) (+ idx 1))))))))
                        (helper lst 0)))))

; -- Either monad utilities --

; left: Create left value (error case)
; right: Create right value (success case)
; left?: Check if left
; right?: Check if right
; from-left: Extract left value
; from-right: Extract right value
; either: Apply one of two functions based on Either
; -- List utilities from Scheme SRFI --

; list-index: Find index where predicate holds
(list-index (fix list-index
                 (fn (f lst)
                     (let ((helper (fix helper
                                        (fn (lst idx)
                                            (if (null? lst)
                                                #f
                                                (if (f (car lst))
                                                    idx
                                                    (helper (cdr lst) (+ idx 1))))))))
                          (helper lst 0)))))

; partition-all: Split list into chunks of n
(partition-all (fn (n lst)
                   (if (null? lst)
                       '()
                       (cons (take-n n lst)
                             (partition-all n (drop-n n lst))))))

; take-right: Take n elements from end
; drop-right: Drop n elements from end
; split-at-pred: Split list at first element matching predicate
(split-at-pred (fix split-at-pred
                    (fn (f lst)
                        (let ((helper (fix helper
                                           (fn (lst acc)
                                               (if (null? lst)
                                                   (list (reverse acc) '())
                                                   (if (f (car lst))
                                                       (list (reverse acc) lst)
                                                       (helper (cdr lst) (cons (car lst) acc))))))))
                             (helper lst '())))))

; -- Numeric utilities --

; sum: Sum of list (alias for sum-list)
(sum sum-list)

; product: Product of list (alias for product-list)
(product product-list)

; REMOVED: Duplicates now in stats.ss
; mean, variance, median (lines 1663-1685)

; clamp-val: Clamp value to range [lo, hi]
; lerp: Linear interpolation
(lerp (fn (a b t)
          (+ a (* t (- b a)))))

; -- Predicate utilities --

; and-fn: Combine predicates with and
(and-fn (fn (f g)
            (fn (x) (if (f x) (g x) #f))))

; or-fn: Combine predicates with or
(or-fn (fn (f g)
           (fn (x) (if (f x) #t (g x)))))

; not-fn: Negate a predicate (alias for complement)
(not-fn complement)

; -- More list operations --

; interpose: Insert separator between elements
(interpose (fn (sep lst)
               (if (null? lst)
                   '()
                   (if (null? (cdr lst))
                       lst
                       (cons (car lst)
                             (cons sep
                                   (interpose sep (cdr lst))))))))

; separate: Separate list by predicate into two lists
(separate partition)

; keep: Alias for filter
(keep filter)

; reject: Alias for filter-not
(reject filter-not)

; distinct: Remove duplicates (alias for nub)
(distinct nub)

; group-runs: Group consecutive equal elements
; -- Applicative utilities --

; lift2: Lift binary function to Maybe
(lift2 (fn (f ma mb)
           (if (nothing? ma)
               ma
               (if (nothing? mb)
                   mb
                   (just (f (from-just ma) (from-just mb)))))))

; sequence-list: Convert list of Maybes to Maybe of list
(sequence-list (fn (lst)
                   (foldr (fn (ma acc)
                              (if (nothing? ma)
                                  ma
                                  (if (nothing? acc)
                                      acc
                                      (just (cons (from-just ma) (from-just acc))))))
                          (just '())
                          lst)))

; -- More list operations --

; zip-with-3: Zip three lists with a ternary function
(zip-with-3 (fix zip-with-3
                 (fn (f xs ys zs)
                     (if (null? xs)
                         '()
                         (if (null? ys)
                             '()
                             (if (null? zs)
                                 '()
                                 (cons (f (car xs) (car ys) (car zs))
                                       (zip-with-3 f (cdr xs) (cdr ys) (cdr zs)))))))))

; unzip3: Unzip a list of 3-tuples into three lists
(unzip3 (fn (lst)
            (list (map car lst)
                  (map cadr lst)
                  (map caddr lst))))

; snoc: Append element to end of list
; single?: Check if list has exactly one element
(single? singleton?)

; drop-last: Drop n elements from end (alias)
; take-last: Take n elements from end (alias)
; find-index-of: Find index of element (different from list-index which uses predicate)
(find-index-of (fn (x lst) (position x lst)))

; last-index-of: Find last index of element
(last-index-of (fix last-index-of
                    (fn (x lst)
                        (let ((helper (fix helper
                                           (fn (lst idx last-found)
                                               (if (null? lst)
                                                   last-found
                                                   (if (eq? x (car lst))
                                                       (helper (cdr lst) (+ idx 1) idx)
                                                       (helper (cdr lst) (+ idx 1) last-found)))))))
                             (helper lst 0 #f)))))

; count-occurrences: Count how many times element appears
; replace-first: Replace first occurrence of old with new
(replace-first (fix replace-first
                    (fn (old new lst)
                        (if (null? lst)
                            '()
                            (if (eq? old (car lst))
                                (cons new (cdr lst))
                                (cons (car lst) (replace-first old new (cdr lst))))))))

; replace-all: Replace all occurrences of old with new
(replace-all (fn (old new lst)
                 (map (fn (x) (if (eq? x old) new x)) lst)))

; insert-sorted: Insert element into sorted list
(insert-sorted (fix insert-sorted
                    (fn (x lst)
                        (if (null? lst)
                            (list x)
                            (if (<= x (car lst))
                                (cons x lst)
                                (cons (car lst) (insert-sorted x (cdr lst))))))))

; insert-sorted-by: Insert element into sorted list using comparison function
; merge-sorted: Merge two sorted lists
(merge-sorted (fix merge-sorted
                   (fn (xs ys)
                       (if (null? xs)
                           ys
                           (if (null? ys)
                               xs
                               (if (<= (car xs) (car ys))
                                   (cons (car xs) (merge-sorted (cdr xs) ys))
                                   (cons (car ys) (merge-sorted xs (cdr ys)))))))))

; -- More function combinators --

; apply-to: Apply value to function (flip of function application)
(apply-to (fn (x f) (f x)))

; thrush: Thread value through list of functions (left to right)
(thrush (fn (x fns)
            (foldl (fn (acc f) (f acc)) x fns)))

; fanout: Apply multiple functions to same value, return list of results
(fanout (fn (fns x)
            (map (fn (f) (f x)) fns)))

; converge-with: Apply two functions to value, combine results
(converge-with (fn (combiner f g)
                   (fn (x) (combiner (f x) (g x)))))

; dup: Duplicate a value as a pair
(dup (fn (x) (cons x x)))

; swap: Swap elements of a pair
(swap (fn (p) (cons (cdr p) (car p))))

; -- More numeric utilities --

; quotient-remainder: Return both quotient and remainder
; wrap: Wrap value to range [lo, hi) (modular arithmetic)
(wrap (fn (lo hi x)
          (let ((range (- hi lo)))
               (let ((offset (mod (- x lo) range)))
                    (if (< offset 0)
                        (+ lo (+ offset range))
                        (+ lo offset))))))

; distance: Absolute difference (alias)
(distance abs-diff)

; average-of: Average of applying function to list
(average-of (fn (f lst)
                (mean (map f lst))))

; geometric-mean: Geometric mean of list
(geometric-mean (fn (lst)
                    (if (null? lst)
                        0
                        (expt (product lst) (/ 1 (length lst))))))

; harmonic-mean: Harmonic mean of list
(harmonic-mean (fn (lst)
                   (if (null? lst)
                       0
                       (/ (length lst)
                          (sum-list (map (fn (x) (/ 1 x)) lst))))))

; -- More predicate utilities --

; all-of: Check if value satisfies all predicates
(all-of (fn (preds x)
            (all (fn (p) (p x)) preds)))

; any-of: Check if value satisfies any predicate
(any-of (fn (preds x)
            (any (fn (p) (p x)) preds)))

; none-of: Check if value satisfies no predicate
(none-of (fn (preds x)
             (not (any (fn (p) (p x)) preds))))

; is: Create equality predicate
(is (fn (target) (fn (x) (eq? x target))))

; is-not: Create inequality predicate
(is-not (fn (target) (fn (x) (not (eq? x target)))))

; -- Either monad extensions --

; map-left: Map function over Left value
(map-left (fn (f e)
              (if (left? e)
                  (left (f (from-left e)))
                  e)))

; map-right: Map function over Right value
(map-right (fn (f e)
               (if (right? e)
                   (right (f (from-right e)))
                   e)))

; bimap-either: Map two functions over Either
(bimap-either (fn (fl fr e)
                  (if (left? e)
                      (left (fl (from-left e)))
                      (right (fr (from-right e))))))

; from-either: Extract value with default for Left
; partition-eithers: Separate list of Eithers into lefts and rights
; try-fn: Wrap function call in Either (catches #f as Left)
(try-fn (fn (f x)
            (let ((result (f x)))
                 (if result
                     (right result)
                     (left "failed")))))

; -- More Maybe utilities --

; map-maybe-fn: Map function over Maybe (tagged version)
(map-maybe-fn (fn (f m)
                  (if (nothing? m)
                      m
                      (just (f (from-just m))))))

; bind-maybe: Monadic bind for Maybe
(bind-maybe (fn (m f)
                (if (nothing? m)
                    m
                    (f (from-just m)))))

; maybe-to-list: Convert Maybe to list (empty or singleton)
; list-to-maybe: Convert list to Maybe (Nothing if empty)
; catMaybes: Filter and extract Just values from list of Maybes
(cat-maybes-fn (fn (lst)
                   (map from-just (filter (fn (m) (not (nothing? m))) lst))))

; -- More string HOFs (using primitives) --

; string-words-fn: Split string on whitespace (alias using primitive)
; string-lines-fn: Split string on newlines (alias using primitive)
; string-join-fn: Join strings with separator (alias using primitive)
; string-trim-fn: Trim whitespace from string (alias using primitive)
; -- Miscellaneous utilities --

; repeat-value: Create list of n copies of value
(repeat-value (fn (n x)
                  (if (<= n 0)
                      '()
                      (replicate n x))))

; cycle: Repeat list elements infinitely... just kidding, repeat n times
(cycle-n (fn (n lst)
             (concat (replicate n lst))))

; chunk-by: Chunk list by predicate (start new chunk when pred changes)
(chunk-by (fix chunk-by
               (fn (f lst)
                   (if (null? lst)
                       '()
                       (let ((first-val (f (car lst))))
                            (let ((same (take-while (fn (x) (eq? (f x) first-val)) lst))
                                  (rest (drop-while (fn (x) (eq? (f x) first-val)) lst)))
                                 (cons same (chunk-by f rest))))))))

; before: Get elements before first match
; after: Get elements after first match (excluding match)
; window: Get sliding windows of size n
(window sliding)

; adjacent-pairs: Get all adjacent pairs
; with-index: Pair each element with its index
(with-index (fn (lst)
                (zip-with-index lst)))

; enumerate: Alias for with-index
(enumerate with-index)

; -- Control flow utilities --

; when-not: Execute body when condition is false
; if-not: Inverted if
; cond-list: Build list conditionally (include items where pred is true)
(cond-list (fn (pairs)
               (filter-not null?
                           (map (fn (pair)
                                    (if (car pair) (cadr pair) '()))
                                pairs))))

; select: Select nth value from list based on index
(select (fn (idx vals)
            (list-ref vals idx)))

; case-pred: Like case but uses predicates
(case-pred (fn (val clauses)
               (let ((matching (find-if (fn (clause) ((car clause) val)) clauses)))
                    (if matching
                        ((cadr matching) val)
                        #f))))

; -- Property list utilities --

; plist-get: Get value from property list
; plist-set: Set value in property list (returns new plist)
; plist-remove: Remove key from property list
; plist-keys: Get all keys from property list
; plist-values: Get all values from property list
; plist->alist: Convert property list to association list
; alist->plist: Convert association list to property list
; -- More comparison utilities --

; min-max: Return both min and max of list
(min-max (fn (lst)
             (if (null? lst)
                 '()
                 (foldl (fn (acc x)
                            (list (min (car acc) x) (max (cadr acc) x)))
                        (list (car lst) (car lst))
                        (cdr lst)))))

; between-inclusive?: Check if value is between lo and hi inclusive
(between-inclusive? (fn (lo hi x)
                        (and (>= x lo) (<= x hi))))

; compare: Three-way comparison (-1, 0, 1)
(compare (fn (a b)
             (if (< a b)
                 -1
                 (if (> a b)
                     1
                     0))))

; compare-by: Three-way comparison with key function
(compare-by-key (fn (f a b)
                    (compare (f a) (f b))))

; lexicographic-compare: Compare lists lexicographically
(lexicographic-compare (fix lexicographic-compare
                            (fn (xs ys)
                                (if (null? xs)
                                    (if (null? ys) 0 -1)
                                    (if (null? ys)
                                        1
                                        (let ((cmp (compare (car xs) (car ys))))
                                             (if (= cmp 0)
                                                 (lexicographic-compare (cdr xs) (cdr ys))
                                                 cmp)))))))

; -- Vector HOFs --

; vec-map: Map function over vector
(vec-map (fn (f v)
             (list->vec (map f (vec->list v)))))

; vec-filter: Filter vector by predicate
(vec-filter (fn (f v)
                (list->vec (filter f (vec->list v)))))

; vec-foldl: Left fold over vector
(vec-foldl (fn (f init v)
               (foldl f init (vec->list v))))

; vec-foldr: Right fold over vector
(vec-foldr (fn (f init v)
               (foldr f init (vec->list v))))

; vec-any: Check if any element satisfies predicate
(vec-any (fn (f v)
             (any f (vec->list v))))

; vec-all: Check if all elements satisfy predicate
(vec-all (fn (f v)
             (all f (vec->list v))))

; vec-find: Find first element matching predicate
(vec-find (fn (f v)
              (find-if f (vec->list v))))

; vec-sum: Sum of vector elements
(vec-sum (fn (v)
             (sum-list (vec->list v))))

; vec-product: Product of vector elements
(vec-product (fn (v)
                 (product-list (vec->list v))))

; -- Iteration utilities --

; iterate-until: Iterate function until predicate is true
(iterate-until (fix iterate-until
                    (fn (f stop? x)
                        (if (stop? x)
                            x
                            (iterate-until f stop? (f x))))))

; iterate-while: Iterate function while predicate is true
(iterate-while (fix iterate-while
                    (fn (f continue? x)
                        (if (continue? x)
                            (iterate-while f continue? (f x))
                            x))))

; -- Accumulator patterns --

; -- Conditional list builders --

; append-if: Append element if condition is true
(append-if (fn (cond elem lst)
               (if cond (append lst (list elem)) lst)))

; prepend-if: Prepend element if condition is true
(prepend-if (fn (cond elem lst)
                (if cond (cons elem lst) lst)))

; maybe-cons: Cons if value is not #f
(maybe-cons (fn (x lst)
                (if x (cons x lst) lst)))

; -- Numeric sequences --

; arithmetic-sequence: Generate arithmetic sequence
(arithmetic-sequence (fn (start step n)
                         (generate n (fn (x) (+ x step)) start)))

; geometric-sequence: Generate geometric sequence
(geometric-sequence (fn (start ratio n)
                        (generate n (fn (x) (* x ratio)) start)))

; fibonacci-sequence: Generate fibonacci sequence
(fibonacci-sequence (fn (n)
                        (let ((fib-helper (fix fib-helper
                                               (fn (n a b acc)
                                                   (if (<= n 0)
                                                       (reverse acc)
                                                       (fib-helper (- n 1) b (+ a b) (cons a acc)))))))
                             (fib-helper n 0 1 '()))))

; -- List searching --

; find-all: Find all elements matching predicate
(find-all filter)

; find-first-n: Find first n elements matching predicate
(find-first-n (fn (n f lst)
                  (take-n n (filter f lst))))

; find-last: Find last element matching predicate
(find-last (fn (f lst)
               (last-where f lst)))

; index-of-all: Find all indices where predicate holds
(index-of-all indices-where)

; -- String builders --

; string-repeat-fn: Repeat string n times
; -- Safe operations --

; safe-car: Car with default for empty list
; safe-cdr: Cdr with default for empty list
; safe-head: Head with default
; safe-tail: Tail with default
; safe-nth: Nth with default for out of bounds
; safe-div: Division with default for divide by zero
; safe-mod: Modulo with default for divide by zero
(safe-mod (fn (default a b)
              (if (= b 0) default (mod a b))))

; -- Composition utilities --

; compose3: Compose three functions
(compose3 (fn (f g h)
              (fn (x) (f (g (h x))))))

; compose4: Compose four functions
(compose4 (fn (f g h i)
              (fn (x) (f (g (h (i x)))))))

; pipe3: Pipe through three functions
(pipe3 (fn (f g h)
           (fn (x) (h (g (f x))))))

; pipe4: Pipe through four functions
(pipe4 (fn (f g h i)
           (fn (x) (i (h (g (f x)))))))

; -- Predicate combinators --

; conjoin: Combine predicates with and
; disjoin: Combine predicates with or
; -- Pair utilities --

; pair-map: Map over both elements of pair
(pair-map (fn (f pair)
              (cons (f (car pair)) (f (cdr pair)))))

; pair-map-car: Map over car only
(pair-map-car (fn (f pair)
                  (cons (f (car pair)) (cdr pair))))

; pair-map-cdr: Map over cdr only
(pair-map-cdr (fn (f pair)
                  (cons (car pair) (f (cdr pair)))))

; pair-swap: Alias for swap
(pair-swap swap)

; -- Result type utilities (using Either) --

; ok: Create success result (alias for right)
; err: Create error result (alias for left)
; ok?: Check if result is ok
; err?: Check if result is error
; ok-value: Extract value from ok result
; err-value: Extract value from err result
; unwrap: Extract value or error
(unwrap (fn (result)
            (if (ok? result)
                (from-right result)
                (from-left result))))

; unwrap-or: Extract value or use default
(unwrap-or from-either)

; map-ok: Map over ok value
(map-ok map-right)

; map-err: Map over error value
(map-err map-left)

; -- Sorting utilities --

; insertion-sort: Sort list using insertion sort (stable)
; insertion-sort-by: Sort by key function
(insertion-sort-by (fn (key-fn lst)
                       (foldl (fn (sorted x) (insert-sorted-by key-fn x sorted)) '() lst)))

; merge-sort: Sort list using merge sort (stable, O(n log n))
; sort-descending: Sort in descending order
; sort-by-descending: Sort by key in descending order
; top-k: Get k largest elements
(top-k (fn (k lst)
           (take-n k (sort-descending lst))))

; bottom-k: Get k smallest elements
(bottom-k (fn (k lst)
              (take-n k (sort lst))))

; rank: Get rank of each element (1-indexed position in sorted order)
(rank (fn (lst)
          (let ((sorted (sort lst)))
               (map (fn (x) (+ 1 (position x sorted))) lst))))

; -- Binary search utilities --

; binary-search: Search sorted list for element, return index or #f
; lower-bound: Find first position where element could be inserted
; upper-bound: Find last position where element could be inserted
; -- More list utilities --

; rotate: Rotate list by n positions
; shuffle: Deterministic shuffle based on seed (not truly random)
(shuffle (fn (seed lst)
             (let ((pairs (zip (map (fn (x) (mod (* x seed) 1000003)) (iota (length lst) 1)) lst)))
                  (map cadr (sort-by car pairs)))))

; dedup-consecutive: Remove consecutive duplicates (like Unix uniq)
; run-length-encode: Encode consecutive runs
; run-length-decode: Decode run-length encoding
; -- Statistics utilities --

; REMOVED: Duplicates now in stats.ss
; percentile, quartiles, interquartile-range, z-score, normalize-list,
; standardize, covariance, correlation (lines 2607-2662)

; -- Matrix operations (lists of lists) --

; matrix-ref: Get element at row, col
; matrix-rows: Get number of rows
; matrix-cols: Get number of columns
; matrix-row: Get row at index
; matrix-col: Get column at index
; dot-product: Dot product of two vectors (lists)
(dot-product (fn (a b)
                 (sum-list (zip-with * a b))))

; zero-matrix: Create m×n zero matrix
(zero-matrix (fn (m n)
                 (replicate m (replicate n 0))))

; -- Functional patterns --

; Y combinator: Note - use the 'fix' special form directly for recursion
; Example: (fix factorial (fn (n) (if (= n 0) 1 (* n (factorial (- n 1))))))
; Y is provided as id for compatibility, but fix is the preferred approach
; memoize: Placeholder for memoization (requires mutation, not fully supported)
; Use memoize-1 instead for basic single-value memoization
(memoize id)

; trampoline: Execute trampolined function
; -- Tree utilities --

; tree-leaves: Get all leaves of tree
; tree-count: Count nodes in tree
; tree-height: Alias for tree-depth
; tree-paths: Get all paths from root to leaves
(tree-paths (fix tree-paths
                 (fn (tree)
                     (if (not (pair? tree))
                         (list (list tree))
                         (concat (map (fn (subtree)
                                          (map (fn (path) (cons (car tree) path))
                                               (tree-paths subtree)))
                                      (cdr tree)))))))

; -- Format utilities --

; number->string-padded: Convert number to string with padding
; format-list: Format list as string with separator
(format-list (fn (sep lst)
                 (string-join (map (fn (x) (if (string? x) x (number->string x))) lst) sep)))

; format-table: Format list of lists as table
(format-table (fn (rows)
                  (string-join (map (fn (row) (format-list " " row)) rows) "\n")))

; pluralize: Simple pluralization
; -- Boolean utilities --

; bool->int: Convert boolean to integer
(bool->int (fn (b) (if b 1 0)))

; int->bool: Convert integer to boolean
(int->bool (fn (n) (not (= n 0))))

; -- List predicates --

; sublist?: Check if xs is a sublist of ys
; prefix?: Check if xs is a prefix of ys
; suffix?: Check if xs is a suffix of ys
; -- Misc utilities --

; clamp-index: Clamp index to valid range for list
(clamp-index (fn (idx lst)
                 (clamp-val 0 (- (length lst) 1) idx)))

; circular-ref: Get element at index with wrap-around
(circular-ref (fn (lst idx)
                  (list-ref lst (mod idx (length lst)))))

; repeat-fn-n: Apply function n times, collecting results
(repeat-fn-n (fn (n f init)
                 (let ((helper (fix helper
                                    (fn (n acc val)
                                        (if (<= n 0)
                                            (reverse acc)
                                            (let ((new-val (f val)))
                                                 (helper (- n 1) (cons new-val acc) new-val)))))))
                      (helper n (list init) init))))

; fixed-point-iterate: Iterate until fixed point or max iterations
(fixed-point-iterate (fn (f max-iter x)
                         (let ((helper (fix helper
                                            (fn (iter val)
                                                (if (>= iter max-iter)
                                                    val
                                                    (let ((new-val (f val)))
                                                         (if (eq? val new-val)
                                                             val
                                                             (helper (+ iter 1) new-val))))))))
                              (helper 0 x))))

; ============================================
; Hash Table / Dictionary Utilities (alist-based)
; ============================================

; dict-new: Create empty dictionary
; dict-set: Set key-value pair (returns new dict)
; dict-get: Get value for key, or default
; dict-get-in: Get nested value using path of keys
; dict-remove: Remove key from dictionary
; dict-has?: Check if key exists
; dict-keys: Get all keys
; dict-values: Get all values
; dict-size: Get number of entries
; dict-empty?: Check if dictionary is empty
; dict-update: Update value with function
; dict-merge: Merge two dictionaries (second wins on conflict)
; dict-filter: Filter dictionary by predicate on (key . value)
; dict-map-values: Map function over values
; dict-from-lists: Create dict from key list and value list
; dict-to-list: Convert dict to list of (key value) pairs
; dict-invert: Swap keys and values
; ============================================
; Set Operations (list-based)
; ============================================

; set-new: Create empty set (use as thunk: (set-new) returns empty list)
; Or use '() directly for an empty set
; set-add: Add element to set
; set-remove: Remove element from set
; set-member?: Check if element is in set
; set-size: Get number of elements
; set-empty?: Check if set is empty
; set-from-list: Create set from list (removes duplicates)
; set-to-list: Convert set to list
; ============================================
; More Numeric Utilities
; ============================================

; mod-exp: Modular exponentiation (base^exp mod m)
; mod-inverse: Modular multiplicative inverse (extended Euclidean)
; ============================================
; Combinatorics
; ============================================

; permutations-count: Number of permutations P(n,k)
; catalan: Catalan number
; triangular: Triangular number
; is-triangular?: Check if number is triangular
; square-number: Square number
; is-square?: Check if number is a perfect square
; pentagonal: Pentagonal number
; hexagonal: Hexagonal number
; ============================================
; More List Utilities
; ============================================

; list-product: Cartesian product of list of lists
; interleave-all: Interleave multiple lists
(interleave-all (fix interleave-all
                     (fn (lists)
                         (if (all null? lists)
                             '()
                             (append (filter-not null? (map (fn (lst) (if (null? lst) '() (car lst))) lists))
                                     (interleave-all (map (fn (lst) (if (null? lst) '() (cdr lst))) lists)))))))

; partition-n: Partition list into n equal parts
(partition-n (fn (n lst)
                 (let ((size (/ (length lst) n)))
                      (map (fn (i) (take-n size (drop-n (* i size) lst))) (iota n 0)))))

; group-into: Group list into groups of size n
(group-into chunks)

; frequencies-by: Count occurrences by key function
(frequencies-by (fn (key-fn lst)
                    (foldl (fn (acc x)
                               (let ((k (key-fn x)))
                                    (dict-update acc k (fn (v) (+ v 1)) 0)))
                           '()
                           lst)))

; mode: Most frequent element
; frequencies returns ((elem count) ...), so use cadr to get count
; REMOVED: mode (duplicate, now in stats.ss)

; majority: Element appearing more than n/2 times (or #f)
; REMOVED: majority (duplicate, now in stats.ss)

; ============================================
; String Utilities (additional)
; ============================================

; string-capitalize: Capitalize first letter
; string-title-case: Capitalize first letter of each word
; string-count-char: Count occurrences of character in string
; string-replace-char: Replace all occurrences of character
; string-squeeze: Remove consecutive duplicate characters
; string-rotate: Rotate string by n positions
; string-interleave: Interleave two strings
; ============================================
; Predicate Utilities
; ============================================

; all-same?: Check if all elements are equal
(all-same? all-equal?)

; all-different?: Check if all elements are different
(all-different? (fn (lst)
                    (= (length lst) (length (nub lst)))))

; monotonic-increasing?: Check if list is monotonically increasing
; monotonic-decreasing?: Check if list is monotonically decreasing
; strictly-increasing?: Check if list is strictly increasing
; strictly-decreasing?: Check if list is strictly decreasing
; ============================================
; Sequence Generators
; ============================================

; naturals: Generate natural numbers from 0 to n-1
; evens: Generate even numbers from 0 to 2*(n-1)
; odds: Generate odd numbers from 1 to 2*n-1
; squares: Generate square numbers
; cubes: Generate cube numbers
; factorials-up-to: Generate factorials up to n!
; ============================================
; Reduction Utilities
; ============================================

; reduce-pairs: Reduce adjacent pairs with function
(reduce-pairs (fn (f lst)
                  (if (null? lst) '()
                      (if (null? (cdr lst)) lst
                          (cons (f (car lst) (cadr lst))
                                (reduce-pairs f (cddr lst)))))))

; pairwise-apply: Apply binary function to all pairs
(pairwise-apply (fn (f lst)
                    (concat (map (fn (x)
                                     (map (fn (y) (f x y))
                                          (filter (fn (y) (not (eq? x y))) lst)))
                                 lst))))

; fold-tree: Fold over tree structure
; ============================================
; Debugging Utilities
; ============================================

; debug-print: Print value and return it
(debug-print (fn (label x)
                 (begin
                  (display label)
                  (display ": ")
                  (display x)
                  (newline)
                  x)))

; time-thunk: Measure time to evaluate (placeholder - needs runtime support)
(time-thunk (fn (thunk)
                (let ((result (thunk)))
                     result)))

; count-calls: Wrap function to count calls (placeholder)
(count-calls (fn (f) f))

; type-of-value: Get type name as symbol
(type-of-value (fn (x)
                   (if (null? x) 'null
                       (if (boolean? x) 'boolean
                           (if (number? x) 'number
                               (if (string? x) 'string
                                   (if (symbol? x) 'symbol
                                       (if (char? x) 'char
                                           (if (pair? x) 'pair
                                               (if (vector? x) 'vector
                                                   (if (procedure? x) 'procedure
                                                       'unknown)))))))))))

; ============================================
; Queue Operations (FIFO, list-based)
; ============================================

; queue-new: Create empty queue
; queue-empty?: Check if queue is empty
; queue-enqueue: Add element to back of queue
; queue-dequeue: Remove and return front element (returns (element . rest))
; queue-front: Peek at front element
; queue-size: Get queue size
; queue-to-list: Convert queue to list
; ============================================
; Stack Operations (LIFO, list-based)
; ============================================

; stack-new: Create empty stack
; stack-empty?: Check if stack is empty
; stack-push: Push element onto stack (stack first, then element)
; stack-pop: Pop element from stack (returns (element . rest))
; stack-top: Peek at top element
; stack-size: Get stack size
; ============================================
; Deque Operations (double-ended queue)
; ============================================

; deque-new: Create empty deque
; deque-empty?: Check if deque is empty
; deque-push-front: Add to front (deque first, then element)
; deque-push-back: Add to back
; deque-pop-front: Remove from front
; deque-pop-back: Remove from back
; deque-front: Peek at front
; deque-back: Peek at back
; ============================================
; Priority Queue (using sorted list)
; ============================================

; pq-new: Create empty priority queue
; pq-empty?: Check if priority queue is empty
; pq-insert: Insert with priority (lower = higher priority)
; pq-peek: Get highest priority element
; pq-pop: Remove and return highest priority element
; ============================================
; Graph Algorithms (adjacency list)
; ============================================

; graph-new: Create empty graph (as adjacency list dict)
; graph-add-vertex: Add a vertex
; graph-vertices: Get all vertices
; graph-path-exists?: Check if path exists from u to v
; graph-connected?: Check if graph is connected (for undirected graphs)
; ============================================
; Validation Utilities
; ============================================

; validate-type: Validate value has expected type
; validate-range: Validate number is in range
; validate-not-empty: Validate list/string is not empty
; validate-all-pred: Validate all elements satisfy predicate
(validate-all-pred (fn (p lst msg)
                       (if (all p lst)
                           (right lst)
                           (left msg))))

; validate-length: Validate list has expected length
; validate-min-length: Validate minimum length
(validate-min-length (fn (n lst)
                         (if (>= (length lst) n)
                             (right lst)
                             (left (list 'too-short n (length lst))))))

; validate-max-length: Validate maximum length
(validate-max-length (fn (n lst)
                         (if (<= (length lst) n)
                             (right lst)
                             (left (list 'too-long n (length lst))))))

; chain-validations: Chain multiple validations
(chain-validations (fn (validators value)
                       (foldl (fn (result validator)
                                  (if (left? result)
                                      result
                                      (validator (from-right result))))
                              (right value)
                              validators)))

; ============================================
; Parsing Utilities
; ============================================

; parse-int: Parse string to integer (returns Either)
(parse-int (fn (s)
               (let ((n (string->number s)))
                    (if n (right n) (left 'not-a-number)))))

; parse-bool: Parse string to boolean
(parse-bool (fn (s)
                (if (or (string=? s "true") (or (string=? s "yes") (string=? s "1")))
                    (right #t)
                    (if (or (string=? s "false") (or (string=? s "no") (string=? s "0")))
                        (right #f)
                        (left 'not-a-boolean)))))

; split-parse: Split string and parse each part
(split-parse (fn (sep parser s)
                 (let ((parts (string-split s sep)))
                      (let ((results (map parser parts)))
                           (if (any left? results)
                               (left 'parse-error)
                               (right (map from-right results)))))))

; ============================================
; More Functional Patterns
; ============================================

; lens-get: Get value at path in nested structure
(lens-get (fn (path data)
              (foldl (fn (acc key)
                         (if (pair? acc)
                             (let ((entry (assoc key acc)))
                                  (if entry (cdr entry) #f))
                             #f))
                     data
                     path)))

; lens-set: Set value at path in nested structure
(lens-set (fix lens-set
               (fn (path value data)
                   (if (null? path)
                       value
                       (if (null? (cdr path))
                           (assoc-set (car path) value data)
                           (let ((key (car path))
                                 (rest (cdr path)))
                                (let ((nested (assoc-ref data key '())))
                                     (assoc-set key (lens-set rest value nested) data))))))))

; lens-update: Update value at path with function
(lens-update (fn (path f data)
                 (let ((current (lens-get path data)))
                      (lens-set path (f current) data))))

; arrow-first: Apply function to first element of pair
(arrow-first (fn (f pair)
                 (cons (f (car pair)) (cdr pair))))

; arrow-second: Apply function to second element of pair
(arrow-second (fn (f pair)
                  (cons (car pair) (f (cdr pair)))))

; arrow-both: Apply function to both elements
(arrow-both (fn (f pair)
                (cons (f (car pair)) (f (cdr pair)))))

; arrow-split: Apply two functions and pair results
(arrow-split (fn (f g x)
                 (cons (f x) (g x))))

; arrow-fanin: Merge two computations
(arrow-fanin (fn (f g pair)
                 (f (car pair) (g (cdr pair)))))

; kleisli-compose: Compose monadic functions (for Maybe/Either)
(kleisli-compose (fn (f g)
                     (fn (x)
                         (let ((result (f x)))
                              (if (or (nothing? result) (left? result))
                                  result
                                  (g (if (right? result) (from-right result) (from-just result))))))))

; ============================================
; More List Algorithms
; ============================================

; list-min: Minimum element (requires numeric list)
(list-min (fn (lst)
              (if (null? lst) #f
                  (foldl (fn (acc x) (if (< x acc) x acc)) (car lst) (cdr lst)))))

; list-max: Maximum element
(list-max (fn (lst)
              (if (null? lst) #f
                  (foldl (fn (acc x) (if (> x acc) x acc)) (car lst) (cdr lst)))))

; list-argmin: Index of minimum element
(list-argmin (fn (lst)
                 (if (null? lst) #f
                     (car (foldl (fn (acc pair)
                                     (if (< (cadr pair) (cadr acc))
                                         pair
                                         acc))
                                 (list 0 (car lst))
                                 (zip (iota (length lst) 0) lst))))))

; list-argmax: Index of maximum element
(list-argmax (fn (lst)
                 (if (null? lst) #f
                     (car (foldl (fn (acc pair)
                                     (if (> (cadr pair) (cadr acc))
                                         pair
                                         acc))
                                 (list 0 (car lst))
                                 (zip (iota (length lst) 0) lst))))))

; list-span-count: Count elements from start satisfying predicate
(list-span-count (fix list-span-count
                      (fn (p lst)
                          (if (null? lst) 0
                              (if (p (car lst))
                                  (+ 1 (list-span-count p (cdr lst)))
                                  0)))))

; list-break-count: Count elements from start NOT satisfying predicate
(list-break-count (fn (p lst)
                      (list-span-count (complement p) lst)))

; list-split-at-first: Split at first element satisfying predicate
(list-split-at-first (fix list-split-at-first
                          (fn (p lst)
                              (if (null? lst)
                                  (list '() '())
                                  (if (p (car lst))
                                      (list '() lst)
                                      (let ((rest (list-split-at-first p (cdr lst))))
                                           (list (cons (car lst) (car rest)) (cadr rest))))))))

; list-unique-by: Remove duplicates by key function
(list-unique-by (fn (key-fn lst)
                    (let ((helper (fix helper
                                       (fn (lst seen)
                                           (if (null? lst)
                                               '()
                                               (let ((k (key-fn (car lst))))
                                                    (if (member? k seen)
                                                        (helper (cdr lst) seen)
                                                        (cons (car lst) (helper (cdr lst) (cons k seen))))))))))
                         (helper lst '()))))

; list-group-runs: Group consecutive runs based on predicate
(list-group-runs (fix list-group-runs
                      (fn (same? lst)
                          (if (null? lst)
                              '()
                              (let ((split-result (span (fn (x) (same? (car lst) x)) lst)))
                                   (cons (car split-result)
                                         (list-group-runs same? (cadr split-result))))))))

; ============================================
; Interval Utilities
; ============================================

; interval-new: Create interval [lo, hi]
(interval-new (fn (lo hi) (list lo hi)))

; interval-lo: Get lower bound
(interval-lo car)

; interval-hi: Get upper bound
(interval-hi cadr)

; interval-contains?: Check if interval contains value
(interval-contains? (fn (interval x)
                        (and (>= x (interval-lo interval))
                             (<= x (interval-hi interval)))))

; interval-overlaps?: Check if two intervals overlap
(interval-overlaps? (fn (i1 i2)
                        (and (<= (interval-lo i1) (interval-hi i2))
                             (<= (interval-lo i2) (interval-hi i1)))))

; interval-union: Union of overlapping intervals
(interval-union (fn (i1 i2)
                    (if (interval-overlaps? i1 i2)
                        (interval-new (min (interval-lo i1) (interval-lo i2))
                                      (max (interval-hi i1) (interval-hi i2)))
                        #f)))

; interval-intersection: Intersection of intervals
(interval-intersection (fn (i1 i2)
                           (if (interval-overlaps? i1 i2)
                               (interval-new (max (interval-lo i1) (interval-lo i2))
                                             (min (interval-hi i1) (interval-hi i2)))
                               #f)))

; interval-width: Width of interval
(interval-width (fn (interval)
                    (- (interval-hi interval) (interval-lo interval))))

; interval-midpoint: Midpoint of interval
(interval-midpoint (fn (interval)
                       (/ (+ (interval-lo interval) (interval-hi interval)) 2)))

; ============================================
; Bisection / Binary Search Utilities
; ============================================

; bisect-find: Find value using bisection
(bisect-find (fix bisect-find
                  (fn (f target lo hi tolerance)
                      (if (< (- hi lo) tolerance)
                          (/ (+ lo hi) 2)
                          (let ((mid (/ (+ lo hi) 2)))
                               (let ((mid-val (f mid)))
                                    (if (< mid-val target)
                                        (bisect-find f target mid hi tolerance)
                                        (bisect-find f target lo mid tolerance))))))))

; bisect-root: Find root of function using bisection
(bisect-root (fn (f lo hi tolerance)
                 (bisect-find f 0 lo hi tolerance)))

; ============================================
; Accumulator Patterns
; ============================================

; scan-while: Scan while predicate holds
(scan-while (fix scan-while
                 (fn (p f init lst)
                     (if (null? lst)
                         (list init)
                         (let ((next (f init (car lst))))
                              (if (p next)
                                  (cons init (scan-while p f next (cdr lst)))
                                  (list init)))))))

; fold-while: Fold while predicate holds
(fold-while (fix fold-while
                 (fn (p f init lst)
                     (if (null? lst)
                         init
                         (let ((next (f init (car lst))))
                              (if (p next)
                                  (fold-while p f next (cdr lst))
                                  init))))))

; fold-until: Fold until predicate holds
(fold-until (fn (p f init lst)
                (fold-while (complement p) f init lst)))

; ============================================
; Utility Combinators
; ============================================

; when-pred: Apply function only if predicate holds
; unless-pred: Apply function only if predicate fails
; with-default: Provide default for #f values
(with-default (fn (default x)
                  (if x x default)))

; null-coalesce: Return first non-null value
(null-coalesce (fix null-coalesce
                    (fn (values)
                        (if (null? values)
                            #f
                            (if (car values)
                                (car values)
                                (null-coalesce (cdr values)))))))

; safe-apply: Apply function, catching errors (placeholder)
(safe-apply (fn (f x)
                (right (f x))))

; retry-n: Retry function n times (placeholder - needs error handling)
(retry-n (fn (n f x)
             (f x)))

; ============================================
; Pattern Matching Utilities
; ============================================

; match-pred: Match value against predicates with corresponding handlers
; Cases is an alist of (predicate . handler) pairs
(match-pred (fn (value cases)
                (let ((finder (fix finder
                                   (fn (cases)
                                       (if (null? cases)
                                           #f
                                           (let ((case (car cases)))
                                                (if ((car case) value)
                                                    (cdr case)
                                                    (finder (cdr cases)))))))))
                     (finder cases))))

; match-equal: Match value for equality with corresponding handlers
; Cases is an alist of (key . value) pairs
(match-equal (fn (value cases default)
                 (let ((finder (fix finder
                                    (fn (cases)
                                        (if (null? cases)
                                            default
                                            (let ((case (car cases)))
                                                 (if (eq? (car case) value)
                                                     (cdr case)
                                                     (finder (cdr cases)))))))))
                      (finder cases))))

; case-of: Case expression (list of (pattern handler) pairs)

; guard: Guard expressions - find first true condition and return its value
; (guard (list (cons #f 'no) (cons #t 'yes))) => 'yes
(guard (fix guard
            (fn (pairs)
                (if (null? pairs)
                    #f
                    (if (car (car pairs))
                        (cdr (car pairs))
                        (guard (cdr pairs)))))))

; destructure-list: Destructure list into head and tail
(destructure-list (fn (lst on-empty on-pair)
                      (if (null? lst)
                          on-empty
                          (on-pair (car lst) (cdr lst)))))

; destructure-pair: Destructure pair
(destructure-pair (fn (p f)
                      (f (car p) (cdr p))))

; let-pair: Bind car and cdr of pair to names
(let-pair (fn (p f)
              (f (car p) (cdr p))))

; ============================================
; Lazy Evaluation / Streams
; ============================================

; thunk: Create a thunk (delayed computation)
(thunk (fn (f) (list 'thunk f #f #f)))

; force: Force a thunk
(force (fn (t)
           (if (and (pair? t) (eq? (car t) 'thunk))
               (if (caddr t)
                   (cadddr t)
                   (let ((result ((cadr t))))
                        result))
               t)))

; stream-cons: Create a lazy stream node
; stream-head: Get head of stream
; stream-tail: Get tail of stream (forces thunk)
; stream-null: Empty stream marker
; stream-null?: Check if stream is empty
; stream-take: Take n elements from stream
; stream-map: Map function over stream
; stream-filter: Filter stream by predicate
; stream-from: Infinite stream starting at n
; stream-iterate: Infinite stream by iterating function
; stream-repeat: Infinite stream of same value
; stream-zip-with: Zip two streams with function
; ============================================
; More Monad Utilities
; ============================================

; bind-maybe: Monadic bind for Maybe
(bind-maybe (fn (m f)
                (if (nothing? m)
                    m
                    (f (from-just m)))))

; bind-either: Monadic bind for Either
(bind-either (fn (m f)
                 (if (left? m)
                     m
                     (f (from-right m)))))

; sequence-maybes: Sequence list of maybes into maybe of list
(sequence-maybes (fn (ms)
                     (foldl (fn (acc m)
                                (if (nothing? acc)
                                    acc
                                    (if (nothing? m)
                                        m
                                        (just (append (from-just acc) (list (from-just m)))))))
                            (just '())
                            ms)))

; sequence-eithers: Sequence list of eithers into either of list
(sequence-eithers (fn (es)
                      (foldl (fn (acc e)
                                 (if (left? acc)
                                     acc
                                     (if (left? e)
                                         e
                                         (right (append (from-right acc) (list (from-right e)))))))
                             (right '())
                             es)))

; traverse-maybe: Map and sequence for Maybe
(traverse-maybe (fn (f lst)
                    (sequence-maybes (map f lst))))

; traverse-either: Map and sequence for Either
; ap-maybe: Applicative apply for Maybe
(ap-maybe (fn (mf mx)
              (if (nothing? mf)
                  mf
                  (if (nothing? mx)
                      mx
                      (just ((from-just mf) (from-just mx)))))))

; ap-either: Applicative apply for Either
(ap-either (fn (ef ex)
               (if (left? ef)
                   ef
                   (if (left? ex)
                       ex
                       (right ((from-right ef) (from-right ex)))))))

; ============================================
; State Monad Utilities
; ============================================

; ============================================
; Reader Monad Utilities
; ============================================

; ============================================
; Writer Monad Utilities
; ============================================

; ============================================
; More Collection Utilities
; ============================================

; multiset-add: Add element to multiset (alist of counts)
; multiset-remove: Remove one occurrence from multiset
; multiset-count: Get count of element in multiset
; multiset-from-list: Create multiset from list
; multiset-to-list: Convert multiset to list (with repetitions)
; filter-map: Map and filter in one pass (returns list of unwrapped just values)
(filter-map (fn (f lst)
                (foldr (fn (x acc)
                           (let ((result (f x)))
                                (if (nothing? result)
                                    acc
                                    (cons (from-just result) acc))))
                       '()
                       lst)))

; bag-union: Union of two multisets
; bag-intersection: Intersection of two multisets
; ============================================
; More Functional Programming Patterns
; ============================================

; fix-with-memo: Fixed-point combinator (memoization requires mutable state)
; This is just an alias for fix in pure environment
(fix-with-memo (fn (f)
                   (fix rec
                        (fn (x)
                            ((f rec) x)))))

; memo-rec: Recursive function helper (memoization requires mutable state)
; This is equivalent to fix in pure environment
(memo-rec (fn (f)
              (fix rec
                   (fn (x)
                       ((f rec) x)))))

; trampoline-call: Call trampolined function until done
(trampoline-call (fix trampoline-call
                      (fn (f)
                          (let ((result (f)))
                               (if (pair? result)
                                   (if (eq? (car result) 'bounce)
                                       (trampoline-call (cadr result))
                                       result)
                                   result)))))

; bounce: Create a bounce for trampolining
; done: Mark value as done for trampolining
(done (fn (x) x))

; ============================================
; More Numeric Utilities
; ============================================

; complex-new: Create complex number as (real . imag)
; complex-real: Get real part
; complex-imag: Get imaginary part
; complex-add: Add two complex numbers
; complex-sub: Subtract complex numbers
; complex-mul: Multiply complex numbers
; complex-magnitude: Magnitude of complex number
; complex-conjugate: Complex conjugate
; NOTE: Simple pair-based rationals removed - use built-in rational type
; Built-in functions: make-rational, rational-numerator, rational-denominator,
; rational-add, rational-sub, rational-mul, rational-div, rational->float

; ============================================
; Format/Display Utilities
; ============================================

; format-number: Format number with precision
; pad-number: Pad number to width with zeros
(pad-number (fn (n width)
                (let ((s (number->string n)))
                     (if (>= (string-length s) width)
                         s
                         (string-append (make-string (- width (string-length s)) #\0) s)))))

; format-hex: Format number as hex string
(format-hex (fix format-hex
                 (fn (n)
                     (if (< n 16)
                         (string-ref "0123456789abcdef" n)
                         (string-append (format-hex (quotient n 16))
                                        (string (string-ref "0123456789abcdef" (remainder n 16))))))))

; format-binary: Format number as binary string
(format-binary (fix format-binary
                    (fn (n)
                        (if (< n 2)
                            (number->string n)
                            (string-append (format-binary (quotient n 2))
                                           (number->string (remainder n 2)))))))

; format-with-commas: Format number with thousand separators
(format-with-commas (fn (n)
                        (let ((s (number->string (abs n))))
                             (let ((helper (fix helper
                                                (fn (chars count result)
                                                    (if (null? chars)
                                                        result
                                                        (let ((new-result (cons (car chars) result)))
                                                             (helper (cdr chars)
                                                                     (+ count 1)
                                                                     (if (and (= (remainder (+ count 1) 3) 0) (not (null? (cdr chars))))
                                                                         (cons #\, new-result)
                                                                         new-result))))))))
                                  (let ((formatted (list->string (helper (reverse (string->list s)) 0 '()))))
                                       (if (< n 0)
                                           (string-append "-" formatted)
                                           formatted))))))

; format-ordinal: Format number as ordinal (1st, 2nd, 3rd, etc.)
(format-ordinal (fn (n)
                    (let ((suffix (if (and (>= (remainder n 100) 11) (<= (remainder n 100) 13))
                                      "th"
                                      (case-of (remainder n 10)
                                               (list (list 1 "st") (list 2 "nd") (list 3 "rd"))))))
                         (string-append (number->string n) (if suffix suffix "th")))))

; ============================================
; More String Utilities
; ============================================

; string-chars: Convert string to list of characters
; chars-string: Convert list of characters to string
(chars-string list->string)

; string-contains?: Check if haystack contains needle
; string-index-of: Find index of needle in haystack (-1 if not found)
; string-starts-with?: Check if string s starts with prefix
; string-ends-with?: Check if string s ends with suffix
; string-replace: Replace all occurrences of old with new in s
; string-split-at: Split string at index
; ============================================
; Error Handling Utilities
; ============================================

; try-catch: Try expression, catch errors (simplified - uses Either)
; Returns (right value) on success, (left error-msg) on failure
; assert: Check condition, return error if false
(assert (fn (condition msg)
            (if condition
                (ok #t)
                (err msg))))

; assert-eq: Assert two values are equal
; ensure-result: Ensure condition, return result type (ok value) or (err msg)
(ensure-result (fn (p value msg)
                   (if (p value)
                       (ok value)
                       (err msg))))

; coerce: Try to coerce value, return default on failure
(coerce (fn (f default value)
            (let ((result (f value)))
                 (if result result default))))

; ============================================
; More Numeric Utilities
; ============================================

; fibonacci: Compute nth Fibonacci number
(fibonacci (fix fibonacci
                (fn (n)
                    (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

; is-prime?: Check if number is prime
(is-prime? (fn (n)
               (if (<= n 1)
                   #f
                   (if (<= n 3)
                       #t
                       (if (or (= 0 (remainder n 2)) (= 0 (remainder n 3)))
                           #f
                           (let ((checker (fix checker
                                               (fn (i)
                                                   (if (> (* i i) n)
                                                       #t
                                                       (if (or (= 0 (remainder n i)) (= 0 (remainder n (+ i 2))))
                                                           #f
                                                           (checker (+ i 6))))))))
                                (checker 5)))))))

; proper-divisors: Get divisors of n excluding n itself
; is-perfect?: Check if number is perfect (sum of proper divisors = n)
(is-perfect? (fn (n)
                 (= n (sum-list (proper-divisors n)))))

; next-prime: Get next prime after n
(next-prime (fix next-prime
                 (fn (n)
                     (let ((candidate (+ n 1)))
                          (if (is-prime? candidate)
                              candidate
                              (next-prime candidate))))))

; prime-factors: Get prime factorization of n
(prime-factors (fn (n)
                   (let ((factorize (fix factorize
                                         (fn (n p factors)
                                             (if (< n 2)
                                                 factors
                                                 (if (= 0 (remainder n p))
                                                     (factorize (quotient n p) p (cons p factors))
                                                     (factorize n (next-prime p) factors)))))))
                        (reverse (factorize n 2 '())))))

; num-digits: Count digits in a number
(num-digits (fn (n)
                (if (= n 0)
                    1
                    (let ((counter (fix counter
                                        (fn (n count)
                                            (if (< n 1)
                                                count
                                                (counter (/ n 10) (+ count 1)))))))
                         (counter (abs n) 0)))))

; sum-digits: Sum of digits
(sum-digits (fn (n)
                (foldl + 0 (digits (abs n)))))

; ============================================
; Control Flow Utilities
; ============================================

; while-loop: Functional while loop
(while-loop (fn (p body init)
                (let ((loop (fix loop
                                 (fn (state)
                                     (if (p state)
                                         (loop (body state))
                                         state)))))
                     (loop init))))

; for-loop: Functional for loop
(for-loop (fn (start end body init)
              (let ((loop (fix loop
                               (fn (i acc)
                                   (if (>= i end)
                                       acc
                                       (loop (+ i 1) (body i acc)))))))
                   (loop start init))))

; times: Execute thunk n times, collect results
(times (fn (n thunk)
           (build-list n (fn (i) (thunk)))))

; repeat-until: Repeat until predicate is true
; do-times: Execute body n times for side effects
; thread-first: Thread value through functions (left-to-right)
(thread-first (fn (x fns)
                  (foldl (fn (acc f) (f acc)) x fns)))

; thread-last: Thread value as last arg through functions
(thread-last (fn (x fns)
                 (foldl (fn (acc f) (f acc)) x fns)))

; pipeline: Create a pipeline of functions (takes a list of functions)
(pipeline (fn (fns)
              (fn (x)
                  (foldl (fn (acc f) (f acc)) x fns))))

; ============================================
; Type Introspection Utilities
; ============================================

; type-name: Get type name as symbol
(type-name (fn (x)
               (if (null? x) 'null
                   (if (boolean? x) 'boolean
                       (if (number? x) 'number
                           (if (string? x) 'string
                               (if (symbol? x) 'symbol
                                   (if (pair? x) 'pair
                                       (if (procedure? x) 'procedure
                                           (if (vector? x) 'vector
                                               'unknown))))))))))

; is-type?: Check if value is of given type
(is-type? (fn (type-sym x)
              (eq? type-sym (type-name x))))

; type-check: Assert value is of expected type
(type-check (fn (expected x)
                (if (is-type? expected x)
                    (ok x)
                    (err (list 'type-error 'expected expected 'got (type-name x))))))

; ============================================
; Association List Utilities (Extended)
; ============================================

; alist-filter-keys: Filter alist by predicate on keys
(alist-filter-keys (fn (p alist)
                       (filter (fn (pair) (p (car pair))) alist)))

; alist-filter-values: Filter alist by predicate on values
(alist-filter-values (fn (p alist)
                         (filter (fn (pair) (p (cdr pair))) alist)))

; alist-keys: Get all keys from alist
; alist-values: Get all values from alist
; alist-has-key?: Check if alist has key
(alist-has-key? (fn (key alist)
                    (not (not (assoc key alist)))))

; alist-update-with-default: Update value at key with function
(alist-update-with-default (fn (key f default alist)
                               (let ((current (assoc-ref key alist)))
                                    (assoc-set key (f (if current current default)) alist))))

; ============================================
; Validation/Predicate Utilities (Extended)
; ============================================

; valid?: Check if value passes all predicates
(valid? (fn (preds value)
            (all (fn (p) (p value)) preds)))

; invalid?: Check if value fails any predicate
(invalid? (fn (preds value)
              (not (valid? preds value))))

; first-failing: Get first predicate that fails
(first-failing (fn (preds value)
                   (find (fn (p) (not (p value))) preds)))

; validate-with: Validate value and return result
(validate-with (fn (preds value)
                   (let ((failing (first-failing preds value)))
                        (if failing
                            (err (list 'validation-failed failing value))
                            (ok value)))))

; ============================================
; More List Utilities
; ============================================

; list-ref-safe: Safe list-ref with default
(list-ref-safe (fn (lst idx default)
                   (if (or (< idx 0) (>= idx (length lst)))
                       default
                       (list-ref lst idx))))

; list-set: Set element at index (functional)
(list-set (fn (lst idx val)
              (let ((setter (fix setter
                                 (fn (lst i)
                                     (if (null? lst)
                                         '()
                                         (if (= i 0)
                                             (cons val (cdr lst))
                                             (cons (car lst) (setter (cdr lst) (- i 1)))))))))
                   (setter lst idx))))

; list-update: Update element at index with function
(list-update (fn (lst idx f)
                 (list-set lst idx (f (list-ref lst idx)))))

; list-insert: Insert element at index
(list-insert (fn (lst idx val)
                 (append (take idx lst) (cons val (drop idx lst)))))

; list-delete: Delete element at index
(list-delete (fn (lst idx)
                 (append (take idx lst) (drop (+ idx 1) lst))))

; list-swap: Swap elements at two indices
(list-swap (fn (lst i j)
               (let ((vi (list-ref lst i))
                     (vj (list-ref lst j)))
                    (list-set (list-set lst i vj) j vi))))

; partition-by: Partition list by predicate into (pass fail)
(partition-by (fn (p lst)
                  (foldl (fn (acc x)
                             (if (p x)
                                 (cons (cons x (car acc)) (cdr acc))
                                 (cons (car acc) (cons x (cdr acc)))))
                         (cons '() '())
                         lst)))

; intersperse-with: Put separator between each pair, with custom function
(intersperse-with (fn (sep-fn lst)
                      (if (or (null? lst) (null? (cdr lst)))
                          lst
                          (let ((builder (fix builder
                                              (fn (lst acc)
                                                  (if (null? (cdr lst))
                                                      (reverse (cons (car lst) acc))
                                                      (builder (cdr lst)
                                                               (cons (sep-fn) (cons (car lst) acc))))))))
                               (builder lst '())))))

; ============================================
; Advanced Sorting Algorithms
; ============================================

; quicksort: Efficient O(n log n) average case sort
(quicksort (fix quicksort
                (fn (lst)
                    (if (null? lst)
                        '()
                        (if (null? (cdr lst))
                            lst
                            (let ((pivot (car lst))
                                  (rest (cdr lst)))
                                 (let ((lesser (filter (fn (x) (< x pivot)) rest))
                                       (greater (filter (fn (x) (>= x pivot)) rest)))
                                      (append (quicksort lesser)
                                              (cons pivot (quicksort greater))))))))))

; quicksort-by: Quicksort with custom key function
(quicksort-by (fix quicksort-by
                   (fn (key-fn lst)
                       (if (null? lst)
                           '()
                           (if (null? (cdr lst))
                               lst
                               (let ((pivot (car lst))
                                     (pivot-key (key-fn (car lst)))
                                     (rest (cdr lst)))
                                    (let ((lesser (filter (fn (x) (< (key-fn x) pivot-key)) rest))
                                          (greater (filter (fn (x) (>= (key-fn x) pivot-key)) rest)))
                                         (append (quicksort-by key-fn lesser)
                                                 (cons pivot (quicksort-by key-fn greater))))))))))

; mergesort: Stable O(n log n) sort
(mergesort (fix mergesort
                (fn (lst)
                    (if (null? lst)
                        '()
                        (if (null? (cdr lst))
                            lst
                            (let ((half (quotient (length lst) 2)))
                                 (let ((left (take lst half))
                                       (right (drop lst half)))
                                      (let ((merge (fix merge
                                                        (fn (a b)
                                                            (if (null? a) b
                                                                (if (null? b) a
                                                                    (if (<= (car a) (car b))
                                                                        (cons (car a) (merge (cdr a) b))
                                                                        (cons (car b) (merge a (cdr b))))))))))
                                           (merge (mergesort left) (mergesort right))))))))))

; mergesort-by: Mergesort with custom comparator
(mergesort-by (fix mergesort-by
                   (fn (cmp lst)
                       (if (null? lst)
                           '()
                           (if (null? (cdr lst))
                               lst
                               (let ((half (quotient (length lst) 2)))
                                    (let ((left (take lst half))
                                          (right (drop lst half)))
                                         (let ((merge (fix merge
                                                           (fn (a b)
                                                               (if (null? a) b
                                                                   (if (null? b) a
                                                                       (if (<= (cmp (car a) (car b)) 0)
                                                                           (cons (car a) (merge (cdr a) b))
                                                                           (cons (car b) (merge a (cdr b))))))))))
                                              (merge (mergesort-by cmp left) (mergesort-by cmp right))))))))))

; ============================================
; Graph Algorithms (Extended)
; ============================================

; graph-topological-sort: Topological sort using Kahn's algorithm
; Returns sorted list or #f if cycle detected
; graph-has-cycle?: Check if directed graph has a cycle
; graph-reverse: Reverse all edges in graph
; graph-transpose: Alias for graph-reverse
; ============================================
; Lens-like Utilities for Nested Data
; ============================================

; get-in: Get nested value at path
; (get-in '((a . ((b . 1)))) '(a b)) => 1
(get-in (fix get-in
             (fn (data path)
                 (if (null? path)
                     data
                     (if (pair? data)
                         (let ((key (car path)))
                              (if (number? key)
                                  (get-in (list-ref data key) (cdr path))
                                  (get-in (assoc-ref key data) (cdr path))))
                         #f)))))

; update-in: Update nested value at path with function
(update-in (fix update-in
                (fn (data path f)
                    (if (null? path)
                        (f data)
                        (let ((key (car path)))
                             (if (number? key)
                                 (list-set data key (update-in (list-ref data key) (cdr path) f))
                                 (assoc-set key (update-in (assoc-ref key data) (cdr path) f) data)))))))

; set-in: Set nested value at path
(set-in (fn (data path val)
            (update-in data path (fn (x) val))))

; ============================================
; Additional String Utilities
; ============================================

; string-repeat: Repeat string n times
; string-count: Count occurrences of substring
; ============================================
; Sequence Utilities
; ============================================

; scan: Like foldl but collects all intermediate results
(scan (fix scan
           (fn (f init lst)
               (if (null? lst)
                   (list init)
                   (cons init (scan f (f init (car lst)) (cdr lst)))))))

; windows: Sliding windows of size n
(windows (fn (n lst)
             (if (< (length lst) n)
                 '()
                 (let ((builder (fix builder
                                     (fn (lst acc)
                                         (if (< (length lst) n)
                                             (reverse acc)
                                             (builder (cdr lst) (cons (take lst n) acc)))))))
                      (builder lst '())))))

; pairwise: Apply binary function to adjacent pairs
(pairwise (fn (f lst)
              (if (or (null? lst) (null? (cdr lst)))
                  '()
                  (let ((builder (fix builder
                                      (fn (lst acc)
                                          (if (null? (cdr lst))
                                              (reverse acc)
                                              (builder (cdr lst)
                                                       (cons (f (car lst) (cadr lst)) acc)))))))
                       (builder lst '())))))

; differences: Compute differences between adjacent elements
(differences (fn (lst)
                 (pairwise - lst)))

; running-sum: Cumulative sum (exclude initial accumulator)
(running-sum (fn (lst)
                 (cdr (scanl + 0 lst))))

; running-product: Cumulative product (exclude initial accumulator)
(running-product (fn (lst)
                     (cdr (scanl * 1 lst))))

; ============================================
; Additional Numeric Utilities
; ============================================

; mean: Arithmetic mean of list
; REMOVED: mean (duplicate, now in stats.ss)

; median: Median of list
; REMOVED: median (duplicate, now in stats.ss)

; mode: Most frequent element
; REMOVED: mode (duplicate, now in stats.ss)

; variance: Population variance
; REMOVED: variance (duplicate, now in stats.ss)

; std-dev: Standard deviation
; REMOVED: std-dev (duplicate, now in stats.ss)

; normalize-list: Normalize list to [0, 1] range
; REMOVED: normalize-list (duplicate, now in stats.ss)

; z-scores: Convert list to z-scores (standard scores for all elements)
; REMOVED: z-scores (duplicate, now in stats.ss)

; ============================================
; Bitwise Operation Wrappers
; ============================================

; bit-and: Bitwise AND
(bit-and (fn (a b) (bitand a b)))

; bit-or: Bitwise OR
(bit-or (fn (a b) (bitor a b)))

; bit-xor: Bitwise XOR
(bit-xor (fn (a b) (bitxor a b)))

; bit-not: Bitwise NOT
(bit-not (fn (n) (bitnot n)))

; bit-shift-left: Left shift
(bit-shift-left (fn (n pos) (shl n pos)))

; bit-shift-right: Right shift
(bit-shift-right (fn (n pos) (shr n pos)))

; bit-set?: Check if bit at position is set
(bit-set? (fn (n pos)
              (not (= 0 (bit-and n (shl 1 pos))))))

; bit-set: Set bit at position
(bit-set (fn (n pos)
             (bit-or n (shl 1 pos))))

; bit-clear: Clear bit at position
(bit-clear (fn (n pos)
               (bit-and n (bit-not (shl 1 pos)))))

; bit-toggle: Toggle bit at position
(bit-toggle (fn (n pos)
                (bit-xor n (shl 1 pos))))

; bit-count: Count set bits (population count)
(bit-count (fn (n)
               (let ((counter (fix counter
                                   (fn (n count)
                                       (if (= n 0)
                                           count
                                           (counter (bit-and n (- n 1)) (+ count 1)))))))
                    (counter (abs n) 0))))

; ============================================
; Predicate Combinators
; ============================================

; complement: Negate a predicate
; conjoin: AND multiple predicates
; disjoin: OR multiple predicates
; pred-and: Combine two predicates with AND
(pred-and (fn (p q)
              (fn (x) (and (p x) (q x)))))

; pred-or: Combine two predicates with OR
(pred-or (fn (p q)
             (fn (x) (or (p x) (q x)))))

; satisfies?: Check if value satisfies all predicates
(satisfies? (fn (x preds)
                ((conjoin preds) x)))

; ============================================
; Advanced Function Combinators
; ============================================

; juxt: Apply multiple functions to same argument, return list of results
; ((juxt (list inc dec double)) 5) => (6 4 10)
; juxt2: Apply multiple functions to two arguments
(juxt2 (fn (fns)
           (fn (x y)
               (map (fn (f) (f x y)) fns))))

; fork: Apply multiple functions, combine with binary function
; ((fork + inc dec) 5) => (+ 6 4) => 10
(fork (fn (combine f g)
          (fn (x)
              (combine (f x) (g x)))))

; converge: Apply functions to args, combine results
; on: Apply binary function after applying unary function to both args
; ((on + length) "abc" "de") => (+ 3 2) => 5
; map-pair: Apply same function to both elements of a pair
(map-pair (fn (f pair)
              (cons (f (car pair)) (f (cdr pair)))))

; ============================================
; Currying and Partial Application
; ============================================

; curry2: Convert 2-arg function to curried form
; ((curry2 +) 1) => closure, (((curry2 +) 1) 2) => 3
; curry3: Convert 3-arg function to curried form
(curry3 (fn (f)
            (fn (a)
                (fn (b)
                    (fn (c)
                        (f a b c))))))

; uncurry2: Convert curried function back to 2-arg
; uncurry3: Convert curried function back to 3-arg
(uncurry3 (fn (f)
              (fn (a b c)
                  (((f a) b) c))))

; partial: Partial application - fix first argument
; ((partial + 1) 2) => 3
(partial (fn (f a)
             (fn (b)
                 (f a b))))

; partial2: Fix first two arguments
; partial-right: Fix last argument
; ((partial-right - 1) 5) => 4
(partial-right (fn (f b)
                   (fn (a)
                       (f a b))))

; flip: Flip arguments of binary function
; ((flip -) 1 5) => 4
; flip3: Flip first and third arguments
(flip3 (fn (f)
           (fn (a b c)
               (f c b a))))

; ============================================
; Iteration Utilities
; ============================================

; iterate-until: Apply function until predicate is true
(iterate-until (fn (f pred x)
                   (let ((iter (fix iter
                                    (fn (acc)
                                        (if (pred acc)
                                            acc
                                            (iter (f acc)))))))
                        (iter x))))

; iterate-while: Apply function while predicate is true
(iterate-while (fn (f pred x)
                   (let ((iter (fix iter
                                    (fn (acc)
                                        (if (pred acc)
                                            (iter (f acc))
                                            acc)))))
                        (iter x))))

; fixed-point: Find fixed point (when f(x) = x)
(fixed-point (fn (f x)
                 (let ((iter (fix iter
                                  (fn (prev)
                                      (let ((next (f prev)))
                                           (if (= next prev)
                                               next
                                               (iter next)))))))
                      (iter x))))

; ============================================
; Collection Utilities (Extended)
; ============================================

; count-if: Count elements satisfying predicate
; remove-if: Remove elements satisfying predicate (opposite of filter)
; remove-duplicates: Remove duplicates preserving order
(remove-duplicates (fn (lst)
                       (let ((helper (fix helper
                                          (fn (lst seen)
                                              (if (null? lst)
                                                  '()
                                                  (if (member? (car lst) seen)
                                                      (helper (cdr lst) seen)
                                                      (cons (car lst)
                                                            (helper (cdr lst) (cons (car lst) seen)))))))))
                            (helper lst '()))))

; unique: Alias for remove-duplicates
(unique remove-duplicates)

; replace-if: Replace elements satisfying predicate
(replace-if (fn (p new-val lst)
                (map (fn (x) (if (p x) new-val x)) lst)))

; substitute: Replace old value with new value
(substitute (fn (old new lst)
                (map (fn (x) (if (eq? x old) new x)) lst)))

; ============================================
; Numeric Range Utilities
; ============================================

; range-step: Range with custom step
(range-step (fn (start end step)
                (let ((builder (fix builder
                                    (fn (current acc)
                                        (if (if (> step 0) (>= current end) (<= current end))
                                            (reverse acc)
                                            (builder (+ current step) (cons current acc)))))))
                     (builder start '()))))

; linspace: Generate n evenly spaced numbers from start to end
(linspace (fn (start end n)
              (if (<= n 1)
                  (list start)
                  (let ((step (/ (- end start) (- n 1))))
                       (build-list n (fn (i) (+ start (* i step))))))))

; geometric-series: Generate geometric series
(geometric-series (fn (start ratio n)
                      (build-list n (fn (i) (* start (expt ratio i))))))

; ============================================
; Tuple/Pair Utilities
; ============================================

; pair-map: Map function over both elements of pair
(pair-map (fn (f pair)
              (cons (f (car pair)) (f (cdr pair)))))

; pair-swap: Swap elements of pair
(pair-swap (fn (pair)
               (cons (cdr pair) (car pair))))

; pair-to-list: Convert pair to two-element list
(pair-to-list (fn (pair)
                  (list (car pair) (cdr pair))))

; list-to-pair: Convert two-element list to pair
(list-to-pair (fn (lst)
                  (cons (car lst) (cadr lst))))

; ============================================
; Alist/Dict Operations (Extended)
; ============================================

; alist-invert: Swap keys and values
; alist-group-by: Group by key function
(alist-group-by (fn (key-fn lst)
                    (foldl (fn (acc x)
                               (let ((k (key-fn x)))
                                    (let ((existing (assoc-ref k acc)))
                                         (assoc-set k (cons x (if existing existing '())) acc))))
                           '()
                           lst)))

; alist-zip: Create alist from two lists
(alist-zip (fn (keys vals)
               (map cons keys vals)))

; alist-select: Select only specified keys
(alist-select (fn (keys alist)
                  (filter (fn (pair) (member? (car pair) keys)) alist)))

; alist-reject: Remove specified keys
(alist-reject (fn (keys alist)
                  (filter (fn (pair) (not (member? (car pair) keys))) alist)))

; alist-rename-key: Rename a key
(alist-rename-key (fn (old-key new-key alist)
                      (map (fn (pair)
                               (if (eq? (car pair) old-key)
                                   (cons new-key (cdr pair))
                                   pair))
                           alist)))

; ============================================
; Mathematical Utilities
; ============================================

; fibonacci: Compute nth Fibonacci number
(fibonacci (fn (n)
               (let ((fib (fix fib
                               (fn (n a b)
                                   (if (<= n 0)
                                       a
                                       (fib (- n 1) b (+ a b)))))))
                    (fib n 0 1))))

; prime?: Check if number is prime
; sum-range: Sum of integers from a to b
(sum-range (fn (a b)
               (* (/ (+ (- b a) 1) 2) (+ a b))))

; product-range: Product of integers from a to b
(product-range (fn (a b)
                   (let ((prod (fix prod
                                    (fn (i acc)
                                        (if (> i b)
                                            acc
                                            (prod (+ i 1) (* acc i)))))))
                        (prod a 1))))

; ============================================
; More List Utilities
; ============================================

; scanr: Right-to-left scan
(scanr (fn (f init lst)
           (if (null? lst)
               (list init)
               (let ((rest (scanr f init (cdr lst))))
                    (cons (f (car lst) (car rest)) rest)))))

; reduce: Like foldl but uses first element as initial value
; reduce-right: Like foldr but uses last element as initial value
; find-last: Find last element satisfying predicate
(find-last (fn (p lst)
               (foldl (fn (acc x) (if (p x) x acc)) #f lst)))

; last-index-where: Find index of last element satisfying predicate
(last-index-where (fn (p lst)
                      (let ((finder (fix finder
                                         (fn (lst i last-found)
                                             (if (null? lst)
                                                 last-found
                                                 (finder (cdr lst) (+ i 1) (if (p (car lst)) i last-found)))))))
                           (finder lst 0 -1))))

; take-last: Take last n elements
; drop-last: Drop last n elements
; set-at: Set element at index
(set-at (fn (idx val lst)
            (update-at idx (const val) lst)))

; ============================================
; String Utilities (Extended)
; ============================================

; string-repeat: Repeat string n times
; string-trim-left: Remove leading whitespace
; string-trim-right: Remove trailing whitespace
; string-trim: Remove leading and trailing whitespace
; string-blank?: Check if string is empty or only whitespace
; ============================================
; Control Flow (Extended)
; ============================================

; unless: Opposite of when
(unless (fn (condition body)
            (if condition '() body)))

; when-let: Execute body if value is truthy, binding value
; cond-result: Evaluate conditions, return first truthy result
(cond-result (fn (pairs)
                 (if (null? pairs)
                     #f
                     (let ((result ((car (car pairs)))))
                          (if result
                              ((cdr (car pairs)) result)
                              (cond-result (cdr pairs)))))))

; ============================================
; Comparison Utilities
; ============================================

; clamp: Clamp value to range
; between-exclusive?: Check if value is strictly between bounds
(between-exclusive? (fn (lo hi x)
                        (and (> x lo) (< x hi))))

; approximately-equal?: Check if two numbers are approximately equal
(approximately-equal? (fn (a b epsilon)
                          (< (abs (- a b)) epsilon)))

; compare: Three-way comparison returning -1, 0, or 1
(compare (fn (a b)
             (if (< a b) -1 (if (> a b) 1 0))))

; ============================================
; Debugging Utilities
; ============================================

; tap: Execute side-effect and return value (useful for debugging)
; trace: Print value and return it
; ============================================
; Set Operations (using sorted lists)
; ============================================

; set-from-list: Create set from list (sorted, unique)
; set-member?: Check membership in set
; set-add: Add element to set
; set-remove: Remove element from set
; set-empty?: Check if set is empty
; set-size: Get size of set
; ============================================
; Tree Operations (using nested lists)
; ============================================

; tree-leaf?: Check if node is a leaf (not a list)
(tree-leaf? (fn (node)
                (not (list? node))))

; tree-node?: Check if node is internal (a list)
(tree-node? (fn (node)
                (list? node)))

; tree-children: Get children of a tree node
(tree-children (fn (node)
                   (if (tree-leaf? node) '() node)))

; tree-map: Map function over all leaves
(tree-map (fix tree-map
               (fn (f tree)
                   (if (tree-leaf? tree)
                       (f tree)
                       (map (fn (child) (tree-map f child)) tree)))))

; tree-fold: Fold over tree with leaf and node functions
(tree-fold (fix tree-fold
                (fn (leaf-fn node-fn tree)
                    (if (tree-leaf? tree)
                        (leaf-fn tree)
                        (node-fn (map (fn (child) (tree-fold leaf-fn node-fn child)) tree))))))

; tree-flatten: Flatten tree to list of leaves
(tree-flatten (fn (tree)
                  (tree-fold (fn (x) (list x)) (fn (xs) (apply append xs)) tree)))

; tree-depth: Get maximum depth of tree
; tree-size: Count number of leaves
; tree-filter: Keep only leaves matching predicate
(tree-filter (fix tree-filter
                  (fn (p tree)
                      (if (tree-leaf? tree)
                          (if (p tree) (list tree) '())
                          (let ((children (flat-map (fn (child) (tree-filter p child)) tree)))
                               (if (null? children) '() children))))))

; tree-find: Find first leaf matching predicate
; ============================================
; Graph Utilities (adjacency list representation)
; ============================================

; graph-nodes: Get all nodes in graph
(graph-nodes (fn (graph)
                 (map car graph)))

; graph-remove-edge: Remove directed edge
; graph-reverse: Reverse all edges
; ============================================
; More Numeric Utilities
; ============================================

; sgn: Sign function (-1, 0, or 1)
(sgn (fn (x)
         (if (< x 0) -1 (if (> x 0) 1 0))))

; copysign: Copy sign from y to x
(copysign (fn (x y)
              (* (abs x) (sgn y))))

; div-ceil: Division rounding up
(div-ceil (fn (a b)
              (+ (/ a b) (if (= 0 (mod a b)) 0 1))))

; div-floor: Division rounding down (same as /)
(div-floor (fn (a b)
               (/ a b)))

; mod-positive: Modulo that always returns positive
(mod-positive (fn (a b)
                  (let ((r (mod a b)))
                       (if (< r 0) (+ r (abs b)) r))))

; integer-sqrt: Integer square root (floor)
(integer-sqrt (fn (n)
                  (if (< n 0)
                      -1
                      (if (< n 2)
                          n
                          (let ((newton (fix newton
                                             (fn (x)
                                                 (let ((x1 (/ (+ x (/ n x)) 2)))
                                                      (if (>= x1 x)
                                                          x
                                                          (newton x1)))))))
                               (newton (/ n 2)))))))

; sum-of-squares: Sum of squares of list
(sum-of-squares (fn (lst)
                    (foldl (fn (acc x) (+ acc (* x x))) 0 lst)))

; dot-product: Dot product of two vectors
(dot-product (fn (v1 v2)
                 (foldl + 0 (zip-with * v1 v2))))

; magnitude: Euclidean magnitude of vector
(magnitude (fn (v)
               (sqrt (sum-of-squares v))))

; normalize-vector: Normalize vector to unit length
(normalize-vector (fn (v)
                      (let ((mag (magnitude v)))
                           (if (= mag 0)
                               v
                               (map (fn (x) (/ x mag)) v)))))

; ============================================
; Sequence Generators
; ============================================

; fibonacci-sequence: Generate first n Fibonacci numbers
(fibonacci-sequence (fn (n)
                        (if (<= n 0)
                            '()
                            (if (= n 1)
                                '(0)
                                (let ((builder (fix builder
                                                    (fn (count a b acc)
                                                        (if (>= count n)
                                                            (reverse acc)
                                                            (builder (+ count 1) b (+ a b) (cons a acc)))))))
                                     (builder 0 0 1 '()))))))

; ============================================
; Combinatorics
; ============================================

; remove-first: Remove first occurrence of element
(remove-first (fn (x lst)
                  (if (null? lst)
                      '()
                      (if (eq? x (car lst))
                          (cdr lst)
                          (cons (car lst) (remove-first x (cdr lst)))))))

; ============================================
; Queue (FIFO) - Simple list-based implementation
; ============================================

; queue-new: Create empty queue
; queue-empty: Empty queue constant
; queue-empty?: Check if queue is empty
; queue-enqueue: Add element to back of queue (queue first, element second)
; queue-dequeue: Remove and return front element (returns (element . rest))
; queue-front: Peek at front element
; queue-size: Get queue size
; queue-to-list: Convert queue to list (identity for list-based)
; queue-from-list: Create queue from list (identity for list-based)
(queue-from-list id)

; ============================================
; Min-Heap (using list, not efficient but functional)
; ============================================

; heap-empty: Empty heap
(heap-empty '())

; heap-empty?: Check if heap is empty
(heap-empty? null?)

; heap-insert: Insert element maintaining heap property
(heap-insert (fn (x heap)
                 (sort (cons x heap))))

; heap-min: Get minimum element
(heap-min (fn (heap)
              (if (null? heap) #f (car heap))))

; heap-pop: Remove min, return (min . new-heap)
(heap-pop (fn (heap)
              (if (null? heap)
                  (cons #f '())
                  (cons (car heap) (cdr heap)))))

; heap-from-list: Create heap from list
(heap-from-list (fn (lst)
                    (sort lst)))

; heap-size: Get heap size
(heap-size length)

; ============================================
; Priority Queue (element . priority pairs)
; ============================================

; pq-empty: Empty priority queue
(pq-empty '())

; pq-empty?: Check if empty
; pq-insert: Insert with priority (lower = higher priority)
; pq-pop: Remove highest priority, return (element . new-pq)
; pq-peek: Peek at highest priority element
; ============================================
; Deque (Double-ended queue)
; ============================================

; deque-empty: Empty deque
(deque-empty (cons '() '()))

; deque-empty?: Check if empty
; deque-push-front: Add to front
; deque-push-back: Add to back
; deque-pop-front: Remove from front
; deque-pop-back: Remove from back
; ============================================
; Stack Operations (simple list-based)
; ============================================

; stack-empty: Empty stack
; stack-empty?: Check if empty
; stack-push: Push element onto stack (stack first, then element)
; stack-pop: Pop element, return (element . new-stack)
; stack-peek: Peek at top
(stack-peek (fn (s)
                (if (null? s) #f (car s))))

; stack-size: Get size
; ============================================
; Number Parsing and Formatting
; ============================================

; ============================================
; Base Conversion
; ============================================

; to-base: Convert number to list of digits in given base
(to-base (fn (n base)
             (let ((convert (fix convert
                                 (fn (n acc)
                                     (if (= n 0)
                                         (if (null? acc) '(0) acc)
                                         (convert (/ n base) (cons (mod n base) acc)))))))
                  (convert (abs n) '()))))

; from-base: Convert list of digits from given base to number
(from-base (fn (digits base)
               (foldl (fn (acc d) (+ (* acc base) d)) 0 digits)))

; to-binary: Convert to binary digits
(to-binary (fn (n) (to-base n 2)))

; from-binary: Convert from binary digits
(from-binary (fn (bits) (from-base bits 2)))

; to-hex-digits: Convert to hex digits (0-15)
(to-hex-digits (fn (n) (to-base n 16)))

; from-hex-digits: Convert from hex digits
(from-hex-digits (fn (digits) (from-base digits 16)))

; ============================================
; List Searching and Manipulation
; ============================================

; binary-search: Binary search on sorted list, returns index or -1
; lower-bound: First index where element >= target
; upper-bound: First index where element > target
; ============================================
; Sliding Window Operations
; ============================================

; sliding-max: Maximum of each window of size k
(sliding-max (fn (k lst)
                 (if (< (length lst) k)
                     '()
                     (let ((windows (fix windows
                                         (fn (lst acc)
                                             (if (< (length lst) k)
                                                 (reverse acc)
                                                 (windows (cdr lst) (cons (take lst k) acc)))))))
                          (map (fn (w) (apply max w)) (windows lst '()))))))

; sliding-min: Minimum of each window of size k
(sliding-min (fn (k lst)
                 (if (< (length lst) k)
                     '()
                     (let ((windows (fix windows
                                         (fn (lst acc)
                                             (if (< (length lst) k)
                                                 (reverse acc)
                                                 (windows (cdr lst) (cons (take lst k) acc)))))))
                          (map (fn (w) (apply min w)) (windows lst '()))))))

; sliding-sum: Sum of each window of size k
(sliding-sum (fn (k lst)
                 (if (< (length lst) k)
                     '()
                     (let ((windows (fix windows
                                         (fn (lst acc)
                                             (if (< (length lst) k)
                                                 (reverse acc)
                                                 (windows (cdr lst) (cons (take lst k) acc)))))))
                          (map (fn (w) (apply + w)) (windows lst '()))))))

; ============================================
; Run-Length Encoding
; ============================================

; rle-encode: Run-length encode list
(rle-encode (fn (lst)
                (if (null? lst)
                    '()
                    (let ((encode (fix encode
                                       (fn (lst current count acc)
                                           (if (null? lst)
                                               (reverse (cons (cons current count) acc))
                                               (if (eq? (car lst) current)
                                                   (encode (cdr lst) current (+ count 1) acc)
                                                   (encode (cdr lst) (car lst) 1
                                                           (cons (cons current count) acc))))))))
                         (encode (cdr lst) (car lst) 1 '())))))

; rle-decode: Decode run-length encoded list
(rle-decode (fn (rle)
                (flat-map (fn (pair) (replicate (cdr pair) (car pair))) rle)))

; ============================================
; More Functional Patterns
; ============================================

; until: Apply f until predicate is true
(until (fn (p f x)
           (if (p x) x (until p f (f x)))))

; while-fn: Apply f while predicate is true
(while-fn (fn (p f x)
              (if (p x) (while-fn p f (f x)) x)))

; iterate-collect: Collect n iterations of f starting from x
(iterate-collect (fn (f n x)
                     (if (<= n 0)
                         '()
                         (cons x (iterate-collect f (- n 1) (f x))))))

; converge-to: Apply f until value stops changing
(converge-to (fn (f x)
                 (let ((next (f x)))
                      (if (equal? x next) x (converge-to f next)))))

; ============================================
; Association List Extensions
; ============================================

; alist-map-values: Map function over values
(alist-map-values (fn (f alist)
                      (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

; alist-map-keys: Map function over keys
(alist-map-keys (fn (f alist)
                    (map (fn (pair) (cons (f (car pair)) (cdr pair))) alist)))

; alist-count: Count pairs matching predicate
(alist-count (fn (p alist)
                 (count-if p alist)))

; ============================================
; List Zipper (for efficient traversal)
; Zipper = (left-reversed . (focus . right))
; ============================================

; zipper-from-list: Create zipper from list, focus on first element
(zipper-from-list (fn (lst)
                      (if (null? lst)
                          (cons '() (cons #f '()))
                          (cons '() (cons (car lst) (cdr lst))))))

; zipper-to-list: Convert zipper back to list
(zipper-to-list (fn (z)
                    (append (reverse (car z))
                            (if (eq? (car (cdr z)) #f)
                                '()
                                (cons (car (cdr z)) (cdr (cdr z)))))))

; zipper-focus: Get focused element
(zipper-focus (fn (z)
                  (car (cdr z))))

; zipper-right: Move focus right
(zipper-right (fn (z)
                  (let ((left (car z))
                        (focus (car (cdr z)))
                        (right (cdr (cdr z))))
                       (if (null? right)
                           z
                           (cons (cons focus left)
                                 (cons (car right) (cdr right)))))))

; zipper-left: Move focus left
(zipper-left (fn (z)
                 (let ((left (car z))
                       (focus (car (cdr z)))
                       (right (cdr (cdr z))))
                      (if (null? left)
                          z
                          (cons (cdr left)
                                (cons (car left) (cons focus right)))))))

; zipper-update: Update focused element with function
(zipper-update (fn (f z)
                   (let ((left (car z))
                         (focus (car (cdr z)))
                         (right (cdr (cdr z))))
                        (cons left (cons (f focus) right)))))

; zipper-set: Set focused element
(zipper-set (fn (val z)
                (zipper-update (const val) z)))

; zipper-insert-left: Insert element to the left of focus
(zipper-insert-left (fn (val z)
                        (cons (cons val (car z)) (cdr z))))

; zipper-insert-right: Insert element to the right of focus
(zipper-insert-right (fn (val z)
                         (let ((left (car z))
                               (focus (car (cdr z)))
                               (right (cdr (cdr z))))
                              (cons left (cons focus (cons val right))))))

; zipper-delete: Delete focused element, focus moves right
(zipper-delete (fn (z)
                   (let ((left (car z))
                         (right (cdr (cdr z))))
                        (if (null? right)
                            (if (null? left)
                                (cons '() (cons #f '()))
                                (cons (cdr left) (cons (car left) '())))
                            (cons left (cons (car right) (cdr right)))))))

; ============================================
; Validation Combinators
; ============================================

; validate-with-message: Validation with custom message - returns (ok value) or (err message)
(validate-with-message (fn (p msg value)
                           (if (p value)
                               (ok value)
                               (err msg))))

; validate-all-with-errors: Run all validations, collect all errors (returns result type)
(validate-all-with-errors (fn (validators value)
                              (let ((results (map (fn (v) (v value)) validators)))
                                   (let ((errors (filter err? results)))
                                        (if (null? errors)
                                            (ok value)
                                            (err (map err-value errors)))))))

; validate-chain: Run validations in sequence, stop on first error
(validate-chain (fix validate-chain
                     (fn (validators value)
                         (if (null? validators)
                             (ok value)
                             (let ((result ((car validators) value)))
                                  (if (ok? result)
                                      (validate-chain (cdr validators) (ok-value result))
                                      result))))))

; validator: Create a validator from predicate and message
; not-empty: Validate not empty (for lists/strings)
(not-empty-validator (validator (fn (x) (not (or (null? x) (and (string? x) (string-empty? x)))))
                                "must not be empty"))

; positive-validator: Validate positive number
(positive-validator (validator positive? "must be positive"))

; non-negative-validator: Validate non-negative number
(non-negative-validator (validator (fn (x) (>= x 0)) "must be non-negative"))

; in-range-validator: Validate number in range
(in-range-validator (fn (lo hi)
                        (validator (fn (x) (and (>= x lo) (<= x hi)))
                                   (string-append "must be between " (number->string lo) " and " (number->string hi)))))

; ============================================
; Lens-like Utilities (for nested data access)
; ============================================

; lens-get: Get value at path in nested structure
(lens-get (fix lens-get
               (fn (path data)
                   (if (null? path)
                       data
                       (let ((key (car path)))
                            (if (number? key)
                                (lens-get (cdr path) (list-ref data key))
                                (lens-get (cdr path) (assoc-ref key data))))))))

; lens-set: Set value at path in nested structure
(lens-set (fix lens-set
               (fn (path value data)
                   (if (null? path)
                       value
                       (let ((key (car path)))
                            (if (number? key)
                                (update-at key (fn (old) (lens-set (cdr path) value old)) data)
                                (assoc-set key (lens-set (cdr path) value (assoc-ref key data)) data)))))))

; lens-update: Update value at path with function
(lens-update (fn (path f data)
                 (lens-set path (f (lens-get path data)) data)))

; ============================================
; Applicative Functor Utilities
; ============================================

; ap: Apply wrapped function to wrapped value (for Maybe/Either)
(ap (fn (wf wa)
        (if (ok? wf)
            (if (ok? wa)
                (ok ((ok-value wf) (ok-value wa)))
                wa)
            wf)))

; lift2: Lift binary function to work on wrapped values
(lift2 (fn (f wa wb)
           (ap (ap (ok f) wa) wb)))

; lift3: Lift ternary function to work on wrapped values
(lift3 (fn (f wa wb wc)
           (ap (ap (ap (ok f) wa) wb) wc)))

; sequence-list: Turn list of (ok x) into (ok list-of-x)
(sequence-list (fn (lst)
                   (foldr (fn (wx acc)
                              (if (err? wx)
                                  wx
                                  (if (err? acc)
                                      acc
                                      (ok (cons (ok-value wx) (ok-value acc))))))
                          (ok '())
                          lst)))

; traverse-list: Map and sequence
(traverse-list (fn (f lst)
                   (sequence-list (map f lst))))

; ============================================
; More String Utilities
; ============================================

; string-split-at: Split string at index
; string-words: Split string into words (by whitespace)
; string-unwords: Join words with space
; string-lines: Split string into lines
; string-unlines: Join lines with newline
; string-ljust: Left-justify string to width
; string-rjust: Right-justify string to width
; ============================================
; Memoization Helpers
; ============================================

; make-memo-table: Create empty memo table (alist)
(make-memo-table (fn () '()))

; memo-lookup: Look up in memo table
; memo-insert: Insert into memo table
; ============================================
; Matrix Operations (2D list)
; ============================================

; matrix-rows: Get number of rows
; matrix-cols: Get number of columns
; matrix-ref: Get element at (row, col)
; matrix-set: Set element at (row, col)
(matrix-set (fn (m row col val)
                (update-at row (fn (r) (set-at col val r)) m)))

; matrix-transpose: Transpose matrix
; matrix-map: Map function over all elements
; matrix-zip-with: Zip two matrices with function
(matrix-zip-with (fn (f m1 m2)
                     (zip-with (fn (r1 r2) (zip-with f r1 r2)) m1 m2)))

; matrix-add: Add two matrices
; matrix-scale: Scale matrix by scalar
; matrix-multiply: Matrix multiplication
; make-matrix: Create m x n matrix filled with value
(make-matrix (fn (rows cols val)
                 (build-list rows (fn (i) (build-list cols (fn (j) val))))))

; identity-matrix: Create n x n identity matrix
(identity-matrix (fn (n)
                     (build-list n (fn (i)
                                       (build-list n (fn (j) (if (= i j) 1 0)))))))

; ============================================
; Logic and Boolean Utilities
; ============================================

; bool->int: Convert boolean to 0/1
(bool->int (fn (b)
               (if b 1 0)))

; int->bool: Convert 0/1 to boolean
(int->bool (fn (n)
               (not (= n 0))))

; ============================================
; State Monad Utilities
; State s a = s -> (a, s)
; ============================================

; state-run: Run state computation
(state-run (fn (ma s)
               (ma s)))

; state-eval: Run and get result only
(state-eval (fn (ma s)
                (car (ma s))))

; state-exec: Run and get final state only
(state-exec (fn (ma s)
                (cadr (ma s))))

; ============================================
; Reader Monad Utilities
; Reader r a = r -> a
; ============================================

; reader-asks: Get part of the environment
(reader-asks (fn (f)
                 (fn (r) (f r))))

; reader-run: Run reader computation
(reader-run (fn (ma r)
                (ma r)))

; ============================================
; Writer Monad Utilities (using lists for log)
; Writer w a = (a, [w])
; ============================================

; writer-listen: Get the log
(writer-listen (fn (wa)
                   (list (list (car wa) (cadr wa)) (cadr wa))))

; writer-run: Extract result and log
(writer-run id)

; ============================================
; More List Utilities
; ============================================

; tails: All suffixes of a list
; inits: All prefixes of a list
; sublists: All contiguous sublists
(sublists (fn (lst)
              (apply append (map tails (inits lst)))))

; is-prefix?: Check if lst1 is prefix of lst2
(is-prefix? (fix is-prefix?
                 (fn (lst1 lst2)
                     (if (null? lst1)
                         #t
                         (if (null? lst2)
                             #f
                             (if (equal? (car lst1) (car lst2))
                                 (is-prefix? (cdr lst1) (cdr lst2))
                                 #f))))))

; is-suffix?: Check if lst1 is suffix of lst2
(is-suffix? (fn (lst1 lst2)
                (is-prefix? (reverse lst1) (reverse lst2))))

; is-infix?: Check if lst1 is infix of lst2
(is-infix? (fn (lst1 lst2)
               (any (fn (t) (is-prefix? lst1 t)) (tails lst2))))

; split-at: Split list at index
; chunks-of: Split list into chunks of size n
(chunks-of (fix chunks-of
                (fn (n lst)
                    (if (null? lst)
                        '()
                        (cons (take lst n) (chunks-of n (drop lst n)))))))

; windows: Sliding windows of size n
(windows (fix windows
              (fn (n lst)
                  (if (< (length lst) n)
                      '()
                      (cons (take lst n) (windows n (cdr lst)))))))

; dedup-consecutive: Remove consecutive duplicates
; ============================================
; Polynomial Operations (coefficients as lists, lowest degree first)
; ============================================

; poly-zero: Zero polynomial
(poly-zero '())

; poly-one: One polynomial
(poly-one (list 1))

; poly-degree: Degree of polynomial (-1 for zero polynomial)
(poly-degree (fn (p)
                 (- (length p) 1)))

; poly-add: Add two polynomials
(poly-add (fix poly-add
               (fn (p q)
                   (if (null? p)
                       q
                       (if (null? q)
                           p
                           (cons (+ (car p) (car q))
                                 (poly-add (cdr p) (cdr q))))))))

; poly-scale: Multiply polynomial by scalar
(poly-scale (fn (k p)
                (map (fn (c) (* k c)) p)))

; poly-shift: Multiply polynomial by x^n (shift coefficients)
(poly-shift (fn (n p)
                (append (replicate n 0) p)))

; poly-multiply: Multiply two polynomials
(poly-multiply (fn (p q)
                   (if (null? p)
                       '()
                       (poly-add (poly-scale (car p) q)
                                 (poly-shift 1 (poly-multiply (cdr p) q))))))

; poly-eval: Evaluate polynomial at x using Horner's method
(poly-eval (fn (p x)
               (foldr (fn (c acc) (+ c (* x acc))) 0 p)))

; poly-derivative: Derivative of polynomial
(poly-derivative (fn (p)
                     (if (null? p)
                         '()
                         (map-indexed (fn (i c) (* (+ i 1) c)) (cdr p)))))

; poly-from-roots: Polynomial from roots (x-r1)(x-r2)...
(poly-from-roots (fn (roots)
                     (foldl (fn (p r) (poly-multiply p (list (- 0 r) 1)))
                            (list 1)
                            roots)))

; ============================================
; Interval Operations (closed intervals as pairs)
; ============================================

; make-interval: Create interval [lo, hi]
(make-interval cons)

; interval-lo: Get lower bound
(interval-lo car)

; interval-hi: Get upper bound
(interval-hi cdr)

; interval-empty?: Check if interval is empty
(interval-empty? (fn (i)
                     (> (car i) (cdr i))))

; interval-contains?: Check if interval contains point
(interval-contains? (fn (i x)
                        (and (>= x (car i)) (<= x (cdr i)))))

; interval-width: Width of interval
(interval-width (fn (i)
                    (- (cdr i) (car i))))

; interval-midpoint: Midpoint of interval
(interval-midpoint (fn (i)
                       (/ (+ (car i) (cdr i)) 2)))

; interval-intersect: Intersection of two intervals
(interval-intersect (fn (i1 i2)
                        (cons (max (car i1) (car i2))
                              (min (cdr i1) (cdr i2)))))

; interval-union: Union of two overlapping intervals
(interval-union (fn (i1 i2)
                    (cons (min (car i1) (car i2))
                          (max (cdr i1) (cdr i2)))))

; interval-overlaps?: Check if intervals overlap
(interval-overlaps? (fn (i1 i2)
                        (and (<= (car i1) (cdr i2))
                             (<= (car i2) (cdr i1)))))

; ============================================
; Arrow Combinators (for function composition)
; ============================================

; arr: Lift function to arrow
(arr id)

; first: Apply arrow to first element of pair
; second: Apply arrow to second element of pair
; split: Apply two arrows to same input, return pair
(split (fn (f g)
           (fn (x)
               (cons (f x) (g x)))))

; fanout: Alias for split
(fanout split)

; combine: Apply two arrows to pair elements
(combine (fn (f g)
             (fn (pair)
                 (cons (f (car pair)) (g (cdr pair))))))

; parallel: Alias for combine
(parallel combine)

; arrow-compose: Compose two arrows (left to right)
(arrow-compose (fn (f g)
                   (compose g f)))

; ============================================
; Continuation Utilities
; ============================================

; call/cc-style: Simulate call/cc with CPS
; Usage: (call-with-escape (fn (escape) ... (escape value) ...))
(call-with-escape (fn (f)
                      (f id)))

; trampoline-return: Return from trampoline
(trampoline-return (fn (x)
                       (cons 'done x)))

; trampoline-bounce: Continue trampolining
(trampoline-bounce (fn (thunk)
                       (cons 'bounce thunk)))

; run-trampoline: Execute trampolined computation
(run-trampoline (fix run-trampoline
                     (fn (t)
                         (if (eq? (car t) 'done)
                             (cdr t)
                             (run-trampoline ((cdr t)))))))

; ============================================
; More Numeric Utilities
; ============================================

; average: Average of a list of numbers
; variance: Variance of a list of numbers
; REMOVED: variance (duplicate, now in stats.ss)

; stddev: Standard deviation
(stddev (fn (lst)
            (sqrt (variance lst))))

; median: Median of a list
; REMOVED: median (duplicate, now in stats.ss)

; clamp-list: Clamp all values in list to range
(clamp-list (fn (lo hi lst)
                (map (fn (x) (clamp lo hi x)) lst)))

; normalize-list: Normalize list to [0, 1] range
; REMOVED: normalize-list (duplicate, now in stats.ss)

; dot: Alias for dot-product
(dot dot-product)

; magnitude: Vector magnitude
(magnitude (fn (v)
               (sqrt (dot v v))))

; normalize-vector: Normalize vector to unit length
(normalize-vector (fn (v)
                      (let ((mag (magnitude v)))
                           (if (= mag 0)
                               v
                               (map (fn (x) (/ x mag)) v)))))

; cross-product: Cross product of 3D vectors
(cross-product (fn (u v)
                   (list (- (* (second u) (third v)) (* (third u) (second v)))
                         (- (* (third u) (first v)) (* (first u) (third v)))
                         (- (* (first u) (second v)) (* (second u) (first v))))))

; ============================================
; Association List Extensions
; ============================================

; alist-insert-with: Insert or update with combining function
(alist-insert-with (fn (f key val alist)
                       (let ((existing (assoc key alist)))
                            (if existing
                                (alist-update key (fn (old) (f val old)) alist)
                                (cons (cons key val) alist)))))

; alist-from-pairs: Create alist from list of pairs
(alist-from-pairs id)

; alist-to-pairs: Convert alist to list of pairs
(alist-to-pairs id)

; alist-has-key?: Check if key exists
(alist-has-key? (fn (key alist)
                    (not (not (assoc key alist)))))

; alist-delete: Remove key from alist
(alist-delete (fn (key alist)
                  (filter (fn (pair) (not (equal? (car pair) key))) alist)))

; alist-select: Select only specified keys
(alist-select (fn (keys alist)
                  (filter (fn (pair) (member (car pair) keys)) alist)))

; alist-reject: Reject specified keys
(alist-reject (fn (keys alist)
                  (filter (fn (pair) (not (member (car pair) keys))) alist)))

; ============================================
; Parser Combinators
; Parser a = String -> [(a, String)] or (ok (value . rest)) | (err msg)
; ============================================

; parse-result: Successful parse result
(parse-ok (fn (value rest)
              (cons 'ok (cons value rest))))

; parse-err: Failed parse
(parse-err (fn (msg)
               (cons 'err msg)))

; parse-ok?: Check if parse succeeded
(parse-ok? (fn (r)
               (eq? (car r) 'ok)))

; parse-value: Get parsed value
(parse-value (fn (r)
                 (car (cdr r))))

; parse-rest: Get remaining input
(parse-rest (fn (r)
                (cdr (cdr r))))

; parse-error: Get error message
(parse-error (fn (r)
                 (cdr r)))

; p-return: Parser that succeeds with value without consuming input
(p-return (fn (x)
              (fn (input)
                  (parse-ok x input))))

; p-fail: Parser that always fails
(p-fail (fn (msg)
            (fn (input)
                (parse-err msg))))

; p-item: Parser that consumes one character
(p-item (fn (input)
            (if (string-empty? input)
                (parse-err "unexpected end of input")
                (parse-ok (string-ref input 0)
                          (string-drop 1 input)))))

; p-satisfy: Parser that consumes char satisfying predicate
(p-satisfy (fn (predicate)
               (fn (input)
                   (if (string-empty? input)
                       (parse-err "unexpected end of input")
                       (let ((c (string-ref input 0)))
                            (if (predicate c)
                                (parse-ok c (string-drop 1 input))
                                (parse-err "predicate not satisfied")))))))

; p-char: Parser for specific character
(p-char (fn (c)
            (p-satisfy (fn (x) (eq? x c)))))

; p-digit: Parser for digit character
(p-digit (p-satisfy char-numeric?))

; p-alpha: Parser for alphabetic character
(p-alpha (p-satisfy char-alphabetic?))

; p-alphanum: Parser for alphanumeric character
(p-alphanum (p-satisfy (fn (c) (or (char-alphabetic? c) (char-numeric? c)))))

; p-space: Parser for whitespace
(p-space (p-satisfy char-whitespace?))

; p-bind: Monadic bind for parsers
(p-bind (fn (p f)
            (fn (input)
                (let ((r (p input)))
                     (if (parse-ok? r)
                         ((f (parse-value r)) (parse-rest r))
                         r)))))

; p-then: Sequence two parsers, keep second result
(p-then (fn (p1 p2)
            (p-bind p1 (fn (_) p2))))

; p-skip: Sequence two parsers, keep first result
(p-skip (fn (p1 p2)
            (p-bind p1 (fn (x) (p-bind p2 (fn (_) (p-return x)))))))

; p-or: Try first parser, on failure try second
(p-or (fn (p1 p2)
          (fn (input)
              (let ((r (p1 input)))
                   (if (parse-ok? r)
                       r
                       (p2 input))))))

; p-many: Zero or more repetitions (defined with fix for self-recursion)
(p-many (fn (p)
            (let ((go (fix go
                           (fn (input)
                               (let ((r (p input)))
                                    (if (parse-ok? r)
                                        (let ((rest-result (go (parse-rest r))))
                                             (if (parse-ok? rest-result)
                                                 (parse-ok (cons (parse-value r) (parse-value rest-result))
                                                           (parse-rest rest-result))
                                                 (parse-ok (list (parse-value r)) (parse-rest r))))
                                        (parse-ok '() input)))))))
                 go)))

; p-many1: One or more repetitions
(p-many1 (fn (p)
             (p-bind p (fn (x)
                           (p-bind (p-many p) (fn (xs)
                                                  (p-return (cons x xs))))))))

; p-optional: Optional parser, returns #f on failure
(p-optional (fn (p)
                (p-or p (p-return #f))))

; p-string: Parse exact string
(p-string (fix p-string
               (fn (s)
                   (if (string-empty? s)
                       (p-return "")
                       (p-bind (p-char (string-ref s 0))
                               (fn (c)
                                   (p-bind (p-string (string-drop 1 s))
                                           (fn (cs)
                                               (p-return (string-append (string c) cs))))))))))

; p-spaces: Skip zero or more spaces
(p-spaces (p-many p-space))

; p-token: Parse token, skip trailing spaces
(p-token (fn (p)
             (p-skip p p-spaces)))

; p-run: Run parser on input
(p-run (fn (p input)
           (p input)))

; ============================================
; Binary Tree Operations
; Tree = () | (value left right)
; ============================================

; tree-empty: Empty tree
(tree-empty '())

; tree-empty?: Check if tree is empty
(tree-empty? null?)

; tree-node: Create tree node
(tree-node (fn (value left right)
               (list value left right)))

; tree-leaf: Create leaf node
(tree-leaf (fn (value)
               (tree-node value tree-empty tree-empty)))

; tree-value: Get node value
(tree-value car)

; tree-left: Get left subtree
(tree-left (fn (t) (car (cdr t))))

; tree-right: Get right subtree
(tree-right (fn (t) (car (cdr (cdr t)))))

; tree-size: Count nodes in tree
; tree-height: Height of tree
; tree-inorder: Inorder traversal
(tree-inorder (fix tree-inorder
                   (fn (t)
                       (if (tree-empty? t)
                           '()
                           (append (tree-inorder (tree-left t))
                                   (cons (tree-value t)
                                         (tree-inorder (tree-right t))))))))

; tree-preorder: Preorder traversal
(tree-preorder (fix tree-preorder
                    (fn (t)
                        (if (tree-empty? t)
                            '()
                            (cons (tree-value t)
                                  (append (tree-preorder (tree-left t))
                                          (tree-preorder (tree-right t))))))))

; tree-postorder: Postorder traversal
(tree-postorder (fix tree-postorder
                     (fn (t)
                         (if (tree-empty? t)
                             '()
                             (append (tree-postorder (tree-left t))
                                     (append (tree-postorder (tree-right t))
                                             (list (tree-value t))))))))

; tree-levelorder: Level-order (BFS) traversal
(tree-levelorder (fn (t)
                     (if (tree-empty? t)
                         '()
                         (let ((bfs (fix bfs
                                         (fn (queue acc)
                                             (if (null? queue)
                                                 (reverse acc)
                                                 (let ((node (car queue))
                                                       (rest (cdr queue)))
                                                      (if (tree-empty? node)
                                                          (bfs rest acc)
                                                          (bfs (append rest (list (tree-left node) (tree-right node)))
                                                               (cons (tree-value node) acc)))))))))
                              (bfs (list t) '())))))

; bst-insert: Insert into binary search tree
(bst-insert (fix bst-insert
                 (fn (x t)
                     (if (tree-empty? t)
                         (tree-leaf x)
                         (if (< x (tree-value t))
                             (tree-node (tree-value t)
                                        (bst-insert x (tree-left t))
                                        (tree-right t))
                             (tree-node (tree-value t)
                                        (tree-left t)
                                        (bst-insert x (tree-right t))))))))

; bst-member?: Check membership in BST
(bst-member? (fix bst-member?
                  (fn (x t)
                      (if (tree-empty? t)
                          #f
                          (if (= x (tree-value t))
                              #t
                              (if (< x (tree-value t))
                                  (bst-member? x (tree-left t))
                                  (bst-member? x (tree-right t))))))))

; bst-from-list: Build BST from list
(bst-from-list (fn (lst)
                   (foldl (fn (t x) (bst-insert x t)) tree-empty lst)))

; bst-min: Find minimum in BST
(bst-min (fix bst-min
              (fn (t)
                  (if (tree-empty? t)
                      #f
                      (if (tree-empty? (tree-left t))
                          (tree-value t)
                          (bst-min (tree-left t)))))))

; bst-max: Find maximum in BST
(bst-max (fix bst-max
              (fn (t)
                  (if (tree-empty? t)
                      #f
                      (if (tree-empty? (tree-right t))
                          (tree-value t)
                          (bst-max (tree-right t)))))))

; ============================================
; Union-Find (Disjoint Set) Data Structure
; Represented as alist of (element . parent)
; ============================================

; uf-make: Create union-find with single element
(uf-make (fn (x)
             (list (cons x x))))

; uf-make-set: Create union-find from list (each element is its own set)
(uf-make-set (fn (lst)
                 (map (fn (x) (cons x x)) lst)))

; uf-find: Find root of element (with path compression simulation)
(uf-find (fix uf-find
              (fn (uf x)
                  (let ((entry (assoc x uf)))
                       (if (not entry)
                           x
                           (if (equal? x (cdr entry))
                               x
                               (uf-find uf (cdr entry))))))))

; uf-union: Union two elements
(uf-union (fn (uf x y)
              (let ((root-x (uf-find uf x))
                    (root-y (uf-find uf y)))
                   (if (equal? root-x root-y)
                       uf
                       (assoc-set root-x root-y uf)))))

; uf-connected?: Check if two elements are in same set
(uf-connected? (fn (uf x y)
                   (equal? (uf-find uf x) (uf-find uf y))))

; ============================================
; Maybe/Option Extensions
; Using (just value) and nothing
; ============================================

; Note: just, nothing, just?, nothing?, from-just are already defined earlier
; These would shadow them with incompatible representations, so removed.

; maybe-fold: Catamorphism for tagged Maybe (use maybe for simple #f-based Maybe)
(maybe-fold (fn (default f m)
                (if (just? m)
                    (f (from-just m))
                    default)))

; maybe-map: Map over Maybe
; maybe-bind: Bind for Maybe monad
; maybe-or: Return first Just, or Nothing
; maybe-and: Return second if both Just, else Nothing
; from-maybe-tagged: Extract from tagged Maybe with default (use from-maybe for simple #f-based)
(from-maybe-tagged (fn (default m)
                       (if (just? m) (from-just m) default)))

; list->maybe: Empty list to Nothing, non-empty to Just first
(list->maybe (fn (lst)
                 (if (null? lst) nothing (just (car lst)))))

; maybe->list: Just to singleton, Nothing to empty
(maybe->list (fn (m)
                 (if (just? m) (list (from-just m)) '())))

; cat-maybes: Filter Nothing values, extract Just values
; map-maybe: Map and filter in one pass
; ============================================
; Either Extensions
; ============================================

; Note: left, right, left?, right?, from-left, from-right are already defined earlier
; These would shadow them with incompatible representations, so removed.

; either: Catamorphism for Either
; either-map: Map over Right
; either-map-left: Map over Left
; either-bind: Bind for Either monad
; either-bimap: Map over both sides
(either-bimap (fn (f g e)
                  (if (left? e)
                      (left (f (from-left e)))
                      (right (g (from-right e))))))

; partition-eithers: Split list of Eithers
; lefts: Extract all Left values
; rights: Extract all Right values
; ============================================
; Format String Utilities
; ============================================

; format-simple: Simple string formatting with ~a placeholders
(format-simple (fix format-simple
                    (fn (template args)
                        (if (string-empty? template)
                            ""
                            (if (< (string-length template) 2)
                                template
                                (if (if (eq? (string-ref template 0) #\~)
                                        (eq? (string-ref template 1) #\a)
                                        #f)
                                    (if (null? args)
                                        (string-append "~a" (format-simple (string-drop 2 template) args))
                                        (string-append (->string (car args))
                                                       (format-simple (string-drop 2 template) (cdr args))))
                                    (string-append (string (string-ref template 0))
                                                   (format-simple (string-drop 1 template) args))))))))

; join-with: Join list of strings with separator
(join-with (fn (sep lst)
               (if (null? lst)
                   ""
                   (foldl (fn (acc s) (string-append acc sep s))
                          (car lst)
                          (cdr lst)))))

; repeat-string: Alias for string-repeat
; ============================================
; Control Flow Extensions
; ============================================

; guard-list: Return value in list if condition true, else empty list (for list monad)
(guard-list (fn (condition value)
                (if condition (list value) '())))

; when-just: Execute function only if Just
(when-just (fn (m f)
               (if (just? m)
                   (f (from-just m))
                   nothing)))

; unless-nothing: Execute function unless Nothing
(unless-nothing when-just)

; if-let: Bind and conditionally execute
; cond-list: Build list from condition-value pairs
(cond-list (fn (pairs)
               (apply append (map (fn (pair)
                                      (if (car pair)
                                          (list (cdr pair))
                                          '()))
                                  pairs))))

; ============================================
; Comparison Utilities
; ============================================

; comparing: Create comparator from key function
; compare-by: Compare two values by key function
; chain-comparators: Chain multiple comparators
(chain-comparators (fn (comparators)
                       (fn (a b)
                           (let ((find-nonzero (fix find-nonzero
                                                    (fn (comps)
                                                        (if (null? comps)
                                                            0
                                                            (let ((result ((car comps) a b)))
                                                                 (if (= result 0)
                                                                     (find-nonzero (cdr comps))
                                                                     result)))))))
                                (find-nonzero comparators)))))

; sort-by: Sort by key function
; sort-on: Alias for sort-by
(sort-on sort-by)

; group-consecutive-by: Group consecutive elements by predicate
(group-consecutive-by (fix group-consecutive-by
                           (fn (eq-fn lst)
                               (if (null? lst)
                                   '()
                                   (let ((x (car lst)))
                                        (let ((split-result (span (fn (y) (eq-fn x y)) lst)))
                                             (cons (car split-result)
                                                   (group-consecutive-by eq-fn (cdr split-result)))))))))

; ============================================
; Numeric Predicates and Utilities
; ============================================

; divisible-by?: Check if n is divisible by d
(divisible-by? (fn (n d)
                   (= 0 (mod n d))))

; perfect-square?: Check if n is a perfect square
(perfect-square? (fn (n)
                     (let ((s (floor (sqrt n))))
                          (= n (* s s)))))

; triangular-number: nth triangular number
(triangular-number (fn (n)
                       (/ (* n (+ n 1)) 2)))

; is-triangular?: Check if n is triangular
; pentagonal-number: nth pentagonal number
(pentagonal-number (fn (n)
                       (/ (* n (- (* 3 n) 1)) 2)))

; hexagonal-number: nth hexagonal number
(hexagonal-number (fn (n)
                      (* n (- (* 2 n) 1))))

; collatz-next: Next number in Collatz sequence
(collatz-next (fn (n)
                  (if (even? n)
                      (/ n 2)
                      (+ (* 3 n) 1))))

; collatz-sequence: Generate Collatz sequence until 1
(collatz-sequence (fix collatz-sequence
                       (fn (n)
                           (if (= n 1)
                               (list 1)
                               (cons n (collatz-sequence (collatz-next n)))))))

; ============================================
; Lazy Streams (thunked lists)
; Stream = () | (head . thunk-for-tail)
; ============================================

; stream-empty: Empty stream
(stream-empty '())

; stream-empty?: Check if stream is empty
(stream-empty? (fn (s) (null? s)))

; stream-cons: Construct stream with head and thunk for tail
; stream-head: Get first element
; stream-tail: Force and get tail
; stream-take: Take first n elements as list
; stream-drop: Drop first n elements
(stream-drop (fix drop-rec
                  (fn (n s)
                      (if (or (= n 0) (stream-empty? s))
                          s
                          (drop-rec (- n 1) (stream-tail s))))))

; stream-map: Map function over stream
; stream-filter: Filter stream
; stream-from: Infinite stream starting at n
; stream-iterate: Infinite stream by iterating function
; stream-repeat: Infinite stream of same value
; stream-cycle: Cycle through list infinitely
(stream-cycle (fn (lst)
                  (let ((go (fix go
                                 (fn (remaining)
                                     (if (null? remaining)
                                         (go lst)
                                         (stream-cons (car remaining)
                                                      (fn () (go (cdr remaining)))))))))
                       (go lst))))

; stream-zip-with: Zip two streams with function
; stream-take-while: Take while predicate holds
(stream-take-while (fix take-while-rec
                        (fn (pred-fn s)
                            (if (stream-empty? s)
                                '()
                                (if (pred-fn (stream-head s))
                                    (cons (stream-head s)
                                          (take-while-rec pred-fn (stream-tail s)))
                                    '())))))

; list->stream: Convert list to stream
(list->stream (fix list->stream-rec
                   (fn (lst)
                       (if (null? lst)
                           stream-empty
                           (stream-cons (car lst)
                                        (fn () (list->stream-rec (cdr lst))))))))

; naturals: Stream of natural numbers starting at 0
; ============================================
; Graph Algorithms (adjacency list representation)
; Graph = alist of (node . list-of-neighbors)
; ============================================

; graph-nodes: Get all nodes in graph
(graph-nodes alist-keys)

; graph-in-degree: Count incoming edges
; graph-out-degree: Count outgoing edges
; topological-sort: Kahn's algorithm for DAG
(topological-sort (fn (g)
                      (let ((nodes (graph-nodes g)))
                           (let ((in-degrees (map (fn (n) (cons n (graph-in-degree g n))) nodes)))
                                (let ((sort-loop (fix sort-loop
                                                      (fn (result remaining in-degs)
                                                          (if (null? remaining)
                                                              (reverse result)
                                                              (let ((zero-nodes (filter (fn (n)
                                                                                            (let ((d (assoc n in-degs)))
                                                                                                 (if d (= (cdr d) 0) #f)))
                                                                                        remaining)))
                                                                   (if (null? zero-nodes)
                                                                       '()  ; cycle detected
                                                                       (let ((node (car zero-nodes)))
                                                                            (let ((new-remaining (filter (fn (n) (not (equal? n node))) remaining)))
                                                                                 (let ((new-degs (foldl (fn (degs neighbor)
                                                                                                            (alist-update neighbor (fn (d) (- d 1)) degs))
                                                                                                        (alist-update node (fn (_) -1) in-degs)
                                                                                                        (graph-neighbors g node))))
                                                                                      (sort-loop (cons node result) new-remaining new-degs)))))))))))
                                     (sort-loop '() nodes in-degrees))))))

; graph-transpose: Reverse all edges
; graph-reachable: All nodes reachable from start (BFS)
; graph-path-exists?: Check if path exists between two nodes
; graph-shortest-path: BFS shortest path (unweighted)
(graph-shortest-path (fn (g from to)
                         (let ((bfs (fix bfs
                                         (fn (queue visited)
                                             (if (null? queue)
                                                 #f
                                                 (let ((path (car queue))
                                                       (rest (cdr queue)))
                                                      (let ((node (car path)))
                                                           (if (equal? node to)
                                                               (reverse path)
                                                               (if (member node visited)
                                                                   (bfs rest visited)
                                                                   (let ((neighbors (graph-neighbors g node)))
                                                                        (let ((new-paths (map (fn (n) (cons n path)) neighbors)))
                                                                             (bfs (append rest new-paths)
                                                                                  (cons node visited)))))))))))))
                              (bfs (list (list from)) '()))))

; ============================================
; Numeric Methods
; ============================================

; newton-raphson: Find root using Newton's method
; f is the function, df is its derivative
(newton-raphson (fn (f df x0 tolerance max-iter)
                    (let ((iterate (fix iterate
                                        (fn (x iter)
                                            (if (= iter 0)
                                                x
                                                (let ((fx (f x))
                                                      (dfx (df x)))
                                                     (if (< (abs fx) tolerance)
                                                         x
                                                         (if (= dfx 0)
                                                             x  ; avoid division by zero
                                                             (iterate (- x (/ fx dfx)) (- iter 1))))))))))
                         (iterate x0 max-iter))))

; bisection: Find root using bisection method
(bisection (fn (f a b tolerance max-iter)
               (let ((iterate (fix iterate
                                   (fn (lo hi iter)
                                       (if (= iter 0)
                                           (/ (+ lo hi) 2)
                                           (let ((mid (/ (+ lo hi) 2)))
                                                (if (< (abs (- hi lo)) tolerance)
                                                    mid
                                                    (if (< (* (f lo) (f mid)) 0)
                                                        (iterate lo mid (- iter 1))
                                                        (iterate mid hi (- iter 1))))))))))
                    (iterate a b max-iter))))

; fixed-point: Find fixed point of function
(fixed-point (fn (f x0 tolerance max-iter)
                 (let ((iterate (fix iterate
                                     (fn (x iter)
                                         (if (= iter 0)
                                             x
                                             (let ((next (f x)))
                                                  (if (< (abs (- next x)) tolerance)
                                                      next
                                                      (iterate next (- iter 1)))))))))
                      (iterate x0 max-iter))))

; numerical-derivative: Approximate derivative at point
(numerical-derivative (fn (f x h)
                          (/ (- (f (+ x h)) (f (- x h))) (* 2 h))))

; numerical-integral: Trapezoidal rule integration
(numerical-integral (fn (f a b n)
                        (let ((h (/ (- b a) n)))
                             (let ((sum (fix sum
                                             (fn (i acc)
                                                 (if (> i n)
                                                     acc
                                                     (let ((x (+ a (* i h))))
                                                          (let ((coeff (if (or (= i 0) (= i n)) 1 2)))
                                                               (sum (+ i 1) (+ acc (* coeff (f x)))))))))))
                                  (* (/ h 2) (sum 0 0))))))

; secant-method: Find root using secant method
(secant-method (fn (f x0 x1 tolerance max-iter)
                   (let ((iterate (fix iterate
                                       (fn (xn-1 xn iter)
                                           (if (= iter 0)
                                               xn
                                               (let ((fxn-1 (f xn-1))
                                                     (fxn (f xn)))
                                                    (if (< (abs fxn) tolerance)
                                                        xn
                                                        (if (= fxn fxn-1)
                                                            xn
                                                            (let ((xn+1 (- xn (* fxn (/ (- xn xn-1) (- fxn fxn-1))))))
                                                                 (iterate xn xn+1 (- iter 1)))))))))))
                        (iterate x0 x1 max-iter))))

; ============================================
; Trie Data Structure (for string keys)
; Trie = (value . children-alist)
; ============================================

; trie-empty: Create empty trie
(trie-empty (cons #f '()))

; trie-value: Get value at trie node
(trie-value car)

; trie-children: Get children alist
(trie-children cdr)

; trie-insert: Insert key-value pair
(trie-insert (fix trie-insert
                  (fn (trie key value)
                      (if (null? key)
                          (cons value (trie-children trie))
                          (let ((c (car key))
                                (rest (cdr key))
                                (children (trie-children trie)))
                               (let ((child (assoc c children)))
                                    (if child
                                        (cons (trie-value trie)
                                              (assoc-set c (trie-insert (cdr child) rest value) children))
                                        (cons (trie-value trie)
                                              (cons (cons c (trie-insert trie-empty rest value)) children)))))))))

; trie-lookup: Look up key in trie
(trie-lookup (fix trie-lookup
                  (fn (trie key)
                      (if (null? key)
                          (trie-value trie)
                          (let ((c (car key))
                                (rest (cdr key))
                                (children (trie-children trie)))
                               (let ((child (assoc c children)))
                                    (if child
                                        (trie-lookup (cdr child) rest)
                                        #f)))))))

; trie-has-key?: Check if key exists
(trie-has-key? (fn (trie key)
                   (not (not (trie-lookup trie key)))))

; string->key: Convert string to key (list of chars)
(string->key string->list)

; trie-insert-string: Insert with string key
(trie-insert-string (fn (trie str value)
                        (trie-insert trie (string->key str) value)))

; trie-lookup-string: Lookup with string key
(trie-lookup-string (fn (trie str)
                        (trie-lookup trie (string->key str))))

; ============================================
; More Combinatorics
; ============================================

; permutations-k: k-permutations of list
(permutations-k (fn (k lst)
                    (if (= k 0)
                        (list '())
                        (if (null? lst)
                            '()
                            (apply append
                                   (map (fn (x)
                                            (map (fn (p) (cons x p))
                                                 (permutations-k (- k 1) (remove-first x lst))))
                                        lst))))))

; derangements: Permutations with no fixed points
(derangements (fn (lst)
                  (let ((indexed (map-indexed (fn (i x) (cons i x)) lst)))
                       (filter (fn (perm)
                                   (all (fn (pair)
                                            (not (equal? (car pair) (cdr pair))))
                                        (zip (map car indexed) perm)))
                               (permutations lst)))))

; count-derangements: Count derangements (subfactorial)
(count-derangements (fix count-derangements
                         (fn (n)
                             (if (= n 0)
                                 1
                                 (if (= n 1)
                                     0
                                     (* (- n 1) (+ (count-derangements (- n 1))
                                                   (count-derangements (- n 2)))))))))

; stirling-second: Stirling number of second kind S(n,k)
(stirling-second (fix stirling-second
                      (fn (n k)
                          (if (or (= k 0) (> k n))
                              0
                              (if (or (= k 1) (= k n))
                                  1
                                  (+ (* k (stirling-second (- n 1) k))
                                     (stirling-second (- n 1) (- k 1))))))))

; bell-number: nth Bell number (partitions of n-set)
(bell-number (fn (n)
                 (sum-list (map (fn (k) (stirling-second n k))
                                (range 0 (+ n 1))))))

; catalan-number: nth Catalan number
(catalan-number (fn (n)
                    (/ (binomial (* 2 n) n) (+ n 1))))

; partitions-integer: Integer partitions of n
(partitions-integer (fix partitions-integer
                         (fn (n max-part)
                             (if (= n 0)
                                 (list '())
                                 (if (or (< n 1) (< max-part 1))
                                     '()
                                     (append (map (fn (p) (cons max-part p))
                                                  (partitions-integer (- n max-part) max-part))
                                             (partitions-integer n (- max-part 1))))))))

; count-partitions: Count integer partitions
(count-partitions (fn (n)
                      (length (partitions-integer n n))))

; ============================================
; Bitwise Utilities
; ============================================

; bit-set?: Check if bit at position is set
(bit-set? (fn (n pos)
              (not (= 0 (bitand n (shl 1 pos))))))

; bit-set: Set bit at position
(bit-set (fn (n pos)
             (bitor n (shl 1 pos))))

; bit-clear: Clear bit at position
(bit-clear (fn (n pos)
               (bitand n (bitnot (shl 1 pos)))))

; bit-toggle: Toggle bit at position
(bit-toggle (fn (n pos)
                (bitxor n (shl 1 pos))))

; bit-count: Count set bits (popcount)
(bit-count (fix bit-count
                (fn (n)
                    (if (= n 0)
                        0
                        (+ (bitand n 1) (bit-count (shr n 1)))))))

; lowest-set-bit: Position of lowest set bit (-1 if none)
(lowest-set-bit (fix lowest-set-bit
                     (fn (n pos)
                         (if (= n 0)
                             -1
                             (if (= (bitand n 1) 1)
                                 pos
                                 (lowest-set-bit (shr n 1) (+ pos 1)))))))

; highest-set-bit: Position of highest set bit (-1 if none)
(highest-set-bit (fn (n)
                     (let ((find (fix find
                                      (fn (n pos highest)
                                          (if (= n 0)
                                              highest
                                              (find (shr n 1) (+ pos 1)
                                                    (if (= (bitand n 1) 1) pos highest)))))))
                          (find n 0 -1))))

; ============================================
; Ring Buffer (fixed-size circular buffer)
; Buffer = (capacity read-idx write-idx data-list)
; ============================================

; ring-buffer-new: Create empty ring buffer
(ring-buffer-new (fn (capacity)
                     (list capacity 0 0 (replicate capacity #f))))

; ring-buffer-capacity: Get capacity
(ring-buffer-capacity (fn (rb) (car rb)))

; ring-buffer-push: Add element (overwrites oldest if full)
(ring-buffer-push (fn (rb item)
                      (let ((cap (car rb))
                            (read-idx (car (cdr rb)))
                            (write-idx (car (cdr (cdr rb))))
                            (data (car (cdr (cdr (cdr rb))))))
                           (let ((new-data (set-at (mod write-idx cap) item data))
                                 (new-write (mod (+ write-idx 1) cap)))
                                (list cap read-idx new-write new-data)))))

; ring-buffer-peek: Get oldest element
(ring-buffer-peek (fn (rb)
                      (let ((cap (car rb))
                            (read-idx (car (cdr rb)))
                            (data (car (cdr (cdr (cdr rb))))))
                           (list-ref data read-idx))))

; ring-buffer-pop: Remove and return oldest element
(ring-buffer-pop (fn (rb)
                     (let ((cap (car rb))
                           (read-idx (car (cdr rb)))
                           (write-idx (car (cdr (cdr rb))))
                           (data (car (cdr (cdr (cdr rb))))))
                          (if (= read-idx write-idx)
                              rb  ; empty
                              (let ((item (list-ref data read-idx))
                                    (new-read (mod (+ read-idx 1) cap)))
                                   (cons item (list cap new-read write-idx data)))))))

; ============================================
; S-Expression Utilities
; ============================================

; sexp-atom?: Check if value is an atom (not a pair)
(sexp-atom? (fn (x)
                (not (pair? x))))

; sexp-list?: Check if value is a proper list
(sexp-list? (fix sexp-list?-rec
                 (fn (x)
                     (if (null? x)
                         #t
                         (if (pair? x)
                             (sexp-list?-rec (cdr x))
                             #f)))))

; sexp-dotted?: Check if value is a dotted list
(sexp-dotted? (fix sexp-dotted?-rec
                   (fn (x)
                       (if (null? x)
                           #f
                           (if (pair? x)
                               (sexp-dotted?-rec (cdr x))
                               #t)))))

; sexp-length: Length of s-expression (for proper lists)
(sexp-length (fix sexp-length-rec
                  (fn (x)
                      (if (null? x)
                          0
                          (if (pair? x)
                              (+ 1 (sexp-length-rec (cdr x)))
                              0)))))

; sexp-depth: Maximum nesting depth
(sexp-depth (fix sexp-depth-rec
                 (fn (x)
                     (if (pair? x)
                         (+ 1 (max (sexp-depth-rec (car x))
                                   (sexp-depth-rec (cdr x))))
                         0))))

; sexp-count-atoms: Count all atoms in s-expression
(sexp-count-atoms (fix sexp-count-atoms-rec
                       (fn (x)
                           (if (null? x)
                               0
                               (if (pair? x)
                                   (+ (sexp-count-atoms-rec (car x))
                                      (sexp-count-atoms-rec (cdr x)))
                                   1)))))

; sexp-flatten: Flatten nested structure to list of atoms
(sexp-flatten (fix sexp-flatten-rec
                   (fn (x)
                       (if (null? x)
                           '()
                           (if (pair? x)
                               (append (sexp-flatten-rec (car x))
                                       (sexp-flatten-rec (cdr x)))
                               (list x))))))

; sexp-map: Map over all atoms in s-expression
(sexp-map (fix sexp-map-rec
               (fn (f x)
                   (if (null? x)
                       '()
                       (if (pair? x)
                           (cons (sexp-map-rec f (car x))
                                 (sexp-map-rec f (cdr x)))
                           (f x))))))

; sexp-filter: Keep only atoms satisfying predicate
(sexp-filter (fix sexp-filter-rec
                  (fn (pred-fn x)
                      (if (null? x)
                          '()
                          (if (pair? x)
                              (let ((filtered-car (sexp-filter-rec pred-fn (car x)))
                                    (filtered-cdr (sexp-filter-rec pred-fn (cdr x))))
                                   (if (and (null? filtered-car) (not (pair? (car x))))
                                       filtered-cdr
                                       (cons filtered-car filtered-cdr)))
                              (if (pred-fn x) (list x) '()))))))

; sexp-find: Find first atom satisfying predicate
(sexp-find (fix sexp-find-rec
                (fn (pred-fn x)
                    (if (null? x)
                        #f
                        (if (pair? x)
                            (let ((found-car (sexp-find-rec pred-fn (car x))))
                                 (if found-car
                                     found-car
                                     (sexp-find-rec pred-fn (cdr x))))
                            (if (pred-fn x) x #f))))))

; sexp-substitute: Replace all occurrences of old with new
(sexp-substitute (fix sexp-substitute-rec
                      (fn (old new x)
                          (if (equal? x old)
                              new
                              (if (pair? x)
                                  (cons (sexp-substitute-rec old new (car x))
                                        (sexp-substitute-rec old new (cdr x)))
                                  x)))))

; sexp-contains?: Check if s-expression contains value
(sexp-contains? (fix sexp-contains?-rec
                     (fn (val x)
                         (if (equal? x val)
                             #t
                             (if (pair? x)
                                 (or (sexp-contains?-rec val (car x))
                                     (sexp-contains?-rec val (cdr x)))
                                 #f)))))

; ============================================
; Transducers (composable sequence transformations)
; Pure, stateless transducers
; ============================================

; t-map: Mapping transducer
(t-map (fn (f)
           (fn (reducer)
               (fn (acc x)
                   (reducer acc (f x))))))

; t-filter: Filtering transducer
(t-filter (fn (pred-fn)
              (fn (reducer)
                  (fn (acc x)
                      (if (pred-fn x)
                          (reducer acc x)
                          acc)))))

; t-cat: Concatenating transducer
(t-cat (fn ()
           (fn (reducer)
               (fn (acc xs)
                   (foldl reducer acc xs)))))

; t-mapcat: Map then concatenate
(t-mapcat (fn (f)
              (t-comp (t-map f) (t-cat))))

; t-keep: Keep non-false results
(t-keep (fn (f)
            (fn (reducer)
                (fn (acc x)
                    (let ((result (f x)))
                         (if result
                             (reducer acc result)
                             acc))))))

; transduce: Apply transducer to collection
(transduce (fn (xform reducer init coll)
               (foldl (xform reducer) init coll)))

; into-list: Transduce into a list
(into-list (fn (xform coll)
               (reverse (transduce xform (fn (acc x) (cons x acc)) '() coll))))

; t-comp: Compose transducers (right to left)
(t-comp (fn (t1 t2)
            (fn (reducer)
                (t1 (t2 reducer)))))

; t-comp3: Compose three transducers
(t-comp3 (fn (t1 t2 t3)
             (t-comp t1 (t-comp t2 t3))))

; ============================================
; Memo Table Utilities (pure, immutable)
; ============================================

; memo-table-new: Create a new memo table (alist-based)
(memo-table-new (fn () '()))

; memo-table-get: Get value from memo table
(memo-table-get (fn (table key)
                    (assoc key table)))

; memo-table-put: Add value to memo table (returns new table)
(memo-table-put (fn (table key value)
                    (cons (cons key value) table)))

; memo-table-contains?: Check if key exists
(memo-table-contains? (fn (table key)
                          (if (assoc key table) #t #f)))

; memo-table-remove: Remove key from table
(memo-table-remove (fn (table key)
                       (filter (fn (entry) (not (equal? (car entry) key))) table)))

; memo-table-keys: Get all keys
(memo-table-keys (fn (table)
                     (map car table)))

; memo-table-values: Get all values
(memo-table-values (fn (table)
                       (map cdr table)))

; ============================================
; Algebraic Structures
; ============================================

; Monoid: (mempty, mappend)
; make-monoid: Create a monoid structure
(make-monoid (fn (empty append-fn)
                 (list 'monoid empty append-fn)))

; monoid-empty: Get identity element
(monoid-empty (fn (m) (cadr m)))

; monoid-append: Get append operation
(monoid-append (fn (m) (caddr m)))

; mconcat: Fold with monoid
(mconcat (fn (monoid xs)
             (foldl (monoid-append monoid) (monoid-empty monoid) xs)))

; Common monoids
(sum-monoid (make-monoid 0 +))
(product-monoid (make-monoid 1 *))
(list-monoid (make-monoid '() append))
(string-monoid (make-monoid "" string-append))
(all-monoid (make-monoid #t (fn (a b) (and a b))))
(any-monoid (make-monoid #f (fn (a b) (or a b))))
(max-monoid (make-monoid -999999999 max))
(min-monoid (make-monoid 999999999 min))

; first-monoid: Returns first non-nothing value
(first-monoid (make-monoid nothing
                           (fn (a b)
                               (if (nothing? a) b a))))

; last-monoid: Returns last non-nothing value
(last-monoid (make-monoid nothing
                          (fn (a b)
                              (if (nothing? b) a b))))

; endo-monoid: Endomorphism monoid (function composition)
(endo-monoid (make-monoid id compose))

; dual: Dual of a monoid (reverses append order)
(dual-monoid (fn (m)
                 (make-monoid
                  (monoid-empty m)
                  (fn (a b) ((monoid-append m) b a)))))

; ============================================
; Foldable Pattern
; ============================================

; fold-map: Map then fold with monoid
(fold-map (fn (monoid f xs)
              (mconcat monoid (map f xs))))

; fold-sum: Sum using monoid
(fold-sum (fn (xs)
              (mconcat sum-monoid xs)))

; fold-product: Product using monoid
(fold-product (fn (xs)
                  (mconcat product-monoid xs)))

; fold-all: All using monoid
(fold-all (fn (xs)
              (mconcat all-monoid xs)))

; fold-any: Any using monoid
(fold-any (fn (xs)
              (mconcat any-monoid xs)))

; ============================================
; Semigroup (monoid without identity)
; ============================================

; make-semigroup: Create semigroup
(make-semigroup (fn (append-fn)
                    (list 'semigroup append-fn)))

; semigroup-append: Get append operation
(semigroup-append (fn (s) (cadr s)))

; sconcat: Fold non-empty list with semigroup
(sconcat (fn (semigroup xs)
             (if (null? xs)
                 (error "sconcat requires non-empty list")
                 (foldl (semigroup-append semigroup) (car xs) (cdr xs)))))

; ============================================
; Functor Pattern
; ============================================

; make-functor: Create functor structure
(make-functor (fn (fmap-fn)
                  (list 'functor fmap-fn)))

; functor-map: Get the fmap operation
(functor-map (fn (f) (cadr f)))

; Common functors
(list-functor (make-functor map))
(maybe-functor (make-functor maybe-map))
(either-functor (make-functor either-map))

; ============================================
; Applicative Pattern
; ============================================

; make-applicative: Create applicative structure
(make-applicative (fn (pure-fn ap-fn)
                      (list 'applicative pure-fn ap-fn)))

; applicative-pure: Get pure operation
(applicative-pure (fn (a) (cadr a)))

; applicative-ap: Get apply operation
(applicative-ap (fn (a) (caddr a)))

; liftA2: Lift binary function to applicative
(liftA2 (fn (applicative f fa fb)
            (let ((ap (applicative-ap applicative)))
                 (ap (ap ((applicative-pure applicative) f) fa) fb))))

; ============================================
; Comonad Pattern
; ============================================

; make-comonad: Create comonad structure
(make-comonad (fn (extract-fn extend-fn)
                  (list 'comonad extract-fn extend-fn)))

; comonad-extract: Get extract operation
(comonad-extract (fn (c) (cadr c)))

; comonad-extend: Get extend operation
(comonad-extend (fn (c) (caddr c)))

; ============================================
; Zipper as Comonad
; ============================================

; list-zipper-comonad: Comonad instance for list zipper
(list-zipper-extract (fn (z)
                         (zipper-focus z)))

(list-zipper-extend (fn (f z)
                        (let ((go-left (fix go-left-rec
                                            (fn (z acc)
                                                (let ((moved (zipper-left z)))
                                                     (if moved
                                                         (go-left-rec moved (cons (f moved) acc))
                                                         acc)))))
                              (go-right (fix go-right-rec
                                             (fn (z acc)
                                                 (let ((moved (zipper-right z)))
                                                      (if moved
                                                          (go-right-rec moved (cons (f moved) acc))
                                                          acc))))))
                             (let ((lefts (go-left z '()))
                                   (focus (f z))
                                   (rights (reverse (go-right z '()))))
                                  (list lefts focus rights)))))

; ============================================
; Bifunctor Pattern
; ============================================

; bimap: Map over both components
(bimap (fn (f g x)
           (if (pair? x)
               (cons (f (car x)) (g (cdr x)))
               x)))

; first-fn: Map over first component only
(first-fn (fn (f x)
              (bimap f id x)))

; second-fn: Map over second component only
(second-fn (fn (g x)
               (bimap id g x)))

; ============================================
; Profunctor Pattern (for functions)
; ============================================

; dimap: Contravariant on input, covariant on output
(dimap (fn (f g h)
           (compose g (compose h f))))

; lmap: Contravariant map on input
(lmap (fn (f h)
          (compose h f)))

; rmap: Covariant map on output
(rmap (fn (g h)
          (compose g h)))

; ============================================
; Kleisli Composition
; ============================================

; kleisli-maybe: Compose Maybe-returning functions
(kleisli-maybe (fn (f g)
                   (fn (x)
                       (let ((result (f x)))
                            (if (nothing? result)
                                nothing
                                (g (from-just result)))))))

; kleisli-either: Compose Either-returning functions
(kleisli-either (fn (f g)
                    (fn (x)
                        (let ((result (f x)))
                             (if (left? result)
                                 result
                                 (g (from-right result)))))))

; kleisli-list: Compose list-returning functions (flatMap)
(kleisli-list (fn (f g)
                  (fn (x)
                      (flat-map g (f x)))))

; ============================================
; Arrow Operations (extended)
; ============================================

; arr-id: Identity arrow
(arr-id id)

; arr-compose: Compose arrows
(arr-compose compose)

; arr-first: Apply to first of pair
(arr-first (fn (f)
               (fn (pair)
                   (cons (f (car pair)) (cdr pair)))))

; arr-second: Apply to second of pair
(arr-second (fn (f)
                (fn (pair)
                    (cons (car pair) (f (cdr pair))))))

; arr-split: Parallel composition
(arr-split (fn (f g)
               (fn (pair)
                   (cons (f (car pair)) (g (cdr pair))))))

; arr-fanout: Duplicate and apply
(arr-fanout (fn (f g)
                (fn (x)
                    (cons (f x) (g x)))))

; arr-choice: Either arrow
(arr-choice (fn (f g)
                (fn (x)
                    (if (left? x)
                        (left (f (from-left x)))
                        (right (g (from-right x)))))))

; ============================================
; Recursive Schemes (simple versions)
; ============================================

; cata: Catamorphism (fold)
(cata (fn (algebra expr)
          (algebra (sexp-map (fn (x) (cata algebra x)) expr))))

; ana: Anamorphism (unfold)
(ana (fix ana-rec
          (fn (coalgebra seed)
              (let ((result (coalgebra seed)))
                   (if (pair? result)
                       (cons (ana-rec coalgebra (car result))
                             (ana-rec coalgebra (cdr result)))
                       result)))))

; hylo: Hylomorphism (unfold then fold)
(hylo (fn (algebra coalgebra seed)
          (cata algebra (ana coalgebra seed))))

; para: Paramorphism (fold with access to original structure)
(para (fix para-rec
           (fn (algebra expr)
               (if (pair? expr)
                   (algebra (cons (cons (car expr) (para-rec algebra (car expr)))
                                  (cons (cdr expr) (para-rec algebra (cdr expr)))))
                   (algebra expr)))))

; ============================================
; Expression Utilities
; ============================================

; quoted?: Check if expression is quoted
(quoted? (fn (x)
             (if (pair? x)
                 (eq? (car x) 'quote)
                 #f)))

; unquote-expr: Remove quote wrapper
(unquote-expr (fn (x)
                  (if (quoted? x)
                      (cadr x)
                      x)))

; make-quote: Wrap expression in quote
(make-quote (fn (x)
                (list 'quote x)))

; lambda?: Check if expression is lambda
(lambda? (fn (x)
             (if (pair? x)
                 (or (eq? (car x) 'fn)
                     (eq? (car x) 'lambda))
                 #f)))

; application?: Check if expression is application
(application? (fn (x)
                  (if (pair? x)
                      (not (or (quoted? x)
                               (lambda? x)
                               (eq? (car x) 'if)
                               (eq? (car x) 'let)
                               (eq? (car x) 'let*)))
                      #f)))

; if?: Check if expression is conditional
(if? (fn (x)
         (if (pair? x)
             (eq? (car x) 'if)
             #f)))

; let?: Check if expression is let
(let? (fn (x)
          (if (pair? x)
              (or (eq? (car x) 'let)
                  (eq? (car x) 'let*))
              #f)))

; ============================================
; Symbol Utilities
; ============================================

; symbol-append: Concatenate symbols
(symbol-append (fn (s1 s2)
                   (string->symbol (string-append (symbol->string s1)
                                                  (symbol->string s2)))))

; make-gensym: Create a gensym generator (returns pair of next-sym and new-generator)
(make-gensym (fn (prefix counter)
                 (cons (string->symbol (string-append (symbol->string prefix)
                                                      (number->string counter)))
                       (+ counter 1))))

; ============================================
; Control Flow Utilities (pure)
; ============================================

; do-while: Execute while condition holds
(do-while (fix do-while-rec
               (fn (test-fn body-fn state)
                   (if (test-fn state)
                       (do-while-rec test-fn body-fn (body-fn state))
                       state))))

; do-until: Execute until condition holds
(do-until (fn (test-fn body-fn state)
              (do-while (fn (s) (not (test-fn s))) body-fn state)))

; iterate-times: Apply function n times
(iterate-times (fix iterate-times-rec
                    (fn (n f x)
                        (if (<= n 0)
                            x
                            (iterate-times-rec (- n 1) f (f x))))))

; find-fixed-point: Find fixed point with max iterations
(find-fixed-point (fix find-fixed-point-rec
                       (fn (f x max-iter)
                           (if (<= max-iter 0)
                               x
                               (let ((next (f x)))
                                    (if (equal? x next)
                                        x
                                        (find-fixed-point-rec f next (- max-iter 1))))))))

; ============================================
; String Manipulation Utilities
; ============================================

; string-null?: Check if string is empty
; string-pad: Pad string on left to width
; string-trim-left: Remove leading chars matching predicate
; string-trim-right: Remove trailing chars matching predicate
; string-trim: Remove chars from both ends
; string-trim-whitespace: Trim whitespace from both ends
(string-trim-whitespace (fn (s)
                            (string-trim (fn (c) (or (char=? c #\space)
                                                     (char=? c #\tab)
                                                     (char=? c #\newline)))
                                         s)))

; string-contains?: Check if s1 contains s2
; string-prefix?: Check if s2 is prefix of s1
; string-suffix?: Check if s2 is suffix of s1
; string-split-at: Split string at index
; string-replace-first: Replace first occurrence of old with new
; string-replace-all: Replace all occurrences of old with new
; string-join: Join list of strings with separator
(string-join (fix join-rec
                  (fn (lst sep)
                      (if (null? lst)
                          ""
                          (if (null? (cdr lst))
                              (car lst)
                              (string-append (car lst)
                                             (string-append sep (join-rec (cdr lst) sep))))))))

; string-split-char: Split string by delimiter character
; string-upcase: Convert string to uppercase
(string-upcase (fn (s)
                   (list->string (map char-upcase (string->list s)))))

; string-downcase: Convert string to lowercase
(string-downcase (fn (s)
                     (list->string (map char-downcase (string->list s)))))

; string-titlecase: Capitalize first letter of each word
(string-titlecase (fn (s)
                      (string-join (map (fn (word)
                                            (if (string-null? word)
                                                word
                                                (string-append (string-upcase (string-take 1 word))
                                                               (string-downcase (string-drop 1 word)))))
                                        (string-split s " "))
                                   " ")))

; string-count: Count occurrences of char in string
; string-index: Find first index of char (or -1 if not found)
(string-index (fn (s c)
                  ((fix index-rec
                        (fn (chars i)
                            (if (null? chars)
                                -1
                                (if (char=? (car chars) c)
                                    i
                                    (index-rec (cdr chars) (+ i 1))))))
                   (string->list s) 0)))

; string-index-right: Find last index of char (or -1 if not found)
(string-index-right (fn (s c)
                        ((fix index-rec
                              (fn (chars i found)
                                  (if (null? chars)
                                      found
                                      (if (char=? (car chars) c)
                                          (index-rec (cdr chars) (+ i 1) i)
                                          (index-rec (cdr chars) (+ i 1) found)))))
                         (string->list s) 0 -1)))

; ============================================
; Association List Utilities
; ============================================

; assoc: Find pair by key using equal?
(assoc (fix assoc-rec
            (fn (key alist)
                (if (null? alist)
                    #f
                    (if (equal? key (caar alist))
                        (car alist)
                        (assoc-rec key (cdr alist)))))))

; assq: Find pair by key using eq?
(assq (fix assq-rec
           (fn (key alist)
               (if (null? alist)
                   #f
                   (if (eq? key (caar alist))
                       (car alist)
                       (assq-rec key (cdr alist)))))))

; assv: Find pair by key using eqv?
; alist-ref: Get value for key (with optional default)
(alist-ref (fn (key alist default)
               (let ((pair (assoc key alist)))
                    (if pair (cdr pair) default))))

; alist-set: Set or update key-value pair
; alist-delete: Remove all pairs with key
(alist-delete (fn (key alist)
                  (filter (fn (pair) (not (equal? key (car pair)))) alist)))

; alist-keys: Get all keys
; alist-values: Get all values
; alist->list: Convert alist to list of key-value lists
(alist->list (fn (alist)
                 (map (fn (pair) (list (car pair) (cdr pair))) alist)))

; list->alist: Convert list of key-value lists to alist
(list->alist (fn (lst)
                 (map (fn (kv) (cons (car kv) (cadr kv))) lst)))

; alist-has-key?: Check if key exists
(alist-has-key? (fn (key alist)
                    (not (eq? (assoc key alist) #f))))

; alist-update-default: Update value for key using function, with default if key missing
(alist-update-default (fn (key f default alist)
                          (let ((old-val (alist-ref key alist default)))
                               (alist-set key (f old-val) alist))))

; ============================================
; Set Operations (using sorted lists)
; ============================================

; set-empty: Empty set
(set-empty '())

; set-singleton: Create single-element set
(set-singleton (fn (x) (list x)))

; set-member?: Check membership
; set-insert: Insert element (maintains uniqueness)
(set-insert (fn (x s)
                (if (set-member? x s)
                    s
                    (cons x s))))

; set-delete: Remove element
(set-delete (fn (x s)
                (filter (fn (y) (not (equal? x y))) s)))

; set-disjoint?: Check if sets have no common elements
; set-size: Number of elements
; set->list: Convert to list
(set->list (fn (s) s))

; list->set: Convert list to set (removes duplicates)
(list->set (fn (lst)
               (foldl (fn (acc x) (set-insert x acc)) '() lst)))

; set-filter: Filter set by predicate
(set-filter filter)

; set-map: Map over set (may introduce duplicates, so re-setify)
(set-map (fn (f s)
             (list->set (map f s))))

; set-fold: Fold over set
(set-fold foldl)

; set-partition: Partition set by predicate
(set-partition (fn (pred s)
                   (cons (filter pred s)
                         (filter (fn (x) (not (pred x))) s))))

; ============================================
; Queue (Simple list-based implementation)
; ============================================

; queue-new: Create empty queue
; queue-empty: Empty queue constant
; queue-empty?: Check if queue is empty
; queue-enqueue: Add element to back of queue (queue first, element second)
; queue-dequeue: Remove and return front element (returns (element . rest))
; queue-front: Peek at front element
; queue-size: Get queue size
; queue->list: Convert to list (identity for list-based)
(queue->list id)

; list->queue: Create queue from list (identity for list-based)
(list->queue id)

; ============================================
; Deque (Simple list-based implementation)
; ============================================

; deque-new: Create empty deque
; deque-empty?: Check if deque is empty
; deque-push-front: Add to front (deque first, then element)
; deque-push-back: Add to back (deque first, then element)
; deque-front: Peek at front
; deque-back: Peek at back
; deque-pop-front: Remove from front (returns (element . rest))
; deque-pop-back: Remove from back (returns (element . rest))
; deque-size: Number of elements
(deque-size length)

; deque->list: Convert to list (identity for list-based)
(deque->list id)

; list->deque: Create deque from list (identity for list-based)
(list->deque id)

; ============================================
; Sorting Algorithms
; ============================================

; insert-sorted-cmp: Insert into sorted list with custom comparator
(insert-sorted-cmp (fix insert-rec
                        (fn (x lst cmp)
                            (if (null? lst)
                                (list x)
                                (if (cmp x (car lst))
                                    (cons x lst)
                                    (cons (car lst) (insert-rec x (cdr lst) cmp)))))))

; insertion-sort-cmp: Insertion sort with comparator (use insertion-sort for default <=)
; merge-sorted-cmp: Merge two sorted lists with comparator
(merge-sorted-cmp (fix merge-rec
                       (fn (l1 l2 cmp)
                           (if (null? l1)
                               l2
                               (if (null? l2)
                                   l1
                                   (if (cmp (car l1) (car l2))
                                       (cons (car l1) (merge-rec (cdr l1) l2 cmp))
                                       (cons (car l2) (merge-rec l1 (cdr l2) cmp))))))))

; merge-sort-cmp: Merge sort with comparator (use merge-sort for default <=)
; quicksort-cmp: Quicksort with comparator
(quicksort-cmp (fix quicksort-rec
                    (fn (lst cmp)
                        (if (null? lst)
                            '()
                            (let ((pivot (car lst))
                                  (rest (cdr lst)))
                                 (append (quicksort-rec (filter (fn (x) (cmp x pivot)) rest) cmp)
                                         (cons pivot
                                               (quicksort-rec (filter (fn (x) (not (cmp x pivot))) rest) cmp))))))))

; sort: Default sort (uses merge-sort-cmp with <)
(sort (fn (lst)
          (merge-sort-cmp lst <)))

; sort-by: Sort using key function
(sort-by-key (fn (key-fn lst)
                 (map cdr
                      (merge-sort-cmp (map (fn (x) (cons (key-fn x) x)) lst)
                                      (fn (a b) (< (car a) (car b)))))))

; sort-descending: Sort in descending order
; sorted-cmp?: Check if list is sorted by comparator (use sorted? for default <=)
; minimum: Find minimum element
(minimum (fn (lst)
             (if (null? lst)
                 (error "minimum: empty list")
                 (foldl (fn (acc x) (if (< x acc) x acc)) (car lst) (cdr lst)))))

; maximum: Find maximum element
(maximum (fn (lst)
             (if (null? lst)
                 (error "maximum: empty list")
                 (foldl (fn (acc x) (if (> x acc) x acc)) (car lst) (cdr lst)))))

; minimum-by: Find minimum by key function
(minimum-by (fn (key-fn lst)
                (if (null? lst)
                    (error "minimum-by: empty list")
                    (car (foldl (fn (acc x)
                                    (let ((kx (key-fn x)))
                                         (if (< kx (cdr acc))
                                             (cons x kx)
                                             acc)))
                                (cons (car lst) (key-fn (car lst)))
                                (cdr lst))))))

; maximum-by: Find maximum by key function
(maximum-by (fn (key-fn lst)
                (if (null? lst)
                    (error "maximum-by: empty list")
                    (car (foldl (fn (acc x)
                                    (let ((kx (key-fn x)))
                                         (if (> kx (cdr acc))
                                             (cons x kx)
                                             acc)))
                                (cons (car lst) (key-fn (car lst)))
                                (cdr lst))))))

; ============================================
; Priority Queue (using sorted list - simple version)
; ============================================

; pq-empty: Create empty priority queue
(pq-empty '())

; pq-empty?: Check if empty
; pq-insert: Insert with priority (lower = higher priority)
; pq-peek: Get highest priority element
; pq-peek-priority: Get highest priority value
(pq-peek-priority (fn (pq)
                      (if (null? pq)
                          (error "pq-peek-priority: empty queue")
                          (caar pq))))

; pq-pop: Remove highest priority element
; pq-size: Number of elements
(pq-size length)

; ============================================
; Multiset / Bag (using alist for counts)
; ============================================

; bag-empty: Create empty bag
(bag-empty '())

; bag-insert: Add element (increment count)
(bag-insert (fn (x bag)
                (let ((current (alist-ref x bag 0)))
                     (alist-set x (+ current 1) bag))))

; bag-count: Get count of element
; bag-remove: Remove one occurrence
; bag-remove-all: Remove all occurrences
(bag-remove-all alist-delete)

; bag-member?: Check if element exists
(bag-member? (fn (x bag)
                 (> (bag-count x bag) 0)))

; bag-unique-elements: Get unique elements
(bag-unique-elements alist-keys)

; bag-total-count: Total number of elements (with multiplicity)
(bag-total-count (fn (bag)
                     (foldl + 0 (alist-values bag))))

; bag->list: Convert to list with repetition
(bag->list (fn (bag)
               (apply append
                      (map (fn (pair)
                               (replicate (cdr pair) (car pair)))
                           bag))))

; list->bag: Create bag from list
(list->bag (fn (lst)
               (foldl (fn (acc x) (bag-insert x acc)) '() lst)))

; bag-union: Union (max of counts)
; bag-intersection: Intersection (min of counts)
; bag-sum: Sum (add counts)
(bag-sum (fn (b1 b2)
             (foldl (fn (acc pair)
                        (alist-update (car pair)
                                      (fn (old) (+ old (cdr pair)))
                                      0
                                      acc))
                    b1
                    b2)))

; ============================================
; Random Number Generation (Linear Congruential)
; ============================================

; make-rng: Create a random number generator state from seed
; Returns: (seed . multiplier . increment . modulus)
(make-rng (fn (seed)
              (list seed 1103515245 12345 2147483648)))

; rng-next: Generate next random state and value
; Returns: (new-rng . random-value)
(rng-next (fn (rng)
              (let ((seed (car rng))
                    (a (cadr rng))
                    (c (caddr rng))
                    (m (cadddr rng)))
                   (let ((next-seed (mod (+ (* a seed) c) m)))
                        (cons (cons next-seed (cdr rng)) next-seed)))))

; rng-int: Get random integer in range [0, max)
(rng-int (fn (rng max-val)
             (let ((result (rng-next rng)))
                  (cons (car result) (mod (cdr result) max-val)))))

; rng-range: Get random integer in range [lo, hi)
(rng-range (fn (rng lo hi)
               (let ((result (rng-int rng (- hi lo))))
                    (cons (car result) (+ lo (cdr result))))))

; rng-bool: Get random boolean
(rng-bool (fn (rng)
              (let ((result (rng-int rng 2)))
                   (cons (car result) (= (cdr result) 1)))))

; rng-choice: Pick random element from list
(rng-choice (fn (rng lst)
                (let ((result (rng-int rng (length lst))))
                     (cons (car result) (list-ref lst (cdr result))))))

; rng-shuffle: Shuffle list (Fisher-Yates)
(rng-shuffle (fn (rng lst)
                 ((fix shuffle-rec
                       (fn (r remaining acc)
                           (if (null? remaining)
                               (cons r acc)
                               (let ((pick-result (rng-int r (length remaining))))
                                    (let ((new-r (car pick-result))
                                          (idx (cdr pick-result)))
                                         (shuffle-rec new-r
                                                      (remove-at idx remaining)
                                                      (cons (list-ref remaining idx) acc)))))))
                  rng lst '())))

; rng-sample: Sample n elements without replacement
(rng-sample (fn (rng n lst)
                (let ((shuffled (rng-shuffle rng lst)))
                     (cons (car shuffled) (take n (cdr shuffled))))))

; rng-take: Generate n random values using generator function
(rng-take (fn (rng n gen-fn)
              ((fix take-rec
                    (fn (r count acc)
                        (if (<= count 0)
                            (cons r (reverse acc))
                            (let ((result (gen-fn r)))
                                 (take-rec (car result) (- count 1) (cons (cdr result) acc))))))
               rng n '())))

; ============================================
; Sequence Utilities
; ============================================

; sliding-window: Generate sliding windows of size n
(sliding-window (fn (n lst)
                    ((fix window-rec
                          (fn (remaining acc)
                              (if (< (length remaining) n)
                                  (reverse acc)
                                  (window-rec (cdr remaining) (cons (take-n n remaining) acc)))))
                     lst '())))

; chunks: Split list into chunks of size n
; chunks-exact: Split into exact chunks (drop remainder)
(chunks-exact (fn (n lst)
                  (filter (fn (chunk) (= (length chunk) n)) (chunks n lst))))

; interleave-all: Interleave multiple lists
(interleave-all (fn (lists)
                    ((fix interleave-all-rec
                          (fn (lsts acc)
                              (if (all null? lsts)
                                  (reverse acc)
                                  (interleave-all-rec
                                   (map cdr (filter (fn (l) (not (null? l))) lsts))
                                   (append (reverse (filter-map (fn (l) (if (null? l) #f (car l))) lsts)) acc)))))
                     lists '())))

; partition-all: Partition into groups of n (including remainder)
(partition-all chunks)

; split-at-pred: Split list at first element satisfying predicate
(split-at-pred (fn (pred lst)
                   ((fix split-rec
                         (fn (remaining before)
                             (if (null? remaining)
                                 (cons (reverse before) '())
                                 (if (pred (car remaining))
                                     (cons (reverse before) remaining)
                                     (split-rec (cdr remaining) (cons (car remaining) before))))))
                    lst '())))

; dedupe: Remove consecutive duplicates
; dedupe-by: Remove consecutive duplicates by key function
; run-length-encode: Encode consecutive runs
; run-length-decode: Decode run-length encoding
; ============================================
; Zipper Data Structure (List Zipper)
; ============================================

; zipper-make: Create zipper from list (focus on first element)
(zipper-make (fn (lst)
                 (if (null? lst)
                     (list '() '() '())  ; (left focus right)
                     (list '() (car lst) (cdr lst)))))

; zipper-left: Elements to the left (reversed)
(zipper-left (fn (z) (car z)))

; zipper-focus: Current focused element
(zipper-focus (fn (z) (cadr z)))

; zipper-right: Elements to the right
(zipper-right (fn (z) (caddr z)))

; zipper-move-left: Move focus one position left
(zipper-move-left (fn (z)
                      (if (null? (zipper-left z))
                          z  ; Can't move left
                          (list (cdr (zipper-left z))
                                (car (zipper-left z))
                                (cons (zipper-focus z) (zipper-right z))))))

; zipper-move-right: Move focus one position right
(zipper-move-right (fn (z)
                       (if (null? (zipper-right z))
                           z  ; Can't move right
                           (list (cons (zipper-focus z) (zipper-left z))
                                 (car (zipper-right z))
                                 (cdr (zipper-right z))))))

; zipper-set: Set focused element
(zipper-set (fn (val z)
                (list (zipper-left z) val (zipper-right z))))

; zipper-modify: Modify focused element with function
(zipper-modify (fn (f z)
                   (zipper-set (f (zipper-focus z)) z)))

; zipper-insert-left: Insert element to the left of focus
(zipper-insert-left (fn (val z)
                        (list (cons val (zipper-left z)) (zipper-focus z) (zipper-right z))))

; zipper-insert-right: Insert element to the right of focus
(zipper-insert-right (fn (val z)
                         (list (zipper-left z) (zipper-focus z) (cons val (zipper-right z)))))

; zipper-delete: Delete focused element (focus moves right, or left if at end)
(zipper-delete (fn (z)
                   (if (not (null? (zipper-right z)))
                       (list (zipper-left z) (car (zipper-right z)) (cdr (zipper-right z)))
                       (if (not (null? (zipper-left z)))
                           (list (cdr (zipper-left z)) (car (zipper-left z)) '())
                           (list '() '() '())))))

; zipper-to-list: Convert zipper back to list
(zipper-to-list (fn (z)
                    (append (reverse (zipper-left z)) (cons (zipper-focus z) (zipper-right z)))))

; zipper-start: Move to start of list
(zipper-start (fn (z)
                  (zipper-make (zipper-to-list z))))

; zipper-end: Move to end of list
(zipper-end (fn (z)
                ((fix move-rec
                      (fn (zp)
                          (if (null? (zipper-right zp))
                              zp
                              (move-rec (zipper-move-right zp)))))
                 z)))

; zipper-find: Move to first element matching predicate
(zipper-find (fn (pred z)
                 (let ((z-start (zipper-start z)))
                      ((fix find-rec
                            (fn (zp)
                                (if (pred (zipper-focus zp))
                                    zp
                                    (if (null? (zipper-right zp))
                                        #f
                                        (find-rec (zipper-move-right zp))))))
                       z-start))))

; ============================================
; Validation and Result Types
; ============================================

; validation-ok: Create success value
(validation-ok (fn (val) (list 'ok val)))

; validation-err: Create error value
(validation-err (fn (err) (list 'err err)))

; validation-ok?: Check if validation succeeded
(validation-ok? (fn (v) (eq? (car v) 'ok)))

; validation-err?: Check if validation failed
(validation-err? (fn (v) (eq? (car v) 'err)))

; validation-value: Get the wrapped value (ok or err)
(validation-value cadr)

; validation-map: Map over successful value
(validation-map (fn (f v)
                    (if (validation-ok? v)
                        (validation-ok (f (validation-value v)))
                        v)))

; validation-map-err: Map over error value
(validation-map-err (fn (f v)
                        (if (validation-err? v)
                            (validation-err (f (validation-value v)))
                            v)))

; validation-bind: Monadic bind for validation
(validation-bind (fn (f v)
                     (if (validation-ok? v)
                         (f (validation-value v))
                         v)))

; validation-ap: Applicative apply (accumulates errors)
(validation-ap (fn (vf va)
                   (if (validation-ok? vf)
                       (if (validation-ok? va)
                           (validation-ok ((validation-value vf) (validation-value va)))
                           va)
                       (if (validation-ok? va)
                           vf
                           (validation-err (append (validation-value vf) (validation-value va)))))))

; validate-all-results: Run all validations, collecting errors (for validation-ok/err results)
(validate-all-results (fn (validations)
                          (foldl (fn (acc v)
                                     (if (validation-ok? acc)
                                         (if (validation-ok? v)
                                             (validation-ok (cons (validation-value v) (validation-value acc)))
                                             v)
                                         (if (validation-ok? v)
                                             acc
                                             (validation-err (append (validation-value acc) (validation-value v))))))
                                 (validation-ok '())
                                 validations)))

; validate-when: Conditional validation
(validate-when (fn (pred err-msg val)
                   (if (pred val)
                       (validation-ok val)
                       (validation-err (list err-msg)))))

; validate-not-null: Validate value is not null/nil
(validate-not-null (fn (err-msg val)
                       (if (null? val)
                           (validation-err (list err-msg))
                           (validation-ok val))))

; validate-positive: Validate number is positive
(validate-positive (fn (err-msg val)
                       (if (> val 0)
                           (validation-ok val)
                           (validation-err (list err-msg)))))

; validate-in-range: Validate number is in range
(validate-in-range (fn (lo hi err-msg val)
                       (if (and (>= val lo) (<= val hi))
                           (validation-ok val)
                           (validation-err (list err-msg)))))

; validate-non-empty: Validate list/string is non-empty
(validate-non-empty (fn (err-msg val)
                        (if (or (null? val) (and (string? val) (string-null? val)))
                            (validation-err (list err-msg))
                            (validation-ok val))))

; ============================================
; Lens/Optics Utilities
; ============================================

; make-lens: Create a lens from getter and setter
(make-lens (fn (getter setter)
               (list 'lens getter setter)))

; lens-get: Get value through lens
(lens-get (fn (lens obj)
              ((cadr lens) obj)))

; lens-set: Set value through lens
(lens-set (fn (lens val obj)
              ((caddr lens) val obj)))

; lens-over: Modify value through lens
(lens-over (fn (lens f obj)
               (lens-set lens (f (lens-get lens obj)) obj)))

; lens-compose: Compose two lenses
(lens-compose (fn (outer inner)
                  (make-lens
                   (fn (obj) (lens-get inner (lens-get outer obj)))
                   (fn (val obj)
                       (lens-over outer (fn (inner-obj) (lens-set inner val inner-obj)) obj)))))

; List lenses

; lens-head: Lens for list head
(lens-head (make-lens
            car
            (fn (val lst) (cons val (cdr lst)))))

; lens-tail: Lens for list tail
(lens-tail (make-lens
            cdr
            (fn (val lst) (cons (car lst) val))))

; lens-nth: Create lens for nth element
(lens-nth (fn (n)
              (make-lens
               (fn (lst) (list-ref lst n))
               (fn (val lst)
                   (append (take lst n) (cons val (drop lst (+ n 1))))))))

; Pair lenses

; lens-fst: Lens for first element of pair
(lens-fst (make-lens
           car
           (fn (val pair) (cons val (cdr pair)))))

; lens-snd: Lens for second element of pair
(lens-snd (make-lens
           cdr
           (fn (val pair) (cons (car pair) val))))

; Alist lenses

; lens-key: Create lens for alist key
(lens-key (fn (key default)
              (make-lens
               (fn (alist) (alist-ref key alist default))
               (fn (val alist) (alist-set key val alist)))))

; ============================================
; Additional Numeric Utilities
; ============================================

; clamp: Clamp value to range [lo, hi]
(clamp (fn (lo hi val)
           (max lo (min hi val))))

; lerp: Linear interpolation
(lerp (fn (a b t)
          (+ a (* t (- b a)))))

; inverse-lerp: Inverse of lerp
(inverse-lerp (fn (a b val)
                  (/ (- val a) (- b a))))

; remap: Remap value from one range to another
(remap (fn (in-lo in-hi out-lo out-hi val)
           (lerp out-lo out-hi (inverse-lerp in-lo in-hi val))))

; sign: Return sign of number (-1, 0, or 1)
(sign (fn (x)
          (if (< x 0) -1 (if (> x 0) 1 0))))

; step: Step function (0 if x < edge, 1 otherwise)
(step (fn (edge x)
          (if (< x edge) 0 1)))

; smoothstep: Smooth step function
(smoothstep (fn (edge0 edge1 x)
                (let ((t (clamp 0 1 (inverse-lerp edge0 edge1 x))))
                     (* t t (- 3 (* 2 t))))))

; wrap: Wrap value to range [lo, hi)
(wrap (fn (lo hi val)
          (let ((range (- hi lo)))
               (+ lo (mod (- val lo) range)))))

; ping-pong: Ping-pong value between 0 and length
(ping-pong (fn (length val)
               (let ((t (mod val (* 2 length))))
                    (if (< t length) t (- (* 2 length) t)))))

; ============================================
; Statistics Utilities
; ============================================

; mean: Calculate arithmetic mean
; REMOVED: mean (duplicate, now in stats.ss)

; variance: Calculate population variance
; REMOVED: variance (duplicate, now in stats.ss)

; std-dev: Calculate population standard deviation
; REMOVED: std-dev (duplicate, now in stats.ss)

; median: Calculate median
; REMOVED: median (duplicate, now in stats.ss)

; mode: Find most common element(s)
; REMOVED: mode (duplicate, now in stats.ss)

; range-stat: Calculate range (max - min)
(range-stat (fn (lst)
                (if (null? lst)
                    0
                    (- (maximum lst) (minimum lst)))))

; percentile: Calculate nth percentile
; REMOVED: percentile (duplicate, now in stats.ss)

; quartiles: Calculate Q1, Q2 (median), Q3
; REMOVED: quartiles (duplicate, now in stats.ss)

; correlation: Pearson correlation coefficient
; REMOVED: correlation (duplicate, now in stats.ss)

; ============================================
; Rose Trees (Multi-way Trees)
; ============================================

; rose-node: Create a rose tree node with value and list of children
(rose-node (fn (value children)
               (cons value children)))

; rose-leaf: Create a leaf node (no children)
(rose-leaf (fn (value)
               (cons value '())))

; rose-value: Get value at node
(rose-value car)

; rose-children: Get children list
(rose-children cdr)

; rose-leaf?: Check if node is a leaf
(rose-leaf? (fn (t)
                (null? (cdr t))))

; rose-map: Map function over all values in tree
(rose-map (fix rose-map-rec
               (fn (f t)
                   (rose-node (f (rose-value t))
                              (map (fn (child) (rose-map-rec f child))
                                   (rose-children t))))))

; rose-fold: Fold over tree (post-order)
; f takes (value, list-of-child-results) -> result
(rose-fold (fix rose-fold-rec
                (fn (f t)
                    (f (rose-value t)
                       (map (fn (child) (rose-fold-rec f child))
                            (rose-children t))))))

; rose-flatten: Get all values as flat list (pre-order)
(rose-flatten (fix rose-flatten-rec
                   (fn (t)
                       (cons (rose-value t)
                             (concat (map rose-flatten-rec (rose-children t)))))))

; rose-depth: Maximum depth of tree
(rose-depth (fix rose-depth-rec
                 (fn (t)
                     (if (rose-leaf? t)
                         1
                         (+ 1 (apply max (map rose-depth-rec (rose-children t))))))))

; rose-size: Count all nodes in tree
(rose-size (fix rose-size-rec
                (fn (t)
                    (+ 1 (sum-list (map rose-size-rec (rose-children t)))))))

; rose-find: Find first node matching predicate (pre-order)
(rose-find (fix rose-find-rec
                (fn (pred t)
                    (if (pred (rose-value t))
                        (just (rose-value t))
                        (let ((child-results (map (fn (c) (rose-find-rec pred c))
                                                  (rose-children t))))
                             (let ((found (find-if (fn (r) (not (nothing? r))) child-results)))
                                  (if found found (nothing))))))))

; rose-filter: Keep only nodes matching predicate (and their ancestors)
(rose-filter (fix rose-filter-rec
                  (fn (pred t)
                      (let ((filtered-children (filter id
                                                       (map (fn (c) (rose-filter-rec pred c))
                                                            (rose-children t)))))
                           (if (or (pred (rose-value t))
                                   (not (null? filtered-children)))
                               (rose-node (rose-value t) filtered-children)
                               #f)))))

; rose-paths: Get all root-to-leaf paths
(rose-paths (fix rose-paths-rec
                 (fn (t)
                     (if (rose-leaf? t)
                         (list (list (rose-value t)))
                         (concat (map (fn (c)
                                          (map (fn (path) (cons (rose-value t) path))
                                               (rose-paths-rec c)))
                                      (rose-children t)))))))

; rose-from-nested: Create rose tree from nested list structure
; ((1 (2) (3 (4)))) -> rose tree with 1 at root, children 2 and 3
(rose-from-nested (fix rose-from-nested-rec
                       (fn (nested)
                           (if (pair? nested)
                               (rose-node (car nested)
                                          (map rose-from-nested-rec (cdr nested)))
                               (rose-leaf nested)))))

; ============================================
; Heap (Priority Queue) - Min Heap
; ============================================

; heap-empty: Empty heap
(heap-empty '())

; heap-empty?: Check if heap is empty
(heap-empty? null?)

; heap-singleton: Single element heap
(heap-singleton (fn (x)
                    (list x '() '())))

; heap-value: Get value at node
(heap-value car)

; heap-left: Get left child
(heap-left cadr)

; heap-right: Get right child
(heap-right caddr)

; heap-merge: Merge two heaps (leftist merge)
(heap-merge (fix heap-merge-rec
                 (fn (h1 h2)
                     (if (null? h1) h2
                         (if (null? h2) h1
                             (if (< (heap-value h1) (heap-value h2))
                                 (list (heap-value h1)
                                       (heap-merge-rec (heap-right h1) h2)
                                       (heap-left h1))
                                 (list (heap-value h2)
                                       (heap-merge-rec h1 (heap-right h2))
                                       (heap-left h2))))))))

; heap-insert: Insert element
(heap-insert (fn (x h)
                 (heap-merge (heap-singleton x) h)))

; heap-find-min: Get minimum element
(heap-find-min (fn (h)
                   (if (null? h) #f (heap-value h))))

; heap-delete-min: Remove minimum element
(heap-delete-min (fn (h)
                     (if (null? h) '()
                         (heap-merge (heap-left h) (heap-right h)))))

; heap-from-list: Build heap from list
(heap-build (fn (lst)
                (foldl (fn (h x) (heap-insert x h)) heap-empty lst)))

; heap-sort: Sort using heap
(heap-sort (fn (lst)
               ((fix extract-rec
                     (fn (h acc)
                         (if (heap-empty? h)
                             (reverse acc)
                             (extract-rec (heap-delete-min h)
                                          (cons (heap-find-min h) acc)))))
                (heap-build lst) '())))

; ============================================
; Ring Buffer (Circular Buffer)
; ============================================

; ring-make: Create ring buffer with capacity
; Stored as (capacity write-pos read-pos buffer)
(ring-make (fn (capacity)
               (list capacity 0 0 (replicate capacity #f))))

; ring-capacity: Get buffer capacity
(ring-capacity car)

; ring-write-pos: Get write position
(ring-write-pos cadr)

; ring-read-pos: Get read position
(ring-read-pos caddr)

; ring-buffer: Get buffer list
(ring-buffer cadddr)

; ring-empty?: Check if buffer is empty
(ring-empty? (fn (r)
                 (= (ring-write-pos r) (ring-read-pos r))))

; ring-full?: Check if buffer is full
(ring-full? (fn (r)
                (= (mod (+ (ring-write-pos r) 1) (ring-capacity r))
                   (ring-read-pos r))))

; ring-size: Current number of elements
(ring-size (fn (r)
               (mod (- (ring-write-pos r) (ring-read-pos r) (- (ring-capacity r)))
                    (ring-capacity r))))

; ring-write: Write element to buffer
(ring-write (fn (x r)
                (if (ring-full? r)
                    r  ; Buffer full, drop write
                    (let ((cap (ring-capacity r))
                          (wp (ring-write-pos r))
                          (rp (ring-read-pos r))
                          (buf (ring-buffer r)))
                         (list cap
                               (mod (+ wp 1) cap)
                               rp
                               (list-set wp x buf))))))

; ring-read: Read element from buffer
; Returns (new-ring . value) or (ring . #f) if empty
(ring-read (fn (r)
               (if (ring-empty? r)
                   (cons r #f)
                   (let ((cap (ring-capacity r))
                         (wp (ring-write-pos r))
                         (rp (ring-read-pos r))
                         (buf (ring-buffer r)))
                        (cons (list cap wp (mod (+ rp 1) cap) buf)
                              (list-ref buf rp))))))

; ring-peek: Peek at next element without removing
(ring-peek (fn (r)
               (if (ring-empty? r)
                   #f
                   (list-ref (ring-buffer r) (ring-read-pos r)))))

; ring-to-list: Convert to list (oldest first)
(ring-to-list (fn (r)
                  ((fix collect-rec
                        (fn (rng acc)
                            (if (ring-empty? rng)
                                (reverse acc)
                                (let ((result (ring-read rng)))
                                     (collect-rec (car result) (cons (cdr result) acc))))))
                   r '())))

; ============================================
; Arrow Combinators
; ============================================

; arr: Lift function to arrow (identity for functions)
(arr id)

; >>>: Arrow composition (left to right)
(arrow-compose (fn (f g)
                   (fn (x) (g (f x)))))

; arrow-first-curried: Apply arrow to first element of pair (curried)
(arrow-first-curried (fn (f)
                         (fn (pair) (cons (f (car pair)) (cdr pair)))))

; arrow-second-curried: Apply arrow to second element of pair (curried)
(arrow-second-curried (fn (f)
                          (fn (pair) (cons (car pair) (f (cdr pair))))))

; arrow-split-curried: Product arrow - parallel composition (curried)
(arrow-split-curried (fn (f g)
                         (fn (pair) (cons (f (car pair)) (g (cdr pair))))))

; &&&: Fanout - apply both arrows to same input
(arrow-fanout (fn (f g)
                  (fn (x) (cons (f x) (g x)))))

; |||: Fanin - apply appropriate arrow based on sum type
(arrow-fanin (fn (f g)
                 (fn (either)
                     (if (left? either)
                         (left (f (from-left either)))
                         (right (g (from-right either)))))))

; arrow-loop: Feedback loop (requires initial state)
(arrow-loop (fn (f init)
                ((fix loop-rec
                      (fn (state)
                          (fn (x)
                              (let ((result (f (cons x state))))
                                   (cons (car result)
                                         (loop-rec (cdr result)))))))
                 init)))

; ============================================
; Applicative Functor Utilities
; ============================================

; pure: Wrap value in minimal context (for lists, singleton)
(pure-list (fn (x) (list x)))

; <*>: Applicative apply for lists
(ap-list (fn (fs xs)
             (concat (map (fn (f) (map f xs)) fs))))

; lift-a2: Lift binary function to applicative
(lift-a2 (fn (f ma mb)
             (ap-list (map (fn (a) (fn (b) (f a b))) ma) mb)))

; lift-a3: Lift ternary function to applicative
(lift-a3 (fn (f ma mb mc)
             (ap-list (ap-list (map (fn (a) (fn (b) (fn (c) (f a b c)))) ma) mb) mc)))

; sequence-a: Sequence list of applicatives (for lists)
(sequence-a (fn (xs)
                (foldr (fn (x acc) (lift-a2 cons x acc))
                       (pure-list '())
                       xs)))

; traverse: Map then sequence
(traverse-list (fn (f xs)
                   (sequence-a (map f xs))))

; zip-list-ap: ZipList applicative (pairwise application)
(zip-list-ap (fn (fs xs)
                 (zip-with (fn (f x) (f x)) fs xs)))

; ============================================
; Comonad Utilities (for zippers/streams)
; ============================================

; extract: Get focused value
(comonad-extract zipper-focus)

; duplicate: Create zipper of zippers
(comonad-duplicate (fn (z)
                       (let ((lefts ((fix collect-left
                                          (fn (zp)
                                              (if (null? (zipper-left zp))
                                                  '()
                                                  (let ((moved (zipper-move-left zp)))
                                                       (cons moved (collect-left moved))))))
                                     z))
                             (rights ((fix collect-right
                                           (fn (zp)
                                               (if (null? (zipper-right zp))
                                                   '()
                                                   (let ((moved (zipper-move-right zp)))
                                                        (cons moved (collect-right moved))))))
                                      z)))
                            (list (reverse lefts) z rights))))

; extend: Apply function in all positions
(comonad-extend (fn (f z)
                    (let ((dup (comonad-duplicate z)))
                         (list (map f (zipper-left dup))
                               (f (zipper-focus dup))
                               (map f (zipper-right dup))))))

; ============================================
; Profunctor Utilities
; ============================================

; dimap: Map over both input and output of function
(dimap (fn (f g h)
           (compose g (compose h f))))

; lmap: Map over input only
(lmap (fn (f h)
          (compose h f)))

; rmap: Map over output only (same as compose)
(rmap compose)

; ============================================
; Catamorphism / Recursion Schemes Basics
; ============================================

; list-cata: List catamorphism (generalized fold)
(list-cata (fn (nil-case cons-case lst)
               ((fix cata-rec
                     (fn (l)
                         (if (null? l)
                             nil-case
                             (cons-case (car l) (cata-rec (cdr l))))))
                lst)))

; list-ana: List anamorphism (generalized unfold)
(list-ana (fn (stop? head-fn tail-fn seed)
              ((fix ana-rec
                    (fn (s)
                        (if (stop? s)
                            '()
                            (cons (head-fn s) (ana-rec (tail-fn s))))))
               seed)))

; list-hylo: Hylomorphism (unfold then fold)
(list-hylo (fn (nil-case cons-case stop? head-fn tail-fn seed)
               ((fix hylo-rec
                     (fn (s)
                         (if (stop? s)
                             nil-case
                             (cons-case (head-fn s) (hylo-rec (tail-fn s))))))
                seed)))

; tree-cata: Rose tree catamorphism
(tree-cata (fn (f t)
               (rose-fold f t)))

; ============================================
; Difference Lists
; ============================================

; dlist-empty: Empty difference list (identity function)
(dlist-empty id)

; dlist-singleton: Single element difference list
(dlist-singleton (fn (x)
                     (fn (rest) (cons x rest))))

; dlist-append: O(1) append for difference lists
(dlist-append compose)

; dlist-cons: Prepend element
(dlist-cons (fn (x dl)
                (compose (dlist-singleton x) dl)))

; dlist-snoc: Append element to end
(dlist-snoc (fn (dl x)
                (compose dl (dlist-singleton x))))

; dlist-to-list: Convert to regular list
(dlist-to-list (fn (dl)
                   (dl '())))

; list-to-dlist: Convert from regular list
(list-to-dlist (fn (lst)
                   (fn (rest) (append lst rest))))

; dlist-concat: Concatenate list of dlists
(dlist-concat (fn (dls)
                  (foldl dlist-append dlist-empty dls)))

; ============================================
; Finger Trees (simplified - just deque operations)
; ============================================

; finger-empty: Empty finger tree
(finger-empty (list 'empty))

; finger-empty?: Check if empty
(finger-empty? (fn (ft)
                   (eq? (car ft) 'empty)))

; finger-single: Single element tree
(finger-single (fn (x)
                   (list 'single x)))

; finger-single?: Check if single
(finger-single? (fn (ft)
                    (eq? (car ft) 'single)))

; finger-deep: Deep tree with prefix, middle, suffix
(finger-deep (fn (prefix middle suffix)
                 (list 'deep prefix middle suffix)))

; finger-push-front: Add to front
(finger-push-front (fn (x ft)
                       (if (finger-empty? ft)
                           (finger-single x)
                           (if (finger-single? ft)
                               (finger-deep (list x) finger-empty (list (cadr ft)))
                               (finger-deep (cons x (cadr ft))
                                            (caddr ft)
                                            (cadddr ft))))))

; finger-push-back: Add to back
(finger-push-back (fn (x ft)
                      (if (finger-empty? ft)
                          (finger-single x)
                          (if (finger-single? ft)
                              (finger-deep (list (cadr ft)) finger-empty (list x))
                              (finger-deep (cadr ft)
                                           (caddr ft)
                                           (snoc (cadddr ft) x))))))

; finger-peek-front: Get front element
(finger-peek-front (fn (ft)
                       (if (finger-empty? ft)
                           #f
                           (if (finger-single? ft)
                               (cadr ft)
                               (car (cadr ft))))))

; finger-peek-back: Get back element
(finger-peek-back (fn (ft)
                      (if (finger-empty? ft)
                          #f
                          (if (finger-single? ft)
                              (cadr ft)
                              (last (cadddr ft))))))

; ============================================
; Additional String Utilities
; ============================================

; string-words: Split on whitespace
; string-lines: Split on newlines
; string-unwords: Join with spaces
; string-unlines: Join with newlines
; string-repeat: Repeat string n times
; string-take-while: Take chars while predicate holds
; string-drop-while: Drop chars while predicate holds
; ============================================
; Logic and Constraint Utilities
; ============================================

; all-different?: Check all elements are unique
(all-different? (fn (lst)
                    (= (length lst) (length (nub lst)))))

; exactly-one?: Check exactly one element satisfies pred
(exactly-one? (fn (pred lst)
                  (= 1 (count-if pred lst))))

; at-most-one?: Check at most one element satisfies pred
(at-most-one? (fn (pred lst)
                  (<= (count-if pred lst) 1)))

; at-least-one?: Check at least one element satisfies pred
(at-least-one? any)

; count-equal: Count how many elements equal value
(count-equal (fn (val lst)
                 (count-if (fn (x) (equal? x val)) lst)))

; all-equal?: Check all elements are equal
; ============================================
; Character Predicates
; ============================================

; char-digit?: Check if char is a digit (ASCII 48-57)
(char-digit? (fn (c)
                 (let ((n (char->integer c)))
                      (and (>= n 48) (<= n 57)))))

; char-alpha?: Check if char is a letter
(char-alpha? (fn (c)
                 (let ((n (char->integer c)))
                      (or (and (>= n 65) (<= n 90))
                          (and (>= n 97) (<= n 122))))))

; char-alphanumeric?: Check if char is alphanumeric
(char-alphanumeric? (fn (c)
                        (or (char-digit? c) (char-alpha? c))))

; char-whitespace?: Check if char is whitespace (space=32, newline=10, tab=9, cr=13)
(char-whitespace? (fn (c)
                      (let ((n (char->integer c)))
                           (or (= n 32) (or (= n 10) (or (= n 9) (= n 13)))))))

; char-lower?: Check if char is lowercase
(char-lower? (fn (c)
                 (let ((n (char->integer c)))
                      (and (>= n 97) (<= n 122)))))

; char-upper?: Check if char is uppercase
(char-upper? (fn (c)
                 (let ((n (char->integer c)))
                      (and (>= n 65) (<= n 90)))))

; char-downcase: Convert char to lowercase
(char-downcase (fn (c)
                   (if (char-upper? c)
                       (integer->char (+ (char->integer c) 32))
                       c)))

; char-upcase: Convert char to uppercase
(char-upcase (fn (c)
                 (if (char-lower? c)
                     (integer->char (- (char->integer c) 32))
                     c)))

; ============================================
; Graph Algorithms
; ============================================
; Graphs as adjacency lists: ((node . (neighbors...)) ...)

; graph-empty: Empty graph
(graph-empty '())

; graph-add-node: Add node to graph
(graph-add-node (fn (node g)
                    (if (assoc node g)
                        g
                        (cons (cons node '()) g))))

; graph-nodes: Get all nodes
(graph-nodes (fn (g) (map car g)))

; graph-from-edges: Build from edge list
; bfs: Breadth-first search
(bfs (fn (start g)
         ((fix bfs-rec
               (fn (queue visited result)
                   (if (null? queue)
                       (reverse result)
                       (let ((cur (car queue))
                             (rest (cdr queue)))
                            (if (member? cur visited)
                                (bfs-rec rest visited result)
                                (bfs-rec (append rest (graph-neighbors cur g))
                                         (cons cur visited)
                                         (cons cur result)))))))
          (list start) '() '())))

; dfs: Depth-first search
(dfs (fn (start g)
         ((fix dfs-rec
               (fn (stack visited result)
                   (if (null? stack)
                       (reverse result)
                       (let ((cur (car stack))
                             (rest (cdr stack)))
                            (if (member? cur visited)
                                (dfs-rec rest visited result)
                                (dfs-rec (append (graph-neighbors cur g) rest)
                                         (cons cur visited)
                                         (cons cur result)))))))
          (list start) '() '())))

; graph-path-exists?: Check if path exists
; ============================================
; State Monad
; ============================================
; State s a = s -> (a, s)

; state-run: Run state computation
(state-run (fn (ma s) (ma s)))

; state-eval: Run and return value
(state-eval (fn (ma s) (car (ma s))))

; state-exec: Run and return state
(state-exec (fn (ma s) (cadr (ma s))))

; ============================================
; Reader Monad
; ============================================
; Reader r a = r -> a

; reader-return: Wrap value
; reader-run: Run reader
(reader-run (fn (ma r) (ma r)))

; ============================================
; Writer Monad
; ============================================
; Writer w a = (a, w) where w is monoid (list)

; writer-run: Get value and log
(writer-run id)

; writer-value: Get just value
(writer-value car)

; writer-log: Get just log
(writer-log cadr)

; ============================================
; Numeric Extensions
; ============================================

; divides?: Check if a divides b
; perfect-square?: Check if n is perfect square
(perfect-square? (fn (n)
                     (let ((s (isqrt n)))
                          (= (* s s) n))))

; triangular: nth triangular number
; pentagonal: nth pentagonal number
; hexagonal: nth hexagonal number
; catalan: nth Catalan number
; stirling1: Stirling first kind (unsigned)
(stirling1 (fix stirling1-rec
                (fn (n k)
                    (if (= n 0)
                        (if (= k 0) 1 0)
                        (if (= k 0)
                            0
                            (+ (* (- n 1) (stirling1-rec (- n 1) k))
                               (stirling1-rec (- n 1) (- k 1))))))))

; stirling2: Stirling second kind
(stirling2 (fix stirling2-rec
                (fn (n k)
                    (if (= n 0)
                        (if (= k 0) 1 0)
                        (if (= k 0)
                            0
                            (+ (* k (stirling2-rec (- n 1) k))
                               (stirling2-rec (- n 1) (- k 1))))))))

; digital-root: Sum digits until single digit
(digital-root (fix dr-rec
                   (fn (n)
                       (if (< n 10)
                           n
                           (dr-rec (sum-list (map (fn (c) (- (char->integer c) 48))
                                                  (string->list (number->string n)))))))))

; collatz: Next in Collatz sequence
(collatz (fn (n)
             (if (even? n)
                 (/ n 2)
                 (+ (* 3 n) 1))))

; collatz-length: Steps to reach 1
(collatz-length (fix coll-rec
                     (fn (n steps)
                         (if (= n 1)
                             steps
                             (coll-rec (collatz n) (+ steps 1))))))

; ============================================================
; Parser Combinators
; A parser is: input -> 'nothing | (value . remaining-input)
; ============================================================

; parse-return: Always succeed with value
(parse-return (fn (x)
                  (fn (input) (cons x input))))

; parse-fail: Always fail
(parse-fail (fn (input) 'nothing))

; parse-item: Consume one item from input
(parse-item (fn (input)
                (if (null? input)
                    'nothing
                    (cons (car input) (cdr input)))))

; parse-satisfy: Consume item matching predicate
(parse-satisfy (fn (test-fn)
                   (fn (input)
                       (if (null? input)
                           'nothing
                           (if (test-fn (car input))
                               (cons (car input) (cdr input))
                               'nothing)))))

; parse-bind: Monadic bind for parsers
(parse-bind (fn (p f)
                (fn (input)
                    (let ((result (p input)))
                         (if (eq? result 'nothing)
                             'nothing
                             ((f (car result)) (cdr result)))))))

; parse-map: Apply function to parser result
(parse-map (fn (f p)
               (parse-bind p (fn (x) (parse-return (f x))))))

; parse-or: Try first parser, if fails try second
(parse-or (fn (p1 p2)
              (fn (input)
                  (let ((result (p1 input)))
                       (if (eq? result 'nothing)
                           (p2 input)
                           result)))))

; parse-seq2: Sequence two parsers, return pair
(parse-seq2 (fn (p1 p2)
                (parse-bind p1 (fn (a)
                                   (parse-bind p2 (fn (b)
                                                      (parse-return (cons a b))))))))

; parse-left: Sequence two parsers, keep left result
(parse-left (fn (p1 p2)
                (parse-bind p1 (fn (a)
                                   (parse-bind p2 (fn (b)
                                                      (parse-return a)))))))

; parse-right: Sequence two parsers, keep right result
(parse-right (fn (p1 p2)
                 (parse-bind p1 (fn (a)
                                    (parse-bind p2 (fn (b)
                                                       (parse-return b)))))))

; parse-many: Zero or more matches
(parse-many (fn (p)
                (fn (input)
                    ((fix many-rec
                          (fn (inp acc)
                              (let ((result (p inp)))
                                   (if (eq? result 'nothing)
                                       (cons (reverse acc) inp)
                                       (many-rec (cdr result) (cons (car result) acc))))))
                     input '()))))

; parse-many1: One or more matches
(parse-many1 (fn (p)
                 (parse-bind p (fn (first)
                                   (parse-bind (parse-many p) (fn (rest)
                                                                  (parse-return (cons first rest))))))))

; parse-optional: Zero or one match
(parse-optional (fn (p default)
                    (parse-or p (parse-return default))))

; parse-between: Parser between open and close
(parse-between (fn (open close p)
                   (parse-right open (parse-left p close))))

; parse-sep-by: Zero or more separated by delimiter
(parse-sep-by (fn (p sep)
                  (parse-or
                   (parse-bind p (fn (first)
                                     (parse-bind (parse-many (parse-right sep p)) (fn (rest)
                                                                                      (parse-return (cons first rest))))))
                   (parse-return '()))))

; parse-sep-by1: One or more separated by delimiter
(parse-sep-by1 (fn (p sep)
                   (parse-bind p (fn (first)
                                     (parse-bind (parse-many (parse-right sep p)) (fn (rest)
                                                                                      (parse-return (cons first rest))))))))

; parse-char: Match specific character
(parse-char (fn (c)
                (parse-satisfy (fn (x) (eq? x c)))))

; parse-not-char: Match any character except
(parse-not-char (fn (c)
                    (parse-satisfy (fn (x) (not (eq? x c))))))

; parse-one-of: Match any of the given characters
(parse-one-of (fn (chars)
                  (parse-satisfy (fn (c) (member? c chars)))))

; parse-none-of: Match none of the given characters
(parse-none-of (fn (chars)
                   (parse-satisfy (fn (c) (not (member? c chars))))))

; parse-string: Match a sequence of characters
(parse-string (fn (str)
                  ((fix str-rec
                        (fn (chars)
                            (if (null? chars)
                                (parse-return '())
                                (parse-bind (parse-char (car chars)) (fn (c)
                                                                         (parse-bind (str-rec (cdr chars)) (fn (cs)
                                                                                                               (parse-return (cons c cs)))))))))
                   (string->list str))))

; parse-digit: Match a digit character
(parse-digit (parse-satisfy char-digit?))

; parse-alpha: Match an alphabetic character
(parse-alpha (parse-satisfy char-alpha?))

; parse-alphanum: Match alphanumeric character
(parse-alphanum (parse-satisfy char-alphanumeric?))

; parse-space: Match whitespace character
(parse-space (parse-satisfy char-whitespace?))

; parse-spaces: Match zero or more spaces
(parse-spaces (parse-many parse-space))

; parse-token: Parse p surrounded by spaces
(parse-token (fn (p)
                 (parse-between parse-spaces parse-spaces p)))

; digits->number: Convert list of digit chars to number
(digits->number (fn (digits)
                    ((fix d2n-rec
                          (fn (ds acc)
                              (if (null? ds)
                                  acc
                                  (d2n-rec (cdr ds) (+ (* acc 10) (- (char->integer (car ds)) 48))))))
                     digits 0)))

; parse-natural: Parse natural number
(parse-natural
 (parse-bind (parse-many1 parse-digit) (fn (digits)
                                           (parse-return (digits->number digits)))))

; parse-integer: Parse integer (with optional minus)
(parse-integer
 (parse-or
  (parse-bind (parse-char (car (string->list "-"))) (fn (unused)
                                                        (parse-bind parse-natural (fn (n)
                                                                                      (parse-return (neg n))))))
  parse-natural))

; parse-run: Run parser on string input
(parse-run (fn (p str)
               (p (string->list str))))

; parse-result: Get just the value from parse result
(parse-result (fn (result)
                  (if (eq? result 'nothing)
                      'nothing
                      (car result))))

; ============================================================
; Control Flow Utilities
; ============================================================

; when-fn: Execute thunk if condition true, return result or nil
(when-fn (fn (condition thunk)
             (if condition (thunk) '())))

; unless-fn: Execute thunk if condition false
(unless-fn (fn (condition thunk)
               (if condition '() (thunk))))

; if-let: Bind value and branch on it being truthy
; (if-let val (get-val) then-expr else-expr)
(if-let-fn (fn (val then-fn else-fn)
               (if val (then-fn val) (else-fn))))

; when-let-fn: Execute with value if truthy
(when-let-fn (fn (val then-fn)
                 (if val (then-fn val) '())))

; cond-helper: Process list of (pred . result-fn) pairs
(cond-helper (fix cond-rec
                  (fn (clauses)
                      (if (null? clauses)
                          '()
                          (let ((clause (car clauses)))
                               (if ((car clause))
                                   ((cdr clause))
                                   (cond-rec (cdr clauses))))))))

; case-eq: Match value against cases ((val . result-fn) ...)
(case-eq (fn (val cases)
             ((fix case-rec
                   (fn (remaining)
                       (if (null? remaining)
                           (fn () '())
                           (let ((c (car remaining)))
                                (if (eq? val (car c))
                                    (cdr c)
                                    (case-rec (cdr remaining)))))))
              cases)))

; do-times: Execute thunk n times, return list of results
; do-while: Execute while condition holds, return final value
(do-while (fn (init step-fn done-fn)
              ((fix while-rec
                    (fn (val)
                        (if (done-fn val)
                            val
                            (while-rec (step-fn val)))))
               init)))

; repeat-until: Execute until condition becomes true
; for-each-indexed: Apply fn to each element with index
; loop-collect: Loop collecting results while condition holds
; try-catch: Try thunk, on error call error-fn with error
; Note: Limited error handling in pure context
; default: Return default if value is falsy
; first-truthy: Return first truthy value from list
; short-circuit-and: Lazy and with thunks
; short-circuit-or: Lazy or with thunks
; guard-value: Return value only if predicate holds, else default
(guard-value (fn (val predicate-fn default-val)
                 (if (predicate-fn val) val default-val)))

; ensure-or-fail: Assert predicate holds, return value or 'assertion-failed symbol
(ensure-or-fail (fn (val predicate-fn)
                    (if (predicate-fn val) val 'assertion-failed)))

; chain-guards: Apply sequence of guards, short-circuit on failure
; ============================================================
; Advanced List Utilities
; ============================================================

; dedupe: Remove consecutive duplicates
; dedupe-by: Remove consecutive duplicates by key
; take-nth: Take every nth element
; drop-nth: Drop every nth element
; split-with: Split list at first element matching predicate
; group-runs: Group consecutive runs of equal elements
; map-runs: Apply function to runs of consecutive equal elements
; find-indices: Find all indices matching predicate
; replace-at: Replace element at index
; swap-at: Swap elements at two indices
; prefix?: Check if first list is prefix of second
; suffix?: Check if first list is suffix of second
; sublist?: Check if first list appears in second
; count-occurrences: Count how many times sublist appears
; windowed: Generate overlapping windows
; ============================================================
; String Algorithms
; ============================================================

; edit-distance: Levenshtein distance between two strings
; hamming-distance: Count differing positions (same-length strings)
; longest-common-prefix: Find longest common prefix of two strings
; longest-common-suffix: Find longest common suffix
; string-similarity: Similarity ratio (0.0 to 1.0)
; fuzzy-match?: Check if strings are similar within threshold
; string-normalize: Lowercase and trim whitespace
; string-tokenize: Split on whitespace, remove empty
; ============================================================
; Testing and Assertion Utilities
; ============================================================

; assert-eq-msg: Assert two values are equal (with message for test framework)
(assert-eq-msg (fn (expected actual msg)
                   (if (equal? expected actual)
                       (list 'pass msg)
                       (list 'fail msg 'expected expected 'got actual))))

; assert-true: Assert value is truthy
; assert-false: Assert value is falsy
; assert-throws: Assert thunk throws (returns error symbol)
; run-tests: Run list of test thunks, return summary
; test-case: Create a named test case
; ============================================================
; Sequence Generation Utilities
; ============================================================

; arithmetic-seq: Generate arithmetic sequence
; geometric-seq: Generate geometric sequence
; fibonacci-seq: Generate first n Fibonacci numbers
; factorials-up-to: Generate factorials up to n
; convergent-seq: Generate sequence until convergence
; cycle-detect: Detect cycle in sequence, return (mu . lambda)
; mu = start of cycle, lambda = length of cycle
; ============================================================
; Encoding Utilities
; ============================================================

; hex-chars: Hex character lookup
; nibble->hex: Convert 0-15 to hex char
; hex->nibble: Convert hex char to 0-15
; byte->hex: Convert byte (0-255) to two hex chars
; hex->byte: Convert two hex chars to byte
; bytes->hex: Convert list of bytes to hex string
; hex->bytes: Convert hex string to list of bytes
; string->bytes: Convert string to list of byte values
; bytes->string: Convert list of byte values to string
; string->hex: Convert string to hex encoding
; hex->string: Convert hex encoding to string
; ============================================================
; Memoization Utilities
; ============================================================

; memo-table: Create empty memoization table
; memo-lookup: Look up value in memo table
; memo-insert: Insert value into memo table
; with-memo: Execute with memoization (returns (result . new-table))
; ============================================================
; Advanced Math Utilities
; ============================================================

; newton-sqrt: Square root via Newton's method
; nth-root: Nth root via Newton's method
; mean: Arithmetic mean
; REMOVED: mean (duplicate, now in stats.ss)

; variance: Population variance
; REMOVED: variance (duplicate, now in stats.ss)

; std-dev: Standard deviation
; REMOVED: std-dev (duplicate, now in stats.ss)

; median: Middle value of sorted list
; REMOVED: median (duplicate, now in stats.ss)

; mode: Most frequent value
; REMOVED: mode (duplicate, now in stats.ss)

; percentile: Value at given percentile (0-100)
; REMOVED: percentile (duplicate, now in stats.ss)

; correlation: Pearson correlation coefficient
; REMOVED: correlation (duplicate, now in stats.ss)

; linear-regression: Simple linear regression, returns (slope . intercept)
; ============================================================
; Combinatorial Utilities
; ============================================================

; subsets: Generate all subsets
; power-set: Alias for subsets
; ============================================================
; Functional Patterns
; ============================================================

; trampoline: Execute thunks until non-thunk result
; Thunks are functions tagged with 'bounce
; Y combinator (for reference, fix is preferred)
; compose-n: Compose n functions
; pipe-n: Pipe value through n functions (left to right)
; constantly: Return a function that always returns val
; negate-pred: Return negation of predicate (complement already exists)
; juxtapose: Apply multiple functions to same argument
; converge-with: Apply two functions to arg, combine results with combiner
; ((converge-with + f1 f2) x) => (+ (f1 x) (f2 x))
(converge-with (fn (combiner f1 f2)
                   (fn (x) (combiner (f1 x) (f2 x)))))
