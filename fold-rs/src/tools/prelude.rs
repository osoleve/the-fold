/// Embedded prelude containing essential higher-order functions and utilities.
/// This eliminates the need for a Scheme runtime.
///
/// The prelude source code in Fold syntax.
/// Uses a single let* to define all functions in scope.
pub const PRELUDE_SOURCE: &str = r#"
; ============================================================
; The Fold Prelude - Essential Higher-Order Functions
; Defined as a single nested let* block for proper scoping
; ============================================================

(let*
  (; -- Core Higher-Order Functions --

   ; map: Apply function to each element of a list
   (map (fix map
     (fn (f lst)
       (if (null? lst)
           '()
           (cons (f (car lst)) (map f (cdr lst)))))))

   ; filter: Keep elements satisfying predicate
   (filter (fix filter
     (fn (p lst)
       (if (null? lst)
           '()
           (if (p (car lst))
               (cons (car lst) (filter p (cdr lst)))
               (filter p (cdr lst)))))))

   ; foldl: Left fold (tail-recursive)
   (foldl (fix foldl
     (fn (f acc lst)
       (if (null? lst)
           acc
           (foldl f (f acc (car lst)) (cdr lst))))))

   ; foldr: Right fold
   (foldr (fix foldr
     (fn (f acc lst)
       (if (null? lst)
           acc
           (f (car lst) (foldr f acc (cdr lst)))))))

   ; any: Check if any element satisfies predicate
   (any (fix any
     (fn (p lst)
       (if (null? lst)
           #f
           (if (p (car lst))
               #t
               (any p (cdr lst)))))))

   ; all: Check if all elements satisfy predicate
   (all (fix all
     (fn (p lst)
       (if (null? lst)
           #t
           (if (p (car lst))
               (all p (cdr lst))
               #f)))))

   ; take-while: Take elements while predicate holds
   (take-while (fix take-while
     (fn (p lst)
       (if (null? lst)
           '()
           (if (p (car lst))
               (cons (car lst) (take-while p (cdr lst)))
               '())))))

   ; drop-while: Drop elements while predicate holds
   (drop-while (fix drop-while
     (fn (p lst)
       (if (null? lst)
           '()
           (if (p (car lst))
               (drop-while p (cdr lst))
               lst)))))

   ; zip-with: Combine two lists with a function
   (zip-with (fix zip-with
     (fn (f lst1 lst2)
       (if (or (null? lst1) (null? lst2))
           '()
           (cons (f (car lst1) (car lst2))
                 (zip-with f (cdr lst1) (cdr lst2)))))))

   ; -- Non-recursive utilities (don't need fix) --

   ; sum-list: Sum a list of numbers (primitives are first-class!)
   (sum-list (fn (lst) (foldl + 0 lst)))

   ; product-list: Product of a list of numbers
   (product-list (fn (lst) (foldl * 1 lst)))

   ; id: Identity function
   (id (fn (x) x))

   ; const: Return a function that always returns the same value
   (const (fn (x) (fn (y) x)))

   ; compose: Compose two functions (f . g)
   (compose (fn (f g) (fn (x) (f (g x)))))

   ; flip: Swap arguments of a binary function
   (flip (fn (f) (fn (x y) (f y x))))

   ; complement: Negate a predicate
   (complement (fn (p) (fn (x) (not (p x)))))

   ; -- Equality predicates --

   ; eqv?: Value equality (same as eq? for symbols, = for numbers)
   (eqv? (fn (a b)
     (if (number? a)
         (if (number? b) (= a b) #f)
         (if (string? a)
             (if (string? b) (string=? a b) #f)
             (if (char? a)
                 (if (char? b) (char=? a b) #f)
                 (eq? a b))))))

   ; equal?: Deep structural equality
   (equal? (fix equal-rec
     (fn (a b)
       (if (pair? a)
           (if (pair? b)
               (if (equal-rec (car a) (car b))
                   (equal-rec (cdr a) (cdr b))
                   #f)
               #f)
           (if (null? a)
               (null? b)
               (eqv? a b))))))

   ; -- More list utilities --

   ; partition: Split list into (matches, non-matches) based on predicate
   (partition (fix partition
     (fn (p lst)
       (if (null? lst)
           (list '() '())
           (let ((rest-result (partition p (cdr lst)))
                 (x (car lst)))
             (if (p x)
                 (list (cons x (car rest-result)) (cadr rest-result))
                 (list (car rest-result) (cons x (cadr rest-result)))))))))

   ; find-if: Find first element matching predicate, or #f
   (find-if (fix find-if
     (fn (p lst)
       (if (null? lst)
           #f
           (if (p (car lst))
               (car lst)
               (find-if p (cdr lst)))))))

   ; remove-if: Remove elements matching predicate (opposite of filter)
   (remove-if (fn (p lst) (filter (complement p) lst)))

   ; count-if: Count elements matching predicate
   (count-if (fn (p lst) (foldl (fn (acc x) (if (p x) (+ acc 1) acc)) 0 lst)))

   ; concat: Concatenate a list of lists
   (concat (fn (lists) (foldl append '() lists)))

   ; replicate: Create list of n copies of x
   (replicate (fix replicate
     (fn (n x)
       (if (<= n 0)
           '()
           (cons x (replicate (- n 1) x))))))

   ; iterate: Generate list by applying f n times: (x (f x) (f (f x)) ...)
   (iterate (fix iterate
     (fn (f n x)
       (if (<= n 0)
           '()
           (cons x (iterate f (- n 1) (f x)))))))

   ; scanl: Like foldl but returns list of intermediate results
   (scanl (fix scanl
     (fn (f acc lst)
       (if (null? lst)
           (list acc)
           (cons acc (scanl f (f acc (car lst)) (cdr lst)))))))

   ; curry2: Curry a 2-argument function
   (curry2 (fn (f) (fn (x) (fn (y) (f x y)))))

   ; uncurry2: Uncurry to a 2-argument function
   (uncurry2 (fn (f) (fn (x y) ((f x) y))))

   ; -- Additional list combinators --

   ; zip: Pair up elements from two lists
   (zip (fn (lst1 lst2) (zip-with cons lst1 lst2)))

   ; unzip: Split list of pairs into (cars, cdrs)
   (unzip (fn (pairs)
     (foldr (fn (pair acc)
              (cons (cons (car pair) (car acc))
                    (cons (cdr pair) (cdr acc))))
            (cons '() '())
            pairs)))

   ; intersperse: Insert separator between elements
   (intersperse (fix intersperse
     (fn (sep lst)
       (if (null? lst)
           '()
           (if (null? (cdr lst))
               (list (car lst))
               (cons (car lst) (cons sep (intersperse sep (cdr lst)))))))))

   ; span: Split at first element not matching predicate
   ; Returns (take-while-result . drop-while-result)
   (span (fn (p lst) (cons (take-while p lst) (drop-while p lst))))

   ; flat-map: Map then concatenate results
   (flat-map (fn (f lst) (concat (map f lst))))

   ; map-indexed: Map with index (0-based)
   (map-indexed (fix map-indexed
     (fn (f lst)
       (let ((go (fix go
                   (fn (i xs)
                     (if (null? xs)
                         '()
                         (cons (f i (car xs)) (go (+ i 1) (cdr xs))))))))
         (go 0 lst)))))

   ; filter-indexed: Filter with index access
   (filter-indexed (fix filter-indexed
     (fn (p lst)
       (let ((go (fix go
                   (fn (i xs)
                     (if (null? xs)
                         '()
                         (if (p i (car xs))
                             (cons (car xs) (go (+ i 1) (cdr xs)))
                             (go (+ i 1) (cdr xs))))))))
         (go 0 lst)))))

   ; nth: Safe list access (returns #f if out of bounds)
   (nth-safe (fix nth-safe
     (fn (n lst)
       (if (null? lst)
           #f
           (if (= n 0)
               (car lst)
               (nth-safe (- n 1) (cdr lst)))))))

   ; -- Extremum functions --

   ; max-by: Find maximum element by key function
   (max-by (fix max-by
     (fn (key-fn lst)
       (if (null? lst)
           #f
           (if (null? (cdr lst))
               (car lst)
               (let ((rest-max (max-by key-fn (cdr lst))))
                 (if (> (key-fn (car lst)) (key-fn rest-max))
                     (car lst)
                     rest-max)))))))

   ; min-by: Find minimum element by key function
   (min-by (fix min-by
     (fn (key-fn lst)
       (if (null? lst)
           #f
           (if (null? (cdr lst))
               (car lst)
               (let ((rest-min (min-by key-fn (cdr lst))))
                 (if (< (key-fn (car lst)) (key-fn rest-min))
                     (car lst)
                     rest-min)))))))

   ; on: Apply binary function to results of unary function
   ; Example: ((on < length) "ab" "abc") => #t
   (on (fn (f g) (fn (x y) (f (g x) (g y)))))

   ; -- More utilities --

   ; juxt: Apply list of functions to same argument, return list of results
   ; ((juxt (list inc dec)) 5) => (6 4)
   (juxt (fn (funcs)
     (fn (x) (map (fn (f) (f x)) funcs))))

   ; pipe: Compose functions left-to-right (opposite of compose)
   (pipe (fn (f g) (fn (x) (g (f x)))))

   ; -- List generators --

   ; unfold: Generate list from seed (dual of fold)
   ; (unfold p f g seed) where:
   ;   p = predicate to stop (returns #t when done)
   ;   f = function to generate value from seed
   ;   g = function to generate next seed
   ;   seed = initial seed
   ; Example: (unfold (fn (x) (> x 5)) id inc 1) => (1 2 3 4 5)
   (unfold (fix unfold
     (fn (stop? extract next seed)
       (if (stop? seed)
           '()
           (cons (extract seed) (unfold stop? extract next (next seed)))))))

   ; tails: All suffixes of a list
   ; (tails '(1 2 3)) => ((1 2 3) (2 3) (3) ())
   (tails (fix tails
     (fn (lst)
       (if (null? lst)
           (list '())
           (cons lst (tails (cdr lst)))))))

   ; inits: All prefixes of a list
   ; (inits '(1 2 3)) => (() (1) (1 2) (1 2 3))
   (inits (fix inits
     (fn (lst)
       (if (null? lst)
           (list '())
           (cons '() (map (fn (t) (cons (car lst) t)) (inits (cdr lst))))))))

   ; group-consecutive: Group consecutive equal elements
   ; (group-consecutive '(1 1 2 2 2 3)) => ((1 1) (2 2 2) (3))
   (group-consecutive (fix group-consecutive
     (fn (lst)
       (if (null? lst)
           '()
           (let ((x (car lst)))
             (let ((result (span (fn (y) (= x y)) lst)))
               (cons (car result) (group-consecutive (cdr result)))))))))

   ; range-list: Generate list of numbers (uses primitive range)
   ; (range-list 1 5) => (1 2 3 4)
   (range-list (fn (start end) (range start end)))

   ; repeat-fn: Apply function n times to initial value, return final result
   ; (repeat-fn inc 5 0) => 5
   (repeat-fn (fix repeat-fn
     (fn (f n x)
       (if (<= n 0)
           x
           (repeat-fn f (- n 1) (f x))))))

   ; -- Maybe/Option utilities (using #f as None) --

   ; maybe: Apply function if value is not #f, else return default
   ; (maybe 0 inc #f) => 0
   ; (maybe 0 inc 5) => 6
   (maybe (fn (default f x)
     (if x (f x) default)))

   ; from-maybe: Get value or default if #f
   ; (from-maybe 0 #f) => 0
   ; (from-maybe 0 5) => 5
   (from-maybe (fn (default x)
     (if x x default)))

   ; map-maybe: Map over list, keeping only non-#f results
   ; (map-maybe (fn (x) (if (> x 0) x #f)) '(-1 2 -3 4)) => (2 4)
   (map-maybe (fn (f lst)
     (filter id (map f lst))))

   ; cat-maybes: Filter out #f values from list
   ; (cat-maybes '(1 #f 2 #f 3)) => (1 2 3)
   (cat-maybes (fn (lst) (filter id lst)))

   ; -- Tagged Maybe type (alternative to #f-as-None) --

   ; just: Create a Just value
   ; (just 42) => (just 42)
   (just (fn (x) (list 'just x)))

   ; nothing: Create a Nothing value
   ; (nothing) => (nothing)
   (nothing (fn () (list 'nothing)))

   ; nothing?: Check if a value is Nothing
   ; (nothing? (nothing)) => #t
   ; (nothing? (just 5)) => #f
   (nothing? (fn (m) (eq? (car m) 'nothing)))

   ; from-just: Extract value from Just
   ; (from-just (just 42)) => 42
   (from-just (fn (m) (cadr m)))

   ; -- Chunking and windowing --

   ; chunks: Split list into chunks of size n
   ; (chunks 2 '(1 2 3 4 5)) => ((1 2) (3 4) (5))
   (chunks (fix chunks
     (fn (n lst)
       (if (null? lst)
           '()
           (cons (take lst n) (chunks n (drop lst n)))))))

   ; sliding: Sliding window of size n
   ; (sliding 2 '(1 2 3 4)) => ((1 2) (2 3) (3 4))
   (sliding (fix sliding
     (fn (n lst)
       (if (< (length lst) n)
           '()
           (cons (take lst n) (sliding n (cdr lst)))))))

   ; pairs: Consecutive pairs (sliding window of 2)
   ; (pairs '(1 2 3 4)) => ((1 2) (2 3) (3 4))
   (pairs (fn (lst) (sliding 2 lst)))

   ; -- More list utilities --

   ; split-at: Split list at index n
   ; (split-at 2 '(a b c d e)) => ((a b) c d e)
   (split-at (fn (n lst) (cons (take lst n) (drop lst n))))

   ; elem?: Check if element is in list (alias for member returning bool)
   (elem? (fn (x lst) (if (member x lst) #t #f)))

   ; nub: Remove duplicates (keep first occurrence)
   ; Uses eq? for comparison (works for symbols, numbers, etc.)
   (nub (fix nub
     (fn (lst)
       (if (null? lst)
           '()
           (cons (car lst) (nub (filter (fn (x) (not (eq? x (car lst)))) (cdr lst))))))))

   ; intercalate: Insert list between lists and concat
   ; (intercalate '(0) '((1 2) (3 4) (5))) => (1 2 0 3 4 0 5)
   (intercalate (fn (sep lists) (concat (intersperse sep lists))))

   ; transpose: Transpose list of lists (matrix transpose)
   ; (transpose '((1 2 3) (4 5 6))) => ((1 4) (2 5) (3 6))
   (transpose (fix transpose
     (fn (lists)
       (if (or (null? lists) (any null? lists))
           '()
           (cons (map car lists) (transpose (map cdr lists)))))))

   ; -- Numeric utilities --

   ; sum-by: Sum elements after applying function
   ; (sum-by length '("a" "bb" "ccc")) => 6
   (sum-by (fn (f lst) (foldl (fn (acc x) (+ acc (f x))) 0 lst)))

   ; product-by: Product of elements after applying function
   (product-by (fn (f lst) (foldl (fn (acc x) (* acc (f x))) 1 lst)))

   ; average: Average of a list of numbers
   ; (average '(1 2 3 4 5)) => 3
   (average (fn (lst)
     (if (null? lst)
         0
         (/ (sum-list lst) (length lst)))))

   ; even-indices: Get elements at even indices (0, 2, 4, ...)
   ; (even-indices '(a b c d e)) => (a c e)
   (even-indices (fn (lst)
     (filter-indexed (fn (i x) (even? i)) lst)))

   ; odd-indices: Get elements at odd indices (1, 3, 5, ...)
   ; (odd-indices '(a b c d e)) => (b d)
   (odd-indices (fn (lst)
     (filter-indexed (fn (i x) (odd? i)) lst)))

   ; -- Comparison utilities --

   ; sort-by: Sort list by key function (using insertion sort)
   ; (sort-by length '("aaa" "b" "cc")) => ("b" "cc" "aaa")
   (sort-by (fix sort-by
     (fn (key-fn lst)
       (if (null? lst)
           '()
           (let ((x (car lst))
                 (rest (sort-by key-fn (cdr lst))))
             (let ((insert-sorted (fix insert-sorted
                    (fn (elem sorted)
                      (if (null? sorted)
                          (list elem)
                          (if (<= (key-fn elem) (key-fn (car sorted)))
                              (cons elem sorted)
                              (cons (car sorted) (insert-sorted elem (cdr sorted)))))))))
               (insert-sorted x rest)))))))

   ; compare-by: Compare two values by applying key function
   ; Returns -1, 0, or 1
   (compare-by (fn (key-fn a b)
     (let ((ka (key-fn a))
           (kb (key-fn b)))
       (if (< ka kb)
           (- 0 1)
           (if (> ka kb) 1 0)))))

   ; equal-by: Check if two values are equal after applying function
   ; (equal-by length "abc" "xyz") => #t
   (equal-by (fn (f a b) (= (f a) (f b))))

   ; group-by: Group elements by key function
   ; (group-by even? '(1 2 3 4 5 6)) => ((2 4 6) (1 3 5))
   (group-by (fn (key-fn lst)
     (let ((keys (nub (map key-fn lst))))
       (map (fn (k) (filter (fn (x) (eq? (key-fn x) k)) lst)) keys))))

   ; -- More function combinators --

   ; apply-n: Apply function n times to initial value (like repeat-fn but returns all intermediates)
   ; (apply-n inc 3 0) => (0 1 2 3)
   (apply-n (fix apply-n
     (fn (f n x)
       (if (< n 0)
           '()
           (cons x (apply-n f (- n 1) (f x)))))))

   ; until: Apply function until predicate is satisfied
   ; (until (fn (x) (> x 10)) (fn (x) (* x 2)) 1) => 16
   (until (fix until
     (fn (stop? f x)
       (if (stop? x)
           x
           (until stop? f (f x))))))

   ; converge: Apply function until result stops changing
   ; (converge (fn (x) (/ (+ x (/ 2 x)) 2)) 1.0) => ~1.414 (sqrt 2)
   (converge (fix converge
     (fn (f x)
       (let ((next (f x)))
         (if (= next x)
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

   ; frequencies: Count occurrences of each element
   ; (frequencies '(a b a c b a)) => ((a 3) (b 2) (c 1))
   (frequencies (fn (lst)
     (let ((unique (nub lst)))
       (map (fn (x) (list x (count-if (fn (y) (eq? x y)) lst))) unique))))

   ; index-where: Find index of first element matching predicate, or #f
   ; (index-where even? '(1 3 4 5)) => 2
   (index-where (fix index-where
     (fn (p lst)
       (let ((go (fix go
                   (fn (i xs)
                     (if (null? xs)
                         #f
                         (if (p (car xs))
                             i
                             (go (+ i 1) (cdr xs))))))))
         (go 0 lst)))))

   ; indices-where: Find all indices of elements matching predicate
   ; (indices-where even? '(1 2 3 4 5 6)) => (1 3 5)
   (indices-where (fn (p lst)
     (map-maybe (fn (pair) (if (p (cdr pair)) (car pair) #f))
                (map-indexed (fn (i x) (cons i x)) lst))))

   ; last-where: Find last element matching predicate
   ; (last-where even? '(1 2 3 4 5)) => 4
   (last-where (fn (p lst)
     (foldr (fn (x acc) (if (and (not acc) (p x)) x acc)) #f lst)))

   ; update-at: Update element at index with function
   ; (update-at 1 inc '(1 2 3)) => (1 3 3)
   (update-at (fn (idx f lst)
     (map-indexed (fn (i x) (if (= i idx) (f x) x)) lst)))

   ; insert-at: Insert element at index
   ; (insert-at 1 'x '(a b c)) => (a x b c)
   (insert-at (fix insert-at
     (fn (idx elem lst)
       (if (= idx 0)
           (cons elem lst)
           (if (null? lst)
               (list elem)
               (cons (car lst) (insert-at (- idx 1) elem (cdr lst))))))))

   ; remove-at: Remove element at index
   ; (remove-at 1 '(a b c)) => (a c)
   (remove-at (fn (idx lst)
     (map-maybe (fn (pair) (if (= (car pair) idx) #f (cdr pair)))
                (map-indexed (fn (i x) (cons i x)) lst))))

   ; -- String higher-order functions --

   ; string-map: Apply function to each character of string
   ; (string-map char-upcase "hello") => "HELLO"
   (string-map (fn (f str)
     (list->string (map f (string->list str)))))

   ; string-filter: Keep characters matching predicate
   ; (string-filter char-alphabetic? "a1b2c3") => "abc"
   (string-filter (fn (p str)
     (list->string (filter p (string->list str)))))

   ; string-any: Check if any character satisfies predicate
   ; (string-any char-numeric? "abc123") => #t
   (string-any (fn (p str)
     (any p (string->list str))))

   ; string-all: Check if all characters satisfy predicate
   ; (string-all char-alphabetic? "abc") => #t
   (string-all (fn (p str)
     (all p (string->list str))))

   ; string-foldl: Left fold over string characters
   ; (string-foldl (fn (acc c) (+ acc 1)) 0 "hello") => 5
   (string-foldl (fn (f acc str)
     (foldl f acc (string->list str))))

   ; string-foldr: Right fold over string characters
   (string-foldr (fn (f acc str)
     (foldr f acc (string->list str))))

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
   (string-take-while (fn (p str)
     (list->string (take-while p (string->list str)))))

   ; string-drop-while: Drop characters while predicate holds
   (string-drop-while (fn (p str)
     (list->string (drop-while p (string->list str)))))

   ; string-find: Find first character matching predicate
   (string-find (fn (p str)
     (find-if p (string->list str))))

   ; string-count: Count characters matching predicate
   (string-count (fn (p str)
     (count-if p (string->list str))))

   ; string-partition: Split string into matching and non-matching chars
   (string-partition (fn (p str)
     (let ((parts (partition p (string->list str))))
       (cons (list->string (car parts))
             (list->string (cdr parts))))))

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

   ; union: Combine two lists, removing duplicates
   ; (union '(1 2 3) '(2 3 4)) => (1 2 3 4)
   (union (fn (xs ys)
     (nub (append xs ys))))

   ; intersection: Elements in both lists
   ; (intersection '(1 2 3) '(2 3 4)) => (2 3)
   (intersection (fn (xs ys)
     (filter (fn (x) (elem? x ys)) xs)))

   ; difference: Elements in first list but not second
   ; (difference '(1 2 3) '(2 3 4)) => (1)
   (difference (fn (xs ys)
     (filter (fn (x) (not (elem? x ys))) xs)))

   ; symmetric-difference: Elements in either list but not both
   ; (symmetric-difference '(1 2 3) '(2 3 4)) => (1 4)
   (symmetric-difference (fn (xs ys)
     (append (difference xs ys) (difference ys xs))))

   ; subset?: Check if first list is subset of second
   ; (subset? '(1 2) '(1 2 3)) => #t
   (subset? (fn (xs ys)
     (all (fn (x) (elem? x ys)) xs)))

   ; disjoint?: Check if lists have no common elements
   ; (disjoint? '(1 2) '(3 4)) => #t
   (disjoint? (fn (xs ys)
     (null? (intersection xs ys))))

   ; -- Association list utilities --

   ; alist-map: Apply function to all values in alist
   ; (alist-map inc '((a . 1) (b . 2))) => ((a . 2) (b . 3))
   (alist-map (fn (f alist)
     (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

   ; alist-filter: Keep pairs where predicate holds on value
   ; (alist-filter even? '((a . 1) (b . 2) (c . 3))) => ((b . 2))
   (alist-filter (fn (p alist)
     (filter (fn (pair) (p (cdr pair))) alist)))

   ; alist-find: Find first pair where predicate holds on value
   (alist-find (fn (p alist)
     (find-if (fn (pair) (p (cdr pair))) alist)))

   ; alist-update: Update value for key using function
   ; (alist-update 'a inc '((a . 1) (b . 2))) => ((a . 2) (b . 2))
   (alist-update (fn (key f alist)
     (map (fn (pair)
            (if (eq? (car pair) key)
                (cons key (f (cdr pair)))
                pair))
          alist)))

   ; alist-merge: Merge two alists (second takes precedence)
   ; (alist-merge '((a . 1) (b . 2)) '((b . 3) (c . 4))) => ((a . 1) (b . 3) (c . 4))
   (alist-merge (fn (a1 a2)
     (let ((keys1 (map car a1))
           (keys2 (map car a2)))
       (append
         (filter (fn (pair) (not (elem? (car pair) keys2))) a1)
         a2))))

   ; alist-invert: Swap keys and values
   ; (alist-invert '((a . 1) (b . 2))) => ((1 . a) (2 . b))
   (alist-invert (fn (alist)
     (map (fn (pair) (cons (cdr pair) (car pair))) alist)))

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
   (when-let (fn (val f)
     (if val (f val) #f)))

   ; if-let: Like when-let but with else clause
   (if-let (fn (val then-fn else-val)
     (if val (then-fn val) else-val)))

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
   (tap (fn (f x)
     (let ((ignored (f x)))
       x)))

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

   ; rotate-left: Rotate list left by n positions
   ; (rotate-left 2 '(1 2 3 4 5)) => (3 4 5 1 2)
   (rotate-left (fn (n lst)
     (if (null? lst)
         '()
         (let ((len (length lst))
               (n-mod (mod n (length lst))))
           (append (drop lst n-mod) (take lst n-mod))))))

   ; rotate-right: Rotate list right by n positions
   ; (rotate-right 2 '(1 2 3 4 5)) => (4 5 1 2 3)
   (rotate-right (fn (n lst)
     (rotate-left (- (length lst) (mod n (length lst))) lst)))

   ; -- Predicate utilities --

   ; all-equal?: Check if all elements are equal
   ; (all-equal? '(1 1 1)) => #t
   (all-equal? (fn (lst)
     (if (null? lst)
         #t
         (all (fn (x) (eq? x (car lst))) lst))))

   ; sorted?: Check if list is sorted (ascending)
   ; (sorted? '(1 2 3)) => #t
   (sorted? (fix sorted?
     (fn (lst)
       (if (null? lst)
           #t
           (if (null? (cdr lst))
               #t
               (if (<= (car lst) (car (cdr lst)))
                   (sorted? (cdr lst))
                   #f))))))

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
   (tree-depth (fix tree-depth
     (fn (tree)
       (if (pair? tree)
           (+ 1 (foldl max 0 (map tree-depth tree)))
           0))))

   ; tree-size: Count all atoms in nested structure
   ; (tree-size '((1 2) (3 (4 5)))) => 5
   (tree-size (fix tree-size
     (fn (tree)
       (if (null? tree)
           0
           (if (pair? tree)
               (+ (tree-size (car tree)) (tree-size (cdr tree)))
               1)))))

   ; tree-find: Find first atom matching predicate in tree
   ; (tree-find even? '((1 3) (5 (6 7)))) => 6
   (tree-find (fix tree-find
     (fn (p tree)
       (if (null? tree)
           #f
           (if (pair? tree)
               (let ((left (tree-find p (car tree))))
                 (if left left (tree-find p (cdr tree))))
               (if (p tree) tree #f))))))

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
   (partial2 (fn (f x y)
     (fn (z) (f x y z))))

   ; compose-n: Compose multiple functions right-to-left
   ; ((compose-n (list inc inc double)) 5) => 12
   (compose-n (fn (fns)
     (foldr compose id fns)))

   ; pipe-n: Compose multiple functions left-to-right
   ; ((pipe-n (list double inc inc)) 5) => 12
   (pipe-n (fn (fns)
     (foldl (flip compose) id fns)))

   ; memoize-1: Stub for memoization (returns function as-is in pure context)
   ; Note: True memoization requires mutation which isn't well-supported
   (memoize-1 (fn (f) f))

   ; -- Math utilities --

   ; factorial: n! = 1 * 2 * ... * n
   ; (factorial 5) => 120
   (factorial (fix factorial
     (fn (n)
       (if (<= n 1)
           1
           (* n (factorial (- n 1)))))))

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
   (prime? (fix prime?
     (fn (n)
       (if (< n 2)
           #f
           (let ((check (fix check
                   (fn (d)
                     (if (> (* d d) n)
                         #t
                         (if (= 0 (mod n d))
                             #f
                             (check (+ d 1))))))))
             (check 2))))))

   ; divisors: List all divisors of n
   ; (divisors 12) => (1 2 3 4 6 12)
   (divisors (fn (n)
     (filter (fn (d) (= 0 (mod n d)))
             (range 1 (+ n 1)))))

   ; perfect?: Check if number equals sum of proper divisors
   ; (perfect? 6) => #t (1+2+3=6)
   (perfect? (fn (n)
     (= n (- (sum-list (divisors n)) n))))

   ; pow-int: Integer exponentiation
   ; (pow-int 2 10) => 1024
   (pow-int (fix pow-int
     (fn (base exp)
       (if (= exp 0)
           1
           (if (even? exp)
               (let ((half (pow-int base (/ exp 2))))
                 (* half half))
               (* base (pow-int base (- exp 1))))))))

   ; -- Validation utilities --

   ; validate: Apply validator, return value or error
   ; (validate positive? 5) => 5
   ; (validate positive? -5) => #f
   (validate (fn (predicate val)
     (if (predicate val) val #f)))

   ; validate-all: Check all predicates pass
   ; (validate-all (list positive? even?) 4) => 4
   (validate-all (fn (preds val)
     (if (all (fn (p) (p val)) preds) val #f)))

   ; ensure: Like validate but with default
   ; (ensure positive? 0 -5) => 0
   (ensure (fn (predicate default val)
     (if (predicate val) val default)))

   ; -- Debug utilities --

   ; trace: Identity with side-effect printing (pure version just returns)
   (trace (fn (label x) x))

   ; spy: Like trace but shows both label and value
   (spy (fn (label x) x))

   ; assert-eq: Check equality (returns #t or #f)
   (assert-eq (fn (expected actual)
     (= expected actual)))

   ; assert-pred: Check predicate holds
   (assert-pred (fn (predicate val)
     (predicate val)))

   ; -- Logical utilities --

   ; xor: Exclusive or of two booleans
   (xor (fn (a b)
     (if a (not b) b)))

   ; implies: Logical implication (a => b)
   (implies (fn (a b)
     (if a b #t)))

   ; iff: If and only if (biconditional)
   (iff (fn (a b)
     (= a b)))

   ; -- More list utilities --

   ; take-n: Take first n elements (HOF version)
   (take-n (fix take-n
     (fn (n lst)
       (if (<= n 0)
           '()
           (if (null? lst)
               '()
               (cons (car lst) (take-n (- n 1) (cdr lst))))))))

   ; drop-n: Drop first n elements (HOF version)
   (drop-n (fix drop-n
     (fn (n lst)
       (if (<= n 0)
           lst
           (if (null? lst)
               '()
               (drop-n (- n 1) (cdr lst)))))))

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
   (butlast (fn (n lst)
     (let ((len (length lst)))
       (take-n (- len n) lst))))

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
   (assoc-ref (fn (key alist)
     (let ((pair (assoc key alist)))
       (if pair (cdr pair) #f))))

   ; assoc-set: Set value for key (returns new alist)
   (assoc-set (fn (key val alist)
     (cons (cons key val)
           (filter (fn (p) (not (eq? key (car p)))) alist))))

   ; assoc-remove: Remove key from alist
   (assoc-remove (fn (key alist)
     (filter (fn (p) (not (eq? key (car p)))) alist)))

   ; assoc-keys: Get all keys from alist
   (assoc-keys (fn (alist)
     (map car alist)))

   ; assoc-values: Get all values from alist
   (assoc-values (fn (alist)
     (map cdr alist)))

   ; -- More string utilities --

   ; string-reverse: Reverse a string
   (string-reverse (fn (s)
     (list->string (reverse (string->list s)))))

   ; string-empty?: Check if string is empty
   (string-empty? (fn (s)
     (= (string-length s) 0)))

   ; Note: string-contains? is a primitive (haystack needle) - uses Rust's contains()

   ; string-prefix?: Check if string starts with prefix
   (string-prefix? (fn (prefix s)
     (let ((prefix-len (string-length prefix))
           (s-len (string-length s)))
       (if (> prefix-len s-len)
           #f
           (string=? prefix (substring s 0 prefix-len))))))

   ; string-suffix?: Check if string ends with suffix
   (string-suffix? (fn (suffix s)
     (let ((suffix-len (string-length suffix))
           (s-len (string-length s)))
       (if (> suffix-len s-len)
           #f
           (string=? suffix (substring s (- s-len suffix-len) s-len))))))

   ; -- Numeric sequences --

   ; iota: Generate list of n integers starting from start
   (iota (fix iota
     (fn (n start)
       (if (<= n 0)
           '()
           (cons start (iota (- n 1) (+ start 1)))))))

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
   (constantly (fn (x)
     (fn (y) x)))

   ; identity: Identity function (same as id, but clearer name)
   (identity (fn (x) x))

   ; first: Get first element (same as car)
   (first car)

   ; second: Get second element
   (second (fn (lst) (car (cdr lst))))

   ; third: Get third element
   (third (fn (lst) (car (cdr (cdr lst)))))

   ; rest: Get rest of list (same as cdr)
   (rest cdr)

   ; empty?: Check if list is empty (same as null?)
   (empty? null?)

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
   (cadr (fn (lst) (car (cdr lst))))

   ; caar: (car (car x))
   (caar (fn (lst) (car (car lst))))

   ; cddr: (cdr (cdr x))
   (cddr (fn (lst) (cdr (cdr lst))))

   ; cdar: (cdr (car x))
   (cdar (fn (lst) (cdr (car lst))))

   ; caddr: (car (cdr (cdr x)))
   (caddr (fn (lst) (car (cdr (cdr lst)))))

   ; caaar: (car (car (car x)))
   (caaar (fn (lst) (car (car (car lst)))))

   ; caadr: (car (car (cdr x)))
   (caadr (fn (lst) (car (car (cdr lst)))))

   ; cadar: (car (cdr (car x)))
   (cadar (fn (lst) (car (cdr (car lst)))))

   ; cdaar: (cdr (car (car x)))
   (cdaar (fn (lst) (cdr (car (car lst)))))

   ; cdadr: (cdr (car (cdr x)))
   (cdadr (fn (lst) (cdr (car (cdr lst)))))

   ; cddar: (cdr (cdr (car x)))
   (cddar (fn (lst) (cdr (cdr (car lst)))))

   ; cdddr: (cdr (cdr (cdr x)))
   (cdddr (fn (lst) (cdr (cdr (cdr lst)))))

   ; cadddr: (car (cdr (cdr (cdr x)))) - fourth element
   (cadddr (fn (lst) (car (cdr (cdr (cdr lst))))))

   ; -- More list utilities --

   ; fourth: Get fourth element
   (fourth (fn (lst) (car (cdr (cdr (cdr lst))))))

   ; fifth: Get fifth element
   (fifth (fn (lst) (car (cdr (cdr (cdr (cdr lst)))))))

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

   ; nand: Not and
   (nand (fn (a b)
     (not (if a b #f))))

   ; nor: Not or
   (nor (fn (a b)
     (not (if a #t b))))

   ; -- More numeric utilities --

   ; square: x^2
   (square (fn (x) (* x x)))

   ; cube: x^3
   (cube (fn (x) (* x x x)))

   ; abs-diff: Absolute difference
   (abs-diff (fn (a b)
     (if (> a b) (- a b) (- b a))))

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
   (for-each (fn (f lst)
     (foldl (fn (acc x) (f x)) '() lst)))

   ; reduce: foldl with first element as initial
   (reduce (fn (f lst)
     (if (null? lst)
         '()
         (foldl f (car lst) (cdr lst)))))

   ; reduce-right: foldr with last element as initial
   (reduce-right (fn (f lst)
     (if (null? lst)
         '()
         (foldr f (last lst) (init lst)))))

   ; every?: Alias for all (Scheme naming)
   (every? all)

   ; some?: Alias for any (Scheme naming)
   (some? any)

   ; filter-not: Keep elements that don't match
   (filter-not (fn (f lst)
     (filter (fn (x) (not (f x))) lst)))

   ; remove: Remove first occurrence
   (remove (fix remove
     (fn (x lst)
       (if (null? lst)
           '()
           (if (= x (car lst))
               (cdr lst)
               (cons (car lst) (remove x (cdr lst))))))))

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
   (fold-left foldl)

   ; fold-right: Alias for foldr (Scheme naming)
   (fold-right foldr)

   ; member: Check membership (like elem but returns tail or #f)
   (member (fix member
     (fn (x lst)
       (if (null? lst)
           #f
           (if (eq? x (car lst))
               lst
               (member x (cdr lst)))))))

   ; member?: Boolean membership check
   (member? (fn (x lst)
     (if (member x lst) #t #f)))

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
   (left (fn (x) (list 'left x)))

   ; right: Create right value (success case)
   (right (fn (x) (list 'right x)))

   ; left?: Check if left
   (left? (fn (e) (eq? (car e) 'left)))

   ; right?: Check if right
   (right? (fn (e) (eq? (car e) 'right)))

   ; from-left: Extract left value
   (from-left (fn (e) (cadr e)))

   ; from-right: Extract right value
   (from-right (fn (e) (cadr e)))

   ; either: Apply one of two functions based on Either
   (either (fn (left-fn right-fn e)
     (if (left? e)
         (left-fn (from-left e))
         (right-fn (from-right e)))))

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
   (take-right (fn (n lst)
     (drop-n (- (length lst) n) lst)))

   ; drop-right: Drop n elements from end
   (drop-right (fn (n lst)
     (take-n (- (length lst) n) lst)))

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

   ; mean: Arithmetic mean
   (mean (fn (lst)
     (if (null? lst)
         0
         (/ (sum-list lst) (length lst)))))

   ; variance: Sample variance
   (variance (fn (lst)
     (if (null? lst)
         0
         (let ((m (mean lst))
               (n (length lst)))
           (/ (sum-list (map (fn (x) (square (- x m))) lst))
              (- n 1))))))

   ; median: Middle value (for sorted list)
   (median (fn (lst)
     (if (null? lst)
         0
         (let ((n (length lst)))
           (let ((mid (/ n 2)))
             (if (even? n)
                 (/ (+ (nth (- mid 1) lst) (nth mid lst)) 2)
                 (nth mid lst)))))))

   ; clamp-val: Clamp value to range [lo, hi]
   (clamp-val (fn (lo hi x)
     (if (< x lo) lo (if (> x hi) hi x))))

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
   (group-runs group-consecutive)

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
   (snoc (fn (lst x) (append lst (list x))))

   ; single?: Check if list has exactly one element
   (single? singleton?)

   ; drop-last: Drop n elements from end (alias)
   (drop-last drop-right)

   ; take-last: Take n elements from end (alias)
   (take-last take-right)

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
   (count-occurrences (fn (x lst)
     (count-if (fn (y) (eq? x y)) lst)))

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
   (insert-sorted-by (fix insert-sorted-by
     (fn (cmp x lst)
       (if (null? lst)
           (list x)
           (if (<= (cmp x) (cmp (car lst)))
               (cons x lst)
               (cons (car lst) (insert-sorted-by cmp x (cdr lst))))))))

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
   (quotient-remainder (fn (a b)
     (list (quotient a b) (remainder a b))))

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
   (from-either (fn (default e)
     (if (left? e)
         default
         (from-right e))))

   ; partition-eithers: Separate list of Eithers into lefts and rights
   (partition-eithers (fn (lst)
     (let ((lefts (filter left? lst))
           (rights (filter right? lst)))
       (list (map from-left lefts)
             (map from-right rights)))))

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
   (maybe-to-list (fn (m)
     (if (nothing? m)
         '()
         (list (from-just m)))))

   ; list-to-maybe: Convert list to Maybe (Nothing if empty)
   (list-to-maybe (fn (lst)
     (if (null? lst)
         (nothing)
         (just (car lst)))))

   ; catMaybes: Filter and extract Just values from list of Maybes
   (cat-maybes-fn (fn (lst)
     (map from-just (filter (fn (m) (not (nothing? m))) lst))))

   ; -- More string HOFs (using primitives) --

   ; string-words-fn: Split string on whitespace (alias using primitive)
   (string-words-fn (fn (s) (string-words s)))

   ; string-lines-fn: Split string on newlines (alias using primitive)
   (string-lines-fn (fn (s) (string-lines s)))

   ; string-join-fn: Join strings with separator (alias using primitive)
   (string-join-fn (fn (strs sep) (string-join strs sep)))

   ; string-trim-fn: Trim whitespace from string (alias using primitive)
   (string-trim-fn (fn (s) (string-trim s)))

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

   ; split-when: Split list when predicate is true
   (split-when (fix split-when
     (fn (f lst)
       (if (null? lst)
           '(())
           (if (f (car lst))
               (cons '() (split-when f (cdr lst)))
               (let ((rest (split-when f (cdr lst))))
                 (cons (cons (car lst) (car rest))
                       (cdr rest))))))))

   ; before: Get elements before first match
   (before (fn (f lst)
     (car (span (complement f) lst))))

   ; after: Get elements after first match (excluding match)
   (after (fn (f lst)
     (let ((tail (drop-while (complement f) lst)))
       (if (null? tail) '() (cdr tail)))))

   ; window: Get sliding windows of size n
   (window sliding)

   ; adjacent-pairs: Get all adjacent pairs
   (adjacent-pairs (fn (lst)
     (if (null? lst)
         '()
         (if (null? (cdr lst))
             '()
             (zip lst (cdr lst))))))

   ; with-index: Pair each element with its index
   (with-index (fn (lst)
     (zip-with-index lst)))

   ; enumerate: Alias for with-index
   (enumerate with-index)

   ; -- Control flow utilities --

   ; when-not: Execute body when condition is false
   (when-not (fn (cond body)
     (if cond #f body)))

   ; if-not: Inverted if
   (if-not (fn (cond then-val else-val)
     (if cond else-val then-val)))

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
   (plist-get (fix plist-get
     (fn (key plist)
       (if (null? plist)
           #f
           (if (null? (cdr plist))
               #f
               (if (eq? key (car plist))
                   (cadr plist)
                   (plist-get key (cddr plist))))))))

   ; plist-set: Set value in property list (returns new plist)
   (plist-set (fix plist-set
     (fn (key val plist)
       (if (null? plist)
           (list key val)
           (if (null? (cdr plist))
               (list key val)
               (if (eq? key (car plist))
                   (cons key (cons val (cddr plist)))
                   (cons (car plist)
                         (cons (cadr plist)
                               (plist-set key val (cddr plist))))))))))

   ; plist-remove: Remove key from property list
   (plist-remove (fix plist-remove
     (fn (key plist)
       (if (null? plist)
           '()
           (if (null? (cdr plist))
               plist
               (if (eq? key (car plist))
                   (cddr plist)
                   (cons (car plist)
                         (cons (cadr plist)
                               (plist-remove key (cddr plist))))))))))

   ; plist-keys: Get all keys from property list
   (plist-keys (fix plist-keys
     (fn (plist)
       (if (null? plist)
           '()
           (if (null? (cdr plist))
               '()
               (cons (car plist) (plist-keys (cddr plist))))))))

   ; plist-values: Get all values from property list
   (plist-values (fix plist-values
     (fn (plist)
       (if (null? plist)
           '()
           (if (null? (cdr plist))
               '()
               (cons (cadr plist) (plist-values (cddr plist))))))))

   ; plist->alist: Convert property list to association list
   (plist->alist (fix plist->alist
     (fn (plist)
       (if (null? plist)
           '()
           (if (null? (cdr plist))
               '()
               (cons (cons (car plist) (cadr plist))
                     (plist->alist (cddr plist))))))))

   ; alist->plist: Convert association list to property list
   (alist->plist (fn (alist)
     (concat (map (fn (pair) (list (car pair) (cdr pair))) alist))))

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

   ; iterate-n: Apply function n times
   (iterate-n (fix iterate-n
     (fn (n f x)
       (if (<= n 0)
           x
           (iterate-n (- n 1) f (f x))))))

   ; generate: Generate list using function and seed
   (generate (fix generate
     (fn (n f seed)
       (if (<= n 0)
           '()
           (cons seed (generate (- n 1) f (f seed)))))))

   ; unfold-right: Right unfold
   (unfold-right (fix unfold-right
     (fn (stop? f seed)
       (if (stop? seed)
           '()
           (snoc (unfold-right stop? f (f seed)) seed)))))

   ; -- Accumulator patterns --

   ; scan-right: Right scan (like scanl but from right)
   (scan-right (fix scan-right
     (fn (f init lst)
       (if (null? lst)
           (list init)
           (let ((rest (scan-right f init (cdr lst))))
             (cons (f (car lst) (car rest)) rest))))))

   ; running-max: Running maximum
   (running-max (fn (lst)
     (if (null? lst)
         '()
         (scanl max (car lst) (cdr lst)))))

   ; running-min: Running minimum
   (running-min (fn (lst)
     (if (null? lst)
         '()
         (scanl min (car lst) (cdr lst)))))

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

   ; primes-up-to: Generate primes up to n using sieve
   (primes-up-to (fn (n)
     (let ((sieve (fix sieve
             (fn (candidates)
               (if (null? candidates)
                   '()
                   (let ((p (car candidates)))
                     (cons p
                           (sieve (filter (fn (x) (not (= 0 (mod x p))))
                                          (cdr candidates))))))))))
       (sieve (range 2 (+ n 1))))))

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
   (string-repeat-fn (fn (n s)
     (string-repeat s n)))

   ; string-pad-left: Pad string on left to width
   (string-pad-left (fn (width pad-char s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append (make-string (- width len) pad-char) s)))))

   ; string-pad-right: Pad string on right to width
   (string-pad-right (fn (width pad-char s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append s (make-string (- width len) pad-char))))))

   ; string-center-fn: Center string to width
   (string-center-fn (fn (width pad-char s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (let ((total-pad (- width len)))
             (let ((left-pad (/ total-pad 2)))
               (let ((right-pad (- total-pad left-pad)))
                 (string-append
                   (make-string left-pad pad-char)
                   s
                   (make-string right-pad pad-char)))))))))

   ; -- Safe operations --

   ; safe-car: Car with default for empty list
   (safe-car (fn (default lst)
     (if (null? lst) default (car lst))))

   ; safe-cdr: Cdr with default for empty list
   (safe-cdr (fn (default lst)
     (if (null? lst) default (cdr lst))))

   ; safe-head: Head with default
   (safe-head safe-car)

   ; safe-tail: Tail with default
   (safe-tail safe-cdr)

   ; safe-nth: Nth with default for out of bounds
   (safe-nth nth-safe)

   ; safe-div: Division with default for divide by zero
   (safe-div (fn (default a b)
     (if (= b 0) default (/ a b))))

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
   (conjoin (fn (preds)
     (fn (x) (all (fn (p) (p x)) preds))))

   ; disjoin: Combine predicates with or
   (disjoin (fn (preds)
     (fn (x) (any (fn (p) (p x)) preds))))

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
   (ok right)

   ; err: Create error result (alias for left)
   (err left)

   ; ok?: Check if result is ok
   (ok? right?)

   ; err?: Check if result is error
   (err? left?)

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
   (insertion-sort (fn (lst)
     (foldl (fn (sorted x) (insert-sorted x sorted)) '() lst)))

   ; insertion-sort-by: Sort by key function
   (insertion-sort-by (fn (key-fn lst)
     (foldl (fn (sorted x) (insert-sorted-by key-fn x sorted)) '() lst)))

   ; merge-sort: Sort list using merge sort (stable, O(n log n))
   (merge-sort (fix merge-sort
     (fn (lst)
       (if (null? lst)
           '()
           (if (null? (cdr lst))
               lst
               (let ((mid (/ (length lst) 2)))
                 (let ((left (take-n mid lst))
                       (right (drop-n mid lst)))
                   (merge-sorted (merge-sort left) (merge-sort right)))))))))

   ; sort-descending: Sort in descending order
   (sort-descending (fn (lst)
     (reverse (sort lst))))

   ; sort-by-descending: Sort by key in descending order
   (sort-by-descending (fn (f lst)
     (reverse (sort-by f lst))))

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
   (binary-search (fix binary-search
     (fn (x sorted-lst)
       (let ((helper (fix helper
               (fn (lo hi)
                 (if (> lo hi)
                     #f
                     (let ((mid (/ (+ lo hi) 2)))
                       (let ((mid-val (list-ref sorted-lst mid)))
                         (if (= x mid-val)
                             mid
                             (if (< x mid-val)
                                 (helper lo (- mid 1))
                                 (helper (+ mid 1) hi))))))))))
         (helper 0 (- (length sorted-lst) 1))))))

   ; lower-bound: Find first position where element could be inserted
   (lower-bound (fix lower-bound
     (fn (x sorted-lst)
       (let ((helper (fix helper
               (fn (lst idx)
                 (if (null? lst)
                     idx
                     (if (< (car lst) x)
                         (helper (cdr lst) (+ idx 1))
                         idx))))))
         (helper sorted-lst 0)))))

   ; upper-bound: Find last position where element could be inserted
   (upper-bound (fix upper-bound
     (fn (x sorted-lst)
       (let ((helper (fix helper
               (fn (lst idx)
                 (if (null? lst)
                     idx
                     (if (<= (car lst) x)
                         (helper (cdr lst) (+ idx 1))
                         idx))))))
         (helper sorted-lst 0)))))

   ; -- More list utilities --

   ; rotate: Rotate list by n positions
   (rotate-list (fn (n lst)
     (if (null? lst)
         '()
         (let ((len (length lst)))
           (let ((n-normalized (mod n len)))
             (append (drop-n n-normalized lst) (take-n n-normalized lst)))))))

   ; shuffle: Deterministic shuffle based on seed (not truly random)
   (shuffle (fn (seed lst)
     (let ((pairs (zip (map (fn (x) (mod (* x seed) 1000003)) (iota (length lst) 1)) lst)))
       (map cadr (sort-by car pairs)))))

   ; dedup-consecutive: Remove consecutive duplicates (like Unix uniq)
   (dedup-consecutive (fix dedup-consecutive
     (fn (lst)
       (if (null? lst)
           '()
           (if (null? (cdr lst))
               lst
               (if (eq? (car lst) (cadr lst))
                   (dedup-consecutive (cdr lst))
                   (cons (car lst) (dedup-consecutive (cdr lst)))))))))

   ; run-length-encode: Encode consecutive runs
   (run-length-encode (fn (lst)
     (map (fn (group) (list (car group) (length group)))
          (group-consecutive lst))))

   ; run-length-decode: Decode run-length encoding
   (run-length-decode (fn (encoded)
     (concat (map (fn (pair) (replicate (cadr pair) (car pair))) encoded))))

   ; -- Statistics utilities --

   ; percentile: Get nth percentile of list
   (percentile (fn (p lst)
     (let ((sorted (sort lst)))
       (let ((idx (/ (* p (- (length sorted) 1)) 100)))
         (list-ref sorted idx)))))

   ; quartiles: Get Q1, Q2 (median), Q3
   (quartiles (fn (lst)
     (list (percentile 25 lst)
           (percentile 50 lst)
           (percentile 75 lst))))

   ; interquartile-range: Q3 - Q1
   (interquartile-range (fn (lst)
     (let ((qs (quartiles lst)))
       (- (caddr qs) (car qs)))))

   ; z-score: Calculate z-score of value
   (z-score (fn (x lst)
     (let ((m (mean lst)))
       (let ((std (sqrt (variance lst))))
         (if (= std 0) 0 (/ (- x m) std))))))

   ; normalize-list: Normalize list to [0, 1] range
   (normalize-list (fn (lst)
     (let ((mn (apply min lst))
           (mx (apply max lst)))
       (let ((range (- mx mn)))
         (if (= range 0)
             (replicate (length lst) 0)
             (map (fn (x) (/ (- x mn) range)) lst))))))

   ; standardize: Standardize list (z-scores)
   (standardize (fn (lst)
     (let ((m (mean lst)))
       (let ((std (sqrt (variance lst))))
         (if (= std 0)
             (replicate (length lst) 0)
             (map (fn (x) (/ (- x m) std)) lst))))))

   ; covariance: Calculate covariance of two lists
   (covariance (fn (xs ys)
     (let ((mx (mean xs))
           (my (mean ys))
           (n (length xs)))
       (/ (sum-list (zip-with * (map (fn (x) (- x mx)) xs)
                                 (map (fn (y) (- y my)) ys)))
          (- n 1)))))

   ; correlation: Calculate Pearson correlation coefficient
   (correlation (fn (xs ys)
     (let ((cov (covariance xs ys))
           (sx (sqrt (variance xs)))
           (sy (sqrt (variance ys))))
       (if (or (= sx 0) (= sy 0))
           0
           (/ cov (* sx sy))))))

   ; -- Matrix operations (lists of lists) --

   ; matrix-ref: Get element at row, col
   (matrix-ref (fn (m row col)
     (list-ref (list-ref m row) col)))

   ; matrix-rows: Get number of rows
   (matrix-rows length)

   ; matrix-cols: Get number of columns
   (matrix-cols (fn (m)
     (if (null? m) 0 (length (car m)))))

   ; matrix-row: Get row at index
   (matrix-row list-ref)

   ; matrix-col: Get column at index
   (matrix-col (fn (m col)
     (map (fn (row) (list-ref row col)) m)))

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
   (Y id)

   ; memoize: Placeholder for memoization (requires mutation, not fully supported)
   ; Use memoize-1 instead for basic single-value memoization
   (memoize id)

   ; trampoline: Execute trampolined function
   (trampoline (fix trampoline
     (fn (f)
       (if (procedure? f)
           (trampoline (f))
           f))))

   ; -- Tree utilities --

   ; tree-leaves: Get all leaves of tree
   (tree-leaves (fix tree-leaves
     (fn (tree)
       (if (not (pair? tree))
           (list tree)
           (concat (map tree-leaves tree))))))

   ; tree-count: Count nodes in tree
   (tree-count (fix tree-count
     (fn (tree)
       (if (not (pair? tree))
           1
           (+ 1 (sum-list (map tree-count tree)))))))

   ; tree-height: Alias for tree-depth
   (tree-height tree-depth)

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
   (number->string-padded (fn (width n)
     (string-pad-left width #\0 (number->string n))))

   ; format-list: Format list as string with separator
   (format-list (fn (sep lst)
     (string-join (map (fn (x) (if (string? x) x (number->string x))) lst) sep)))

   ; format-table: Format list of lists as table
   (format-table (fn (rows)
     (string-join (map (fn (row) (format-list " " row)) rows) "\n")))

   ; pluralize: Simple pluralization
   (pluralize (fn (n singular plural)
     (if (= n 1) singular plural)))

   ; -- Boolean utilities --

   ; bool->int: Convert boolean to integer
   (bool->int (fn (b) (if b 1 0)))

   ; int->bool: Convert integer to boolean
   (int->bool (fn (n) (not (= n 0))))

   ; -- List predicates --

   ; sublist?: Check if xs is a sublist of ys
   (sublist? (fix sublist?
     (fn (xs ys)
       (if (null? xs)
           #t
           (if (null? ys)
               #f
               (if (eq? (car xs) (car ys))
                   (sublist? (cdr xs) (cdr ys))
                   (sublist? xs (cdr ys))))))))

   ; prefix?: Check if xs is a prefix of ys
   (prefix? (fix prefix?
     (fn (xs ys)
       (if (null? xs)
           #t
           (if (null? ys)
               #f
               (if (eq? (car xs) (car ys))
                   (prefix? (cdr xs) (cdr ys))
                   #f))))))

   ; suffix?: Check if xs is a suffix of ys
   (suffix? (fn (xs ys)
     (prefix? (reverse xs) (reverse ys))))

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
   (dict-new (fn () '()))

   ; dict-set: Set key-value pair (returns new dict)
   (dict-set (fn (d k v)
     (cons (cons k v) (dict-remove d k))))

   ; dict-get: Get value for key, or default
   (dict-get (fn (d k default)
     (let ((entry (assoc k d)))
       (if entry (cdr entry) default))))

   ; dict-get-in: Get nested value using path of keys
   (dict-get-in (fix dict-get-in
     (fn (d keys default)
       (if (null? keys)
           d
           (let ((entry (assoc (car keys) d)))
             (if entry
                 (dict-get-in (cdr entry) (cdr keys) default)
                 default))))))

   ; dict-remove: Remove key from dictionary
   (dict-remove (fn (d k)
     (filter (fn (pair) (not (eq? (car pair) k))) d)))

   ; dict-has?: Check if key exists
   (dict-has? (fn (d k)
     (if (assoc k d) #t #f)))

   ; dict-keys: Get all keys
   (dict-keys (fn (d) (map car d)))

   ; dict-values: Get all values
   (dict-values (fn (d) (map cdr d)))

   ; dict-size: Get number of entries
   (dict-size length)

   ; dict-empty?: Check if dictionary is empty
   (dict-empty? null?)

   ; dict-update: Update value with function
   (dict-update (fn (d k f default)
     (dict-set d k (f (dict-get d k default)))))

   ; dict-merge: Merge two dictionaries (second wins on conflict)
   (dict-merge (fn (d1 d2)
     (foldl (fn (acc pair) (dict-set acc (car pair) (cdr pair))) d1 d2)))

   ; dict-filter: Filter dictionary by predicate on (key . value)
   (dict-filter filter)

   ; dict-map-values: Map function over values
   (dict-map-values (fn (f d)
     (map (fn (pair) (cons (car pair) (f (cdr pair)))) d)))

   ; dict-from-lists: Create dict from key list and value list
   (dict-from-lists (fn (keys vals)
     (zip keys vals)))

   ; dict-to-list: Convert dict to list of (key value) pairs
   (dict-to-list (fn (d)
     (map (fn (pair) (list (car pair) (cdr pair))) d)))

   ; dict-invert: Swap keys and values
   (dict-invert (fn (d)
     (map (fn (pair) (cons (cdr pair) (car pair))) d)))

   ; ============================================
   ; Set Operations (list-based)
   ; ============================================

   ; set-new: Create empty set (use as thunk: (set-new) returns empty list)
   ; Or use '() directly for an empty set
   (set-new (const '()))

   ; set-add: Add element to set
   (set-add (fn (s x)
     (if (member? x s) s (cons x s))))

   ; set-remove: Remove element from set
   (set-remove remove)

   ; set-member?: Check if element is in set
   (set-member? member?)

   ; set-size: Get number of elements
   (set-size length)

   ; set-empty?: Check if set is empty
   (set-empty? null?)

   ; set-from-list: Create set from list (removes duplicates)
   (set-from-list nub)

   ; set-to-list: Convert set to list
   (set-to-list id)

   ; set-union: Union of two sets
   (set-union union)

   ; set-intersection: Intersection of two sets
   (set-intersection intersection)

   ; set-difference: Difference of two sets
   (set-difference difference)

   ; set-symmetric-difference: Symmetric difference
   (set-symmetric-difference symmetric-difference)

   ; set-subset?: Check if s1 is subset of s2
   (set-subset? subset?)

   ; set-equal?: Check if two sets have same elements
   (set-equal? (fn (s1 s2)
     (and (set-subset? s1 s2) (set-subset? s2 s1))))

   ; ============================================
   ; More Numeric Utilities
   ; ============================================

   ; mod-exp: Modular exponentiation (base^exp mod m)
   (mod-exp (fix mod-exp
     (fn (base exp m)
       (if (= exp 0)
           1
           (if (= (mod exp 2) 0)
               (let ((half (mod-exp base (/ exp 2) m)))
                 (mod (* half half) m))
               (mod (* base (mod-exp base (- exp 1) m)) m))))))

   ; mod-inverse: Modular multiplicative inverse (extended Euclidean)
   (mod-inverse (fn (a m)
     (let ((egcd (fix egcd
             (fn (a b)
               (if (= b 0)
                   (list a 1 0)
                   (let ((result (egcd b (mod a b))))
                     (list (car result)
                           (caddr result)
                           (- (cadr result) (* (/ a b) (caddr result))))))))))
       (let ((result (egcd a m)))
         (if (= (car result) 1)
             (mod (+ (cadr result) m) m)
             #f)))))

   ; coprime?: Check if two numbers are coprime
   (coprime? (fn (a b) (= (gcd a b) 1)))

   ; totient: Euler's totient function (count of coprimes less than n)
   (totient (fn (n)
     (length (filter (fn (k) (coprime? k n)) (iota n 1)))))

   ; digits: Get list of digits of a number
   (digits (fix digits
     (fn (n)
       (if (< n 10)
           (list n)
           (append (digits (/ n 10)) (list (mod n 10)))))))

   ; from-digits: Convert list of digits to number
   (from-digits (fn (ds)
     (foldl (fn (acc d) (+ (* acc 10) d)) 0 ds)))

   ; digit-sum: Sum of digits
   (digit-sum (fn (n) (sum-list (digits n))))

   ; digit-count: Number of digits
   (digit-count (fn (n) (length (digits n))))

   ; reverse-number: Reverse digits of a number
   (reverse-number (fn (n) (from-digits (reverse (digits n)))))

   ; palindrome-number?: Check if number is a palindrome
   (palindrome-number? (fn (n) (= n (reverse-number n))))

   ; is-power-of-2?: Check if n is a power of 2
   (is-power-of-2? (fn (n)
     (and (> n 0) (= (mod n 2) 0) (or (= n 1) (is-power-of-2? (/ n 2))))))

   ; next-power-of-2: Find next power of 2 >= n
   (next-power-of-2 (fix next-power-of-2
     (fn (n)
       (if (<= n 1) 1
           (let ((helper (fix helper
                   (fn (p) (if (>= p n) p (helper (* p 2)))))))
             (helper 1))))))

   ; log2-int: Integer log base 2 (floor)
   (log2-int (fix log2-int
     (fn (n)
       (if (<= n 1) 0 (+ 1 (log2-int (/ n 2)))))))

   ; ============================================
   ; Combinatorics
   ; ============================================

   ; binomial: Binomial coefficient (n choose k)
   (binomial (fn (n k)
     (if (or (< k 0) (> k n))
         0
         (if (or (= k 0) (= k n))
             1
             (/ (factorial n) (* (factorial k) (factorial (- n k))))))))

   ; permutations-count: Number of permutations P(n,k)
   (permutations-count (fn (n k)
     (/ (factorial n) (factorial (- n k)))))

   ; catalan: Catalan number
   (catalan (fn (n)
     (/ (binomial (* 2 n) n) (+ n 1))))

   ; triangular: Triangular number
   (triangular (fn (n) (/ (* n (+ n 1)) 2)))

   ; is-triangular?: Check if number is triangular
   (is-triangular? (fn (n)
     (let ((k (floor (sqrt (* 2 n)))))
       (= n (triangular k)))))

   ; square-number: Square number
   (square-number (fn (n) (* n n)))

   ; is-square?: Check if number is a perfect square
   (is-square? (fn (n)
     (let ((k (floor (sqrt n))))
       (= n (* k k)))))

   ; pentagonal: Pentagonal number
   (pentagonal (fn (n) (/ (* n (- (* 3 n) 1)) 2)))

   ; hexagonal: Hexagonal number
   (hexagonal (fn (n) (* n (- (* 2 n) 1))))

   ; ============================================
   ; More List Utilities
   ; ============================================

   ; cartesian-product: Cartesian product of two lists
   (cartesian-product (fn (xs ys)
     (concat (map (fn (x) (map (fn (y) (list x y)) ys)) xs))))

   ; power-set: All subsets of a list
   (power-set (fix power-set
     (fn (lst)
       (if (null? lst)
           (list '())
           (let ((rest (power-set (cdr lst))))
             (append rest (map (fn (s) (cons (car lst) s)) rest)))))))

   ; permutations: All permutations of a list
   (permutations (fix permutations
     (fn (lst)
       (if (null? lst)
           (list '())
           (concat (map (fn (x)
                          (map (fn (p) (cons x p))
                               (permutations (remove x lst))))
                        lst))))))

   ; combinations: All k-combinations of a list
   (combinations (fix combinations
     (fn (k lst)
       (if (= k 0)
           (list '())
           (if (null? lst)
               '()
               (append
                 (map (fn (c) (cons (car lst) c))
                      (combinations (- k 1) (cdr lst)))
                 (combinations k (cdr lst))))))))

   ; list-product: Cartesian product of list of lists
   (list-product (fix list-product
     (fn (lists)
       (if (null? lists)
           (list '())
           (concat (map (fn (x)
                          (map (fn (rest) (cons x rest))
                               (list-product (cdr lists))))
                        (car lists)))))))

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
   (mode (fn (lst)
     (let ((freqs (frequencies lst)))
       (car (max-by cadr freqs)))))

   ; majority: Element appearing more than n/2 times (or #f)
   (majority (fn (lst)
     (let ((n (length lst))
           (freqs (frequencies lst)))
       (let ((candidates (filter (fn (p) (> (cadr p) (/ n 2))) freqs)))
         (if (null? candidates) #f (caar candidates))))))

   ; ============================================
   ; String Utilities (additional)
   ; ============================================

   ; string-capitalize: Capitalize first letter
   (string-capitalize (fn (s)
     (if (string-empty? s)
         s
         (string-append (string-upcase (substring s 0 1))
                        (substring s 1 (string-length s))))))

   ; string-title-case: Capitalize first letter of each word
   (string-title-case (fn (s)
     (string-join (map string-capitalize (string-split s " ")) " ")))

   ; string-count-char: Count occurrences of character in string
   (string-count-char (fn (c s)
     (length (filter (fn (ch) (eq? ch c)) (string->list s)))))

   ; string-replace-char: Replace all occurrences of character
   (string-replace-char (fn (old new s)
     (list->string (map (fn (c) (if (eq? c old) new c)) (string->list s)))))

   ; string-squeeze: Remove consecutive duplicate characters
   (string-squeeze (fn (s)
     (list->string (dedup-consecutive (string->list s)))))

   ; string-rotate: Rotate string by n positions
   (string-rotate (fn (n s)
     (let ((chars (string->list s)))
       (list->string (rotate-list n chars)))))

   ; string-interleave: Interleave two strings
   (string-interleave (fn (s1 s2)
     (list->string (concat (zip-with list (string->list s1) (string->list s2))))))

   ; ============================================
   ; Predicate Utilities
   ; ============================================

   ; all-same?: Check if all elements are equal
   (all-same? all-equal?)

   ; all-different?: Check if all elements are different
   (all-different? (fn (lst)
     (= (length lst) (length (nub lst)))))

   ; monotonic-increasing?: Check if list is monotonically increasing
   (monotonic-increasing? (fn (lst)
     (if (null? lst) #t
         (if (null? (cdr lst)) #t
             (and (<= (car lst) (cadr lst))
                  (monotonic-increasing? (cdr lst)))))))

   ; monotonic-decreasing?: Check if list is monotonically decreasing
   (monotonic-decreasing? (fn (lst)
     (if (null? lst) #t
         (if (null? (cdr lst)) #t
             (and (>= (car lst) (cadr lst))
                  (monotonic-decreasing? (cdr lst)))))))

   ; strictly-increasing?: Check if list is strictly increasing
   (strictly-increasing? (fn (lst)
     (if (null? lst) #t
         (if (null? (cdr lst)) #t
             (and (< (car lst) (cadr lst))
                  (strictly-increasing? (cdr lst)))))))

   ; strictly-decreasing?: Check if list is strictly decreasing
   (strictly-decreasing? (fn (lst)
     (if (null? lst) #t
         (if (null? (cdr lst)) #t
             (and (> (car lst) (cadr lst))
                  (strictly-decreasing? (cdr lst)))))))

   ; ============================================
   ; Sequence Generators
   ; ============================================

   ; naturals: Generate natural numbers from 0 to n-1
   (naturals (fn (n) (iota n 0)))

   ; evens: Generate even numbers from 0 to 2*(n-1)
   (evens (fn (n) (map (fn (x) (* 2 x)) (iota n 0))))

   ; odds: Generate odd numbers from 1 to 2*n-1
   (odds (fn (n) (map (fn (x) (+ (* 2 x) 1)) (iota n 0))))

   ; squares: Generate square numbers
   (squares (fn (n) (map (fn (x) (* x x)) (iota n 0))))

   ; cubes: Generate cube numbers
   (cubes (fn (n) (map (fn (x) (* x x x)) (iota n 0))))

   ; powers-of: Generate powers of base
   (powers-of (fn (base n)
     (map (fn (i) (pow-int base i)) (iota n 0))))

   ; factorials-up-to: Generate factorials up to n!
   (factorials-up-to (fn (n)
     (map factorial (iota (+ n 1) 0))))

   ; triangular-numbers: Generate triangular numbers
   (triangular-numbers (fn (n)
     (map triangular (iota n 1))))

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
   (fold-tree (fix fold-tree
     (fn (leaf-fn node-fn tree)
       (if (not (pair? tree))
           (leaf-fn tree)
           (node-fn (car tree)
                    (map (fn (child) (fold-tree leaf-fn node-fn child))
                         (cdr tree)))))))

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
   (queue-new (fn () '()))

   ; queue-empty?: Check if queue is empty
   (queue-empty? null?)

   ; queue-enqueue: Add element to back of queue
   (queue-enqueue (fn (q x) (append q (list x))))

   ; queue-dequeue: Remove and return front element (returns (element . rest))
   (queue-dequeue (fn (q)
     (if (null? q)
         (cons #f '())
         (cons (car q) (cdr q)))))

   ; queue-front: Peek at front element
   (queue-front (fn (q) (if (null? q) #f (car q))))

   ; queue-size: Get queue size
   (queue-size length)

   ; queue-to-list: Convert queue to list
   (queue-to-list id)

   ; ============================================
   ; Stack Operations (LIFO, list-based)
   ; ============================================

   ; stack-new: Create empty stack
   (stack-new (fn () '()))

   ; stack-empty?: Check if stack is empty
   (stack-empty? null?)

   ; stack-push: Push element onto stack (stack first, then element)
   (stack-push (fn (s x) (cons x s)))

   ; stack-pop: Pop element from stack (returns (element . rest))
   (stack-pop (fn (s)
     (if (null? s)
         (cons #f '())
         (cons (car s) (cdr s)))))

   ; stack-top: Peek at top element
   (stack-top (fn (s) (if (null? s) #f (car s))))

   ; stack-size: Get stack size
   (stack-size length)

   ; ============================================
   ; Deque Operations (double-ended queue)
   ; ============================================

   ; deque-new: Create empty deque
   (deque-new (fn () '()))

   ; deque-empty?: Check if deque is empty
   (deque-empty? null?)

   ; deque-push-front: Add to front (deque first, then element)
   (deque-push-front (fn (d x) (cons x d)))

   ; deque-push-back: Add to back
   (deque-push-back (fn (d x) (append d (list x))))

   ; deque-pop-front: Remove from front
   (deque-pop-front (fn (d)
     (if (null? d)
         (cons #f '())
         (cons (car d) (cdr d)))))

   ; deque-pop-back: Remove from back
   (deque-pop-back (fn (d)
     (if (null? d)
         (cons #f '())
         (cons (last d) (init d)))))

   ; deque-front: Peek at front
   (deque-front (fn (d) (if (null? d) #f (car d))))

   ; deque-back: Peek at back
   (deque-back (fn (d) (if (null? d) #f (last d))))

   ; ============================================
   ; Priority Queue (using sorted list)
   ; ============================================

   ; pq-new: Create empty priority queue
   (pq-new (fn () '()))

   ; pq-empty?: Check if priority queue is empty
   (pq-empty? null?)

   ; pq-insert: Insert with priority (lower = higher priority)
   (pq-insert (fn (pq priority value)
     (insert-sorted-by car (list priority value) pq)))

   ; pq-peek: Get highest priority element
   (pq-peek (fn (pq)
     (if (null? pq) #f (cadar pq))))

   ; pq-pop: Remove and return highest priority element
   (pq-pop (fn (pq)
     (if (null? pq)
         (cons #f '())
         (cons (cadar pq) (cdr pq)))))

   ; ============================================
   ; Graph Algorithms (adjacency list)
   ; ============================================

   ; graph-new: Create empty graph (as adjacency list dict)
   (graph-new (fn () '()))

   ; graph-add-vertex: Add a vertex
   (graph-add-vertex (fn (g v)
     (if (dict-has? g v) g (dict-set g v '()))))

   ; graph-add-edge: Add directed edge from u to v
   (graph-add-edge (fn (g u v)
     (let ((g1 (graph-add-vertex g u)))
       (let ((g2 (graph-add-vertex g1 v)))
         (let ((neighbors (dict-get g2 u '())))
           (if (member? v neighbors)
               g2
               (dict-set g2 u (cons v neighbors))))))))

   ; graph-add-undirected-edge: Add undirected edge
   (graph-add-undirected-edge (fn (g u v)
     (graph-add-edge (graph-add-edge g u v) v u)))

   ; graph-neighbors: Get neighbors of vertex
   (graph-neighbors (fn (g v)
     (dict-get g v '())))

   ; graph-vertices: Get all vertices
   (graph-vertices dict-keys)

   ; graph-has-edge?: Check if edge exists
   (graph-has-edge? (fn (g u v)
     (member? v (graph-neighbors g u))))

   ; graph-degree: Get degree of vertex
   (graph-degree (fn (g v)
     (length (graph-neighbors g v))))

   ; graph-bfs: Breadth-first search from start (returns list of visited vertices)
   (graph-bfs (fix graph-bfs
     (fn (g start)
       (let ((bfs-helper (fix bfs-helper
               (fn (queue visited)
                 (if (null? queue)
                     (reverse visited)
                     (let ((current (car queue))
                           (rest-queue (cdr queue)))
                       (if (member? current visited)
                           (bfs-helper rest-queue visited)
                           (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                    (graph-neighbors g current))))
                             (bfs-helper (append rest-queue neighbors)
                                        (cons current visited))))))))))
         (bfs-helper (list start) '())))))

   ; graph-dfs: Depth-first search from start
   (graph-dfs (fix graph-dfs
     (fn (g start)
       (let ((dfs-helper (fix dfs-helper
               (fn (stack visited)
                 (if (null? stack)
                     (reverse visited)
                     (let ((current (car stack))
                           (rest-stack (cdr stack)))
                       (if (member? current visited)
                           (dfs-helper rest-stack visited)
                           (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                    (graph-neighbors g current))))
                             (dfs-helper (append neighbors rest-stack)
                                        (cons current visited))))))))))
         (dfs-helper (list start) '())))))

   ; graph-path-exists?: Check if path exists from u to v
   (graph-path-exists? (fn (g u v)
     (member? v (graph-bfs g u))))

   ; graph-connected?: Check if graph is connected (for undirected graphs)
   (graph-connected? (fn (g)
     (let ((vertices (graph-vertices g)))
       (if (null? vertices)
           #t
           (= (length (graph-bfs g (car vertices)))
              (length vertices))))))

   ; ============================================
   ; Validation Utilities
   ; ============================================

   ; validate-type: Validate value has expected type
   (validate-type (fn (type-pred value msg)
     (if (type-pred value)
         (right value)
         (left msg))))

   ; validate-range: Validate number is in range
   (validate-range (fn (lo hi value)
     (if (and (>= value lo) (<= value hi))
         (right value)
         (left (list 'out-of-range lo hi value)))))

   ; validate-not-empty: Validate list/string is not empty
   (validate-not-empty (fn (value)
     (if (if (string? value)
             (not (string-empty? value))
             (not (null? value)))
         (right value)
         (left 'empty-value))))

   ; validate-all-pred: Validate all elements satisfy predicate
   (validate-all-pred (fn (p lst msg)
     (if (all p lst)
         (right lst)
         (left msg))))

   ; validate-length: Validate list has expected length
   (validate-length (fn (n lst)
     (if (= (length lst) n)
         (right lst)
         (left (list 'wrong-length n (length lst))))))

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
   (when-pred (fn (p f x)
     (if (p x) (f x) x)))

   ; unless-pred: Apply function only if predicate fails
   (unless-pred (fn (p f x)
     (if (p x) x (f x))))

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
   (case-of (fn (value cases)
     (match-equal value cases #f)))

   ; guard: Guard expressions (list of (condition . value) pairs)
   (guard (fn (cases)
     (let ((finder (fix finder
             (fn (cases)
               (if (null? cases)
                   #f
                   (let ((case (car cases)))
                     (if (car case)
                         (cdr case)
                         (finder (cdr cases)))))))))
       (finder cases))))

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
   (stream-cons (fn (head tail-thunk)
     (list 'stream head tail-thunk)))

   ; stream-head: Get head of stream
   (stream-head (fn (s)
     (if (and (pair? s) (eq? (car s) 'stream))
         (cadr s)
         (error "not a stream"))))

   ; stream-tail: Get tail of stream (forces thunk)
   (stream-tail (fn (s)
     (if (and (pair? s) (eq? (car s) 'stream))
         ((caddr s))
         (error "not a stream"))))

   ; stream-null: Empty stream marker
   (stream-null (list 'stream-null))

   ; stream-null?: Check if stream is empty
   (stream-null? (fn (s)
     (and (pair? s) (eq? (car s) 'stream-null))))

   ; stream-take: Take n elements from stream
   (stream-take (fix stream-take
     (fn (n s)
       (if (or (<= n 0) (stream-null? s))
           '()
           (cons (stream-head s) (stream-take (- n 1) (stream-tail s)))))))

   ; stream-map: Map function over stream
   (stream-map (fix stream-map
     (fn (f s)
       (if (stream-null? s)
           stream-null
           (stream-cons (f (stream-head s))
                        (fn () (stream-map f (stream-tail s))))))))

   ; stream-filter: Filter stream by predicate
   (stream-filter (fix stream-filter
     (fn (p s)
       (if (stream-null? s)
           stream-null
           (if (p (stream-head s))
               (stream-cons (stream-head s)
                            (fn () (stream-filter p (stream-tail s))))
               (stream-filter p (stream-tail s)))))))

   ; stream-from: Infinite stream starting at n
   (stream-from (fix stream-from
     (fn (n)
       (stream-cons n (fn () (stream-from (+ n 1)))))))

   ; stream-iterate: Infinite stream by iterating function
   (stream-iterate (fix stream-iterate
     (fn (f x)
       (stream-cons x (fn () (stream-iterate f (f x)))))))

   ; stream-repeat: Infinite stream of same value
   (stream-repeat (fix stream-repeat
     (fn (x)
       (stream-cons x (fn () (stream-repeat x))))))

   ; stream-zip-with: Zip two streams with function
   (stream-zip-with (fix stream-zip-with
     (fn (f s1 s2)
       (if (or (stream-null? s1) (stream-null? s2))
           stream-null
           (stream-cons (f (stream-head s1) (stream-head s2))
                        (fn () (stream-zip-with f (stream-tail s1) (stream-tail s2))))))))

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
   (traverse-either (fn (f lst)
     (sequence-eithers (map f lst))))

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

   ; state-return: Return value in state monad
   (state-return (fn (x)
     (fn (s) (list x s))))

   ; state-bind: Bind for state monad
   (state-bind (fn (m f)
     (fn (s)
       (let ((result (m s)))
         ((f (car result)) (cadr result))))))

   ; state-get: Get current state
   (state-get (fn (s) (list s s)))

   ; state-put: Set new state
   (state-put (fn (new-state)
     (fn (s) (list '() new-state))))

   ; state-modify: Modify state with function
   (state-modify (fn (f)
     (fn (s) (list '() (f s)))))

   ; run-state: Run state monad with initial state
   (run-state (fn (m init-state)
     (m init-state)))

   ; eval-state: Evaluate state monad, returning value
   (eval-state (fn (m init-state)
     (car (m init-state))))

   ; exec-state: Execute state monad, returning final state
   (exec-state (fn (m init-state)
     (cadr (m init-state))))

   ; ============================================
   ; Reader Monad Utilities
   ; ============================================

   ; reader-return: Return value in reader monad
   (reader-return (fn (x)
     (fn (env) x)))

   ; reader-bind: Bind for reader monad
   (reader-bind (fn (m f)
     (fn (env)
       ((f (m env)) env))))

   ; reader-ask: Get environment
   (reader-ask (fn (env) env))

   ; reader-local: Run with modified environment
   (reader-local (fn (f m)
     (fn (env) (m (f env)))))

   ; run-reader: Run reader with environment
   (run-reader (fn (m env)
     (m env)))

   ; ============================================
   ; Writer Monad Utilities
   ; ============================================

   ; writer-return: Return value in writer monad
   (writer-return (fn (x)
     (list x '())))

   ; writer-bind: Bind for writer monad (log is list)
   (writer-bind (fn (m f)
     (let ((result (f (car m))))
       (list (car result) (append (cadr m) (cadr result))))))

   ; writer-tell: Write to log
   (writer-tell (fn (w)
     (list '() (list w))))

   ; run-writer: Run writer, returning (value log) pair
   (run-writer id)

   ; ============================================
   ; More Collection Utilities
   ; ============================================

   ; multiset-add: Add element to multiset (alist of counts)
   (multiset-add (fn (ms x)
     (let ((found (assoc-ref x ms)))
       (let ((count (if found found 0)))
         (assoc-set x (+ count 1) ms)))))

   ; multiset-remove: Remove one occurrence from multiset
   (multiset-remove (fn (ms x)
     (let ((found (assoc-ref x ms)))
       (let ((count (if found found 0)))
         (if (<= count 1)
             (assoc-remove x ms)
             (assoc-set x (- count 1) ms))))))

   ; multiset-count: Get count of element in multiset
   (multiset-count (fn (ms x)
     (let ((found (assoc-ref x ms)))
       (if found found 0))))

   ; multiset-from-list: Create multiset from list
   (multiset-from-list (fn (lst)
     (foldl multiset-add '() lst)))

   ; multiset-to-list: Convert multiset to list (with repetitions)
   (multiset-to-list (fn (ms)
     (flat-map (fn (pair)
                 (replicate (cdr pair) (car pair)))
               ms)))

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
   (bag-union (fn (ms1 ms2)
     (foldl (fn (acc pair)
              (let ((count (+ (multiset-count acc (car pair)) (cdr pair))))
                (assoc-set (car pair) count acc)))
            ms1
            ms2)))

   ; bag-intersection: Intersection of two multisets
   (bag-intersection (fn (ms1 ms2)
     (filter-map (fn (pair)
                   (let ((c1 (cdr pair))
                         (c2 (multiset-count ms2 (car pair))))
                     (if (> c2 0)
                         (just (cons (car pair) (min c1 c2)))
                         (nothing))))
                 ms1)))

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
   (bounce (fn (f) (list 'bounce f)))

   ; done: Mark value as done for trampolining
   (done (fn (x) x))

   ; ============================================
   ; More Numeric Utilities
   ; ============================================

   ; complex-new: Create complex number as (real . imag)
   (complex-new (fn (r i) (cons r i)))

   ; complex-real: Get real part
   (complex-real car)

   ; complex-imag: Get imaginary part
   (complex-imag cdr)

   ; complex-add: Add two complex numbers
   (complex-add (fn (c1 c2)
     (complex-new (+ (complex-real c1) (complex-real c2))
                  (+ (complex-imag c1) (complex-imag c2)))))

   ; complex-sub: Subtract complex numbers
   (complex-sub (fn (c1 c2)
     (complex-new (- (complex-real c1) (complex-real c2))
                  (- (complex-imag c1) (complex-imag c2)))))

   ; complex-mul: Multiply complex numbers
   (complex-mul (fn (c1 c2)
     (let ((r1 (complex-real c1)) (i1 (complex-imag c1))
           (r2 (complex-real c2)) (i2 (complex-imag c2)))
       (complex-new (- (* r1 r2) (* i1 i2))
                    (+ (* r1 i2) (* i1 r2))))))

   ; complex-magnitude: Magnitude of complex number
   (complex-magnitude (fn (c)
     (sqrt (+ (* (complex-real c) (complex-real c))
              (* (complex-imag c) (complex-imag c))))))

   ; complex-conjugate: Complex conjugate
   (complex-conjugate (fn (c)
     (complex-new (complex-real c) (- (complex-imag c)))))

   ; NOTE: Simple pair-based rationals removed - use built-in rational type
   ; Built-in functions: make-rational, rational-numerator, rational-denominator,
   ; rational-add, rational-sub, rational-mul, rational-div, rational->float

   ; ============================================
   ; Format/Display Utilities
   ; ============================================

   ; format-number: Format number with precision
   (format-number (fn (n precision)
     (let ((factor (pow-int 10 precision)))
       (/ (round (* n factor)) factor))))

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
   (string-chars string->list)

   ; chars-string: Convert list of characters to string
   (chars-string list->string)

   ; string-empty?: Check if string is empty
   (string-empty? (fn (s) (= (string-length s) 0)))

   ; string-first: Get first character of string
   (string-first (fn (s)
     (if (string-empty? s) #f (string-ref s 0))))

   ; string-last: Get last character of string
   (string-last (fn (s)
     (if (string-empty? s) #f (string-ref s (- (string-length s) 1)))))

   ; string-take: Take first n characters
   (string-take (fn (n s)
     (substring s 0 (min n (string-length s)))))

   ; string-drop: Drop first n characters
   (string-drop (fn (n s)
     (substring s (min n (string-length s)) (string-length s))))

   ; string-take-right: Take last n characters
   (string-take-right (fn (n s)
     (let ((len (string-length s)))
       (substring s (max 0 (- len n)) len))))

   ; string-drop-right: Drop last n characters
   (string-drop-right (fn (n s)
     (let ((len (string-length s)))
       (substring s 0 (max 0 (- len n))))))

   ; string-contains?: Check if haystack contains needle
   (string-contains? (fn (haystack needle)
     (let ((needle-len (string-length needle))
           (haystack-len (string-length haystack)))
       (if (> needle-len haystack-len)
           #f
           (let ((finder (fix finder
                   (fn (i)
                     (if (> (+ i needle-len) haystack-len)
                         #f
                         (if (string=? needle (substring haystack i (+ i needle-len)))
                             #t
                             (finder (+ i 1))))))))
             (finder 0))))))

   ; string-index-of: Find index of needle in haystack (-1 if not found)
   (string-index-of (fn (haystack needle)
     (let ((needle-len (string-length needle))
           (haystack-len (string-length haystack)))
       (if (> needle-len haystack-len)
           -1
           (let ((finder (fix finder
                   (fn (i)
                     (if (> (+ i needle-len) haystack-len)
                         -1
                         (if (string=? needle (substring haystack i (+ i needle-len)))
                             i
                             (finder (+ i 1))))))))
             (finder 0))))))

   ; string-starts-with?: Check if string s starts with prefix
   (string-starts-with? (fn (s prefix)
     (let ((plen (string-length prefix)))
       (if (> plen (string-length s))
           #f
           (string=? prefix (substring s 0 plen))))))

   ; string-ends-with?: Check if string s ends with suffix
   (string-ends-with? (fn (s suffix)
     (let ((slen (string-length suffix))
           (len (string-length s)))
       (if (> slen len)
           #f
           (string=? suffix (substring s (- len slen) len))))))

   ; string-replace: Replace all occurrences of old with new in s
   (string-replace (fix string-replace
     (fn (s old new)
       (let ((idx (string-index-of s old)))
         (if (< idx 0)
             s
             (string-replace
               (string-append (substring s 0 idx)
                              new
                              (substring s (+ idx (string-length old)) (string-length s)))
               old new))))))

   ; string-split-at: Split string at index
   (string-split-at (fn (idx s)
     (cons (substring s 0 idx) (substring s idx (string-length s)))))

   ; ============================================
   ; Error Handling Utilities
   ; ============================================

   ; try-catch: Try expression, catch errors (simplified - uses Either)
   ; Returns (right value) on success, (left error-msg) on failure
   (try-catch (fn (thunk on-error)
     (let ((result (thunk)))
       (if (err? result)
           (on-error (unwrap result))
           (right result)))))

   ; assert: Check condition, return error if false
   (assert (fn (condition msg)
     (if condition
         (ok #t)
         (err msg))))

   ; assert-eq: Assert two values are equal
   (assert-eq (fn (expected actual)
     (if (eq? expected actual)
         (ok #t)
         (err (list 'assertion-failed 'expected expected 'got actual)))))

   ; ensure: Ensure condition, return value or error
   (ensure (fn (p value msg)
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

   ; factorial: Compute n!
   (factorial (fix factorial
     (fn (n)
       (if (<= n 1) 1 (* n (factorial (- n 1)))))))

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

   ; divisors: Get all divisors of n
   (divisors (fn (n)
     (filter (fn (d) (= 0 (remainder n d)))
             (range 1 (+ n 1)))))

   ; proper-divisors: Get divisors of n excluding n itself
   (proper-divisors (fn (n)
     (filter (fn (d) (= 0 (remainder n d)))
             (range 1 n))))

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
   (repeat-until (fn (p body init)
     (let ((loop (fix loop
             (fn (state)
               (if (p state)
                   state
                   (loop (body state)))))))
       (loop init))))

   ; do-times: Execute body n times for side effects
   (do-times (fn (n body)
     (for-loop 0 n (fn (i acc) (block (body i) acc)) '())))

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

   ; alist-map: Map over alist values
   (alist-map (fn (f alist)
     (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

   ; alist-filter: Filter alist by predicate on pairs
   (alist-filter (fn (p alist)
     (filter p alist)))

   ; alist-filter-keys: Filter alist by predicate on keys
   (alist-filter-keys (fn (p alist)
     (filter (fn (pair) (p (car pair))) alist)))

   ; alist-filter-values: Filter alist by predicate on values
   (alist-filter-values (fn (p alist)
     (filter (fn (pair) (p (cdr pair))) alist)))

   ; alist-keys: Get all keys from alist
   (alist-keys (fn (alist)
     (map car alist)))

   ; alist-values: Get all values from alist
   (alist-values (fn (alist)
     (map cdr alist)))

   ; alist-has-key?: Check if alist has key
   (alist-has-key? (fn (key alist)
     (not (not (assoc key alist)))))

   ; alist-update: Update value at key with function
   (alist-update (fn (key f default alist)
     (let ((current (assoc-ref key alist)))
       (assoc-set key (f (if current current default)) alist))))

   ; alist-merge: Merge two alists (second takes precedence)
   (alist-merge (fn (a1 a2)
     (foldl (fn (acc pair)
              (assoc-set (car pair) (cdr pair) acc))
            a1
            a2)))

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

   ; group-by-key: Group list elements by key function
   (group-by-key (fn (key-fn lst)
     (foldl (fn (acc x)
              (let ((k (key-fn x)))
                (alist-update k (fn (group) (cons x group)) '() acc)))
            '()
            lst)))

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
   (graph-topological-sort (fn (g)
     (let ((vertices (graph-vertices g)))
       (let ((in-degree (foldl (fn (acc v)
                                  (assoc-set v 0 acc))
                                '() vertices)))
         (let ((in-degree2 (foldl (fn (acc v)
                                     (foldl (fn (acc2 n)
                                              (assoc-set n (+ 1 (assoc-ref n acc2)) acc2))
                                            acc
                                            (graph-neighbors g v)))
                                  in-degree vertices)))
           (let ((topo-helper (fix topo-helper
                   (fn (queue result degrees)
                     (if (null? queue)
                         (if (= (length result) (length vertices))
                             (reverse result)
                             #f)
                         (let ((v (car queue))
                               (rest-queue (cdr queue)))
                           (let ((new-degrees-and-queue
                                   (foldl (fn (acc n)
                                            (let ((new-deg (- (assoc-ref n (car acc)) 1)))
                                              (cons (assoc-set n new-deg (car acc))
                                                    (if (= new-deg 0)
                                                        (cons n (cdr acc))
                                                        (cdr acc)))))
                                          (cons degrees rest-queue)
                                          (graph-neighbors g v))))
                             (topo-helper (cdr new-degrees-and-queue)
                                          (cons v result)
                                          (car new-degrees-and-queue)))))))))
             (let ((initial-queue (filter (fn (v) (= 0 (assoc-ref v in-degree2))) vertices)))
               (topo-helper initial-queue '() in-degree2))))))))

   ; graph-has-cycle?: Check if directed graph has a cycle
   (graph-has-cycle? (fn (g)
     (not (graph-topological-sort g))))

   ; graph-reverse: Reverse all edges in graph
   (graph-reverse (fn (g)
     (foldl (fn (acc v)
              (foldl (fn (acc2 n)
                       (graph-add-edge acc2 n v))
                     acc
                     (graph-neighbors g v)))
            (graph-new)
            (graph-vertices g))))

   ; graph-transpose: Alias for graph-reverse
   (graph-transpose graph-reverse)

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

   ; string-pad-left: Pad string on left to reach length (pad-char must be a char like #\space)
   (string-pad-left (fn (s len pad-char)
     (let ((slen (string-length s)))
       (if (>= slen len)
           s
           (string-append (make-string (- len slen) pad-char) s)))))

   ; string-pad-right: Pad string on right to reach length (pad-char must be a char)
   (string-pad-right (fn (s len pad-char)
     (let ((slen (string-length s)))
       (if (>= slen len)
           s
           (string-append s (make-string (- len slen) pad-char))))))

   ; string-center: Center string in field of given length (pad-char must be a char)
   (string-center (fn (s len pad-char)
     (let ((slen (string-length s)))
       (if (>= slen len)
           s
           (let ((total-pad (- len slen)))
             (let ((left-pad (quotient total-pad 2))
                   (right-pad (- total-pad (quotient total-pad 2))))
               (string-append (make-string left-pad pad-char)
                              s
                              (make-string right-pad pad-char))))))))

   ; string-repeat: Repeat string n times
   (string-repeat (fn (s n)
     (let ((builder (fix builder
             (fn (count acc)
               (if (<= count 0)
                   acc
                   (builder (- count 1) (string-append acc s)))))))
       (builder n ""))))

   ; string-count: Count occurrences of substring
   (string-count (fn (haystack needle)
     (let ((counter (fix counter
             (fn (s count)
               (let ((idx (string-index-of s needle)))
                 (if (< idx 0)
                     count
                     (counter (string-drop (+ idx (string-length needle)) s)
                              (+ count 1))))))))
       (counter haystack 0))))

   ; ============================================
   ; Sequence Utilities
   ; ============================================

   ; scan: Like foldl but collects all intermediate results
   (scan (fix scan
     (fn (f init lst)
       (if (null? lst)
           (list init)
           (cons init (scan f (f init (car lst)) (cdr lst)))))))

   ; scan-right: Like foldr but collects all intermediate results
   (scan-right (fix scan-right
     (fn (f init lst)
       (if (null? lst)
           (list init)
           (let ((rest-scan (scan-right f init (cdr lst))))
             (cons (f (car lst) (car rest-scan)) rest-scan))))))

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

   ; running-sum: Cumulative sum
   (running-sum (fn (lst)
     (scan + 0 lst)))

   ; running-product: Cumulative product
   (running-product (fn (lst)
     (scan * 1 lst)))

   ; ============================================
   ; Additional Numeric Utilities
   ; ============================================

   ; mean: Arithmetic mean of list
   (mean (fn (lst)
     (if (null? lst)
         0
         (/ (sum-list lst) (length lst)))))

   ; median: Median of list
   (median (fn (lst)
     (if (null? lst)
         #f
         (let ((sorted (quicksort lst))
               (n (length lst)))
           (if (odd? n)
               (list-ref sorted (quotient n 2))
               (/ (+ (list-ref sorted (- (quotient n 2) 1))
                     (list-ref sorted (quotient n 2)))
                  2))))))

   ; mode: Most frequent element
   (mode (fn (lst)
     (if (null? lst)
         #f
         (let ((freqs (frequencies lst)))
           (car (foldl (fn (best pair)
                         (if (> (cdr pair) (cdr best))
                             pair
                             best))
                       (car freqs)
                       (cdr freqs)))))))

   ; variance: Population variance
   (variance (fn (lst)
     (if (or (null? lst) (null? (cdr lst)))
         0
         (let ((m (mean lst)))
           (mean (map (fn (x) (sq (- x m))) lst))))))

   ; std-dev: Standard deviation
   (std-dev (fn (lst)
     (sqrt (variance lst))))

   ; normalize-list: Normalize list to [0, 1] range
   (normalize-list (fn (lst)
     (if (null? lst)
         '()
         (let ((lo (apply min lst))
               (hi (apply max lst)))
           (if (= lo hi)
               (map (const 0.5) lst)
               (map (fn (x) (/ (- x lo) (- hi lo))) lst))))))

   ; z-score: Convert to z-scores (standard scores)
   (z-score (fn (lst)
     (let ((m (mean lst))
           (sd (std-dev lst)))
       (if (= sd 0)
           (map (const 0) lst)
           (map (fn (x) (/ (- x m) sd)) lst)))))

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
   (complement (fn (p)
     (fn (x) (not (p x)))))

   ; conjoin: AND multiple predicates
   (conjoin (fn (preds)
     (fn (x)
       (all (fn (p) (p x)) preds))))

   ; disjoin: OR multiple predicates
   (disjoin (fn (preds)
     (fn (x)
       (any (fn (p) (p x)) preds))))

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
   (juxt (fn (fns)
     (fn (x)
       (map (fn (f) (f x)) fns))))

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
   (converge (fn (combine fns)
     (fn (x)
       (apply combine (map (fn (f) (f x)) fns)))))

   ; on: Apply binary function after applying unary function to both args
   ; ((on + length) "abc" "de") => (+ 3 2) => 5
   (on (fn (f g)
     (fn (x y)
       (f (g x) (g y)))))

   ; both: Apply same function to both elements of a pair
   (both (fn (f pair)
     (cons (f (car pair)) (f (cdr pair)))))

   ; ============================================
   ; Currying and Partial Application
   ; ============================================

   ; curry2: Convert 2-arg function to curried form
   ; ((curry2 +) 1) => closure, (((curry2 +) 1) 2) => 3
   (curry2 (fn (f)
     (fn (a)
       (fn (b)
         (f a b)))))

   ; curry3: Convert 3-arg function to curried form
   (curry3 (fn (f)
     (fn (a)
       (fn (b)
         (fn (c)
           (f a b c))))))

   ; uncurry2: Convert curried function back to 2-arg
   (uncurry2 (fn (f)
     (fn (a b)
       ((f a) b))))

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
   (partial2 (fn (f a b)
     (fn (c)
       (f a b c))))

   ; partial-right: Fix last argument
   ; ((partial-right - 1) 5) => 4
   (partial-right (fn (f b)
     (fn (a)
       (f a b))))

   ; flip: Flip arguments of binary function
   ; ((flip -) 1 5) => 4
   (flip (fn (f)
     (fn (a b)
       (f b a))))

   ; flip3: Flip first and third arguments
   (flip3 (fn (f)
     (fn (a b c)
       (f c b a))))

   ; ============================================
   ; Iteration Utilities
   ; ============================================

   ; iterate-n: Apply function n times, return final result
   (iterate-n (fn (f n x)
     (let ((iter (fix iter
             (fn (i acc)
               (if (= i 0)
                   acc
                   (iter (- i 1) (f acc)))))))
       (iter n x))))

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
   (count-if (fn (p lst)
     (length (filter p lst))))

   ; remove-if: Remove elements satisfying predicate (opposite of filter)
   (remove-if (fn (p lst)
     (filter (complement p) lst)))

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

   ; split-when: Split list when predicate becomes true
   (split-when (fn (p lst)
     (let ((helper (fix helper
             (fn (lst current groups)
               (if (null? lst)
                   (if (null? current)
                       (reverse groups)
                       (reverse (cons (reverse current) groups)))
                   (if (p (car lst))
                       (helper (cdr lst)
                               (list (car lst))
                               (if (null? current)
                                   groups
                                   (cons (reverse current) groups)))
                       (helper (cdr lst)
                               (cons (car lst) current)
                               groups)))))))
       (helper lst '() '()))))

   ; interleave: Interleave elements from two lists
   (interleave (fix interleave
     (fn (xs ys)
       (if (null? xs)
           ys
           (if (null? ys)
               xs
               (cons (car xs)
                     (cons (car ys)
                           (interleave (cdr xs) (cdr ys)))))))))

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
   (alist-invert (fn (alist)
     (map (fn (pair) (cons (cdr pair) (car pair))) alist)))

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

   ; factorial: Compute n!
   (factorial (fn (n)
     (let ((fact (fix fact
             (fn (n acc)
               (if (<= n 1)
                   acc
                   (fact (- n 1) (* n acc)))))))
       (fact n 1))))

   ; fibonacci: Compute nth Fibonacci number
   (fibonacci (fn (n)
     (let ((fib (fix fib
             (fn (n a b)
               (if (<= n 0)
                   a
                   (fib (- n 1) b (+ a b)))))))
       (fib n 0 1))))

   ; prime?: Check if number is prime
   (prime? (fn (n)
     (if (<= n 1)
         #f
         (if (<= n 3)
             #t
             (if (or (= 0 (mod n 2)) (= 0 (mod n 3)))
                 #f
                 (let ((check (fix check
                         (fn (i)
                           (if (> (* i i) n)
                               #t
                               (if (or (= 0 (mod n i)) (= 0 (mod n (+ i 2))))
                                   #f
                                   (check (+ i 6))))))))
                   (check 5)))))))

   ; divisors: Get all divisors of n
   (divisors (fn (n)
     (let ((find-divs (fix find-divs
             (fn (i acc)
               (if (> (* i i) n)
                   acc
                   (if (= 0 (mod n i))
                       (if (= i (/ n i))
                           (find-divs (+ i 1) (cons i acc))
                           (find-divs (+ i 1) (cons (/ n i) (cons i acc))))
                       (find-divs (+ i 1) acc)))))))
       (sort (find-divs 1 '())))))

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

   ; binomial: Binomial coefficient (n choose k)
   (binomial (fn (n k)
     (if (or (< k 0) (> k n))
         0
         (if (or (= k 0) (= k n))
             1
             (/ (product-range (+ (- n k) 1) n) (factorial k))))))

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
   (reduce (fn (f lst)
     (if (null? lst)
         '()
         (foldl f (car lst) (cdr lst)))))

   ; reduce-right: Like foldr but uses last element as initial value
   (reduce-right (fn (f lst)
     (if (null? lst)
         '()
         (let ((rev (reverse lst)))
           (foldr f (car rev) (cdr rev))))))

   ; find-last: Find last element satisfying predicate
   (find-last (fn (p lst)
     (foldl (fn (acc x) (if (p x) x acc)) #f lst)))

   ; index-where: Find index of first element satisfying predicate
   (index-where (fn (p lst)
     (let ((finder (fix finder
             (fn (lst i)
               (if (null? lst)
                   -1
                   (if (p (car lst))
                       i
                       (finder (cdr lst) (+ i 1))))))))
       (finder lst 0))))

   ; last-index-where: Find index of last element satisfying predicate
   (last-index-where (fn (p lst)
     (let ((finder (fix finder
             (fn (lst i last-found)
               (if (null? lst)
                   last-found
                   (finder (cdr lst) (+ i 1) (if (p (car lst)) i last-found)))))))
       (finder lst 0 -1))))

   ; indices-where: Find all indices satisfying predicate
   (indices-where (fn (p lst)
     (let ((finder (fix finder
             (fn (lst i acc)
               (if (null? lst)
                   (reverse acc)
                   (finder (cdr lst) (+ i 1)
                           (if (p (car lst)) (cons i acc) acc)))))))
       (finder lst 0 '()))))

   ; take-last: Take last n elements
   (take-last (fn (n lst)
     (let ((len (length lst)))
       (if (<= n 0)
           '()
           (drop lst (max 0 (- len n)))))))

   ; drop-last: Drop last n elements
   (drop-last (fn (n lst)
     (let ((len (length lst)))
       (if (<= n 0)
           lst
           (take lst (max 0 (- len n)))))))

   ; insert-at: Insert element at index
   (insert-at (fn (idx elem lst)
     (if (<= idx 0)
         (cons elem lst)
         (if (null? lst)
             (list elem)
             (cons (car lst) (insert-at (- idx 1) elem (cdr lst)))))))

   ; remove-at: Remove element at index
   (remove-at (fn (idx lst)
     (if (null? lst)
         '()
         (if (= idx 0)
             (cdr lst)
             (cons (car lst) (remove-at (- idx 1) (cdr lst)))))))

   ; update-at: Update element at index
   (update-at (fn (idx f lst)
     (if (null? lst)
         '()
         (if (= idx 0)
             (cons (f (car lst)) (cdr lst))
             (cons (car lst) (update-at (- idx 1) f (cdr lst)))))))

   ; set-at: Set element at index
   (set-at (fn (idx val lst)
     (update-at idx (const val) lst)))

   ; rotate-left: Rotate list left by n positions
   (rotate-left (fn (n lst)
     (if (or (null? lst) (<= n 0))
         lst
         (let ((len (length lst)))
           (let ((n-mod (mod n len)))
             (append (drop lst n-mod) (take lst n-mod)))))))

   ; rotate-right: Rotate list right by n positions
   (rotate-right (fn (n lst)
     (if (or (null? lst) (<= n 0))
         lst
         (rotate-left (- (length lst) (mod n (length lst))) lst))))

   ; ============================================
   ; String Utilities (Extended)
   ; ============================================

   ; string-repeat: Repeat string n times
   (string-repeat (fn (s n)
     (let ((builder (fix builder
             (fn (i acc)
               (if (<= i 0)
                   acc
                   (builder (- i 1) (string-append acc s)))))))
       (builder n ""))))

   ; string-reverse: Reverse a string
   (string-reverse (fn (s)
     (list->string (reverse (string->list s)))))

   ; string-take: Take first n characters
   (string-take (fn (n s)
     (substring s 0 (min n (string-length s)))))

   ; string-drop: Drop first n characters
   (string-drop (fn (n s)
     (let ((len (string-length s)))
       (if (>= n len)
           ""
           (substring s n len)))))

   ; string-take-right: Take last n characters
   (string-take-right (fn (s n)
     (let ((len (string-length s)))
       (if (>= n len)
           s
           (substring s (- len n) len)))))

   ; string-drop-right: Drop last n characters
   (string-drop-right (fn (s n)
     (let ((len (string-length s)))
       (if (>= n len)
           ""
           (substring s 0 (- len n))))))

   ; string-trim-left: Remove leading whitespace
   (string-trim-left (fn (s)
     (let ((chars (string->list s)))
       (let ((trimmer (fix trimmer
               (fn (lst)
                 (if (null? lst)
                     '()
                     (if (or (eq? (car lst) #\space)
                             (eq? (car lst) #\tab)
                             (eq? (car lst) #\newline))
                         (trimmer (cdr lst))
                         lst))))))
         (list->string (trimmer chars))))))

   ; string-trim-right: Remove trailing whitespace
   (string-trim-right (fn (s)
     (string-reverse (string-trim-left (string-reverse s)))))

   ; string-trim: Remove leading and trailing whitespace
   (string-trim (fn (s)
     (string-trim-left (string-trim-right s))))

   ; string-empty?: Check if string is empty
   (string-empty? (fn (s)
     (= (string-length s) 0)))

   ; string-blank?: Check if string is empty or only whitespace
   (string-blank? (fn (s)
     (string-empty? (string-trim s))))

   ; ============================================
   ; Control Flow (Extended)
   ; ============================================

   ; unless: Opposite of when
   (unless (fn (condition body)
     (if condition '() body)))

   ; when-let: Execute body if value is truthy, binding value
   (when-let (fn (val body-fn)
     (if val (body-fn val) '())))

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
   (clamp-val (fn (lo hi x)
     (max lo (min hi x))))

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
   (tap (fn (f x)
     (block (f x) x)))

   ; trace: Print value and return it
   (trace (fn (label x)
     (block (display label) (display ": ") (write x) (newline) x)))

   ; ============================================
   ; Set Operations (using sorted lists)
   ; ============================================

   ; set-from-list: Create set from list (sorted, unique)
   (set-from-list (fn (lst)
     (remove-duplicates (sort lst))))

   ; set-member?: Check membership in set
   (set-member? (fn (x set)
     (member? x set)))

   ; set-add: Add element to set
   (set-add (fn (x set)
     (if (member? x set)
         set
         (sort (cons x set)))))

   ; set-remove: Remove element from set
   (set-remove (fn (x set)
     (filter (fn (y) (not (eq? x y))) set)))

   ; set-union: Union of two sets
   (set-union (fn (s1 s2)
     (set-from-list (append s1 s2))))

   ; set-intersection: Intersection of two sets
   (set-intersection (fn (s1 s2)
     (filter (fn (x) (member? x s2)) s1)))

   ; set-difference: Elements in s1 but not s2
   (set-difference (fn (s1 s2)
     (filter (fn (x) (not (member? x s2))) s1)))

   ; set-symmetric-difference: Elements in exactly one set
   (set-symmetric-difference (fn (s1 s2)
     (set-union (set-difference s1 s2) (set-difference s2 s1))))

   ; set-subset?: Check if s1 is subset of s2
   (set-subset? (fn (s1 s2)
     (all (fn (x) (member? x s2)) s1)))

   ; set-equal?: Check if two sets are equal
   (set-equal? (fn (s1 s2)
     (and (set-subset? s1 s2) (set-subset? s2 s1))))

   ; set-empty?: Check if set is empty
   (set-empty? (fn (set)
     (null? set)))

   ; set-size: Get size of set
   (set-size (fn (set)
     (length set)))

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
   (tree-depth (fn (tree)
     (tree-fold (const 0) (fn (depths) (+ 1 (if (null? depths) 0 (apply max depths)))) tree)))

   ; tree-size: Count number of leaves
   (tree-size (fn (tree)
     (tree-fold (const 1) (fn (sizes) (apply + sizes)) tree)))

   ; tree-filter: Keep only leaves matching predicate
   (tree-filter (fix tree-filter
     (fn (p tree)
       (if (tree-leaf? tree)
           (if (p tree) (list tree) '())
           (let ((children (flat-map (fn (child) (tree-filter p child)) tree)))
             (if (null? children) '() children))))))

   ; tree-find: Find first leaf matching predicate
   (tree-find (fix tree-find
     (fn (p tree)
       (if (tree-leaf? tree)
           (if (p tree) tree #f)
           (let ((finder (fix finder
                   (fn (children)
                     (if (null? children)
                         #f
                         (let ((result (tree-find p (car children))))
                           (if result result (finder (cdr children)))))))))
             (finder tree))))))

   ; ============================================
   ; Graph Utilities (adjacency list representation)
   ; ============================================

   ; graph-nodes: Get all nodes in graph
   (graph-nodes (fn (graph)
     (map car graph)))

   ; graph-neighbors: Get neighbors of node
   (graph-neighbors (fn (node graph)
     (let ((entry (assoc node graph)))
       (if entry (cdr entry) '()))))

   ; graph-has-edge?: Check if edge exists
   (graph-has-edge? (fn (from to graph)
     (member? to (graph-neighbors from graph))))

   ; graph-add-edge: Add directed edge
   (graph-add-edge (fn (from to graph)
     (let ((entry (assoc from graph)))
       (if entry
           (map (fn (pair)
                  (if (eq? (car pair) from)
                      (cons from (set-add to (cdr pair)))
                      pair))
                graph)
           (cons (cons from (list to)) graph)))))

   ; graph-remove-edge: Remove directed edge
   (graph-remove-edge (fn (from to graph)
     (map (fn (pair)
            (if (eq? (car pair) from)
                (cons from (set-remove to (cdr pair)))
                pair))
          graph)))

   ; graph-reverse: Reverse all edges
   (graph-reverse (fn (graph)
     (foldl (fn (acc pair)
              (foldl (fn (g neighbor)
                       (graph-add-edge neighbor (car pair) g))
                     acc
                     (cdr pair)))
            '()
            graph)))

   ; graph-bfs: Breadth-first search, return visited nodes in order
   (graph-bfs (fn (start graph)
     (let ((bfs-loop (fix bfs-loop
             (fn (queue visited)
               (if (null? queue)
                   (reverse visited)
                   (let ((current (car queue))
                         (rest-queue (cdr queue)))
                     (if (member? current visited)
                         (bfs-loop rest-queue visited)
                         (let ((neighbors (graph-neighbors current graph)))
                           (bfs-loop (append rest-queue neighbors)
                                     (cons current visited))))))))))
       (bfs-loop (list start) '()))))

   ; graph-dfs: Depth-first search, return visited nodes in order
   (graph-dfs (fn (start graph)
     (let ((dfs-loop (fix dfs-loop
             (fn (stack visited)
               (if (null? stack)
                   (reverse visited)
                   (let ((current (car stack))
                         (rest-stack (cdr stack)))
                     (if (member? current visited)
                         (dfs-loop rest-stack visited)
                         (let ((neighbors (graph-neighbors current graph)))
                           (dfs-loop (append neighbors rest-stack)
                                     (cons current visited))))))))))
       (dfs-loop (list start) '()))))

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

   ; is-power-of-2?: Check if n is a power of 2
   (is-power-of-2? (fn (n)
     (and (> n 0) (= 0 (bit-and n (- n 1))))))

   ; next-power-of-2: Find smallest power of 2 >= n
   (next-power-of-2 (fn (n)
     (if (<= n 1)
         1
         (let ((find-pow (fix find-pow
                 (fn (p)
                   (if (>= p n)
                       p
                       (find-pow (* 2 p)))))))
           (find-pow 1)))))

   ; log2-int: Integer log base 2 (floor)
   (log2-int (fn (n)
     (if (<= n 0)
         -1
         (let ((counter (fix counter
                 (fn (x acc)
                   (if (<= x 1)
                       acc
                       (counter (shr x 1) (+ acc 1)))))))
           (counter n 0)))))

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

   ; primes-up-to: Generate primes up to n using sieve
   (primes-up-to (fn (n)
     (if (< n 2)
         '()
         (let ((sieve (fix sieve
                 (fn (candidates)
                   (if (null? candidates)
                       '()
                       (let ((p (car candidates)))
                         (if (> (* p p) n)
                             candidates
                             (cons p (sieve (filter (fn (x) (not (= 0 (mod x p))))
                                                    (cdr candidates)))))))))))
           (sieve (range 2 (+ n 1)))))))

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

   ; powers-of: Generate powers of base: base^0, base^1, ..., base^(n-1)
   (powers-of (fn (base n)
     (build-list n (fn (i) (expt base i)))))

   ; triangular-numbers: First n triangular numbers
   (triangular-numbers (fn (n)
     (build-list n (fn (i) (/ (* i (+ i 1)) 2)))))

   ; ============================================
   ; Combinatorics
   ; ============================================

   ; permutations: Generate all permutations of list
   (permutations (fix permutations
     (fn (lst)
       (if (null? lst)
           '(())
           (flat-map (fn (x)
                       (map (fn (p) (cons x p))
                            (permutations (remove-first x lst))))
                     lst)))))

   ; remove-first: Remove first occurrence of element
   (remove-first (fn (x lst)
     (if (null? lst)
         '()
         (if (eq? x (car lst))
             (cdr lst)
             (cons (car lst) (remove-first x (cdr lst)))))))

   ; combinations: Generate all k-combinations
   (combinations (fix combinations
     (fn (k lst)
       (if (= k 0)
           '(())
           (if (null? lst)
               '()
               (append (map (fn (c) (cons (car lst) c))
                           (combinations (- k 1) (cdr lst)))
                      (combinations k (cdr lst))))))))

   ; cartesian-product: Cartesian product of two lists
   (cartesian-product (fn (xs ys)
     (flat-map (fn (x)
                 (map (fn (y) (list x y)) ys))
               xs)))

   ; power-set: Generate all subsets
   (power-set (fix power-set
     (fn (lst)
       (if (null? lst)
           '(())
           (let ((rest (power-set (cdr lst))))
             (append rest
                     (map (fn (s) (cons (car lst) s)) rest)))))))

   ; ============================================
   ; Queue (FIFO) - represented as (front . back)
   ; ============================================

   ; queue-empty: Create empty queue
   (queue-empty (cons '() '()))

   ; queue-empty?: Check if queue is empty
   (queue-empty? (fn (q)
     (and (null? (car q)) (null? (cdr q)))))

   ; queue-enqueue: Add element to back of queue
   (queue-enqueue (fn (x q)
     (cons (car q) (cons x (cdr q)))))

   ; queue-normalize: Move back to front if front is empty
   (queue-normalize (fn (q)
     (if (null? (car q))
         (cons (reverse (cdr q)) '())
         q)))

   ; queue-dequeue: Remove and return (element . new-queue)
   (queue-dequeue (fn (q)
     (let ((nq (queue-normalize q)))
       (if (null? (car nq))
           (cons #f q)
           (cons (car (car nq)) (cons (cdr (car nq)) (cdr nq)))))))

   ; queue-front: Peek at front element
   (queue-front (fn (q)
     (let ((nq (queue-normalize q)))
       (if (null? (car nq)) #f (car (car nq))))))

   ; queue-size: Get queue size
   (queue-size (fn (q)
     (+ (length (car q)) (length (cdr q)))))

   ; queue-to-list: Convert queue to list
   (queue-to-list (fn (q)
     (append (car q) (reverse (cdr q)))))

   ; queue-from-list: Create queue from list
   (queue-from-list (fn (lst)
     (cons lst '())))

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
   (pq-empty? null?)

   ; pq-insert: Insert with priority (lower = higher priority)
   (pq-insert (fn (elem priority pq)
     (let ((insert-sorted (fix insert-sorted
             (fn (lst)
               (if (null? lst)
                   (list (cons elem priority))
                   (if (<= priority (cdr (car lst)))
                       (cons (cons elem priority) lst)
                       (cons (car lst) (insert-sorted (cdr lst)))))))))
       (insert-sorted pq))))

   ; pq-pop: Remove highest priority, return (element . new-pq)
   (pq-pop (fn (pq)
     (if (null? pq)
         (cons #f '())
         (cons (car (car pq)) (cdr pq)))))

   ; pq-peek: Peek at highest priority element
   (pq-peek (fn (pq)
     (if (null? pq) #f (car (car pq)))))

   ; ============================================
   ; Deque (Double-ended queue)
   ; ============================================

   ; deque-empty: Empty deque
   (deque-empty (cons '() '()))

   ; deque-empty?: Check if empty
   (deque-empty? (fn (d)
     (and (null? (car d)) (null? (cdr d)))))

   ; deque-push-front: Add to front
   (deque-push-front (fn (x d)
     (cons (cons x (car d)) (cdr d))))

   ; deque-push-back: Add to back
   (deque-push-back (fn (x d)
     (cons (car d) (cons x (cdr d)))))

   ; deque-pop-front: Remove from front
   (deque-pop-front (fn (d)
     (if (null? (car d))
         (if (null? (cdr d))
             (cons #f d)
             (let ((new-front (reverse (cdr d))))
               (cons (car new-front) (cons (cdr new-front) '()))))
         (cons (car (car d)) (cons (cdr (car d)) (cdr d))))))

   ; deque-pop-back: Remove from back
   (deque-pop-back (fn (d)
     (if (null? (cdr d))
         (if (null? (car d))
             (cons #f d)
             (let ((new-back (reverse (car d))))
               (cons (car new-back) (cons '() (cdr new-back)))))
         (cons (car (cdr d)) (cons (car d) (cdr (cdr d)))))))

   ; ============================================
   ; Stack Operations (simple list-based)
   ; ============================================

   ; stack-empty: Empty stack
   (stack-empty '())

   ; stack-empty?: Check if empty
   (stack-empty? null?)

   ; stack-push: Push element
   (stack-push cons)

   ; stack-pop: Pop element, return (element . new-stack)
   (stack-pop (fn (s)
     (if (null? s)
         (cons #f '())
         (cons (car s) (cdr s)))))

   ; stack-peek: Peek at top
   (stack-peek (fn (s)
     (if (null? s) #f (car s))))

   ; stack-size: Get size
   (stack-size length)

   ; ============================================
   ; Number Parsing and Formatting
   ; ============================================

   ; digits: Get list of digits from number
   (digits (fn (n)
     (let ((get-digits (fix get-digits
             (fn (n acc)
               (if (= n 0)
                   (if (null? acc) '(0) acc)
                   (get-digits (/ n 10) (cons (mod n 10) acc)))))))
       (get-digits (abs n) '()))))

   ; from-digits: Convert list of digits to number
   (from-digits (fn (ds)
     (foldl (fn (acc d) (+ (* acc 10) d)) 0 ds)))

   ; digit-sum: Sum of digits
   (digit-sum (fn (n)
     (foldl + 0 (digits n))))

   ; digit-count: Count digits
   (digit-count (fn (n)
     (length (digits n))))

   ; reverse-number: Reverse digits of number
   (reverse-number (fn (n)
     (from-digits (reverse (digits n)))))

   ; palindrome-number?: Check if number is palindrome
   (palindrome-number? (fn (n)
     (let ((ds (digits n)))
       (equal? ds (reverse ds)))))

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
   (binary-search (fn (target lst)
     (let ((search (fix search
             (fn (lo hi)
               (if (> lo hi)
                   -1
                   (let ((mid (/ (+ lo hi) 2)))
                     (let ((elem (list-ref lst mid)))
                       (if (= elem target)
                           mid
                           (if (< elem target)
                               (search (+ mid 1) hi)
                               (search lo (- mid 1)))))))))))
       (search 0 (- (length lst) 1)))))

   ; lower-bound: First index where element >= target
   (lower-bound (fn (target lst)
     (let ((search (fix search
             (fn (lo hi)
               (if (>= lo hi)
                   lo
                   (let ((mid (/ (+ lo hi) 2)))
                     (if (< (list-ref lst mid) target)
                         (search (+ mid 1) hi)
                         (search lo mid))))))))
       (search 0 (length lst)))))

   ; upper-bound: First index where element > target
   (upper-bound (fn (target lst)
     (let ((search (fix search
             (fn (lo hi)
               (if (>= lo hi)
                   lo
                   (let ((mid (/ (+ lo hi) 2)))
                     (if (<= (list-ref lst mid) target)
                         (search (+ mid 1) hi)
                         (search lo mid))))))))
       (search 0 (length lst)))))

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

   ; unfold-right: Build list from right (like unfoldr in Haskell)
   (unfold-right (fn (p f g seed)
     (let ((build (fix build
             (fn (seed)
               (if (p seed)
                   '()
                   (cons (f seed) (build (g seed))))))))
       (build seed))))

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

   ; alist-merge: Merge two alists (second overwrites first)
   (alist-merge (fn (a1 a2)
     (foldl (fn (acc pair)
              (assoc-set (car pair) (cdr pair) acc))
            a1
            a2)))

   ; alist-map-values: Map function over values
   (alist-map-values (fn (f alist)
     (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

   ; alist-map-keys: Map function over keys
   (alist-map-keys (fn (f alist)
     (map (fn (pair) (cons (f (car pair)) (cdr pair))) alist)))

   ; alist-find: Find first pair matching predicate
   (alist-find (fn (p alist)
     (find-if p alist)))

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

   ; validate: Basic validation - returns (ok value) or (err message)
   (validate (fn (p msg value)
     (if (p value)
         (ok value)
         (err msg))))

   ; validate-all: Run all validations, collect all errors
   (validate-all (fn (validators value)
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
   (validator (fn (p msg)
     (fn (value) (validate p msg value))))

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
   (string-split-at (fn (idx s)
     (cons (string-take idx s) (string-drop idx s))))

   ; string-words: Split string into words (by whitespace)
   (string-words (fn (s)
     (let ((chars (string->list s)))
       (let ((split-words (fix split-words
               (fn (chars current words)
                 (if (null? chars)
                     (if (null? current)
                         (reverse words)
                         (reverse (cons (list->string (reverse current)) words)))
                     (if (or (eq? (car chars) #\space)
                             (eq? (car chars) #\tab)
                             (eq? (car chars) #\newline))
                         (if (null? current)
                             (split-words (cdr chars) '() words)
                             (split-words (cdr chars) '()
                                         (cons (list->string (reverse current)) words)))
                         (split-words (cdr chars) (cons (car chars) current) words)))))))
         (split-words chars '() '())))))

   ; string-unwords: Join words with space
   (string-unwords (fn (words)
     (if (null? words)
         ""
         (foldl (fn (acc w) (string-append acc " " w))
                (car words)
                (cdr words)))))

   ; string-lines: Split string into lines
   (string-lines (fn (s)
     (let ((chars (string->list s)))
       (let ((split-lines (fix split-lines
               (fn (chars current lines)
                 (if (null? chars)
                     (reverse (cons (list->string (reverse current)) lines))
                     (if (eq? (car chars) #\newline)
                         (split-lines (cdr chars) '()
                                     (cons (list->string (reverse current)) lines))
                         (split-lines (cdr chars) (cons (car chars) current) lines)))))))
         (split-lines chars '() '())))))

   ; string-unlines: Join lines with newline
   (string-unlines (fn (lines)
     (if (null? lines)
         ""
         (foldl (fn (acc l) (string-append acc "\n" l))
                (car lines)
                (cdr lines)))))

   ; string-center: Center string to width with padding
   (string-center (fn (width s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (let ((pad-total (- width len)))
             (let ((pad-left (/ pad-total 2))
                   (pad-right (- pad-total (/ pad-total 2))))
               (string-append (string-repeat " " pad-left)
                             s
                             (string-repeat " " pad-right))))))))

   ; string-ljust: Left-justify string to width
   (string-ljust (fn (width s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append s (string-repeat " " (- width len)))))))

   ; string-rjust: Right-justify string to width
   (string-rjust (fn (width s)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append (string-repeat " " (- width len)) s)))))

   ; ============================================
   ; Memoization Helpers
   ; ============================================

   ; make-memo-table: Create empty memo table (alist)
   (make-memo-table (fn () '()))

   ; memo-lookup: Look up in memo table
   (memo-lookup (fn (key table)
     (assoc key table)))

   ; memo-insert: Insert into memo table
   (memo-insert (fn (key value table)
     (cons (cons key value) table)))

   ; ============================================
   ; Matrix Operations (2D list)
   ; ============================================

   ; matrix-rows: Get number of rows
   (matrix-rows (fn (m)
     (length m)))

   ; matrix-cols: Get number of columns
   (matrix-cols (fn (m)
     (if (null? m) 0 (length (car m)))))

   ; matrix-ref: Get element at (row, col)
   (matrix-ref (fn (m row col)
     (list-ref (list-ref m row) col)))

   ; matrix-set: Set element at (row, col)
   (matrix-set (fn (m row col val)
     (update-at row (fn (r) (set-at col val r)) m)))

   ; matrix-transpose: Transpose matrix
   (matrix-transpose (fix matrix-transpose
     (fn (m)
       (if (or (null? m) (null? (car m)))
           '()
           (cons (map car m)
                 (matrix-transpose (map cdr m)))))))

   ; matrix-map: Map function over all elements
   (matrix-map (fn (f m)
     (map (fn (row) (map f row)) m)))

   ; matrix-zip-with: Zip two matrices with function
   (matrix-zip-with (fn (f m1 m2)
     (map (fn (r1 r2) (zip-with f r1 r2)) m1 m2)))

   ; matrix-add: Add two matrices
   (matrix-add (fn (m1 m2)
     (matrix-zip-with + m1 m2)))

   ; matrix-scale: Scale matrix by scalar
   (matrix-scale (fn (k m)
     (matrix-map (fn (x) (* k x)) m)))

   ; matrix-multiply: Matrix multiplication
   (matrix-multiply (fn (a b)
     (let ((bt (matrix-transpose b)))
       (map (fn (row-a)
              (map (fn (col-b)
                     (foldl + 0 (zip-with * row-a col-b)))
                   bt))
            a))))

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

   ; implies: Logical implication (p -> q)
   (implies (fn (p q)
     (or (not p) q)))

   ; iff: Logical biconditional (p <-> q)
   (iff (fn (p q)
     (eq? p q)))

   ; nand: Not-and
   (nand (fn (p q)
     (not (and p q))))

   ; nor: Not-or
   (nor (fn (p q)
     (not (or p q))))

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

   ; state-return: Wrap value in state monad
   (state-return (fn (a)
     (fn (s) (cons a s))))

   ; state-bind: Bind/flatMap for state monad
   (state-bind (fn (ma f)
     (fn (s)
       (let ((result (ma s)))
         ((f (car result)) (cdr result))))))

   ; state-get: Get current state
   (state-get (fn (s) (cons s s)))

   ; state-put: Set new state
   (state-put (fn (new-s)
     (fn (s) (cons #f new-s))))

   ; state-modify: Modify state with function
   (state-modify (fn (f)
     (fn (s) (cons #f (f s)))))

   ; state-run: Run state computation
   (state-run (fn (ma s)
     (ma s)))

   ; state-eval: Run and get result only
   (state-eval (fn (ma s)
     (car (ma s))))

   ; state-exec: Run and get final state only
   (state-exec (fn (ma s)
     (cdr (ma s))))

   ; ============================================
   ; Reader Monad Utilities
   ; Reader r a = r -> a
   ; ============================================

   ; reader-return: Wrap value in reader monad
   (reader-return (fn (a)
     (fn (r) a)))

   ; reader-bind: Bind/flatMap for reader monad
   (reader-bind (fn (ma f)
     (fn (r)
       ((f (ma r)) r))))

   ; reader-ask: Get the environment
   (reader-ask (fn (r) r))

   ; reader-asks: Get part of the environment
   (reader-asks (fn (f)
     (fn (r) (f r))))

   ; reader-local: Run with modified environment
   (reader-local (fn (f ma)
     (fn (r) (ma (f r)))))

   ; reader-run: Run reader computation
   (reader-run (fn (ma r)
     (ma r)))

   ; ============================================
   ; Writer Monad Utilities (using lists for log)
   ; Writer w a = (a, [w])
   ; ============================================

   ; writer-return: Wrap value in writer monad
   (writer-return (fn (a)
     (cons a '())))

   ; writer-bind: Bind/flatMap for writer monad
   (writer-bind (fn (wa f)
     (let ((result (f (car wa))))
       (cons (car result) (append (cdr wa) (cdr result))))))

   ; writer-tell: Write to log
   (writer-tell (fn (w)
     (cons #f (list w))))

   ; writer-listen: Get the log
   (writer-listen (fn (wa)
     (cons (cons (car wa) (cdr wa)) (cdr wa))))

   ; writer-run: Extract result and log
   (writer-run id)

   ; ============================================
   ; More List Utilities
   ; ============================================

   ; rotate-left: Rotate list left by n positions
   (rotate-left (fn (n lst)
     (if (or (null? lst) (= n 0))
         lst
         (let ((len (length lst)))
           (let ((n-mod (mod n len)))
             (append (drop lst n-mod) (take lst n-mod)))))))

   ; rotate-right: Rotate list right by n positions
   (rotate-right (fn (n lst)
     (if (null? lst)
         lst
         (rotate-left (- (length lst) (mod n (length lst))) lst))))

   ; tails: All suffixes of a list
   (tails (fix tails
     (fn (lst)
       (if (null? lst)
           (list '())
           (cons lst (tails (cdr lst)))))))

   ; inits: All prefixes of a list
   (inits (fix inits
     (fn (lst)
       (cons '()
             (if (null? lst)
                 '()
                 (map (fn (t) (cons (car lst) t))
                      (inits (cdr lst))))))))

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
   (split-at (fn (n lst)
     (cons (take lst n) (drop lst n))))

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

   ; interleave: Interleave two lists
   (interleave (fix interleave
     (fn (lst1 lst2)
       (if (null? lst1)
           lst2
           (cons (car lst1) (interleave lst2 (cdr lst1)))))))

   ; dedup-consecutive: Remove consecutive duplicates
   (dedup-consecutive (fix dedup-consecutive
     (fn (lst)
       (if (null? lst)
           '()
           (if (null? (cdr lst))
               lst
               (if (equal? (car lst) (car (cdr lst)))
                   (dedup-consecutive (cdr lst))
                   (cons (car lst) (dedup-consecutive (cdr lst)))))))))

   ; group-by: Group elements by key function
   (group-by (fn (key-fn lst)
     (foldl (fn (acc x)
              (let ((k (key-fn x)))
                (let ((existing (assoc k acc)))
                  (if existing
                      (assoc-set k (cons x (cdr existing)) acc)
                      (cons (cons k (list x)) acc)))))
            '()
            lst)))

   ; frequencies: Count occurrences
   (frequencies (fn (lst)
     (foldl (fn (acc x)
              (let ((existing (assoc x acc)))
                (if existing
                    (assoc-set x (+ 1 (cdr existing)) acc)
                    (cons (cons x 1) acc))))
            '()
            lst)))

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
   (first (fn (f)
     (fn (pair)
       (cons (f (car pair)) (cdr pair)))))

   ; second: Apply arrow to second element of pair
   (second (fn (f)
     (fn (pair)
       (cons (car pair) (f (cdr pair))))))

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
   (average (fn (lst)
     (if (null? lst)
         0
         (/ (sum-list lst) (length lst)))))

   ; variance: Variance of a list of numbers
   (variance (fn (lst)
     (if (null? lst)
         0
         (let ((avg (average lst)))
           (average (map (fn (x) (let ((d (- x avg))) (* d d))) lst))))))

   ; stddev: Standard deviation
   (stddev (fn (lst)
     (sqrt (variance lst))))

   ; median: Median of a list
   (median (fn (lst)
     (let ((sorted (sort lst)))
       (let ((n (length sorted)))
         (if (odd? n)
             (list-ref sorted (/ n 2))
             (/ (+ (list-ref sorted (- (/ n 2) 1))
                   (list-ref sorted (/ n 2)))
                2))))))

   ; clamp-list: Clamp all values in list to range
   (clamp-list (fn (lo hi lst)
     (map (fn (x) (clamp lo hi x)) lst)))

   ; normalize-list: Normalize list to [0, 1] range
   (normalize-list (fn (lst)
     (if (null? lst)
         '()
         (let ((lo (apply min lst))
               (hi (apply max lst)))
           (if (= lo hi)
               (replicate (length lst) 0)
               (map (fn (x) (/ (- x lo) (- hi lo))) lst))))))

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

   ; alist-update: Update value at key with function
   (alist-update (fn (key f alist)
     (map (fn (pair)
            (if (equal? (car pair) key)
                (cons key (f (cdr pair)))
                pair))
          alist)))

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
   (tree-size (fix tree-size
     (fn (t)
       (if (tree-empty? t)
           0
           (+ 1 (tree-size (tree-left t))
                (tree-size (tree-right t)))))))

   ; tree-height: Height of tree
   (tree-height (fix tree-height
     (fn (t)
       (if (tree-empty? t)
           0
           (+ 1 (max (tree-height (tree-left t))
                     (tree-height (tree-right t))))))))

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

   ; just: Wrap value in Just
   (just (fn (x) (cons 'just x)))

   ; nothing: The Nothing value
   (nothing 'nothing)

   ; just?: Check if Just
   (just? (fn (m)
     (if (pair? m)
         (eq? (car m) 'just)
         #f)))

   ; nothing?: Check if Nothing
   (nothing? (fn (m) (eq? m 'nothing)))

   ; from-just: Extract value from Just (unsafe)
   (from-just cdr)

   ; maybe: Catamorphism for Maybe
   (maybe (fn (default f m)
     (if (just? m)
         (f (from-just m))
         default)))

   ; maybe-map: Map over Maybe
   (maybe-map (fn (f m)
     (if (just? m)
         (just (f (from-just m)))
         nothing)))

   ; maybe-bind: Bind for Maybe monad
   (maybe-bind (fn (m f)
     (if (just? m)
         (f (from-just m))
         nothing)))

   ; maybe-or: Return first Just, or Nothing
   (maybe-or (fn (m1 m2)
     (if (just? m1) m1 m2)))

   ; maybe-and: Return second if both Just, else Nothing
   (maybe-and (fn (m1 m2)
     (if (just? m1)
         (if (just? m2) m2 nothing)
         nothing)))

   ; from-maybe: Extract with default
   (from-maybe (fn (default m)
     (if (just? m) (from-just m) default)))

   ; list->maybe: Empty list to Nothing, non-empty to Just first
   (list->maybe (fn (lst)
     (if (null? lst) nothing (just (car lst)))))

   ; maybe->list: Just to singleton, Nothing to empty
   (maybe->list (fn (m)
     (if (just? m) (list (from-just m)) '())))

   ; cat-maybes: Filter Nothing values, extract Just values
   (cat-maybes (fn (lst)
     (filter-map from-just (filter just? lst))))

   ; map-maybe: Map and filter in one pass
   (map-maybe (fn (f lst)
     (cat-maybes (map f lst))))

   ; ============================================
   ; Either Extensions
   ; ============================================

   ; left: Create Left value
   (left (fn (x) (cons 'left x)))

   ; right: Create Right value
   (right (fn (x) (cons 'right x)))

   ; left?: Check if Left
   (left? (fn (e)
     (if (pair? e)
         (eq? (car e) 'left)
         #f)))

   ; right?: Check if Right
   (right? (fn (e)
     (if (pair? e)
         (eq? (car e) 'right)
         #f)))

   ; from-left: Extract Left value (unsafe)
   (from-left cdr)

   ; from-right: Extract Right value (unsafe)
   (from-right cdr)

   ; either: Catamorphism for Either
   (either (fn (f g e)
     (if (left? e)
         (f (from-left e))
         (g (from-right e)))))

   ; either-map: Map over Right
   (either-map (fn (f e)
     (if (right? e)
         (right (f (from-right e)))
         e)))

   ; either-map-left: Map over Left
   (either-map-left (fn (f e)
     (if (left? e)
         (left (f (from-left e)))
         e)))

   ; either-bind: Bind for Either monad
   (either-bind (fn (e f)
     (if (right? e)
         (f (from-right e))
         e)))

   ; either-bimap: Map over both sides
   (either-bimap (fn (f g e)
     (if (left? e)
         (left (f (from-left e)))
         (right (g (from-right e))))))

   ; partition-eithers: Split list of Eithers
   (partition-eithers (fn (lst)
     (let ((go (fix go
             (fn (lst lefts rights)
               (if (null? lst)
                   (cons (reverse lefts) (reverse rights))
                   (let ((e (car lst)))
                     (if (left? e)
                         (go (cdr lst) (cons (from-left e) lefts) rights)
                         (go (cdr lst) lefts (cons (from-right e) rights)))))))))
       (go lst '() '()))))

   ; lefts: Extract all Left values
   (lefts (fn (lst)
     (car (partition-eithers lst))))

   ; rights: Extract all Right values
   (rights (fn (lst)
     (cdr (partition-eithers lst))))

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
   (repeat-string string-repeat)

   ; ============================================
   ; Control Flow Extensions
   ; ============================================

   ; guard: Return value if condition true, else empty list
   (guard (fn (condition value)
     (if condition (list value) '())))

   ; when-just: Execute function only if Just
   (when-just (fn (m f)
     (if (just? m)
         (f (from-just m))
         nothing)))

   ; unless-nothing: Execute function unless Nothing
   (unless-nothing when-just)

   ; if-let: Bind and conditionally execute
   (if-let (fn (m then-fn else-val)
     (if (just? m)
         (then-fn (from-just m))
         else-val)))

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
   (comparing (fn (key-fn)
     (fn (a b)
       (let ((ka (key-fn a))
             (kb (key-fn b)))
         (if (< ka kb)
             -1
             (if (> ka kb)
                 1
                 0))))))

   ; compare-by: Compare two values by key function
   (compare-by (fn (key-fn a b)
     ((comparing key-fn) a b)))

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
   (sort-by (fn (key-fn lst)
     (map cdr
          (sort (map (fn (x) (cons (key-fn x) x)) lst)))))

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

   ; coprime?: Check if two numbers are coprime
   (coprime? (fn (a b)
     (= 1 (gcd a b))))

   ; perfect-square?: Check if n is a perfect square
   (perfect-square? (fn (n)
     (let ((s (floor (sqrt n))))
       (= n (* s s)))))

   ; triangular-number: nth triangular number
   (triangular-number (fn (n)
     (/ (* n (+ n 1)) 2)))

   ; is-triangular?: Check if n is triangular
   (is-triangular? (fn (n)
     (let ((k (floor (sqrt (* 2 n)))))
       (= n (triangular-number k)))))

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
   (stream-cons (fn (head tail-thunk)
     (cons head tail-thunk)))

   ; stream-head: Get first element
   (stream-head (fn (s) (car s)))

   ; stream-tail: Force and get tail
   (stream-tail (fn (s)
     ((cdr s))))

   ; stream-take: Take first n elements as list
   (stream-take (fix take-rec
     (fn (n s)
       (if (or (= n 0) (stream-empty? s))
           '()
           (cons (stream-head s)
                 (take-rec (- n 1) (stream-tail s)))))))

   ; stream-drop: Drop first n elements
   (stream-drop (fix drop-rec
     (fn (n s)
       (if (or (= n 0) (stream-empty? s))
           s
           (drop-rec (- n 1) (stream-tail s))))))

   ; stream-map: Map function over stream
   (stream-map (fix map-rec
     (fn (f s)
       (if (stream-empty? s)
           stream-empty
           (stream-cons (f (stream-head s))
                        (fn () (map-rec f (stream-tail s))))))))

   ; stream-filter: Filter stream
   (stream-filter (fix filter-rec
     (fn (pred-fn s)
       (if (stream-empty? s)
           stream-empty
           (if (pred-fn (stream-head s))
               (stream-cons (stream-head s)
                            (fn () (filter-rec pred-fn (stream-tail s))))
               (filter-rec pred-fn (stream-tail s)))))))

   ; stream-from: Infinite stream starting at n
   (stream-from (fix from-rec
     (fn (n)
       (stream-cons n (fn () (from-rec (+ n 1)))))))

   ; stream-iterate: Infinite stream by iterating function
   (stream-iterate (fix iterate-rec
     (fn (f x)
       (stream-cons x (fn () (iterate-rec f (f x)))))))

   ; stream-repeat: Infinite stream of same value
   (stream-repeat (fix repeat-rec
     (fn (x)
       (stream-cons x (fn () (repeat-rec x))))))

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
   (stream-zip-with (fix zip-rec
     (fn (f s1 s2)
       (if (or (stream-empty? s1) (stream-empty? s2))
           stream-empty
           (stream-cons (f (stream-head s1) (stream-head s2))
                        (fn () (zip-rec f (stream-tail s1) (stream-tail s2))))))))

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
   (naturals (stream-from 0))

   ; ============================================
   ; Graph Algorithms (adjacency list representation)
   ; Graph = alist of (node . list-of-neighbors)
   ; ============================================

   ; graph-nodes: Get all nodes in graph
   (graph-nodes alist-keys)

   ; graph-neighbors: Get neighbors of node
   (graph-neighbors (fn (g node)
     (let ((entry (assoc node g)))
       (if entry (cdr entry) '()))))

   ; graph-add-edge: Add directed edge
   (graph-add-edge (fn (g from to)
     (let ((neighbors (graph-neighbors g from)))
       (assoc-set from (cons to neighbors) g))))

   ; graph-add-undirected-edge: Add undirected edge
   (graph-add-undirected-edge (fn (g a b)
     (graph-add-edge (graph-add-edge g a b) b a)))

   ; graph-has-edge?: Check if edge exists
   (graph-has-edge? (fn (g from to)
     (member to (graph-neighbors g from))))

   ; graph-in-degree: Count incoming edges
   (graph-in-degree (fn (g node)
     (foldl (fn (count pair)
              (if (member node (cdr pair))
                  (+ count 1)
                  count))
            0
            g)))

   ; graph-out-degree: Count outgoing edges
   (graph-out-degree (fn (g node)
     (length (graph-neighbors g node))))

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
   (graph-transpose (fn (g)
     (let ((nodes (graph-nodes g)))
       (foldl (fn (acc pair)
                (let ((from (car pair))
                      (neighbors (cdr pair)))
                  (foldl (fn (a to)
                           (graph-add-edge a to from))
                         acc
                         neighbors)))
              (map (fn (n) (cons n '())) nodes)
              g))))

   ; graph-reachable: All nodes reachable from start (BFS)
   (graph-reachable (fn (g start)
     (let ((bfs (fix bfs
             (fn (queue visited)
               (if (null? queue)
                   visited
                   (let ((node (car queue))
                         (rest (cdr queue)))
                     (if (member node visited)
                         (bfs rest visited)
                         (let ((neighbors (graph-neighbors g node)))
                           (bfs (append rest neighbors)
                                (cons node visited))))))))))
       (reverse (bfs (list start) '())))))

   ; graph-path-exists?: Check if path exists between two nodes
   (graph-path-exists? (fn (g from to)
     (member to (graph-reachable g from))))

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

   ; is-power-of-2?: Check if n is a power of 2
   (is-power-of-2? (fn (n)
     (if (<= n 0)
         #f
         (= 0 (bitand n (- n 1))))))

   ; next-power-of-2: Next power of 2 >= n
   (next-power-of-2 (fn (n)
     (if (<= n 1)
         1
         (let ((find (fix find
                 (fn (p)
                   (if (>= p n)
                       p
                       (find (shl p 1)))))))
           (find 1)))))

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
   (string-null? (fn (s)
     (= (string-length s) 0)))

   ; string-take: Take first n characters
   (string-take (fn (n s)
     (substring s 0 (min n (string-length s)))))

   ; string-drop: Drop first n characters
   (string-drop (fn (n s)
     (substring s (min n (string-length s)) (string-length s))))

   ; string-take-right: Take last n characters
   (string-take-right (fn (n s)
     (let ((len (string-length s)))
       (substring s (max 0 (- len n)) len))))

   ; string-drop-right: Drop last n characters
   (string-drop-right (fn (n s)
     (let ((len (string-length s)))
       (substring s 0 (max 0 (- len n))))))

   ; string-pad: Pad string on left to width
   (string-pad (fn (s width char)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append (make-string (- width len) char) s)))))

   ; string-pad-right: Pad string on right to width
   (string-pad-right (fn (s width char)
     (let ((len (string-length s)))
       (if (>= len width)
           s
           (string-append s (make-string (- width len) char))))))

   ; string-trim-left: Remove leading chars matching predicate
   (string-trim-left (fix trim-left-rec
     (fn (pred s)
       (if (string-null? s)
           s
           (if (pred (string-ref s 0))
               (trim-left-rec pred (string-drop 1 s))
               s)))))

   ; string-trim-right: Remove trailing chars matching predicate
   (string-trim-right (fn (pred s)
     ((fix trim-right-rec
        (fn (i)
          (if (<= i 0)
              ""
              (if (pred (string-ref s (- i 1)))
                  (trim-right-rec (- i 1))
                  (substring s 0 i)))))
      (string-length s))))

   ; string-trim: Remove chars from both ends
   (string-trim (fn (pred s)
     (string-trim-right pred (string-trim-left pred s))))

   ; string-trim-whitespace: Trim whitespace from both ends
   (string-trim-whitespace (fn (s)
     (string-trim (fn (c) (or (char=? c #\space)
                               (char=? c #\tab)
                               (char=? c #\newline)))
                  s)))

   ; string-contains?: Check if s1 contains s2
   (string-contains? (fix contains-rec
     (fn (s1 s2)
       (let ((len1 (string-length s1))
             (len2 (string-length s2)))
         (if (< len1 len2)
             #f
             (if (string=? (string-take len2 s1) s2)
                 #t
                 (contains-rec (string-drop 1 s1) s2)))))))

   ; string-prefix?: Check if s2 is prefix of s1
   (string-prefix? (fn (s1 s2)
     (let ((len1 (string-length s1))
           (len2 (string-length s2)))
       (if (< len1 len2)
           #f
           (string=? (string-take len2 s1) s2)))))

   ; string-suffix?: Check if s2 is suffix of s1
   (string-suffix? (fn (s1 s2)
     (let ((len1 (string-length s1))
           (len2 (string-length s2)))
       (if (< len1 len2)
           #f
           (string=? (string-take-right len2 s1) s2)))))

   ; string-split-at: Split string at index
   (string-split-at (fn (s i)
     (cons (string-take i s) (string-drop i s))))

   ; string-replace-first: Replace first occurrence of old with new
   (string-replace-first (fix replace-first-rec
     (fn (s old new)
       (let ((len-s (string-length s))
             (len-old (string-length old)))
         (if (< len-s len-old)
             s
             (if (string=? (string-take len-old s) old)
                 (string-append new (string-drop len-old s))
                 (string-append (string-take 1 s)
                                (replace-first-rec (string-drop 1 s) old new))))))))

   ; string-replace-all: Replace all occurrences of old with new
   (string-replace-all (fix replace-all-rec
     (fn (s old new)
       (let ((len-s (string-length s))
             (len-old (string-length old)))
         (if (< len-s len-old)
             s
             (if (string=? (string-take len-old s) old)
                 (string-append new (replace-all-rec (string-drop len-old s) old new))
                 (string-append (string-take 1 s)
                                (replace-all-rec (string-drop 1 s) old new))))))))

   ; string-reverse: Reverse a string
   (string-reverse (fn (s)
     (list->string (reverse (string->list s)))))

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
   (string-split-char (fn (s delim)
     ((fix split-rec
        (fn (chars current acc)
          (if (null? chars)
              (reverse (cons (list->string (reverse current)) acc))
              (if (char=? (car chars) delim)
                  (split-rec (cdr chars) '() (cons (list->string (reverse current)) acc))
                  (split-rec (cdr chars) (cons (car chars) current) acc)))))
      (string->list s) '() '())))

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
   (string-count (fn (s c)
     (length (filter (fn (x) (char=? x c)) (string->list s)))))

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
   (assv (fix assv-rec
     (fn (key alist)
       (if (null? alist)
           #f
           (if (eqv? key (caar alist))
               (car alist)
               (assv-rec key (cdr alist)))))))

   ; alist-ref: Get value for key (with optional default)
   (alist-ref (fn (key alist default)
     (let ((pair (assoc key alist)))
       (if pair (cdr pair) default))))

   ; alist-set: Set or update key-value pair
   (alist-set (fn (key val alist)
     (cons (cons key val)
           (filter (fn (pair) (not (equal? key (car pair)))) alist))))

   ; alist-delete: Remove all pairs with key
   (alist-delete (fn (key alist)
     (filter (fn (pair) (not (equal? key (car pair)))) alist)))

   ; alist-keys: Get all keys
   (alist-keys (fn (alist) (map car alist)))

   ; alist-values: Get all values
   (alist-values (fn (alist) (map cdr alist)))

   ; alist-map: Map over values
   (alist-map (fn (f alist)
     (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

   ; alist-filter: Filter pairs by predicate on key-value
   (alist-filter (fn (pred alist)
     (filter (fn (pair) (pred (car pair) (cdr pair))) alist)))

   ; alist-merge: Merge two alists (second takes precedence)
   (alist-merge (fn (alist1 alist2)
     (foldl (fn (acc pair) (alist-set (car pair) (cdr pair) acc))
            alist1
            alist2)))

   ; alist->list: Convert alist to list of key-value lists
   (alist->list (fn (alist)
     (map (fn (pair) (list (car pair) (cdr pair))) alist)))

   ; list->alist: Convert list of key-value lists to alist
   (list->alist (fn (lst)
     (map (fn (kv) (cons (car kv) (cadr kv))) lst)))

   ; alist-has-key?: Check if key exists
   (alist-has-key? (fn (key alist)
     (not (eq? (assoc key alist) #f))))

   ; alist-update: Update value for key using function
   (alist-update (fn (key f default alist)
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
   (set-member? (fix member-rec
     (fn (x s)
       (if (null? s)
           #f
           (if (equal? x (car s))
               #t
               (member-rec x (cdr s)))))))

   ; set-insert: Insert element (maintains uniqueness)
   (set-insert (fn (x s)
     (if (set-member? x s)
         s
         (cons x s))))

   ; set-delete: Remove element
   (set-delete (fn (x s)
     (filter (fn (y) (not (equal? x y))) s)))

   ; set-union: Union of two sets
   (set-union (fn (s1 s2)
     (foldl (fn (acc x) (set-insert x acc)) s1 s2)))

   ; set-intersection: Intersection of two sets
   (set-intersection (fn (s1 s2)
     (filter (fn (x) (set-member? x s2)) s1)))

   ; set-difference: Set difference (s1 - s2)
   (set-difference (fn (s1 s2)
     (filter (fn (x) (not (set-member? x s2))) s1)))

   ; set-symmetric-difference: Symmetric difference
   (set-symmetric-difference (fn (s1 s2)
     (set-union (set-difference s1 s2)
                (set-difference s2 s1))))

   ; set-subset?: Check if s1 is subset of s2
   (set-subset? (fn (s1 s2)
     (all (fn (x) (set-member? x s2)) s1)))

   ; set-superset?: Check if s1 is superset of s2
   (set-superset? (fn (s1 s2)
     (set-subset? s2 s1)))

   ; set-equal?: Check if sets are equal
   (set-equal? (fn (s1 s2)
     (and (set-subset? s1 s2)
          (set-subset? s2 s1))))

   ; set-disjoint?: Check if sets have no common elements
   (set-disjoint? (fn (s1 s2)
     (null? (set-intersection s1 s2))))

   ; set-size: Number of elements
   (set-size length)

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
   ; Queue (Functional, Two-List Implementation)
   ; ============================================

   ; queue-empty: Create empty queue
   (queue-empty (cons '() '()))

   ; queue-empty?: Check if queue is empty
   (queue-empty? (fn (q)
     (and (null? (car q)) (null? (cdr q)))))

   ; queue-enqueue: Add element to back
   (queue-enqueue (fn (x q)
     (cons (car q) (cons x (cdr q)))))

   ; queue-front: Get front element
   (queue-front (fn (q)
     (if (null? (car q))
         (if (null? (cdr q))
             (error "queue-front: empty queue")
             (car (reverse (cdr q))))
         (caar q))))

   ; queue-dequeue: Remove front element
   (queue-dequeue (fn (q)
     (if (null? (car q))
         (if (null? (cdr q))
             (error "queue-dequeue: empty queue")
             (let ((front (reverse (cdr q))))
               (cons (cdr front) '())))
         (cons (cdar q) (cdr q)))))

   ; queue-size: Number of elements
   (queue-size (fn (q)
     (+ (length (car q)) (length (cdr q)))))

   ; queue->list: Convert to list (front to back)
   (queue->list (fn (q)
     (append (car q) (reverse (cdr q)))))

   ; list->queue: Create queue from list
   (list->queue (fn (lst)
     (cons lst '())))

   ; ============================================
   ; Deque (Double-ended Queue)
   ; ============================================

   ; deque-empty: Create empty deque
   (deque-empty (cons '() '()))

   ; deque-empty?: Check if deque is empty
   (deque-empty? (fn (d)
     (and (null? (car d)) (null? (cdr d)))))

   ; deque-push-front: Add to front
   (deque-push-front (fn (x d)
     (cons (cons x (car d)) (cdr d))))

   ; deque-push-back: Add to back
   (deque-push-back (fn (x d)
     (cons (car d) (cons x (cdr d)))))

   ; deque-front: Get front element
   (deque-front (fn (d)
     (if (null? (car d))
         (if (null? (cdr d))
             (error "deque-front: empty deque")
             (car (reverse (cdr d))))
         (caar d))))

   ; deque-back: Get back element
   (deque-back (fn (d)
     (if (null? (cdr d))
         (if (null? (car d))
             (error "deque-back: empty deque")
             (car (reverse (car d))))
         (cadr d))))

   ; deque-pop-front: Remove front element
   (deque-pop-front (fn (d)
     (if (null? (car d))
         (if (null? (cdr d))
             (error "deque-pop-front: empty deque")
             (let ((front (reverse (cdr d))))
               (cons (cdr front) '())))
         (cons (cdar d) (cdr d)))))

   ; deque-pop-back: Remove back element
   (deque-pop-back (fn (d)
     (if (null? (cdr d))
         (if (null? (car d))
             (error "deque-pop-back: empty deque")
             (let ((back (reverse (car d))))
               (cons '() (cdr back))))
         (cons (car d) (cddr d)))))

   ; deque-size: Number of elements
   (deque-size (fn (d)
     (+ (length (car d)) (length (cdr d)))))

   ; deque->list: Convert to list
   (deque->list (fn (d)
     (append (car d) (reverse (cdr d)))))

   ; list->deque: Create deque from list
   (list->deque (fn (lst)
     (cons lst '())))

   ; ============================================
   ; Sorting Algorithms
   ; ============================================

   ; insert-sorted: Insert into sorted list
   (insert-sorted (fix insert-rec
     (fn (x lst cmp)
       (if (null? lst)
           (list x)
           (if (cmp x (car lst))
               (cons x lst)
               (cons (car lst) (insert-rec x (cdr lst) cmp)))))))

   ; insertion-sort: Insertion sort with comparator
   (insertion-sort (fn (lst cmp)
     (foldl (fn (acc x) (insert-sorted x acc cmp)) '() lst)))

   ; merge-sorted: Merge two sorted lists
   (merge-sorted (fix merge-rec
     (fn (l1 l2 cmp)
       (if (null? l1)
           l2
           (if (null? l2)
               l1
               (if (cmp (car l1) (car l2))
                   (cons (car l1) (merge-rec (cdr l1) l2 cmp))
                   (cons (car l2) (merge-rec l1 (cdr l2) cmp))))))))

   ; merge-sort: Merge sort with comparator
   (merge-sort (fix merge-sort-rec
     (fn (lst cmp)
       (let ((len (length lst)))
         (if (<= len 1)
             lst
             (let ((mid (quotient len 2)))
               (merge-sorted
                 (merge-sort-rec (take mid lst) cmp)
                 (merge-sort-rec (drop mid lst) cmp)
                 cmp)))))))

   ; quicksort: Quicksort with comparator
   (quicksort (fix quicksort-rec
     (fn (lst cmp)
       (if (null? lst)
           '()
           (let ((pivot (car lst))
                 (rest (cdr lst)))
             (append (quicksort-rec (filter (fn (x) (cmp x pivot)) rest) cmp)
                     (cons pivot
                           (quicksort-rec (filter (fn (x) (not (cmp x pivot))) rest) cmp))))))))

   ; sort: Default sort (uses merge-sort with <)
   (sort (fn (lst)
     (merge-sort lst <)))

   ; sort-by: Sort using key function
   (sort-by-key (fn (key-fn lst)
     (map cdr
       (merge-sort (map (fn (x) (cons (key-fn x) x)) lst)
                   (fn (a b) (< (car a) (car b)))))))

   ; sort-descending: Sort in descending order
   (sort-descending (fn (lst)
     (merge-sort lst >)))

   ; sorted?: Check if list is sorted
   (sorted? (fix sorted-rec
     (fn (lst cmp)
       (if (null? lst)
           #t
           (if (null? (cdr lst))
               #t
               (if (cmp (car lst) (cadr lst))
                   (sorted-rec (cdr lst) cmp)
                   (if (equal? (car lst) (cadr lst))
                       (sorted-rec (cdr lst) cmp)
                       #f)))))))

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
   (pq-empty? null?)

   ; pq-insert: Insert with priority (lower = higher priority)
   (pq-insert (fn (priority value pq)
     (insert-sorted (cons priority value) pq
                    (fn (a b) (< (car a) (car b))))))

   ; pq-peek: Get highest priority element
   (pq-peek (fn (pq)
     (if (null? pq)
         (error "pq-peek: empty queue")
         (cdar pq))))

   ; pq-peek-priority: Get highest priority value
   (pq-peek-priority (fn (pq)
     (if (null? pq)
         (error "pq-peek-priority: empty queue")
         (caar pq))))

   ; pq-pop: Remove highest priority element
   (pq-pop (fn (pq)
     (if (null? pq)
         (error "pq-pop: empty queue")
         (cdr pq))))

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
   (bag-count (fn (x bag)
     (alist-ref x bag 0)))

   ; bag-remove: Remove one occurrence
   (bag-remove (fn (x bag)
     (let ((current (alist-ref x bag 0)))
       (if (<= current 1)
           (alist-delete x bag)
           (alist-set x (- current 1) bag)))))

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
   (bag-union (fn (b1 b2)
     (let ((keys (set-union (alist-keys b1) (alist-keys b2))))
       (map (fn (k) (cons k (max (bag-count k b1) (bag-count k b2))))
            keys))))

   ; bag-intersection: Intersection (min of counts)
   (bag-intersection (fn (b1 b2)
     (let ((keys (set-intersection (alist-keys b1) (alist-keys b2))))
       (filter (fn (pair) (> (cdr pair) 0))
         (map (fn (k) (cons k (min (bag-count k b1) (bag-count k b2))))
              keys)))))

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
   (chunks (fn (n lst)
     ((fix chunk-rec
        (fn (remaining acc)
          (if (null? remaining)
              (reverse acc)
              (chunk-rec (drop-n n remaining) (cons (take-n n remaining) acc)))))
      lst '())))

   ; chunks-exact: Split into exact chunks (drop remainder)
   (chunks-exact (fn (n lst)
     (filter (fn (chunk) (= (length chunk) n)) (chunks n lst))))

   ; interleave: Interleave two lists
   (interleave (fix interleave-rec
     (fn (l1 l2)
       (if (null? l1)
           l2
           (if (null? l2)
               l1
               (cons (car l1) (cons (car l2) (interleave-rec (cdr l1) (cdr l2)))))))))

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

   ; rotate-left: Rotate list left by n positions
   (rotate-left (fn (n lst)
     (let ((len (length lst)))
       (if (= len 0)
           lst
           (let ((n-mod (mod n len)))
             (append (drop-n n-mod lst) (take-n n-mod lst)))))))

   ; rotate-right: Rotate list right by n positions
   (rotate-right (fn (n lst)
     (rotate-left (- (length lst) (mod n (length lst))) lst)))

   ; frequencies: Count occurrences of each element
   (frequencies (fn (lst)
     (foldl (fn (acc x)
              (alist-update x (fn (c) (+ c 1)) 0 acc))
            '()
            lst)))

   ; group-by: Group elements by key function
   (group-by (fn (key-fn lst)
     (foldl (fn (acc x)
              (let ((k (key-fn x)))
                (alist-update k (fn (vs) (cons x vs)) '() acc)))
            '()
            lst)))

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

   ; split-when: Split list whenever predicate is true
   (split-when (fn (pred lst)
     ((fix split-rec
        (fn (remaining current acc)
          (if (null? remaining)
              (reverse (if (null? current) acc (cons (reverse current) acc)))
              (if (pred (car remaining))
                  (split-rec (cdr remaining) '()
                             (if (null? current) acc (cons (reverse current) acc)))
                  (split-rec (cdr remaining) (cons (car remaining) current) acc)))))
      lst '() '())))

   ; dedupe: Remove consecutive duplicates
   (dedupe (fn (lst)
     (if (null? lst)
         '()
         ((fix dedupe-rec
            (fn (prev remaining acc)
              (if (null? remaining)
                  (reverse acc)
                  (if (equal? prev (car remaining))
                      (dedupe-rec prev (cdr remaining) acc)
                      (dedupe-rec (car remaining) (cdr remaining) (cons (car remaining) acc))))))
          (car lst) (cdr lst) (list (car lst))))))

   ; dedupe-by: Remove consecutive duplicates by key function
   (dedupe-by (fn (key-fn lst)
     (if (null? lst)
         '()
         ((fix dedupe-rec
            (fn (prev-key remaining acc)
              (if (null? remaining)
                  (reverse acc)
                  (let ((curr-key (key-fn (car remaining))))
                    (if (equal? prev-key curr-key)
                        (dedupe-rec prev-key (cdr remaining) acc)
                        (dedupe-rec curr-key (cdr remaining) (cons (car remaining) acc)))))))
          (key-fn (car lst)) (cdr lst) (list (car lst))))))

   ; run-length-encode: Encode consecutive runs
   (run-length-encode (fn (lst)
     (if (null? lst)
         '()
         ((fix rle-rec
            (fn (current count remaining acc)
              (if (null? remaining)
                  (reverse (cons (cons count current) acc))
                  (if (equal? current (car remaining))
                      (rle-rec current (+ count 1) (cdr remaining) acc)
                      (rle-rec (car remaining) 1 (cdr remaining)
                               (cons (cons count current) acc))))))
          (car lst) 1 (cdr lst) '()))))

   ; run-length-decode: Decode run-length encoding
   (run-length-decode (fn (encoded)
     (apply append
       (map (fn (pair) (replicate (car pair) (cdr pair))) encoded))))

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

   ; validate-all: Run all validations, collecting errors
   (validate-all (fn (validations)
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
   (mean (fn (lst)
     (if (null? lst)
         0
         (/ (sum-list lst) (length lst)))))

   ; variance: Calculate population variance
   (variance (fn (lst)
     (if (null? lst)
         0
         (let ((m (mean lst)))
           (mean (map (fn (x) (* (- x m) (- x m))) lst))))))

   ; std-dev: Calculate population standard deviation
   (std-dev (fn (lst)
     (sqrt (variance lst))))

   ; median: Calculate median
   (median (fn (lst)
     (if (null? lst)
         0
         (let ((sorted (sort lst))
               (n (length lst)))
           (if (odd? n)
               (list-ref sorted (quotient n 2))
               (/ (+ (list-ref sorted (- (quotient n 2) 1))
                     (list-ref sorted (quotient n 2)))
                  2))))))

   ; mode: Find most common element(s)
   (mode (fn (lst)
     (if (null? lst)
         '()
         (let ((freqs (frequencies lst)))
           (let ((max-count (maximum (map cdr freqs))))
             (map car (filter (fn (p) (= (cdr p) max-count)) freqs)))))))

   ; range-stat: Calculate range (max - min)
   (range-stat (fn (lst)
     (if (null? lst)
         0
         (- (maximum lst) (minimum lst)))))

   ; percentile: Calculate nth percentile
   (percentile (fn (n lst)
     (if (null? lst)
         0
         (let ((sorted (sort lst))
               (k (/ (* n (- (length lst) 1)) 100)))
           (let ((floor-k (floor k))
                 (ceil-k (ceiling k)))
             (if (= floor-k ceil-k)
                 (list-ref sorted floor-k)
                 (lerp (list-ref sorted floor-k)
                       (list-ref sorted ceil-k)
                       (- k floor-k))))))))

   ; quartiles: Calculate Q1, Q2 (median), Q3
   (quartiles (fn (lst)
     (list (percentile 25 lst)
           (percentile 50 lst)
           (percentile 75 lst))))

   ; correlation: Pearson correlation coefficient
   (correlation (fn (xs ys)
     (if (or (null? xs) (null? ys))
         0
         (let ((n (length xs))
               (mx (mean xs))
               (my (mean ys))
               (sx (std-dev xs))
               (sy (std-dev ys)))
           (if (or (= sx 0) (= sy 0))
               0
               (/ (mean (zip-with (fn (x y) (* (- x mx) (- y my))) xs ys))
                  (* sx sy)))))))

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

   ; first: Apply arrow to first element of pair
   (arrow-first (fn (f)
     (fn (pair) (cons (f (car pair)) (cdr pair)))))

   ; second: Apply arrow to second element of pair
   (arrow-second (fn (f)
     (fn (pair) (cons (car pair) (f (cdr pair))))))

   ; ***: Product arrow (parallel composition)
   (arrow-split (fn (f g)
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
   (string-words (fn (s)
     (filter (fn (w) (not (string=? w "")))
             (string-split s " "))))

   ; string-lines: Split on newlines
   (string-lines (fn (s)
     (string-split s "\n")))

   ; string-unwords: Join with spaces
   (string-unwords (fn (words)
     (string-join words " ")))

   ; string-unlines: Join with newlines
   (string-unlines (fn (lines)
     (string-join lines "\n")))

   ; string-pad-left: Pad string to length on left
   (string-pad-left (fn (len char s)
     (let ((pad-len (- len (string-length s))))
       (if (<= pad-len 0)
           s
           (string-append (make-string pad-len char) s)))))

   ; string-pad-right: Pad string to length on right
   (string-pad-right (fn (len char s)
     (let ((pad-len (- len (string-length s))))
       (if (<= pad-len 0)
           s
           (string-append s (make-string pad-len char))))))

   ; string-center: Center string with padding
   (string-center (fn (len char s)
     (let ((pad-len (- len (string-length s))))
       (if (<= pad-len 0)
           s
           (let ((left-pad (/ pad-len 2))
                 (right-pad (- pad-len (/ pad-len 2))))
             (string-append (make-string left-pad char)
                           s
                           (make-string right-pad char)))))))

   ; string-repeat: Repeat string n times
   (string-repeat (fn (n s)
     (string-join (replicate n s) "")))

   ; string-take-while: Take chars while predicate holds
   (string-take-while (fn (pred s)
     (list->string (take-while pred (string->list s)))))

   ; string-drop-while: Drop chars while predicate holds
   (string-drop-while (fn (pred s)
     (list->string (drop-while pred (string->list s)))))

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
   (all-equal? (fn (lst)
     (or (null? lst)
         (all (fn (x) (equal? x (car lst))) (cdr lst)))))

   ; implies: Logical implication
   (implies (fn (p q)
     (or (not p) q)))

   ; iff: Logical biconditional
   (iff (fn (p q)
     (eq? p q)))

   ; xor: Exclusive or
   (xor (fn (p q)
     (not (eq? p q))))

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

   ; char-whitespace?: Check if char is whitespace
   (char-whitespace? (fn (c)
     (let ((n (char->integer c)))
       (or (= n 32) (= n 10) (= n 9) (= n 13)))))

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

   ; graph-add-edge: Add directed edge
   (graph-add-edge (fn (from to g)
     (let ((g1 (graph-add-node from g)))
       (let ((g2 (graph-add-node to g1)))
         (alist-set from (cons to (cdr (assoc from g2))) g2)))))

   ; graph-neighbors: Get neighbors of node
   (graph-neighbors (fn (node g)
     (let ((entry (assoc node g)))
       (if entry (cdr entry) '()))))

   ; graph-nodes: Get all nodes
   (graph-nodes (fn (g) (map car g)))

   ; graph-has-edge?: Check if edge exists
   (graph-has-edge? (fn (from to g)
     (member? to (graph-neighbors from g))))

   ; graph-from-edges: Build from edge list
   (graph-from-edges (fn (edges)
     (foldl (fn (g e) (graph-add-edge (car e) (cdr e) g))
            graph-empty edges)))

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
   (graph-path-exists? (fn (from to g)
     (member? to (bfs from g))))

   ; ============================================
   ; State Monad
   ; ============================================
   ; State s a = s -> (a, s)

   ; state-return: Wrap value in state
   (state-return (fn (x)
     (fn (s) (cons x s))))

   ; state-bind: Bind for state monad
   (state-bind (fn (ma f)
     (fn (s)
       (let ((result (ma s)))
         ((f (car result)) (cdr result))))))

   ; state-get: Get current state
   (state-get (fn (s) (cons s s)))

   ; state-put: Replace state
   (state-put (fn (new-s)
     (fn (s) (cons '() new-s))))

   ; state-modify: Modify state with function
   (state-modify (fn (f)
     (fn (s) (cons '() (f s)))))

   ; state-run: Run state computation
   (state-run (fn (ma s) (ma s)))

   ; state-eval: Run and return value
   (state-eval (fn (ma s) (car (ma s))))

   ; state-exec: Run and return state
   (state-exec (fn (ma s) (cdr (ma s))))

   ; ============================================
   ; Reader Monad
   ; ============================================
   ; Reader r a = r -> a

   ; reader-return: Wrap value
   (reader-return const)

   ; reader-bind: Bind for reader
   (reader-bind (fn (ma f)
     (fn (r) ((f (ma r)) r))))

   ; reader-ask: Get environment
   (reader-ask id)

   ; reader-local: Run with modified env
   (reader-local (fn (f ma)
     (fn (r) (ma (f r)))))

   ; reader-run: Run reader
   (reader-run (fn (ma r) (ma r)))

   ; ============================================
   ; Writer Monad
   ; ============================================
   ; Writer w a = (a, w) where w is monoid (list)

   ; writer-return: Wrap value with empty log
   (writer-return (fn (x) (cons x '())))

   ; writer-bind: Bind for writer
   (writer-bind (fn (ma f)
     (let ((result (f (car ma))))
       (cons (car result) (append (cdr ma) (cdr result))))))

   ; writer-tell: Write to log
   (writer-tell (fn (w) (cons '() (list w))))

   ; writer-run: Get value and log
   (writer-run id)

   ; writer-value: Get just value
   (writer-value car)

   ; writer-log: Get just log
   (writer-log cdr)

   ; ============================================
   ; Numeric Extensions
   ; ============================================

   ; divides?: Check if a divides b
   (divides? (fn (a b) (= 0 (mod b a))))

   ; coprime?: Check if gcd is 1
   (coprime? (fn (a b) (= 1 (gcd-euclid a b))))

   ; perfect-square?: Check if n is perfect square
   (perfect-square? (fn (n)
     (let ((s (isqrt n)))
       (= (* s s) n))))

   ; triangular: nth triangular number
   (triangular (fn (n) (/ (* n (+ n 1)) 2)))

   ; pentagonal: nth pentagonal number
   (pentagonal (fn (n) (/ (* n (- (* 3 n) 1)) 2)))

   ; hexagonal: nth hexagonal number
   (hexagonal (fn (n) (* n (- (* 2 n) 1))))

   ; binomial: n choose k
   (binomial (fn (n k)
     (if (or (< k 0) (> k n))
         0
         (/ (factorial n)
            (* (factorial k) (factorial (- n k)))))))

   ; catalan: nth Catalan number
   (catalan (fn (n)
     (/ (binomial (* 2 n) n) (+ n 1))))

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
   (do-times (fn (n thunk)
     ((fix times-rec
        (fn (i acc)
          (if (>= i n)
              (reverse acc)
              (times-rec (+ i 1) (cons (thunk) acc)))))
      0 '())))

   ; do-while: Execute while condition holds, return final value
   (do-while (fn (init step-fn done-fn)
     ((fix while-rec
        (fn (val)
          (if (done-fn val)
              val
              (while-rec (step-fn val)))))
      init)))

   ; repeat-until: Execute until condition becomes true
   (repeat-until (fn (init step-fn stop-fn)
     ((fix until-rec
        (fn (val)
          (let ((next (step-fn val)))
            (if (stop-fn next)
                next
                (until-rec next)))))
      init)))

   ; for-each-indexed: Apply fn to each element with index
   (for-each-indexed (fn (fn-2 lst)
     ((fix each-rec
        (fn (i remaining)
          (if (null? remaining)
              '()
              (begin
                (fn-2 i (car remaining))
                (each-rec (+ i 1) (cdr remaining))))))
      0 lst)))

   ; loop-collect: Loop collecting results while condition holds
   (loop-collect (fn (init step-fn done-fn val-fn)
     ((fix loop-rec
        (fn (state acc)
          (if (done-fn state)
              (reverse acc)
              (loop-rec (step-fn state) (cons (val-fn state) acc)))))
      init '())))

   ; try-catch: Try thunk, on error call error-fn with error
   ; Note: Limited error handling in pure context
   (try-catch (fn (thunk error-fn)
     (thunk)))

   ; default: Return default if value is falsy
   (default (fn (val def)
     (if val val def)))

   ; first-truthy: Return first truthy value from list
   (first-truthy (fn (vals)
     ((fix coal-rec
        (fn (remaining)
          (if (null? remaining)
              #f
              (if (car remaining)
                  (car remaining)
                  (coal-rec (cdr remaining))))))
      vals)))

   ; short-circuit-and: Lazy and with thunks
   (short-and (fn (thunks)
     ((fix and-rec
        (fn (remaining)
          (if (null? remaining)
              #t
              (if ((car remaining))
                  (and-rec (cdr remaining))
                  #f))))
      thunks)))

   ; short-circuit-or: Lazy or with thunks
   (short-or (fn (thunks)
     ((fix or-rec
        (fn (remaining)
          (if (null? remaining)
              #f
              (let ((v ((car remaining))))
                (if v v (or-rec (cdr remaining)))))))
      thunks)))

   ; guard: Return value only if predicate holds, else default
   (guard (fn (val predicate-fn default-val)
     (if (predicate-fn val) val default-val)))

   ; ensure: Assert predicate holds, return value or error symbol
   (ensure (fn (val predicate-fn)
     (if (predicate-fn val) val 'assertion-failed)))

   ; chain-guards: Apply sequence of guards, short-circuit on failure
   (chain-guards (fn (val guards)
     ((fix guard-rec
        (fn (v remaining)
          (if (null? remaining)
              v
              (let ((g (car remaining)))
                (let ((result ((car g) v)))
                  (if result
                      (guard-rec v (cdr remaining))
                      (cdr g)))))))
      val guards)))

   ; ============================================================
   ; Advanced List Utilities
   ; ============================================================

   ; frequencies: Count occurrences of each element
   (frequencies (fn (lst)
     ((fix freq-rec
        (fn (remaining acc)
          (if (null? remaining)
              acc
              (let ((x (car remaining)))
                (let ((current (assoc x acc)))
                  (freq-rec (cdr remaining)
                    (if current
                        (alist-set x (+ 1 (cdr current)) acc)
                        (cons (cons x 1) acc))))))))
      lst '())))

   ; dedupe: Remove consecutive duplicates
   (dedupe (fn (lst)
     (if (null? lst)
         '()
         ((fix dedup-rec
            (fn (prev remaining acc)
              (if (null? remaining)
                  (reverse (cons prev acc))
                  (if (equal? prev (car remaining))
                      (dedup-rec prev (cdr remaining) acc)
                      (dedup-rec (car remaining) (cdr remaining) (cons prev acc))))))
          (car lst) (cdr lst) '()))))

   ; dedupe-by: Remove consecutive duplicates by key
   (dedupe-by (fn (key-fn lst)
     (if (null? lst)
         '()
         ((fix dedup-rec
            (fn (prev prev-key remaining acc)
              (if (null? remaining)
                  (reverse (cons prev acc))
                  (let ((cur-key (key-fn (car remaining))))
                    (if (equal? prev-key cur-key)
                        (dedup-rec prev prev-key (cdr remaining) acc)
                        (dedup-rec (car remaining) cur-key (cdr remaining) (cons prev acc)))))))
          (car lst) (key-fn (car lst)) (cdr lst) '()))))

   ; interleave: Interleave two lists
   (interleave (fn (l1 l2)
     ((fix inter-rec
        (fn (a b acc)
          (if (null? a)
              (append (reverse acc) b)
              (if (null? b)
                  (append (reverse acc) a)
                  (inter-rec (cdr a) (cdr b) (cons (car b) (cons (car a) acc)))))))
      l1 l2 '())))

   ; take-nth: Take every nth element
   (take-nth (fn (n lst)
     ((fix nth-rec
        (fn (i remaining acc)
          (if (null? remaining)
              (reverse acc)
              (if (= (mod i n) 0)
                  (nth-rec (+ i 1) (cdr remaining) (cons (car remaining) acc))
                  (nth-rec (+ i 1) (cdr remaining) acc)))))
      0 lst '())))

   ; drop-nth: Drop every nth element
   (drop-nth (fn (n lst)
     ((fix nth-rec
        (fn (i remaining acc)
          (if (null? remaining)
              (reverse acc)
              (if (= (mod i n) 0)
                  (nth-rec (+ i 1) (cdr remaining) acc)
                  (nth-rec (+ i 1) (cdr remaining) (cons (car remaining) acc))))))
      0 lst '())))

   ; rotate-left: Rotate list left by n positions
   (rotate-left (fn (n lst)
     (if (null? lst)
         '()
         (let ((len (length lst)))
           (let ((n-mod (mod n len)))
             (append (drop lst n-mod) (take lst n-mod)))))))

   ; rotate-right: Rotate list right by n positions
   (rotate-right (fn (n lst)
     (if (null? lst)
         '()
         (let ((len (length lst)))
           (rotate-left (- len (mod n len)) lst)))))

   ; split-when: Split list when predicate becomes true
   (split-when (fn (pred-fn lst)
     ((fix split-rec
        (fn (remaining current acc)
          (if (null? remaining)
              (reverse (if (null? current) acc (cons (reverse current) acc)))
              (if (pred-fn (car remaining))
                  (split-rec (cdr remaining) '()
                    (cons (reverse current) acc))
                  (split-rec (cdr remaining) (cons (car remaining) current) acc)))))
      lst '() '())))

   ; split-with: Split list at first element matching predicate
   (split-with (fn (pred-fn lst)
     ((fix split-rec
        (fn (remaining before)
          (if (null? remaining)
              (cons (reverse before) '())
              (if (pred-fn (car remaining))
                  (cons (reverse before) remaining)
                  (split-rec (cdr remaining) (cons (car remaining) before))))))
      lst '())))

   ; group-runs: Group consecutive runs of equal elements
   (group-runs (fn (lst)
     (if (null? lst)
         '()
         ((fix run-rec
            (fn (remaining current-run acc)
              (if (null? remaining)
                  (reverse (cons (reverse current-run) acc))
                  (if (equal? (car remaining) (car current-run))
                      (run-rec (cdr remaining) (cons (car remaining) current-run) acc)
                      (run-rec (cdr remaining) (list (car remaining))
                        (cons (reverse current-run) acc))))))
          (cdr lst) (list (car lst)) '()))))

   ; map-runs: Apply function to runs of consecutive equal elements
   (map-runs (fn (fn-1 lst)
     (map fn-1 (group-runs lst))))

   ; find-indices: Find all indices matching predicate
   (find-indices (fn (pred-fn lst)
     ((fix find-rec
        (fn (i remaining acc)
          (if (null? remaining)
              (reverse acc)
              (if (pred-fn (car remaining))
                  (find-rec (+ i 1) (cdr remaining) (cons i acc))
                  (find-rec (+ i 1) (cdr remaining) acc)))))
      0 lst '())))

   ; replace-at: Replace element at index
   (replace-at (fn (idx val lst)
     ((fix rep-rec
        (fn (i remaining acc)
          (if (null? remaining)
              (reverse acc)
              (if (= i idx)
                  (rep-rec (+ i 1) (cdr remaining) (cons val acc))
                  (rep-rec (+ i 1) (cdr remaining) (cons (car remaining) acc))))))
      0 lst '())))

   ; insert-at: Insert element at index
   (insert-at (fn (idx val lst)
     ((fix ins-rec
        (fn (i remaining acc)
          (if (= i idx)
              (append (reverse (cons val acc)) remaining)
              (if (null? remaining)
                  (reverse (cons val acc))
                  (ins-rec (+ i 1) (cdr remaining) (cons (car remaining) acc))))))
      0 lst '())))

   ; remove-at: Remove element at index
   (remove-at (fn (idx lst)
     ((fix rem-rec
        (fn (i remaining acc)
          (if (null? remaining)
              (reverse acc)
              (if (= i idx)
                  (rem-rec (+ i 1) (cdr remaining) acc)
                  (rem-rec (+ i 1) (cdr remaining) (cons (car remaining) acc))))))
      0 lst '())))

   ; swap-at: Swap elements at two indices
   (swap-at (fn (i j lst)
     (let ((vi (nth i lst))
           (vj (nth j lst)))
       (replace-at j vi (replace-at i vj lst)))))

   ; prefix?: Check if first list is prefix of second
   (prefix? (fn (pre lst)
     ((fix pre-rec
        (fn (p l)
          (if (null? p)
              #t
              (if (null? l)
                  #f
                  (if (equal? (car p) (car l))
                      (pre-rec (cdr p) (cdr l))
                      #f)))))
      pre lst)))

   ; suffix?: Check if first list is suffix of second
   (suffix? (fn (suf lst)
     (prefix? (reverse suf) (reverse lst))))

   ; sublist?: Check if first list appears in second
   (sublist? (fn (sub lst)
     ((fix sub-rec
        (fn (l)
          (if (null? l)
              #f
              (if (prefix? sub l)
                  #t
                  (sub-rec (cdr l))))))
      lst)))

   ; count-occurrences: Count how many times sublist appears
   (count-occurrences (fn (sub lst)
     ((fix count-rec
        (fn (l acc)
          (if (null? l)
              acc
              (if (prefix? sub l)
                  (count-rec (cdr l) (+ acc 1))
                  (count-rec (cdr l) acc)))))
      lst 0)))

   ; windowed: Generate overlapping windows
   (windowed (fn (n lst)
     (if (< (length lst) n)
         '()
         ((fix win-rec
            (fn (remaining acc)
              (if (< (length remaining) n)
                  (reverse acc)
                  (win-rec (cdr remaining) (cons (take remaining n) acc)))))
          lst '()))))

   ; ============================================================
   ; String Algorithms
   ; ============================================================

   ; edit-distance: Levenshtein distance between two strings
   (edit-distance (fn (s1 s2)
     (let ((l1 (string->list s1))
           (l2 (string->list s2)))
       (let ((len1 (length l1))
             (len2 (length l2)))
         (if (= len1 0) len2
           (if (= len2 0) len1
             ((fix ed-rec
                (fn (i j memo)
                  (if (> i len1)
                      (cdr (assoc (cons len1 len2) memo))
                      (if (> j len2)
                          (ed-rec (+ i 1) 1 memo)
                          (let ((c1 (nth (- i 1) l1))
                                (c2 (nth (- j 1) l2)))
                            (let ((cost (if (eq? c1 c2) 0 1)))
                              (let ((above (if (= i 1) j
                                             (+ 1 (cdr (assoc (cons (- i 1) j) memo)))))
                                    (left (if (= j 1) i
                                            (+ 1 (cdr (assoc (cons i (- j 1)) memo)))))
                                    (diag (if (or (= i 1) (= j 1))
                                              (+ (max (- i 1) (- j 1)) cost)
                                              (+ cost (cdr (assoc (cons (- i 1) (- j 1)) memo))))))
                                (ed-rec i (+ j 1)
                                  (cons (cons (cons i j) (min above (min left diag))) memo)))))))))
              1 1 '())))))))

   ; hamming-distance: Count differing positions (same-length strings)
   (hamming-distance (fn (s1 s2)
     (let ((l1 (string->list s1))
           (l2 (string->list s2)))
       (if (not (= (length l1) (length l2)))
           -1  ; undefined for different lengths
           ((fix ham-rec
              (fn (a b count)
                (if (null? a)
                    count
                    (ham-rec (cdr a) (cdr b)
                      (if (eq? (car a) (car b)) count (+ count 1))))))
            l1 l2 0)))))

   ; longest-common-prefix: Find longest common prefix of two strings
   (longest-common-prefix (fn (s1 s2)
     (let ((l1 (string->list s1))
           (l2 (string->list s2)))
       (list->string
         ((fix lcp-rec
            (fn (a b acc)
              (if (or (null? a) (null? b))
                  (reverse acc)
                  (if (eq? (car a) (car b))
                      (lcp-rec (cdr a) (cdr b) (cons (car a) acc))
                      (reverse acc)))))
          l1 l2 '())))))

   ; longest-common-suffix: Find longest common suffix
   (longest-common-suffix (fn (s1 s2)
     (string-reverse (longest-common-prefix (string-reverse s1) (string-reverse s2)))))

   ; string-similarity: Similarity ratio (0.0 to 1.0)
   (string-similarity (fn (s1 s2)
     (let ((max-len (max (string-length s1) (string-length s2))))
       (if (= max-len 0)
           1
           (/ (- max-len (edit-distance s1 s2)) max-len)))))

   ; fuzzy-match?: Check if strings are similar within threshold
   (fuzzy-match? (fn (s1 s2 threshold)
     (>= (string-similarity s1 s2) threshold)))

   ; string-normalize: Lowercase and trim whitespace
   (string-normalize (fn (s)
     (string-trim (string-map char-downcase s))))

   ; string-tokenize: Split on whitespace, remove empty
   (string-tokenize (fn (s)
     (filter (fn (tok) (not (string-empty? tok)))
       (string-split s " "))))

   ; ============================================================
   ; Testing and Assertion Utilities
   ; ============================================================

   ; assert-eq: Assert two values are equal
   (assert-eq (fn (expected actual msg)
     (if (equal? expected actual)
         (list 'pass msg)
         (list 'fail msg 'expected expected 'got actual))))

   ; assert-true: Assert value is truthy
   (assert-true (fn (val msg)
     (if val
         (list 'pass msg)
         (list 'fail msg 'expected 'truthy 'got val))))

   ; assert-false: Assert value is falsy
   (assert-false (fn (val msg)
     (if val
         (list 'fail msg 'expected 'falsy 'got val)
         (list 'pass msg))))

   ; assert-throws: Assert thunk throws (returns error symbol)
   (assert-throws (fn (thunk msg)
     (let ((result (thunk)))
       (if (and (symbol? result) (eq? result 'error))
           (list 'pass msg)
           (list 'fail msg 'expected 'error 'got result)))))

   ; run-tests: Run list of test thunks, return summary
   (run-tests (fn (tests)
     (let ((results (map (fn (t) (t)) tests)))
       (let ((passed (filter (fn (r) (eq? (car r) 'pass)) results))
             (failed (filter (fn (r) (eq? (car r) 'fail)) results)))
         (list
           (cons 'total (length results))
           (cons 'passed (length passed))
           (cons 'failed (length failed))
           (cons 'failures failed))))))

   ; test-case: Create a named test case
   (test-case (fn (name thunk)
     (fn () (cons name (thunk)))))

   ; ============================================================
   ; Sequence Generation Utilities
   ; ============================================================

   ; arithmetic-seq: Generate arithmetic sequence
   (arithmetic-seq (fn (start step count)
     ((fix arith-rec
        (fn (cur n acc)
          (if (<= n 0)
              (reverse acc)
              (arith-rec (+ cur step) (- n 1) (cons cur acc)))))
      start count '())))

   ; geometric-seq: Generate geometric sequence
   (geometric-seq (fn (start ratio count)
     ((fix geom-rec
        (fn (cur n acc)
          (if (<= n 0)
              (reverse acc)
              (geom-rec (* cur ratio) (- n 1) (cons cur acc)))))
      start count '())))

   ; fibonacci-seq: Generate first n Fibonacci numbers
   (fibonacci-seq (fn (n)
     (if (<= n 0) '()
       (if (= n 1) '(0)
         (if (= n 2) '(0 1)
           ((fix fib-rec
              (fn (a b count acc)
                (if (<= count 0)
                    (reverse acc)
                    (fib-rec b (+ a b) (- count 1) (cons a acc)))))
            0 1 n '()))))))

   ; primes-up-to: Generate primes up to n (simple sieve)
   (primes-up-to (fn (n)
     (if (< n 2) '()
       ((fix sieve
          (fn (candidates primes)
            (if (null? candidates)
                (reverse primes)
                (let ((p (car candidates)))
                  (sieve
                    (filter (fn (x) (not (= 0 (mod x p)))) (cdr candidates))
                    (cons p primes))))))
        (range-list 2 (+ n 1)) '()))))

   ; powers-of: Generate powers of base up to limit
   (powers-of (fn (base limit)
     ((fix pow-rec
        (fn (cur acc)
          (if (> cur limit)
              (reverse acc)
              (pow-rec (* cur base) (cons cur acc)))))
      1 '())))

   ; factorials-up-to: Generate factorials up to n
   (factorials-up-to (fn (n)
     ((fix fact-rec
        (fn (i cur acc)
          (if (> i n)
              (reverse acc)
              (fact-rec (+ i 1) (* cur i) (cons cur acc)))))
      1 1 '())))

   ; triangular-numbers: Generate first n triangular numbers
   (triangular-numbers (fn (n)
     ((fix tri-rec
        (fn (i sum acc)
          (if (> i n)
              (reverse acc)
              (tri-rec (+ i 1) (+ sum i) (cons sum acc)))))
      1 0 '())))

   ; convergent-seq: Generate sequence until convergence
   (convergent-seq (fn (start step-fn converged-fn max-iter)
     ((fix conv-rec
        (fn (cur iter acc)
          (if (or (converged-fn cur) (>= iter max-iter))
              (reverse (cons cur acc))
              (conv-rec (step-fn cur) (+ iter 1) (cons cur acc)))))
      start 0 '())))

   ; cycle-detect: Detect cycle in sequence, return (mu . lambda)
   ; mu = start of cycle, lambda = length of cycle
   (cycle-detect (fn (start step-fn)
     ((fix floyd
        (fn (tortoise hare power lambda mu)
          (if (equal? tortoise hare)
              (if (= lambda 0)
                  ; Find actual start
                  ((fix find-mu
                     (fn (t h m)
                       (if (equal? t h)
                           (cons m lambda)
                           (find-mu (step-fn t) (step-fn h) (+ m 1)))))
                   start
                   ((fix advance (fn (x n) (if (= n 0) x (advance (step-fn x) (- n 1)))))
                    start lambda)
                   0)
                  (cons mu lambda))
              (if (= power lambda)
                  (floyd hare (step-fn hare) (* power 2) 1 mu)
                  (floyd tortoise (step-fn hare) power (+ lambda 1) mu)))))
      start (step-fn start) 1 1 0)))

   ; iterate-n: Apply function n times, collect results
   (iterate-n (fn (fn-1 start n)
     ((fix iter-rec
        (fn (cur count acc)
          (if (<= count 0)
              (reverse acc)
              (iter-rec (fn-1 cur) (- count 1) (cons cur acc)))))
      start n '())))
  )

  ; Body returns a list of all defined functions as an alist
  ; The caller can extract what they need
  (list
    (cons 'map map)
    (cons 'filter filter)
    (cons 'foldl foldl)
    (cons 'foldr foldr)
    (cons 'any any)
    (cons 'all all)
    (cons 'take-while take-while)
    (cons 'drop-while drop-while)
    (cons 'zip-with zip-with)
    (cons 'sum-list sum-list)
    (cons 'product-list product-list)
    (cons 'id id)
    (cons 'const const)
    (cons 'compose compose)
    (cons 'flip flip)
    (cons 'complement complement)
    (cons 'eqv? eqv?)
    (cons 'equal? equal?)
    (cons 'partition partition)
    (cons 'find-if find-if)
    (cons 'remove-if remove-if)
    (cons 'count-if count-if)
    (cons 'concat concat)
    (cons 'replicate replicate)
    (cons 'iterate iterate)
    (cons 'scanl scanl)
    (cons 'curry2 curry2)
    (cons 'uncurry2 uncurry2)
    (cons 'zip zip)
    (cons 'unzip unzip)
    (cons 'intersperse intersperse)
    (cons 'span span)
    (cons 'flat-map flat-map)
    (cons 'map-indexed map-indexed)
    (cons 'filter-indexed filter-indexed)
    (cons 'nth-safe nth-safe)
    (cons 'max-by max-by)
    (cons 'min-by min-by)
    (cons 'on on)
    (cons 'juxt juxt)
    (cons 'curry2 curry2)
    (cons 'curry3 curry3)
    (cons 'uncurry2 uncurry2)
    (cons 'uncurry3 uncurry3)
    (cons 'partial partial)
    (cons 'partial2 partial2)
    (cons 'partial-right partial-right)
    (cons 'flip flip)
    (cons 'flip3 flip3)
    (cons 'pipe pipe)
    (cons 'unfold unfold)
    (cons 'tails tails)
    (cons 'inits inits)
    (cons 'group-consecutive group-consecutive)
    (cons 'range-list range-list)
    (cons 'repeat-fn repeat-fn)
    (cons 'maybe maybe)
    (cons 'from-maybe from-maybe)
    (cons 'map-maybe map-maybe)
    (cons 'cat-maybes cat-maybes)
    ; Tagged Maybe type
    (cons 'just just)
    (cons 'nothing nothing)
    (cons 'nothing? nothing?)
    (cons 'from-just from-just)
    (cons 'chunks chunks)
    (cons 'sliding sliding)
    (cons 'pairs pairs)
    (cons 'split-at split-at)
    (cons 'elem? elem?)
    (cons 'nub nub)
    (cons 'intercalate intercalate)
    (cons 'transpose transpose)
    (cons 'sum-by sum-by)
    (cons 'product-by product-by)
    (cons 'average average)
    (cons 'even-indices even-indices)
    (cons 'odd-indices odd-indices)
    (cons 'sort-by sort-by)
    (cons 'compare-by compare-by)
    (cons 'equal-by equal-by)
    (cons 'group-by group-by)
    (cons 'apply-n apply-n)
    (cons 'until until)
    (cons 'converge converge)
    (cons 'fixed-point fixed-point)
    (cons 'both both)
    (cons 'either-pred either-pred)
    (cons 'neither neither)
    (cons 'frequencies frequencies)
    (cons 'index-where index-where)
    (cons 'indices-where indices-where)
    (cons 'last-where last-where)
    (cons 'update-at update-at)
    (cons 'insert-at insert-at)
    (cons 'remove-at remove-at)
    (cons 'string-map string-map)
    (cons 'string-filter string-filter)
    (cons 'string-any string-any)
    (cons 'string-all string-all)
    (cons 'string-foldl string-foldl)
    (cons 'string-foldr string-foldr)
    (cons 'words words)
    (cons 'unwords unwords)
    (cons 'lines lines)
    (cons 'unlines unlines)
    (cons 'join join)
    (cons 'string-take-while string-take-while)
    (cons 'string-drop-while string-drop-while)
    (cons 'string-find string-find)
    (cons 'string-count string-count)
    (cons 'string-partition string-partition)
    (cons 'char-between? char-between?)
    (cons 'char-alphabetic? char-alphabetic?)
    (cons 'char-numeric? char-numeric?)
    (cons 'char-whitespace? char-whitespace?)
    (cons 'char-upcase char-upcase)
    (cons 'char-downcase char-downcase)
    (cons 'union union)
    (cons 'intersection intersection)
    (cons 'difference difference)
    (cons 'symmetric-difference symmetric-difference)
    (cons 'subset? subset?)
    (cons 'disjoint? disjoint?)
    (cons 'alist-map alist-map)
    (cons 'alist-filter alist-filter)
    (cons 'alist-find alist-find)
    (cons 'alist-update alist-update)
    (cons 'alist-merge alist-merge)
    (cons 'alist-invert alist-invert)
    (cons 'alist-group alist-group)
    (cons 'when-let when-let)
    (cons 'if-let if-let)
    (cons 'cond-fn cond-fn)
    (cons 'thread-first thread-first)
    (cons 'thread-last thread-last)
    (cons 'tap tap)
    (cons 'clamp-list clamp-list)
    (cons 'normalize normalize)
    (cons 'running-sum running-sum)
    (cons 'running-product running-product)
    (cons 'differences differences)
    (cons 'rotate-left rotate-left)
    (cons 'rotate-right rotate-right)
    (cons 'all-equal? all-equal?)
    (cons 'sorted? sorted?)
    (cons 'sorted-by? sorted-by?)
    (cons 'palindrome? palindrome?)
    (cons 'deep-map deep-map)
    (cons 'deep-filter deep-filter)
    (cons 'flatten-deep flatten-deep)
    (cons 'tree-depth tree-depth)
    (cons 'tree-size tree-size)
    (cons 'tree-find tree-find)
    (cons 'curry3 curry3)
    (cons 'uncurry3 uncurry3)
    (cons 'partial1 partial1)
    (cons 'partial2 partial2)
    (cons 'compose-n compose-n)
    (cons 'pipe-n pipe-n)
    (cons 'memoize-1 memoize-1)
    (cons 'factorial factorial)
    (cons 'fibonacci fibonacci)
    (cons 'gcd-list gcd-list)
    (cons 'lcm-list lcm-list)
    (cons 'prime? prime?)
    (cons 'divisors divisors)
    (cons 'perfect? perfect?)
    (cons 'pow-int pow-int)
    (cons 'validate validate)
    (cons 'validate-all validate-all)
    (cons 'ensure ensure)
    (cons 'trace trace)
    (cons 'spy spy)
    (cons 'assert-eq assert-eq)
    (cons 'assert-pred assert-pred)
    (cons 'xor xor)
    (cons 'implies implies)
    (cons 'iff iff)
    (cons 'take-n take-n)
    (cons 'drop-n drop-n)
    (cons 'nth nth)
    (cons 'init init)
    (cons 'last last)
    (cons 'butlast butlast)
    (cons 'count-eq count-eq)
    (cons 'assoc assoc)
    (cons 'assoc-ref assoc-ref)
    (cons 'assoc-set assoc-set)
    (cons 'assoc-remove assoc-remove)
    (cons 'assoc-keys assoc-keys)
    (cons 'assoc-values assoc-values)
    (cons 'string-reverse string-reverse)
    (cons 'string-empty? string-empty?)
    (cons 'string-prefix? string-prefix?)
    (cons 'string-suffix? string-suffix?)
    (cons 'iota iota)
    (cons 'range range)
    (cons 'range-step range-step)
    (cons 'zip3 zip3)
    (cons 'zip-with-index zip-with-index)
    (cons 'constantly constantly)
    (cons 'identity identity)
    (cons 'first first)
    (cons 'second second)
    (cons 'third third)
    (cons 'rest rest)
    (cons 'empty? empty?)
    (cons 'singleton? singleton?)
    (cons 'has-pair? has-pair?)
    (cons 'cadr cadr)
    (cons 'caar caar)
    (cons 'cddr cddr)
    (cons 'cdar cdar)
    (cons 'caddr caddr)
    (cons 'caaar caaar)
    (cons 'caadr caadr)
    (cons 'cadar cadar)
    (cons 'cdaar cdaar)
    (cons 'cdadr cdadr)
    (cons 'cddar cddar)
    (cons 'cdddr cdddr)
    (cons 'cadddr cadddr)
    (cons 'fourth fourth)
    (cons 'fifth fifth)
    (cons 'list-tail list-tail)
    (cons 'list-head list-head)
    (cons 'memq memq)
    (cons 'assq assq)
    (cons 'append-map append-map)
    (cons 'nand nand)
    (cons 'nor nor)
    (cons 'square square)
    (cons 'cube cube)
    (cons 'abs-diff abs-diff)
    (cons 'in-range? in-range?)
    (cons 'signum signum)
    (cons 'unless unless)
    (cons 'when when)
    (cons 'for-each for-each)
    (cons 'reduce reduce)
    (cons 'reduce-right reduce-right)
    (cons 'every? every?)
    (cons 'some? some?)
    (cons 'filter-not filter-not)
    (cons 'remove remove)
    (cons 'remove-all remove-all)
    (cons 'cons* cons*)
    (cons 'list* list*)
    (cons 'build-list build-list)
    (cons 'make-list-fn make-list-fn)
    (cons 'tabulate tabulate)
    (cons 'max-of max-of)
    (cons 'min-of min-of)
    (cons 'argmax argmax)
    (cons 'succ succ)
    (cons 'pred-fn pred-fn)
    (cons 'double double)
    (cons 'halve halve)
    ; Scheme compatibility
    (cons 'fold-left fold-left)
    (cons 'fold-right fold-right)
    (cons 'member member)
    (cons 'member? member?)
    (cons 'position position)
    ; Either monad
    (cons 'left left)
    (cons 'right right)
    (cons 'left? left?)
    (cons 'right? right?)
    (cons 'from-left from-left)
    (cons 'from-right from-right)
    (cons 'either either)
    ; SRFI list utilities
    (cons 'list-index list-index)
    (cons 'partition-all partition-all)
    (cons 'take-right take-right)
    (cons 'drop-right drop-right)
    (cons 'split-at-pred split-at-pred)
    ; Numeric utilities
    (cons 'sum sum)
    (cons 'product product)
    (cons 'mean mean)
    (cons 'variance variance)
    (cons 'median median)
    (cons 'clamp-val clamp-val)
    (cons 'lerp-fn lerp)
    ; Predicate utilities
    (cons 'and-fn and-fn)
    (cons 'or-fn or-fn)
    (cons 'not-fn not-fn)
    ; List aliases
    (cons 'interpose interpose)
    (cons 'separate separate)
    (cons 'keep keep)
    (cons 'reject reject)
    (cons 'distinct distinct)
    (cons 'group-runs group-runs)
    ; Applicative utilities
    (cons 'lift2 lift2)
    (cons 'sequence-list sequence-list)
    ; More list operations
    (cons 'zip-with-3 zip-with-3)
    (cons 'unzip3 unzip3)
    (cons 'snoc snoc)
    (cons 'single? single?)
    (cons 'drop-last drop-last)
    (cons 'take-last take-last)
    (cons 'find-index-of find-index-of)
    (cons 'last-index-of last-index-of)
    (cons 'count-occurrences count-occurrences)
    (cons 'replace-first replace-first)
    (cons 'replace-all replace-all)
    (cons 'insert-sorted insert-sorted)
    (cons 'insert-sorted-by insert-sorted-by)
    (cons 'merge-sorted merge-sorted)
    ; Function combinators
    (cons 'apply-to apply-to)
    (cons 'thrush thrush)
    (cons 'fanout fanout)
    (cons 'converge-with converge-with)
    (cons 'dup dup)
    (cons 'swap swap)
    ; Numeric utilities
    (cons 'quotient-remainder quotient-remainder)
    (cons 'wrap wrap)
    (cons 'distance distance)
    (cons 'average-of average-of)
    (cons 'geometric-mean geometric-mean)
    (cons 'harmonic-mean harmonic-mean)
    ; Predicate utilities
    (cons 'all-of all-of)
    (cons 'any-of any-of)
    (cons 'none-of none-of)
    (cons 'is is)
    (cons 'is-not is-not)
    ; Either monad extensions
    (cons 'map-left map-left)
    (cons 'map-right map-right)
    (cons 'bimap-either bimap-either)
    (cons 'from-either from-either)
    (cons 'partition-eithers partition-eithers)
    (cons 'try-fn try-fn)
    ; More Maybe utilities
    (cons 'map-maybe-fn map-maybe-fn)
    (cons 'bind-maybe bind-maybe)
    (cons 'maybe-to-list maybe-to-list)
    (cons 'list-to-maybe list-to-maybe)
    (cons 'cat-maybes-fn cat-maybes-fn)
    ; String HOFs
    (cons 'string-words-fn string-words-fn)
    (cons 'string-lines-fn string-lines-fn)
    (cons 'string-join-fn string-join-fn)
    (cons 'string-trim-fn string-trim-fn)
    ; Miscellaneous
    (cons 'repeat-value repeat-value)
    (cons 'cycle-n cycle-n)
    (cons 'chunk-by chunk-by)
    (cons 'split-when split-when)
    (cons 'before before)
    (cons 'after after)
    (cons 'window window)
    (cons 'adjacent-pairs adjacent-pairs)
    (cons 'with-index with-index)
    (cons 'enumerate enumerate)
    ; Control flow utilities
    (cons 'when-not when-not)
    (cons 'if-not if-not)
    (cons 'cond-list cond-list)
    (cons 'select select)
    (cons 'case-pred case-pred)
    ; Property list utilities
    (cons 'plist-get plist-get)
    (cons 'plist-set plist-set)
    (cons 'plist-remove plist-remove)
    (cons 'plist-keys plist-keys)
    (cons 'plist-values plist-values)
    (cons 'plist->alist plist->alist)
    (cons 'alist->plist alist->plist)
    ; Comparison utilities
    (cons 'min-max min-max)
    (cons 'between-inclusive? between-inclusive?)
    (cons 'compare compare)
    (cons 'compare-by-key compare-by-key)
    (cons 'lexicographic-compare lexicographic-compare)
    ; Vector HOFs
    (cons 'vec-map vec-map)
    (cons 'vec-filter vec-filter)
    (cons 'vec-foldl vec-foldl)
    (cons 'vec-foldr vec-foldr)
    (cons 'vec-any vec-any)
    (cons 'vec-all vec-all)
    (cons 'vec-find vec-find)
    (cons 'vec-sum vec-sum)
    (cons 'vec-product vec-product)
    ; Iteration utilities
    (cons 'iterate-until iterate-until)
    (cons 'iterate-while iterate-while)
    (cons 'iterate-n iterate-n)
    (cons 'generate generate)
    (cons 'unfold-right unfold-right)
    ; Accumulator patterns
    (cons 'scan-right scan-right)
    (cons 'running-max running-max)
    (cons 'running-min running-min)
    ; Conditional list builders
    (cons 'append-if append-if)
    (cons 'prepend-if prepend-if)
    (cons 'maybe-cons maybe-cons)
    ; Numeric sequences
    (cons 'arithmetic-sequence arithmetic-sequence)
    (cons 'geometric-sequence geometric-sequence)
    (cons 'fibonacci-sequence fibonacci-sequence)
    (cons 'primes-up-to primes-up-to)
    ; List searching
    (cons 'find-all find-all)
    (cons 'find-first-n find-first-n)
    (cons 'find-last find-last)
    (cons 'index-of-all index-of-all)
    ; String builders
    (cons 'string-repeat-fn string-repeat-fn)
    (cons 'string-pad-left string-pad-left)
    (cons 'string-pad-right string-pad-right)
    (cons 'string-center-fn string-center-fn)
    ; Safe operations
    (cons 'safe-car safe-car)
    (cons 'safe-cdr safe-cdr)
    (cons 'safe-head safe-head)
    (cons 'safe-tail safe-tail)
    (cons 'safe-nth safe-nth)
    (cons 'safe-div safe-div)
    (cons 'safe-mod safe-mod)
    ; Composition utilities
    (cons 'compose3 compose3)
    (cons 'compose4 compose4)
    (cons 'pipe3 pipe3)
    (cons 'pipe4 pipe4)
    ; Predicate combinators
    (cons 'conjoin conjoin)
    (cons 'disjoin disjoin)
    ; Pair utilities
    (cons 'pair-map pair-map)
    (cons 'pair-map-car pair-map-car)
    (cons 'pair-map-cdr pair-map-cdr)
    (cons 'pair-swap pair-swap)
    ; Result type utilities
    (cons 'ok ok)
    (cons 'err err)
    (cons 'ok? ok?)
    (cons 'err? err?)
    (cons 'unwrap unwrap)
    (cons 'unwrap-or unwrap-or)
    (cons 'map-ok map-ok)
    (cons 'map-err map-err)
    ; Sorting utilities
    (cons 'insertion-sort insertion-sort)
    (cons 'merge-sort merge-sort)
    (cons 'sort-descending sort-descending)
    (cons 'sort-by-descending sort-by-descending)
    (cons 'top-k top-k)
    (cons 'bottom-k bottom-k)
    (cons 'rank rank)
    ; Binary search utilities
    (cons 'binary-search binary-search)
    (cons 'lower-bound lower-bound)
    (cons 'upper-bound upper-bound)
    ; More list utilities
    (cons 'rotate-list rotate-list)
    (cons 'shuffle shuffle)
    (cons 'dedup-consecutive dedup-consecutive)
    (cons 'run-length-encode run-length-encode)
    (cons 'run-length-decode run-length-decode)
    ; Statistics utilities
    (cons 'percentile percentile)
    (cons 'quartiles quartiles)
    (cons 'interquartile-range interquartile-range)
    (cons 'z-score z-score)
    (cons 'normalize-list normalize-list)
    (cons 'standardize standardize)
    (cons 'covariance covariance)
    (cons 'correlation correlation)
    ; Matrix operations
    (cons 'matrix-ref matrix-ref)
    (cons 'matrix-rows matrix-rows)
    (cons 'matrix-cols matrix-cols)
    (cons 'matrix-row matrix-row)
    (cons 'matrix-col matrix-col)
    (cons 'matrix-map matrix-map)
    (cons 'matrix-add matrix-add)
    (cons 'matrix-scale matrix-scale)
    (cons 'matrix-transpose matrix-transpose)
    (cons 'dot-product dot-product)
    (cons 'matrix-multiply matrix-multiply)
    (cons 'identity-matrix identity-matrix)
    (cons 'zero-matrix zero-matrix)
    ; Functional patterns
    (cons 'Y Y)
    (cons 'memoize memoize)
    (cons 'trampoline trampoline)
    ; Tree utilities
    (cons 'tree-leaves tree-leaves)
    (cons 'tree-count tree-count)
    (cons 'tree-height tree-height)
    (cons 'tree-paths tree-paths)
    ; Format utilities
    (cons 'number->string-padded number->string-padded)
    (cons 'format-list format-list)
    (cons 'format-table format-table)
    (cons 'pluralize pluralize)
    ; Boolean utilities
    (cons 'bool->int bool->int)
    (cons 'int->bool int->bool)
    ; List predicates
    (cons 'sublist? sublist?)
    (cons 'prefix? prefix?)
    (cons 'suffix? suffix?)
    ; Misc utilities
    (cons 'clamp-index clamp-index)
    (cons 'circular-ref circular-ref)
    (cons 'repeat-fn-n repeat-fn-n)
    (cons 'fixed-point-iterate fixed-point-iterate)
    ; Dictionary utilities
    (cons 'dict-new dict-new)
    (cons 'dict-set dict-set)
    (cons 'dict-get dict-get)
    (cons 'dict-get-in dict-get-in)
    (cons 'dict-remove dict-remove)
    (cons 'dict-has? dict-has?)
    (cons 'dict-keys dict-keys)
    (cons 'dict-values dict-values)
    (cons 'dict-size dict-size)
    (cons 'dict-empty? dict-empty?)
    (cons 'dict-update dict-update)
    (cons 'dict-merge dict-merge)
    (cons 'dict-filter dict-filter)
    (cons 'dict-map-values dict-map-values)
    (cons 'dict-from-lists dict-from-lists)
    (cons 'dict-to-list dict-to-list)
    (cons 'dict-invert dict-invert)
    ; Set utilities
    (cons 'set-new set-new)
    (cons 'set-add set-add)
    (cons 'set-remove set-remove)
    (cons 'set-member? set-member?)
    (cons 'set-size set-size)
    (cons 'set-empty? set-empty?)
    (cons 'set-from-list set-from-list)
    (cons 'set-to-list set-to-list)
    (cons 'set-union set-union)
    (cons 'set-intersection set-intersection)
    (cons 'set-difference set-difference)
    (cons 'set-symmetric-difference set-symmetric-difference)
    (cons 'set-subset? set-subset?)
    (cons 'set-equal? set-equal?)
    ; Numeric utilities
    (cons 'mod-exp mod-exp)
    (cons 'mod-inverse mod-inverse)
    (cons 'coprime? coprime?)
    (cons 'totient totient)
    (cons 'digits digits)
    (cons 'from-digits from-digits)
    (cons 'digit-sum digit-sum)
    (cons 'digit-count digit-count)
    (cons 'reverse-number reverse-number)
    (cons 'palindrome-number? palindrome-number?)
    (cons 'is-power-of-2? is-power-of-2?)
    (cons 'next-power-of-2 next-power-of-2)
    (cons 'log2-int log2-int)
    ; Combinatorics
    (cons 'binomial binomial)
    (cons 'permutations-count permutations-count)
    (cons 'catalan catalan)
    (cons 'triangular triangular)
    (cons 'is-triangular? is-triangular?)
    (cons 'square-number square-number)
    (cons 'is-square? is-square?)
    (cons 'pentagonal pentagonal)
    (cons 'hexagonal hexagonal)
    ; More list utilities
    (cons 'cartesian-product cartesian-product)
    (cons 'power-set power-set)
    (cons 'permutations permutations)
    (cons 'combinations combinations)
    (cons 'list-product list-product)
    (cons 'interleave-all interleave-all)
    (cons 'partition-n partition-n)
    (cons 'group-into group-into)
    (cons 'frequencies-by frequencies-by)
    (cons 'mode mode)
    (cons 'majority majority)
    ; String utilities
    (cons 'string-capitalize string-capitalize)
    (cons 'string-title-case string-title-case)
    (cons 'string-count-char string-count-char)
    (cons 'string-replace-char string-replace-char)
    (cons 'string-squeeze string-squeeze)
    (cons 'string-rotate string-rotate)
    (cons 'string-interleave string-interleave)
    ; Predicate utilities
    (cons 'all-same? all-same?)
    (cons 'all-different? all-different?)
    (cons 'monotonic-increasing? monotonic-increasing?)
    (cons 'monotonic-decreasing? monotonic-decreasing?)
    (cons 'strictly-increasing? strictly-increasing?)
    (cons 'strictly-decreasing? strictly-decreasing?)
    ; Sequence generators
    (cons 'naturals naturals)
    (cons 'evens evens)
    (cons 'odds odds)
    (cons 'squares squares)
    (cons 'cubes cubes)
    (cons 'powers-of powers-of)
    (cons 'factorials-up-to factorials-up-to)
    (cons 'triangular-numbers triangular-numbers)
    ; Reduction utilities
    (cons 'reduce-pairs reduce-pairs)
    (cons 'pairwise-apply pairwise-apply)
    (cons 'fold-tree fold-tree)
    ; Debug utilities
    (cons 'debug-print debug-print)
    (cons 'time-thunk time-thunk)
    (cons 'count-calls count-calls)
    (cons 'type-of-value type-of-value)
    ; Queue operations
    (cons 'queue-new queue-new)
    (cons 'queue-empty? queue-empty?)
    (cons 'queue-enqueue queue-enqueue)
    (cons 'queue-dequeue queue-dequeue)
    (cons 'queue-front queue-front)
    (cons 'queue-size queue-size)
    (cons 'queue-to-list queue-to-list)
    ; Stack operations
    (cons 'stack-new stack-new)
    (cons 'stack-empty? stack-empty?)
    (cons 'stack-push stack-push)
    (cons 'stack-pop stack-pop)
    (cons 'stack-top stack-top)
    (cons 'stack-size stack-size)
    ; Deque operations
    (cons 'deque-new deque-new)
    (cons 'deque-empty? deque-empty?)
    (cons 'deque-push-front deque-push-front)
    (cons 'deque-push-back deque-push-back)
    (cons 'deque-pop-front deque-pop-front)
    (cons 'deque-pop-back deque-pop-back)
    (cons 'deque-front deque-front)
    (cons 'deque-back deque-back)
    ; Priority queue operations
    (cons 'pq-new pq-new)
    (cons 'pq-empty? pq-empty?)
    (cons 'pq-insert pq-insert)
    (cons 'pq-peek pq-peek)
    (cons 'pq-pop pq-pop)
    ; Graph algorithms
    (cons 'graph-new graph-new)
    (cons 'graph-add-vertex graph-add-vertex)
    (cons 'graph-add-edge graph-add-edge)
    (cons 'graph-add-undirected-edge graph-add-undirected-edge)
    (cons 'graph-neighbors graph-neighbors)
    (cons 'graph-vertices graph-vertices)
    (cons 'graph-has-edge? graph-has-edge?)
    (cons 'graph-degree graph-degree)
    (cons 'graph-bfs graph-bfs)
    (cons 'graph-dfs graph-dfs)
    (cons 'graph-path-exists? graph-path-exists?)
    (cons 'graph-connected? graph-connected?)
    ; Validation utilities
    (cons 'validate-type validate-type)
    (cons 'validate-range validate-range)
    (cons 'validate-not-empty validate-not-empty)
    (cons 'validate-all-pred validate-all-pred)
    (cons 'validate-length validate-length)
    (cons 'validate-min-length validate-min-length)
    (cons 'validate-max-length validate-max-length)
    (cons 'chain-validations chain-validations)
    ; Parsing utilities
    (cons 'parse-int parse-int)
    (cons 'parse-bool parse-bool)
    (cons 'split-parse split-parse)
    ; Lens utilities
    (cons 'lens-get lens-get)
    (cons 'lens-set lens-set)
    (cons 'lens-update lens-update)
    ; Arrow utilities
    (cons 'arrow-first arrow-first)
    (cons 'arrow-second arrow-second)
    (cons 'arrow-both arrow-both)
    (cons 'arrow-split arrow-split)
    (cons 'arrow-fanin arrow-fanin)
    (cons 'kleisli-compose kleisli-compose)
    ; List algorithms
    (cons 'list-min list-min)
    (cons 'list-max list-max)
    (cons 'list-argmin list-argmin)
    (cons 'list-argmax list-argmax)
    (cons 'list-span-count list-span-count)
    (cons 'list-break-count list-break-count)
    (cons 'list-split-at-first list-split-at-first)
    (cons 'list-unique-by list-unique-by)
    (cons 'list-group-runs-by list-group-runs)
    ; Interval utilities
    (cons 'interval-new interval-new)
    (cons 'interval-lo interval-lo)
    (cons 'interval-hi interval-hi)
    (cons 'interval-contains? interval-contains?)
    (cons 'interval-overlaps? interval-overlaps?)
    (cons 'interval-union interval-union)
    (cons 'interval-intersection interval-intersection)
    (cons 'interval-width interval-width)
    (cons 'interval-midpoint interval-midpoint)
    ; Bisection utilities
    (cons 'bisect-find bisect-find)
    (cons 'bisect-root bisect-root)
    ; Accumulator patterns
    (cons 'scan-while scan-while)
    (cons 'fold-while fold-while)
    (cons 'fold-until fold-until)
    ; Utility combinators
    (cons 'when-pred when-pred)
    (cons 'unless-pred unless-pred)
    (cons 'with-default with-default)
    (cons 'null-coalesce null-coalesce)
    (cons 'safe-apply safe-apply)
    (cons 'retry-n retry-n)
    ; Pattern matching utilities
    (cons 'match-pred match-pred)
    (cons 'match-equal match-equal)
    (cons 'case-of case-of)
    (cons 'guard guard)
    (cons 'destructure-list destructure-list)
    (cons 'destructure-pair destructure-pair)
    (cons 'let-pair let-pair)
    ; Lazy evaluation / streams
    (cons 'thunk thunk)
    (cons 'force force)
    (cons 'stream-cons stream-cons)
    (cons 'stream-head stream-head)
    (cons 'stream-tail stream-tail)
    (cons 'stream-null stream-null)
    (cons 'stream-null? stream-null?)
    (cons 'stream-take stream-take)
    (cons 'stream-map stream-map)
    (cons 'stream-filter stream-filter)
    (cons 'stream-from stream-from)
    (cons 'stream-iterate stream-iterate)
    (cons 'stream-repeat stream-repeat)
    (cons 'stream-zip-with stream-zip-with)
    ; Monad utilities
    (cons 'bind-either bind-either)
    (cons 'sequence-maybes sequence-maybes)
    (cons 'sequence-eithers sequence-eithers)
    (cons 'traverse-maybe traverse-maybe)
    (cons 'traverse-either traverse-either)
    (cons 'ap-maybe ap-maybe)
    (cons 'ap-either ap-either)
    ; State monad
    (cons 'state-return state-return)
    (cons 'state-bind state-bind)
    (cons 'state-get state-get)
    (cons 'state-put state-put)
    (cons 'state-modify state-modify)
    (cons 'run-state run-state)
    (cons 'eval-state eval-state)
    (cons 'exec-state exec-state)
    ; Reader monad
    (cons 'reader-return reader-return)
    (cons 'reader-bind reader-bind)
    (cons 'reader-ask reader-ask)
    (cons 'reader-local reader-local)
    (cons 'run-reader run-reader)
    ; Writer monad
    (cons 'writer-return writer-return)
    (cons 'writer-bind writer-bind)
    (cons 'writer-tell writer-tell)
    (cons 'run-writer run-writer)
    ; Multiset / bag utilities
    (cons 'multiset-add multiset-add)
    (cons 'multiset-remove multiset-remove)
    (cons 'multiset-count multiset-count)
    (cons 'multiset-from-list multiset-from-list)
    (cons 'multiset-to-list multiset-to-list)
    (cons 'bag-union bag-union)
    (cons 'bag-intersection bag-intersection)
    ; FP patterns
    (cons 'filter-map filter-map)
    (cons 'fix-with-memo fix-with-memo)
    (cons 'memo-rec memo-rec)
    (cons 'trampoline-call trampoline-call)
    (cons 'bounce bounce)
    (cons 'done done)
    ; Complex numbers (simple representation)
    (cons 'complex-new complex-new)
    (cons 'complex-real complex-real)
    (cons 'complex-imag complex-imag)
    (cons 'complex-add complex-add)
    (cons 'complex-sub complex-sub)
    (cons 'complex-mul complex-mul)
    (cons 'complex-magnitude complex-magnitude)
    (cons 'complex-conjugate complex-conjugate)
    ; Format utilities
    (cons 'format-number format-number)
    (cons 'pad-number pad-number)
    (cons 'format-hex format-hex)
    (cons 'format-binary format-binary)
    (cons 'format-with-commas format-with-commas)
    (cons 'format-ordinal format-ordinal)
    ; More string utilities
    (cons 'string-chars string-chars)
    (cons 'chars-string chars-string)
    (cons 'string-empty? string-empty?)
    (cons 'string-first string-first)
    (cons 'string-last string-last)
    (cons 'string-take string-take)
    (cons 'string-drop string-drop)
    (cons 'string-take-right string-take-right)
    (cons 'string-drop-right string-drop-right)
    (cons 'string-contains? string-contains?)
    (cons 'string-index-of string-index-of)
    (cons 'string-starts-with? string-starts-with?)
    (cons 'string-ends-with? string-ends-with?)
    (cons 'string-replace string-replace)
    (cons 'string-split-at string-split-at)
    ; Error handling utilities
    (cons 'try-catch try-catch)
    (cons 'assert assert)
    (cons 'assert-eq assert-eq)
    (cons 'ensure ensure)
    (cons 'coerce coerce)
    ; More numeric utilities
    (cons 'factorial factorial)
    (cons 'fibonacci fibonacci)
    (cons 'is-prime? is-prime?)
    (cons 'divisors divisors)
    (cons 'proper-divisors proper-divisors)
    (cons 'is-perfect? is-perfect?)
    (cons 'next-prime next-prime)
    (cons 'prime-factors prime-factors)
    (cons 'num-digits num-digits)
    (cons 'sum-digits sum-digits)
    ; Control flow utilities
    (cons 'while-loop while-loop)
    (cons 'for-loop for-loop)
    (cons 'times times)
    (cons 'repeat-until repeat-until)
    (cons 'do-times do-times)
    (cons 'thread-first thread-first)
    (cons 'thread-last thread-last)
    (cons 'pipeline pipeline)
    ; Type introspection utilities
    (cons 'type-name type-name)
    (cons 'is-type? is-type?)
    (cons 'type-check type-check)
    ; Extended alist utilities
    (cons 'alist-map alist-map)
    (cons 'alist-filter alist-filter)
    (cons 'alist-filter-keys alist-filter-keys)
    (cons 'alist-filter-values alist-filter-values)
    (cons 'alist-keys alist-keys)
    (cons 'alist-values alist-values)
    (cons 'alist-has-key? alist-has-key?)
    (cons 'alist-update alist-update)
    (cons 'alist-merge alist-merge)
    ; Validation utilities
    (cons 'valid? valid?)
    (cons 'invalid? invalid?)
    (cons 'first-failing first-failing)
    (cons 'validate-with validate-with)
    ; More list utilities
    (cons 'list-ref-safe list-ref-safe)
    (cons 'list-set list-set)
    (cons 'list-update list-update)
    (cons 'list-insert list-insert)
    (cons 'list-delete list-delete)
    (cons 'list-swap list-swap)
    (cons 'group-by-key group-by-key)
    (cons 'partition-by partition-by)
    (cons 'intersperse-with intersperse-with)
    ; Advanced sorting
    (cons 'quicksort quicksort)
    (cons 'quicksort-by quicksort-by)
    (cons 'mergesort mergesort)
    (cons 'mergesort-by mergesort-by)
    ; Extended graph algorithms
    (cons 'graph-topological-sort graph-topological-sort)
    (cons 'graph-has-cycle? graph-has-cycle?)
    (cons 'graph-reverse graph-reverse)
    (cons 'graph-transpose graph-transpose)
    ; Lens-like utilities
    (cons 'get-in get-in)
    (cons 'update-in update-in)
    (cons 'set-in set-in)
    ; Additional string utilities
    (cons 'string-pad-left string-pad-left)
    (cons 'string-pad-right string-pad-right)
    (cons 'string-center string-center)
    (cons 'string-repeat string-repeat)
    (cons 'string-count string-count)
    ; Sequence utilities
    (cons 'scan scan)
    (cons 'scan-right scan-right)
    (cons 'windows windows)
    (cons 'pairwise pairwise)
    (cons 'differences differences)
    (cons 'running-sum running-sum)
    (cons 'running-product running-product)
    ; Statistical utilities
    (cons 'mean mean)
    (cons 'median median)
    (cons 'mode mode)
    (cons 'variance variance)
    (cons 'std-dev std-dev)
    (cons 'normalize-list normalize-list)
    (cons 'z-score z-score)
    ; Bitwise operations
    (cons 'bit-and bit-and)
    (cons 'bit-or bit-or)
    (cons 'bit-xor bit-xor)
    (cons 'bit-not bit-not)
    (cons 'bit-shift-left bit-shift-left)
    (cons 'bit-shift-right bit-shift-right)
    (cons 'bit-set? bit-set?)
    (cons 'bit-set bit-set)
    (cons 'bit-clear bit-clear)
    (cons 'bit-toggle bit-toggle)
    (cons 'bit-count bit-count)
    ; Predicate combinators
    (cons 'complement complement)
    (cons 'conjoin conjoin)
    (cons 'disjoin disjoin)
    (cons 'pred-and pred-and)
    (cons 'pred-or pred-or)
    (cons 'satisfies? satisfies?)
    ; Advanced function combinators
    (cons 'juxt juxt)
    (cons 'juxt2 juxt2)
    (cons 'fork fork)
    (cons 'converge converge)
    (cons 'on on)
    (cons 'both both)
    ; Currying and partial application
    (cons 'curry2 curry2)
    (cons 'curry3 curry3)
    (cons 'uncurry2 uncurry2)
    (cons 'uncurry3 uncurry3)
    (cons 'partial partial)
    (cons 'partial2 partial2)
    (cons 'partial-right partial-right)
    (cons 'flip flip)
    (cons 'flip3 flip3)
    ; Iteration utilities
    (cons 'iterate-n iterate-n)
    (cons 'iterate-until iterate-until)
    (cons 'iterate-while iterate-while)
    (cons 'fixed-point fixed-point)
    ; Collection utilities
    (cons 'count-if count-if)
    (cons 'remove-if remove-if)
    (cons 'remove-duplicates remove-duplicates)
    (cons 'unique unique)
    (cons 'replace-if replace-if)
    (cons 'substitute substitute)
    (cons 'split-when split-when)
    (cons 'interleave interleave)
    ; Numeric range utilities
    (cons 'range-step range-step)
    (cons 'linspace linspace)
    (cons 'geometric-series geometric-series)
    ; Pair utilities
    (cons 'pair-map pair-map)
    (cons 'pair-swap pair-swap)
    (cons 'pair-to-list pair-to-list)
    (cons 'list-to-pair list-to-pair)
    ; Extended alist operations
    (cons 'alist-invert alist-invert)
    (cons 'alist-group-by alist-group-by)
    (cons 'alist-zip alist-zip)
    (cons 'alist-select alist-select)
    (cons 'alist-reject alist-reject)
    (cons 'alist-rename-key alist-rename-key)
    ; Mathematical utilities
    (cons 'factorial factorial)
    (cons 'fibonacci fibonacci)
    (cons 'prime? prime?)
    (cons 'divisors divisors)
    (cons 'sum-range sum-range)
    (cons 'product-range product-range)
    (cons 'binomial binomial)
    ; More list utilities
    (cons 'scanr scanr)
    (cons 'reduce reduce)
    (cons 'reduce-right reduce-right)
    (cons 'find-last find-last)
    (cons 'index-where index-where)
    (cons 'last-index-where last-index-where)
    (cons 'indices-where indices-where)
    (cons 'take-last take-last)
    (cons 'drop-last drop-last)
    (cons 'insert-at insert-at)
    (cons 'remove-at remove-at)
    (cons 'update-at update-at)
    (cons 'set-at set-at)
    (cons 'rotate-left rotate-left)
    (cons 'rotate-right rotate-right)
    ; Extended string utilities
    (cons 'string-repeat string-repeat)
    (cons 'string-reverse string-reverse)
    (cons 'string-take string-take)
    (cons 'string-drop string-drop)
    (cons 'string-take-right string-take-right)
    (cons 'string-drop-right string-drop-right)
    (cons 'string-trim-left string-trim-left)
    (cons 'string-trim-right string-trim-right)
    (cons 'string-trim string-trim)
    (cons 'string-empty? string-empty?)
    (cons 'string-blank? string-blank?)
    ; Control flow
    (cons 'unless unless)
    (cons 'when-let when-let)
    (cons 'cond-result cond-result)
    ; Comparison utilities
    (cons 'clamp-val clamp-val)
    (cons 'between-exclusive? between-exclusive?)
    (cons 'approximately-equal? approximately-equal?)
    (cons 'compare compare)
    ; Debugging utilities
    (cons 'tap tap)
    (cons 'trace trace)
    ; Set operations
    (cons 'set-from-list set-from-list)
    (cons 'set-member? set-member?)
    (cons 'set-add set-add)
    (cons 'set-remove set-remove)
    (cons 'set-union set-union)
    (cons 'set-intersection set-intersection)
    (cons 'set-difference set-difference)
    (cons 'set-symmetric-difference set-symmetric-difference)
    (cons 'set-subset? set-subset?)
    (cons 'set-equal? set-equal?)
    (cons 'set-empty? set-empty?)
    (cons 'set-size set-size)
    ; Tree operations
    (cons 'tree-leaf? tree-leaf?)
    (cons 'tree-node? tree-node?)
    (cons 'tree-children tree-children)
    (cons 'tree-map tree-map)
    (cons 'tree-fold tree-fold)
    (cons 'tree-flatten tree-flatten)
    (cons 'tree-depth tree-depth)
    (cons 'tree-size tree-size)
    (cons 'tree-filter tree-filter)
    (cons 'tree-find tree-find)
    ; Graph utilities
    (cons 'graph-nodes graph-nodes)
    (cons 'graph-neighbors graph-neighbors)
    (cons 'graph-has-edge? graph-has-edge?)
    (cons 'graph-add-edge graph-add-edge)
    (cons 'graph-remove-edge graph-remove-edge)
    (cons 'graph-reverse graph-reverse)
    (cons 'graph-bfs graph-bfs)
    (cons 'graph-dfs graph-dfs)
    ; More numeric utilities
    (cons 'sgn sgn)
    (cons 'copysign copysign)
    (cons 'div-ceil div-ceil)
    (cons 'div-floor div-floor)
    (cons 'mod-positive mod-positive)
    (cons 'is-power-of-2? is-power-of-2?)
    (cons 'next-power-of-2 next-power-of-2)
    (cons 'log2-int log2-int)
    (cons 'integer-sqrt integer-sqrt)
    (cons 'sum-of-squares sum-of-squares)
    (cons 'dot-product dot-product)
    (cons 'magnitude magnitude)
    (cons 'normalize-vector normalize-vector)
    ; Sequence generators
    (cons 'primes-up-to primes-up-to)
    (cons 'fibonacci-sequence fibonacci-sequence)
    (cons 'powers-of powers-of)
    (cons 'triangular-numbers triangular-numbers)
    ; Combinatorics
    (cons 'permutations permutations)
    (cons 'remove-first remove-first)
    (cons 'combinations combinations)
    (cons 'cartesian-product cartesian-product)
    (cons 'power-set power-set)
    ; Queue operations
    (cons 'queue-empty queue-empty)
    (cons 'queue-empty? queue-empty?)
    (cons 'queue-enqueue queue-enqueue)
    (cons 'queue-dequeue queue-dequeue)
    (cons 'queue-front queue-front)
    (cons 'queue-size queue-size)
    (cons 'queue-to-list queue-to-list)
    (cons 'queue-from-list queue-from-list)
    ; Heap operations
    (cons 'heap-empty heap-empty)
    (cons 'heap-empty? heap-empty?)
    (cons 'heap-insert heap-insert)
    (cons 'heap-min heap-min)
    (cons 'heap-pop heap-pop)
    (cons 'heap-from-list heap-from-list)
    (cons 'heap-size heap-size)
    ; Priority queue
    (cons 'pq-empty pq-empty)
    (cons 'pq-empty? pq-empty?)
    (cons 'pq-insert pq-insert)
    (cons 'pq-pop pq-pop)
    (cons 'pq-peek pq-peek)
    ; Deque operations
    (cons 'deque-empty deque-empty)
    (cons 'deque-empty? deque-empty?)
    (cons 'deque-push-front deque-push-front)
    (cons 'deque-push-back deque-push-back)
    (cons 'deque-pop-front deque-pop-front)
    (cons 'deque-pop-back deque-pop-back)
    ; Stack operations
    (cons 'stack-empty stack-empty)
    (cons 'stack-empty? stack-empty?)
    (cons 'stack-push stack-push)
    (cons 'stack-pop stack-pop)
    (cons 'stack-peek stack-peek)
    (cons 'stack-size stack-size)
    ; Number parsing
    (cons 'digits digits)
    (cons 'from-digits from-digits)
    (cons 'digit-sum digit-sum)
    (cons 'digit-count digit-count)
    (cons 'reverse-number reverse-number)
    (cons 'palindrome-number? palindrome-number?)
    ; Base conversion
    (cons 'to-base to-base)
    (cons 'from-base from-base)
    (cons 'to-binary to-binary)
    (cons 'from-binary from-binary)
    (cons 'to-hex-digits to-hex-digits)
    (cons 'from-hex-digits from-hex-digits)
    ; List searching
    (cons 'binary-search binary-search)
    (cons 'lower-bound lower-bound)
    (cons 'upper-bound upper-bound)
    ; Sliding window
    (cons 'sliding-max sliding-max)
    (cons 'sliding-min sliding-min)
    (cons 'sliding-sum sliding-sum)
    ; Run-length encoding
    (cons 'rle-encode rle-encode)
    (cons 'rle-decode rle-decode)
    ; More functional patterns
    (cons 'unfold-right unfold-right)
    (cons 'until until)
    (cons 'while-fn while-fn)
    (cons 'iterate-collect iterate-collect)
    (cons 'converge-to converge-to)
    ; Alist extensions
    (cons 'alist-merge alist-merge)
    (cons 'alist-map-values alist-map-values)
    (cons 'alist-map-keys alist-map-keys)
    (cons 'alist-find alist-find)
    (cons 'alist-count alist-count)
    ; Zipper operations
    (cons 'zipper-from-list zipper-from-list)
    (cons 'zipper-to-list zipper-to-list)
    (cons 'zipper-focus zipper-focus)
    (cons 'zipper-right zipper-right)
    (cons 'zipper-left zipper-left)
    (cons 'zipper-update zipper-update)
    (cons 'zipper-set zipper-set)
    (cons 'zipper-insert-left zipper-insert-left)
    (cons 'zipper-insert-right zipper-insert-right)
    (cons 'zipper-delete zipper-delete)
    ; Validation combinators
    (cons 'validate validate)
    (cons 'validate-all validate-all)
    (cons 'validate-chain validate-chain)
    (cons 'validator validator)
    (cons 'not-empty-validator not-empty-validator)
    (cons 'positive-validator positive-validator)
    (cons 'non-negative-validator non-negative-validator)
    (cons 'in-range-validator in-range-validator)
    ; Lens-like utilities
    (cons 'lens-get lens-get)
    (cons 'lens-set lens-set)
    (cons 'lens-update lens-update)
    ; Applicative functor utilities
    (cons 'ap ap)
    (cons 'lift2 lift2)
    (cons 'lift3 lift3)
    (cons 'sequence-list sequence-list)
    (cons 'traverse-list traverse-list)
    ; More string utilities
    (cons 'string-split-at string-split-at)
    (cons 'string-words string-words)
    (cons 'string-unwords string-unwords)
    (cons 'string-lines string-lines)
    (cons 'string-unlines string-unlines)
    (cons 'string-center string-center)
    (cons 'string-ljust string-ljust)
    (cons 'string-rjust string-rjust)
    ; Memoization helpers
    (cons 'make-memo-table make-memo-table)
    (cons 'memo-lookup memo-lookup)
    (cons 'memo-insert memo-insert)
    ; Matrix operations
    (cons 'matrix-rows matrix-rows)
    (cons 'matrix-cols matrix-cols)
    (cons 'matrix-ref matrix-ref)
    (cons 'matrix-set matrix-set)
    (cons 'matrix-map matrix-map)
    (cons 'matrix-zip-with matrix-zip-with)
    (cons 'matrix-add matrix-add)
    (cons 'matrix-scale matrix-scale)
    (cons 'matrix-multiply matrix-multiply)
    (cons 'make-matrix make-matrix)
    (cons 'identity-matrix identity-matrix)
    ; Logic utilities
    (cons 'implies implies)
    (cons 'iff iff)
    (cons 'nand nand)
    (cons 'nor nor)
    (cons 'bool->int bool->int)
    (cons 'int->bool int->bool)
    ; State monad
    (cons 'state-return state-return)
    (cons 'state-bind state-bind)
    (cons 'state-get state-get)
    (cons 'state-put state-put)
    (cons 'state-modify state-modify)
    (cons 'state-run state-run)
    (cons 'state-eval state-eval)
    (cons 'state-exec state-exec)
    ; Reader monad
    (cons 'reader-return reader-return)
    (cons 'reader-bind reader-bind)
    (cons 'reader-ask reader-ask)
    (cons 'reader-asks reader-asks)
    (cons 'reader-local reader-local)
    (cons 'reader-run reader-run)
    ; Writer monad
    (cons 'writer-return writer-return)
    (cons 'writer-bind writer-bind)
    (cons 'writer-tell writer-tell)
    (cons 'writer-listen writer-listen)
    (cons 'writer-run writer-run)
    ; More list utilities
    (cons 'rotate-left rotate-left)
    (cons 'rotate-right rotate-right)
    (cons 'tails tails)
    (cons 'inits inits)
    (cons 'sublists sublists)
    (cons 'is-prefix? is-prefix?)
    (cons 'is-suffix? is-suffix?)
    (cons 'is-infix? is-infix?)
    (cons 'split-at split-at)
    (cons 'chunks-of chunks-of)
    (cons 'windows windows)
    (cons 'interleave interleave)
    (cons 'dedup-consecutive dedup-consecutive)
    (cons 'group-by group-by)
    (cons 'frequencies frequencies)
    ; Polynomial operations
    (cons 'poly-zero poly-zero)
    (cons 'poly-one poly-one)
    (cons 'poly-degree poly-degree)
    (cons 'poly-add poly-add)
    (cons 'poly-scale poly-scale)
    (cons 'poly-shift poly-shift)
    (cons 'poly-multiply poly-multiply)
    (cons 'poly-eval poly-eval)
    (cons 'poly-derivative poly-derivative)
    (cons 'poly-from-roots poly-from-roots)
    ; Interval operations
    (cons 'make-interval make-interval)
    (cons 'interval-lo interval-lo)
    (cons 'interval-hi interval-hi)
    (cons 'interval-empty? interval-empty?)
    (cons 'interval-contains? interval-contains?)
    (cons 'interval-width interval-width)
    (cons 'interval-midpoint interval-midpoint)
    (cons 'interval-intersect interval-intersect)
    (cons 'interval-union interval-union)
    (cons 'interval-overlaps? interval-overlaps?)
    ; Arrow combinators
    (cons 'arr arr)
    (cons 'first first)
    (cons 'second second)
    (cons 'split split)
    (cons 'fanout fanout)
    (cons 'combine combine)
    (cons 'parallel parallel)
    (cons 'arrow-compose arrow-compose)
    ; Continuation utilities
    (cons 'call-with-escape call-with-escape)
    (cons 'trampoline-return trampoline-return)
    (cons 'trampoline-bounce trampoline-bounce)
    (cons 'run-trampoline run-trampoline)
    ; More numeric utilities
    (cons 'average average)
    (cons 'variance variance)
    (cons 'stddev stddev)
    (cons 'median median)
    (cons 'clamp-list clamp-list)
    (cons 'normalize-list normalize-list)
    (cons 'dot dot)
    (cons 'magnitude magnitude)
    (cons 'normalize-vector normalize-vector)
    (cons 'cross-product cross-product)
    ; Association list extensions
    (cons 'alist-update alist-update)
    (cons 'alist-insert-with alist-insert-with)
    (cons 'alist-from-pairs alist-from-pairs)
    (cons 'alist-to-pairs alist-to-pairs)
    (cons 'alist-has-key? alist-has-key?)
    (cons 'alist-delete alist-delete)
    (cons 'alist-select alist-select)
    (cons 'alist-reject alist-reject)
    ; Parser combinators
    (cons 'parse-ok parse-ok)
    (cons 'parse-err parse-err)
    (cons 'parse-ok? parse-ok?)
    (cons 'parse-value parse-value)
    (cons 'parse-rest parse-rest)
    (cons 'parse-error parse-error)
    (cons 'p-return p-return)
    (cons 'p-fail p-fail)
    (cons 'p-item p-item)
    (cons 'p-satisfy p-satisfy)
    (cons 'p-char p-char)
    (cons 'p-digit p-digit)
    (cons 'p-alpha p-alpha)
    (cons 'p-alphanum p-alphanum)
    (cons 'p-space p-space)
    (cons 'p-bind p-bind)
    (cons 'p-then p-then)
    (cons 'p-skip p-skip)
    (cons 'p-or p-or)
    (cons 'p-many p-many)
    (cons 'p-many1 p-many1)
    (cons 'p-optional p-optional)
    (cons 'p-string p-string)
    (cons 'p-spaces p-spaces)
    (cons 'p-token p-token)
    (cons 'p-run p-run)
    ; Binary tree operations
    (cons 'tree-empty tree-empty)
    (cons 'tree-empty? tree-empty?)
    (cons 'tree-node tree-node)
    (cons 'tree-leaf tree-leaf)
    (cons 'tree-value tree-value)
    (cons 'tree-left tree-left)
    (cons 'tree-right tree-right)
    (cons 'tree-size tree-size)
    (cons 'tree-height tree-height)
    (cons 'tree-inorder tree-inorder)
    (cons 'tree-preorder tree-preorder)
    (cons 'tree-postorder tree-postorder)
    (cons 'tree-levelorder tree-levelorder)
    (cons 'bst-insert bst-insert)
    (cons 'bst-member? bst-member?)
    (cons 'bst-from-list bst-from-list)
    (cons 'bst-min bst-min)
    (cons 'bst-max bst-max)
    ; Union-Find
    (cons 'uf-make uf-make)
    (cons 'uf-make-set uf-make-set)
    (cons 'uf-find uf-find)
    (cons 'uf-union uf-union)
    (cons 'uf-connected? uf-connected?)
    ; Maybe/Option
    (cons 'just just)
    (cons 'nothing nothing)
    (cons 'just? just?)
    (cons 'nothing? nothing?)
    (cons 'from-just from-just)
    (cons 'maybe maybe)
    (cons 'maybe-map maybe-map)
    (cons 'maybe-bind maybe-bind)
    (cons 'maybe-or maybe-or)
    (cons 'maybe-and maybe-and)
    (cons 'from-maybe from-maybe)
    (cons 'list->maybe list->maybe)
    (cons 'maybe->list maybe->list)
    (cons 'cat-maybes cat-maybes)
    (cons 'map-maybe map-maybe)
    ; Either
    (cons 'left left)
    (cons 'right right)
    (cons 'left? left?)
    (cons 'right? right?)
    (cons 'from-left from-left)
    (cons 'from-right from-right)
    (cons 'either either)
    (cons 'either-map either-map)
    (cons 'either-map-left either-map-left)
    (cons 'either-bind either-bind)
    (cons 'either-bimap either-bimap)
    (cons 'partition-eithers partition-eithers)
    (cons 'lefts lefts)
    (cons 'rights rights)
    ; Format utilities
    (cons 'format-simple format-simple)
    (cons 'join-with join-with)
    (cons 'repeat-string repeat-string)
    ; Control flow
    (cons 'guard guard)
    (cons 'when-just when-just)
    (cons 'unless-nothing unless-nothing)
    (cons 'if-let if-let)
    (cons 'cond-list cond-list)
    ; Comparison utilities
    (cons 'comparing comparing)
    (cons 'compare-by compare-by)
    (cons 'chain-comparators chain-comparators)
    (cons 'sort-by sort-by)
    (cons 'sort-on sort-on)
    (cons 'group-consecutive-by group-consecutive-by)
    ; Numeric predicates
    (cons 'divisible-by? divisible-by?)
    (cons 'coprime? coprime?)
    (cons 'perfect-square? perfect-square?)
    (cons 'triangular-number triangular-number)
    (cons 'is-triangular? is-triangular?)
    (cons 'pentagonal-number pentagonal-number)
    (cons 'hexagonal-number hexagonal-number)
    (cons 'collatz-next collatz-next)
    (cons 'collatz-sequence collatz-sequence)
    ; Lazy stream extensions
    (cons 'stream-empty stream-empty)
    (cons 'stream-empty? stream-empty?)
    (cons 'stream-drop stream-drop)
    (cons 'stream-cycle stream-cycle)
    (cons 'stream-take-while stream-take-while)
    (cons 'list->stream list->stream)
    ; Extended graph algorithms
    (cons 'graph-in-degree graph-in-degree)
    (cons 'graph-out-degree graph-out-degree)
    (cons 'topological-sort topological-sort)
    (cons 'graph-reachable graph-reachable)
    (cons 'graph-shortest-path graph-shortest-path)
    ; Numeric methods
    (cons 'newton-raphson newton-raphson)
    (cons 'bisection bisection)
    (cons 'numerical-derivative numerical-derivative)
    (cons 'numerical-integral numerical-integral)
    (cons 'secant-method secant-method)
    ; Trie data structure
    (cons 'trie-empty trie-empty)
    (cons 'trie-value trie-value)
    (cons 'trie-children trie-children)
    (cons 'trie-insert trie-insert)
    (cons 'trie-lookup trie-lookup)
    (cons 'trie-has-key? trie-has-key?)
    (cons 'string->key string->key)
    (cons 'trie-insert-string trie-insert-string)
    (cons 'trie-lookup-string trie-lookup-string)
    ; Extended combinatorics
    (cons 'permutations-k permutations-k)
    (cons 'derangements derangements)
    (cons 'count-derangements count-derangements)
    (cons 'stirling-second stirling-second)
    (cons 'bell-number bell-number)
    (cons 'catalan-number catalan-number)
    (cons 'partitions-integer partitions-integer)
    (cons 'count-partitions count-partitions)
    ; Extended bitwise utilities
    (cons 'lowest-set-bit lowest-set-bit)
    (cons 'highest-set-bit highest-set-bit)
    ; Ring buffer
    (cons 'ring-buffer-new ring-buffer-new)
    (cons 'ring-buffer-capacity ring-buffer-capacity)
    (cons 'ring-buffer-push ring-buffer-push)
    (cons 'ring-buffer-peek ring-buffer-peek)
    (cons 'ring-buffer-pop ring-buffer-pop)
    ; S-expression utilities
    (cons 'sexp-atom? sexp-atom?)
    (cons 'sexp-list? sexp-list?)
    (cons 'sexp-dotted? sexp-dotted?)
    (cons 'sexp-length sexp-length)
    (cons 'sexp-depth sexp-depth)
    (cons 'sexp-count-atoms sexp-count-atoms)
    (cons 'sexp-flatten sexp-flatten)
    (cons 'sexp-map sexp-map)
    (cons 'sexp-filter sexp-filter)
    (cons 'sexp-find sexp-find)
    (cons 'sexp-substitute sexp-substitute)
    (cons 'sexp-contains? sexp-contains?)
    ; Transducers
    (cons 't-map t-map)
    (cons 't-filter t-filter)
    (cons 't-cat t-cat)
    (cons 't-mapcat t-mapcat)
    (cons 't-keep t-keep)
    (cons 'transduce transduce)
    (cons 'into-list into-list)
    (cons 't-comp t-comp)
    (cons 't-comp3 t-comp3)
    ; Memo table utilities
    (cons 'memo-table-new memo-table-new)
    (cons 'memo-table-get memo-table-get)
    (cons 'memo-table-put memo-table-put)
    (cons 'memo-table-contains? memo-table-contains?)
    (cons 'memo-table-remove memo-table-remove)
    (cons 'memo-table-keys memo-table-keys)
    (cons 'memo-table-values memo-table-values)
    ; Algebraic structures - Monoid
    (cons 'make-monoid make-monoid)
    (cons 'monoid-empty monoid-empty)
    (cons 'monoid-append monoid-append)
    (cons 'mconcat mconcat)
    (cons 'sum-monoid sum-monoid)
    (cons 'product-monoid product-monoid)
    (cons 'list-monoid list-monoid)
    (cons 'string-monoid string-monoid)
    (cons 'all-monoid all-monoid)
    (cons 'any-monoid any-monoid)
    (cons 'max-monoid max-monoid)
    (cons 'min-monoid min-monoid)
    (cons 'first-monoid first-monoid)
    (cons 'last-monoid last-monoid)
    (cons 'endo-monoid endo-monoid)
    (cons 'dual-monoid dual-monoid)
    ; Foldable
    (cons 'fold-map fold-map)
    (cons 'fold-sum fold-sum)
    (cons 'fold-product fold-product)
    (cons 'fold-all fold-all)
    (cons 'fold-any fold-any)
    ; Semigroup
    (cons 'make-semigroup make-semigroup)
    (cons 'semigroup-append semigroup-append)
    (cons 'sconcat sconcat)
    ; Functor
    (cons 'make-functor make-functor)
    (cons 'functor-map functor-map)
    (cons 'list-functor list-functor)
    (cons 'maybe-functor maybe-functor)
    (cons 'either-functor either-functor)
    ; Applicative
    (cons 'make-applicative make-applicative)
    (cons 'applicative-pure applicative-pure)
    (cons 'applicative-ap applicative-ap)
    (cons 'liftA2 liftA2)
    ; Comonad
    (cons 'make-comonad make-comonad)
    (cons 'comonad-extract comonad-extract)
    (cons 'comonad-extend comonad-extend)
    (cons 'list-zipper-extract list-zipper-extract)
    (cons 'list-zipper-extend list-zipper-extend)
    ; Bifunctor
    (cons 'bimap bimap)
    (cons 'first-fn first-fn)
    (cons 'second-fn second-fn)
    ; Profunctor
    (cons 'dimap dimap)
    (cons 'lmap lmap)
    (cons 'rmap rmap)
    ; Kleisli
    (cons 'kleisli-maybe kleisli-maybe)
    (cons 'kleisli-either kleisli-either)
    (cons 'kleisli-list kleisli-list)
    ; Arrow operations
    (cons 'arr-id arr-id)
    (cons 'arr-compose arr-compose)
    (cons 'arr-first arr-first)
    (cons 'arr-second arr-second)
    (cons 'arr-split arr-split)
    (cons 'arr-fanout arr-fanout)
    (cons 'arr-choice arr-choice)
    ; Recursive schemes
    (cons 'cata cata)
    (cons 'ana ana)
    (cons 'hylo hylo)
    (cons 'para para)
    ; Expression utilities
    (cons 'quoted? quoted?)
    (cons 'unquote-expr unquote-expr)
    (cons 'make-quote make-quote)
    (cons 'lambda? lambda?)
    (cons 'application? application?)
    (cons 'if? if?)
    (cons 'let? let?)
    ; Symbol utilities
    (cons 'symbol-append symbol-append)
    (cons 'make-gensym make-gensym)
    ; Control flow
    (cons 'iterate-times iterate-times)
    (cons 'find-fixed-point find-fixed-point)
    (cons 'do-while do-while)
    (cons 'do-until do-until)
    ; String manipulation
    (cons 'string-null? string-null?)
    (cons 'string-take string-take)
    (cons 'string-drop string-drop)
    (cons 'string-take-right string-take-right)
    (cons 'string-drop-right string-drop-right)
    (cons 'string-pad string-pad)
    (cons 'string-pad-right string-pad-right)
    (cons 'string-trim-left string-trim-left)
    (cons 'string-trim-right string-trim-right)
    (cons 'string-trim string-trim)
    (cons 'string-trim-whitespace string-trim-whitespace)
    (cons 'string-contains? string-contains?)
    (cons 'string-prefix? string-prefix?)
    (cons 'string-suffix? string-suffix?)
    (cons 'string-split-at string-split-at)
    (cons 'string-replace-first string-replace-first)
    (cons 'string-replace-all string-replace-all)
    (cons 'string-reverse string-reverse)
    (cons 'string-join string-join)
    (cons 'string-split-char string-split-char)
    (cons 'string-upcase string-upcase)
    (cons 'string-downcase string-downcase)
    (cons 'string-titlecase string-titlecase)
    (cons 'string-count string-count)
    (cons 'string-index string-index)
    (cons 'string-index-right string-index-right)
    ; Association list utilities
    (cons 'assoc assoc)
    (cons 'assq assq)
    (cons 'assv assv)
    (cons 'alist-ref alist-ref)
    (cons 'alist-set alist-set)
    (cons 'alist-delete alist-delete)
    (cons 'alist-keys alist-keys)
    (cons 'alist-values alist-values)
    (cons 'alist-map alist-map)
    (cons 'alist-filter alist-filter)
    (cons 'alist-merge alist-merge)
    (cons 'alist->list alist->list)
    (cons 'list->alist list->alist)
    (cons 'alist-has-key? alist-has-key?)
    (cons 'alist-update alist-update)
    ; Set operations
    (cons 'set-empty set-empty)
    (cons 'set-singleton set-singleton)
    (cons 'set-member? set-member?)
    (cons 'set-insert set-insert)
    (cons 'set-delete set-delete)
    (cons 'set-union set-union)
    (cons 'set-intersection set-intersection)
    (cons 'set-difference set-difference)
    (cons 'set-symmetric-difference set-symmetric-difference)
    (cons 'set-subset? set-subset?)
    (cons 'set-superset? set-superset?)
    (cons 'set-equal? set-equal?)
    (cons 'set-disjoint? set-disjoint?)
    (cons 'set-size set-size)
    (cons 'set->list set->list)
    (cons 'list->set list->set)
    (cons 'set-filter set-filter)
    (cons 'set-map set-map)
    (cons 'set-fold set-fold)
    (cons 'set-partition set-partition)
    ; Queue
    (cons 'queue-empty queue-empty)
    (cons 'queue-empty? queue-empty?)
    (cons 'queue-enqueue queue-enqueue)
    (cons 'queue-front queue-front)
    (cons 'queue-dequeue queue-dequeue)
    (cons 'queue-size queue-size)
    (cons 'queue->list queue->list)
    (cons 'list->queue list->queue)
    ; Deque
    (cons 'deque-empty deque-empty)
    (cons 'deque-empty? deque-empty?)
    (cons 'deque-push-front deque-push-front)
    (cons 'deque-push-back deque-push-back)
    (cons 'deque-front deque-front)
    (cons 'deque-back deque-back)
    (cons 'deque-pop-front deque-pop-front)
    (cons 'deque-pop-back deque-pop-back)
    (cons 'deque-size deque-size)
    (cons 'deque->list deque->list)
    (cons 'list->deque list->deque)
    ; Sorting
    (cons 'insert-sorted insert-sorted)
    (cons 'insertion-sort insertion-sort)
    (cons 'merge-sorted merge-sorted)
    (cons 'merge-sort merge-sort)
    (cons 'quicksort quicksort)
    (cons 'sort sort)
    (cons 'sort-by-key sort-by-key)
    (cons 'sort-descending sort-descending)
    (cons 'sorted? sorted?)
    (cons 'minimum minimum)
    (cons 'maximum maximum)
    (cons 'minimum-by minimum-by)
    (cons 'maximum-by maximum-by)
    ; Priority queue
    (cons 'pq-empty pq-empty)
    (cons 'pq-empty? pq-empty?)
    (cons 'pq-insert pq-insert)
    (cons 'pq-peek pq-peek)
    (cons 'pq-peek-priority pq-peek-priority)
    (cons 'pq-pop pq-pop)
    (cons 'pq-size pq-size)
    ; Bag/Multiset
    (cons 'bag-empty bag-empty)
    (cons 'bag-insert bag-insert)
    (cons 'bag-count bag-count)
    (cons 'bag-remove bag-remove)
    (cons 'bag-remove-all bag-remove-all)
    (cons 'bag-member? bag-member?)
    (cons 'bag-unique-elements bag-unique-elements)
    (cons 'bag-total-count bag-total-count)
    (cons 'bag->list bag->list)
    (cons 'list->bag list->bag)
    (cons 'bag-union bag-union)
    (cons 'bag-intersection bag-intersection)
    (cons 'bag-sum bag-sum)
    ; Random number generation
    (cons 'make-rng make-rng)
    (cons 'rng-next rng-next)
    (cons 'rng-int rng-int)
    (cons 'rng-range rng-range)
    (cons 'rng-bool rng-bool)
    (cons 'rng-choice rng-choice)
    (cons 'rng-shuffle rng-shuffle)
    (cons 'rng-sample rng-sample)
    (cons 'rng-take rng-take)
    ; Sequence utilities
    (cons 'sliding-window sliding-window)
    (cons 'chunks chunks)
    (cons 'chunks-exact chunks-exact)
    (cons 'interleave interleave)
    (cons 'interleave-all interleave-all)
    (cons 'rotate-left rotate-left)
    (cons 'rotate-right rotate-right)
    (cons 'frequencies frequencies)
    (cons 'group-by group-by)
    (cons 'partition-all partition-all)
    (cons 'split-at-pred split-at-pred)
    (cons 'split-when split-when)
    (cons 'dedupe dedupe)
    (cons 'dedupe-by dedupe-by)
    (cons 'run-length-encode run-length-encode)
    (cons 'run-length-decode run-length-decode)
    ; Zipper
    (cons 'zipper-make zipper-make)
    (cons 'zipper-left zipper-left)
    (cons 'zipper-focus zipper-focus)
    (cons 'zipper-right zipper-right)
    (cons 'zipper-move-left zipper-move-left)
    (cons 'zipper-move-right zipper-move-right)
    (cons 'zipper-set zipper-set)
    (cons 'zipper-modify zipper-modify)
    (cons 'zipper-insert-left zipper-insert-left)
    (cons 'zipper-insert-right zipper-insert-right)
    (cons 'zipper-delete zipper-delete)
    (cons 'zipper-to-list zipper-to-list)
    (cons 'zipper-start zipper-start)
    (cons 'zipper-end zipper-end)
    (cons 'zipper-find zipper-find)
    ; Validation
    (cons 'validation-ok validation-ok)
    (cons 'validation-err validation-err)
    (cons 'validation-ok? validation-ok?)
    (cons 'validation-err? validation-err?)
    (cons 'validation-value validation-value)
    (cons 'validation-map validation-map)
    (cons 'validation-map-err validation-map-err)
    (cons 'validation-bind validation-bind)
    (cons 'validation-ap validation-ap)
    (cons 'validate-all validate-all)
    (cons 'validate-when validate-when)
    (cons 'validate-not-null validate-not-null)
    (cons 'validate-positive validate-positive)
    (cons 'validate-in-range validate-in-range)
    (cons 'validate-non-empty validate-non-empty)
    ; Lenses
    (cons 'make-lens make-lens)
    (cons 'lens-get lens-get)
    (cons 'lens-set lens-set)
    (cons 'lens-over lens-over)
    (cons 'lens-compose lens-compose)
    (cons 'lens-head lens-head)
    (cons 'lens-tail lens-tail)
    (cons 'lens-nth lens-nth)
    (cons 'lens-fst lens-fst)
    (cons 'lens-snd lens-snd)
    (cons 'lens-key lens-key)
    ; Numeric utilities
    (cons 'clamp clamp)
    (cons 'lerp lerp)
    (cons 'inverse-lerp inverse-lerp)
    (cons 'remap remap)
    (cons 'sign sign)
    (cons 'step step)
    (cons 'smoothstep smoothstep)
    (cons 'wrap wrap)
    (cons 'ping-pong ping-pong)
    ; Statistics
    (cons 'mean mean)
    (cons 'variance variance)
    (cons 'std-dev std-dev)
    (cons 'median median)
    (cons 'mode mode)
    (cons 'range-stat range-stat)
    (cons 'percentile percentile)
    (cons 'quartiles quartiles)
    (cons 'correlation correlation)
    ; Rose trees
    (cons 'rose-node rose-node)
    (cons 'rose-leaf rose-leaf)
    (cons 'rose-value rose-value)
    (cons 'rose-children rose-children)
    (cons 'rose-leaf? rose-leaf?)
    (cons 'rose-map rose-map)
    (cons 'rose-fold rose-fold)
    (cons 'rose-flatten rose-flatten)
    (cons 'rose-depth rose-depth)
    (cons 'rose-size rose-size)
    (cons 'rose-find rose-find)
    (cons 'rose-filter rose-filter)
    (cons 'rose-paths rose-paths)
    (cons 'rose-from-nested rose-from-nested)
    ; Heap (Priority Queue)
    (cons 'heap-empty heap-empty)
    (cons 'heap-empty? heap-empty?)
    (cons 'heap-singleton heap-singleton)
    (cons 'heap-value heap-value)
    (cons 'heap-left heap-left)
    (cons 'heap-right heap-right)
    (cons 'heap-merge heap-merge)
    (cons 'heap-insert heap-insert)
    (cons 'heap-find-min heap-find-min)
    (cons 'heap-delete-min heap-delete-min)
    (cons 'heap-build heap-build)
    (cons 'heap-sort heap-sort)
    ; Ring Buffer
    (cons 'ring-make ring-make)
    (cons 'ring-capacity ring-capacity)
    (cons 'ring-write-pos ring-write-pos)
    (cons 'ring-read-pos ring-read-pos)
    (cons 'ring-buffer ring-buffer)
    (cons 'ring-empty? ring-empty?)
    (cons 'ring-full? ring-full?)
    (cons 'ring-size ring-size)
    (cons 'ring-write ring-write)
    (cons 'ring-read ring-read)
    (cons 'ring-peek ring-peek)
    (cons 'ring-to-list ring-to-list)
    ; Arrow Combinators
    (cons 'arr arr)
    (cons 'arrow-compose arrow-compose)
    (cons 'arrow-first arrow-first)
    (cons 'arrow-second arrow-second)
    (cons 'arrow-split arrow-split)
    (cons 'arrow-fanout arrow-fanout)
    (cons 'arrow-fanin arrow-fanin)
    (cons 'arrow-loop arrow-loop)
    ; Applicative
    (cons 'pure-list pure-list)
    (cons 'ap-list ap-list)
    (cons 'lift-a2 lift-a2)
    (cons 'lift-a3 lift-a3)
    (cons 'sequence-a sequence-a)
    (cons 'traverse-list traverse-list)
    (cons 'zip-list-ap zip-list-ap)
    ; Comonad
    (cons 'comonad-extract comonad-extract)
    (cons 'comonad-duplicate comonad-duplicate)
    (cons 'comonad-extend comonad-extend)
    ; Profunctor
    (cons 'dimap dimap)
    (cons 'lmap lmap)
    (cons 'rmap rmap)
    ; Recursion Schemes
    (cons 'list-cata list-cata)
    (cons 'list-ana list-ana)
    (cons 'list-hylo list-hylo)
    (cons 'tree-cata tree-cata)
    ; Difference Lists
    (cons 'dlist-empty dlist-empty)
    (cons 'dlist-singleton dlist-singleton)
    (cons 'dlist-append dlist-append)
    (cons 'dlist-cons dlist-cons)
    (cons 'dlist-snoc dlist-snoc)
    (cons 'dlist-to-list dlist-to-list)
    (cons 'list-to-dlist list-to-dlist)
    (cons 'dlist-concat dlist-concat)
    ; Finger Trees
    (cons 'finger-empty finger-empty)
    (cons 'finger-empty? finger-empty?)
    (cons 'finger-single finger-single)
    (cons 'finger-single? finger-single?)
    (cons 'finger-deep finger-deep)
    (cons 'finger-push-front finger-push-front)
    (cons 'finger-push-back finger-push-back)
    (cons 'finger-peek-front finger-peek-front)
    (cons 'finger-peek-back finger-peek-back)
    ; Additional String Utilities
    (cons 'string-words string-words)
    (cons 'string-lines string-lines)
    (cons 'string-unwords string-unwords)
    (cons 'string-unlines string-unlines)
    (cons 'string-pad-left string-pad-left)
    (cons 'string-pad-right string-pad-right)
    (cons 'string-center string-center)
    (cons 'string-repeat string-repeat)
    (cons 'string-take-while string-take-while)
    (cons 'string-drop-while string-drop-while)
    ; Logic and Constraints
    (cons 'all-different? all-different?)
    (cons 'exactly-one? exactly-one?)
    (cons 'at-most-one? at-most-one?)
    (cons 'at-least-one? at-least-one?)
    (cons 'count-equal count-equal)
    (cons 'all-equal? all-equal?)
    (cons 'implies implies)
    (cons 'iff iff)
    (cons 'xor xor)
    ; Character Predicates
    (cons 'char-digit? char-digit?)
    (cons 'char-alpha? char-alpha?)
    (cons 'char-alphanumeric? char-alphanumeric?)
    (cons 'char-whitespace? char-whitespace?)
    (cons 'char-lower? char-lower?)
    (cons 'char-upper? char-upper?)
    (cons 'char-downcase char-downcase)
    (cons 'char-upcase char-upcase)
    ; Graph Algorithms
    (cons 'graph-empty graph-empty)
    (cons 'graph-add-node graph-add-node)
    (cons 'graph-add-edge graph-add-edge)
    (cons 'graph-neighbors graph-neighbors)
    (cons 'graph-nodes graph-nodes)
    (cons 'graph-has-edge? graph-has-edge?)
    (cons 'graph-from-edges graph-from-edges)
    (cons 'bfs bfs)
    (cons 'dfs dfs)
    (cons 'graph-path-exists? graph-path-exists?)
    ; State Monad
    (cons 'state-return state-return)
    (cons 'state-bind state-bind)
    (cons 'state-get state-get)
    (cons 'state-put state-put)
    (cons 'state-modify state-modify)
    (cons 'state-run state-run)
    (cons 'state-eval state-eval)
    (cons 'state-exec state-exec)
    ; Reader Monad
    (cons 'reader-return reader-return)
    (cons 'reader-bind reader-bind)
    (cons 'reader-ask reader-ask)
    (cons 'reader-local reader-local)
    (cons 'reader-run reader-run)
    ; Writer Monad
    (cons 'writer-return writer-return)
    (cons 'writer-bind writer-bind)
    (cons 'writer-tell writer-tell)
    (cons 'writer-run writer-run)
    (cons 'writer-value writer-value)
    (cons 'writer-log writer-log)
    ; Numeric Extensions
    (cons 'divides? divides?)
    (cons 'coprime? coprime?)
    (cons 'perfect-square? perfect-square?)
    (cons 'triangular triangular)
    (cons 'pentagonal pentagonal)
    (cons 'hexagonal hexagonal)
    (cons 'binomial binomial)
    (cons 'catalan catalan)
    (cons 'stirling1 stirling1)
    (cons 'stirling2 stirling2)
    (cons 'digital-root digital-root)
    (cons 'collatz collatz)
    (cons 'collatz-length collatz-length)
    ; Parser Combinators
    (cons 'parse-return parse-return)
    (cons 'parse-fail parse-fail)
    (cons 'parse-item parse-item)
    (cons 'parse-satisfy parse-satisfy)
    (cons 'parse-bind parse-bind)
    (cons 'parse-map parse-map)
    (cons 'parse-or parse-or)
    (cons 'parse-seq2 parse-seq2)
    (cons 'parse-left parse-left)
    (cons 'parse-right parse-right)
    (cons 'parse-many parse-many)
    (cons 'parse-many1 parse-many1)
    (cons 'parse-optional parse-optional)
    (cons 'parse-between parse-between)
    (cons 'parse-sep-by parse-sep-by)
    (cons 'parse-sep-by1 parse-sep-by1)
    (cons 'parse-char parse-char)
    (cons 'parse-not-char parse-not-char)
    (cons 'parse-one-of parse-one-of)
    (cons 'parse-none-of parse-none-of)
    (cons 'parse-string parse-string)
    (cons 'parse-digit parse-digit)
    (cons 'parse-alpha parse-alpha)
    (cons 'parse-alphanum parse-alphanum)
    (cons 'parse-space parse-space)
    (cons 'parse-spaces parse-spaces)
    (cons 'parse-token parse-token)
    (cons 'digits->number digits->number)
    (cons 'parse-natural parse-natural)
    (cons 'parse-integer parse-integer)
    (cons 'parse-run parse-run)
    (cons 'parse-result parse-result)
    ; Control Flow
    (cons 'when-fn when-fn)
    (cons 'unless-fn unless-fn)
    (cons 'if-let-fn if-let-fn)
    (cons 'when-let-fn when-let-fn)
    (cons 'cond-helper cond-helper)
    (cons 'case-eq case-eq)
    (cons 'do-times do-times)
    (cons 'do-while do-while)
    (cons 'repeat-until repeat-until)
    (cons 'for-each-indexed for-each-indexed)
    (cons 'loop-collect loop-collect)
    (cons 'try-catch try-catch)
    (cons 'default default)
    (cons 'first-truthy first-truthy)
    (cons 'short-and short-and)
    (cons 'short-or short-or)
    (cons 'guard guard)
    (cons 'ensure ensure)
    (cons 'chain-guards chain-guards)
    ; Advanced List Utilities
    (cons 'frequencies frequencies)
    (cons 'dedupe dedupe)
    (cons 'dedupe-by dedupe-by)
    (cons 'interleave interleave)
    (cons 'take-nth take-nth)
    (cons 'drop-nth drop-nth)
    (cons 'rotate-left rotate-left)
    (cons 'rotate-right rotate-right)
    (cons 'split-when split-when)
    (cons 'split-with split-with)
    (cons 'group-runs group-runs)
    (cons 'map-runs map-runs)
    (cons 'find-indices find-indices)
    (cons 'replace-at replace-at)
    (cons 'insert-at insert-at)
    (cons 'remove-at remove-at)
    (cons 'swap-at swap-at)
    (cons 'prefix? prefix?)
    (cons 'suffix? suffix?)
    (cons 'sublist? sublist?)
    (cons 'count-occurrences count-occurrences)
    (cons 'windowed windowed)
    ; String Algorithms
    (cons 'edit-distance edit-distance)
    (cons 'hamming-distance hamming-distance)
    (cons 'longest-common-prefix longest-common-prefix)
    (cons 'longest-common-suffix longest-common-suffix)
    (cons 'string-similarity string-similarity)
    (cons 'fuzzy-match? fuzzy-match?)
    (cons 'string-normalize string-normalize)
    (cons 'string-tokenize string-tokenize)
    ; Testing Utilities
    (cons 'assert-eq assert-eq)
    (cons 'assert-true assert-true)
    (cons 'assert-false assert-false)
    (cons 'assert-throws assert-throws)
    (cons 'run-tests run-tests)
    (cons 'test-case test-case)
    ; Sequence Generation
    (cons 'arithmetic-seq arithmetic-seq)
    (cons 'geometric-seq geometric-seq)
    (cons 'fibonacci-seq fibonacci-seq)
    (cons 'primes-up-to primes-up-to)
    (cons 'powers-of powers-of)
    (cons 'factorials-up-to factorials-up-to)
    (cons 'triangular-numbers triangular-numbers)
    (cons 'convergent-seq convergent-seq)
    (cons 'cycle-detect cycle-detect)
    (cons 'iterate-n iterate-n)
))
"#;

use crate::fabric::{Env, EnvRef, EvalOutcome, Value, eval_spanned};
use crate::tools::{lower_expr, parse_fold_expr};

/// Load the prelude into an environment, returning the augmented environment.
/// The prelude returns an alist of (name . value) pairs which are inserted into the env.
pub fn load_prelude(env: EnvRef, fuel: usize) -> Result<EnvRef, String> {
    let parsed = parse_fold_expr(PRELUDE_SOURCE, Some("<prelude>"))
        .map_err(|e| format!("prelude parse error: {}", e))?;
    let expr = lower_expr(&parsed).map_err(|e| format!("prelude lower error: {}", e))?;

    let result = match eval_spanned(expr, env.clone(), fuel) {
        Ok(EvalOutcome::Done(value)) => value,
        Ok(EvalOutcome::Suspended { .. }) => {
            return Err("prelude evaluation ran out of fuel".to_string());
        }
        Err(e) => {
            return Err(format!("prelude eval error: {}", e));
        }
    };

    // The prelude returns an alist of (name . value) pairs
    // Insert each binding into the environment
    let mut current = &result;
    loop {
        match current {
            Value::Nil => break,
            Value::Pair(head, tail) => {
                if let Value::Pair(name_box, value_box) = &**head
                    && let Value::Symbol(name) = &**name_box
                {
                    Env::insert(&env, name.clone(), (**value_box).clone());
                }
                current = tail;
            }
            _ => break,
        }
    }

    Ok(env)
}

/// Create a new environment with the prelude loaded.
pub fn prelude_env(fuel: usize) -> Result<EnvRef, String> {
    let env = primitives_env();
    load_prelude(env, fuel)
}

/// All primitive operator names that should be bound as first-class values.
/// These can be passed directly to higher-order functions like foldl.
const PRIMITIVE_NAMES: &[&str] = &[
    // Arithmetic
    "+",
    "-",
    "*",
    "/",
    "add",
    "sub",
    "mul",
    "div",
    "mod",
    "quotient",
    "remainder",
    "abs",
    "neg",
    "sqrt",
    "expt",
    "pow",
    "floor",
    "ceiling",
    "round",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "log",
    "exp",
    "min",
    "max",
    "gcd",
    "lcm",
    "clamp",
    "between?",
    "sign",
    "divmod",
    "int->float",
    "float->int",
    "truncate",
    "inc",
    "dec",
    "succ",
    "pred",
    "sq",
    "cube",
    "double",
    "halve",
    "negate",
    "zero",
    "one",
    "scale",
    "denormalize",
    "lerp",
    "inverse-lerp",
    "smoothstep",
    "saturation-add",
    "percent-of",
    "percent-change",
    "round-to",
    // Rational operations
    "make-rational",
    "rational",
    "rational?",
    "rational-zero?",
    "rational-positive?",
    "rational-negative?",
    "rational-integer?",
    "rational-numerator",
    "rational-denominator",
    "rational-add",
    "rational-sub",
    "rational-mul",
    "rational-div",
    "rational-abs",
    "rational-neg",
    "rational-recip",
    "rational-floor",
    "rational-ceiling",
    "rational-round",
    "rational-truncate",
    "rational->float",
    "rational<?",
    "rational<=?",
    "rational=?",
    "rational>=?",
    "rational>?",
    // Comparison
    "<",
    ">",
    "<=",
    ">=",
    "=",
    "eq?",
    "lt?",
    "gt?",
    "le?",
    "ge?",
    // Type checks and predicates
    "number?",
    "integer?",
    "string?",
    "list?",
    "pair?",
    "null?",
    "boolean?",
    "symbol?",
    "procedure?",
    "char?",
    "vector?",
    "bytevector?",
    "zero?",
    "positive?",
    "negative?",
    "even?",
    "odd?",
    // Bitwise operations
    "bitand",
    "bitor",
    "bitxor",
    "bitnot",
    "shl",
    "shr",
    "atom?",
    "atomic?",
    "not-null?",
    "proper-list?",
    "pair-list?",
    // Symbol operations
    "symbol-upcase",
    "symbol-downcase",
    "symbol-length",
    "symbol-concat",
    // Bytevector operations
    "bytevector-length",
    "bytevector-ref",
    "bytevector->string",
    "string->bytevector",
    // Association list operations
    "alist-get",
    "alist-set",
    "alist-keys",
    "alist-values",
    // List operations
    "car",
    "cdr",
    "cons",
    "set-car!",
    "set-cdr!",
    "list",
    "append",
    "length",
    "reverse",
    "member",
    "memq",
    "list-ref",
    "list->string",
    "list->vec",
    "take",
    "drop",
    "last",
    "init",
    "flatten",
    "range",
    "list-copy",
    "make-list",
    "first",
    "rest",
    "second",
    "third",
    "head",
    "tail",
    "repeat",
    "sum",
    "product",
    "sum-of",
    "max-of",
    "min-of",
    "sort",
    "unique",
    "find-index",
    "index-of",
    "count",
    "find",
    "position",
    "contains?",
    "min-list",
    "max-list",
    "nth",
    "zip",
    "unzip",
    "rotate",
    "list->set",
    "interleave",
    "list->pairs",
    "list->symbols",
    "list->numbers",
    "list-indices",
    "list-reverse-indices",
    "list-length-of",
    "list-split",
    "list-repeat-element",
    "list-cycle",
    "list-intersperse",
    "list-map-indexed",
    "list-enumerate",
    // String operations
    "string-append",
    "string-length",
    "string-ref",
    "substring",
    "make-string",
    "string->list",
    "string->number",
    "string->symbol",
    "string->utf8",
    "string=?",
    "string<?",
    "string>?",
    "string-upcase",
    "string-downcase",
    "upcase",
    "upcase-string",
    "downcase",
    "downcase-string",
    "string-split",
    "string-trim",
    "string-ltrim",
    "string-rtrim",
    "string-pad",
    "string-starts-with?",
    "string-ends-with?",
    "string-contains?",
    "string-reverse",
    "string-repeat",
    "string-replace",
    "string-index",
    "string-rindex",
    "pad-left",
    "pad-right",
    "string-center",
    "char-code",
    "code-char",
    "chars",
    "string-concat",
    "string-take",
    "string-drop",
    "starts-with?",
    "ends-with?",
    "substring?",
    "string-words",
    "string-lines",
    "string-join",
    "string-replace-all",
    "string-trim-start",
    "string-trim-end",
    "string-trim-both",
    "string-ascii?",
    "string-numeric?",
    "string-alphabetic?",
    "string-alphanumeric?",
    "string-whitespace?",
    "string-empty?",
    "format",
    "number->string",
    "symbol->string",
    // Vector operations
    "vec-length",
    "vec-ref",
    "vec-make",
    "vec->list",
    "vec-copy",
    "vec-slice",
    "vec-reverse",
    "vec-sort",
    "vec-contains?",
    "vec-index-of",
    // Type introspection
    "type-of",
    "is-number?",
    "is-string?",
    "is-vector?",
    "is-list?",
    // Value comparison
    "equals?",
    "not-equals?",
    "identical?",
    "hash-value",
    // Block operations
    "make-block",
    "block",
    "block?",
    "block-tag",
    "block-ref",
    "block->bytes",
    "block-payload",
    "block-refs",
    // Hash/CAS
    "hash-block",
    "sha256",
    "fetch",
    "store!",
    "pin!",
    "pinned?",
    // Byte operations
    "bv-make",
    "bv-length",
    "bv-ref",
    "bv-slice",
    "bv-concat",
    "bv-copy",
    "bytes->block",
    // Character operations
    "integer->char",
    "char->integer",
    "char=?",
    "char<?",
    // Logic
    "and",
    "or",
    "not",
    // Logical operations
    "xor",
    "gensym",
    // Conditional and logical utilities
    "when",
    "unless",
    "implies",
    "iff",
    "nand",
    "nor",
    "if",
    "cond-value",
    "coalesce",
    "default-value",
    "both?",
    "either?",
    // Utilities
    "identity",
    "apply",
    "error",
    "deep-equal?",
    "list-every?",
    "assq",
    "assoc",
    // List analysis utilities
    "list-count",
    "list-sum",
    "list-avg",
    "list-min-value",
    "list-max-value",
    // String and value conversion
    "string-byte-length",
    "string-char-length",
    "value->string",
    "string->value?",
    "constantly",
    // REPL commands
    "version",
    "who",
    "help",
    "display",
    "write",
    "newline",
];

/// Create an environment with all primitives bound as first-class values.
/// This allows primitives like + to be passed directly to higher-order functions.
pub fn primitives_env() -> EnvRef {
    let env = Env::new();
    for &name in PRIMITIVE_NAMES {
        Env::insert(&env, name.to_string(), Value::Primitive(name.to_string()));
    }
    env
}
