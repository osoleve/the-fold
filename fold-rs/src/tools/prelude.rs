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

   ; matrix-map: Map function over all elements
   (matrix-map (fn (f m)
     (map (fn (row) (map f row)) m)))

   ; matrix-add: Add two matrices
   (matrix-add (fn (a b)
     (zip-with (fn (row-a row-b) (zip-with + row-a row-b)) a b)))

   ; matrix-scale: Multiply matrix by scalar
   (matrix-scale (fn (k m)
     (matrix-map (fn (x) (* k x)) m)))

   ; matrix-transpose: Transpose matrix (uses existing transpose)
   (matrix-transpose transpose)

   ; dot-product: Dot product of two vectors (lists)
   (dot-product (fn (a b)
     (sum-list (zip-with * a b))))

   ; matrix-multiply: Multiply two matrices
   (matrix-multiply (fn (a b)
     (let ((b-t (transpose b)))
       (map (fn (row-a)
              (map (fn (col-b) (dot-product row-a col-b)) b-t))
            a))))

   ; identity-matrix: Create n×n identity matrix
   (identity-matrix (fn (n)
     (map (fn (i)
            (map (fn (j) (if (= i j) 1 0))
                 (iota n 0)))
          (iota n 0))))

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
    (cons 'format-ordinal format-ordinal)))
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
