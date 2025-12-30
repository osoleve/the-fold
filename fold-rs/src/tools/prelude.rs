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
           (cons '() '())
           (let ((rest-result (partition p (cdr lst)))
                 (x (car lst)))
             (if (p x)
                 (cons (cons x (car rest-result)) (cdr rest-result))
                 (cons (car rest-result) (cons x (cdr rest-result)))))))))

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

   ; either: Combine two predicates with or
   ; ((either positive? even?) -2) => #t
   (either (fn (p1 p2) (fn (x) (or (p1 x) (p2 x)))))

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
    (cons 'either either)
    (cons 'neither neither)
    (cons 'frequencies frequencies)
    (cons 'index-where index-where)
    (cons 'indices-where indices-where)
    (cons 'last-where last-where)
    (cons 'update-at update-at)
    (cons 'insert-at insert-at)
    (cons 'remove-at remove-at)))
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
