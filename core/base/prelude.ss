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

;;; ============================================================
;;; List Predicates
;;; ============================================================

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

;;; ============================================================
;;; List Utilities
;;; ============================================================

;;; unique : (List α) → (List α)
;;; Remove duplicates, preserving first occurrence order.
;;; Uses eq? for comparison.
(define (unique lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(memq (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

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

;;; ============================================================
;;; Collection Utilities (Missing Functional Primitives)
;;; ============================================================

;;; identity : α → α
;;; The identity function - returns its argument unchanged.
(define (identity x) x)

;;; flatten : (List (List α)) → (List α)
;;; Flatten a list of lists into a single list.
(define (flatten lst-of-lists)
  (fold-right append '() lst-of-lists))

;;; partition : (α → Bool) × (List α) → (List (List α) (List α))
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

;;; ============================================================
;;; Result Type (Standardized Error Handling)
;;; ============================================================

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

;;; unwrap-ok : Result → Value
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

;;; ============================================================
;;; String Utilities
;;; ============================================================

;;; string-join : (List String) × String → String
;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; string-split : String × Char → (List String)
;;; Split string by single-character delimiter.
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

;;; whitespace? : Char → Boolean
;;; Check if character is whitespace.
(define (whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch (integer->char 9))
      (char=? ch (integer->char 10))
      (char=? ch (integer->char 13))))

;;; string-trim-left : String → String
;;; Remove leading whitespace.
(define (string-trim-left str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) ""]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else (substring str i len)]))))

;;; string-trim-right : String → String
;;; Remove trailing whitespace.
(define (string-trim-right str)
  (let ([len (string-length str)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) ""]
             [(whitespace? (string-ref str i)) (loop (- i 1))]
             [else (substring str 0 (+ i 1))]))))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;;; string-contains? : String × String → Boolean
;;; Check if haystack contains needle as substring.
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) #t]
        [(> n-len h-len) #f]
        [else
         (let loop ([i 0])
              (cond
               [(> (+ i n-len) h-len) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) #t]
               [else (loop (+ i 1))]))])))

;;; string-starts-with? : String × String → Boolean
;;; Check if string starts with prefix.
(define (string-starts-with? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (<= pre-len str-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; string-ends-with? : String × String → Boolean
;;; Check if string ends with suffix.
(define (string-ends-with? str suffix)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (<= suf-len str-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

;;; string-index-of : String × String → (Maybe Integer)
;;; Find first occurrence of needle in haystack.
;;; Returns index or #f if not found.
(define (string-index-of haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) 0]
        [(> n-len h-len) #f]
        [else
         (let loop ([i 0])
              (cond
               [(> (+ i n-len) h-len) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) i]
               [else (loop (+ i 1))]))])))

;;; string-last-index-of : String × String → (Maybe Integer)
;;; Find last occurrence of needle in haystack.
;;; Returns index or #f if not found.
(define (string-last-index-of haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) h-len]
        [(> n-len h-len) #f]
        [else
         (let loop ([i (- h-len n-len)])
              (cond
               [(< i 0) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) i]
               [else (loop (- i 1))]))])))

;;; string-replace : String × String × String → String
;;; Replace all occurrences of old with new.
(define (string-replace str old new)
  (let ([old-len (string-length old)])
       (cond
        [(= old-len 0) str]
        [else
         (let loop ([start 0]
                    [result ""])
              (let ([idx (string-index-of (substring str start (string-length str)) old)])
                   (if idx
                       (let ([pos (+ start idx)])
                            (loop (+ pos old-len)
                                  (string-append result
                                                 (substring str start pos)
                                                 new)))
                       (string-append result (substring str start (string-length str))))))])))

;;; string-reverse : String → String
;;; Reverse a string.
(define (string-reverse str)
  (list->string (reverse (string->list str))))

;;; string-empty? : String → Boolean
;;; Check if string is empty.
(define (string-empty? str)
  (= (string-length str) 0))

;;; string-blank? : String → Boolean
;;; Check if string is empty or contains only whitespace.
(define (string-blank? str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

;;; string-all-match? : String × (Char → Boolean) → Boolean
;;; Check if all characters in string satisfy predicate.
(define (string-all-match? str predicate)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(predicate (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

;;; string-upcase : String → String
;;; Convert string to uppercase.
(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

;;; string-downcase : String → String
;;; Convert string to lowercase.
(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;;; string-pad-left : String × Integer × Char → String
;;; Pad string on the left to reach target width.
(define (string-pad-left str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append (make-string (- width len) pad-char) str))))

;;; string-pad-right : String × Integer × Char → String
;;; Pad string on the right to reach target width.
(define (string-pad-right str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append str (make-string (- width len) pad-char)))))

;;; edit-distance : String × String → Nat
;;; Compute the Levenshtein distance between two strings.
(define (edit-distance s1 s2)
  (let ([len1 (string-length s1)]
        [len2 (string-length s2)])
       (cond
        [(= len1 0) len2]
        [(= len2 0) len1]
        [else
         (let ([matrix (make-vector (* (+ len1 1) (+ len2 1)) 0)])
              ;; Initialize first row and column
              (do ([i 0 (+ i 1)]) ((> i len1))
                  (vector-set! matrix (* i (+ len2 1)) i))
              (do ([j 0 (+ j 1)]) ((> j len2))
                  (vector-set! matrix j j))
              ;; Fill matrix
              (do ([i 1 (+ i 1)])
                  ((> i len1))
                  (do ([j 1 (+ j 1)])
                      ((> j len2))
                      (let* ([cost (if (char=? (string-ref s1 (- i 1))
                                               (string-ref s2 (- j 1)))
                                       0 1)]
                             [del (+ 1 (vector-ref matrix (+ (* (- i 1) (+ len2 1)) j)))]
                             [ins (+ 1 (vector-ref matrix (+ (* i (+ len2 1)) (- j 1))))]
                             [sub (+ cost (vector-ref matrix (+ (* (- i 1) (+ len2 1)) (- j 1))))])
                            (vector-set! matrix (+ (* i (+ len2 1)) j) (min del ins sub)))))
              ;; Return final distance
              (vector-ref matrix (+ (* len1 (+ len2 1)) len2)))])))

;;; ============================================================
;;; Debug Utilities
;;; ============================================================

;;; trace : String × α → α
;;; Print a debug message and return the value unchanged.
;;; Useful for debugging without changing code structure.
(define (trace msg val)
  (display msg)
  (display ": ")
  (write val)
  (newline)
  val)

;;; ============================================================
;;; Unicode Aliases — Mathematical Notation
;;; ============================================================
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

;;; ¬ : Logical negation (not)
(define ¬ not)

;;; --- Comparison ---

;;; ≠ : Not equal
(define (≠ a b) (not (equal? a b)))

;;; ≤ : Less than or equal
(define ≤ <=)

;;; ≥ : Greater than or equal
(define ≥ >=)

;;; --- Arithmetic ---

;;; × : Multiplication (alternative to *)
(define × *)

;;; ÷ : Division (alternative to /)
(define ÷ /)

;;; ² : Square function
(define (² x) (* x x))

;;; ³ : Cube function
(define (³ x) (* x x x))

;;; √ : Square root
(define √ sqrt)

;;; --- Collections ---

;;; ∈ : Membership test (member, returns boolean)
(define (∈ x lst) (if (member x lst) #t #f))

;;; ∉ : Non-membership test
(define (∉ x lst) (not (∈ x lst)))

;;; ∅ : Empty list
(define ∅ '())

;;; --- Function Composition ---

;;; ∘ : Function composition (f ∘ g)(x) = f(g(x))
(define (∘ f g) (λ (x) (f (g x))))

;;; --- Constants ---

;;; π : Pi (3.14159...)
(define π 3.141592653589793)

;;; τ : Tau (2π)
(define τ 6.283185307179586)

;;; 𝑒 : Euler's number
(define 𝑒 2.718281828459045)

;;; ∞ : Infinity (largest flonum)
(define ∞ +inf.0)

;;; -∞ : Negative infinity
(define -∞ -inf.0)
