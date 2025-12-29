;;; fabric/stitches/fp/regex.ss — Regular Expressions
;;;
;;; A pure functional regular expression library using derivatives.
;;; Based on Brzozowski's derivative algorithm for elegant, principled matching.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Core regex constructors (empty, epsilon, char, seq, alt, star)
;;;   - Extended patterns (plus, opt, range, any, dot)
;;;   - Character classes (digit, alpha, word, space, etc.)
;;;   - Matching and searching
;;;   - Regex optimization (simplification)
;;;   - Kleene algebra laws
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Regex Type
;;; ============================================================
;;;
;;; A regex is one of:
;;;   - (rx-empty)        : matches nothing
;;;   - (rx-epsilon)      : matches empty string
;;;   - (rx-char c)       : matches single character c
;;;   - (rx-char-class p) : matches char satisfying predicate p
;;;   - (rx-seq r1 r2)    : matches r1 followed by r2
;;;   - (rx-alt r1 r2)    : matches r1 or r2
;;;   - (rx-star r)       : matches r zero or more times

;;; rx-empty : Regex
;;; The empty language (matches nothing).
(define rx-empty (list 'rx-empty))

;;; rx-empty? : Any -> Boolean
(define (rx-empty? r)
  (and (pair? r) (eq? (car r) 'rx-empty)))

;;; rx-epsilon : Regex
;;; Matches the empty string only.
(define rx-epsilon (list 'rx-epsilon))

;;; rx-epsilon? : Any -> Boolean
(define (rx-epsilon? r)
  (and (pair? r) (eq? (car r) 'rx-epsilon)))

;;; rx-char : Char -> Regex
;;; Match a specific character.
(define (rx-char c)
  (list 'rx-char c))

;;; rx-char? : Any -> Boolean
(define (rx-char? r)
  (and (pair? r) (eq? (car r) 'rx-char)))

;;; rx-char-value : Regex -> Char
(define (rx-char-value r)
  (list-ref r 1))

;;; rx-char-class : (Char -> Boolean) -> String -> Regex
;;; Match character satisfying predicate.
;;; description is for display purposes.
(define (rx-char-class pred description)
  (list 'rx-char-class pred description))

;;; rx-char-class? : Any -> Boolean
(define (rx-char-class? r)
  (and (pair? r) (eq? (car r) 'rx-char-class)))

;;; rx-char-class-pred : Regex -> (Char -> Boolean)
(define (rx-char-class-pred r)
  (list-ref r 1))

;;; rx-char-class-desc : Regex -> String
(define (rx-char-class-desc r)
  (list-ref r 2))

;;; rx-seq : Regex -> Regex -> Regex
;;; Sequence: r1 followed by r2.
(define (rx-seq r1 r2)
  (cond
   ;; Simplifications
   [(rx-empty? r1) rx-empty]
   [(rx-empty? r2) rx-empty]
   [(rx-epsilon? r1) r2]
   [(rx-epsilon? r2) r1]
   [else (list 'rx-seq r1 r2)]))

;;; rx-seq? : Any -> Boolean
(define (rx-seq? r)
  (and (pair? r) (eq? (car r) 'rx-seq)))

;;; rx-seq-first : Regex -> Regex
(define (rx-seq-first r)
  (list-ref r 1))

;;; rx-seq-second : Regex -> Regex
(define (rx-seq-second r)
  (list-ref r 2))

;;; rx-alt : Regex -> Regex -> Regex
;;; Alternation: r1 or r2.
(define (rx-alt r1 r2)
  (cond
   ;; Simplifications
   [(rx-empty? r1) r2]
   [(rx-empty? r2) r1]
   [(equal? r1 r2) r1]
   [else (list 'rx-alt r1 r2)]))

;;; rx-alt? : Any -> Boolean
(define (rx-alt? r)
  (and (pair? r) (eq? (car r) 'rx-alt)))

;;; rx-alt-left : Regex -> Regex
(define (rx-alt-left r)
  (list-ref r 1))

;;; rx-alt-right : Regex -> Regex
(define (rx-alt-right r)
  (list-ref r 2))

;;; rx-star : Regex -> Regex
;;; Kleene star: zero or more repetitions.
(define (rx-star r)
  (cond
   ;; Simplifications
   [(rx-empty? r) rx-epsilon]
   [(rx-epsilon? r) rx-epsilon]
   [(and (rx-star? r)) r]  ; (r*)* = r*
   [else (list 'rx-star r)]))

;;; rx-star? : Any -> Boolean
(define (rx-star? r)
  (and (pair? r) (eq? (car r) 'rx-star)))

;;; rx-star-inner : Regex -> Regex
(define (rx-star-inner r)
  (list-ref r 1))

;;; ============================================================
;;; Extended Constructors
;;; ============================================================

;;; rx-plus : Regex -> Regex
;;; One or more repetitions: r+ = r r*
(define (rx-plus r)
  (rx-seq r (rx-star r)))

;;; rx-opt : Regex -> Regex
;;; Optional: r? = r | epsilon
(define (rx-opt r)
  (rx-alt r rx-epsilon))

;;; rx-count : Int -> Regex -> Regex
;;; Exactly n repetitions.
(define (rx-count n r)
  (if (<= n 0)
      rx-epsilon
      (rx-seq r (rx-count (- n 1) r))))

;;; rx-range : Int -> Int -> Regex -> Regex
;;; Between min and max repetitions.
(define (rx-range min max r)
  (cond
   [(<= max 0) rx-epsilon]
   [(<= min 0) (rx-opt (rx-range 1 max r))]
   [(= min max) (rx-count min r)]
   [else (rx-seq (rx-count min r)
                 (rx-range 0 (- max min) r))]))

;;; rx-string : String -> Regex
;;; Match an exact string.
(define (rx-string s)
  (if (string=? s "")
      rx-epsilon
      (let ([chars (string->list s)])
           (fold-right (lambda (c acc) (rx-seq (rx-char c) acc))
                       rx-epsilon
                       chars))))

;;; rx-one-of : String -> Regex
;;; Match any single character from string.
(define (rx-one-of chars)
  (rx-char-class
   (lambda (c)
           (let loop ([i 0])
                (if (>= i (string-length chars))
                    #f
                    (or (char=? c (string-ref chars i))
                        (loop (+ i 1))))))
   (string-append "[" chars "]")))

;;; rx-none-of : String -> Regex
;;; Match any character NOT in string.
(define (rx-none-of chars)
  (rx-char-class
   (lambda (c)
           (let loop ([i 0])
                (if (>= i (string-length chars))
                    #t
                    (and (not (char=? c (string-ref chars i)))
                         (loop (+ i 1))))))
   (string-append "[^" chars "]")))

;;; rx-char-range : Char -> Char -> Regex
;;; Match character in range [lo, hi].
(define (rx-char-range lo hi)
  (rx-char-class
   (lambda (c)
           (and (char>=? c lo) (char<=? c hi)))
   (string-append "[" (string lo) "-" (string hi) "]")))

;;; rx-any-of : List Regex -> Regex
;;; Alternation of multiple regexes.
(define (rx-any-of rs)
  (if (null? rs)
      rx-empty
      (fold-right rx-alt rx-empty rs)))

;;; rx-seq-of : List Regex -> Regex
;;; Sequence of multiple regexes.
(define (rx-seq-of rs)
  (if (null? rs)
      rx-epsilon
      (fold-right rx-seq rx-epsilon rs)))

;;; ============================================================
;;; Standard Character Classes
;;; ============================================================

;;; rx-dot : Regex
;;; Match any single character.
(define rx-dot
  (rx-char-class (lambda (c) #t) "."))

;;; rx-digit : Regex
;;; Match digit [0-9].
(define rx-digit
  (rx-char-class char-numeric? "\d"))

;;; rx-non-digit : Regex
;;; Match non-digit.
(define rx-non-digit
  (rx-char-class (lambda (c) (not (char-numeric? c))) "\D"))

;;; rx-alpha : Regex
;;; Match alphabetic character.
(define rx-alpha
  (rx-char-class char-alphabetic? "[a-zA-Z]"))

;;; rx-alnum : Regex
;;; Match alphanumeric.
(define rx-alnum
  (rx-char-class
   (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
   "[a-zA-Z0-9]"))

;;; rx-word : Regex
;;; Match word character [a-zA-Z0-9_].
(define rx-word
  (rx-char-class
   (lambda (c) (or (char-alphabetic? c)
                   (char-numeric? c)
                   (char=? c #\_)))
   "\w"))

;;; rx-non-word : Regex
;;; Match non-word character.
(define rx-non-word
  (rx-char-class
   (lambda (c) (not (or (char-alphabetic? c)
                        (char-numeric? c)
                        (char=? c #\_))))
   "\W"))

;;; rx-space : Regex
;;; Match whitespace.
(define rx-space
  (rx-char-class char-whitespace? "\s"))

;;; rx-non-space : Regex
;;; Match non-whitespace.
(define rx-non-space
  (rx-char-class (lambda (c) (not (char-whitespace? c))) "\S"))

;;; rx-lower : Regex
;;; Match lowercase letter.
(define rx-lower
  (rx-char-class char-lower-case? "[a-z]"))

;;; rx-upper : Regex
;;; Match uppercase letter.
(define rx-upper
  (rx-char-class char-upper-case? "[A-Z]"))

;;; ============================================================
;;; Nullability
;;; ============================================================
;;;
;;; A regex is nullable if it matches the empty string.

;;; nullable? : Regex -> Boolean
(define (nullable? r)
  (cond
   [(rx-empty? r) #f]
   [(rx-epsilon? r) #t]
   [(rx-char? r) #f]
   [(rx-char-class? r) #f]
   [(rx-seq? r) (and (nullable? (rx-seq-first r))
                     (nullable? (rx-seq-second r)))]
   [(rx-alt? r) (or (nullable? (rx-alt-left r))
                    (nullable? (rx-alt-right r)))]
   [(rx-star? r) #t]
   [else #f]))

;;; ============================================================
;;; Derivative
;;; ============================================================
;;;
;;; The derivative of a regex r with respect to character c
;;; is a regex that matches the remainder after consuming c.

;;; derivative : Regex -> Char -> Regex
(define (derivative r c)
  (cond
   [(rx-empty? r) rx-empty]
   [(rx-epsilon? r) rx-empty]
   [(rx-char? r)
    (if (char=? (rx-char-value r) c)
        rx-epsilon
        rx-empty)]
   [(rx-char-class? r)
    (if ((rx-char-class-pred r) c)
        rx-epsilon
        rx-empty)]
   [(rx-seq? r)
    (let ([r1 (rx-seq-first r)]
          [r2 (rx-seq-second r)])
         (if (nullable? r1)
             (rx-alt (rx-seq (derivative r1 c) r2)
                     (derivative r2 c))
             (rx-seq (derivative r1 c) r2)))]
   [(rx-alt? r)
    (rx-alt (derivative (rx-alt-left r) c)
            (derivative (rx-alt-right r) c))]
   [(rx-star? r)
    (rx-seq (derivative (rx-star-inner r) c) r)]
   [else rx-empty]))

;;; ============================================================
;;; Matching
;;; ============================================================

;;; rx-match? : Regex -> String -> Boolean
;;; Test if regex matches entire string.
(define (rx-match? r s)
  (let loop ([r r] [i 0])
       (if (>= i (string-length s))
           (nullable? r)
           (loop (derivative r (string-ref s i)) (+ i 1)))))

;;; rx-match-list? : Regex -> List Char -> Boolean
;;; Test if regex matches list of characters.
(define (rx-match-list? r chars)
  (if (null? chars)
      (nullable? r)
      (rx-match-list? (derivative r (car chars)) (cdr chars))))

;;; rx-prefix? : Regex -> String -> Boolean
;;; Test if regex matches a prefix of the string.
(define (rx-prefix? r s)
  (let loop ([r r] [i 0])
       (cond
        [(nullable? r) #t]
        [(>= i (string-length s)) #f]
        [else (loop (derivative r (string-ref s i)) (+ i 1))])))

;;; rx-find-first : Regex -> String -> Maybe (Int . Int)
;;; Find first (longest) match position (start . end).
(define (rx-find-first r s)
  (let outer ([start 0])
       (if (>= start (string-length s))
           (if (nullable? r) (just (cons start start)) nothing)
           ;; Try to find longest match starting at 'start'
           (let inner ([r r] [end start] [last-match (if (nullable? r) end #f)])
                (cond
                 [(>= end (string-length s))
                  ;; End of string - return last match if any
                  (if last-match
                      (just (cons start last-match))
                      (outer (+ start 1)))]
                 [else
                  (let* ([next (derivative r (string-ref s end))]
                         [new-last (if (nullable? next) (+ end 1) last-match)])
                        (if (rx-empty? next)
                            ;; Can't continue - return last match or try next start
                            (if last-match
                                (just (cons start last-match))
                                (outer (+ start 1)))
                            (inner next (+ end 1) new-last)))])))))

;;; rx-find-all : Regex -> String -> List (Int . Int)
;;; Find all non-overlapping matches.
(define (rx-find-all r s)
  (let loop ([pos 0] [acc '()])
       (if (>= pos (string-length s))
           (reverse acc)
           (let ([result (rx-find-first r (substring s pos (string-length s)))])
                (if (nothing? result)
                    (reverse acc)
                    (let* ([match (from-just result)]
                           [start (+ pos (car match))]
                           [end (+ pos (cdr match))])
                          (if (= start end)
                              ;; Empty match, advance by 1 to avoid infinite loop
                              (loop (+ pos 1) (cons (cons start end) acc))
                              (loop end (cons (cons start end) acc)))))))))

;;; rx-extract : Regex -> String -> Maybe String
;;; Extract the first match as a substring.
(define (rx-extract r s)
  (let ([result (rx-find-first r s)])
       (if (nothing? result)
           nothing
           (let ([match (from-just result)])
                (just (substring s (car match) (cdr match)))))))

;;; rx-extract-all : Regex -> String -> List String
;;; Extract all matches as substrings.
(define (rx-extract-all r s)
  (map (lambda (match)
               (substring s (car match) (cdr match)))
       (rx-find-all r s)))

;;; ============================================================
;;; String Operations
;;; ============================================================

;;; rx-split : Regex -> String -> List String
;;; Split string by regex matches.
(define (rx-split r s)
  (let ([matches (rx-find-all r s)])
       (if (null? matches)
           (list s)
           (let loop ([pos 0] [ms matches] [acc '()])
                (if (null? ms)
                    (reverse (cons (substring s pos (string-length s)) acc))
                    (let* ([match (car ms)]
                           [start (car match)]
                           [end (cdr match)])
                          (loop end (cdr ms)
                                (cons (substring s pos start) acc))))))))

;;; rx-replace : Regex -> String -> String -> String
;;; Replace first match with replacement.
(define (rx-replace r replacement s)
  (let ([result (rx-find-first r s)])
       (if (nothing? result)
           s
           (let* ([match (from-just result)]
                  [start (car match)]
                  [end (cdr match)])
                 (string-append (substring s 0 start)
                                replacement
                                (substring s end (string-length s)))))))

;;; rx-replace-all : Regex -> String -> String -> String
;;; Replace all matches with replacement.
(define (rx-replace-all r replacement s)
  (let ([matches (rx-find-all r s)])
       (if (null? matches)
           s
           (let loop ([pos 0] [ms matches] [acc ""])
                (if (null? ms)
                    (string-append acc (substring s pos (string-length s)))
                    (let* ([match (car ms)]
                           [start (car match)]
                           [end (cdr match)])
                          (loop end (cdr ms)
                                (string-append acc
                                               (substring s pos start)
                                               replacement))))))))

;;; ============================================================
;;; Regex Display
;;; ============================================================

;;; rx->string : Regex -> String
;;; Convert regex to string representation.
(define (rx->string r)
  (cond
   [(rx-empty? r) "∅"]
   [(rx-epsilon? r) "ε"]
   [(rx-char? r) (string (rx-char-value r))]
   [(rx-char-class? r) (rx-char-class-desc r)]
   [(rx-seq? r)
    (string-append "(" (rx->string (rx-seq-first r))
                   (rx->string (rx-seq-second r)) ")")]
   [(rx-alt? r)
    (string-append "(" (rx->string (rx-alt-left r))
                   "|" (rx->string (rx-alt-right r)) ")")]
   [(rx-star? r)
    (string-append "(" (rx->string (rx-star-inner r)) ")*")]
   [else "?"]))

;;; ============================================================
;;; Common Patterns
;;; ============================================================

;;; rx-identifier : Regex
;;; Match identifier: letter followed by word chars.
(define rx-identifier
  (rx-seq rx-alpha (rx-star rx-word)))

;;; rx-integer : Regex
;;; Match integer: optional sign followed by digits.
(define rx-integer
  (rx-seq (rx-opt (rx-one-of "+-"))
          (rx-plus rx-digit)))

;;; rx-decimal : Regex
;;; Match decimal number.
(define rx-decimal
  (rx-seq rx-integer
          (rx-opt (rx-seq (rx-char #\.)
                          (rx-plus rx-digit)))))

;;; rx-email-simple : Regex
;;; Simple email pattern.
(define rx-email-simple
  (rx-seq (rx-plus rx-word)
          (rx-seq (rx-char #\@)
                  (rx-seq (rx-plus rx-word)
                          (rx-seq (rx-char #\.)
                                  (rx-plus rx-alpha))))))

;;; rx-whitespace : Regex
;;; Match one or more whitespace.
(define rx-whitespace (rx-plus rx-space))

;;; ============================================================
;;; Optimization
;;; ============================================================

;;; rx-simplify : Regex -> Regex
;;; Simplify regex by applying algebraic identities.
(define (rx-simplify r)
  (cond
   [(rx-empty? r) rx-empty]
   [(rx-epsilon? r) rx-epsilon]
   [(rx-char? r) r]
   [(rx-char-class? r) r]
   [(rx-seq? r)
    (let ([r1 (rx-simplify (rx-seq-first r))]
          [r2 (rx-simplify (rx-seq-second r))])
         (rx-seq r1 r2))]  ; rx-seq already simplifies
   [(rx-alt? r)
    (let ([r1 (rx-simplify (rx-alt-left r))]
          [r2 (rx-simplify (rx-alt-right r))])
         (rx-alt r1 r2))]  ; rx-alt already simplifies
   [(rx-star? r)
    (let ([inner (rx-simplify (rx-star-inner r))])
         (rx-star inner))]  ; rx-star already simplifies
   [else r]))

;;; ============================================================
;;; Kleene Algebra Properties
;;; ============================================================
;;;
;;; These can be used for testing/verification.

;;; rx-equiv? : Regex -> Regex -> (String -> Boolean)
;;; Test if two regexes match the same language (up to given strings).
(define (rx-equiv? r1 r2)
  (lambda (s) (eq? (rx-match? r1 s) (rx-match? r2 s))))
