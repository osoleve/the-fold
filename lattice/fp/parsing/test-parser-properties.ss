;;; lattice/fp/parsing/test-parser-properties.ss — QuickCheck Property Tests for Parser Combinators
;;; @requires prelude parser quickcheck

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'parser)

;;; =================================================================
;;; Generators
;;; =================================================================

(define gen-char-lowercase
  (gen-map (lambda (n) (integer->char (+ 97 n)))
           (gen-int-range 0 25)))

(define gen-char-uppercase
  (gen-map (lambda (n) (integer->char (+ 65 n)))
           (gen-int-range 0 25)))

(define gen-char-digit
  (gen-map (lambda (n) (integer->char (+ 48 n)))
           (gen-int-range 0 9)))

(define gen-char-alpha
  (gen-one-of (list gen-char-lowercase gen-char-uppercase)))

(define gen-char-alnum
  (gen-one-of (list gen-char-alpha gen-char-digit)))

(define gen-string-small
  (gen-map list->string
           (gen-list gen-char-lowercase)))

(define gen-string-non-empty
  (gen-map list->string
           (gen-bind (gen-int-range 1 10)
             (lambda (n)
               (gen-list-of n gen-char-lowercase)))))

(define gen-int-small
  (gen-int-range -100 100))

;;; =================================================================
;;; Parser Properties
;;; =================================================================

(test-group parser-properties

  ;; === Monad Laws ===
  
  ;; Left identity: return a >>= f === f a
  (define-property "parser left identity law"
    (gen-bind gen-char-lowercase
      (lambda (c)
        (gen-map (lambda (s) (cons c s)) gen-string-small)))
    (lambda (cs)
      (let* ([c (car cs)]
             [s (cdr cs)]
             [f (lambda (x) (parser-pure (char-upcase x)))]
             [lhs (parser-parse (parser-bind (parser-pure c) f) s)]
             [rhs (parser-parse (f c) s)])
        (and (right? lhs) (right? rhs)
             (equal? (from-right lhs) (from-right rhs)))))
    'tests 100)

  ;; Right identity: m >>= return === m
  (define-property "parser right identity law"
    gen-string-non-empty
    (lambda (s)
      (let* ([m (parser-char (string-ref s 0))]
             [lhs (parser-parse (parser-bind m parser-pure) s)]
             [rhs (parser-parse m s)])
        (equal? (right? lhs) (right? rhs))))
    'tests 100)

  ;; Associativity: (m >>= f) >>= g === m >>= (\x -> f x >>= g)
  (define-property "parser associativity law"
    gen-string-non-empty
    (lambda (s)
      (let* ([m (parser-char (string-ref s 0))]
             [f (lambda (x) (parser-pure (char-upcase x)))]
             [g (lambda (x) (parser-pure (char-downcase x)))]
             [lhs (parser-parse (parser-bind (parser-bind m f) g) s)]
             [rhs (parser-parse (parser-bind m (lambda (x) (parser-bind (f x) g))) s)])
        (and (right? lhs) (right? rhs)
             (equal? (from-right lhs) (from-right rhs)))))
    'tests 100)

  ;; === Functor Laws ===
  
  ;; Identity: fmap id === id
  (define-property "parser fmap identity law"
    gen-string-non-empty
    (lambda (s)
      (let* ([p (parser-char (string-ref s 0))]
             [result (parser-parse (parser-map id p) s)])
        (right? result)))
    'tests 100)

  ;; Composition: fmap (f . g) === fmap f . fmap g
  (define-property "parser fmap composition law"
    gen-string-non-empty
    (lambda (s)
      (let* ([p (parser-char (string-ref s 0))]
             [f char-upcase]
             [g (lambda (c) (integer->char (+ 1 (char->integer c))))]
             [lhs (parser-parse (parser-map (compose f g) p) s)]
             [rhs (parser-parse (parser-map f (parser-map g p)) s)])
        (and (right? lhs) (right? rhs)
             (equal? (from-right lhs) (from-right rhs)))))
    'tests 100)

  ;; === Applicative Laws ===
  
  ;; Identity: pure id <*> v === v
  (define-property "parser applicative identity"
    gen-string-non-empty
    (lambda (s)
      (let* ([v (parser-char (string-ref s 0))]
             [result (parser-parse (parser-ap (parser-pure id) v) s)])
        (right? result)))
    'tests 100)

  ;; Homomorphism: pure f <*> pure x === pure (f x)
  (define-property "parser applicative homomorphism"
    gen-char-lowercase
    (lambda (c)
      (let* ([f char-upcase]
             [lhs (parser-parse (parser-ap (parser-pure f) (parser-pure c)) "")]
             [rhs (parser-parse (parser-pure (f c)) "")])
        (and (right? lhs) (right? rhs)
             (equal? (from-right lhs) (from-right rhs)))))
    'tests 100)

  ;; === Primitive Parsers ===
  
  ;; parser-pure always succeeds with value, no consumption
  (define-property "parser-pure succeeds without consuming"
    (gen-bind gen-char-lowercase
      (lambda (c)
        (gen-map (lambda (s) (cons c s)) gen-string-small)))
    (lambda (cs)
      (let* ([c (car cs)] [s (cdr cs)]
             [result (parser-parse (parser-pure c) s)])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; parser-char succeeds on matching char
  (define-property "parser-char succeeds on match"
    gen-char-lowercase
    (lambda (c)
      (let ([result (parser-parse (parser-char c) (string c))])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; parser-char fails on non-matching char
  (define-property "parser-char fails on mismatch"
    (gen-bind gen-char-lowercase
      (lambda (c)
        (gen-map (lambda (d) (cons c d)) gen-char-uppercase)))
    (lambda (cd)
      (let* ([c (car cd)] [d (cdr cd)]
             [result (parser-parse (parser-char c) (string d))])
        (left? result)))
    'tests 100)

  ;; parser-satisfy with always-true predicate always succeeds
  (define-property "parser-satisfy with true predicate succeeds"
    gen-string-non-empty
    (lambda (s)
      (let ([result (parser-parse (parser-satisfy (lambda (_) #t) "anything") s)])
        (right? result)))
    'tests 100)

  ;; parser-satisfy with always-false predicate always fails
  (define-property "parser-satisfy with false predicate fails"
    gen-string-non-empty
    (lambda (s)
      (let ([result (parser-parse (parser-satisfy (lambda (_) #f) "nothing") s)])
        (left? result)))
    'tests 100)

  ;; parser-string succeeds on exact match
  (define-property "parser-string succeeds on exact match"
    gen-string-small
    (lambda (s)
      (let ([result (parser-parse (parser-string s) s)])
        (and (right? result)
             (equal? (from-right result) s))))
    'tests 100)

  ;; === Alternative ===
  
  ;; parser-or left bias: if first succeeds, return first
  (define-property "parser-or uses first on success"
    gen-char-lowercase
    (lambda (c)
      (let* ([p1 (parser-char c)]
             [p2 (parser-fail "second")]
             [result (parser-parse (parser-or p1 p2) (string c))])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; parser-or tries second if first fails without consuming
  (define-property "parser-or tries second on failure"
    gen-char-lowercase
    (lambda (c)
      (let* ([p1 (parser-fail "first")]
             [p2 (parser-char c)]
             [result (parser-parse (parser-or p1 p2) (string c))])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; parser-optional returns default on failure
  (define-property "parser-optional returns default on failure"
    gen-string-small
    (lambda (s)
      (let* ([p (parser-fail "always fails")]
             [default 'default-value]
             [result (parser-parse (parser-optional p default) s)])
        (and (right? result)
             (equal? (from-right result) default))))
    'tests 100)

  ;; parser-optional returns value on success
  (define-property "parser-optional returns value on success"
    gen-char-lowercase
    (lambda (c)
      (let* ([p (parser-char c)]
             [default 'default-value]
             [result (parser-parse (parser-optional p default) (string c))])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; === Sequence ===
  
  ;; parser-then discards first result
  (define-property "parser-then discards first"
    (gen-bind gen-char-lowercase
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lowercase)))
    (lambda (cs)
      (let* ([c1 (car cs)] [c2 (cdr cs)]
             [p (parser-then (parser-char c1) (parser-char c2))]
             [result (parser-parse p (string c1 c2))])
        (and (right? result)
             (equal? (from-right result) c2))))
    'tests 100)

  ;; parser-left discards second result
  (define-property "parser-left discards second"
    (gen-bind gen-char-lowercase
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lowercase)))
    (lambda (cs)
      (let* ([c1 (car cs)] [c2 (cdr cs)]
             [p (parser-left (parser-char c1) (parser-char c2))]
             [result (parser-parse p (string c1 c2))])
        (and (right? result)
             (equal? (from-right result) c1))))
    'tests 100)

  ;; === Repetition ===
  
  ;; parser-many on always-failing parser returns empty list
  (define-property "parser-many returns empty on failure"
    gen-string-small
    (lambda (s)
      (let* ([p (parser-fail "never")]
             [result (parser-parse (parser-many p) s)])
        (and (right? result)
             (equal? (from-right result) '()))))
    'tests 100)

  ;; parser-many succeeds even with zero matches
  (define-property "parser-many succeeds with zero matches"
    (gen-pure "zzz")  ; String that won't match #\x
    (lambda (s)
      (let* ([p (parser-char #\x)]
             [result (parser-parse (parser-many p) s)])
        (and (right? result)
             (equal? (from-right result) '()))))
    'tests 100)

  ;; parser-some fails on zero matches
  (define-property "parser-some fails on zero matches"
    (gen-pure "zzz")  ; String that won't match #\x
    (lambda (s)
      (let* ([p (parser-char #\x)]
             [result (parser-parse (parser-some p) s)])
        (left? result)))
    'tests 100)

  ;; parser-count n returns exactly n items
  (define-property "parser-count returns exact count"
    (gen-int-range 1 5)
    (lambda (n)
      (let* ([p (parser-char #\a)]
             [result (parser-parse (parser-count n p) (make-string n #\a))])
        (and (right? result)
             (equal? (length (from-right result)) n))))
    'tests 100)

  ;; parser-between parses content between delimiters
  (define-property "parser-between parses between delimiters"
    gen-char-lowercase
    (lambda (c)
      (let* ([open (parser-char #\()]
             [close (parser-char #\))]
             [content (parser-char c)]
             [p (parser-between open close content)]
             [result (parser-parse p (string #\( c #\)) )])
        (and (right? result)
             (equal? (from-right result) c))))
    'tests 100)

  ;; === Position ===
  
  ;; parser-initial-pos starts at line 1, col 1, offset 0
  (define-property "initial position is (1,1,0)"
    (gen-pure #t)
    (lambda (_)
      (let ([p parser-initial-pos])
        (and (= 1 (pos-line p))
             (= 1 (pos-col p))
             (= 0 (pos-offset p)))))
    'tests 10)

  ;; advance-pos increments offset
  (define-property "advance-pos increments offset"
    gen-char-lowercase
    (lambda (c)
      (let* ([p1 (make-pos 1 1 0)]
             [p2 (advance-pos p1 c)])
        (= 1 (pos-offset p2))))
    'tests 100)

  ;; advance-pos on newline increments line, resets col
  (define-property "advance-pos on newline resets column"
    (gen-pure #t)
    (lambda (_)
      (let* ([p1 (make-pos 1 5 4)]
             [nl (integer->char 10)]
             [p2 (advance-pos p1 nl)])
        (and (= 2 (pos-line p2))
             (= 1 (pos-col p2)))))
    'tests 10)

  ;; parser advances position on successful parse
  (define-property "successful parse advances position"
    gen-char-lowercase
    (lambda (c)
      (let* ([p (parser-char c)]
             [state (parser-initial-state (string c))]
             [result (run-parser p state)])
        (and (right? result)
             (let* ([new-state (cdr (from-right result))])
               (= 1 (pos-offset (parser-state-pos new-state)))))))
    'tests 100)

  ;; parser-state-at-end? true for empty string
  (define-property "at-end true for empty string"
    (gen-pure #t)
    (lambda (_)
      (let ([state (parser-initial-state "")])
        (parser-state-at-end? state)))
    'tests 10)

  ;; parser-state-at-end? false for non-empty string
  (define-property "at-end false for non-empty"
    gen-string-non-empty
    (lambda (s)
      (let ([state (parser-initial-state s)])
        (not (parser-state-at-end? state))))
    'tests 100)

  ;; === Error Handling ===
  
  ;; parse-error contains position
  (define-property "parse-error has position"
    gen-string-small
    (lambda (s)
      (let* ([p (parser-fail "test error")]
             [result (run-parser p (parser-initial-state s))])
        (and (left? result)
             (let ([err (from-left result)])
               (pos? (error-pos err))))))
    'tests 100)

  ;; parse-error contains message
  (define-property "parse-error has message"
    (gen-pure #t)
    (lambda (_)
      (let* ([msg "specific error message"]
             [p (parser-fail msg)]
             [result (run-parser p (parser-initial-state ""))])
        (and (left? result)
             (let ([err (from-left result)])
               (equal? msg (error-message err))))))
    'tests 10)

  ;; === Character Classes ===
  
  ;; parser-digit matches digits 0-9
  (define-property "parser-digit matches all digits"
    (gen-int-range 0 9)
    (lambda (n)
      (let* ([c (integer->char (+ 48 n))]
             [result (parser-parse parser-digit (string c))])
        (right? result)))
    'tests 100)

  ;; parser-letter matches a-z and A-Z
  (define-property "parser-letter matches letters"
    (gen-int-range 0 51)
    (lambda (n)
      (let* ([c (if (< n 26)
                   (integer->char (+ 65 n))
                   (integer->char (+ 97 (- n 26))))]
             [result (parser-parse parser-letter (string c))])
        (right? result)))
    'tests 100)

  ;; parser-space matches whitespace
  (define-property "parser-space matches space"
    (gen-pure #t)
    (lambda (_)
      (let ([result (parser-parse parser-space " ")])
        (right? result)))
    'tests 10)
)

;;; =================================================================
;;; Run
;;; =================================================================

(run-all-tests-and-exit)
