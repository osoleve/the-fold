;;; core/base/test-prelude.ss — Tests for prelude.ss
;;;
;;; Dependencies:
;;;   - prelude.ss (base module)

(load "core/base/prelude.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "Testing prelude.ss
")
(display "====

")

;;; andmap tests
(display "andmap:
")
(test "empty list" #t (andmap number? '()))
(test "all pass" #t (andmap number? '(1 2 3)))
(test "one fails" #f (andmap number? '(1 "x" 3)))
(test "first fails" #f (andmap number? '("x" 2 3)))

;;; ormap tests
(display "
ormap:
")
(test "empty list" #f (ormap number? '()))
(test "one passes" #t (ormap number? '("a" 2 "c")))
(test "all fail" #f (ormap number? '("a" "b" "c")))

;;; unique tests
(display "
unique:
")
(test "no duplicates" '(a b c) (unique '(a b c)))
(test "with duplicates" '(a b c) (unique '(a b a c b)))
(test "empty list" '() (unique '()))
(test "all same" '(x) (unique '(x x x)))

;;; filter tests
(display "
filter:
")
(test "keep evens" '(2 4) (filter even? '(1 2 3 4 5)))
(test "keep none" '() (filter even? '(1 3 5)))
(test "keep all" '(2 4 6) (filter even? '(2 4 6)))

;;; fold-left tests
(display "
fold-left:
")
(test "sum" 10 (fold-left + 0 '(1 2 3 4)))
(test "empty" 0 (fold-left + 0 '()))
(test "left assoc" '(((0 1) 2) 3) (fold-left (lambda (acc x) (list acc x)) 0 '(1 2 3)))

;;; fold-right tests
(display "
fold-right:
")
(test "sum" 10 (fold-right + 0 '(1 2 3 4)))
(test "right assoc" '(1 (2 (3 0))) (fold-right (lambda (x acc) (list x acc)) 0 '(1 2 3)))

;;; zip tests
(display "
zip:
")
(test "equal length" '((1 . a) (2 . b)) (zip '(1 2) '(a b)))
(test "first shorter" '((1 . a)) (zip '(1) '(a b)))
(test "empty" '() (zip '() '(a b)))

;;; iota tests
(display "
iota:
")
(test "iota 0" '() (iota 0))
(test "iota 5" '(0 1 2 3 4) (iota 5))

;;; take/drop tests
(display "
take/drop:
")
(test "take 2" '(a b) (take 2 '(a b c d)))
(test "take 0" '() (take 0 '(a b c)))
(test "drop 2" '(c d) (drop 2 '(a b c d)))
(test "drop 0" '(a b c) (drop 0 '(a b c)))

;;; Result type tests
(display "
Result type:
")
(test "ok? true" #t (ok? '(ok 42)))
(test "ok? false" #f (ok? '(error bad)))
(test "error? true" #t (error? '(error bad)))
(test "error? false" #f (error? '(ok 42)))
(test "unwrap-ok" 42 (unwrap-ok '(ok 42)))
(test "unwrap-error" '(bad thing) (unwrap-error '(error bad thing)))

;;; result-map tests
(display "
result-map:
")
(test "map ok" '(ok 10) (result-map (lambda (x) (* x 2)) '(ok 5)))
(test "map error" '(error oops) (result-map (lambda (x) (* x 2)) '(error oops)))

;;; result-bind tests
(display "
result-bind:
")
(test "bind ok" '(ok 10) (result-bind '(ok 5) (lambda (x) `(ok ,(* x 2)))))
(test "bind error" '(error oops) (result-bind '(error oops) (lambda (x) `(ok ,(* x 2)))))

;;; result-sequence tests
(display "
result-sequence:
")
(test "all ok" '(ok (1 2 3)) (result-sequence '((ok 1) (ok 2) (ok 3))))
(test "one error" '(error bad) (result-sequence '((ok 1) (error bad) (ok 3))))
(test "empty" '(ok ()) (result-sequence '()))

;;; string-join tests
(display "
string-join:
")
(test "join words" "a,b,c" (string-join '("a" "b" "c") ","))
(test "join empty" "" (string-join '() ","))
(test "join one" "hello" (string-join '("hello") ","))

;;; ====
;;; Migrated List Utilities (from fold-2rj)
;;; ====

;;; find tests
(display "
find:
")
(test "find even" 2 (find even? '(1 2 3 4)))
(test "find first" 1 (find odd? '(1 2 3)))
(test "find none" #f (find even? '(1 3 5)))
(test "find empty" #f (find even? '()))

;;; last tests
(display "
last:
")
(test "last of list" 3 (last '(1 2 3)))
(test "last singleton" 5 (last '(5)))
(test "last symbols" 'z (last '(x y z)))

;;; init tests
(display "
init:
")
(test "init of list" '(1 2) (init '(1 2 3)))
(test "init singleton" '() (init '(5)))
(test "init longer" '(a b c) (init '(a b c d)))

;;; replicate tests
(display "
replicate:
")
(test "replicate 3" '(x x x) (replicate 3 'x))
(test "replicate 0" '() (replicate 0 'x))
(test "replicate 1" '(42) (replicate 1 42))
(test "replicate negative" '() (replicate -1 'x))

;;; span tests
(display "
span:
")
(let-values ([(pre suf) (span (lambda (x) (< x 3)) '(1 2 3 4 5))])
            (test "span prefix" '(1 2) pre)
            (test "span suffix" '(3 4 5) suf))
(let-values ([(pre suf) (span even? '(1 2 3))])
            (test "span no match at start" '() pre)
            (test "span all suffix" '(1 2 3) suf))
(let-values ([(pre suf) (span number? '(1 2 3))])
            (test "span all match" '(1 2 3) pre)
            (test "span empty suffix" '() suf))
(let-values ([(pre suf) (span odd? '())])
            (test "span empty list" '() pre)
            (test "span empty suffix" '() suf))

;;; break tests
(display "
break:
")
(let-values ([(pre suf) (break (lambda (x) (>= x 3)) '(1 2 3 4 5))])
            (test "break prefix" '(1 2) pre)
            (test "break suffix" '(3 4 5) suf))
(let-values ([(pre suf) (break even? '(1 3 5 2 4))])
            (test "break odd prefix" '(1 3 5) pre)
            (test "break at first even" '(2 4) suf))

;;; ====
;;; Collection Utilities
;;; ====

;;; identity tests
(display "
identity:
")
(test "identity number" 42 (identity 42))
(test "identity list" '(1 2 3) (identity '(1 2 3)))
(test "identity symbol" 'foo (identity 'foo))

;;; flatten tests
(display "
flatten:
")
(test "flatten nested" '(1 2 3 4 5 6) (flatten '((1 2) (3 4) (5 6))))
(test "flatten empty inner" '(1 2 3) (flatten '((1) () (2 3))))
(test "flatten empty" '() (flatten '()))
(test "flatten single" '(a b c) (flatten '((a b c))))

;;; append-map tests
(display "
append-map:
")
(test "append-map basic" '(1 1 2 2 3 3) (append-map (lambda (x) (list x x)) '(1 2 3)))
(test "append-map empty" '() (append-map (lambda (x) (list x x)) '()))
(test "append-map varying" '(0 0 1 0 1 2) (append-map iota '(1 2 3)))
(test "append-map identity" '(a b c) (append-map list '(a b c)))

;;; partition tests
(display "
partition:
")
(test "partition evens" '((2 4) (1 3 5)) (partition even? '(1 2 3 4 5)))
(test "partition all pass" '((1 2 3) ()) (partition number? '(1 2 3)))
(test "partition none pass" '(() (a b c)) (partition number? '(a b c)))
(test "partition empty" '(() ()) (partition even? '()))

;;; group-by tests
(display "
group-by:
")
(test "group-by length" '((3 . ("foo" "bar")) (5 . ("hello")))
      (group-by string-length '("foo" "bar" "hello")))
(test "group-by empty" '() (group-by identity '()))
(test "group-by single" '((x . (x))) (group-by identity '(x)))

;;; distinct-by tests
(display "
distinct-by:
")
(test "distinct-by first char"
      '("apple" "banana" "cherry")
      (distinct-by (lambda (s) (string-ref s 0)) '("apple" "apricot" "banana" "blueberry" "cherry")))
(test "distinct-by identity" '(1 2 3) (distinct-by identity '(1 2 2 3 3 3)))
(test "distinct-by empty" '() (distinct-by identity '()))

;;; ====
;;; String Utilities
;;; ====

;;; string-split tests
(display "
string-split:
")
(test "split by comma" '("a" "b" "c") (string-split "a,b,c" #\,))
(test "split no delim" '("hello") (string-split "hello" #\,))
(test "split empty parts" '("" "a" "" "b" "") (string-split ",a,,b," #\,))

;;; string-trim tests
(display "
string-trim:
")
(test "trim both" "hello" (string-trim "  hello  "))
(test "trim left" "hello" (string-trim-left "  hello"))
(test "trim right" "hello" (string-trim-right "hello  "))
(test "trim none needed" "hello" (string-trim "hello"))
(test "trim all spaces" "" (string-trim "   "))

;;; string-contains? tests
(display "
string-contains?:
")
(test "contains substring" #t (string-contains? "hello world" "wor"))
(test "contains at start" #t (string-contains? "hello" "hel"))
(test "contains at end" #t (string-contains? "hello" "llo"))
(test "not contains" #f (string-contains? "hello" "xyz"))
(test "contains empty" #t (string-contains? "hello" ""))

;;; string-starts-with? tests
(display "
string-starts-with?:
")
(test "starts with prefix" #t (string-starts-with? "hello" "hel"))
(test "starts with full" #t (string-starts-with? "hello" "hello"))
(test "not starts with" #f (string-starts-with? "hello" "llo"))
(test "starts with empty" #t (string-starts-with? "hello" ""))

;;; string-ends-with? tests
(display "
string-ends-with?:
")
(test "ends with suffix" #t (string-ends-with? "hello" "llo"))
(test "ends with full" #t (string-ends-with? "hello" "hello"))
(test "not ends with" #f (string-ends-with? "hello" "hel"))
(test "ends with empty" #t (string-ends-with? "hello" ""))

;;; string-index-of tests
(display "
string-index-of:
")
(test "index of found" 6 (string-index-of "hello world" "world"))
(test "index of start" 0 (string-index-of "hello" "hel"))
(test "index not found" #f (string-index-of "hello" "xyz"))
(test "index of empty" 0 (string-index-of "hello" ""))

;;; string-last-index-of tests
(display "
string-last-index-of:
")
(test "last index found" 8 (string-last-index-of "foo bar foo" "foo"))
(test "last index once" 4 (string-last-index-of "hello" "o"))
(test "last index not found" #f (string-last-index-of "hello" "xyz"))

;;; string-replace tests
(display "
string-replace:
")
(test "replace single" "h-llo" (string-replace "hello" "e" "-"))
(test "replace all l" "he--o" (string-replace "hello" "l" "-"))
(test "replace word" "goodbye world" (string-replace "hello world" "hello" "goodbye"))
(test "replace nothing" "hello" (string-replace "hello" "xyz" "abc"))
(test "replace empty old" "hello" (string-replace "hello" "" "x"))

;;; string-reverse tests
(display "
string-reverse:
")
(test "reverse string" "olleh" (string-reverse "hello"))
(test "reverse empty" "" (string-reverse ""))
(test "reverse single" "a" (string-reverse "a"))

;;; string-empty? tests
(display "
string-empty?:
")
(test "empty is empty" #t (string-empty? ""))
(test "non-empty not empty" #f (string-empty? "a"))

;;; string-blank? tests
(display "
string-blank?:
")
(test "empty is blank" #t (string-blank? ""))
(test "spaces blank" #t (string-blank? "   "))
(test "tabs blank" #t (string-blank? "\t\t"))
(test "text not blank" #f (string-blank? "hello"))

;;; string-all-match? tests
(display "
string-all-match?:
")
(test "all digits" #t (string-all-match? "12345" char-numeric?))
(test "not all digits" #f (string-all-match? "123a5" char-numeric?))
(test "empty all match" #t (string-all-match? "" char-numeric?))

;;; string-upcase/downcase tests
(display "
string-upcase/downcase:
")
(test "upcase" "HELLO" (string-upcase "hello"))
(test "downcase" "hello" (string-downcase "HELLO"))
(test "mixed upcase" "HELLO WORLD" (string-upcase "Hello World"))

;;; string-pad tests
(display "
string-pad:
")
(test "pad left" "00042" (string-pad-left "42" 5 #\0))
(test "pad right" "42   " (string-pad-right "42" 5 #\space))
(test "pad no change" "hello" (string-pad-left "hello" 3 #\x))

;;; edit-distance tests
(display "
edit-distance:
")
(test "same string" 0 (edit-distance "hello" "hello"))
(test "one insert" 1 (edit-distance "hello" "helo"))
(test "one delete" 1 (edit-distance "helo" "hello"))
(test "one substitute" 1 (edit-distance "hello" "hallo"))
(test "kitten sitting" 3 (edit-distance "kitten" "sitting"))
(test "empty first" 5 (edit-distance "" "hello"))
(test "empty second" 5 (edit-distance "hello" ""))

;;; ====
;;; Unicode Aliases
;;; ====

(display "
Unicode Aliases:
")
;; Lambda alias
(test "lambda alias" 25 ((λ (x) (* x x)) 5))

;; Function composition (using Unicode ring operator)
(test "composition" 100 ((∘ (λ (x) (* x x)) (λ (x) (* x 2))) 5))

;; Logic operators
(test "and alias" #t (∧ #t #t))
(test "or alias" #t (∨ #f #t))
(test "not alias" #f (¬ #t))

;; Comparison operators
(test "not equal" #t (≠ 1 2))
(test "less-equal" #t (≤ 1 2))
(test "greater-equal" #t (≥ 2 1))

;; Arithmetic
(test "multiply alias" 6 (× 2 3))
(test "divide alias" 2 (÷ 6 3))
(test "square" 25 (² 5))
(test "cube" 125 (³ 5))
(test "sqrt alias" 5 (√ 25))

;; Collections
(test "member alias" #t (∈ 2 '(1 2 3)))
(test "not member alias" #t (∉ 4 '(1 2 3)))
(test "empty list alias" '() ∅)

;; Constants
(test "pi defined" #t (and (number? π) (> π 3.14) (< π 3.15)))
(test "tau = 2pi" #t (< (abs (- τ (* 2 π))) 0.0001))
(test "e defined" #t (and (number? 𝑒) (> 𝑒 2.71) (< 𝑒 2.72)))
(test "infinity" #t (> ∞ 1e308))

;;; Summary
(newline)
(display "====
")
(display "Passed: ") (display tests-passed) (newline)
(display "Failed: ") (display tests-failed) (newline)

(if (> tests-failed 0)
    (exit 1)
    (display "
✓ All prelude tests passed!
"))
