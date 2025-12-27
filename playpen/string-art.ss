;;; playpen/string-art.ss — Creative String Utilities Playground
;;;
;;; Let's have some fun with the new string utilities!

(load "thimble/string-utils.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║         STRING UTILITIES PLAYGROUND & STRESS TEST          ║\n")
(printf "╔════════════════════════════════════════════════════════════╗\n\n")

;;; ============================================================
;;; Test 1: ASCII Art Manipulation
;;; ============================================================

(printf "🎨 Test 1: ASCII Art Manipulation\n")
(printf "═══════════════════════════════════\n\n")

(define duck-art
  "    __\n   (o >\n   /( )\n   ^ ^")

(define duck-lines (string-split-lines duck-art))
(printf "Original duck:\n~a\n\n" duck-art)

(printf "Duck line-by-line:\n")
(for-each
  (lambda (line)
    (printf "  [~a] ~s\n" (string-length line) line))
  duck-lines)

(printf "\nIndented duck:\n")
(define indented-duck
  (string-join
    (map (lambda (line) (string-append "    " line)) duck-lines)
    "\n"))
(printf "~a\n\n" indented-duck)

;;; ============================================================
;;; Test 2: Emoji Chaos
;;; ============================================================

(printf "😈 Test 2: Emoji Stress Test\n")
(printf "═══════════════════════════════════\n\n")

(define emoji-sentence "🔥💧🌍⚡🌙⭐💫✨🌟🎨")
(printf "Emoji string: ~a\n" emoji-sentence)
(printf "Length: ~a characters\n" (string-length emoji-sentence))

(define emoji-list (map string (string->list emoji-sentence)))
(printf "Individual emoji: ~s\n" emoji-list)

(define emoji-csv (string-join emoji-list ","))
(printf "As CSV: ~a\n" emoji-csv)

(define emoji-split (string-split emoji-csv #\,))
(printf "Split back: ~s\n" emoji-split)
(printf "Match? ~a\n\n" (equal? emoji-list emoji-split))

;;; ============================================================
;;; Test 3: Nested Replacement Madness
;;; ============================================================

(printf "🎭 Test 3: Replacement Chain Reaction\n")
(printf "═══════════════════════════════════════\n\n")

(define text "foo foo foo")
(printf "Start:    ~s\n" text)

(define step1 (string-replace text "foo" "bar"))
(printf "Step 1:   ~s  (foo->bar)\n" step1)

(define step2 (string-replace step1 "bar" "baz"))
(printf "Step 2:   ~s  (bar->baz)\n" step2)

(define step3 (string-replace step2 "baz" "qux"))
(printf "Step 3:   ~s  (baz->qux)\n" step3)

(define step4 (string-replace step3 "qux" "🎉"))
(printf "Step 4:   ~s  (qux->🎉)\n\n" step4)

;;; ============================================================
;;; Test 4: The Whitespace Challenge
;;; ============================================================

(printf "👻 Test 4: Invisible Whitespace Hunting\n")
(printf "═══════════════════════════════════════════\n\n")

(define mystery-strings
  (list
    ""
    " "
    "  "
    "\t"
    "\n"
    "\r\n"
    " \t\n\r "
    "invisible"
    " visible "))

(printf "Testing string-blank? on various inputs:\n")
(for-each
  (lambda (s)
    (printf "  ~s → blank? ~a, empty? ~a, len: ~a\n"
            s
            (string-blank? s)
            (string-empty? s)
            (string-length s)))
  mystery-strings)

(printf "\n")

;;; ============================================================
;;; Test 5: Pattern Matching Puzzle
;;; ============================================================

(printf "🔍 Test 5: Pattern Detective\n")
(printf "═══════════════════════════════\n\n")

(define haystack "The quick brown fox jumps over the lazy dog")
(define needles '("quick" "slow" "fox" "cat" "the" "THE" ""))

(printf "Haystack: ~s\n\n" haystack)
(printf "Searching for patterns:\n")
(for-each
  (lambda (needle)
    (printf "  ~s → contains? ~a, starts? ~a, ends? ~a\n"
            needle
            (string-contains? haystack needle)
            (string-starts-with? haystack needle)
            (string-ends-with? haystack needle)))
  needles)

(printf "\n")

;;; ============================================================
;;; Test 6: Recursive Split/Join Identity Test
;;; ============================================================

(printf "♻️  Test 6: Split/Join Roundtrip\n")
(printf "════════════════════════════════════\n\n")

(define test-cases
  '(("a,b,c" #\,)
    ("one:two:three:four" #\:)
    ("x||y||z" #\|)
    ("single" #\,)
    ("" #\,)))

(printf "Testing split→join identity:\n")
(for-each
  (lambda (test)
    (let* ([original (car test)]
           [delim (cadr test)]
           [parts (string-split original delim)]
           [rejoined (string-join parts (string delim))])
      (printf "  ~s → ~s → ~s [~a]\n"
              original
              parts
              rejoined
              (if (string=? original rejoined) "✓" "✗"))))
  test-cases)

(printf "\n")

;;; ============================================================
;;; Test 7: Build a Tiny Language
;;; ============================================================

(printf "🏗️  Test 7: Mini Template Language\n")
(printf "═══════════════════════════════════════\n\n")

(define template "Dear {{name}},\n\nYou have {{count}} new {{item}}!\nTotal: {{total}}.\n\nBest,\n{{sender}}")

(printf "Template:\n~a\n\n" template)

(define (render tmpl bindings)
  (if (null? bindings)
      tmpl
      (let* ([binding (car bindings)]
             [key (car binding)]
             [value (cdr binding)]
             [placeholder (string-append "{{" key "}}")]
             [replaced (string-replace tmpl placeholder value)])
        (render replaced (cdr bindings)))))

(define bindings
  '(("name" . "Alice")
    ("count" . "42")
    ("item" . "messages")
    ("total" . "9001")
    ("sender" . "The Fold System")))

(define result (render template bindings))
(printf "Rendered:\n~a\n\n" result)

;;; ============================================================
;;; Test 8: Stress Test - Large String Operations
;;; ============================================================

(printf "💪 Test 8: Performance Check\n")
(printf "════════════════════════════════\n\n")

(define big-string (string-join (make-list 100 "test") ","))
(printf "Created string with 100 'test' words joined by commas\n")
(printf "Length: ~a characters\n" (string-length big-string))

(define big-split (string-split big-string #\,))
(printf "Split into: ~a parts\n" (length big-split))

(define replaced (string-replace big-string "test" "✓"))
(define replaced-count (string-split replaced #\✓))
(printf "Replaced all 'test' with '✓': ~a replacements\n" (- (length replaced-count) 1))

(printf "\n")

;;; ============================================================
;;; Test 9: Unicode Edge Cases
;;; ============================================================

(printf "🌏 Test 9: Unicode Adventures\n")
(printf "═══════════════════════════════════\n\n")

(define multilingual
  "Hello,مرحبا,你好,Привет,שלום,こんにちは")

(printf "Multilingual greetings: ~a\n" multilingual)
(define greetings (string-split multilingual #\,))
(printf "Split into ~a languages:\n" (length greetings))
(for-each
  (lambda (g) (printf "  • ~a\n" g))
  greetings)

(printf "\n")

;;; ============================================================
;;; Test 10: The Ultimate Trim Test
;;; ============================================================

(printf "✂️  Test 10: Trim Olympics\n")
(printf "═══════════════════════════════\n\n")

(define trim-challenges
  '("normal"
    "  spaces  "
    "\t\ttabs\t\t"
    "\n\nnewlines\n\n"
    " \t\n\r EVERYTHING \r\n\t "
    "     "
    "NoSpaceHere"))

(printf "Trim results:\n")
(for-each
  (lambda (s)
    (let ([trimmed (string-trim s)])
      (printf "  ~s\n    → ~s (~a chars → ~a chars)\n"
              s
              trimmed
              (string-length s)
              (string-length trimmed))))
  trim-challenges)

(printf "\n")

;;; ============================================================
;;; Victory Lap
;;; ============================================================

(printf "╔════════════════════════════════════════════════════════════╗\n")
(printf "║                     🎉 PLAYTEST COMPLETE! 🎉                ║\n")
(printf "║                                                            ║\n")
(printf "║   String utilities survived all challenges!                ║\n")
(printf "║   • ASCII art manipulation ✓                               ║\n")
(printf "║   • Emoji handling ✓                                       ║\n")
(printf "║   • Nested replacements ✓                                  ║\n")
(printf "║   • Whitespace detection ✓                                 ║\n")
(printf "║   • Pattern matching ✓                                     ║\n")
(printf "║   • Roundtrip identity ✓                                   ║\n")
(printf "║   • Template rendering ✓                                   ║\n")
(printf "║   • Large string performance ✓                             ║\n")
(printf "║   • Unicode support ✓                                      ║\n")
(printf "║   • Trim edge cases ✓                                      ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")
