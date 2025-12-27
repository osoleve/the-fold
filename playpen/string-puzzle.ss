;;; playpen/string-puzzle.ss — Word Puzzle Game
;;;
;;; A fun text adventure using string utilities!

(load "thimble/string-utils.ss")

;;; Helper functions
(define (unique lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (unique (filter (lambda (x) (not (equal? x (car lst))))
                           (cdr lst))))))

(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

(printf "\n")
(printf "╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║                   🎮 THE STRING PUZZLE 🎮                      ║\n")
(printf "║                                                               ║\n")
(printf "║  A mysterious message has been scrambled! Can you decode it?  ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")

;;; The scrambled message
(define scrambled "XXXehTXXX XXXdloFXXX XXXsiXXX XXXaXXX XXXecalpXXX XXXerehwXXX XXXnoitaerCXXX XXXdnaXXX XXXyrevocsiDXXX XXXtnemXXX")

(printf "🔐 ENCRYPTED MESSAGE:\n")
(printf "   ~a\n\n" scrambled)

(printf "🔍 CLUE: Remove the 'XXX' markers...\n\n")

(define step1 (string-replace scrambled "XXX" ""))
(printf "Step 1 - Removed markers:\n")
(printf "   ~a\n\n" step1)

(printf "🔍 CLUE: The words are backwards!\n\n")

(define (reverse-string s)
  (list->string (reverse (string->list s))))

(define words (string-split step1 #\space))
(define reversed-words (map reverse-string words))
(define step2 (string-join reversed-words " "))

(printf "Step 2 - Reversed each word:\n")
(printf "   ~a\n\n" step2)

(printf "✨ MESSAGE DECODED! ✨\n\n")

;;; Let's create an even more complex puzzle!
(printf "═══════════════════════════════════════════════════════════════\n\n")
(printf "🎯 BONUS PUZZLE: The Cipher Challenge!\n\n")

(define secret-cipher
  "Uifsf!jt!b!tfdsfu!jo!uif!gpmE/!Dbo!zpv!gjoe!ju@")

(printf "🔐 ENCRYPTED: ~a\n\n" secret-cipher)
(printf "🔍 HINT: Each letter is shifted by 1 in the alphabet...\n\n")

(define (shift-char c shift)
  (let ([code (char->integer c)])
    (cond
      [(and (>= code 65) (<= code 90))  ; A-Z
       (integer->char (+ 65 (modulo (+ (- code 65) shift) 26)))]
      [(and (>= code 97) (<= code 122)) ; a-z
       (integer->char (+ 97 (modulo (+ (- code 97) shift) 26)))]
      [else c])))

(define (caesar-cipher text shift)
  (list->string
    (map (lambda (c) (shift-char c shift))
         (string->list text))))

(define decoded (caesar-cipher secret-cipher -1))
(printf "🔓 DECRYPTED: ~a\n\n" decoded)

;;; Now let's play with word patterns!
(printf "═══════════════════════════════════════════════════════════════\n\n")
(printf "🎨 PATTERN FINDER: Hidden Words\n\n")

(define haystack
  "The Fold contains entities relations collections queries and knowledge")

(printf "Text: ~a\n\n" haystack)

(define patterns
  '("know" "query" "fold" "block" "entity" "relation"))

(printf "Searching for hidden words:\n")
(for-each
  (lambda (pattern)
    (if (string-contains? haystack pattern)
        (let ([pos (string-index-of haystack pattern)])
          (printf "  ✓ Found '~a' at position ~a\n" pattern pos))
        (printf "  ✗ '~a' not found\n" pattern)))
  patterns)

(printf "\n")

;;; Word frequency counter
(printf "═══════════════════════════════════════════════════════════════\n\n")
(printf "📊 WORD FREQUENCY ANALYZER\n\n")

(define sample-text
  "the quick brown fox jumps over the lazy dog the fox was quick")

(printf "Text: ~a\n\n" sample-text)

(define all-words (string-split sample-text #\space))
(printf "Total words: ~a\n" (length all-words))
(printf "Unique words: ~a\n\n" (length (unique all-words)))

(printf "Word occurrences:\n")
(define unique-words (unique all-words))
(for-each
  (lambda (word)
    (let ([count (length (filter (lambda (w) (string=? w word)) all-words))])
      (printf "  '~a' appears ~a time~a\n"
              word
              count
              (if (= count 1) "" "s"))))
  unique-words)

(printf "\n")

;;; Palindrome checker
(printf "═══════════════════════════════════════════════════════════════\n\n")
(printf "🔄 PALINDROME DETECTOR\n\n")

(define (is-palindrome? s)
  (let* ([cleaned (string-trim (string-replace s " " ""))]
         [reversed (reverse-string cleaned)])
    (string=? cleaned reversed)))

(define test-phrases
  '("racecar"
    "hello"
    "A man a plan a canal Panama"
    "madam"
    "The Fold"
    "noon"))

(printf "Testing for palindromes:\n")
(for-each
  (lambda (phrase)
    (printf "  ~s → ~a\n"
            phrase
            (if (is-palindrome? phrase) "✓ PALINDROME!" "✗ not a palindrome")))
  test-phrases)

(printf "\n")

;;; Final challenge: Build a haiku formatter
(printf "═══════════════════════════════════════════════════════════════\n\n")
(printf "🌸 HAIKU FORMATTER\n\n")

(define haiku-lines
  '("Content addressed blocks"
    "Pure functions compose with grace"
    "The Fold holds all truth"))

(printf "Haiku:\n\n")
(for-each
  (lambda (line)
    (printf "     ~a\n" line))
  haiku-lines)

(printf "\n     Syllables: 5-7-5\n")
(printf "     Total characters: ~a\n"
        (apply + (map string-length haiku-lines)))

(define haiku-oneline (string-join haiku-lines " / "))
(printf "     One-line: ~a\n" haiku-oneline)

(printf "\n")

;;; Victory!
(printf "╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║                   🏆 ALL PUZZLES SOLVED! 🏆                    ║\n")
(printf "║                                                               ║\n")
(printf "║  You've mastered the string utilities through:                ║\n")
(printf "║  • Message decoding with replace & reverse                    ║\n")
(printf "║  • Caesar cipher decryption                                   ║\n")
(printf "║  • Pattern matching & searching                               ║\n")
(printf "║  • Word frequency analysis                                    ║\n")
(printf "║  • Palindrome detection                                       ║\n")
(printf "║  • Text formatting & composition                              ║\n")
(printf "║                                                               ║\n")
(printf "║  String utilities: Not just useful, but FUN! 🎉               ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")
