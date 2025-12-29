;;; fabric/stitches/test-help.ss — Test harness for help.ss

(load "help.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

;;; Test primitive documentation lookup
(test "find-primitive-doc 'add"
      #t
      (not (not (find-primitive-doc 'add))))

(test "find-primitive-doc 'nonexistent"
      #f
      (find-primitive-doc 'nonexistent))

;;; Test category functions
(test "get-primitives-by-category 'arithmetic"
      7
      (length (get-primitives-by-category 'arithmetic)))

(test "get-primitives-by-category 'string"
      3
      (length (get-primitives-by-category 'string)))

;;; Test search functionality
(test "apropos search for 'string'"
      4
      (length (filter
               (lambda (doc)
                       (let ([name-str (symbol->string (cdr (assq 'name doc)))])
                            (string-contains? (string-downcase name-str) "string")))
               *primitive-docs*)))

(test "apropos search for 'length'"
      2
      (length (filter
               (lambda (doc)
                       (let ([name-str (symbol->string (cdr (assq 'name doc)))])
                            (string-contains? (string-downcase name-str) "length")))
               *primitive-docs*)))

;;; Test suggestion functionality
(test "suggest-primitive for 'ad'"
      'add
      (suggest-primitive 'ad))

(test "suggest-primitive for 'strng'"
      'string?
      (suggest-primitive 'strng))

(test "suggest-primitive for 'xyz'"
      #f
      (suggest-primitive 'xyz))

;;; Test string utilities
(test "string-downcase"
      "hello world"
      (string-downcase "HELLO WORLD"))

(test "string-contains? found"
      #t
      (string-contains? "hello world" "world"))

(test "string-contains? not found"
      #f
      (string-contains? "hello world" "xyz"))

;;; Test edit distance
(test "edit-distance identical"
      0
      (edit-distance "hello" "hello"))

(test "edit-distance one substitution"
      1
      (edit-distance "hello" "hallo"))

(test "edit-distance one insertion"
      1
      (edit-distance "hello" "helllo"))

(test "edit-distance one deletion"
      1
      (edit-distance "hello" "helo"))

(display "
Help system tests completed.
")
