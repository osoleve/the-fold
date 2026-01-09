;;; core/query/test-aho-corasick.ss --- Tests for Aho-Corasick
;;;
;;; Comprehensive tests for multi-pattern string matching.

(load "lattice/query/aho-corasick.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (and (list? expected) (list? actual) (not (null? expected)) (pair? (car expected)))
      ;; Sort lists of pairs for order-independent comparison
      (let ([expected-sorted (list-sort (lambda (a b)
                                                (or (< (car a) (car b))
                                                    (and (= (car a) (car b))
                                                         (string<? (cdr a) (cdr b)))))
                                        expected)]
            [actual-sorted (list-sort (lambda (a b)
                                              (or (< (car a) (car b))
                                                  (and (= (car a) (car b))
                                                       (string<? (cdr a) (cdr b)))))
                                      actual)])
           (if (equal? expected-sorted actual-sorted)
               (begin
                (set! tests-passed (+ tests-passed 1))
                (display "  [PASS] ") (display name) (newline))
               (begin
                (set! tests-failed (+ tests-failed 1))
                (display "  [FAIL] ") (display name)
                (display " - expected ") (write expected-sorted)
                (display ", got ") (write actual-sorted)
                (newline))))
      ;; Direct comparison for non-lists
      (if (equal? expected actual)
          (begin
           (set! tests-passed (+ tests-passed 1))
           (display "  [PASS] ") (display name) (newline))
          (begin
           (set! tests-failed (+ tests-failed 1))
           (display "  [FAIL] ") (display name)
           (display " - expected ") (write expected)
           (display ", got ") (write actual)
           (newline)))))

(display "Testing aho-corasick.ss\n")
(display "========================\n\n")

;;; ============================================================
;;; Basic Pattern Matching
;;; ============================================================

(display "Basic Matching:\n")
(display "---------------\n")

(define ac1 (make-automaton '("he" "she" "his" "hers")))

(test "find 'he' in 'he'"
      '((0 . "he"))
      (search ac1 "he"))

(test "find 'she' in 'she'"
      '((0 . "she") (1 . "he"))  ; "he" is a suffix of "she"
      (search ac1 "she"))

(test "find patterns in 'ushers'"
      '((1 . "she") (2 . "he") (2 . "hers"))
      (search ac1 "ushers"))

(test "find patterns in 'she sells sea shells'"
      '((0 . "she") (1 . "he") (14 . "she") (15 . "he"))
      (search ac1 "she sells sea shells"))

(test "no matches in 'xyz'"
      '()
      (search ac1 "xyz"))

;;; ============================================================
;;; Overlapping Patterns
;;; ============================================================

(display "\nOverlapping Patterns:\n")
(display "---------------------\n")

(define ac2 (make-automaton '("a" "aa" "aaa")))

(test "overlapping 'a' patterns in 'aaa'"
      '((0 . "a") (0 . "aa") (0 . "aaa") (1 . "a") (1 . "aa") (2 . "a"))
      (search ac2 "aaa"))

(test "overlapping patterns in 'aaaa'"
      '((0 . "a") (0 . "aa") (0 . "aaa")
        (1 . "a") (1 . "aa") (1 . "aaa")
        (2 . "a") (2 . "aa")
        (3 . "a"))
      (search ac2 "aaaa"))

;;; ============================================================
;;; Single Pattern
;;; ============================================================

(display "\nSingle Pattern:\n")
(display "---------------\n")

(define ac3 (make-automaton '("test")))

(test "single pattern match"
      '((0 . "test"))
      (search ac3 "test"))

(test "single pattern multiple matches"
      '((0 . "test") (5 . "test"))
      (search ac3 "test test"))

(test "single pattern no match"
      '()
      (search ac3 "xyz"))

;;; ============================================================
;;; Empty and Edge Cases
;;; ============================================================

(display "\nEdge Cases:\n")
(display "-----------\n")

(define ac-empty (make-automaton '()))

(test "empty pattern list"
      '()
      (search ac-empty "anything"))

(define ac4 (make-automaton '("a")))

(test "single char pattern"
      '((0 . "a") (2 . "a"))
      (search ac4 "a a"))

(test "search empty text"
      '()
      (search ac4 ""))

;;; ============================================================
;;; Real-World Example: DNA Sequences
;;; ============================================================

(display "\nDNA Sequences:\n")
(display "--------------\n")

(define ac-dna (make-automaton '("ATG" "TGA" "TAG" "TAA")))

(test "find start codon"
      '((0 . "ATG"))
      (search ac-dna "ATGCCC"))

(test "find stop codons"
      '((2 . "ATG") (3 . "TGA") (6 . "TAG"))
      (search ac-dna "AAATGATAG"))

;;; ============================================================
;;; Performance: Many Patterns
;;; ============================================================

(display "\nMany Patterns:\n")
(display "--------------\n")

(define ac5 (make-automaton '("the" "he" "she" "her" "his" "i" "is")))

(define text "she is his friend")
(define matches (search ac5 text))

(test "count matches in sentence"
      8  ; she, he, is, i, his, i, he, i
      (length matches))

(test "verify key matches present"
      #t
      (and (if (member '(0 . "she") matches) #t #f)
           (if (member '(1 . "he") matches) #t #f)
           (if (member '(4 . "is") matches) #t #f)))

;;; ============================================================
;;; Case Sensitivity
;;; ============================================================

(display "\nCase Sensitivity:\n")
(display "-----------------\n")

(define ac6 (make-automaton '("test")))

(test "case sensitive (lowercase pattern)"
      '((0 . "test"))
      (search ac6 "test"))

(test "case sensitive (uppercase text)"
      '()
      (search ac6 "TEST"))

;;; ============================================================
;;; Prefix/Suffix Patterns
;;; ============================================================

(display "\nPrefix/Suffix Patterns:\n")
(display "-----------------------\n")

(define ac7 (make-automaton '("pre" "prefix" "fix")))

(test "prefix and full word"
      '((0 . "pre") (0 . "prefix") (3 . "fix"))
      (search ac7 "prefix"))

(test "suffix only"
      '((3 . "fix"))
      (search ac7 "suffix"))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "========================\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)

(if (= tests-failed 0)
    (begin
     (display "\n[PASS] All tests passed!\n")
     (display "\nSuccessfully dogfooded:\n")
     (display "  - Queue: BFS during failure link construction\n")
     (display "  - Dict: State transitions (char -> state-id)\n")
     (display "  - Set: Pattern storage and output functions\n\n")
     (exit 0))
    (begin
     (display "\n[FAIL] Some tests failed.\n")
     (exit 1)))
