;;; coding.sexp — Development tasks for lattice/info/coding.ss
;;;
;;; Module: coding (tier 0, depends on entropy, sort)
;;; 534 lines, ~30 exported functions
;;; Coding theory: Huffman trees, arithmetic coding, LZ78, RLE,
;;; parity checks, repetition codes, Hamming(7,4), code properties.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement rle-encode
  ;; ──────────────────────────────────────────────
  ;; Run-length encoding — state machine with accumulator.

  (task
    (id "info/coding/rle-encode-impl")
    (module coding)
    (tier 0)
    (type implement)
    (description "Implement rle-encode: run-length encode a list of symbols. Consecutive equal elements are collapsed into pairs (symbol . count). For example: (a a a b b c) becomes ((a . 3) (b . 2) (c . 1)). Use a helper with four arguments: remaining list, current symbol, current run count, and accumulator. When the next symbol differs from current, push (current . count) onto the accumulator and start a new run. When the list is empty, push the final run and reverse.")
    (context "Available: null?, car, cdr, equal?, cons, reverse, +. State machine pattern with accumulator.")
    (before "(define (rle-encode message)
  (doc 'export #t)
  ;; TODO: run-length encode the message
  '())")
    (test-file "lattice/info/test-coding.ss")
    (test-forms (";; Standard runs
(assert-equal '((a . 3) (b . 2) (c . 4))
              (rle-encode '(a a a b b c c c c)))"
                 ";; No runs (all different)
(assert-equal '((a . 1) (b . 1) (c . 1))
              (rle-encode '(a b c)))"
                 ";; Single element
(assert-equal '((x . 1)) (rle-encode '(x)))"
                 ";; Empty list
(assert-equal '() (rle-encode '()))"))
    (available-modules (prelude))
    (hints ("Handle empty case first: (null? message) → '()"
            "Start helper with: remaining=(cdr message), current=(car message), count=1, acc='()"
            "If remaining is null: reverse (cons (current . count) acc)"
            "If (car remaining) = current: recurse with count+1"
            "If different: push (current . count) to acc, start new run with count=1"))
    (reference-solution "(define (rle-encode message)
  (if (null? message)
      '()
      (let loop ([remaining (cdr message)]
                 [current (car message)]
                 [count 1]
                 [acc '()])
        (cond
         [(null? remaining)
          (reverse (cons (cons current count) acc))]
         [(equal? (car remaining) current)
          (loop (cdr remaining) current (+ count 1) acc)]
         [else
          (loop (cdr remaining)
                (car remaining)
                1
                (cons (cons current count) acc))]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement majority-vote
  ;; ──────────────────────────────────────────────
  ;; Core of repetition code decoding.

  (task
    (id "info/coding/majority-vote-impl")
    (module coding)
    (tier 0)
    (type implement)
    (description "Implement majority-vote: given a list of bits (0s and 1s), return the majority bit. Count the number of 1s using filter and length. If the count of 1s is strictly greater than half the total length, return 1; otherwise return 0. This is the decoding step for repetition codes — each group of n repeated bits is decoded by majority vote, correcting up to floor((n-1)/2) bit errors.")
    (context "Available: filter, length, =, >, /, lambda. Count 1s and compare to threshold.")
    (before "(define (majority-vote bits)
  (doc 'export #t)
  ;; TODO: return the majority bit (0 or 1)
  0)")
    (test-file "lattice/info/test-coding.ss")
    (test-forms (";; Majority 1s
(assert-equal 1 (majority-vote '(1 1 0)))"
                 ";; Majority 0s
(assert-equal 0 (majority-vote '(0 0 1)))"
                 ";; Tie breaks to 0
(assert-equal 0 (majority-vote '(0 1)))"
                 ";; All same
(assert-equal 1 (majority-vote '(1 1 1)))"))
    (available-modules (prelude))
    (hints ("Count ones: (length (filter (lambda (b) (= b 1)) bits))"
            "Total length: (length bits)"
            "If ones > total/2 then 1, else 0"))
    (reference-solution "(define (majority-vote bits)
  (let ([ones (length (filter (lambda (b) (= b 1)) bits))])
    (if (> ones (/ (length bits) 2)) 1 0)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement hamming-distance
  ;; ──────────────────────────────────────────────
  ;; The fundamental code-theoretic metric.

  (task
    (id "info/coding/hamming-distance-impl")
    (module coding)
    (tier 0)
    (type implement)
    (description "Implement hamming-distance: compute the Hamming distance between two codewords (lists of bits). The Hamming distance is the number of positions where the corresponding bits differ. Recurse simultaneously over both lists: if the heads differ, add 1; otherwise add 0. Base case: if either list is empty, return 0. The Hamming distance determines a code's error detection and correction capability: a code with minimum distance d can detect d-1 errors and correct floor((d-1)/2) errors.")
    (context "Available: null?, or, car, cdr, =, +, if. Simple parallel list recursion.")
    (before "(define (hamming-distance a b)
  (doc 'export #t)
  ;; TODO: count positions where a and b differ
  0)")
    (test-file "lattice/info/test-coding.ss")
    (test-forms (";; Identical codewords: distance 0
(assert-equal 0 (hamming-distance '(1 0 1) '(1 0 1)))"
                 ";; One position differs
(assert-equal 1 (hamming-distance '(1 0 1) '(1 0 0)))"
                 ";; Two positions differ
(assert-equal 2 (hamming-distance '(1 0 1) '(0 0 0)))"
                 ";; All positions differ
(assert-equal 3 (hamming-distance '(1 1 1) '(0 0 0)))"))
    (available-modules (prelude))
    (hints ("Base case: (or (null? a) (null? b)) → 0"
            "Recursive case: add 1 if (car a) != (car b), else add 0"
            "Recurse on (cdr a) (cdr b)"
            "Can use: (+ (if (= (car a) (car b)) 0 1) (hamming-distance (cdr a) (cdr b)))"))
    (reference-solution "(define (hamming-distance a b)
  (if (or (null? a) (null? b))
      0
      (+ (if (= (car a) (car b)) 0 1)
         (hamming-distance (cdr a) (cdr b)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement prefix-match-length
  ;; ──────────────────────────────────────────────
  ;; Common prefix of two lists — used in LZ78.

  (task
    (id "info/coding/prefix-match-length-impl")
    (module coding)
    (tier 0)
    (type implement)
    (description "Implement prefix-match-length: compute the length of the common prefix between two lists. Walk both lists in parallel: while both are non-empty and their heads are equal, increment a counter and advance both. Return the counter when the lists diverge or either is exhausted. This is used in LZ78 dictionary compression to find the longest matching phrase. For example: (a b c) and (a b x y) share prefix length 2.")
    (context "Available: null?, or, car, cdr, equal?, +. Named let with two list pointers and a counter.")
    (before "(define (prefix-match-length a b)
  (doc 'export #t)
  ;; TODO: length of common prefix
  0)")
    (test-file "lattice/info/test-coding.ss")
    (test-forms (";; Full match
(assert-equal 3 (prefix-match-length '(a b c) '(a b c d)))"
                 ";; Partial match
(assert-equal 2 (prefix-match-length '(a b x) '(a b c)))"
                 ";; No match
(assert-equal 0 (prefix-match-length '(x) '(a)))"
                 ";; Empty list
(assert-equal 0 (prefix-match-length '() '(a b c)))"))
    (available-modules (prelude))
    (hints ("Named let with a, b, and len=0"
            "If either is null: return len"
            "If (equal? (car a) (car b)): recurse with (cdr a), (cdr b), len+1"
            "Else: return len"))
    (reference-solution "(define (prefix-match-length a b)
  (let loop ([a a] [b b] [len 0])
    (if (or (null? a) (null? b))
        len
        (if (equal? (car a) (car b))
            (loop (cdr a) (cdr b) (+ len 1))
            len))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement build-cumulative-probs
  ;; ──────────────────────────────────────────────
  ;; Running sum for arithmetic coding intervals.

  (task
    (id "info/coding/build-cumulative-probs-impl")
    (module coding)
    (tier 0)
    (type implement)
    (description "Implement build-cumulative-probs: build a cumulative probability table for arithmetic coding. Given a list of (symbol . probability) pairs, produce a list of (symbol low high) triples where [low, high) is each symbol's interval within [0, 1). The intervals partition [0, 1) in order: the first symbol gets [0, p1), the second gets [p1, p1+p2), etc. This is a running sum (scan) over the probabilities. For example: ((a . 0.5) (b . 0.3) (c . 0.2)) produces ((a 0.0 0.5) (b 0.5 0.8) (c 0.8 1.0)).")
    (context "Available: null?, car, cdr, caar, cdar, cons, list, reverse, +. Named let with cumulative sum.")
    (before "(define (build-cumulative-probs symbol-probs)
  (doc 'export #t)
  ;; TODO: build cumulative [low, high) intervals
  '())")
    (test-file "lattice/info/test-coding.ss")
    (test-forms (";; Three symbols partition [0,1)
(let ([result (build-cumulative-probs '((a . 0.5) (b . 0.3) (c . 0.2)))])
  (assert-equal 3 (length result))
  ;; First entry starts at 0
  (assert-true (< (abs (- (cadr (car result)) 0.0)) 0.001))
  ;; Last entry ends at 1
  (assert-true (< (abs (- (caddr (caddr result)) 1.0)) 0.001)))"
                 ";; Single symbol covers [0,1)
(let ([result (build-cumulative-probs '((x . 1.0)))])
  (assert-equal 1 (length result)))"
                 ";; Intervals are contiguous
(let ([result (build-cumulative-probs '((a . 0.5) (b . 0.5)))])
  ;; End of first = start of second
  (assert-true (< (abs (- (caddr (car result)) (cadr (cadr result)))) 0.001)))"))
    (available-modules (prelude))
    (hints ("Named let with: remaining list, running cumsum=0.0, accumulator"
            "For each entry: sym = (caar remaining), prob = (cdar remaining)"
            "new-cumsum = (+ cumsum prob)"
            "Accumulate (list sym cumsum new-cumsum)"
            "Reverse at end"))
    (reference-solution "(define (build-cumulative-probs symbol-probs)
  (let loop ([remaining symbol-probs] [cumsum 0.0] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let* ([sym (caar remaining)]
               [prob (cdar remaining)]
               [new-cumsum (+ cumsum prob)])
          (loop (cdr remaining)
                new-cumsum
                (cons (list sym cumsum new-cumsum) acc))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
