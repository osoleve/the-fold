;;; fair-division.sexp — Development tasks for lattice/game-theory/fair-division.ss
;;;
;;; Module: fair-division (tier 0, depends on prelude, sort, vec)
;;; 788 lines, ~35 exported functions
;;; Algorithms for fairly dividing divisible and indivisible goods.
;;; Cake cutting protocols (cut-and-choose, Dubins-Spanier, Selfridge-Conway),
;;; fairness criteria (proportional, envy-free, equitable), adjusted winner,
;;; discrete fair division (round-robin, EF1, maximin share).
;;;
;;; Git history:
;;;   c6dffccb refactor(lattice): reorganize skills, consolidate graph modules
;;;   e68b9849 feat(module): migrate game-theory to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement piece-value
  ;; ──────────────────────────────────────────────
  ;; Compute a player's value for a piece of cake.

  (task
    (id "game-theory/fair-division/piece-value-impl")
    (module fair-division)
    (tier 0)
    (type implement)
    (description "Implement piece-value: compute player i's value for a piece of cake. A piece is a list of disjoint intervals, each represented as a pair (a . b). For each interval, call cake-valuation to get the player's value for [a, b], then sum all interval values. Use fold-left with + and 0 as the accumulator, mapping over the piece's intervals. Extract interval endpoints with car and cdr. This is the fundamental valuation primitive — every fairness criterion (proportional?, envy-free?, equitable?) calls this to measure what each player actually received.")
    (context "Available: cake-valuation, car, cdr, fold-left, +, map, lambda. Sum of valuations over intervals.")
    (before "(define (piece-value c i piece)
  ;; TODO: sum player i's value over all intervals
  0)")
    (test-file "lattice/game-theory/test-fair-division.ss")
    (test-forms (";; Single interval on uniform cake
(let ([c (uniform-cake 2)]
      [p (piece-singleton 0.25 0.75)])
  (assert-equal 0.5 (piece-value c 0 p)))"
                 ";; Empty piece has zero value
(let ([c (uniform-cake 2)])
  (assert-equal 0 (piece-value c 0 piece-empty)))"
                 ";; Multi-interval piece sums values
(let ([c (uniform-cake 2)]
      [p (piece-merge (piece-singleton 0 0.3)
                      (piece-singleton 0.5 0.8))])
  (assert-true (< (abs (- (piece-value c 0 p) 0.6)) 0.01)))"
                 ";; Different players value same piece differently
(let* ([c (simple-cake-2)]
       [p (piece-singleton 0 0.5)])
  (assert-true (> (piece-value c 0 p) 0.9))
  (assert-true (< (piece-value c 1 p) 0.1)))"))
    (available-modules (prelude sort vec))
    (hints ("Map each interval to its value: (cake-valuation c i (car interval) (cdr interval))"
            "Sum with fold-left: (fold-left + 0 (map ... piece))"
            "car = left endpoint, cdr = right endpoint"))
    (reference-solution "(define (piece-value c i piece)
  (fold-left + 0
             (map (lambda (interval)
                    (cake-valuation c i (car interval) (cdr interval)))
                  piece)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement proportional?
  ;; ──────────────────────────────────────────────
  ;; Check if a division is proportional.

  (task
    (id "game-theory/fair-division/proportional-impl")
    (module fair-division)
    (tier 0)
    (type implement)
    (description "Implement proportional?: check if a cake division is proportional, meaning each of n players receives at least 1/n of their own total value. The threshold is (/ 1 n). For each player i from 0 to n-1: compute their total value for the whole cake, compute their value for the piece they received, compute their share as received/total, and check share >= threshold. Use a named let loop over player indices. Handle the edge case where a player's total value is 0 (treat as share = 1). Use a small tolerance (1e-6) for numerical precision from the trapezoidal integration.")
    (context "Available: cake-players, cake-total-value, piece-value, division-piece, /, >=, -, =, and, let*, let loop, if. Iterative check over all players.")
    (before "(define (proportional? c div)
  ;; TODO: does each player get >= 1/n of their value?
  #f)")
    (test-file "lattice/game-theory/test-fair-division.ss")
    (test-forms (";; Equal split of uniform cake is proportional
(let* ([c (uniform-cake 2)]
       [div (make-division 2)])
  (division-assign! div 0 (piece-singleton 0 0.5))
  (division-assign! div 1 (piece-singleton 0.5 1))
  (assert-true (proportional? c div)))"
                 ";; Unfair split is not proportional
(let* ([c (uniform-cake 2)]
       [div (make-division 2)])
  (division-assign! div 0 (piece-singleton 0 0.3))
  (division-assign! div 1 (piece-singleton 0.3 1))
  (assert-false (proportional? c div)))"
                 ";; Three-player equal split is proportional
(let* ([c (uniform-cake 3)]
       [div (make-division 3)])
  (division-assign! div 0 (piece-singleton 0 0.334))
  (division-assign! div 1 (piece-singleton 0.334 0.667))
  (division-assign! div 2 (piece-singleton 0.667 1))
  (assert-true (proportional? c div)))"))
    (available-modules (prelude sort vec))
    (hints ("Threshold: (/ 1 n) where n = (cake-players c)"
            "Loop from i=0 to n-1"
            "For each player: total = (cake-total-value c i), received = (piece-value c i (division-piece div i))"
            "Share = (if (= total 0) 1 (/ received total))"
            "Check: (>= share (- threshold 1e-6)) with tolerance"
            "Short-circuit with (and ... (loop (+ i 1)))"))
    (reference-solution "(define (proportional? c div)
  (let* ([n (cake-players c)]
         [threshold (/ 1 n)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (let* ([total (cake-total-value c i)]
                 [received (piece-value c i (division-piece div i))]
                 [share (if (= total 0) 1 (/ received total))])
            (and (>= share (- threshold 1e-6))
                 (loop (+ i 1))))))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement envy-free?
  ;; ──────────────────────────────────────────────
  ;; Check if no player prefers another's piece.

  (task
    (id "game-theory/fair-division/envy-free-impl")
    (module fair-division)
    (tier 0)
    (type implement)
    (description "Implement envy-free?: check if a cake division is envy-free, meaning no player values another player's piece more than their own. Nested loop: for each player i, compute their value for their own piece, then for every other player j (j != i), check that player i's value for j's piece does not exceed i's own piece value. If any player envies another, return #f. Use named let for both outer (over i) and inner (over j) loops. Skip when i = j. Use tolerance 1e-10 for numerical precision. Envy-freeness implies proportionality (for n >= 2) but is strictly stronger — a proportional division can still have envy.")
    (context "Available: cake-players, piece-value, division-piece, <=, +, =, and, let, if. Doubly-nested loop with short-circuit.")
    (before "(define (envy-free? c div)
  ;; TODO: does no player prefer another's piece?
  #f)")
    (test-file "lattice/game-theory/test-fair-division.ss")
    (test-forms (";; Opposing preferences, correct assignment -> no envy
(let* ([c (simple-cake-2)]
       [div (make-division 2)])
  (division-assign! div 0 (piece-singleton 0 0.5))
  (division-assign! div 1 (piece-singleton 0.5 1))
  (assert-true (envy-free? c div)))"
                 ";; Wrong assignment -> envy
(let* ([c (simple-cake-2)]
       [div (make-division 2)])
  (division-assign! div 0 (piece-singleton 0.5 1))
  (division-assign! div 1 (piece-singleton 0 0.5))
  (assert-false (envy-free? c div)))"
                 ";; Equal split of uniform cake is envy-free
(let* ([c (uniform-cake 2)]
       [div (make-division 2)])
  (division-assign! div 0 (piece-singleton 0 0.5))
  (division-assign! div 1 (piece-singleton 0.5 1))
  (assert-true (envy-free? c div)))"))
    (available-modules (prelude sort vec))
    (hints ("Outer loop over i from 0 to n-1"
            "my-value = (piece-value c i (division-piece div i))"
            "Inner loop over j from 0 to n-1, skip when i = j"
            "other-value = (piece-value c i (division-piece div j))"
            "Envy check: (<= other-value (+ my-value 1e-10))"
            "Short-circuit: (and check (inner (+ j 1)))"))
    (reference-solution "(define (envy-free? c div)
  (let ([n (cake-players c)])
    (let outer ([i 0])
      (if (>= i n)
          #t
          (let ([my-value (piece-value c i (division-piece div i))])
            (let inner ([j 0])
              (if (>= j n)
                  (outer (+ i 1))
                  (if (= i j)
                      (inner (+ j 1))
                      (let ([other-value (piece-value c i (division-piece div j))])
                        (and (<= other-value (+ my-value 1e-10))
                             (inner (+ j 1))))))))))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement argmax
  ;; ──────────────────────────────────────────────
  ;; Return index of maximum element in a list.

  (task
    (id "game-theory/fair-division/argmax-impl")
    (module fair-division)
    (tier 0)
    (type implement)
    (description "Implement argmax: return the index (0-based) of the maximum element in a non-empty list of reals. Use a named let loop starting from the second element (index 1), tracking the current maximum value and its index. For each element, if it exceeds the current max, update both max-val and max-idx. When the list is exhausted, return max-idx. Initialize with the first element (car lst) at index 0. This is used by the Selfridge-Conway protocol and round-robin allocation to find which piece or good a player prefers most.")
    (context "Available: car, cdr, null?, >, let loop, cond. Single-pass max-tracking loop.")
    (before "(define (argmax lst)
  ;; TODO: index of maximum element
  0)")
    (test-file "lattice/game-theory/test-fair-division.ss")
    (test-forms (";; Maximum at start
(assert-equal 0 (argmax '(10 5 3)))"
                 ";; Maximum at end
(assert-equal 2 (argmax '(1 2 7)))"
                 ";; Maximum in middle
(assert-equal 1 (argmax '(3 9 4)))"
                 ";; Single element
(assert-equal 0 (argmax '(42)))"))
    (available-modules (prelude sort vec))
    (hints ("Initialize: max-val = (car lst), max-idx = 0, start from (cdr lst) at i = 1"
            "Loop: (let loop ([lst (cdr lst)] [i 1] [max-val (car lst)] [max-idx 0]) ...)"
            "Base: (null? lst) -> max-idx"
            "Update: (> (car lst) max-val) -> loop with new max"
            "Otherwise: loop with same max"))
    (reference-solution "(define (argmax lst)
  (let loop ([lst (cdr lst)] [i 1] [max-val (car lst)] [max-idx 0])
    (cond
      [(null? lst) max-idx]
      [(> (car lst) max-val) (loop (cdr lst) (+ i 1) (car lst) i)]
      [else (loop (cdr lst) (+ i 1) max-val max-idx)])))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement discrete-allocation-value
  ;; ──────────────────────────────────────────────
  ;; Total value a player assigns to a bundle of goods.

  (task
    (id "game-theory/fair-division/discrete-alloc-value-impl")
    (module fair-division)
    (tier 0)
    (type implement)
    (description "Implement discrete-allocation-value: compute the total value player i assigns to a bundle (list of good symbols). For each good symbol in the bundle, find its index in the goods vector (by linear search), look up the player's valuation at that index, and sum all values. Use fold-left with + to accumulate. The inner search is a named let loop over the goods vector indices: if the good symbol matches (via eq?), return the player's valuation at that index; if not found, return 0. This is used by round-robin, EF1 checking, and maximin share computation.")
    (context "Available: discrete-problem%-goods (vector of symbols), discrete-problem%-valuations (vector of vectors), vector-ref, vector-length, eq?, fold-left, +, map, lambda, let loop, if, >=. Linear search for symbol, then vector lookup.")
    (before "(define (discrete-allocation-value dp player bundle)
  ;; TODO: total value player assigns to bundle of goods
  0)")
    (test-file "lattice/game-theory/test-fair-division.ss")
    (test-forms (";; Single good
(let ([dp (make-discrete-problem '(A B C) '((10 20 30) (30 20 10)))])
  (assert-equal 10 (discrete-allocation-value dp 0 '(A))))"
                 ";; Multiple goods sum
(let ([dp (make-discrete-problem '(A B C) '((10 20 30) (30 20 10)))])
  (assert-equal 30 (discrete-allocation-value dp 0 '(A B))))"
                 ";; Different player values same bundle differently
(let ([dp (make-discrete-problem '(A B C) '((10 20 30) (30 20 10)))])
  (assert-equal 50 (discrete-allocation-value dp 1 '(A B))))"
                 ";; Empty bundle has zero value
(let ([dp (make-discrete-problem '(A B C) '((10 20 30) (30 20 10)))])
  (assert-equal 0 (discrete-allocation-value dp 0 '())))"))
    (available-modules (prelude sort vec))
    (hints ("Get goods vector and player's valuation vector from the record"
            "For each good g in bundle, search goods vector for matching symbol"
            "Inner loop: (let loop ([i 0]) (if (>= i (vector-length goods)) 0 (if (eq? g (vector-ref goods i)) (vector-ref vals i) (loop (+ i 1)))))"
            "Sum with fold-left: (fold-left + 0 (map (lambda (g) ...) bundle))"))
    (reference-solution "(define (discrete-allocation-value dp player bundle)
  (let ([goods (discrete-problem%-goods dp)]
        [vals (vector-ref (discrete-problem%-valuations dp) player)])
    (fold-left + 0
               (map (lambda (g)
                      (let loop ([i 0])
                        (if (>= i (vector-length goods))
                            0
                            (if (eq? g (vector-ref goods i))
                                (vector-ref vals i)
                                (loop (+ i 1))))))
                    bundle))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

) ;; end tasks
