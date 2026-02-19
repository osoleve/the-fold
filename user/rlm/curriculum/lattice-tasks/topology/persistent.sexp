;;; persistent.sexp — Development tasks for lattice/topology/persistent.ss
;;;
;;; Module: persistent (tier 1, depends on homology, simplicial-complex)
;;; 623 lines, ~20 exported functions
;;; Persistent homology for topological data analysis. Vietoris-Rips
;;; filtrations, standard persistence reduction over Z_2, persistence
;;; diagrams, barcodes, and bottleneck distance.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/topology/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement euclidean-distance
  ;; ──────────────────────────────────────────────
  ;; L2 distance between points.

  (task
    (id "topology/persistent/euclidean-distance-impl")
    (module persistent)
    (tier 1)
    (type implement)
    (description "Implement euclidean-distance: compute the Euclidean (L2) distance between two points represented as lists of coordinates. The formula is sqrt(Σ(aᵢ - bᵢ)²). Use map to compute (a-b)² for each pair of coordinates, apply + to sum them, and sqrt for the final distance. This is the default distance function for Vietoris-Rips complex construction.")
    (context "Available: sqrt, apply, +, map, *, -, lambda. Single composed expression.")
    (before "(define (euclidean-distance p1 p2)
  ;; TODO: sqrt(sum of squared differences)
  0)")
    (test-file "lattice/topology/test-persistent.ss")
    (test-forms (";; 3-4-5 right triangle
(assert-equal 5 (euclidean-distance '(0 0) '(3 4)))"
                 ";; Same point: distance 0
(assert-equal 0 (euclidean-distance '(1 2) '(1 2)))"
                 ";; 1D distance
(assert-equal 7 (euclidean-distance '(3) '(10)))"
                 ";; 3D distance: sqrt(1+4+9) = sqrt(14)
(let ([d (euclidean-distance '(0 0 0) '(1 2 3))])
  (assert-true (< (abs (- d (sqrt 14))) 1e-10)))"))
    (available-modules (prelude homology simplicial-complex))
    (hints ("Squared differences: (map (lambda (a b) (* (- a b) (- a b))) p1 p2)"
            "Sum: (apply + ...)"
            "Final: (sqrt (apply + (map ...)))"))
    (reference-solution "(define (euclidean-distance p1 p2)
  (sqrt (apply + (map (lambda (a b) (* (- a b) (- a b))) p1 p2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement lex<?
  ;; ──────────────────────────────────────────────
  ;; Lexicographic list comparison.

  (task
    (id "topology/persistent/lex-less-impl")
    (module persistent)
    (tier 1)
    (type implement)
    (description "Implement lex<?: lexicographic less-than comparison for two lists of numbers. Compare element by element from left to right. If the first elements differ, return the comparison result. If they're equal, recurse on the rest. An empty list is less than any non-empty list. Two empty lists are not less-than (they're equal). This ordering is used to canonically order simplices in a filtration when birth times and dimensions match.")
    (context "Available: null?, not, <, >, car, cdr, cond, else. Recursive with three base cases.")
    (before "(define (lex<? lst1 lst2)
  ;; TODO: lexicographic comparison
  #f)")
    (test-file "lattice/topology/test-persistent.ss")
    (test-forms (";; First elements differ
(assert-true (lex<? '(1 5 3) '(2 0 0)))"
                 ";; First elements equal, second differs
(assert-true (lex<? '(1 2 3) '(1 2 4)))"
                 ";; Greater is not less
(assert-false (lex<? '(1 2 4) '(1 2 3)))"
                 ";; Empty < non-empty
(assert-true (lex<? '() '(1)))"
                 ";; Equal lists: not less-than
(assert-false (lex<? '(1 2) '(1 2)))"
                 ";; Non-empty not < empty
(assert-false (lex<? '(1) '()))"))
    (available-modules (prelude homology simplicial-complex))
    (hints ("Base: (null? lst1) → (not (null? lst2)) — empty is less than non-empty"
            "Base: (null? lst2) → #f — nothing is less than empty"
            "Step: compare (car lst1) vs (car lst2), recurse on cdr if equal"))
    (reference-solution "(define (lex<? lst1 lst2)
  (cond
    [(null? lst1) (not (null? lst2))]
    [(null? lst2) #f]
    [(< (car lst1) (car lst2)) #t]
    [(> (car lst1) (car lst2)) #f]
    [else (lex<? (cdr lst1) (cdr lst2))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement symmetric-difference
  ;; ──────────────────────────────────────────────
  ;; XOR of sorted lists — the core of Z_2 column reduction.

  (task
    (id "topology/persistent/symmetric-difference-impl")
    (module persistent)
    (tier 1)
    (type implement)
    (description "Implement symmetric-difference: compute the XOR (symmetric difference) of two sorted descending integer lists. This is Z_2 addition of sparse column vectors — the core operation in the persistence reduction algorithm. Merge two sorted-descending lists: when elements match (appear in both), they cancel (XOR: 1+1=0 in Z_2). When they don't match, include them in the result. Build result in reverse using an accumulator, prepending the remaining list at each base case.")
    (context "Available: null?, =, >, car, cdr, cons, reverse, append, let, cond. Named let merge loop with accumulator.")
    (before "(define (symmetric-difference lst1 lst2)
  ;; TODO: XOR merge of two sorted-descending lists
  '())")
    (test-file "lattice/topology/test-persistent.ss")
    (test-forms (";; Matching elements cancel: (5 3 1) XOR (4 3 2) = (5 4 2 1)
(assert-equal '(5 4 2 1) (symmetric-difference '(5 3 1) '(4 3 2)))"
                 ";; Identical lists cancel completely
(assert-equal '() (symmetric-difference '(3 2 1) '(3 2 1)))"
                 ";; Disjoint lists merge
(assert-equal '(5 4 3 2 1 0) (symmetric-difference '(5 3 1) '(4 2 0)))"
                 ";; One empty
(assert-equal '(3 2 1) (symmetric-difference '(3 2 1) '()))"
                 ";; Both empty
(assert-equal '() (symmetric-difference '() '()))"))
    (available-modules (prelude homology simplicial-complex))
    (hints ("Named let: (let loop ([l1 lst1] [l2 lst2] [acc '()]) ...)"
            "Both empty → (append (reverse acc) l2) [or l1]"
            "Equal heads → cancel: (loop (cdr l1) (cdr l2) acc)"
            "l1 head > l2 head → include l1 head: (loop (cdr l1) l2 (cons (car l1) acc))"
            "Otherwise → include l2 head"))
    (reference-solution "(define (symmetric-difference lst1 lst2)
  (let loop ([l1 lst1] [l2 lst2] [acc '()])
    (cond
      [(null? l1) (append (reverse acc) l2)]
      [(null? l2) (append (reverse acc) l1)]
      [(= (car l1) (car l2))
       (loop (cdr l1) (cdr l2) acc)]
      [(> (car l1) (car l2))
       (loop (cdr l1) l2 (cons (car l1) acc))]
      [else
       (loop l1 (cdr l2) (cons (car l2) acc))])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement diagonal-distance
  ;; ──────────────────────────────────────────────
  ;; Distance from persistence point to diagonal.

  (task
    (id "topology/persistent/diagonal-distance-impl")
    (module persistent)
    (tier 1)
    (type implement)
    (description "Implement diagonal-distance: compute the distance from a persistence point to the diagonal. In a persistence diagram, each point (birth, death) represents a topological feature. The diagonal is where birth = death (zero persistence). The distance from a point to the diagonal is (death - birth) / 2 — this measures half the persistence of the feature. Points close to the diagonal are 'noisy' features; points far from it are 'significant' features. A persistence point is a 2-element list (birth death).")
    (context "Available: car, cadr, -, /. Single arithmetic expression.")
    (before "(define (diagonal-distance pt)
  ;; TODO: (death - birth) / 2
  0)")
    (test-file "lattice/topology/test-persistent.ss")
    (test-forms (";; Feature born at 1, dies at 5: distance = 2
(assert-equal 2 (diagonal-distance '(1 5)))"
                 ";; Born and dies at same time: distance = 0
(assert-equal 0 (diagonal-distance '(3 3)))"
                 ";; Large persistence
(assert-equal 5.0 (diagonal-distance '(0.0 10.0)))"))
    (available-modules (prelude homology simplicial-complex))
    (hints ("Birth: (car pt), Death: (cadr pt)"
            "Distance: (/ (- death birth) 2)"))
    (reference-solution "(define (diagonal-distance pt)
  (/ (- (cadr pt) (car pt)) 2))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement diagram-betti
  ;; ──────────────────────────────────────────────
  ;; Count features alive at a given time.

  (task
    (id "topology/persistent/diagram-betti-impl")
    (module persistent)
    (tier 1)
    (type implement)
    (description "Implement diagram-betti: compute the Betti number β_k at time t from a persistence diagram. The Betti number counts how many k-dimensional features are 'alive' at time t — born at or before t and dying strictly after t (birth ≤ t < death). Each point in the diagram is a triple (birth death dim). Filter for points matching the requested dimension where birth ≤ time and death > time, then count them. This gives the time-varying topology: how many connected components (β₀), loops (β₁), voids (β₂) exist at each moment.")
    (context "Available: diagram-points, length, filter, lambda, and, =, <=, >, car, cadr, caddr. One filter + length.")
    (before "(define (diagram-betti pd dim time)
  ;; TODO: count features with birth <= time < death in dimension dim
  0)")
    (test-file "lattice/topology/test-persistent.ss")
    (test-forms (";; Triangle filtration: at t=0.5, 3 vertices → β₀ = 3
(let* ([s0 (make-simplex '(0))]
       [s1 (make-simplex '(1))]
       [s2 (make-simplex '(2))]
       [e01 (make-simplex '(0 1))]
       [e12 (make-simplex '(1 2))]
       [e02 (make-simplex '(0 2))]
       [tri (make-simplex '(0 1 2))]
       [filt (make-filtration
               (list (cons s0 0) (cons s1 0) (cons s2 0)
                     (cons e01 1) (cons e12 2) (cons e02 3)
                     (cons tri 4)))]
       [result (persistence-reduce filt)]
       [pd (make-persistence-diagram result)])
  (assert-equal 3 (diagram-betti pd 0 0.5))
  ;; After first edge: β₀ = 2
  (assert-equal 2 (diagram-betti pd 0 1.5))
  ;; After all edges, before triangle: loop exists → β₁ = 1
  (assert-equal 1 (diagram-betti pd 1 3.5))
  ;; After triangle fills loop: β₁ = 0
  (assert-equal 0 (diagram-betti pd 1 4.5)))"))
    (available-modules (prelude homology simplicial-complex))
    (hints ("Get all points: (diagram-points pd)"
            "Filter: matching dim, birth ≤ time, death > time"
            "Each point is (birth death dim): (caddr pt) for dim, (car pt) for birth, (cadr pt) for death"
            "Count: (length (filter ...))"))
    (reference-solution "(define (diagram-betti pd dim time)
  (length (filter (lambda (pt)
                    (and (= (caddr pt) dim)
                         (<= (car pt) time)
                         (> (cadr pt) time)))
                  (diagram-points pd))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
