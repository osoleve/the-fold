;;; index.sexp — Module DAG for the lattice development task curriculum
;;;
;;; Each entry: (module-name tier (deps ...) task-file status)
;;; Status: pending | in-progress | complete
;;; The DAG must be closed under dependencies — every dep must appear.

(curriculum
  (name "Lattice Development Tasks")
  (version 1)
  (description "Hand-curated development tasks for training Fold-native models.
Each module has 3-7 tasks spanning implement/fix/extend/optimize/refactor/test types.
Authored in S-expressions, exportable to JSONL for Verifiers/Prime Lab.")

  ;; Phase 1 — Data Structures (24 tasks total)
  (modules
    (sort      0 ()        "data/sort.sexp"      complete)   ;; 6 tasks
    (dict      0 ()        "data/dict.sexp"      complete)   ;; 4 tasks
    (set       0 ()        "data/set.sexp"       complete)   ;; 4 tasks
    (avl-tree  0 ()        "data/avl-tree.sexp"  complete)   ;; 5 tasks
    (heap      0 ()        "data/heap.sexp"      complete))  ;; 5 tasks

  ;; Phase 3 — Algebra (19 tasks total)
  (modules
    (group             0 (prelude)       "algebra/group.sexp"      complete)   ;; 5 tasks
    (ring              0 (prelude group) "algebra/ring.sexp"       complete)   ;; 5 tasks
    (field             1 (prelude ring)  "algebra/field.sexp"      complete)   ;; 4 tasks
    (algebra/polynomial 2 (prelude field) "algebra/polynomial.sexp" complete))  ;; 5 tasks

  ;; Phase 2 — Linear Algebra (19 tasks total)
  (modules
    (vec             0 (prelude)                          "linalg/vec.sexp"             complete)   ;; 5 tasks
    (matrix          0 (prelude vec)                      "linalg/matrix.sexp"          complete)   ;; 5 tasks
    (matrix-decomp   1 (prelude vec matrix)               "linalg/matrix-decomp.sexp"   complete)   ;; 4 tasks
    (matrix-solvers  1 (prelude vec matrix matrix-decomp) "linalg/matrix-solvers.sexp"  complete))) ;; 5 tasks

  ;; Phase 4 — Number Theory (5 tasks total)
  (modules
    (modular  0 (prelude)  "number-theory/modular.sexp"  complete))  ;; 5 tasks

  ;; Phase 5 — Numeric (5 tasks total)
  (modules
    (complex  0 (prelude transcendental)  "numeric/complex.sexp"  complete))  ;; 5 tasks

  ;; Phase 6 — Information Theory (5 tasks total)
  (modules
    (entropy  0 (prelude transcendental)  "info/entropy.sexp"  complete))  ;; 5 tasks

  ;; Phase 7 — Spatial Data Structures (5 tasks total)
  (modules
    (kdtree  0 (prelude sort heap)  "data/kdtree.sexp"  complete))  ;; 5 tasks

  ;; Phase 8 — Topology (5 tasks total)
  (modules
    (simplicial-complex  1 (set sort)  "topology/simplicial-complex.sexp"  complete))  ;; 5 tasks

  ;; Phase 9 — Spatial Data Structures II (5 tasks total)
  (modules
    (quadtree  0 (prelude sort heap)  "data/quadtree.sexp"  complete))  ;; 5 tasks

  ;; Phase 10+ entries will be added as we review modules
