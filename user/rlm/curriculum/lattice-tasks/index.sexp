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
    (heap      0 ()        "data/heap.sexp"      complete))) ;; 5 tasks

  ;; Phase 2, 3, 4 entries will be added as we review modules
