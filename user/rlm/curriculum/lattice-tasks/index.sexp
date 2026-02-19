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

  ;; Phase 10 — Computational Geometry (5 tasks total)
  (modules
    (convex-hull  0 (prelude mesh-gen sort)  "geometry/convex-hull.sexp"  complete))  ;; 5 tasks

  ;; Phase 11 — Number Theory II (5 tasks total)
  (modules
    (primality  0 (prelude sort modular)  "number-theory/primality.sexp"  complete))  ;; 5 tasks

  ;; Phase 12 — Signal Processing (5 tasks total)
  (modules
    (dft  0 (prelude complex vec)  "numeric/dft.sexp"  complete))  ;; 5 tasks

  ;; Phase 13 — Cryptography (5 tasks total)
  (modules
    (sha512  0 ()  "crypto/sha512.sexp"  complete))  ;; 5 tasks

  ;; Phase 14 — Signal Processing II (5 tasks total)
  (modules
    (window-functions  0 (prelude)  "numeric/window-functions.sexp"  complete))  ;; 5 tasks

  ;; Phase 15 — Signal Processing III (5 tasks total)
  (modules
    (convolution  0 (prelude complex dft vec)  "numeric/convolution.sexp"  complete))  ;; 5 tasks

  ;; Phase 16 — Transcendental Functions (5 tasks total)
  (modules
    (transcendental  0 (prelude)  "numeric/transcendental.sexp"  complete))  ;; 5 tasks

  ;; Phase 17 — Cryptography II (5 tasks total)
  (modules
    (blake2b  0 ()  "crypto/blake2b.sexp"  complete))  ;; 5 tasks

  ;; Phase 18 — Information Theory II (5 tasks total)
  (modules
    (channel-capacity  0 (entropy)  "info/channel-capacity.sexp"  complete))  ;; 5 tasks

  ;; Phase 19 — Information Theory III (5 tasks total)
  (modules
    (statistical-measures  0 (prelude entropy transcendental)  "info/statistical-measures.sexp"  complete))  ;; 5 tasks

  ;; Phase 20 — Algebra II (5 tasks total)
  (modules
    (tropical  0 (prelude matrix)  "algebra/tropical.sexp"  complete))  ;; 5 tasks

  ;; Phase 21 — Topology II (5 tasks total)
  (modules
    (homology  1 (simplicial-complex)  "topology/homology.sexp"  complete))  ;; 5 tasks

  ;; Phase 22 — Signal Processing IV (5 tasks total)
  (modules
    (wavelet  0 (prelude vec)  "numeric/wavelet.sexp"  complete))  ;; 5 tasks

  ;; Phase 23 — E-Graphs (5 tasks total)
  (modules
    (egraph/union-find  0 (prelude)  "egraph/union-find.sexp"  complete))  ;; 5 tasks

  ;; Phase 24 — Coding Theory (5 tasks total)
  (modules
    (coding  0 (entropy sort)  "info/coding.sexp"  complete))  ;; 5 tasks

  ;; Phase 25 — Spectral Analysis (5 tasks total)
  (modules
    (spectral-analysis  0 (prelude complex dft window-functions)  "numeric/spectral-analysis.sexp"  complete))  ;; 5 tasks

  ;; Phase 26 — Information Theory IV (5 tasks total)
  (modules
    (rate-distortion  0 (entropy sort)  "info/rate-distortion.sexp"  complete))  ;; 5 tasks

  ;; Phase 27 — Statistics (5 tasks total)
  (modules
    (summary-stats  0 (prelude sort)  "statistics/summary-stats.sexp"  complete))  ;; 5 tasks

  ;; Phase 28 — Special Functions (5 tasks total)
  (modules
    (special-functions  0 (prelude)  "fp/special-functions.sexp"  complete))  ;; 5 tasks

  ;; Phase 29 — Lock-Free Data Structures (5 tasks total)
  (modules
    (chase-lev-deque  0 (prelude)  "data/chase-lev-deque.sexp"  complete))  ;; 5 tasks

  ;; Phase 30 — Control Systems (5 tasks total)
  (modules
    (kalman  0 (prelude)  "control-systems/kalman.sexp"  complete))  ;; 5 tasks

  ;; Phase 31 — Game Theory (5 tasks total)
  (modules
    (normal-form  0 (prelude)  "game-theory/normal-form.sexp"  complete))  ;; 5 tasks

  ;; Phase 32+ entries will be added as we review modules
