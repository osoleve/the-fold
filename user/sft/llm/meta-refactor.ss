;;; user/sft/llm/meta-refactor.ss — Refactoring toolkit SFT task generator
;;;
;;; Generates SFT samples teaching the refactoring toolkit:
;;;   - Rename: preview + apply symbol rename across codebase
;;;   - Deps: query callers/callees of a symbol
;;;   - Dead code: scan for unused definitions
;;;   - Move: plan cross-module symbol relocation
;;;   - Multi-step: compose refactoring operations
;;;
;;; Seeds use real lattice symbols for realistic scenarios.
;;; Verification: commands execute without error in toolkit-loaded session.
;;;
;;; Usage:
;;;   (load "user/sft/llm/meta-refactor.ss")
;;;   (define seeds (meta-refactor/generate-seeds))

(load "user/sft/extract/emit.ss")

;;; ====
;;; Symbol Archetypes
;;; ====
;;; Real lattice symbols with known cross-references, grouped by skill.
;;; Each archetype provides enough context for multiple task types.

(define *mr-archetypes*
  `(;; --- linalg: widely used vector/matrix ops ---
    ((skill . linalg)
     (symbols
       . ((make-vec
            (module . "lattice/linalg/vec.ss")
            (type . "(-> Nat a (Vec a))")
            (callers . (vec-add vec-sub vec-dot vec-scale))
            (deps . (make-vector))
            (rename-to . create-vector)
            (move-to . "lattice/linalg/vec-core.ss"))
          (matrix-multiply
            (module . "lattice/linalg/matrix.ss")
            (type . "(-> Matrix Matrix Matrix)")
            (callers . (matrix-power matrix-solve lu-decompose))
            (deps . (matrix-ref matrix-set! make-matrix vec-dot))
            (rename-to . mat-mul)
            (move-to . "lattice/linalg/mat-ops.ss"))
          (vec-dot
            (module . "lattice/linalg/vec.ss")
            (type . "(-> (Vec Num) (Vec Num) Num)")
            (callers . (matrix-multiply vec-normalize vec-project))
            (deps . (vec-ref vec-length))
            (rename-to . vector-dot-product)
            (move-to . "lattice/linalg/vec-ops.ss")))))

    ;; --- fp: functional programming building blocks ---
    ((skill . fp)
     (symbols
       . ((maybe-bind
            (module . "lattice/fp/maybe.ss")
            (type . "(-> (Maybe a) (-> a (Maybe b)) (Maybe b))")
            (callers . (maybe-chain maybe-sequence))
            (deps . (maybe-just? maybe-value))
            (rename-to . maybe-flat-map)
            (move-to . "lattice/fp/maybe-core.ss"))
          (parser-bind
            (module . "lattice/fp/parser.ss")
            (type . "(-> (Parser a) (-> a (Parser b)) (Parser b))")
            (callers . (parser-sequence parser-many parser-choice))
            (deps . (parser-run make-parser))
            (rename-to . parser-chain)
            (move-to . "lattice/fp/parser-core.ss"))
          (stream-take
            (module . "lattice/fp/stream.ss")
            (type . "(-> Nat (Stream a) (List a))")
            (callers . (stream-to-list))
            (deps . (stream-null? stream-car stream-cdr))
            (rename-to . stream-first-n)
            (move-to . "lattice/fp/stream-ops.ss")))))

    ;; --- data: fundamental data structures ---
    ((skill . data)
     (symbols
       . ((hamt-ref
            (module . "lattice/data/hamt.ss")
            (type . "(-> HAMT Key (Maybe Value))")
            (callers . (hamt-contains? hamt-update))
            (deps . (hamt-hash hamt-node-ref))
            (rename-to . hamt-lookup)
            (move-to . "lattice/data/hamt-core.ss"))
          (graph-neighbors
            (module . "lattice/data/graph.ss")
            (type . "(-> Graph Node (List Node))")
            (callers . (graph-bfs graph-dfs shortest-path))
            (deps . (graph-edges graph-has-node?))
            (rename-to . graph-adjacent)
            (move-to . "lattice/data/graph-traversal.ss"))
          (heap-insert
            (module . "lattice/data/heap.ss")
            (type . "(-> Heap a Heap)")
            (callers . (heap-from-list priority-queue-push))
            (deps . (heap-merge make-heap))
            (rename-to . heap-push)
            (move-to . "lattice/data/heap-core.ss")))))

    ;; --- optics: lens/prism operations ---
    ((skill . optics)
     (symbols
       . ((lens-view
            (module . "lattice/optics/lens.ss")
            (type . "(-> Lens s a)")
            (callers . (lens-modify lens-set traversal-to-list))
            (deps . (lens-get))
            (rename-to . view)
            (move-to . "lattice/optics/lens-core.ss"))
          (prism-review
            (module . "lattice/optics/prism.ss")
            (type . "(-> Prism b t)")
            (callers . (prism-compose iso-to-prism))
            (deps . (prism-match))
            (rename-to . prism-build)
            (move-to . "lattice/optics/prism-core.ss")))))))

;;; ====
;;; Task Type Generators
;;; ====

(define *mr-counter* 0)

(define (mr/next-id)
  (set! *mr-counter* (+ *mr-counter* 1))
  (format "meta_refactor_~3,'0d" *mr-counter*))

(define (mr/sym-ref sym-spec key)
  (let ([entry (assq key (cdr sym-spec))])
    (if entry (cdr entry) #f)))

(define (mr/generate-rename skill sym-spec)
  (doc 'description "Generate: rename symbol with preview + verification")
  (let* ([sym-name (car sym-spec)]
         [module (mr/sym-ref sym-spec 'module)]
         [type-str (mr/sym-ref sym-spec 'type)]
         [callers (mr/sym-ref sym-spec 'callers)]
         [new-name (mr/sym-ref sym-spec 'rename-to)])
    (let* ([prompt-body
            (format "Using The Fold's refactoring toolkit, rename the symbol `~a` to `~a`.

This symbol is defined in `~a` with type `~a`.
It is called by: ~a.

Show the refactoring preview, then apply the changes. Explain what each step does."
                    sym-name new-name module type-str callers)]
           [ground-truth
            (format ";; Load the refactoring toolkit
(load \"boundary/tools/refactor-toolkit.ss\")

;; Preview the rename — shows all files and locations affected
(refactor 'rename '~a '~a)

;; Review the changes
(refactor 'status)

;; Apply all pending changes
(refactor 'apply)"
                    sym-name new-name)]
           [verify-sexp
            `(guard (e [else #f])
               (begin
                 (refactor 'rename ',sym-name ',new-name)
                 #t))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mr/next-id)]
           [tags (list (symbol->string skill) (symbol->string sym-name)
                       "meta_refactor" "rename")])
      (make-sample id "meta_refactor" "usage" "easy"
                   module ""
                   (symbol->string sym-name)
                   prompt-body ground-truth verify-str tags))))

(define (mr/generate-deps skill sym-spec)
  (doc 'description "Generate: query dependencies and callers of a symbol")
  (let* ([sym-name (car sym-spec)]
         [module (mr/sym-ref sym-spec 'module)]
         [type-str (mr/sym-ref sym-spec 'type)]
         [callers (mr/sym-ref sym-spec 'callers)]
         [deps (mr/sym-ref sym-spec 'deps)])
    (let* ([prompt-body
            (format "Using The Fold's refactoring toolkit, analyze the dependencies of `~a`.

Find out:
1. What functions call `~a` (its callers)
2. What functions `~a` depends on (its callees)
3. How many total references exist across the codebase

The symbol is defined in `~a`."
                    sym-name sym-name sym-name module)]
           [ground-truth
            (format ";; Load the refactoring toolkit
(load \"boundary/tools/refactor-toolkit.ss\")

;; Show full dependency analysis
(refactor 'deps '~a)

;; Expected callers include: ~a
;; Expected callees include: ~a"
                    sym-name callers deps)]
           [verify-sexp
            `(guard (e [else #f])
               (begin
                 (refactor 'deps ',sym-name)
                 #t))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mr/next-id)]
           [tags (list (symbol->string skill) (symbol->string sym-name)
                       "meta_refactor" "deps")])
      (make-sample id "meta_refactor" "analysis" "easy"
                   module ""
                   (symbol->string sym-name)
                   prompt-body ground-truth verify-str tags))))

(define (mr/generate-dead-code skill sym-spec)
  (doc 'description "Generate: dead code scan on a skill's directory")
  (let* ([sym-name (car sym-spec)]
         [module (mr/sym-ref sym-spec 'module)]
         ;; Extract directory from module path
         [skill-dir (let* ([parts (string-split module #\/)]
                           [dir-parts (list-head parts (- (length parts) 1))])
                      (string-join dir-parts "/"))])
    (let* ([prompt-body
            (format "Using The Fold's refactoring toolkit, scan the `~a` directory for dead code.

Dead code means definitions that are:
- Not exported by any module
- Not referenced by any other function in the codebase
- Not used in any test file

Report any unused definitions found. For each, show its location and suggest
whether it should be deleted or might be an accidentally unexported utility."
                    skill-dir)]
           [ground-truth
            (format ";; Load the refactoring toolkit
(load \"boundary/tools/refactor-toolkit.ss\")

;; Scan the skill directory for unused definitions
(refactor 'dead-code \"~a\")

;; For any results, get safe-delete analysis
;; (refactor 'dead-code-delete 'suspicious-symbol)"
                    skill-dir)]
           [verify-sexp
            `(guard (e [else #f])
               (begin
                 (refactor 'dead-code ,(format "~a" skill-dir))
                 #t))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mr/next-id)]
           [tags (list (symbol->string skill) skill-dir
                       "meta_refactor" "dead_code")])
      (make-sample id "meta_refactor" "analysis" "medium"
                   module ""
                   (symbol->string sym-name)
                   prompt-body ground-truth verify-str tags))))

(define (mr/generate-move skill sym-spec)
  (doc 'description "Generate: move symbol with dependency check + plan")
  (let* ([sym-name (car sym-spec)]
         [module (mr/sym-ref sym-spec 'module)]
         [callers (mr/sym-ref sym-spec 'callers)]
         [deps (mr/sym-ref sym-spec 'deps)]
         [move-to (mr/sym-ref sym-spec 'move-to)])
    (let* ([prompt-body
            (format "Using The Fold's refactoring toolkit, plan a move of `~a` from `~a` to `~a`.

Before moving, you need to:
1. Check all dependencies — what does `~a` call? Will those be available at the target?
2. Check all callers — what calls `~a`? They'll need updated imports.
3. Preview the move to see all affected files.
4. Apply if the preview looks correct.

Known callers: ~a
Known dependencies: ~a"
                    sym-name module move-to sym-name sym-name callers deps)]
           [ground-truth
            (format ";; Load the refactoring toolkit
(load \"boundary/tools/refactor-toolkit.ss\")

;; Step 1: Check dependencies first
(refactor 'deps '~a)
;; Verify all deps (~a) are available at target location

;; Step 2: Preview the move
(refactor 'move '~a \"~a\")
;; This shows:
;;   - The definition being moved
;;   - All files that reference ~a (callers: ~a)
;;   - Required import updates

;; Step 3: Review pending changes
(refactor 'status)

;; Step 4: Apply if correct
(refactor 'apply)"
                    sym-name deps sym-name move-to sym-name callers)]
           [verify-sexp
            `(guard (e [else #f])
               (and (begin (refactor 'deps ',sym-name) #t)
                    (begin (refactor 'move ',sym-name ,move-to) #t)))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mr/next-id)]
           [tags (list (symbol->string skill) (symbol->string sym-name)
                       "meta_refactor" "move")])
      (make-sample id "meta_refactor" "usage" "hard"
                   module ""
                   (symbol->string sym-name)
                   prompt-body ground-truth verify-str tags))))

(define (mr/generate-multi-step skill sym-spec)
  (doc 'description "Generate: multi-step refactoring workflow (deps → rename → verify)")
  (let* ([sym-name (car sym-spec)]
         [module (mr/sym-ref sym-spec 'module)]
         [callers (mr/sym-ref sym-spec 'callers)]
         [new-name (mr/sym-ref sym-spec 'rename-to)])
    (let* ([prompt-body
            (format "Perform a safe rename of `~a` to `~a` using the refactoring toolkit.

Follow this workflow:
1. First, analyze dependencies to understand the blast radius
2. Preview the rename to see all affected locations
3. Review the pending changes
4. Apply the rename
5. Verify no dead code was created by the rename (e.g., stale references)

Show each step with its output."
                    sym-name new-name)]
           [ground-truth
            (format ";; Safe rename workflow
(load \"boundary/tools/refactor-toolkit.ss\")

;; 1. Analyze blast radius
(refactor 'deps '~a)
;; Callers: ~a — all will be updated

;; 2. Preview rename
(refactor 'rename '~a '~a)

;; 3. Review changes
(refactor 'status)

;; 4. Apply
(refactor 'apply)

;; 5. Verify: scan for any stale references to old name
(refactor 'dead-code)"
                    sym-name callers sym-name new-name)]
           [verify-sexp
            `(guard (e [else #f])
               (begin
                 (refactor 'deps ',sym-name)
                 (refactor 'rename ',sym-name ',new-name)
                 #t))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mr/next-id)]
           [tags (list (symbol->string skill) (symbol->string sym-name)
                       "meta_refactor" "multi_step")])
      (make-sample id "meta_refactor" "usage" "hard"
                   module ""
                   (symbol->string sym-name)
                   prompt-body ground-truth verify-str tags))))

;;; ====
;;; String Utilities
;;; ====

(define (string-split str ch)
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(= i (string-length str))
       (reverse (cons (substring str start i) acc))]
      [(char=? (string-ref str i) ch)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
      [else (loop (+ i 1) start acc)])))

(define (string-join lst sep)
  (if (null? lst) ""
      (let loop ([rest (cdr lst)] [acc (car lst)])
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; ====
;;; Main Generator
;;; ====

(define (meta-refactor/generate-seeds)
  (doc 'description
    "Generate refactoring toolkit SFT samples from archetypes.
     Returns list of sample alists in standard schema.")
  (set! *mr-counter* 0)
  (printf "Generating refactoring toolkit seeds from ~a archetypes...\\n"
          (length *mr-archetypes*))
  (let loop ([archetypes *mr-archetypes*] [acc '()])
    (if (null? archetypes)
        (begin
          (printf "Generated ~a refactoring toolkit seeds\\n" (length acc))
          (reverse acc))
        (let* ([arch (car archetypes)]
               [skill (cdr (assq 'skill arch))]
               [symbols (cdr (assq 'symbols arch))]
               [new-samples
                (apply append
                  (map (lambda (sym-spec)
                         (let* ([s1 (guard (ex [else #f])
                                     (mr/generate-rename skill sym-spec))]
                                [s2 (guard (ex [else #f])
                                     (mr/generate-deps skill sym-spec))]
                                [s3 (guard (ex [else #f])
                                     (mr/generate-dead-code skill sym-spec))]
                                [s4 (guard (ex [else #f])
                                     (mr/generate-move skill sym-spec))]
                                [s5 (guard (ex [else #f])
                                     (mr/generate-multi-step skill sym-spec))])
                           (filter (lambda (x) x)
                                   (list s1 s2 s3 s4 s5))))
                       symbols))])
          (loop (cdr archetypes)
                (append (reverse new-samples) acc))))))

;;; ====
;;; REPL interface
;;; ====

(printf "meta-refactor.ss loaded.\\n")
(printf "  (meta-refactor/generate-seeds) - Generate refactoring toolkit seeds\\n")
