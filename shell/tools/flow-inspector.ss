;;; shell/tools/flow-inspector.ss — Effect + Flow Inspector
;;;
;;; Trace effectful flows and data dependencies in The Fold:
;;;   (trace-flow 'sym)      - Trace call paths with effect annotations
;;;   (show-effects 'sym)    - Show effect analysis for a symbol
;;;   (effect-boundary 'sym) - Find pure/impure transitions
;;;   (call-slice 'sym)      - Effect-filtered call graph slice
;;;
;;; This is Shell code: performs analysis with I/O.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - effect-lint.ss (infer-effect, effect-join)
;;;   - call-graph.ss (call-graph-callers, call-graph-callees)
;;;   - index.ss (index-lookup for file:line info)

(load "core/base/prelude.ss")
(load "shell/tools/effect-lint.ss")
(load "shell/lens/call-graph.ss")
(load "core/lang/index.ss")

;;; ============================================================
;;; Data Structures
;;; ============================================================

;;; Flow node: represents a symbol with its effect
;;; ((symbol . Symbol)
;;;  (effect . Effect)
;;;  (file . String | #f)
;;;  (line . Nat | #f))

(define (make-flow-node symbol effect file line)
  `((symbol . ,symbol)
    (effect . ,effect)
    (file . ,file)
    (line . ,line)))

(define (flow-node-symbol n) (cdr (assq 'symbol n)))
(define (flow-node-effect n) (cdr (assq 'effect n)))
(define (flow-node-file n) (cdr (assq 'file n)))
(define (flow-node-line n) (cdr (assq 'line n)))

;;; Effect boundary: transition from one effect to another
;;; ((from . Symbol)
;;;  (to . Symbol)
;;;  (from-effect . Effect)
;;;  (to-effect . Effect))

(define (make-effect-boundary from to from-effect to-effect)
  `((from . ,from)
    (to . ,to)
    (from-effect . ,from-effect)
    (to-effect . ,to-effect)))

(define (boundary-from b) (cdr (assq 'from b)))
(define (boundary-to b) (cdr (assq 'to b)))
(define (boundary-from-effect b) (cdr (assq 'from-effect b)))
(define (boundary-to-effect b) (cdr (assq 'to-effect b)))

;;; ============================================================
;;; Effect Lookup
;;; ============================================================

;;; get-symbol-effect : Symbol → Effect
;;; Get the effect of a symbol using the effect database.
(define (get-symbol-effect sym)
  (lookup-effect sym))

;;; get-symbol-location : Symbol → (String × Nat) | (#f × #f)
;;; Get file and line for a symbol from the index.
(define (get-symbol-location sym)
  (let ([entry (index-lookup sym)])
       (if entry
           (let ([file (cdr (assq 'file entry))]
                 [line (cdr (assq 'line entry))])
                (values (or file #f) (or line #f)))
           (values #f #f))))

;;; ============================================================
;;; Flow Graph Building
;;; ============================================================

;;; build-flow-tree : Symbol × Nat × (HashTable Symbol) → FlowTree
;;; Build a tree of call relationships with effect annotations.
;;; Uses fuel to limit traversal depth.
(define (build-flow-tree root fuel visited)
  (if (or (<= fuel 0) (hashtable-ref visited root #f))
      #f
      (begin
       (hashtable-set! visited root #t)
       (let-values ([(file line) (get-symbol-location root)])
                   (let* ([effect (get-symbol-effect root)]
                          [callees (call-graph-callees root)]
                          [children (filter-map
                                     (lambda (c)
                                             (build-flow-tree c (- fuel 1) visited))
                                     callees)])
                         `((node . ,(make-flow-node root effect file line))
                           (children . ,children)))))))

;;; ============================================================
;;; Effect Boundary Detection
;;; ============================================================

;;; find-boundaries-in-tree : FlowTree × Effect → (List EffectBoundary)
;;; Walk the tree and collect effect boundaries.
(define (find-boundaries-in-tree tree parent-effect)
  (if (not tree)
      '()
      (let* ([node (cdr (assq 'node tree))]
             [children (cdr (assq 'children tree))]
             [sym (flow-node-symbol node)]
             [effect (flow-node-effect node)]
             [is-boundary? (and (not (eq? parent-effect 'unknown))
                                (not (eq? effect 'unknown))
                                (not (eq? parent-effect effect))
                                (or (eq? parent-effect 'pure)
                                    (eq? effect 'pure)))]
             [this-boundary (if is-boundary?
                                (list (make-effect-boundary
                                       'parent sym parent-effect effect))
                                '())]
             [child-boundaries (apply append
                                      (map (lambda (c)
                                                   (find-boundaries-in-tree c effect))
                                           children))])
            (append this-boundary child-boundaries))))

;;; find-effect-boundaries : Symbol × Nat → (List EffectBoundary)
;;; Find all effect boundaries reachable from a symbol.
(define (find-effect-boundaries root fuel)
  (let* ([visited (make-eq-hashtable)]
         [tree (build-flow-tree root fuel visited)])
        (if tree
            (find-boundaries-in-tree tree 'pure)
            '())))

;;; ============================================================
;;; Effect-Filtered Call Slicing
;;; ============================================================

;;; call-slice-by-effect : Symbol × Effect × Nat → (List (List Symbol))
;;; Get callees filtered by effect, organized by depth.
(define (call-slice-by-effect root target-effect max-depth)
  (let ([results (make-vector (+ max-depth 1) '())])
       (letrec
        ([visit
          (lambda (sym depth visited)
                  (when (and (<= depth max-depth)
                             (not (hashtable-ref visited sym #f)))
                        (hashtable-set! visited sym #t)
                        (let ([effect (get-symbol-effect sym)])
                             ;; Include if effect matches (or target is 'all)
                             (when (or (eq? target-effect 'all)
                                       (eq? effect target-effect))
                                   (vector-set! results depth
                                                (cons sym (vector-ref results depth))))
                             ;; Recurse to callees
                             (for-each
                              (lambda (c)
                                      (visit c (+ depth 1) visited))
                              (call-graph-callees sym)))))])
        (visit root 0 (make-eq-hashtable))
        ;; Convert vector to list of lists, removing empty levels
        (let loop ([i 0] [acc '()])
             (if (> i max-depth)
                 (reverse acc)
                 (let ([level (reverse (vector-ref results i))])
                      (loop (+ i 1)
                            (if (null? level)
                                acc
                                (cons (cons i level) acc)))))))))

;;; ============================================================
;;; REPL Commands
;;; ============================================================

;;; trace-flow : Symbol [Nat] → Void
;;; Trace effectful flows from a symbol.
(define trace-flow
  (case-lambda
   [(sym) (trace-flow sym 100)]
   [(sym fuel)
    (display "\n")
    (printf "  Flow Trace: ~a\n" sym)
    (display "  ═══════════════════════════════════════════════════\n\n")
    (let* ([visited (make-eq-hashtable)]
           [tree (build-flow-tree sym fuel visited)])
          (if tree
              (begin
               (display-flow-tree tree 0 #t)
               (display "\n")
               (let ([boundaries (find-boundaries-in-tree tree 'pure)])
                    (if (null? boundaries)
                        (display "  No effect boundaries found.\n")
                        (begin
                         (printf "  Boundaries: ~a\n" (length boundaries))
                         (for-each
                          (lambda (b)
                                  (printf "    ~a [~a] -> ~a [~a]\n"
                                          (boundary-from b)
                                          (boundary-from-effect b)
                                          (boundary-to b)
                                          (boundary-to-effect b)))
                          boundaries)))))
              (printf "  Symbol ~a not found in call graph.\n" sym)))
    (display "\n")]))

;;; display-flow-tree : FlowTree × Nat × Bool → Void
(define (display-flow-tree tree depth is-last)
  (when tree
        (let* ([node (cdr (assq 'node tree))]
               [children (cdr (assq 'children tree))]
               [sym (flow-node-symbol node)]
               [effect (flow-node-effect node)]
               [indent (make-indent depth is-last)]
               [effect-str (format "[~a]" effect)]
               [boundary-marker (if (and (> depth 0) (eq? effect 'io))
                                    " <- BOUNDARY"
                                    "")])
              (printf "  ~a~a ~a~a\n" indent sym effect-str boundary-marker)
              (let loop ([cs children] [idx 0])
                   (unless (null? cs)
                           (display-flow-tree (car cs)
                                              (+ depth 1)
                                              (null? (cdr cs)))
                           (loop (cdr cs) (+ idx 1)))))))

;;; make-indent : Nat × Bool → String
(define (make-indent depth is-last)
  (if (= depth 0)
      ""
      (string-append
       (apply string-append (make-list (- depth 1) "│   "))
       (if is-last "└─> " "├─> "))))

;;; show-effects : Symbol → Void
;;; Show detailed effect analysis for a symbol.
(define (show-effects sym)
  (display "\n")
  (printf "  Effect Analysis: ~a\n" sym)
  (display "  ═══════════════════════════════════════════════════\n\n")
  (let* ([effect (get-symbol-effect sym)]
         [callees (call-graph-callees sym)]
         [callee-effects (map (lambda (c) (cons c (get-symbol-effect c))) callees)])
        (printf "  Inferred Effect: ~a\n\n" effect)
        (if (null? callees)
            (display "  No direct callees.\n")
            (begin
             (display "  Direct Callees:\n")
             (for-each
              (lambda (ce)
                      (printf "    ~a~a~a\n"
                              (car ce)
                              (make-string (max 1 (- 20 (string-length (symbol->string (car ce))))) #\space)
                              (cdr ce)))
              callee-effects)
             (display "\n")
             ;; Show effect composition
             (let ([effects (map cdr callee-effects)])
                  (unless (null? effects)
                          (let ([composed (effects-join effects)])
                               (printf "  Effect Composition: ~a = ~a\n"
                                       (join-effect-strings (map symbol->string effects))
                                       composed)))))))
  (display "\n"))

;;; join-effect-strings : (List String) → String
(define (join-effect-strings strs)
  (if (null? strs)
      "pure"
      (fold-left (lambda (acc s) (string-append acc " ⊔ " s))
                 (car strs)
                 (cdr strs))))

;;; effect-boundary : Symbol → Void
;;; Find and display effect boundaries around a symbol.
(define (effect-boundary sym)
  (display "\n")
  (printf "  Effect Boundaries: ~a\n" sym)
  (display "  ═══════════════════════════════════════════════════\n\n")
  (let* ([effect (get-symbol-effect sym)]
         [callers (call-graph-callers sym)]
         [callees (call-graph-callees sym)])
        (printf "  Symbol Effect: ~a\n" effect)
        (let-values ([(file line) (get-symbol-location sym)])
                    (when file
                          (printf "  Location: ~a:~a\n" file (or line "?"))))
        (display "\n")
        ;; Show callers that transition TO this effect
        (let ([incoming (filter
                         (lambda (c)
                                 (let ([c-effect (get-symbol-effect c)])
                                      (and (not (eq? c-effect effect))
                                           (or (eq? c-effect 'pure)
                                               (eq? effect 'pure)))))
                         callers)])
             (if (null? incoming)
                 (display "  No incoming effect boundaries.\n")
                 (begin
                  (display "  Incoming Boundaries:\n")
                  (for-each
                   (lambda (c)
                           (let-values ([(f l) (get-symbol-location c)])
                                       (printf "    ~a [~a]~a\n"
                                               c
                                               (get-symbol-effect c)
                                               (if f (format " ~a:~a" f (or l "?")) ""))))
                   incoming))))
        (display "\n")
        ;; Show callees
        (if (null? callees)
            (display "  No callees.\n")
            (begin
             (display "  Callees:\n")
             (for-each
              (lambda (c)
                      (printf "    ~a~a~a\n"
                              c
                              (make-string (max 1 (- 20 (string-length (symbol->string c)))) #\space)
                              (get-symbol-effect c)))
              callees))))
  (display "\n"))

;;; call-slice : Symbol [Effect] [Nat] → Void
;;; Display call graph slice filtered by effect.
(define call-slice
  (case-lambda
   [(sym) (call-slice sym 'all 5)]
   [(sym effect) (call-slice sym effect 5)]
   [(sym effect depth)
    (display "\n")
    (printf "  Call Slice: ~a (filter: ~a, depth: ~a)\n" sym effect depth)
    (display "  ═══════════════════════════════════════════════════\n\n")
    (let ([slices (call-slice-by-effect sym effect depth)])
         (if (null? slices)
             (printf "  No callees~a found.\n"
                     (if (eq? effect 'all) "" (format " with effect ~a" effect)))
             (begin
              (for-each
               (lambda (level)
                       (let ([d (car level)]
                             [syms (cdr level)])
                            (printf "  Depth ~a:\n" d)
                            (for-each
                             (lambda (s)
                                     (printf "    ~a~a~a\n"
                                             s
                                             (make-string (max 1 (- 20 (string-length (symbol->string s)))) #\space)
                                             (get-symbol-effect s)))
                             syms)
                            (display "\n")))
               slices)
              (let ([total (apply + (map (lambda (l) (length (cdr l))) slices))])
                   (printf "  Total: ~a symbol~a\n" total (if (= total 1) "" "s"))))))
    (display "\n")]))

;;; ============================================================
;;; Help
;;; ============================================================

(define (flow-inspector-help)
  (display "\n")
  (display "  Effect + Flow Inspector Commands\n")
  (display "  ────────────────────────────────────────────\n")
  (display "  (trace-flow 'sym [fuel])  - Trace call paths with effects\n")
  (display "  (show-effects 'sym)       - Show effect analysis\n")
  (display "  (effect-boundary 'sym)    - Find effect transitions\n")
  (display "  (call-slice 'sym [effect] [depth]) - Effect-filtered slice\n")
  (display "\n")
  (display "  Effects:\n")
  (display "    pure       - No side effects\n")
  (display "    io         - Input/output operations\n")
  (display "    mutation   - State modification\n")
  (display "    exception  - May throw errors\n")
  (display "    unknown    - Effect not determined\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (trace-flow 'my-function)         ;; Show call tree with effects\n")
  (display "    (call-slice 'my-function 'pure 3) ;; Pure callees, depth 3\n")
  (display "    (effect-boundary 'do-io)          ;; Where pure->io happens\n")
  (display "\n"))

;;; filter-map helper (if not already defined)
(define (filter-map* f lst)
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

;;; make-list helper
(define (make-list* n val)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           acc
           (loop (+ i 1) (cons val acc)))))

(display "Flow inspector loaded. Use (flow-inspector-help) for usage.\n")
