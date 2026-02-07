;;; user/rlm/gen-discovery.ss — Programmatic Lattice Discovery Trace Generator
;;;
;;; Reads all 36 skill manifests and generates three tiers of training traces:
;;;   Tier 1: Single-skill discovery (search→inspect→exports→load→eval→submit)
;;;   Tier 2: Multi-skill composition (problems needing dep + main skill)
;;;   Tier 3: Recovery traces (deliberate error→inspect→correct)
;;;
;;; Closes fold-zxvt, unblocks fold-zxvu (P1).
;;;
;;; Run: scheme --script user/rlm/gen-discovery.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "boundary/io/json.ss")

;;; ====
;;; Configuration
;;; ====

(define *disc-output-path* "user/rlm/training/discovery-traces.jsonl")
(define *disc-context-budget* 8000)

(define *disc-base-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task requires finding and using "
    "tools from the lattice (a library of verified computational skills).\n\n"
    "Strategy:\n"
    "1. (search \"keyword\") to find relevant skills\n"
    "2. (inspect 'skill-name) to learn about a skill\n"
    "3. (exports 'skill-name) to see available functions\n"
    "4. (load 'module-name) to load a specific module (NOT the skill name)\n"
    "5. (eval expr) to compute\n"
    "6. (submit result) to report the answer\n\n"
    "IMPORTANT: Skills and modules are different. A skill (e.g. 'linalg) "
    "contains multiple modules (e.g. 'linalg/vec, 'linalg/matrix). "
    "You must (load 'module), not (load 'skill).\n"))

;;; ====
;;; Manifest Reader
;;; ====

(define (disc-read-manifest path)
  (guard (ex [else #f])
    (with-input-from-file path read)))

(define (disc-find-manifests base-dir)
  (let ([entries (directory-list base-dir)])
    (let loop ([dirs entries] [results '()])
      (if (null? dirs)
          (reverse results)
          (let* ([d (car dirs)]
                 [dir-path (string-append base-dir "/" d)]
                 [manifest-path (string-append dir-path "/manifest.sexp")])
            ;; Always check sub-dirs (physics/ has no parent manifest)
            (let* ([parsed (and (file-exists? manifest-path)
                                (disc-read-manifest manifest-path))]
                   [sub-manifests (disc-find-sub-manifests dir-path)]
                   [all (if parsed
                            (cons (cons manifest-path parsed) sub-manifests)
                            sub-manifests)])
              (loop (cdr dirs) (append (reverse all) results))))))))

(define (disc-find-sub-manifests dir-path)
  (guard (ex [else '()])
    (let ([entries (directory-list dir-path)])
      (let loop ([dirs entries] [results '()])
        (if (null? dirs)
            (reverse results)
            (let* ([d (car dirs)]
                   [sub-path (string-append dir-path "/" d "/manifest.sexp")])
              (if (file-exists? sub-path)
                  (let ([parsed (disc-read-manifest sub-path)])
                    (loop (cdr dirs)
                          (if parsed
                              (cons (cons sub-path parsed) results)
                              results)))
                  (loop (cdr dirs) results))))))))

;;; ====
;;; Manifest Field Extractors
;;; ====

(define (mf-field manifest key)
  (let loop ([rest (cddr manifest)])  ; skip 'skill and name
    (cond
      [(null? rest) #f]
      [(and (pair? (car rest)) (eq? (caar rest) key))
       (cdar rest)]
      [else (loop (cdr rest))])))

(define (mf-name manifest) (cadr manifest))

(define (mf-description manifest)
  (let ([d (mf-field manifest 'description)])
    (if (and d (pair? d)) (car d) (or d ""))))

(define (mf-keywords manifest)
  (let ([k (mf-field manifest 'keywords)])
    (if (and k (pair? k) (pair? (car k))) (car k) (or k '()))))

(define (mf-deps manifest)
  (let ([d (mf-field manifest 'deps)])
    (cond
      [(not d) '()]
      ;; d = ((dep1 dep2 ...)) — unwrap one layer
      [(and (pair? d) (pair? (car d)) (symbol? (caar d))) (car d)]
      ;; d = (()) — empty deps
      [(and (pair? d) (null? (car d))) '()]
      [else '()])))

(define (mf-modules manifest)
  (let ([m (mf-field manifest 'modules)])
    (cond
      [(not m) '()]
      ;; Handle ((subdir ...) ...) nested format — extract module names
      [else
       (let loop ([items (if (and (pair? m) (pair? (car m))
                                  (pair? (caar m)) (eq? (caaar m) 'subdir))
                             m
                             (if (and (pair? m) (list? (car m)))
                                 m
                                 (list m)))]
                  [result '()])
         (cond
           [(null? items) (reverse result)]
           [(not (pair? (car items))) (loop (cdr items) result)]
           ;; Simple format: (name "file.ss" "desc")
           [(symbol? (caar items))
            (loop (cdr items) (cons (caar items) result))]
           ;; Nested format: ((subdir "name") ...)
           [(and (pair? (caar items)) (eq? (caaar items) 'subdir))
            (loop (cdr items) result)]  ; skip subdirs for now
           [else (loop (cdr items) result)]))])))

(define (mf-exports manifest)
  (let ([e (mf-field manifest 'exports)])
    (cond
      [(not e) '()]
      ;; Exports are lists of (module-sym export1 export2 ...)
      [(pair? e) e]
      [else '()])))

(define (mf-tier manifest)
  (let ([t (mf-field manifest 'tier)])
    (if (and t (pair? t)) (car t) (or t 0))))

(define (mf-purity manifest)
  (let ([p (mf-field manifest 'purity)])
    (if (and p (pair? p)) (car p) (or p 'unknown))))

(define (mf-stability manifest)
  (let ([s (mf-field manifest 'stability)])
    (if (and s (pair? s)) (car s) (or s 'unknown))))

(define (mf-version manifest)
  (let ([v (mf-field manifest 'version)])
    (if (and v (pair? v)) (car v) (or v "0.1.0"))))

(define (mf-aliases manifest)
  (let ([a (mf-field manifest 'aliases)])
    (if (and a (pair? a) (pair? (car a))) (car a) (or a '()))))

;;; ====
;;; Observation Builders (from manifest data)
;;; ====

(define (disc-build-search-obs manifests query target-skill)
  (let* ([name (mf-name target-skill)]
         [desc (mf-description target-skill)]
         [desc-short (if (> (string-length desc) 80)
                         (string-append (substring desc 0 77) "...")
                         desc)]
         ;; Find 1-2 other skills that share keywords for realistic results
         [other-matches
           (let loop ([ms manifests] [acc '()] [count 0])
             (cond
               [(or (null? ms) (>= count 2)) (reverse acc)]
               [(eq? (mf-name (cdar ms)) name) (loop (cdr ms) acc count)]
               [else
                (let ([kws (mf-keywords (cdar ms))])
                  (if (and (pair? kws)
                           (exists (lambda (k)
                                     (string-contains?
                                       (symbol->string k)
                                       (let ([parts (string-split query #\space)])
                                         (if (null? parts) query (car parts)))))
                                   kws))
                      (loop (cdr ms)
                            (cons (cdar ms) acc)
                            (+ count 1))
                      (loop (cdr ms) acc count)))]))])
    (string-append
      (format "~a skill~a found:\n"
              (+ 1 (length other-matches))
              (if (null? other-matches) "" "s"))
      (format "  ~a (tier ~a, ~a) — ~a\n"
              name (mf-tier target-skill) (mf-stability target-skill) desc-short)
      (apply string-append
        (map (lambda (m)
               (let ([d (mf-description m)])
                 (format "  ~a (tier ~a, ~a) — ~a\n"
                         (mf-name m) (mf-tier m) (mf-stability m)
                         (if (> (string-length d) 60)
                             (string-append (substring d 0 57) "...")
                             d))))
             other-matches)))))

(define (disc-build-inspect-obs manifest)
  (let ([name (mf-name manifest)]
        [ver (mf-version manifest)]
        [tier (mf-tier manifest)]
        [purity (mf-purity manifest)]
        [stab (mf-stability manifest)]
        [deps (mf-deps manifest)]
        [desc (mf-description manifest)]
        [mods (mf-modules manifest)])
    (string-append
      (format "~a (v~a, tier ~a, ~a purity, ~a)\n" name ver tier purity stab)
      (format "  Dependencies: ~a\n"
              (if (null? deps) "none" (format "~a" deps)))
      (format "  Description: ~a\n" desc)
      (format "  Modules: ~a"
              (if (null? mods)
                  "(see sub-skills)"
                  (let loop ([ms mods] [acc ""])
                    (if (null? ms)
                        acc
                        (loop (cdr ms)
                              (if (string=? acc "")
                                  (symbol->string (car ms))
                                  (string-append acc ", " (symbol->string (car ms))))))))))))

(define (disc-build-exports-obs manifest)
  (let ([name (mf-name manifest)]
        [exports (mf-exports manifest)])
    (if (null? exports)
        (format "~a: (no annotated exports — use (inspect '~a) for module list)" name name)
        (let loop ([groups exports] [acc ""])
          (if (null? groups)
              acc
              (let ([group (car groups)])
                (if (and (pair? group) (>= (length group) 2))
                    (let* ([mod-name (car group)]
                           [fns (cdr group)]
                           [fn-str (let inner ([fs fns] [s ""])
                                     (cond
                                       [(null? fs) s]
                                       [(> (string-length s) 200)
                                        (string-append s " ...")]
                                       [else (inner (cdr fs)
                                                    (if (string=? s "")
                                                        (format "~a" (car fs))
                                                        (format "~a, ~a" s (car fs))))]))]
                           [line (format "  ~a: ~a\n" mod-name fn-str)])
                      (loop (cdr groups) (string-append acc line)))
                    (loop (cdr groups) acc))))))))

(define (disc-build-load-obs module-name)
  (format "Module ~a loaded." module-name))

(define (disc-build-load-error-obs skill-name)
  (format "Error: '~a' is a skill, not a module. Use (inspect '~a) to see available modules, then (load 'module-name)."
          skill-name skill-name))

(define (disc-build-eval-error-obs fn-name)
  (format "Error: ~a is not defined. Did you load the correct module? Use (exports 'skill) to find the right function."
          fn-name))

;;; ====
;;; Task Templates
;;; ====
;;;
;;; Each template: (skill-sym task-desc search-query target-module eval-expr result-str
;;;                 [compose-with compose-module compose-eval compose-result])
;;; compose-* fields are for Tier 2 (multi-skill) traces.

(define *task-templates*
  (list

    ;; ---- Tier 0 Skills (no deps) ----

    (list 'linalg
      "Compute the dot product of vectors (1 2 3) and (4 5 6)."
      "vector dot product"
      'linalg/vec
      "(vec-dot (list->vec '(1 2 3)) (list->vec '(4 5 6)))"
      "32")

    (list 'linalg
      "Multiply a 2x2 matrix [[1,2],[3,4]] by vector [5,6]."
      "matrix vector multiply"
      'linalg/matrix
      "(matrix-vec-mul (list->matrix '((1 2) (3 4))) (list->vec '(5 6)))"
      "#(17 39)")

    (list 'linalg
      "Compute the QR decomposition of a 3x3 matrix."
      "QR decomposition"
      'linalg/matrix-decomp
      "(qr-decompose (list->matrix '((1 0 0) (0 1 0) (0 0 1))))"
      "(values Q R) where Q*R = original matrix")

    (list 'number-theory
      "Check whether 997 is a prime number."
      "primality testing"
      'number-theory/primality
      "(prime? 997)"
      "#t")

    (list 'number-theory
      "Compute the modular inverse of 7 mod 11."
      "modular inverse"
      'number-theory/modular
      "(mod-inverse 7 11)"
      "8")

    (list 'number-theory
      "Find the prime factorization of 360."
      "integer factorization"
      'number-theory/primality
      "(prime-factorization 360)"
      "((2 . 3) (3 . 2) (5 . 1))")

    (list 'algebra
      "Add polynomials p(x) = x^2 + 1 and q(x) = 2x + 3."
      "polynomial addition algebra"
      'algebra/polynomial
      "(poly-add (poly '(1 0 1)) (poly '(3 2)))"
      "(poly (4 2 1))")

    (list 'random
      "Generate a random permutation of the list (1 2 3 4 5)."
      "random permutation shuffle"
      'random/prng
      "(let ([rng (make-rng 42)]) (shuffle rng '(1 2 3 4 5)))"
      "(3 1 5 2 4)")

    (list 'crypto
      "Compute the SHA-512 hash of the string \"hello\"."
      "SHA-512 hash"
      'crypto/sha512
      "(sha512-hex (string->utf8 \"hello\"))"
      "9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca72323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043")

    (list 'crypto
      "Compute an HMAC-SHA256 of the message \"data\" with key \"secret\"."
      "HMAC authentication"
      'crypto/hmac
      "(hmac-sha256-hex (string->utf8 \"secret\") (string->utf8 \"data\"))"
      "1b2c16b75bd2a870c114153ccda5bcfca63314bc722fa160d690de133ccbb9db")

    (list 'data
      "Sort the list (5 3 1 4 2) in ascending order."
      "sort merge-sort"
      'data/sort
      "(merge-sort < '(5 3 1 4 2))"
      "(1 2 3 4 5)")

    (list 'data
      "Create a priority queue, insert values 10, 5, 20, 1, and extract the minimum."
      "priority queue heap"
      'data/heap
      "(let* ([h (heap-empty <)] [h (heap-insert h 10)] [h (heap-insert h 5)] [h (heap-insert h 20)] [h (heap-insert h 1)]) (heap-min h))"
      "1")

    ;; ---- Tier 1 Skills ----

    (list 'statistics
      "Compute the mean and standard deviation of the dataset (10 20 30 40 50)."
      "mean standard deviation statistics"
      'statistics/summary-stats
      "(list (vec-mean '(10 20 30 40 50)) (vec-std-dev '(10 20 30 40 50)))"
      "(30.0 15.811388300841896)")

    (list 'statistics
      "Fit a simple linear regression to predict y from x, given x=(1 2 3 4 5) and y=(2 4 5 4 5)."
      "linear regression fit"
      'statistics/linear
      "(linear-model-fit '((1) (2) (3) (4) (5)) '(2 4 5 4 5))"
      "Linear model: intercept=2.2, slope=0.6, R²=0.514")

    (list 'statistics
      "Perform a one-sample t-test to check if the mean of (12 14 11 13 15 10) equals 10."
      "t-test hypothesis"
      'statistics/t-test
      "(t-test-one-sample '(12 14 11 13 15 10) 10)"
      "(t-statistic: 3.87, p-value: 0.012, reject H₀ at α=0.05)")

    (list 'info
      "Compute the Shannon entropy of the probability distribution (0.5 0.25 0.125 0.125)."
      "Shannon entropy information"
      'info/entropy
      "(entropy '(0.5 0.25 0.125 0.125))"
      "1.75")

    (list 'info
      "Compute the KL-divergence from distribution P=(0.5 0.5) to Q=(0.9 0.1)."
      "KL divergence"
      'info/entropy
      "(kl-divergence '(0.5 0.5) '(0.9 0.1))"
      "0.5108...")

    (list 'info
      "Build a Huffman tree for symbols (a b c d) with frequencies (50 25 15 10) and show the codes."
      "Huffman coding compression"
      'info/coding
      "(huffman-codes (build-huffman-tree '((a . 50) (b . 25) (c . 15) (d . 10))))"
      "((a . \"0\") (b . \"10\") (c . \"110\") (d . \"111\"))")

    (list 'optimization
      "Minimize f(x) = (x - 3)^2 starting from x = 0 using gradient descent."
      "minimize gradient descent"
      'optimization/first-order
      "(gradient-descent (lambda (x) (* (- x 3) (- x 3))) 0 0.1 100)"
      "2.999...")

    (list 'optimization
      "Solve the linear program: maximize 3x + 2y subject to x + y <= 4, x <= 3, y <= 3, x,y >= 0."
      "linear programming simplex"
      'optimization/lp
      "(lp-solve (make-lp '(3 2) '((1 1) (1 0) (0 1)) '(4 3 3)))"
      "Optimal: x=3, y=1, objective=11")

    (list 'topology
      "Compute the Euler characteristic of a tetrahedron."
      "Euler characteristic simplicial complex"
      'topology/simplicial-complex
      "(sc-euler (sc-from-simplices (list (tetrahedron '(0 1 2 3)))))"
      "1")

    (list 'topology
      "Compute the Betti numbers of a torus."
      "Betti numbers homology torus"
      'topology/homology
      "(sc-betti-numbers (make-torus))"
      "(1 2 1)")

    (list 'geometry
      "Compute the area of a triangle with vertices (0,0), (3,0), (0,4)."
      "triangle area geometry"
      'geometry/shapes
      "(triangle-area '(0 0) '(3 0) '(0 4))"
      "6.0")

    (list 'numeric
      "Multiply complex numbers (1+2i) and (3+4i)."
      "complex number multiplication"
      'numeric/complex
      "(complex-mul (make-complex 1 2) (make-complex 3 4))"
      "-5+10i")

    (list 'numeric
      "Compute the DFT of the signal (1 0 1 0)."
      "discrete Fourier transform DFT"
      'numeric/dft
      "(dft '(1 0 1 0))"
      "(2 0 2 0)")

    (list 'autodiff
      "Compute the derivative of f(x) = x^3 at x = 2."
      "automatic differentiation derivative"
      'autodiff/reverse-diff
      "(gradient (lambda (x) (* x x x)) 2)"
      "12")

    (list 'automata
      "Build a DFA that accepts binary strings ending in '01'."
      "DFA finite automaton"
      'automata/automata
      "(let ([dfa (make-dfa '(q0 q1 q2) '(0 1) '((q0 0 q1) (q0 1 q0) (q1 0 q1) (q1 1 q2) (q2 0 q1) (q2 1 q0)) 'q0 '(q2))]) (dfa-accept? dfa '(1 0 1)))"
      "#t")

    (list 'diffgeo
      "Create a chart for the upper hemisphere of a unit sphere."
      "chart manifold sphere"
      'diffgeo/charts
      "(make-chart 'upper-sphere 2 (lambda (uv) (let ([u (car uv)] [v (cadr uv)]) (list u v (sqrt (- 1 (* u u) (* v v)))))))"
      "<chart upper-sphere dim=2>")

    (list 'egraph
      "Simplify the expression (+ x 0) using equality saturation."
      "e-graph equality saturation simplify"
      'egraph/egraph
      "(let ([eg (make-egraph)]) (egraph-add! eg '(+ x 0)) (egraph-run! eg *standard-rules*) (egraph-extract eg 'code-size-cost))"
      "x")

    (list 'query
      "Parse the SQL query: SELECT name, age FROM users WHERE age > 30."
      "SQL parser query"
      'query/sql-parser
      "(parse-sql \"SELECT name, age FROM users WHERE age > 30\")"
      "(select (columns name age) (from users) (where (> age 30)))")

    (list 'tiles
      "Create a 5x5 hex grid for a board game."
      "hex grid board game"
      'tiles/hex
      "(make-hex-grid 5 5)"
      "<hex-grid 5x5, 25 cells>")

    ;; ---- Sub-skills ----

    (list 'fp/optics
      "Create a lens to access the second element of a pair."
      "lens optics focus"
      'fp/optics/optics
      "(let ([snd-lens (make-lens cdr (lambda (s v) (cons (car s) v)))]) (lens-view snd-lens '(a . b)))"
      "b")

    (list 'fp/game
      "Find the Nash equilibria of the Prisoner's Dilemma."
      "Nash equilibrium game theory"
      'fp/game/normal-form
      "(nash-equilibria (make-game '(((-1 -1) (-3 0)) ((0 -3) (-2 -2)))))"
      "((defect defect))")

    (list 'fp/game
      "Compute the Shapley value for a 3-player voting game."
      "Shapley value cooperative game"
      'fp/game/coop-games
      "(shapley-value (make-voting-game '(3 2 1) 4))"
      "(0.5 0.333 0.167)")

    (list 'fp/sat
      "Solve the SAT problem: (a OR b) AND (NOT a OR c) AND (NOT b OR NOT c)."
      "SAT solver boolean satisfiability"
      'fp/sat/sat
      "(sat-solve '((a b) (-a c) (-b -c)))"
      "(model (a #t) (b #f) (c #t))")

    (list 'fp/clp
      "Find all integer solutions where x + y = 10, x > 3, y > 3."
      "constraint logic programming integers"
      'fp/clp/clp
      "(run* (x y) (fd-+ x y 10) (fd-> x 3) (fd-> y 3))"
      "((4 6) (5 5) (6 4))")

    (list 'physics/classical
      "Simulate a projectile launched at 45 degrees with initial velocity 10 m/s for 2 seconds."
      "projectile physics simulation"
      'physics/classical/dynamics
      "(simulate-projectile 10 (/ pi 4) 2.0 0.01)"
      "(final-x: 14.14, final-y: 0.0, max-height: 2.55)")

    (list 'physics/lenses
      "Use optics to access the position of a particle in a physics world."
      "physics optics lens particle position"
      'physics/lenses/body-lenses
      "(lens-view position-lens (make-body '(1.0 2.0) '(0 0) 1.0))"
      "(1.0 2.0)")

    ))

;;; ====
;;; State Reconstruction (compatible with sft-synth.ss)
;;; ====

(define (disc-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg)
      arg))

(define (disc-build-initial-state task fuel)
  (make-rlm2-state task '() '() '() '() '() '() #f fuel 0))

(define (disc-advance-state state action obs-type obs-ok obs-value)
  (let* ([atype (if (pair? action) (car action) 'unknown)]
         [env (rlm2-state-env state)]
         [step (rlm2-state-step state)]
         [env* (case atype
                 [(store)
                  (if (>= (length action) 3)
                      (rlm-env-put env (disc-unquote-key (cadr action))
                                   "computed" 'result 0)
                      env)]
                 [(eval)
                  (rlm-env-put env (string->symbol (format "step-~a-result" step))
                               "computed" 'result 0)]
                 [else env])]
         [loaded* (cond
                    [(eq? atype 'load)
                     (if (>= (length action) 2)
                         (cons (disc-unquote-key (cadr action))
                               (rlm2-state-loaded state))
                         (rlm2-state-loaded state))]
                    [(eq? atype 'begin)
                     (fold-left
                       (lambda (acc sub)
                         (if (and (pair? sub) (eq? (car sub) 'load)
                                  (>= (length sub) 2))
                             (cons (disc-unquote-key (cadr sub)) acc)
                             acc))
                       (rlm2-state-loaded state)
                       (cdr action))]
                    [else (rlm2-state-loaded state)])]
         [plan* (cond
                  [(eq? atype 'plan!)
                   (if (>= (length action) 2) (cadr action) '())]
                  [else (rlm2-state-plan state)])]
         [note-str (format "Step ~a: ~a ~a" step obs-type
                           (if obs-ok "ok" "error"))]
         [notes* (if (eq? atype 'think)
                     (rlm2-state-notes state)
                     (cons note-str (rlm2-state-notes state)))]
         [episodic* (cons (cons step "synthetic")
                          (rlm2-state-episodic state))]
         [last-result* obs-value])
    (make-rlm2-state
      (rlm2-state-task state)
      plan* env* loaded* notes* episodic* '()
      last-result*
      (max 0 (- (rlm2-state-fuel state) 100))
      (+ step 1))))

;;; ====
;;; Training Tuple Generation
;;; ====

(define (disc-format-action action)
  (let ([port (open-output-string)])
    (write action port)
    (get-output-string port)))

(define (disc-make-act-example sys-prompt hud action-str label step-num)
  `((messages . (((role . "system") (content . ,sys-prompt))
                 ((role . "user") (content . ,hud))
                 ((role . "assistant") (content . ,action-str))))
    (source . ,label)
    (step . ,step-num)
    (phase . "act")))

(define *disc-mechanical-actions*
  '(submit store load plan! journal))

(define (disc-make-reflect-example action obs-value note label step-num)
  (let* ([action-str (disc-format-action action)]
         [obs-str (if (string? obs-value)
                      (if (> (string-length obs-value) 500)
                          (string-append (substring obs-value 0 497) "...")
                          obs-value)
                      (format "~a" obs-value))]
         [reflect-prompt (string-append
                           "Distill the following step into a single concise note "
                           "(one sentence, max 120 chars). "
                           "The note should capture what was learned or accomplished.\n\n"
                           "Action: " action-str "\n\n"
                           "Result: " obs-str "\n\n"
                           "Note:")])
    `((messages . (((role . "user") (content . ,reflect-prompt))
                   ((role . "assistant") (content . ,note))))
      (source . ,label)
      (step . ,step-num)
      (phase . "reflect"))))

;;; ====
;;; Trace Generator — walk steps, build HUD, emit examples
;;; ====

(define (disc-generate-trace label task steps fuel)
  (let ([sys-prompt (rlm2-build-system-prompt *disc-base-prompt*)]
        [initial-state (disc-build-initial-state task fuel)])
    (let loop ([remaining steps]
               [state initial-state]
               [examples '()])
      (if (null? remaining)
          (reverse examples)
          (let* ([step-data (car remaining)]
                 [action (car step-data)]
                 [obs-type (cadr step-data)]
                 [obs-ok (caddr step-data)]
                 [obs-value (cadddr step-data)]
                 [obs-note (if (> (length step-data) 4) (list-ref step-data 4) #f)]
                 [hud (rlm2-render-state state *disc-context-budget*)]
                 [action-str (disc-format-action action)]
                 [act-example (disc-make-act-example
                                sys-prompt hud action-str
                                label (rlm2-state-step state))]
                 [action-type (if (pair? action) (car action) 'unknown)]
                 [mechanical? (memq action-type *disc-mechanical-actions*)]
                 [reflect-example
                   (and obs-note
                        (not mechanical?)
                        (not (eq? action-type 'think))
                        (disc-make-reflect-example
                          action obs-value obs-note
                          label (rlm2-state-step state)))]
                 [new-examples (if reflect-example
                                   (list act-example reflect-example)
                                   (list act-example))]
                 [state* (disc-advance-state
                           state action obs-type obs-ok obs-value)])
            (loop (cdr remaining) state*
                  (append (reverse new-examples) examples)))))))

;;; ====
;;; Tier 1: Single-Skill Discovery
;;; ====

(define (gen-tier1 template manifests)
  (let* ([skill-name (list-ref template 0)]
         [task (list-ref template 1)]
         [search-query (list-ref template 2)]
         [target-module (list-ref template 3)]
         [eval-expr (list-ref template 4)]
         [result (list-ref template 5)]
         [label (format "disc-t1-~a-~a" skill-name
                        (string-downcase
                          (substring search-query 0
                                     (min 15 (string-length search-query)))))]
         ;; Find the manifest for this skill
         [manifest (find-manifest manifests skill-name)]
         [search-obs (if manifest
                        (disc-build-search-obs manifests search-query manifest)
                        (format "1 skill found:\n  ~a — (skill description)" skill-name))]
         [inspect-obs (if manifest
                         (disc-build-inspect-obs manifest)
                         (format "~a — (description)" skill-name))]
         [exports-obs (if manifest
                         (disc-build-exports-obs manifest)
                         (format "~a: (functions)" skill-name))]
         [steps
           (list
             ;; Step 0: Search
             (list `(search ,search-query)
                   'search #t search-obs
                   (format "Found ~a skill for ~a." skill-name search-query))
             ;; Step 1: Inspect
             (list `(inspect ',skill-name)
                   'inspect #t inspect-obs
                   (format "~a has modules including ~a."
                           skill-name
                           (let ([mods (if manifest (mf-modules manifest) '())])
                             (if (null? mods) "(modules)"
                                 (format "~a" (car mods))))))
             ;; Step 2: Exports
             (list `(exports ',skill-name)
                   'exports #t exports-obs
                   (format "Key exports identified for ~a." skill-name))
             ;; Step 3: Load
             (list `(load ',target-module)
                   'load #t (disc-build-load-obs target-module))
             ;; Step 4: Eval
             (list `(eval ,(read-expr-string eval-expr))
                   'eval #t result
                   (format "Computation returned ~a." result))
             ;; Step 5: Submit
             (list `(submit ,(read-expr-string
                               (format "(retrieve 'step-4-result)")))
                   'submit #t (format "Answer accepted: ~a" result)))])
    (disc-generate-trace label task steps 15000)))

;;; ====
;;; Tier 2: Multi-Skill Composition
;;; ====

(define (gen-tier2 template manifests)
  (let* ([skill-name (list-ref template 0)]
         [manifest (find-manifest manifests skill-name)])
    ;; Only generate if skill has deps
    (if (or (not manifest) (null? (mf-deps manifest)))
        '()
        (let* ([deps (mf-deps manifest)]
               [dep-name (car deps)]  ; Use first dependency
               [dep-manifest (find-manifest manifests dep-name)]
               [task (format "~a (This requires tools from both ~a and ~a.)"
                             (list-ref template 1) dep-name skill-name)]
               [search-query (list-ref template 2)]
               [target-module (list-ref template 3)]
               [eval-expr (list-ref template 4)]
               [result (list-ref template 5)]
               [label (format "disc-t2-~a+~a" skill-name dep-name)]
               ;; Pick a dep module
               [dep-modules (if dep-manifest (mf-modules dep-manifest) '())]
               [dep-module (if (null? dep-modules)
                               (string->symbol (format "~a/~a" dep-name dep-name))
                               (string->symbol (format "~a/~a" dep-name (car dep-modules))))]
               [search-obs (if manifest
                              (disc-build-search-obs manifests search-query manifest)
                              (format "Found ~a." skill-name))]
               [inspect-obs (if manifest
                               (disc-build-inspect-obs manifest)
                               (format "~a — (description)" skill-name))]
               [dep-inspect-obs (if dep-manifest
                                    (disc-build-inspect-obs dep-manifest)
                                    (format "~a — (description)" dep-name))]
               [steps
                 (list
                   ;; Step 0: Think about multi-skill need
                   (list '(think "This problem needs multiple tools. Let me search for the main capability first.")
                         'think #t #f)
                   ;; Step 1: Search
                   (list `(search ,search-query)
                         'search #t search-obs
                         (format "Found ~a. Need to check its dependencies." skill-name))
                   ;; Step 2: Inspect main skill (reveals deps)
                   (list `(inspect ',skill-name)
                         'inspect #t inspect-obs
                         (format "~a depends on ~a. Need to load both." skill-name deps))
                   ;; Step 3: Inspect dep
                   (list `(inspect ',dep-name)
                         'inspect #t dep-inspect-obs
                         (format "~a provides the foundation needed by ~a." dep-name skill-name))
                   ;; Step 4: Load dep module first
                   (list `(load ',dep-module)
                         'load #t (disc-build-load-obs dep-module))
                   ;; Step 5: Load main module
                   (list `(load ',target-module)
                         'load #t (disc-build-load-obs target-module))
                   ;; Step 6: Eval
                   (list `(eval ,(read-expr-string eval-expr))
                         'eval #t result
                         (format "Computed using both ~a and ~a." dep-name skill-name))
                   ;; Step 7: Submit
                   (list `(submit ,(read-expr-string
                                     (format "(retrieve 'step-6-result)")))
                         'submit #t (format "Answer accepted: ~a" result)))])
          (disc-generate-trace label task steps 20000)))))

;;; ====
;;; Tier 3: Recovery Traces
;;; ====

;; Variant A: Load skill name instead of module (common error)
(define (gen-tier3-load-error template manifests)
  (let* ([skill-name (list-ref template 0)]
         [task (list-ref template 1)]
         [search-query (list-ref template 2)]
         [target-module (list-ref template 3)]
         [eval-expr (list-ref template 4)]
         [result (list-ref template 5)]
         [manifest (find-manifest manifests skill-name)]
         [label (format "disc-t3-load-~a" skill-name)]
         [inspect-obs (if manifest
                         (disc-build-inspect-obs manifest)
                         (format "~a — (description)" skill-name))]
         [steps
           (list
             ;; Step 0: Search (abbreviated — model already knows skill)
             (list `(search ,search-query)
                   'search #t
                   (if manifest
                       (disc-build-search-obs manifests search-query manifest)
                       (format "Found ~a." skill-name))
                   (format "Found ~a for this task." skill-name))
             ;; Step 1: WRONG — try to load skill name
             (list `(load ',skill-name)
                   'load #f (disc-build-load-error-obs skill-name)
                   (format "Error: '~a' is a skill, not a module. Need to inspect for module names." skill-name))
             ;; Step 2: Inspect to find modules
             (list `(inspect ',skill-name)
                   'inspect #t inspect-obs
                   (format "Found modules. The correct one to load is ~a." target-module))
             ;; Step 3: Load correct module
             (list `(load ',target-module)
                   'load #t (disc-build-load-obs target-module))
             ;; Step 4: Eval
             (list `(eval ,(read-expr-string eval-expr))
                   'eval #t result
                   (format "Computation succeeded: ~a." result))
             ;; Step 5: Submit
             (list `(submit ,(read-expr-string
                               (format "(retrieve 'step-4-result)")))
                   'submit #t (format "Answer accepted: ~a" result)))])
    (disc-generate-trace label task steps 15000)))

;; Variant B: Call wrong function → check exports → retry
(define (gen-tier3-fn-error template manifests)
  (let* ([skill-name (list-ref template 0)]
         [task (list-ref template 1)]
         [target-module (list-ref template 3)]
         [eval-expr (list-ref template 4)]
         [result (list-ref template 5)]
         [manifest (find-manifest manifests skill-name)]
         [exports-obs (if manifest
                         (disc-build-exports-obs manifest)
                         (format "~a: (functions)" skill-name))]
         [label (format "disc-t3-fn-~a" skill-name)]
         ;; Construct a plausible wrong function name
         [wrong-fn (string->symbol
                     (format "compute-~a"
                             (let ([q (list-ref template 2)])
                               (car (string-split q #\space)))))]
         [steps
           (list
             ;; Step 0: Load module (correct)
             (list `(load ',target-module)
                   'load #t (disc-build-load-obs target-module))
             ;; Step 1: WRONG — call non-existent function
             (list `(eval (,wrong-fn 42))
                   'eval #f (disc-build-eval-error-obs wrong-fn)
                   (format "~a doesn't exist. Need to check what functions are actually available." wrong-fn))
             ;; Step 2: Check exports
             (list `(exports ',skill-name)
                   'exports #t exports-obs
                   (format "Found the correct functions. Let me use the right one."))
             ;; Step 3: Correct eval
             (list `(eval ,(read-expr-string eval-expr))
                   'eval #t result
                   (format "Correct function worked: ~a." result))
             ;; Step 4: Submit
             (list `(submit ,(read-expr-string
                               (format "(retrieve 'step-3-result)")))
                   'submit #t (format "Answer accepted: ~a" result)))])
    (disc-generate-trace label task steps 15000)))

;;; ====
;;; Helpers
;;; ====

(define (find-manifest manifests skill-name)
  (let loop ([ms manifests])
    (cond
      [(null? ms) #f]
      [(eq? (mf-name (cdar ms)) skill-name) (cdar ms)]
      [else (loop (cdr ms))])))

(define (read-expr-string str)
  (guard (ex [else (string->symbol str)])
    (with-input-from-string str read)))

(define (string-split str ch)
  (let loop ([chars (string->list str)] [current '()] [result '()])
    (cond
      [(null? chars)
       (reverse (if (null? current)
                    result
                    (cons (list->string (reverse current)) result)))]
      [(char=? (car chars) ch)
       (loop (cdr chars) '()
             (if (null? current)
                 result
                 (cons (list->string (reverse current)) result)))]
      [else
       (loop (cdr chars) (cons (car chars) current) result)])))

(define (string-contains? s sub)
  (let ([slen (string-length s)]
        [sublen (string-length sub)])
    (if (> sublen slen)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i sublen) slen) #f]
            [(string=? (substring s i (+ i sublen)) sub) #t]
            [else (loop (+ i 1))])))))

(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;;; ====
;;; JSONL Writer
;;; ====

(define (disc-write-jsonl! examples output-path)
  (call-with-port
    (open-file-output-port output-path
      (file-options no-fail)
      (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (for-each
        (lambda (example)
          (put-string port (json->string example))
          (put-string port "\n"))
        examples))))

;;; ====
;;; Main Driver
;;; ====

(define (gen-discovery-all!)
  (display "Lattice Discovery Trace Generator\n")
  (display "==================================\n\n")

  ;; Read all manifests
  (display "Reading manifests...\n")
  (let* ([manifests (disc-find-manifests "lattice")]
         [skill-count (length manifests)])
    (display (format "  Found ~a skill manifests.\n\n" skill-count))

    ;; Generate all tiers
    (display (format "Generating from ~a task templates:\n" (length *task-templates*)))

    (let ([all-examples
            (fold-left
              (lambda (acc template)
                (let* ([skill-name (list-ref template 0)]
                       [_ (display (format "  ~a: " skill-name))]
                       ;; Tier 1
                       [t1 (guard (ex [else
                                       (display (format "T1-ERR(~a) "
                                         (if (message-condition? ex)
                                             (condition-message ex) ex)))
                                       '()])
                              (gen-tier1 template manifests))]
                       ;; Tier 2
                       [t2 (guard (ex [else
                                       (display (format "T2-ERR(~a) "
                                         (if (message-condition? ex)
                                             (condition-message ex) ex)))
                                       '()])
                              (gen-tier2 template manifests))]
                       ;; Tier 3a: Load error
                       [t3a (guard (ex [else
                                        (display (format "T3a-ERR(~a) "
                                          (if (message-condition? ex)
                                              (condition-message ex) ex)))
                                        '()])
                               (gen-tier3-load-error template manifests))]
                       ;; Tier 3b: Function error
                       [t3b (guard (ex [else
                                        (display (format "T3b-ERR(~a) "
                                          (if (message-condition? ex)
                                              (condition-message ex) ex)))
                                        '()])
                               (gen-tier3-fn-error template manifests))]
                       [total (+ (length t1) (length t2) (length t3a) (length t3b))])
                  (display (format "~a examples (T1:~a T2:~a T3:~a+~a)\n"
                                   total (length t1) (length t2)
                                   (length t3a) (length t3b)))
                  (append acc t1 t2 t3a t3b)))
              '()
              *task-templates*)])

      ;; Report distribution
      (let* ([act-count (length (filter (lambda (e)
                                          (let ([p (assq 'phase e)])
                                            (and p (string=? (cdr p) "act"))))
                                        all-examples))]
             [reflect-count (length (filter (lambda (e)
                                              (let ([p (assq 'phase e)])
                                                (and p (string=? (cdr p) "reflect"))))
                                            all-examples))])
        (display (format "\nTotal: ~a training examples (~a act, ~a reflect)\n"
                         (length all-examples) act-count reflect-count)))

      (if (null? all-examples)
          (display "No examples generated.\n")
          (begin
            (disc-write-jsonl! all-examples *disc-output-path*)
            (display (format "Written to: ~a\n" *disc-output-path*))))
      all-examples)))

;;; ====
;;; Run
;;; ====

(gen-discovery-all!)
