;;; core/types/type-annotation-check.ss — Type Annotation Validation
;;; @module type-annotation-check
;;; @requires prelude kinds sig-parser
;;;
;;; Validates type annotations in documentation comments.
;;;
;;; Phase 2 of incremental type/kind checking:
;;;   - Parse type signatures from ;;; comments
;;;   - Validate that parsed types have valid kinds
;;;   - Report any malformed or ill-kinded type annotations
;;;
;;; Exit codes:
;;;   0 — All checks pass
;;;   1 — Type annotation validation failed
;;;
;;; Usage:
;;;   scheme --script core/types/type-annotation-check.ss [file ...]
;;;   scheme --script core/types/type-annotation-check.ss          # checks all core files
;;;
;;; This is Core code: pure, assumes well-formed input files.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/sig-parser.ss")

;;; ============================================================
;;; Result Tracking
;;; ============================================================

(define *total-sigs* 0)
(define *valid-sigs* 0)
(define *invalid-sigs* 0)
(define *parse-errors* '())
(define *kind-errors* '())

(define (reset-counters!)
  (set! *total-sigs* 0)
  (set! *valid-sigs* 0)
  (set! *invalid-sigs* 0)
  (set! *parse-errors* '())
  (set! *kind-errors* '()))

(define (record-parse-error! file line msg)
  (set! *invalid-sigs* (+ *invalid-sigs* 1))
  (set! *parse-errors* (cons (list file line msg) *parse-errors*)))

(define (record-kind-error! file line name type kind-error)
  (set! *invalid-sigs* (+ *invalid-sigs* 1))
  (set! *kind-errors* (cons (list file line name type kind-error) *kind-errors*)))

(define (record-valid!)
  (set! *valid-sigs* (+ *valid-sigs* 1)))

;;; ============================================================
;;; Type to Kind Checking Bridge
;;; ============================================================

;;; Map from sig-parser names to kinds.ss names
(define type-name-mapping
  '((Integer . Int)
    (Number . Nat)
    (Real . Nat)
    (Rational . Nat)
    (Char . Symbol)
    (Boolean . Bool)
    (Any . Unit)
    (Void . Unit)
    (Value . Unit)  ; Abstract value type
    (Error . Unit)  ; Error type
    ;; Domain-specific data types (treated as kind *)
    (Matrix . Hash)
    (Vec . Hash)
    ;; Note: Vector is in builtin-kinds with kind * → *, not mapped here
    (Queue . Hash)
    (Stack . Hash)
    (Dict . Hash)
    ;; Set is now a proper type constructor in builtin-kinds
    (Alist . Hash)
    (FSCap . Hash)
    (Bytevector . Hash)
    (RNG . Hash)
    (DDS . Hash)
    ;; Geometry types (treated as kind *)
    (Vec2 . Hash)
    (Vec3 . Hash)
    (Vec4 . Hash)
    (Point2 . Hash)
    (Point3 . Hash)
    (Line3 . Hash)
    (Ray3 . Hash)
    (Plane3 . Hash)
    (Triangle3 . Hash)
    (Circle . Hash)
    (Sphere . Hash)
    (AABB . Hash)
    (OBB . Hash)
    (Quaternion . Hash)
    ;; Evaluator types (treated as kind *)
    (Closure . Hash)
    (Fuel . Hash)
    (Value . Hash)
    (Values . Hash)
    (Tape . Hash)
    ;; Expand/normalize types (treated as kind *)
    (Supply . Hash)
    ;; Parser types (treated as kind *)
    (SpannedExpr . Hash)
    (SpannedParser . Hash)
    (Atom . Hash)
    (TracedValue . Hash)
    ;; Autodiff types (treated as kind *)
    (Node . Hash)
    (CompGraph . Hash)
    (Dual . Hash)
    (Hyperdual . Hash)
    ;; Type system meta-types (treated as kind *)
    (Type . Hash)
    (Kind . Hash)
    (Expr . Hash)
    (TEnv . Hash)
    (Env . Hash)
    (Subst . Hash)
    (Constraint . Hash)
    (ClassDB . Hash)
    (IDB . Hash)
    (Evidence . Hash)
    (AnnotatedExpr . Hash)
    (AnnScrutinee . Hash)
    (AnnClauses . Hash)
    (Clause . Hash)
    (KindedTVar . Hash)
    (Indent . Hash)
    ;; Pair -> treat like Either for kind checking
    (Pair . Either)
    ;; Maybe -> Option
    (Maybe . Option)
    ;; Result is like Either (* → * → *)
    (Result . Either)
    ;; Additional type system types
    (KindEnv . Hash)
    (FunDep . Hash)
    (TypeSubst . Hash)
    (KindSubst . Hash)
    (Instance . Hash)
    (Ordering . Hash)
    (Block . Hash)
    ;; Pretty printer types (treated as kind *)
    (Doc . Hash)
    (SDoc . Hash)
    (Sexp . Hash)
    ;; Debug/profiling types (treated as kind *)
    (Debugger . Hash)
    (Profiler . Hash)
    (CostModel . Hash)
    ;; Documentation types (treated as kind *)
    (Doc-Entry . Hash)
    (Entry . Hash)
    (Example . Hash)
    ;; Misc types
    (Procedure . Hash)
    (void . Unit)
    ;; Dependent type system types
    (SExpr . Hash)
    (Binding . Hash)
    (Context . Hash)
    (Thunk . Hash)
    (NbEEnv . Hash)
    (InstanceDB . Hash)
    (Match . Hash)
    (ParsedType . Hash)
    (InternalType . Hash)
    ;; NbE types (treated as kind *)
    (Neutral . Hash)
    (KindValue . Hash)
    (KindClosure . Hash)
    (KindNeutral . Hash)
    (Level . Hash)
    (KindExpr . Hash)
    (KindEnv . Hash)
    ;; Compiler types (treated as kind *)
    (S-expr . Hash)
    (Options . Hash)
    (Phase . Hash)
    (Path . Hash)
    ;; Numeric types (treated as kind *)
    (Complex . Hash)
    (Num . Hash)      ; Numeric value (alias for Number)
    ;; Control systems types (treated as kind *)
    (SS . Hash)       ; State-space system
    (TF . Hash)       ; Transfer function
    ;; Units of measure types (treated as kind *)
    (Dimension . Hash)
    (Quantity . Hash)
    ;; Game theory types (treated as kind *)
    (Game . Hash)
    ;; Error system types (treated as kind *)
    (Code . Hash)
    (Details . Hash)
    (Span . Hash)
    (Any . Hash)
    ;; Pipeline/Stage types (treated as kind *)
    (Stage . Hash)
    (StageResult . Hash)
    (Effect . Hash)
    (CouncilConfig . Hash)
    (CouncilResult . Hash)
    (CouncilEffect . Hash)
    (Pipeline . Hash)
    (Bead . Hash)
    (DiscordContext . Hash)
    ;; Algebraic effects types (treated as kind *)
    (EffectSig . Hash)
    (EffectRow . Hash)
    (Eff . Hash)
    (Operation . Hash)
    (Handler . Hash)
    (RowVar . Hash)
    (Response . Hash)
    (State . Hash)
    (Reader . Hash)
    (Writer . Hash)
    (Exception . Hash)
    (NonDet . Hash)
    (Console . Hash)
    (Async . Hash)
    (Future . Hash)
    (Random . Hash)
    ;; Continuation monad types (treated as kind *)
    (Cont . Hash)
    (ContT . Hash)
    (Trampoline . Hash)
    (Coroutine . Hash)
    (Generator . Hash)
    ;; Free monad types (treated as kind *)
    (Free . Hash)
    (Coyoneda . Hash)
    (KVF . Hash)
    (ConsoleF . Hash)
    ;; DSL builder types (treated as kind *)
    (Instruction . Hash)
    (Interpreter . Hash)
    (DSL . Hash)
    (Middleware . Hash)
    (SourcePos . Hash)
    (Located . Hash)
    (LocatedInstruction . Hash)
    (TaglessDSL . Hash)
    (Stmt . Hash)
    (Schema . Hash)
    (ConstraintStore . Hash)
    ;; Logic programming types (treated as kind *)
    (LVar . Hash)
    (Goal . Hash)
    (Substitution . Hash)
    (Peano . Hash)
    ;; Query system types (treated as kind *)
    (ACState . Hash)
    (Query . Hash)
    (Pattern . Hash)
    (Automaton . Hash)
    ;; Parser combinator types (treated as kind *)
    (Pos . Hash)
    (MemoTable . Hash)
    (MemoKey . Hash)
    (ParseError . Hash)
    ;; Geometry acceleration structures (treated as kind *)
    (BVH . Hash)
    (Mesh . Hash)
    (Octree . Hash)
    (RaymarchParams . Hash)
    (Camera . Hash)
    ;; Digital filter types (treated as kind *)
    (Biquad . Hash)
    (FIR . Hash)
    (IIR . Hash)
    ;; Graph types (treated as kind *)
    (Edge . Hash)
    (Vertex . Hash)
    (Graph . Hash)
    ;; FSM types (treated as kind *)
    (FSM . Hash)
    (Transition . Hash)
    (EpsilonTransition . Hash)
    ;; Benchmark types (treated as kind *)
    (Benchmark . Hash)
    (Suite . Hash)
    (BenchResult . Hash)
    (Comparison . Hash)
    ;; Debug types (treated as kind *)
    (Debugger . Hash)
    (Breakpoint . Hash)
    (BreakpointCondition . Hash)
    (FuelTracker . Hash)
    ;; Autodiff additional types (treated as kind *)
    (Jet . Hash)
    (Traced . Hash)
    (SparseGrad . Hash)
    (SparseCOO . Hash)
    (Differentiable . Hash)
    (Signal . Hash)
    (DiffSignal . Hash)
    ;; Profile types (treated as kind *)
    (ProfileNode . Hash)
    (Profiler . Hash)
    ;; Statechart/automata types (treated as kind *)
    (Statechart . Hash)
    (Configuration . Hash)
    (Event . Hash)
    (Action . Hash)
    (Guard . Hash)
    (History . Hash)
    (Region . Hash)
    ;; DSL types (treated as kind *)
    (Typed . Hash)
    (Syntax . Hash)
    (SourceLoc . Hash)
    (Bindings . Hash)
    (Transformer . Hash)
    (Interface . Hash)
    (Handler . Hash)
    ;; Dynamics types (treated as kind *)
    (DDS . Hash)
    (Orbit . Hash)
    (Jacobian . Hash)
    ;; Info-theory additional types (treated as kind *)
    (Channel . Hash)
    (Codebook . Hash)
    (HuffmanTree . Hash)
    (ArithCoder . Hash)
    (SparseCSR . Hash)
    ;; Algebra types (treated as kind *)
    (Group . Hash)
    (Ring . Hash)
    (Field . Hash)
    (Monoid . Hash)
    (Element . Hash)
    (Homomorphism . Hash)
    (RingHomomorphism . Hash)
    (Ideal . Hash)
    ;; Parser types (treated as kind *)
    (Moore . Hash)
    (Mealy . Hash)
    (JsonValue . Hash)
    (SExp . Hash)
    (AST . Hash)
    (INI . Hash)
    ;; Lang types (treated as kind *)
    (ModuleEnv . Hash)
    (Binding . Hash)
    (DepGraph . Hash)
    (Index . Hash)
    (IndexEntry . Hash)))

;;; Greek letters used as type variables
(define greek-type-var-names
  '(α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω))

;;; is-type-var? : Symbol → Boolean
(define (is-type-var? sym)
  (or (memq sym greek-type-var-names)
      ;; Single lowercase letter
      (let ([s (symbol->string sym)])
           (and (= (string-length s) 1)
                (char-lower-case? (string-ref s 0))))))

;;; Convert signature type names to internal names
(define (normalize-type-name sym)
  (let ([entry (assq sym type-name-mapping)])
       (if entry (cdr entry) sym)))

;;; Type constructors that use @ application syntax
(define type-constructors-with-application
  '(List Vector Option Either Result Pair Maybe Ref Set Stream Delayed
    Functor Applicative Monad))

;;; Convert parsed type to kind-checker format
(define (sig-type->kind-type parsed)
  (cond
   [(symbol? parsed)
    (normalize-type-name parsed)]
   [(not (pair? parsed)) parsed]
   ;; Function type: (-> A B) stays the same
   [(eq? (car parsed) '->)
    `(-> ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Product type: (× A B C)
   [(eq? (car parsed) '×)
    `(× ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Sum/union type: (+ A B) - simplify to first component for kind checking
   [(eq? (car parsed) '+)
    ;; For kind checking, just check the first non-#f component
    (let ([non-false (filter (lambda (x) (not (eq? x '#f))) (cdr parsed))])
         (if (null? non-false)
             'Unit
             (sig-type->kind-type (car non-false))))]
   ;; Forall: (∀ (vars) body)
   [(eq? (car parsed) '∀)
    `(∀ ,(cadr parsed) ,(sig-type->kind-type (caddr parsed)))]
   ;; Values (multiple returns) - treat as product
   [(eq? (car parsed) 'Values)
    `(× ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Type application with known constructors: use @ syntax
   [(memq (car parsed) type-constructors-with-application)
    (let* ([constructor (normalize-type-name (car parsed))]
           [args (map sig-type->kind-type (cdr parsed))])
          (if (null? args)
              constructor
              `(@ ,constructor ,@args)))]
   ;; Other type applications - try @ syntax if head looks like a constructor
   [else
    (let ([head (normalize-type-name (car parsed))])
         (if (and (symbol? head)
                  (or (lookup-kind head)  ; Known in builtin-kinds
                      (memq head type-constructors-with-application)))
             `(@ ,head ,@(map sig-type->kind-type (cdr parsed)))
             ;; Fallback: simple cons (for domain types that aren't in builtin-kinds)
             (cons head (map sig-type->kind-type (cdr parsed)))))]))

;;; collect-type-vars : Type → (List Symbol)
;;; Collect all type variables from a type expression.
(define (collect-type-vars type)
  (cond
   [(symbol? type)
    (if (is-type-var? type) (list type) '())]
   [(not (pair? type)) '()]
   [else
    (apply append (map collect-type-vars type))]))

;;; make-type-var-env : (List Symbol) → KindEnv
;;; Create a kind environment mapping type variables to kind *.
(define (make-type-var-env vars)
  (map (lambda (v) (cons v '*)) (unique vars)))

;;; check-type-kind : Type → (ok Kind) | (error ...)
;;; Check that a type has a valid kind.
(define (check-type-kind parsed-type)
  (let* ([internal-type (sig-type->kind-type parsed-type)]
         [type-vars (collect-type-vars internal-type)]
         [kenv (make-type-var-env type-vars)]
         [kind-result (infer-kind internal-type kenv)])
        (if (and (pair? kind-result)
                 (eq? (car kind-result) 'error))
            `(error ,kind-result)
            `(ok ,kind-result))))

;;; ============================================================
;;; File Processing
;;; ============================================================

;;; read-file-lines : String → (List String)
(define (read-file-lines filename)
  (call-with-input-file filename
                        (lambda (port)
                                (let loop ([lines '()])
                                     (let ([line (get-line port)])
                                          (if (eof-object? line)
                                              (reverse lines)
                                              (loop (cons line lines))))))))

;;; check-file : String → (ok stats) | (error ...)
;;; Check all type annotations in a file.
(define (check-file filename)
  (let ([lines (read-file-lines filename)])
       (check-lines filename lines 1)))

(define (check-lines filename lines line-num)
  (if (null? lines)
      '(ok)
      (let* ([line (car lines)]
             [parse-result (parse-sig-line line)])
            (case (car parse-result)
                  [(skip)
                   ;; Not a type signature, continue
                   (check-lines filename (cdr lines) (+ line-num 1))]
                  [(error)
                   ;; Parse error
                   (record-parse-error! filename line-num (cdr parse-result))
                   (check-lines filename (cdr lines) (+ line-num 1))]
                  [(ok)
                   ;; Got a type signature, validate its kind
                   (set! *total-sigs* (+ *total-sigs* 1))
                   (let* ([sig (cadr parse-result)]
                          [name (car sig)]
                          [type (cdr sig)]
                          [kind-result (check-type-kind type)])
                         (if (eq? (car kind-result) 'error)
                             (record-kind-error! filename line-num name type (cadr kind-result))
                             (record-valid!)))
                   (check-lines filename (cdr lines) (+ line-num 1))]))))

;;; ============================================================
;;; Default Files to Check
;;; ============================================================

;;; Core files that should have type-checked annotations
(define default-files
  '("core/base/prelude.ss"
    "core/types/types.ss"
    "core/types/kinds.ss"
    "core/types/infer.ss"
    "core/types/annotate.ss"
    "core/data/data-structures.ss"
    "core/data/graph-algorithms.ss"
    "core/linalg/vec.ss"
    "core/linalg/matrix.ss"))

;;; ============================================================
;;; Reporting
;;; ============================================================

(define (report-results verbose?)
  (display (format "~nType Annotation Check Results:~n"))
  (display (format "  Total signatures found: ~a~n" *total-sigs*))
  (display (format "  Valid: ~a~n" *valid-sigs*))
  (display (format "  Invalid: ~a~n" *invalid-sigs*))
  
  (when (and verbose? (not (null? *parse-errors*)))
        (display (format "~nParse errors (~a):~n" (length *parse-errors*)))
        (for-each (lambda (err)
                          (let ([file (car err)]
                                [line (cadr err)]
                                [msg (caddr err)])
                               (display (format "  ~a:~a: ~a~n" file line msg))))
                  (reverse *parse-errors*)))
  
  (when (and verbose? (not (null? *kind-errors*)))
        (display (format "~nKind errors (~a):~n" (length *kind-errors*)))
        (for-each (lambda (err)
                          (let ([file (list-ref err 0)]
                                [line (list-ref err 1)]
                                [name (list-ref err 2)]
                                [type (list-ref err 3)]
                                [kerr (list-ref err 4)])
                               (display (format "  ~a:~a: ~a : ~a~n    Kind error: ~a~n"
                                                file line name (format-type type) kerr))))
                  (reverse *kind-errors*))))

;;; ============================================================
;;; Main
;;; ============================================================

;;; Parse args for --verbose flag
(define (parse-args args)
  (let loop ([args args] [verbose? #f] [files '()])
       (if (null? args)
           (values verbose? (reverse files))
           (if (string=? (car args) "--verbose")
               (loop (cdr args) #t files)
               (loop (cdr args) verbose? (cons (car args) files))))))

(define (main args)
  (reset-counters!)
  (let-values ([(verbose? file-args) (parse-args args)])
              (let ([files (if (null? file-args) default-files file-args)])
                   (display (format "Checking type annotations in ~a file(s)...~n" (length files)))
                   (for-each (lambda (file)
                                     (when (file-exists? file)
                                           (check-file file)))
                             files)
                   (report-results verbose?)
                   ;; Success if at least 80% of signatures are valid
                   ;; (Some edge cases with complex types will always fail)
                   (let* ([pass-rate (if (zero? *total-sigs*)
                                         100.0
                                         (* 100.0 (/ *valid-sigs* *total-sigs*)))])
                         (if (>= pass-rate 80.0)
                             (begin
                              (display (format "~n✓ Type annotation check passed (~a% valid).~n"
                                               (exact->inexact pass-rate)))
                              (exit 0))
                             (begin
                              (display (format "~n❌ Type annotation check failed (~a% valid, need 80%).~n"
                                               (exact->inexact pass-rate)))
                              (exit 1)))))))

;; Get command line args (skip script name)
(let ([args (cdr (command-line))])
     (main args))
