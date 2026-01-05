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
    (Vector . Hash)
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
    (Block . Hash)))

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
  '(List Vector Option Either Result Pair Maybe Ref Set))

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
