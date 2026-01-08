;;; core/types/gadt-infer.ss — GADT Type Inference
;;;
;;; Type inference for Generalized Algebraic Data Types (GADTs).
;;;
;;; GADTs extend inductive types with type indices that can be refined
;;; by pattern matching. The key innovation is that each constructor can
;;; specify a more precise return type, and matching on that constructor
;;; brings the type refinement into scope.
;;;
;;; This module provides:
;;;   - GADT declaration synthesis (dep-synth-gadt)
;;;   - GADT case synthesis with type refinement (dep-synth-gadt-case)
;;;   - GADT registry management
;;;   - Type equation extraction and refinement application
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss
;;;   - gadt.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/gadt.ss")

;;; ============================================================
;;; Forward Declarations
;;; ============================================================
;;;
;;; These functions are provided by dep-infer.ss and used here.
;;; The main dep-infer.ss file loads this module and provides these.

;;; Note: dep-synth, dep-check, dep-check-type, dep-ctx-extend,
;;; dep-ctx-extend-def, dep-types-equal? are expected to be defined
;;; when this module is loaded as part of dep-infer.ss.

;;; ============================================================
;;; GADT Registry (Global State)
;;; ============================================================

(define *gadt-registry* '())

;;; reset-gadt-registry! : -> Unit
(define (reset-gadt-registry!)
  (set! *gadt-registry* '()))

;;; register-gadt! : GADTDecl -> Unit
(define (register-gadt! decl)
  (set! *gadt-registry* (gadt-registry-add *gadt-registry* decl)))

;;; lookup-gadt : Symbol -> GADTDecl | #f
(define (lookup-gadt name)
  (gadt-registry-lookup *gadt-registry* name))

;;; ============================================================
;;; GADT Declaration Synthesis
;;; ============================================================

;;; gadt-infer-synth : GADTDecl x Context x (Expr x Ctx -> Result) x (Expr x Type x Ctx -> Result) x (Type x Ctx -> Result)
;;;                    -> (Result Type Error)
;;; Type-check a GADT declaration and register it.
;;; Takes dep-synth, dep-check, dep-check-type as parameters for dependency injection.
(define (gadt-infer-synth decl ctx dep-synth dep-check dep-check-type)
  (if (not (gadt-well-formed? decl))
      `(error malformed-gadt-declaration ,decl)
      (let* ([name (gadt-name decl)]
             [index-kinds (gadt-index-kinds decl)]
             [ctors (gadt-constructors decl)])
            ;; Check all constructor types
            (let ([ctor-checks (gadt-infer-check-ctors name index-kinds ctors ctx dep-check-type)])
                 (if (not (eq? (car ctor-checks) 'ok))
                     ctor-checks
                     (begin
                      (register-gadt! decl)
                      `(ok (gadt-type ,name))))))))

;;; gadt-infer-check-ctors : Symbol x (List (Symbol . Kind)) x (List (Symbol . Type)) x Context x (Type x Ctx -> Result)
;;;                          -> (ok) | (error ...)
(define (gadt-infer-check-ctors gadt-name index-kinds ctors ctx dep-check-type)
  (if (null? ctors)
      '(ok)
      (let* ([ctor (car ctors)]
             [ctor-name (car ctor)]
             [ctor-type (cdr ctor)]
             [type-check (dep-check-type ctor-type ctx)])
            (if (not (eq? (car type-check) 'ok))
                `(error ctor-type-invalid ,ctor-name ,type-check)
                (if (not (gadt-ctor-returns-gadt? ctor-type gadt-name))
                    `(error ctor-wrong-return-type ,ctor-name ,ctor-type ,gadt-name)
                    (gadt-infer-check-ctors gadt-name index-kinds (cdr ctors) ctx dep-check-type))))))

;;; ============================================================
;;; GADT Case Synthesis
;;; ============================================================

;;; gadt-infer-synth-case : Expr x Context x (Expr x Ctx -> Result) x (Expr x Type x Ctx -> Result) x (Type x Type x Ctx -> Bool)
;;;                          x (Ctx x Symbol x Type -> Ctx) x (Ctx x Symbol x Type x Val -> Ctx)
;;;                          -> (Result Type Error)
;;; Type-check a gadt-case expression with type refinement.
(define (gadt-infer-synth-case expr ctx dep-synth dep-check dep-types-equal?
                               dep-ctx-extend dep-ctx-extend-def)
  (let* ([scrutinee (gadt-case-scrutinee expr)]
         [clauses (gadt-case-clauses expr)]
         [scrut-synth (dep-synth scrutinee ctx)])
        (if (not (eq? (car scrut-synth) 'ok))
            scrut-synth
            (let ([scrut-type (cadr scrut-synth)])
                 (let ([gadt-info (gadt-infer-decompose-type scrut-type)])
                      (if (not gadt-info)
                          `(error scrutinee-not-gadt-type ,scrut-type)
                          (let* ([gadt-name (car gadt-info)]
                                 [scrut-indices (cdr gadt-info)]
                                 [gadt-decl (lookup-gadt gadt-name)])
                                (if (not gadt-decl)
                                    `(error unknown-gadt ,gadt-name)
                                    (gadt-infer-synth-clauses gadt-decl scrut-indices clauses ctx
                                                              dep-synth dep-types-equal?
                                                              dep-ctx-extend dep-ctx-extend-def)))))))))

;;; gadt-infer-decompose-type : Type -> (Symbol . (List Type)) | #f
(define (gadt-infer-decompose-type t)
  (cond
   [(symbol? t)
    (if (lookup-gadt t)
        (cons t '())
        #f)]
   [(and (pair? t) (symbol? (car t)))
    (if (lookup-gadt (car t))
        (cons (car t) (cdr t))
        #f)]
   [else #f]))

;;; gadt-infer-synth-clauses : GADTDecl x (List Type) x (List Clause) x Context
;;;                            x (Expr x Ctx -> Result) x (Type x Type x Ctx -> Bool)
;;;                            x (Ctx x Symbol x Type -> Ctx) x (Ctx x Symbol x Type x Val -> Ctx)
;;;                            -> (Result Type Error)
(define (gadt-infer-synth-clauses gadt-decl scrut-indices clauses ctx
                                  dep-synth dep-types-equal?
                                  dep-ctx-extend dep-ctx-extend-def)
  (if (null? clauses)
      `(error empty-gadt-case)
      (let loop ([clauses clauses] [types '()])
           (if (null? clauses)
               ;; All clauses checked, unify return types
               (gadt-infer-unify-return-types (reverse types) ctx dep-types-equal?)
               (let ([result (gadt-infer-synth-clause gadt-decl scrut-indices (car clauses) ctx
                                                      dep-synth dep-ctx-extend dep-ctx-extend-def)])
                    (if (not (eq? (car result) 'ok))
                        result
                        (loop (cdr clauses) (cons (cadr result) types))))))))

;;; gadt-infer-synth-clause : GADTDecl x (List Type) x Clause x Context
;;;                           x (Expr x Ctx -> Result) x (Ctx x Symbol x Type -> Ctx)
;;;                           x (Ctx x Symbol x Type x Val -> Ctx)
;;;                           -> (Result Type Error)
(define (gadt-infer-synth-clause gadt-decl scrut-indices clause ctx
                                 dep-synth dep-ctx-extend dep-ctx-extend-def)
  (let* ([pattern (gadt-clause-pattern clause)]
         [body (gadt-clause-body clause)]
         [ctor-name (gadt-pattern-ctor pattern)]
         [pattern-vars (gadt-pattern-vars pattern)]
         [gadt-name (gadt-name gadt-decl)]
         [index-vars (gadt-index-vars gadt-decl)]
         [ctor-type (gadt-ctor-type gadt-decl ctor-name)])
        (if (not ctor-type)
            `(error unknown-constructor ,ctor-name ,gadt-name)
            (let* ([ctor-return-indices (gadt-ctor-return-indices ctor-type gadt-name)]
                   [refinements (extract-type-equations scrut-indices ctor-return-indices index-vars)])
                  ;; Check for structural mismatch (unreachable branch)
                  (if (eq? refinements 'mismatch)
                      `(error unreachable-gadt-branch
                        (constructor ,ctor-name)
                        (scrutinee-indices ,scrut-indices)
                        (ctor-indices ,ctor-return-indices))
                      (let* ([raw-param-types (gadt-ctor-param-types ctor-type)]
                             [param-types (map (lambda (t) (apply-refinements t refinements)) raw-param-types)]
                             [expected-arity (length param-types)]
                             [actual-arity (length pattern-vars)])
                            (if (not (= expected-arity actual-arity))
                                `(error pattern-arity-mismatch
                                  (constructor ,ctor-name)
                                  (expected ,expected-arity)
                                  (got ,actual-arity))
                                ;; Build refined context
                                (let* ([ctx-with-refs (gadt-infer-apply-refinements ctx refinements dep-ctx-extend-def)]
                                       [ctx-with-vars (gadt-infer-bind-pattern-vars ctx-with-refs pattern-vars param-types dep-ctx-extend)])
                                      (dep-synth body ctx-with-vars)))))))))

;;; gadt-infer-apply-refinements : Context x (List (Symbol . Type)) x (Ctx x Symbol x Type x Val -> Ctx) -> Context
(define (gadt-infer-apply-refinements ctx refinements dep-ctx-extend-def)
  (fold-left (lambda (c ref)
                     (dep-ctx-extend-def c (car ref) 'Type (cdr ref)))
             ctx
             refinements))

;;; gadt-infer-bind-pattern-vars : Context x (List Symbol) x (List Type) x (Ctx x Symbol x Type -> Ctx) -> Context
(define (gadt-infer-bind-pattern-vars ctx vars types dep-ctx-extend)
  (fold-left (lambda (c pair)
                     (dep-ctx-extend c (car pair) (cdr pair)))
             ctx
             (map cons vars types)))

;;; gadt-infer-unify-return-types : (List Type) x Context x (Type x Type x Ctx -> Bool) -> (Result Type Error)
(define (gadt-infer-unify-return-types types ctx dep-types-equal?)
  (if (null? types)
      `(error empty-gadt-case)
      (let ([first-type (car types)])
           (let loop ([remaining (cdr types)])
                (if (null? remaining)
                    `(ok ,first-type)
                    (if (dep-types-equal? first-type (car remaining) ctx)
                        (loop (cdr remaining))
                        `(error gadt-case-branch-type-mismatch
                          (first ,first-type)
                          (other ,(car remaining)))))))))

;;; ============================================================
;;; Test GADT Setup Helper
;;; ============================================================

;;; gadt-define-test-gadts! : -> Unit
;;; Helper to set up test GADTs.
(define (gadt-define-test-gadts!)
  (reset-gadt-registry!)
  (register-gadt!
   '(gadt (Expr a)
     [Lit  : (-> Int (Expr Int))]
     [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
     [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]
     [If   : (forall (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))]))
  (register-gadt!
   '(gadt (Vec (n : Nat) a)
     [VNil  : (Vec 0 a)]
     [VCons : (forall (m) (-> a (Vec m a) (Vec (succ m) a)))])))
