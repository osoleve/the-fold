;;; core/types/existential-infer.ss — Existential Type Inference
;;;
;;; Type inference for existential types.
;;;
;;; Existential types allow hiding type information behind an interface.
;;; The key operations are:
;;;   - Pack: hide a concrete type inside an existential
;;;   - Unpack: use an existential with the type held abstract (skolemized)
;;;
;;; This module provides:
;;;   - Existential type synthesis (existential-infer-synth)
;;;   - Pack expression synthesis (existential-infer-synth-pack)
;;;   - Unpack expression synthesis (existential-infer-synth-unpack)
;;;
;;; Multi-variable existentials are fully supported:
;;;   - Pack: (pack (T1 T2 ...) value : (∃ ((a : Type) (b : Type)) body))
;;;   - Unpack: (unpack (((a b) val) packed) body)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss
;;;   - existential.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/existential.ss")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; subst-types : Type x (List Symbol) x (List Type) -> Type
;;; Substitute multiple types for multiple variables simultaneously.
(define (subst-types type vars witnesses)
  (fold-left (lambda (t var-witness)
                     (subst-type t (car var-witness) (cdr var-witness)))
             type
             (map cons vars witnesses)))

;;; check-all-types : (List Type) x Context x (Type x Ctx -> Result) -> (ok) | (error ...)
;;; Check that all types in a list are valid types.
(define (check-all-types types ctx dep-check-type)
  (let loop ([ts types])
       (if (null? ts)
           '(ok)
           (let ([check (dep-check-type (car ts) ctx)])
                (if (not (eq? (car check) 'ok))
                    check
                    (loop (cdr ts)))))))

;;; extend-ctx-with-skolems : Context x (List Symbol) x (List Symbol) x (Ctx x Symbol x Type x Val -> Ctx) -> Context
;;; Extend context with multiple type var -> skolem bindings.
(define (extend-ctx-with-skolems ctx type-vars skolems dep-ctx-extend-def)
  (fold-left (lambda (c var-skolem)
                     (dep-ctx-extend-def c (car var-skolem) 'Type (cdr var-skolem)))
             ctx
             (map cons type-vars skolems)))

;;; ============================================================
;;; Existential Type Synthesis
;;; ============================================================

;;; existential-infer-synth : Expr x Context x (Expr x Ctx -> Result) x (Type x Ctx -> Result)
;;;                           x (Ctx x Symbol x Type -> Ctx)
;;;                           -> (Result Type Error)
;;; (exists ((a : K)) T) : Type when K is valid and T : Type under a:K
(define (existential-infer-synth expr ctx dep-synth dep-check-type dep-ctx-extend)
  (if (not (existential-well-formed? expr))
      `(error malformed-existential-type ,expr)
      (let* ([var-bindings (cadr expr)]
             [body (caddr expr)])
            ;; Check each binding has a valid kind
            (let loop ([bindings var-bindings] [ctx ctx])
                 (if (null? bindings)
                     ;; All bindings valid, check body is a type
                     (let ([body-check (dep-check-type body ctx)])
                          (if (eq? (car body-check) 'ok)
                              '(ok Type)
                              body-check))
                     (let* ([b (car bindings)]
                            [var (binding-var b)]
                            [kind (binding-type b)])
                           ;; Kind should be Type or a valid kind expression
                           (if (or (eq? kind 'Type) (eq? kind '*))
                               (loop (cdr bindings)
                                     (dep-ctx-extend ctx var kind))
                               ;; Check kind is valid
                               (let ([kind-check (dep-check-type kind ctx)])
                                    (if (not (eq? (car kind-check) 'ok))
                                        `(error invalid-existential-kind ,kind)
                                        (loop (cdr bindings)
                                              (dep-ctx-extend ctx var kind)))))))))))

;;; ============================================================
;;; Pack Expression Synthesis
;;; ============================================================

;;; existential-infer-synth-pack : Expr x Context x (Expr x Ctx -> Result) x (Expr x Type x Ctx -> Result)
;;;                                 x (Type x Ctx -> Result)
;;;                                 -> (Result Type Error)
;;; (pack WitnessType Value : ExistentialType) : ExistentialType
;;; (pack (T1 T2 ...) Value : ExistentialType) : ExistentialType  ; multi-var
;;; Check that Value : Body[Witnesses/vars]
(define (existential-infer-synth-pack expr ctx dep-synth dep-check dep-check-type)
  (if (not (pack-well-formed? expr))
      `(error malformed-pack ,expr)
      (let* ([witness-types (pack-witness-types expr)]
             [value-expr (pack-value expr)]
             [exist-type (pack-existential-type expr)])
            (if (not (existential-type? exist-type))
                `(error pack-not-existential ,exist-type)
                (let* ([vars (existential-vars exist-type)]
                       [body (existential-body exist-type)])
                      ;; Check witness count matches var count
                      (if (not (= (length vars) (length witness-types)))
                          `(error pack-witness-count-mismatch
                            (expected ,(length vars))
                            (got ,(length witness-types))
                            (vars ,vars)
                            (witnesses ,witness-types))
                          ;; Substitute all witnesses for all vars in body
                          (let* ([expected-value-type (subst-types body vars witness-types)]
                                 ;; Check all witness types are valid
                                 [witness-checks (check-all-types witness-types ctx dep-check-type)])
                                (if (not (eq? (car witness-checks) 'ok))
                                    witness-checks
                                    ;; Check value has expected type
                                    (let ([value-check (dep-check value-expr expected-value-type ctx)])
                                         (if (eq? (car value-check) 'ok)
                                             `(ok ,exist-type)
                                             value-check))))))))))

;;; ============================================================
;;; Unpack Expression Synthesis
;;; ============================================================

;;; existential-infer-synth-unpack : Expr x Context x (Expr x Ctx -> Result)
;;;                                   x (Ctx x Symbol x Type -> Ctx) x (Ctx x Symbol x Type x Val -> Ctx)
;;;                                   -> (Result Type Error)
;;; (unpack ((a val) packed-expr) body) : T              ; single-var
;;; (unpack (((a b ...) val) packed-expr) body) : T      ; multi-var
;;; where packed-expr : exists a.S, val:S[skolem/a] in body, and T doesn't mention skolem
(define (existential-infer-synth-unpack expr ctx dep-synth dep-ctx-extend dep-ctx-extend-def)
  (if (not (unpack-well-formed? expr))
      `(error malformed-unpack ,expr)
      (let* ([type-vars (unpack-type-vars expr)]
             [val-var (unpack-val-var expr)]
             [packed-expr (unpack-packed-expr expr)]
             [body (unpack-body expr)]
             ;; Synthesize packed expression
             [packed-synth (dep-synth packed-expr ctx)])
            (if (not (eq? (car packed-synth) 'ok))
                packed-synth
                (let ([packed-type (cadr packed-synth)])
                     (if (not (existential-type? packed-type))
                         `(error unpack-not-existential ,packed-type)
                         (let* ([exist-vars (existential-vars packed-type)]
                                [exist-body (existential-body packed-type)])
                               ;; Check type var count matches existential var count
                               (if (not (= (length exist-vars) (length type-vars)))
                                   `(error unpack-type-var-count-mismatch
                                     (expected ,(length exist-vars))
                                     (got ,(length type-vars))
                                     (exist-vars ,exist-vars)
                                     (type-vars ,type-vars))
                                   ;; Generate fresh skolems for each existential var
                                   (let* ([skolems (map fresh-skolem exist-vars)]
                                          ;; Substitute all skolems for all existential vars in body type
                                          [skolemized-body (subst-types exist-body exist-vars skolems)]
                                          ;; Extend context with all type vars bound to their skolems
                                          [ctx-with-skolems (extend-ctx-with-skolems ctx type-vars skolems dep-ctx-extend-def)]
                                          ;; Extend context with value var bound to skolemized type
                                          [ctx-with-val (dep-ctx-extend ctx-with-skolems val-var skolemized-body)]
                                          ;; Synthesize body
                                          [body-synth (dep-synth body ctx-with-val)])
                                         (if (not (eq? (car body-synth) 'ok))
                                             body-synth
                                             ;; Check result type doesn't mention any skolem
                                             (let ([result-type (cadr body-synth)])
                                                  (if (type-mentions-any-skolem? result-type skolems)
                                                      `(error skolem-escape ,skolems ,result-type)
                                                      `(ok ,result-type)))))))))))))
