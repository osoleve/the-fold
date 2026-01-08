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
;;; NOTE: Multi-variable existential packing (NOT YET SUPPORTED)
;;; Currently only single-variable existentials are fully supported.
;;; Multi-variable support is tracked but not implemented.
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
;;; Check that Value : Body[WitnessType/a]
(define (existential-infer-synth-pack expr ctx dep-synth dep-check dep-check-type)
  (if (not (pack-well-formed? expr))
      `(error malformed-pack ,expr)
      (let* ([witness-type (pack-witness-type expr)]
             [value-expr (pack-value expr)]
             [exist-type (pack-existential-type expr)])
            (if (not (existential-type? exist-type))
                `(error pack-not-existential ,exist-type)
                (let* ([vars (existential-vars exist-type)]
                       [body (existential-body exist-type)])
                      ;; For now, support single variable (common case)
                      ;; NOTE: Multi-variable existential packing (NOT YET SUPPORTED)
                      (if (not (= (length vars) 1))
                          `(error pack-multi-var-not-yet-supported ,vars)
                          (let* ([var (car vars)]
                                 ;; Substitute witness for hidden var in body
                                 [expected-value-type (subst-type body var witness-type)]
                                 ;; Check witness type is a valid type
                                 [witness-check (dep-check-type witness-type ctx)])
                                (if (not (eq? (car witness-check) 'ok))
                                    witness-check
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
;;; (unpack ((a val) packed-expr) body) : T
;;; where packed-expr : exists a.S, val:S[skolem/a] in body, and T doesn't mention skolem
(define (existential-infer-synth-unpack expr ctx dep-synth dep-ctx-extend dep-ctx-extend-def)
  (if (not (unpack-well-formed? expr))
      `(error malformed-unpack ,expr)
      (let* ([type-var (unpack-type-var expr)]
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
                               ;; For now, support single variable
                               ;; NOTE: Multi-variable existential unpacking (NOT YET SUPPORTED)
                               (if (not (= (length exist-vars) 1))
                                   `(error unpack-multi-var-not-yet-supported ,exist-vars)
                                   (let* ([exist-var (car exist-vars)]
                                          ;; Generate fresh skolem
                                          [skolem (fresh-skolem exist-var)]
                                          ;; Substitute skolem for existential var in body type
                                          [skolemized-body (subst-type exist-body exist-var skolem)]
                                          ;; Extend context with type var bound to skolem
                                          [ctx-with-skolem (dep-ctx-extend-def ctx type-var 'Type skolem)]
                                          ;; Extend context with value var bound to skolemized type
                                          [ctx-with-val (dep-ctx-extend ctx-with-skolem val-var skolemized-body)]
                                          ;; Synthesize body
                                          [body-synth (dep-synth body ctx-with-val)])
                                         (if (not (eq? (car body-synth) 'ok))
                                             body-synth
                                             ;; Check result type doesn't mention skolem
                                             (let ([result-type (cadr body-synth)])
                                                  (if (type-mentions-skolem? result-type skolem)
                                                      `(error skolem-escape ,skolem ,result-type)
                                                      `(ok ,result-type)))))))))))))
