;;; core/types/existential.ss — Existential Types
;;;
;;; Existential types allow hiding type information behind an interface.
;;; The key insight is that an existential packages a value with its
;;; operations, but the concrete type is hidden from consumers.
;;;
;;; Syntax:
;;;   Type:   (∃ ((a : Kind)) T)     ; Type a is hidden in T
;;;   Pack:   (pack WitnessType Value : ExistentialType)
;;;   Unpack: (unpack ((a val) packed-expr) body)
;;;
;;; Example:
;;;   ;; A "showable" value hides its concrete type
;;;   (: showable (∃ ((a : Type)) (× a (-> a String))))
;;;   (define showable (pack Int 42 int->string : (∃ ((a : Type)) (× a (-> a String)))))
;;;
;;;   ;; Using it - we can't access the concrete type
;;;   (unpack ((a pair) showable)
;;;     (let ([val (fst pair)]
;;;           [show (snd pair)])
;;;       (show val)))  ; Returns "42"
;;;
;;; Existentials are related to Sigma types but with different scoping:
;;; - Sigma types expose the first component's type in the second
;;; - Existentials hide the type entirely
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")

;;; ============================================================
;;; Existential Type Grammar
;;; ============================================================
;;;
;;;   ExistentialType ::= (∃ (Binding ...) Body)
;;;
;;;   Binding ::= (Symbol : Kind)
;;;
;;; The hidden type variables in Bindings can appear in Body.

;;; ============================================================
;;; Existential Type Predicates
;;; ============================================================

;;; existential-type? : SExpr → Boolean
;;; Check if this is an existential type.
;;; Accepts both ∃ and exists as valid syntax.
(define (existential-type? t)
  (and (pair? t) (or (eq? (car t) '∃) (eq? (car t) 'exists))))

;;; existential-well-formed? : SExpr → Boolean
;;; Check if an existential type is well-formed.
(define (existential-well-formed? t)
  (and (existential-type? t)
       (= (length t) 3)
       (list? (cadr t))
       (not (null? (cadr t)))
       (andmap typed-binding? (cadr t))))

;;; ============================================================
;;; Existential Type Accessors
;;; ============================================================

;;; existential-vars : SExpr → (List Symbol)
;;; Get the hidden type variable names.
(define (existential-vars t)
  (if (existential-type? t)
      (map binding-var (cadr t))
      '()))

;;; existential-var-kinds : SExpr → (List (Symbol . Kind))
;;; Get the hidden type variables with their kinds.
(define (existential-var-kinds t)
  (if (existential-type? t)
      (map (lambda (b) (cons (binding-var b) (binding-type b)))
           (cadr t))
      '()))

;;; existential-body : SExpr → Type
;;; Get the body type (may reference hidden vars).
(define (existential-body t)
  (if (existential-type? t)
      (caddr t)
      t))

;;; binding-var : Binding → Symbol
;;; Extract variable from (var : type) binding.
(define (binding-var b)
  (if (and (pair? b) (= (length b) 3) (eq? (cadr b) ':))
      (car b)
      (if (symbol? b) b #f)))

;;; binding-type : Binding → Type
;;; Extract type from (var : type) binding.
(define (binding-type b)
  (if (and (pair? b) (= (length b) 3) (eq? (cadr b) ':))
      (caddr b)
      'Type))

;;; ============================================================
;;; Existential Type Construction
;;; ============================================================

;;; t-exists : (List (Symbol . Kind)) × Type → SExpr
;;; Construct an existential type from bindings and body.
(define (t-exists var-kinds body)
  `(∃ ,(map (lambda (vk) `(,(car vk) : ,(cdr vk))) var-kinds)
    ,body))

;;; t-exists-simple : Symbol × Type → SExpr
;;; Single type variable with kind Type.
(define (t-exists-simple var body)
  (t-exists `((,var . Type)) body))

;;; ============================================================
;;; Pack Expression Predicates
;;; ============================================================

;;; pack-expr? : SExpr → Boolean
;;; Check if this is a pack expression.
;;; Form: (pack WitnessType Value : ExistentialType)
(define (pack-expr? e)
  (and (pair? e) (eq? (car e) 'pack)))

;;; pack-well-formed? : SExpr → Boolean
;;; Check if a pack expression is well-formed.
;;; Form: (pack Witness Value : ExistentialType)
;;; where Witness is either a single type or (Type1 Type2 ...) for multi-var
(define (pack-well-formed? e)
  (and (pack-expr? e)
       (= (length e) 5)
       (eq? (cadddr e) ':)))

;;; pack-witness-types : SExpr → (List Type)
;;; Get the witness types as a list (for both single and multi-var).
(define (pack-witness-types e)
  (if (pack-well-formed? e)
      (let ([w (cadr e)])
           (if (and (pair? w) (not (eq? (car w) '->)))
               w  ; Already a list of types
               (list w)))  ; Single type, wrap in list
      '()))

;;; pack-witness-type : SExpr → Type
;;; Get the witness type (for single-var pack, returns single type).
;;; DEPRECATED: Use pack-witness-types for multi-var support.
(define (pack-witness-type e)
  (if (pack-well-formed? e)
      (cadr e)
      #f))

;;; pack-value : SExpr → SExpr
;;; Get the value being packed.
(define (pack-value e)
  (if (pack-well-formed? e)
      (caddr e)
      #f))

;;; pack-existential-type : SExpr → SExpr
;;; Get the target existential type.
(define (pack-existential-type e)
  (if (pack-well-formed? e)
      (car (cddddr e))
      #f))

;;; ============================================================
;;; Unpack Expression Predicates
;;; ============================================================

;;; unpack-expr? : SExpr → Boolean
;;; Check if this is an unpack expression.
;;; Form: (unpack ((type-var val-var) packed-expr) body)
(define (unpack-expr? e)
  (and (pair? e) (eq? (car e) 'unpack)))

;;; unpack-well-formed? : SExpr → Boolean
;;; Check if an unpack expression is well-formed.
;;; Single-var: (unpack ((type-var val-var) packed-expr) body)
;;; Multi-var:  (unpack (((t1 t2 ...) val-var) packed-expr) body)
(define (unpack-well-formed? e)
  (and (unpack-expr? e)
       (= (length e) 3)
       (pair? (cadr e))
       (= (length (cadr e)) 2)
       (pair? (caadr e))
       (>= (length (caadr e)) 2)))

;;; unpack-type-vars : SExpr → (List Symbol)
;;; Get the type variable names as a list (for both single and multi-var).
(define (unpack-type-vars e)
  (if (unpack-well-formed? e)
      (let ([binding (caadr e)])
           (if (and (pair? (car binding)) (not (symbol? (car binding))))
               (car binding)  ; Multi-var: ((a b ...) val) -> (a b ...)
               (list (car binding))))  ; Single-var: (a val) -> (a)
      '()))

;;; unpack-type-var : SExpr → Symbol
;;; Get the type variable name (for single-var unpack).
;;; DEPRECATED: Use unpack-type-vars for multi-var support.
(define (unpack-type-var e)
  (if (unpack-well-formed? e)
      (car (caadr e))
      #f))

;;; unpack-val-var : SExpr → Symbol
;;; Get the value variable name.
(define (unpack-val-var e)
  (if (unpack-well-formed? e)
      (let ([binding (caadr e)])
           (if (and (pair? (car binding)) (not (symbol? (car binding))))
               (cadr binding)  ; Multi-var: ((a b ...) val) -> val
               (cadr binding)))  ; Single-var: (a val) -> val
      #f))

;;; unpack-packed-expr : SExpr → SExpr
;;; Get the packed expression.
(define (unpack-packed-expr e)
  (if (unpack-well-formed? e)
      (cadadr e)
      #f))

;;; unpack-body : SExpr → SExpr
;;; Get the body expression.
(define (unpack-body e)
  (if (unpack-well-formed? e)
      (caddr e)
      #f))

;;; ============================================================
;;; Skolemization
;;; ============================================================
;;;
;;; When unpacking an existential, we introduce a fresh "skolem"
;;; constant for the hidden type. This skolem is:
;;; - Unknown to the outside world
;;; - Must not escape in the result type
;;;
;;; Skolem escape checking ensures type soundness.

;;; Skolem counter for generating fresh names
(define *skolem-counter* 0)

;;; reset-skolem-counter! : → Unit
(define (reset-skolem-counter!)
  (set! *skolem-counter* 0))

;;; fresh-skolem : Symbol → Symbol
;;; Generate a fresh skolem constant for an existential variable.
(define (fresh-skolem base)
  (set! *skolem-counter* (+ *skolem-counter* 1))
  (string->symbol
   (string-append (symbol->string base)
                  "!"
                  (number->string *skolem-counter*))))

;;; skolem? : Symbol → Boolean
;;; Check if a symbol is a skolem constant (contains ! followed by digits).
(define (skolem? sym)
  (and (symbol? sym)
       (let ([s (symbol->string sym)])
            (let loop ([i 0])
                 (cond
                  [(>= i (string-length s)) #f]
                  [(char=? #\! (string-ref s i))
                   ;; Found !, check if followed by digits
                   (and (< (+ i 1) (string-length s))
                        (char-numeric? (string-ref s (+ i 1))))]
                  [else (loop (+ i 1))])))))

;;; skolemize-existential : ExistentialType → (values Type (List (Symbol . Symbol)))
;;; Replace existential variables with fresh skolem constants.
;;; Returns the skolemized body type and a mapping from vars to skolems.
(define (skolemize-existential exist-type)
  (if (not (existential-type? exist-type))
      (values exist-type '())
      (let* ([vars (existential-vars exist-type)]
             [body (existential-body exist-type)]
             [skolems (map (lambda (v) (cons v (fresh-skolem v))) vars)]
             [skolemized-body (fold-left
                               (lambda (t mapping)
                                       (subst-type t (car mapping) (cdr mapping)))
                               body
                               skolems)])
            (values skolemized-body skolems))))

;;; type-mentions-skolem? : Type × Symbol → Boolean
;;; Check if a type mentions a specific skolem constant.
(define (type-mentions-skolem? t skolem)
  (cond
   [(eq? t skolem) #t]
   [(not (pair? t)) #f]
   [else (ormap (lambda (sub) (type-mentions-skolem? sub skolem)) t)]))

;;; type-mentions-any-skolem? : Type × (List Symbol) → Boolean
;;; Check if a type mentions any of the given skolems.
(define (type-mentions-any-skolem? t skolems)
  (ormap (lambda (sk) (type-mentions-skolem? t sk)) skolems))

;;; ============================================================
;;; Existential Type Substitution
;;; ============================================================

;;; existential-subst : SExpr × Symbol × Type → Type
;;; Substitute witness type for hidden variable in existential body.
(define (existential-subst exist-type var witness)
  (let ([body (existential-body exist-type)])
       (subst-type body var witness)))

;;; ============================================================
;;; Free Variables in Existentials
;;; ============================================================

;;; existential-free-vars : SExpr → (List Symbol)
;;; Get free type variables in an existential type.
;;; The hidden variables are bound, not free.
(define (existential-free-vars t)
  (if (existential-type? t)
      (let ([bound-vars (existential-vars t)]
            [body-vars (free-tvars (existential-body t))])
           (filter (lambda (v) (not (memq v bound-vars))) body-vars))
      (free-tvars t)))

;;; ============================================================
;;; Existential Type Display
;;; ============================================================

;;; existential->string : SExpr → String
(define (existential->string t)
  (if (existential-type? t)
      (string-append "∃"
                     (format-bindings (cadr t))
                     ". "
                     (type->string (existential-body t)))
      (type->string t)))

(define (format-bindings bindings)
  (if (null? bindings)
      ""
      (string-append
       " "
       (join-strings " "
                     (map (lambda (b)
                                  (if (and (pair? b) (eq? (cadr b) ':))
                                      (string-append "(" (symbol->string (car b))
                                                     " : " (type->string (caddr b)) ")")
                                      (symbol->string b)))
                          bindings)))))

;;; ============================================================
;;; Existential Type Inference
;;; ============================================================
;;;
;;; The following inference functions are used by dep-infer.ss.
;;; They depend on dep-synth, dep-check, etc. being defined when called.

;;; ============================================================
;;; Inference Helper Functions
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
