;;; lattice/meta/analyzer-effects.ss — Effect inference analyzer
;;; @module meta/analyzer-effects
;;; @requires prelude meta/lattice-walk

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)

(doc 'module 'meta/analyzer-effects)
(doc 'description "Effect inference analyzer for the lattice walker.
Infers function purity by analyzing expression trees. Processes modules
in dependency order so cross-module calls resolve to known effects
instead of 'unknown. Uses fixed-point iteration within each module.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Effect Lattice
;;; ============================================================================

(doc 'section 'effect-lattice)

(define all-effects '(pure io mutation exception nonterminating async unknown))

(doc effect? 'export #t)
(define (effect? x) (and (memq x all-effects) #t))

;;; effect-join : Effect Effect -> Effect
;;; Lattice join: combine two effects to the least upper bound.
;;; pure is bottom, unknown is top.
(doc effect-join 'export #t)
(define (effect-join e1 e2)
  (cond
    [(eq? e1 e2) e1]
    [(eq? e1 'pure) e2]
    [(eq? e2 'pure) e1]
    [(eq? e1 'unknown) e2]   ; optimistic: prefer known effects
    [(eq? e2 'unknown) e1]
    [else 'unknown]))         ; mixed known effects → unknown

;;; effects-join : (List Effect) -> Effect
(doc effects-join 'export #t)
(define (effects-join effects)
  (fold-left effect-join 'pure effects))

;;; ============================================================================
;;; Primitive Effect Database
;;; ============================================================================

(doc 'section 'primitives)

;;; seed-primitives! : Hashtable -> Void
;;; Seed the effect DB with known Chez Scheme primitive effects.
(define (seed-primitives! db)
  ;; Pure functions
  (for-each
   (lambda (name) (hashtable-set! db name 'pure))
   '(+ - * / < > = <= >= not
     car cdr cons null? pair? list? length append reverse
     list list-ref list-tail
     map filter fold-left fold-right for-each exists for-all
     apply values call-with-values
     string-length string-append substring string-ref string=? string<?
     string->symbol symbol->string
     number->string string->number
     vector vector-ref vector-length vector->list list->vector
     make-vector vector-map vector-for-each
     bytevector-length bytevector-u8-ref
     equal? eq? eqv?
     abs min max quotient remainder modulo gcd lcm
     floor ceiling round truncate exact->inexact inexact->exact
     sin cos tan asin acos atan sqrt exp log expt
     bitwise-and bitwise-or bitwise-xor bitwise-not
     bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
     char->integer integer->char char=? char<? char-alphabetic? char-numeric?
     boolean? symbol? string? number? integer? real? char? vector? bytevector?
     procedure? port? eof-object?
     assq assv assoc memq memv member
     caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
     cadddr
     sort list-sort
     make-eq-hashtable make-eqv-hashtable
     hashtable? hashtable-ref hashtable-contains? hashtable-keys hashtable-size
     even? odd? zero? positive? negative?
     exact? inexact?
     make-string string-copy
     with-exception-handler condition?
     make-list iota
     append-map filter-map
     unique
     format
     ;; Lambda/let forms produce pure values
     lambda case-lambda let let* letrec letrec*))

  ;; IO functions
  (for-each
   (lambda (name) (hashtable-set! db name 'io))
   '(display newline printf fprintf
     read write
     open-input-file open-output-file open-input-string open-output-string
     close-input-port close-output-port close-port
     call-with-input-file call-with-output-file
     call-with-port
     get-line get-char get-datum get-string-all get-string-n get-bytevector-some
     put-char put-string put-datum put-bytevector
     file-exists? delete-file rename-file
     current-directory directory-list
     system load
     current-time
     input-port-ready?
     standard-input-port standard-output-port standard-error-port
     current-input-port current-output-port current-error-port
     open-process-ports process))

  ;; Mutation functions
  (for-each
   (lambda (name) (hashtable-set! db name 'mutation))
   '(set! set-car! set-cdr!
     vector-set! vector-fill!
     bytevector-u8-set!
     hashtable-set! hashtable-delete! hashtable-update! hashtable-clear!
     string-set!))

  ;; Exception functions
  (for-each
   (lambda (name) (hashtable-set! db name 'exception))
   '(error assertion-violation raise raise-continuable
     guard)))

;;; ============================================================================
;;; Effect Inference
;;; ============================================================================

(doc 'section 'inference)

;;; infer-effect : SExp Hashtable -> Effect
;;; Infer the effect of an expression given an effect database.
;;; The DB maps symbol -> effect for all known functions.
(define (infer-effect expr db)
  (cond
    ;; Literals are pure
    [(or (number? expr) (boolean? expr) (string? expr) (char? expr))
     'pure]
    ;; Symbol references are pure (only applications have effects)
    [(symbol? expr) 'pure]
    ;; Non-pair non-atoms
    [(not (pair? expr)) 'pure]
    ;; Quote
    [(eq? (car expr) 'quote) 'pure]
    ;; Quasiquote — scan for unquotes
    [(eq? (car expr) 'quasiquote)
     (if (pair? (cdr expr))
         (infer-quasiquote-effect (cadr expr) db)
         'pure)]
    ;; Lambda: definition is pure (body effects are latent)
    [(memq (car expr) '(lambda case-lambda fn))
     'pure]
    ;; Let/let*/letrec: effects of bindings + body
    [(memq (car expr) '(let let* letrec letrec*))
     (infer-let-effect expr db)]
    ;; If: effects of test + both branches
    [(eq? (car expr) 'if)
     (let* ([test-eff (infer-effect (cadr expr) db)]
            [then-eff (if (pair? (cddr expr))
                          (infer-effect (caddr expr) db)
                          'pure)]
            [else-eff (if (and (pair? (cddr expr)) (pair? (cdddr expr)))
                         (infer-effect (cadddr expr) db)
                         'pure)])
       (effects-join (list test-eff then-eff else-eff)))]
    ;; Cond: effects of all clauses
    [(eq? (car expr) 'cond)
     (effects-join
      (map (lambda (clause)
             (if (pair? clause)
                 (effects-join (map (lambda (e) (infer-effect e db)) clause))
                 'pure))
           (cdr expr)))]
    ;; Begin: effects of all expressions
    [(eq? (car expr) 'begin)
     (effects-join (map (lambda (e) (infer-effect e db)) (cdr expr)))]
    ;; When/unless: effects of body
    [(memq (car expr) '(when unless))
     (if (and (pair? (cdr expr)) (pair? (cddr expr)))
         (effects-join
          (cons (infer-effect (cadr expr) db)
                (map (lambda (e) (infer-effect e db)) (cddr expr))))
         'pure)]
    ;; Set!: mutation
    [(eq? (car expr) 'set!)
     'mutation]
    ;; And/or: effects of all subexpressions
    [(memq (car expr) '(and or))
     (effects-join (map (lambda (e) (infer-effect e db)) (cdr expr)))]
    ;; Define inside bodies (e.g., internal defines in let)
    [(eq? (car expr) 'define) 'pure]
    ;; Doc forms are pure metadata
    [(eq? (car expr) 'doc) 'pure]
    ;; Require/load — skip (analysis-only, not execution)
    [(memq (car expr) '(require load)) 'pure]
    ;; Application: effect of function + effects of arguments
    [(pair? expr)
     (let* ([fn-name (car expr)]
            [fn-effect (if (symbol? fn-name)
                           (hashtable-ref db fn-name 'unknown)
                           (infer-effect fn-name db))]
            [arg-effects (map (lambda (a) (infer-effect a db))
                              (cdr expr))])
       (effects-join (cons fn-effect arg-effects)))]
    [else 'unknown]))

;;; infer-let-effect : SExp Hashtable -> Effect
;;; Handle let/let*/letrec/named-let forms.
(define (infer-let-effect expr db)
  (cond
    ;; Named let: (let name ([var init] ...) body ...)
    [(and (pair? (cdr expr))
          (symbol? (cadr expr))
          (pair? (cddr expr))
          (list? (caddr expr)))
     (let* ([bindings (caddr expr)]
            [body (cdddr expr)]
            [binding-effs (map (lambda (b)
                                 (if (and (pair? b) (pair? (cdr b)))
                                     (infer-effect (cadr b) db)
                                     'pure))
                               bindings)]
            [body-effs (map (lambda (e) (infer-effect e db)) body)])
       (effects-join (append binding-effs body-effs)))]
    ;; Regular let: (let ([var init] ...) body ...)
    [(and (pair? (cdr expr)) (list? (cadr expr)))
     (let* ([bindings (cadr expr)]
            [body (cddr expr)]
            [binding-effs (map (lambda (b)
                                 (if (and (pair? b) (pair? (cdr b)))
                                     (infer-effect (cadr b) db)
                                     'pure))
                               bindings)]
            [body-effs (map (lambda (e) (infer-effect e db)) body)])
       (effects-join (append binding-effs body-effs)))]
    [else 'pure]))

;;; infer-quasiquote-effect : SExp Hashtable -> Effect
;;; Walk quasiquoted data, only inferring effects in unquote positions.
(define (infer-quasiquote-effect datum db)
  (cond
    [(not (pair? datum)) 'pure]
    [(and (eq? (car datum) 'unquote) (pair? (cdr datum)))
     (infer-effect (cadr datum) db)]
    [(and (eq? (car datum) 'unquote-splicing) (pair? (cdr datum)))
     (infer-effect (cadr datum) db)]
    [else
     (effect-join (infer-quasiquote-effect (car datum) db)
                  (infer-quasiquote-effect (cdr datum) db))]))

;;; ============================================================================
;;; Definition Extraction
;;; ============================================================================

(doc 'section 'definitions)

;;; extract-definitions : (List SExp) -> (List (Pair Symbol SExp))
;;; Extract top-level define forms as (name . body) pairs.
(doc extract-definitions 'export #t)
(define (extract-definitions sexps)
  (let loop ([forms sexps] [acc '()])
    (if (null? forms)
        (reverse acc)
        (let ([form (car forms)])
          (loop (cdr forms)
                (cond
                  [(and (pair? form) (eq? (car form) 'define)
                        (pair? (cdr form)) (pair? (cddr form)))
                   (let ([second (cadr form)])
                     (cond
                       ;; (define (name args...) body...)
                       [(pair? second)
                        (cons (cons (car second)
                                    ;; Wrap body in begin if multiple
                                    (if (null? (cdddr form))
                                        (caddr form)
                                        (cons 'begin (cddr form))))
                              acc)]
                       ;; (define name value)
                       [(symbol? second)
                        (cons (cons second (caddr form)) acc)]
                       [else acc]))]
                  ;; Recurse into top-level begin
                  [(and (pair? form) (eq? (car form) 'begin))
                   (append (reverse (extract-definitions (cdr form))) acc)]
                  [else acc]))))))

;;; ============================================================================
;;; Fixed-Point Inference
;;; ============================================================================

(doc 'section 'fixpoint)

;;; infer-module-effects : (List (Pair Symbol SExp)) Hashtable -> (List (Pair Symbol Effect))
;;; Run effect inference on a module's definitions to fixed point.
;;; Iterates until no effect changes or max iterations reached.
(define (infer-module-effects defs db)
  (let ([names (map car defs)]
        [max-iters 20])
    ;; Seed all local defs as pure (optimistic)
    (for-each (lambda (name) (hashtable-set! db name 'pure)) names)
    ;; Iterate to fixed point
    (let loop ([iter 0])
      (let* ([results (map (lambda (def)
                             (cons (car def) (infer-effect (cdr def) db)))
                           defs)]
             ;; Check for changes
             [changed? (exists (lambda (r)
                                 (not (eq? (cdr r) (hashtable-ref db (car r) 'unknown))))
                               results)])
        ;; Update DB with new effects
        (for-each (lambda (r) (hashtable-set! db (car r) (cdr r))) results)
        ;; Continue if changed and under limit
        (if (and changed? (< iter max-iters))
            (loop (+ iter 1))
            results)))))

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

(doc 'section 'analyzer)

;;; make-effect-analyzer : -> Analyzer
;;; Creates an effect inference analyzer.
;;; State: eq-hashtable mapping Symbol -> Effect.
;;; Seeded with Chez primitives. Effects persist across modules
;;; (not cleaned up), so downstream modules see resolved effects.
(doc make-effect-analyzer 'export #t)
(doc make-effect-analyzer 'type '(-> Analyzer))
(define (make-effect-analyzer)
  (make-analyzer
   'effects
   ;; init: seed with Chez primitives
   (lambda ()
     (let ([db (make-eq-hashtable)])
       (seed-primitives! db)
       db))
   ;; step: analyze module, persist results
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [defs (extract-definitions sexps)])
       ;; Run fixed-point inference (modifies state in-place)
       (infer-module-effects defs state)
       ;; Return the same hashtable (modified)
       state))))

;;; ============================================================================
;;; Query helpers
;;; ============================================================================

;;; effect-db-lookup : Hashtable Symbol -> Effect
(doc effect-db-lookup 'export #t)
(define (effect-db-lookup db name)
  (hashtable-ref db name 'unknown))

;;; effect-db-module-summary : Hashtable (List Symbol) -> (List (Pair Symbol Effect))
;;; Get effects for a list of function names.
(doc effect-db-module-summary 'export #t)
(define (effect-db-module-summary db names)
  (map (lambda (n) (cons n (hashtable-ref db n 'unknown))) names))
