;;; shell/tools/typed-holes.ss — Type-Driven Development with Holes
;;;
;;; Agda/Idris-inspired workflow for The Fold:
;;;   (?) or (? name) — placeholder expressions
;;;   (hole-type expr) — show what type is needed
;;;   (hole-fits expr) — show available bindings that fit
;;;   (hole-refine expr) — narrow holes with partial info
;;;   (case-split expr var) — split on sum type constructors
;;;
;;; Example:
;;;   (let ((xs (list 1 2 3)))
;;;     (map (?) xs))
;;;
;;;   Hole has type: (-> Int a)
;;;   Available fits:
;;;     - (fn (x) ...) where x : Int
;;;     - square : (-> Int Int)
;;;     - show : (-> Int String)
;;;
;;; This is Shell code: provides REPL integration.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - infer.ss
;;;   - types.ss
;;;   - index.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/infer.ss")
(load "shell/tools/index.ss")

;;; ====
;;; Hole Detection
;;; ====

;;; hole? : Any → Boolean
;;; Is this a hole expression?
(define (hole? x)
  (or (eq? x '?)
      (and (pair? x) (eq? (car x) '?) (= (length x) 2))))

;;; hole-name : Expr → Symbol | #f
;;; Get the name of a named hole, or #f for anonymous.
(define (hole-name h)
  (if (and (pair? h) (eq? (car h) '?))
      (cadr h)
      #f))

;;; find-holes : Expr → (List (Path × Hole))
;;; Find all holes in an expression with their paths.
(define (find-holes expr)
  (find-holes-aux expr '()))

(define (find-holes-aux expr path)
  (cond
   [(hole? expr)
    (list (cons (reverse path) expr))]
   [(not (pair? expr))
    '()]
   ;; Special handling for let to properly traverse bindings
   [(eq? (car expr) 'let)
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           ;; Find holes in bindings (path index 1 for bindings list)
           [binding-holes
            (let bloop ([bs bindings] [bidx 0] [bresults '()])  ;; 0-based index
                 (if (null? bs)
                     bresults
                     (let* ([binding (car bs)]
                            ;; Each binding is (var init), hole would be in init at index 1
                            [init-holes (find-holes-aux (cadr binding) (cons 1 (cons bidx (cons 1 path))))])
                           (bloop (cdr bs) (+ bidx 1) (append bresults init-holes)))))]
           ;; Find holes in body (path index 2)
           [body-holes (find-holes-aux body (cons 2 path))])
          (append binding-holes body-holes))]
   [else
    (let loop ([items (cdr expr)] [idx 1] [results '()])
         (if (null? items)
             results
             (loop (cdr items)
                   (+ idx 1)
                   (append results
                           (find-holes-aux (car items)
                                           (cons idx path))))))]))

;;; ====
;;; Type Context Building
;;; ====

;;; expr-context : Expr × Path → TEnv
;;; Build the type environment visible at a hole location.
(define (expr-context expr path)
  (expr-context-aux expr path '()))

(define (expr-context-aux expr path env)
  (cond
   [(null? path) env]
   [(not (pair? expr)) env]
   [(eq? (car expr) 'let)
    ;; (let ((x e1) ...) body)
    ;; Path structure:
    ;;   (1 binding-idx sub-idx ...) -> in bindings list, specific binding
    ;;   (2 ...) -> in body
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [idx (car path)]
           [rest-path (cdr path)])
          (cond
           ;; idx=2 means we're in the body
           [(= idx 2)
            ;; Build env with all bindings for body context
            (let ([new-env (fold-left
                            (lambda (e b)
                                    (let* ([bvar (car b)]
                                           [bexpr (cadr b)]
                                           [result (infer bexpr e)])
                                          (if (eq? (car result) 'ok)
                                              (tenv-extend e bvar (cadr result))
                                              e)))
                            env
                            bindings)])
                 (expr-context-aux body rest-path new-env))]
           ;; idx=1 means we're in the bindings list
           [(= idx 1)
            (if (null? rest-path)
                env  ;; At bindings list itself
                ;; Next path element tells us which binding (0-indexed)
                (let* ([binding-idx (car rest-path)]
                       [binding-rest (cdr rest-path)]
                       ;; Only include bindings BEFORE this one in env
                       [prior-bindings (if (> binding-idx 0)
                                           (take-n bindings binding-idx)
                                           '())]
                       [partial-env (fold-left
                                     (lambda (e b)
                                             (let* ([bvar (car b)]
                                                    [bexpr (cadr b)]
                                                    [result (infer bexpr e)])
                                                   (if (eq? (car result) 'ok)
                                                       (tenv-extend e bvar (cadr result))
                                                       e)))
                                     env
                                     prior-bindings)])
                      (if (or (null? binding-rest) (>= binding-idx (length bindings)))
                          partial-env
                          ;; Recurse into the binding's init expression
                          (let* ([binding (list-ref bindings binding-idx)]
                                 [sub-idx (car binding-rest)])
                                ;; sub-idx 1 = init expression (index 0 is var name)
                                (if (= sub-idx 1)
                                    (expr-context-aux (cadr binding) (cdr binding-rest) partial-env)
                                    partial-env)))))]
           [else env]))]
   [(eq? (car expr) 'fn)
    ;; (fn (x y) body)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (map (lambda (_) (fresh-tvar)) params)]
           [new-env (tenv-extend* env (map cons params param-types))])
          (expr-context-aux body (cdr path) new-env))]
   [else
    (let* ([idx (car path)]
           [subexpr (list-ref expr idx)])
          (expr-context-aux subexpr (cdr path) env))]))

;;; ====
;;; Hole Type Inference
;;; ====

;;; infer-hole-type : Expr × Path → Type | Error
;;; Infer what type a hole at the given path should have.
(define (infer-hole-type expr path)
  (let* ([ctx (expr-context expr path)]
         [hole-expr (path-get expr path)]
         ;; Find the parent expression and position
         [parent-path (if (null? path) '() (reverse (cdr (reverse path))))]
         [position (if (null? path) 0 (car (reverse path)))]
         [parent (path-get expr parent-path)])
        (infer-hole-from-context parent position ctx)))

;;; path-get : Expr × Path → Expr
(define (path-get expr path)
  (if (null? path)
      expr
      (path-get (list-ref expr (car path)) (cdr path))))

;;; infer-hole-from-context : Expr × Nat × TEnv → Type | Error
(define (infer-hole-from-context parent position ctx)
  (cond
   ;; Application: (f ? arg...)
   [(and (pair? parent) (not (special-form? (car parent))))
    (let* ([fn-expr (car parent)]
           [fn-result (infer fn-expr ctx)])
          (if (not (eq? (car fn-result) 'ok))
              fn-result
              (let ([fn-type (cadr fn-result)])
                   (if (function-type? fn-type)
                       ;; Get the type of the parameter at this position
                       (let ([param-types (function-param-types fn-type)])
                            (if (< (- position 1) (length param-types))
                                (list-ref param-types (- position 1))
                                `(error position-out-of-range ,position)))
                       ;; Function type not known, return fresh var
                       (fresh-tvar)))))]
   
   ;; Let binding: (let ((x ?)) body)
   [(and (pair? parent) (eq? (car parent) 'let))
    ;; Hole is in the init, type is inferred from usage
    (fresh-tvar)]
   
   ;; If branches: (if test ? ?)
   [(and (pair? parent) (eq? (car parent) 'if))
    (cond
     [(= position 1) 'Bool]  ;; Test must be Bool
     [else (fresh-tvar)])]   ;; Branches can be anything
   
   ;; Default: unknown
   [else (fresh-tvar)]))

;;; ====
;;; Finding Available Fits
;;; ====

;;; find-fits : Type × TEnv → (List (Symbol × Type))
;;; Find bindings in scope that can fill a hole of the given type.
(define (find-fits target-type env)
  (let ([direct-fits (find-direct-fits target-type env)]
        [func-fits (if (function-type? target-type)
                       '()  ;; Lambda templates handled separately
                       (find-function-fits target-type env))])
       (append direct-fits func-fits)))

;;; find-direct-fits : Type × TEnv → (List (Symbol × Type))
;;; Find bindings whose type unifies with the target.
(define (find-direct-fits target env)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [type (cdr binding)]
                  [inst-type (instantiate type)]
                  [result (unify target inst-type)])
                 (if (eq? (car result) 'ok)
                     (cons name type)
                     #f)))
   env))

;;; find-function-fits : Type × TEnv → (List (Symbol × Type))
;;; Find functions that could produce the target type when applied.
(define (find-function-fits target env)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [type (cdr binding)]
                  [inst-type (instantiate type)])
                 (if (function-type? inst-type)
                     (let* ([ret-type (function-return-type inst-type)]
                            [result (unify target ret-type)])
                           (if (eq? (car result) 'ok)
                               (cons name type)
                               #f))
                     #f)))
   env))

;;; ====
;;; REPL Commands
;;; ====

;;; hole-type : Expr → Void
;;; Show what type each hole in the expression should have.
(define (hole-type expr)
  (reset-fresh!)
  (let ([holes (find-holes expr)])
       (display "\n")
       (if (null? holes)
           (display "  No holes found in expression.\n")
           (for-each
            (lambda (hole-info)
                    (let* ([path (car hole-info)]
                           [hole (cdr hole-info)]
                           [hole-n (hole-name hole)]
                           [type (infer-hole-type expr path)])
                          (if hole-n
                              (printf "  Hole ~a at ~a : ~a\n"
                                      hole-n path (type->string type))
                              (printf "  Hole at ~a : ~a\n"
                                      path (type->string type)))))
            holes))
       (display "\n")))

;;; hole-fits : Expr → Void
;;; Show available fits for each hole in the expression.
(define (hole-fits expr)
  (reset-fresh!)
  (let ([holes (find-holes expr)])
       (display "\n")
       (if (null? holes)
           (display "  No holes found in expression.\n")
           (for-each
            (lambda (hole-info)
                    (let* ([path (car hole-info)]
                           [hole (cdr hole-info)]
                           [hole-n (hole-name hole)]
                           [env (expr-context expr path)]
                           ;; Add standard bindings
                           [full-env (append standard-env env)]
                           [type (infer-hole-type expr path)]
                           [fits (if (symbol? type)
                                     (find-fits type full-env)
                                     '())])
                          (if hole-n
                              (printf "  Hole ~a : ~a\n" hole-n (type->string type))
                              (printf "  Hole : ~a\n" (type->string type)))
                          (if (null? fits)
                              (display "    No direct fits found.\n")
                              (for-each
                               (lambda (fit)
                                       (printf "    - ~a : ~a\n"
                                               (car fit) (type->string (cdr fit))))
                               (take-upto 10 fits)))
                          ;; Suggest lambda for function holes
                          (when (function-type? type)
                                (let ([params (function-param-types type)])
                                     (printf "    - (fn (~a) ...)\n"
                                             (join-strings " "
                                                           (map (lambda (i)
                                                                        (string-append "x"
                                                                                       (number->string i)))
                                                                (iota (length params)))))))))
            holes))
       (display "\n")))

;;; hole-refine : Expr × Type → Void
;;; Show what the expression looks like with holes narrowed to the given type.
(define (hole-refine expr expected-type)
  (reset-fresh!)
  (let ([result (check expr expected-type empty-tenv)])
       (display "\n")
       (if (eq? (car result) 'ok)
           (begin
            (display "  Expression type-checks against ")
            (display (type->string expected-type))
            (display "\n"))
           (begin
            (display "  Type error: ")
            (display (format-type-error result))
            (display "\n")))
       (display "\n")))

;;; case-split : Expr × Symbol → Void
;;; Generate case patterns for a sum type variable.
(define (case-split type var)
  (display "\n")
  (cond
   [(not (pair? type))
    (printf "  ~a is not a sum type.\n" type)]
   [(eq? (car type) '+)
    (display "  Case patterns:\n")
    (for-each
     (lambda (variant)
             (let ([tag (car variant)]
                   [fields (cdr variant)])
                  (if (null? fields)
                      (printf "    [~a ...]\n" tag)
                      (printf "    [(~a ~a) ...]\n"
                              tag
                              (join-strings " "
                                            (map (lambda (i)
                                                         (string-append
                                                          (string-downcase (symbol->string tag))
                                                          (number->string i)))
                                                 (iota (length fields))))))))
     (cdr type))]
   [(eq? (car type) 'List)
    (display "  Case patterns:\n")
    (display "    [() ...]\n")
    (display "    [(cons x xs) ...]\n")]
   [(eq? type 'Bool)
    (display "  Case patterns:\n")
    (display "    [#t ...]\n")
    (display "    [#f ...]\n")]
   [(eq? type 'Nat)
    (display "  Case patterns:\n")
    (display "    [0 ...]\n")
    (display "    [(succ n) ...]\n")]
   [else
    (printf "  Don't know how to split ~a\n" (type->string type))])
  (display "\n"))

;;; ====
;;; Standard Environment
;;; ====

;;; Common functions available in the standard environment
(define standard-env
  `((id . (∀ (a) (-> a a)))
    (const . (∀ (a b) (-> a b a)))
    (compose . (∀ (a b c) (-> (-> b c) (-> a b) (-> a c))))
    (flip . (∀ (a b c) (-> (-> a b c) (-> b a c))))
    (+ . (-> Int Int Int))
    (- . (-> Int Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (> . (-> Int Int Bool))
    (= . (-> Int Int Bool))
    (not . (-> Bool Bool))
    (and . (-> Bool Bool Bool))
    (or . (-> Bool Bool Bool))
    (car . (∀ (a) (-> (List a) a)))
    (cdr . (∀ (a) (-> (List a) (List a))))
    (cons . (∀ (a) (-> a (List a) (List a))))
    (null? . (∀ (a) (-> (List a) Bool)))
    (map . (∀ (a b) (-> (-> a b) (List a) (List b))))
    (filter . (∀ (a) (-> (-> a Bool) (List a) (List a))))
    (foldl . (∀ (a b) (-> (-> b a b) b (List a) b)))
    (foldr . (∀ (a b) (-> (-> a b b) b (List a) b)))
    (length . (∀ (a) (-> (List a) Int)))
    (append . (∀ (a) (-> (List a) (List a) (List a))))
    (reverse . (∀ (a) (-> (List a) (List a))))
    (take . (∀ (a) (-> Int (List a) (List a))))
    (drop . (∀ (a) (-> Int (List a) (List a))))
    (show . (∀ (a) (-> a String)))
    (square . (-> Int Int))
    (double . (-> Int Int))
    (even? . (-> Int Bool))
    (odd? . (-> Int Bool))
    (zero? . (-> Int Bool))
    (positive? . (-> Int Bool))
    (negative? . (-> Int Bool))
    (fst . (∀ (a b) (-> (× a b) a)))
    (snd . (∀ (a b) (-> (× a b) b)))))

;;; ====
;;; Suggestion Engine (fold-moe)
;;; ====

;;; Scoring weights (higher = better)
(define SCORE-LOCAL-EXACT 100)
(define SCORE-LOCAL-PARTIAL 80)
(define SCORE-STANDARD-EXACT 70)
(define SCORE-STANDARD-PARTIAL 60)
(define SCORE-INDEX-EXACT 50)
(define SCORE-INDEX-PARTIAL 40)
(define SCORE-SYNTHESIS 30)
(define SCORE-LAMBDA-TEMPLATE 20)

;;; make-suggestion : Symbol × Expr × Type × Symbol × Nat × String → Suggestion
(define (make-suggestion kind expr type source score explanation)
  `((kind . ,kind)
    (expr . ,expr)
    (type . ,type)
    (source . ,source)
    (score . ,score)
    (explanation . ,explanation)))

;;; Suggestion accessors
(define (suggestion-kind s) (cdr (assq 'kind s)))
(define (suggestion-expr s) (cdr (assq 'expr s)))
(define (suggestion-type s) (cdr (assq 'type s)))
(define (suggestion-source s) (cdr (assq 'source s)))
(define (suggestion-score s) (cdr (assq 'score s)))
(define (suggestion-explanation s) (cdr (assq 'explanation s)))

;;; ====
;;; Type Fit Checking
;;; ====

;;; check-fit : Type × Type → FitResult | #f
;;; Determine how a candidate type fits the target type.
;;; Returns:
;;;   (direct . subst)             - Exact match after instantiation
;;;   ((partial-app . n) . subst)  - Needs n more arguments
;;;   #f                           - Does not fit
(define (check-fit target candidate)
  (let ([inst-candidate (instantiate candidate)])
       (cond
        ;; Try direct unification first
        [(let ([result (unify target inst-candidate)])
              (and (eq? (car result) 'ok)
                   (cons 'direct (cadr result))))]
        ;; Try partial application (if candidate returns target)
        [(and (function-type? inst-candidate)
              (let* ([ret-type (function-return-type inst-candidate)]
                     [result (unify target ret-type)])
                    (and (eq? (car result) 'ok)
                         (let ([params (function-param-types inst-candidate)])
                              (cons (cons 'partial-app (length params))
                                    (cadr result))))))]
        [else #f])))

;;; ====
;;; Suggestion Finders
;;; ====

;;; find-local-suggestions : Type × TEnv → (List Suggestion)
(define (find-local-suggestions target-type ctx)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [binding-type (cdr binding)]
                  [fit-result (check-fit target-type binding-type)])
                 (and fit-result
                      (make-suggestion-from-fit 'local name binding-type fit-result target-type))))
   ctx))

;;; find-standard-suggestions : Type → (List Suggestion)
(define (find-standard-suggestions target-type)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [binding-type (cdr binding)]
                  [fit-result (check-fit target-type binding-type)])
                 (and fit-result
                      (make-suggestion-from-fit 'standard name binding-type fit-result target-type))))
   standard-env))

;;; make-suggestion-from-fit : Symbol × Symbol × Type × FitResult × Type → Suggestion
(define (make-suggestion-from-fit source name candidate-type fit-result target-type)
  (let* ([fit-kind (car fit-result)]
         [is-partial? (and (pair? fit-kind) (eq? (car fit-kind) 'partial-app))]
         [score (calculate-score source fit-kind)]
         [explanation (explain-fit name candidate-type target-type fit-result)])
        (if is-partial?
            ;; For partial application, suggest with holes for remaining args
            (let* ([n-args (cdr fit-kind)]
                   [arg-holes (make-list n-args '?)]
                   [expr (cons name arg-holes)])
                  (make-suggestion 'partial-app expr target-type source score explanation))
            ;; Direct fit
            (make-suggestion 'direct name candidate-type source score explanation))))

;;; calculate-score : Symbol × FitKind → Nat
(define (calculate-score source fit-kind)
  (let ([base (case source
                    [(local) SCORE-LOCAL-EXACT]
                    [(standard) SCORE-STANDARD-EXACT]
                    [(index) SCORE-INDEX-EXACT]
                    [(synthesis) SCORE-SYNTHESIS]
                    [(template) SCORE-LAMBDA-TEMPLATE]
                    [else 0])])
       (if (and (pair? fit-kind) (eq? (car fit-kind) 'partial-app))
           (- base (* 5 (cdr fit-kind)))  ; Penalty per missing arg
           base)))

;;; explain-fit : Symbol × Type × Type × FitResult → String
(define (explain-fit name candidate-type target-type fit-result)
  (let ([fit-kind (car fit-result)])
       (cond
        [(eq? fit-kind 'direct)
         (if (polymorphic-type? candidate-type)
             (format "~a : ~a (instantiates to match)"
                     name (type->string candidate-type))
             (format "~a : ~a (exact match)"
                     name (type->string candidate-type)))]
        [(and (pair? fit-kind) (eq? (car fit-kind) 'partial-app))
         (format "~a needs ~a arg~a to produce ~a"
                 name (cdr fit-kind)
                 (if (= (cdr fit-kind) 1) "" "s")
                 (type->string target-type))]
        [else (format "~a : ~a" name (type->string candidate-type))])))

;;; polymorphic-type? : Type → Boolean
(define (polymorphic-type? t)
  (and (pair? t) (eq? (car t) '∀)))

;;; ====
;;; Synthesis Recipes
;;; ====

;;; find-synthesis-suggestions : Type × TEnv → (List Suggestion)
(define (find-synthesis-suggestions target-type ctx)
  (append
   (suggest-map-patterns target-type ctx)
   (suggest-filter-patterns target-type ctx)
   (suggest-lambda-templates target-type)))

;;; suggest-map-patterns : Type × TEnv → (List Suggestion)
;;; Suggest (map ? xs) when target is (List b) and xs : (List a) in scope.
(define (suggest-map-patterns target-type ctx)
  (if (and (pair? target-type) (eq? (car target-type) 'List))
      (let ([target-elem (cadr target-type)])
           (filter-map
            (lambda (binding)
                    (let ([name (car binding)]
                          [type (cdr binding)])
                         (let ([inst-type (instantiate type)])
                              (and (pair? inst-type)
                                   (eq? (car inst-type) 'List)
                                   (let ([src-elem (cadr inst-type)])
                                        (make-suggestion
                                         'synthesis
                                         `(map ? ,name)
                                         target-type
                                         'synthesis
                                         SCORE-SYNTHESIS
                                         (format "map over ~a, need (-> ~a ~a)"
                                                 name
                                                 (type->string src-elem)
                                                 (type->string target-elem))))))))
            ctx))
      '()))

;;; suggest-filter-patterns : Type × TEnv → (List Suggestion)
;;; Suggest (filter ? xs) when target is (List a) and xs : (List a) in scope.
(define (suggest-filter-patterns target-type ctx)
  (if (and (pair? target-type) (eq? (car target-type) 'List))
      (let ([target-elem (cadr target-type)])
           (filter-map
            (lambda (binding)
                    (let ([name (car binding)]
                          [type (cdr binding)])
                         (let ([inst-type (instantiate type)])
                              (and (pair? inst-type)
                                   (eq? (car inst-type) 'List)
                                   (let* ([src-elem (cadr inst-type)]
                                          [result (unify target-elem src-elem)])
                                         (and (eq? (car result) 'ok)
                                              (make-suggestion
                                               'synthesis
                                               `(filter ? ,name)
                                               target-type
                                               'synthesis
                                               (- SCORE-SYNTHESIS 5)
                                               (format "filter ~a, need (-> ~a Bool)"
                                                       name
                                                       (type->string src-elem)))))))))
            ctx))
      '()))

;;; suggest-lambda-templates : Type → (List Suggestion)
;;; Suggest lambda templates when target is a function type.
(define (suggest-lambda-templates target-type)
  (if (function-type? target-type)
      (let* ([params (function-param-types target-type)]
             [ret-type (function-return-type target-type)]
             [param-names (generate-param-names (length params))])
            (list
             (make-suggestion
              'lambda-template
              `(fn ,param-names ?)
              target-type
              'template
              SCORE-LAMBDA-TEMPLATE
              (format "lambda: ~a -> ~a"
                      (join-strings ", " (map type->string params))
                      (type->string ret-type)))))
      '()))

;;; generate-param-names : Nat → (List Symbol)
(define (generate-param-names n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (string->symbol (string-append "x" (number->string i))) acc)))))

;;; make-list : Nat × a → (List a)
(define (make-list n val)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           acc
           (loop (+ i 1) (cons val acc)))))

;;; ====
;;; Main Suggestion Engine
;;; ====

;;; suggest-for-hole : Expr → (List HoleSuggestions)
(define (suggest-for-hole expr)
  (reset-fresh!)
  (let ([holes (find-holes expr)])
       (map (lambda (hole-info)
                    (let* ([path (car hole-info)]
                           [hole (cdr hole-info)]
                           [ctx (expr-context expr path)]
                           [target-type (infer-hole-type expr path)])
                          (make-hole-suggestions hole target-type ctx)))
            holes)))

;;; make-hole-suggestions : Hole × Type × TEnv → HoleSuggestions
(define (make-hole-suggestions hole target-type ctx)
  (let* ([local-suggestions (find-local-suggestions target-type ctx)]
         [standard-suggestions (find-standard-suggestions target-type)]
         [synthesis-suggestions (find-synthesis-suggestions target-type ctx)]
         [all-suggestions (append local-suggestions
                                  standard-suggestions
                                  synthesis-suggestions)])
        `((hole . ,hole)
          (type . ,target-type)
          (suggestions . ,(sort-suggestions all-suggestions)))))

;;; sort-suggestions : (List Suggestion) → (List Suggestion)
(define (sort-suggestions suggestions)
  (list-sort
   (lambda (a b)
           (> (suggestion-score a) (suggestion-score b)))
   suggestions))

;;; ====
;;; REPL Command: hole-suggest
;;; ====

;;; hole-suggest : Expr → Void
(define (hole-suggest expr)
  (display "\n")
  (let ([results (suggest-for-hole expr)])
       (if (null? results)
           (display "  No holes found in expression.\n")
           (for-each display-hole-suggestions results)))
  (display "\n"))

;;; display-hole-suggestions : HoleSuggestions → Void
(define (display-hole-suggestions hs)
  (let ([hole (cdr (assq 'hole hs))]
        [type (cdr (assq 'type hs))]
        [suggestions (cdr (assq 'suggestions hs))])
       (printf "  Hole ~a : ~a\n"
               (or (hole-name hole) "?")
               (type->string type))
       (display "  ────────────────────────────────────────────\n")
       (if (null? suggestions)
           (display "    No suggestions found.\n")
           (for-each display-suggestion (take-upto 10 suggestions)))
       (display "\n")))

;;; display-suggestion : Suggestion → Void
(define (display-suggestion sug)
  (let ([score (suggestion-score sug)]
        [expr (suggestion-expr sug)]
        [explanation (suggestion-explanation sug)])
       (printf "    [~a] ~s\n" score expr)
       (printf "         ~a\n" explanation)))

;;; ====
;;; Helper Functions
;;; ====

;;; take-upto : Nat × (List a) → (List a)
(define (take-upto n lst)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-upto (- n 1) (cdr lst)))))

;;; take-n : (List a) × Nat → (List a)
;;; Take first n elements from list.
(define (take-n lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;;; iota : Nat → (List Nat)
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; join-strings : String × (List String) → String
(define (join-strings sep strs)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; filter-map : (a → b | #f) × (List a) → (List b)
(define (filter-map f lst)
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

;;; ====
;;; Usage Help
;;; ====

(define (hole-help)
  (display "\n")
  (display "  Type-Driven Development with Holes\n")
  (display "  ────────────────────────────────────────────\n")
  (display "  (hole-type expr)        - Show types for holes in expr\n")
  (display "  (hole-fits expr)        - Show available fits for holes\n")
  (display "  (hole-suggest expr)     - Ranked suggestions with synthesis\n")
  (display "  (hole-refine expr type) - Check expr against expected type\n")
  (display "  (case-split type var)   - Generate case patterns for type\n")
  (display "\n")
  (display "  Hole syntax:\n")
  (display "    ?           - Anonymous hole\n")
  (display "    (? name)    - Named hole\n")
  (display "\n")
  (display "  Example:\n")
  (display "    (hole-suggest '(let ((xs '(1 2 3))) (map ? xs)))\n")
  (display "    ;; Shows ranked suggestions: direct fits, partial apps, synthesis\n")
  (display "\n"))

(display "Typed holes loaded. Use (hole-help) for usage.\n")
