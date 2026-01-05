;;; core/lang/eval.ss — The Evaluator with Fuel
;;; @module eval
;;; @requires prelude block prim reverse-diff
;;;
;;; The lens that lets the telescope see.
;;;
;;; Every evaluation takes fuel. When fuel exhausts, we return
;;; a suspension — the remaining expression and zero fuel.
;;; This makes Core total: evaluation always terminates.
;;;
;;; Forms:
;;;   (quote datum)           → datum
;;;   (fn (x ...) body)       → closure
;;;   (call f args...)        → apply f to args
;;;   (let ((x e) ...) body)  → bind and evaluate
;;;   (fix name (fn ...))     → recursive binding
;;;   (case e ((tag vars) body) ...) → pattern match on Block tag
;;;   (prim 'op args...)      → pure primitive
;;;   (if test then else)     → conditional
;;;   x                       → variable lookup
;;;
;;; Values:
;;;   numbers, strings, symbols, booleans, bytevectors
;;;   closures: (closure (params...) body env)
;;;   blocks: as produced by make-block
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - block.ss
;;;   - prim.ss
;;;   - reverse-diff.ss (for traced evaluation)

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Fuel
;;; ============================================================

;;; Fuel is a natural number. Each eval call costs 1 fuel.
;;; Primitives consume 0 fuel. Suspension happens only at eval boundaries.

;;; fuel? : α → Bool
(define (fuel? n) (and (integer? n) (>= n 0)))

;;; out-of-fuel? : Nat → Bool
(define (out-of-fuel? n) (zero? n))

;;; ============================================================
;;; Values
;;; ============================================================

;;; A value is something that doesn't reduce further.

;;; value? : α → Bool
(define (value? v)
  (or (number? v)
      (string? v)
      (boolean? v)
      (null? v)
      (bytevector? v)
      (closure? v)
      (block? v)))

;;; closure? : α → Bool
(define (closure? v)
  (and (pair? v) (eq? (car v) 'closure)))

;;; closure-params : Closure → (List Symbol)
(define (closure-params c) (cadr c))
;;; closure-body : Closure → Expr
(define (closure-body c) (caddr c))
;;; closure-env : Closure → Env
(define (closure-env c) (cadddr c))

;;; make-closure : (List Symbol) × Expr × Env → Closure
(define (make-closure params body env)
  `(closure ,params ,body ,env))

;;; ============================================================
;;; Environments
;;; ============================================================

;;; An environment is an alist mapping symbols to values.

;;; empty-env : Env
(define empty-env '())

;;; env-lookup : Env × Symbol → (Result Value Error)
(define (env-lookup env name)
  (let ([entry (assq name env)])
       (if entry
           `(ok ,(cdr entry))
           `(error unbound-variable ,name))))

;;; env-extend : Env × Symbol × Value → Env
(define (env-extend env name value)
  (cons (cons name value) env))

;;; env-extend* : Env × (List Symbol) × (List Value) → Env
(define (env-extend* env names values)
  (if (null? names)
      env
      (env-extend* (env-extend env (car names) (car values))
                   (cdr names)
                   (cdr values))))

;;; env-extend-alist : Env × (List (Pair Symbol Value)) → Env
(define (env-extend-alist env alist)
  (append alist env))

;;; ============================================================
;;; The Evaluator
;;; ============================================================

;;; eval-expr : Expr × Env × Fuel → (ok Value Fuel) | (suspended Expr Env) | (error ...)
;;;
;;; Returns:
;;;   (ok value remaining-fuel) — evaluation complete
;;;   (suspended expr env)      — ran out of fuel
;;;   (error tag info)          — evaluation error

(define (eval-expr expr env fuel)
  (cond
   ;; Out of fuel — suspend
   [(out-of-fuel? fuel)
    `(suspended ,expr ,env)]
   
   [else
    ;; Consume fuel for this eval call.
    (let ([remaining (- fuel 1)])
         (cond
          ;; Already a value
          [(value? expr)
           `(ok ,expr ,remaining)]
          
          ;; Variable reference
          [(symbol? expr)
           (let ([result (env-lookup env expr)])
                (if (eq? (car result) 'ok)
                    `(ok ,(cadr result) ,remaining)
                    result))]
          
          ;; Must be a compound form
          [(not (pair? expr))
           `(error invalid-expression ,expr)]
          
          [else
           (let ([head (car expr)])
                (cond
                 ;; Quote — return datum as-is
                 [(eq? head 'quote)
                  `(ok ,(cadr expr) ,remaining)]
                 
                 ;; Lambda — create closure
                 [(eq? head 'fn)
                  (let ([params (cadr expr)]
                        [body (caddr expr)])
                       `(ok ,(make-closure params body env) ,remaining))]
                 
                 ;; Let — evaluate bindings, extend env, evaluate body
                 [(eq? head 'let)
                  (eval-let (cadr expr) (caddr expr) env remaining)]
                 
                 ;; Fix — recursive binding
                 [(eq? head 'fix)
                  (eval-fix (cadr expr) (caddr expr) env remaining)]
                 
                 ;; If — conditional
                 [(eq? head 'if)
                  (eval-if (cadr expr) (caddr expr) (cadddr expr) env remaining)]
                 
                 ;; Case — pattern match on block tag
                 [(eq? head 'case)
                  (eval-case (cadr expr) (cddr expr) env remaining)]
                 
                 ;; Prim — pure primitive
                 [(eq? head 'prim)
                  (eval-prim (cadr expr) (cddr expr) env remaining)]
                 
                 ;; Par — parallel evaluation hint
                 [(eq? head 'par)
                  (eval-par (cadr expr) (caddr expr) env remaining)]
                 
                 ;; Pseq — sequential evaluation
                 [(eq? head 'pseq)
                  (eval-pseq (cadr expr) (caddr expr) env remaining)]
                 
                 ;; Call — explicit application
                 [(eq? head 'call)
                  (eval-call (cadr expr) (cddr expr) env remaining)]
                 
                 ;; Implicit application — (f args...)
                 [else
                  (eval-call (car expr) (cdr expr) env remaining)]))]))]))

;;; ============================================================
;;; Par/Pseq Evaluation
;;; ============================================================
;;;
;;; NOTE ON CURRENT IMPLEMENTATION:
;;; eval-par and eval-pseq currently have identical implementations —
;;; both evaluate a then b sequentially. This is intentional.
;;;
;;; These forms are SEMANTIC HINTS for future parallel execution:
;;;   - (par a b)  = "a and b can run in parallel; return b"
;;;   - (pseq a b) = "force a to complete before starting b; return b"
;;;
;;; In the current sequential evaluator, both behave identically.
;;; In a future parallel runtime:
;;;   - par would enable speculative/concurrent evaluation of a
;;;   - pseq would enforce strict ordering (useful when a has effects
;;;     that b depends on, or for controlling evaluation order)
;;;
;;; The distinction matters for:
;;;   1. Documenting programmer intent about parallelizability
;;;   2. Future optimization passes that could parallelize par
;;;   3. Reasoning about evaluation order in a parallel context
;;;
;;; ============================================================

;;; eval-par : Expr × Expr × Env × Fuel → (Result Value Error)
;;; Parallel evaluation hint: (par a b)
;;; Evaluate both a and b, return b.
;;; Currently evaluates sequentially, but provides a hint for
;;; future parallel execution strategies.
(define (eval-par a-expr b-expr env fuel)
  ;; Evaluate a (for side-effect of forcing computation)
  (let ([a-result (eval-expr a-expr env fuel)])
       (case (car a-result)
             [(ok)
              (let ([a-value (cadr a-result)]
                    [fuel-after-a (caddr a-result)])
                   ;; Now evaluate b with remaining fuel
                   (eval-expr b-expr env fuel-after-a))]
             [(suspended) a-result]  ; Forward suspension
             [(error) a-result])))   ; Forward error

;;; eval-pseq : Expr × Expr × Env × Fuel → (Result Value Error)
;;; Sequential evaluation: (pseq a b)
;;; Force evaluation of a, then evaluate b, return b.
;;; Ensures a is strictly evaluated before b.
(define (eval-pseq a-expr b-expr env fuel)
  ;; Evaluate a first (strict evaluation)
  (let ([a-result (eval-expr a-expr env fuel)])
       (case (car a-result)
             [(ok)
              (let ([a-value (cadr a-result)]
                    [fuel-after-a (caddr a-result)])
                   ;; a is fully evaluated, now evaluate b
                   (eval-expr b-expr env fuel-after-a))]
             [(suspended) a-result]  ; Forward suspension
             [(error) a-result])))   ; Forward error

;;; ============================================================
;;; Let Evaluation
;;; ============================================================

(define (eval-let bindings body env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (let ,bindings ,body) ,env)
      (eval-let-bindings bindings body env '() fuel)))

(define (eval-let-bindings bindings body env acc fuel)
  (if (null? bindings)
      ;; All bindings evaluated — evaluate body in extended env
      (eval-expr body (env-extend-alist env (reverse acc)) fuel)
      ;; Evaluate next binding
      (let* ([binding (car bindings)]
             [name (car binding)]
             [expr (cadr binding)]
             [result (eval-expr expr env fuel)])
            (cond
             [(eq? (car result) 'ok)
              (eval-let-bindings
               (cdr bindings) body env
               (cons (cons name (cadr result)) acc)
               (caddr result))]
             [(eq? (car result) 'suspended)
              `(suspended (let ,(cons binding (cdr bindings)) ,body) ,env)]
             [else result]))))

;;; ============================================================
;;; Fix Evaluation (Recursion)
;;; ============================================================
;;;
;;; IMPORTANT: MUTATION EXCEPTION
;;; ============================
;;; This is the ONLY use of mutation (set-cdr!) in all of core/.
;;;
;;; Rationale: Recursive closures require a cyclic structure where
;;; the closure's environment contains a reference to the closure itself.
;;; In a purely functional setting, this requires either:
;;;   1. Lazy evaluation (not our strategy - we use call-by-value)
;;;   2. Explicit fixed-point combinators (Y combinator - less efficient)
;;;   3. A single, localized mutation to tie the knot
;;;
;;; We chose option 3 because:
;;;   - It's a single, well-understood pattern
;;;   - The mutation is localized and not observable from outside
;;;   - It matches how most Scheme implementations handle letrec
;;;   - Alternatives are significantly more complex or less efficient
;;;
;;; The mutation is semantically pure: the cyclic structure IS the
;;; fixed point; we're just constructing it directly rather than
;;; computing it through repeated application.
;;;
;;; See forum/engineering/0011-adr-002-eval-mutation-exception.sexp
;;; ============================================================

(define (eval-fix name fn-expr env fuel)
  ;; fn-expr should be (fn (params...) body)
  (if (and (pair? fn-expr) (eq? (car fn-expr) 'fn))
      (let* ([params (cadr fn-expr)]
             [body (caddr fn-expr)]
             ;; Create a recursive closure by including itself in its env
             [rec-env (env-extend env name 'placeholder)]
             [closure (make-closure params body rec-env)])
            ;; MUTATION: Tie the knot - see rationale above
            (set-cdr! (car rec-env) closure)
            `(ok ,closure ,fuel))
      `(error fix-requires-fn ,fn-expr)))

;;; ============================================================
;;; If Evaluation
;;; ============================================================

(define (eval-if test-expr then-expr else-expr env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (if ,test-expr ,then-expr ,else-expr) ,env)
      (let ([test-result (eval-expr test-expr env fuel)])
           (cond
            [(eq? (car test-result) 'ok)
             (let ([test-val (cadr test-result)]
                   [remaining (caddr test-result)])
                  (if test-val
                      (eval-expr then-expr env remaining)
                      (eval-expr else-expr env remaining)))]
            [(eq? (car test-result) 'suspended)
             `(suspended (if ,(cadr test-result) ,then-expr ,else-expr)
               ,(caddr test-result))]
            [else test-result]))))

;;; ============================================================
;;; Case Evaluation (Pattern Matching)
;;; ============================================================

;;; (case expr
;;;   ((Tag1 x y) body1)
;;;   ((Tag2 z) body2)
;;;   ...)
;;;
;;; Matches on block tag, binds payload/refs to variables.

(define (eval-case scrutinee clauses env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (case ,scrutinee ,@clauses) ,env)
      (let ([scrut-result (eval-expr scrutinee env fuel)])
           (cond
            [(eq? (car scrut-result) 'ok)
             (let ([val (cadr scrut-result)]
                   [remaining (caddr scrut-result)])
                  (if (block? val)
                      (match-clauses val clauses env remaining)
                      `(error case-requires-block ,val)))]
            [(eq? (car scrut-result) 'suspended)
             `(suspended (case ,(cadr scrut-result) ,@clauses)
               ,(caddr scrut-result))]
            [else scrut-result]))))

(define (match-clauses block clauses env fuel)
  (if (null? clauses)
      `(error no-matching-clause ,(block-tag block))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             [tag (car pattern)]
             [vars (cdr pattern)])
            (if (eq? tag (block-tag block))
                ;; Match! Bind refs to vars
                (let ([refs (block-refs-list block)])
                     (if (= (length vars) (length refs))
                         (eval-expr body (env-extend* env vars refs) fuel)
                         `(error pattern-arity-mismatch ,tag)))
                ;; No match, try next clause
                (match-clauses block (cdr clauses) env fuel)))))

;;; Helper: get refs as a list
(define (block-refs-list blk)
  (let ([refs (block-refs blk)])
       (let loop ([i 0] [acc '()])
            (if (>= i (vector-length refs))
                (reverse acc)
                (loop (+ i 1) (cons (vector-ref refs i) acc))))))

;;; ============================================================
;;; Primitive Evaluation
;;; ============================================================

;;; Primitives consume no fuel themselves; only argument evaluation costs fuel.

(define (eval-prim op args env fuel)
  (if (and (out-of-fuel? fuel) (pair? args))
      `(suspended (prim ,op ,@args) ,env)
      (eval-prim-args op args env '() fuel)))

(define (eval-prim-args op remaining env acc fuel)
  (if (null? remaining)
      ;; All args evaluated — call the primitive
      (let* ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                         (cadr op)
                         op)]
             [arg-vals (reverse acc)])
            `(ok ,(apply prim (cons op-sym arg-vals)) ,fuel))
      ;; Evaluate next arg
      (let ([result (eval-expr (car remaining) env fuel)])
           (cond
            [(eq? (car result) 'ok)
             (eval-prim-args op (cdr remaining) env
                             (cons (cadr result) acc)
                             (caddr result))]
            [(eq? (car result) 'suspended)
             `(suspended (prim ,op ,@(reverse acc) ,(cadr result) ,@(cdr remaining)) ,env)]
            [else result]))))

;;; ============================================================
;;; Call Evaluation (Application)
;;; ============================================================

(define (eval-call fn-expr arg-exprs env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (call ,fn-expr ,@arg-exprs) ,env)
      ;; Evaluate the function
      (let ([fn-result (eval-expr fn-expr env fuel)])
           (cond
            [(eq? (car fn-result) 'ok)
             (let ([fn-val (cadr fn-result)]
                   [remaining (caddr fn-result)])
                  (if (closure? fn-val)
                      (eval-call-args fn-val arg-exprs env '() remaining)
                      `(error not-a-function ,fn-val)))]
            [(eq? (car fn-result) 'suspended)
             `(suspended (call ,(cadr fn-result) ,@arg-exprs) ,(caddr fn-result))]
            [else fn-result]))))

(define (eval-call-args closure remaining env acc fuel)
  (if (null? remaining)
      ;; All args evaluated — apply the closure
      (let* ([params (closure-params closure)]
             [body (closure-body closure)]
             [closure-env (closure-env closure)]
             [arg-vals (reverse acc)])
            (if (= (length params) (length arg-vals))
                (eval-expr body (env-extend* closure-env params arg-vals) fuel)
                `(error arity-mismatch (expected ,(length params)) (got ,(length arg-vals)))))
      ;; Evaluate next arg
      (let ([result (eval-expr (car remaining) env fuel)])
           (cond
            [(eq? (car result) 'ok)
             (eval-call-args closure (cdr remaining) env
                             (cons (cadr result) acc)
                             (caddr result))]
            [(eq? (car result) 'suspended)
             `(suspended (call ,closure ,@(reverse acc) ,(cadr result) ,@(cdr remaining)) ,env)]
            [else result]))))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; run : Expr × Fuel → (ok Value) | (suspended Expr) | (error ...)
;;; Evaluate an expression with empty environment.
(define (run expr fuel)
  (let ([result (eval-expr expr empty-env fuel)])
       (cond
        [(eq? (car result) 'ok)
         `(ok ,(cadr result))]
        [(eq? (car result) 'suspended)
         `(suspended ,(cadr result))]
        [else result])))

;;; run-to-completion : Expr × MaxFuel → (ok Value) | (error ...)
;;; Keep running until completion or error (not suspension).
;;; Uses max-fuel for each resumption attempt, with a retry limit.
(define (run-to-completion expr max-fuel)
  (let loop ([expr expr] [retries 100])  ; Limit retries to prevent infinite loops
       (if (zero? retries)
           `(error retry-limit-exceeded ,expr)
           (let ([result (eval-expr expr empty-env max-fuel)])
                (cond
                 [(eq? (car result) 'ok)
                  `(ok ,(cadr result))]
                 [(eq? (car result) 'suspended)
                  ;; Give fresh fuel budget for resumption
                  (loop (cadr result) (- retries 1))]
                 [else result])))))

;;; eval-with-env : Expr × Env × Fuel → (Result Value Error)
;;; Evaluate with a given environment.
(define (eval-with-env expr env fuel)
  (eval-expr expr env fuel))

;;; ============================================================
;;; Standard Prelude
;;; ============================================================

;;; Some useful functions defined in The Fold's own language.
;;; These are defined as expressions to be evaluated, ensuring
;;; proper closure semantics.

(define prelude-defs
  '(;; Basic combinators
    (id      . (fn (x) x))
    (const   . (fn (x) (fn (y) x)))
    (compose . (fn (f) (fn (g) (fn (x) (f (g x))))))
    (flip    . (fn (f) (fn (x) (fn (y) ((f y) x)))))
    (on      . (fn (f) (fn (g) (fn (x) (fn (y) ((f (g x)) (g y)))))))
    
    ;; Pair operations (pairs as 2-element lists)
    (fst     . (fn (p) (prim 'car p)))
    (snd     . (fn (p) (prim 'car (prim 'cdr p))))
    (pair    . (fn (a) (fn (b) (prim 'list a b))))
    
    ;; Boolean combinators
    (bool-not . (fn (b) (if b #f #t)))
    (bool-and . (fn (a) (fn (b) (if a b #f))))
    (bool-or  . (fn (a) (fn (b) (if a #t b))))
    
    ;; List operations (defined with fix for recursion)
    (map     . (fix map (fn (f xs)
                            (if (prim 'null? xs)
                                '()
                                (prim 'cons (f (prim 'car xs))
                                      (map f (prim 'cdr xs)))))))
    
    (filter  . (fix filter (fn (p xs)
                               (if (prim 'null? xs)
                                   '()
                                   (let ((x (prim 'car xs))
                                         (rest (filter p (prim 'cdr xs))))
                                        (if (p x)
                                            (prim 'cons x rest)
                                            rest))))))
    
    (foldl   . (fix foldl (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (foldl f (f acc (prim 'car xs)) (prim 'cdr xs))))))
    
    (foldr   . (fix foldr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (f (prim 'car xs) (foldr f acc (prim 'cdr xs)))))))
    
    (scanl   . (fix scanl (fn (f acc xs)
                              (prim 'cons acc
                                    (if (prim 'null? xs)
                                        '()
                                        (let ((new-acc (f acc (prim 'car xs))))
                                             (scanl f new-acc (prim 'cdr xs))))))))
    
    (scanr   . (fix scanr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  (prim 'list acc)
                                  (let ((rest (scanr f acc (prim 'cdr xs))))
                                       (prim 'cons (f (prim 'car xs) (prim 'car rest))
                                             rest))))))
    
    (take    . (fix take (fn (n xs)
                             (if (prim 'zero? n)
                                 '()
                                 (if (prim 'null? xs)
                                     '()
                                     (prim 'cons (prim 'car xs)
                                           (take (prim 'sub n 1) (prim 'cdr xs))))))))
    
    (drop    . (fix drop (fn (n xs)
                             (if (prim 'zero? n)
                                 xs
                                 (if (prim 'null? xs)
                                     '()
                                     (drop (prim 'sub n 1) (prim 'cdr xs)))))))
    
    (zip     . (fix zip (fn (xs ys)
                            (if (prim 'null? xs)
                                '()
                                (if (prim 'null? ys)
                                    '()
                                    (prim 'cons (prim 'list (prim 'car xs) (prim 'car ys))
                                          (zip (prim 'cdr xs) (prim 'cdr ys))))))))
    
    (range   . (fix range (fn (start end)
                              (if (prim 'ge? start end)
                                  '()
                                  (prim 'cons start (range (prim 'add start 1) end))))))
    
    (sum     . (fix sum (fn (xs)
                            (if (prim 'null? xs)
                                0
                                (prim 'add (prim 'car xs) (sum (prim 'cdr xs)))))))
    
    (product . (fix product (fn (xs)
                                (if (prim 'null? xs)
                                    1
                                    (prim 'mul (prim 'car xs) (product (prim 'cdr xs)))))))
    
    ;; Advanced list operations
    (flatten . (fix flatten (fn (xss)
                                (if (prim 'null? xss)
                                    '()
                                    (prim 'append (prim 'car xss)
                                          (flatten (prim 'cdr xss)))))))
    
    (flatMap . (fn (f)
                   (fix flatMap-rec (fn (xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (prim 'append (f (prim 'car xs))
                                                  (flatMap-rec (prim 'cdr xs))))))))
    
    (any     . (fix any (fn (p xs)
                            (if (prim 'null? xs)
                                #f
                                (if (p (prim 'car xs))
                                    #t
                                    (any p (prim 'cdr xs)))))))
    
    (all     . (fix all (fn (p xs)
                            (if (prim 'null? xs)
                                #t
                                (if (p (prim 'car xs))
                                    (all p (prim 'cdr xs))
                                    #f)))))
    
    (elem    . (fix elem (fn (x xs)
                             (if (prim 'null? xs)
                                 #f
                                 (if (prim 'eq? x (prim 'car xs))
                                     #t
                                     (elem x (prim 'cdr xs)))))))
    
    (replicate . (fix replicate (fn (n x)
                                    (if (prim 'zero? n)
                                        '()
                                        (prim 'cons x (replicate (prim 'sub n 1) x))))))
    
    (takeWhile . (fix takeWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (prim 'cons (prim 'car xs)
                                                  (takeWhile p (prim 'cdr xs)))
                                            '())))))
    
    (dropWhile . (fix dropWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (dropWhile p (prim 'cdr xs))
                                            xs)))))
    
    (span    . (fix span (fn (p xs)
                             (if (prim 'null? xs)
                                 (prim 'list '() '())
                                 (if (p (prim 'car xs))
                                     (let ((rest (span p (prim 'cdr xs))))
                                          (prim 'list
                                                (prim 'cons (prim 'car xs) (prim 'car rest))
                                                (prim 'car (prim 'cdr rest))))
                                     (prim 'list '() xs))))))
    
    (break   . (fn (p)
                   (fix break-rec (fn (xs)
                                      (if (prim 'null? xs)
                                          (prim 'list '() '())
                                          (if (p (prim 'car xs))
                                              (prim 'list '() xs)
                                              (let ((rest (break-rec (prim 'cdr xs))))
                                                   (prim 'list
                                                         (prim 'cons (prim 'car xs) (prim 'car rest))
                                                         (prim 'car (prim 'cdr rest))))))))))
    
    (partition . (fix partition (fn (p xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((x (prim 'car xs))
                                              (rest (partition p (prim 'cdr xs))))
                                             (if (p x)
                                                 (prim 'list
                                                       (prim 'cons x (prim 'car rest))
                                                       (prim 'car (prim 'cdr rest)))
                                                 (prim 'list
                                                       (prim 'car rest)
                                                       (prim 'cons x (prim 'car (prim 'cdr rest))))))))))
    
    (zipWith . (fix zipWith (fn (f xs ys)
                                (if (prim 'null? xs)
                                    '()
                                    (if (prim 'null? ys)
                                        '()
                                        (prim 'cons (f (prim 'car xs) (prim 'car ys))
                                              (zipWith f (prim 'cdr xs) (prim 'cdr ys))))))))
    
    (unzip   . (fix unzip (fn (pairs)
                              (if (prim 'null? pairs)
                                  (prim 'list '() '())
                                  (let ((p (prim 'car pairs))
                                        (rest (unzip (prim 'cdr pairs))))
                                       (prim 'list
                                             (prim 'cons (prim 'car p)
                                                   (prim 'car rest))
                                             (prim 'cons (prim 'car (prim 'cdr p))
                                                   (prim 'car (prim 'cdr rest)))))))))
    
    (intersperse . (fix intersperse (fn (sep xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (if (prim 'null? (prim 'cdr xs))
                                                xs
                                                (prim 'cons (prim 'car xs)
                                                      (prim 'cons sep
                                                            (intersperse sep (prim 'cdr xs)))))))))
    
    (group   . (fix group (fn (xs)
                              (if (prim 'null? xs)
                                  '()
                                  (let ((x (prim 'car xs)))
                                       (let ((rest-result (span (fn (y) (prim 'eq? x y)) (prim 'cdr xs))))
                                            (prim 'cons
                                                  (prim 'cons x (prim 'car rest-result))
                                                  (group (prim 'car (prim 'cdr rest-result))))))))))
    
    (nub     . (fix nub (fn (xs)
                            (if (prim 'null? xs)
                                '()
                                (let ((x (prim 'car xs)))
                                     (prim 'cons x (nub (filter (fn (y) (prim 'not (prim 'eq? x y))) (prim 'cdr xs)))))))))
    
    (find    . (fix find (fn (p xs)
                             (if (prim 'null? xs)
                                 'none
                                 (if (p (prim 'car xs))
                                     (prim 'cons 'some (prim 'car xs))
                                     (find p (prim 'cdr xs)))))))
    
    (splitAt . (fix splitAt (fn (n xs)
                                (if (prim 'zero? n)
                                    (prim 'list '() xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((rest (splitAt (prim 'sub n 1) (prim 'cdr xs))))
                                             (prim 'list
                                                   (prim 'cons (prim 'car xs) (prim 'car rest))
                                                   (prim 'car (prim 'cdr rest)))))))))
    
    ;; ================================================================
    ;; Type Class Method Implementations
    ;; ================================================================
    ;; These implement the methods declared in core/resolve.ss
    ;; Names match those used in instance definitions.
    
    ;; --- Functor ---
    ;; list-fmap is 'map' (already defined above)
    (list-fmap . (fix list-fmap (fn (f xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'cons (f (prim 'car xs))
                                              (list-fmap f (prim 'cdr xs)))))))
    
    ;; option-fmap: Option is (+ (None) (Some a))
    ;; Represented as 'none or ('some . value)
    (option-fmap . (fn (f opt)
                       (if (prim 'eq? opt 'none)
                           'none
                           (prim 'cons 'some (f (prim 'cdr opt))))))
    
    ;; either-fmap: Either is (+ (Left e) (Right a))
    ;; Represented as ('left . e) or ('right . a)
    (either-fmap . (fn (f e)
                       (if (prim 'eq? (prim 'car e) 'left)
                           e
                           (prim 'cons 'right (f (prim 'cdr e))))))
    
    ;; --- Applicative ---
    (list-pure . (fn (x) (prim 'list x)))
    
    ;; list-ap: Apply list of functions to list of values
    (list-ap . (fix list-ap (fn (fs xs)
                                (if (prim 'null? fs)
                                    '()
                                    (prim 'append
                                          (map (prim 'car fs) xs)
                                          (list-ap (prim 'cdr fs) xs))))))
    
    (option-pure . (fn (x) (prim 'cons 'some x)))
    
    (option-ap . (fn (mf mx)
                     (if (prim 'eq? mf 'none)
                         'none
                         (if (prim 'eq? mx 'none)
                             'none
                             (prim 'cons 'some
                                   ((prim 'cdr mf) (prim 'cdr mx)))))))
    
    ;; --- Monad ---
    (list-bind . (fix list-bind (fn (xs f)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'append (f (prim 'car xs))
                                              (list-bind (prim 'cdr xs) f))))))
    
    (list-return . (fn (x) (prim 'list x)))
    
    (option-bind . (fn (mx f)
                       (if (prim 'eq? mx 'none)
                           'none
                           (f (prim 'cdr mx)))))
    
    (option-return . (fn (x) (prim 'cons 'some x)))
    
    (either-bind . (fn (mx f)
                       (if (prim 'eq? (prim 'car mx) 'left)
                           mx
                           (f (prim 'cdr mx)))))
    
    (either-return . (fn (x) (prim 'cons 'right x)))
    
    ;; --- Eq ---
    (nat-eq . (fn (a b) (prim 'eq? a b)))
    (nat-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (int-eq . (fn (a b) (prim 'eq? a b)))
    (int-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (bool-eq . (fn (a b) (prim 'eq? a b)))
    (bool-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (char-eq . (fn (a b) (prim 'char=? a b)))
    (char-neq . (fn (a b) (prim 'not (prim 'char=? a b))))
    (string-eq . (fn (a b) (prim 'string=? a b)))
    (string-neq . (fn (a b) (prim 'not (prim 'string=? a b))))
    (symbol-eq . (fn (a b) (prim 'eq? a b)))
    (symbol-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    
    ;; list-eq: Requires Eq on elements (recursive)
    (list-eq . (fix list-eq (fn (xs ys)
                                (if (prim 'null? xs)
                                    (prim 'null? ys)
                                    (if (prim 'null? ys)
                                        #f
                                        (if (prim 'eq? (prim 'car xs) (prim 'car ys))
                                            (list-eq (prim 'cdr xs) (prim 'cdr ys))
                                            #f))))))
    
    (list-neq . (fn (xs ys) (prim 'not (list-eq xs ys))))
    
    ;; option-eq: Requires Eq on element
    (option-eq . (fn (a b)
                     (if (prim 'eq? a 'none)
                         (prim 'eq? b 'none)
                         (if (prim 'eq? b 'none)
                             #f
                             (prim 'eq? (prim 'cdr a) (prim 'cdr b))))))
    
    ;; --- Ord ---
    ;; compare returns 'LT, 'EQ, or 'GT
    (nat-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (nat-lt . (fn (a b) (prim 'lt? a b)))
    (nat-lte . (fn (a b) (prim 'le? a b)))
    (nat-gt . (fn (a b) (prim 'gt? a b)))
    (nat-gte . (fn (a b) (prim 'ge? a b)))
    
    (int-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (int-lt . (fn (a b) (prim 'lt? a b)))
    (int-lte . (fn (a b) (prim 'le? a b)))
    (int-gt . (fn (a b) (prim 'gt? a b)))
    (int-gte . (fn (a b) (prim 'ge? a b)))
    
    (char-compare . (fn (a b)
                        (if (prim 'char<? a b) 'LT
                            (if (prim 'char=? a b) 'EQ 'GT))))
    (char-lt . (fn (a b) (prim 'char<? a b)))
    (char-lte . (fn (a b) (prim 'or (prim 'char<? a b) (prim 'char=? a b))))
    (char-gt . (fn (a b) (prim 'char<? b a)))
    (char-gte . (fn (a b) (prim 'or (prim 'char<? b a) (prim 'char=? a b))))
    
    (string-compare . (fn (a b)
                          (if (prim 'string<? a b) 'LT
                              (if (prim 'string=? a b) 'EQ 'GT))))
    (string-lt . (fn (a b) (prim 'string<? a b)))
    (string-lte . (fn (a b) (prim 'or (prim 'string<? a b) (prim 'string=? a b))))
    (string-gt . (fn (a b) (prim 'string>? a b)))
    (string-gte . (fn (a b) (prim 'or (prim 'string>? a b) (prim 'string=? a b))))
    
    ;; --- Show ---
    ;; Note: These require number->string which we add to prim.ss
    (nat-show . (fn (n) (prim 'number->string n)))
    (int-show . (fn (n) (prim 'number->string n)))
    (bool-show . (fn (b) (if b "#t" "#f")))
    (char-show . (fn (c) (prim 'list->string (prim 'list c))))
    (string-show . (fn (s) s))  ; String shows as itself
    (symbol-show . (fn (s) (prim 'symbol->string s)))
    
    ;; list-show: Requires Show on elements
    (list-show . (fix list-show (fn (xs)
                                    (if (prim 'null? xs)
                                        "()"
                                        (prim 'string-append
                                              "("
                                              (prim 'string-append
                                                    (nat-show (prim 'car xs))
                                                    (prim 'string-append
                                                          (list-show-rest (prim 'cdr xs))
                                                          ")")))))))
    
    (list-show-rest . (fix list-show-rest (fn (xs)
                                              (if (prim 'null? xs)
                                                  ""
                                                  (prim 'string-append
                                                        " "
                                                        (prim 'string-append
                                                              (nat-show (prim 'car xs))
                                                              (list-show-rest (prim 'cdr xs))))))))
    
    ;; --- Semigroup ---
    (list-append . (fn (xs ys) (prim 'append xs ys)))
    ;; string-append is already a primitive
    
    ;; --- Monoid ---
    (list-empty . '())
    ;; string-empty would be ""
    ))

;;; Build the prelude environment by evaluating definitions
(define (build-prelude-env fuel)
  (let loop ([defs prelude-defs] [env empty-env] [remaining fuel])
       (if (null? defs)
           env
           (let* ([def (car defs)]
                  [name (car def)]
                  [expr (cdr def)]
                  [result (eval-expr expr env remaining)])
                 (if (eq? (car result) 'ok)
                     (loop (cdr defs)
                           (env-extend env name (cadr result))
                           (caddr result))
                     env)))))

;;; run-prelude : Expr × Fuel → (Result Value Error)
;;; Evaluate with the standard prelude.
(define (run-prelude expr fuel)
  (let ([prelude-env (build-prelude-env 1000)])
       (eval-expr expr prelude-env fuel)))

;;; ============================================================
;;; Traced Evaluation (Automatic Differentiation)
;;; ============================================================
;;;
;;; eval-traced mirrors the structure of eval-expr, but builds a
;;; computational graph during evaluation for automatic differentiation.
;;;
;;; Key differences from eval-expr:
;;;   1. Takes a tape parameter for gradient tracking
;;;   2. Numeric primitives use traced-* operations
;;;   3. Returns traced values for numeric results
;;;   4. Suspension includes tape for resumption
;;;   5. Fuel costs are higher (differentiation overhead)
;;;
;;; Returns:
;;;   (ok value remaining-fuel tape) — evaluation complete
;;;   (suspended expr env fuel tape) — ran out of fuel
;;;   (error tag info)                — evaluation error

;;; eval-expr-traced : Expr × Env × Fuel × Tape → (Result TracedValue Error)
;;; Evaluate with automatic differentiation enabled.
(define (eval-expr-traced expr env fuel tape)
  (cond
   ;; Out of fuel — suspend
   [(out-of-fuel? fuel)
    `(suspended ,expr ,env ,fuel ,tape)]
   
   [else
    ;; Consume fuel for this eval call.
    (let ([remaining (- fuel 1)])
         (cond
          ;; Already a value
          [(value? expr)
           ;; Values are returned as-is (traced operations handle constants)
           `(ok ,expr ,remaining ,tape)]
          
          ;; Variable reference
          [(symbol? expr)
           (let ([result (env-lookup env expr)])
                (if (eq? (car result) 'ok)
                    `(ok ,(cadr result) ,remaining ,tape)
                    result))]
          
          ;; Must be a compound form
          [(not (pair? expr))
           `(error invalid-expression ,expr)]
          
          [else
           (let ([head (car expr)])
                (cond
                 ;; Quote — return datum as-is
                 [(eq? head 'quote)
                  `(ok ,(cadr expr) ,remaining ,tape)]
                 
                 ;; Lambda — create closure
                 [(eq? head 'fn)
                  (let ([params (cadr expr)]
                        [body (caddr expr)])
                       `(ok ,(make-closure params body env) ,remaining ,tape))]
                 
                 ;; Let — evaluate bindings, extend env, evaluate body
                 [(eq? head 'let)
                  (eval-let-traced (cadr expr) (caddr expr) env remaining tape)]
                 
                 ;; Fix — recursive binding
                 [(eq? head 'fix)
                  (eval-fix-traced (cadr expr) (caddr expr) env remaining tape)]
                 
                 ;; If — conditional
                 [(eq? head 'if)
                  (eval-if-traced (cadr expr) (caddr expr) (cadddr expr) env remaining tape)]
                 
                 ;; Case — pattern match on block tag
                 [(eq? head 'case)
                  (eval-case-traced (cadr expr) (cddr expr) env remaining tape)]
                 
                 ;; Prim — pure primitive (use traced operations for diff primitives)
                 [(eq? head 'prim)
                  (eval-prim-traced (cadr expr) (cddr expr) env remaining tape)]
                 
                 ;; Par — parallel evaluation hint
                 [(eq? head 'par)
                  (eval-par-traced (cadr expr) (caddr expr) env remaining tape)]
                 
                 ;; Pseq — sequential evaluation
                 [(eq? head 'pseq)
                  (eval-pseq-traced (cadr expr) (caddr expr) env remaining tape)]
                 
                 ;; Call — explicit application
                 [(eq? head 'call)
                  (eval-call-traced (cadr expr) (cddr expr) env remaining tape)]
                 
                 ;; Implicit application — (f args...)
                 [else
                  (eval-call-traced (car expr) (cdr expr) env remaining tape)]))]))]))

;;; ============================================================
;;; Traced Par/Pseq Evaluation
;;; ============================================================

(define (eval-par-traced a-expr b-expr env fuel tape)
  (let ([a-result (eval-expr-traced a-expr env fuel tape)])
       (case (car a-result)
             [(ok)
              (let ([a-value (cadr a-result)]
                    [fuel-after-a (caddr a-result)]
                    [tape-after-a (cadddr a-result)])
                   (eval-expr-traced b-expr env fuel-after-a tape-after-a))]
             [(suspended) a-result]
             [(error) a-result])))

(define (eval-pseq-traced a-expr b-expr env fuel tape)
  (let ([a-result (eval-expr-traced a-expr env fuel tape)])
       (case (car a-result)
             [(ok)
              (let ([a-value (cadr a-result)]
                    [fuel-after-a (caddr a-result)]
                    [tape-after-a (cadddr a-result)])
                   (eval-expr-traced b-expr env fuel-after-a tape-after-a))]
             [(suspended) a-result]
             [(error) a-result])))

;;; ============================================================
;;; Traced Let Evaluation
;;; ============================================================

(define (eval-let-traced bindings body env fuel tape)
  (if (out-of-fuel? fuel)
      `(suspended (let ,bindings ,body) ,env ,fuel ,tape)
      (eval-let-bindings-traced bindings body env '() fuel tape)))

(define (eval-let-bindings-traced bindings body env acc fuel tape)
  (if (null? bindings)
      ;; All bindings evaluated — evaluate body in extended env
      (eval-expr-traced body (env-extend-alist env (reverse acc)) fuel tape)
      ;; Evaluate next binding
      (let* ([binding (car bindings)]
             [name (car binding)]
             [expr (cadr binding)]
             [result (eval-expr-traced expr env fuel tape)])
            (cond
             [(eq? (car result) 'ok)
              (eval-let-bindings-traced
               (cdr bindings) body env
               (cons (cons name (cadr result)) acc)
               (caddr result)
               (cadddr result))]
             [(eq? (car result) 'suspended)
              `(suspended (let ,(cons binding (cdr bindings)) ,body) ,env
                ,(caddr result) ,(cadddr result))]
             [else result]))))

;;; ============================================================
;;; Traced Fix Evaluation
;;; ============================================================

(define (eval-fix-traced name fn-expr env fuel tape)
  (if (and (pair? fn-expr) (eq? (car fn-expr) 'fn))
      (let* ([params (cadr fn-expr)]
             [body (caddr fn-expr)]
             [rec-env (env-extend env name 'placeholder)]
             [closure (make-closure params body rec-env)])
            (set-cdr! (car rec-env) closure)
            `(ok ,closure ,fuel ,tape))
      `(error fix-requires-fn ,fn-expr)))

;;; ============================================================
;;; Traced If Evaluation
;;; ============================================================

(define (eval-if-traced test-expr then-expr else-expr env fuel tape)
  (if (out-of-fuel? fuel)
      `(suspended (if ,test-expr ,then-expr ,else-expr) ,env ,fuel ,tape)
      (let ([test-result (eval-expr-traced test-expr env fuel tape)])
           (cond
            [(eq? (car test-result) 'ok)
             (let ([test-val (cadr test-result)]
                   [remaining (caddr test-result)]
                   [tape* (cadddr test-result)])
                  ;; Extract primal value from traced values for condition
                  (let ([test-bool (if (traced? test-val)
                                       (traced-value test-val)
                                       test-val)])
                       (if test-bool
                           (eval-expr-traced then-expr env remaining tape*)
                           (eval-expr-traced else-expr env remaining tape*))))]
            [(eq? (car test-result) 'suspended)
             `(suspended (if ,(cadr test-result) ,then-expr ,else-expr)
               ,(caddr test-result) ,(cadddr test-result) ,(car (cddddr test-result)))]
            [else test-result]))))

;;; ============================================================
;;; Traced Case Evaluation
;;; ============================================================

(define (eval-case-traced scrutinee clauses env fuel tape)
  (if (out-of-fuel? fuel)
      `(suspended (case ,scrutinee ,@clauses) ,env ,fuel ,tape)
      (let ([scrut-result (eval-expr-traced scrutinee env fuel tape)])
           (cond
            [(eq? (car scrut-result) 'ok)
             (let ([val (cadr scrut-result)]
                   [remaining (caddr scrut-result)]
                   [tape* (cadddr scrut-result)])
                  (if (block? val)
                      (match-clauses-traced val clauses env remaining tape*)
                      `(error case-requires-block ,val)))]
            [(eq? (car scrut-result) 'suspended)
             `(suspended (case ,(cadr scrut-result) ,@clauses)
               ,(caddr scrut-result) ,(cadddr scrut-result) ,(car (cddddr scrut-result)))]
            [else scrut-result]))))

(define (match-clauses-traced block clauses env fuel tape)
  (if (null? clauses)
      `(error no-matching-clause ,(block-tag block))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             [tag (car pattern)]
             [vars (cdr pattern)])
            (if (eq? tag (block-tag block))
                (let ([refs (block-refs-list block)])
                     (if (= (length vars) (length refs))
                         (eval-expr-traced body (env-extend* env vars refs) fuel tape)
                         `(error pattern-arity-mismatch ,tag)))
                (match-clauses-traced block (cdr clauses) env fuel tape)))))

;;; ============================================================
;;; Traced Primitive Evaluation
;;; ============================================================
;;;
;;; For differentiable primitives, use traced operations.
;;; For non-differentiable primitives, extract primal values and use normal operations.

(define (eval-prim-traced op args env fuel tape)
  (if (and (out-of-fuel? fuel) (pair? args))
      `(suspended (prim ,op ,@args) ,env ,fuel ,tape)
      (eval-prim-args-traced op args env '() fuel tape)))

(define (eval-prim-args-traced op remaining env acc fuel tape)
  (if (null? remaining)
      ;; All args evaluated — apply the primitive (traced or normal)
      (let* ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                         (cadr op)
                         op)]
             [arg-vals (reverse acc)])
            (apply-prim-traced op-sym arg-vals fuel tape))
      ;; Evaluate next arg
      (let ([result (eval-expr-traced (car remaining) env fuel tape)])
           (cond
            [(eq? (car result) 'ok)
             (eval-prim-args-traced op (cdr remaining) env
                                    (cons (cadr result) acc)
                                    (caddr result)
                                    (cadddr result))]
            [(eq? (car result) 'suspended)
             `(suspended (prim ,op ,@(reverse acc) ,(cadr result) ,@(cdr remaining))
               ,(caddr result) ,(cadddr result) ,(car (cddddr result)))]
            [else result]))))

;;; apply-prim-traced : Symbol × Values × Fuel × Tape → (Result TracedValue Error)
;;; Apply a primitive with traced operations for differentiable ops.
(define (apply-prim-traced op args fuel tape)
  (case op
        ;; --------------------------------------------------------
        ;; Differentiable Arithmetic (traced operations)
        ;; --------------------------------------------------------
        [(add)
         (if (>= (length args) 2)
             (let ([x (car args)]
                   [y (cadr args)]
                   [fuel-cost 3])  ; base 2 + tracing 1
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'add ,x ,y) empty-env ,fuel ,tape)
                      `(ok ,(traced-add x y) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(sub)
         (if (>= (length args) 2)
             (let ([x (car args)]
                   [y (cadr args)]
                   [fuel-cost 3])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'sub ,x ,y) empty-env ,fuel ,tape)
                      `(ok ,(traced-sub x y) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(mul)
         (if (>= (length args) 2)
             (let ([x (car args)]
                   [y (cadr args)]
                   [fuel-cost 3])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'mul ,x ,y) empty-env ,fuel ,tape)
                      `(ok ,(traced-mul x y) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(div)
         (if (>= (length args) 2)
             (let ([x (car args)]
                   [y (cadr args)]
                   [fuel-cost 3])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'div ,x ,y) empty-env ,fuel ,tape)
                      ;; Check for division by zero
                      (let ([y-val (if (traced? y) (traced-value y) y)])
                           (if (zero? y-val)
                               `(error div-by-zero)
                               `(ok ,(traced-div x y) ,(- fuel fuel-cost) ,tape)))))
             `(error prim-arity ,op))]
        
        [(neg)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 3])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'neg ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-neg x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        ;; --------------------------------------------------------
        ;; Differentiable Transcendental Functions
        ;; --------------------------------------------------------
        [(sqrt)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 6])  ; base 4 + tracing 2
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'sqrt ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-sqrt x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(exp)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 7])  ; base 5 + tracing 2
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'exp ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-exp x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(log)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 7])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'log ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-log x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(sin)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 7])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'sin ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-sin x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        [(cos)
         (if (= (length args) 1)
             (let ([x (car args)]
                   [fuel-cost 7])
                  (if (< fuel fuel-cost)
                      `(suspended (prim 'cos ,x) empty-env ,fuel ,tape)
                      `(ok ,(traced-cos x) ,(- fuel fuel-cost) ,tape)))
             `(error prim-arity ,op))]
        
        ;; --------------------------------------------------------
        ;; Non-differentiable Operations (extract primal, apply normally)
        ;; --------------------------------------------------------
        [else
         ;; Extract primal values from traced args
         (let* ([primal-args (map (lambda (v)
                                          (if (traced? v)
                                              (traced-value v)
                                              v))
                                  args)]
                ;; Apply normal primitive
                [result (apply prim (cons op primal-args))])
               ;; Return result as-is (constants are handled by traced ops)
               `(ok ,result ,fuel ,tape))]))

;;; ensure-traced : Value → TracedValue
;;; Ensure a value is traced (wrap constants in traced-const).
(define (ensure-traced v)
  (if (traced? v)
      v
      (make-traced-const v)))

;;; ============================================================
;;; Traced Call Evaluation
;;; ============================================================

(define (eval-call-traced fn-expr arg-exprs env fuel tape)
  (if (out-of-fuel? fuel)
      `(suspended (call ,fn-expr ,@arg-exprs) ,env ,fuel ,tape)
      (let ([fn-result (eval-expr-traced fn-expr env fuel tape)])
           (cond
            [(eq? (car fn-result) 'ok)
             (let ([fn-val (cadr fn-result)]
                   [remaining (caddr fn-result)]
                   [tape* (cadddr fn-result)])
                  (if (closure? fn-val)
                      (eval-call-args-traced fn-val arg-exprs env '() remaining tape*)
                      `(error not-a-function ,fn-val)))]
            [(eq? (car fn-result) 'suspended)
             `(suspended (call ,(cadr fn-result) ,@arg-exprs)
               ,(caddr fn-result) ,(cadddr fn-result) ,(car (cddddr fn-result)))]
            [else fn-result]))))

(define (eval-call-args-traced closure remaining env acc fuel tape)
  (if (null? remaining)
      ;; All args evaluated — apply the closure
      (let* ([params (closure-params closure)]
             [body (closure-body closure)]
             [closure-env (closure-env closure)]
             [arg-vals (reverse acc)])
            (if (= (length params) (length arg-vals))
                (eval-expr-traced body (env-extend* closure-env params arg-vals) fuel tape)
                `(error arity-mismatch (expected ,(length params)) (got ,(length arg-vals)))))
      ;; Evaluate next arg
      (let ([result (eval-expr-traced (car remaining) env fuel tape)])
           (cond
            [(eq? (car result) 'ok)
             (eval-call-args-traced closure (cdr remaining) env
                                    (cons (cadr result) acc)
                                    (caddr result)
                                    (cadddr result))]
            [(eq? (car result) 'suspended)
             `(suspended (call ,closure ,@(reverse acc) ,(cadr result) ,@(cdr remaining))
               ,(caddr result) ,(cadddr result) ,(car (cddddr result)))]
            [else result]))))

;;; ============================================================
;;; High-Level Traced Evaluation API
;;; ============================================================

;;; eval-and-grad : Expr × Env × VarNames × Values × Fuel → (Value, Gradients)
;;; Evaluate expression and compute gradients w.r.t. named variables.
(define (eval-and-grad expr env var-names values fuel)
  (let* ([tape (make-reverse-tape)]
         ;; Create pairs of (name . value)
         [pairs (let loop ([names var-names] [vals values])
                     (if (null? names)
                         '()
                         (cons (cons (car names) (car vals))
                               (loop (cdr names) (cdr vals)))))]
         ;; Extend environment with traced variables
         [env* (fold-left (lambda (e name-val)
                                  (let ([name (car name-val)]
                                        [val (cdr name-val)])
                                       (env-extend e name (make-traced-var val tape))))
                          env
                          pairs)]
         ;; Traced evaluation
         [result (eval-expr-traced expr env* fuel tape)])
        (cond
         [(eq? (car result) 'ok)
          (let ([traced-val (cadr result)])
               (if (traced? traced-val)
                   ;; Backward pass
                   (let* ([output-id (traced-id traced-val)]
                          [grads-table (backward tape output-id 1)]
                          ;; Extract gradients for input variables
                          [grads (map (lambda (name)
                                              (let ([var (cadr (env-lookup env* name))])
                                                   (if (traced? var)
                                                       (hashtable-ref grads-table (traced-id var) 0)
                                                       0)))
                                      var-names)]
                          [primal (traced-value traced-val)])
                         (values primal grads))
                   ;; Constant result - all gradients are 0
                   (values traced-val (map (lambda (_) 0) var-names))))]
         [(eq? (car result) 'suspended)
          (error 'eval-and-grad "fuel exhausted during traced evaluation")]
         [else
          (error 'eval-and-grad "evaluation error" result)])))

;;; run-traced : Expr × Fuel → (ok TracedValue) | (suspended Expr) | (error ...)
;;; Evaluate with tracing enabled and empty environment.
(define (run-traced expr fuel)
  (let* ([tape (make-reverse-tape)]
         [result (eval-expr-traced expr empty-env fuel tape)])
        (cond
         [(eq? (car result) 'ok)
          `(ok ,(cadr result))]
         [(eq? (car result) 'suspended)
          `(suspended ,(cadr result))]
         [else result])))
