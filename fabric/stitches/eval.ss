;;; fabric/stitches/eval.ss — The Evaluator with Fuel
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

(load "prelude.ss")
(load "block.ss")
(load "prim.ss")

;;; ============================================================
;;; Fuel
;;; ============================================================

;;; Fuel is a natural number. Each reduction step costs 1 fuel.
;;; When fuel hits 0, we suspend.

(define (fuel? n) (and (integer? n) (>= n 0)))

(define (out-of-fuel? n) (zero? n))

;;; ============================================================
;;; Values
;;; ============================================================

;;; A value is something that doesn't reduce further.

(define (value? v)
  (or (number? v)
      (string? v)
      (boolean? v)
      (null? v)
      (bytevector? v)
      (closure? v)
      (block? v)))

(define (closure? v)
  (and (pair? v) (eq? (car v) 'closure)))

(define (closure-params c) (cadr c))
(define (closure-body c) (caddr c))
(define (closure-env c) (cadddr c))

(define (make-closure params body env)
  `(closure ,params ,body ,env))

;;; ============================================================
;;; Environments
;;; ============================================================

;;; An environment is an alist mapping symbols to values.

(define empty-env '())

(define (env-lookup env name)
  (let ([entry (assq name env)])
    (if entry
        `(ok ,(cdr entry))
        `(error unbound-variable ,name))))

(define (env-extend env name value)
  (cons (cons name value) env))

(define (env-extend* env names values)
  (if (null? names)
      env
      (env-extend* (env-extend env (car names) (car values))
                   (cdr names)
                   (cdr values))))

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

    ;; Already a value
    [(value? expr)
     `(ok ,expr ,fuel)]

    ;; Variable reference
    [(symbol? expr)
     (let ([result (env-lookup env expr)])
       (if (eq? (car result) 'ok)
           `(ok ,(cadr result) ,(- fuel 1))
           result))]

    ;; Must be a compound form
    [(not (pair? expr))
     `(error invalid-expression ,expr)]

    [else
     (let ([head (car expr)])
       (cond
         ;; Quote — return datum as-is
         [(eq? head 'quote)
          `(ok ,(cadr expr) ,(- fuel 1))]

         ;; Lambda — create closure
         [(eq? head 'fn)
          (let ([params (cadr expr)]
                [body (caddr expr)])
            `(ok ,(make-closure params body env) ,(- fuel 1)))]

         ;; Let — evaluate bindings, extend env, evaluate body
         [(eq? head 'let)
          (eval-let (cadr expr) (caddr expr) env fuel)]

         ;; Fix — recursive binding
         [(eq? head 'fix)
          (eval-fix (cadr expr) (caddr expr) env fuel)]

         ;; If — conditional
         [(eq? head 'if)
          (eval-if (cadr expr) (caddr expr) (cadddr expr) env fuel)]

         ;; Case — pattern match on block tag
         [(eq? head 'case)
          (eval-case (cadr expr) (cddr expr) env fuel)]

         ;; Prim — pure primitive
         [(eq? head 'prim)
          (eval-prim (cadr expr) (cddr expr) env fuel)]

         ;; Call — explicit application
         [(eq? head 'call)
          (eval-call (cadr expr) (cddr expr) env fuel)]

         ;; Implicit application — (f args...)
         [else
          (eval-call (car expr) (cdr expr) env fuel)]))]))

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
  (if (out-of-fuel? fuel)
      `(suspended (fix ,name ,fn-expr) ,env)
      ;; fn-expr should be (fn (params...) body)
      (if (and (pair? fn-expr) (eq? (car fn-expr) 'fn))
          (let* ([params (cadr fn-expr)]
                 [body (caddr fn-expr)]
                 ;; Create a recursive closure by including itself in its env
                 [rec-env (env-extend env name 'placeholder)]
                 [closure (make-closure params body rec-env)])
            ;; MUTATION: Tie the knot - see rationale above
            (set-cdr! (car rec-env) closure)
            `(ok ,closure ,(- fuel 1)))
          `(error fix-requires-fn ,fn-expr))))

;;; ============================================================
;;; If Evaluation
;;; ============================================================

(define (eval-if test-expr then-expr else-expr env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (if ,test-expr ,then-expr ,else-expr) ,env)
      (let ([test-result (eval-expr test-expr env (- fuel 1))])
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
      (let ([scrut-result (eval-expr scrutinee env (- fuel 1))])
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

(define (eval-prim op args env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (prim ,op ,@args) ,env)
      ;; Evaluate all arguments
      (eval-prim-args op args env '() (- fuel 1))))

(define (eval-prim-args op remaining env acc fuel)
  (if (null? remaining)
      ;; All args evaluated — call the primitive
      (let ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
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
      (let ([fn-result (eval-expr fn-expr env (- fuel 1))])
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

;;; eval-with-env : Expr × Env × Fuel → Result
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

;;; run-prelude : Expr × Fuel → Result
;;; Evaluate with the standard prelude.
(define (run-prelude expr fuel)
  (let ([prelude-env (build-prelude-env 100)])
    (eval-expr expr prelude-env fuel)))
