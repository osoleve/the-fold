;;; @module abstract-interp
;;; @requires adjunction interval hamt

(require 'adjunction)
(require 'interval)
(require 'hamt)

(doc 'module 'abstract-interp)
(doc 'description "Abstract Interpretation via Galois Connections. Provides domains and abstract semantics for static analysis: Signs⊣Embed, Intervals⊣Embed, Types⊣Erase, Reach⊣Concrete.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'tier 1)
(doc 'see-also '(adjunction.ss interval.ss interval-autodiff.ss))

;;; ============================================================================
;;; Section 1: Abstract Domain Infrastructure
;;; ============================================================================

(doc 'section 'abstract-domains)
(doc 'note "An abstract domain is a complete lattice with:")
(doc 'note "  - ⊥ (bottom): no information / unreachable")
(doc 'note "  - ⊤ (top): any value / no precision")
(doc 'note "  - ⊔ (join): least upper bound")
(doc 'note "  - ⊓ (meet): greatest lower bound")
(doc 'note "  - ⊑ (order): approximation ordering")

;;; make-abstract-domain : Symbol × Any × Any × (a × a → a) × (a × a → a) × (a × a → Boolean) → AbstractDomain
(define (make-abstract-domain name bottom top join meet leq)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any Any (-> Any Any Any) (-> Any Any Any) (-> Any Any Boolean) AbstractDomain))
  (list 'abstract-domain name bottom top join meet leq))

(define (abstract-domain? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'abstract-domain)))

(define (domain-name d) (list-ref d 1))
(define (domain-bottom d) (list-ref d 2))
(define (domain-top d) (list-ref d 3))
(define (domain-join d) (list-ref d 4))
(define (domain-meet d) (list-ref d 5))
(define (domain-leq d) (list-ref d 6))

;;; domain-join-all : AbstractDomain × (List a) → a
;;; Join a list of abstract values.
(define (domain-join-all domain vals)
  (doc 'export #t)
  (if (null? vals)
      (domain-bottom domain)
      (fold-left (domain-join domain) (car vals) (cdr vals))))

;;; domain-meet-all : AbstractDomain × (List a) → a
;;; Meet a list of abstract values.
(define (domain-meet-all domain vals)
  (doc 'export #t)
  (if (null? vals)
      (domain-top domain)
      (fold-left (domain-meet domain) (car vals) (cdr vals))))

;;; ============================================================================
;;; Section 2: Sign Domain (Signs⊣Embed)
;;; ============================================================================

(doc 'section 'sign-domain)
(doc 'note "The sign domain abstracts integers to {⊥, -, 0, +, ⊤}.")
(doc 'note "Galois connection: α(S) = ⊔{sign(n) | n ∈ S}, γ(s) = {n | sign(n) ⊑ s}")
(doc 'note "Lattice structure:")
(doc 'note "        ⊤")
(doc 'note "      / | \\")
(doc 'note "     -  0  +")
(doc 'note "      \\ | /")
(doc 'note "        ⊥")

;;; Sign values (using symbols for readability)
(define sign-bottom 'sign-bottom)  ; Unreachable
(define sign-neg    'sign-neg)     ; Negative
(define sign-zero   'sign-zero)    ; Zero
(define sign-pos    'sign-pos)     ; Positive
(define sign-top    'sign-top)     ; Any sign

(define (sign? x)
  (doc 'export #t)
  (memq x (list sign-bottom sign-neg sign-zero sign-pos sign-top)))

;;; sign-leq : Sign × Sign → Boolean
;;; Ordering: ⊥ ⊑ everything, everything ⊑ ⊤
(define (sign-leq a b)
  (doc 'export #t)
  (or (eq? a sign-bottom)
      (eq? b sign-top)
      (eq? a b)))

;;; sign-join : Sign × Sign → Sign
;;; Least upper bound.
(define (sign-join a b)
  (doc 'export #t)
  (cond
    [(eq? a sign-bottom) b]
    [(eq? b sign-bottom) a]
    [(eq? a b) a]
    [else sign-top]))

;;; sign-meet : Sign × Sign → Sign
;;; Greatest lower bound.
(define (sign-meet a b)
  (doc 'export #t)
  (cond
    [(eq? a sign-top) b]
    [(eq? b sign-top) a]
    [(eq? a b) a]
    [else sign-bottom]))

;;; The Sign abstract domain
(doc sign-domain 'export #t)
(define sign-domain
  (make-abstract-domain 'signs sign-bottom sign-top sign-join sign-meet sign-leq))

;;; sign-alpha : Integer → Sign
;;; Abstraction function: concrete integer to sign.
(define (sign-alpha n)
  (doc 'export #t)
  (doc 'type '(-> Integer Sign))
  (cond
    [(< n 0) sign-neg]
    [(= n 0) sign-zero]
    [else sign-pos]))

;;; sign-gamma : Sign → (Integer → Boolean)
;;; Concretization function: sign to predicate on integers.
(define (sign-gamma s)
  (doc 'export #t)
  (doc 'type '(-> Sign (-> Integer Boolean)))
  (case s
    [(sign-bottom) (lambda (n) #f)]
    [(sign-neg)    (lambda (n) (< n 0))]
    [(sign-zero)   (lambda (n) (= n 0))]
    [(sign-pos)    (lambda (n) (> n 0))]
    [(sign-top)    (lambda (n) #t)]))

;;; sign-alpha-set : (List Integer) → Sign
;;; Abstraction for a set of integers (join all signs).
(define (sign-alpha-set nums)
  (doc 'export #t)
  (if (null? nums)
      sign-bottom
      (fold-left sign-join sign-bottom (map sign-alpha nums))))

;;; galois-signs : Galois
;;; The Galois connection Signs⊣Embed.
;;; Lower adjoint (α): abstracts to sign
;;; Upper adjoint (γ): embeds sign as predicate
(doc galois-signs 'export #t)
(define galois-signs
  (make-galois 'signs-embed sign-alpha sign-gamma))

;;; ============================================================================
;;; Section 3: Abstract Sign Arithmetic
;;; ============================================================================

(doc 'section 'sign-arithmetic)
(doc 'note "Abstract operations that are sound with respect to concrete semantics.")
(doc 'note "For operation ⊕: α(a ⊕ b) ⊑ α(a) ⊕♯ α(b)")

;;; sign-add : Sign × Sign → Sign
;;; Abstract addition.
(define (sign-add a b)
  (doc 'export #t)
  (cond
    [(or (eq? a sign-bottom) (eq? b sign-bottom)) sign-bottom]
    [(or (eq? a sign-top) (eq? b sign-top)) sign-top]
    [(eq? a sign-zero) b]
    [(eq? b sign-zero) a]
    [(eq? a b) a]  ; neg+neg=neg, pos+pos=pos
    [else sign-top]))  ; neg+pos or pos+neg = unknown

;;; sign-neg : Sign → Sign
;;; Abstract negation.
(define (sign-negate s)
  (doc 'export #t)
  (case s
    [(sign-bottom) sign-bottom]
    [(sign-neg)    sign-pos]
    [(sign-zero)   sign-zero]
    [(sign-pos)    sign-neg]
    [(sign-top)    sign-top]))

;;; sign-sub : Sign × Sign → Sign
;;; Abstract subtraction: a - b = a + (-b)
(define (sign-sub a b)
  (doc 'export #t)
  (sign-add a (sign-negate b)))

;;; sign-mul : Sign × Sign → Sign
;;; Abstract multiplication.
(define (sign-mul a b)
  (doc 'export #t)
  (cond
    [(or (eq? a sign-bottom) (eq? b sign-bottom)) sign-bottom]
    [(or (eq? a sign-zero) (eq? b sign-zero)) sign-zero]
    [(or (eq? a sign-top) (eq? b sign-top)) sign-top]
    [(eq? a b) sign-pos]  ; neg*neg=pos, pos*pos=pos
    [else sign-neg]))     ; neg*pos or pos*neg = neg

;;; sign-div : Sign × Sign → Sign
;;; Abstract division (integer division semantics).
(define (sign-div a b)
  (doc 'export #t)
  (cond
    [(or (eq? a sign-bottom) (eq? b sign-bottom)) sign-bottom]
    [(eq? b sign-zero) sign-bottom]  ; Division by zero = unreachable
    [(eq? a sign-zero) sign-zero]
    [(or (eq? a sign-top) (eq? b sign-top)) sign-top]
    [(eq? a b) sign-pos]
    [else sign-neg]))

;;; ============================================================================
;;; Section 4: Interval Domain (Intervals⊣Embed)
;;; ============================================================================

(doc 'section 'interval-domain)
(doc 'note "The interval domain abstracts sets of reals to closed intervals [lo, hi].")
(doc 'note "Integrates with lattice/numeric/interval.ss for arithmetic.")
(doc 'note "⊥ = empty interval, ⊤ = (-∞, +∞)")

;;; Interval domain bottom (empty interval - use special marker)
(define interval-bottom '(interval-bottom))
(define (interval-bottom? x)
  (and (pair? x) (eq? (car x) 'interval-bottom)))

;;; interval-domain-leq : Interval × Interval → Boolean
;;; Ordering by set inclusion: [a,b] ⊑ [c,d] iff c ≤ a and b ≤ d
(define (interval-domain-leq a b)
  (doc 'export #t)
  (cond
    [(interval-bottom? a) #t]
    [(interval-bottom? b) #f]
    [else (interval-subset? a b)]))

;;; interval-domain-join : Interval × Interval → Interval
;;; Join is hull (smallest enclosing interval).
(define (interval-domain-join a b)
  (doc 'export #t)
  (cond
    [(interval-bottom? a) b]
    [(interval-bottom? b) a]
    [else (interval-hull a b)]))

;;; interval-domain-meet : Interval × Interval → Interval
;;; Meet is intersection.
(define (interval-domain-meet a b)
  (doc 'export #t)
  (cond
    [(interval-bottom? a) interval-bottom]
    [(interval-bottom? b) interval-bottom]
    [else
     (let ([result (interval-intersection a b)])
       (if (error? result)
           interval-bottom
           result))]))

;;; The Interval abstract domain
(doc interval-domain 'export #t)
(define interval-domain
  (make-abstract-domain
   'intervals
   interval-bottom
   (entire-interval)
   interval-domain-join
   interval-domain-meet
   interval-domain-leq))

;;; interval-alpha : Real → Interval
;;; Abstraction: single real to point interval.
(define (interval-alpha n)
  (doc 'export #t)
  (interval-singleton n))

;;; interval-alpha-set : (List Real) → Interval
;;; Abstraction for a set of reals (hull).
(define (interval-alpha-set nums)
  (doc 'export #t)
  (if (null? nums)
      interval-bottom
      (make-interval (apply min nums) (apply max nums))))

;;; interval-gamma : Interval → (Real → Boolean)
;;; Concretization: interval to membership predicate.
(define (interval-gamma iv)
  (doc 'export #t)
  (if (interval-bottom? iv)
      (lambda (n) #f)
      (lambda (n) (interval-contains? iv n))))

;;; galois-intervals : Galois
;;; The Galois connection Intervals⊣Embed.
(doc galois-intervals 'export #t)
(define galois-intervals
  (make-galois 'intervals-embed interval-alpha interval-gamma))

;;; ============================================================================
;;; Section 5: Type Domain (Types⊣Erase)
;;; ============================================================================

(doc 'section 'type-domain)
(doc 'note "Simple type abstraction for demonstration.")
(doc 'note "Abstracts values to: ⊥, int, real, bool, string, ⊤")
(doc 'note "Useful for type-based dead code elimination.")

(define type-bottom 'type-bottom)
(define type-int    'type-int)
(define type-real   'type-real)
(define type-bool   'type-bool)
(define type-string 'type-string)
(define type-list   'type-list)
(define type-proc   'type-proc)
(define type-top    'type-top)

(define all-types (list type-bottom type-int type-real type-bool
                        type-string type-list type-proc type-top))

(define (simple-type? x)
  (doc 'export #t)
  (memq x all-types))

;;; Type lattice: flat except int ⊑ real (numeric tower)
;;;        ⊤
;;;    / | | | | \
;;;   real bool str list proc
;;;    |
;;;   int
;;;    \ | | | | /
;;;        ⊥

;;; type-leq : Type × Type → Boolean
(define (type-leq a b)
  (doc 'export #t)
  (or (eq? a type-bottom)
      (eq? b type-top)
      (eq? a b)
      (and (eq? a type-int) (eq? b type-real))))

;;; type-join : Type × Type → Type
(define (type-join a b)
  (doc 'export #t)
  (cond
    [(eq? a type-bottom) b]
    [(eq? b type-bottom) a]
    [(eq? a b) a]
    [(and (eq? a type-int) (eq? b type-real)) type-real]
    [(and (eq? a type-real) (eq? b type-int)) type-real]
    [else type-top]))

;;; type-meet : Type × Type → Type
(define (type-meet a b)
  (doc 'export #t)
  (cond
    [(eq? a type-top) b]
    [(eq? b type-top) a]
    [(eq? a b) a]
    [(and (eq? a type-int) (eq? b type-real)) type-int]
    [(and (eq? a type-real) (eq? b type-int)) type-int]
    [else type-bottom]))

;;; The Type abstract domain
(doc type-domain 'export #t)
(define type-domain
  (make-abstract-domain 'types type-bottom type-top type-join type-meet type-leq))

;;; type-alpha : Any → Type
;;; Abstraction: value to type.
(define (type-alpha v)
  (doc 'export #t)
  (cond
    [(integer? v) type-int]
    [(real? v) type-real]
    [(boolean? v) type-bool]
    [(string? v) type-string]
    [(list? v) type-list]
    [(procedure? v) type-proc]
    [else type-top]))

;;; type-gamma : Type → (Any → Boolean)
;;; Concretization: type to predicate.
(define (type-gamma t)
  (doc 'export #t)
  (case t
    [(type-bottom) (lambda (v) #f)]
    [(type-int)    integer?]
    [(type-real)   real?]
    [(type-bool)   boolean?]
    [(type-string) string?]
    [(type-list)   list?]
    [(type-proc)   procedure?]
    [(type-top)    (lambda (v) #t)]))

;;; galois-types : Galois
;;; The Galois connection Types⊣Erase.
(doc galois-types 'export #t)
(define galois-types
  (make-galois 'types-erase type-alpha type-gamma))

;;; ============================================================================
;;; Section 6: Reachability Domain (Reach⊣Concrete)
;;; ============================================================================

(doc 'section 'reachability-domain)
(doc 'note "Binary reachability domain: {⊥, reachable}.")
(doc 'note "Used for dead code elimination.")
(doc 'note "⊥ = definitely unreachable, reachable = possibly reachable")

(define reach-bottom 'reach-unreachable)
(define reach-top    'reach-reachable)

(define (reach? x)
  (doc 'export #t)
  (or (eq? x reach-bottom) (eq? x reach-top)))

(define (reach-leq a b)
  (or (eq? a reach-bottom) (eq? b reach-top)))

(define (reach-join a b)
  (if (or (eq? a reach-top) (eq? b reach-top))
      reach-top
      reach-bottom))

(define (reach-meet a b)
  (if (and (eq? a reach-top) (eq? b reach-top))
      reach-top
      reach-bottom))

;;; The Reachability domain
(doc reach-domain 'export #t)
(define reach-domain
  (make-abstract-domain 'reach reach-bottom reach-top reach-join reach-meet reach-leq))

;;; reach-alpha : Any → Reach
;;; Everything concrete is reachable.
(define (reach-alpha v)
  reach-top)

;;; reach-gamma : Reach → (Any → Boolean)
(define (reach-gamma r)
  (if (eq? r reach-bottom)
      (lambda (v) #f)
      (lambda (v) #t)))

;;; galois-reach : Galois
;;; The Galois connection Reach⊣Concrete.
(doc galois-reach 'export #t)
(define galois-reach
  (make-galois 'reach-concrete reach-alpha reach-gamma))

;;; ============================================================================
;;; Section 7: Abstract Interpretation Framework
;;; ============================================================================

(doc 'section 'abstract-interpretation)
(doc 'note "Framework for interpreting expressions over abstract domains.")

;;; make-abstract-env : AbstractDomain → AbstractEnv
;;; Create an empty abstract environment.
(define (make-abstract-env domain)
  (doc 'export #t)
  (list 'abstract-env domain hamt-empty))

(define (abstract-env? x)
  (and (pair? x) (eq? (car x) 'abstract-env)))

(define (env-domain env) (cadr env))
(define (env-table env) (caddr env))

;;; env-lookup : AbstractEnv × Symbol → AbstractValue
(define (env-lookup env var)
  (doc 'export #t)
  (hamt-lookup-or var (env-table env) (domain-bottom (env-domain env))))

;;; env-extend : AbstractEnv × Symbol × AbstractValue → AbstractEnv
(define (env-extend env var val)
  (doc 'export #t)
  (list 'abstract-env (env-domain env) (hamt-assoc var val (env-table env))))

;;; env-join : AbstractEnv × AbstractEnv → AbstractEnv
;;; Join environments pointwise.
(define (env-join env1 env2)
  (doc 'export #t)
  (let* ([domain (env-domain env1)]
         [join (domain-join domain)]
         [keys1 (hamt-keys (env-table env1))]
         [keys2 (hamt-keys (env-table env2))])
    ;; Start with keys from env1, joining with env2 values
    (let* ([merged (fold-left
                    (lambda (acc k)
                      (let ([v1 (env-lookup env1 k)]
                            [v2 (env-lookup env2 k)])
                        (hamt-assoc k (join v1 v2) acc)))
                    hamt-empty
                    keys1)]
           ;; Add keys only in env2
           [merged (fold-left
                    (lambda (acc k)
                      (if (hamt-has-key? k acc)
                          acc
                          (hamt-assoc k (env-lookup env2 k) acc)))
                    merged
                    keys2)])
      (list 'abstract-env domain merged))))

;;; ============================================================================
;;; Section 8: Abstract Interpreter for Simple Expressions
;;; ============================================================================

(doc 'section 'abstract-eval)
(doc 'note "Abstract evaluation over sign and interval domains.")

;;; abstract-eval-sign : Expr × SignEnv → Sign
;;; Evaluate expression abstractly in sign domain.
(define (abstract-eval-sign expr env)
  (doc 'export #t)
  (doc 'type '(-> Expr AbstractEnv Sign))
  (cond
    ;; Literals
    [(integer? expr) (sign-alpha expr)]
    [(real? expr) (sign-alpha (inexact->exact (round expr)))]

    ;; Variables
    [(symbol? expr) (env-lookup env expr)]

    ;; Arithmetic operations
    [(and (pair? expr) (eq? (car expr) '+))
     (let ([args (cdr expr)])
       (if (null? args)
           sign-zero
           (fold-left sign-add
                     (abstract-eval-sign (car args) env)
                     (map (lambda (e) (abstract-eval-sign e env)) (cdr args)))))]

    [(and (pair? expr) (eq? (car expr) '-))
     (let ([args (cdr expr)])
       (cond
         [(null? args) sign-zero]
         [(null? (cdr args)) (sign-negate (abstract-eval-sign (car args) env))]
         [else
          (fold-left sign-sub
                    (abstract-eval-sign (car args) env)
                    (map (lambda (e) (abstract-eval-sign e env)) (cdr args)))]))]

    [(and (pair? expr) (eq? (car expr) '*))
     (let ([args (cdr expr)])
       (if (null? args)
           sign-pos  ; (* ) = 1
           (fold-left sign-mul
                     (abstract-eval-sign (car args) env)
                     (map (lambda (e) (abstract-eval-sign e env)) (cdr args)))))]

    [(and (pair? expr) (eq? (car expr) '/))
     (let ([args (cdr expr)])
       (cond
         [(null? args) sign-top]  ; (/) is invalid
         [(null? (cdr args))      ; Unary: (/ x) = 1/x
          (let ([a (abstract-eval-sign (car args) env)])
            (if (eq? a sign-zero)
                sign-bottom  ; Division by zero = unreachable
                a))]         ; sign(1/x) = sign(x) for x ≠ 0
         [else
          (fold-left sign-div
                    (abstract-eval-sign (car args) env)
                    (map (lambda (e) (abstract-eval-sign e env)) (cdr args)))]))]

    [(and (pair? expr) (eq? (car expr) 'sqr))
     ;; sqr always produces non-negative: sign-pos or sign-zero
     (let ([a (abstract-eval-sign (cadr expr) env)])
       (case a
         [(sign-bottom) sign-bottom]
         [(sign-zero) sign-zero]
         [(sign-neg sign-pos) sign-pos]  ; x² > 0 for x ≠ 0
         [else sign-top]))]  ; sign-top → could be zero or positive

    ;; Default: top
    [else sign-top]))

;;; abstract-eval-interval : Expr × IntervalEnv → Interval
;;; Evaluate expression abstractly in interval domain.
(define (abstract-eval-interval expr env)
  (doc 'export #t)
  (doc 'type '(-> Expr AbstractEnv Interval))
  (cond
    ;; Literals
    [(number? expr) (interval-singleton expr)]

    ;; Variables
    [(symbol? expr)
     (env-lookup env expr)]

    ;; Arithmetic
    [(and (pair? expr) (eq? (car expr) '+))
     (fold-left interval-add
               (interval-singleton 0)
               (map (lambda (e) (abstract-eval-interval e env)) (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '-))
     (let ([args (cdr expr)])
       (cond
         [(null? args) (interval-singleton 0)]
         [(null? (cdr args)) (interval-neg (abstract-eval-interval (car args) env))]
         [else
          (let ([first (abstract-eval-interval (car args) env)])
            (fold-left interval-sub first
                      (map (lambda (e) (abstract-eval-interval e env)) (cdr args))))]))]

    [(and (pair? expr) (eq? (car expr) '*))
     (fold-left interval-mul
               (interval-singleton 1)
               (map (lambda (e) (abstract-eval-interval e env)) (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '/))
     (let ([args (cdr expr)])
       (cond
         [(null? args) (entire-interval)]  ; (/) is invalid
         [(null? (cdr args))               ; Unary: (/ x) = 1/x
          (let* ([x (abstract-eval-interval (car args) env)]
                 [result (interval-recip x)])
            (if (error? result)
                (entire-interval)  ; Division by zero → top
                result))]
         [else                             ; Binary+: x / y / z ...
          (let ([num (abstract-eval-interval (car args) env)])
            (fold-left (lambda (acc e)
                         (let ([den (abstract-eval-interval e env)])
                           (let ([result (interval-div acc den)])
                             (if (error? result) (entire-interval) result))))
                       num
                       (cdr args)))]))]

    [(and (pair? expr) (eq? (car expr) 'sqr))
     (interval-sqr (abstract-eval-interval (cadr expr) env))]

    [(and (pair? expr) (eq? (car expr) 'sqrt))
     (let ([result (interval-sqrt (abstract-eval-interval (cadr expr) env))])
       (if (error? result) (entire-interval) result))]

    ;; Default
    [else (entire-interval)]))

;;; ============================================================================
;;; Section 9: Soundness Verification
;;; ============================================================================

(doc 'section 'soundness)
(doc 'note "Verify that abstract operations are sound approximations.")

;;; verify-galois-soundness : Galois × (List Concrete) → Boolean
;;; Check: for all c in concrete-set, c ∈ γ(α(c))
(define (verify-galois-soundness gc concrete-vals)
  (doc 'export #t)
  (let ([alpha (galois-lower gc)]
        [gamma (galois-upper gc)])
    (let loop ([vals concrete-vals])
      (or (null? vals)
          (let* ([c (car vals)]
                 [a (alpha c)]
                 [pred (gamma a)])
            (and (pred c)
                 (loop (cdr vals))))))))

;;; verify-sign-soundness : (List Integer) → Boolean
;;; Verify sign domain soundness on given integers.
(define (verify-sign-soundness nums)
  (doc 'export #t)
  (verify-galois-soundness galois-signs nums))

;;; verify-interval-soundness : (List Real) → Boolean
;;; Verify interval domain soundness on given reals.
(define (verify-interval-soundness nums)
  (doc 'export #t)
  (verify-galois-soundness galois-intervals nums))

;;; ============================================================================
;;; Section 10: Utility Functions
;;; ============================================================================

;;; sign->string : Sign → String
(define (sign->string s)
  (doc 'export #t)
  (case s
    [(sign-bottom) "⊥"]
    [(sign-neg)    "-"]
    [(sign-zero)   "0"]
    [(sign-pos)    "+"]
    [(sign-top)    "⊤"]))

;;; interval->abstract-string : Interval → String
(define (interval->abstract-string iv)
  (doc 'export #t)
  (if (interval-bottom? iv)
      "⊥"
      (interval->string iv)))

;;; type->string : Type → String
(define (type->string t)
  (doc 'export #t)
  (case t
    [(type-bottom) "⊥"]
    [(type-int)    "int"]
    [(type-real)   "real"]
    [(type-bool)   "bool"]
    [(type-string) "string"]
    [(type-list)   "list"]
    [(type-proc)   "proc"]
    [(type-top)    "⊤"]))

;;; ============================================================================
;;; Section 11: Integration Example - Widening for Loops
;;; ============================================================================

(doc 'section 'widening)
(doc 'note "Widening operators ensure termination of fixpoint iteration.")
(doc 'note "For intervals: if bound grows, jump to infinity.")

;;; interval-widen : Interval × Interval → Interval
;;; Widening: accelerate convergence by jumping to infinity when bounds grow.
(define (interval-widen prev curr)
  (doc 'export #t)
  (doc 'type '(-> Interval Interval Interval))
  (doc 'description "Widening operator for interval domain")
  (if (or (interval-bottom? prev) (interval-bottom? curr))
      curr
      (let ([lo (if (< (interval-lo curr) (interval-lo prev))
                    -inf.0
                    (interval-lo prev))]
            [hi (if (> (interval-hi curr) (interval-hi prev))
                    +inf.0
                    (interval-hi prev))])
        (make-interval lo hi))))

;;; sign-widen : Sign × Sign → Sign
;;; Widening for signs is just join (finite domain, always terminates).
(define (sign-widen prev curr)
  (doc 'export #t)
  (sign-join prev curr))

;;; ============================================================================
;;; Display
;;; ============================================================================

(display "Domains: sign-domain, interval-domain, type-domain, reach-domain\n")
(display "Galois connections: galois-signs, galois-intervals, galois-types, galois-reach\n")
