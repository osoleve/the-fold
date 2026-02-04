;;; core/types/contract-refine.ss — Integration between Contracts and Refinement Types
;;;
;;; This module bridges the contract system with the refinement type system:
;;;   - Convert refinement types to contracts
;;;   - Infer contracts from types
;;;   - Static verification of contracts against types
;;;   - Subtyping relationships between refined types and contracts

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/contracts.ss")

(doc 'module 'contract-refine)
(doc 'description "Integration between contracts and refinement types - static verification and contract inference.")
(doc 'layer 'core)

(doc 'section 'refinement-to-contract)

;;; Base type to predicate mapping
;;; Maps type symbols to their runtime predicates
(define *base-type-predicates*
  `((Nat . ,(lambda (x) (and (integer? x) (>= x 0))))
    (Int . ,integer?)
    (Float . ,flonum?)
    (Number . ,number?)
    (Real . ,real?)
    (Bool . ,boolean?)
    (String . ,string?)
    (Symbol . ,symbol?)
    (List . ,list?)
    (Vector . ,vector?)
    (Pair . ,pair?)
    (Procedure . ,procedure?)
    (Unit . ,(lambda (x) (eq? x '())))
    (Void . ,(lambda (x) (eq? x (void))))))

(define (base-type->predicate type)
  (doc 'type (-> Symbol (Or Procedure #f)))
  (doc 'description "Get the runtime predicate for a base type.")
  (doc 'export #t)
  (let ([entry (assq type *base-type-predicates*)])
    (if entry (cdr entry) #f)))

(define (refinement->contract refine-type)
  (doc 'type (-> Type Contract))
  (doc 'description "Convert a refinement type to a flat contract.
A refinement type (refine ((x : T)) P) becomes a flat contract that:
1. Checks the base type T
2. Binds x to the value
3. Evaluates predicate P")
  (doc 'export #t)
  (if (not (refinement-type? refine-type))
      (error 'refinement->contract "Expected refinement type" refine-type)
      (let* ([var (refinement-var refine-type)]
             [base-type (refinement-base-type refine-type)]
             [predicate-expr (refinement-predicate refine-type)]
             [base-pred (type->predicate base-type)])
        (flat
         (lambda (value)
           ;; First check base type
           (and (base-pred value)
                ;; Then check refinement predicate
                (eval-predicate-with-binding var value predicate-expr)))))))

(define (type->predicate type)
  (doc 'type (-> Type (-> Any Boolean)))
  (doc 'description "Convert any type to a runtime predicate.")
  (doc 'export #t)
  (cond
   ;; Base types
   [(symbol? type)
    (let ([pred (base-type->predicate type)])
      (if pred pred (lambda (x) #t)))]  ; Unknown types pass trivially
   ;; List types: (List T)
   [(and (pair? type) (eq? (car type) 'List))
    (let ([elem-pred (type->predicate (cadr type))])
      (lambda (x) (and (list? x) (andmap elem-pred x))))]
   ;; Vector types: (Vector T) or (Vec n T)
   [(and (pair? type) (memq (car type) '(Vector Vec)))
    (let ([type-len (length type)])
      (if (< type-len 2)
          (lambda (x) (vector? x))  ; Bare Vector/Vec - just check vector?
          (let* ([has-dim (= type-len 3)]
                 [elem-type (if has-dim (caddr type) (cadr type))]
                 [elem-pred (type->predicate elem-type)])
            (lambda (x)
              (and (vector? x)
                   (let ([vec-len (vector-length x)])
                     (let loop ([i 0])
                       (if (>= i vec-len)
                           #t
                           (and (elem-pred (vector-ref x i))
                                (loop (+ i 1)))))))))))]
   ;; Pair types: (Pair A B) or (× A B)
   [(and (pair? type) (memq (car type) '(Pair ×)))
    (let ([fst-pred (type->predicate (cadr type))]
          [snd-pred (type->predicate (caddr type))])
      (lambda (x) (and (pair? x) (fst-pred (car x)) (snd-pred (cdr x)))))]
   ;; Function types: (-> A B) - can only check procedure?
   [(and (pair? type) (eq? (car type) '->))
    procedure?]
   ;; Pi types - also just procedure?
   [(pi-type? type)
    procedure?]
   ;; Refinement types - recursive conversion
   [(refinement-type? type)
    (let ([contract (refinement->contract type)])
      (flat-predicate contract))]
   ;; Default: accept anything
   [else (lambda (x) #t)]))

(doc 'section 'predicate-evaluation)

;;; Predicate Evaluation
;;;
;;; Evaluates simple predicate expressions with variable bindings.
;;; Supports: comparisons, boolean ops, arithmetic, type checks
;;;
;;; This is intentionally limited to keep evaluation tractable.
;;; Complex predicates should be wrapped in procedures.

(define (eval-predicate-with-binding var value predicate-expr)
  (doc 'type (-> Symbol Any SExpr Boolean))
  (doc 'description "Evaluate a predicate expression with var bound to value.")
  (doc 'export #f)
  (eval-predicate `((,var . ,value)) predicate-expr))

(define (eval-predicate env expr)
  (doc 'type (-> Alist SExpr Boolean))
  (doc 'description "Evaluate a predicate in an environment.")
  (doc 'export #f)
  (cond
   ;; Literals
   [(eq? expr #t) #t]
   [(eq? expr #f) #f]
   [(number? expr) expr]
   [(string? expr) expr]
   ;; Variable lookup
   [(symbol? expr)
    (let ([binding (assq expr env)])
      (if binding
          (cdr binding)
          (error 'eval-predicate "Unbound variable" expr)))]
   ;; Not a list - error
   [(not (pair? expr))
    (error 'eval-predicate "Invalid predicate expression" expr)]
   ;; Comparisons
   [(eq? (car expr) '>)
    (> (eval-predicate env (cadr expr))
       (eval-predicate env (caddr expr)))]
   [(eq? (car expr) '<)
    (< (eval-predicate env (cadr expr))
       (eval-predicate env (caddr expr)))]
   [(eq? (car expr) '>=)
    (>= (eval-predicate env (cadr expr))
        (eval-predicate env (caddr expr)))]
   [(eq? (car expr) '<=)
    (<= (eval-predicate env (cadr expr))
        (eval-predicate env (caddr expr)))]
   [(eq? (car expr) '=)
    (= (eval-predicate env (cadr expr))
       (eval-predicate env (caddr expr)))]
   [(eq? (car expr) 'eq?)
    (eq? (eval-predicate env (cadr expr))
         (eval-predicate env (caddr expr)))]
   [(eq? (car expr) 'equal?)
    (equal? (eval-predicate env (cadr expr))
            (eval-predicate env (caddr expr)))]
   ;; Boolean operations
   [(eq? (car expr) 'and)
    (andmap (lambda (e) (eval-predicate env e)) (cdr expr))]
   [(eq? (car expr) 'or)
    (ormap (lambda (e) (eval-predicate env e)) (cdr expr))]
   [(eq? (car expr) 'not)
    (not (eval-predicate env (cadr expr)))]
   ;; Arithmetic
   [(eq? (car expr) '+)
    (apply + (map (lambda (e) (eval-predicate env e)) (cdr expr)))]
   [(eq? (car expr) '-)
    (apply - (map (lambda (e) (eval-predicate env e)) (cdr expr)))]
   [(eq? (car expr) '*)
    (apply * (map (lambda (e) (eval-predicate env e)) (cdr expr)))]
   [(eq? (car expr) '/)
    (apply / (map (lambda (e) (eval-predicate env e)) (cdr expr)))]
   [(eq? (car expr) 'modulo)
    (modulo (eval-predicate env (cadr expr))
            (eval-predicate env (caddr expr)))]
   [(eq? (car expr) 'remainder)
    (remainder (eval-predicate env (cadr expr))
               (eval-predicate env (caddr expr)))]
   ;; Type predicates
   [(eq? (car expr) 'integer?)
    (integer? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'number?)
    (number? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'positive?)
    (positive? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'negative?)
    (negative? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'zero?)
    (zero? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'even?)
    (even? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'odd?)
    (odd? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'null?)
    (null? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'pair?)
    (pair? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'list?)
    (list? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'string?)
    (string? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'symbol?)
    (symbol? (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'procedure?)
    (procedure? (eval-predicate env (cadr expr)))]
   ;; List operations
   [(eq? (car expr) 'length)
    (length (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'car)
    (car (eval-predicate env (cadr expr)))]
   [(eq? (car expr) 'cdr)
    (cdr (eval-predicate env (cadr expr)))]
   ;; Conditional
   [(eq? (car expr) 'if)
    (if (eval-predicate env (cadr expr))
        (eval-predicate env (caddr expr))
        (eval-predicate env (cadddr expr)))]
   ;; Let binding
   [(eq? (car expr) 'let)
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [new-env (fold-left
                     (lambda (acc b)
                       (cons (cons (car b)
                                   (eval-predicate acc (cadr b)))
                             acc))
                     env
                     bindings)])
      (eval-predicate new-env body))]
   ;; Unknown - return #f for safety
   [else
    (error 'eval-predicate "Unsupported predicate form" (car expr))]))

(doc 'section 'contract-inference)

;;; Contract Inference from Types
;;;
;;; Infers the most precise contract that can be derived from a type.

(define (type->contract type)
  (doc 'type (-> Type Contract))
  (doc 'description "Infer a contract from a type. Returns the most precise contract
that can be derived from the type structure.")
  (doc 'export #t)
  (cond
   ;; Base types -> flat contracts with type predicates
   [(symbol? type)
    (let ([pred (base-type->predicate type)])
      (if pred
          (flat pred)
          any/c))]
   ;; Refinement types -> flat contracts with combined predicates
   [(refinement-type? type)
    (refinement->contract type)]
   ;; List types -> listof contracts
   [(and (pair? type) (eq? (car type) 'List))
    (let ([elem-contract (type->contract (cadr type))])
      (if (flat-contract? elem-contract)
          (listof elem-contract)
          (listof/c elem-contract)))]
   ;; Vec types -> listof with length
   [(vec-type? type)
    (let* ([len-expr (cadr type)]
           [elem-type (caddr type)]
           [elem-contract (type->contract elem-type)])
      ;; If length is known, create dependent contract
      (if (integer? len-expr)
          (and/c (listof elem-contract)
                 (flat (lambda (lst) (= (length lst) len-expr))))
          (listof elem-contract)))]
   ;; Function types: (-> A B)
   [(and (pair? type) (eq? (car type) '->))
    (let* ([args (butlast (cdr type))]
           [result (last (cdr type))]
           [domain-contracts (map type->contract args)]
           [range-contract (type->contract result)])
      (->c domain-contracts range-contract))]
   ;; Pi types: (Π ((x : A)) B)
   [(pi-type? type)
    (let* ([bindings (pi-bindings type)]
           [body (pi-codomain type)]
           [vars (map car bindings)]
           [domain-contracts (map (lambda (b) (type->contract (cdr b))) bindings)]
           [range-contract (type->contract body)])
      ;; If body references bound vars, create dependent contract
      (if (any-free-in? vars body)
          (dep vars (lambda args
                      (let ([range (apply-type-subst body vars args)])
                        (type->contract range))))
          (->c domain-contracts range-contract)))]
   ;; Product types: (× A B)
   [(and (pair? type) (eq? (car type) '×))
    (let ([fst-c (type->contract (cadr type))]
          [snd-c (type->contract (caddr type))])
      (and/c (flat pair?)
             (flat (lambda (p)
                     (and (eq? 'Ok (car (check-flat fst-c (car p) 'pair-fst)))
                          (eq? 'Ok (car (check-flat snd-c (cdr p) 'pair-snd))))))))]
   ;; Sigma types - similar to products but with dependency
   [(sigma-type? type)
    (let* ([var (sigma-var type)]
           [fst-type (sigma-fst-type type)]
           [snd-type (sigma-snd-type type)]
           [fst-c (type->contract fst-type)])
      (flat (lambda (p)
              (and (pair? p)
                   (eq? 'Ok (car (check-flat fst-c (car p) 'sigma-fst)))
                   (let ([snd-c (type->contract
                                 (dep-subst-type snd-type var (car p)))])
                     (eq? 'Ok (car (check-flat snd-c (cdr p) 'sigma-snd))))))))]
   ;; Default - any contract
   [else any/c]))

;;; Helpers

(define (any-free-in? vars expr)
  (doc 'type (-> (List Symbol) SExpr Boolean))
  (doc 'description "Check if any of vars appears free in expr.")
  (doc 'export #f)
  (let ([free (dep-free-vars expr)])
    (ormap (lambda (v) (memq v free)) vars)))

(define (apply-type-subst type vars values)
  (doc 'type (-> Type (List Symbol) (List Any) Type))
  (doc 'description "Substitute multiple vars with values in type.")
  (doc 'export #f)
  (fold-left (lambda (t pair)
               (dep-subst-type t (car pair) (cdr pair)))
             type
             (map cons vars values)))

(define (butlast lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (butlast (cdr lst)))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(doc 'section 'static-verification)

;;; Static Contract Verification
;;;
;;; Attempts to prove that a contract is satisfied by all values of a type.
;;; Returns (Ok ()) if proven, (Err reason) otherwise.

(define (verify-contract-for-type contract type)
  (doc 'type (-> Contract Type (Result Unit String)))
  (doc 'description "Verify that all values of type satisfy the contract.
Returns (Ok ()) if proven, (Err reason) if not provable.")
  (doc 'export #t)
  (cond
   ;; Any contract is always satisfied
   [(eq? contract 'Any)
    '(Ok ())]
   ;; None contract is never satisfied (unless type is uninhabited)
   [(eq? contract 'None)
    '(Err "None contract cannot be satisfied")]
   ;; Flat contracts
   [(flat-contract? contract)
    (verify-flat-contract-for-type contract type)]
   ;; Function contracts
   [(function-contract? contract)
    (verify-function-contract-for-type contract type)]
   ;; Dependent contracts
   [(dependent-contract? contract)
    (verify-dependent-contract-for-type contract type)]
   ;; And contracts - all must be satisfied
   [(and (pair? contract) (eq? (car contract) 'And))
    (let loop ([cs (cdr contract)])
      (if (null? cs)
          '(Ok ())
          (let ([result (verify-contract-for-type (car cs) type)])
            (if (eq? (car result) 'Ok)
                (loop (cdr cs))
                result))))]
   ;; Or contracts - at least one must be satisfied
   [(and (pair? contract) (eq? (car contract) 'Or))
    (let loop ([cs (cdr contract)])
      (if (null? cs)
          '(Err "No contract in disjunction proven to hold")
          (let ([result (verify-contract-for-type (car cs) type)])
            (if (eq? (car result) 'Ok)
                result
                (loop (cdr cs))))))]
   ;; Unknown contract form
   [else
    '(Err "Unknown contract form for static verification")]))

(define (verify-flat-contract-for-type contract type)
  (doc 'type (-> Contract Type (Result Unit String)))
  (doc 'description "Verify a flat contract against a type.")
  (doc 'export #f)
  ;; For flat contracts, we try to prove the predicate from type info
  ;; This is necessarily incomplete - we handle common cases
  (let ([pred (flat-predicate contract)])
    (cond
     ;; If type is a refinement, check if refinement implies contract
     [(refinement-type? type)
      (let* ([base-type (refinement-base-type type)]
             [type-pred (refinement-predicate type)])
        ;; First verify contract holds for base type
        (let ([base-result (verify-flat-contract-for-type contract base-type)])
          (if (eq? (car base-result) 'Ok)
              base-result
              ;; Try to prove type predicate implies contract predicate
              ;; This would require a theorem prover - for now, be conservative
              '(Err "Cannot statically verify refinement implies contract"))))]
     ;; Base type checks
     [(and (symbol? type) (base-type->predicate type))
      (let ([type-pred (base-type->predicate type)])
        ;; Check if type predicate is stronger than contract predicate
        ;; This is a heuristic - we check if they're the same predicate
        (if (eq? pred type-pred)
            '(Ok ())
            '(Err "Cannot prove type predicate implies contract predicate")))]
     [else
      '(Err "Cannot statically verify flat contract for this type")])))

(define (verify-function-contract-for-type contract type)
  (doc 'type (-> Contract Type (Result Unit String)))
  (doc 'description "Verify a function contract against a type.")
  (doc 'export #f)
  (cond
   ;; Type must be a function type
   [(and (pair? type) (eq? (car type) '->))
    (let* ([contract-domains (function-contract-domain contract)]
           [contract-range (function-contract-range contract)]
           [type-args (butlast (cdr type))]
           [type-result (last (cdr type))])
      ;; Check arity matches
      (if (not (= (length contract-domains) (length type-args)))
          '(Err "Function arity mismatch")
          ;; Check contravariance on domains, covariance on range
          (let ([domain-checks
                 (map (lambda (c t)
                        ;; Domain: type must be subtype of contract
                        ;; (contract accepts what type provides)
                        (verify-contract-for-type c t))
                      contract-domains type-args)]
                [range-check (verify-contract-for-type contract-range type-result)])
            (if (andmap (lambda (r) (eq? (car r) 'Ok)) domain-checks)
                range-check
                '(Err "Domain contract verification failed")))))]
   [(pi-type? type)
    (let* ([bindings (pi-bindings type)]
           [body (pi-codomain type)]
           [contract-domains (function-contract-domain contract)]
           [contract-range (function-contract-range contract)])
      (if (not (= (length contract-domains) (length bindings)))
          '(Err "Function arity mismatch with Pi type")
          ;; Similar check but with bindings
          (let ([domain-checks
                 (map (lambda (c b)
                        (verify-contract-for-type c (cdr b)))
                      contract-domains bindings)]
                [range-check (verify-contract-for-type contract-range body)])
            (if (andmap (lambda (r) (eq? (car r) 'Ok)) domain-checks)
                range-check
                '(Err "Domain contract verification failed for Pi type")))))]
   [else
    '(Err "Expected function type for function contract")]))

(define (verify-dependent-contract-for-type contract type)
  (doc 'type (-> Contract Type (Result Unit String)))
  (doc 'description "Verify a dependent contract against a type.")
  (doc 'export #f)
  ;; Dependent contracts are hard to verify statically
  ;; We'd need to reason about the contract factory
  '(Err "Dependent contracts require runtime checking"))

(doc 'section 'verifier-integration)

;;; Install the Static Verifier
;;;
;;; This registers our verifier with the contract system's compile-time hooks.

(define (install-refinement-verifier!)
  (doc 'type (-> Void))
  (doc 'description "Install the refinement-aware contract verifier.")
  (doc 'export #t)
  (set-compile-time-verifier!
   (lambda (contract type)
     (verify-contract-for-type contract type))))

(doc 'section 'contract-subtyping)

;;; Contract Subtyping
;;;
;;; Contract C1 is a subtype of C2 if every value satisfying C1 also satisfies C2.
;;; For refinement types: {x:T | P} <: {x:T | Q} when P ⟹ Q

(define (contract-subtype? c1 c2)
  (doc 'type (-> Contract Contract Boolean))
  (doc 'description "Check if contract c1 is a subtype of c2.
Returns #t if every value satisfying c1 also satisfies c2.")
  (doc 'export #t)
  (cond
   ;; Any is top - everything is subtype of Any
   [(eq? c2 'Any) #t]
   ;; None is bottom - only None is subtype of None
   [(eq? c2 'None) (eq? c1 'None)]
   ;; None is subtype of everything
   [(eq? c1 'None) #t]
   ;; Any is only subtype of Any
   [(eq? c1 'Any) (eq? c2 'Any)]
   ;; Same contract
   [(equal? c1 c2) #t]
   ;; And contracts: c1 <: (And cs...) if c1 <: each c
   [(and (pair? c2) (eq? (car c2) 'And))
    (andmap (lambda (c) (contract-subtype? c1 c)) (cdr c2))]
   ;; (And cs...) <: c2 if any c <: c2
   [(and (pair? c1) (eq? (car c1) 'And))
    (ormap (lambda (c) (contract-subtype? c c2)) (cdr c1))]
   ;; Or contracts: c1 <: (Or cs...) if c1 <: some c
   [(and (pair? c2) (eq? (car c2) 'Or))
    (ormap (lambda (c) (contract-subtype? c1 c)) (cdr c2))]
   ;; (Or cs...) <: c2 if each c <: c2
   [(and (pair? c1) (eq? (car c1) 'Or))
    (andmap (lambda (c) (contract-subtype? c c2)) (cdr c1))]
   ;; Function contracts: contravariant domains, covariant range
   [(and (function-contract? c1) (function-contract? c2))
    (let ([d1 (function-contract-domain c1)]
          [r1 (function-contract-range c1)]
          [d2 (function-contract-domain c2)]
          [r2 (function-contract-range c2)])
      (and (= (length d1) (length d2))
           ;; Contravariant domains: c2 domains <: c1 domains
           (andmap contract-subtype? d2 d1)
           ;; Covariant range: c1 range <: c2 range
           (contract-subtype? r1 r2)))]
   ;; For flat contracts, we'd need to prove one predicate implies the other
   ;; This is undecidable in general - return #f conservatively
   [else #f]))

(doc 'section 'refinement-subtyping)

;;; Refinement Type Subtyping
;;;
;;; {x:T | P} <: {x:T | Q} when ∀x:T. P(x) ⟹ Q(x)
;;; {x:T | P} <: T (forgetting refinement)

(define (refinement-subtype? t1 t2)
  (doc 'type (-> Type Type Boolean))
  (doc 'description "Check refinement subtyping: t1 <: t2.")
  (doc 'export #t)
  (cond
   ;; Same type
   [(equal? t1 t2) #t]
   ;; {x:T | P} <: T (forgetting refinement)
   [(and (refinement-type? t1) (not (refinement-type? t2)))
    (equal? (refinement-base-type t1) t2)]
   ;; {x:T | P} <: {x:T | Q} when P implies Q
   ;; We can't prove this in general, but check syntactic equality
   [(and (refinement-type? t1) (refinement-type? t2))
    (and (equal? (refinement-base-type t1) (refinement-base-type t2))
         ;; Check if predicates are equal (modulo alpha)
         (equal? (refinement-predicate t1) (refinement-predicate t2)))]
   ;; T <: {x:T | #t} (trivial refinement)
   [(and (not (refinement-type? t1)) (refinement-type? t2))
    (and (equal? t1 (refinement-base-type t2))
         (eq? #t (refinement-predicate t2)))]
   [else #f]))

(doc 'section 'combined-apply)

;;; Combined Contract Application with Static Optimization
;;;
;;; If static verification succeeds, skip runtime check.
;;; Otherwise, perform runtime check.

(define (apply-contract/smart contract value type location)
  (doc 'type (-> Contract Any Type Symbol Any))
  (doc 'description "Apply contract with static optimization.
If the contract can be statically verified for the type, skip runtime check.
Otherwise, perform runtime check.")
  (doc 'export #t)
  ;; Try static verification first (if in compile-time mode)
  (if (eq? (get-contract-mode) 'compile-time)
      (let ([static-result (verify-contract-for-type contract type)])
        (if (eq? (car static-result) 'Ok)
            value  ; Statically verified - no runtime check needed
            (apply-contract contract value location)))
      ;; Runtime mode - always check
      (apply-contract/mode contract value location)))

(doc 'section 'convenience-forms)

;;; Convenience Macros/Forms

(define (define/contract name contract body)
  (doc 'type (-> Symbol Contract Any Wrapped))
  (doc 'description "Define a value with a contract attached.
Returns wrapped value that checks contract on use.")
  (doc 'export #t)
  (let ([result (contract-wrap contract body name)])
    (if (eq? (car result) 'Err)
        (error 'define/contract (blame->string (cadr result)))
        (cadr result))))

(define (lambda/contract params domain-contracts range-contract body-proc)
  (doc 'type (-> (List Symbol) (List Contract) Contract Procedure Procedure))
  (doc 'description "Create a contracted function.
Wraps body-proc with domain and range contracts.")
  (doc 'export #t)
  (let ([fn-contract (->c domain-contracts range-contract)])
    (wrap-function fn-contract body-proc 'lambda/contract 'caller)))

(define (with-refinement type value location)
  (doc 'type (-> Type Any Symbol Any))
  (doc 'description "Check that value satisfies a refinement type.
Equivalent to (apply-contract (refinement->contract type) value location).")
  (doc 'export #t)
  (if (not (refinement-type? type))
      value  ; Non-refinement types pass through
      (let ([contract (refinement->contract type)])
        (apply-contract contract value location))))
