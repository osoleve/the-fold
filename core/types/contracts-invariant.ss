;;; @module contracts-invariant
;;; @description Invariant contracts — structural invariant checking with state tracking.
;;; Loaded by contracts.ss — requires contract core, blame, wrap, and checking modes.

(doc 'section 'invariant-contracts)

;;; Invariant Contracts
;;;
;;; Invariants are contracts that ensure a property holds on data structures.
;;; Unlike simple flat contracts that check once, invariants are designed to:
;;;
;;;   1. Check at wrap time that the invariant holds
;;;   2. Provide machinery for functions to "preserve" the invariant
;;;   3. Support composed invariants (multiple properties)
;;;
;;; Example usage:
;;;   (define sorted-list/c (invariant/c list? sorted?))
;;;   (define positive-point/c (invariant/c point? (λ (p) (and (> (point-x p) 0) (> (point-y p) 0)))))
;;;
;;; For functions that maintain invariants:
;;;   (define insert-sorted/c
;;;     (maintains-invariant sorted-list/c
;;;       (->c (list int/c sorted-list/c) sorted-list/c)))

(define (invariant/c base-predicate invariant-predicate)
  (doc 'type (-> (-> Any Boolean) (-> Any Boolean) Contract))
  (doc 'description "Create an invariant contract.
An invariant combines a base type check with a property that must hold.
The base-predicate checks the structural type (e.g., list?, pair?).
The invariant-predicate checks the semantic property (e.g., sorted?, balanced?).")
  (doc 'export #t)
  `(Invariant ,base-predicate ,invariant-predicate))

(define (invariant-contract? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is an invariant contract.")
  (doc 'export #t)
  (and (pair? c) (eq? (car c) 'Invariant)))

(define (invariant-base-predicate c)
  (doc 'type (-> Contract (-> Any Boolean)))
  (doc 'description "Extract the base predicate from an invariant contract.")
  (doc 'export #t)
  (if (invariant-contract? c)
      (cadr c)
      (lambda (x) #t)))

(define (invariant-predicate c)
  (doc 'type (-> Contract (-> Any Boolean)))
  (doc 'description "Extract the invariant predicate from an invariant contract.")
  (doc 'export #t)
  (if (invariant-contract? c)
      (caddr c)
      (lambda (x) #t)))

(define (check-invariant contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Check an invariant contract against a value.")
  (doc 'export #t)
  (if (not (invariant-contract? contract))
      `(Ok ,value)
      (let ([base-pred (invariant-base-predicate contract)]
            [inv-pred (invariant-predicate contract)])
        (cond
         ;; First check base type
         [(not (base-pred value))
          `(Err ,(make-blame 'callee location
                             "Invariant base type check failed"
                             value))]
         ;; Then check invariant property
         [(not (inv-pred value))
          `(Err ,(make-blame 'callee location
                             "Invariant property violated"
                             value))]
         ;; Both passed
         [else `(Ok ,value)]))))

(define (apply-invariant contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Apply an invariant contract, raising on violation.")
  (doc 'export #t)
  (let ([result (check-invariant contract value location)])
    (if (eq? (car result) 'Err)
        (raise-contract-violation/blame! (cadr result))
        (cadr result))))

;;; Invariant Combinators

(define (and-invariant/c . invariants)
  (doc 'type (-> Contract ... Contract))
  (doc 'description "Combine multiple invariant contracts. All must be satisfied.
Note: Both base predicates AND invariant predicates are conjuncted. This means the
value must satisfy ALL base types simultaneously. Use for compatible types like
sorted-list/c AND unique-list/c (both have list? base). Incompatible base types
(e.g., list? AND vector?) will always fail.")
  (doc 'export #t)
  (if (null? invariants)
      any/c
      ;; Combine base predicates and invariant predicates
      (let ([base-preds (map invariant-base-predicate invariants)]
            [inv-preds (map invariant-predicate invariants)])
        (invariant/c
         (lambda (x) (andmap (lambda (p) (p x)) base-preds))
         (lambda (x) (andmap (lambda (p) (p x)) inv-preds))))))

(define (or-invariant/c . invariants)
  (doc 'type (-> Contract ... Contract))
  (doc 'description "Combine invariant contracts with disjunction. At least one must be satisfied.")
  (doc 'export #t)
  (if (null? invariants)
      none/c
      (let ([checks (map (lambda (inv)
                           (cons (invariant-base-predicate inv)
                                 (invariant-predicate inv)))
                         invariants)])
        (invariant/c
         (lambda (x) (ormap (lambda (c) ((car c) x)) checks))
         (lambda (x) (ormap (lambda (c) (and ((car c) x) ((cdr c) x))) checks))))))

;;; Common Invariant Predicates (named for introspection)

(define sorted?
  (lambda (lst)
    (or (null? lst)
        (null? (cdr lst))
        (let loop ([prev (car lst)] [rest (cdr lst)])
          (cond
           [(null? rest) #t]
           [(not (and (real? prev) (real? (car rest)))) #f]
           [(<= prev (car rest))
            (loop (car rest) (cdr rest))]
           [else #f])))))

(define strictly-sorted?
  (lambda (lst)
    (or (null? lst)
        (null? (cdr lst))
        (let loop ([prev (car lst)] [rest (cdr lst)])
          (cond
           [(null? rest) #t]
           [(not (and (real? prev) (real? (car rest)))) #f]
           [(< prev (car rest))
            (loop (car rest) (cdr rest))]
           [else #f])))))

(define non-empty?
  (lambda (lst) (not (null? lst))))

(define unique-elements?
  (lambda (lst)
    (let loop ([seen '()] [rest lst])
      (cond
       [(null? rest) #t]
       [(memq (car rest) seen) #f]
       [else (loop (cons (car rest) seen) (cdr rest))]))))

;;; Common Invariants

(doc sorted-list/c 'type 'Contract)
(doc sorted-list/c 'description "Invariant contract for sorted lists (non-decreasing order).
Elements must satisfy: a <= b <= c <= ... Use strictly-sorted-list/c for strict ordering.")
(doc sorted-list/c 'export #t)
(define sorted-list/c (invariant/c list? sorted?))

(doc strictly-sorted-list/c 'type 'Contract)
(doc strictly-sorted-list/c 'description "Invariant contract for strictly sorted lists (strictly increasing).
Elements must satisfy: a < b < c < ... No duplicates allowed.")
(doc strictly-sorted-list/c 'export #t)
(define strictly-sorted-list/c (invariant/c list? strictly-sorted?))

(doc non-empty-list/c 'type 'Contract)
(doc non-empty-list/c 'description "Invariant contract for non-empty lists.")
(doc non-empty-list/c 'export #t)
(define non-empty-list/c (invariant/c list? non-empty?))

(doc unique-list/c 'type 'Contract)
(doc unique-list/c 'description "Invariant contract for lists with unique elements.
Uses eq? for comparison via memq. O(n²) complexity - suitable for small lists.")
(doc unique-list/c 'export #t)
(define unique-list/c (invariant/c list? unique-elements?))

;;; Invariant-Preserving Function Contracts

(define (maintains-invariant invariant fn-contract)
  (doc 'type (-> Contract Contract Contract))
  (doc 'description "Wrap a function contract to ensure it maintains an invariant.
The invariant is checked on all arguments and results that match the invariant's base type.
Use this for functions that transform data while preserving a property.")
  (doc 'export #t)
  (if (not (function-contract? fn-contract))
      (error 'maintains-invariant "Expected a function contract")
      (if (not (invariant-contract? invariant))
          fn-contract  ; No invariant to maintain
          (let* ([domain-cs (function-contract-domain fn-contract)]
                 [range-c (function-contract-range fn-contract)]
                 [base-pred (invariant-base-predicate invariant)]
                 [inv-pred (invariant-predicate invariant)]
                 ;; Create a conditional invariant check: only check if base type matches
                 [conditional-inv-check
                  (flat (lambda (x)
                          (if (base-pred x)
                              (inv-pred x)  ; Check invariant if it's the right type
                              #t)))]        ; Skip check for non-matching types
                 ;; Wrap domain contracts: add conditional invariant check
                 [wrapped-domains
                  (map (lambda (dc)
                         (if (or (eq? dc any/c)
                                 (flat-contract? dc)
                                 (invariant-contract? dc))
                             (and/c dc conditional-inv-check)
                             dc))
                       domain-cs)]
                 ;; Wrap range contract: add conditional invariant check
                 [wrapped-range (and/c range-c conditional-inv-check)])
            (->c wrapped-domains wrapped-range)))))

(define (invariant->flat-contract inv)
  (doc 'type (-> Contract Contract))
  (doc 'description "Convert an invariant contract to an equivalent flat contract.
Useful for using invariants in contexts that expect flat contracts.")
  (doc 'export #t)
  (if (not (invariant-contract? inv))
      (error 'invariant->flat-contract "Expected an invariant contract")
      (flat (lambda (x)
              (and ((invariant-base-predicate inv) x)
                   ((invariant-predicate inv) x))))))

;;; Display

(define (predicate-display-name proc fallback)
  (doc 'type (-> Procedure String String))
  (doc 'description "Extract a displayable name from a procedure, falling back to the given string.")
  (let ([name (#%$procedure-name proc)])
    (cond
     [(not name) fallback]
     [(string? name) name]
     [(symbol? name) (symbol->string name)]
     [else fallback])))

(define (invariant->string inv)
  (doc 'type (-> Contract String))
  (doc 'description "Convert an invariant contract to a human-readable string.
Introspects predicate names via Chez's procedure-name metadata when available.")
  (doc 'export #t)
  (if (invariant-contract? inv)
      (let ([base-str (predicate-display-name (invariant-base-predicate inv) "<base>")]
            [inv-str (predicate-display-name (invariant-predicate inv) "<property>")])
        (string-append "(invariant/c " base-str " " inv-str ")"))
      "???"))
