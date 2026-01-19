;;; core/types/contracts.ss — Contract System
;;; @module contracts
;;; @requires prelude types
;;;
;;; Contracts are first-class specifications that can be attached to values.
;;; They provide runtime checking with blame tracking.
;;;
;;; Contract Types:
;;;   - Flat contracts: Simple predicates
;;;   - Function contracts: Pre/post conditions
;;;   - Dependent contracts: Contracts that depend on values
;;;   - Blame tracking: Identify contract violators
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ====
;;; Contract Grammar
;;; ====
;;;
;;;   Contract ::= (Flat Predicate)                   ; Flat contract
;;;              | (-> (Contract ...) Contract)       ; Function contract
;;;              | (Dep (Var ...) Contract)           ; Dependent contract
;;;              | (And Contract ...)                 ; Conjunction
;;;              | (Or Contract ...)                  ; Disjunction
;;;              | (Not Contract)                     ; Negation
;;;              | Any                                ; Top contract (always satisfied)
;;;              | None                               ; Bottom contract (never satisfied)
;;;
;;;   Predicate ::= (lambda (x) body)                ; Scheme predicate
;;;
;;;   Blame ::= (blame Party Location Message)
;;;   Party ::= 'caller | 'callee
;;;   Location ::= symbol (function name or source location)

;;; ====
;;; Contract Predicates
;;; ====

;;; contract? : Any → Boolean
;;; Is this a well-formed contract?
(define (contract? c)
  (cond
   ;; Simple contracts
   [(eq? c 'Any) #t]
   [(eq? c 'None) #t]
   [(not (pair? c)) #f]
   ;; Flat contract: (Flat pred)
   [(eq? (car c) 'Flat)
    (and (= (length c) 2)
         (procedure? (cadr c)))]
   ;; Function contract: (-> (c1 ...) cresult)
   [(eq? (car c) '->)
    (and (= (length c) 3)
         (list? (cadr c))
         (andmap contract? (cadr c))
         (contract? (caddr c)))]
   ;; Dependent contract: (Dep (vars...) body)
   [(eq? (car c) 'Dep)
    (and (= (length c) 3)
         (list? (cadr c))
         (andmap symbol? (cadr c))
         (contract? (caddr c)))]
   ;; Conjunction: (And c1 c2 ...)
   [(eq? (car c) 'And)
    (andmap contract? (cdr c))]
   ;; Disjunction: (Or c1 c2 ...)
   [(eq? (car c) 'Or)
    (andmap contract? (cdr c))]
   ;; Negation: (Not c)
   [(eq? (car c) 'Not)
    (and (= (length c) 2)
         (contract? (cadr c)))]
   [else #f]))

;;; flat-contract? : Any → Bool
(define (flat-contract? c)
  (and (pair? c) (eq? (car c) 'Flat)))

;;; function-contract? : Any → Bool
(define (function-contract? c)
  (and (pair? c) (eq? (car c) '->)))

;;; dependent-contract? : Any → Bool
(define (dependent-contract? c)
  (and (pair? c) (eq? (car c) 'Dep)))

;;; ====
;;; Contract Constructors
;;; ====

;;; flat : (-> Any Bool) → Any
;;; Create a flat contract from a predicate.
(define (flat pred)
  `(Flat ,pred))

;;; ->c : (List Any) → Any → Any
;;; Create a function contract.
(define (->c domain-contracts range-contract)
  `(-> ,domain-contracts ,range-contract))

;;; dep : (List Symbol) → Any → Any
;;; Create a dependent contract.
(define (dep vars body)
  `(Dep ,vars ,body))

;;; and/c : Any ... → Any
;;; Conjunction of contracts.
(define (and/c . contracts)
  (cons 'And contracts))

;;; or/c : Any ... → Any
;;; Disjunction of contracts.
(define (or/c . contracts)
  (cons 'Or contracts))

;;; not/c : Any → Any
;;; Negation of a contract.
(define (not/c contract)
  `(Not ,contract))

;;; any/c : Symbol
;;; Top contract - always satisfied.
(define any/c 'Any)

;;; none/c : Symbol
;;; Bottom contract - never satisfied.
(define none/c 'None)

;;; ====
;;; Common Flat Contracts
;;; ====

;;; Natural number contract
(define nat/c
  (flat (lambda (x) (and (integer? x) (>= x 0)))))

;;; Positive number contract
(define pos/c
  (flat (lambda (x) (and (number? x) (> x 0)))))

;;; Non-negative number contract
(define non-neg/c
  (flat (lambda (x) (and (number? x) (>= x 0)))))

;;; Integer contract
(define int/c
  (flat integer?))

;;; Boolean contract
(define bool/c
  (flat boolean?))

;;; String contract
(define string/c
  (flat string?))

;;; Symbol contract
(define symbol/c
  (flat symbol?))

;;; List contract (any element type)
(define list/c
  (flat list?))

;;; Vector contract (any element type)
(define vector/c
  (flat vector?))

;;; Procedure contract (any arity)
(define procedure/c
  (flat procedure?))

;;; ====
;;; Contract Accessors
;;; ====

;;; flat-predicate : Any → (-> Any Bool)
(define (flat-predicate c)
  (if (flat-contract? c)
      (cadr c)
      (lambda (x) #f)))

;;; function-contract-domain : Any → (List Any)
(define (function-contract-domain c)
  (if (function-contract? c)
      (cadr c)
      '()))

;;; function-contract-range : Any → Any
(define (function-contract-range c)
  (if (function-contract? c)
      (caddr c)
      none/c))

;;; dependent-contract-vars : Any → (List Symbol)
(define (dependent-contract-vars c)
  (if (dependent-contract? c)
      (cadr c)
      '()))

;;; dependent-contract-body : Any → Any
(define (dependent-contract-body c)
  (if (dependent-contract? c)
      (caddr c)
      none/c))

;;; ====
;;; Blame Tracking
;;; ====

;;; A blame record tracks who violated a contract.
;;; Structure: (blame party location message value)
;;;
;;;   party    : 'caller | 'callee
;;;   location : symbol (function name or source location)
;;;   message  : string (human-readable description)
;;;   value    : the offending value

;;; make-blame : Symbol → Symbol → String → Any → Any
(define (make-blame party location message value)
  `(blame ,party ,location ,message ,value))

;;; blame? : Any → Bool
(define (blame? x)
  (and (pair? x)
       (eq? (car x) 'blame)
       (= (length x) 5)))

;;; blame-party : Any → Symbol
(define (blame-party b)
  (if (blame? b) (cadr b) #f))

;;; blame-location : Any → Symbol
(define (blame-location b)
  (if (blame? b) (caddr b) #f))

;;; blame-message : Any → String
(define (blame-message b)
  (if (blame? b) (cadddr b) ""))

;;; blame-value : Any → Any
(define (blame-value b)
  (if (blame? b) (car (cddddr b)) #f))

;;; flip-blame : Any → Any
;;; Swap caller and callee (used when traversing contract boundaries).
(define (flip-blame b)
  (if (blame? b)
      (make-blame (if (eq? (blame-party b) 'caller) 'callee 'caller)
                  (blame-location b)
                  (blame-message b)
                  (blame-value b))
      b))

;;; ====
;;; Contract Checking (Pure - No Side Effects)
;;; ====

;;; check-flat : Any → Any → Symbol → Any
;;; Check a flat contract against a value.
;;; Returns (Ok value) if satisfied, (Err blame) otherwise.
(define (check-flat contract value location)
  (cond
   [(eq? contract 'Any) `(Ok ,value)]
   [(eq? contract 'None)
    `(Err ,(make-blame 'callee location "Contract is None (unsatisfiable)" value))]
   [(flat-contract? contract)
    (let ([pred (flat-predicate contract)])
         (if (pred value)
             `(Ok ,value)
             `(Err ,(make-blame 'callee location
                                "Flat contract violated"
                                value))))]
   [(eq? (car contract) 'And)
    ;; Check all contracts in conjunction
    (check-and-contracts (cdr contract) value location)]
   [(eq? (car contract) 'Or)
    ;; Check if any contract in disjunction is satisfied
    (check-or-contracts (cdr contract) value location)]
   [(eq? (car contract) 'Not)
    ;; Negate the result
    (let ([result (check-flat (cadr contract) value location)])
         (if (eq? (car result) 'Ok)
             `(Err ,(make-blame 'callee location
                                "Negated contract satisfied (should fail)"
                                value))
             `(Ok ,value)))]
   [else
    `(Err ,(make-blame 'callee location
                       "Not a flat contract"
                       value))]))

;;; check-and-contracts : (List Any) → Any → Symbol → Any
(define (check-and-contracts contracts value location)
  (if (null? contracts)
      `(Ok ,value)
      (let ([result (check-flat (car contracts) value location)])
           (if (eq? (car result) 'Ok)
               (check-and-contracts (cdr contracts) value location)
               result))))

;;; check-or-contracts : (List Any) → Any → Symbol → Any
(define (check-or-contracts contracts value location)
  (if (null? contracts)
      `(Err ,(make-blame 'callee location
                         "None of the contracts in disjunction satisfied"
                         value))
      (let ([result (check-flat (car contracts) value location)])
           (if (eq? (car result) 'Ok)
               result
               (check-or-contracts (cdr contracts) value location)))))

;;; ====
;;; Contract Wrapping
;;; ====

;;; A wrapped value carries its contract and blame information.
;;; Structure: (wrapped contract value location)
;;;
;;; This is a pure representation - actual enforcement would happen
;;; in the boundary layer during evaluation.

;;; wrap : Any → Any → Symbol → Any
(define (wrap contract value location)
  `(wrapped ,contract ,value ,location))

;;; wrapped? : Any → Bool
(define (wrapped? x)
  (and (pair? x)
       (eq? (car x) 'wrapped)
       (= (length x) 4)))

;;; unwrap : Any → Any
(define (unwrap w)
  (if (wrapped? w) (caddr w) w))

;;; wrapped-contract : Any → Any
(define (wrapped-contract w)
  (if (wrapped? w) (cadr w) any/c))

;;; wrapped-location : Any → Symbol
(define (wrapped-location w)
  (if (wrapped? w) (cadddr w) 'unknown))

;;; ====
;;; Contract Combinators
;;; ====

;;; listof : Any → Any
;;; Contract for a list of elements satisfying a contract.
(define (listof elem-contract)
  (flat (lambda (lst)
                (and (list? lst)
                     (andmap (lambda (x)
                                     (let ([result (check-flat elem-contract x 'listof)])
                                          (eq? (car result) 'Ok)))
                             lst)))))

;;; vectorof : Any → Any
;;; Contract for a vector of elements satisfying a contract.
(define (vectorof elem-contract)
  (flat (lambda (vec)
                (and (vector? vec)
                     (let ([len (vector-length vec)])
                          (let loop ([i 0])
                               (if (>= i len)
                                   #t
                                   (let ([result (check-flat elem-contract
                                                             (vector-ref vec i)
                                                             'vectorof)])
                                        (and (eq? (car result) 'Ok)
                                             (loop (+ i 1)))))))))))

;;; between/c : Int → Int → Any
;;; Contract for numbers in a range [low, high].
(define (between/c low high)
  (flat (lambda (x)
                (and (number? x)
                     (>= x low)
                     (<= x high)))))

;;; one-of/c : Any ... → Any
;;; Contract for values that are eq? to one of the given values.
(define (one-of/c . values)
  (flat (lambda (x)
                (if (memq x values) #t #f))))

;;; ====
;;; Contract Display
;;; ====

;;; contract->string : Any → String
(define (contract->string c)
  (cond
   [(eq? c 'Any) "any/c"]
   [(eq? c 'None) "none/c"]
   [(flat-contract? c) "(flat ...)"]
   [(function-contract? c)
    (string-append "(-> ("
                   (join-strings " " (map contract->string
                                          (function-contract-domain c)))
                   ") "
                   (contract->string (function-contract-range c))
                   ")")]
   [(dependent-contract? c)
    (string-append "(dep ("
                   (join-strings " " (map symbol->string
                                          (dependent-contract-vars c)))
                   ") "
                   (contract->string (dependent-contract-body c))
                   ")")]
   [(and (pair? c) (eq? (car c) 'And))
    (string-append "(and/c " (join-strings " " (map contract->string (cdr c))) ")")]
   [(and (pair? c) (eq? (car c) 'Or))
    (string-append "(or/c " (join-strings " " (map contract->string (cdr c))) ")")]
   [(and (pair? c) (eq? (car c) 'Not))
    (string-append "(not/c " (contract->string (cadr c)) ")")]
   [else "???"]))

;;; blame->string : Any → String
(define (blame->string b)
  (if (blame? b)
      (string-append "Contract violation\n"
                     "  Party: " (symbol->string (blame-party b)) "\n"
                     "  Location: " (symbol->string (blame-location b)) "\n"
                     "  Message: " (blame-message b) "\n"
                     "  Value: " (format "~s" (blame-value b)))
      "Invalid blame"))

;;; ====
;;; Higher-Order Contract Wrapping
;;; ====
;;;
;;; contract-wrap applies a contract to a value, producing a wrapped value
;;; that enforces the contract at runtime.
;;;
;;; For flat contracts: checks immediately
;;; For function contracts: returns a wrapper procedure
;;;
;;; Blame assignment for function contracts:
;;;   - Domain violations blame the CALLER (they passed bad arguments)
;;;   - Range violations blame the CALLEE (function returned bad result)
;;;   - Higher-order argument contracts get FLIPPED blame:
;;;     If caller passes a callback that violates its contract when called,
;;;     the callee is blamed (they misused the callback).

;;; contract-wrap : Contract → Any → Symbol → (Ok Any) | (Err Blame)
;;; Wrap a value with a contract. Returns wrapped/checked value or blame.
(define (contract-wrap contract value location)
  (cond
   ;; Flat contracts: check immediately
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract)
        (and (pair? contract) (memq (car contract) '(And Or Not))))
    (check-flat contract value location)]
   ;; Function contracts: wrap the procedure
   [(function-contract? contract)
    (if (procedure? value)
        `(Ok ,(wrap-function contract value location 'caller))
        `(Err ,(make-blame 'callee location
                           "Expected a procedure for function contract"
                           value)))]
   ;; Dependent contracts: not yet supported for wrapping
   [(dependent-contract? contract)
    `(Err ,(make-blame 'callee location
                       "Dependent contract wrapping not yet implemented"
                       value))]
   [else
    `(Err ,(make-blame 'callee location
                       "Unknown contract type"
                       contract))]))

;;; wrap-function : Contract → Procedure → Symbol → Symbol → Procedure
;;; Wrap a procedure with a function contract.
;;; blame-party is 'caller or 'callee - determines who gets blamed for domain violations.
(define (wrap-function contract proc location blame-party)
  (let ([domain-contracts (function-contract-domain contract)]
        [range-contract (function-contract-range contract)])
    (lambda args
      ;; Check arity
      (if (not (= (length args) (length domain-contracts)))
          (error 'contract-violation
                 (format "~a: expected ~a arguments, got ~a"
                         location (length domain-contracts) (length args)))
          ;; Check domain contracts and wrap HO arguments
          (let ([wrapped-args (check-and-wrap-args domain-contracts args location blame-party)])
            (if (eq? (car wrapped-args) 'Err)
                (error 'contract-violation (blame->string (cadr wrapped-args)))
                ;; Call the function with wrapped arguments
                (let ([result (apply proc (cadr wrapped-args))])
                  ;; Check/wrap the result (blame callee for range violations)
                  (let ([checked-result (contract-wrap-with-blame
                                          range-contract result location
                                          (if (eq? blame-party 'caller) 'callee 'caller))])
                    (if (eq? (car checked-result) 'Err)
                        (error 'contract-violation (blame->string (cadr checked-result)))
                        (cadr checked-result))))))))))

;;; check-and-wrap-args : (List Contract) → (List Any) → Symbol → Symbol → (Ok (List Any)) | (Err Blame)
;;; Check each argument against its contract, wrapping HO arguments.
(define (check-and-wrap-args contracts args location blame-party)
  (let loop ([cs contracts] [as args] [acc '()])
    (if (null? cs)
        `(Ok ,(reverse acc))
        (let ([result (contract-wrap-with-blame (car cs) (car as) location blame-party)])
          (if (eq? (car result) 'Err)
              result
              (loop (cdr cs) (cdr as) (cons (cadr result) acc)))))))

;;; contract-wrap-with-blame : Contract → Any → Symbol → Symbol → (Ok Any) | (Err Blame)
;;; Wrap with explicit blame party for domain/range checking.
(define (contract-wrap-with-blame contract value location blame-party)
  (cond
   ;; Flat contracts: check immediately
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract)
        (and (pair? contract) (memq (car contract) '(And Or Not))))
    (let ([result (check-flat contract value location)])
      (if (eq? (car result) 'Err)
          ;; Adjust blame party
          (let ([b (cadr result)])
            `(Err ,(make-blame blame-party
                               (blame-location b)
                               (blame-message b)
                               (blame-value b))))
          result))]
   ;; Function contracts: wrap with FLIPPED blame for HO arguments
   [(function-contract? contract)
    (if (procedure? value)
        ;; Higher-order: flip blame because if caller passes f,
        ;; and callee calls f incorrectly, callee is at fault
        `(Ok ,(wrap-function contract value location
                             (if (eq? blame-party 'caller) 'callee 'caller)))
        `(Err ,(make-blame blame-party location
                           "Expected a procedure for function contract"
                           value)))]
   ;; Dependent contracts: not yet supported
   [(dependent-contract? contract)
    `(Err ,(make-blame blame-party location
                       "Dependent contract wrapping not yet implemented"
                       value))]
   [else
    `(Err ,(make-blame blame-party location
                       "Unknown contract type"
                       contract))]))

;;; apply-contract : Contract → Any → Symbol → Any
;;; Convenience function: apply contract, raise error on violation, return value on success.
(define (apply-contract contract value location)
  (let ([result (contract-wrap contract value location)])
    (if (eq? (car result) 'Err)
        (error 'contract-violation (blame->string (cadr result)))
        (cadr result))))
