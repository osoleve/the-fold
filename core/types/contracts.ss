(load "core/base/prelude.ss")
(load "core/types/types.ss")

(doc 'module 'contracts)
(doc 'description "Contract System - first-class specifications with runtime checking and blame tracking.")
(doc 'layer 'core)

(doc 'section 'contract-predicates)

(define (contract? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is a well-formed contract.")
  (doc 'export #t)
  (cond
   [(eq? c 'Any) #t]
   [(eq? c 'None) #t]
   [(not (pair? c)) #f]
   [(eq? (car c) 'Flat)
    (and (= (length c) 2)
         (procedure? (cadr c)))]
   [(eq? (car c) '->)
    (and (= (length c) 3)
         (list? (cadr c))
         (andmap contract? (cadr c))
         (contract? (caddr c)))]
   [(eq? (car c) 'Dep)
    (and (= (length c) 3)
         (list? (cadr c))
         (andmap symbol? (cadr c))
         ;; Body can be a contract (static) or procedure (dynamic contract factory)
         (or (contract? (caddr c))
             (procedure? (caddr c))))]
   [(eq? (car c) 'And)
    (andmap contract? (cdr c))]
   [(eq? (car c) 'Or)
    (andmap contract? (cdr c))]
   [(eq? (car c) 'Not)
    (and (= (length c) 2)
         (contract? (cadr c)))]
   [(eq? (car c) 'Invariant)
    (and (= (length c) 3)
         (procedure? (cadr c))    ; base-predicate
         (procedure? (caddr c)))] ; invariant-predicate
   [else #f]))

(define (flat-contract? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a contract is a flat contract.")
  (doc 'export #t)
  (and (pair? c) (eq? (car c) 'Flat)))

(define (function-contract? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a contract is a function contract.")
  (doc 'export #t)
  (and (pair? c) (eq? (car c) '->)))

(define (dependent-contract? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a contract is a dependent contract.")
  (doc 'export #t)
  (and (pair? c) (eq? (car c) 'Dep)))

(define (higher-order-contract? c)
  (doc 'type (-> Contract Boolean))
  (doc 'description "Check if a contract is higher-order (function, dependent, or chaperone).")
  (doc 'export #t)
  (cond
   [(not (pair? c)) #f]
   [(eq? (car c) '->) #t]
   [(eq? (car c) 'Dep) #t]
   [(eq? (car c) 'chaperone-listof) #t]
   [(eq? (car c) 'chaperone-vectorof) #t]
   [(eq? (car c) 'And) (ormap higher-order-contract? (cdr c))]
   [(eq? (car c) 'Or) (ormap higher-order-contract? (cdr c))]
   [(eq? (car c) 'Not) (higher-order-contract? (cadr c))]
   [else #f]))

(define (all-flat-contracts? contracts)
  (doc 'type (-> (List Contract) Boolean))
  (doc 'description "Check if all contracts in a list are flat (no function contracts).")
  (doc 'export #f)
  (not (ormap higher-order-contract? contracts)))

(doc 'section 'contract-constructors)

(define (flat pred)
  (doc 'type (-> (-> Any Boolean) Contract))
  (doc 'description "Create a flat contract from a predicate.")
  (doc 'export #t)
  `(Flat ,pred))

(define (->c domain-contracts range-contract)
  (doc 'type (-> (List Contract) Contract Contract))
  (doc 'description "Create a function contract with domain and range contracts.")
  (doc 'export #t)
  `(-> ,domain-contracts ,range-contract))

(define (dep vars body)
  (doc 'type (-> (List Symbol) Contract Contract))
  (doc 'description "Create a dependent contract with variables and body.")
  (doc 'export #t)
  `(Dep ,vars ,body))

(define (and/c . contracts)
  (doc 'type (-> Contract ... Contract))
  (doc 'description "Conjunction of contracts - all must be satisfied.")
  (doc 'export #t)
  (cons 'And contracts))

(define (or/c . contracts)
  (doc 'type (-> Contract ... Contract))
  (doc 'description "Disjunction of contracts - at least one must be satisfied.")
  (doc 'export #t)
  (cons 'Or contracts))

(define (not/c contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Negation of a contract - must NOT be satisfied.")
  (doc 'export #t)
  `(Not ,contract))

(doc any/c 'type 'Contract)
(doc any/c 'description "Top contract - always satisfied.")
(doc any/c 'export #t)
(define any/c 'Any)

(doc none/c 'type 'Contract)
(doc none/c 'description "Bottom contract - never satisfied.")
(doc none/c 'export #t)
(define none/c 'None)

(doc 'section 'common-flat-contracts)

(doc nat/c 'type 'Contract)
(doc nat/c 'description "Natural number contract (non-negative integer).")
(doc nat/c 'export #t)
(define nat/c
  (flat (lambda (x) (and (integer? x) (>= x 0)))))

(doc pos/c 'type 'Contract)
(doc pos/c 'description "Positive number contract.")
(doc pos/c 'export #t)
(define pos/c
  (flat (lambda (x) (and (number? x) (> x 0)))))

(doc non-neg/c 'type 'Contract)
(doc non-neg/c 'description "Non-negative number contract.")
(doc non-neg/c 'export #t)
(define non-neg/c
  (flat (lambda (x) (and (number? x) (>= x 0)))))

(doc int/c 'type 'Contract)
(doc int/c 'description "Integer contract.")
(doc int/c 'export #t)
(define int/c
  (flat integer?))

(doc bool/c 'type 'Contract)
(doc bool/c 'description "Boolean contract.")
(doc bool/c 'export #t)
(define bool/c
  (flat boolean?))

(doc string/c 'type 'Contract)
(doc string/c 'description "String contract.")
(doc string/c 'export #t)
(define string/c
  (flat string?))

(doc symbol/c 'type 'Contract)
(doc symbol/c 'description "Symbol contract.")
(doc symbol/c 'export #t)
(define symbol/c
  (flat symbol?))

(doc list/c 'type 'Contract)
(doc list/c 'description "List contract (any element type).")
(doc list/c 'export #t)
(define list/c
  (flat list?))

(doc vector/c 'type 'Contract)
(doc vector/c 'description "Vector contract (any element type).")
(doc vector/c 'export #t)
(define vector/c
  (flat vector?))

(doc procedure/c 'type 'Contract)
(doc procedure/c 'description "Procedure contract (any arity).")
(doc procedure/c 'export #t)
(define procedure/c
  (flat procedure?))

(doc 'section 'contract-accessors)

(define (flat-predicate c)
  (doc 'type (-> Contract (-> Any Boolean)))
  (doc 'description "Extract predicate from a flat contract.")
  (doc 'export #t)
  (if (flat-contract? c)
      (cadr c)
      (lambda (x) #f)))

(define (function-contract-domain c)
  (doc 'type (-> Contract (List Contract)))
  (doc 'description "Extract domain contracts from a function contract.")
  (doc 'export #t)
  (if (function-contract? c)
      (cadr c)
      '()))

(define (function-contract-range c)
  (doc 'type (-> Contract Contract))
  (doc 'description "Extract range contract from a function contract.")
  (doc 'export #t)
  (if (function-contract? c)
      (caddr c)
      none/c))

(define (dependent-contract-vars c)
  (doc 'type (-> Contract (List Symbol)))
  (doc 'description "Extract variables from a dependent contract.")
  (doc 'export #t)
  (if (dependent-contract? c)
      (cadr c)
      '()))

(define (dependent-contract-body c)
  (doc 'type (-> Contract Contract))
  (doc 'description "Extract body from a dependent contract.")
  (doc 'export #t)
  (if (dependent-contract? c)
      (caddr c)
      none/c))

(doc 'section 'dependent-contract-instantiation)

(define (dependent-contract-body-is-procedure? c)
  (doc 'type (-> Contract Boolean))
  (doc 'description "Check if dependent contract body is a procedure (contract factory).")
  (doc 'export #f)
  (and (dependent-contract? c)
       (procedure? (dependent-contract-body c))))

(define (instantiate-dependent-contract dep-contract values)
  (doc 'type (-> Contract (List Any) Contract))
  (doc 'description "Instantiate a dependent contract by binding vars to values.
If body is a procedure, apply it to values. If body is a contract, return as-is.")
  (doc 'export #t)
  (if (not (dependent-contract? dep-contract))
      dep-contract
      (let ([vars (dependent-contract-vars dep-contract)]
            [body (dependent-contract-body dep-contract)])
        (cond
         ;; Body is a procedure (contract factory) - apply it to get concrete contract
         [(procedure? body)
          (if (not (= (length vars) (length values)))
              (error 'instantiate-dependent-contract
                     (format "Expected ~a values for vars ~s, got ~a"
                             (length vars) vars (length values)))
              (apply body values))]
         ;; Body is already a contract - use as-is (vars don't affect it)
         [(contract? body)
          body]
         [else
          (error 'instantiate-dependent-contract
                 "Dependent contract body must be a contract or procedure")]))))

(doc 'section 'blame-tracking)

(define (make-blame party location message value)
  (doc 'type (-> Symbol Symbol String Any Blame))
  (doc 'description "Create a blame record: party is 'caller or 'callee.")
  (doc 'export #t)
  `(blame ,party ,location ,message ,value))

(define (blame? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is a blame record.")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'blame)
       (= (length x) 5)))

(define (blame-party b)
  (doc 'type (-> Blame Symbol))
  (doc 'description "Extract party from blame record.")
  (doc 'export #t)
  (if (blame? b) (cadr b) #f))

(define (blame-location b)
  (doc 'type (-> Blame Symbol))
  (doc 'description "Extract location from blame record.")
  (doc 'export #t)
  (if (blame? b) (caddr b) #f))

(define (blame-message b)
  (doc 'type (-> Blame String))
  (doc 'description "Extract message from blame record.")
  (doc 'export #t)
  (if (blame? b) (cadddr b) ""))

(define (blame-value b)
  (doc 'type (-> Blame Any))
  (doc 'description "Extract offending value from blame record.")
  (doc 'export #t)
  (if (blame? b) (car (cddddr b)) #f))

(define (flip-blame b)
  (doc 'type (-> Blame Blame))
  (doc 'description "Swap caller and callee - used when traversing contract boundaries.")
  (doc 'export #t)
  (if (blame? b)
      (make-blame (if (eq? (blame-party b) 'caller) 'callee 'caller)
                  (blame-location b)
                  (blame-message b)
                  (blame-value b))
      b))

(doc 'section 'contract-checking)

(define (check-flat contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Check a flat contract against a value - returns (Ok value) or (Err blame).")
  (doc 'export #t)
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
   [(invariant-contract? contract)
    (check-invariant contract value location)]
   [(eq? (car contract) 'And)
    (check-and-contracts (cdr contract) value location)]
   [(eq? (car contract) 'Or)
    (check-or-contracts (cdr contract) value location)]
   [(eq? (car contract) 'Not)
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

(define (check-and-contracts contracts value location)
  (doc 'type (-> (List Contract) Any Symbol (Result Any Blame)))
  (doc 'description "Check conjunction of contracts - all must be satisfied.")
  (doc 'export #f)
  (if (null? contracts)
      `(Ok ,value)
      (let ([result (check-flat (car contracts) value location)])
           (if (eq? (car result) 'Ok)
               (check-and-contracts (cdr contracts) value location)
               result))))

(define (check-or-contracts contracts value location)
  (doc 'type (-> (List Contract) Any Symbol (Result Any Blame)))
  (doc 'description "Check disjunction of contracts - at least one must be satisfied.")
  (doc 'export #f)
  (if (null? contracts)
      `(Err ,(make-blame 'callee location
                         "None of the contracts in disjunction satisfied"
                         value))
      (let ([result (check-flat (car contracts) value location)])
           (if (eq? (car result) 'Ok)
               result
               (check-or-contracts (cdr contracts) value location)))))

(doc 'section 'contract-wrapping)

(define (wrap contract value location)
  (doc 'type (-> Contract Any Symbol Wrapped))
  (doc 'description "Wrap a value with its contract and location for deferred checking.")
  (doc 'export #t)
  `(wrapped ,contract ,value ,location))

(define (wrapped? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is contract-wrapped.")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'wrapped)
       (= (length x) 4)))

(define (unwrap w)
  (doc 'type (-> Any Any))
  (doc 'description "Extract the underlying value from a wrapped value.")
  (doc 'export #t)
  (if (wrapped? w) (caddr w) w))

(define (wrapped-contract w)
  (doc 'type (-> Wrapped Contract))
  (doc 'description "Extract the contract from a wrapped value.")
  (doc 'export #t)
  (if (wrapped? w) (cadr w) any/c))

(define (wrapped-location w)
  (doc 'type (-> Wrapped Symbol))
  (doc 'description "Extract the location from a wrapped value.")
  (doc 'export #t)
  (if (wrapped? w) (cadddr w) 'unknown))

(doc 'section 'contract-combinators)

(define (listof elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Contract for a list of elements all satisfying elem-contract.")
  (doc 'export #t)
  (flat (lambda (lst)
                (and (list? lst)
                     (andmap (lambda (x)
                                     (let ([result (check-flat elem-contract x 'listof)])
                                          (eq? (car result) 'Ok)))
                             lst)))))

(define (vectorof elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Contract for a vector of elements all satisfying elem-contract.")
  (doc 'export #t)
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

(define (between/c low high)
  (doc 'type (-> Number Number Contract))
  (doc 'description "Contract for numbers in range [low, high].")
  (doc 'export #t)
  (flat (lambda (x)
                (and (number? x)
                     (>= x low)
                     (<= x high)))))

(define (one-of/c . values)
  (doc 'type (-> Any ... Contract))
  (doc 'description "Contract for values eq? to one of the given values.")
  (doc 'export #t)
  (flat (lambda (x)
                (if (memq x values) #t #f))))

(define (listof-length n elem-contract)
  (doc 'type (-> Nat Contract Contract))
  (doc 'description "Contract for a list of exactly n elements, each satisfying elem-contract.")
  (doc 'export #t)
  (flat (lambda (lst)
          (and (list? lst)
               (= (length lst) n)
               (andmap (lambda (x)
                        (let ([result (check-flat elem-contract x 'listof-length)])
                          (eq? (car result) 'Ok)))
                       lst)))))

(define (less-than/c bound)
  (doc 'type (-> Number Contract))
  (doc 'description "Contract for numbers strictly less than bound.")
  (doc 'export #t)
  (flat (lambda (x) (and (number? x) (< x bound)))))

(doc 'section 'contract-display)

(define (contract->string c)
  (doc 'type (-> Contract String))
  (doc 'description "Convert a contract to a human-readable string.")
  (doc 'export #t)
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
   [(invariant-contract? c)
    (invariant->string c)]
   [else "???"]))

(define (blame->string b)
  (doc 'type (-> Blame String))
  (doc 'description "Convert a blame record to a human-readable error message.")
  (doc 'export #t)
  (if (blame? b)
      (string-append "Contract violation\n"
                     "  Party: " (symbol->string (blame-party b)) "\n"
                     "  Location: " (symbol->string (blame-location b)) "\n"
                     "  Message: " (blame-message b) "\n"
                     "  Value: " (format "~s" (blame-value b)))
      "Invalid blame"))

(doc 'section 'centralized-violation-handling)

;;; Centralized Contract Violation Handling
;;;
;;; All contract violations should go through raise-contract-violation!
;;; This ensures statistical monitoring can track violations that occur
;;; during wrapped function execution (call-time violations), not just
;;; violations detected during initial wrap-time checks.

;;; Forward declaration - actual implementation is in statistical-monitoring section
;;; This breaks the circular dependency: violations need to record stats,
;;; but stats section is defined later.
(define *record-violation-if-statistical* #f)

(define (raise-contract-violation! message)
  (doc 'type (-> String Never))
  (doc 'description "Raise a contract violation error, recording it for statistical monitoring if enabled.")
  (doc 'export #t)
  ;; Record violation if statistical monitoring is active
  (when (and *record-violation-if-statistical*
             (eq? *contract-mode* 'statistical))
    (*record-violation-if-statistical*))
  (error 'contract-violation message))

(define (raise-contract-violation/blame! blame)
  (doc 'type (-> Blame Never))
  (doc 'description "Raise a contract violation from a blame record.")
  (doc 'export #t)
  (raise-contract-violation! (blame->string blame)))

(define (raise-contract-violation/fmt! location fmt-string . args)
  (doc 'type (-> Symbol String Any ... Never))
  (doc 'description "Raise a contract violation with formatted message.")
  (doc 'export #t)
  (raise-contract-violation! (apply format (cons fmt-string args))))

(doc 'section 'higher-order-wrapping)

;;; Forward declarations for chaperone contract predicates
;;; (full definitions are in the chaperone-contracts section below)
(define (chaperone-listof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-listof)))

(define (chaperone-vectorof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-vectorof)))

;;; Forward declarations for invariant contracts
;;; (full definitions are in the invariant-contracts section below)
(define (invariant-contract? c)
  (and (pair? c) (eq? (car c) 'Invariant)))

(define (check-invariant contract value location)
  ;; Placeholder - redefined in invariant-contracts section
  `(Ok ,value))

;;; Note: make-chaperone-list and make-chaperone-vector are defined later
;;; but contract-wrap needs them. We use a thunk pattern to delay evaluation.
(define *make-chaperone-list* #f)
(define *make-chaperone-vector* #f)

(define (contract-wrap contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with a contract - flat contracts check immediately, function contracts wrap.")
  (doc 'export #t)
  (cond
   ;; Simple cases: Any, None, flat contracts
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract))
    (check-flat contract value location)]
   ;; And/Or/Not: check if all sub-contracts are flat
   [(and (pair? contract) (eq? (car contract) 'And))
    (if (all-flat-contracts? (cdr contract))
        (check-flat contract value location)
        (wrap-and-contract (cdr contract) value location 'caller))]
   [(and (pair? contract) (eq? (car contract) 'Or))
    (if (all-flat-contracts? (cdr contract))
        (check-flat contract value location)
        (wrap-or-contract (cdr contract) value location 'caller))]
   [(and (pair? contract) (eq? (car contract) 'Not))
    (if (not (higher-order-contract? (cadr contract)))
        (check-flat contract value location)
        (wrap-not-contract (cadr contract) value location 'caller))]
   ;; Function contracts
   [(function-contract? contract)
    (if (procedure? value)
        `(Ok ,(wrap-function contract value location 'caller))
        `(Err ,(make-blame 'callee location
                           "Expected a procedure for function contract"
                           value)))]
   [(dependent-contract? contract)
    (wrap-dependent-contract contract value location 'caller)]
   ;; Chaperone contracts
   [(chaperone-listof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (list? value)
          `(Ok ,(*make-chaperone-list* value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a list for listof contract"
                             value))))]
   [(chaperone-vectorof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (vector? value)
          `(Ok ,(*make-chaperone-vector* value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a vector for vectorof contract"
                             value))))]
   [else
    `(Err ,(make-blame 'callee location
                       "Unknown contract type"
                       contract))]))

(define (wrap-function contract proc location blame-party)
  (doc 'type (-> Contract Procedure Symbol Symbol Procedure))
  (doc 'description "Wrap a procedure with a function contract - domain violations blame caller, range violations blame callee.")
  (doc 'export #f)
  (let ([domain-contracts (function-contract-domain contract)]
        [range-contract (function-contract-range contract)])
    (lambda args
      (if (not (= (length args) (length domain-contracts)))
          (raise-contract-violation!
           (format "~a: expected ~a arguments, got ~a"
                   location (length domain-contracts) (length args)))
          (let ([wrapped-args (check-and-wrap-args domain-contracts args location blame-party)])
            (if (eq? (car wrapped-args) 'Err)
                (raise-contract-violation/blame! (cadr wrapped-args))
                (let ([result (apply proc (cadr wrapped-args))])
                  (let ([checked-result (contract-wrap-with-blame
                                          range-contract result location
                                          (if (eq? blame-party 'caller) 'callee 'caller))])
                    (if (eq? (car checked-result) 'Err)
                        (raise-contract-violation/blame! (cadr checked-result))
                        (cadr checked-result))))))))))

(doc 'section 'dependent-contract-wrapping)

;;; Dependent Contract Wrapping
;;;
;;; Dependent contracts have the form (Dep (vars...) body) where:
;;; - vars are symbols that will be bound to actual values at check time
;;; - body is either a contract or a procedure (contract factory)
;;;
;;; For dependent function contracts like:
;;;   (dep '(n) (lambda (n) (->c (list nat/c) (listof-length n nat/c))))
;;; The vars correspond positionally to the function's arguments.
;;; When the wrapped function is called, we capture the args, instantiate
;;; the contract, and apply the resulting function contract.

(define (wrap-dependent-contract dep-contract value location blame-party)
  (doc 'type (-> Contract Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with a dependent contract.")
  (doc 'export #f)
  (let ([vars (dependent-contract-vars dep-contract)]
        [body (dependent-contract-body dep-contract)])
    (cond
     ;; Body is a procedure (contract factory)
     [(procedure? body)
      (if (procedure? value)
          ;; Wrapping a function - create a wrapper that captures args
          `(Ok ,(wrap-dependent-function dep-contract value location blame-party))
          ;; Non-function value - we need var values to instantiate
          ;; This case requires the values to be provided externally
          `(Err ,(make-blame blame-party location
                             (format "Dependent contract with ~a vars requires a procedure, or use instantiate-dependent-contract first"
                                     (length vars))
                             value)))]
     ;; Body is a static contract - delegate to appropriate wrapper
     [(contract? body)
      (contract-wrap-with-blame body value location blame-party)]
     [else
      `(Err ,(make-blame blame-party location
                         "Invalid dependent contract body"
                         dep-contract))])))

(define (wrap-dependent-function dep-contract proc location blame-party)
  (doc 'type (-> Contract Procedure Symbol Symbol Procedure))
  (doc 'description "Wrap a procedure with a dependent function contract.
Captures argument values at call time, instantiates the contract, and applies it.")
  (doc 'export #f)
  (let ([vars (dependent-contract-vars dep-contract)]
        [body (dependent-contract-body dep-contract)])
    (lambda args
      ;; Capture the first N arguments for dependent var binding
      ;; where N = number of dependent vars
      (let* ([num-vars (length vars)]
             [captured-vals (if (<= num-vars (length args))
                               (take args num-vars)
                               args)]
             ;; Instantiate the contract with captured values
             [concrete-contract (instantiate-dependent-contract dep-contract captured-vals)])
        ;; Now apply the concrete contract
        (cond
         ;; If instantiation produced a function contract, apply it
         [(function-contract? concrete-contract)
          (let* ([domain-contracts (function-contract-domain concrete-contract)]
                 [range-contract (function-contract-range concrete-contract)])
            (if (not (= (length args) (length domain-contracts)))
                (raise-contract-violation!
                 (format "~a: expected ~a arguments, got ~a"
                         location (length domain-contracts) (length args)))
                (let ([wrapped-args (check-and-wrap-args domain-contracts args location blame-party)])
                  (if (eq? (car wrapped-args) 'Err)
                      (raise-contract-violation/blame! (cadr wrapped-args))
                      (let ([result (apply proc (cadr wrapped-args))])
                        (let ([checked-result (contract-wrap-with-blame
                                                range-contract result location
                                                (if (eq? blame-party 'caller) 'callee 'caller))])
                          (if (eq? (car checked-result) 'Err)
                              (raise-contract-violation/blame! (cadr checked-result))
                              (cadr checked-result))))))))]
         ;; If instantiation produced another dependent contract, recurse
         [(dependent-contract? concrete-contract)
          (let ([inner-result (wrap-dependent-contract concrete-contract proc location blame-party)])
            (if (eq? (car inner-result) 'Err)
                (raise-contract-violation/blame! (cadr inner-result))
                (apply (cadr inner-result) args)))]
         ;; If it's a flat contract, check and call
         [else
          (let ([check-result (check-flat concrete-contract proc location)])
            (if (eq? (car check-result) 'Err)
                (raise-contract-violation/blame! (cadr check-result))
                (apply proc args)))])))))

;;; Helper to take first n elements from a list
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(doc 'section 'higher-order-combinators)

;;; Higher-Order Contract Combinators
;;;
;;; When And/Or/Not contain function contracts, we cannot check them immediately.
;;; Instead, we wrap the value and check at call-time.

(define (wrap-and-contract contracts value location blame-party)
  (doc 'type (-> (List Contract) Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with conjunction of contracts, some of which may be higher-order.")
  (doc 'export #f)
  ;; Strategy: Check all flat contracts immediately, wrap for function contracts
  ;; For and/c, ALL contracts must be satisfied
  (if (not (procedure? value))
      ;; Non-procedure: check flat contracts, fail on function contracts
      (let loop ([cs contracts])
        (if (null? cs)
            `(Ok ,value)
            (let ([c (car cs)])
              (if (higher-order-contract? c)
                  `(Err ,(make-blame blame-party location
                                     "and/c: expected procedure for function contract"
                                     value))
                  (let ([result (check-flat c value location)])
                    (if (eq? (car result) 'Ok)
                        (loop (cdr cs))
                        `(Err ,(make-blame blame-party
                                           (blame-location (cadr result))
                                           (blame-message (cadr result))
                                           (blame-value (cadr result))))))))))
      ;; Procedure: wrap it to check all contracts at call time
      `(Ok ,(wrap-and-function contracts value location blame-party))))

(define (wrap-and-function contracts proc location blame-party)
  (doc 'type (-> (List Contract) Procedure Symbol Symbol Procedure))
  (doc 'description "Wrap a procedure to satisfy all contracts in conjunction.")
  (doc 'export #f)
  ;; At call time, the wrapped function checks each function contract
  ;; All must pass - we use the first function contract's arity
  (let ([fn-contracts (filter function-contract? contracts)]
        [flat-contracts (filter (lambda (c) (not (higher-order-contract? c))) contracts)])
    (if (null? fn-contracts)
        ;; No function contracts - shouldn't happen but handle gracefully
        proc
        (let* ([first-fn (car fn-contracts)]
               [domain-contracts (function-contract-domain first-fn)]
               [contract-str (contract->string (cons 'And contracts))])  ; For error messages
          (lambda args
            ;; First, verify arity matches
            (if (not (= (length args) (length domain-contracts)))
                (raise-contract-violation!
                 (format "~a: ~a arity mismatch - expected ~a arguments, got ~a"
                         location contract-str (length domain-contracts) (length args)))
                ;; Check domain contracts from ALL function contracts
                (let ([domain-check (check-all-domains fn-contracts args location blame-party)])
                  (if (eq? (car domain-check) 'Err)
                      (raise-contract-violation/blame! (cadr domain-check))
                      (let ([result (apply proc (cadr domain-check))])
                        ;; Check range contracts from ALL function contracts
                        (let ([range-check (check-all-ranges fn-contracts result location
                                                             (if (eq? blame-party 'caller) 'callee 'caller))])
                          (if (eq? (car range-check) 'Err)
                              (raise-contract-violation/blame! (cadr range-check))
                              (cadr range-check))))))))))))

(define (check-all-domains fn-contracts args location current-blame-party)
  (doc 'type (-> (List Contract) (List Any) Symbol Symbol (Result (List Any) Blame)))
  (doc 'description "Check arguments against all function contracts' domains.")
  (doc 'export #f)
  ;; For and/c, ALL domain contracts must pass
  ;; We check each contract's domain in sequence, threading wrapped args through
  ;; Track index for precise blame messages
  (let loop ([fns fn-contracts] [current-args args] [idx 0])
    (if (null? fns)
        `(Ok ,current-args)
        (let* ([fn (car fns)]
               [domain-cs (function-contract-domain fn)]
               [check (check-and-wrap-args domain-cs current-args location current-blame-party)])
          (if (eq? (car check) 'Err)
              ;; Enhance blame message with contract index and description
              (let ([orig-blame (cadr check)])
                `(Err ,(make-blame (blame-party orig-blame)
                                   (blame-location orig-blame)
                                   (format "and/c contract #~a ~a: ~a"
                                           (+ idx 1)
                                           (contract->string fn)
                                           (blame-message orig-blame))
                                   (blame-value orig-blame))))
              (loop (cdr fns) (cadr check) (+ idx 1)))))))

(define (check-all-ranges fn-contracts result location current-blame-party)
  (doc 'type (-> (List Contract) Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Check result against all function contracts' ranges.")
  (doc 'export #f)
  ;; Track index for precise blame messages
  (let loop ([fns fn-contracts] [val result] [idx 0])
    (if (null? fns)
        `(Ok ,val)
        (let* ([fn (car fns)]
               [range-c (function-contract-range fn)]
               [check (contract-wrap-with-blame range-c val location current-blame-party)])
          (if (eq? (car check) 'Err)
              ;; Enhance blame message with contract index and description
              (let ([orig-blame (cadr check)])
                `(Err ,(make-blame (blame-party orig-blame)
                                   (blame-location orig-blame)
                                   (format "and/c contract #~a ~a range: ~a"
                                           (+ idx 1)
                                           (contract->string fn)
                                           (blame-message orig-blame))
                                   (blame-value orig-blame))))
              (loop (cdr fns) (cadr check) (+ idx 1)))))))

(define (wrap-or-contract contracts value location blame-party)
  (doc 'type (-> (List Contract) Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with disjunction of contracts, some of which may be higher-order.")
  (doc 'export #f)
  ;; Strategy for or/c:
  ;; 1. If value is not a procedure, try flat contracts first
  ;; 2. If value is a procedure, try function contracts
  ;; 3. At call time, try each function contract until one works
  (let ([flat-contracts (filter (lambda (c) (not (higher-order-contract? c))) contracts)]
        [fn-contracts (filter function-contract? contracts)])
    (if (not (procedure? value))
        ;; Non-procedure: only flat contracts can match
        (let loop ([cs flat-contracts])
          (if (null? cs)
              `(Err ,(make-blame blame-party location
                                 "or/c: no flat contract satisfied for non-procedure"
                                 value))
              (let ([result (check-flat (car cs) value location)])
                (if (eq? (car result) 'Ok)
                    result
                    (loop (cdr cs))))))
        ;; Procedure: wrap to try function contracts at call time
        (if (null? fn-contracts)
            ;; No function contracts - try flat contracts (might be contract for procedures)
            (let loop ([cs flat-contracts])
              (if (null? cs)
                  `(Err ,(make-blame blame-party location
                                     "or/c: no contract satisfied for procedure"
                                     value))
                  (let ([result (check-flat (car cs) value location)])
                    (if (eq? (car result) 'Ok)
                        result
                        (loop (cdr cs))))))
            `(Ok ,(wrap-or-function fn-contracts value location blame-party))))))

(define (wrap-or-function contracts proc location blame-party)
  (doc 'type (-> (List Contract) Procedure Symbol Symbol Procedure))
  (doc 'description "Wrap a procedure to satisfy at least one contract in disjunction.")
  (doc 'export #f)
  ;; At call time, try each function contract in order
  ;; First one whose domain accepts the args is used for range checking
  ;; Track failures for precise blame messages
  (lambda args
    (let try-contracts ([cs contracts] [idx 0] [failures '()])
      (if (null? cs)
          ;; No contract matched - build informative error message
          (let ([failure-details
                 (if (null? failures)
                     "all contracts had arity mismatch"
                     (string-append
                      "tried " (number->string (length contracts)) " contracts:\n"
                      (join-strings "\n"
                                    (map (lambda (f)
                                           (format "  #~a ~a: ~a"
                                                   (car f)
                                                   (contract->string (cadr f))
                                                   (caddr f)))
                                         (reverse failures)))))])
            (raise-contract-violation!
             (format "~a: or/c - no function contract matched\n~a"
                     location failure-details)))
          (let* ([c (car cs)]
                 [domain-contracts (function-contract-domain c)])
            (if (not (= (length args) (length domain-contracts)))
                ;; Arity mismatch, try next contract
                (try-contracts (cdr cs) (+ idx 1)
                               (cons (list (+ idx 1) c
                                          (format "arity mismatch (expected ~a, got ~a)"
                                                  (length domain-contracts) (length args)))
                                     failures))
                ;; Try this contract
                (let ([domain-check (check-and-wrap-args domain-contracts args location blame-party)])
                  (if (eq? (car domain-check) 'Err)
                      ;; Domain check failed, try next contract
                      (try-contracts (cdr cs) (+ idx 1)
                                     (cons (list (+ idx 1) c
                                                (blame-message (cadr domain-check)))
                                           failures))
                      ;; Domain passed, use this contract
                      (let ([result (apply proc (cadr domain-check))])
                        (let ([range-check (contract-wrap-with-blame
                                            (function-contract-range c) result location
                                            (if (eq? blame-party 'caller) 'callee 'caller))])
                          (if (eq? (car range-check) 'Err)
                              ;; Range failed - include which contract was selected
                              (let ([orig-blame (cadr range-check)])
                                (raise-contract-violation/blame!
                                 (make-blame (blame-party orig-blame)
                                             (blame-location orig-blame)
                                             (format "or/c contract #~a ~a range: ~a"
                                                     (+ idx 1)
                                                     (contract->string c)
                                                     (blame-message orig-blame))
                                             (blame-value orig-blame))))
                              (cadr range-check))))))))))))

(define (wrap-not-contract inner-contract value location blame-party)
  (doc 'type (-> Contract Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with negation of a higher-order contract.")
  (doc 'export #f)
  ;; For not/c with function contracts:
  ;; The value must NOT satisfy the inner contract
  ;; For functions, this is tricky - we'd need to prove the function CAN'T satisfy it
  ;; Practical approach: wrap and check at call time, inverting the result
  (if (not (procedure? value))
      ;; Non-procedure with HO inner contract: it trivially doesn't satisfy a function contract
      `(Ok ,value)
      ;; Procedure: wrap to check at call time
      (if (function-contract? inner-contract)
          `(Ok ,(wrap-not-function inner-contract value location blame-party))
          ;; Other HO contracts (chaperones, etc.) - not supported yet
          `(Err ,(make-blame blame-party location
                             "not/c: unsupported higher-order contract type"
                             inner-contract)))))

(define (wrap-not-function inner-contract proc location blame-party)
  (doc 'type (-> Contract Procedure Symbol Symbol Procedure))
  (doc 'description "Wrap a procedure to NOT satisfy a function contract.")
  (doc 'export #f)
  ;; Semantics: The wrapped function should fail if it WOULD satisfy the inner contract
  ;; This is useful for negative testing or exclusion constraints
  (let ([domain-contracts (function-contract-domain inner-contract)]
        [range-contract (function-contract-range inner-contract)]
        [contract-str (contract->string inner-contract)])  ; Cache for error messages
    (lambda args
      (if (not (= (length args) (length domain-contracts)))
          ;; Arity mismatch - doesn't match the contract, so not/c passes
          (apply proc args)
          ;; Same arity - check if args satisfy domain
          (let ([domain-check (check-and-wrap-args domain-contracts args location blame-party)])
            (if (eq? (car domain-check) 'Err)
                ;; Domain check failed - doesn't match contract, not/c passes
                (apply proc args)
                ;; Domain passed - call function and check if result satisfies range
                (let ([result (apply proc (cadr domain-check))])
                  (let ([range-check (contract-wrap-with-blame
                                      range-contract result location
                                      (if (eq? blame-party 'caller) 'callee 'caller))])
                    (if (eq? (car range-check) 'Ok)
                        ;; Range satisfied - the function DOES satisfy the contract, so not/c FAILS
                        (raise-contract-violation!
                         (format "~a: not/c ~a violated - function satisfies negated contract (args: ~s, result: ~s)"
                                 location contract-str args result))
                        ;; Range failed - doesn't satisfy contract, not/c passes
                        result)))))))))

(define (check-and-wrap-args contracts args location blame-party)
  (doc 'type (-> (List Contract) (List Any) Symbol Symbol (Result (List Any) Blame)))
  (doc 'description "Check each argument against its contract, wrapping higher-order arguments.")
  (doc 'export #f)
  (let loop ([cs contracts] [as args] [acc '()])
    (if (null? cs)
        `(Ok ,(reverse acc))
        (let ([result (contract-wrap-with-blame (car cs) (car as) location blame-party)])
          (if (eq? (car result) 'Err)
              result
              (loop (cdr cs) (cdr as) (cons (cadr result) acc)))))))

(define (contract-wrap-with-blame contract value location blame-party)
  (doc 'type (-> Contract Any Symbol Symbol (Result Any Blame)))
  (doc 'description "Wrap with explicit blame party - higher-order arguments get flipped blame.")
  (doc 'export #f)
  (cond
   ;; Simple cases: Any, None, flat contracts
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract))
    (let ([result (check-flat contract value location)])
      (if (eq? (car result) 'Err)
          (let ([b (cadr result)])
            `(Err ,(make-blame blame-party
                               (blame-location b)
                               (blame-message b)
                               (blame-value b))))
          result))]
   ;; And/Or/Not: check if all sub-contracts are flat
   [(and (pair? contract) (eq? (car contract) 'And))
    (if (all-flat-contracts? (cdr contract))
        (let ([result (check-flat contract value location)])
          (if (eq? (car result) 'Err)
              (let ([b (cadr result)])
                `(Err ,(make-blame blame-party (blame-location b)
                                   (blame-message b) (blame-value b))))
              result))
        (wrap-and-contract (cdr contract) value location blame-party))]
   [(and (pair? contract) (eq? (car contract) 'Or))
    (if (all-flat-contracts? (cdr contract))
        (let ([result (check-flat contract value location)])
          (if (eq? (car result) 'Err)
              (let ([b (cadr result)])
                `(Err ,(make-blame blame-party (blame-location b)
                                   (blame-message b) (blame-value b))))
              result))
        (wrap-or-contract (cdr contract) value location blame-party))]
   [(and (pair? contract) (eq? (car contract) 'Not))
    (if (not (higher-order-contract? (cadr contract)))
        (let ([result (check-flat contract value location)])
          (if (eq? (car result) 'Err)
              (let ([b (cadr result)])
                `(Err ,(make-blame blame-party (blame-location b)
                                   (blame-message b) (blame-value b))))
              result))
        (wrap-not-contract (cadr contract) value location blame-party))]
   ;; Function contracts
   [(function-contract? contract)
    (if (procedure? value)
        `(Ok ,(wrap-function contract value location
                             (if (eq? blame-party 'caller) 'callee 'caller)))
        `(Err ,(make-blame blame-party location
                           "Expected a procedure for function contract"
                           value)))]
   [(dependent-contract? contract)
    (wrap-dependent-contract contract value location blame-party)]
   ;; Chaperone contracts - delegate to chaperone wrapper
   [(chaperone-listof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (list? value)
          `(Ok ,(make-chaperone-list value elem-contract location))
          `(Err ,(make-blame blame-party location
                             "Expected a list for listof contract"
                             value))))]
   [(chaperone-vectorof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (vector? value)
          `(Ok ,(make-chaperone-vector value elem-contract location))
          `(Err ,(make-blame blame-party location
                             "Expected a vector for vectorof contract"
                             value))))]
   [else
    `(Err ,(make-blame blame-party location
                       "Unknown contract type"
                       contract))]))

(define (apply-contract contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Apply contract and raise error on violation, return value on success.")
  (doc 'export #t)
  (let ([result (contract-wrap contract value location)])
    (if (eq? (car result) 'Err)
        (raise-contract-violation/blame! (cadr result))
        (cadr result))))

(doc 'section 'chaperone-contracts)

;;; Chaperone-style contracts for higher-order elements in collections.
;;; Unlike flat contracts that check immediately, chaperones wrap the
;;; collection and apply element contracts lazily when elements are accessed.
;;;
;;; OPTIMIZATION (fold-zxva): Uses shared-spine representation to reduce
;;; allocation from O(4N) to O(3N + 4) cons cells during iteration.
;;; The spine (original list, contract, location) is shared across all
;;; chaperone positions, so chaperone-cdr only allocates 3 cons cells
;;; instead of 4 per step.

;;; Internal: Spine shared by all positions in a chaperoned list
;;; Spine stores: (chaperone-spine original len contract location)
;;; Length is cached to avoid O(N) computation on every cdr call.
(define (chaperone-spine? x)
  ;; O(1) structural check for 5-element spine
  (and (pair? x)
       (eq? (car x) 'chaperone-spine)
       (pair? (cdr x))      ; original
       (pair? (cddr x))     ; len
       (pair? (cdddr x))    ; contract
       (pair? (cddddr x))   ; location
       (null? (cdr (cddddr x)))))

(define (make-chaperone-spine original contract location)
  (doc 'type (-> List Contract Symbol ChaperoneSpine))
  (doc 'description "Create a shared spine for chaperone list positions.")
  (doc 'export #f)
  ;; Cache the length to avoid O(N) lookup on every cdr
  `(chaperone-spine ,original ,(length original) ,contract ,location))

(define (chaperone-spine-original spine)
  (cadr spine))

(define (chaperone-spine-length spine)
  (caddr spine))

(define (chaperone-spine-contract spine)
  (cadddr spine))

(define (chaperone-spine-location spine)
  (car (cddddr spine)))

;;; External: chaperone-list is (chaperone-list index spine)
;;; Index is the current position in the original list

(define (chaperone-list? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is a chaperoned list. O(1) structural check.")
  (doc 'export #t)
  ;; Structural check: (chaperone-list index spine) where spine is valid
  ;; Avoid (length x) which is O(N); instead check structure directly
  (and (pair? x)
       (eq? (car x) 'chaperone-list)
       (pair? (cdr x))          ; has at least index
       (integer? (cadr x))
       (pair? (cddr x))         ; has spine
       (chaperone-spine? (caddr x))
       (null? (cdddr x))))

(define (make-chaperone-list underlying elem-contract location)
  (doc 'type (-> List Contract Symbol ChaperoneList))
  (doc 'description "Create a chaperoned list that applies elem-contract on access.")
  (doc 'export #f)
  ;; Create spine from the full list, starting at index 0
  `(chaperone-list 0 ,(make-chaperone-spine underlying elem-contract location)))

;; Internal constructor for advancing position (reuses existing spine)
(define (make-chaperone-list-at-index index spine)
  `(chaperone-list ,index ,spine))

;; Wire up forward reference
(set! *make-chaperone-list* make-chaperone-list)

(define (chaperone-list-index cl)
  (if (chaperone-list? cl) (cadr cl) 0))

(define (chaperone-list-spine cl)
  (if (chaperone-list? cl) (caddr cl) #f))

(define (chaperone-list-underlying cl)
  (doc 'type (-> ChaperoneList List))
  (doc 'description "Get the remaining underlying list from current position.")
  (doc 'export #f)
  (if (chaperone-list? cl)
      (let ([spine (chaperone-list-spine cl)]
            [idx (chaperone-list-index cl)])
        (list-tail (chaperone-spine-original spine) idx))
      cl))

(define (chaperone-list-contract cl)
  (if (chaperone-list? cl)
      (chaperone-spine-contract (chaperone-list-spine cl))
      any/c))

(define (chaperone-list-location cl)
  (if (chaperone-list? cl)
      (chaperone-spine-location (chaperone-list-spine cl))
      'unknown))

(define (chaperone-car cl)
  (doc 'type (-> ChaperoneList Any))
  (doc 'description "Get first element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [elem (list-ref original idx)])
        (apply-contract elem-contract elem location))
      (car cl)))

(define (chaperone-cdr cl)
  (doc 'type (-> ChaperoneList ChaperoneList))
  (doc 'description "Get rest of chaperoned list, preserving chaperone. O(1) via shared spine with cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)]  ; O(1) cached lookup
             [new-idx (+ idx 1)])
        (if (>= new-idx len)
            '()
            ;; Only allocate 3 cons cells, reusing the spine
            (make-chaperone-list-at-index new-idx spine)))
      (cdr cl)))

(define (chaperone-list-ref cl n)
  (doc 'type (-> ChaperoneList Nat Any))
  (doc 'description "Get nth element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [elem (list-ref original (+ idx n))])
        (apply-contract elem-contract elem location))
      (list-ref cl n)))

(define (chaperone-list->list cl)
  (doc 'type (-> ChaperoneList List))
  (doc 'description "Convert chaperoned list to regular list, checking all elements.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [remaining (list-tail original idx)])
        (map (lambda (elem) (apply-contract elem-contract elem location))
             remaining))
      cl))

(define (chaperone-list-length cl)
  (doc 'type (-> ChaperoneList Nat))
  (doc 'description "Get length of chaperoned list from current position. O(1) via cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)])  ; O(1) cached lookup
        (- len idx))
      (length cl)))

(define (chaperone-list-null? cl)
  (doc 'type (-> ChaperoneList Boolean))
  (doc 'description "Check if chaperoned list is empty. O(1) via cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)])  ; O(1) cached lookup
        (>= idx len))
      (null? cl)))

;;; Chaperone vectors

(define (chaperone-vector? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is a chaperoned vector.")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'chaperone-vector)))

(define (make-chaperone-vector underlying elem-contract location)
  (doc 'type (-> Vector Contract Symbol ChaperoneVector))
  (doc 'description "Create a chaperoned vector that applies elem-contract on access.")
  (doc 'export #f)
  `(chaperone-vector ,underlying ,elem-contract ,location))

;; Wire up forward reference
(set! *make-chaperone-vector* make-chaperone-vector)

(define (chaperone-vector-underlying cv)
  (if (chaperone-vector? cv) (cadr cv) cv))

(define (chaperone-vector-contract cv)
  (if (chaperone-vector? cv) (caddr cv) any/c))

(define (chaperone-vector-location cv)
  (if (chaperone-vector? cv) (cadddr cv) 'unknown))

(define (chaperone-vector-ref cv n)
  (doc 'type (-> ChaperoneVector Nat Any))
  (doc 'description "Get nth element of chaperoned vector, applying element contract.")
  (doc 'export #t)
  (if (chaperone-vector? cv)
      (let* ([underlying (chaperone-vector-underlying cv)]
             [elem-contract (chaperone-vector-contract cv)]
             [location (chaperone-vector-location cv)]
             [elem (vector-ref underlying n)])
        (apply-contract elem-contract elem location))
      (vector-ref cv n)))

(define (chaperone-vector-length cv)
  (doc 'type (-> ChaperoneVector Nat))
  (doc 'description "Get length of chaperoned vector.")
  (doc 'export #t)
  (vector-length (if (chaperone-vector? cv)
                     (chaperone-vector-underlying cv)
                     cv)))

(define (chaperone-vector->list cv)
  (doc 'type (-> ChaperoneVector List))
  (doc 'description "Convert chaperoned vector to list, checking all elements.")
  (doc 'export #t)
  (if (chaperone-vector? cv)
      (let ([underlying (chaperone-vector-underlying cv)]
            [elem-contract (chaperone-vector-contract cv)]
            [location (chaperone-vector-location cv)]
            [len (vector-length (chaperone-vector-underlying cv))])
        (let loop ([i 0] [acc '()])
          (if (>= i len)
              (reverse acc)
              (loop (+ i 1)
                    (cons (apply-contract elem-contract
                                         (vector-ref underlying i)
                                         location)
                          acc)))))
      (vector->list cv)))

;;; Smart listof/vectorof that auto-detect when chaperones are needed

(define (needs-chaperone? elem-contract)
  (doc 'type (-> Contract Boolean))
  (doc 'description "Check if element contract requires chaperone wrapping.")
  (doc 'export #f)
  (or (function-contract? elem-contract)
      (dependent-contract? elem-contract)
      ;; Also check for nested listof/vectorof with HO contracts
      (and (pair? elem-contract)
           (memq (car elem-contract) '(chaperone-listof chaperone-vectorof)))))

(define (listof/c elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Smart list contract: uses chaperone for HO contracts, flat otherwise.")
  (doc 'export #t)
  (if (needs-chaperone? elem-contract)
      `(chaperone-listof ,elem-contract)
      (listof elem-contract)))

(define (vectorof/c elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Smart vector contract: uses chaperone for HO contracts, flat otherwise.")
  (doc 'export #t)
  (if (needs-chaperone? elem-contract)
      `(chaperone-vectorof ,elem-contract)
      (vectorof elem-contract)))

;;; Extend contract-wrap to handle chaperone contracts

(define (chaperone-listof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-listof)))

(define (chaperone-vectorof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-vectorof)))

(define (contract-wrap-chaperone contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Extended contract-wrap that handles chaperone contracts.")
  (doc 'export #t)
  (cond
   ;; Chaperone listof
   [(chaperone-listof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (list? value)
          `(Ok ,(make-chaperone-list value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a list for listof contract"
                             value))))]
   ;; Chaperone vectorof
   [(chaperone-vectorof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (vector? value)
          `(Ok ,(make-chaperone-vector value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a vector for vectorof contract"
                             value))))]
   ;; Fall through to standard contract-wrap
   [else
    (contract-wrap contract value location)]))

(define (apply-contract/c contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Apply contract with chaperone support, raise error on violation.")
  (doc 'export #t)
  (let ([result (contract-wrap-chaperone contract value location)])
    (if (eq? (car result) 'Err)
        (raise-contract-violation/blame! (cadr result))
        (cadr result))))

(doc 'section 'contract-checking-modes)

;;; Contract Checking Modes
;;;
;;; Contracts can operate in different enforcement modes:
;;;
;;;   'runtime     - Always check at runtime (default, current behavior)
;;;   'compile-time - Trust static verification, elide runtime checks
;;;   'test-only   - Only check when in test context
;;;   'doc-only    - No checking, contracts serve as documentation only
;;;
;;; Mode selection affects performance vs. safety tradeoffs:
;;; - Production code may use 'compile-time after static verification
;;; - Performance-critical paths may use 'test-only
;;; - Legacy integration may use 'doc-only during gradual adoption

(doc contract-modes 'type '(List Symbol))
(doc contract-modes 'description "Valid contract checking modes.")
(doc contract-modes 'export #t)
(define contract-modes '(runtime compile-time test-only doc-only statistical))

(define (contract-mode? m)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is a valid contract mode.")
  (doc 'export #t)
  (if (memq m contract-modes) #t #f))

;;; Global mode parameter - defaults to runtime checking
(doc *contract-mode* 'type 'Symbol)
(doc *contract-mode* 'description "Current contract checking mode. One of: runtime, compile-time, test-only, doc-only.")
(doc *contract-mode* 'export #t)
(define *contract-mode* 'runtime)

;;; Test context flag - used by test-only mode
(doc *in-test-context* 'type 'Boolean)
(doc *in-test-context* 'description "Flag indicating whether we're currently in a test context.")
(doc *in-test-context* 'export #t)
(define *in-test-context* #f)

(define (set-contract-mode! mode)
  (doc 'type (-> Symbol Void))
  (doc 'description "Set the global contract checking mode.")
  (doc 'export #t)
  (if (contract-mode? mode)
      (set! *contract-mode* mode)
      (error 'set-contract-mode!
             (format "Invalid contract mode: ~s. Must be one of: ~s"
                     mode contract-modes))))

(define (get-contract-mode)
  (doc 'type (-> Symbol))
  (doc 'description "Get the current contract checking mode.")
  (doc 'export #t)
  *contract-mode*)

(define (enter-test-context!)
  (doc 'type (-> Void))
  (doc 'description "Mark entry into a test context (enables test-only contracts).")
  (doc 'export #t)
  (set! *in-test-context* #t))

(define (exit-test-context!)
  (doc 'type (-> Void))
  (doc 'description "Mark exit from test context (disables test-only contracts).")
  (doc 'export #t)
  (set! *in-test-context* #f))

(define (in-test-context?)
  (doc 'type (-> Boolean))
  (doc 'description "Check if we're currently in a test context.")
  (doc 'export #t)
  *in-test-context*)

(define (contracts-enabled?)
  (doc 'type (-> Boolean))
  (doc 'description "Check if contract checking is currently enabled based on mode and context.
For statistical mode, returns #t probabilistically based on sample rate.")
  (doc 'export #t)
  (case *contract-mode*
    [(runtime) #t]
    [(compile-time) #f]  ; Trust static verification
    [(test-only) *in-test-context*]
    [(doc-only) #f]
    [(statistical) (should-check-statistically?)]
    [else #t]))  ; Default to enabled for safety

;;; Compile-time verification hooks
;;; These are stubs for future integration with the type system (fold-hex)

(doc *compile-time-verifier* 'type '(Or #f (-> Contract Type (Result Unit String))))
(doc *compile-time-verifier* 'description "Optional compile-time contract verifier. Set by type system integration.")
(doc *compile-time-verifier* 'export #t)
(define *compile-time-verifier* #f)

(define (set-compile-time-verifier! verifier)
  (doc 'type (-> (-> Contract Type (Result Unit String)) Void))
  (doc 'description "Register a compile-time contract verifier (called by type system).")
  (doc 'export #t)
  (if (procedure? verifier)
      (set! *compile-time-verifier* verifier)
      (error 'set-compile-time-verifier!
             "Verifier must be a procedure")))

(define (verify-contract-statically contract type location)
  (doc 'type (-> Contract Type Symbol (Result Unit String)))
  (doc 'description "Attempt static verification of a contract against a type.")
  (doc 'export #t)
  (if *compile-time-verifier*
      (*compile-time-verifier* contract type)
      ;; No verifier registered - cannot verify statically
      '(Err "No compile-time verifier registered")))

(doc 'section 'mode-aware-checking)

;;; Mode-aware contract checking functions
;;; These wrap the core checking functions with mode awareness

(define (check-flat/mode contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Mode-aware flat contract check. Respects *contract-mode* setting.
In statistical mode, tracks checks and violations.")
  (doc 'export #t)
  (cond
   [(eq? *contract-mode* 'statistical)
    (if (should-check-statistically?)
        (begin
          (record-statistical-check!)
          (let ([result (check-flat contract value location)])
            (when (eq? (car result) 'Err)
              (record-statistical-violation!))
            result))
        `(Ok ,value))]
   [(contracts-enabled?)
    (check-flat contract value location)]
   [else `(Ok ,value)]))

(define (contract-wrap/mode contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Mode-aware contract wrap. Respects *contract-mode* setting.
In statistical mode, tracks checks and violations.")
  (doc 'export #t)
  (cond
   [(eq? *contract-mode* 'statistical)
    (if (should-check-statistically?)
        (begin
          (record-statistical-check!)
          (let ([result (contract-wrap contract value location)])
            (when (eq? (car result) 'Err)
              (record-statistical-violation!))
            result))
        `(Ok ,value))]
   [(contracts-enabled?)
    (contract-wrap contract value location)]
   [else `(Ok ,value)]))

(define (apply-contract/mode contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Mode-aware apply-contract. Respects *contract-mode* setting.
In statistical mode, tracks checks via check-count; violations are tracked
by the centralized raise-contract-violation! function.")
  (doc 'export #t)
  (cond
   [(eq? *contract-mode* 'statistical)
    (if (should-check-statistically?)
        (begin
          (record-statistical-check!)
          ;; Note: violations are recorded by raise-contract-violation! inside apply-contract
          (apply-contract contract value location))
        value)]
   [(contracts-enabled?)
    (apply-contract contract value location)]
   [else value]))

(define (apply-contract/c/mode contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Mode-aware apply-contract/c with chaperone support. Respects *contract-mode* setting.")
  (doc 'export #t)
  (if (contracts-enabled?)
      (apply-contract/c contract value location)
      value))

;;; Convenience forms for scoped mode changes

(define (call-with-contract-mode mode thunk)
  (doc 'type (-> Symbol (-> a) a))
  (doc 'description "Call thunk with contract mode temporarily set to mode.")
  (doc 'export #t)
  (let ([saved-mode *contract-mode*])
    (dynamic-wind
      (lambda () (set! *contract-mode* mode))
      thunk
      (lambda () (set! *contract-mode* saved-mode)))))

(define (call-with-test-context thunk)
  (doc 'type (-> (-> a) a))
  (doc 'description "Call thunk with test context enabled.")
  (doc 'export #t)
  (let ([saved-context *in-test-context*])
    (dynamic-wind
      (lambda () (set! *in-test-context* #t))
      thunk
      (lambda () (set! *in-test-context* saved-context)))))

(define (call-without-contracts thunk)
  (doc 'type (-> (-> a) a))
  (doc 'description "Call thunk with contracts disabled (doc-only mode).")
  (doc 'export #t)
  (call-with-contract-mode 'doc-only thunk))

(define (call-with-runtime-contracts thunk)
  (doc 'type (-> (-> a) a))
  (doc 'description "Call thunk with runtime contracts explicitly enabled.")
  (doc 'export #t)
  (call-with-contract-mode 'runtime thunk))

(define (call-with-statistical-contracts rate thunk)
  (doc 'type (-> Real (-> a) a))
  (doc 'description "Call thunk with statistical contract checking at the given sample rate.
Useful for production code where full checking is too expensive.")
  (doc 'export #t)
  (let ([saved-mode *contract-mode*]
        [saved-rate *statistical-sample-rate*])
    (dynamic-wind
      (lambda ()
        (set! *contract-mode* 'statistical)
        (set! *statistical-sample-rate* rate))
      thunk
      (lambda ()
        (set! *contract-mode* saved-mode)
        (set! *statistical-sample-rate* saved-rate)))))

(doc 'section 'mode-reporting)

(define (contract-mode->string mode)
  (doc 'type (-> Symbol String))
  (doc 'description "Convert contract mode to human-readable description.")
  (doc 'export #t)
  (case mode
    [(runtime) "runtime (always check)"]
    [(compile-time) "compile-time (static verification only)"]
    [(test-only) "test-only (check only during tests)"]
    [(doc-only) "documentation-only (no checking)"]
    [(statistical) "statistical (probabilistic sampling)"]
    [else "unknown"]))

(define (contract-status)
  (doc 'type (-> String))
  (doc 'description "Return human-readable contract system status.")
  (doc 'export #t)
  (string-append
   "Contract mode: " (contract-mode->string *contract-mode*) "\n"
   "In test context: " (if *in-test-context* "yes" "no") "\n"
   "Contracts enabled: " (if (contracts-enabled?) "yes" "no") "\n"
   "Compile-time verifier: " (if *compile-time-verifier* "registered" "not registered")
   (if (eq? *contract-mode* 'statistical)
       (string-append "\n"
        "Statistical sample rate: " (number->string *statistical-sample-rate*) "\n"
        "Checks performed: " (number->string *statistical-check-count*) "\n"
        "Violations detected: " (number->string *statistical-violation-count*))
       "")))

(doc 'section 'statistical-monitoring)

;;; Statistical Contract Monitoring
;;;
;;; In statistical mode, contracts are checked probabilistically based on
;;; a configurable sample rate. This provides a performance/safety tradeoff:
;;;
;;;   - Sample rate 1.0: Check every call (equivalent to runtime mode)
;;;   - Sample rate 0.1: Check ~10% of calls (90% overhead reduction)
;;;   - Sample rate 0.01: Check ~1% of calls (high-volume production)
;;;
;;; Statistical monitoring tracks:
;;;   - Number of checks performed
;;;   - Number of violations detected
;;;   - Estimated violation rate
;;;
;;; This is useful for production systems where full contract checking
;;; is too expensive, but you still want visibility into contract health.

(doc *statistical-sample-rate* 'type 'Real)
(doc *statistical-sample-rate* 'description "Probability of checking a contract in statistical mode (0.0 to 1.0).")
(doc *statistical-sample-rate* 'export #t)
(define *statistical-sample-rate* 0.1)

(doc *statistical-check-count* 'type 'Nat)
(doc *statistical-check-count* 'description "Number of contract checks performed in statistical mode.")
(doc *statistical-check-count* 'export #t)
(define *statistical-check-count* 0)

(doc *statistical-violation-count* 'type 'Nat)
(doc *statistical-violation-count* 'description "Number of contract violations detected in statistical mode.")
(doc *statistical-violation-count* 'export #t)
(define *statistical-violation-count* 0)

(define (set-statistical-sample-rate! rate)
  (doc 'type (-> Real Void))
  (doc 'description "Set the probability of checking contracts in statistical mode.")
  (doc 'export #t)
  (if (and (number? rate) (>= rate 0.0) (<= rate 1.0))
      (set! *statistical-sample-rate* rate)
      (error 'set-statistical-sample-rate!
             "Sample rate must be a number between 0.0 and 1.0")))

(define (get-statistical-sample-rate)
  (doc 'type (-> Real))
  (doc 'description "Get the current statistical sampling rate.")
  (doc 'export #t)
  *statistical-sample-rate*)

(define (reset-statistical-counters!)
  (doc 'type (-> Void))
  (doc 'description "Reset the statistical monitoring counters.")
  (doc 'export #t)
  (set! *statistical-check-count* 0)
  (set! *statistical-violation-count* 0))

(define (statistical-violation-rate)
  (doc 'type (-> Real))
  (doc 'description "Calculate the observed violation rate (violations / checks).
Returns 0.0 if no checks have been performed.")
  (doc 'export #t)
  (if (= *statistical-check-count* 0)
      0.0
      (/ *statistical-violation-count* *statistical-check-count*)))

(define (should-check-statistically?)
  (doc 'type (-> Boolean))
  (doc 'description "Decide whether to check this contract based on sample rate.
Uses (random 1.0) < sample-rate for probabilistic sampling.")
  (doc 'export #f)
  (< (random 1.0) *statistical-sample-rate*))

(define (record-statistical-check!)
  (doc 'type (-> Void))
  (doc 'description "Record that a statistical contract check was performed.")
  (doc 'export #f)
  (set! *statistical-check-count* (+ *statistical-check-count* 1)))

(define (record-statistical-violation!)
  (doc 'type (-> Void))
  (doc 'description "Record that a contract violation was detected in statistical mode.")
  (doc 'export #f)
  (set! *statistical-violation-count* (+ *statistical-violation-count* 1)))

;; Wire up the forward reference for centralized violation handling
(set! *record-violation-if-statistical* record-statistical-violation!)

(define (statistical-monitoring-report)
  (doc 'type (-> String))
  (doc 'description "Generate a detailed report of statistical monitoring metrics.")
  (doc 'export #t)
  (let ([rate (statistical-violation-rate)]
        [checks *statistical-check-count*]
        [violations *statistical-violation-count*]
        [sample-rate *statistical-sample-rate*])
    (string-append
     "Statistical Contract Monitoring Report\n"
     "======================================\n"
     "Sample rate: " (number->string (* sample-rate 100)) "%\n"
     "Checks performed: " (number->string checks) "\n"
     "Violations detected: " (number->string violations) "\n"
     "Violation rate: " (number->string (* rate 100)) "%\n"
     (if (and (> checks 0) (> sample-rate 0))
         (string-append
          "Estimated total violations: "
          (number->string (inexact->exact (round (/ violations sample-rate))))
          " (if all calls were checked)\n")
         ""))))

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
