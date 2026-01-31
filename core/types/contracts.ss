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
         (contract? (caddr c)))]
   [(eq? (car c) 'And)
    (andmap contract? (cdr c))]
   [(eq? (car c) 'Or)
    (andmap contract? (cdr c))]
   [(eq? (car c) 'Not)
    (and (= (length c) 2)
         (contract? (cadr c)))]
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

(doc 'section 'higher-order-wrapping)

(define (contract-wrap contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Wrap a value with a contract - flat contracts check immediately, function contracts wrap.")
  (doc 'export #t)
  (cond
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract)
        (and (pair? contract) (memq (car contract) '(And Or Not))))
    (check-flat contract value location)]
   [(function-contract? contract)
    (if (procedure? value)
        `(Ok ,(wrap-function contract value location 'caller))
        `(Err ,(make-blame 'callee location
                           "Expected a procedure for function contract"
                           value)))]
   [(dependent-contract? contract)
    `(Err ,(make-blame 'callee location
                       "Dependent contract wrapping not yet implemented"
                       value))]
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
          (error 'contract-violation
                 (format "~a: expected ~a arguments, got ~a"
                         location (length domain-contracts) (length args)))
          (let ([wrapped-args (check-and-wrap-args domain-contracts args location blame-party)])
            (if (eq? (car wrapped-args) 'Err)
                (error 'contract-violation (blame->string (cadr wrapped-args)))
                (let ([result (apply proc (cadr wrapped-args))])
                  (let ([checked-result (contract-wrap-with-blame
                                          range-contract result location
                                          (if (eq? blame-party 'caller) 'callee 'caller))])
                    (if (eq? (car checked-result) 'Err)
                        (error 'contract-violation (blame->string (cadr checked-result)))
                        (cadr checked-result))))))))))

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
   [(or (eq? contract 'Any)
        (eq? contract 'None)
        (flat-contract? contract)
        (and (pair? contract) (memq (car contract) '(And Or Not))))
    (let ([result (check-flat contract value location)])
      (if (eq? (car result) 'Err)
          (let ([b (cadr result)])
            `(Err ,(make-blame blame-party
                               (blame-location b)
                               (blame-message b)
                               (blame-value b))))
          result))]
   [(function-contract? contract)
    (if (procedure? value)
        `(Ok ,(wrap-function contract value location
                             (if (eq? blame-party 'caller) 'callee 'caller)))
        `(Err ,(make-blame blame-party location
                           "Expected a procedure for function contract"
                           value)))]
   [(dependent-contract? contract)
    `(Err ,(make-blame blame-party location
                       "Dependent contract wrapping not yet implemented"
                       value))]
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
        (error 'contract-violation (blame->string (cadr result)))
        (cadr result))))

(doc 'section 'chaperone-contracts)

;;; Chaperone-style contracts for higher-order elements in collections.
;;; Unlike flat contracts that check immediately, chaperones wrap the
;;; collection and apply element contracts lazily when elements are accessed.

(define (chaperone-list? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is a chaperoned list.")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'chaperone-list)))

(define (make-chaperone-list underlying elem-contract location)
  (doc 'type (-> List Contract Symbol ChaperoneList))
  (doc 'description "Create a chaperoned list that applies elem-contract on access.")
  (doc 'export #f)
  `(chaperone-list ,underlying ,elem-contract ,location))

(define (chaperone-list-underlying cl)
  (if (chaperone-list? cl) (cadr cl) cl))

(define (chaperone-list-contract cl)
  (if (chaperone-list? cl) (caddr cl) any/c))

(define (chaperone-list-location cl)
  (if (chaperone-list? cl) (cadddr cl) 'unknown))

(define (chaperone-car cl)
  (doc 'type (-> ChaperoneList Any))
  (doc 'description "Get first element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([underlying (chaperone-list-underlying cl)]
             [elem-contract (chaperone-list-contract cl)]
             [location (chaperone-list-location cl)]
             [elem (car underlying)])
        (apply-contract elem-contract elem location))
      (car cl)))

(define (chaperone-cdr cl)
  (doc 'type (-> ChaperoneList ChaperoneList))
  (doc 'description "Get rest of chaperoned list, preserving chaperone.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([underlying (chaperone-list-underlying cl)]
             [rest (cdr underlying)])
        (if (null? rest)
            '()
            (make-chaperone-list rest
                                 (chaperone-list-contract cl)
                                 (chaperone-list-location cl))))
      (cdr cl)))

(define (chaperone-list-ref cl n)
  (doc 'type (-> ChaperoneList Nat Any))
  (doc 'description "Get nth element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([underlying (chaperone-list-underlying cl)]
             [elem-contract (chaperone-list-contract cl)]
             [location (chaperone-list-location cl)]
             [elem (list-ref underlying n)])
        (apply-contract elem-contract elem location))
      (list-ref cl n)))

(define (chaperone-list->list cl)
  (doc 'type (-> ChaperoneList List))
  (doc 'description "Convert chaperoned list to regular list, checking all elements.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let ([underlying (chaperone-list-underlying cl)]
            [elem-contract (chaperone-list-contract cl)]
            [location (chaperone-list-location cl)])
        (map (lambda (elem) (apply-contract elem-contract elem location))
             underlying))
      cl))

(define (chaperone-list-length cl)
  (doc 'type (-> ChaperoneList Nat))
  (doc 'description "Get length of chaperoned list.")
  (doc 'export #t)
  (length (if (chaperone-list? cl)
              (chaperone-list-underlying cl)
              cl)))

(define (chaperone-list-null? cl)
  (doc 'type (-> ChaperoneList Boolean))
  (doc 'description "Check if chaperoned list is empty.")
  (doc 'export #t)
  (null? (if (chaperone-list? cl)
             (chaperone-list-underlying cl)
             cl)))

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
        (error 'contract-violation (blame->string (cadr result)))
        (cadr result))))
