;;; core/types/test-contracts.ss — Contract System Tests
;;;
;;; Tests for the contract primitives including flat contracts,
;;; function contracts, dependent contracts, and blame tracking.
;;;
;;; Run from project root: scheme --script core/types/test-contracts.ss

(load "core/types/contracts.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Contract Predicates
;;; ====
(test-section "Contract Predicates")
(test "any/c is a contract" #t (contract? any/c))
(test "none/c is a contract" #t (contract? none/c))
(test "flat contract is recognized" #t (contract? (flat integer?)))
(test "function contract is recognized" #t (contract? (->c (list nat/c) pos/c)))
(test "dependent contract is recognized" #t (contract? (dep '(x) nat/c)))
(test "and/c is recognized" #t (contract? (and/c nat/c pos/c)))
(test "or/c is recognized" #t (contract? (or/c nat/c string/c)))
(test "not/c is recognized" #t (contract? (not/c nat/c)))
(test "invalid contract is rejected" #f (contract? '(invalid stuff)))

;;; ====
;;; Flat Contract Constructors
;;; ====
(test-section "Flat Contract Constructors")
(test "flat creates flat contract" #t (flat-contract? (flat integer?)))
(test "nat/c is a flat contract" #t (flat-contract? nat/c))
(test "pos/c is a flat contract" #t (flat-contract? pos/c))
(test "int/c is a flat contract" #t (flat-contract? int/c))
(test "bool/c is a flat contract" #t (flat-contract? bool/c))
(test "string/c is a flat contract" #t (flat-contract? string/c))

;;; ====
;;; Function Contract Constructors
;;; ====
(test-section "Function Contract Constructors")
(test "->c creates function contract" #t (function-contract? (->c (list nat/c) pos/c)))
(test "function contract domain extraction"
      #t
      (equal? (function-contract-domain (->c (list nat/c pos/c) bool/c))
              (list nat/c pos/c)))
(test "function contract range extraction"
      #t
      (eq? (function-contract-range (->c (list nat/c) pos/c)) pos/c))

;;; ====
;;; Dependent Contract Constructors
;;; ====
(test-section "Dependent Contract Constructors")
(test "dep creates dependent contract" #t (dependent-contract? (dep '(x) nat/c)))
(test "dependent contract vars extraction"
      #t
      (equal? (dependent-contract-vars (dep '(x y) nat/c)) '(x y)))
(test "dependent contract body extraction"
      #t
      (eq? (dependent-contract-body (dep '(x) pos/c)) pos/c))

;;; ====
;;; Blame Tracking
;;; ====
(test-section "Blame Tracking")
(test "make-blame creates blame record" #t (blame? (make-blame 'caller 'test "message" 42)))
(test "blame-party extraction" 'caller (blame-party (make-blame 'caller 'test "msg" 42)))
(test "blame-location extraction" 'test (blame-location (make-blame 'caller 'test "msg" 42)))
(test "blame-message extraction" "error msg" (blame-message (make-blame 'caller 'test "error msg" 42)))
(test "blame-value extraction" 42 (blame-value (make-blame 'caller 'test "msg" 42)))
(test "flip-blame swaps caller and callee" 'callee (blame-party (flip-blame (make-blame 'caller 'test "msg" 42))))
(test "flip-blame preserves location" 'test (blame-location (flip-blame (make-blame 'caller 'test "msg" 42))))

;;; ====
;;; Flat Contract Checking
;;; ====
(test-section "Flat Contract Checking")
(test "any/c accepts all values" 'Ok (car (check-flat any/c 42 'test)))
(test "none/c rejects all values" 'Err (car (check-flat none/c 42 'test)))
(test "nat/c accepts natural numbers" 'Ok (car (check-flat nat/c 5 'test)))
(test "nat/c rejects negative numbers" 'Err (car (check-flat nat/c -1 'test)))
(test "pos/c accepts positive numbers" 'Ok (car (check-flat pos/c 3.14 'test)))
(test "pos/c rejects zero" 'Err (car (check-flat pos/c 0 'test)))
(test "int/c accepts integers" 'Ok (car (check-flat int/c -42 'test)))
(test "int/c rejects non-integers" 'Err (car (check-flat int/c 3.14 'test)))
(test "bool/c accepts booleans" 'Ok (car (check-flat bool/c #t 'test)))
(test "bool/c rejects non-booleans" 'Err (car (check-flat bool/c 42 'test)))
(test "string/c accepts strings" 'Ok (car (check-flat string/c "hello" 'test)))
(test "string/c rejects non-strings" 'Err (car (check-flat string/c 'symbol 'test)))

;;; ====
;;; Contract Combinators - And
;;; ====
(test-section "Contract Combinators - And")
(test "and/c with all satisfied" 'Ok (car (check-flat (and/c nat/c pos/c) 5 'test)))
(test "and/c with first failing" 'Err (car (check-flat (and/c nat/c pos/c) -1 'test)))
(test "and/c with second failing" 'Err (car (check-flat (and/c nat/c pos/c) 0 'test)))
(test "and/c with both failing" 'Err (car (check-flat (and/c string/c int/c) 42 'test)))

;;; ====
;;; Contract Combinators - Or
;;; ====
(test-section "Contract Combinators - Or")
(test "or/c with first satisfied" 'Ok (car (check-flat (or/c nat/c string/c) 5 'test)))
(test "or/c with second satisfied" 'Ok (car (check-flat (or/c nat/c string/c) "hello" 'test)))
(test "or/c with neither satisfied" 'Err (car (check-flat (or/c nat/c string/c) 'symbol 'test)))
(test "or/c with both satisfied uses first" 'Ok (car (check-flat (or/c int/c nat/c) 5 'test)))

;;; ====
;;; Contract Combinators - Not
;;; ====
(test-section "Contract Combinators - Not")
(test "not/c inverts success to failure" 'Err (car (check-flat (not/c nat/c) 5 'test)))
(test "not/c inverts failure to success" 'Ok (car (check-flat (not/c nat/c) -1 'test)))
(test "not/c with string success becomes failure" 'Err (car (check-flat (not/c string/c) "hello" 'test)))
(test "not/c with string failure becomes success" 'Ok (car (check-flat (not/c string/c) 42 'test)))

;;; ====
;;; Higher-Order Contract Combinators
;;; ====
(test-section "Higher-Order Contract Combinators - or/c")

;; or/c with function contracts
(define nat->nat-contract (->c (list nat/c) nat/c))
(define string->string-contract (->c (list string/c) string/c))
(define either-fn-contract (or/c nat->nat-contract string->string-contract))

;; Wrap a function that works with nats
(define wrapped-add1
  (let ([result (contract-wrap either-fn-contract (lambda (x) (+ x 1)) 'add1)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "or/c wraps function contract" #t (procedure? wrapped-add1))
(test "or/c wrapped function works with first alternative" 6 (wrapped-add1 5))

;; Wrap a function that works with strings
(define wrapped-upcase
  (let ([result (contract-wrap either-fn-contract
                               (lambda (s) (string-append s "!"))
                               'upcase)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "or/c wraps string function" #t (procedure? wrapped-upcase))
(test "or/c wrapped function works with second alternative" "hello!"
      (wrapped-upcase "hello"))

;; Test or/c failure - neither contract matches
(define (test-or-c-no-match)
  (guard (e [else #t])
    ;; Function takes nat but returns string - neither contract works
    (let* ([bad-fn (lambda (x) "not a number")]
           [result (contract-wrap either-fn-contract bad-fn 'bad)]
           [wrapped (if (eq? (car result) 'Ok) (cadr result) #f)])
      (wrapped 5))  ; Call it
    #f))
(test "or/c fails when no contract matches at call time" #t (test-or-c-no-match))

(test-section "Higher-Order Contract Combinators - and/c")

;; and/c with function contracts - both must be satisfied
(define int->int-contract (->c (list int/c) int/c))
(define nat->pos-contract (->c (list nat/c) pos/c))
;; A function satisfying both must: take nat (subset of int), return pos (subset of int)

;; Test that and/c checks ALL domains, not just the first
;; pos/c requires x > 0, (between/c 0 10) allows 0 but caps at 10
(define pos->nat-contract (->c (list pos/c) nat/c))
(define range-limited-contract (->c (list (between/c 0 10)) nat/c))
;; The intersection: must be > 0 AND <= 10, i.e., (1, 10]
(define both-domain-contract (and/c pos->nat-contract range-limited-contract))

(define wrapped-both-domains
  (let ([result (contract-wrap both-domain-contract (lambda (x) x) 'id)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "and/c wraps with multiple domain contracts" #t (procedure? wrapped-both-domains))
(test "and/c passes when both domains satisfied" 5 (wrapped-both-domains 5))

;; Test that 0 fails (pos/c requires > 0)
(define (test-and-c-first-domain-fails)
  (guard (e [else #t])
    (wrapped-both-domains 0)  ; Fails pos/c (first contract)
    #f))
(test "and/c fails when first domain fails" #t (test-and-c-first-domain-fails))

;; Test that 15 fails (between/c 0 10 requires <= 10)
(define (test-and-c-second-domain-fails)
  (guard (e [else #t])
    (wrapped-both-domains 15)  ; Passes pos/c but fails between/c (second contract)
    #f))
(test "and/c fails when second domain fails" #t (test-and-c-second-domain-fails))
(define both-fn-contract (and/c nat->nat-contract nat->pos-contract))

;; Wrap a function that satisfies both (takes nat, returns positive)
(define wrapped-add1-both
  (let ([result (contract-wrap both-fn-contract (lambda (x) (+ x 1)) 'add1)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "and/c wraps function contract" #t (procedure? wrapped-add1-both))
(test "and/c wrapped function works" 6 (wrapped-add1-both 5))

;; Test and/c with non-procedure value and mixed contracts
(define mixed-and-contract (and/c nat/c pos/c))
(test "and/c with flat contracts still works" 'Ok (car (contract-wrap mixed-and-contract 5 'test)))
(test "and/c flat fails when one fails" 'Err (car (contract-wrap mixed-and-contract 0 'test)))

(test-section "Higher-Order Contract Combinators - not/c")

;; not/c with function contract - value must NOT satisfy it
(define not-nat-fn-contract (not/c nat->nat-contract))

;; A string function does NOT satisfy nat->nat
(define wrapped-string-fn
  (let ([result (contract-wrap not-nat-fn-contract
                               (lambda (s) (string-append s "!"))
                               'string-fn)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "not/c wraps function that doesn't match inner" #t (procedure? wrapped-string-fn))
;; When called with a string (not a nat), domain check fails, so not/c passes
(test "not/c function works when domain doesn't match" "hi!"
      (wrapped-string-fn "hi"))

;; Test not/c violation - when function DOES satisfy the negated contract
(define (test-not-c-violation)
  (guard (e [else #t])
    (let* ([result (contract-wrap not-nat-fn-contract (lambda (x) (+ x 1)) 'add1)]
           [wrapped (if (eq? (car result) 'Ok) (cadr result) #f)])
      ;; This function satisfies nat->nat, so not/c should fail
      (wrapped 5))
    #f))
(test "not/c fails when function satisfies negated contract" #t (test-not-c-violation))

;;; ====
;;; Advanced Combinators
;;; ====
(test-section "Advanced Combinators")
(test "listof empty list" 'Ok (car (check-flat (listof nat/c) '() 'test)))
(test "listof with valid elements" 'Ok (car (check-flat (listof nat/c) '(1 2 3) 'test)))
(test "listof with invalid element" 'Err (car (check-flat (listof nat/c) '(1 -2 3) 'test)))
(test "listof rejects non-list" 'Err (car (check-flat (listof nat/c) 42 'test)))
(test "vectorof empty vector" 'Ok (car (check-flat (vectorof nat/c) '#() 'test)))
(test "vectorof with valid elements" 'Ok (car (check-flat (vectorof nat/c) '#(1 2 3) 'test)))
(test "vectorof with invalid element" 'Err (car (check-flat (vectorof nat/c) '#(1 -2 3) 'test)))
(test "between/c in range" 'Ok (car (check-flat (between/c 0 10) 5 'test)))
(test "between/c below range" 'Err (car (check-flat (between/c 0 10) -1 'test)))
(test "between/c above range" 'Err (car (check-flat (between/c 0 10) 11 'test)))
(test "between/c at lower bound" 'Ok (car (check-flat (between/c 0 10) 0 'test)))
(test "between/c at upper bound" 'Ok (car (check-flat (between/c 0 10) 10 'test)))
(test "one-of/c matching value" 'Ok (car (check-flat (one-of/c 'red 'green 'blue) 'green 'test)))
(test "one-of/c non-matching value" 'Err (car (check-flat (one-of/c 'red 'green 'blue) 'yellow 'test)))

;;; ====
;;; Contract Wrapping
;;; ====
(test-section "Contract Wrapping")
(test "wrap creates wrapped value" #t (wrapped? (wrap nat/c 42 'test)))
(test "unwrap extracts value" 42 (unwrap (wrap nat/c 42 'test)))
(test "wrapped-contract extracts contract" #t (eq? (wrapped-contract (wrap nat/c 42 'test)) nat/c))
(test "wrapped-location extracts location" 'test (wrapped-location (wrap nat/c 42 'test)))
(test "unwrap on non-wrapped returns value" 42 (unwrap 42))
(test "wrapped-contract on non-wrapped returns any/c" any/c (wrapped-contract 42))

;;; ====
;;; Contract Display
;;; ====
(test-section "Contract Display")
(test "any/c display" "any/c" (contract->string any/c))
(test "none/c display" "none/c" (contract->string none/c))
(test "flat contract display" "(flat ...)" (contract->string (flat integer?)))
(test "function contract display" #t (string? (contract->string (->c (list nat/c) pos/c))))
(test "dependent contract display" #t (string? (contract->string (dep '(x) nat/c))))
(test "invariant contract display" "(invariant/c <base> <property>)"
      (contract->string sorted-list/c))
(test "blame display is string" #t (string? (blame->string (make-blame 'caller 'test "msg" 42))))

;;; ====
;;; Blame in Check Results
;;; ====
(test-section "Blame in Check Results")
(test "failed nat/c produces callee blame"
      #t
      (let ([result (check-flat nat/c -1 'test-location)])
           (and (eq? (car result) 'Err)
                (blame? (cadr result))
                (eq? (blame-party (cadr result)) 'callee))))
(test "blame includes location"
      'my-function
      (let ([result (check-flat nat/c -1 'my-function)])
           (blame-location (cadr result))))
(test "blame includes offending value"
      -1
      (let ([result (check-flat nat/c -1 'test)])
           (blame-value (cadr result))))
(test "none/c blame includes message"
      #t
      (let ([result (check-flat none/c 42 'test)])
           (string? (blame-message (cadr result)))))

;;; ====
;;; Higher-Order Contract Wrapping
;;; ====
(test-section "Higher-Order Contract Wrapping - Flat")

;; contract-wrap with flat contracts
(test "contract-wrap any/c succeeds" 'Ok (car (contract-wrap any/c 42 'test)))
(test "contract-wrap nat/c succeeds" 'Ok (car (contract-wrap nat/c 5 'test)))
(test "contract-wrap nat/c fails" 'Err (car (contract-wrap nat/c -1 'test)))
(test "contract-wrap returns value on success" 42 (cadr (contract-wrap any/c 42 'test)))

(test-section "Higher-Order Contract Wrapping - First-Order Functions")

;; Simple function contract: (-> (nat) nat)
(define add1-contract (->c (list nat/c) nat/c))

;; Wrap a valid function
(define wrapped-add1
  (let ([result (contract-wrap add1-contract (lambda (x) (+ x 1)) 'add1)])
    (if (eq? (car result) 'Ok)
        (cadr result)
        #f)))

(test "wrap-function succeeds for procedure" #t (procedure? wrapped-add1))
(test "wrapped function works with valid input" 6 (wrapped-add1 5))

;; Test domain violation (blame caller)
(define (test-domain-violation)
  (guard (e [else #t])  ; any exception means violation was caught
    (wrapped-add1 -1)  ; -1 violates nat/c
    #f))  ; no exception = test failed
(test "domain violation caught" #t (test-domain-violation))

;; Function that returns wrong type
(define bad-return-fn
  (let ([result (contract-wrap add1-contract (lambda (x) "not a number") 'bad-fn)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(define (test-range-violation)
  (guard (e [else #t])  ; any exception means violation was caught
    (bad-return-fn 5)  ; returns "not a number", violates nat/c range
    #f))  ; no exception = test failed
(test "range violation caught" #t (test-range-violation))

(test-section "Higher-Order Contract Wrapping - Higher-Order Functions")

;; Higher-order contract: ((nat -> nat) -> nat)
;; A function that takes a function and returns a nat
(define ho-contract
  (->c (list (->c (list nat/c) nat/c)) nat/c))

;; apply-twice: applies f to 5, then to the result
(define apply-twice
  (lambda (f) (f (f 5))))

(define wrapped-apply-twice
  (let ([result (contract-wrap ho-contract apply-twice 'apply-twice)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "HO wrap succeeds" #t (procedure? wrapped-apply-twice))
(test "HO function works with valid callback" 7 (wrapped-apply-twice (lambda (x) (+ x 1))))

;; Test: caller provides callback that violates its own contract (returns wrong type)
;; The callback's range contract is violated - blame should go to CALLER
;; (because caller provided a bad callback)
(define (test-ho-callback-range-violation)
  (guard (e [else #t])  ; any exception means violation was caught
    (wrapped-apply-twice (lambda (x) "bad"))  ; callback returns string, not nat
    #f))  ; no exception = test failed
(test "HO callback range violation caught" #t (test-ho-callback-range-violation))

;; Test: callee calls the callback with wrong argument type
;; This would be a callee fault - they misused the callback
;; We need a function that deliberately calls its callback wrong
(define misuse-callback
  (lambda (f) (f "not a number")))  ; calls f with string instead of nat

(define wrapped-misuse
  (let ([result (contract-wrap ho-contract misuse-callback 'misuse)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(define (test-ho-misuse-violation)
  (guard (e [else #t])  ; any exception means violation was caught
    (wrapped-misuse (lambda (x) (+ x 1)))  ; good callback, but callee misuses it
    #f))  ; no exception = test failed
(test "HO callee misuse caught" #t (test-ho-misuse-violation))

(test-section "Higher-Order Contract Wrapping - Multi-Argument")

;; Contract for binary function: (nat nat -> nat)
(define binary-contract (->c (list nat/c nat/c) nat/c))

(define wrapped-add
  (let ([result (contract-wrap binary-contract + 'add)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "multi-arg wrap succeeds" #t (procedure? wrapped-add))
(test "multi-arg function works" 8 (wrapped-add 3 5))

;; Test arity mismatch
(define (test-arity-violation)
  (guard (e [else #t])  ; any exception means violation was caught
    (wrapped-add 5)  ; only 1 argument, expects 2
    #f))  ; no exception = test failed
(test "arity violation caught" #t (test-arity-violation))

(test-section "Higher-Order Contract Wrapping - apply-contract convenience")

(test "apply-contract returns value on success" 42 (apply-contract nat/c 42 'test))
(test "apply-contract with wrapped fn" 6 ((apply-contract add1-contract (lambda (x) (+ x 1)) 'f) 5))

(define (test-apply-contract-failure)
  (guard (e [else #t])
    (apply-contract nat/c "not a nat" 'test)
    #f))
(test "apply-contract raises on failure" #t (test-apply-contract-failure))

;;; ====
;;; Chaperone Contracts
;;; ====
(test-section "Chaperone List Contracts")

;; Test chaperone list creation
(define fn-list-contract (listof/c (->c (list nat/c) nat/c)))
(test "listof/c detects HO contract needs chaperone" #t (chaperone-listof-contract? fn-list-contract))

;; Test that flat element contracts don't create chaperones
(define flat-list-contract (listof/c nat/c))
(test "listof/c uses flat for flat elements" #t (flat-contract? flat-list-contract))

;; Create a chaperoned list of functions
(define fn-list (list (lambda (x) (+ x 1))
                      (lambda (x) (* x 2))))
(define chaperoned-fn-list
  (apply-contract/c fn-list-contract fn-list 'fn-list))

(test "apply-contract/c creates chaperone list" #t (chaperone-list? chaperoned-fn-list))

;; Test chaperone list operations
(test "chaperone-list-length" 2 (chaperone-list-length chaperoned-fn-list))
(test "chaperone-list-null? on non-empty" #f (chaperone-list-null? chaperoned-fn-list))

;; Test element access - elements should be wrapped with contracts
(define first-fn (chaperone-car chaperoned-fn-list))
(test "chaperone-car returns procedure" #t (procedure? first-fn))
(test "chaperoned function works" 6 (first-fn 5))

;; Test cdr
(define rest-list (chaperone-cdr chaperoned-fn-list))
(test "chaperone-cdr returns chaperone list" #t (chaperone-list? rest-list))
(test "chaperone-cdr length" 1 (chaperone-list-length rest-list))

;; Test list-ref
(define second-fn (chaperone-list-ref chaperoned-fn-list 1))
(test "chaperone-list-ref returns procedure" #t (procedure? second-fn))
(test "chaperoned function from list-ref" 10 (second-fn 5))

;; Test that element contract is enforced
(define (test-chaperone-element-contract-violation)
  (guard (e [else #t])
    ((chaperone-car chaperoned-fn-list) -1)  ; violates nat/c domain
    #f))
(test "chaperone enforces element contract domain" #t (test-chaperone-element-contract-violation))

(test-section "Chaperone Vector Contracts")

;; Test chaperone vector creation
(define fn-vec-contract (vectorof/c (->c (list nat/c) nat/c)))
(test "vectorof/c detects HO contract" #t (chaperone-vectorof-contract? fn-vec-contract))

;; Test flat element contracts
(define flat-vec-contract (vectorof/c nat/c))
(test "vectorof/c uses flat for flat elements" #t (flat-contract? flat-vec-contract))

;; Create a chaperoned vector of functions
(define fn-vec (vector (lambda (x) (+ x 10))
                       (lambda (x) (- x 1))))
(define chaperoned-fn-vec
  (apply-contract/c fn-vec-contract fn-vec 'fn-vec))

(test "apply-contract/c creates chaperone vector" #t (chaperone-vector? chaperoned-fn-vec))
(test "chaperone-vector-length" 2 (chaperone-vector-length chaperoned-fn-vec))

;; Test element access
(define vec-first-fn (chaperone-vector-ref chaperoned-fn-vec 0))
(test "chaperone-vector-ref returns procedure" #t (procedure? vec-first-fn))
(test "chaperoned vector fn works" 15 (vec-first-fn 5))

;; Test that element contract is enforced
(define (test-chaperone-vec-element-violation)
  (guard (e [else #t])
    ((chaperone-vector-ref chaperoned-fn-vec 0) "not a nat")
    #f))
(test "chaperone vector enforces element contract" #t (test-chaperone-vec-element-violation))

(test-section "Chaperone List to Regular List Conversion")

;; Test converting chaperone list back to regular list
(define simple-fn-list (list (lambda (x) x)))
(define simple-chaperoned (apply-contract/c fn-list-contract simple-fn-list 'test))
(define converted-list (chaperone-list->list simple-chaperoned))

(test "chaperone-list->list returns list" #t (list? converted-list))
(test "converted list length" 1 (length converted-list))
(test "converted list element is procedure" #t (procedure? (car converted-list)))

(test-section "Nested Chaperone Contracts")

;; Test listof with nested HO contracts: list of (list of functions)
;; This is more complex - inner lists should also be chaperoned
(define nested-contract (listof/c (listof/c (->c (list nat/c) nat/c))))
(test "nested listof detects HO" #t (chaperone-listof-contract? nested-contract))

(test-section "Chaperone Contracts in Function Contracts")

;; Test that chaperone contracts can be used in function domains/ranges
;; Note: Our chaperones are wrapper data structures, not runtime impersonators.
;; Functions receiving chaperoned values must use chaperone-car/cdr/ref.
(define fn-taking-fn-list-contract
  (->c (list (listof/c (->c (list nat/c) nat/c))) nat/c))

;; Function that knows to use chaperone accessors
(define apply-first-fn-chaperone-aware
  (lambda (fn-list)
    (if (chaperone-list-null? fn-list)
        0
        ((chaperone-car fn-list) 5))))

(define wrapped-apply-first
  (let ([result (contract-wrap fn-taking-fn-list-contract apply-first-fn-chaperone-aware 'test)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "chaperone in function domain wraps" #t (procedure? wrapped-apply-first))
(test "chaperone in function domain works" 6 (wrapped-apply-first (list (lambda (x) (+ x 1)))))

;;; ====
;;; Contract Checking Modes
;;; ====
(test-section "Contract Checking Modes - Mode Validation")

(test "runtime is valid mode" #t (contract-mode? 'runtime))
(test "compile-time is valid mode" #t (contract-mode? 'compile-time))
(test "test-only is valid mode" #t (contract-mode? 'test-only))
(test "doc-only is valid mode" #t (contract-mode? 'doc-only))
(test "invalid mode rejected" #f (contract-mode? 'invalid))
(test "non-symbol rejected" #f (contract-mode? 42))

(test-section "Contract Checking Modes - Default State")

;; Save current state for restoration
(define saved-mode (get-contract-mode))
(define saved-test-context (in-test-context?))

(test "default mode is runtime" 'runtime (get-contract-mode))
(test "default not in test context" #f (in-test-context?))
(test "contracts enabled by default" #t (contracts-enabled?))

(test-section "Contract Checking Modes - Mode Switching")

;; Test mode switching
(set-contract-mode! 'doc-only)
(test "mode switches to doc-only" 'doc-only (get-contract-mode))
(test "contracts disabled in doc-only" #f (contracts-enabled?))

(set-contract-mode! 'compile-time)
(test "mode switches to compile-time" 'compile-time (get-contract-mode))
(test "contracts disabled in compile-time" #f (contracts-enabled?))

(set-contract-mode! 'test-only)
(test "mode switches to test-only" 'test-only (get-contract-mode))
(test "contracts disabled in test-only outside test context" #f (contracts-enabled?))

;; Enable test context
(enter-test-context!)
(test "in test context after enter" #t (in-test-context?))
(test "contracts enabled in test-only with test context" #t (contracts-enabled?))

(exit-test-context!)
(test "out of test context after exit" #f (in-test-context?))
(test "contracts disabled in test-only outside test context" #f (contracts-enabled?))

;; Back to runtime
(set-contract-mode! 'runtime)
(test "mode back to runtime" 'runtime (get-contract-mode))
(test "contracts enabled in runtime" #t (contracts-enabled?))

(test-section "Contract Checking Modes - Mode-Aware Functions")

;; Test apply-contract/mode in runtime mode
(test "apply-contract/mode works in runtime" 42 (apply-contract/mode nat/c 42 'test))

;; Test that mode-aware function respects doc-only
(set-contract-mode! 'doc-only)
(test "apply-contract/mode passes through in doc-only" "not a nat"
      (apply-contract/mode nat/c "not a nat" 'test))

;; Back to runtime for violation test
(set-contract-mode! 'runtime)
(define (test-mode-aware-violation)
  (guard (e [else #t])
    (apply-contract/mode nat/c "not a nat" 'test)
    #f))
(test "apply-contract/mode catches violation in runtime" #t (test-mode-aware-violation))

(test-section "Contract Checking Modes - Scoped Mode Changes")

;; Test call-with-contract-mode
(set-contract-mode! 'runtime)
(test "call-with-contract-mode temporarily changes mode"
      'doc-only
      (call-with-contract-mode 'doc-only (lambda () (get-contract-mode))))
(test "mode restored after call-with-contract-mode" 'runtime (get-contract-mode))

;; Test call-without-contracts
(test "call-without-contracts disables checking"
      "invalid"
      (call-without-contracts
        (lambda () (apply-contract/mode nat/c "invalid" 'test))))
(test "mode restored after call-without-contracts" 'runtime (get-contract-mode))

;; Test call-with-test-context
(set-contract-mode! 'test-only)
(test "test-only disabled outside test context" #f (contracts-enabled?))
(test "call-with-test-context enables contracts"
      #t
      (call-with-test-context (lambda () (contracts-enabled?))))
(test "test context restored" #f (in-test-context?))

;; Test scoped mode with exception
(set-contract-mode! 'runtime)
(guard (e [else #t])
  (call-with-contract-mode 'doc-only
    (lambda () (error 'test "intentional"))))
(test "mode restored after exception" 'runtime (get-contract-mode))

(test-section "Contract Checking Modes - check-flat/mode and contract-wrap/mode")

;; Test check-flat/mode
(set-contract-mode! 'runtime)
(test "check-flat/mode checks in runtime" 'Ok (car (check-flat/mode nat/c 5 'test)))
(test "check-flat/mode fails in runtime" 'Err (car (check-flat/mode nat/c -1 'test)))

(set-contract-mode! 'doc-only)
(test "check-flat/mode passes in doc-only" 'Ok (car (check-flat/mode nat/c -1 'test)))
(test "check-flat/mode returns value in doc-only" -1 (cadr (check-flat/mode nat/c -1 'test)))

;; Test contract-wrap/mode
(set-contract-mode! 'runtime)
(test "contract-wrap/mode wraps in runtime" #t
      (let ([result (contract-wrap/mode add1-contract (lambda (x) (+ x 1)) 'test)])
        (and (eq? (car result) 'Ok)
             (procedure? (cadr result)))))

(set-contract-mode! 'doc-only)
(test "contract-wrap/mode returns unwrapped in doc-only" #t
      (let ([result (contract-wrap/mode add1-contract (lambda (x) x) 'test)])
        (and (eq? (car result) 'Ok)
             (procedure? (cadr result))
             ;; The function should be the original, not wrapped
             (eq? ((cadr result) 5) 5))))

(test-section "Contract Checking Modes - Status Reporting")

(set-contract-mode! 'runtime)
(test "contract-mode->string for runtime" "runtime (always check)"
      (contract-mode->string 'runtime))
(test "contract-mode->string for compile-time" "compile-time (static verification only)"
      (contract-mode->string 'compile-time))
(test "contract-status returns string" #t (string? (contract-status)))

(test-section "Contract Checking Modes - Compile-Time Verifier Hook")

(test "no verifier by default" #f *compile-time-verifier*)
(test "verify-contract-statically fails without verifier"
      'Err
      (car (verify-contract-statically nat/c 'Nat 'test)))

;; Test that non-procedure verifier is rejected
(define (test-verifier-must-be-procedure)
  (guard (e [else #t])
    (set-compile-time-verifier! "not a procedure")
    #f))
(test "verifier must be a procedure" #t (test-verifier-must-be-procedure))

;; Register a simple verifier
(set-compile-time-verifier!
  (lambda (contract type)
    (if (and (eq? contract nat/c) (eq? type 'Nat))
        '(Ok ())
        '(Err "Type mismatch"))))
(test "verifier registered" #t (procedure? *compile-time-verifier*))
(test "static verification succeeds" 'Ok
      (car (verify-contract-statically nat/c 'Nat 'test)))
(test "static verification fails on mismatch" 'Err
      (car (verify-contract-statically nat/c 'String 'test)))

;; Clean up verifier
(set! *compile-time-verifier* #f)

;; Restore original state
(set! *contract-mode* saved-mode)
(set! *in-test-context* saved-test-context)

;;; ====
;;; Blame Precision Tests
;;; ====
(test-section "Blame Precision - and/c Domain")
;; Test that and/c domain failures include contract index and description
(define precise-pos/c (->c (list pos/c) any/c))
(define precise-even/c (->c (list (flat (lambda (x) (and (integer? x) (even? x))))) any/c))
(define precise-and-contract (and/c precise-pos/c precise-even/c))

(define precise-wrapped
  (let ([result (contract-wrap precise-and-contract (lambda (x) x) 'blame-test)])
    (if (eq? (car result) 'Ok)
        (cadr result)
        #f)))

(test "precise and/c wraps function" #t (procedure? precise-wrapped))

;; Call with value that fails second domain (3 is pos but not even)
(define and-c-domain-error
  (guard (ex [else (condition-message ex)])
    (precise-wrapped 3)
    "no error"))

(test "and/c domain error includes contract #" #t
      (and (string? and-c-domain-error)
           (string-contains? and-c-domain-error "#")))

(test-section "Blame Precision - or/c No Match")
;; Test that or/c no-match errors list what was tried
(define or-pos/c (->c (list pos/c) any/c))
(define or-string/c (->c (list string/c) any/c))
(define precise-or-contract (or/c or-pos/c or-string/c))

(define precise-or-wrapped
  (let ([result (contract-wrap precise-or-contract (lambda (x) x) 'or-blame-test)])
    (if (eq? (car result) 'Ok)
        (cadr result)
        #f)))

(test "precise or/c wraps function" #t (procedure? precise-or-wrapped))

;; Call with value that matches neither (negative number, not string)
(define or-c-no-match-error
  (guard (ex [else (condition-message ex)])
    (precise-or-wrapped -5)
    "no error"))

(test "or/c no-match error includes tried contracts" #t
      (and (string? or-c-no-match-error)
           (string-contains? or-c-no-match-error "tried")))

(test-section "Blame Precision - not/c")
;; Test that not/c violations include the contract description
(define not-contract-inner (->c (list nat/c) nat/c))
(define precise-not-contract (not/c not-contract-inner))

(define precise-not-wrapped
  (let ([result (contract-wrap precise-not-contract (lambda (x) x) 'not-blame-test)])
    (if (eq? (car result) 'Ok)
        (cadr result)
        #f)))

(test "precise not/c wraps function" #t (procedure? precise-not-wrapped))

;; Call with valid input that would satisfy the negated contract (triggers not/c violation)
(define not-c-violation-error
  (guard (ex [else (condition-message ex)])
    (precise-not-wrapped 5)
    "no error"))

(test "not/c violation error includes contract description" #t
      (and (string? not-c-violation-error)
           (string-contains? not-c-violation-error "->")))

;;; ====
;;; Dependent Contract Wrapping
;;; ====
(test-section "Dependent Contract Instantiation")

;; Test instantiate-dependent-contract with procedure body
(define dep-listof-n
  (dep '(n) (lambda (n) (listof-length n nat/c))))

(test "instantiate with procedure body" #t
      (contract? (instantiate-dependent-contract dep-listof-n '(3))))
(test "instantiated contract is flat" #t
      (flat-contract? (instantiate-dependent-contract dep-listof-n '(3))))

;; Test instantiation with multiple vars
(define dep-multi-var
  (dep '(x y) (lambda (x y) (between/c x y))))

(test "instantiate with multiple vars" #t
      (contract? (instantiate-dependent-contract dep-multi-var '(0 10))))

;; Test instantiation check - value passes
(let* ([instantiated (instantiate-dependent-contract dep-listof-n '(3))]
       [result (check-flat instantiated '(1 2 3) 'test)])
  (test "instantiated listof-length passes for correct length" 'Ok (car result)))

;; Test instantiation check - value fails (wrong length)
(let* ([instantiated (instantiate-dependent-contract dep-listof-n '(3))]
       [result (check-flat instantiated '(1 2) 'test)])
  (test "instantiated listof-length fails for wrong length" 'Err (car result)))

;; Test instantiation with static body (no vars used)
(define dep-static-body
  (dep '(unused) nat/c))

(test "instantiate with static body returns body" nat/c
      (instantiate-dependent-contract dep-static-body '(anything)))

(test-section "Dependent Function Contracts - Range Depends on Input")

;; Dependent function contract: result list must have length equal to input
;; (dep '(n) (lambda (n) (->c (list nat/c) (listof-length n nat/c))))
(define dep-fn-range-contract
  (dep '(n) (lambda (n) (->c (list nat/c) (listof-length n nat/c)))))

(test "dep function contract recognized" #t (dependent-contract? dep-fn-range-contract))

;; Function that returns a list of n zeros
(define (make-zeros n)
  (if (<= n 0)
      '()
      (cons 0 (make-zeros (- n 1)))))

;; Wrap the function
(define wrapped-make-zeros
  (let ([result (contract-wrap dep-fn-range-contract make-zeros 'make-zeros)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "dep function wraps successfully" #t (procedure? wrapped-make-zeros))
(test "dep function works with valid return" '(0 0 0) (wrapped-make-zeros 3))
(test "dep function works with zero" '() (wrapped-make-zeros 0))

;; Function that returns wrong length - should fail
(define (bad-make-zeros n)
  '(1 2))  ; Always returns 2 elements regardless of n

(define wrapped-bad-make-zeros
  (let ([result (contract-wrap dep-fn-range-contract bad-make-zeros 'bad-make-zeros)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(define (test-dep-range-violation)
  (guard (e [else #t])
    (wrapped-bad-make-zeros 5)  ; Expects 5 elements, gets 2
    #f))
(test "dep function range violation caught" #t (test-dep-range-violation))

(test-section "Dependent Function Contracts - Domain Depends on Earlier Arg")

;; Dependent function contract: second arg must be less than first
;; (dep '(x) (lambda (x) (->c (list nat/c (less-than/c x)) nat/c)))
(define dep-fn-domain-contract
  (dep '(x) (lambda (x) (->c (list nat/c (less-than/c x)) nat/c))))

(test "dep domain contract recognized" #t (dependent-contract? dep-fn-domain-contract))

;; Simple function that adds two numbers
(define wrapped-add-with-constraint
  (let ([result (contract-wrap dep-fn-domain-contract + 'constrained-add)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "dep domain wraps successfully" #t (procedure? wrapped-add-with-constraint))
(test "dep domain passes when constraint satisfied" 7 (wrapped-add-with-constraint 5 2))  ; 2 < 5
(test "dep domain passes at boundary" 9 (wrapped-add-with-constraint 5 4))  ; 4 < 5

;; Test violation: second arg not less than first
(define (test-dep-domain-violation)
  (guard (e [else #t])
    (wrapped-add-with-constraint 5 5)  ; 5 is NOT less than 5
    #f))
(test "dep domain violation caught (equal)" #t (test-dep-domain-violation))

(define (test-dep-domain-violation-greater)
  (guard (e [else #t])
    (wrapped-add-with-constraint 5 10)  ; 10 is NOT less than 5
    #f))
(test "dep domain violation caught (greater)" #t (test-dep-domain-violation-greater))

(test-section "Dependent Function Contracts - Multiple Dependent Vars")

;; Contract with two dependent vars: (->c (list nat/c nat/c) (between/c x y))
;; Return must be between first and second arg
(define dep-multi-var-contract
  (dep '(x y) (lambda (x y) (->c (list nat/c nat/c) (between/c x y)))))

(define (clamp-to-range low high)
  (max low (min high (+ low (quotient (- high low) 2)))))

(define wrapped-clamp
  (let ([result (contract-wrap dep-multi-var-contract clamp-to-range 'clamp)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "multi-var dep wraps successfully" #t (procedure? wrapped-clamp))
(test "multi-var dep passes when in range" 5 (wrapped-clamp 0 10))  ; 5 is between 0 and 10

;; Function that returns out of range
(define (bad-clamp low high)
  100)  ; Always returns 100

(define wrapped-bad-clamp
  (let ([result (contract-wrap dep-multi-var-contract bad-clamp 'bad-clamp)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(define (test-multi-var-range-violation)
  (guard (e [else #t])
    (wrapped-bad-clamp 0 10)  ; Expects 0-10, gets 100
    #f))
(test "multi-var dep range violation caught" #t (test-multi-var-range-violation))

(test-section "Dependent Contract Blame")

;; Test that blame is correctly assigned for dependent contracts
(define dep-blame-contract
  (dep '(n) (lambda (n) (->c (list nat/c) (listof-length n nat/c)))))

(define wrapped-for-blame
  (let ([result (contract-wrap dep-blame-contract (lambda (n) '(1)) 'blame-test)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

;; Range violation should blame callee
(define dep-blame-error
  (guard (ex [else (condition-message ex)])
    (wrapped-for-blame 3)  ; Expects 3 elements, function returns 1
    "no error"))

(test "dep contract violation produces error message" #t
      (string? dep-blame-error))

(test-section "Dependent Contract Edge Cases")

;; Empty vars list - body should just be used directly
(define dep-no-vars
  (dep '() (lambda () nat/c)))

(test "dep with no vars instantiates correctly" nat/c
      (instantiate-dependent-contract dep-no-vars '()))

;; Nested dependent contracts (dep returning another dep)
;; This is an advanced case - just verify it doesn't crash
(define dep-returning-dep
  (dep '(outer) (lambda (outer)
    (dep '(inner) (lambda (inner) (between/c outer inner))))))

(test "nested dep contract recognized" #t (dependent-contract? dep-returning-dep))

;;; ====
;;; Statistical Contract Monitoring
;;; ====
(test-section "Statistical Contract Monitoring - Configuration")

;; Save state
(define saved-mode-stat (get-contract-mode))
(define saved-rate-stat (get-statistical-sample-rate))
(define saved-checks *statistical-check-count*)
(define saved-violations *statistical-violation-count*)

(test "statistical is valid mode" #t (contract-mode? 'statistical))
(test "default sample rate is 0.1" 0.1 (get-statistical-sample-rate))

;; Test setting sample rate
(set-statistical-sample-rate! 0.5)
(test "sample rate can be set" 0.5 (get-statistical-sample-rate))

(set-statistical-sample-rate! 1.0)
(test "sample rate can be 1.0" 1.0 (get-statistical-sample-rate))

(set-statistical-sample-rate! 0.0)
(test "sample rate can be 0.0" 0.0 (get-statistical-sample-rate))

;; Test invalid sample rates
(define (test-invalid-sample-rate-negative)
  (guard (e [else #t])
    (set-statistical-sample-rate! -0.1)
    #f))
(test "rejects negative sample rate" #t (test-invalid-sample-rate-negative))

(define (test-invalid-sample-rate-over-one)
  (guard (e [else #t])
    (set-statistical-sample-rate! 1.5)
    #f))
(test "rejects sample rate > 1.0" #t (test-invalid-sample-rate-over-one))

(test-section "Statistical Contract Monitoring - Counter Tracking")

;; Reset for clean slate
(reset-statistical-counters!)
(test "counters reset check count" 0 *statistical-check-count*)
(test "counters reset violation count" 0 *statistical-violation-count*)
(test "violation rate is 0 with no checks" 0.0 (statistical-violation-rate))

;; Set to 100% sampling so all checks happen
(set-contract-mode! 'statistical)
(set-statistical-sample-rate! 1.0)
(reset-statistical-counters!)

;; Perform some checks
(check-flat/mode nat/c 5 'test)   ; Should pass
(check-flat/mode nat/c 10 'test)  ; Should pass
(check-flat/mode nat/c -1 'test)  ; Should fail

(test "statistical tracks check count" 3 *statistical-check-count*)
(test "statistical tracks violation count" 1 *statistical-violation-count*)

;; Calculate violation rate
(test "violation rate calculated correctly" #t
      (and (> (statistical-violation-rate) 0.33)
           (< (statistical-violation-rate) 0.34)))

(test-section "Statistical Contract Monitoring - Zero Sampling")

;; At 0% sampling, no checks should happen
(set-statistical-sample-rate! 0.0)
(reset-statistical-counters!)

;; These should not be checked
(check-flat/mode nat/c -100 'test)  ; Would fail but isn't checked
(check-flat/mode nat/c -200 'test)  ; Would fail but isn't checked

(test "0% sampling performs no checks" 0 *statistical-check-count*)
(test "0% sampling detects no violations" 0 *statistical-violation-count*)

(test-section "Statistical Contract Monitoring - Scoped Usage")

;; Test call-with-statistical-contracts
(set-contract-mode! 'runtime)
(reset-statistical-counters!)

(define stat-scoped-result
  (call-with-statistical-contracts 1.0
    (lambda ()
      (check-flat/mode nat/c 5 'test)
      (check-flat/mode nat/c -1 'test)
      *statistical-check-count*)))

(test "scoped statistical mode performs checks" 2 stat-scoped-result)
(test "mode restored after scoped statistical" 'runtime (get-contract-mode))

(test-section "Statistical Contract Monitoring - Reporting")

(reset-statistical-counters!)
(set-contract-mode! 'statistical)
(set-statistical-sample-rate! 1.0)
(check-flat/mode nat/c 5 'test)
(check-flat/mode nat/c -1 'test)

(define report (statistical-monitoring-report))
(test "report is a string" #t (string? report))
(test "report mentions sample rate" #t (string-contains? report "Sample rate"))
(test "report mentions checks" #t (string-contains? report "Checks performed"))
(test "report mentions violations" #t (string-contains? report "Violations detected"))

(test-section "Statistical Contract Monitoring - Call-Time Violations")

;; This tests the key fix: violations that occur when a wrapped function is CALLED
;; (not just at wrap-time) should still be tracked in statistical mode.
(set-contract-mode! 'statistical)
(set-statistical-sample-rate! 1.0)
(reset-statistical-counters!)

;; Wrap a function (this should succeed and count as 1 check)
(define stat-wrapped-fn
  (let ([result (contract-wrap/mode add1-contract (lambda (x) (+ x 1)) 'stat-test)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "wrap-time check recorded" 1 *statistical-check-count*)
(test "no wrap-time violation" 0 *statistical-violation-count*)

;; Now call with valid input - no violation
(stat-wrapped-fn 5)

;; Call with invalid input - should raise AND record violation
(define (test-call-time-violation-tracked)
  (guard (e [else #t])
    (stat-wrapped-fn -1)  ; -1 violates nat/c domain
    #f))

(test "call-time violation raises error" #t (test-call-time-violation-tracked))
(test "call-time violation was tracked" 1 *statistical-violation-count*)

(test-section "Statistical Contract Monitoring - apply-contract/mode Violation Count")

;; This test addresses the QA gap from 7c462fff: verify that apply-contract/mode
;; increments the violation count exactly once in statistical mode (no double-counting).
(set-contract-mode! 'statistical)
(set-statistical-sample-rate! 1.0)
(reset-statistical-counters!)

;; Flat contract failure via apply-contract/mode
(guard (e [else #t])
  (apply-contract/mode nat/c "not a nat" 'stat-exact-test))

(test "apply-contract/mode records exactly 1 violation (flat)" 1 *statistical-violation-count*)
(test "apply-contract/mode records exactly 1 check (flat)" 1 *statistical-check-count*)

;; Reset and test with function contract call-time violation
(reset-statistical-counters!)

;; Wrap a function - this is the "check" (should succeed)
(define stat-exact-fn
  (let ([result (contract-wrap/mode add1-contract (lambda (x) (+ x 1)) 'stat-exact)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

;; Now trigger a call-time violation
(guard (e [else #t])
  (stat-exact-fn -1))  ; -1 violates nat/c domain

(test "apply-contract/mode records exactly 1 violation (call-time)" 1 *statistical-violation-count*)

(test-section "Statistical Contract Monitoring - Mode String")

(test "statistical mode string" "statistical (probabilistic sampling)"
      (contract-mode->string 'statistical))

;; Contract status in statistical mode includes stats
(set-contract-mode! 'statistical)
(define stat-status (contract-status))
(test "status includes statistical info" #t
      (string-contains? stat-status "Statistical sample rate"))

;; Restore original state
(set! *contract-mode* saved-mode-stat)
(set! *statistical-sample-rate* saved-rate-stat)
(set! *statistical-check-count* saved-checks)
(set! *statistical-violation-count* saved-violations)

;;; ====
;;; Invariant Contracts
;;; ====
(test-section "Invariant Contracts - Basic")

;; Test invariant/c constructor
(define test-positive-list/c
  (invariant/c list? (lambda (lst) (andmap positive? lst))))

(test "invariant/c is recognized as contract" #t (contract? test-positive-list/c))
(test "invariant-contract? recognizes invariants" #t (invariant-contract? test-positive-list/c))
(test "invariant-contract? rejects non-invariants" #f (invariant-contract? nat/c))

;; Test check-invariant
(test "invariant passes for valid value" 'Ok
      (car (check-invariant test-positive-list/c '(1 2 3) 'test)))
(test "invariant returns value on success" '(1 2 3)
      (cadr (check-invariant test-positive-list/c '(1 2 3) 'test)))

(test "invariant fails for invalid base type" 'Err
      (car (check-invariant test-positive-list/c 42 'test)))
(test "invariant fails for violated property" 'Err
      (car (check-invariant test-positive-list/c '(1 -2 3) 'test)))

;; Test apply-invariant
(test "apply-invariant returns value on success" '(1 2 3)
      (apply-invariant test-positive-list/c '(1 2 3) 'test))

(define (test-apply-invariant-failure)
  (guard (e [else #t])
    (apply-invariant test-positive-list/c '(-1 2 3) 'test)
    #f))
(test "apply-invariant raises on violation" #t (test-apply-invariant-failure))

(test-section "Invariant Contracts - Built-in Invariants")

;; sorted-list/c (non-decreasing: allows duplicates)
(test "sorted-list/c accepts empty list" 'Ok
      (car (check-flat sorted-list/c '() 'test)))
(test "sorted-list/c accepts singleton" 'Ok
      (car (check-flat sorted-list/c '(5) 'test)))
(test "sorted-list/c accepts sorted list" 'Ok
      (car (check-flat sorted-list/c '(1 2 3 4 5) 'test)))
(test "sorted-list/c accepts list with duplicates" 'Ok
      (car (check-flat sorted-list/c '(1 2 2 3) 'test)))
(test "sorted-list/c rejects unsorted list" 'Err
      (car (check-flat sorted-list/c '(1 3 2) 'test)))
(test "sorted-list/c rejects non-list" 'Err
      (car (check-flat sorted-list/c 42 'test)))
(test "sorted-list/c rejects non-numeric elements" 'Err
      (car (check-flat sorted-list/c '("c" "a" "b") 'test)))
(test "sorted-list/c rejects mixed types" 'Err
      (car (check-flat sorted-list/c '(1 "two" 3) 'test)))

;; strictly-sorted-list/c (strictly increasing: no duplicates)
(test "strictly-sorted-list/c accepts strictly sorted" 'Ok
      (car (check-flat strictly-sorted-list/c '(1 2 3) 'test)))
(test "strictly-sorted-list/c rejects duplicates" 'Err
      (car (check-flat strictly-sorted-list/c '(1 2 2 3) 'test)))
(test "strictly-sorted-list/c rejects non-numeric elements" 'Err
      (car (check-flat strictly-sorted-list/c '("a" "b" "c") 'test)))

;; non-empty-list/c
(test "non-empty-list/c accepts non-empty" 'Ok
      (car (check-flat non-empty-list/c '(1) 'test)))
(test "non-empty-list/c rejects empty" 'Err
      (car (check-flat non-empty-list/c '() 'test)))

;; unique-list/c
(test "unique-list/c accepts unique elements" 'Ok
      (car (check-flat unique-list/c '(1 2 3) 'test)))
(test "unique-list/c rejects duplicates" 'Err
      (car (check-flat unique-list/c '(1 2 1) 'test)))
(test "unique-list/c accepts empty list" 'Ok
      (car (check-flat unique-list/c '() 'test)))

(test-section "Invariant Contracts - Combinators")

;; and-invariant/c
(define sorted-unique/c (and-invariant/c sorted-list/c unique-list/c))
(test "and-invariant/c is a contract" #t (contract? sorted-unique/c))
(test "and-invariant/c passes when both hold" 'Ok
      (car (check-flat sorted-unique/c '(1 2 3) 'test)))
(test "and-invariant/c fails when sorted but not unique" 'Err
      (car (check-flat sorted-unique/c '(1 2 2 3) 'test)))

;; or-invariant/c
(define either-sorted-or-empty/c (or-invariant/c sorted-list/c non-empty-list/c))
(test "or-invariant/c is a contract" #t (contract? either-sorted-or-empty/c))
;; Note: '(3 2 1) is not sorted but IS non-empty, so should pass
(test "or-invariant/c passes when second holds" 'Ok
      (car (check-flat either-sorted-or-empty/c '(3 2 1) 'test)))
;; Sorted list passes
(test "or-invariant/c passes when first holds" 'Ok
      (car (check-flat either-sorted-or-empty/c '(1 2 3) 'test)))

(test-section "Invariant Contracts - maintains-invariant")

;; Create a function contract that maintains sorted-list invariant
(define sorted-insert-contract
  (maintains-invariant sorted-list/c
    (->c (list int/c sorted-list/c) sorted-list/c)))

(test "maintains-invariant creates function contract" #t
      (function-contract? sorted-insert-contract))

;; A correct sorted insert function
(define (sorted-insert n lst)
  (cond
   [(null? lst) (list n)]
   [(<= n (car lst)) (cons n lst)]
   [else (cons (car lst) (sorted-insert n (cdr lst)))]))

;; Wrap it
(define wrapped-sorted-insert
  (let ([result (contract-wrap sorted-insert-contract sorted-insert 'sorted-insert)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(test "maintains-invariant wraps function" #t (procedure? wrapped-sorted-insert))
(test "maintained invariant passes for correct function" '(1 2 3 4 5)
      (wrapped-sorted-insert 3 '(1 2 4 5)))

;; Test that violation is caught when result violates invariant
(define (bad-sorted-insert n lst)
  (cons n lst))  ; Just prepends - doesn't maintain sorted order

(define wrapped-bad-insert
  (let ([result (contract-wrap sorted-insert-contract bad-sorted-insert 'bad-insert)])
    (if (eq? (car result) 'Ok) (cadr result) #f)))

(define (test-maintains-invariant-violation)
  (guard (e [else #t])
    (wrapped-bad-insert 5 '(1 2 3))  ; Would produce '(5 1 2 3) - not sorted
    #f))
(test "maintains-invariant catches violation" #t (test-maintains-invariant-violation))

(test-section "Invariant Contracts - invariant->flat-contract")

(define sorted-flat (invariant->flat-contract sorted-list/c))
(test "invariant->flat-contract creates flat contract" #t (flat-contract? sorted-flat))
(test "converted invariant checks correctly" 'Ok
      (car (check-flat sorted-flat '(1 2 3) 'test)))
(test "converted invariant fails correctly" 'Err
      (car (check-flat sorted-flat '(3 2 1) 'test)))

(newline)
(display "All tests completed!")
(newline)
