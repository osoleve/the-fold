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

(newline)
(display "All tests completed!")
(newline)
