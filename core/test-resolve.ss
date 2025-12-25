;;; Test harness for core/resolve.ss — Type Class Instance Resolution

(load "block.ss")
(load "types.ss")
(load "kinds.ss")
(load "infer.ss")
(load "resolve.ss")

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

(define (test-result name expected-tag result)
  (display "  ")
  (display name)
  (display ": ")
  (if (eq? (car result) expected-tag)
      (display "✓")
      (begin
        (display "✗\n    expected tag: ")
        (display expected-tag)
        (display "\n    got: ")
        (display result)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Instance Database
;;; ============================================================
(test-section "Instance Database")
(test "empty-idb" '() empty-idb)
(test "idb-add single" 1 (length (idb-add empty-idb inst-Functor-List)))
(test "idb-add* multiple" 8 (length (idb-add* empty-idb standard-instances)))

;;; ============================================================
;;; Constraint Representation
;;; ============================================================
(test-section "Constraint Representation")
(test "constraint?" #t (constraint? '(Functor List)))
(test "constraint? monad" #t (constraint? '(Monad Option)))
(test "constraint? applied" #t (constraint? '(Functor (@ Either String))))
(test "not constraint" #f (constraint? '(just-a-symbol)))
(test "constraint-class" 'Functor (constraint-class '(Functor List)))
(test "constraint-type" 'List (constraint-type '(Functor List)))

;;; ============================================================
;;; Finding Matching Instances
;;; ============================================================
(test-section "Finding Matching Instances")

;; Simple match: Functor List
(define matches-functor-list
  (find-matching-instances '(Functor List) standard-instances))
(test "find Functor List" 1 (length matches-functor-list))

;; Simple match: Monad Option
(define matches-monad-option
  (find-matching-instances '(Monad Option) standard-instances))
(test "find Monad Option" 1 (length matches-monad-option))

;; No match: Eq List (not in standard instances)
(define matches-eq-list
  (find-matching-instances '(Eq List) standard-instances))
(test "no match Eq List" 0 (length matches-eq-list))

;; Parametric match: Functor (@ Either String)
(define matches-functor-either
  (find-matching-instances '(Functor (@ Either String)) standard-instances))
(test "find Functor (Either String)" 1 (length matches-functor-either))

;;; ============================================================
;;; Resolution — Success Cases
;;; ============================================================
(test-section "Resolution — Success Cases")

;; Resolve Functor List
(define result-functor-list (resolve '(Functor List) standard-instances))
(test-result "resolve Functor List succeeds" 'ok result-functor-list)
(test "evidence class" 'Functor (cadr (cadr result-functor-list)))
(test "evidence type" 'List (caddr (cadr result-functor-list)))

;; Resolve Monad List
(define result-monad-list (resolve '(Monad List) standard-instances))
(test-result "resolve Monad List succeeds" 'ok result-monad-list)

;; Resolve Monad Option
(define result-monad-option (resolve '(Monad Option) standard-instances))
(test-result "resolve Monad Option succeeds" 'ok result-monad-option)

;; Resolve Functor Option
(define result-functor-option (resolve '(Functor Option) standard-instances))
(test-result "resolve Functor Option succeeds" 'ok result-functor-option)

;; Resolve Applicative List
(define result-app-list (resolve '(Applicative List) standard-instances))
(test-result "resolve Applicative List succeeds" 'ok result-app-list)

;;; ============================================================
;;; Resolution — Parametric Instances
;;; ============================================================
(test-section "Resolution — Parametric Instances")

;; Resolve Functor (Either String)
(define result-functor-either-str
  (resolve '(Functor (@ Either String)) standard-instances))
(test-result "resolve Functor (Either String) succeeds" 'ok result-functor-either-str)

;; Resolve Monad (Either Int)
(define result-monad-either-int
  (resolve '(Monad (@ Either Int)) standard-instances))
(test-result "resolve Monad (Either Int) succeeds" 'ok result-monad-either-int)

;;; ============================================================
;;; Resolution — Failure Cases
;;; ============================================================
(test-section "Resolution — Failure Cases")

;; No Eq instance
(define result-eq-list (resolve '(Eq List) standard-instances))
(test-result "no Eq List" 'error result-eq-list)

;; No Show instance
(define result-show-nat (resolve '(Show Nat) standard-instances))
(test-result "no Show Nat" 'error result-show-nat)

;; No Ord instance
(define result-ord-option (resolve '(Ord Option) standard-instances))
(test-result "no Ord Option" 'error result-ord-option)

;;; ============================================================
;;; Convenience API
;;; ============================================================
(test-section "Convenience API")

;; resolve-std
(define std-result (resolve-std '(Functor List)))
(test-result "resolve-std Functor List" 'ok std-result)

;; has-instance?
(test "has-instance? Functor List" #t (has-instance? '(Functor List) standard-instances))
(test "has-instance? Monad Option" #t (has-instance? '(Monad Option) standard-instances))
(test "not has-instance? Eq List" #f (has-instance? '(Eq List) standard-instances))

;;; ============================================================
;;; Method Extraction
;;; ============================================================
(test-section "Method Extraction")

;; Our standard instances have method associations
(define evidence-functor-list (cadr result-functor-list))
(test "get-method fmap" 'list-fmap (get-method evidence-functor-list 'fmap))

(define evidence-monad-list (cadr result-monad-list))
(test "get-method >>=" 'list-bind (get-method evidence-monad-list '>>=))
(test "get-method return" 'list-return (get-method evidence-monad-list 'return))

(define evidence-app-list (cadr result-app-list))
(test "get-method pure" 'list-pure (get-method evidence-app-list 'pure))
(test "get-method <*>" 'list-ap (get-method evidence-app-list '<*>))

;; Non-existent method
(test "get-method unknown" #f (get-method evidence-functor-list 'unknown-method))

;;; ============================================================
;;; Instance with Context (Future)
;;; ============================================================
(test-section "Instance with Context")

;; Create an instance with context requirements
;; e.g., Eq a => Eq (List a)
(define inst-Eq-List-with-context
  (make-instance 'Eq '(@ List a) '((Eq a))
    '((== . list-eq))))

;; Add Eq Nat as a base instance
(define inst-Eq-Nat
  (make-instance 'Eq 'Nat '()
    '((== . nat-eq))))

(define extended-db
  (idb-add* standard-instances (list inst-Eq-List-with-context inst-Eq-Nat)))

;; Should resolve Eq Nat directly
(define result-eq-nat (resolve '(Eq Nat) extended-db))
(test-result "resolve Eq Nat" 'ok result-eq-nat)

;; Should resolve Eq (List Nat) via context
(define result-eq-list-nat (resolve '(Eq (@ List Nat)) extended-db))
(test-result "resolve Eq (List Nat)" 'ok result-eq-list-nat)

;;; ============================================================
;;; Class Database
;;; ============================================================
(test-section "Class Database")

(test "lookup Functor" TC-Functor (lookup-class 'Functor standard-classes))
(test "lookup Monad" TC-Monad (lookup-class 'Monad standard-classes))
(test "lookup unknown" #f (lookup-class 'Unknown standard-classes))

;;; ============================================================
;;; filter-map helper
;;; ============================================================
(test-section "Helper Functions")

(test "filter-map identity" '(1 2 3) (filter-map (lambda (x) x) '(1 2 3)))
(test "filter-map with #f" '(2 4) (filter-map (lambda (x) (if (even? x) x #f)) '(1 2 3 4 5)))
(test "filter-map empty" '() (filter-map (lambda (x) x) '()))
(test "filter-map all #f" '() (filter-map (lambda (x) #f) '(1 2 3)))

(newline)
(display "✓ All instance resolution tests complete.\n")
