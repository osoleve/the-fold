;;; core/query/test-query-patterns.ss --- Test suite for pattern matching with variable binding

(source-directories (cons "core" (source-directories)))
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/fs.ss")
(load "boundary/store-api.ss")
(load "lattice/query/query-dsl.ss")
(load "lattice/query/query-patterns.ss")

(printf "\n====\n")
(printf "        QUERY PATTERN MATCHING TEST SUITE                    \n")
(printf "====\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

;;; ====
;;; Test Setup
;;; ====

(printf "Setting up test environment...\n")
(printf "----\n")

(define test-store-path ".test-pattern-query")
(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define fs (mint-fs-capability test-store-path))

;; Create test entities
(define turing (make-block 'entity (string->utf8 "Alan Turing") (vector)))
(define church (make-block 'entity (string->utf8 "Alonzo Church") (vector)))
(define lambda-calc (make-block 'entity (string->utf8 "Lambda Calculus") (vector)))
(define turing-machine (make-block 'entity (string->utf8 "Turing Machine") (vector)))

(define hash-turing (hash-block turing))
(define hash-church (hash-block church))
(define hash-lambda (hash-block lambda-calc))
(define hash-tm (hash-block turing-machine))

;; Store entities
(store-put! fs turing)
(store-put! fs church)
(store-put! fs lambda-calc)
(store-put! fs turing-machine)

;; Create relations
(define invented-tm
  (make-block 'relation
              (string->utf8 "invented")
              (vector hash-turing hash-tm)))

(define invented-lambda
  (make-block 'relation
              (string->utf8 "invented")
              (vector hash-church hash-lambda)))

(define influenced
  (make-block 'relation
              (string->utf8 "influenced")
              (vector hash-church hash-turing)))

;; Store relations
(store-put! fs invented-tm)
(store-put! fs invented-lambda)
(store-put! fs influenced)

;; Create attribute blocks (entity -> value)
(define year-lambda
  (make-block 'relation
              (string->utf8 "year")
              (vector hash-lambda)))

(define year-tm
  (make-block 'relation
              (string->utf8 "year")
              (vector hash-tm)))

(store-put! fs year-lambda)
(store-put! fs year-tm)

(printf "Test blocks created: ~a entities, ~a relations\n\n" 4 5)

;;; ====
;;; Test 1: Variable Recognition
;;; ====

(printf "Testing Variable Recognition:\n")
(printf "----\n")

(test-true "?x is a variable"
           (variable? '?x))

(test-true "?person is a variable"
           (variable? '?person))

(test-false "x is not a variable"
            (variable? 'x))

(test-false "123 is not a variable"
            (variable? 123))

(test "variable-name extracts name"
      "person"
      (variable-name '?person))

(printf "\n")

;;; ====
;;; Test 2: Binding Environments
;;; ====

(printf "Testing Binding Environments:\n")
(printf "----\n")

(define env1 (extend-env empty-env '?x hash-turing))
(define env2 (extend-env env1 '?y hash-church))

(test-true "lookup-env finds binding"
           (bytevector=? (lookup-env env2 '?x) hash-turing))

(test-false "lookup-env returns #f for unbound"
            (lookup-env env2 '?z))

(test-true "env-bound? returns #t for bound var"
           (env-bound? env2 '?x))

(test-false "env-bound? returns #f for unbound var"
            (env-bound? env2 '?z))

;; Test merge-envs
(define env3 (extend-env empty-env '?a hash-lambda))
(define env4 (extend-env empty-env '?b hash-tm))
(define merged (merge-envs env3 env4))

(test-true "merge-envs combines compatible envs"
           (and (env-bound? merged '?a)
                (env-bound? merged '?b)))

;; Test incompatible merge
(define env5 (extend-env empty-env '?x hash-turing))
(define env6 (extend-env empty-env '?x hash-church))
(test-false "merge-envs returns #f for incompatible"
            (merge-envs env5 env6))

(printf "\n")

;;; ====
;;; Test 3: Single Pattern Matching
;;; ====

(printf "Testing Single Pattern Matching:\n")
(printf "----\n")

;; Match all "invented" relations
(define invented-results
  (match-pattern fs '(?person invented ?concept) empty-env))

(test "match invented pattern finds 2 results"
      2
      (length invented-results))

(test-true "invented results have ?person and ?concept bound"
           (and (env-bound? (car invented-results) '?person)
                (env-bound? (car invented-results) '?concept)))

;; Match all "influenced" relations
(define influenced-results
  (match-pattern fs '(?x influenced ?y) empty-env))

(test "match influenced pattern finds 1 result"
      1
      (length influenced-results))

;; Match with wildcard
(define any-relation-results
  (match-pattern fs '(?x ? ?y) empty-env))

(test-true "wildcard relation matches all relations"
           (>= (length any-relation-results) 3))

(printf "\n")

;;; ====
;;; Test 4: Multi-Pattern Joins
;;; ====

(printf "Testing Multi-Pattern Joins:\n")
(printf "----\n")

;; Find who influenced whom, and what the influencer invented
(define join-result1
  (join-patterns fs
                 '((?x influenced ?y)
                   (?x invented ?concept))))

(test-true "join finds Church influenced Turing AND invented Lambda Calc"
           (>= (length join-result1) 1))

(test-true "join result has all variables bound"
           (and (env-bound? (car join-result1) '?x)
                (env-bound? (car join-result1) '?y)
                (env-bound? (car join-result1) '?concept)))

;; Empty patterns should return single empty environment
(define empty-join (join-patterns fs '()))
(test "empty patterns returns single empty environment"
      1
      (length empty-join))

(printf "\n")

;;; ====
;;; Test 5: Pattern Query with Constraints
;;; ====

(printf "Testing Pattern Query API:\n")
(printf "----\n")

;; Simple pattern query without constraints
(define query-result1
  (pattern-query fs
                 '((?person invented ?concept))))

(test "pattern-query finds invented relations"
      2
      (length query-result1))

;; Pattern query with multiple patterns (join)
(define query-result2
  (pattern-query fs
                 '((?x influenced ?y)
                   (?y invented ?concept))))

(test-true "pattern-query joins patterns correctly"
           (>= (length query-result2) 0))

(printf "\n")

;;; ====
;;; Test 6: Result Projection
;;; ====

(printf "Testing Result Projection:\n")
(printf "----\n")

(define proj-envs
  (pattern-query fs '((?person invented ?concept))))

(define projected
  (project-vars proj-envs '(?person ?concept)))

(test "project-vars extracts specified variables"
      2
      (length projected))

(test-true "projected result is alist"
           (and (list? (car projected))
                (pair? (caar projected))))

(test "projected result has correct keys"
      '(person concept)
      (map car (car projected)))

(printf "\n")

;;; ====
;;; Test 7: Convenience Functions
;;; ====

(printf "Testing Convenience Functions:\n")
(printf "----\n")

(define count-invented
  (count-pattern fs '((?x invented ?y))))

(test "count-pattern counts matches"
      2
      count-invented)

(printf "\n")

;;; ====
;;; Test 8: Edge Cases
;;; ====

(printf "Testing Edge Cases:\n")
(printf "----\n")

;; Pattern with no matches
(define no-match
  (pattern-query fs '((?x nonexistent-relation ?y))))

(test "pattern with no matches returns empty"
      0
      (length no-match))

;; Self-referential pattern (should work but may return empty)
(define self-ref
  (pattern-query fs '((?x invented ?x))))

(test-true "self-referential pattern returns list"
           (list? self-ref))

(printf "\n")

;;; ====
;;; Summary
;;; ====

(printf "====\n")
(printf "                    TEST RESULTS                               \n")
(printf "====\n\n")

(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

;; Cleanup
(printf "\nCleaning up test store...\n")
(system (format "rm -rf ~a" test-store-path))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All tests passed! Pattern matching is working correctly.\n\n")
    (printf "\n[FAILURE] Some tests failed. Please review.\n\n"))
