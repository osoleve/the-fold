(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'boundary/fp/test-category)
(doc 'description "Tests for Validated Category Entry Points — Tests the boundary-layer validation wrappers in boundary/fp/category.ss. These wrappers validate inputs before calling pure lattice functions.")
(doc 'layer 'boundary)
(doc 'usage "Run: scheme --script boundary/fp/test-category.ss")
(doc 'dependencies '(boundary/fp/category))

(load "boundary/fp/category.ss")

(doc 'section 'test-helpers)

(define (test name expected actual)
  (doc 'description "Generic test helper that compares expected vs actual values")
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

(define (test-true name actual)
  (doc 'description "Test helper that verifies a truthy value")
  (test name #t (if actual #t #f)))

(define (test-error name thunk)
  (doc 'description "Test helper that verifies a thunk raises an error")
  (display "  ")
  (display name)
  (display ": ")
  (let ([result (guard (e [else 'error-raised])
                  (thunk)
                  'no-error)])
    (if (eq? result 'error-raised)
        (display "✓")
        (begin
          (display "✗ (expected error, got: ")
          (display result)
          (display ")"))))
  (newline))

(define (section name)
  (doc 'description "Print a test section header")
  (newline)
  (display name)
  (newline))

(doc 'section 'test-validated-natural-transform-operations)

(section "Validated Natural Transform Operations")

(test-error "validated-nat-compose rejects non-nat-transform η (second arg)"
            (lambda () (validated-nat-compose (nat-id functor-list) 'not-a-nat-transform)))

(test-error "validated-nat-compose rejects non-nat-transform ε (first arg)"
            (lambda () (validated-nat-compose 'not-a-nat-transform (nat-id functor-list))))

(let* ([id-list (nat-id functor-list)]
       [composed (validated-nat-compose id-list id-list)])
  (test-true "validated-nat-compose creates nat-transform"
             (nat-transform? composed))
  (test "validated-nat-compose identity composition"
        '(1 2 3)
        (nat-apply composed '(1 2 3))))

(test-error "validated-nat-apply rejects non-nat-transform"
            (lambda () (validated-nat-apply 'not-a-nat-transform '(1 2 3))))

(test "validated-nat-apply with nat-id"
      '(a b c)
      (validated-nat-apply (nat-id functor-list) '(a b c)))

(test-error "validated-nat-horizontal rejects non-nat-transform η"
            (lambda () (validated-nat-horizontal 'not-a-nat-transform (nat-id functor-list))))

(test-error "validated-nat-horizontal rejects non-nat-transform ε"
            (lambda () (validated-nat-horizontal (nat-id functor-list) 'not-a-nat-transform)))

(doc 'section 'test-validated-adjunction-operations)

(section "Validated Adjunction Operations")

(test-error "validated-adjunction-compose rejects non-adjunction arg1"
            (lambda () (validated-adjunction-compose adj-free-list 'not-an-adjunction)))

(test-error "validated-adjunction-compose rejects non-adjunction arg2"
            (lambda () (validated-adjunction-compose 'not-an-adjunction adj-free-list)))

(doc adj-id 'type 'Adjunction)
(doc adj-id 'description "Simple identity adjunction for composition testing")
(define adj-id
  (make-adjunction
   'id
   functor-id
   functor-id
   (nat-id functor-id)
   (nat-id functor-id)))

(let ([composed (validated-adjunction-compose adj-free-list adj-id)])
  (test-true "validated-adjunction-compose creates adjunction"
             (adjunction? composed)))

(test-error "validated-adjunction-transpose-left rejects non-adjunction"
            (lambda () (validated-adjunction-transpose-left 'not-an-adjunction (lambda (x) x))))

(test-error "validated-adjunction-transpose-left rejects non-procedure"
            (lambda () (validated-adjunction-transpose-left adj-free-list 'not-a-procedure)))

(test-error "validated-adjunction-transpose-right rejects non-adjunction"
            (lambda () (validated-adjunction-transpose-right 'not-an-adjunction (lambda (x) x))))

(test-error "validated-adjunction-transpose-right rejects non-procedure"
            (lambda () (validated-adjunction-transpose-right adj-free-list 'not-a-procedure)))

(doc 'section 'summary)

(newline)
(display "Shell category validation tests complete.")
(newline)
