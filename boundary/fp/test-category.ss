;;; boundary/fp/test-category.ss — Tests for Validated Category Entry Points
;;;
;;; Tests the shell-layer validation wrappers in boundary/fp/category.ss.
;;; These wrappers validate inputs before calling pure lattice functions.
;;;
;;; Run: scheme --script boundary/fp/test-category.ss

(load "boundary/fp/category.ss")

;;; ====
;;; Test Helpers
;;; ====

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

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-error name thunk)
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
  (newline)
  (display name)
  (newline))

;;; ====
;;; Test: Validated Natural Transform Operations
;;; ====

(section "Validated Natural Transform Operations")

;; validated-nat-compose rejects non-nat-transform arguments
(test-error "validated-nat-compose rejects non-nat-transform η (second arg)"
            (lambda () (validated-nat-compose (nat-id functor-list) 'not-a-nat-transform)))

(test-error "validated-nat-compose rejects non-nat-transform ε (first arg)"
            (lambda () (validated-nat-compose 'not-a-nat-transform (nat-id functor-list))))

;; validated-nat-compose works with valid inputs
(let* ([id-list (nat-id functor-list)]
       [composed (validated-nat-compose id-list id-list)])
  (test-true "validated-nat-compose creates nat-transform"
             (nat-transform? composed))
  (test "validated-nat-compose identity composition"
        '(1 2 3)
        (nat-apply composed '(1 2 3))))

;; validated-nat-apply rejects non-nat-transform
(test-error "validated-nat-apply rejects non-nat-transform"
            (lambda () (validated-nat-apply 'not-a-nat-transform '(1 2 3))))

;; validated-nat-apply works with valid inputs
(test "validated-nat-apply with nat-id"
      '(a b c)
      (validated-nat-apply (nat-id functor-list) '(a b c)))

;; validated-nat-horizontal rejects non-nat-transform arguments
(test-error "validated-nat-horizontal rejects non-nat-transform η"
            (lambda () (validated-nat-horizontal 'not-a-nat-transform (nat-id functor-list))))

(test-error "validated-nat-horizontal rejects non-nat-transform ε"
            (lambda () (validated-nat-horizontal (nat-id functor-list) 'not-a-nat-transform)))

;;; ====
;;; Test: Validated Adjunction Operations
;;; ====

(section "Validated Adjunction Operations")

;; validated-adjunction-compose rejects non-adjunction arguments
(test-error "validated-adjunction-compose rejects non-adjunction arg1"
            (lambda () (validated-adjunction-compose adj-free-list 'not-an-adjunction)))

(test-error "validated-adjunction-compose rejects non-adjunction arg2"
            (lambda () (validated-adjunction-compose 'not-an-adjunction adj-free-list)))

;; Define a simple identity adjunction for composition testing
(define adj-id
  (make-adjunction
   'id
   functor-id
   functor-id
   (nat-id functor-id)
   (nat-id functor-id)))

;; validated-adjunction-compose works with valid inputs
(let ([composed (validated-adjunction-compose adj-free-list adj-id)])
  (test-true "validated-adjunction-compose creates adjunction"
             (adjunction? composed)))

;; validated-adjunction-transpose-left rejects invalid inputs
(test-error "validated-adjunction-transpose-left rejects non-adjunction"
            (lambda () (validated-adjunction-transpose-left 'not-an-adjunction (lambda (x) x))))

(test-error "validated-adjunction-transpose-left rejects non-procedure"
            (lambda () (validated-adjunction-transpose-left adj-free-list 'not-a-procedure)))

;; validated-adjunction-transpose-right rejects invalid inputs
(test-error "validated-adjunction-transpose-right rejects non-adjunction"
            (lambda () (validated-adjunction-transpose-right 'not-an-adjunction (lambda (x) x))))

(test-error "validated-adjunction-transpose-right rejects non-procedure"
            (lambda () (validated-adjunction-transpose-right adj-free-list 'not-a-procedure)))

;;; ====
;;; Summary
;;; ====

(newline)
(display "Shell category validation tests complete.")
(newline)
