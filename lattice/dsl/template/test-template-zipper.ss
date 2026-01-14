;;; lattice/dsl/template/test-template-zipper.ss — Tests for Template Hole Zipper
;;;
;;; Tests hole-focused navigation for template expressions.

(load "lattice/dsl/template/template-zipper.ss")
(load "core/testing/test-framework.ss")

;;; ====
;;; Test Fixtures
;;; ====

;;; Simple template with 3 holes
(define simple-template '(define $name $body))

;;; Template with nested holes
(define nested-template '(define ($name $args) $body))

;;; Template with many holes
(define many-holes '(if $cond $then $else))

;;; Deep template
(define deep-template '(let (($var $init)) (if $cond (+ $var $inc) $else)))

;;; No holes
(define no-holes '(+ 1 2))

;;; ====
;;; Constructor Tests
;;; ====

(test-group "hole-zipper-constructors"

  (define-test "expr->hole-zipper creates zipper"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-true (hole-zipper? hz))
      (assert-equal 2 (hole-zipper-count hz))))

  (define-test "no holes returns #f"
    (let ([hz (expr->hole-zipper no-holes)])
      (assert-false hz)))

  (define-test "template->hole-zipper works"
    (let* ([t (new-template nested-template)]
           [hz (template->hole-zipper t)])
      (assert-true (hole-zipper? hz))
      (assert-equal 3 (hole-zipper-count hz))))

  (define-test "hole-zipper->expr round-trips"
    (let* ([hz (expr->hole-zipper simple-template)]
           [expr (hole-zipper->expr hz)])
      (assert-equal simple-template expr))))

;;; ====
;;; Current Hole Tests
;;; ====

(test-group "hole-zipper-current"

  (define-test "current returns first hole"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-equal '$name (hole-zipper-current hz))))

  (define-test "current position is correct"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-equal '(1) (hole-zipper-current-position hz))))

  (define-test "nested template first hole"
    (let ([hz (expr->hole-zipper nested-template)])
      (assert-equal '$name (hole-zipper-current hz)))))

;;; ====
;;; Position Information Tests
;;; ====

(test-group "hole-zipper-position"

  (define-test "position string shows 1 of n"
    (let ([hz (expr->hole-zipper many-holes)])
      (assert-equal "hole 1 of 3" (hole-zipper-position-string hz))))

  (define-test "at-first? is true initially"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-true (hole-zipper-at-first? hz))))

  (define-test "at-last? is false initially"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-false (hole-zipper-at-last? hz))))

  (define-test "complete? is false with holes"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-false (hole-zipper-complete? hz)))))

;;; ====
;;; Navigation Tests
;;; ====

(test-group "hole-zipper-navigation"

  (define-test "next moves to second hole"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-next hz)])
      (assert-true (hole-zipper? hz2))
      (assert-equal '$body (hole-zipper-current hz2))))

  (define-test "next at last returns #f"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-next hz)]  ; at $body
           [hz3 (hole-zipper-next hz2)])
      (assert-false hz3)))

  (define-test "prev at first returns #f"
    (let* ([hz (expr->hole-zipper simple-template)]
           [prev (hole-zipper-prev hz)])
      (assert-false prev)))

  (define-test "prev after next returns to first"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-next hz)]
           [hz3 (hole-zipper-prev hz2)])
      (assert-true (hole-zipper? hz3))
      (assert-equal '$name (hole-zipper-current hz3))))

  (define-test "first goes to first hole"
    (let* ([hz (expr->hole-zipper many-holes)]
           [hz2 (hole-zipper-next hz)]
           [hz3 (hole-zipper-next hz2)]
           [hz4 (hole-zipper-first hz3)])
      (assert-equal '$cond (hole-zipper-current hz4))))

  (define-test "last goes to last hole"
    (let* ([hz (expr->hole-zipper many-holes)]
           [hz2 (hole-zipper-last hz)])
      (assert-equal '$else (hole-zipper-current hz2))))

  (define-test "goto-index navigates correctly"
    (let* ([hz (expr->hole-zipper many-holes)]
           [hz2 (hole-zipper-goto-index hz 2)])
      (assert-true (hole-zipper? hz2))
      (assert-equal '$else (hole-zipper-current hz2))))

  (define-test "goto-index out of bounds returns #f"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-goto-index hz 10)])
      (assert-false hz2))))

;;; ====
;;; Fill Tests
;;; ====

(test-group "hole-zipper-fill"

  (define-test "fill replaces hole"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)]
           [expr (hole-zipper->expr hz2)])
      (assert-equal '(define foo $body) expr)))

  (define-test "fill updates hole count"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)])
      (assert-equal 1 (hole-zipper-count hz2))))

  (define-test "fill with hole propagates"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz '($sig $more))]
           [expr (hole-zipper->expr hz2)])
      (assert-equal '(define ($sig $more) $body) expr)
      (assert-equal 3 (hole-zipper-count hz2))))

  (define-test "fill by name works"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill-by-name hz '$body '(+ x 1))]
           [expr (hole-zipper->expr hz2)])
      (assert-equal '(define $name (+ x 1)) expr)))

  (define-test "fill moves to remaining hole"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)])
      ;; After filling $name, should be at $body (the only remaining hole)
      (assert-equal '$body (hole-zipper-current hz2)))))

;;; ====
;;; Undo Tests
;;; ====

(test-group "hole-zipper-undo"

  (define-test "can-undo? false initially"
    (let ([hz (expr->hole-zipper simple-template)])
      (assert-false (hole-zipper-can-undo? hz))))

  (define-test "can-undo? true after fill"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)])
      (assert-true (hole-zipper-can-undo? hz2))))

  (define-test "undo restores previous state"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)]
           [hz3 (hole-zipper-undo hz2)])
      (assert-true (hole-zipper? hz3))
      (assert-equal simple-template (hole-zipper->expr hz3))))

  (define-test "undo count tracks fills"
    (let* ([hz (expr->hole-zipper many-holes)]
           [hz2 (hole-zipper-fill hz '#t)]
           [hz3 (hole-zipper-fill hz2 '(+ 1 2))]
           [hz4 (hole-zipper-fill hz3 '(- 3 4))])
      (assert-equal 3 (hole-zipper-undo-count hz4))))

  (define-test "multiple undos work"
    (let* ([hz (expr->hole-zipper simple-template)]
           [hz2 (hole-zipper-fill hz 'foo)]
           [hz3 (hole-zipper-fill hz2 '(+ x 1))]
           [hz4 (hole-zipper-undo hz3)]
           [hz5 (hole-zipper-undo hz4)])
      (assert-equal simple-template (hole-zipper->expr hz5)))))

;;; ====
;;; Context Tests
;;; ====

(test-group "hole-zipper-context"

  (define-test "context shows parent expressions"
    (let* ([hz (expr->hole-zipper nested-template)]
           ;; First hole is $name, inside ($name $args)
           [ctx (hole-zipper-context hz)])
      ;; Should have (define ($name $args) $body) as parent
      (assert-true (and (member nested-template ctx) #t))))

  (define-test "immediate context is innermost parent"
    (let* ([hz (expr->hole-zipper nested-template)]
           [immediate (hole-zipper-immediate-context hz)])
      ;; $name is inside ($name $args), which is inside the define
      ;; immediate parent of $name should be ($name $args)
      (assert-equal '($name $args) immediate))))

;;; ====
;;; All Holes Tests
;;; ====

(test-group "hole-zipper-holes"

  (define-test "all-holes returns all"
    (let* ([hz (expr->hole-zipper many-holes)]
           [holes (hole-zipper-all-holes hz)])
      (assert-equal 3 (length holes))
      (assert-true (and (member '$cond holes) #t))
      (assert-true (and (member '$then holes) #t))
      (assert-true (and (member '$else holes) #t))))

  (define-test "remaining-holes from start"
    (let* ([hz (expr->hole-zipper many-holes)]
           [remaining (hole-zipper-remaining-holes hz)])
      (assert-equal 3 (length remaining))))

  (define-test "remaining-holes from middle"
    (let* ([hz (expr->hole-zipper many-holes)]
           [hz2 (hole-zipper-next hz)]
           [remaining (hole-zipper-remaining-holes hz2)])
      (assert-equal 2 (length remaining))
      (assert-equal '$then (car remaining)))))

;;; ====
;;; Status Tests
;;; ====

(test-group "hole-zipper-status"

  (define-test "status shows position and hole"
    (let* ([hz (expr->hole-zipper simple-template)]
           [status (hole-zipper-status hz)])
      (assert-true (and (string-contains status "1 of 2") #t))
      (assert-true (and (string-contains status "$name") #t))))

  (define-test "summary includes remaining"
    (let* ([hz (expr->hole-zipper many-holes)]
           [summary (hole-zipper-summary hz)])
      (assert-true (and (string-contains summary "remaining") #t)))))

;;; string-contains helper
(define (string-contains str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sub-len) str-len) #f]
        [(string=? (substring str i (+ i sub-len)) substr) #t]
        [else (loop (+ i 1))]))))

;;; ====
;;; Complete Workflow Test
;;; ====

(test-group "hole-zipper-workflow"

  (define-test "complete filling workflow"
    (let* ([hz (expr->hole-zipper '(define ($fn $arg) $body))]
           ;; Fill $fn
           [hz2 (hole-zipper-fill hz 'add1)]
           ;; Fill $arg (should be current now)
           [hz3 (hole-zipper-fill hz2 'x)]
           ;; Fill $body
           [hz4 (hole-zipper-fill hz3 '(+ x 1))]
           [result (hole-zipper->expr hz4)])
      (assert-true (hole-zipper-complete? hz4))
      (assert-equal '(define (add1 x) (+ x 1)) result)))

  (define-test "fill and undo workflow"
    (let* ([hz (expr->hole-zipper '(if $c $t $e))]
           [hz2 (hole-zipper-fill hz '#t)]
           [hz3 (hole-zipper-fill hz2 '1)]
           [hz4 (hole-zipper-fill hz3 '2)]
           ;; Now complete
           [result (hole-zipper->expr hz4)])
      (assert-equal '(if #t 1 2) result)
      ;; Undo back to having one hole
      (let* ([hz5 (hole-zipper-undo hz4)]
             [expr5 (hole-zipper->expr hz5)])
        (assert-equal '(if #t 1 $e) expr5)
        (assert-equal 1 (hole-zipper-count hz5))))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
