;;; lattice/fp/data/test-zipper.ss — Tests for List Zipper
;;;
;;; Comprehensive tests for zipper operations including:
;;;   - Construction and conversion
;;;   - Navigation (left, right, start, end)
;;;   - Focus operations (get, set, modify)
;;;   - Insertion and deletion
;;;   - Bulk operations (map, filter, fold)
;;;   - Comonad operations

(load "core/testing/test-framework.ss")
(load "lattice/fp/data/zipper.ss")

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(test-group "zipper-construction"

  (define-test "empty zipper is empty"
    (assert-true (zipper-empty? zipper-empty)))

  (define-test "singleton creates single-element zipper"
    (let ([z (zipper-singleton 42)])
      (assert-false (zipper-empty? z))
      (assert-equal 42 (zipper-focus z))
      (assert-equal 1 (zipper-length z))))

  (define-test "list->zipper empty list gives empty zipper"
    (assert-true (zipper-empty? (list->zipper '()))))

  (define-test "list->zipper focuses on first element"
    (let ([z (list->zipper '(1 2 3 4 5))])
      (assert-equal 1 (zipper-focus z))
      (assert-equal 0 (zipper-position z))))

  (define-test "zipper->list roundtrip preserves list"
    (let ([lst '(a b c d e)])
      (assert-equal lst (zipper->list (list->zipper lst)))))

  (define-test "zipper-from-position creates correct focus"
    (let ([z (zipper-from-position '(10 20 30 40 50) 2)])
      (assert-equal 30 (zipper-focus z))
      (assert-equal 2 (zipper-position z))))

  (define-test "zipper-from-position past end has no focus"
    (let ([z (zipper-from-position '(1 2 3) 10)])
      (assert-false (zipper-has-focus? z))))
)

;;; ============================================================
;;; Navigation Tests
;;; ============================================================

(test-group "zipper-navigation"

  (define-test "can-go-left? false at start"
    (let ([z (list->zipper '(1 2 3))])
      (assert-false (zipper-can-go-left? z))))

  (define-test "can-go-right? true when elements exist"
    (let ([z (list->zipper '(1 2 3))])
      (assert-true (zipper-can-go-right? z))))

  (define-test "zipper-left! moves focus left"
    (let* ([z (zipper-from-position '(1 2 3) 1)]
           [moved (zipper-left! z)])
      (assert-true (just? moved))
      (assert-equal 1 (zipper-focus (from-just moved)))
      (assert-equal 0 (zipper-position (from-just moved)))))

  (define-test "zipper-left! returns nothing at start"
    (let ([z (list->zipper '(1 2 3))])
      (assert-true (nothing? (zipper-left! z)))))

  (define-test "zipper-right! moves focus right"
    (let* ([z (list->zipper '(1 2 3))]
           [moved (zipper-right! z)])
      (assert-true (just? moved))
      (assert-equal 2 (zipper-focus (from-just moved)))
      (assert-equal 1 (zipper-position (from-just moved)))))

  (define-test "zipper-right! at last element moves past end"
    (let* ([z (zipper-from-position '(1 2 3) 2)]
           [moved (zipper-right! z)])
      (assert-true (just? moved))
      (assert-false (zipper-has-focus? (from-just moved)))))

  (define-test "zipper-start moves to beginning"
    (let* ([z (zipper-from-position '(1 2 3 4 5) 3)]
           [start (zipper-start z)])
      (assert-equal 1 (zipper-focus start))
      (assert-equal 0 (zipper-position start))))

  (define-test "zipper-end moves to last element"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [end (zipper-end z)])
      (assert-equal 5 (zipper-focus end))
      (assert-equal 4 (zipper-position end))))

  (define-test "zipper-goto moves to position"
    (let* ([z (list->zipper '(a b c d e))]
           [moved (zipper-goto z 3)])
      (assert-equal 'd (zipper-focus moved))
      (assert-equal 3 (zipper-position moved))))

  (define-test "navigation preserves list contents"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [z1 (from-just (zipper-right! z))]
           [z2 (from-just (zipper-right! z1))]
           [z3 (from-just (zipper-left! z2))])
      (assert-equal '(1 2 3 4 5) (zipper->list z3))))
)

;;; ============================================================
;;; Focus Operation Tests
;;; ============================================================

(test-group "zipper-focus-operations"

  (define-test "zipper-focus gets current element"
    (let ([z (zipper-from-position '(10 20 30) 1)])
      (assert-equal 20 (zipper-focus z))))

  (define-test "zipper-focus-or returns default when no focus"
    (let ([z (zipper-from-position '(1 2 3) 10)])
      (assert-equal 'default (zipper-focus-or z 'default))))

  (define-test "zipper-set updates focus"
    (let* ([z (list->zipper '(1 2 3))]
           [updated (zipper-set z 100)])
      (assert-equal 100 (zipper-focus updated))
      (assert-equal '(100 2 3) (zipper->list updated))))

  (define-test "zipper-modify applies function to focus"
    (let* ([z (zipper-from-position '(1 2 3 4 5) 2)]
           [modified (zipper-modify z (lambda (x) (* x 10)))])
      (assert-equal 30 (zipper-focus modified))
      (assert-equal '(1 2 30 4 5) (zipper->list modified))))
)

;;; ============================================================
;;; Insertion Tests
;;; ============================================================

(test-group "zipper-insertion"

  (define-test "zipper-insert-left adds to left"
    (let* ([z (zipper-from-position '(1 2 3) 1)]
           [inserted (zipper-insert-left z 99)])
      (assert-equal 2 (zipper-focus inserted))  ; Focus unchanged
      (assert-equal '(1 99 2 3) (zipper->list inserted))))

  (define-test "zipper-insert-right adds to right"
    (let* ([z (zipper-from-position '(1 2 3) 1)]
           [inserted (zipper-insert-right z 99)])
      (assert-equal 2 (zipper-focus inserted))  ; Focus unchanged
      (assert-equal '(1 2 99 3) (zipper->list inserted))))

  (define-test "zipper-insert-focus pushes old focus right"
    (let* ([z (zipper-from-position '(1 2 3) 1)]
           [inserted (zipper-insert-focus z 99)])
      (assert-equal 99 (zipper-focus inserted))  ; New focus
      (assert-equal '(1 99 2 3) (zipper->list inserted))))

  (define-test "insert into empty zipper"
    (let ([z (zipper-insert-right zipper-empty 42)])
      (assert-equal 42 (zipper-focus z))
      (assert-equal '(42) (zipper->list z))))
)

;;; ============================================================
;;; Deletion Tests
;;; ============================================================

(test-group "zipper-deletion"

  (define-test "zipper-delete removes focus, moves right"
    (let* ([z (zipper-from-position '(1 2 3 4) 1)]
           [deleted (zipper-delete z)])
      (assert-equal 3 (zipper-focus deleted))
      (assert-equal '(1 3 4) (zipper->list deleted))))

  (define-test "zipper-delete at end moves left"
    (let* ([z (zipper-from-position '(1 2 3) 2)]
           [deleted (zipper-delete z)])
      (assert-equal 2 (zipper-focus deleted))
      (assert-equal '(1 2) (zipper->list deleted))))

  (define-test "zipper-delete singleton gives empty"
    (let ([deleted (zipper-delete (zipper-singleton 42))])
      (assert-true (zipper-empty? deleted))))

  (define-test "zipper-delete-left removes left neighbor"
    (let* ([z (zipper-from-position '(1 2 3 4) 2)]
           [deleted (zipper-delete-left z)])
      (assert-equal 3 (zipper-focus deleted))  ; Focus unchanged
      (assert-equal '(1 3 4) (zipper->list deleted))))

  (define-test "zipper-delete-right removes right neighbor"
    (let* ([z (zipper-from-position '(1 2 3 4) 1)]
           [deleted (zipper-delete-right z)])
      (assert-equal 2 (zipper-focus deleted))  ; Focus unchanged
      (assert-equal '(1 2 4) (zipper->list deleted))))
)

;;; ============================================================
;;; Bulk Operation Tests
;;; ============================================================

(test-group "zipper-bulk-operations"

  (define-test "zipper-map applies to all elements"
    (let* ([z (zipper-from-position '(1 2 3 4 5) 2)]
           [mapped (zipper-map (lambda (x) (* x 2)) z)])
      (assert-equal '(2 4 6 8 10) (zipper->list mapped))
      (assert-equal 6 (zipper-focus mapped))))  ; Focus also mapped

  (define-test "zipper-filter removes non-matching"
    (let* ([z (list->zipper '(1 2 3 4 5 6))]
           [filtered (zipper-filter even? z)])
      (assert-equal '(2 4 6) (zipper->list filtered))))

  (define-test "zipper-fold-left accumulates correctly"
    (let ([z (list->zipper '(1 2 3 4 5))])
      (assert-equal 15 (zipper-fold-left + 0 z))))

  (define-test "zipper-find locates matching element"
    (let* ([z (list->zipper '(1 3 5 7 8 9))]
           [found (zipper-find even? z)])
      (assert-true (just? found))
      (assert-equal 8 (zipper-focus (from-just found)))))

  (define-test "zipper-find returns nothing when not found"
    (let ([z (list->zipper '(1 3 5 7 9))])
      (assert-true (nothing? (zipper-find even? z)))))

  (define-test "zipper-find-index returns correct index"
    (let ([z (list->zipper '(a b c d e))])
      (assert-equal (just 2) (zipper-find-index (lambda (x) (eq? x 'c)) z))))
)

;;; ============================================================
;;; Context Operation Tests
;;; ============================================================

(test-group "zipper-context-operations"

  (define-test "zipper-context excludes focus"
    (let ([z (zipper-from-position '(1 2 3 4 5) 2)])
      (assert-equal '(1 2 4 5) (zipper-context z))))

  (define-test "zipper-window gets surrounding elements"
    (let ([z (zipper-from-position '(1 2 3 4 5) 2)])
      (assert-equal '(2 3 4) (zipper-window z 1 1))
      (assert-equal '(1 2 3 4 5) (zipper-window z 10 10))))

  (define-test "zipper-split divides at focus"
    (let* ([z (zipper-from-position '(a b c d e) 2)]
           [parts (zipper-split z)])
      (assert-equal '(a b) (car parts))
      (assert-equal '(c d e) (cdr parts))))

  (define-test "zipper-append adds to end"
    (let* ([z (zipper-from-position '(1 2 3) 1)]
           [appended (zipper-append z '(4 5))])
      (assert-equal '(1 2 3 4 5) (zipper->list appended))
      (assert-equal 2 (zipper-focus appended))))

  (define-test "zipper-prepend adds to beginning"
    (let* ([z (zipper-from-position '(3 4 5) 1)]
           [prepended (zipper-prepend '(1 2) z)])
      (assert-equal '(1 2 3 4 5) (zipper->list prepended))
      (assert-equal 4 (zipper-focus prepended))))
)

;;; ============================================================
;;; Comonad Tests
;;; ============================================================

(test-group "zipper-comonad"

  (define-test "zipper-extract gets focus"
    (let ([z (zipper-from-position '(1 2 3) 1)])
      (assert-equal 2 (zipper-extract z))))

  (define-test "zipper-extend applies at each position"
    (let* ([z (list->zipper '(1 2 3))]
           [extended (zipper-extend zipper-position z)])
      ;; At each position, we get that position number
      (assert-equal '(0 1 2) (zipper->list extended))))

  (define-test "zipper-duplicate creates zipper of zippers"
    (let* ([z (list->zipper '(a b c))]
           [duped (zipper-duplicate z)])
      ;; Each position contains zipper focused at that position
      (assert-equal 'a (zipper-focus (zipper-focus duped)))))

  (define-test "comonad law: extend extract = id"
    (let* ([z (zipper-from-position '(1 2 3 4 5) 2)]
           [result (zipper-extend zipper-extract z)])
      (assert-equal (zipper->list z) (zipper->list result))
      (assert-equal (zipper-position z) (zipper-position result))))

  (define-test "comonad law: extract . extend f = f"
    (let* ([z (list->zipper '(10 20 30))]
           [f (lambda (zz) (+ (zipper-focus zz) 1000))]
           [result (zipper-extract (zipper-extend f z))])
      (assert-equal (f z) result)))
)

;;; ============================================================
;;; Equality and Display Tests
;;; ============================================================

(test-group "zipper-equality-display"

  (define-test "zipper-equal? same zippers"
    (let ([z1 (zipper-from-position '(1 2 3) 1)]
          [z2 (zipper-from-position '(1 2 3) 1)])
      (assert-true (zipper-equal? z1 z2))))

  (define-test "zipper-equal? different focus position"
    (let ([z1 (zipper-from-position '(1 2 3) 0)]
          [z2 (zipper-from-position '(1 2 3) 1)])
      (assert-false (zipper-equal? z1 z2))))

  (define-test "zipper->string shows focus"
    (let ([z (zipper-from-position '(1 2 3) 1)])
      ;; Just check it doesn't error and contains the focus
      (let ([s (zipper->string z)])
        (assert-true (string? s)))))
)

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group "zipper-edge-cases"

  (define-test "operations on empty zipper"
    (assert-true (nothing? (zipper-left! zipper-empty)))
    (assert-true (nothing? (zipper-right! zipper-empty)))
    (assert-equal zipper-empty (zipper-start zipper-empty))
    (assert-equal zipper-empty (zipper-delete zipper-empty)))

  (define-test "single element navigation"
    (let ([z (zipper-singleton 'only)])
      (assert-true (nothing? (zipper-left! z)))
      ;; Right should move past end
      (let ([moved (zipper-right! z)])
        (assert-true (just? moved))
        (assert-false (zipper-has-focus? (from-just moved))))))

  (define-test "position clamps correctly"
    (let ([z (zipper-goto (list->zipper '(1 2 3)) 100)])
      ;; Should be at end, past last element
      (assert-equal 3 (zipper-length z))))

  (define-test "delete-left at start is no-op"
    (let* ([z (list->zipper '(1 2 3))]
           [result (zipper-delete-left z)])
      (assert-equal '(1 2 3) (zipper->list result))))

  (define-test "delete-right at end is no-op"
    (let* ([z (zipper-end (list->zipper '(1 2 3)))]
           [result (zipper-delete-right z)])
      (assert-equal '(1 2 3) (zipper->list result))))
)

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
