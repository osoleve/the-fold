;;; zipper.sexp — Development tasks for lattice/fp/data/zipper.ss
;;;
;;; Module: zipper (tier 0, depends on prelude, combinators)
;;; 504 lines, ~30 exported functions
;;; Huet's list zipper: O(1) local navigation and modification at focus,
;;; list/zipper conversion, insertion/deletion, bulk operations (map, filter, fold),
;;; comonad instance (extract, extend, duplicate), equality, display.
;;;
;;; Git history:
;;;   71b6b156 feat(module): migrate lattice/fp/ leaves + depth-1 to require
;;;   7771eec3 refactor: Consolidate last definitions to use prelude

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement zipper-singleton
  ;; ──────────────────────────────────────────────
  ;; Create a zipper with a single element.

  (task
    (id "fp/zipper/singleton-impl")
    (module zipper)
    (tier 0)
    (type implement)
    (description "Implement zipper-singleton: create a zipper with a single element focused. A list zipper consists of (left-context, focus, right-context) represented as (list zipper-tag left focus right) where zipper-tag is the symbol 'list-zipper. For a singleton, left is empty, focus is (just x), and right is empty. Use make-zipper to construct the result.")
    (context "Available: make-zipper, just, '(). Single constructor call.")
    (before "(define (zipper-singleton x)
  ;; TODO: single-element zipper with x focused
  zipper-empty)")
    (test-file "lattice/fp/data/test-zipper.ss")
    (test-forms (";; Singleton is not empty
(let ([z (zipper-singleton 42)])
  (assert-false (zipper-empty? z)))"
                 ";; Focus is the element
(assert-equal 42 (zipper-focus (zipper-singleton 42)))"
                 ";; Length is 1
(assert-equal 1 (zipper-length (zipper-singleton 42)))"
                 ";; Roundtrip to list
(assert-equal '(42) (zipper->list (zipper-singleton 42)))"))
    (available-modules (prelude combinators))
    (hints ("A zipper is: (make-zipper left focus right)"
            "Singleton: left = '(), focus = (just x), right = '()"
            "(make-zipper '() (just x) '())"))
    (reference-solution "(define (zipper-singleton x)
  (make-zipper '() (just x) '()))")
    (alternatives ())
    (source-commit "71b6b156")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement list->zipper
  ;; ──────────────────────────────────────────────
  ;; Convert a list to a zipper focused on the first element.

  (task
    (id "fp/zipper/list-to-zipper-impl")
    (module zipper)
    (tier 0)
    (type implement)
    (description "Implement list->zipper: convert a list to a zipper focused on the first element. If the list is empty, return zipper-empty. Otherwise, the first element becomes the focus (wrapped in just), and the rest becomes the right context. The left context is empty since we're at the start. This is the primary way to create a zipper from existing data.")
    (context "Available: null?, car, cdr, just, make-zipper, zipper-empty, if. One conditional + one constructor.")
    (before "(define (list->zipper lst)
  ;; TODO: focus on first element
  zipper-empty)")
    (test-file "lattice/fp/data/test-zipper.ss")
    (test-forms (";; Empty list gives empty zipper
(assert-true (zipper-empty? (list->zipper '())))"
                 ";; Focuses on first element
(assert-equal 1 (zipper-focus (list->zipper '(1 2 3))))"
                 ";; Position is 0
(assert-equal 0 (zipper-position (list->zipper '(1 2 3))))"
                 ";; Roundtrip preserves list
(assert-equal '(a b c d e) (zipper->list (list->zipper '(a b c d e))))"))
    (available-modules (prelude combinators))
    (hints ("Empty list: return zipper-empty"
            "Non-empty: (make-zipper '() (just (car lst)) (cdr lst))"
            "Left context is empty because we focus on the first element"))
    (reference-solution "(define (list->zipper lst)
  (if (null? lst)
      zipper-empty
      (make-zipper '() (just (car lst)) (cdr lst))))")
    (alternatives ())
    (source-commit "71b6b156")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement zipper-position
  ;; ──────────────────────────────────────────────
  ;; Get the 0-indexed position of the focus.

  (task
    (id "fp/zipper/position-impl")
    (module zipper)
    (tier 0)
    (type implement)
    (description "Implement zipper-position: return the 0-indexed position of the focus in the underlying list. The key insight is that the left context stores elements in reverse order (closest first), so the number of elements to the left IS the position. Just return the length of the left context.")
    (context "Available: length, zipper-left. Single expression.")
    (before "(define (zipper-position z)
  ;; TODO: 0-indexed position of focus
  0)")
    (test-file "lattice/fp/data/test-zipper.ss")
    (test-forms (";; Start of list
(assert-equal 0 (zipper-position (list->zipper '(1 2 3))))"
                 ";; Middle of list
(assert-equal 2 (zipper-position (zipper-from-position '(a b c d e) 2)))"
                 ";; End of list
(assert-equal 4 (zipper-position (zipper-from-position '(1 2 3 4 5) 4)))"
                 ";; Empty zipper
(assert-equal 0 (zipper-position zipper-empty))"))
    (available-modules (prelude combinators))
    (hints ("The left context is reversed: closest element first"
            "Number of elements to the left = position"
            "(length (zipper-left z))"))
    (reference-solution "(define (zipper-position z)
  (length (zipper-left z)))")
    (alternatives ())
    (source-commit "71b6b156")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement zipper-set
  ;; ──────────────────────────────────────────────
  ;; Replace the focused element.

  (task
    (id "fp/zipper/set-impl")
    (module zipper)
    (tier 0)
    (type implement)
    (description "Implement zipper-set: replace the focused element with a new value x. The zipper must have a focus (error if not). Create a new zipper with the same left and right contexts but with (just x) as the new focus. This is O(1) since we only replace one component of the zipper triple. Check zipper-has-focus? first; if no focus, raise an error with (error 'zipper-set \"no focused element to set\").")
    (context "Available: zipper-has-focus?, zipper-left, zipper-right, make-zipper, just, not, if, error. Guard + one constructor.")
    (before "(define (zipper-set z x)
  ;; TODO: replace focus with x
  z)")
    (test-file "lattice/fp/data/test-zipper.ss")
    (test-forms (";; Set updates the focus
(let* ([z (list->zipper '(1 2 3))]
       [updated (zipper-set z 100)])
  (assert-equal 100 (zipper-focus updated)))"
                 ";; List is updated
(assert-equal '(100 2 3) (zipper->list (zipper-set (list->zipper '(1 2 3)) 100)))"
                 ";; Position unchanged
(let* ([z (zipper-from-position '(a b c) 1)]
       [updated (zipper-set z 'X)])
  (assert-equal 1 (zipper-position updated))
  (assert-equal '(a X c) (zipper->list updated)))"))
    (available-modules (prelude combinators))
    (hints ("Guard: (if (not (zipper-has-focus? z)) (error ...) ...)"
            "Replace focus: (make-zipper (zipper-left z) (just x) (zipper-right z))"
            "Left and right contexts are unchanged"))
    (reference-solution "(define (zipper-set z x)
  (if (not (zipper-has-focus? z))
      (error 'zipper-set \"no focused element to set\")
      (make-zipper (zipper-left z)
                   (just x)
                   (zipper-right z))))")
    (alternatives ())
    (source-commit "71b6b156")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement zipper-delete-left
  ;; ──────────────────────────────────────────────
  ;; Delete the element immediately to the left of focus.

  (task
    (id "fp/zipper/delete-left-impl")
    (module zipper)
    (tier 0)
    (type implement)
    (description "Implement zipper-delete-left: remove the element immediately to the left of the focus. If there is nothing to the left (left context is empty), return the zipper unchanged (no-op). Otherwise, drop the first element of the left context (which is the closest element to the left, since left is stored in reverse order). The focus and right context stay the same.")
    (context "Available: zipper-left, zipper-focus-maybe, zipper-right, null?, cdr, make-zipper, if, let. Guard + one constructor.")
    (before "(define (zipper-delete-left z)
  ;; TODO: remove element to the left of focus
  z)")
    (test-file "lattice/fp/data/test-zipper.ss")
    (test-forms (";; Delete left neighbor
(let* ([z (zipper-from-position '(1 2 3 4) 2)]
       [deleted (zipper-delete-left z)])
  (assert-equal 3 (zipper-focus deleted))
  (assert-equal '(1 3 4) (zipper->list deleted)))"
                 ";; No-op at start
(let* ([z (list->zipper '(1 2 3))]
       [result (zipper-delete-left z)])
  (assert-equal '(1 2 3) (zipper->list result)))"
                 ";; Position shifts left by one
(let* ([z (zipper-from-position '(a b c d) 2)]
       [deleted (zipper-delete-left z)])
  (assert-equal 1 (zipper-position deleted)))"))
    (available-modules (prelude combinators))
    (hints ("Get left context: (let ([left (zipper-left z)]) ...)"
            "If empty, return z unchanged"
            "Otherwise: (make-zipper (cdr left) (zipper-focus-maybe z) (zipper-right z))"
            "(cdr left) drops the closest-left element"))
    (reference-solution "(define (zipper-delete-left z)
  (let ([left (zipper-left z)])
    (if (null? left)
        z
        (make-zipper (cdr left)
                     (zipper-focus-maybe z)
                     (zipper-right z)))))")
    (alternatives ())
    (source-commit "71b6b156")
    (difficulty 1))

) ;; end tasks
