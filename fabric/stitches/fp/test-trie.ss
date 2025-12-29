;;; fabric/stitches/fp/test-trie.ss — Tests for Trie Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/trie.ss")

(display "\n")
(display "==============================================================\n")
(display "         TRIE (PREFIX TREE) TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Basic Trie Construction Tests
;;; ============================================================

(test-group trie-construction
  (define-test empty-trie-test
    (assert-true (trie? empty-trie))
    (assert-true (trie-empty? empty-trie))
    (assert-true (nothing? (trie-value empty-trie)))
    (assert-equal '() (trie-children empty-trie)))

  (define-test make-trie-test
    (let ([t (make-trie (just 42) '())])
      (assert-true (trie? t))
      (assert-false (trie-empty? t))
      (assert-true (just? (trie-value t)))
      (assert-equal 42 (from-just (trie-value t))))))

;;; ============================================================
;;; Basic Operations Tests
;;; ============================================================

(test-group basic-operations
  (define-test insert-lookup-test
    (let* ([t (trie-insert empty-trie '(a b c) "abc")]
           [result (trie-lookup t '(a b c))])
      (assert-true (just? result))
      (assert-equal "abc" (from-just result))))

  (define-test insert-multiple-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b) 1)]
           [t (trie-insert t '(a b c) 2)]
           [t (trie-insert t '(a) 3)])
      (assert-equal 1 (from-just (trie-lookup t '(a b))))
      (assert-equal 2 (from-just (trie-lookup t '(a b c))))
      (assert-equal 3 (from-just (trie-lookup t '(a))))))

  (define-test lookup-not-found-test
    (let ([t (trie-insert empty-trie '(a b c) "abc")])
      (assert-true (nothing? (trie-lookup t '(x y z))))
      (assert-true (nothing? (trie-lookup t '(a b))))
      (assert-true (nothing? (trie-lookup t '(a b c d))))))

  (define-test contains-test
    (let ([t (trie-insert empty-trie '(h e l l o) "hello")])
      (assert-true (trie-contains? t '(h e l l o)))
      (assert-false (trie-contains? t '(h e l l)))
      (assert-false (trie-contains? t '(w o r l d)))))

  (define-test delete-test
    (let* ([t (trie-insert empty-trie '(a b c) "abc")]
           [t (trie-delete t '(a b c))])
      (assert-false (trie-contains? t '(a b c)))))

  (define-test delete-preserves-prefix-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b) 1)]
           [t (trie-insert t '(a b c) 2)]
           [t (trie-delete t '(a b c))])
      (assert-true (trie-contains? t '(a b)))
      (assert-false (trie-contains? t '(a b c)))))

  (define-test delete-preserves-extension-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b) 1)]
           [t (trie-insert t '(a b c) 2)]
           [t (trie-delete t '(a b))])
      (assert-false (trie-contains? t '(a b)))
      (assert-true (trie-contains? t '(a b c)))))

  (define-test insert-overwrites-test
    (let* ([t (trie-insert empty-trie '(x) 1)]
           [t (trie-insert t '(x) 2)])
      (assert-equal 2 (from-just (trie-lookup t '(x)))))))

;;; ============================================================
;;; Prefix Operations Tests
;;; ============================================================

(test-group prefix-operations
  (define-test subtrie-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a b d) 2)]
           [sub (trie-subtrie t '(a b))])
      (assert-true (just? sub))
      (let ([s (from-just sub)])
        (assert-true (trie-contains? s '(c)))
        (assert-true (trie-contains? s '(d))))))

  (define-test subtrie-not-found-test
    (let ([t (trie-insert empty-trie '(a b c) 1)])
      (assert-true (nothing? (trie-subtrie t '(x y))))))

  (define-test has-prefix-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a b d) 2)])
      (assert-true (trie-has-prefix? t '(a)))
      (assert-true (trie-has-prefix? t '(a b)))
      (assert-true (trie-has-prefix? t '(a b c)))
      (assert-false (trie-has-prefix? t '(x)))))

  (define-test keys-with-prefix-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a b d) 2)]
           [t (trie-insert t '(a x) 3)]
           [keys (trie-keys-with-prefix t '(a b))])
      (assert-equal 2 (length keys))
      (assert-true (not (not (member '(a b c) keys))))
      (assert-true (not (not (member '(a b d) keys))))))

  (define-test values-with-prefix-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a b d) 2)]
           [t (trie-insert t '(a x) 3)]
           [vals (trie-values-with-prefix t '(a b))])
      (assert-equal 2 (length vals))
      (assert-true (not (not (member 1 vals))))
      (assert-true (not (not (member 2 vals)))))))

;;; ============================================================
;;; Enumeration Tests
;;; ============================================================

(test-group enumeration
  (define-test trie-keys-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [t (trie-insert t '(a b) 3)]
           [keys (trie-keys t)])
      (assert-equal 3 (length keys))
      (assert-true (not (not (member '(a) keys))))
      (assert-true (not (not (member '(b) keys))))
      (assert-true (not (not (member '(a b) keys))))))

  (define-test trie-values-test
    (let* ([t empty-trie]
           [t (trie-insert t '(x) 10)]
           [t (trie-insert t '(y) 20)]
           [vals (trie-values t)])
      (assert-equal 2 (length vals))
      (assert-true (not (not (member 10 vals))))
      (assert-true (not (not (member 20 vals))))))

  (define-test trie-entries-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [entries (trie-entries t)])
      (assert-equal 2 (length entries))))

  (define-test trie-size-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [t (trie-insert t '(a b) 3)])
      (assert-equal 3 (trie-size t)))
    (assert-equal 0 (trie-size empty-trie))))

;;; ============================================================
;;; String Operations Tests
;;; ============================================================

(test-group string-operations
  (define-test insert-lookup-string-test
    (let ([t (trie-insert-string empty-trie "hello" "world")])
      (assert-true (just? (trie-lookup-string t "hello")))
      (assert-equal "world" (from-just (trie-lookup-string t "hello")))))

  (define-test contains-string-test
    (let ([t (trie-insert-string empty-trie "cat" 1)])
      (assert-true (trie-contains-string? t "cat"))
      (assert-false (trie-contains-string? t "car"))
      (assert-false (trie-contains-string? t "cats"))))

  (define-test delete-string-test
    (let* ([t (trie-insert-string empty-trie "hello" 1)]
           [t (trie-delete-string t "hello")])
      (assert-false (trie-contains-string? t "hello"))))

  (define-test complete-test
    (let* ([t empty-trie]
           [t (trie-insert-string t "cat" 1)]
           [t (trie-insert-string t "car" 2)]
           [t (trie-insert-string t "card" 3)]
           [t (trie-insert-string t "dog" 4)]
           [completions (trie-complete t "ca")])
      (assert-equal 3 (length completions))
      (assert-true (not (not (member "cat" completions))))
      (assert-true (not (not (member "car" completions))))
      (assert-true (not (not (member "card" completions))))))

  (define-test strings-to-trie-test
    (let ([t (strings->trie '("apple" "apricot" "banana"))])
      (assert-equal 3 (trie-size t))
      (assert-true (trie-contains-string? t "apple"))
      (assert-true (trie-contains-string? t "apricot"))
      (assert-true (trie-contains-string? t "banana"))))

  (define-test alist-to-trie-test
    (let ([t (alist->trie '(("one" . 1) ("two" . 2) ("three" . 3)))])
      (assert-equal 3 (trie-size t))
      (assert-equal 1 (from-just (trie-lookup-string t "one")))
      (assert-equal 2 (from-just (trie-lookup-string t "two")))
      (assert-equal 3 (from-just (trie-lookup-string t "three"))))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
  (define-test trie-map-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [mapped (trie-map (lambda (x) (* x 10)) t)])
      (assert-equal 10 (from-just (trie-lookup mapped '(a))))
      (assert-equal 20 (from-just (trie-lookup mapped '(b))))))

  (define-test trie-filter-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [t (trie-insert t '(c) 3)]
           [filtered (trie-filter (lambda (x) (> x 1)) t)])
      (assert-false (trie-contains? filtered '(a)))
      (assert-true (trie-contains? filtered '(b)))
      (assert-true (trie-contains? filtered '(c)))))

  (define-test trie-fold-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 1)]
           [t (trie-insert t '(b) 2)]
           [t (trie-insert t '(c) 3)]
           [sum (trie-fold + 0 t)])
      (assert-equal 6 sum)))

  (define-test trie-fold-entries-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a) 10)]
           [t (trie-insert t '(b) 20)]
           [result (trie-fold-entries
                    (lambda (acc key val)
                      (cons (cons key val) acc))
                    '()
                    t)])
      (assert-equal 2 (length result)))))

;;; ============================================================
;;; Merge Tests
;;; ============================================================

(test-group merging
  (define-test trie-merge-test
    (let* ([t1 (trie-insert empty-trie '(a) 1)]
           [t2 (trie-insert empty-trie '(b) 2)]
           [merged (trie-merge t1 t2)])
      (assert-equal 2 (trie-size merged))
      (assert-true (trie-contains? merged '(a)))
      (assert-true (trie-contains? merged '(b)))))

  (define-test trie-merge-overwrites-test
    (let* ([t1 (trie-insert empty-trie '(x) 1)]
           [t2 (trie-insert empty-trie '(x) 2)]
           [merged (trie-merge t1 t2)])
      (assert-equal 2 (from-just (trie-lookup merged '(x))))))

  (define-test trie-merge-with-test
    (let* ([t1 (trie-insert empty-trie '(x) 10)]
           [t2 (trie-insert empty-trie '(x) 20)]
           [merged (trie-merge-with + t1 t2)])
      (assert-equal 30 (from-just (trie-lookup merged '(x)))))))

;;; ============================================================
;;; Longest Prefix Match Tests
;;; ============================================================

(test-group longest-prefix
  (define-test longest-prefix-exact-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) "abc")]
           [result (trie-longest-prefix t '(a b c))])
      (assert-true (not (not result)))
      (assert-equal '(a b c) (car result))
      (assert-equal "abc" (cdr result))))

  (define-test longest-prefix-partial-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b) "ab")]
           [result (trie-longest-prefix t '(a b c d))])
      (assert-true (not (not result)))
      (assert-equal '(a b) (car result))
      (assert-equal "ab" (cdr result))))

  (define-test longest-prefix-none-test
    (let* ([t (trie-insert empty-trie '(x y) "xy")]
           [result (trie-longest-prefix t '(a b c))])
      (assert-false result)))

  (define-test longest-prefix-string-test
    (let* ([t empty-trie]
           [t (trie-insert-string t "http" "protocol")]
           [t (trie-insert-string t "https" "secure")]
           [result (trie-longest-prefix-string t "https://example.com")])
      (assert-true (not (not result)))
      (assert-equal "https" (car result))
      (assert-equal "secure" (cdr result)))))

;;; ============================================================
;;; Specialized Builders Tests
;;; ============================================================

(test-group specialized-builders
  (define-test trie-set-test
    (let ([t (trie-set '((a b) (c d) (a b)))])
      (assert-equal 2 (trie-size t))  ; Duplicates collapsed
      (assert-true (trie-contains? t '(a b)))
      (assert-true (trie-contains? t '(c d)))))

  (define-test trie-string-set-test
    (let ([t (trie-string-set '("hello" "world" "hello"))])
      (assert-equal 2 (trie-size t))
      (assert-true (trie-contains-string? t "hello"))
      (assert-true (trie-contains-string? t "world"))))

  (define-test trie-frequency-test
    (let ([t (trie-frequency '((a) (b) (a) (a) (b)))])
      (assert-equal 3 (from-just (trie-lookup t '(a))))
      (assert-equal 2 (from-just (trie-lookup t '(b))))))

  (define-test trie-string-frequency-test
    (let ([t (trie-string-frequency '("cat" "dog" "cat" "cat"))])
      (assert-equal 3 (from-just (trie-lookup-string t "cat")))
      (assert-equal 1 (from-just (trie-lookup-string t "dog"))))))

;;; ============================================================
;;; Autocomplete Tests
;;; ============================================================

(test-group autocomplete
  (define-test complete-ranked-test
    (let* ([t empty-trie]
           [t (trie-insert-string t "apple" 10)]
           [t (trie-insert-string t "application" 50)]
           [t (trie-insert-string t "apply" 30)]
           [results (trie-complete-ranked t "app" 2)])
      (assert-equal 2 (length results))
      ;; Should be sorted by value descending
      (assert-equal "application" (car results))
      (assert-equal "apply" (cadr results)))))

;;; ============================================================
;;; Wildcard Matching Tests
;;; ============================================================

(test-group wildcard-matching
  (define-test match-wildcard-single-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a x c) 2)]
           [t (trie-insert t '(a y c) 3)]
           [matches (trie-match-wildcard t '(a wild c))])
      (assert-equal 3 (length matches))
      (assert-true (not (not (member 1 matches))))
      (assert-true (not (not (member 2 matches))))
      (assert-true (not (not (member 3 matches))))))

  (define-test match-wildcard-no-match-test
    (let* ([t (trie-insert empty-trie '(a b c) 1)]
           [matches (trie-match-wildcard t '(x wild z))])
      (assert-equal 0 (length matches)))))

;;; ============================================================
;;; Common Prefix Tests
;;; ============================================================

(test-group common-prefix
  (define-test common-prefix-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b c) 1)]
           [t (trie-insert t '(a b d) 2)]
           [prefix (trie-common-prefix t)])
      (assert-equal '(a b) prefix)))

  (define-test common-prefix-divergent-test
    (let* ([t empty-trie]
           [t (trie-insert t '(a b) 1)]
           [t (trie-insert t '(c d) 2)]
           [prefix (trie-common-prefix t)])
      (assert-equal '() prefix))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test empty-key-test
    (let ([t (trie-insert empty-trie '() "root")])
      (assert-true (trie-contains? t '()))
      (assert-equal "root" (from-just (trie-lookup t '())))))

  (define-test empty-string-key-test
    (let ([t (trie-insert-string empty-trie "" "empty")])
      (assert-true (trie-contains-string? t ""))
      (assert-equal "empty" (from-just (trie-lookup-string t "")))))

  (define-test single-char-keys-test
    (let* ([t (strings->trie '("a" "b" "c"))])
      (assert-equal 3 (trie-size t))
      (assert-true (trie-contains-string? t "a"))))

  (define-test delete-nonexistent-test
    (let* ([t (trie-insert empty-trie '(a) 1)]
           [t2 (trie-delete t '(b))])
      ;; Should return unchanged trie
      (assert-true (trie-contains? t2 '(a))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All trie tests passed.\n")
    (display "\n[FAILURE] Some trie tests failed.\n"))
