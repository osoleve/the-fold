;;; lattice/data/test-alist.ss — Tests for association list helpers

(load "core/testing/test-framework.ss")
(load "lattice/data/alist.ss")

(test-group "alist-ref"
  (define-test "finds existing key"
    (assert-equal 2 (alist-ref '((a . 1) (b . 2) (c . 3)) 'b)))

  (define-test "returns #f for missing key"
    (assert-false (alist-ref '((a . 1)) 'z)))

  (define-test "returns first match on duplicate keys"
    (assert-equal 10 (alist-ref '((a . 10) (a . 20)) 'a)))

  (define-test "handles empty alist"
    (assert-false (alist-ref '() 'x))))

(test-group "alist-ref/default"
  (define-test "finds existing key"
    (assert-equal 2 (alist-ref/default '((a . 1) (b . 2)) 'b 99)))

  (define-test "returns default for missing key"
    (assert-equal 99 (alist-ref/default '((a . 1)) 'z 99)))

  (define-test "returns default for empty alist"
    (assert-equal 0 (alist-ref/default '() 'x 0))))

(test-group "alist-set"
  (define-test "adds new key"
    (let ([result (alist-set '((a . 1)) 'b 2)])
      (assert-equal 1 (alist-ref result 'a))
      (assert-equal 2 (alist-ref result 'b))))

  (define-test "replaces existing key"
    (let ([result (alist-set '((a . 1) (b . 2)) 'a 10)])
      (assert-equal 10 (alist-ref result 'a))
      (assert-equal 2 (alist-ref result 'b))))

  (define-test "works on empty alist"
    (let ([result (alist-set '() 'x 42)])
      (assert-equal 42 (alist-ref result 'x))))

  (define-test "does not duplicate keys"
    (let ([result (alist-set '((a . 1) (b . 2)) 'a 10)])
      (assert-equal 2 (length result)))))

(test-group "alist-update"
  (define-test "updates existing key"
    (let ([result (alist-update '((a . 1) (b . 2)) 'a (lambda (v) (+ v 10)) 0)])
      (assert-equal 11 (alist-ref result 'a))))

  (define-test "uses default for missing key"
    (let ([result (alist-update '((a . 1)) 'b (lambda (v) (+ v 10)) 0)])
      (assert-equal 10 (alist-ref result 'b))))

  (define-test "uses default on empty alist"
    (let ([result (alist-update '() 'x (lambda (v) (* v 2)) 5)])
      (assert-equal 10 (alist-ref result 'x)))))

(test-group "alist-remove"
  (define-test "removes existing key"
    (let ([result (alist-remove '((a . 1) (b . 2) (c . 3)) 'b)])
      (assert-false (alist-ref result 'b))
      (assert-equal 1 (alist-ref result 'a))
      (assert-equal 3 (alist-ref result 'c))))

  (define-test "no-op for missing key"
    (let ([result (alist-remove '((a . 1)) 'z)])
      (assert-equal '((a . 1)) result)))

  (define-test "handles empty alist"
    (assert-equal '() (alist-remove '() 'x)))

  (define-test "removes only first occurrence"
    (let ([result (alist-remove '((a . 1) (b . 2) (a . 3)) 'a)])
      (assert-equal 3 (alist-ref result 'a))
      (assert-equal 2 (length result)))))

(test-group "alist-merge"
  (define-test "merges disjoint alists"
    (let ([result (alist-merge '((a . 1)) '((b . 2)))])
      (assert-equal 1 (alist-ref result 'a))
      (assert-equal 2 (alist-ref result 'b))))

  (define-test "b wins on conflict"
    (let ([result (alist-merge '((a . 1) (b . 2)) '((a . 10) (c . 3)))])
      (assert-equal 10 (alist-ref result 'a))
      (assert-equal 2 (alist-ref result 'b))
      (assert-equal 3 (alist-ref result 'c))))

  (define-test "merging with empty"
    (assert-equal 1 (alist-ref (alist-merge '() '((a . 1))) 'a))
    (assert-equal 1 (alist-ref (alist-merge '((a . 1)) '()) 'a))))

(test-group "alist-keys"
  (define-test "extracts keys"
    (assert-equal '(a b c) (alist-keys '((a . 1) (b . 2) (c . 3)))))

  (define-test "empty alist gives empty keys"
    (assert-equal '() (alist-keys '()))))

(test-group "alist-values"
  (define-test "extracts values"
    (assert-equal '(1 2 3) (alist-values '((a . 1) (b . 2) (c . 3)))))

  (define-test "empty alist gives empty values"
    (assert-equal '() (alist-values '()))))

(test-group "alist-map"
  (define-test "maps over values"
    (let ([result (alist-map (lambda (k v) (* v 2)) '((a . 1) (b . 2) (c . 3)))])
      (assert-equal '((a . 2) (b . 4) (c . 6)) result)))

  (define-test "fn receives key and value"
    (let ([result (alist-map (lambda (k v) (list k v)) '((x . 1)))])
      (assert-equal '((x . (x 1))) result)))

  (define-test "empty alist maps to empty"
    (assert-equal '() (alist-map (lambda (k v) v) '()))))

(run-all-tests)
