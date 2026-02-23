;;; lattice/data/test-hamt.ss — Tests for Persistent HAMT
;;; Run with: scheme --script lattice/data/test-hamt.ss

(load "core/testing/test-framework.ss")
(load "lattice/data/hamt.ss")
(load "lattice/data/sort.ss")

;;; ============================================================
;;; Empty HAMT
;;; ============================================================

(test-group "hamt-empty"
  (define-test "hamt-empty is empty"
    (assert-true (hamt-empty? hamt-empty)))

  (define-test "non-empty HAMT is not empty"
    (assert-false (hamt-empty? (hamt-assoc 'a 1 hamt-empty))))

  (define-test "lookup on empty returns #f"
    (assert-false (hamt-lookup 'x hamt-empty)))

  (define-test "has-key? on empty returns #f"
    (assert-false (hamt-has-key? 'x hamt-empty)))

  (define-test "size of empty is 0"
    (assert-equal 0 (hamt-size hamt-empty)))

  (define-test "keys of empty is nil"
    (assert-equal '() (hamt-keys hamt-empty)))

  (define-test "entries of empty is nil"
    (assert-equal '() (hamt-entries hamt-empty))))

;;; ============================================================
;;; Single Entry
;;; ============================================================

(test-group "hamt-single"
  (define h (hamt-assoc 'key 42 hamt-empty))

  (define-test "lookup finds inserted key"
    (assert-equal 42 (hamt-lookup 'key h)))

  (define-test "size is 1"
    (assert-equal 1 (hamt-size h)))

  (define-test "has-key? true for present key"
    (assert-true (hamt-has-key? 'key h)))

  (define-test "has-key? false for absent key"
    (assert-false (hamt-has-key? 'other h)))

  (define-test "lookup returns #f for absent key"
    (assert-false (hamt-lookup 'other h)))

  (define-test "dissoc removes only entry"
    (assert-true (hamt-empty? (hamt-dissoc 'key h))))

  (define-test "dissoc of absent key is identity"
    (assert-equal 42 (hamt-lookup 'key (hamt-dissoc 'other h)))))

;;; ============================================================
;;; Multiple Entries
;;; ============================================================

(test-group "hamt-multiple"
  (define h (hamt-assoc 'c 3 (hamt-assoc 'b 2 (hamt-assoc 'a 1 hamt-empty))))

  (define-test "all keys retrievable"
    (assert-equal 1 (hamt-lookup 'a h))
    (assert-equal 2 (hamt-lookup 'b h))
    (assert-equal 3 (hamt-lookup 'c h)))

  (define-test "size is 3"
    (assert-equal 3 (hamt-size h)))

  (define-test "update existing key"
    (let ([h2 (hamt-assoc 'b 99 h)])
      (assert-equal 99 (hamt-lookup 'b h2))
      (assert-equal 1 (hamt-lookup 'a h2))
      (assert-equal 3 (hamt-lookup 'c h2))))

  (define-test "delete middle key"
    (let ([h2 (hamt-dissoc 'b h)])
      (assert-equal 2 (hamt-size h2))
      (assert-false (hamt-lookup 'b h2))
      (assert-equal 1 (hamt-lookup 'a h2))
      (assert-equal 3 (hamt-lookup 'c h2)))))

;;; ============================================================
;;; Key Types
;;; ============================================================

(test-group "hamt-key-types"
  (define-test "integer keys"
    (let ([h (hamt-assoc 42 "answer" (hamt-assoc 0 "zero" hamt-empty))])
      (assert-equal "answer" (hamt-lookup 42 h))
      (assert-equal "zero" (hamt-lookup 0 h))))

  (define-test "string keys"
    (let ([h (hamt-assoc "hello" 1 (hamt-assoc "world" 2 hamt-empty))])
      (assert-equal 1 (hamt-lookup "hello" h))
      (assert-equal 2 (hamt-lookup "world" h))))

  (define-test "pair keys (structural equality)"
    (let ([h (hamt-assoc (cons 1 2) "a" (hamt-assoc (cons 3 4) "b" hamt-empty))])
      (assert-equal "a" (hamt-lookup (cons 1 2) h))
      (assert-equal "b" (hamt-lookup (cons 3 4) h))))

  (define-test "list keys"
    (let ([h (hamt-assoc '(1 2 3) "x" hamt-empty)])
      (assert-equal "x" (hamt-lookup '(1 2 3) h))
      (assert-false (hamt-lookup '(1 2) h))))

  (define-test "mixed key types"
    (let ([h (hamt-assoc 1 "int" (hamt-assoc "1" "str" (hamt-assoc 'one "sym" hamt-empty)))])
      (assert-equal "int" (hamt-lookup 1 h))
      (assert-equal "str" (hamt-lookup "1" h))
      (assert-equal "sym" (hamt-lookup 'one h)))))

;;; ============================================================
;;; #f as a Value
;;; ============================================================

(test-group "hamt-false-values"
  (define h (hamt-assoc 'key #f hamt-empty))

  (define-test "lookup returns #f (the stored value)"
    (assert-false (hamt-lookup 'key h)))

  (define-test "has-key? distinguishes stored #f from absent"
    (assert-true (hamt-has-key? 'key h))
    (assert-false (hamt-has-key? 'other h))))

;;; ============================================================
;;; Persistence (Immutability)
;;; ============================================================

(test-group "hamt-persistence"
  (define h1 (hamt-assoc 'a 1 hamt-empty))
  (define h2 (hamt-assoc 'b 2 h1))
  (define h3 (hamt-dissoc 'a h2))

  (define-test "insert doesn't modify original"
    (assert-equal 1 (hamt-size h1))
    (assert-false (hamt-lookup 'b h1)))

  (define-test "delete doesn't modify original"
    (assert-equal 2 (hamt-size h2))
    (assert-equal 1 (hamt-lookup 'a h2)))

  (define-test "derived HAMT has correct state"
    (assert-equal 1 (hamt-size h3))
    (assert-false (hamt-lookup 'a h3))
    (assert-equal 2 (hamt-lookup 'b h3))))

;;; ============================================================
;;; Scale
;;; ============================================================

(test-group "hamt-scale"
  (define n 1000)
  (define big
    (let loop ([i 0] [h hamt-empty])
      (if (= i n) h
          (loop (+ i 1) (hamt-assoc i (* i i) h)))))

  (define-test "insert 1000 keys"
    (assert-equal n (hamt-size big)))

  (define-test "lookup all 1000 keys"
    (let loop ([i 0])
      (when (< i n)
        (assert-equal (* i i) (hamt-lookup i big))
        (loop (+ i 1)))))

  (define-test "delete 500 keys"
    (let ([half (let loop ([i 0] [h big])
                  (if (= i 500) h
                      (loop (+ i 1) (hamt-dissoc i h))))])
      (assert-equal 500 (hamt-size half))
      (assert-false (hamt-lookup 0 half))
      (assert-false (hamt-lookup 499 half))
      (assert-equal (* 500 500) (hamt-lookup 500 half))
      (assert-equal (* 999 999) (hamt-lookup 999 half)))))

;;; ============================================================
;;; Hash Collisions (synthetic)
;;; ============================================================
;;; Force collisions by inserting keys that produce the same hash
;;; fragment at the first level. Integer keys often cluster.

(test-group "hamt-collisions"
  ;; Build a HAMT with enough integer keys to guarantee some share
  ;; hash prefixes (pigeonhole: 33 keys in 32 slots = at least 1 collision)
  (define h
    (let loop ([i 0] [h hamt-empty])
      (if (= i 100) h
          (loop (+ i 1) (hamt-assoc (* i 997) i h)))))

  (define-test "all 100 keys retrievable despite hash pressure"
    (let loop ([i 0])
      (when (< i 100)
        (assert-equal i (hamt-lookup (* i 997) h))
        (loop (+ i 1)))))

  (define-test "size correct under hash pressure"
    (assert-equal 100 (hamt-size h)))

  (define-test "deletion works under hash pressure"
    (let ([h2 (hamt-dissoc (* 50 997) h)])
      (assert-equal 99 (hamt-size h2))
      (assert-false (hamt-lookup (* 50 997) h2))
      (assert-equal 49 (hamt-lookup (* 49 997) h2))
      (assert-equal 51 (hamt-lookup (* 51 997) h2))))

  (define-test "update works under hash pressure"
    (let ([h2 (hamt-assoc (* 25 997) 'updated h)])
      (assert-equal 'updated (hamt-lookup (* 25 997) h2))
      (assert-equal 100 (hamt-size h2)))))

;;; ============================================================
;;; True Hash Collisions (same 32-bit hash, different keys)
;;; ============================================================
;;; These exercise the hamt-collision node path (alist-based
;;; bucket for keys that produce identical hashes).

(test-group "hamt-true-collisions"
  ;; Verified collision pairs: same (fxlogand (equal-hash k) #x7FFFFFFF)
  ;; k2430 and k4084 both hash to 24494962
  ;; k10872 and k15993 both hash to 1859062013
  (define k1 'k2430)
  (define k2 'k4084)
  (define k3 'k10872)
  (define k4 'k15993)

  (define-test "insert two keys with same hash"
    (let* ([h (hamt-assoc k1 "first" hamt-empty)]
           [h (hamt-assoc k2 "second" h)])
      (assert-equal "first" (hamt-lookup k1 h))
      (assert-equal "second" (hamt-lookup k2 h))
      (assert-equal 2 (hamt-size h))))

  (define-test "update value in collision bucket"
    (let* ([h (hamt-assoc k1 "old" hamt-empty)]
           [h (hamt-assoc k2 "other" h)]
           [h (hamt-assoc k1 "new" h)])
      (assert-equal "new" (hamt-lookup k1 h))
      (assert-equal "other" (hamt-lookup k2 h))
      (assert-equal 2 (hamt-size h))))

  (define-test "delete from collision bucket"
    (let* ([h (hamt-assoc k1 1 hamt-empty)]
           [h (hamt-assoc k2 2 h)]
           [h (hamt-dissoc k1 h)])
      (assert-false (hamt-lookup k1 h))
      (assert-equal 2 (hamt-lookup k2 h))
      (assert-equal 1 (hamt-size h))))

  (define-test "delete last from collision demotes to leaf"
    (let* ([h (hamt-assoc k1 1 hamt-empty)]
           [h (hamt-assoc k2 2 h)]
           [h (hamt-dissoc k1 h)]
           [h (hamt-dissoc k2 h)])
      (assert-true (hamt-empty? h))))

  (define-test "has-key? works in collision bucket"
    (let* ([h (hamt-assoc k1 1 hamt-empty)]
           [h (hamt-assoc k2 2 h)])
      (assert-true (hamt-has-key? k1 h))
      (assert-true (hamt-has-key? k2 h))
      (assert-false (hamt-has-key? 'nonexistent h))))

  (define-test "multiple collision pairs coexist"
    (let* ([h (hamt-assoc k1 "a" hamt-empty)]
           [h (hamt-assoc k2 "b" h)]
           [h (hamt-assoc k3 "c" h)]
           [h (hamt-assoc k4 "d" h)])
      (assert-equal "a" (hamt-lookup k1 h))
      (assert-equal "b" (hamt-lookup k2 h))
      (assert-equal "c" (hamt-lookup k3 h))
      (assert-equal "d" (hamt-lookup k4 h))
      (assert-equal 4 (hamt-size h))))

  (define-test "fold traverses collision entries"
    (let* ([h (hamt-assoc k1 10 hamt-empty)]
           [h (hamt-assoc k2 20 h)])
      (assert-equal 30 (hamt-fold (lambda (acc k v) (+ acc v)) 0 h))))

  (define-test "merge-with on collision keys"
    (let* ([h1 (hamt-assoc k1 10 hamt-empty)]
           [h2 (hamt-assoc k1 20 hamt-empty)])
      (let ([m (hamt-merge-with + h1 h2)])
        (assert-equal 30 (hamt-lookup k1 m))))))

;;; ============================================================
;;; Fold
;;; ============================================================

(test-group "hamt-fold"
  (define h (hamt-assoc 'a 10 (hamt-assoc 'b 20 (hamt-assoc 'c 30 hamt-empty))))

  (define-test "fold sum of values"
    (assert-equal 60 (hamt-fold (lambda (acc k v) (+ acc v)) 0 h)))

  (define-test "fold count entries"
    (assert-equal 3 (hamt-fold (lambda (acc k v) (+ acc 1)) 0 h)))

  (define-test "fold on empty"
    (assert-equal 0 (hamt-fold (lambda (acc k v) (+ acc 1)) 0 hamt-empty))))

;;; ============================================================
;;; Keys / Values / Entries
;;; ============================================================

(test-group "hamt-enumeration"
  (define h (hamt-assoc 3 "c" (hamt-assoc 1 "a" (hamt-assoc 2 "b" hamt-empty))))

  (define-test "keys returns all keys"
    (assert-equal '(1 2 3) (sort-by < (hamt-keys h))))

  (define-test "values returns all values"
    (assert-equal 3 (length (hamt-values h))))

  (define-test "entries returns pairs"
    (let ([es (hamt-entries h)])
      (assert-equal 3 (length es))
      (assert-true (pair? (car es))))))

;;; ============================================================
;;; Map Values
;;; ============================================================

(test-group "hamt-map-values"
  (define h (hamt-assoc 'a 1 (hamt-assoc 'b 2 hamt-empty)))

  (define-test "map doubles values"
    (let ([h2 (hamt-map-values (lambda (v) (* v 2)) h)])
      (assert-equal 2 (hamt-lookup 'a h2))
      (assert-equal 4 (hamt-lookup 'b h2))))

  (define-test "map on empty"
    (assert-true (hamt-empty? (hamt-map-values (lambda (v) v) hamt-empty))))

  (define-test "map preserves size"
    (let ([h2 (hamt-map-values (lambda (v) (+ v 100)) h)])
      (assert-equal 2 (hamt-size h2)))))

;;; ============================================================
;;; Filter
;;; ============================================================

(test-group "hamt-filter"
  (define h (hamt-assoc 'a 1 (hamt-assoc 'b 5 (hamt-assoc 'c 10 hamt-empty))))

  (define-test "filter keeps matching entries"
    (let ([h2 (hamt-filter (lambda (k v) (> v 3)) h)])
      (assert-equal 2 (hamt-size h2))
      (assert-true (hamt-has-key? 'b h2))
      (assert-true (hamt-has-key? 'c h2))
      (assert-false (hamt-has-key? 'a h2))))

  (define-test "filter all returns empty"
    (let ([h2 (hamt-filter (lambda (k v) #f) h)])
      (assert-true (hamt-empty? h2))))

  (define-test "filter none returns all"
    (let ([h2 (hamt-filter (lambda (k v) #t) h)])
      (assert-equal 3 (hamt-size h2)))))

;;; ============================================================
;;; Merge
;;; ============================================================

(test-group "hamt-merge"
  (define ha (hamt-assoc 'a 1 (hamt-assoc 'b 2 hamt-empty)))
  (define hb (hamt-assoc 'b 99 (hamt-assoc 'c 3 hamt-empty)))

  (define-test "merge union of keys"
    (let ([m (hamt-merge ha hb)])
      (assert-equal 3 (hamt-size m))
      (assert-equal 1 (hamt-lookup 'a m))
      (assert-equal 3 (hamt-lookup 'c m))))

  (define-test "merge right wins on conflict"
    (let ([m (hamt-merge ha hb)])
      (assert-equal 99 (hamt-lookup 'b m))))

  (define-test "merge with empty"
    (assert-equal 2 (hamt-size (hamt-merge ha hamt-empty)))
    (assert-equal 2 (hamt-size (hamt-merge hamt-empty ha))))

  (define-test "merge-with resolves conflicts"
    (let ([m (hamt-merge-with + ha hb)])
      (assert-equal 101 (hamt-lookup 'b m))))  ; 2 + 99

  (define-test "merge-with handles #f values correctly"
    (let* ([h1 (hamt-assoc 'k #f hamt-empty)]
           [h2 (hamt-assoc 'k 1 hamt-empty)]
           [m (hamt-merge-with (lambda (a b) 'merged) h1 h2)])
      (assert-equal 'merged (hamt-lookup 'k m)))))

;;; ============================================================
;;; Conversions
;;; ============================================================

(test-group "hamt-conversions"
  (define alist '((a . 1) (b . 2) (c . 3)))

  (define-test "alist->hamt preserves entries"
    (let ([h (alist->hamt alist)])
      (assert-equal 3 (hamt-size h))
      (assert-equal 1 (hamt-lookup 'a h))
      (assert-equal 3 (hamt-lookup 'c h))))

  (define-test "hamt->dict round-trip"
    (let* ([h (alist->hamt alist)]
           [d (hamt->dict h)])
      (assert-equal 3 (length d))))

  (define-test "dict->hamt round-trip"
    (let* ([h (dict->hamt alist)]
           [d (hamt->dict h)]
           [h2 (dict->hamt d)])
      (assert-equal 3 (hamt-size h2))
      (assert-equal 1 (hamt-lookup 'a h2)))))

;;; ============================================================
;;; Dissoc Edge Cases
;;; ============================================================

(test-group "hamt-dissoc-edges"
  (define-test "dissoc from empty"
    (assert-true (hamt-empty? (hamt-dissoc 'x hamt-empty))))

  (define-test "dissoc absent key from non-empty"
    (let ([h (hamt-assoc 'a 1 hamt-empty)])
      (assert-equal 1 (hamt-size (hamt-dissoc 'b h)))))

  (define-test "dissoc all keys returns empty"
    (let* ([h (hamt-assoc 'a 1 (hamt-assoc 'b 2 (hamt-assoc 'c 3 hamt-empty)))]
           [h (hamt-dissoc 'a h)]
           [h (hamt-dissoc 'b h)]
           [h (hamt-dissoc 'c h)])
      (assert-true (hamt-empty? h))))

  (define-test "insert after dissoc works"
    (let* ([h (hamt-assoc 'a 1 hamt-empty)]
           [h (hamt-dissoc 'a h)]
           [h (hamt-assoc 'a 99 h)])
      (assert-equal 99 (hamt-lookup 'a h)))))

;;; ============================================================
;;; First Key / First Entry
;;; ============================================================

(test-group "hamt-first-key"
  (define-test "first-key on empty returns #f"
    (assert-false (hamt-first-key hamt-empty)))

  (define-test "first-key on single-entry returns that key"
    (let ([h (hamt-assoc 'only 42 hamt-empty)])
      (assert-equal 'only (hamt-first-key h))))

  (define-test "first-key on multi-entry returns a valid key"
    (let ([h (hamt-assoc 'c 3 (hamt-assoc 'b 2 (hamt-assoc 'a 1 hamt-empty)))])
      (assert-true (hamt-has-key? (hamt-first-key h) h))))

  (define-test "first-entry on empty returns #f"
    (assert-false (hamt-first-entry hamt-empty)))

  (define-test "first-entry returns cons pair with valid key and value"
    (let* ([h (hamt-assoc 'x 10 (hamt-assoc 'y 20 hamt-empty))]
           [entry (hamt-first-entry h)])
      (assert-true (pair? entry))
      (assert-true (hamt-has-key? (car entry) h))
      (assert-equal (hamt-lookup (car entry) h) (cdr entry)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
