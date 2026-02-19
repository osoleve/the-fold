;;; dict.sexp — Development tasks for lattice/data/dict.ss
;;;
;;; Module: dict (tier 0, depends only on prelude)
;;; 116 lines, ~14 exported functions
;;; Simple alist-based immutable dictionary.
;;;
;;; No test file exists — one of the tasks is to write tests.
;;;
;;; Git history:
;;;   f00e3d4d refactor(data): Split data-structures.ss into semantic modules
;;;   89839988 docs(lattice): Convert data files to doc forms
;;;   596c1d4d feat(module): migrate data/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement dict-dissoc (key removal)
  ;; ──────────────────────────────────────────────
  ;; The core deletion operation for an alist-backed dictionary.
  ;; Must handle: key not present (return unchanged), key present
  ;; (remove first matching pair), preserve order of remaining entries.

  (task
    (id "data/dict/dissoc-impl")
    (module dict)
    (tier 0)
    (type implement)
    (description "Implement dict-dissoc: remove a key from an alist-based dictionary. Walk the list, collecting non-matching pairs into an accumulator. When the matching key is found, skip it and append the rest. If the key isn't found, return the dictionary unchanged. Use equal? for key comparison. Preserve the order of remaining entries.")
    (context "Available: prelude (car, cdr, cons, null?, equal?, reverse, append). A dictionary is an association list: ((key1 . val1) (key2 . val2) ...). Each entry is a pair.")
    (before "(define (dict-dissoc key dict)
  (doc 'type (-> k Dict Dict))
  (doc 'description \"Remove key from dictionary\")
  ;; TODO: implement — walk the alist, skip the matching pair
  dict)")
    (test-file #f)
    (test-forms ("(assert-equal '((b . 2)) (dict-dissoc 'a '((a . 1) (b . 2))))"
                 "(assert-equal '((a . 1) (b . 2)) (dict-dissoc 'c '((a . 1) (b . 2))))"
                 "(assert-equal '() (dict-dissoc 'a '((a . 1))))"))
    (available-modules (prelude))
    (hints ("Use a tail-recursive loop with an accumulator for entries seen so far"
            "When you find the matching key: (append (reverse acc) (cdr remaining))"
            "When you reach the end without finding the key: (reverse acc)"))
    (reference-solution "(define (dict-dissoc key dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? key (car (car remaining)))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))")
    (alternatives ())
    (source-commit "f00e3d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement dict-merge
  ;; ──────────────────────────────────────────────
  ;; Merging two dictionaries by folding dict-assoc. The key insight
  ;; is that dict-assoc handles deduplication (via dissoc-then-cons),
  ;; so we just need to iterate over dict2's entries.

  (task
    (id "data/dict/merge-impl")
    (module dict)
    (tier 0)
    (type implement)
    (description "Implement dict-merge: merge two dictionaries. When keys overlap, dict2's value wins (dict2 takes precedence). Use dict-assoc to add each entry from dict2 into dict1. dict-assoc already handles updating existing keys.")
    (context "Available: prelude, dict-assoc (already defined: adds or updates a key-value pair in a dictionary).")
    (before "(define (dict-merge dict1 dict2)
  (doc 'type (-> Dict Dict Dict))
  (doc 'description \"Merge two dictionaries\")
  (doc 'note \"If keys overlap, dict2 wins\")
  ;; TODO: fold over dict2's entries, adding each to dict1
  dict1)")
    (test-file #f)
    (test-forms ("(assert-equal '((b . 20) (a . 1)) (dict-merge '((a . 1)) '((b . 20))))"
                 "(assert-equal 20 (dict-lookup 'a (dict-merge '((a . 1)) '((a . 20)))))"
                 "(assert-equal 1 (dict-lookup 'a (dict-merge '((a . 1)) '((b . 2)))))"))
    (available-modules (prelude))
    (hints ("Loop over remaining entries in dict2"
            "For each entry, use (dict-assoc key value result)"
            "The result accumulates — start with dict1, fold in dict2"))
    (reference-solution "(define (dict-merge dict1 dict2)
  (let loop ([remaining dict2]
             [result dict1])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (dict-assoc (car (car remaining))
                             (cdr (car remaining))
                             result)))))")
    (alternatives ())
    (source-commit "f00e3d4d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement dict-filter
  ;; ──────────────────────────────────────────────
  ;; Higher-order operation: filter entries by a predicate that
  ;; receives both key and value.

  (task
    (id "data/dict/filter-impl")
    (module dict)
    (tier 0)
    (type implement)
    (description "Implement dict-filter: keep only entries where (pred key value) returns true. The predicate takes two arguments: the key and the value. Walk the dictionary, accumulate matching entries, preserve order.")
    (context "Available: prelude. A dictionary entry is a pair (key . value). The predicate takes the key and value as separate arguments.")
    (before "(define (dict-filter pred dict)
  (doc 'type (-> (-> k v Boolean) Dict Dict))
  (doc 'description \"Filter dictionary by predicate on key-value pairs\")
  ;; TODO: implement
  dict)")
    (test-file #f)
    (test-forms ("(assert-equal '((b . 2)) (dict-filter (lambda (k v) (> v 1)) '((a . 1) (b . 2) (c . 0))))"
                 "(assert-equal '() (dict-filter (lambda (k v) #f) '((a . 1))))"
                 "(assert-equal '((a . 1)) (dict-filter (lambda (k v) (eq? k 'a)) '((a . 1) (b . 2))))"))
    (available-modules (prelude))
    (hints ("Use a tail-recursive loop with an accumulator"
            "Test each entry: (pred (car entry) (cdr entry))"
            "Reverse the accumulator at the end to preserve order"))
    (reference-solution "(define (dict-filter pred dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(pred (car (car remaining)) (cdr (car remaining)))
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))")
    (alternatives ("(filter (lambda (entry) (pred (car entry) (cdr entry))) dict)"))
    (source-commit "f00e3d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Write tests for dict module
  ;; ──────────────────────────────────────────────
  ;; dict.ss has zero tests. This task exercises the ability to
  ;; understand an implementation and write verification for it.
  ;; The model must identify edge cases and the dict-lookup
  ;; ambiguity (returns #f for missing keys AND for keys mapped to #f).

  (task
    (id "data/dict/write-tests")
    (module dict)
    (tier 0)
    (type test)
    (description "Write a comprehensive test suite for the dict module. The module implements an alist-based immutable dictionary with: dict-empty, dict-empty?, dict-lookup, dict-has-key?, dict-assoc, dict-dissoc, dict-keys, dict-values, dict-merge, dict-map-values, dict-filter, dict-size. Cover: (1) empty dictionary behavior, (2) single-entry CRUD, (3) multi-entry operations, (4) key update semantics (dict-assoc on existing key), (5) merge precedence, (6) edge case: dict-lookup returns #f for missing keys — note this is ambiguous if #f is a valid value.")
    (context "Available: the full dict module (loaded via require). Use assert-equal and assert-true from test-framework.ss.")
    (before ";; lattice/data/test-dict.ss
;; No tests exist yet for the dict module.
;; Write comprehensive tests covering the API.")
    (test-file #f)
    (test-forms ())
    (available-modules (prelude dict))
    (hints ("Test dict-empty? on both empty and non-empty dicts"
            "Test that dict-assoc on an existing key updates the value"
            "Test dict-dissoc on a key that doesn't exist (should return unchanged)"
            "Note: dict-lookup returns #f for missing keys, which is indistinguishable from a key mapped to #f — this is a known design limitation"))
    (reference-solution ";; Core operations
(assert-true (dict-empty? dict-empty))
(assert-false (dict-empty? (dict-assoc 'a 1 dict-empty)))
(assert-equal 1 (dict-lookup 'a (dict-assoc 'a 1 dict-empty)))
(assert-false (dict-lookup 'b (dict-assoc 'a 1 dict-empty)))
(assert-true (dict-has-key? 'a (dict-assoc 'a 1 dict-empty)))
(assert-false (dict-has-key? 'b (dict-assoc 'a 1 dict-empty)))

;; Update semantics
(let ([d (dict-assoc 'a 2 (dict-assoc 'a 1 dict-empty))])
  (assert-equal 2 (dict-lookup 'a d))
  (assert-equal 1 (dict-size d)))

;; Removal
(let ([d (dict-dissoc 'a (dict-assoc 'a 1 (dict-assoc 'b 2 dict-empty)))])
  (assert-false (dict-has-key? 'a d))
  (assert-equal 2 (dict-lookup 'b d)))

;; Merge precedence
(let ([d (dict-merge (dict-assoc 'a 1 dict-empty)
                     (dict-assoc 'a 99 dict-empty))])
  (assert-equal 99 (dict-lookup 'a d)))

;; Keys and values
(let ([d (dict-assoc 'b 2 (dict-assoc 'a 1 dict-empty))])
  (assert-equal 2 (dict-size d)))")
    (alternatives ())
    (source-commit #f)
    (difficulty 2))

) ;; end tasks
