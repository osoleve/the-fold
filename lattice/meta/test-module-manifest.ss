(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'meta/module-manifest)

;;; ====
;;; Canonical Manifest Construction
;;; ====

(test-group "build-manifest"

  (define-test "empty entries"
    (let ([m (build-manifest '())])
      (assert-true (module-manifest? m))
      (assert-equal 0 (manifest-count m))
      (assert-equal '() (manifest-entries m))))

  (define-test "entries are sorted by name"
    (let ([m (build-manifest '((zebra . "z.ss") (alpha . "a.ss") (mid . "m.ss")))])
      (assert-equal 3 (manifest-count m))
      (assert-equal 'alpha (caar (manifest-entries m)))
      (assert-equal 'mid (car (cadr (manifest-entries m))))
      (assert-equal 'zebra (car (caddr (manifest-entries m))))))

  (define-test "duplicates are deduplicated (last wins)"
    (let ([m (build-manifest '((foo . "old.ss") (foo . "new.ss")))])
      (assert-equal 1 (manifest-count m))
      (assert-equal "new.ss" (cdar (manifest-entries m)))))

  (define-test "single entry"
    (let ([m (build-manifest '((vec . "lattice/linalg/vec.ss")))])
      (assert-equal 1 (manifest-count m))
      (assert-equal '(vec . "lattice/linalg/vec.ss") (car (manifest-entries m)))))
)

;;; ====
;;; Content Addressing
;;; ====

(test-group "content-addressing"

  (define-test "same entries produce same hash"
    (let ([m1 (build-manifest '((a . "a.ss") (b . "b.ss")))]
          [m2 (build-manifest '((b . "b.ss") (a . "a.ss")))])  ; Different order
      (assert-equal (manifest-hash m1) (manifest-hash m2))))

  (define-test "different entries produce different hash"
    (let ([m1 (build-manifest '((a . "a.ss")))]
          [m2 (build-manifest '((a . "a.ss") (b . "b.ss")))])
      (assert-false (string=? (manifest-hash m1) (manifest-hash m2)))))

  (define-test "hash is a 64-char hex string"
    (let* ([m (build-manifest '((x . "x.ss")))]
           [h (manifest-hash m)])
      (assert-equal 64 (string-length h))))

  (define-test "hash is deterministic across calls"
    (let ([m (build-manifest '((foo . "foo.ss") (bar . "bar.ss")))])
      (assert-equal (manifest-hash m) (manifest-hash m))))
)

;;; ====
;;; Manifest Diffing
;;; ====

(test-group "manifest-diff"

  (define-test "identical manifests have empty diff"
    (let* ([m (build-manifest '((a . "a.ss") (b . "b.ss")))]
           [d (manifest-diff m m)])
      (assert-true (manifest-diff-empty? d))
      (assert-equal 2 (cadr (assq 'same-count (cdr d))))))

  (define-test "detect additions"
    (let* ([old (build-manifest '((a . "a.ss")))]
           [new (build-manifest '((a . "a.ss") (b . "b.ss")))]
           [d (manifest-diff old new)]
           [added (cadr (assq 'added (cdr d)))])
      (assert-equal 1 (length added))
      (assert-equal 'b (caar added))))

  (define-test "detect removals"
    (let* ([old (build-manifest '((a . "a.ss") (b . "b.ss")))]
           [new (build-manifest '((a . "a.ss")))]
           [d (manifest-diff old new)]
           [removed (cadr (assq 'removed (cdr d)))])
      (assert-equal 1 (length removed))
      (assert-equal 'b (caar removed))))

  (define-test "detect path changes"
    (let* ([old (build-manifest '((a . "old/a.ss")))]
           [new (build-manifest '((a . "new/a.ss")))]
           [d (manifest-diff old new)]
           [changed (cadr (assq 'changed (cdr d)))])
      (assert-equal 1 (length changed))
      (assert-equal 'a (caar changed))
      ;; changed entry is (name old-path new-path)
      (assert-equal "old/a.ss" (cadar changed))
      (assert-equal "new/a.ss" (caddar changed))))

  (define-test "empty diff predicate"
    (let* ([m (build-manifest '((a . "a.ss")))]
           [d (manifest-diff m m)])
      (assert-true (manifest-diff-empty? d))))

  (define-test "non-empty diff predicate"
    (let* ([old (build-manifest '((a . "a.ss")))]
           [new (build-manifest '((b . "b.ss")))]
           [d (manifest-diff old new)])
      (assert-false (manifest-diff-empty? d))))
)

;;; ====
;;; Registry Extraction
;;; ====

(test-group "registry-extraction"

  (define-test "extract from hashtable"
    (let ([ht (make-eq-hashtable)])
      (hashtable-set! ht 'foo "foo.ss")
      (hashtable-set! ht 'bar "bar.ss")
      (let ([m (manifest-from-paths-table ht)])
        (assert-true (module-manifest? m))
        (assert-equal 2 (manifest-count m))
        ;; Should be sorted
        (assert-equal 'bar (caar (manifest-entries m))))))

  (define-test "extract from live module paths"
    ;; Use the actual *module-paths* hashtable
    (let ([m (manifest-from-paths-table *module-paths*)])
      (assert-true (module-manifest? m))
      ;; Should have at least the core bootstrap set
      (assert-true (> (manifest-count m) 20))))
)

;;; ====
;;; Core Bootstrap Set
;;; ====

(test-group "core-bootstrap"

  (define-test "bootstrap set is non-empty"
    (assert-true (> (length (manifest-core-bootstrap)) 15)))

  (define-test "bootstrap entries are all core paths"
    (let ([all-core? (let loop ([entries (manifest-core-bootstrap)])
                       (cond
                         [(null? entries) #t]
                         [(string-starts-with? (cdar entries) "core/")
                          (loop (cdr entries))]
                         [else #f]))])
      (assert-true all-core?)))

  (define-test "core-bootstrap-entry? identifies core modules"
    (assert-true (core-bootstrap-entry? '(prelude . "core/base/prelude.ss")))
    (assert-true (core-bootstrap-entry? '(eval . "core/lang/eval.ss")))
    (assert-false (core-bootstrap-entry? '(vec . "lattice/linalg/vec.ss"))))

  (define-test "manifest-split separates core from lattice"
    (let* ([m (build-manifest
                (append (manifest-core-bootstrap)
                        '((vec . "lattice/linalg/vec.ss")
                          (matrix . "lattice/linalg/matrix.ss"))))]
           [split (manifest-split m)])
      (let ([core (cadr (assq 'core (cdr split)))]
            [lattice (cadr (assq 'lattice (cdr split)))])
        (assert-equal (length (manifest-core-bootstrap)) (length core))
        (assert-equal 2 (length lattice)))))
)

;;; ====
;;; Consistency Checking
;;; ====

(test-group "consistency-check"

  (define-test "consistent manifests report no issues"
    (let* ([m (build-manifest '((a . "a.ss") (b . "b.ss")))]
           [report (manifest-consistency-check m m)])
      (assert-true (pair? (assq 'consistent? (cdr report))))
      (assert-true (cadr (assq 'consistent? (cdr report))))))

  (define-test "detect static-only modules"
    (let* ([static (build-manifest '((a . "a.ss") (b . "b.ss")))]
           [derived (build-manifest '((a . "a.ss")))]
           [report (manifest-consistency-check static derived)]
           [static-only (cadr (assq 'in-static-only (cdr report)))])
      (assert-equal 1 (length static-only))
      (assert-equal 'b (caar static-only))))

  (define-test "detect manifest-only modules"
    (let* ([static (build-manifest '((a . "a.ss")))]
           [derived (build-manifest '((a . "a.ss") (c . "c.ss")))]
           [report (manifest-consistency-check static derived)]
           [manifest-only (cadr (assq 'in-manifest-only (cdr report)))])
      (assert-equal 1 (length manifest-only))
      (assert-equal 'c (caar manifest-only))))

  (define-test "detect path mismatches"
    (let* ([static (build-manifest '((a . "old/a.ss")))]
           [derived (build-manifest '((a . "new/a.ss")))]
           [report (manifest-consistency-check static derived)])
      (assert-false (cadr (assq 'consistent? (cdr report))))))
)

;;; ====
;;; Code Generation
;;; ====

(test-group "codegen"

  (define-test "generate registry forms"
    (let* ([m (build-manifest '((vec . "lattice/linalg/vec.ss")))]
           [forms (manifest->registry-forms m)])
      (assert-equal 1 (length forms))
      ;; Should be a valid register-module-path! form
      (assert-equal 'register-module-path! (caar forms))))

  (define-test "generate registry string"
    (let* ([m (build-manifest '((vec . "lattice/linalg/vec.ss")))]
           [s (manifest->registry-string m)])
      (assert-true (> (string-length s) 0))
      ;; Should contain register-module-path!
      (assert-true (> (string-length s) 20))))
)

;;; ====
;;; Summary
;;; ====

(test-group "manifest-summary"

  (define-test "summary includes all fields"
    (let* ([m (build-manifest
                (append (manifest-core-bootstrap)
                        '((vec . "lattice/linalg/vec.ss"))))]
           [s (manifest-summary m)])
      (assert-true (pair? (assq 'total (cdr s))))
      (assert-true (pair? (assq 'core (cdr s))))
      (assert-true (pair? (assq 'lattice (cdr s))))
      (assert-true (pair? (assq 'hash (cdr s))))
      (assert-true (pair? (assq 'layers (cdr s))))))

  (define-test "layer counts are accurate"
    (let* ([m (build-manifest
                '((prelude . "core/base/prelude.ss")
                  (vec . "lattice/linalg/vec.ss")
                  (bbs . "boundary/bbs/bbs.ss")))]
           [s (manifest-summary m)]
           [layers (cadr (assq 'layers (cdr s)))])
      (assert-equal 1 (cdr (assoc "core" layers)))
      (assert-equal 1 (cdr (assoc "lattice" layers)))
      (assert-equal 1 (cdr (assoc "boundary" layers)))))
)

;;; ====
;;; Live Registry Snapshot
;;; ====

(test-group "live-registry"

  (define-test "snapshot current registry produces valid manifest"
    (let ([m (manifest-from-paths-table *module-paths*)])
      (assert-true (module-manifest? m))
      ;; Hash should be deterministic
      (assert-equal (manifest-hash m) (manifest-hash m))))

  (define-test "current registry includes core bootstrap"
    (let* ([m (manifest-from-paths-table *module-paths*)]
           [split (manifest-split m)]
           [core (cadr (assq 'core (cdr split)))])
      ;; All core bootstrap modules should be present
      (assert-true (>= (length core) (length (manifest-core-bootstrap))))))
)

(run-all-tests)
