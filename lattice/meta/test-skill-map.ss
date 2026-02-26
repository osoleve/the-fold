(load "core/testing/test-framework.ss")
(load "lattice/meta/skill-map.ss")

;;; ====
;;; Test: build-skill-map
;;; ====

(test-group "build-skill-map"

  ;; Helper: minimal manifest for testing
  (define (make-test-manifest name path modules)
    `((name . ,name)
      (version . "1.0.0")
      (path . ,path)
      (purity . total)
      (stability . stable)
      (fuel-bound . "O(?)")
      (deps . ())
      (description . "test")
      (keywords . ())
      (aliases . ())
      (exports . ())
      (modules . ,modules)
      (concepts . ())))

  (define m-linalg
    (make-test-manifest 'linalg "lattice/linalg"
      '((vec "vec.ss" "vectors")
        (matrix "matrix.ss" "matrices")
        (sparse "sparse.ss" "sparse"))))

  (define m-data
    (make-test-manifest 'data "lattice/data"
      '((hamt "hamt.ss" "hash-array-mapped tries")
        (sort "sort.ss" "sorting"))))

  (define-test "maps modules to skills"
    (let ([smap (build-skill-map (list m-linalg m-data))])
      (assert-equal 'linalg (skill-map-lookup smap 'vec))
      (assert-equal 'linalg (skill-map-lookup smap 'matrix))
      (assert-equal 'data (skill-map-lookup smap 'hamt))))

  (define-test "returns #f for unknown modules"
    (let ([smap (build-skill-map (list m-linalg))])
      (assert-false (skill-map-lookup smap 'nonexistent))))

  (define-test "empty manifests produce empty map"
    (let ([smap (build-skill-map '())])
      (assert-equal '() smap)))

  (define-test "handles manifest with no modules"
    (let* ([m-empty (make-test-manifest 'empty "lattice/empty" '())]
           [smap (build-skill-map (list m-empty))])
      (assert-equal '() smap)))

  (define-test "all modules from all skills present"
    (let ([smap (build-skill-map (list m-linalg m-data))])
      ;; 3 from linalg + 2 from data = 5
      (assert-equal 5 (length smap))))
)

;;; ====
;;; Test: path-based-skill
;;; ====

(test-group "path-based-skill"

  (define (make-test-manifest name path modules)
    `((name . ,name)
      (version . "1.0.0")
      (path . ,path)
      (purity . total)
      (stability . stable)
      (fuel-bound . "O(?)")
      (deps . ())
      (description . "test")
      (keywords . ())
      (aliases . ())
      (exports . ())
      (modules . ,modules)
      (concepts . ())))

  (define manifests
    (list (make-test-manifest 'linalg "lattice/linalg" '())
          (make-test-manifest 'data "lattice/data" '())
          (make-test-manifest 'optics "lattice/optics" '())))

  (define-test "matches file to skill by path"
    (assert-equal 'linalg
      (path-based-skill "lattice/linalg/vec.ss" manifests)))

  (define-test "matches nested paths"
    (assert-equal 'data
      (path-based-skill "lattice/data/graph/community.ss" manifests)))

  (define-test "returns #f for unmatched path"
    (assert-false
      (path-based-skill "lattice/unknown/foo.ss" manifests)))

  (define-test "returns #f for empty manifests"
    (assert-false
      (path-based-skill "lattice/linalg/vec.ss" '())))
)

;;; ====
;;; Run
;;; ====

(run-all-tests)
