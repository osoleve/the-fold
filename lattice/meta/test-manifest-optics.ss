;;; lattice/meta/test-manifest-optics.ss — Tests for Manifest Optics
;;;
;;; Verifies that manifest optics correctly access and modify
;;; nested alist structures like those in manifest.sexp files.

(load "core/testing/test-framework.ss")
(load "lattice/meta/manifest-optics.ss")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define sample-manifest
  '((name . linalg)
    (version . "1.2.0")
    (tier . 0)
    (path . "lattice/linalg")
    (purity . total)
    (stability . stable)
    (fuel-bound . "O(n³)")
    (description . "Linear algebra primitives")
    (keywords . (matrix vector dot-product))
    (aliases . (la linear))
    (deps . ())
    (exports . ((vec vec-add vec-sub vec-dot)
                (matrix matrix-mul matrix-inv)))
    (modules . ((vec "vec.ss" "Vector operations")
                (matrix "matrix.ss" "Matrix operations")))))

(define minimal-manifest
  '((name . tiny)))

;;; ============================================================
;;; Part 1: Basic Alist Optics
;;; ============================================================

(test-group "alist-affine"
  (define-test "gets existing key"
    (assert-equal (just 'linalg)
                  (affine-preview (alist-affine 'name) sample-manifest)))

  (define-test "returns nothing for missing key"
    (assert-true (nothing? (affine-preview (alist-affine 'nonexistent) sample-manifest))))

  (define-test "sets existing key"
    (let ([updated (affine-set (alist-affine 'version) "2.0.0" sample-manifest)])
      (assert-equal (just "2.0.0") (affine-preview (alist-affine 'version) updated))))

  (define-test "inserts missing key"
    (let ([updated (affine-set (alist-affine 'new-field) 'value minimal-manifest)])
      (assert-equal (just 'value) (affine-preview (alist-affine 'new-field) updated)))))

(test-group "alist-affine-default"
  (define-test "gets existing key"
    (assert-equal (just "1.2.0")
                  (affine-preview (alist-affine-default 'version "0.0.0") sample-manifest)))

  (define-test "returns default for missing key"
    (assert-equal (just "0.0.0")
                  (affine-preview (alist-affine-default 'version "0.0.0") minimal-manifest)))

  (define-test "default works for various types"
    (assert-equal (just '())
                  (affine-preview (alist-affine-default 'deps '()) minimal-manifest))
    (assert-equal (just 0)
                  (affine-preview (alist-affine-default 'tier 0) minimal-manifest))))

(test-group "alist-lens"
  (define-test "gets required key"
    (assert-equal 'linalg (view (alist-lens 'name) sample-manifest)))

  (define-test "sets required key"
    (let ([updated (set-lens (alist-lens 'name) 'renamed sample-manifest)])
      (assert-equal 'renamed (view (alist-lens 'name) updated)))))

;;; ============================================================
;;; Part 2: Manifest Field Optics
;;; ============================================================

(test-group "manifest-field"
  (define-test "gets standard fields with defaults"
    (assert-equal (just "0.0.0") (affine-preview (manifest-field 'version) minimal-manifest))
    (assert-equal (just 0) (affine-preview (manifest-field 'tier) minimal-manifest))
    (assert-equal (just "") (affine-preview (manifest-field 'path) minimal-manifest))
    (assert-equal (just 'unknown) (affine-preview (manifest-field 'purity) minimal-manifest))
    (assert-equal (just '()) (affine-preview (manifest-field 'keywords) minimal-manifest)))

  (define-test "gets populated fields"
    (assert-equal (just "1.2.0") (affine-preview (manifest-field 'version) sample-manifest))
    (assert-equal (just 0) (affine-preview (manifest-field 'tier) sample-manifest))
    (assert-equal (just 'total) (affine-preview (manifest-field 'purity) sample-manifest)))

  (define-test "unknown fields use raw alist-affine"
    (assert-true (nothing? (affine-preview (manifest-field 'custom-field) sample-manifest)))))

(test-group "named field optics"
  (define-test "mf-version works"
    (assert-equal (just "1.2.0") (affine-preview mf-version sample-manifest)))

  (define-test "mf-tier works"
    (assert-equal (just 0) (affine-preview mf-tier sample-manifest)))

  (define-test "mf-description works"
    (assert-equal (just "Linear algebra primitives")
                  (affine-preview mf-description sample-manifest))))

;;; ============================================================
;;; Part 3: Convenience Functions
;;; ============================================================

(test-group "manifest-get"
  (define-test "gets existing field"
    (assert-equal "1.2.0" (manifest-get sample-manifest 'version)))

  (define-test "gets default for missing field"
    (assert-equal "0.0.0" (manifest-get minimal-manifest 'version)))

  (define-test "returns false for truly absent field"
    (assert-false (manifest-get minimal-manifest 'custom-field))))

(test-group "manifest-get*"
  (define-test "gets multiple fields"
    (assert-equal '("1.2.0" 0 total)
                  (manifest-get* sample-manifest 'version 'tier 'purity))))

(test-group "manifest-set"
  (define-test "sets a field"
    (let ([updated (manifest-set sample-manifest 'tier 1)])
      (assert-equal 1 (manifest-get updated 'tier))))

  (define-test "original unchanged"
    (manifest-set sample-manifest 'tier 1)
    (assert-equal 0 (manifest-get sample-manifest 'tier))))

(test-group "manifest-update"
  (define-test "updates with function"
    (let ([updated (manifest-update sample-manifest 'tier (lambda (t) (+ t 1)))])
      (assert-equal 1 (manifest-get updated 'tier)))))

;;; ============================================================
;;; Part 4: Collection Traversals
;;; ============================================================

(test-group "exports-each"
  (define-test "gets all export symbols"
    ;; Note: exports format is ((module-name sym1 sym2 ...) ...)
    ;; so we skip module names and get just the exported symbols
    (let ([exports (traversal-to-list exports-each sample-manifest)])
      (assert-true (pair? (memq 'vec-add exports)))
      (assert-true (pair? (memq 'matrix-mul exports)))
      ;; 3 from vec group + 2 from matrix group = 5
      (assert-equal 5 (length exports))))

  (define-test "empty for minimal manifest"
    (assert-equal '() (traversal-to-list exports-each minimal-manifest))))

(test-group "modules-each"
  (define-test "gets all module entries"
    (let ([modules (traversal-to-list modules-each sample-manifest)])
      (assert-equal 2 (length modules))
      (assert-equal 'vec (caar modules))))

  (define-test "empty for minimal manifest"
    (assert-equal '() (traversal-to-list modules-each minimal-manifest))))

(test-group "keywords-each"
  (define-test "gets all keywords"
    (let ([kws (traversal-to-list keywords-each sample-manifest)])
      (assert-equal 3 (length kws))
      (assert-true (pair? (memq 'matrix kws)))))

  (define-test "empty for minimal manifest"
    (assert-equal '() (traversal-to-list keywords-each minimal-manifest))))

(test-group "deps-each"
  (define-test "gets all deps"
    (assert-equal '() (traversal-to-list deps-each sample-manifest)))

  (define-test "handles deps list"
    (let ([manifest-with-deps '((name . foo) (deps . (bar baz)))])
      (assert-equal '(bar baz) (traversal-to-list deps-each manifest-with-deps)))))

;;; ============================================================
;;; Part 5: Module Entry Optics
;;; ============================================================

(define sample-module-entry '(vec "vec.ss" "Vector operations"))

(test-group "module-entry-lenses"
  (define-test "module-name-lens gets name"
    (assert-equal 'vec (view module-name-lens sample-module-entry)))

  (define-test "module-file-lens gets file"
    (assert-equal "vec.ss" (view module-file-lens sample-module-entry)))

  (define-test "module-desc-lens gets description"
    (assert-equal "Vector operations" (view module-desc-lens sample-module-entry)))

  (define-test "module-file-lens sets file"
    (let ([updated (set-lens module-file-lens "vector.ss" sample-module-entry)])
      (assert-equal "vector.ss" (view module-file-lens updated))
      ;; Other fields unchanged
      (assert-equal 'vec (view module-name-lens updated)))))

;;; ============================================================
;;; Part 6: Composition Tests
;;; ============================================================

(test-group "optic-composition"
  (define-test "compose affines for nested access"
    ;; Access first module's file through composition
    (let* ([first-module (car (traversal-to-list modules-each sample-manifest))]
           [file (view module-file-lens first-module)])
      (assert-equal "vec.ss" file)))

  (define-test "operator syntax works"
    ;; Using ^? for affine preview
    (assert-equal (just "1.2.0") (^? sample-manifest mf-version))
    (assert-equal (just 0) (^? sample-manifest mf-tier)))

  (define-test "set operator works"
    ;; Using .~ for setting
    (let ([updated (& sample-manifest (.~ mf-tier 2))])
      (assert-equal (just 2) (^? updated mf-tier))))

  (define-test "modify operator works"
    ;; Using %~ for modification
    (let ([updated (& sample-manifest (%~ mf-tier (lambda (t) (+ t 1))))])
      (assert-equal (just 1) (^? updated mf-tier)))))

;;; ============================================================
;;; Part 7: Real-World Pattern Tests
;;; ============================================================

(test-group "real-world-patterns"
  (define-test "replaces inspect.ss pattern"
    ;; Old pattern: (cdr (or (assq 'version data) '(version . "0.0.0")))
    ;; New pattern: (manifest-get data 'version)
    (assert-equal (cdr (or (assq 'version sample-manifest) '(version . "0.0.0")))
                  (manifest-get sample-manifest 'version)))

  (define-test "replaces multiple field access"
    ;; Old: multiple let bindings with assq
    ;; New: manifest-get*
    (let* ([old-way (list (cdr (or (assq 'version sample-manifest) '(version . "0.0.0")))
                          (cdr (or (assq 'tier sample-manifest) '(tier . 0))))]
           [new-way (manifest-get* sample-manifest 'version 'tier)])
      (assert-equal old-way new-way))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
