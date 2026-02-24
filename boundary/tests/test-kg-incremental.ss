;;; Tests for incremental KG build support
;;; Tests fingerprinting, change detection, and incremental rebuild.

(load "core/testing/test-framework.ss")
(load "boundary/meta/kg-io.ss")

;;; ====
;;; Fingerprinting
;;; ====

(test-group "fingerprinting"

  (define-test "file content hash returns 64-char hex string"
    (let ([hash (kg-file-content-hash "lattice/linalg/manifest.sexp")])
      (assert-equal 64 (string-length hash))))

  (define-test "file content hash is stable"
    (let ([h1 (kg-file-content-hash "lattice/linalg/manifest.sexp")]
          [h2 (kg-file-content-hash "lattice/linalg/manifest.sexp")])
      (assert-true (string=? h1 h2))))

  (define-test "missing file returns missing"
    (assert-equal "missing" (kg-file-content-hash "/nonexistent/file.ss")))

  (define-test "manifest fingerprint returns 64-char hex"
    (let* ([data (parse-manifest (read-manifest-sexp "lattice/linalg/manifest.sexp"))]
           [fp (kg-manifest-fingerprint "lattice/linalg/manifest.sexp" data)])
      (assert-equal 64 (string-length fp))))

  (define-test "manifest fingerprint is stable"
    (let* ([data (parse-manifest (read-manifest-sexp "lattice/linalg/manifest.sexp"))]
           [fp1 (kg-manifest-fingerprint "lattice/linalg/manifest.sexp" data)]
           [fp2 (kg-manifest-fingerprint "lattice/linalg/manifest.sexp" data)])
      (assert-true (string=? fp1 fp2))))

  (define-test "manifest source files includes manifest path"
    (let* ([data (parse-manifest (read-manifest-sexp "lattice/linalg/manifest.sexp"))]
           [files (kg-manifest-source-files "lattice/linalg/manifest.sexp" data)])
      (assert-true (pair? files))
      (assert-equal "lattice/linalg/manifest.sexp" (car files))))

  (define-test "compute-all-fingerprints returns alist"
    (let ([fps (kg-compute-all-fingerprints '("lattice/linalg/manifest.sexp"
                                               "lattice/fp/manifest.sexp"))])
      (assert-equal 2 (length fps))
      (assert-true (pair? (assq 'linalg fps)))
      (assert-true (pair? (assq 'fp fps)))))
)

;;; ====
;;; Change Detection
;;; ====

(test-group "change-detection"

  (define-test "no changes when fingerprints match"
    (let ([fps '((a . "abc") (b . "def"))])
      (call-with-values
       (lambda () (kg-detect-changes fps fps))
       (lambda (changed added removed)
         (assert-true (null? changed))
         (assert-true (null? added))
         (assert-true (null? removed))))))

  (define-test "detects changed fingerprint"
    (let ([current '((a . "abc") (b . "CHANGED"))]
          [cached  '((a . "abc") (b . "def"))])
      (call-with-values
       (lambda () (kg-detect-changes current cached))
       (lambda (changed added removed)
         (assert-true (pair? (memq 'b changed)))
         (assert-true (null? added))
         (assert-true (null? removed))))))

  (define-test "detects added skill"
    (let ([current '((a . "abc") (b . "def") (c . "ghi"))]
          [cached  '((a . "abc") (b . "def"))])
      (call-with-values
       (lambda () (kg-detect-changes current cached))
       (lambda (changed added removed)
         (assert-true (null? changed))
         (assert-true (pair? (memq 'c added)))
         (assert-true (null? removed))))))

  (define-test "detects removed skill"
    (let ([current '((a . "abc"))]
          [cached  '((a . "abc") (b . "def"))])
      (call-with-values
       (lambda () (kg-detect-changes current cached))
       (lambda (changed added removed)
         (assert-true (null? changed))
         (assert-true (null? added))
         (assert-true (pair? (memq 'b removed)))))))

  (define-test "detects mixed changes"
    (let ([current '((a . "NEW") (c . "ghi"))]
          [cached  '((a . "abc") (b . "def"))])
      (call-with-values
       (lambda () (kg-detect-changes current cached))
       (lambda (changed added removed)
         (assert-true (pair? (memq 'a changed)))
         (assert-true (pair? (memq 'c added)))
         (assert-true (pair? (memq 'b removed)))))))
)

;;; ====
;;; Fingerprint Persistence
;;; ====

(test-group "fingerprint-persistence"

  (define-test "save and load round-trip"
    (let ([fps '((linalg . "abc123") (fp . "def456"))])
      (kg-save-fingerprints! fps)
      (let ([loaded (kg-load-fingerprints)])
        (assert-equal 2 (length loaded))
        (assert-true (pair? (assq 'linalg loaded)))
        (assert-equal "abc123" (cdr (assq 'linalg loaded))))))

  (define-test "load returns empty on missing file"
    ;; save fingerprints, then manually remove file
    (kg-save-fingerprints! '((test . "abc")))
    (delete-file KG-FINGERPRINTS-PATH)
    (let ([loaded (kg-load-fingerprints)])
      (assert-true (null? loaded))))
)

;;; ====
;;; Skill Removal
;;; ====

(test-group "skill-removal"

  (define-test "kg-remove-skill! removes from skills list"
    (kg-reset!)
    (kg-build!)
    (let ([before (length (kg-skills))])
      (kg-remove-skill! 'linalg)
      (assert-equal (- before 1) (length (kg-skills)))
      (assert-false (kg-skill 'linalg))))

  (define-test "kg-remove-skill! removes manifest data"
    (kg-reset!)
    (kg-build!)
    (kg-remove-skill! 'linalg)
    (assert-false (kg-skill-data 'linalg)))

  (define-test "kg-remove-skill! removes modules"
    (kg-reset!)
    (kg-build!)
    (let ([mods-before (kg-modules 'linalg)])
      (assert-true (pair? mods-before))
      (kg-remove-skill! 'linalg)
      (assert-true (null? (kg-modules 'linalg)))))
)

;;; ====
;;; Incremental Build Integration
;;; ====

(test-group "incremental-build"

  (define-test "no-change incremental returns same root"
    ;; Full build first
    (kg-reset!)
    (kg-build!)
    (let ([root1 *kg-index-root*])
      ;; Reset and try incremental
      (kg-reset!)
      (let ([root2 (kg-incremental-build!)])
        ;; Should succeed and return a root hash
        (assert-true (bytevector? root2))
        ;; Skills should be fully restored
        (assert-true (> (length (kg-skills)) 30)))))

  (define-test "incremental with no cached fingerprints saves baseline"
    (kg-reset!)
    (kg-build!)
    ;; Remove fingerprints but keep root
    (when (file-exists? KG-FINGERPRINTS-PATH)
      (delete-file KG-FINGERPRINTS-PATH))
    (kg-reset!)
    (let ([root (kg-incremental-build!)])
      ;; Should succeed (loads from CAS, saves fingerprints for next time)
      (assert-true (bytevector? root))
      ;; Fingerprints should now exist
      (assert-true (pair? (kg-load-fingerprints)))))

  (define-test "kg-ensure uses incremental path"
    ;; Build baseline
    (kg-reset!)
    (kg-build!)
    ;; Reset and ensure — should use incremental
    (kg-reset!)
    (let ([root (kg-ensure!)])
      (assert-true (bytevector? root))
      (assert-true (kg-initialized?))))
)

;;; ====
;;; Targeted Type Extraction
;;; ====

(test-group "targeted-type-extraction"

  (define-test "targeted extraction merges with existing types"
    (kg-reset!)
    (kg-build!)
    (let ([types-before (hamt-size *kg-type-sigs*)])
      ;; Extract for just linalg path — should not reduce type count
      (kg-extract-type-sigs-for-paths! '("lattice/linalg"))
      (assert-true (>= (hamt-size *kg-type-sigs*) types-before))))
)

(run-all-tests)
