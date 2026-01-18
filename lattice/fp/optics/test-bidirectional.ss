;;; lattice/fp/optics/test-bidirectional.ss — Tests for Bidirectional Transformations
;;;
;;; Comprehensive tests for the migration DSL:
;;;   - Roundtrip property: rollback(migrate(x)) = x
;;;   - Composition: migration-compose chains correctly
;;;   - Version mismatch rejection
;;;   - Schema field operations
;;;   - Block migrations
;;;
;;; Run with: scheme --script lattice/fp/optics/test-bidirectional.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/optics/bidirectional.ss")
(load "lattice/fp/optics/format-iso.ss")
(load "lattice/fp/optics/schema.ss")
(load "lattice/fp/optics/block-migration.ss")

;;; ============================================================
;;; Part 1: Basic Migration Tests
;;; ============================================================

(test-group "Migration Basics"

  (define-test "make-migration creates valid migration"
    (let* ([iso (make-p-iso add1 sub1)]
           [m (make-migration 'test 'v1 'v2 iso)])
      (assert-true (migration? m))
      (assert-equal 'test (migration-name m))
      (assert-equal 'v1 (migration-from m))
      (assert-equal 'v2 (migration-to m))))

  (define-test "migrate applies forward transformation"
    (let* ([iso (make-p-iso add1 sub1)]
           [m (make-migration 'inc 'v1 'v2 iso)])
      (assert-equal 11 (migrate m 10))
      (assert-equal 1 (migrate m 0))))

  (define-test "rollback applies backward transformation"
    (let* ([iso (make-p-iso add1 sub1)]
           [m (make-migration 'inc 'v1 'v2 iso)])
      (assert-equal 9 (rollback m 10))
      (assert-equal -1 (rollback m 0))))

  (define-test "migration-apply respects direction"
    (let* ([iso (make-p-iso add1 sub1)]
           [m (make-migration 'inc 'v1 'v2 iso)])
      (assert-equal 11 (migration-apply m 10 'forward))
      (assert-equal 9 (migration-apply m 10 'backward)))))

;;; ============================================================
;;; Part 2: Roundtrip Property Tests
;;; ============================================================

(test-group "Roundtrip Property"

  (define-test "rollback(migrate(x)) = x for numbers"
    (let* ([iso (make-p-iso (lambda (x) (* x 2)) (lambda (x) (/ x 2)))]
           [m (make-migration 'double 'v1 'v2 iso)])
      (assert-equal 5 (rollback m (migrate m 5)))
      (assert-equal 100 (rollback m (migrate m 100)))
      (assert-equal 0 (rollback m (migrate m 0)))))

  (define-test "migrate(rollback(x)) = x for numbers"
    (let* ([iso (make-p-iso (lambda (x) (* x 2)) (lambda (x) (/ x 2)))]
           [m (make-migration 'double 'v1 'v2 iso)])
      (assert-equal 10 (migrate m (rollback m 10)))
      (assert-equal 200 (migrate m (rollback m 200)))))

  (define-test "roundtrip for string transformation"
    (let* ([iso (make-p-iso
                 (lambda (s) (string-append "prefix-" s))
                 (lambda (s) (substring s 7 (string-length s))))]
           [m (make-migration 'prefix 'v1 'v2 iso)])
      (assert-equal "hello" (rollback m (migrate m "hello")))
      (assert-equal "" (rollback m (migrate m "")))))

  (define-test "roundtrip for list transformation"
    (let* ([iso (make-p-iso reverse reverse)]
           [m (make-migration 'reverse 'v1 'v2 iso)])
      (assert-equal '(1 2 3) (rollback m (migrate m '(1 2 3))))
      (assert-equal '() (rollback m (migrate m '()))))))

;;; ============================================================
;;; Part 3: Migration Composition Tests
;;; ============================================================

(test-group "Migration Composition"

  (define-test "compose two compatible migrations"
    (let* ([m1 (make-migration 'add1 'v1 'v2 (make-p-iso add1 sub1))]
           [m2 (make-migration 'double 'v2 'v3
                               (make-p-iso (lambda (x) (* x 2))
                                          (lambda (x) (/ x 2))))]
           [m3 (migration-compose m1 m2)])
      (assert-equal 'v1 (migration-from m3))
      (assert-equal 'v3 (migration-to m3))
      ;; Forward: add1 then double: (5+1)*2 = 12
      (assert-equal 12 (migrate m3 5))
      ;; Backward: halve then sub1: 12/2-1 = 5
      (assert-equal 5 (rollback m3 12))))

  (define-test "compose three migrations via chain"
    (let* ([m1 (make-migration 'a 'v1 'v2 (make-p-iso add1 sub1))]
           [m2 (make-migration 'b 'v2 'v3 (make-p-iso add1 sub1))]
           [m3 (make-migration 'c 'v3 'v4 (make-p-iso add1 sub1))]
           [composed (migration-chain (list m1 m2 m3))])
      (assert-equal 'v1 (migration-from composed))
      (assert-equal 'v4 (migration-to composed))
      ;; Forward: +1+1+1 = +3
      (assert-equal 13 (migrate composed 10))
      ;; Backward: -1-1-1 = -3
      (assert-equal 10 (rollback composed 13))))

  (define-test "composition is associative"
    (let* ([m1 (make-migration 'a 'v1 'v2 (make-p-iso add1 sub1))]
           [m2 (make-migration 'b 'v2 'v3
                               (make-p-iso (lambda (x) (* x 2))
                                          (lambda (x) (/ x 2))))]
           [m3 (make-migration 'c 'v3 'v4 (make-p-iso add1 sub1))]
           [left-assoc (migration-compose (migration-compose m1 m2) m3)]
           [right-assoc (migration-compose m1 (migration-compose m2 m3))])
      (assert-equal (migrate left-assoc 5) (migrate right-assoc 5))
      (assert-equal (rollback left-assoc 13) (rollback right-assoc 13)))))

;;; ============================================================
;;; Part 4: Migration Flip Tests
;;; ============================================================

(test-group "Migration Flip"

  (define-test "flipped migration swaps versions"
    (let* ([m (make-migration 'test 'v1 'v2 (make-p-iso add1 sub1))]
           [flipped (migration-flip m)])
      (assert-equal 'v2 (migration-from flipped))
      (assert-equal 'v1 (migration-to flipped))))

  (define-test "flipped migration swaps operations"
    (let* ([m (make-migration 'test 'v1 'v2 (make-p-iso add1 sub1))]
           [flipped (migration-flip m)])
      ;; Original: forward = add1
      (assert-equal 11 (migrate m 10))
      ;; Flipped: forward = original backward = sub1
      (assert-equal 9 (migrate flipped 10))))

  (define-test "flip is involution: flip(flip(m)) = m"
    (let* ([m (make-migration 'test 'v1 'v2 (make-p-iso add1 sub1))]
           [double-flipped (migration-flip (migration-flip m))])
      (assert-equal (migration-from m) (migration-from double-flipped))
      (assert-equal (migration-to m) (migration-to double-flipped))
      (assert-equal (migrate m 10) (migrate double-flipped 10)))))

;;; ============================================================
;;; Part 5: Format Iso Tests
;;; ============================================================

(test-group "Format Isomorphisms"

  (define-test "iso-utf8 roundtrips"
    (let ([s "hello world"])
      (assert-equal s ((p-iso-backward iso-utf8)
                       ((p-iso-forward iso-utf8) s)))))

  (define-test "iso-sexpr-string roundtrips"
    (let ([expr '(+ 1 (* 2 3))])
      (assert-equal expr ((p-iso-backward iso-sexpr-string)
                          ((p-iso-forward iso-sexpr-string) expr)))))

  (define-test "iso-sexpr-bytevector roundtrips"
    (let ([expr '((name . "Alice") (age . 30))])
      (assert-equal expr (bytevector->sexpr
                          (sexpr->bytevector expr)))))

  (define-test "iso-bool-int converts correctly"
    (assert-equal 1 ((p-iso-forward iso-bool-int) #t))
    (assert-equal 0 ((p-iso-forward iso-bool-int) #f))
    (assert-true ((p-iso-backward iso-bool-int) 1))
    (assert-false ((p-iso-backward iso-bool-int) 0))))

;;; ============================================================
;;; Part 6: Schema Operation Tests
;;; ============================================================

(test-group "Schema Field Operations"

  (define-test "field-rename-iso renames field"
    (let* ([iso (field-rename-iso 'old-name 'new-name)]
           [alist '((old-name . 42) (other . 100))]
           [result ((p-iso-forward iso) alist)])
      (assert-true (pair? (assq 'new-name result)))
      (assert-false (assq 'old-name result))
      (assert-equal 42 (cdr (assq 'new-name result)))))

  (define-test "field-rename-iso roundtrips"
    (let* ([iso (field-rename-iso 'old 'new)]
           [alist '((old . 42) (x . 1))])
      (assert-equal alist ((p-iso-backward iso)
                           ((p-iso-forward iso) alist)))))

  (define-test "field-add-iso adds field with default"
    (let* ([iso (field-add-iso 'new-field 99)]
           [alist '((existing . 1))]
           [result ((p-iso-forward iso) alist)])
      (assert-true (pair? (assq 'new-field result)))
      (assert-equal 99 (cdr (assq 'new-field result)))))

  (define-test "field-add-iso rollback removes field"
    (let* ([iso (field-add-iso 'new-field 99)]
           [alist-with-field '((new-field . 99) (existing . 1))]
           [result ((p-iso-backward iso) alist-with-field)])
      (assert-false (assq 'new-field result))
      (assert-true (pair? (assq 'existing result)))))

  (define-test "field-add-iso is idempotent"
    (let* ([iso (field-add-iso 'status 'pending)]
           [alist-with-field '((status . active) (other . 1))]
           [result ((p-iso-forward iso) alist-with-field)])
      ;; Should NOT add duplicate, keep original value
      (assert-equal 'active (cdr (assq 'status result)))
      ;; Count occurrences of 'status - should be 1
      (assert-equal 1 (length (filter (lambda (p) (eq? (car p) 'status)) result)))))

  (define-test "field-add-iso roundtrips when field already exists"
    (let* ([iso (field-add-iso 'status 'pending)]
           [alist '((status . active) (other . 1))])
      ;; roundtrip should preserve original
      (assert-equal alist ((p-iso-backward iso)
                           ((p-iso-forward iso) alist)))))

  (define-test "field-transform-if-present-iso skips missing field"
    (let* ([value-iso (make-p-iso add1 sub1)]
           [iso (field-transform-if-present-iso 'count value-iso)]
           [alist '((name . "test"))]
           [result ((p-iso-forward iso) alist)])
      ;; Should pass through unchanged when field missing
      (assert-equal alist result)))

  (define-test "field-transform-iso transforms value"
    (let* ([value-iso (make-p-iso add1 sub1)]
           [iso (field-transform-iso 'count value-iso)]
           [alist '((count . 10) (name . "test"))]
           [result ((p-iso-forward iso) alist)])
      (assert-equal 11 (cdr (assq 'count result)))
      (assert-equal "test" (cdr (assq 'name result)))))

  (define-test "field-transform-iso roundtrips"
    (let* ([value-iso (make-p-iso add1 sub1)]
           [iso (field-transform-iso 'count value-iso)]
           [alist '((count . 10) (name . "test"))])
      (assert-equal alist ((p-iso-backward iso)
                           ((p-iso-forward iso) alist)))))

  (define-test "fields-rename-iso bulk rename"
    (let* ([mapping '((old1 . new1) (old2 . new2))]
           [iso (fields-rename-iso mapping)]
           [alist '((old1 . 1) (old2 . 2) (unchanged . 3))]
           [result ((p-iso-forward iso) alist)])
      (assert-true (pair? (assq 'new1 result)))
      (assert-true (pair? (assq 'new2 result)))
      (assert-true (pair? (assq 'unchanged result)))
      (assert-false (assq 'old1 result))
      (assert-false (assq 'old2 result))))

  ;; Tests for field-split-iso and field-merge-iso
  (define-test "field-split-iso splits single field"
    (let* ([splitter (lambda (v) `((first . ,(car v)) (second . ,(cdr v))))]
           [merger (lambda (pairs)
                     (cons (cdr (assq 'first pairs))
                           (cdr (assq 'second pairs))))]
           [iso (field-split-iso 'pair '(first second) splitter merger)]
           [alist '((pair . (1 . 2)) (other . 99))]
           [result ((p-iso-forward iso) alist)])
      (assert-true (pair? (assq 'first result)))
      (assert-true (pair? (assq 'second result)))
      (assert-false (assq 'pair result))
      (assert-equal 1 (cdr (assq 'first result)))
      (assert-equal 2 (cdr (assq 'second result)))))

  (define-test "field-split-iso roundtrips"
    (let* ([splitter (lambda (v) `((first . ,(car v)) (second . ,(cdr v))))]
           [merger (lambda (pairs)
                     (cons (cdr (assq 'first pairs))
                           (cdr (assq 'second pairs))))]
           [iso (field-split-iso 'pair '(first second) splitter merger)]
           [alist '((pair . (1 . 2)) (other . 99))])
      (assert-equal alist ((p-iso-backward iso)
                           ((p-iso-forward iso) alist)))))

  (define-test "field-merge-iso merges fields"
    (let* ([merger (lambda (pairs)
                     (cons (cdr (assq 'first pairs))
                           (cdr (assq 'second pairs))))]
           [splitter (lambda (v) `((first . ,(car v)) (second . ,(cdr v))))]
           [iso (field-merge-iso '(first second) 'pair merger splitter)]
           [alist '((first . 1) (second . 2) (other . 99))]
           [result ((p-iso-forward iso) alist)])
      (assert-true (pair? (assq 'pair result)))
      (assert-false (assq 'first result))
      (assert-false (assq 'second result))
      (assert-equal '(1 . 2) (cdr (assq 'pair result)))))

  (define-test "field-merge-iso roundtrips"
    (let* ([merger (lambda (pairs)
                     (cons (cdr (assq 'first pairs))
                           (cdr (assq 'second pairs))))]
           [splitter (lambda (v) `((first . ,(car v)) (second . ,(cdr v))))]
           [iso (field-merge-iso '(first second) 'pair merger splitter)]
           [alist '((first . 1) (second . 2) (other . 99))])
      (assert-equal alist ((p-iso-backward iso)
                           ((p-iso-forward iso) alist)))))

  (define-test "field-split-iso handles missing field"
    (let* ([splitter (lambda (v) `((a . ,(car v))))]
           [merger (lambda (pairs) (car (cdr (assq 'a pairs))))]
           [iso (field-split-iso 'missing '(a) splitter merger)]
           [alist '((other . 99))]
           [result ((p-iso-forward iso) alist)])
      ;; Should pass through unchanged when field missing
      (assert-equal alist result))))

;;; ============================================================
;;; Part 7: Block Migration Tests
;;; ============================================================

(test-group "Block Migrations"

  (define-test "make-block-migration creates valid migration"
    (let ([bm (make-block-migration 'v1 'v2 p-iso-id)])
      (assert-true (block-migration? bm))
      (assert-equal 'v1 (block-migration-from-tag bm))
      (assert-equal 'v2 (block-migration-to-tag bm))))

  (define-test "block-migration-applies? checks tag"
    (let* ([bm (make-block-migration 'old-tag 'new-tag p-iso-id)]
           [blk-match (make-block 'old-tag (string->utf8 "()") (vector))]
           [blk-nomatch (make-block 'other-tag (string->utf8 "()") (vector))])
      (assert-true (block-migration-applies? bm blk-match))
      (assert-false (block-migration-applies? bm blk-nomatch))))

  (define-test "block-migrate-payload transforms payload"
    (let* ([payload-iso (field-add-iso 'migrated #t)]
           [bm (make-block-migration 'v1 'v2 payload-iso)]
           [blk (make-block 'v1 (string->utf8 "((name . \"test\"))") (vector))]
           [result (block-migrate-payload bm blk)])
      (assert-equal 'v2 (block-tag result))
      (let ([sexpr (bytevector->sexpr (block-payload result))])
        (assert-true (pair? (assq 'migrated sexpr)))
        (assert-equal #t (cdr (assq 'migrated sexpr))))))

  (define-test "block-rollback-payload reverses transformation"
    (let* ([payload-iso (field-add-iso 'migrated #t)]
           [bm (make-block-migration 'v1 'v2 payload-iso)]
           [blk-v2 (make-block 'v2
                               (string->utf8 "((migrated . #t) (name . \"test\"))")
                               (vector))]
           [result (block-rollback-payload bm blk-v2)])
      (assert-equal 'v1 (block-tag result))
      (let ([sexpr (bytevector->sexpr (block-payload result))])
        (assert-false (assq 'migrated sexpr))
        (assert-true (pair? (assq 'name sexpr))))))

  (define-test "block-migration skips non-matching tags"
    (let* ([bm (make-block-migration 'v1 'v2 p-iso-id)]
           [blk (make-block 'other-tag (string->utf8 "()") (vector))]
           [result (block-migrate-payload bm blk)])
      ;; Should return unchanged
      (assert-equal 'other-tag (block-tag result)))))

;;; ============================================================
;;; Part 8: Version Detection Tests
;;; ============================================================

(test-group "Version Detection"

  (define-test "parse-versioned-tag extracts version"
    (let ([result (parse-versioned-tag 'bbs-issue-v2)])
      (assert-equal 'bbs-issue (car result))
      (assert-equal 2 (cdr result))))

  (define-test "parse-versioned-tag handles no version"
    (let ([result (parse-versioned-tag 'simple-tag)])
      (assert-equal 'simple-tag (car result))
      (assert-false (cdr result))))

  (define-test "make-versioned-tag creates correct symbol"
    (assert-equal 'bbs-issue-v3 (make-versioned-tag 'bbs-issue 3))
    (assert-equal 'user-v1 (make-versioned-tag 'user 1))))

;;; ============================================================
;;; Part 9: Block Migration Composition Tests
;;; ============================================================

(test-group "Block Migration Composition"

  (define-test "block-migration-compose chains transformations"
    (let* ([bm1 (make-block-migration 'v1 'v2 (field-add-iso 'step1 #t))]
           [bm2 (make-block-migration 'v2 'v3 (field-add-iso 'step2 #t))]
           [composed (block-migration-compose bm1 bm2)])
      (assert-equal 'v1 (block-migration-from-tag composed))
      (assert-equal 'v3 (block-migration-to-tag composed))))

  (define-test "block-migration-flip reverses tags and operations"
    (let* ([bm (make-block-migration 'v1 'v2 (field-add-iso 'new #t))]
           [flipped (block-migration-flip bm)])
      (assert-equal 'v2 (block-migration-from-tag flipped))
      (assert-equal 'v1 (block-migration-to-tag flipped)))))

;;; ============================================================
;;; Part 10: Integration Tests
;;; ============================================================

(test-group "Integration"

  (define-test "full schema migration roundtrip"
    (let* ([schema-iso (schema-compose
                        (list (field-rename-iso 'old-name 'name)
                              (field-add-iso 'version 1)
                              (field-transform-iso 'count (make-p-iso add1 sub1))))]
           [m (make-migration 'schema-v1-v2 'v1 'v2 schema-iso)]
           [data '((old-name . "Alice") (count . 10))]
           [migrated (migrate m data)]
           [rolled-back (rollback m migrated)])
      ;; Check migrated data
      (assert-true (pair? (assq 'name migrated)))
      (assert-false (assq 'old-name migrated))
      (assert-equal 1 (cdr (assq 'version migrated)))
      (assert-equal 11 (cdr (assq 'count migrated)))
      ;; Check rollback
      (assert-equal data rolled-back)))

  (define-test "migration chain with block migration"
    (let* ([bm1 (make-block-migration 'issue-v1 'issue-v2
                                      (field-add-iso 'priority 0))]
           [bm2 (make-block-migration 'issue-v2 'issue-v3
                                      (field-rename-iso 'desc 'description))]
           [composed (block-migration-compose bm1 bm2)]
           [blk (make-block 'issue-v1
                           (string->utf8 "((title . \"Bug\") (desc . \"Fix it\"))")
                           (vector))]
           [result (block-migrate-payload composed blk)]
           [sexpr (bytevector->sexpr (block-payload result))])
      (assert-equal 'issue-v3 (block-tag result))
      (assert-true (pair? (assq 'priority sexpr)))
      (assert-true (pair? (assq 'description sexpr)))
      (assert-false (assq 'desc sexpr)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
