;;; lattice/optics/test-block-optics.ss — Tests for Block Optics
;;;
;;; Tests for optics over The Fold's block system.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/optics/block-optics.ss")

;;; ============================================================
;;; Test Setup
;;; ============================================================

;;; Create test blocks for use in tests.

;;; Test payloads
(define test-payload-1 (string->utf8 "hello world"))
(define test-payload-2 (string->utf8 "(+ 1 2 3)"))
(define test-payload-empty empty-payload)

;;; Test refs (fake hashes for testing)
(define test-ref-1 (make-bytevector 33 1))
(define test-ref-2 (make-bytevector 33 2))
(define test-ref-3 (make-bytevector 33 3))

;;; Test blocks
(define block-no-refs
  (make-block 'lit test-payload-1 empty-refs))

(define block-one-ref
  (make-block 'app test-payload-empty (vector test-ref-1)))

(define block-many-refs
  (make-block 'lambda test-payload-2 (vector test-ref-1 test-ref-2 test-ref-3)))

(define block-expr
  (make-block 'expr (string->utf8 "(list 1 2 3)") empty-refs))

;;; ============================================================
;;; Part 1: Basic Lens Tests
;;; ============================================================

(test-group "block-tag-lens"
  (define-test "view extracts tag"
    (assert-equal 'lit (view block-tag-lens block-no-refs))
    (assert-equal 'app (view block-tag-lens block-one-ref))
    (assert-equal 'lambda (view block-tag-lens block-many-refs)))

  (define-test "set creates new block with updated tag"
    (let ([updated (set-lens block-tag-lens 'custom block-no-refs)])
      (assert-equal 'custom (block-tag updated))
      ;; Payload and refs unchanged
      (assert-true (bytevector=? test-payload-1 (block-payload updated)))
      (assert-equal 0 (vector-length (block-refs updated)))))

  (define-test "over modifies tag"
    (let ([updated (over block-tag-lens
                         (lambda (t) (string->symbol (string-append (symbol->string t) "-v2")))
                         block-no-refs)])
      (assert-equal 'lit-v2 (block-tag updated)))))

(test-group "block-payload-lens"
  (define-test "view extracts payload"
    (assert-true (bytevector=? test-payload-1 (view block-payload-lens block-no-refs))))

  (define-test "set creates new block with updated payload"
    (let* ([new-payload (string->utf8 "new content")]
           [updated (set-lens block-payload-lens new-payload block-no-refs)])
      (assert-true (bytevector=? new-payload (block-payload updated)))
      ;; Tag and refs unchanged
      (assert-equal 'lit (block-tag updated))
      (assert-equal 0 (vector-length (block-refs updated))))))

(test-group "block-refs-lens"
  (define-test "view extracts refs vector"
    (let ([refs (view block-refs-lens block-many-refs)])
      (assert-equal 3 (vector-length refs))
      (assert-true (bytevector=? test-ref-1 (vector-ref refs 0)))))

  (define-test "set creates new block with updated refs"
    (let* ([new-refs (vector test-ref-3 test-ref-2)]
           [updated (set-lens block-refs-lens new-refs block-no-refs)])
      (assert-equal 2 (vector-length (block-refs updated)))
      (assert-true (bytevector=? test-ref-3 (vector-ref (block-refs updated) 0))))))

;;; ============================================================
;;; Part 2: Ref Access Tests
;;; ============================================================

(test-group "block-ref-at"
  (define-test "preview existing ref succeeds"
    (let ([result (affine-preview (block-ref-at 0) block-many-refs)])
      (assert-true (just? result))
      (assert-true (bytevector=? test-ref-1 (from-just result)))))

  (define-test "preview second ref"
    (let ([result (affine-preview (block-ref-at 1) block-many-refs)])
      (assert-true (just? result))
      (assert-true (bytevector=? test-ref-2 (from-just result)))))

  (define-test "preview out of bounds returns nothing"
    (let ([result (affine-preview (block-ref-at 5) block-many-refs)])
      (assert-true (nothing? result))))

  (define-test "preview on empty refs returns nothing"
    (let ([result (affine-preview (block-ref-at 0) block-no-refs)])
      (assert-true (nothing? result))))

  (define-test "set updates existing ref"
    (let* ([new-ref (make-bytevector 33 99)]
           [updated (affine-set (block-ref-at 1) new-ref block-many-refs)])
      (assert-true (bytevector=? new-ref (vector-ref (block-refs updated) 1)))
      ;; Other refs unchanged
      (assert-true (bytevector=? test-ref-1 (vector-ref (block-refs updated) 0)))
      (assert-true (bytevector=? test-ref-3 (vector-ref (block-refs updated) 2)))))

  (define-test "set out of bounds leaves block unchanged"
    (let* ([new-ref (make-bytevector 33 99)]
           [updated (affine-set (block-ref-at 10) new-ref block-many-refs)])
      ;; Should be unchanged
      (assert-equal 3 (vector-length (block-refs updated)))
      (assert-true (bytevector=? test-ref-1 (vector-ref (block-refs updated) 0))))))

(test-group "block-refs-each"
  (define-test "traversal-to-list returns all refs"
    (let ([refs (traversal-to-list block-refs-each block-many-refs)])
      (assert-equal 3 (length refs))
      (assert-true (bytevector=? test-ref-1 (car refs)))))

  (define-test "traversal-to-list on empty refs returns empty"
    (let ([refs (traversal-to-list block-refs-each block-no-refs)])
      (assert-equal 0 (length refs))))

  (define-test "traversal-over modifies all refs"
    (let* ([increment-first-byte
            (lambda (ref)
              (let ([new-ref (bytevector-copy ref)])
                (bytevector-u8-set! new-ref 0 (+ 1 (bytevector-u8-ref ref 0)))
                new-ref))]
           [updated (traversal-over block-refs-each increment-first-byte block-many-refs)])
      ;; All refs should have first byte incremented
      (assert-equal 2 (bytevector-u8-ref (vector-ref (block-refs updated) 0) 0))
      (assert-equal 3 (bytevector-u8-ref (vector-ref (block-refs updated) 1) 0))
      (assert-equal 4 (bytevector-u8-ref (vector-ref (block-refs updated) 2) 0)))))

(test-group "block-refs-count"
  (define-test "counts refs correctly"
    (assert-equal 0 (block-refs-count block-no-refs))
    (assert-equal 1 (block-refs-count block-one-ref))
    (assert-equal 3 (block-refs-count block-many-refs))))

;;; ============================================================
;;; Part 3: CAS-Aware Optics Tests
;;; ============================================================

;;; Create a mock CAS for testing
(define mock-cas (make-hashtable equal-hash equal?))

;;; Counter for generating unique hashes
(define mock-hash-counter 0)

;;; Store test blocks in mock CAS
(define (mock-store! blk)
  (let ([hash (make-bytevector 33 0)])
    ;; Use incrementing counter for unique hashes
    (set! mock-hash-counter (+ mock-hash-counter 1))
    (bytevector-u8-set! hash 0 mock-hash-counter)
    (hashtable-set! mock-cas hash blk)
    hash))

(define (mock-fetch hash)
  (hashtable-ref mock-cas hash #f))

;;; Set up mock store with child blocks
(define child-block-1 (make-block 'alpha (string->utf8 "first child") empty-refs))
(define child-block-2 (make-block 'beta (string->utf8 "second child") empty-refs))
(define child-hash-1 (mock-store! child-block-1))
(define child-hash-2 (mock-store! child-block-2))
(define parent-block (make-block 'parent empty-payload (vector child-hash-1 child-hash-2)))

(test-group "follow-ref"
  (define-test "follows existing ref to block"
    (let ([result (affine-preview (follow-ref mock-fetch) child-hash-1)])
      (assert-true (just? result))
      (assert-equal 'alpha (block-tag (from-just result)))))

  (define-test "returns nothing for missing ref"
    (let* ([missing-hash (make-bytevector 33 255)]
           [result (affine-preview (follow-ref mock-fetch) missing-hash)])
      (assert-true (nothing? result)))))

(test-group "block-child-at"
  (define-test "accesses child block by index"
    (let ([result (affine-preview (block-child-at mock-fetch 0) parent-block)])
      (assert-true (just? result))
      (assert-equal 'alpha (block-tag (from-just result)))))

  (define-test "accesses second child"
    (let ([result (affine-preview (block-child-at mock-fetch 1) parent-block)])
      (assert-true (just? result))
      (assert-equal 'beta (block-tag (from-just result)))))

  (define-test "returns nothing for out of bounds"
    (let ([result (affine-preview (block-child-at mock-fetch 5) parent-block)])
      (assert-true (nothing? result)))))

(test-group "block-children-each"
  (define-test "traverses all child blocks"
    (let ([children (traversal-to-list (block-children-each mock-fetch) parent-block)])
      (assert-equal 2 (length children))
      (assert-equal 'alpha (block-tag (car children)))
      (assert-equal 'beta (block-tag (cadr children)))))

  (define-test "empty for leaf block"
    (let ([children (traversal-to-list (block-children-each mock-fetch) child-block-1)])
      (assert-equal 0 (length children)))))

;;; ============================================================
;;; Part 4: Type Prism Tests
;;; ============================================================

(test-group "block-type-prism"
  (define-test "matches correct type"
    (let ([result (preview block-lambda-prism block-many-refs)])
      (assert-true (just? result))
      (assert-equal 'lambda (block-tag (from-just result)))))

  (define-test "returns nothing for wrong type"
    (let ([result (preview block-lambda-prism block-no-refs)])
      (assert-true (nothing? result))))

  (define-test "custom type prism works"
    (let* ([custom-prism (block-type-prism 'lit)]
           [result (preview custom-prism block-no-refs)])
      (assert-true (just? result)))))

(test-group "common-type-prisms"
  (define-test "block-app-prism matches app"
    (assert-true (just? (preview block-app-prism block-one-ref))))

  (define-test "block-literal-prism matches lit"
    (assert-true (just? (preview block-literal-prism block-no-refs))))

  (define-test "block-expr-prism matches expr"
    (assert-true (just? (preview block-expr-prism block-expr)))))

;;; ============================================================
;;; Part 5: Payload Optics Tests
;;; ============================================================

(test-group "block-payload-string-iso"
  (define-test "iso-view converts bytevector to string"
    (let ([str (iso-view block-payload-string-iso test-payload-1)])
      (assert-equal "hello world" str)))

  (define-test "iso-review converts string to bytevector"
    (let ([bv (iso-review block-payload-string-iso "test")])
      (assert-true (bytevector=? (string->utf8 "test") bv)))))

(test-group "block-payload-as-string-lens"
  (define-test "view extracts payload as string"
    (let ([str (view block-payload-as-string-lens block-no-refs)])
      (assert-equal "hello world" str)))

  (define-test "set updates payload from string"
    (let ([updated (set-lens block-payload-as-string-lens "new string" block-no-refs)])
      (assert-equal "new string" (utf8->string (block-payload updated))))))

(test-group "block-payload-sexpr-affine"
  (define-test "preview parses sexpr payload"
    (let ([result (affine-preview block-payload-sexpr-affine block-expr)])
      (assert-true (just? result))
      (assert-equal '(list 1 2 3) (from-just result))))

  (define-test "preview returns nothing for non-sexpr"
    ;; "hello world" is not a valid S-expression (two tokens)
    (let ([result (affine-preview block-payload-sexpr-affine block-no-refs)])
      (assert-true (nothing? result))))

  (define-test "set updates payload with sexpr"
    (let* ([new-expr '(define x 42)]
           [updated (affine-set block-payload-sexpr-affine new-expr block-expr)])
      (assert-equal "(define x 42)" (utf8->string (block-payload updated))))))

;;; ============================================================
;;; Part 6: Composition Tests
;;; ============================================================

(test-group "optic-composition"
  (define-test ">>> composes lenses"
    (let* ([composed (>>> block-refs-lens (make-lens vector-length (lambda (n v) v)))]
           [count (view composed block-many-refs)])
      (assert-equal 3 count)))

  (define-test "compose tag lens with prism"
    ;; First filter by type, then access tag (should be same as type)
    (let ([composed (optic-compose block-lambda-prism block-tag-lens)])
      ;; affine-compose returns affine
      (let ([result (affine-preview composed block-many-refs)])
        (assert-true (just? result))
        (assert-equal 'lambda (from-just result))))))

;;; ============================================================
;;; Part 7: Utility Function Tests
;;; ============================================================

(test-group "utility-predicates"
  (define-test "block-has-refs? works"
    (assert-false (block-has-refs? block-no-refs))
    (assert-true (block-has-refs? block-one-ref))
    (assert-true (block-has-refs? block-many-refs)))

  (define-test "block-is-leaf? works"
    (assert-true (block-is-leaf? block-no-refs))
    (assert-false (block-is-leaf? block-one-ref)))

  (define-test "block-tag-is? works"
    (assert-true ((block-tag-is? 'lit) block-no-refs))
    (assert-false ((block-tag-is? 'lit) block-one-ref))))

(test-group "collect-block-tree"
  (define-test "collects single block"
    (let ([tree (collect-block-tree mock-fetch child-block-1)])
      (assert-equal 1 (length tree))))

  (define-test "collects parent and children"
    (let ([tree (collect-block-tree mock-fetch parent-block)])
      ;; Should have parent + 2 children = 3
      (assert-equal 3 (length tree)))))

;;; ============================================================
;;; Part 8: Operator Tests
;;; ============================================================

(test-group "optic-operators"
  (define-test "^. views through lens"
    (assert-equal 'lit (^. block-no-refs block-tag-lens)))

  (define-test "^? previews through affine"
    (let ([result (^? block-many-refs (block-ref-at 0))])
      (assert-true (just? result))))

  (define-test "^.. gets all targets from traversal"
    (let ([refs (^.. block-many-refs block-refs-each)])
      (assert-equal 3 (length refs))))

  (define-test ".~ sets through lens"
    (let ([updated (& block-no-refs (.~ block-tag-lens 'modified))])
      (assert-equal 'modified (block-tag updated))))

  (define-test "%~ modifies through lens"
    (let ([updated (& block-no-refs
                      (%~ block-payload-as-string-lens
                          (lambda (s) (string-append s "!"))))])
      (assert-equal "hello world!" (view block-payload-as-string-lens updated)))))

;;; ============================================================
;;; Part 9: Lens Law Verification
;;; ============================================================

(test-group "lens-laws"
  (define-test "block-tag-lens satisfies Get-Put"
    ;; set l (view l s) s = s
    (let* ([s block-no-refs]
           [result (set-lens block-tag-lens (view block-tag-lens s) s)])
      (assert-equal (block-tag s) (block-tag result))
      (assert-true (bytevector=? (block-payload s) (block-payload result)))))

  (define-test "block-tag-lens satisfies Put-Get"
    ;; view l (set l a s) = a
    (let* ([s block-no-refs]
           [a 'new-tag]
           [result (view block-tag-lens (set-lens block-tag-lens a s))])
      (assert-equal a result)))

  (define-test "block-payload-lens satisfies Get-Put"
    (let* ([s block-no-refs]
           [result (set-lens block-payload-lens (view block-payload-lens s) s)])
      (assert-true (bytevector=? (block-payload s) (block-payload result)))))

  (define-test "block-refs-lens satisfies Get-Put"
    (let* ([s block-many-refs]
           [result (set-lens block-refs-lens (view block-refs-lens s) s)])
      (assert-equal (vector-length (block-refs s))
                    (vector-length (block-refs result))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
