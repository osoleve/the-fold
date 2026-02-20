(load "core/testing/test-framework.ss")
(load "lattice/meta/bbs-xref.ss")

;;; ====
;;; Label Construction
;;; ====

(test-group "label-construction"

  (define-test "xref-skill creates @skill: label"
    (assert-equal '@skill:egraph (xref-skill 'egraph)))

  (define-test "xref-module creates @module: label"
    (assert-equal '@module:reverse-diff (xref-module 'reverse-diff)))

  (define-test "xref-export creates @export: label"
    (assert-equal '@export:vec2-add (xref-export 'vec2-add)))

  (define-test "labels are symbols"
    (assert-true (symbol? (xref-skill 'linalg)))
    (assert-true (symbol? (xref-module 'vec)))
    (assert-true (symbol? (xref-export 'dot)))))

;;; ====
;;; Label Parsing
;;; ====

(test-group "label-parsing"

  (define-test "parse skill label"
    (let ([parsed (xref-parse '@skill:egraph)])
      (assert-true (pair? parsed))
      (assert-equal 'skill (car parsed))
      (assert-equal 'egraph (cdr parsed))))

  (define-test "parse module label"
    (let ([parsed (xref-parse '@module:vec)])
      (assert-true (pair? parsed))
      (assert-equal 'module (car parsed))
      (assert-equal 'vec (cdr parsed))))

  (define-test "parse export label"
    (let ([parsed (xref-parse '@export:dot)])
      (assert-true (pair? parsed))
      (assert-equal 'export (car parsed))
      (assert-equal 'dot (cdr parsed))))

  (define-test "non-xref label returns #f"
    (assert-false (xref-parse 'bug))
    (assert-false (xref-parse 'enhancement)))

  (define-test "xref-label? identifies xref labels"
    (assert-true (xref-label? '@skill:linalg))
    (assert-true (xref-label? '@module:vec))
    (assert-true (xref-label? '@export:dot))
    (assert-false (xref-label? 'bug))
    (assert-false (xref-label? 'wontfix)))

  (define-test "xref-type? checks specific type"
    (assert-true (xref-type? '@skill:linalg 'skill))
    (assert-false (xref-type? '@skill:linalg 'module))
    (assert-true (xref-type? '@module:vec 'module))
    (assert-false (xref-type? 'bug 'skill))))

;;; ====
;;; Roundtrip
;;; ====

(test-group "roundtrip"

  (define-test "construct then parse roundtrips"
    (let ([label (xref-skill 'egraph)])
      (let ([parsed (xref-parse label)])
        (assert-equal 'skill (car parsed))
        (assert-equal 'egraph (cdr parsed)))))

  (define-test "all types roundtrip"
    (for-each
      (lambda (pair)
        (let* ([constructor (car pair)]
               [type (cdr pair)]
               [label (constructor 'test-name)]
               [parsed (xref-parse label)])
          (assert-true (pair? parsed))
          (assert-equal type (car parsed))
          (assert-equal 'test-name (cdr parsed))))
      (list (cons xref-skill 'skill)
            (cons xref-module 'module)
            (cons xref-export 'export)))))

;;; ====
;;; Extraction
;;; ====

(test-group "extraction"

  (define labels '(@skill:linalg @module:vec bug @export:dot enhancement @skill:egraph))

  (define-test "extract-labels gets all xrefs"
    (let ([xrefs (xref-extract-labels labels)])
      (assert-equal 4 (length xrefs))
      (assert-true (pair? (assq 'skill xrefs)))))

  (define-test "xref-skills extracts only skills"
    (let ([skills (xref-skills labels)])
      (assert-equal 2 (length skills))
      (assert-true (pair? (memq 'linalg skills)))
      (assert-true (pair? (memq 'egraph skills)))))

  (define-test "xref-modules extracts only modules"
    (let ([mods (xref-modules labels)])
      (assert-equal 1 (length mods))
      (assert-equal 'vec (car mods))))

  (define-test "xref-exports extracts only exports"
    (let ([exps (xref-exports labels)])
      (assert-equal 1 (length exps))
      (assert-equal 'dot (car exps))))

  (define-test "empty labels returns empty"
    (assert-true (null? (xref-extract-labels '())))
    (assert-true (null? (xref-skills '()))))

  (define-test "no xrefs returns empty"
    (assert-true (null? (xref-extract-labels '(bug enhancement))))))

;;; ====
;;; Index
;;; ====

(test-group "index"

  (define-test "make-xref-index creates valid index"
    (let ([idx (make-xref-index)])
      (assert-true (xref-index? idx))))

  (define-test "index-issue adds entries"
    (let ([idx (make-xref-index)])
      (xref-index-issue! idx "fold-abc1" '(@skill:linalg @module:vec bug))
      (assert-equal '("fold-abc1") (xref-issues-for-skill idx 'linalg))
      (assert-equal '("fold-abc1") (xref-issues-for-module idx 'vec))
      (assert-true (null? (xref-issues-for-export idx 'dot)))))

  (define-test "multiple issues per entity"
    (let ([idx (make-xref-index)])
      (xref-index-issue! idx "fold-abc1" '(@skill:linalg))
      (xref-index-issue! idx "fold-abc2" '(@skill:linalg))
      (let ([issues (xref-issues-for-skill idx 'linalg)])
        (assert-equal 2 (length issues))
        (assert-true (pair? (member "fold-abc1" issues)))
        (assert-true (pair? (member "fold-abc2" issues))))))

  (define-test "idempotent indexing"
    (let ([idx (make-xref-index)])
      (xref-index-issue! idx "fold-abc1" '(@skill:linalg))
      (xref-index-issue! idx "fold-abc1" '(@skill:linalg))
      (assert-equal 1 (length (xref-issues-for-skill idx 'linalg)))))

  (define-test "build-index from issue list"
    (let ([idx (xref-build-index
                 '(("fold-abc1" . (@skill:linalg @module:vec))
                   ("fold-abc2" . (@skill:egraph @module:vec))
                   ("fold-abc3" . (bug enhancement))))])
      (assert-equal 1 (length (xref-issues-for-skill idx 'linalg)))
      (assert-equal 1 (length (xref-issues-for-skill idx 'egraph)))
      (assert-equal 2 (length (xref-issues-for-module idx 'vec)))
      (assert-true (null? (xref-issues-for-skill idx 'optics))))))

;;; ====
;;; Summary
;;; ====

(test-group "summary"

  (define-test "summary counts correctly"
    (let ([idx (xref-build-index
                 '(("i1" . (@skill:linalg @module:vec @export:dot))
                   ("i2" . (@skill:egraph))))])
      (let ([s (xref-summary idx)])
        (assert-equal 'xref-summary (car s))
        (assert-equal 2 (cadr (assq 'skills (cdr s))))
        (assert-equal 1 (cadr (assq 'modules (cdr s))))
        (assert-equal 1 (cadr (assq 'exports (cdr s))))
        (assert-equal 4 (cadr (assq 'total-refs (cdr s))))))))

;;; ====
;;; Label Merging
;;; ====

(test-group "label-merging"

  (define-test "add-skill appends new label"
    (let ([labels (xref-add-skill '(bug) 'linalg)])
      (assert-equal 2 (length labels))
      (assert-true (pair? (memq '@skill:linalg labels)))))

  (define-test "add-skill is idempotent"
    (let* ([labels (xref-add-skill '(bug) 'linalg)]
           [labels2 (xref-add-skill labels 'linalg)])
      (assert-equal 2 (length labels2))))

  (define-test "add-module works"
    (let ([labels (xref-add-module '() 'vec)])
      (assert-equal 1 (length labels))
      (assert-true (pair? (memq '@module:vec labels)))))

  (define-test "add-export works"
    (let ([labels (xref-add-export '() 'dot)])
      (assert-equal 1 (length labels))
      (assert-true (pair? (memq '@export:dot labels)))))

  (define-test "strip removes all xref labels"
    (let ([labels '(@skill:linalg bug @module:vec enhancement @export:dot)])
      (let ([stripped (xref-strip labels)])
        (assert-equal 2 (length stripped))
        (assert-true (pair? (memq 'bug stripped)))
        (assert-true (pair? (memq 'enhancement stripped))))))

  (define-test "strip on no xrefs is identity"
    (let ([labels '(bug enhancement wontfix)])
      (assert-equal labels (xref-strip labels)))))

;;; ====
;;; Auto-Detection
;;; ====

(test-group "auto-detect"

  (define-test "detects known skill name in text"
    (let ([results (xref-auto-detect "fix the linalg eigenvalue bug" '(linalg egraph optics))])
      (assert-equal 1 (length results))
      (assert-equal 'skill (car (car results)))
      (assert-equal 'linalg (cdr (car results)))))

  (define-test "detects multiple skills"
    (let ([results (xref-auto-detect "bridge linalg and optics" '(linalg egraph optics))])
      (assert-equal 2 (length results))))

  (define-test "case insensitive"
    (let ([results (xref-auto-detect "LINALG is broken" '(linalg))])
      (assert-equal 1 (length results))))

  (define-test "no match returns empty"
    (let ([results (xref-auto-detect "nothing relevant here" '(linalg egraph))])
      (assert-true (null? results))))

  (define-test "hyphenated names work"
    (let ([results (xref-auto-detect "fix number-theory primes" '(number-theory linalg))])
      (assert-equal 1 (length results))
      (assert-equal 'number-theory (cdr (car results))))))

(run-all-tests)
