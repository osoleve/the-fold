;;; lattice/meta/test-kg-ingest.ss — Tests for KG Ingestion Pipeline
;;;
;;; Run with: scheme --script lattice/meta/test-kg-ingest.ss

(load "core/testing/test-framework.ss")
(load "lattice/meta/kg-ingest.ss")

(display "\n====\n")
(display "KG Ingestion Pipeline Tests\n")
(display "====\n\n")

;;; ====
;;; Entity Normalization
;;; ====

(test-group normalize-entity-tests

  (define-test test-entity-basic
    (assert-equal 'ada-lovelace (kg-normalize-entity "Ada Lovelace")))

  (define-test test-entity-symbol-input
    (assert-equal 'new-york (kg-normalize-entity 'New_York)))

  (define-test test-entity-empty
    (assert-equal 'unknown (kg-normalize-entity "  ")))

  (define-test test-entity-alphanumeric
    (assert-equal 'c3po (kg-normalize-entity "C3PO")))

)

;;; ====
;;; Relation Normalization
;;; ====

(test-group normalize-relation-tests

  (define-test test-rel-works-at
    (assert-equal 'works_at (kg-normalize-relation "works at")))

  (define-test test-rel-employee-of
    (assert-equal 'works_at (kg-normalize-relation "employee of")))

  (define-test test-rel-located-in
    (assert-equal 'located_in (kg-normalize-relation "located in")))

  (define-test test-rel-part-of
    (assert-equal 'part_of (kg-normalize-relation "part of")))

  (define-test test-rel-founded
    (assert-equal 'founded (kg-normalize-relation "founded")))

  (define-test test-rel-fallback
    (assert-equal 'rel_contains (kg-normalize-relation "contains")))

)

;;; ====
;;; Triple Construction
;;; ====

(test-group make-triple-tests

  (define-test test-make-triple-strings
    (assert-equal '(ada-lovelace works_at analytical-engine)
                  (kg-make-triple "Ada Lovelace" "works at" "Analytical Engine")))

  (define-test test-make-triple-mixed
    (assert-equal '(paris located_in france)
                  (kg-make-triple 'Paris "located in" 'France)))

)

;;; ====
;;; Triple Validation
;;; ====

(test-group valid-triple-tests

  (define-test test-valid-basic
    (assert-true (kg-valid-triple? '(ada works_at acme))))

  (define-test test-invalid-unknown-subject
    (assert-false (kg-valid-triple? '(unknown works_at acme))))

  (define-test test-invalid-unknown-relation
    (assert-false (kg-valid-triple? '(ada unknown_relation acme))))

  (define-test test-invalid-short
    (assert-false (kg-valid-triple? '(ada works_at))))

  (define-test test-invalid-non-symbol
    (assert-false (kg-valid-triple? '(ada works_at 42))))

)

;;; ====
;;; Fact Parsing
;;; ====

(test-group parse-fact-tests

  (define-test test-parse-works-at
    (assert-equal '(ada works_at acme) (kg-parse-simple-fact "Ada works at Acme")))

  (define-test test-parse-is-part-of
    (assert-equal '(paris part_of france) (kg-parse-simple-fact "Paris is part of France")))

  (define-test test-parse-founded
    (assert-equal '(linus founded linux) (kg-parse-simple-fact "Linus founded Linux")))

  (define-test test-parse-lives-in
    (assert-equal '(bob located_in paris) (kg-parse-simple-fact "Bob lives in Paris")))

  (define-test test-parse-no-match
    (assert-false (kg-parse-simple-fact "No relation sentence")))

  (define-test test-parse-empty-subject
    (assert-false (kg-parse-simple-fact "works at Acme")))

)

;;; ====
;;; Batch Extraction
;;; ====

(test-group extract-triples-tests

  (define-test test-extract-dedup-and-filter
    (assert-equal '((ada works_at acme) (bob located_in paris))
                  (kg-extract-triples '("Ada works at Acme"
                                        "Ada works at Acme"
                                        "Bob lives in Paris"
                                        "No relation sentence"))))

)

;;; ====
;;; Store Upsert
;;; ====

(test-group upsert-tests

  (define-test test-upsert-single
    (assert-equal '((ada (works_at acme)))
                  (kg-upsert-triple '() '(ada works_at acme))))

  (define-test test-upsert-dedup
    (assert-equal '((ada (works_at acme)))
                  (kg-upsert-triple (kg-upsert-triple '() '(ada works_at acme))
                                    '(ada works_at acme))))

  (define-test test-upsert-multi-object
    (assert-equal '((ada (works_at acme babbage-labs)))
                  (kg-upsert-triple (kg-upsert-triple '() '(ada works_at acme))
                                    '(ada works_at babbage-labs))))

  (define-test test-upsert-triples-batch
    (assert-equal '((ada (works_at acme) (founded analytical-engine))
                   (bob (located_in paris)))
                  (kg-upsert-triples '()
                                     '((ada works_at acme)
                                       (bob located_in paris)
                                       (ada works_at acme)
                                       (ada founded analytical-engine)))))

)

;;; ====
;;; Integration: NL -> Store
;;; ====

(test-group integration-tests

  (define-test test-end-to-end
    (let* ([sentences '("Ada works at Acme"
                        "Bob lives in Paris"
                        "Ada works at Acme"
                        "Ada founded Analytical Engine"
                        "Not a fact")]
           [triples (kg-extract-triples sentences)]
           [store (kg-upsert-triples '() triples)])
      ;; Should have 3 unique triples
      (assert-equal 3 (length triples))
      ;; Store should have 2 subjects
      (assert-equal 2 (length store))
      ;; Ada should have 2 predicates
      (let ([ada-row (assq 'ada store)])
        (assert-true (if ada-row #t #f))
        (assert-equal 2 (length (cdr ada-row))))))

)

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
