;;; fabric/stitches/test-pretty.ss -- Tests for Pretty Printing Combinators

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "pretty.ss")

(display "
")
(display "==============================================================
")
(display "          PRETTY PRINTING COMBINATOR TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Document Type Tests
;;; ============================================================

(test-group doc-type-tests
            (define-test doc-empty-test
              (assert-true (doc? (doc-empty)))
              (assert-true (doc-empty? (doc-empty))))
            
            (define-test doc-text-test
              (assert-true (doc? (doc-text "hello")))
              (assert-true (doc-text? (doc-text "hello")))
              (assert-equal "hello" (doc-text-str (doc-text "hello"))))
            
            (define-test doc-line-test
              (assert-true (doc? (doc-line)))
              (assert-true (doc-line? (doc-line))))
            
            (define-test doc-nest-test
              (let ([d (doc-nest 4 (doc-text "x"))])
                   (assert-true (doc? d))
                   (assert-true (doc-nest? d))
                   (assert-equal 4 (doc-nest-n d))))
            
            (define-test doc-concat-test
              (let ([d (doc-concat (doc-text "a") (doc-text "b"))])
                   (assert-true (doc? d))
                   (assert-true (doc-concat? d))))
            
            (define-test doc-group-test
              (let ([d (doc-group (doc-text "x"))])
                   (assert-true (doc? d))
                   (assert-true (doc-group? d)))))

;;; ============================================================
;;; Primitive Tests
;;; ============================================================

(test-group primitive-tests
            (define-test empty-renders-empty
              (assert-equal "" (pretty 80 empty)))
            
            (define-test text-renders-text
              (assert-equal "hello" (pretty 80 (text "hello"))))
            
            (define-test empty-text-is-empty
              (assert-equal "" (pretty 80 (text ""))))
            
            (define-test line-renders-newline
              (assert-equal "a
b" (pretty 80 (<> (text "a") (<> line (text "b")))))))

;;; ============================================================
;;; Combinator Tests
;;; ============================================================

(test-group combinator-tests
            (define-test concat-test
              (assert-equal "ab" (pretty 80 (<> (text "a") (text "b")))))
            
            (define-test concat-with-empty-left
              (assert-equal "b" (pretty 80 (<> empty (text "b")))))
            
            (define-test concat-with-empty-right
              (assert-equal "a" (pretty 80 (<> (text "a") empty))))
            
            (define-test space-concat-test
              (assert-equal "a b" (pretty 80 (<+> (text "a") (text "b")))))
            
            (define-test nest-increases-indent
              (assert-equal "a
  b"
                            (pretty 80 (<> (text "a")
                                           (nest 2 (<> line (text "b"))))))))

;;; ============================================================
;;; Group Tests
;;; ============================================================

(test-group group-tests
            ;; Group flattens when it fits
            (define-test group-flattens-when-fits
              (let ([doc (group (<> (text "a") (<> line (text "b"))))])
                   (assert-equal "a b" (pretty 80 doc))))
            
            ;; Group breaks when doesn't fit
            (define-test group-breaks-when-needed
              (let ([doc (group (<> (text "aaaaaaaaaa")
                                    (<> line (text "bbbbbbbbbb"))))])
                   ;; Width 15 can't fit "aaaaaaaaaa bbbbbbbbbb" (21 chars)
                   (assert-equal "aaaaaaaaaa
bbbbbbbbbb" (pretty 15 doc)))))

;;; ============================================================
;;; List Combinator Tests
;;; ============================================================

(test-group list-combinator-tests
            (define-test hcat-test
              (assert-equal "abc"
                            (pretty 80 (hcat (list (text "a") (text "b") (text "c"))))))
            
            (define-test hsep-test
              (assert-equal "a b c"
                            (pretty 80 (hsep (list (text "a") (text "b") (text "c"))))))
            
            (define-test vcat-test
              (assert-equal "a
b
c"
                            (pretty 80 (vcat (list (text "a") (text "b") (text "c"))))))
            
            (define-test vsep-test
              ;; vsep uses line which flattens to space in group, but here no group
              (let* ([docs (list (text "a") (text "b") (text "c"))]
                     [result (pretty 80 (vsep docs))])
                    ;; vsep uses line between elements
                    (assert-equal "a
b
c" result)))
            
            (define-test sep-fits-on-one-line
              (let ([doc (sep (list (text "a") (text "b") (text "c")))])
                   (assert-equal "a b c" (pretty 80 doc))))
            
            (define-test sep-breaks-when-needed
              (let ([doc (sep (list (text "aaaa") (text "bbbb") (text "cccc")))])
                   (assert-equal "aaaa
bbbb
cccc" (pretty 8 doc))))
            
            (define-test empty-list-test
              (assert-equal "" (pretty 80 (hcat '())))
              (assert-equal "" (pretty 80 (hsep '())))
              (assert-equal "" (pretty 80 (vcat '())))))

;;; ============================================================
;;; Bracketing Tests
;;; ============================================================

(test-group bracketing-tests
            (define-test parens-test
              (assert-equal "(x)" (pretty 80 (parens (text "x")))))
            
            (define-test brackets-test
              (assert-equal "[x]" (pretty 80 (brackets (text "x")))))
            
            (define-test braces-test
              (assert-equal "{x}" (pretty 80 (braces (text "x")))))
            
            (define-test angles-test
              (assert-equal "<x>" (pretty 80 (angles (text "x")))))
            
            (define-test quotes-test
              (assert-equal "'x'" (pretty 80 (quotes (text "x")))))
            
            (define-test double-quotes-test
              (assert-equal "\"x\"" (pretty 80 (double-quotes (text "x"))))))

;;; ============================================================
;;; Punctuate Tests
;;; ============================================================

(test-group punctuate-tests
            (define-test punctuate-comma-test
              (let* ([items (list (text "a") (text "b") (text "c"))]
                     [punctuated (punctuate (text ",") items)]
                     [doc (hcat punctuated)])
                    (assert-equal "a,b,c" (pretty 80 doc))))
            
            (define-test punctuate-empty-test
              (assert-equal '() (punctuate (text ",") '())))
            
            (define-test punctuate-single-test
              (let ([result (punctuate (text ",") (list (text "x")))])
                   (assert-equal 1 (length result)))))

;;; ============================================================
;;; Flatten Tests
;;; ============================================================

(test-group flatten-tests
            (define-test flatten-line-to-space
              (let ([doc (flatten (<> (text "a") (<> line (text "b"))))])
                   (assert-equal "a b" (pretty 80 doc))))
            
            (define-test flatten-preserves-text
              (assert-equal "hello" (pretty 80 (flatten (text "hello")))))
            
            (define-test hardline-doesnt-flatten
              ;; hardline stays as newline even in group
              (let ([doc (group (<> (text "a") (<> hardline (text "b"))))])
                   (assert-equal "a
b" (pretty 80 doc)))))

;;; ============================================================
;;; Indentation Tests
;;; ============================================================

(test-group indentation-tests
            (define-test nested-indent-test
              (let ([doc (<> (text "if")
                             (nest 2 (<> line
                                         (<> (text "then")
                                             (nest 2 (<> line (text "x")))))))])
                   (assert-equal "if
  then
    x" (pretty 80 doc))))
            
            (define-test indent-combinator-test
              (let ([doc (indent 4 (text "indented"))])
                   (assert-equal "    indented" (pretty 80 doc)))))

;;; ============================================================
;;; S-expression Tests
;;; ============================================================

(test-group sexp-tests
            (define-test sexp-symbol-test
              (assert-equal "foo" (pretty-sexp 80 'foo)))
            
            (define-test sexp-number-test
              (assert-equal "42" (pretty-sexp 80 42)))
            
            (define-test sexp-empty-list-test
              (assert-equal "()" (pretty-sexp 80 '())))
            
            (define-test sexp-simple-list-test
              (assert-equal "(a b c)" (pretty-sexp 80 '(a b c))))
            
            (define-test sexp-nested-list-test
              (let ([result (pretty-sexp 80 '(define (f x) (+ x 1)))])
                   (assert-true (string? result))
                   ;; Should contain the elements
                   (assert-true (> (string-length result) 0)))))

;;; ============================================================
;;; Width Tests
;;; ============================================================

(test-group width-tests
            (define-test narrow-width-forces-breaks
              (let ([doc (sep (list (text "hello") (text "world")))])
                   ;; "hello world" is 11 chars, width 10 should break
                   (assert-equal "hello
world" (pretty 10 doc))))
            
            (define-test wide-width-keeps-flat
              (let ([doc (sep (list (text "hello") (text "world")))])
                   (assert-equal "hello world" (pretty 80 doc)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "
")
(display "==============================================================
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All pretty printing tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
