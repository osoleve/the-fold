;;; boundary/ui/test-ansi.ss — Tests for ANSI Terminal Output
;;;
;;; Run with: scheme --script boundary/ui/test-ansi.ss

(load "core/testing/test-framework.ss")
(load "boundary/ui/ansi.ss")

;;; ====
;;; Style Tests
;;; ====

(define-test "style-default has no attributes"
  (let ([s style-default])
    (assert-false (style%-fg s))
    (assert-false (style%-bg s))
    (assert-false (style%-bold s))
    (assert-false (style%-underline s))))

(define-test "style-fg creates foreground-only style"
  (let ([s (style-fg color-red)])
    (assert-equal color-red (style%-fg s))
    (assert-false (style%-bg s))
    (assert-false (style%-bold s))))

(define-test "style-bold creates bold-only style"
  (let ([s style-bold])
    (assert-true (style%-bold s))
    (assert-false (style%-fg s))))

(define-test "style-merge combines styles correctly"
  (let* ([s1 (style-fg color-red)]
         [s2 style-bold]
         [merged (style-merge s1 s2)])
    (assert-equal (style%-fg s1) (style%-fg merged))
    (assert-true (style%-bold merged))))

(define-test "style-merge inner overrides outer"
  (let* ([s1 (style-fg color-red)]
         [s2 (style-fg color-green)]
         [merged (style-merge s1 s2)])
    (assert-equal color-green (style%-fg merged))))

;;; ====
;;; ANSI Code Generation Tests
;;; ====

(define-test "style->ansi generates bold code"
  (let ([code (style->ansi style-bold)])
    (assert-true (string-contains? code "1"))))

(define-test "style->ansi generates underline code"
  (let ([code (style->ansi style-underline)])
    (assert-true (string-contains? code "4"))))

(define-test "style->ansi generates empty string for default style"
  (assert-equal "" (style->ansi style-default)))

(define-test "style->ansi generates foreground color code"
  (let ([code (style->ansi (style-fg color-red))])
    (assert-true (string-contains? code "38;5;1"))))

;;; Helper for string-contains?
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
    (if (> sub-len str-len)
        #f
        (let loop ([i 0])
          (cond
           [(> (+ i sub-len) str-len) #f]
           [(string=? (substring str i (+ i sub-len)) substr) #t]
           [else (loop (+ i 1))])))))

;;; ====
;;; Document Annotation Tests
;;; ====

(define-test "doc-annotate creates annotated document"
  (let ([doc (doc-annotate style-bold (text "test"))])
    (assert-true (doc-annotate? doc))
    (assert-equal style-bold (doc-annotate-style doc))))

(define-test "bold creates annotated document"
  (assert-true (doc-annotate? (bold (text "test")))))

(define-test "colored creates annotated document"
  (assert-true (doc-annotate? (colored color-red (text "test")))))

;;; ====
;;; Rendering Tests
;;; ====

(define-test "ansi-render produces string with escape codes"
  (let ([result (ansi-render (bold (text "test")))])
    (assert-true (> (string-length result) 4))))  ; More than just "test"

(define-test "ansi-render handles nested styles"
  (let ([result (ansi-render (bold (colored color-red (text "test"))))])
    (assert-true (string-contains? result "test"))))

(define-test "ansi-render-plain strips annotations"
  (let ([result (ansi-render-plain (bold (colored color-red (text "test"))))])
    (assert-equal "test" result)))

(define-test "strip-annotations removes all annotations"
  (let* ([doc (bold (colored color-red (text "test")))]
         [stripped (strip-annotations doc)])
    (assert-false (doc-annotate? stripped))))

;;; ====
;;; Semantic Helper Tests
;;; ====

(define-test "error-text creates styled document"
  (let ([doc (error-text "error message")])
    (assert-true (doc-annotate? doc))))

(define-test "keyword-text creates styled document"
  (let ([doc (keyword-text "define")])
    (assert-true (doc-annotate? doc))))

;;; ====
;;; Layout with Annotations Tests
;;; ====

(define-test "ansi-render preserves layout structure"
  (let* ([doc (group (vsep (list (bold (text "line1"))
                                 (colored color-green (text "line2")))))]
         [result (ansi-render-width 10 doc)])
    (assert-true (string-contains? result "line1"))
    (assert-true (string-contains? result "line2"))))

(define-test "ansi-render handles concat with mixed annotations"
  (let* ([doc (<> (bold (text "bold"))
                  (<> (text " ")
                      (underline (text "underline"))))]
         [result (ansi-render doc)])
    (assert-true (string-contains? result "bold"))
    (assert-true (string-contains? result "underline"))))

;;; ====
;;; Run Tests
;;; ====

(display "\n=== ANSI Terminal Output Tests ===\n\n")
(run-all-tests)
