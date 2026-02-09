(load "core/testing/test-framework.ss")
(load "lattice/pipeline/curriculum.ss")

(doc 'module 'test-curriculum)
(doc 'description "Tests for RL curriculum extraction from lattice source files")

;; ====
;; Helpers
;; ====

;; string-contains : check if a string contains a substring
(define (string-contains str substr)
  (let ([slen (string-length str)]
        [plen (string-length substr)])
    (and (>= slen plen)
         (let loop ([i 0])
           (cond
             [(> (+ i plen) slen) #f]
             [(string=? (substring str i (+ i plen)) substr) #t]
             [else (loop (+ i 1))])))))

;; ====
;; Fixtures — S-expressions mimicking lattice/data/sort.ss structure
;; ====

(define *sort-like-sexps*
  '((load "core/base/prelude.ss")

    (doc 'module 'sort)
    (doc 'description "Sorting algorithms")
    (doc 'layer 'lattice)
    (doc 'tier 0)
    (doc 'purity 'total)

    (doc 'section 'merge-sort)

    (define (merge xs ys cmp)
      (doc 'type (-> (List a) (List a) (-> a a Boolean) (List a)))
      (doc 'description "Merge two sorted lists")
      (doc 'note "Stable: equal from left first")
      (cond
        [(null? xs) ys]
        [(null? ys) xs]
        [(not (cmp (car ys) (car xs)))
         (cons (car xs) (merge (cdr xs) ys cmp))]
        [else
         (cons (car ys) (merge xs (cdr ys) cmp))]))

    (define (split-at lst n)
      (doc 'type (-> (List a) Nat (Pair (List a) (List a))))
      (doc 'description "Split list at position n")
      (if (or (= n 0) (null? lst))
          (cons '() lst)
          (let ([rest (split-at (cdr lst) (- n 1))])
            (cons (cons (car lst) (car rest))
                  (cdr rest)))))

    (define (merge-sort-by cmp lst)
      (doc 'type (-> (-> a a Boolean) (List a) (List a)))
      (doc 'description "Sort list using merge sort with custom comparator")
      (doc 'note "Stable: preserves order of equal elements")
      (doc 'complexity "O(n log n)")
      (let ([len (length lst)])
        (if (<= len 1)
            lst
            (let* ([mid (quotient len 2)]
                   [split (split-at lst mid)])
              (merge (merge-sort-by cmp (car split))
                     (merge-sort-by cmp (cdr split))
                     cmp)))))

    (define (merge-sort lst)
      (doc 'type (-> (List a) (List a)))
      (doc 'description "Sort ascending via merge sort")
      (merge-sort-by < lst))

    (define sort merge-sort)
    (doc sort 'description "Sort ascending (alias)")

    (doc 'section 'utilities)

    (define (sorted? lst)
      (doc 'type (-> (List a) Boolean))
      (doc 'description "Check if list is sorted ascending")
      (or (null? lst)
          (null? (cdr lst))
          (and (<= (car lst) (cadr lst))
               (sorted? (cdr lst)))))))

;; ====
;; task-entry type
;; ====

(test-group task-entry-type

  (define-test "make-task-entry creates vector"
    (let ([te (make-task-entry "data/sort/merge" 'data 'sort 0 0 'merge
                               "(-> (List a) (List a) (-> a a Boolean) (List a))"
                               "Merge two sorted lists"
                               '("Stable: equal from left first")
                               'merge-sort '() '()
                               "(define (merge xs ys cmp) ...)")])
      (assert-true (task-entry? te))
      (assert-equal "data/sort/merge" (task-entry-id te))
      (assert-equal 'data (task-entry-skill te))
      (assert-equal 'sort (task-entry-module te))
      (assert-equal 0 (task-entry-tier te))
      (assert-equal 0 (task-entry-position te))
      (assert-equal 'merge (task-entry-name te))
      (assert-equal "Merge two sorted lists" (task-entry-desc te))
      (assert-equal '("Stable: equal from left first") (task-entry-notes te))
      (assert-equal 'merge-sort (task-entry-section te))))

  (define-test "task-entry? rejects non-entries"
    (assert-false (task-entry? '(not a task)))
    (assert-false (task-entry? 42))
    (assert-false (task-entry? #f))))

;; ====
;; Doc form extraction
;; ====

(test-group doc-extraction

  (define-test "doc-form? recognizes doc forms"
    (assert-true (doc-form? '(doc 'type (-> a b))))
    (assert-true (doc-form? '(doc 'section 'merge-sort)))
    (assert-false (doc-form? '(define x 1)))
    (assert-false (doc-form? 42)))

  (define-test "internal-doc-form? distinguishes internal from external"
    (assert-true (internal-doc-form? '(doc 'type (-> a b))))
    (assert-true (internal-doc-form? '(doc 'description "hello")))
    ;; External: (doc sort 'description "...") — second elem is symbol, not quoted
    (assert-false (internal-doc-form? '(doc sort 'description "hello"))))

  (define-test "extract-doc-annotations gets type and description"
    (let ([body '((doc 'type (-> a b))
                  (doc 'description "Add two numbers")
                  (doc 'note "Pure function")
                  (+ a b))])
      (let-values ([(type desc notes) (extract-doc-annotations body)])
        (assert-equal "(-> a b)" type)
        (assert-equal "Add two numbers" desc)
        (assert-equal '("Pure function") notes))))

  (define-test "extract-doc-annotations handles multiple notes"
    (let ([body '((doc 'type (-> a a))
                  (doc 'note "Note 1")
                  (doc 'complexity "O(n)")
                  x)])
      (let-values ([(type desc notes) (extract-doc-annotations body)])
        (assert-equal "(-> a a)" type)
        (assert-equal "" desc)
        (assert-equal '("Note 1" "O(n)") notes))))

  (define-test "strip-doc-forms removes doc forms from body"
    (let ([body '((doc 'type (-> a b))
                  (doc 'description "desc")
                  (+ a b))])
      (assert-equal '((+ a b)) (strip-doc-forms body)))))

;; ====
;; Define recognition
;; ====

(test-group define-recognition

  (define-test "define-form? recognizes defines"
    (assert-true (define-form? '(define x 1)))
    (assert-true (define-form? '(define (f x) x)))
    (assert-false (define-form? '(lambda (x) x)))
    (assert-false (define-form? '(define))))

  (define-test "alias-define? detects aliases"
    (assert-true (alias-define? '(define sort merge-sort)))
    (assert-false (alias-define? '(define (f x) x)))
    (assert-false (alias-define? '(define x 42))))

  (define-test "function-define? detects function defines"
    (assert-true (function-define? '(define (f x) x)))
    (assert-false (function-define? '(define x 1)))
    (assert-false (function-define? '(define sort merge-sort))))

  (define-test "define-name extracts name"
    (assert-equal 'f (define-name '(define (f x) x)))
    (assert-equal 'x (define-name '(define x 1)))
    (assert-equal 'sort (define-name '(define sort merge-sort)))))

;; ====
;; Section tracking
;; ====

(test-group section-tracking

  (define-test "section-doc? recognizes section markers"
    (assert-true (section-doc? '(doc 'section 'merge-sort)))
    (assert-false (section-doc? '(doc 'type (-> a b))))
    (assert-false (section-doc? '(doc 'module 'sort))))

  (define-test "section-name extracts name"
    (assert-equal 'merge-sort (section-name '(doc 'section 'merge-sort)))
    (assert-equal 'utilities (section-name '(doc 'section 'utilities)))))

;; ====
;; Code string conversion
;; ====

(test-group code-string

  (define-test "sexp->code-string handles atoms"
    (assert-equal "foo" (sexp->code-string 'foo))
    (assert-equal "42" (sexp->code-string 42))
    (assert-equal "#t" (sexp->code-string #t))
    (assert-equal "\"hello\"" (sexp->code-string "hello")))

  (define-test "sexp->code-string handles lists"
    (assert-equal "(+ 1 2)" (sexp->code-string '(+ 1 2)))
    (assert-equal "(define (f x) x)" (sexp->code-string '(define (f x) x))))

  (define-test "sexp->code-string handles quotes"
    (assert-equal "'foo" (sexp->code-string '(quote foo)))
    (assert-equal "'(a b)" (sexp->code-string '(quote (a b)))))

  (define-test "sexp->code-string handles empty list"
    (assert-equal "()" (sexp->code-string '())))

  (define-test "strip-define-docs removes docs from function define"
    (let ([sexp '(define (f x)
                   (doc 'type (-> a b))
                   (doc 'description "desc")
                   (+ x 1))])
      (assert-equal '(define (f x) (+ x 1))
                    (strip-define-docs sexp))))

  (define-test "strip-define-docs preserves alias defines"
    (assert-equal '(define sort merge-sort)
                  (strip-define-docs '(define sort merge-sort)))))

;; ====
;; External doc collection
;; ====

(test-group external-docs

  (define-test "collect-external-docs gets description for alias"
    (let ([sexps '((doc sort 'description "Sort ascending (alias)")
                   (doc 'section 'next))])
      (let-values ([(type desc notes) (collect-external-docs sexps 'sort)])
        (assert-equal "Sort ascending (alias)" desc))))

  (define-test "collect-external-docs stops at non-matching form"
    (let ([sexps '((doc sort 'description "Sort ascending")
                   (define (other-fn x) x))])
      (let-values ([(type desc notes) (collect-external-docs sexps 'sort)])
        (assert-equal "Sort ascending" desc)))))

;; ====
;; Full extraction on sort-like fixture
;; ====

(test-group full-extraction

  (define-test "extract-defines produces correct number of entries"
    (let ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)])
      ;; merge, split-at, merge-sort-by, merge-sort, sort, sorted? = 6
      (assert-equal 6 (length entries))))

  (define-test "first entry is merge with no available functions"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [first (car entries)])
      (assert-equal 'merge (task-entry-name first))
      (assert-equal "data/sort/merge" (task-entry-id first))
      (assert-equal 'data (task-entry-skill first))
      (assert-equal 'sort (task-entry-module first))
      (assert-equal 0 (task-entry-tier first))
      (assert-equal 0 (task-entry-position first))
      (assert-equal '() (task-entry-available first))
      (assert-equal 'merge-sort (task-entry-section first))))

  (define-test "merge has correct type annotation"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [first (car entries)])
      (assert-equal "(-> (List a) (List a) (-> a a Boolean) (List a))"
                    (task-entry-type first))))

  (define-test "merge has note extracted"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [first (car entries)])
      (assert-equal '("Stable: equal from left first")
                    (task-entry-notes first))))

  (define-test "merge-sort-by has merge and split-at available"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [msb (list-ref entries 2)])  ; merge, split-at, merge-sort-by
      (assert-equal 'merge-sort-by (task-entry-name msb))
      ;; Available is in reverse accumulation order (most recent first)
      (assert-true (pair? (memq 'merge (task-entry-available msb))))
      (assert-true (pair? (memq 'split-at (task-entry-available msb))))))

  (define-test "merge-sort-by has multiple notes"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [msb (list-ref entries 2)])
      ;; note + complexity
      (assert-equal 2 (length (task-entry-notes msb)))))

  (define-test "alias define (sort = merge-sort) is included"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [alias (list-ref entries 4)])  ; merge, split-at, merge-sort-by, merge-sort, sort
      (assert-equal 'sort (task-entry-name alias))
      (assert-true (alias-define? '(define sort merge-sort)))))

  (define-test "alias define has external doc description"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [alias (list-ref entries 4)])
      (assert-equal "Sort ascending (alias)" (task-entry-desc alias))))

  (define-test "sorted? is in utilities section"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [last-entry (list-ref entries 5)])
      (assert-equal 'sorted? (task-entry-name last-entry))
      (assert-equal 'utilities (task-entry-section last-entry))))

  (define-test "answer fields contain no doc forms"
    (let ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)])
      (for-each
        (lambda (te)
          (let ([answer (task-entry-answer te)])
            (assert-false (string-contains answer "(doc '"))))
        entries)))

  (define-test "deps extracted from load forms"
    (let* ([entries (extract-defines *sort-like-sexps* 'data 'sort 0)]
           [first (car entries)])
      (assert-true (pair? (memq 'core/base/prelude.ss (task-entry-deps first)))))))

;; ====
;; case-lambda handling
;; ====

(define *case-lambda-sexps*
  '((define make-point
      (case-lambda
        ((x y)
         (doc 'type '(-> Num Num Point))
         (doc 'description "Make a 2D point")
         (list x y))
        ((x y z)
         (doc 'type '(-> Num Num Num Point))
         (doc 'description "Make a 3D point")
         (list x y z))))))

(test-group case-lambda-extraction

  (define-test "case-lambda type extracted from first clause"
    (let* ([entries (extract-defines *case-lambda-sexps* 'geom 'point 0)]
           [entry (car entries)])
      (assert-equal 'make-point (task-entry-name entry))
      (assert-equal "(-> Num Num Point)" (task-entry-type entry))))

  (define-test "case-lambda description extracted from first clause"
    (let* ([entries (extract-defines *case-lambda-sexps* 'geom 'point 0)]
           [entry (car entries)])
      (assert-equal "Make a 2D point" (task-entry-desc entry))))

  (define-test "case-lambda answer has no doc forms"
    (let* ([entries (extract-defines *case-lambda-sexps* 'geom 'point 0)]
           [entry (car entries)])
      (assert-false (string-contains (task-entry-answer entry) "(doc '")))))

;; ====
;; Difficulty scoring
;; ====

(test-group difficulty

  (define-test "compute-difficulty basic formula"
    (assert-equal 0 (compute-difficulty 0 0 0))
    (assert-equal 10203 (compute-difficulty 1 2 3))
    (assert-equal 20500 (compute-difficulty 2 5 0))))

;; ====
;; Output formatting
;; ====

(test-group output-formatting

  (define-test "task-entry->sexp produces valid structure"
    (let* ([te (make-task-entry "data/sort/merge" 'data 'sort 0 0 'merge
                                "(-> a b)" "Merge lists" '("stable")
                                'merge-sort '(prelude) '()
                                "(define (merge xs ys) ...)")]
           [sexp (task-entry->sexp te)])
      (assert-equal 'task-entry (car sexp))
      (assert-equal "data/sort/merge" (cadr (assq 'id (cdr sexp))))
      (assert-equal 'data (cadr (assq 'skill (cdr sexp))))
      (assert-equal 'merge (cadr (assq 'name (cdr sexp))))
      (assert-equal "(define (merge xs ys) ...)" (cadr (assq 'answer (cdr sexp))))))

  (define-test "task-entry->sexp/difficulty sets difficulty"
    (let* ([te (make-task-entry "x" 'a 'b 0 0 'f "" "" '() #f '() '() "")]
           [sexp (task-entry->sexp/difficulty te 42)])
      (assert-equal 42 (cadr (assq 'difficulty (cdr sexp)))))))

(run-all-tests)
