(load "core/testing/test-framework.ss")

;; meta-printf is daemon prelude-only; stub it for standalone testing
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf (lambda args (void)))))

(load "lattice/meta/xref.ss")

;;; ========================================
;;; Test: extract-symbols-from-sexp
;;; ========================================

(test-group "extract-symbols-from-sexp"

  ;; --- Basic extraction ---

  (define-test "bare symbol returns itself"
    (assert-equal '(x) (extract-symbols-from-sexp 'x)))

  (define-test "number returns empty"
    (assert-equal '() (extract-symbols-from-sexp 42)))

  (define-test "string returns empty"
    (assert-equal '() (extract-symbols-from-sexp "hello")))

  (define-test "boolean returns empty"
    (assert-equal '() (extract-symbols-from-sexp #t)))

  (define-test "simple application extracts all symbols"
    (let ([syms (extract-symbols-from-sexp '(+ x y))])
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'x syms)))
      (assert-true (pair? (memq 'y syms)))))

  (define-test "nested application extracts all symbols"
    (let ([syms (extract-symbols-from-sexp '(+ (* a b) (- c d)))])
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq '* syms)))
      (assert-true (pair? (memq '- syms)))
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'b syms)))
      (assert-true (pair? (memq 'c syms)))
      (assert-true (pair? (memq 'd syms)))))

  ;; --- Quoting ---

  (define-test "quoted symbol is skipped"
    (assert-equal '() (extract-symbols-from-sexp '(quote foo))))

  (define-test "quoted list is skipped"
    (assert-equal '() (extract-symbols-from-sexp '(quote (a b c)))))

  (define-test "quasiquote is skipped entirely"
    ;; xref.ss treats quasiquote the same as quote — skips everything
    (assert-equal '() (extract-symbols-from-sexp '(quasiquote (a b c)))))

  ;; --- define ---

  (define-test "define variable: skips name, extracts value"
    (let ([syms (extract-symbols-from-sexp '(define x (+ a b)))])
      ;; x is the name being defined — skipped
      (assert-true (not (memq 'x syms)))
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'b syms)))))

  (define-test "define function: skips name and params, extracts body"
    (let ([syms (extract-symbols-from-sexp '(define (f x y) (+ x y)))])
      ;; (f x y) is the name-part — entire thing skipped by cddr
      ;; Body (+ x y) is extracted
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'x syms)))
      (assert-true (pair? (memq 'y syms)))))

  (define-test "define with multiple body forms"
    (let ([syms (extract-symbols-from-sexp '(define (f x) (display x) (+ x 1)))])
      (assert-true (pair? (memq 'display syms)))
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'x syms)))))

  (define-test "define with too few parts returns empty"
    (assert-equal '() (extract-symbols-from-sexp '(define))))

  ;; --- let / let* / letrec ---

  (define-test "let: skips binding names, extracts init values and body"
    ;; Use a body that doesn't reference the bound names, so we can verify
    ;; the binding-position names are truly skipped
    (let ([syms (extract-symbols-from-sexp '(let ([x (foo a)] [y (bar b)]) (baz)))])
      ;; Binding names x, y are NOT extracted (only skipped from binding position;
      ;; they don't appear in body either)
      (assert-true (not (memq 'x syms)))
      (assert-true (not (memq 'y syms)))
      ;; Init values extracted
      (assert-true (pair? (memq 'foo syms)))
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'bar syms)))
      (assert-true (pair? (memq 'b syms)))
      ;; Body extracted
      (assert-true (pair? (memq 'baz syms)))))

  (define-test "let: body references to bound names ARE extracted"
    ;; The extractor doesn't do full scope analysis — body references to bound
    ;; variables appear in output. This is correct for xref purposes.
    (let ([syms (extract-symbols-from-sexp '(let ([x (foo)]) (use x)))])
      (assert-true (pair? (memq 'foo syms)))
      (assert-true (pair? (memq 'use syms)))
      ;; x appears from body, even though it was a binding name
      (assert-true (pair? (memq 'x syms)))))

  (define-test "let*: skips binding names, extracts init values and body"
    (let ([syms (extract-symbols-from-sexp '(let* ([x (compute a)]) (use)))])
      ;; x not in body, so it shouldn't appear
      (assert-true (not (memq 'x syms)))
      (assert-true (pair? (memq 'compute syms)))
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'use syms)))))

  (define-test "letrec: skips binding names in binding position"
    ;; In letrec, binding names are skipped at the binding level.
    ;; But init values (lambda bodies) and the letrec body may reference
    ;; the same symbols — those references ARE extracted.
    (let ([syms (extract-symbols-from-sexp '(letrec ([f (lambda (n) (g n))]
                                                      [g (lambda (n) (h n))])
                                               (begin)))])
      ;; f and g are skipped as binding names, but g appears inside f's
      ;; lambda body and f does NOT appear in any body here.
      ;; f's init: (lambda (n) (g n)) → extracts g, n from body
      ;; g's init: (lambda (n) (h n)) → extracts h, n from body
      ;; letrec body: (begin) → extracts begin
      (assert-true (pair? (memq 'g syms)))  ;; from f's lambda body
      (assert-true (pair? (memq 'h syms)))  ;; from g's lambda body
      (assert-true (pair? (memq 'n syms)))
      ;; f does not appear in any init value or body
      (assert-true (not (memq 'f syms)))))

  (define-test "let with empty bindings extracts body"
    (let ([syms (extract-symbols-from-sexp '(let () (vec-add a b)))])
      (assert-true (pair? (memq 'vec-add syms)))
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'b syms)))))

  (define-test "named let: skips loop name, extracts init values and body"
    (let ([syms (extract-symbols-from-sexp
                  '(let loop ([i (start)] [acc (init)])
                     (if (done? i)
                         acc
                         (loop (next i) (add acc i)))))])
      ;; loop name skipped (does not appear in init values or body as plain symbol
      ;; — it only appears as car of an application which IS extracted)
      ;; Actually: body contains (loop ...) which recurses into all parts,
      ;; so loop IS extracted from the body application. Let's verify:
      ;; The named-let handler extracts from body, and body is
      ;; (if (done? i) acc (loop (next i) (add acc i)))
      ;; Default case recurses into all parts → loop is extracted.
      ;; So loop appears in output despite being the named-let name.
      ;; This is a known limitation of xref.ss's extract-symbols-from-sexp.

      ;; Binding names i, acc appear in body references (not skipped there)
      (assert-true (pair? (memq 'i syms)))
      (assert-true (pair? (memq 'acc syms)))
      ;; Init values extracted
      (assert-true (pair? (memq 'start syms)))
      (assert-true (pair? (memq 'init syms)))
      ;; Body extracted
      (assert-true (pair? (memq 'done? syms)))
      (assert-true (pair? (memq 'next syms)))
      (assert-true (pair? (memq 'add syms)))
      ;; if is just a normal symbol in body
      (assert-true (pair? (memq 'if syms)))))

  (define-test "named let with empty bindings"
    ;; (let loop () body...) — loop is the name, () is bindings
    ;; Body contains (process) and (loop) — both get extracted via recursion
    (let ([syms (extract-symbols-from-sexp '(let loop () (process)))])
      ;; loop doesn't appear in body (only process does)
      (assert-true (not (memq 'loop syms)))
      (assert-true (pair? (memq 'process syms)))))

  (define-test "nested let forms: binding names skipped, body refs extracted"
    ;; x and y are skipped in binding positions, but they appear in bodies
    ;; (inner-fn x), (combine x y) — so they're extracted from there.
    (let ([syms (extract-symbols-from-sexp
                  '(let ([x (outer-fn)])
                     (let ([y (inner-fn)])
                       (combine))))])
      ;; x doesn't appear in any body (outer let body is another let,
      ;; inner let init is (inner-fn) with no args, body is (combine))
      (assert-true (not (memq 'x syms)))
      (assert-true (not (memq 'y syms)))
      ;; Functions extracted
      (assert-true (pair? (memq 'outer-fn syms)))
      (assert-true (pair? (memq 'inner-fn syms)))
      (assert-true (pair? (memq 'combine syms)))))

  ;; --- lambda ---

  (define-test "lambda: skips parameter names, extracts body"
    (let ([syms (extract-symbols-from-sexp '(lambda (x y) (+ x y)))])
      ;; Body symbols extracted (x, y appear in body as references)
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'x syms)))
      (assert-true (pair? (memq 'y syms)))))

  (define-test "lambda with no body returns empty"
    (assert-equal '() (extract-symbols-from-sexp '(lambda (x)))))

  (define-test "lambda with multiple body forms"
    (let ([syms (extract-symbols-from-sexp '(lambda (x) (validate x) (transform x)))])
      (assert-true (pair? (memq 'validate syms)))
      (assert-true (pair? (memq 'transform syms)))
      (assert-true (pair? (memq 'x syms)))))

  ;; --- Default recursion ---

  (define-test "if form recurses into all subexpressions"
    (let ([syms (extract-symbols-from-sexp '(if (pred? x) (then-fn x) (else-fn x)))])
      (assert-true (pair? (memq 'if syms)))
      (assert-true (pair? (memq 'pred? syms)))
      (assert-true (pair? (memq 'then-fn syms)))
      (assert-true (pair? (memq 'else-fn syms)))
      (assert-true (pair? (memq 'x syms)))))

  (define-test "cond form recurses into clauses"
    (let ([syms (extract-symbols-from-sexp '(cond [(null? x) (base)] [else (step x)]))])
      (assert-true (pair? (memq 'cond syms)))
      (assert-true (pair? (memq 'null? syms)))
      (assert-true (pair? (memq 'base syms)))
      (assert-true (pair? (memq 'step syms)))))

  (define-test "begin form recurses into all subexpressions"
    (let ([syms (extract-symbols-from-sexp '(begin (setup) (run) (cleanup)))])
      (assert-true (pair? (memq 'begin syms)))
      (assert-true (pair? (memq 'setup syms)))
      (assert-true (pair? (memq 'run syms)))
      (assert-true (pair? (memq 'cleanup syms)))))

  ;; --- Edge cases ---

  (define-test "empty list returns empty"
    (assert-equal '() (extract-symbols-from-sexp '())))

  (define-test "deeply nested structure"
    (let ([syms (extract-symbols-from-sexp '(a (b (c (d (e))))))])
      (assert-true (pair? (memq 'a syms)))
      (assert-true (pair? (memq 'b syms)))
      (assert-true (pair? (memq 'c syms)))
      (assert-true (pair? (memq 'd syms)))
      (assert-true (pair? (memq 'e syms)))))
)

;;; ========================================
;;; Test: extract-xref-from-toplevel
;;; ========================================

(test-group "extract-xref-from-toplevel"

  (define-test "simple define function returns (name . refs)"
    (let ([result (extract-xref-from-toplevel '(define (f x) (+ x 1)))])
      (assert-true (pair? result))
      (assert-equal 'f (car result))
      ;; refs should include +, x
      (assert-true (pair? (memq '+ (cdr result))))
      (assert-true (pair? (memq 'x (cdr result))))))

  (define-test "define variable returns (name . refs)"
    (let ([result (extract-xref-from-toplevel '(define x (compute a b)))])
      (assert-true (pair? result))
      (assert-equal 'x (car result))
      (assert-true (pair? (memq 'compute (cdr result))))
      (assert-true (pair? (memq 'a (cdr result))))
      (assert-true (pair? (memq 'b (cdr result))))))

  (define-test "define with complex body extracts all refs"
    (let ([result (extract-xref-from-toplevel
                    '(define (process lst)
                       (let ([filtered (filter odd? lst)])
                         (map transform filtered))))])
      (assert-true (pair? result))
      (assert-equal 'process (car result))
      (let ([refs (cdr result)])
        (assert-true (pair? (memq 'filter refs)))
        (assert-true (pair? (memq 'odd? refs)))
        (assert-true (pair? (memq 'map refs)))
        (assert-true (pair? (memq 'transform refs))))))

  (define-test "refs are deduplicated"
    (let ([result (extract-xref-from-toplevel
                    '(define (f x) (+ x (+ x 1))))])
      (assert-true (pair? result))
      ;; + appears twice in source but should be deduplicated in refs
      (let ([plus-count (length (filter (lambda (s) (eq? s '+)) (cdr result)))])
        (assert-equal 1 plus-count))))

  (define-test "define-syntax returns #f"
    (assert-false (extract-xref-from-toplevel '(define-syntax my-mac (syntax-rules () [(_ x) x])))))

  (define-test "non-define form returns #f"
    (assert-false (extract-xref-from-toplevel '(+ 1 2))))

  (define-test "non-pair returns #f"
    (assert-false (extract-xref-from-toplevel 42)))

  (define-test "define with no body returns #f"
    ;; (define) has no cadr
    (assert-false (extract-xref-from-toplevel '(define))))
)

;;; ========================================
;;; Test: get-define-name
;;; ========================================

(test-group "get-define-name"

  (define-test "variable define returns name"
    (assert-equal 'x (get-define-name '(define x 10))))

  (define-test "function define returns name"
    (assert-equal 'f (get-define-name '(define (f x y) (+ x y)))))

  (define-test "define with non-symbol name-part returns #f"
    (assert-false (get-define-name '(define 42 something))))

  (define-test "define with no cdr returns #f"
    (assert-false (get-define-name '(define))))
)

;;; ========================================
;;; Test: remove-duplicates-sym
;;; ========================================

(test-group "remove-duplicates-sym"

  (define-test "empty list"
    (assert-equal '() (remove-duplicates-sym '())))

  (define-test "no duplicates preserves order"
    (assert-equal '(a b c) (remove-duplicates-sym '(a b c))))

  (define-test "removes duplicates preserving first occurrence"
    (assert-equal '(a b c) (remove-duplicates-sym '(a b c a b))))

  (define-test "all same symbol"
    (assert-equal '(x) (remove-duplicates-sym '(x x x x))))

  (define-test "single element"
    (assert-equal '(a) (remove-duplicates-sym '(a))))

  (define-test "adjacent duplicates"
    (assert-equal '(a b c) (remove-duplicates-sym '(a a b b c c))))

  (define-test "interleaved duplicates"
    (assert-equal '(a b c) (remove-duplicates-sym '(a b c b a c))))
)

;;; ========================================
;;; Test: populate-xref-cache! and query API
;;; ========================================

(test-group "populate-xref-cache"

  ;; Reset state before tests
  (define-test "populate builds correct caller index"
    (let ()
      (populate-xref-cache!
        '((f . (g h))
          (g . (h))
          (h . ())))
      ;; h is called by both f and g
      (let ([h-callers (xref-callers 'h)])
        (assert-true (pair? (memq 'f h-callers)))
        (assert-true (pair? (memq 'g h-callers))))
      ;; g is called by f
      (let ([g-callers (xref-callers 'g)])
        (assert-true (pair? (memq 'f g-callers))))
      ;; f has no callers
      (assert-equal '() (xref-callers 'f))))

  (define-test "populate builds correct callee index"
    (let ()
      (populate-xref-cache!
        '((f . (g h))
          (g . (h))
          (h . ())))
      ;; f calls g and h
      (let ([f-callees (xref-callees 'f)])
        (assert-true (pair? (memq 'g f-callees)))
        (assert-true (pair? (memq 'h f-callees))))
      ;; g calls h
      (let ([g-callees (xref-callees 'g)])
        (assert-true (pair? (memq 'h g-callees))))
      ;; h calls nothing
      (assert-equal '() (xref-callees 'h))))

  (define-test "unknown symbol returns empty list"
    (let ()
      (populate-xref-cache! '((f . (g)) (g . ())))
      (assert-equal '() (xref-callers 'nonexistent))
      (assert-equal '() (xref-callees 'nonexistent))))

  (define-test "self-references are filtered out"
    (let ()
      ;; f references itself (recursive) — should be excluded from callees
      (populate-xref-cache! '((f . (f g)) (g . ())))
      (let ([f-callees (xref-callees 'f)])
        (assert-true (not (memq 'f f-callees)))
        (assert-true (pair? (memq 'g f-callees))))))

  (define-test "references to unknown definitions are filtered out"
    (let ()
      ;; f references 'unknown which isn't in the definitions list
      (populate-xref-cache! '((f . (g unknown)) (g . ())))
      (let ([f-callees (xref-callees 'f)])
        (assert-true (pair? (memq 'g f-callees)))
        (assert-true (not (memq 'unknown f-callees))))))

  (define-test "empty xref list produces empty indices"
    (let ()
      (populate-xref-cache! '())
      (assert-equal '() (xref-callers 'anything))
      (assert-equal '() (xref-callees 'anything))))
)

;;; ========================================
;;; Test: transitive queries
;;; ========================================

(test-group "transitive-queries"

  (define-test "transitive callers follows chain"
    (let ()
      ;; a -> b -> c (a calls b, b calls c)
      (populate-xref-cache!
        '((a . (b))
          (b . (c))
          (c . ())))
      ;; Transitive callers of c: b (direct), a (transitive)
      (let ([tc (xref-callers-transitive 'c)])
        (assert-true (pair? (memq 'b tc)))
        (assert-true (pair? (memq 'a tc))))))

  (define-test "transitive callees follows chain"
    (let ()
      (populate-xref-cache!
        '((a . (b))
          (b . (c))
          (c . ())))
      ;; Transitive callees of a: b (direct), c (transitive)
      (let ([tc (xref-callees-transitive 'a)])
        (assert-true (pair? (memq 'b tc)))
        (assert-true (pair? (memq 'c tc))))))

  (define-test "transitive callers handles diamond"
    (let ()
      ;; a -> c, b -> c, a -> b (diamond: a calls both b and c, b calls c)
      (populate-xref-cache!
        '((a . (b c))
          (b . (c))
          (c . ())))
      (let ([tc (xref-callers-transitive 'c)])
        (assert-true (pair? (memq 'a tc)))
        (assert-true (pair? (memq 'b tc)))
        ;; Each appears exactly once
        (assert-equal 2 (length tc)))))

  (define-test "transitive callees handles cycle"
    (let ()
      ;; a -> b -> a (mutual recursion)
      (populate-xref-cache!
        '((a . (b))
          (b . (a))))
      ;; Should terminate despite cycle
      (let ([tc (xref-callees-transitive 'a)])
        (assert-true (pair? (memq 'b tc)))
        ;; a should appear since b calls a (and a is a known def)
        ;; but self-ref is filtered by populate, so b's callees include a
        ;; Actually: populate filters self-refs only (caller != callee).
        ;; b calling a is NOT a self-ref for b, so a appears in b's callees.
        (assert-true (pair? (memq 'a tc))))))

  (define-test "transitive callers of unknown returns empty"
    (let ()
      (populate-xref-cache! '((a . (b)) (b . ())))
      (assert-equal '() (xref-callers-transitive 'nonexistent))))

  (define-test "transitive callees of leaf returns empty"
    (let ()
      (populate-xref-cache! '((a . (b)) (b . ())))
      (assert-equal '() (xref-callees-transitive 'b))))
)

;;; ========================================
;;; Test: xref-most-called
;;; ========================================

(test-group "xref-most-called"

  (define-test "returns functions sorted by caller count"
    (let ()
      ;; h is called by both f and g; g is called by f only
      (populate-xref-cache!
        '((f . (g h))
          (g . (h))
          (h . ())))
      (let ([top (xref-most-called 10)])
        ;; top should be a list of (symbol . count) pairs
        (assert-true (pair? top))
        ;; h has 2 callers, g has 1
        (let ([h-entry (assq 'h top)]
              [g-entry (assq 'g top)])
          (assert-true (pair? h-entry))
          (assert-equal 2 (cdr h-entry))
          (assert-true (pair? g-entry))
          (assert-equal 1 (cdr g-entry))))))

  (define-test "respects limit"
    (let ()
      (populate-xref-cache!
        '((a . (c d))
          (b . (c))
          (c . ())
          (d . ())))
      ;; c has 2 callers, d has 1 — asking for top 1
      (let ([top (xref-most-called 1)])
        (assert-equal 1 (length top))
        (assert-equal 'c (car (car top))))))

  (define-test "empty cache returns empty"
    (let ()
      (populate-xref-cache! '())
      (assert-equal '() (xref-most-called 10))))
)

;;; ========================================
;;; Test: take-n-xref
;;; ========================================

(test-group "take-n-xref"

  (define-test "takes first n elements"
    (assert-equal '(a b c) (take-n-xref 3 '(a b c d e))))

  (define-test "takes all if n exceeds length"
    (assert-equal '(a b) (take-n-xref 5 '(a b))))

  (define-test "zero returns empty"
    (assert-equal '() (take-n-xref 0 '(a b c))))

  (define-test "negative returns empty"
    (assert-equal '() (take-n-xref -1 '(a b c))))

  (define-test "empty list returns empty"
    (assert-equal '() (take-n-xref 3 '())))
)

;;; ========================================
;;; Run
;;; ========================================

(run-all-tests)
