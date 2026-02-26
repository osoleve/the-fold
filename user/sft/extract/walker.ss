;;; user/sft/extract/walker.ss — Extract function IR from lattice source
;;;
;;; Walk all lattice manifests and produce one IR record per defined function.
;;; Output is a list of alists suitable for downstream SFT generation.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (define ir (walk-all-manifests))
;;;   (ir-summary ir)
;;;   (car (ir-exported-only ir))  ; inspect one exported function

;;; ====
;;; Dependencies
;;; ====

(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))
(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))

;; Manifest parsing (pure)
(unless (top-level-bound? 'parse-manifest) (load "lattice/meta/manifest.ss"))

;; Define pattern recognition (pure)
(unless (top-level-bound? 'define-form?) (load "lattice/meta/exports.ss"))

;; Line-number extraction (pure parsing, mutable cache)
(unless (top-level-bound? 'parse-definitions-with-lines) (load "lattice/meta/source-loc.ss"))

;; Symbol extraction from bodies (mutable caches)
(unless (top-level-bound? 'extract-symbols-from-sexp) (load "lattice/meta/xref.ss"))

;; Docstring extraction (pure parsing, mutable cache)
(unless (top-level-bound? 'parse-docstrings) (load "lattice/meta/docstrings.ss"))

;;; ====
;;; Section 1: File I/O
;;; ====

(define (walker/read-sexps path)
  (guard (exn [else (begin (printf "  WARN: read-sexps failed for ~a\n" path) '())])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([form (read port)])
            (if (eof-object? form)
                (reverse acc)
                (loop (cons form acc)))))))))

(define (walker/read-lines path)
  (guard (exn [else '()])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([line (get-line port)])
            (if (eof-object? line)
                (reverse acc)
                (loop (cons line acc)))))))))

;;; ====
;;; Section 2: String Utilities
;;; ====

(define (walker/trim-left s)
  (let loop ([i 0])
    (if (and (< i (string-length s))
             (char-whitespace? (string-ref s i)))
        (loop (+ i 1))
        (if (= i 0) s (substring s i (string-length s))))))

(define (walker/split-whitespace s)
  (let loop ([i 0] [start #f] [acc '()])
    (cond
      [(= i (string-length s))
       (reverse (if start (cons (substring s start i) acc) acc))]
      [(char-whitespace? (string-ref s i))
       (loop (+ i 1) #f (if start (cons (substring s start i) acc) acc))]
      [else (loop (+ i 1) (or start i) acc)])))

(define (walker/string-prefix? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))

(define (walker/string-suffix? s suffix)
  (let ([sl (string-length s)] [xl (string-length suffix)])
    (and (>= sl xl)
         (string=? (substring s (- sl xl) sl) suffix))))

;;; ====
;;; Section 3: @requires Extraction
;;; ====

(define (walker/extract-requires lines)
  ;; Scan header lines for ;;; @requires annotation
  (let loop ([remaining lines] [n 0])
    (cond
      [(or (null? remaining) (> n 20)) '()]  ; stop after 20 lines
      [(walker/string-prefix? (walker/trim-left (car remaining)) ";;; @requires")
       (let* ([line (walker/trim-left (car remaining))]
              [rest (substring line 14 (string-length line))])
         (map string->symbol (walker/split-whitespace rest)))]
      [else (loop (cdr remaining) (+ n 1))])))

;;; ====
;;; Section 4: Doc Form Detection
;;; ====

;; After (read), 'type becomes (quote type).
;; (doc 'type '(-> Nat a (Vec a))) reads as:
;;   (doc (quote type) (quote (-> Nat a (Vec a))))

(define (walker/quoted-symbol? x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (symbol? (cadr x))))

(define (walker/unquote-value x)
  (if (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)))
      (cadr x)
      x))

;; Internal doc: (doc 'tag value) — appears inside define body
;; After read: (doc (quote tag) value)
(define (walker/internal-doc? expr)
  (and (pair? expr)
       (eq? (car expr) 'doc)
       (pair? (cdr expr))
       (walker/quoted-symbol? (cadr expr))
       (pair? (cddr expr))))

;; External doc: (doc name 'tag value) — appears before define
;; After read: (doc name (quote tag) value)
(define (walker/external-doc? expr)
  (and (pair? expr)
       (eq? (car expr) 'doc)
       (pair? (cdr expr))
       (symbol? (cadr expr))        ; bare symbol = function name
       (pair? (cddr expr))
       (walker/quoted-symbol? (caddr expr))
       (pair? (cdddr expr))))

;;; ====
;;; Section 5: Doc Extraction
;;; ====

(define (walker/extract-internal-docs body-exprs)
  ;; Extract (tag . value) pairs from internal doc forms in a define body.
  (filter-map
    (lambda (expr)
      (if (walker/internal-doc? expr)
          (cons (cadr (cadr expr))                ; tag symbol
                (walker/unquote-value (caddr expr))) ; value
          #f))
    body-exprs))

(define (walker/strip-docs body-exprs)
  ;; Remove doc forms from body, leaving the actual implementation.
  (filter (lambda (expr) (not (walker/internal-doc? expr))) body-exprs))

(define (walker/collect-external-docs sexps)
  ;; Scan top-level forms for (doc name 'tag value) patterns.
  ;; Returns alist: ((name . ((tag . value) ...)) ...)
  (let loop ([remaining sexps] [acc '()])
    (if (null? remaining)
        acc
        (let ([form (car remaining)])
          (if (walker/external-doc? form)
              (let* ([target (cadr form)]
                     [tag (cadr (caddr form))]
                     [value (walker/unquote-value (cadddr form))]
                     [existing (let ([e (assq target acc)])
                                 (if e (cdr e) '()))]
                     [updated (cons (cons tag value) existing)]
                     [new-acc (cons (cons target updated)
                                   (remp (lambda (p) (eq? (car p) target)) acc))])
                (loop (cdr remaining) new-acc))
              (loop (cdr remaining) acc))))))

;;; ====
;;; Section 6: Define Analysis
;;; ====

(define (walker/define-formals form)
  ;; Extract parameter list from a define form.
  ;; Handles: (define (f x y) ...) → (x y)
  ;;          (define (f x . rest) ...) → (x . rest)
  ;;          (define f (lambda (x) ...)) → (x)
  ;;          (define f value) → ()
  (let ([name-part (cadr form)])
    (cond
      [(pair? name-part) (cdr name-part)]
      [(and (pair? (cddr form))
            (pair? (caddr form))
            (eq? (car (caddr form)) 'lambda)
            (pair? (cdr (caddr form))))
       (cadr (caddr form))]
      [else '()])))

(define (walker/flatten-formals formals)
  ;; Flatten potentially dotted formals to a proper list.
  ;; (a b c) → (a b c)
  ;; (a b . rest) → (a b rest)
  ;; rest → (rest)
  (cond
    [(null? formals) '()]
    [(symbol? formals) (list formals)]
    [(pair? formals) (cons (car formals) (walker/flatten-formals (cdr formals)))]
    [else '()]))

;;; ====
;;; Section 7: Test Extraction
;;; ====

(define (walker/mentions-symbol? sexp sym)
  ;; Does sexp contain sym anywhere in its tree?
  (cond
    [(eq? sexp sym) #t]
    [(pair? sexp)
     (or (walker/mentions-symbol? (car sexp) sym)
         (walker/mentions-symbol? (cdr sexp) sym))]
    [else #f]))

(define (walker/extract-test-forms sexps)
  ;; Extract test assertions from a file's S-expressions.
  ;; Walks into test-group forms. Returns flat list of test forms.
  (let loop ([remaining sexps] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([form (car remaining)])
          (cond
            ;; New framework: (define-test name body...)
            [(and (pair? form) (eq? (car form) 'define-test))
             (loop (cdr remaining) (cons form acc))]
            ;; Old framework: (test "name" expected actual)
            [(and (pair? form) (eq? (car form) 'test)
                  (pair? (cdr form)))
             (loop (cdr remaining) (cons form acc))]
            ;; (test-error "name" thunk)
            [(and (pair? form) (eq? (car form) 'test-error))
             (loop (cdr remaining) (cons form acc))]
            ;; (test-group name body...) — recurse into body
            [(and (pair? form) (eq? (car form) 'test-group)
                  (pair? (cdr form)) (pair? (cddr form)))
             (loop (cdr remaining)
                   (append (reverse (walker/extract-test-forms (cddr form))) acc))]
            ;; Other — skip
            [else (loop (cdr remaining) acc)])))))

(define (walker/tests-for-function test-forms fn-name)
  ;; Filter test forms that reference a specific function name.
  (filter (lambda (form) (walker/mentions-symbol? form fn-name))
          test-forms))

;;; ====
;;; Section 8: Test File Discovery
;;; ====

(define (walker/find-test-files dir)
  ;; Find test-*.ss files in dir and immediate subdirectories.
  ;; Returns list of full paths.
  (define (test-file? f)
    (and (walker/string-prefix? f "test-")
         (walker/string-suffix? f ".ss")))
  (guard (exn [else '()])
    (let* ([entries (directory-list dir)]
           [top-tests (map (lambda (f) (string-append dir "/" f))
                           (filter test-file? entries))]
           [subdirs (filter-map
                      (lambda (e)
                        (let ([p (string-append dir "/" e)])
                          (if (and (file-directory? p)
                                   (not (char=? (string-ref e 0) #\.)))
                              p #f)))
                      entries)]
           [sub-tests (append-map
                        (lambda (sd)
                          (guard (exn [else '()])
                            (map (lambda (f) (string-append sd "/" f))
                                 (filter test-file? (directory-list sd)))))
                        subdirs)])
      (append top-tests sub-tests))))

;;; ====
;;; Section 9: Manifest Discovery
;;; ====

(define (walker/find-all-manifests base-dir)
  ;; Recursively find all manifest.sexp files under base-dir.
  (let loop ([dirs (list base-dir)] [acc '()])
    (if (null? dirs)
        (reverse acc)
        (let* ([d (car dirs)]
               [entries (guard (exn [else '()]) (directory-list d))]
               [manifest (string-append d "/manifest.sexp")]
               [has-manifest? (file-exists? manifest)]
               [subdirs (filter-map
                          (lambda (e)
                            (let ([p (string-append d "/" e)])
                              (if (and (file-directory? p)
                                       (not (char=? (string-ref e 0) #\.)))
                                  p #f)))
                          entries)])
          (loop (append (cdr dirs) subdirs)
                (if has-manifest? (cons manifest acc) acc))))))

;;; ====
;;; Section 10: Module Walking
;;; ====

(define (walker/walk-module-path skill-name module-name full-path concepts)
  ;; Walk a single module file (given full path) and extract function IR records.
  ;; Per-function guard: one bad form doesn't kill the whole module.
  (if (not (file-exists? full-path))
      (begin (printf "  WARN: not found ~a\n" full-path) '())
      (let* ([sexps (walker/read-sexps full-path)]
             [lines (walker/read-lines full-path)]
             ;; Line numbers for each define
             [line-defs (parse-definitions-with-lines full-path lines)]
             ;; ;;; docstrings
             [docstring-entries (parse-docstrings lines)]
             ;; @requires from module header
             [requires (walker/extract-requires lines)]
             ;; External doc forms: (doc name 'tag value)
             [ext-docs (walker/collect-external-docs sexps)])
        ;; Walk each top-level form with per-function error handling
        (filter-map
          (lambda (form)
            (and (define-form? form)
                 (let ([name (define-name form)])
                   (and name
                        (guard (exn [else #f])  ; skip single broken function
                          (let* ([formals (walker/define-formals form)]
                                 [body-raw (define-body form)]
                                 [int-docs (walker/extract-internal-docs body-raw)]
                                 [clean-body (walker/strip-docs body-raw)]
                                 ;; External docs for this function
                                 [ext-entry (assq name ext-docs)]
                                 [ext-doc-alist (if ext-entry (cdr ext-entry) '())]
                                 ;; Merge: internal takes priority
                                 [all-docs (append int-docs ext-doc-alist)]
                                 ;; Type signature
                                 [type-sig (let ([t (assq 'type all-docs)])
                                             (if t (cdr t) #f))]
                                 ;; Description (from doc form or ;;; docstring)
                                 [doc-desc (let ([d (assq 'description all-docs)])
                                             (if d (cdr d) #f))]
                                 [docstring (let ([d (assq name docstring-entries)])
                                              (if d (cdr d) #f))]
                                 [desc (or doc-desc docstring)]
                                 ;; Line number
                                 [line-entry (find (lambda (e) (eq? (car e) name))
                                                   line-defs)]
                                 [line-num (if line-entry (caddr line-entry) #f)]
                                 ;; Deps: symbols referenced in body (guarded)
                                 [flat-formals (walker/flatten-formals formals)]
                                 [raw-deps (guard (exn [else '()])
                                             (if (pair? clean-body)
                                                 (extract-symbols-from-sexp
                                                   (cons 'begin clean-body))
                                                 '()))]
                                 [deps (remp (lambda (s)
                                               (or (eq? s name)
                                                   (memq s flat-formals)))
                                             raw-deps)]
                                 ;; Build clean define (without doc forms)
                                 [clean-define
                                   (if (pair? (cadr form))
                                       (cons 'define (cons (cadr form) clean-body))
                                       form)])
                            `((name . ,name)
                              (module . ,module-name)
                              (skill . ,skill-name)
                              (file . ,full-path)
                              (line . ,line-num)
                              (type . ,type-sig)
                              (description . ,desc)
                              (formals . ,formals)
                              (body . ,clean-body)
                              (clean-define . ,clean-define)
                              (full-source . ,form)
                              (deps . ,deps)
                              (requires . ,requires)
                              (concepts . ,concepts))))))))
          sexps))))

;;; ====
;;; Section 11: Skill Walking
;;; ====

(define (walker/all-export-symbols parsed)
  ;; Build a flat set (list) of all exported symbols from a manifest.
  ;; Handles both grouped ((mod sym1 sym2...) ...) and flat (sym1 sym2 ...) formats.
  (let ([raw (manifest-exports parsed)])
    (if (null? raw) '()
        (if (and (pair? (car raw)) (symbol? (car (car raw))))
            ;; Grouped: ((mod sym1 sym2 ...) ...) — flatten, skip module labels
            (append-map cdr raw)
            ;; Flat: (sym1 sym2 ...) — all symbols directly
            (filter symbol? raw)))))

(define (walker/robust-module-index parsed)
  ;; Like manifest->module-index but handles the flatten-single bug
  ;; where single-module skills get unwrapped from ((entry)) to (entry).
  (let ([index (manifest->module-index parsed)])
    (if (pair? index)
        index
        ;; Empty — possibly a single-module skill hit the flatten-single trap.
        ;; Check if raw modules looks like one unwrapped entry: (name "file" "desc")
        (let ([raw (manifest-modules parsed)]
              [skill-path (manifest-path parsed)])
          (if (and (pair? raw)
                   (or (symbol? (car raw)) (string? (car raw)))
                   (pair? (cdr raw))
                   (string? (cadr raw)))
              ;; Single unwrapped entry — re-wrap and parse
              (parse-module-entry raw skill-path)
              ;; Genuinely empty
              '())))))

(define (walker/walk-skill manifest-path)
  ;; Walk a single skill: parse manifest, walk all modules, attach tests.
  (guard (exn [else (begin
                      (printf "WARN: failed ~a: ~a\n"
                              manifest-path
                              (if (condition? exn)
                                  (condition-message exn)
                                  exn))
                      '())])
    (let* ([sexp (call-with-input-file manifest-path read)])
      (if (not (and (pair? sexp) (eq? (car sexp) 'skill)))
          (begin (printf "  WARN: not a skill manifest: ~a\n" manifest-path) '())
          (let* ([parsed (parse-manifest sexp)]
                 [skill-name (cdr (assq 'name parsed))]
                 [skill-path (cdr (assq 'path parsed))]
                 ;; Use robust module-index (handles flatten-single bug)
                 ;; Returns ((mod-name . "full/path.ss") ...)
                 [mod-index (walker/robust-module-index parsed)]
                 ;; Flat set of all exported symbols
                 [all-exports (walker/all-export-symbols parsed)]
                 ;; Concepts
                 [concept-forms (let ([c (assq 'concepts parsed)])
                                  (if c (cdr c) '()))]
                 [concept-names (filter-map
                                  (lambda (c)
                                    (if (and (pair? c) (eq? (car c) 'concept)
                                             (pair? (cdr c)))
                                        (cadr c) #f))
                                  (if (list? concept-forms) concept-forms '()))]
                 ;; Test files
                 [test-files (if (and (string? skill-path)
                                      (> (string-length skill-path) 0))
                                 (walker/find-test-files skill-path)
                                 '())]
                 [all-test-forms
                   (append-map
                     (lambda (tf) (walker/extract-test-forms (walker/read-sexps tf)))
                     test-files)])
            ;; Walk each module via module-index (handles all manifest formats)
            (let ([results
                    (append-map
                      (lambda (entry)
                        (let ([mod-name (car entry)]
                              [full-path (cdr entry)])
                          (guard (exn [else
                                   (printf "  WARN: module ~a failed: ~a\n"
                                           mod-name
                                           (if (condition? exn)
                                               (condition-message exn)
                                               exn))
                                   '()])
                            (let ([ir-records
                                    (walker/walk-module-path skill-name mod-name
                                                             full-path concept-names)])
                              ;; Annotate each record with export status and tests
                              (map (lambda (ir)
                                     (let* ([fn-name (cdr (assq 'name ir))]
                                            [exported? (if (memq fn-name all-exports)
                                                           #t #f)]
                                            [fn-tests (walker/tests-for-function
                                                        all-test-forms fn-name)])
                                       (append ir
                                               `((exported? . ,exported?)
                                                 (tests . ,fn-tests)))))
                                   ir-records)))))
                      mod-index)])
              (printf "  ~a: ~a functions from ~a modules (~a test files)\n"
                      skill-name (length results) (length mod-index)
                      (length test-files))
              results))))))

;;; ====
;;; Section 12: Walk All
;;; ====

(define (walk-all-manifests)
  ;; Walk all lattice manifests and return complete function IR.
  (let* ([manifests (walker/find-all-manifests "lattice")]
         [_ (printf "Found ~a manifests\n" (length manifests))]
         [all-ir (append-map walker/walk-skill manifests)])
    (printf "\nTotal: ~a function IR records\n" (length all-ir))
    all-ir))

;;; ====
;;; Section 13: Query Utilities
;;; ====

(define (ir-get ir key)
  (let ([entry (assq key ir)])
    (if entry (cdr entry) #f)))

(define (ir-exported-only records)
  (filter (lambda (ir) (ir-get ir 'exported?)) records))

(define (ir-with-type records)
  (filter (lambda (ir) (ir-get ir 'type)) records))

(define (ir-with-tests records)
  (filter (lambda (ir) (pair? (ir-get ir 'tests))) records))

(define (ir-with-description records)
  (filter (lambda (ir) (ir-get ir 'description)) records))

(define (ir-by-skill records skill)
  (filter (lambda (ir) (eq? (ir-get ir 'skill) skill)) records))

(define (ir-by-module records mod)
  (filter (lambda (ir) (eq? (ir-get ir 'module) mod)) records))

(define (ir-by-name records name)
  (find (lambda (ir) (eq? (ir-get ir 'name) name)) records))

(define (ir-skills records)
  ;; Unique skill names
  (let loop ([remaining records] [seen '()] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([s (ir-get (car remaining) 'skill)])
          (if (memq s seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining) (cons s seen) (cons s acc)))))))

(define (ir-summary records)
  (let* ([total (length records)]
         [exported (length (ir-exported-only records))]
         [typed (length (ir-with-type records))]
         [tested (length (ir-with-tests records))]
         [described (length (ir-with-description records))]
         [skills (ir-skills records)])
    (printf "Function IR Summary:\n")
    (printf "  Total records:    ~a\n" total)
    (printf "  Exported:         ~a (~a%)\n" exported
            (if (> total 0) (quotient (* exported 100) total) 0))
    (printf "  With type sigs:   ~a (~a%)\n" typed
            (if (> total 0) (quotient (* typed 100) total) 0))
    (printf "  With tests:       ~a (~a%)\n" tested
            (if (> total 0) (quotient (* tested 100) total) 0))
    (printf "  With description: ~a (~a%)\n" described
            (if (> total 0) (quotient (* described 100) total) 0))
    (printf "  Skills (~a):\n" (length skills))
    (for-each
      (lambda (s)
        (let* ([skill-fns (ir-by-skill records s)]
               [skill-exported (length (ir-exported-only skill-fns))])
          (printf "    ~a: ~a total, ~a exported\n"
                  s (length skill-fns) skill-exported)))
      skills)))

;;; ====
;;; Section 14: Single-Record Inspection
;;; ====

(define (ir-inspect ir)
  ;; Pretty-print a single IR record.
  (printf "~a (~a/~a)\n" (ir-get ir 'name) (ir-get ir 'skill) (ir-get ir 'module))
  (printf "  File: ~a" (ir-get ir 'file))
  (when (ir-get ir 'line) (printf ":~a" (ir-get ir 'line)))
  (printf "\n")
  (when (ir-get ir 'type)
    (printf "  Type: ~s\n" (ir-get ir 'type)))
  (when (ir-get ir 'description)
    (printf "  Desc: ~a\n" (ir-get ir 'description)))
  (printf "  Formals: ~s\n" (ir-get ir 'formals))
  (printf "  Exported: ~a\n" (ir-get ir 'exported?))
  (printf "  Deps: ~a\n" (length (or (ir-get ir 'deps) '())))
  (let ([tests (ir-get ir 'tests)])
    (printf "  Tests: ~a\n" (if (pair? tests) (length tests) 0))))

;;; ====
;;; REPL Interface
;;; ====

(printf "walker.ss loaded.\n")
(printf "  (walk-all-manifests)    - Extract all function IR\n")
(printf "  (ir-summary records)    - Print summary statistics\n")
(printf "  (ir-inspect record)     - Inspect single record\n")
(printf "  (ir-exported-only recs) - Filter to exported functions\n")
(printf "  (ir-with-type recs)     - Filter to functions with type sigs\n")
(printf "  (ir-with-tests recs)    - Filter to functions with tests\n")
