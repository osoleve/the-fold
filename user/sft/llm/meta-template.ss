;;; user/sft/llm/meta-template.ss — Template DSL SFT task generator
;;;
;;; Generates SFT samples teaching the template DSL:
;;;   - Basic fill: create template, fill holes, compile
;;;   - Zipper navigation: multi-hole, navigate + fill in order
;;;   - Propagating holes: fill with expressions containing new holes
;;;   - Undo recovery: fill incorrectly, undo, refill
;;;   - Try-compile: check completeness, handle incomplete templates
;;;
;;; Seeds are mechanically constructed (no LLM needed for ground truth).
;;; Verification is pure: compile-template produces expected S-expression.
;;;
;;; Usage:
;;;   (load "user/sft/llm/meta-template.ss")
;;;   (define seeds (meta-template/generate-seeds all-ir))

(load "user/sft/extract/walker.ss")
(load "user/sft/extract/emit.ss")

;;; ====
;;; Seed Data: realistic function shapes from walker IR
;;; ====

(define (mt/sample-ir-functions all-ir n seed)
  (doc 'description "Sample n IR records with bodies, deterministically from seed")
  (let* ([with-body (filter (lambda (ir)
                              (let ([b (assq 'body ir)])
                                (and b (pair? (cdr b)) (not (null? (cdr b))))))
                            all-ir)]
         [sorted (list-sort (lambda (a b)
                              (string<? (symbol->string (cdr (assq 'name a)))
                                        (symbol->string (cdr (assq 'name b)))))
                            with-body)]
         [total (length sorted)])
    (if (<= total n)
        sorted
        ;; Deterministic sampling via stride
        (let ([stride (max 1 (quotient total n))])
          (let loop ([i 0] [acc '()])
            (if (or (>= i total) (>= (length acc) n))
                (reverse acc)
                (loop (+ i stride)
                      (cons (list-ref sorted i) acc))))))))

;;; ====
;;; Template Patterns
;;; ====
;;; Each pattern is a function (ir -> (template-expr expected-result fills))
;;; where fills is an alist of ($hole . value) and expected-result is the
;;; compiled output.

(define (mt/pattern-simple-define ir)
  (doc 'description "Basic: (define ($name $args) $body) — fill all three")
  (let* ([name (cdr (assq 'name ir))]
         [formals (let ([f (assq 'formals ir)]) (if f (cdr f) '()))]
         [body-forms (cdr (assq 'body ir))]
         [body (if (and (pair? body-forms) (= (length body-forms) 1))
                   (car body-forms)
                   (if (pair? body-forms) (cons 'begin body-forms) '(void)))]
         [template-expr '(define ($name . $args) $body)]
         [fills `(($name . ,name)
                  ($args . ,formals)
                  ($body . ,body))]
         [expected `(define (,name . ,formals) ,body)])
    (list template-expr expected fills "basic_fill")))

(define (mt/pattern-let-binding ir)
  (doc 'description "Let binding: (let (($var $init)) $body)")
  (let* ([name (cdr (assq 'name ir))]
         [formals (let ([f (assq 'formals ir)]) (if f (cdr f) '()))]
         [body-forms (cdr (assq 'body ir))]
         [body (if (and (pair? body-forms) (= (length body-forms) 1))
                   (car body-forms) (cons 'begin body-forms))]
         [var-name (if (pair? formals) (car formals) 'x)]
         [template-expr '(let (($var $init)) $body)]
         [fills `(($var . ,var-name)
                  ($init . 0)
                  ($body . (+ ,var-name 1)))]
         [expected `(let ((,var-name 0)) (+ ,var-name 1))])
    (list template-expr expected fills "basic_fill")))

(define (mt/pattern-cond-branches ir)
  (doc 'description "Cond: (cond ($test1 $result1) ($test2 $result2) (else $default))")
  (let* ([name (cdr (assq 'name ir))]
         [template-expr '(cond ($test1 $result1) ($test2 $result2) (else $default))]
         [fills `(($test1 . (null? lst))
                  ($result1 . '())
                  ($test2 . (pair? lst))
                  ($result2 . (car lst))
                  ($default . (error 'bad "unexpected")))]
         [expected '(cond ((null? lst) '()) ((pair? lst) (car lst))
                          (else (error 'bad "unexpected")))])
    (list template-expr expected fills "basic_fill")))

(define (mt/pattern-propagating ir)
  (doc 'description
    "Propagating: fill $body with (if $cond $then $else),
     introducing 3 new holes, then fill those.")
  (let* ([name (cdr (assq 'name ir))]
         [formals (let ([f (assq 'formals ir)]) (if f (cdr f) '()))]
         [first-arg (if (pair? formals) (car formals) 'x)]
         [template-expr '(define ($fn $arg) $body)]
         ;; Phase 1: fill $fn, $arg. $body gets (if $cond $then $else) — propagates holes
         [phase1-fills `(($fn . ,name)
                         ($arg . ,first-arg)
                         ($body . (if $cond $then $else)))]
         ;; Phase 2: fill the propagated holes
         [phase2-fills `(($cond . (null? ,first-arg))
                         ($then . '())
                         ($else . (cdr ,first-arg)))]
         [expected `(define (,name ,first-arg)
                     (if (null? ,first-arg) '() (cdr ,first-arg)))])
    (list template-expr expected
          (append phase1-fills phase2-fills) "propagating")))

(define (mt/pattern-try-compile ir)
  (doc 'description
    "Try-compile: create template, fill partially, check incomplete,
     then complete and verify.")
  (let* ([name (cdr (assq 'name ir))]
         [template-expr '(lambda ($arg) $body)]
         [partial-fills `(($arg . x))]
         ;; After partial fill: (lambda (x) $body) — still incomplete
         [full-fills `(($arg . x) ($body . (* x x)))]
         [expected '(lambda (x) (* x x))])
    (list template-expr expected full-fills "try_compile")))

(define (mt/pattern-zipper-nav ir)
  (doc 'description
    "Zipper: create 4-hole template, navigate to 3rd hole, fill from there.")
  (let* ([template-expr '(define ($name $a $b $c)
                           (+ (* $a $b) $c))]
         [fills `(($name . compute)
                  ($a . x)
                  ($b . y)
                  ($c . z))]
         [expected '(define (compute x y z)
                     (+ (* x y) z))])
    (list template-expr expected fills "zipper_nav")))

(define (mt/pattern-undo ir)
  (doc 'description
    "Undo: fill a hole incorrectly, undo, then fill correctly.")
  (let* ([name (cdr (assq 'name ir))]
         [template-expr '(define ($fn $arg) $body)]
         ;; Wrong fill, then undo, then correct fill
         [wrong-fills `(($fn . ,name) ($arg . WRONG))]
         [correct-fills `(($fn . ,name)
                          ($arg . x)
                          ($body . (+ x 1)))]
         [expected `(define (,name x) (+ x 1))])
    (list template-expr expected correct-fills "undo_recovery")))

;;; ====
;;; Sample Construction
;;; ====

(define *mt-patterns*
  (list mt/pattern-simple-define
        mt/pattern-let-binding
        mt/pattern-cond-branches
        mt/pattern-propagating
        mt/pattern-try-compile
        mt/pattern-zipper-nav
        mt/pattern-undo))

(define (mt/pattern-difficulty task-type)
  (cond
    [(member task-type '("basic_fill")) "easy"]
    [(member task-type '("try_compile" "zipper_nav" "propagating")) "medium"]
    [(member task-type '("undo_recovery")) "hard"]
    [else "medium"]))

(define (mt/build-prompt template-expr fills task-type)
  (doc 'description "Build a mechanical prompt for a template task")
  (let ([tmpl-str (let ([p (open-output-string)])
                    (write template-expr p)
                    (get-output-string p))]
        [fills-str (let ([p (open-output-string)])
                     (for-each (lambda (f)
                                 (display (format "  ~a → " (car f)) p)
                                 (write (cdr f) p)
                                 (newline p))
                               fills)
                     (get-output-string p))])
    (case task-type
      [("basic_fill")
       (format "Using the Template DSL, create a template from:
```scheme
~a
```

Fill the holes with these values:
~a
Then compile the template and show the result." tmpl-str fills-str)]

      [("propagating")
       (format "Using the Template DSL, create a template from:
```scheme
~a
```

Fill holes in two phases. First fill $fn, $arg, and $body — but $body should be
`(if $cond $then $else)`, introducing new holes. Then fill the propagated holes:
~a
Show the final compiled result." tmpl-str fills-str)]

      [("try_compile")
       (format "Using the Template DSL, create a template from:
```scheme
~a
```

First, fill only $arg with `x` and verify the template is NOT complete
using `try-compile-template`. Then fill $body with `(* x x)` and compile.
Show the final result." tmpl-str)]

      [("zipper_nav")
       (format "Using the Template DSL with zipper navigation, create a template from:
```scheme
~a
```

Use `template->hole-zipper` to navigate through the holes.
Fill each hole in order using `hole-zipper-fill`:
~a
Compile the completed template." tmpl-str fills-str)]

      [("undo_recovery")
       (format "Using the Template DSL with zipper navigation, create a template from:
```scheme
~a
```

Fill $fn correctly, then fill $arg with `WRONG` (an incorrect value).
Use `hole-zipper-undo` to reverse the bad fill.
Then fill correctly:
~a
Compile and verify the result." tmpl-str fills-str)]

      [else (format "Template DSL task:\n~a\nFills:\n~a" tmpl-str fills-str)])))

(define (mt/build-ground-truth template-expr fills expected task-type)
  (doc 'description "Build ground truth code showing the full solution")
  (let ([tmpl-str (let ([p (open-output-string)])
                    (write template-expr p)
                    (get-output-string p))]
        [expected-str (let ([p (open-output-string)])
                        (pretty-print expected p)
                        (get-output-string p))])
    (case task-type
      [("basic_fill")
       (let ([fill-strs
              (apply string-append
                (map (lambda (f)
                       (let ([p (open-output-string)])
                         (display (format "(fill-hole t '~a " (car f)) p)
                         (write (cdr f) p)
                         (display ")" p)
                         (let ([s (get-output-string p)])
                           (string-append "  (set! t " s ")\n"))))
                     fills))])
         (format "(let ([t (new-template '~a)])
~a  (compile-template t))
;; => ~a" tmpl-str fill-strs expected-str))]

      [("propagating")
       (format "(let* ([t (new-template '~a)]
        ;; Phase 1: fill with expression containing new holes
        [t (fill-holes t '~a)]
        ;; Phase 2: fill propagated holes
        [t (fill-holes t '~a)])
  (compile-template t))
;; => ~a"
               tmpl-str
               (let ([p (open-output-string)])
                 (write (list-head fills 3) p) (get-output-string p))
               (let ([p (open-output-string)])
                 (write (list-tail fills 3) p) (get-output-string p))
               expected-str)]

      [("try_compile")
       (format "(let* ([t (new-template '~a)]
        [t (fill-hole t '$arg 'x)]
        [result (try-compile-template t)])
  ;; result => (err ($body)) — not yet complete
  (let ([t (fill-hole t '$body '(* x x))])
    (compile-template t)))
;; => ~a" tmpl-str expected-str)]

      [("zipper_nav")
       (let ([fill-strs
              (apply string-append
                (map (lambda (f)
                       (format "        [hz (hole-zipper-fill hz '~a)]\n"
                               (cdr f)))
                     fills))])
         (format "(let* ([t (new-template '~a)]
        [hz (template->hole-zipper t)]
~a        [t (hole-zipper->template hz)])
  (compile-template t))
;; => ~a" tmpl-str fill-strs expected-str))]

      [("undo_recovery")
       (format "(let* ([t (new-template '~a)]
        [hz (template->hole-zipper t)]
        ;; Fill $fn correctly
        [hz (hole-zipper-fill hz '~a)]
        ;; Fill $arg incorrectly
        [hz (hole-zipper-fill hz 'WRONG)]
        ;; Undo the bad fill
        [hz (hole-zipper-undo hz)]
        ;; Fill $arg correctly
        [hz (hole-zipper-fill hz 'x)]
        ;; Fill $body
        [hz (hole-zipper-fill hz '(+ x 1))]
        [t (hole-zipper->template hz)])
  (compile-template t))
;; => ~a" tmpl-str (cdr (car fills)) expected-str)]

      [else (format ";; ~a\n~a" task-type expected-str)])))

(define (mt/build-verify-expr template-expr fills expected task-type)
  (doc 'description "Build verify expression string — self-contained template check")
  (let ([sexp (mt/build-verify-sexp template-expr fills expected task-type)])
    (let ([p (open-output-string)])
      (write sexp p)
      (get-output-string p))))

(define (mt/build-verify-sexp template-expr fills expected task-type)
  (doc 'description
    "Build verify S-expression — self-contained template check.
     Creates template, fills all holes, compiles, and checks against expected.")
  ;; Self-contained: independently re-derive the result
  (let ([fill-forms
         (map (lambda (f)
                `(set! t (fill-hole t ',(car f) ',(cdr f))))
              fills)])
    `(let ([t (new-template ',template-expr)])
       ,@fill-forms
       (equal? (compile-template t) ',expected))))

;;; ====
;;; Main Generator
;;; ====

(define *mt-counter* 0)

(define (mt/next-id)
  (set! *mt-counter* (+ *mt-counter* 1))
  (format "meta_template_~3,'0d" *mt-counter*))

(define (meta-template/generate-seeds all-ir)
  (doc 'description
    "Generate template DSL SFT samples from walker IR.
     Returns list of sample alists in standard schema.")
  (set! *mt-counter* 0)
  (let* ([sampled (mt/sample-ir-functions all-ir 200 42)]
         [n-patterns (length *mt-patterns*)])
    (printf "Generating template DSL seeds from ~a IR records...\\n"
            (length sampled))
    (let loop ([irs sampled] [pat-idx 0] [acc '()])
      (if (null? irs)
          (begin
            (printf "Generated ~a template DSL seeds\\n" (length acc))
            (reverse acc))
          (let* ([ir (car irs)]
                 [pattern-fn (list-ref *mt-patterns*
                               (modulo pat-idx n-patterns))]
                 [result (guard (ex [else #f])
                           (pattern-fn ir))])
            (if (not result)
                ;; Pattern didn't work for this IR, skip
                (loop (cdr irs) (+ pat-idx 1) acc)
                (let* ([template-expr (list-ref result 0)]
                       [expected (list-ref result 1)]
                       [fills (list-ref result 2)]
                       [task-type (list-ref result 3)]
                       [id (mt/next-id)]
                       [prompt-body (mt/build-prompt template-expr fills task-type)]
                       [ground-truth (mt/build-ground-truth
                                       template-expr fills expected task-type)]
                       [verify-str (mt/build-verify-expr
                                     template-expr fills expected task-type)]
                       [difficulty (mt/pattern-difficulty task-type)]
                       [ir-skill (cdr (assq 'skill ir))]
                       [ir-module (cdr (assq 'module ir))]
                       [ir-name (cdr (assq 'name ir))]
                       [tags (emit/make-tags ir-skill ir-module
                               "meta_template" ir-name
                               (list task-type))]
                       [sample (make-sample id
                                 "meta_template" "usage" difficulty
                                 "lattice/dsl/template/template.ss"
                                 "" (symbol->string ir-name)
                                 prompt-body ground-truth verify-str
                                 tags)])
                  (loop (cdr irs) (+ pat-idx 1)
                        (cons sample acc)))))))))

;;; ====
;;; REPL interface
;;; ====

(printf "meta-template.ss loaded.\\n")
(printf "  (meta-template/generate-seeds all-ir) - Generate template DSL seeds\\n")
