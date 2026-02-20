;;; lattice/pipeline/test-rlm2-alias.ss — Tests for RLM v2 action aliasing
;;;
;;; Verifies that common action synonyms (emitted by smaller models) are
;;; resolved to canonical action names, and that alias info is propagated
;;; through parse results for HUD correction notes.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/pipeline/rlm2.ss")
(load "lattice/pipeline/rlm2-parse.ss")
(load "lattice/pipeline/rlm2-hud.ss")

;;; ====
;;; Alias Table Lookup
;;; ====

(test-group "rlm2-alias-lookup"

  (define-test "finds known aliases"
    (assert-equal 'search (rlm2-alias-lookup 'find))
    (assert-equal 'eval (rlm2-alias-lookup 'run))
    (assert-equal 'eval (rlm2-alias-lookup 'execute))
    (assert-equal 'load (rlm2-alias-lookup 'import))
    (assert-equal 'load (rlm2-alias-lookup 'require))
    (assert-equal 'load (rlm2-alias-lookup 'use))
    (assert-equal 'submit (rlm2-alias-lookup 'answer))
    (assert-equal 'submit (rlm2-alias-lookup 'respond))
    (assert-equal 'submit (rlm2-alias-lookup 'output))
    (assert-equal 'inspect (rlm2-alias-lookup 'look))
    (assert-equal 'inspect (rlm2-alias-lookup 'view))
    (assert-equal 'inspect (rlm2-alias-lookup 'info))
    (assert-equal 'inspect (rlm2-alias-lookup 'describe))
    (assert-equal 'inspect (rlm2-alias-lookup 'show))
    (assert-equal 'symbols (rlm2-alias-lookup 'list))
    (assert-equal 'symbols (rlm2-alias-lookup 'dir))
    (assert-equal 'retrieve (rlm2-alias-lookup 'get))
    (assert-equal 'retrieve (rlm2-alias-lookup 'fetch))
    (assert-equal 'retrieve (rlm2-alias-lookup 'cat))
    (assert-equal 'search (rlm2-alias-lookup 'help))
    (assert-equal 'exports (rlm2-alias-lookup 'list-exports)))

  (define-test "returns #f for non-aliases"
    (assert-false (rlm2-alias-lookup 'search))
    (assert-false (rlm2-alias-lookup 'eval))
    (assert-false (rlm2-alias-lookup 'submit))
    (assert-false (rlm2-alias-lookup 'think))
    (assert-false (rlm2-alias-lookup 'begin))
    (assert-false (rlm2-alias-lookup 'nonexistent)))
)

;;; ====
;;; Alias Resolution
;;; ====

(test-group "rlm2-resolve-alias"

  (define-test "resolves simple alias"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(find "eigenvalue"))])
      (assert-equal '(search "eigenvalue") resolved)
      (assert-equal 'find (car alias-info))
      (assert-equal 'search (cdr alias-info))))

  (define-test "resolves run to eval"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(run (+ 1 2)))])
      (assert-equal '(eval (+ 1 2)) resolved)
      (assert-equal 'run (car alias-info))
      (assert-equal 'eval (cdr alias-info))))

  (define-test "resolves import to load"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(import linalg/vec))])
      (assert-equal '(load linalg/vec) resolved)
      (assert-equal 'import (car alias-info))
      (assert-equal 'load (cdr alias-info))))

  (define-test "resolves answer to submit"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(answer 42))])
      (assert-equal '(submit 42) resolved)
      (assert-equal 'answer (car alias-info))
      (assert-equal 'submit (cdr alias-info))))

  (define-test "passes through canonical actions unchanged"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(search "query"))])
      (assert-equal '(search "query") resolved)
      (assert-false alias-info)))

  (define-test "passes through non-list expressions"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias 42)])
      (assert-equal 42 resolved)
      (assert-false alias-info)))

  (define-test "passes through non-symbol-headed lists"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(42 "hello"))])
      (assert-equal '(42 "hello") resolved)
      (assert-false alias-info)))

  (define-test "preserves arguments during resolution"
    (let-values ([(resolved alias-info) (rlm2-resolve-alias '(view (quote linalg)))])
      (assert-equal '(inspect (quote linalg)) resolved)
      (assert-equal 'view (car alias-info))
      (assert-equal 'inspect (cdr alias-info))))
)

;;; ====
;;; Recursive Alias Resolution (begin blocks)
;;; ====

(test-group "rlm2-resolve-alias-recursive"

  (define-test "resolves aliases inside begin"
    (let-values ([(resolved alias-info)
                  (rlm2-resolve-alias-recursive
                    '(begin (find "foo") (run (+ 1 2))))])
      (assert-equal '(begin (search "foo") (eval (+ 1 2))) resolved)
      ;; alias-info is the first alias found
      (assert-equal 'find (car alias-info))
      (assert-equal 'search (cdr alias-info))))

  (define-test "resolves single alias in begin"
    (let-values ([(resolved alias-info)
                  (rlm2-resolve-alias-recursive
                    '(begin (search "foo") (answer 42)))])
      (assert-equal '(begin (search "foo") (submit 42)) resolved)
      (assert-equal 'answer (car alias-info))
      (assert-equal 'submit (cdr alias-info))))

  (define-test "passes through begin with no aliases"
    (let-values ([(resolved alias-info)
                  (rlm2-resolve-alias-recursive
                    '(begin (search "foo") (eval (+ 1 2))))])
      (assert-equal '(begin (search "foo") (eval (+ 1 2))) resolved)
      (assert-false alias-info)))

  (define-test "resolves non-begin aliases at top level"
    (let-values ([(resolved alias-info)
                  (rlm2-resolve-alias-recursive '(execute (+ 1 2)))])
      (assert-equal '(eval (+ 1 2)) resolved)
      (assert-equal 'execute (car alias-info))
      (assert-equal 'eval (cdr alias-info))))
)

;;; ====
;;; Full Parser Integration — aliased actions parse correctly
;;; ====

(test-group "rlm2-parse-alias-integration"

  (define-test "parses (find ...) as search"
    (let ([r (rlm2-parse-response "(find \"eigenvalue\")")])
      (assert-true (rlm2-parse-result? r))
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))
      (assert-equal "eigenvalue"
                    (rlm2-search-query (rlm2-parse-result-action r)))
      ;; Alias info should be present
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'find (car ai))
        (assert-equal 'search (cdr ai)))))

  (define-test "parses (run ...) as eval"
    (let ([r (rlm2-parse-response "(run (+ 1 2))")])
      (assert-true (rlm2-eval? (rlm2-parse-result-action r)))
      (assert-equal '(+ 1 2) (rlm2-eval-expr (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'run (car ai))
        (assert-equal 'eval (cdr ai)))))

  (define-test "parses (execute ...) as eval"
    (let ([r (rlm2-parse-response "(execute (* 3 4))")])
      (assert-true (rlm2-eval? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'execute (car ai)))))

  (define-test "parses (import ...) as load"
    (let ([r (rlm2-parse-response "(import linalg/vec)")])
      (assert-true (rlm2-load? (rlm2-parse-result-action r)))
      (assert-equal 'linalg/vec
                    (rlm2-load-module (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'import (car ai))
        (assert-equal 'load (cdr ai)))))

  (define-test "parses (require ...) as load"
    (let ([r (rlm2-parse-response "(require 'linalg/matrix)")])
      (assert-true (rlm2-load? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'require (car ai))
        (assert-equal 'load (cdr ai)))))

  (define-test "parses (answer ...) as submit"
    (let ([r (rlm2-parse-response "(answer 42)")])
      (assert-true (rlm2-submit? (rlm2-parse-result-action r)))
      (assert-equal 42 (rlm2-submit-expr (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'answer (car ai))
        (assert-equal 'submit (cdr ai)))))

  (define-test "parses (respond ...) as submit"
    (let ([r (rlm2-parse-response "(respond \"the answer is 42\")")])
      (assert-true (rlm2-submit? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'respond (car ai)))))

  (define-test "parses (view ...) as inspect"
    (let ([r (rlm2-parse-response "(view linalg)")])
      (assert-true (rlm2-inspect? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'view (car ai))
        (assert-equal 'inspect (cdr ai)))))

  (define-test "parses (info ...) as inspect"
    (let ([r (rlm2-parse-response "(info linalg)")])
      (assert-true (rlm2-inspect? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'info (car ai)))))

  (define-test "parses (get ...) as retrieve"
    (let ([r (rlm2-parse-response "(get 'input)")])
      (assert-true (rlm2-retrieve? (rlm2-parse-result-action r)))
      (assert-equal 'input
                    (rlm2-retrieve-key (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'get (car ai))
        (assert-equal 'retrieve (cdr ai)))))

  (define-test "parses (list ...) as symbols"
    (let ([r (rlm2-parse-response "(list \"matrix\")")])
      (assert-true (rlm2-symbols? (rlm2-parse-result-action r)))
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'list (car ai))
        (assert-equal 'symbols (cdr ai)))))

  (define-test "canonical actions have no alias info"
    (let ([r (rlm2-parse-response "(search \"eigenvalue\")")])
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))
      (assert-false (rlm2-parse-result-alias-info r))))

  (define-test "alias resolves inside begin via fuzzy path"
    (let ([r (rlm2-parse-response "I think I should:\n(begin (find \"linalg\") (answer 42))")])
      (let ([action (rlm2-parse-result-action r)])
        ;; Should be a begin with search and submit
        (assert-true (rlm2-begin? action))
        (assert-true (rlm2-search? (car (rlm2-begin-actions action))))
        (assert-true (rlm2-submit? (cadr (rlm2-begin-actions action)))))))

  (define-test "alias resolves in direct begin parse"
    (let ([r (rlm2-parse-response "(begin (find \"tools\") (run (+ 1 2)))")])
      (let ([action (rlm2-parse-result-action r)])
        (assert-true (rlm2-begin? action))
        (assert-true (rlm2-search? (car (rlm2-begin-actions action))))
        (assert-true (rlm2-eval? (cadr (rlm2-begin-actions action))))
        ;; alias info present
        (let ([ai (rlm2-parse-result-alias-info r)])
          (assert-true (pair? ai))))))

  (define-test "wrong arity alias still fails"
    ;; (find "a" "b") -> (search "a" "b") -> arity error -> think fallback
    (let ([r (rlm2-parse-response "(find \"a\" \"b\")")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))

  (define-test "unknown action with wrong alias target still fails"
    ;; completely unknown action, not in alias table
    (let ([r (rlm2-parse-response "(frobulate 42)")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))
)

;;; ====
;;; Alias Correction Note
;;; ====

(test-group "rlm2-alias-correction-note"

  (define-test "formats correction note for find->search"
    (let ([note (rlm2-alias-correction-note (cons 'find 'search))])
      (assert-true (string? note))
      ;; Contains both the alias and canonical name
      (assert-true (> (string-length note) 0))
      ;; Check it mentions the canonical action
      (let ([has-search? (let loop ([i 0])
                           (cond
                             [(> (+ i 6) (string-length note)) #f]
                             [(string=? (substring note i (+ i 6)) "search") #t]
                             [else (loop (+ i 1))]))])
        (assert-true has-search?))))

  (define-test "formats correction note for answer->submit"
    (let ([note (rlm2-alias-correction-note (cons 'answer 'submit))])
      (assert-true (string? note))
      (let ([has-submit? (let loop ([i 0])
                           (cond
                             [(> (+ i 6) (string-length note)) #f]
                             [(string=? (substring note i (+ i 6)) "submit") #t]
                             [else (loop (+ i 1))]))])
        (assert-true has-submit?))))
)

;;; ====
;;; Parse Result Alias Info Accessor
;;; ====

(test-group "rlm2-parse-result-alias-accessors"

  (define-test "alias-info is #f for normal parse results"
    (let ([r (make-rlm2-parse-result '(search "q") #f "(search \"q\")")])
      (assert-false (rlm2-parse-result-alias-info r))))

  (define-test "alias-info is accessible when set"
    (let ([r (make-rlm2-parse-result '(search "q") #f "(find \"q\")"
                                     #f #f (cons 'find 'search))])
      (let ([ai (rlm2-parse-result-alias-info r)])
        (assert-equal 'find (car ai))
        (assert-equal 'search (cdr ai)))))

  (define-test "parse-result-with-alias sets alias info"
    (let* ([r0 (make-rlm2-parse-result '(search "q") #f "(find \"q\")")]
           [r1 (rlm2-parse-result-with-alias r0 (cons 'find 'search))])
      (assert-false (rlm2-parse-result-alias-info r0))
      (let ([ai (rlm2-parse-result-alias-info r1)])
        (assert-equal 'find (car ai))
        (assert-equal 'search (cdr ai)))
      ;; Original fields preserved
      (assert-equal '(search "q") (rlm2-parse-result-action r1))
      (assert-false (rlm2-parse-result-thought r1))
      (assert-equal "(find \"q\")" (rlm2-parse-result-raw r1))))
)

(run-all-tests)
