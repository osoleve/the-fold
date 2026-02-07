;;; lattice/pipeline/test-rlm2.ss — Tests for pure RLM v2 types, parser, and HUD
;;;
;;; Tests state constructors/accessors, action validation, parser (clean/fuzzy/garbage),
;;; and HUD rendering. All pure, no IO.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/rlm2.ss")
(load "lattice/pipeline/rlm2-parse.ss")
(load "lattice/pipeline/rlm2-hud.ss")

;;; ====
;;; Phase 1: State & Action Types
;;; ====

(test-group "rlm2-state"

  (define-test "make-rlm2-state creates tagged state"
    (let ([s (make-rlm2-state "task" '() '() '() '() '() #f 1000 0)])
      (assert-true (rlm2-state? s))
      (assert-equal "task" (rlm2-state-task s))
      (assert-equal '() (rlm2-state-plan s))
      (assert-equal '() (rlm2-state-env s))
      (assert-equal '() (rlm2-state-loaded s))
      (assert-equal '() (rlm2-state-notes s))
      (assert-equal '() (rlm2-state-episodic s))
      (assert-false (rlm2-state-last-result s))
      (assert-equal 1000 (rlm2-state-fuel s))
      (assert-equal 0 (rlm2-state-step s))))

  (define-test "make-initial-rlm2-state has defaults"
    (let ([s (make-initial-rlm2-state "solve x^2=4" "x^2=4" 5000)])
      (assert-true (rlm2-state? s))
      (assert-equal "solve x^2=4" (rlm2-state-task s))
      (assert-equal "x^2=4" (rlm2-state-last-result s))
      (assert-equal 5000 (rlm2-state-fuel s))
      (assert-equal 0 (rlm2-state-step s))
      (assert-equal '() (rlm2-state-plan s))
      (assert-equal '() (rlm2-state-notes s))))

  (define-test "state-with-plan replaces plan"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [plan '((search . pending) (compute . pending))]
           [s* (rlm2-state-with-plan s plan)])
      (assert-equal plan (rlm2-state-plan s*))
      (assert-equal "t" (rlm2-state-task s*))))

  (define-test "state-add-note appends"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [s1 (rlm2-state-add-note s "first note")]
           [s2 (rlm2-state-add-note s1 "second note")])
      (assert-equal '("first note") (rlm2-state-notes s1))
      (assert-equal '("first note" "second note") (rlm2-state-notes s2))))

  (define-test "state-with-notes replaces for compaction"
    (let* ([s (rlm2-state-add-note
               (rlm2-state-add-note
                (make-initial-rlm2-state "t" #f 100)
                "old1")
               "old2")]
           [s* (rlm2-state-with-notes s '("compacted summary"))])
      (assert-equal '("compacted summary") (rlm2-state-notes s*))))

  (define-test "state-add-episodic appends"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [s1 (rlm2-state-add-episodic s 0 "hash0")]
           [s2 (rlm2-state-add-episodic s1 1 "hash1")])
      (assert-equal '((0 . "hash0")) (rlm2-state-episodic s1))
      (assert-equal '((0 . "hash0") (1 . "hash1")) (rlm2-state-episodic s2))))

  (define-test "state-use-fuel decrements"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [s* (rlm2-state-use-fuel s 300)])
      (assert-equal 700 (rlm2-state-fuel s*))))

  (define-test "state-use-fuel floors at zero"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [s* (rlm2-state-use-fuel s 500)])
      (assert-equal 0 (rlm2-state-fuel s*))))

  (define-test "state-advance-step increments"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [s* (rlm2-state-advance-step s)])
      (assert-equal 1 (rlm2-state-step s*))))

  (define-test "state-with-loaded replaces"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [s* (rlm2-state-with-loaded s '(linalg/vec linalg/matrix))])
      (assert-equal '(linalg/vec linalg/matrix) (rlm2-state-loaded s*))))

  (define-test "state-with-env replaces"
    (let* ([s (make-initial-rlm2-state "t" #f 100)]
           [env '((input text 2400 "hash1"))]
           [s* (rlm2-state-with-env s env)])
      (assert-equal env (rlm2-state-env s*))))

  (define-test "state-with-last-result replaces"
    (let* ([s (make-initial-rlm2-state "t" "old" 100)]
           [s* (rlm2-state-with-last-result s "new")])
      (assert-equal "new" (rlm2-state-last-result s*))))
)

(test-group "rlm2-completion"

  (define-test "state-complete? is false initially"
    (assert-false (rlm2-state-complete?
                   (make-initial-rlm2-state "t" #f 100))))

  (define-test "state-complete? detects rlm2-result"
    (let ([s (rlm2-state-with-last-result
              (make-initial-rlm2-state "t" #f 100)
              (make-rlm2-result 42))])
      (assert-true (rlm2-state-complete? s))
      (assert-equal 42 (rlm2-state-result s))))

  (define-test "state-exhausted? on fuel"
    (let ([s (rlm2-state-use-fuel
              (make-initial-rlm2-state "t" #f 100) 100)]
          [cfg (make-rlm2-config-default 'p "sys")])
      (assert-true (rlm2-state-exhausted? s cfg))))

  (define-test "state-exhausted? on steps"
    (let* ([cfg (make-rlm2-config 'p "sys" 2 50000 2000 2 3 8000 #f)]
           [s (rlm2-state-advance-step
               (rlm2-state-advance-step
                (make-initial-rlm2-state "t" #f 50000)))])
      (assert-true (rlm2-state-exhausted? s cfg))))

  (define-test "state-exhausted? false when resources remain"
    (let ([s (make-initial-rlm2-state "t" #f 50000)]
          [cfg (make-rlm2-config-default 'p "sys")])
      (assert-false (rlm2-state-exhausted? s cfg))))
)

(test-group "rlm2-config"

  (define-test "make-rlm2-config creates valid config"
    (let ([cfg (make-rlm2-config 'dummy "sys" 10 5000 2000 2 3 8000 #f)])
      (assert-true (rlm2-config? cfg))
      (assert-equal 'dummy (rlm2-config-provider cfg))
      (assert-equal "sys" (rlm2-config-system-prompt cfg))
      (assert-equal 10 (rlm2-config-max-steps cfg))
      (assert-equal 5000 (rlm2-config-max-fuel cfg))
      (assert-equal 2000 (rlm2-config-chunk-size cfg))
      (assert-equal 2 (rlm2-config-max-depth cfg))
      (assert-equal 3 (rlm2-config-loop-window cfg))
      (assert-equal 8000 (rlm2-config-context-budget cfg))
      (assert-false (rlm2-config-verifier cfg))))

  (define-test "make-rlm2-config-default has sensible values"
    (let ([cfg (make-rlm2-config-default 'p "sys")])
      (assert-equal 20 (rlm2-config-max-steps cfg))
      (assert-equal 50000 (rlm2-config-max-fuel cfg))
      (assert-equal 8000 (rlm2-config-context-budget cfg))
      (assert-false (rlm2-config-verifier cfg))))
)

;;; ====
;;; Action Types & Validation
;;; ====

(test-group "rlm2-actions"

  (define-test "rlm2-action? accepts valid actions"
    (assert-true (rlm2-action? '(search "eigenvalue")))
    (assert-true (rlm2-action? '(inspect linalg)))
    (assert-true (rlm2-action? '(exports linalg)))
    (assert-true (rlm2-action? '(load linalg/vec)))
    (assert-true (rlm2-action? '(eval (+ 1 2))))
    (assert-true (rlm2-action? '(store x (+ 1 2))))
    (assert-true (rlm2-action? '(retrieve x)))
    (assert-true (rlm2-action? '(peek input 200)))
    (assert-true (rlm2-action? '(grep input "pattern" 5)))
    (assert-true (rlm2-action? '(slice input 0 3)))
    (assert-true (rlm2-action? '(recall-step 2)))
    (assert-true (rlm2-action? '(submit 42)))
    (assert-true (rlm2-action? '(think "hmm")))
    (assert-true (rlm2-action? '(plan! ((a . pending)))))
    (assert-true (rlm2-action? '(map-chunks input "(split-lines *chunk*)"))))

  (define-test "rlm2-action? rejects non-actions"
    (assert-false (rlm2-action? 42))
    (assert-false (rlm2-action? "hello"))
    (assert-false (rlm2-action? '(unknown-action foo)))
    (assert-false (rlm2-action? '())))

  (define-test "rlm2-action-type extracts tag"
    (assert-equal 'search (rlm2-action-type '(search "query")))
    (assert-equal 'begin (rlm2-action-type '(begin (think "x") (search "y")))))

  (define-test "per-type predicates work"
    (assert-true (rlm2-search? '(search "q")))
    (assert-false (rlm2-search? '(inspect x)))
    (assert-true (rlm2-begin? '(begin (search "q"))))
    (assert-true (rlm2-think? '(think "hmm")))
    (assert-true (rlm2-submit? '(submit 42)))
    (assert-true (rlm2-plan!? '(plan! ((a . done)))))
    (assert-true (rlm2-map-chunks? '(map-chunks input "expr")))
    (assert-false (rlm2-map-chunks? '(eval 42))))

  (define-test "per-type accessors work"
    (assert-equal "eigenvalue" (rlm2-search-query '(search "eigenvalue")))
    (assert-equal 'linalg (rlm2-inspect-skill '(inspect linalg)))
    (assert-equal 'linalg/vec (rlm2-load-module '(load linalg/vec)))
    (assert-equal '(+ 1 2) (rlm2-eval-expr '(eval (+ 1 2))))
    (assert-equal 'x (rlm2-store-key '(store x (+ 1 2))))
    (assert-equal '(+ 1 2) (rlm2-store-expr '(store x (+ 1 2))))
    (assert-equal 'input (rlm2-peek-key '(peek input 200)))
    (assert-equal 200 (rlm2-peek-n '(peek input 200)))
    (assert-equal "pattern" (rlm2-grep-pattern '(grep input "pattern" 5)))
    (assert-equal 5 (rlm2-grep-k '(grep input "pattern" 5)))
    (assert-equal 0 (rlm2-slice-start '(slice input 0 3)))
    (assert-equal 3 (rlm2-slice-end '(slice input 0 3)))
    (assert-equal 2 (rlm2-recall-step-n '(recall-step 2)))
    (assert-equal 42 (rlm2-submit-expr '(submit 42)))
    (assert-equal "hmm" (rlm2-think-text '(think "hmm")))
    (assert-equal 'input (rlm2-map-chunks-key '(map-chunks input "(split-lines *chunk*)")))
    (assert-equal "(split-lines *chunk*)" (rlm2-map-chunks-expr '(map-chunks input "(split-lines *chunk*)"))))

  (define-test "begin-actions returns children"
    (let ([a '(begin (search "q") (load linalg))])
      (assert-equal 2 (length (rlm2-begin-actions a)))
      (assert-true (rlm2-search? (car (rlm2-begin-actions a))))
      (assert-true (rlm2-load? (cadr (rlm2-begin-actions a))))))
)

(test-group "rlm2-validation"

  (define-test "validate-action accepts valid fixed-arity"
    (let ([r (rlm2-validate-action '(search "q"))])
      (assert-true (rlm2-validation-ok? r))
      (assert-equal '(search "q") (rlm2-validation-value r))))

  (define-test "validate-action rejects wrong arity"
    (let ([r (rlm2-validate-action '(search "a" "b"))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action rejects missing args"
    (let ([r (rlm2-validate-action '(peek input))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action rejects unknown type"
    (let ([r (rlm2-validate-action '(foobar 42))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action rejects non-list"
    (let ([r (rlm2-validate-action 42)])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action rejects non-symbol head"
    (let ([r (rlm2-validate-action '(42 "hello"))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action accepts begin with valid children"
    (let ([r (rlm2-validate-action '(begin (think "x") (search "y")))])
      (assert-true (rlm2-validation-ok? r))))

  (define-test "validate-action rejects begin with invalid child"
    (let ([r (rlm2-validate-action '(begin (think "x") (search "a" "b")))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action rejects empty begin"
    (let ([r (rlm2-validate-action '(begin))])
      (assert-false (rlm2-validation-ok? r))))

  (define-test "validate-action accepts nested begin"
    (let ([r (rlm2-validate-action '(begin (begin (search "a") (search "b"))))])
      (assert-true (rlm2-validation-ok? r))))

  (define-test "validate-action accepts map-chunks"
    (let ([r (rlm2-validate-action '(map-chunks input "(split-lines *chunk*)"))])
      (assert-true (rlm2-validation-ok? r))))

  (define-test "validate-action rejects map-chunks wrong arity"
    (let ([r (rlm2-validate-action '(map-chunks input))])
      (assert-false (rlm2-validation-ok? r))))
)

;;; ====
;;; Step Record & Trajectory
;;; ====

(test-group "rlm2-step-record"

  (define-test "make-rlm2-step-record creates valid record"
    (let ([r (make-rlm2-step-record 0 '(search "q")
               (make-rlm2-observation 'search "q" "results..." #t)
               "Found linalg tools" "statehash")])
      (assert-true (rlm2-step-record? r))
      (assert-equal 0 (rlm2-step-record-num r))
      (assert-equal '(search "q") (rlm2-step-record-action r))
      (assert-equal "Found linalg tools" (rlm2-step-record-note r))
      (assert-equal "statehash" (rlm2-step-record-state-hash r))))
)

(test-group "rlm2-observation"

  (define-test "make-rlm2-observation creates valid observation"
    (let ([o (make-rlm2-observation 'search "eigenvalue" "results" #t)])
      (assert-true (rlm2-observation? o))
      (assert-equal 'search (rlm2-observation-action-type o))
      (assert-equal "eigenvalue" (rlm2-observation-target o))
      (assert-equal "results" (rlm2-observation-value o))
      (assert-true (rlm2-observation-ok? o))))
)

(test-group "rlm2-act-result"

  (define-test "make-rlm2-act-result packages thought and action"
    (let ([r (make-rlm2-act-result "thinking..." '(search "q"))])
      (assert-true (rlm2-act-result? r))
      (assert-equal "thinking..." (rlm2-act-result-thought r))
      (assert-equal '(search "q") (rlm2-act-result-action r))))
)

;;; ====
;;; Loop Detection
;;; ====

(test-group "rlm2-loop-detection"

  (define-test "no loop in empty history"
    (let ([s (make-initial-rlm2-state "t" #f 100)])
      (assert-false (rlm2-loop-detected? '() (rlm2-semantic-fingerprint s 'search) 3))))

  (define-test "detects repeated fingerprint"
    (let* ([s (rlm2-state-with-plan
               (make-initial-rlm2-state "t" #f 100)
               '((a . pending)))]
           [fp (rlm2-semantic-fingerprint s 'search)])
      (assert-true (rlm2-loop-detected? (list fp "other" "other2") fp 3))))

  (define-test "respects window size"
    (let* ([s (rlm2-state-with-plan
               (make-initial-rlm2-state "t" #f 100)
               '((a . pending)))]
           [fp (rlm2-semantic-fingerprint s 'search)])
      ;; fp is at position 0, but window of 1 only checks position 0 (the first element)
      (assert-true (rlm2-loop-detected? (list fp "b" "c") fp 1))
      ;; fp is past the window
      (assert-false (rlm2-loop-detected? (list "a" "b" fp) fp 2))))

  (define-test "plan change breaks fingerprint"
    (let* ([s1 (rlm2-state-with-plan
                (make-initial-rlm2-state "t" #f 100)
                '((a . pending)))]
           [s2 (rlm2-state-with-plan
                (make-initial-rlm2-state "t" #f 100)
                '((a . done)))]
           [fp1 (rlm2-semantic-fingerprint s1 'search)]
           [fp2 (rlm2-semantic-fingerprint s2 'search)])
      (assert-false (string=? fp1 fp2))))
)

;;; ====
;;; Phase 2: Parser Tests
;;; ====

(test-group "rlm2-parse-clean"

  (define-test "parses clean search action"
    (let ([r (rlm2-parse-response "(search \"eigenvalue\")")])
      (assert-true (rlm2-parse-result? r))
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))
      (assert-equal "eigenvalue" (rlm2-search-query (rlm2-parse-result-action r)))
      (assert-false (rlm2-parse-result-thought r))))

  (define-test "parses clean begin with think"
    (let* ([r (rlm2-parse-response "(begin (think \"hmm\") (search \"eigen\"))")]
           [action (rlm2-parse-result-action r)]
           [thought (rlm2-parse-result-thought r)])
      (assert-true (rlm2-search? action))
      (assert-equal "hmm" thought)))

  (define-test "parses multi-action begin"
    (let* ([r (rlm2-parse-response "(begin (think \"x\") (load linalg) (search \"y\"))")]
           [action (rlm2-parse-result-action r)]
           [thought (rlm2-parse-result-thought r)])
      ;; think is separated, remaining actions form a begin
      (assert-true (rlm2-begin? action))
      (assert-equal 2 (length (rlm2-begin-actions action)))
      (assert-equal "x" thought)))

  (define-test "parses submit"
    (let ([r (rlm2-parse-response "(submit 42)")])
      (assert-true (rlm2-submit? (rlm2-parse-result-action r)))
      (assert-equal 42 (rlm2-submit-expr (rlm2-parse-result-action r)))))

  (define-test "parses plan!"
    (let ([r (rlm2-parse-response "(plan! ((search . pending) (compute . pending)))")])
      (assert-true (rlm2-plan!? (rlm2-parse-result-action r)))))

  (define-test "parses map-chunks"
    (let ([r (rlm2-parse-response "(map-chunks 'input \"(split-lines *chunk*)\")")])
      (assert-true (rlm2-map-chunks? (rlm2-parse-result-action r)))
      (assert-equal 'input (rlm2-map-chunks-key (rlm2-parse-result-action r)))
      (assert-equal "(split-lines *chunk*)" (rlm2-map-chunks-expr (rlm2-parse-result-action r)))))

  (define-test "parses eval with nested expression"
    (let ([r (rlm2-parse-response "(eval (matrix-eigen (retrieve 'input)))")])
      (assert-true (rlm2-eval? (rlm2-parse-result-action r)))))

  (define-test "parses think alone"
    (let ([r (rlm2-parse-response "(think \"I need to figure this out\")")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))
      (assert-equal "I need to figure this out"
                    (rlm2-parse-result-thought r))))

  (define-test "handles whitespace around action"
    (let ([r (rlm2-parse-response "  \n  (search \"q\")  \n  ")])
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))))
)

(test-group "rlm2-parse-fuzzy"

  (define-test "extracts action from noisy output"
    (let ([r (rlm2-parse-response "I think I should search for eigenvalue tools.\n(search \"eigenvalue\")\nThat should work.")])
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))
      ;; Pre-text becomes thought
      (assert-true (string? (rlm2-parse-result-thought r)))))

  (define-test "extracts begin from preamble"
    (let ([r (rlm2-parse-response "Let me try:\n(begin (think \"hmm\") (search \"q\"))")])
      (assert-true (rlm2-search? (rlm2-parse-result-action r)))
      ;; thought includes both preamble and explicit think
      (let ([t (rlm2-parse-result-thought r)])
        (assert-true (string? t)))))

  (define-test "falls back to think on invalid action after extraction"
    (let ([r (rlm2-parse-response "I want to (foobar something) here")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))
)

(test-group "rlm2-parse-garbage"

  (define-test "empty input becomes think"
    (let ([r (rlm2-parse-response "")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))

  (define-test "pure prose becomes think"
    (let ([r (rlm2-parse-response "I'm not sure what to do next. Let me think about it.")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))

  (define-test "unclosed paren becomes think"
    (let ([r (rlm2-parse-response "(search \"eigenvalue")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))

  (define-test "wrong arity after extraction becomes think"
    (let ([r (rlm2-parse-response "try this: (search \"a\" \"b\" \"c\")")])
      (assert-true (rlm2-think? (rlm2-parse-result-action r)))))
)

(test-group "rlm2-parse-balanced-extraction"

  (define-test "extract-balanced finds first sexp"
    (let ([r (rlm2-extract-balanced "foo (bar baz) quux")])
      (assert-equal "(bar baz)" r)))

  (define-test "extract-balanced handles nesting"
    (let ([r (rlm2-extract-balanced "x (a (b c) d) y")])
      (assert-equal "(a (b c) d)" r)))

  (define-test "extract-balanced handles strings"
    (let ([r (rlm2-extract-balanced "x (search \"hello world\") y")])
      (assert-equal "(search \"hello world\")" r)))

  (define-test "extract-balanced returns #f on unclosed"
    (assert-false (rlm2-extract-balanced "x (search \"hello")))

  (define-test "extract-balanced returns #f on no parens"
    (assert-false (rlm2-extract-balanced "no parens here")))

  (define-test "extract-balanced handles escaped quotes in strings"
    (let ([r (rlm2-extract-balanced "(think \"he said \\\"hello\\\"\")")])
      (assert-equal "(think \"he said \\\"hello\\\"\")" r)))
)

;;; ====
;;; Test helper (must be defined before HUD tests)
;;; ====

;;; Simple substring search — returns index or -1
(define (rlm2-hud-contains? text needle)
  (let ([tlen (string-length text)]
        [nlen (string-length needle)])
    (if (> nlen tlen)
        -1
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) tlen) -1]
            [(string=? (substring text i (+ i nlen)) needle) i]
            [else (loop (+ i 1))])))))

;;; ====
;;; Phase 3: HUD Renderer Tests
;;; ====

(test-group "rlm2-hud-render"

  (define-test "renders minimal state"
    (let* ([s (make-initial-rlm2-state "Find eigenvalues" #f 5000)]
           [hud (rlm2-render-state s 4000)])
      ;; Should contain key sections
      (assert-true (string? hud))
      (assert-true (> (string-length hud) 0))
      ;; Check for section markers
      (assert-true (pair? (member #\( (string->list hud))))
      ;; Check task is present
      (assert-true (>= (rlm2-hud-contains? hud "Find eigenvalues") 0))))

  (define-test "renders plan section"
    (let* ([s (rlm2-state-with-plan
              (make-initial-rlm2-state "t" #f 5000)
              '((search . done) (compute . pending)))]
           [hud (rlm2-render-state s 4000)])
      (assert-true (>= (rlm2-hud-contains? hud "search") 0))
      (assert-true (>= (rlm2-hud-contains? hud "pending") 0))))

  (define-test "renders env section with metadata only"
    (let* ([s (rlm2-state-with-env
              (make-initial-rlm2-state "t" #f 5000)
              '((input text 2400 "hash1") (result sexpr 50 "hash2")))]
           [hud (rlm2-render-state s 4000)])
      ;; Shows key and type and size, NOT hash
      (assert-true (>= (rlm2-hud-contains? hud "input") 0))
      (assert-true (>= (rlm2-hud-contains? hud "2400") 0))))

  (define-test "renders loaded modules"
    (let* ([s (rlm2-state-with-loaded
              (make-initial-rlm2-state "t" #f 5000)
              '(linalg/vec linalg/matrix))]
           [hud (rlm2-render-state s 4000)])
      (assert-true (>= (rlm2-hud-contains? hud "linalg/vec") 0))))

  (define-test "renders notes in order"
    (let* ([s (rlm2-state-add-note
              (rlm2-state-add-note
               (make-initial-rlm2-state "t" #f 5000)
               "Input is 50x50 matrix")
              "linalg has eigen tools")]
           [hud (rlm2-render-state s 8000)])
      (assert-true (>= (rlm2-hud-contains? hud "50x50") 0))
      (assert-true (>= (rlm2-hud-contains? hud "eigen") 0))))

  (define-test "renders fuel and step"
    (let* ([s (rlm2-state-advance-step
              (rlm2-state-use-fuel
               (make-initial-rlm2-state "t" #f 5000) 200))]
           [hud (rlm2-render-state s 4000)])
      (assert-true (>= (rlm2-hud-contains? hud "4800") 0))
      (assert-true (>= (rlm2-hud-contains? hud "step 1") 0))))

  (define-test "truncates notes under tight budget"
    (let* ([notes (map (lambda (i) (format "Note ~a: This is a detailed observation about step ~a" i i))
                       '(1 2 3 4 5 6 7 8 9 10))]
           [s (rlm2-state-with-notes
               (make-initial-rlm2-state "t" #f 5000)
               notes)]
           ;; Very tight budget
           [hud (rlm2-render-state s 500)])
      ;; Should include truncation marker
      (assert-true (>= (rlm2-hud-contains? hud "omitted") 0))))

  (define-test "respects large budget without truncation"
    (let* ([s (rlm2-state-add-note
              (make-initial-rlm2-state "t" #f 5000)
              "A short note")]
           [hud (rlm2-render-state s 10000)])
      ;; No omission marker with large budget
      (assert-false (>= (rlm2-hud-contains? hud "omitted") 0))))
)

(test-group "rlm2-system-prompt"

  (define-test "build-system-prompt includes action grammar"
    (let ([prompt (rlm2-build-system-prompt "You are a math solver.")])
      (assert-true (>= (rlm2-hud-contains? prompt "search query") 0))
      (assert-true (>= (rlm2-hud-contains? prompt "submit expr") 0))
      (assert-true (>= (rlm2-hud-contains? prompt "plan! items") 0))
      (assert-true (>= (rlm2-hud-contains? prompt "You are a math solver.") 0))))
)

(test-group "rlm2-reflection-prompt"

  (define-test "build-reflection-prompt includes all parts"
    (let ([prompt (rlm2-build-reflection-prompt
                   "I think eigenvalues are needed"
                   '(search "eigenvalue")
                   '(ok "linalg (0.87): decomposition"))])
      (assert-true (>= (rlm2-hud-contains? prompt "eigenvalues") 0))
      (assert-true (>= (rlm2-hud-contains? prompt "search") 0))
      (assert-true (>= (rlm2-hud-contains? prompt "linalg") 0))))

  (define-test "build-reflection-prompt handles no thought"
    (let ([prompt (rlm2-build-reflection-prompt
                   #f
                   '(load linalg/vec)
                   '(ok "loaded"))])
      ;; Should not crash, should still have action and result
      (assert-true (>= (rlm2-hud-contains? prompt "load") 0))))
)

;;; ====
;;; QA Fix Tests: Quoted arg normalization, non-pair plan guards
;;; ====

(test-group "rlm2-unquote-normalization"

  (define-test "rlm2-unquote strips quote wrapper"
    (assert-equal 'foo (rlm2-unquote '(quote foo)))
    (assert-equal 'bar (rlm2-unquote '(quote bar))))

  (define-test "rlm2-unquote passes through bare symbols"
    (assert-equal 'x (rlm2-unquote 'x))
    (assert-equal 42 (rlm2-unquote 42)))

  (define-test "accessors unquote parsed actions"
    ;; After (read "(retrieve 'input)"), the arg is (quote input)
    (assert-equal 'input (rlm2-retrieve-key '(retrieve (quote input))))
    (assert-equal 'linalg (rlm2-inspect-skill '(inspect (quote linalg))))
    (assert-equal 'linalg (rlm2-exports-skill '(exports (quote linalg))))
    (assert-equal 'vec (rlm2-load-module '(load (quote vec))))
    (assert-equal 'x (rlm2-store-key '(store (quote x) (+ 1 2))))
    (assert-equal 'data (rlm2-peek-key '(peek (quote data) 100)))
    (assert-equal 'text (rlm2-grep-key '(grep (quote text) "pattern" 5)))
    (assert-equal 'input (rlm2-slice-key '(slice (quote input) 0 3))))

  (define-test "accessors work with bare symbols too"
    (assert-equal 'input (rlm2-retrieve-key '(retrieve input)))
    (assert-equal 'linalg (rlm2-inspect-skill '(inspect linalg))))

  (define-test "expr accessors preserve quote forms"
    ;; eval, store-expr, submit-expr should NOT unquote
    (assert-equal '(+ 1 2) (rlm2-eval-expr '(eval (+ 1 2))))
    (assert-equal '(+ 1 2) (rlm2-store-expr '(store x (+ 1 2))))
    (assert-equal '(quote foo) (rlm2-submit-expr '(submit (quote foo)))))
)

(test-group "rlm2-non-pair-plan-guards"

  (define-test "fingerprint survives non-pair plan items"
    (let ([s (rlm2-state-with-plan
               (make-initial-rlm2-state "t" #f 100)
               '("search for tools" "compute answer"))])
      ;; Should not crash
      (assert-true (string? (rlm2-semantic-fingerprint s 'search)))))

  (define-test "fingerprint survives mixed plan items"
    (let ([s (rlm2-state-with-plan
               (make-initial-rlm2-state "t" #f 100)
               '((search . done) "orphan item" (compute . pending)))])
      (assert-true (string? (rlm2-semantic-fingerprint s 'eval)))))

  (define-test "HUD renders non-pair plan items without crash"
    (let* ([s (rlm2-state-with-plan
                (make-initial-rlm2-state "t" #f 5000)
                '("just a string" (real . pending)))]
           [hud (rlm2-render-state s 4000)])
      (assert-true (string? hud))
      (assert-true (>= (rlm2-hud-contains? hud "pending") 0))))
)

(run-all-tests)
