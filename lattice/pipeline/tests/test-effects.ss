;;; lattice/pipeline/tests/test-effects.ss — Comprehensive Tests for Pipeline Effects
;;;
;;; Tests the effect construction and predicates.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

;;; ====
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; LLM Effects
;;; ====

(test-group "LLM Effects"
  (define-test "llm creates stage"
    (let ([eff (llm 'opus "Analyze: ${input}")])
      (assert-true (stage? eff))))

  (define-test "llm produces effect"
    (let* ([eff (llm 'opus "Analyze: ${input}")]
           [result (run-stage eff test-ctx "test input")])
      (assert-true (stage-effect? result))))

  (define-test "llm effect type is llm"
    (let* ([eff (llm 'opus "Analyze: ${input}")]
           [result (run-stage eff test-ctx "test input")])
      (assert-equal 'llm (stage-effect-type result))))

  (define-test "llm effect has call payload"
    (let* ([eff (llm 'opus "Analyze: ${input}")]
           [result (run-stage eff test-ctx "test input")])
      (assert-equal (list 'call 'opus "Analyze: ${input}")
                    (stage-effect-payload result))))

  (define-test "llm-with-system has call-with-system payload"
    (let* ([eff (llm-with-system 'sonnet "You are helpful" "Help me")]
           [result (run-stage eff test-ctx "test")])
      (assert-equal (list 'call-with-system 'sonnet "You are helpful" "Help me")
                    (stage-effect-payload result))))

  (define-test "llm-json has call-json payload"
    (let* ([eff (llm-json 'haiku "Return JSON")]
           [result (run-stage eff test-ctx "test")])
      (assert-equal (list 'call-json 'haiku "Return JSON")
                    (stage-effect-payload result))))

  (define-test "llm-structured has call-structured payload"
    (let* ([eff (llm-structured 'opus "Parse user" '((name . string) (age . number)))]
           [result (run-stage eff test-ctx "test")])
      (assert-equal (list 'call-structured 'opus "Parse user" '((name . string) (age . number)))
                    (stage-effect-payload result)))))

;;; ====
;;; Fold Effects
;;; ====

(test-group "Fold Effects"
  (define-test "fold-eval produces effect"
    (let* ([eff (fold-eval "(+ 1 2)")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "fold-eval effect type is fold"
    (let* ([eff (fold-eval "(+ 1 2)")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'fold (stage-effect-type result))))

  (define-test "fold-eval has eval payload"
    (let* ([eff (fold-eval "(+ 1 2)")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'eval "(+ 1 2)")
                    (stage-effect-payload result))))

  (define-test "fold-call has call payload"
    (let* ([eff (fold-call 'my-fn '(1 2 3))]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'call 'my-fn '(1 2 3))
                    (stage-effect-payload result))))

  (define-test "fold-load has load payload"
    (let* ([eff (fold-load "path/to/file.ss")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'load "path/to/file.ss")
                    (stage-effect-payload result))))

  (define-test "forum-post has forum-post payload"
    (let* ([eff (forum-post 'engineering "Title" "Body")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'forum-post 'engineering "Title" "Body")
                    (stage-effect-payload result))))

  (define-test "forum-digest has call digest-posts payload"
    (let ([result (run-stage forum-digest test-ctx "input")])
      (assert-equal (list 'call 'digest-posts '())
                    (stage-effect-payload result))))

  (define-test "forum-browse has call browse payload"
    (let* ([eff (forum-browse 'design 10)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'call 'browse (list 'design 10))
                    (stage-effect-payload result)))))

;;; ====
;;; Shell Effects
;;; ====

(test-group "Shell Effects"
  (define-test "shell produces effect"
    (let* ([eff (shell "ls -la")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "shell effect type is shell"
    (let* ([eff (shell "ls -la")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'shell (stage-effect-type result))))

  (define-test "shell has run payload"
    (let* ([eff (shell "ls -la")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'run "ls -la")
                    (stage-effect-payload result))))

  (define-test "shell-with-stdin has run-stdin payload"
    (let* ([eff (shell-with-stdin "grep pattern")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'run-stdin "grep pattern")
                    (stage-effect-payload result))))

  (define-test "shell-env has run-env payload"
    (let* ([eff (shell-env '(("PATH" . "/usr/bin")) "echo $PATH")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'run-env '(("PATH" . "/usr/bin")) "echo $PATH")
                    (stage-effect-payload result))))

  (define-test "shell-bg has run-bg payload"
    (let* ([eff (shell-bg "long-running-task")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'run-bg "long-running-task")
                    (stage-effect-payload result)))))

;;; ====
;;; Store Effects
;;; ====

(test-group "Store Effects"
  (define-test "store-put produces effect"
    (let ([result (run-stage store-put test-ctx "some value")])
      (assert-true (stage-effect? result))))

  (define-test "store-put effect type is store"
    (let ([result (run-stage store-put test-ctx "some value")])
      (assert-equal 'store (stage-effect-type result))))

  (define-test "store-put has put payload"
    (let ([result (run-stage store-put test-ctx "some value")])
      (assert-equal (list 'put)
                    (stage-effect-payload result))))

  (define-test "store-get has get payload"
    (let* ([eff (store-get "abc123")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'get "abc123")
                    (stage-effect-payload result))))

  (define-test "store-has has has payload"
    (let* ([eff (store-has "def456")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'has "def456")
                    (stage-effect-payload result))))

  (define-test "store-pin has pin payload"
    (let* ([eff (store-pin "ghi789")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'pin "ghi789")
                    (stage-effect-payload result)))))

;;; ====
;;; Log Effects
;;; ====

(test-group "Log Effects"
  (define-test "log-info produces effect"
    (let* ([eff (log-info "Info message")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "log-info effect type is log"
    (let* ([eff (log-info "Info message")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'log (stage-effect-type result))))

  (define-test "log-info has info payload"
    (let* ([eff (log-info "Info message")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'info "Info message")
                    (stage-effect-payload result))))

  (define-test "log-warn has warn payload"
    (let* ([eff (log-warn "Warning message")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'warn "Warning message")
                    (stage-effect-payload result))))

  (define-test "log-error has error payload"
    (let* ([eff (log-error "Error message")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'error "Error message")
                    (stage-effect-payload result))))

  (define-test "log-debug has debug payload"
    (let* ([eff (log-debug "Debug message")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'debug "Debug message")
                    (stage-effect-payload result))))

  (define-test "log-metric has metric payload"
    (let* ([eff (log-metric 'latency 42.5)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'metric 'latency 42.5)
                    (stage-effect-payload result)))))

;;; ====
;;; Checkpoint Effects
;;; ====

(test-group "Checkpoint Effects"
  (define-test "checkpoint produces effect"
    (let* ([eff (checkpoint 'my-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "checkpoint effect type is checkpoint"
    (let* ([eff (checkpoint 'my-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'checkpoint (stage-effect-type result))))

  (define-test "checkpoint has save payload"
    (let* ([eff (checkpoint 'my-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'save 'my-checkpoint)
                    (stage-effect-payload result))))

  (define-test "checkpoint-with has save-value payload"
    (let* ([eff (checkpoint-with 'saved-data '(a b c))]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'save-value 'saved-data '(a b c))
                    (stage-effect-payload result))))

  (define-test "restore has restore payload"
    (let* ([eff (restore 'my-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'restore 'my-checkpoint)
                    (stage-effect-payload result))))

  (define-test "checkpoint-exists has exists payload"
    (let* ([eff (checkpoint-exists 'maybe-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'exists 'maybe-checkpoint)
                    (stage-effect-payload result))))

  (define-test "checkpoint-clear has clear payload"
    (let* ([eff (checkpoint-clear 'old-checkpoint)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'clear 'old-checkpoint)
                    (stage-effect-payload result)))))

;;; ====
;;; HTTP Effects
;;; ====

(test-group "HTTP Effects"
  (define-test "http-get produces effect"
    (let* ([eff (http-get "https://example.com")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "http-get effect type is http"
    (let* ([eff (http-get "https://example.com")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'http (stage-effect-type result))))

  (define-test "http-get has get payload"
    (let* ([eff (http-get "https://example.com")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'get "https://example.com")
                    (stage-effect-payload result))))

  (define-test "http-post has post payload"
    (let* ([eff (http-post "https://api.example.com/data")]
           [result (run-stage eff test-ctx "body content")])
      (assert-equal (list 'post "https://api.example.com/data")
                    (stage-effect-payload result))))

  (define-test "http-json has get-json payload"
    (let* ([eff (http-json "https://api.example.com/json")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'get-json "https://api.example.com/json")
                    (stage-effect-payload result))))

  (define-test "http-with-headers has get-headers payload"
    (let* ([eff (http-with-headers '(("Authorization" . "Bearer token")) "https://api.example.com")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'get-headers '(("Authorization" . "Bearer token")) "https://api.example.com")
                    (stage-effect-payload result)))))

;;; ====
;;; Await Effects
;;; ====

(test-group "Await Effects"
  (define-test "await-tag produces effect"
    (let* ([eff (await-tag "@opus")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "await-tag effect type is await"
    (let* ([eff (await-tag "@opus")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'await (stage-effect-type result))))

  (define-test "await-tag has forum-tag payload"
    (let* ([eff (await-tag "@opus")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'forum-tag "@opus")
                    (stage-effect-payload result))))

  (define-test "await-file has file payload"
    (let* ([eff (await-file "/path/to/file")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'file "/path/to/file")
                    (stage-effect-payload result))))

  (define-test "await-timeout has timeout payload"
    (let* ([eff (await-timeout 5000)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'timeout 5000)
                    (stage-effect-payload result))))

  (define-test "await-signal has signal payload"
    (let* ([eff (await-signal 'ready)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'signal 'ready)
                    (stage-effect-payload result)))))

;;; ====
;;; BBS (Issue Tracker) Effects
;;; ====

(test-group "BBS Effects"
  (define-test "bbs-create produces effect"
    (let* ([eff (bbs-create "New issue title")]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "bbs-create effect type is bbs"
    (let* ([eff (bbs-create "New issue title")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'bbs (stage-effect-type result))))

  (define-test "bbs-create has create payload"
    (let* ([eff (bbs-create "New issue title")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'create "New issue title")
                    (stage-effect-payload result))))

  (define-test "bbs-create-full has create-full payload"
    (let* ([eff (bbs-create-full "Title" "Description" 'task 2)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'create-full "Title" "Description" 'task 2)
                    (stage-effect-payload result))))

  (define-test "bbs-update has update payload"
    (let* ([eff (bbs-update "fold-abc" '((status . in_progress)))]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'update "fold-abc" '((status . in_progress)))
                    (stage-effect-payload result))))

  (define-test "bbs-close has close payload"
    (let* ([eff (bbs-close "fold-xyz")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'close "fold-xyz")
                    (stage-effect-payload result))))

  (define-test "bbs-ready has ready payload"
    (let ([result (run-stage bbs-ready test-ctx "input")])
      (assert-equal (list 'ready)
                    (stage-effect-payload result))))

  (define-test "bbs-show has show payload"
    (let* ([eff (bbs-show "fold-123")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'show "fold-123")
                    (stage-effect-payload result))))

  (define-test "beads-create alias works"
    (let ([result (run-stage (beads-create "test") test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "beads-ready alias works"
    (let ([result (run-stage beads-ready test-ctx "input")])
      (assert-true (stage-effect? result)))))

;;; ====
;;; Git Effects
;;; ====

(test-group "Git Effects"
  (define-test "git-status produces effect"
    (let ([result (run-stage git-status test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "git-status effect type is git"
    (let ([result (run-stage git-status test-ctx "input")])
      (assert-equal 'git (stage-effect-type result))))

  (define-test "git-status has status payload"
    (let ([result (run-stage git-status test-ctx "input")])
      (assert-equal (list 'status)
                    (stage-effect-payload result))))

  (define-test "git-diff has diff payload"
    (let ([result (run-stage git-diff test-ctx "input")])
      (assert-equal (list 'diff)
                    (stage-effect-payload result))))

  (define-test "git-commit has commit payload"
    (let* ([eff (git-commit "feat: add new feature")]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'commit "feat: add new feature")
                    (stage-effect-payload result))))

  (define-test "git-push has push payload"
    (let ([result (run-stage git-push test-ctx "input")])
      (assert-equal (list 'push)
                    (stage-effect-payload result)))))

;;; ====
;;; Pipeline Effects
;;; ====

(test-group "Pipeline Effects"
  (define-test "invoke-pipeline produces effect"
    (let* ([eff (invoke-pipeline 'my-pipeline)]
           [result (run-stage eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "invoke-pipeline effect type is pipeline"
    (let* ([eff (invoke-pipeline 'my-pipeline)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal 'pipeline (stage-effect-type result))))

  (define-test "invoke-pipeline has invoke payload"
    (let* ([eff (invoke-pipeline 'my-pipeline)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'invoke 'my-pipeline)
                    (stage-effect-payload result))))

  (define-test "invoke-with-fuel has invoke-fuel payload"
    (let* ([eff (invoke-with-fuel 5000 'fuel-limited-pipeline)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'invoke-fuel 5000 'fuel-limited-pipeline)
                    (stage-effect-payload result))))

  (define-test "spawn-pipeline has spawn payload"
    (let* ([eff (spawn-pipeline 'async-pipeline)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'spawn 'async-pipeline)
                    (stage-effect-payload result))))

  (define-test "await-pipeline has await payload"
    (let* ([eff (await-pipeline 'run-123)]
           [result (run-stage eff test-ctx "input")])
      (assert-equal (list 'await 'run-123)
                    (stage-effect-payload result)))))

;;; ====
;;; Effect Predicates
;;; ====

(test-group "Effect Predicates"
  ;; Create one of each effect type
  (define llm-eff (run-stage (llm 'opus "test") test-ctx "in"))
  (define fold-eff (run-stage (fold-eval "expr") test-ctx "in"))
  (define shell-eff (run-stage (shell "ls") test-ctx "in"))
  (define store-eff (run-stage store-put test-ctx "in"))
  (define log-eff (run-stage (log-info "msg") test-ctx "in"))
  (define checkpoint-eff (run-stage (checkpoint 'cp) test-ctx "in"))
  (define http-eff (run-stage (http-get "url") test-ctx "in"))
  (define await-eff (run-stage (await-signal 's) test-ctx "in"))
  (define bbs-eff (run-stage bbs-ready test-ctx "in"))
  (define git-eff (run-stage git-status test-ctx "in"))
  (define pipeline-eff (run-stage (invoke-pipeline 'p) test-ctx "in"))

  (define-test "llm-effect? true for llm"
    (assert-true (llm-effect? llm-eff)))

  (define-test "llm-effect? false for fold"
    (assert-equal #f (llm-effect? fold-eff)))

  (define-test "fold-effect? true for fold"
    (assert-true (fold-effect? fold-eff)))

  (define-test "fold-effect? false for llm"
    (assert-equal #f (fold-effect? llm-eff)))

  (define-test "shell-effect? true for shell"
    (assert-true (shell-effect? shell-eff)))

  (define-test "shell-effect? false for http"
    (assert-equal #f (shell-effect? http-eff)))

  (define-test "store-effect? true for store"
    (assert-true (store-effect? store-eff)))

  (define-test "store-effect? false for log"
    (assert-equal #f (store-effect? log-eff)))

  (define-test "log-effect? true for log"
    (assert-true (log-effect? log-eff)))

  (define-test "log-effect? false for store"
    (assert-equal #f (log-effect? store-eff)))

  (define-test "checkpoint-effect? true for checkpoint"
    (assert-true (checkpoint-effect? checkpoint-eff)))

  (define-test "checkpoint-effect? false for await"
    (assert-equal #f (checkpoint-effect? await-eff)))

  (define-test "http-effect? true for http"
    (assert-true (http-effect? http-eff)))

  (define-test "http-effect? false for shell"
    (assert-equal #f (http-effect? shell-eff)))

  (define-test "await-effect? true for await"
    (assert-true (await-effect? await-eff)))

  (define-test "await-effect? false for bbs"
    (assert-equal #f (await-effect? bbs-eff)))

  (define-test "bbs-effect? true for bbs"
    (assert-true (bbs-effect? bbs-eff)))

  (define-test "bbs-effect? false for git"
    (assert-equal #f (bbs-effect? git-eff)))

  (define-test "beads-effect? alias works"
    (assert-true (beads-effect? bbs-eff)))

  (define-test "git-effect? true for git"
    (assert-true (git-effect? git-eff)))

  (define-test "git-effect? false for pipeline"
    (assert-equal #f (git-effect? pipeline-eff)))

  (define-test "pipeline-effect? true for pipeline"
    (assert-true (pipeline-effect? pipeline-eff)))

  (define-test "pipeline-effect? false for llm"
    (assert-equal #f (pipeline-effect? llm-eff))))

;;; ====
;;; Template Expansion
;;; ====

(test-group "Template Expansion"
  (define-test "expand-template with no variables"
    (assert-equal "Hello World"
                  (expand-template "Hello World" '())))

  (define-test "expand-template with one variable"
    (assert-equal "Hello Alice"
                  (expand-template "Hello ${name}" '(("name" . "Alice")))))

  (define-test "expand-template with multiple variables"
    (assert-equal "Hello Alice, you are 30 years old"
                  (expand-template "Hello ${name}, you are ${age} years old"
                                   '(("name" . "Alice") ("age" . 30)))))

  (define-test "expand-template with missing variable"
    (assert-equal "Hello ${unknown}"
                  (expand-template "Hello ${unknown}" '())))

  (define-test "expand-template with nested text"
    (assert-equal "The result is: 42"
                  (expand-template "The result is: ${result}" '(("result" . 42)))))

  (define-test "expand-template preserves other text"
    (assert-equal "Price: $10.00 for item"
                  (expand-template "Price: $10.00 for ${item}" '(("item" . "item"))))))

;;; ====
;;; Effect Function (generic constructor)
;;; ====

(test-group "Effect Function"
  (define-test "effect creates stage"
    (let ([custom-eff (effect 'custom-type '(payload data))])
      (assert-true (stage? custom-eff))))

  (define-test "effect produces stage-effect"
    (let* ([custom-eff (effect 'custom-type '(payload data))]
           [result (run-stage custom-eff test-ctx "input")])
      (assert-true (stage-effect? result))))

  (define-test "effect type is custom"
    (let* ([custom-eff (effect 'custom-type '(payload data))]
           [result (run-stage custom-eff test-ctx "input")])
      (assert-equal 'custom-type (stage-effect-type result))))

  (define-test "effect payload is correct"
    (let* ([custom-eff (effect 'custom-type '(payload data))]
           [result (run-stage custom-eff test-ctx "input")])
      (assert-equal '(payload data)
                    (stage-effect-payload result)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
