;;; fabric/stitches/pipeline/tests/test-effects.ss — Comprehensive Tests for Pipeline Effects
;;;
;;; Tests the effect construction and predicates.

(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

;;; ============================================================
;;; Test Framework (minimal)
;;; ============================================================

(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Expected: ~s\n    Actual:   ~s\n"
                        name expected actual)))))

(define (test-pred name pred actual)
  (set! *test-count* (+ *test-count* 1))
  (if (pred actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Value did not satisfy predicate: ~s\n"
                        name actual)))))

(define (run-tests name thunk)
  (display (format "\n=== ~a ===\n" name))
  (thunk))

(define (test-summary)
  (display (format "\n=== Summary ===\n"))
  (display (format "Total: ~a  Passed: ~a  Failed: ~a\n"
                   *test-count* *test-passed* *test-failed*))
  (if (= *test-failed* 0)
      (display "All tests passed!\n")
      (begin
       (display "SOME TESTS FAILED\n")
       (exit 1))))

;;; ============================================================
;;; Test Context
;;; ============================================================

(define test-ctx empty-context)

;;; ============================================================
;;; LLM Effects
;;; ============================================================

(run-tests "LLM Effects"
           (lambda ()
                   ;; Test llm effect construction
                   (let ([eff (llm 'opus "Analyze: ${input}")])
                        (test-pred "llm creates stage"
                                   stage?
                                   eff)
                        
                        (let ([result (run-stage eff test-ctx "test input")])
                             (test-pred "llm produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "llm effect type is llm"
                                   'llm
                                   (stage-effect-type result))
                             
                             (test "llm effect has call payload"
                                   (list 'call 'opus "Analyze: ${input}")
                                   (stage-effect-payload result))))
                   
                   ;; Test llm-with-system
                   (let ([eff (llm-with-system 'sonnet "You are helpful" "Help me")])
                        (let ([result (run-stage eff test-ctx "test")])
                             (test "llm-with-system has call-with-system payload"
                                   (list 'call-with-system 'sonnet "You are helpful" "Help me")
                                   (stage-effect-payload result))))
                   
                   ;; Test llm-json
                   (let ([eff (llm-json 'haiku "Return JSON")])
                        (let ([result (run-stage eff test-ctx "test")])
                             (test "llm-json has call-json payload"
                                   (list 'call-json 'haiku "Return JSON")
                                   (stage-effect-payload result))))
                   
                   ;; Test llm-structured
                   (let ([schema '((name . string) (age . number))]
                         [eff (llm-structured 'opus "Parse user" '((name . string) (age . number)))])
                        (let ([result (run-stage eff test-ctx "test")])
                             (test "llm-structured has call-structured payload"
                                   (list 'call-structured 'opus "Parse user" '((name . string) (age . number)))
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Fold Effects
;;; ============================================================

(run-tests "Fold Effects"
           (lambda ()
                   ;; Test fold-eval
                   (let ([eff (fold-eval "(+ 1 2)")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "fold-eval produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "fold-eval effect type is fold"
                                   'fold
                                   (stage-effect-type result))
                             
                             (test "fold-eval has eval payload"
                                   (list 'eval "(+ 1 2)")
                                   (stage-effect-payload result))))
                   
                   ;; Test fold-call
                   (let ([eff (fold-call 'my-fn '(1 2 3))])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "fold-call has call payload"
                                   (list 'call 'my-fn '(1 2 3))
                                   (stage-effect-payload result))))
                   
                   ;; Test fold-load
                   (let ([eff (fold-load "path/to/file.ss")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "fold-load has load payload"
                                   (list 'load "path/to/file.ss")
                                   (stage-effect-payload result))))
                   
                   ;; Test forum-post
                   (let ([eff (forum-post 'engineering "Title" "Body")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "forum-post has forum-post payload"
                                   (list 'forum-post 'engineering "Title" "Body")
                                   (stage-effect-payload result))))
                   
                   ;; Test forum-digest
                   (let ([result (run-stage forum-digest test-ctx "input")])
                        (test "forum-digest has call digest-posts payload"
                              (list 'call 'digest-posts '())
                              (stage-effect-payload result)))
                   
                   ;; Test forum-browse
                   (let ([eff (forum-browse 'design 10)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "forum-browse has call browse payload"
                                   (list 'call 'browse (list 'design 10))
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Shell Effects
;;; ============================================================

(run-tests "Shell Effects"
           (lambda ()
                   ;; Test shell
                   (let ([eff (shell "ls -la")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "shell produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "shell effect type is shell"
                                   'shell
                                   (stage-effect-type result))
                             
                             (test "shell has run payload"
                                   (list 'run "ls -la")
                                   (stage-effect-payload result))))
                   
                   ;; Test shell-with-stdin
                   (let ([eff (shell-with-stdin "grep pattern")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "shell-with-stdin has run-stdin payload"
                                   (list 'run-stdin "grep pattern")
                                   (stage-effect-payload result))))
                   
                   ;; Test shell-env
                   (let ([eff (shell-env '(("PATH" . "/usr/bin")) "echo $PATH")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "shell-env has run-env payload"
                                   (list 'run-env '(("PATH" . "/usr/bin")) "echo $PATH")
                                   (stage-effect-payload result))))
                   
                   ;; Test shell-bg
                   (let ([eff (shell-bg "long-running-task")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "shell-bg has run-bg payload"
                                   (list 'run-bg "long-running-task")
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Store Effects
;;; ============================================================

(run-tests "Store Effects"
           (lambda ()
                   ;; Test store-put
                   (let ([result (run-stage store-put test-ctx "some value")])
                        (test-pred "store-put produces effect"
                                   stage-effect?
                                   result)
                        
                        (test "store-put effect type is store"
                              'store
                              (stage-effect-type result))
                        
                        (test "store-put has put payload"
                              (list 'put)
                              (stage-effect-payload result)))
                   
                   ;; Test store-get
                   (let ([eff (store-get "abc123")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "store-get has get payload"
                                   (list 'get "abc123")
                                   (stage-effect-payload result))))
                   
                   ;; Test store-has
                   (let ([eff (store-has "def456")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "store-has has has payload"
                                   (list 'has "def456")
                                   (stage-effect-payload result))))
                   
                   ;; Test store-pin
                   (let ([eff (store-pin "ghi789")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "store-pin has pin payload"
                                   (list 'pin "ghi789")
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Log Effects
;;; ============================================================

(run-tests "Log Effects"
           (lambda ()
                   ;; Test log-info
                   (let ([eff (log-info "Info message")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "log-info produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "log-info effect type is log"
                                   'log
                                   (stage-effect-type result))
                             
                             (test "log-info has info payload"
                                   (list 'info "Info message")
                                   (stage-effect-payload result))))
                   
                   ;; Test log-warn
                   (let ([eff (log-warn "Warning message")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "log-warn has warn payload"
                                   (list 'warn "Warning message")
                                   (stage-effect-payload result))))
                   
                   ;; Test log-error
                   (let ([eff (log-error "Error message")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "log-error has error payload"
                                   (list 'error "Error message")
                                   (stage-effect-payload result))))
                   
                   ;; Test log-debug
                   (let ([eff (log-debug "Debug message")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "log-debug has debug payload"
                                   (list 'debug "Debug message")
                                   (stage-effect-payload result))))
                   
                   ;; Test log-metric
                   (let ([eff (log-metric 'latency 42.5)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "log-metric has metric payload"
                                   (list 'metric 'latency 42.5)
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Checkpoint Effects
;;; ============================================================

(run-tests "Checkpoint Effects"
           (lambda ()
                   ;; Test checkpoint (save)
                   (let ([eff (checkpoint 'my-checkpoint)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "checkpoint produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "checkpoint effect type is checkpoint"
                                   'checkpoint
                                   (stage-effect-type result))
                             
                             (test "checkpoint has save payload"
                                   (list 'save 'my-checkpoint)
                                   (stage-effect-payload result))))
                   
                   ;; Test checkpoint-with
                   (let ([eff (checkpoint-with 'saved-data '(a b c))])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "checkpoint-with has save-value payload"
                                   (list 'save-value 'saved-data '(a b c))
                                   (stage-effect-payload result))))
                   
                   ;; Test restore
                   (let ([eff (restore 'my-checkpoint)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "restore has restore payload"
                                   (list 'restore 'my-checkpoint)
                                   (stage-effect-payload result))))
                   
                   ;; Test checkpoint-exists
                   (let ([eff (checkpoint-exists 'maybe-checkpoint)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "checkpoint-exists has exists payload"
                                   (list 'exists 'maybe-checkpoint)
                                   (stage-effect-payload result))))
                   
                   ;; Test checkpoint-clear
                   (let ([eff (checkpoint-clear 'old-checkpoint)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "checkpoint-clear has clear payload"
                                   (list 'clear 'old-checkpoint)
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; HTTP Effects
;;; ============================================================

(run-tests "HTTP Effects"
           (lambda ()
                   ;; Test http-get
                   (let ([eff (http-get "https://example.com")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "http-get produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "http-get effect type is http"
                                   'http
                                   (stage-effect-type result))
                             
                             (test "http-get has get payload"
                                   (list 'get "https://example.com")
                                   (stage-effect-payload result))))
                   
                   ;; Test http-post
                   (let ([eff (http-post "https://api.example.com/data")])
                        (let ([result (run-stage eff test-ctx "body content")])
                             (test "http-post has post payload"
                                   (list 'post "https://api.example.com/data")
                                   (stage-effect-payload result))))
                   
                   ;; Test http-json
                   (let ([eff (http-json "https://api.example.com/json")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "http-json has get-json payload"
                                   (list 'get-json "https://api.example.com/json")
                                   (stage-effect-payload result))))
                   
                   ;; Test http-with-headers
                   (let ([headers '(("Authorization" . "Bearer token"))]
                         [eff (http-with-headers '(("Authorization" . "Bearer token")) "https://api.example.com")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "http-with-headers has get-headers payload"
                                   (list 'get-headers '(("Authorization" . "Bearer token")) "https://api.example.com")
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Await Effects
;;; ============================================================

(run-tests "Await Effects"
           (lambda ()
                   ;; Test await-tag
                   (let ([eff (await-tag "@opus")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "await-tag produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "await-tag effect type is await"
                                   'await
                                   (stage-effect-type result))
                             
                             (test "await-tag has forum-tag payload"
                                   (list 'forum-tag "@opus")
                                   (stage-effect-payload result))))
                   
                   ;; Test await-file
                   (let ([eff (await-file "/path/to/file")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-file has file payload"
                                   (list 'file "/path/to/file")
                                   (stage-effect-payload result))))
                   
                   ;; Test await-timeout
                   (let ([eff (await-timeout 5000)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-timeout has timeout payload"
                                   (list 'timeout 5000)
                                   (stage-effect-payload result))))
                   
                   ;; Test await-signal
                   (let ([eff (await-signal 'ready)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-signal has signal payload"
                                   (list 'signal 'ready)
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; BBS (Issue Tracker) Effects
;;; ============================================================

(run-tests "BBS Effects"
           (lambda ()
                   ;; Test bbs-create
                   (let ([eff (bbs-create "New issue title")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "bbs-create produces effect"
                                        stage-effect?
                                        result)

                             (test "bbs-create effect type is bbs"
                                   'bbs
                                   (stage-effect-type result))

                             (test "bbs-create has create payload"
                                   (list 'create "New issue title")
                                   (stage-effect-payload result))))

                   ;; Test bbs-create-full
                   (let ([eff (bbs-create-full "Title" "Description" 'task 2)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "bbs-create-full has create-full payload"
                                   (list 'create-full "Title" "Description" 'task 2)
                                   (stage-effect-payload result))))

                   ;; Test bbs-update
                   (let ([eff (bbs-update "fold-abc" '((status . in_progress)))])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "bbs-update has update payload"
                                   (list 'update "fold-abc" '((status . in_progress)))
                                   (stage-effect-payload result))))

                   ;; Test bbs-close
                   (let ([eff (bbs-close "fold-xyz")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "bbs-close has close payload"
                                   (list 'close "fold-xyz")
                                   (stage-effect-payload result))))

                   ;; Test bbs-ready
                   (let ([result (run-stage bbs-ready test-ctx "input")])
                        (test "bbs-ready has ready payload"
                              (list 'ready)
                              (stage-effect-payload result)))

                   ;; Test bbs-show
                   (let ([eff (bbs-show "fold-123")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "bbs-show has show payload"
                                   (list 'show "fold-123")
                                   (stage-effect-payload result))))

                   ;; Test backwards compatibility aliases
                   (test-pred "beads-create alias works"
                              stage-effect?
                              (run-stage (beads-create "test") test-ctx "input"))
                   (test-pred "beads-ready alias works"
                              stage-effect?
                              (run-stage beads-ready test-ctx "input"))))

;;; ============================================================
;;; Git Effects
;;; ============================================================

(run-tests "Git Effects"
           (lambda ()
                   ;; Test git-status
                   (let ([result (run-stage git-status test-ctx "input")])
                        (test-pred "git-status produces effect"
                                   stage-effect?
                                   result)
                        
                        (test "git-status effect type is git"
                              'git
                              (stage-effect-type result))
                        
                        (test "git-status has status payload"
                              (list 'status)
                              (stage-effect-payload result)))
                   
                   ;; Test git-diff
                   (let ([result (run-stage git-diff test-ctx "input")])
                        (test "git-diff has diff payload"
                              (list 'diff)
                              (stage-effect-payload result)))
                   
                   ;; Test git-commit
                   (let ([eff (git-commit "feat: add new feature")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "git-commit has commit payload"
                                   (list 'commit "feat: add new feature")
                                   (stage-effect-payload result))))
                   
                   ;; Test git-push
                   (let ([result (run-stage git-push test-ctx "input")])
                        (test "git-push has push payload"
                              (list 'push)
                              (stage-effect-payload result)))))

;;; ============================================================
;;; Pipeline Effects
;;; ============================================================

(run-tests "Pipeline Effects"
           (lambda ()
                   ;; Test invoke-pipeline
                   (let ([eff (invoke-pipeline 'my-pipeline)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "invoke-pipeline produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "invoke-pipeline effect type is pipeline"
                                   'pipeline
                                   (stage-effect-type result))
                             
                             (test "invoke-pipeline has invoke payload"
                                   (list 'invoke 'my-pipeline)
                                   (stage-effect-payload result))))
                   
                   ;; Test invoke-with-fuel
                   (let ([eff (invoke-with-fuel 5000 'fuel-limited-pipeline)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "invoke-with-fuel has invoke-fuel payload"
                                   (list 'invoke-fuel 5000 'fuel-limited-pipeline)
                                   (stage-effect-payload result))))
                   
                   ;; Test spawn-pipeline
                   (let ([eff (spawn-pipeline 'async-pipeline)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "spawn-pipeline has spawn payload"
                                   (list 'spawn 'async-pipeline)
                                   (stage-effect-payload result))))
                   
                   ;; Test await-pipeline
                   (let ([eff (await-pipeline 'run-123)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-pipeline has await payload"
                                   (list 'await 'run-123)
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Discord Effects
;;; ============================================================

(run-tests "Discord Effects"
           (lambda ()
                   ;; Test discord-post
                   (let ([eff (discord-post 'engineering "Update" "Here is the update")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test-pred "discord-post produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "discord-post effect type is discord"
                                   'discord
                                   (stage-effect-type result))
                             
                             (test "discord-post has post payload"
                                   (list 'post 'engineering "Update" "Here is the update")
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-post-embed
                   (let ([embed '((title . "Embed") (description . "Test"))]
                         [eff (discord-post-embed 'design '((title . "Embed") (description . "Test")))])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "discord-post-embed has post-embed payload"
                                   (list 'post-embed 'design '((title . "Embed") (description . "Test")))
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-chat
                   (let ([eff (discord-chat 'chat)])
                        (let ([result (run-stage eff test-ctx "Hello!")])
                             (test "discord-chat has chat payload"
                                   (list 'chat 'chat)
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-reply
                   (let ([eff (discord-reply "msg-123")])
                        (let ([result (run-stage eff test-ctx "Reply content")])
                             (test "discord-reply has reply payload"
                                   (list 'reply "msg-123")
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-react
                   (let ([eff (discord-react "msg-123" ":thumbsup:")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "discord-react has react payload"
                                   (list 'react "msg-123" ":thumbsup:")
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-thread
                   (let ([eff (discord-thread "msg-123" "Discussion Thread")])
                        (let ([result (run-stage eff test-ctx "First message")])
                             (test "discord-thread has thread payload"
                                   (list 'thread "msg-123" "Discussion Thread")
                                   (stage-effect-payload result))))
                   
                   ;; Test discord-dm
                   (let ([eff (discord-dm "user-456")])
                        (let ([result (run-stage eff test-ctx "DM content")])
                             (test "discord-dm has dm payload"
                                   (list 'dm "user-456")
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Discord Await Effects
;;; ============================================================

(run-tests "Discord Await Effects"
           (lambda ()
                   ;; Test await-discord-mention
                   (let ([eff (await-discord-mention 'sentinel)])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-discord-mention has discord-mention payload"
                                   (list 'discord-mention 'sentinel)
                                   (stage-effect-payload result))))
                   
                   ;; Test await-discord-reaction
                   (let ([eff (await-discord-reaction "msg-123" ":fire:")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-discord-reaction has discord-reaction payload"
                                   (list 'discord-reaction "msg-123" ":fire:")
                                   (stage-effect-payload result))))
                   
                   ;; Test await-discord-reply
                   (let ([eff (await-discord-reply "msg-456")])
                        (let ([result (run-stage eff test-ctx "input")])
                             (test "await-discord-reply has discord-reply payload"
                                   (list 'discord-reply "msg-456")
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Effect Predicates
;;; ============================================================

(run-tests "Effect Predicates"
           (lambda ()
                   ;; Create one of each effect type
                   (let ([llm-eff (run-stage (llm 'opus "test") test-ctx "in")]
                         [fold-eff (run-stage (fold-eval "expr") test-ctx "in")]
                         [shell-eff (run-stage (shell "ls") test-ctx "in")]
                         [store-eff (run-stage store-put test-ctx "in")]
                         [log-eff (run-stage (log-info "msg") test-ctx "in")]
                         [checkpoint-eff (run-stage (checkpoint 'cp) test-ctx "in")]
                         [http-eff (run-stage (http-get "url") test-ctx "in")]
                         [await-eff (run-stage (await-signal 's) test-ctx "in")]
                         [bbs-eff (run-stage bbs-ready test-ctx "in")]
                         [git-eff (run-stage git-status test-ctx "in")]
                         [pipeline-eff (run-stage (invoke-pipeline 'p) test-ctx "in")]
                         [discord-eff (run-stage (discord-chat 'ch) test-ctx "in")])
                        
                        ;; Test llm-effect?
                        (test-pred "llm-effect? true for llm" llm-effect? llm-eff)
                        (test "llm-effect? false for fold" #f (llm-effect? fold-eff))
                        
                        ;; Test fold-effect?
                        (test-pred "fold-effect? true for fold" fold-effect? fold-eff)
                        (test "fold-effect? false for llm" #f (fold-effect? llm-eff))
                        
                        ;; Test shell-effect?
                        (test-pred "shell-effect? true for shell" shell-effect? shell-eff)
                        (test "shell-effect? false for http" #f (shell-effect? http-eff))
                        
                        ;; Test store-effect?
                        (test-pred "store-effect? true for store" store-effect? store-eff)
                        (test "store-effect? false for log" #f (store-effect? log-eff))
                        
                        ;; Test log-effect?
                        (test-pred "log-effect? true for log" log-effect? log-eff)
                        (test "log-effect? false for store" #f (log-effect? store-eff))
                        
                        ;; Test checkpoint-effect?
                        (test-pred "checkpoint-effect? true for checkpoint" checkpoint-effect? checkpoint-eff)
                        (test "checkpoint-effect? false for await" #f (checkpoint-effect? await-eff))
                        
                        ;; Test http-effect?
                        (test-pred "http-effect? true for http" http-effect? http-eff)
                        (test "http-effect? false for shell" #f (http-effect? shell-eff))
                        
                        ;; Test await-effect?
                        (test-pred "await-effect? true for await" await-effect? await-eff)
                        (test "await-effect? false for bbs" #f (await-effect? bbs-eff))

                        ;; Test bbs-effect?
                        (test-pred "bbs-effect? true for bbs" bbs-effect? bbs-eff)
                        (test "bbs-effect? false for git" #f (bbs-effect? git-eff))
                        ;; Test backwards compat alias
                        (test-pred "beads-effect? alias works" beads-effect? bbs-eff)
                        
                        ;; Test git-effect?
                        (test-pred "git-effect? true for git" git-effect? git-eff)
                        (test "git-effect? false for pipeline" #f (git-effect? pipeline-eff))
                        
                        ;; Test pipeline-effect?
                        (test-pred "pipeline-effect? true for pipeline" pipeline-effect? pipeline-eff)
                        (test "pipeline-effect? false for discord" #f (pipeline-effect? discord-eff))
                        
                        ;; Test discord-effect?
                        (test-pred "discord-effect? true for discord" discord-effect? discord-eff)
                        (test "discord-effect? false for llm" #f (discord-effect? llm-eff)))))

;;; ============================================================
;;; Template Expansion
;;; ============================================================

(run-tests "Template Expansion"
           (lambda ()
                   (test "expand-template with no variables"
                         "Hello World"
                         (expand-template "Hello World" '()))
                   
                   (test "expand-template with one variable"
                         "Hello Alice"
                         (expand-template "Hello ${name}" '(("name" . "Alice"))))
                   
                   (test "expand-template with multiple variables"
                         "Hello Alice, you are 30 years old"
                         (expand-template "Hello ${name}, you are ${age} years old"
                                          '(("name" . "Alice") ("age" . 30))))
                   
                   (test "expand-template with missing variable"
                         "Hello ${unknown}"
                         (expand-template "Hello ${unknown}" '()))
                   
                   (test "expand-template with nested text"
                         "The result is: 42"
                         (expand-template "The result is: ${result}" '(("result" . 42))))
                   
                   (test "expand-template preserves other text"
                         "Price: $10.00 for item"
                         (expand-template "Price: $10.00 for ${item}" '(("item" . "item"))))))

;;; ============================================================
;;; Effect Function (generic constructor)
;;; ============================================================

(run-tests "Effect Function"
           (lambda ()
                   (let ([custom-eff (effect 'custom-type '(payload data))])
                        (test-pred "effect creates stage"
                                   stage?
                                   custom-eff)
                        
                        (let ([result (run-stage custom-eff test-ctx "input")])
                             (test "effect produces stage-effect"
                                   #t
                                   (stage-effect? result))
                             
                             (test "effect type is custom"
                                   'custom-type
                                   (stage-effect-type result))
                             
                             (test "effect payload is correct"
                                   '(payload data)
                                   (stage-effect-payload result))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(test-summary)
