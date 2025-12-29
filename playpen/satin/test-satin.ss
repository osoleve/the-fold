;;; playpen/satin/test-satin.ss — Tests for Satin DSL
;;;
;;; Comprehensive test suite for the Satin authoring DSL.
;;;
;;; Run from project root: scheme --script playpen/satin/test-satin.ss

;; Load prelude first to establish context
(load "fabric/stitches/prelude.ss")

;; Load the main Satin module (which loads Quill)
(load "playpen/satin/satin.ss")

;;; Helper: quill-initial-state (aliased from make-quill-state)
(define quill-initial-state make-quill-state)

;;; quill-story? predicate
(define (quill-story? x)
  (quill-story%? x))

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display name)
       (display ": ok")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  FAIL ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-pred name pred actual)
  (test name #t (pred actual)))

(define (run-tests)
  (display "
=== Satin DSL Test Suite ===

")
  
  ;; ============================================================
  ;; Span Tests
  ;; ============================================================
  (display "--- Span Operations ---
")
  
  (let ([s (make-span "test.satin" 10 5 10 20)])
       (test-true "span?" (span? s))
       (test "span-file" "test.satin" (span-file s))
       (test "span-line" 10 (span-line s))
       (test "span-column" 5 (span-column s))
       (test "span-end-line" 10 (span-end-line s))
       (test "span-end-column" 20 (span-end-column s))
       (test "span->string" "test.satin:10:5" (span->string s)))
  
  (test-true "no-span is span" (span? no-span))
  (test-true "no-span?" (no-span? no-span))
  (test "no-span->string" "<unknown location>" (span->string no-span))
  
  (let ([s1 (make-span "f.ss" 1 0 1 10)]
        [s2 (make-span "f.ss" 3 0 3 10)])
       (let ([combined (span-union s1 s2)])
            (test "span-union start-line" 1 (span-line combined))
            (test "span-union end-line" 3 (span-end-line combined))))
  
  (let ([form '(node test)]
        [s (make-span "f.ss" 5 0 5 15)])
       (let ([annotated (with-span s form)])
            (test-true "with-span?" (with-span? annotated))
            (test "get-span" s (get-span annotated))
            (test "strip-span" form (strip-span annotated))))
  
  ;; ============================================================
  ;; Syntax Recognition Tests
  ;; ============================================================
  (display "
--- Syntax Recognition ---
")
  
  (test-true "satin-story?" (satin-story? '(satin-story (id test))))
  (test-false "satin-story? list" (satin-story? '(node foo)))
  (test-true "satin-node?" (satin-node? '(node foo)))
  (test-true "satin-choice?" (satin-choice? '(choice "Go" next)))
  (test-true "satin-dialogue?" (satin-dialogue? '(dialogue chat)))
  (test-true "satin-quest?" (satin-quest? '(quest find-key)))
  (test-true "satin-exercise?" (satin-exercise? '(exercise math-1)))
  (test-true "satin-rule?" (satin-rule? '(rule trigger)))
  
  ;; ============================================================
  ;; Field Extraction Tests
  ;; ============================================================
  (display "
--- Field Extraction ---
")
  
  (let ([spec '(satin-story
                (id my-story)
                (title "My Story")
                (author "Shepherd")
                (start intro)
                (node intro (title "Intro")))])
       (test "story-id" 'my-story (satin-story-id spec))
       (test "story-title" "My Story" (satin-story-title spec))
       (test "story-author" "Shepherd" (satin-story-author spec))
       (test "story-start" 'intro (satin-story-start spec))
       (test "story-nodes count" 1 (length (satin-story-nodes spec))))
  
  (let ([node '(node intro
                (title "Introduction")
                (body "Welcome!")
                (on-enter (set-flag! visited?))
                (choice "Go" next)
                (choice "Stay" intro))])
       (test "node-id" 'intro (satin-node-id node))
       (test "node-title" "Introduction" (satin-node-title node))
       (test "node-body" "Welcome!" (satin-node-body node))
       (test "node-choices count" 2 (length (satin-node-choices node)))
       (test "node-on-enter count" 1 (length (satin-node-on-enter node))))
  
  (let ([choice '(choice "Open door" hallway :when (has-item? key) :effects (take-item! key))])
       (test "choice-label" "Open door" (satin-choice-label choice))
       (test "choice-target" 'hallway (satin-choice-target choice))
       (test-pred "choice-guard" pair? (satin-choice-guard choice))
       (test "choice-effects count" 1 (length (satin-choice-effects choice))))
  
  ;; ============================================================
  ;; Guard Compilation Tests
  ;; ============================================================
  (display "
--- Guard Compilation ---
")
  
  ;; Create test state and run guard tests
  (let ([test-state
         (let* ([s (quill-initial-state)]
                [s1 (quill-state-set-flag s 'seen? #t)]
                [s2 (quill-state-add-item s1 'key)]
                [s3 (quill-state-set-var s2 'score 100)])
               s3)])
       
       (let ([g (satin-compile-guard #t)])
            (test-true "guard #t" (g test-state)))
       
       (let ([g (satin-compile-guard #f)])
            (test-false "guard #f" (g test-state)))
       
       (let ([g (satin-compile-guard '(flag? seen?))])
            (test-true "guard flag? true" (g test-state)))
       
       (let ([g (satin-compile-guard '(flag? unknown?))])
            (test-false "guard flag? false" (g test-state)))
       
       (let ([g (satin-compile-guard '(has-item? key))])
            (test-true "guard has-item? true" (g test-state)))
       
       (let ([g (satin-compile-guard '(has-item? sword))])
            (test-false "guard has-item? false" (g test-state)))
       
       (let ([g (satin-compile-guard '(var>= score 50))])
            (test-true "guard var>= true" (g test-state)))
       
       (let ([g (satin-compile-guard '(var>= score 150))])
            (test-false "guard var>= false" (g test-state)))
       
       (let ([g (satin-compile-guard '(var= score 100))])
            (test-true "guard var= true" (g test-state)))
       
       (let ([g (satin-compile-guard '(not (flag? unknown?)))])
            (test-true "guard not" (g test-state)))
       
       (let ([g (satin-compile-guard '(and (flag? seen?) (has-item? key)))])
            (test-true "guard and true" (g test-state)))
       
       (let ([g (satin-compile-guard '(and (flag? seen?) (flag? unknown?)))])
            (test-false "guard and false" (g test-state)))
       
       (let ([g (satin-compile-guard '(or (flag? unknown?) (has-item? key)))])
            (test-true "guard or true" (g test-state))))  ; close outer let for test-state
  
  ;; ============================================================
  ;; Effect Compilation Tests
  ;; ============================================================
  (display "
--- Effect Compilation ---
")
  
  (test "effect set-flag!"
        '(set-flag visited?)
        (satin-compile-effect '(set-flag! visited?)))
  
  (test "effect clear-flag!"
        '(clear-flag seen?)
        (satin-compile-effect '(clear-flag! seen?)))
  
  (test "effect give-item!"
        '(give-item key)
        (satin-compile-effect '(give-item! key)))
  
  (test "effect take-item!"
        '(take-item key)
        (satin-compile-effect '(take-item! key)))
  
  (test "effect set-var!"
        '(set-var score 100)
        (satin-compile-effect '(set-var! score 100)))
  
  (test "effect inc-var!"
        '(inc-var score 1)
        (satin-compile-effect '(inc-var! score)))
  
  (test "effect inc-var! with amount"
        '(inc-var score 10)
        (satin-compile-effect '(inc-var! score 10)))
  
  (test "effect dec-var!"
        '(dec-var health 5)
        (satin-compile-effect '(dec-var! health 5)))
  
  (test "effect print!"
        '(print "Hello!")
        (satin-compile-effect '(print! "Hello!")))
  
  (test "effect goto!"
        '(goto next-node)
        (satin-compile-effect '(goto! next-node)))
  
  (test "effect start-dialogue!"
        '(start-dialogue npc-chat)
        (satin-compile-effect '(start-dialogue! npc-chat)))
  
  (test "effect activate-quest!"
        '(activate-quest find-key)
        (satin-compile-effect '(activate-quest! find-key)))
  
  (test-true "effect valid? set-flag!"
             (satin-effect-valid? '(set-flag! foo)))
  
  (test-true "effect valid? print!"
             (satin-effect-valid? '(print! "hello")))
  
  (test-false "effect valid? missing arg"
              (satin-effect-valid? '(set-flag!)))
  
  ;; ============================================================
  ;; Validation Tests
  ;; ============================================================
  (display "
--- Validation ---
")
  
  (let ([issues (satin-validate
                 '(satin-story
                   (id test)
                   (start intro)
                   (node intro
                         (title "Intro")
                         (body "Hello")
                         (choice "Go" ending))
                   (node ending
                         (title "End")
                         (body "Goodbye"))))])
       (test "valid story has no errors"
             0
             (length (filter satin-error? issues))))
  
  (let ([issues (satin-validate
                 '(satin-story
                   (title "Missing ID")
                   (start intro)
                   (node intro (title "Intro"))))])
       (test-true "missing id is error"
                  (ormap (lambda (i)
                                 (eq? (satin-issue-code i) 'missing-field))
                         issues)))
  
  (let ([issues (satin-validate
                 '(satin-story
                   (id test)
                   (start nonexistent)
                   (node intro (title "Intro"))))])
       (test-true "undefined start is error"
                  (ormap (lambda (i)
                                 (eq? (satin-issue-code i) 'undefined-node))
                         issues)))
  
  (let ([issues (satin-validate
                 '(satin-story
                   (id test)
                   (start intro)
                   (node intro
                         (title "Intro")
                         (choice "Go" nowhere))))])
       (test-true "undefined choice target is error"
                  (ormap (lambda (i)
                                 (eq? (satin-issue-code i) 'undefined-node))
                         issues)))
  
  (let ([issues (satin-validate
                 '(satin-story
                   (id test)
                   (start intro)
                   (node intro (title "First"))
                   (node intro (title "Duplicate"))))])
       (test-true "duplicate node id is error"
                  (ormap (lambda (i)
                                 (eq? (satin-issue-code i) 'duplicate-id))
                         issues)))
  
  ;; ============================================================
  ;; Full Compilation Tests
  ;; ============================================================
  (display "
--- Full Compilation ---
")
  
  (let-values ([(story issues) (satin-compile (satin-example))])
              (test-true "example compiles" (quill-story? story))
              (test "example no errors"
                    0
                    (length (filter satin-error? issues)))
              (test "example id" 'demo (quill-story-id story))
              (test "example title" "Satin Demo" (quill-story-title story))
              (test "example start" 'welcome (quill-story-start-node story))
              (test "example node count" 3 (length (quill-story-nodes story))))
  
  (let* ([spec '(satin-story
                 (id with-guards)
                 (title "Guards Test")
                 (start room)
                 (node room
                       (title "Room")
                       (body "You are in a room.")
                       (choice "Open door" hall :when (has-item? key))
                       (choice "Look around" room))
                 (node hall
                       (title "Hallway")
                       (body "You are in a hall.")))]
         [story (satin-compile! spec)]
         [room-node (quill-story-node story 'room)]
         [choices (quill-node-choices room-node)])
        (test "compiled node has choices" 2 (length choices))
        (let ([first-choice (car choices)])
             (test "choice has label" "Open door" (quill-choice-label first-choice))
             (test "choice has target" 'hall (quill-choice-target first-choice))
             (test-pred "choice has guard" procedure? (quill-choice-guard first-choice))))
  
  (let* ([spec '(satin-story
                 (id with-effects)
                 (title "Effects Test")
                 (start start)
                 (node start
                       (title "Start")
                       (body "Begin")
                       (on-enter (set-flag! started?))
                       (choice "Next" finish :effects (inc-var! counter)))
                 (node finish
                       (title "Finish")
                       (body "Done")))]
         [story (satin-compile! spec)]
         [start-node (quill-story-node story 'start)]
         [on-enter (quill-node-on-enter start-node)])
        (test-true "on-enter has effects" (> (length on-enter) 0))
        ;; First effect is auto-added visited tracking
        (test-pred "on-enter includes visited flag"
                   (lambda (effects)
                           (ormap (lambda (e)
                                          (and (pair? e)
                                               (eq? (car e) 'set-flag)))
                                  effects))
                   on-enter))
  
  ;; ============================================================
  ;; Issue Formatting Tests
  ;; ============================================================
  (display "
--- Issue Formatting ---
")
  
  (let ([issue (satin-error 'test-code "Test message" no-span '())])
       (test-true "issue?" (satin-issue? issue))
       (test-true "error?" (satin-error? issue))
       (test-false "warning?" (satin-warning? issue))
       (test "issue-code" 'test-code (satin-issue-code issue))
       (test "issue-message" "Test message" (satin-issue-message issue))
       (test-pred "format-issue" string? (satin-format-issue issue)))
  
  ;; ============================================================
  ;; Template Tests
  ;; ============================================================
  (display "
--- Templates ---
")
  
  (test-true "has-template?" (satin-has-template? "Hello {var:name}!"))
  (test-false "no template" (satin-has-template? "Hello world"))
  
  (let* ([state (quill-state-set-var (quill-initial-state) 'name "Alice")]
         [result (satin-expand-template "Hello {var:name}!" state)])
        (test "template expansion" "Hello Alice!" result))
  
  ;; ============================================================
  ;; Check Predicate Tests
  ;; ============================================================
  (display "
--- Check Predicates ---
")
  
  (let ([pred (satin-compile-check-predicate '(answer-eq? "hello"))])
       (test-true "answer-eq? matches" (pred #f #f "hello"))
       (test-false "answer-eq? no match" (pred #f #f "world")))
  
  (let ([pred (satin-compile-check-predicate '(answer-contains? "foo"))])
       (test-true "answer-contains? yes" (pred #f #f "foobar"))
       (test-false "answer-contains? no" (pred #f #f "baz")))
  
  (let ([pred (satin-compile-check-predicate '(answer-length>= 5))])
       (test-true "answer-length>= yes" (pred #f #f "hello"))
       (test-false "answer-length>= no" (pred #f #f "hi")))
  
  (let ([pred (satin-compile-check-predicate
               '(and (answer-contains? "(")
                 (answer-contains? ")")))])
       (test-true "and predicate" (pred #f #f "(+ 1 2)"))
       (test-false "and predicate fail" (pred #f #f "1 2")))
  
  ;; ============================================================
  ;; String Helpers Tests
  ;; ============================================================
  (display "
--- String Helpers ---
")
  
  (test-true "string-contains? yes" (string-contains? "hello world" "world"))
  (test-true "string-contains? at start" (string-contains? "hello" "hel"))
  (test-true "string-contains? at end" (string-contains? "hello" "llo"))
  (test-false "string-contains? no" (string-contains? "hello" "xyz"))
  (test-false "string-contains? too long" (string-contains? "hi" "hello"))
  
  ;; ============================================================
  ;; Summary
  ;; ============================================================
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
