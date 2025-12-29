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
  ;; Education Form Recognition Tests
  ;; ============================================================
  (display "
--- Education Form Recognition ---
")
  
  (test-true "satin-lesson?" (satin-lesson? '(lesson intro-lesson)))
  (test-false "satin-lesson? node" (satin-lesson? '(node foo)))
  
  (test-true "satin-mcq?" (satin-mcq? '(mcq q1)))
  (test-false "satin-mcq? exercise" (satin-mcq? '(exercise q1)))
  
  (test-true "satin-short-answer?" (satin-short-answer? '(short-answer sa1)))
  (test-false "satin-short-answer? mcq" (satin-short-answer? '(mcq q1)))
  
  (test-true "satin-code-task?" (satin-code-task? '(code-task task1)))
  (test-false "satin-code-task? node" (satin-code-task? '(node n1)))
  
  (test-true "satin-mastery?" (satin-mastery? '(mastery skill1)))
  (test-false "satin-mastery? quest" (satin-mastery? '(quest q1)))
  
  ;; ============================================================
  ;; Lesson Extraction Tests
  ;; ============================================================
  (display "
--- Lesson Extraction ---
")
  
  (let ([lesson '(lesson scheme-basics
                  (title "Introduction to Scheme")
                  (objectives "Understand S-expressions" "Write basic forms")
                  (prerequisites scheme-setup)
                  (sequence node-intro mcq-1 exercise-1)
                  (on-complete (set-flag! scheme-basics-done?)))])
       (test "lesson-id" 'scheme-basics (satin-lesson-id lesson))
       (test "lesson-title" "Introduction to Scheme" (satin-lesson-title lesson))
       (test "lesson-objectives" '("Understand S-expressions" "Write basic forms")
             (satin-lesson-objectives lesson))
       (test "lesson-prerequisites" '(scheme-setup) (satin-lesson-prerequisites lesson))
       (test "lesson-sequence" '(node-intro mcq-1 exercise-1) (satin-lesson-sequence lesson))
       (test "lesson-on-complete count" 1 (length (satin-lesson-on-complete lesson))))
  
  ;; ============================================================
  ;; MCQ Extraction Tests
  ;; ============================================================
  (display "
--- MCQ Extraction ---
")
  
  (let ([mcq '(mcq what-is-lambda
               (question "What is a lambda expression?")
               (options "A variable" "An anonymous function" "A loop" "A string")
               (correct 2)
               (explanation "Lambda creates anonymous functions")
               (hints "Think about functions"))])
       (test "mcq-id" 'what-is-lambda (satin-mcq-id mcq))
       (test "mcq-question" "What is a lambda expression?" (satin-mcq-question mcq))
       (test "mcq-options count" 4 (length (satin-mcq-options mcq)))
       (test "mcq-correct" 2 (satin-mcq-correct mcq))
       (test "mcq-explanation" "Lambda creates anonymous functions" (satin-mcq-explanation mcq))
       (test "mcq-hints count" 1 (length (satin-mcq-hints mcq))))
  
  ;; ============================================================
  ;; Short Answer Extraction Tests
  ;; ============================================================
  (display "
--- Short Answer Extraction ---
")
  
  (let ([sa '(short-answer define-function
              (question "What keyword defines a function?")
              (expected "define")
              (hints "It starts with 'd'"))])
       (test "short-answer-id" 'define-function (satin-short-answer-id sa))
       (test "short-answer-question" "What keyword defines a function?" (satin-short-answer-question sa))
       (test "short-answer-expected" "define" (satin-short-answer-expected sa))
       (test "short-answer-hints count" 1 (length (satin-short-answer-hints sa))))
  
  ;; ============================================================
  ;; Code Task Extraction Tests
  ;; ============================================================
  (display "
--- Code Task Extraction ---
")
  
  (let ([ct '(code-task write-add
              (title "Write an add function")
              (description "Write a function that adds two numbers")
              (starter "(define (add a b)
  ; your code here
  )")
              (solution "(define (add a b) (+ a b))")
              (tests
               (test "adds 1 + 2" (answer-eq? 3))
               (test "adds 0 + 0" (answer-eq? 0)))
              (hints "Use the + operator" "Two arguments"))])
       (test "code-task-id" 'write-add (satin-code-task-id ct))
       (test "code-task-title" "Write an add function" (satin-code-task-title ct))
       (test "code-task-description" "Write a function that adds two numbers" (satin-code-task-description ct))
       (test-pred "code-task-starter" string? (satin-code-task-starter ct))
       (test "code-task-tests count" 2 (length (satin-code-task-tests ct)))
       (test "code-task-hints count" 2 (length (satin-code-task-hints ct))))
  
  ;; ============================================================
  ;; Mastery Extraction Tests
  ;; ============================================================
  (display "
--- Mastery Extraction ---
")
  
  (let ([mastery '(mastery recursion-skill
                   (skill "Recursive Thinking")
                   (levels beginner intermediate advanced)
                   (threshold 3)
                   (exercises rec-1 rec-2 rec-3 rec-4))])
       (test "mastery-id" 'recursion-skill (satin-mastery-id mastery))
       (test "mastery-skill" "Recursive Thinking" (satin-mastery-skill mastery))
       (test "mastery-levels" '(beginner intermediate advanced) (satin-mastery-levels mastery))
       (test "mastery-threshold" 3 (satin-mastery-threshold mastery))
       (test "mastery-exercises" '(rec-1 rec-2 rec-3 rec-4) (satin-mastery-exercises mastery)))
  
  ;; ============================================================
  ;; Education Form Compilation Tests
  ;; ============================================================
  (display "
--- Education Form Compilation ---
")
  
  ;; Test MCQ compilation
  (let* ([mcq '(mcq test-mcq
                (question "Which is correct?")
                (options "A" "B" "C")
                (correct 2))]
         [compiled (satin-compile-mcq mcq)])
        (test "mcq compile is exercise" #t (quill-exercise%? compiled))
        (test "mcq compile id" 'test-mcq (quill-exercise-id compiled))
        (test-pred "mcq has checks" (lambda (x) (> (length x) 0)) (quill-exercise-checks compiled))
        (let ([meta (quill-exercise-meta compiled)])
             (test "mcq meta type" 'mcq (cdr (assq 'type meta)))
             (test "mcq meta correct" 2 (cdr (assq 'correct meta)))))
  
  ;; Test short-answer compilation
  (let* ([sa '(short-answer test-sa
               (question "What is 2+2?")
               (expected "4"))]
         [compiled (satin-compile-short-answer sa)])
        (test "short-answer compile is exercise" #t (quill-exercise%? compiled))
        (test "short-answer compile id" 'test-sa (quill-exercise-id compiled))
        (let ([meta (quill-exercise-meta compiled)])
             (test "short-answer meta type" 'short-answer (cdr (assq 'type meta)))))
  
  ;; Test code-task compilation
  (let* ([ct '(code-task test-ct
               (title "Test Task")
               (description "Do something"))]
         [compiled (satin-compile-code-task ct)])
        (test "code-task compile is exercise" #t (quill-exercise%? compiled))
        (test "code-task compile id" 'test-ct (quill-exercise-id compiled))
        (let ([meta (quill-exercise-meta compiled)])
             (test "code-task meta type" 'code-task (cdr (assq 'type meta)))))
  
  ;; Test lesson compilation
  (let* ([lesson '(lesson test-lesson
                   (title "Test Lesson")
                   (objectives "Learn stuff")
                   (sequence node1 mcq1))]
         [compiled (satin-compile-lesson lesson)])
        (test "lesson compile id" 'test-lesson (cdr (assq 'id compiled)))
        (test "lesson compile title" "Test Lesson" (cdr (assq 'title compiled)))
        (test "lesson compile objectives" '("Learn stuff") (cdr (assq 'objectives compiled))))
  
  ;; Test mastery compilation
  (let* ([mastery '(mastery test-mastery
                    (skill "Testing")
                    (threshold 5))]
         [compiled (satin-compile-mastery mastery)])
        (test "mastery compile id" 'test-mastery (cdr (assq 'id compiled)))
        (test "mastery compile skill" "Testing" (cdr (assq 'skill compiled)))
        (test "mastery compile threshold" 5 (cdr (assq 'threshold compiled))))
  
  ;; ============================================================
  ;; Full Story with Education Forms
  ;; ============================================================
  (display "
--- Story with Education Forms ---
")
  
  (let* ([spec '(satin-story
                 (id edu-demo)
                 (title "Education Demo")
                 (start intro)
                 
                 (node intro
                       (title "Welcome")
                       (body "Welcome to the lesson")
                       (choice "Start" lesson1))
                 
                 (node lesson1
                       (title "Lesson 1")
                       (body "Learn this")
                       (choice "Done" ending))
                 
                 (node ending
                       (title "End")
                       (body "Complete")
                       (end))
                 
                 (lesson basics
                         (title "The Basics")
                         (objectives "Learn" "Practice")
                         (sequence intro lesson1 ending))
                 
                 (mcq test-q
                      (question "Is 1+1=2?")
                      (options "Yes" "No")
                      (correct 1))
                 
                 (short-answer fill-blank
                               (question "What is (+ 1 1)?")
                               (expected "2"))
                 
                 (code-task write-code
                            (title "Write Code")
                            (description "Write any code"))
                 
                 (mastery math-skill
                          (skill "Math")
                          (threshold 2)
                          (exercises test-q fill-blank)))]
         [story (satin-compile! spec)]
         [meta (quill-story-meta story)])
        (test "edu story compiles" 'edu-demo (quill-story-id story))
        (test-true "has lessons in meta"
                   (if (assq 'lessons meta) #t #f))
        (test-true "has exercises in meta"
                   (if (assq 'exercises meta) #t #f))
        (test-true "has masteries in meta"
                   (if (assq 'masteries meta) #t #f))
        (let ([lessons (cdr (assq 'lessons meta))])
             (test "lesson count" 1 (length lessons)))
        (let ([masteries (cdr (assq 'masteries meta))])
             (test "mastery count" 1 (length masteries))))
  
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
