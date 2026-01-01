;;; playpen/chronicle/test-chronicle.ss — Tests for Chronicle DSL
;;;
;;; Comprehensive tests for the narrative DSL.

(load "core/test-framework.ss")
(load "user/chronicle/chronicle.ss")

;;; ============================================================
;;; State Tests
;;; ============================================================

(test-group chronicle-state
            (define-test empty-state-creation
              (let ([s (empty-state)])
                   (assert-true (cstate? s))
                   (assert-equal '() (cstate-vars s))
                   (assert-equal '() (cstate-flags s))
                   (assert-equal '() (cstate-inventory s))))
            
            (define-test state-get-default
              (let ([s (empty-state)])
                   (assert-equal 'default (state-get s 'missing 'default))
                   (assert-equal 0 (state-get s 'count 0))))
            
            (define-test state-set-and-get
              (let* ([s0 (empty-state)]
                     [s1 (state-set s0 'name "Alice")]
                     [s2 (state-set s1 'age 30)])
                    (assert-equal "Alice" (state-get s2 'name #f))
                    (assert-equal 30 (state-get s2 'age 0))
                    ;; Original unchanged (immutable)
                    (assert-equal #f (state-get s0 'name #f))))
            
            (define-test state-flags
              (let* ([s0 (empty-state)]
                     [s1 (state-flag! s0 'visited)]
                     [s2 (state-flag! s1 'ready)])
                    (assert-true (state-flag? s2 'visited))
                    (assert-true (state-flag? s2 'ready))
                    (assert-false (state-flag? s2 'unknown))
                    ;; Unflag
                    (let ([s3 (state-unflag! s2 'visited)])
                         (assert-false (state-flag? s3 'visited))
                         (assert-true (state-flag? s3 'ready)))))
            
            (define-test state-inventory
              (let* ([s0 (empty-state)]
                     [s1 (state-add-item s0 'sword)]
                     [s2 (state-add-item s1 'shield)]
                     [s3 (state-add-item s2 'sword)])  ; duplicate
                    (assert-true (state-has-item? s2 'sword))
                    (assert-true (state-has-item? s2 'shield))
                    (assert-false (state-has-item? s0 'sword))
                    ;; Duplicate doesn't add twice
                    (assert-equal (cstate-inventory s2) (cstate-inventory s3))
                    ;; Remove
                    (let ([s4 (state-remove-item s2 'sword)])
                         (assert-false (state-has-item? s4 'sword))
                         (assert-true (state-has-item? s4 'shield))))))

;;; ============================================================
;;; Guard Tests
;;; ============================================================

(test-group chronicle-guards
            (define-test guard-always-true
              (let ([g (guard-always)]
                    [s (empty-state)])
                   (assert-true (g s))))
            
            (define-test guard-never-false
              (let ([g (guard-never)]
                    [s (empty-state)])
                   (assert-false (g s))))
            
            (define-test guard-flag-check
              (let* ([s0 (empty-state)]
                     [s1 (state-flag! s0 'ready)]
                     [g (guard-flag 'ready)])
                    (assert-false (g s0))
                    (assert-true (g s1))))
            
            (define-test guard-not-flag
              (let* ([s0 (empty-state)]
                     [s1 (state-flag! s0 'ready)]
                     [g (guard-not-flag 'ready)])
                    (assert-true (g s0))
                    (assert-false (g s1))))
            
            (define-test guard-has-item
              (let* ([s0 (empty-state)]
                     [s1 (state-add-item s0 'key)]
                     [g (guard-has-item 'key)])
                    (assert-false (g s0))
                    (assert-true (g s1))))
            
            (define-test guard-var-comparisons
              (let* ([s (state-set (empty-state) 'score 50)]
                     [g-eq (guard-var-eq 'score 50)]
                     [g-gte (guard-var>= 'score 40)]
                     [g-lte (guard-var<= 'score 60)])
                    (assert-true (g-eq s))
                    (assert-true (g-gte s))
                    (assert-true (g-lte s))
                    (assert-false ((guard-var-eq 'score 100) s))
                    (assert-false ((guard-var>= 'score 100) s))))
            
            (define-test guard-combinators
              (let* ([s (state-set (state-flag! (empty-state) 'ready) 'level 5)]
                     [g1 (guard-and (guard-flag 'ready) (guard-var>= 'level 3))]
                     [g2 (guard-or (guard-flag 'done) (guard-var>= 'level 3))]
                     [g3 (guard-not (guard-flag 'done))])
                    (assert-true (g1 s))
                    (assert-true (g2 s))
                    (assert-true (g3 s)))))

;;; ============================================================
;;; Effect Tests
;;; ============================================================

(test-group chronicle-effects
            (define-test effect-none-identity
              (let* ([s (empty-state)]
                     [e (effect-none)]
                     [s1 (e s)])
                    (assert-equal s s1)))
            
            (define-test effect-set-var
              (let* ([s0 (empty-state)]
                     [e (effect-set 'health 100)]
                     [s1 (e s0)])
                    (assert-equal 100 (state-get s1 'health 0))))
            
            (define-test effect-inc-var
              (let* ([s0 (state-set (empty-state) 'gold 50)]
                     [e (effect-inc 'gold 25)]
                     [s1 (e s0)])
                    (assert-equal 75 (state-get s1 'gold 0))))
            
            (define-test effect-inc-from-zero
              (let* ([s0 (empty-state)]
                     [e (effect-inc 'points 10)]
                     [s1 (e s0)])
                    (assert-equal 10 (state-get s1 'points 0))))
            
            (define-test effect-flag-operations
              (let* ([s0 (empty-state)]
                     [e1 (effect-flag 'visited)]
                     [e2 (effect-unflag 'visited)]
                     [s1 (e1 s0)]
                     [s2 (e2 s1)])
                    (assert-true (state-flag? s1 'visited))
                    (assert-false (state-flag? s2 'visited))))
            
            (define-test effect-inventory-operations
              (let* ([s0 (empty-state)]
                     [e1 (effect-add-item 'torch)]
                     [e2 (effect-remove-item 'torch)]
                     [s1 (e1 s0)]
                     [s2 (e2 s1)])
                    (assert-true (state-has-item? s1 'torch))
                    (assert-false (state-has-item? s2 'torch))))
            
            (define-test effect-seq-composition
              (let* ([s0 (empty-state)]
                     [e (effect-seq (effect-set 'x 1)
                                    (effect-set 'y 2)
                                    (effect-flag 'done))]
                     [s1 (e s0)])
                    (assert-equal 1 (state-get s1 'x 0))
                    (assert-equal 2 (state-get s1 'y 0))
                    (assert-true (state-flag? s1 'done))))
            
            (define-test effect-when-conditional
              (let* ([s0 (state-flag! (empty-state) 'ready)]
                     [s1 (empty-state)]
                     [e (effect-when (guard-flag 'ready)
                                     (effect-set 'bonus 100))])
                    (assert-equal 100 (state-get (e s0) 'bonus 0))
                    (assert-equal 0 (state-get (e s1) 'bonus 0)))))

;;; ============================================================
;;; Template Tests
;;; ============================================================

(test-group chronicle-templates
            (define-test template-no-variables
              (let ([s (empty-state)])
                   (assert-equal "Hello world"
                                 (expand-template-string "Hello world" s))))
            
            (define-test template-single-variable
              (let ([s (state-set (empty-state) 'name "Alice")])
                   (assert-equal "Hello, Alice!"
                                 (expand-template-string "Hello, ${name}!" s))))
            
            (define-test template-multiple-variables
              (let ([s (state-set (state-set (empty-state) 'name "Bob") 'score 42)])
                   (assert-equal "Bob scored 42 points"
                                 (expand-template-string "${name} scored ${score} points" s))))
            
            (define-test template-missing-variable
              (let ([s (empty-state)])
                   ;; Missing variable expands to empty string
                   (assert-equal "Hello, !"
                                 (expand-template-string "Hello, ${name}!" s))))
            
            (define-test eval-template-string
              (let ([s (state-set (empty-state) 'x 5)])
                   (assert-equal "x is 5"
                                 (eval-template "x is ${x}" s))))
            
            (define-test eval-template-procedure
              (let ([s (state-set (empty-state) 'count 3)]
                    [tmpl (lambda (st) (format "Count: ~a" (state-get st 'count 0)))])
                   (assert-equal "Count: 3"
                                 (eval-template tmpl s)))))

;;; ============================================================
;;; Chronicle Structure Tests
;;; ============================================================

(test-group chronicle-structure
            (define-test build-simple-scene
              (let ([sc (build-scene 'intro "Welcome!" '() '() '() '())])
                   (assert-true (scene? sc))
                   (assert-equal 'intro (scene-id sc))
                   (assert-equal "Welcome!" (scene-text sc))))
            
            (define-test build-choice
              (let ([ch (build-choice "Go north" 'north (guard-always) '() '())])
                   (assert-true (choice? ch))
                   (assert-equal "Go north" (choice-label ch))
                   (assert-equal 'north (choice-target ch))))
            
            (define-test build-chronicle
              (let* ([s1 (build-scene 'start "Begin" '() '() '() '())]
                     [s2 (build-scene 'end "End" '() '() '() '())]
                     [chr (build-chronicle 'test-story "Test" (list s1 s2) 'start '())])
                    (assert-true (chronicle? chr))
                    (assert-equal 'test-story (chronicle-id chr))
                    (assert-equal "Test" (chronicle-title chr))
                    (assert-equal 'start (chronicle-start-scene chr))))
            
            (define-test chronicle-scene-lookup
              (let* ([s1 (build-scene 'intro "Intro" '() '() '() '())]
                     [s2 (build-scene 'middle "Middle" '() '() '() '())]
                     [chr (build-chronicle 'story "Story" (list s1 s2) 'intro '())])
                    (assert-true (chronicle-has-scene? chr 'intro))
                    (assert-true (chronicle-has-scene? chr 'middle))
                    (assert-false (chronicle-has-scene? chr 'missing))
                    (assert-equal 'intro (scene-id (chronicle-scene chr 'intro))))))

;;; ============================================================
;;; DSL Parsing Tests
;;; ============================================================

(test-group chronicle-dsl-parsing
            (define-test parse-simple-guard
              (let ([g1 (parse-guard-expr #t)]
                    [g2 (parse-guard-expr #f)]
                    [s (empty-state)])
                   (assert-true (g1 s))
                   (assert-false (g2 s))))
            
            (define-test parse-flag-guard
              (let* ([g (parse-guard-expr '(flag? visited))]
                     [s0 (empty-state)]
                     [s1 (state-flag! s0 'visited)])
                    (assert-false (g s0))
                    (assert-true (g s1))))
            
            (define-test parse-compound-guard
              (let* ([g (parse-guard-expr '(and (flag? ready) (var>= level 5)))]
                     [s (state-set (state-flag! (empty-state) 'ready) 'level 10)])
                    (assert-true (g s))))
            
            (define-test parse-effect-set
              (let* ([e (parse-effect-expr '(set! health 100))]
                     [s (e (empty-state))])
                    (assert-equal 100 (state-get s 'health 0))))
            
            (define-test parse-effect-seq
              (let* ([e (parse-effect-expr '(seq (set! x 1) (flag! done)))]
                     [s (e (empty-state))])
                    (assert-equal 1 (state-get s 'x 0))
                    (assert-true (state-flag? s 'done))))
            
            (define-test parse-choice-def
              (let ([ch (parse-choice-def '(-> "Open door" door :when (flag? has-key)))])
                   (assert-equal "Open door" (choice-label ch))
                   (assert-equal 'door (choice-target ch))
                   ;; Guard should be a function
                   (assert-true (procedure? (choice-guard ch)))))
            
            (define-test parse-scene-def
              (let ([sc (parse-scene-def '(scene intro "Welcome!"
                                           (-> "Continue" next)
                                           (on-enter (flag! visited))))])
                   (assert-equal 'intro (scene-id sc))
                   (assert-equal "Welcome!" (scene-text sc))
                   (assert-equal 1 (length (scene-choices sc)))
                   (assert-equal 1 (length (scene-on-enter sc))))))

;;; ============================================================
;;; Runtime Tests
;;; ============================================================

(test-group chronicle-runtime
            ;; Build a simple test chronicle
            (define test-chronicle
              (let* ([intro-choices
                      (list (build-choice "Go to hall" 'hall (guard-always) '() '())
                            (build-choice "Stay" 'intro (guard-always) '() '()))]
                     [hall-choices
                      (list (build-choice "Return" 'intro (guard-always) '() '())
                            (build-choice "Exit" 'ending (guard-always) '() '()))]
                     [ending-choices '()]
                     [intro (build-scene 'intro "You are at the entrance."
                                         intro-choices '() '() '())]
                     [hall (build-scene 'hall "A grand hall stretches before you."
                                        hall-choices '() '() '())]
                     [ending (build-scene 'ending "The end."
                                          ending-choices '() '() '())])
                    (build-chronicle 'test "Test Story"
                                     (list intro hall ending)
                                     'intro '())))
            
            (define-test runtime-start
              (let ([run (chronicle-start test-chronicle)])
                   (assert-true (crun? run))
                   (assert-equal 'test (crun-chronicle-id run))
                   (assert-equal 'intro (crun-scene-id run))
                   (assert-false (crun-done? run))))
            
            (define-test runtime-visible-choices
              (let ([run (chronicle-start test-chronicle)])
                   (assert-equal 2 (length (chronicle-visible-choices test-chronicle run)))))
            
            (define-test runtime-choose
              (let* ([run0 (chronicle-start test-chronicle)]
                     [run1 (chronicle-choose test-chronicle run0 1)])  ; "Go to hall"
                    (assert-equal 'hall (crun-scene-id run1))))
            
            (define-test runtime-invalid-choice
              (let* ([run0 (chronicle-start test-chronicle)]
                     [run1 (chronicle-choose test-chronicle run0 99)])
                    (assert-equal "Invalid choice." (crun-message run1))))
            
            (define-test runtime-render
              (let ([run (chronicle-start test-chronicle)])
                   (let ([output (chronicle-render test-chronicle run)])
                        (assert-true (string? output))
                        (assert-true (> (string-length output) 0))))))

;;; ============================================================
;;; Integration: Full Story Test
;;; ============================================================

(test-group chronicle-integration
            (define adventure-chronicle
              (parse-chronicle-def
               '(chronicle adventure "A Small Adventure"
                 (start intro)
                 (scene intro
                        "You stand before a mysterious door. ${message}"
                        (-> "Open the door" hallway
                            :do (flag! entered))
                        (-> "Examine the door" intro
                            :do (set! message "It's old and creaky.")))
                 (scene hallway
                        "A long corridor. You see a key on the floor."
                        (-> "Take the key" hallway
                            :when (not (has-item? key))
                            :do (add-item! key))
                        (-> "Go to locked room" locked
                            :when (has-item? key))
                        (-> "Go back" intro))
                 (scene locked
                        "You used the key! You found treasure!"
                        (-> "Celebrate" ending
                            :do (set! ending "victory")))
                 (scene ending
                        "The adventure ends. ${ending}"))))
            
            (define-test integration-full-playthrough
              ;; Start
              (let* ([run0 (chronicle-start adventure-chronicle)]
                     ;; Should be at intro
                     [_ (assert-equal 'intro (crun-scene-id run0))]
                     ;; Choice 1: Open the door
                     [run1 (chronicle-choose adventure-chronicle run0 1)]
                     [_ (assert-equal 'hallway (crun-scene-id run1))]
                     [_ (assert-true (state-flag? (crun-state run1) 'entered))]
                     ;; Choice 1: Take the key
                     [run2 (chronicle-choose adventure-chronicle run1 1)]
                     [_ (assert-true (state-has-item? (crun-state run2) 'key))]
                     ;; Now "Go to locked room" should be visible
                     [choices (chronicle-visible-choices adventure-chronicle run2)]
                     [_ (assert-equal 2 (length choices))]  ; "Go to locked room" + "Go back"
                     ;; Choice 1: Go to locked room
                     [run3 (chronicle-choose adventure-chronicle run2 1)]
                     [_ (assert-equal 'locked (crun-scene-id run3))])
                    ;; Final assertion
                    (assert-true (state-has-item? (crun-state run3) 'key))))
            
            (define-test integration-conditional-choice
              ;; Start fresh - key not available until taken
              (let* ([run0 (chronicle-start adventure-chronicle)]
                     [run1 (chronicle-choose adventure-chronicle run0 1)]  ; Open door
                     ;; In hallway, no key yet
                     [choices1 (chronicle-visible-choices adventure-chronicle run1)])
                    ;; Should see: "Take the key", "Go back" (not "Go to locked room")
                    (assert-equal 2 (length choices1))
                    (assert-equal "Take the key" (choice-label (car choices1))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Chronicle DSL Tests\n")
(display "========================================\n\n")

(run-all-tests)
