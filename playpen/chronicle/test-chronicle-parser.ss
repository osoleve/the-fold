;;; playpen/chronicle/test-chronicle-parser.ss — Tests for Chronicle Parser
;;;
;;; Tests for the parser combinator-based Chronicle parser.

(load "fabric/stitches/test-framework.ss")
(load "playpen/chronicle/chronicle-parser.ss")

;;; ============================================================
;;; Lexical Tests
;;; ============================================================

(test-group chronicle-parser-lexical
            (define-test parse-identifier
              (let ([result (parse identifier "hello_world")])
                   (assert-true (right? result))
                   (assert-equal 'hello_world (from-right result))))
            
            (define-test parse-identifier-with-dash
              (let ([result (parse identifier "my-var")])
                   (assert-true (right? result))
                   (assert-equal 'my-var (from-right result))))
            
            (define-test parse-quoted-string-simple
              (let ([result (parse quoted-string "\"hello\"")])
                   (assert-true (right? result))
                   (assert-equal "hello" (from-right result))))
            
            (define-test parse-quoted-string-escapes
              (let ([result (parse quoted-string "\"line1\\nline2\"")])
                   (assert-true (right? result))
                   (assert-equal "line1\nline2" (from-right result))))
            
            (define-test parse-number-positive
              (let ([result (parse parse-number "42")])
                   (assert-true (right? result))
                   (assert-equal 42 (from-right result))))
            
            (define-test parse-number-negative
              (let ([result (parse parse-number "-10")])
                   (assert-true (right? result))
                   (assert-equal -10 (from-right result)))))

;;; ============================================================
;;; Guard Expression Tests
;;; ============================================================

(test-group chronicle-parser-guards
            (define-test parse-guard-flag
              (let ([result (parse parse-guard "flag?(visited)")])
                   (assert-true (right? result))
                   (assert-equal '(flag? visited) (from-right result))))
            
            (define-test parse-guard-has-item
              (let ([result (parse parse-guard "has-item?(key)")])
                   (assert-true (right? result))
                   (assert-equal '(has-item? key) (from-right result))))
            
            (define-test parse-guard-var-ge
              (let ([result (parse parse-guard "var>=(score, 100)")])
                   (assert-true (right? result))
                   (assert-equal '(var>= score 100) (from-right result))))
            
            (define-test parse-guard-true
              (let ([result (parse parse-guard "true")])
                   (assert-true (right? result))
                   (assert-equal #t (from-right result))))
            
            (define-test parse-guard-false
              (let ([result (parse parse-guard "false")])
                   (assert-true (right? result))
                   (assert-equal #f (from-right result)))))

;;; ============================================================
;;; Effect Expression Tests
;;; ============================================================

(test-group chronicle-parser-effects
            (define-test parse-effect-flag
              (let ([result (parse parse-effect "flag!(visited)")])
                   (assert-true (right? result))
                   (assert-equal '(flag! visited) (from-right result))))
            
            (define-test parse-effect-set
              (let ([result (parse parse-effect "set!(health, 100)")])
                   (assert-true (right? result))
                   (assert-equal '(set! health 100) (from-right result))))
            
            (define-test parse-effect-inc
              (let ([result (parse parse-effect "inc!(gold, 50)")])
                   (assert-true (right? result))
                   (assert-equal '(inc! gold 50) (from-right result))))
            
            (define-test parse-effect-add-item
              (let ([result (parse parse-effect "add-item!(sword)")])
                   (assert-true (right? result))
                   (assert-equal '(add-item! sword) (from-right result)))))

;;; ============================================================
;;; Choice Tests
;;; ============================================================

(test-group chronicle-parser-choices
            (define-test parse-simple-choice
              (let ([result (parse parse-choice "-> \"Go north\" => north")])
                   (assert-true (right? result))
                   (let ([choice (from-right result)])
                        (assert-equal '-> (car choice))
                        (assert-equal "Go north" (cadr choice))
                        (assert-equal 'north (caddr choice)))))
            
            (define-test parse-choice-with-guard
              (let ([result (parse parse-choice "-> \"Open door\" => room [when: flag?(has_key)]")])
                   (assert-true (right? result))
                   (let ([choice (from-right result)])
                        (assert-equal "Open door" (cadr choice))
                        (assert-equal 'room (caddr choice))
                        ;; Check guard keyword is present (flat format: -> label target :when guard)
                        (assert-equal ':when (cadddr choice)))))
            
            (define-test parse-choice-with-effect
              (let ([result (parse parse-choice "-> \"Take key\" => here [do: add-item!(key)]")])
                   (assert-true (right? result))
                   (let ([choice (from-right result)])
                        (assert-equal "Take key" (cadr choice))))))

;;; ============================================================
;;; Scene Tests
;;; ============================================================

(test-group chronicle-parser-scenes
            (define-test parse-simple-scene
              (let ([result (parse parse-scene "scene intro { \"Welcome!\" }")])
                   (assert-true (right? result))
                   (let ([scene (from-right result)])
                        (assert-equal 'scene (car scene))
                        (assert-equal 'intro (cadr scene))
                        (assert-equal "Welcome!" (caddr scene)))))
            
            (define-test parse-scene-with-choice
              (let ([result (parse parse-scene "scene start { \"Hello\" -> \"Continue\" => next }")])
                   (assert-true (right? result))
                   (let ([scene (from-right result)])
                        (assert-equal 'scene (car scene))
                        (assert-equal 'start (cadr scene))
                        (assert-equal "Hello" (caddr scene))
                        ;; Should have a choice
                        (assert-true (> (length scene) 3)))))
            
            (define-test parse-scene-with-on-enter
              (let ([result (parse parse-scene "scene hall { \"The hall.\" on-enter { flag!(entered) } }")])
                   (assert-true (right? result))
                   (let ([scene (from-right result)])
                        ;; Check on-enter is present
                        (assert-true (pair? (member 'on-enter (map (lambda (x) (if (pair? x) (car x) x)) scene))))))))

;;; ============================================================
;;; Full Chronicle Tests
;;; ============================================================

(test-group chronicle-parser-full
            (define test-chronicle-source "
chronicle \"Test Story\" {
  start: intro

  scene intro {
    \"You are at the beginning.\"
    -> \"Continue\" => middle
  }

  scene middle {
    \"You are in the middle.\"
    -> \"End\" => ending
  }

  scene ending {
    \"The end.\"
  }
}
")
            
            (define-test parse-full-chronicle
              (let ([result (parse-chronicle-text test-chronicle-source)])
                   (assert-true (right? result))
                   (let ([chron (from-right result)])
                        (assert-true (chronicle? chron))
                        (assert-equal "Test Story" (chronicle-title chron))
                        (assert-equal 'intro (chronicle-start-scene chron)))))
            
            (define-test parsed-chronicle-has-scenes
              (let ([result (parse-chronicle-text test-chronicle-source)])
                   (assert-true (right? result))
                   (let ([chron (from-right result)])
                        (assert-equal 3 (length (chronicle-scenes chron)))
                        (assert-true (chronicle-has-scene? chron 'intro))
                        (assert-true (chronicle-has-scene? chron 'middle))
                        (assert-true (chronicle-has-scene? chron 'ending)))))
            
            (define-test parsed-chronicle-playable
              (let ([result (parse-chronicle-text test-chronicle-source)])
                   (assert-true (right? result))
                   (let* ([chron (from-right result)]
                          [run (chronicle-start chron)])
                         ;; Should start at intro
                         (assert-equal 'intro (crun-scene-id run))
                         ;; Should have visible choices
                         (assert-equal 1 (length (chronicle-visible-choices chron run)))))))

;;; ============================================================
;;; Complex Chronicle Test
;;; ============================================================

(test-group chronicle-parser-complex
            (define complex-source "
chronicle \"Adventure\" {
  start: entrance

  scene entrance {
    \"A mysterious door blocks your path.\"
    -> \"Open door\" => hallway [when: has-item?(key)]
    -> \"Search area\" => entrance [do: add-item!(key)]
    -> \"Leave\" => outside
  }

  scene hallway {
    \"The hallway stretches into darkness.\"
    on-enter { flag!(entered_hall) }
    -> \"Continue\" => treasure
    -> \"Go back\" => entrance
  }

  scene treasure {
    \"You found the treasure!\"
    on-enter { inc!(gold, 1000) }
  }

  scene outside {
    \"You leave the dungeon.\"
  }
}
")
            
            (define-test parse-complex-chronicle
              (let ([result (parse-chronicle-text complex-source)])
                   (assert-true (right? result))
                   (let ([chron (from-right result)])
                        (assert-equal 4 (length (chronicle-scenes chron))))))
            
            (define-test complex-chronicle-conditional-choices
              (let ([result (parse-chronicle-text complex-source)])
                   (assert-true (right? result))
                   (let* ([chron (from-right result)]
                          [run (chronicle-start chron)]
                          ;; At entrance, no key yet
                          [choices (chronicle-visible-choices chron run)])
                         ;; "Open door" should NOT be visible (needs key)
                         ;; "Search area" and "Leave" should be visible
                         (assert-equal 2 (length choices)))))
            
            (define-test complex-chronicle-effects-work
              (let ([result (parse-chronicle-text complex-source)])
                   (assert-true (right? result))
                   (let* ([chron (from-right result)]
                          [run0 (chronicle-start chron)]
                          ;; Search area (choice 1) to get key
                          [run1 (chronicle-choose chron run0 1)])
                         ;; Should now have key
                         (assert-true (state-has-item? (crun-state run1) 'key))
                         ;; Now "Open door" should be visible
                         (let ([choices (chronicle-visible-choices chron run1)])
                              ;; All 3 choices should be visible now
                              (assert-equal 3 (length choices)))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Chronicle Parser Tests\n")
(display "========================================\n\n")

(run-all-tests)
