;;; playpen/chronicle/test-tagless-chronicle.ss — Tests for Tagless Chronicle
;;;
;;; Tests for the tagless final Chronicle DSL.

(load "core/test-framework.ss")
(load "user/chronicle/tagless-chronicle.ss")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; Find a scene by id in the scenes list
(define (find-scene scenes id)
  (let loop ([ss scenes])
       (cond
        [(null? ss) #f]
        [(and (pair? (car ss))
              (eq? (car (car ss)) 'scene)
              (eq? (cadr (car ss)) id))
         (car ss)]
        [else (loop (cdr ss))])))

;;; Check if list contains element
(define (contains? lst elem)
  (and (memq elem lst) #t))

;;; string-contains helper
(define (string-contains haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

;;; ============================================================
;;; Runtime Interpreter Tests
;;; ============================================================

(test-group tagless-chronicle-runtime
            (define-test runtime-creates-scenes
              (let ([scenes (mysterious-door-chronicle runtime-chronicle-dict)])
                   (assert-equal 5 (length scenes))))
            
            (define-test runtime-has-entrance-scene
              (let ([scenes (mysterious-door-chronicle runtime-chronicle-dict)])
                   (assert-true (and (find-scene scenes 'entrance) #t))))
            
            (define-test runtime-scene-has-text
              (let* ([scenes (mysterious-door-chronicle runtime-chronicle-dict)]
                     [entrance (find-scene scenes 'entrance)])
                    (assert-true (string? (caddr entrance)))))
            
            (define-test runtime-guard-always-passes
              (let ([guard ((dict-ref runtime-chronicle-dict 'guard-always))])
                   (assert-true (guard '()))))
            
            (define-test runtime-guard-flag-false-when-missing
              (let ([guard ((dict-ref runtime-chronicle-dict 'guard-flag) 'visited)])
                   (assert-false (guard '()))))
            
            (define-test runtime-guard-flag-true-when-present
              (let ([guard ((dict-ref runtime-chronicle-dict 'guard-flag) 'visited)])
                   (assert-true (guard '((flags . ((visited . #t))))))))
            
            (define-test runtime-guard-item-false-when-missing
              (let ([guard ((dict-ref runtime-chronicle-dict 'guard-item) 'key)])
                   (assert-false (guard '()))))
            
            (define-test runtime-guard-item-true-when-present
              (let ([guard ((dict-ref runtime-chronicle-dict 'guard-item) 'key)])
                   (assert-true (guard '((inventory key sword))))))
            
            (define-test runtime-effect-flag-sets-flag
              (let* ([effect ((dict-ref runtime-chronicle-dict 'effect-flag) 'visited #t)]
                     [state '()]
                     [new-state (effect state)]
                     [flags (assq 'flags new-state)])
                    (assert-true (pair? flags))
                    (assert-true (pair? (assq 'visited (cdr flags))))))
            
            (define-test runtime-effect-item-adds-item
              (let* ([effect ((dict-ref runtime-chronicle-dict 'effect-item) 'add 'key)]
                     [state '()]
                     [new-state (effect state)]
                     [inv (assq 'inventory new-state)])
                    (assert-true (pair? inv))
                    (assert-true (contains? (cdr inv) 'key))))
            
            (define-test runtime-effect-var-sets-variable
              (let* ([effect ((dict-ref runtime-chronicle-dict 'effect-var) 'gold 'set 100)]
                     [state '()]
                     [new-state (effect state)]
                     [vars (assq 'variables new-state)])
                    (assert-true (pair? vars))
                    (assert-equal 100 (cdr (assq 'gold (cdr vars))))))
            
            (define-test runtime-effect-var-adds
              (let* ([set-effect ((dict-ref runtime-chronicle-dict 'effect-var) 'gold 'set 50)]
                     [add-effect ((dict-ref runtime-chronicle-dict 'effect-var) 'gold 'add 25)]
                     [state (set-effect '())]
                     [new-state (add-effect state)])
                    (assert-equal 75 (cdr (assq 'gold (cdr (assq 'variables new-state)))))))
            
            (define-test runtime-effect-seq-chains
              (let* ([e1 ((dict-ref runtime-chronicle-dict 'effect-flag) 'a #t)]
                     [e2 ((dict-ref runtime-chronicle-dict 'effect-flag) 'b #t)]
                     [seq ((dict-ref runtime-chronicle-dict 'effect-seq) e1 e2)]
                     [state '()]
                     [new-state (seq state)]
                     [flags (cdr (assq 'flags new-state))])
                    (assert-true (pair? (assq 'a flags)))
                    (assert-true (pair? (assq 'b flags))))))

;;; ============================================================
;;; Validation Interpreter Tests
;;; ============================================================

(test-group tagless-chronicle-validation
            (define-test validation-collects-scenes
              (let ([result (validate-chronicle mysterious-door-chronicle 'entrance)])
                   (assert-equal 5 (length (cdr (assq 'scenes result))))))
            
            (define-test validation-finds-entrance
              (let* ([result (validate-chronicle mysterious-door-chronicle 'entrance)]
                     [scenes (cdr (assq 'scenes result))])
                    (assert-true (contains? scenes 'entrance))))
            
            (define-test validation-finds-all-scenes
              (let* ([result (validate-chronicle mysterious-door-chronicle 'entrance)]
                     [scenes (cdr (assq 'scenes result))])
                    (assert-true (contains? scenes 'hallway))
                    (assert-true (contains? scenes 'treasure))))
            
            (define-test validation-detects-valid-chronicle
              (let ([result (validate-chronicle mysterious-door-chronicle 'entrance)])
                   (assert-true (cdr (assq 'valid result)))))
            
            (define-test validation-detects-dead-ends
              (let* ([result (validate-chronicle mysterious-door-chronicle 'entrance)]
                     [dead-ends (cdr (assq 'dead-ends result))])
                    ;; outside and ending have no choices
                    (assert-true (contains? dead-ends 'outside))
                    (assert-true (contains? dead-ends 'ending)))))

;;; ============================================================
;;; Graph Export Tests
;;; ============================================================

(test-group tagless-chronicle-graph
            (define-test graph-collects-scenes
              (let ([scenes (mysterious-door-chronicle graph-chronicle-dict)])
                   (assert-true (pair? scenes))
                   (assert-equal 5 (length scenes))))
            
            (define-test dot-export-produces-string
              (let ([dot (chronicle->dot mysterious-door-chronicle 'test)])
                   (assert-true (string? dot))
                   (assert-true (> (string-length dot) 0))))
            
            (define-test dot-contains-graph-header
              (let ([dot (chronicle->dot mysterious-door-chronicle 'test)])
                   (assert-true (string-contains dot "digraph"))))
            
            (define-test mermaid-export-produces-string
              (let ([mermaid (chronicle->mermaid mysterious-door-chronicle)])
                   (assert-true (string? mermaid))
                   (assert-true (string-contains mermaid "flowchart")))))

;;; ============================================================
;;; Analysis Interpreter Tests
;;; ============================================================

(test-group tagless-chronicle-analysis
            (define-test analysis-returns-alist
              (let ([result (analyze-chronicle mysterious-door-chronicle)])
                   (assert-true (pair? result))
                   (assert-true (pair? (assq 'scene-count result)))))
            
            (define-test analysis-counts-scenes
              (let ([result (analyze-chronicle mysterious-door-chronicle)])
                   (assert-equal 5 (cdr (assq 'scene-count result)))))
            
            (define-test analysis-counts-choices
              (let ([result (analyze-chronicle mysterious-door-chronicle)])
                   ;; entrance: 4, hallway: 2, treasure: 1, outside: 0, ending: 0 = 7
                   (assert-equal 7 (cdr (assq 'total-choices result))))))

;;; ============================================================
;;; Runtime Execution Tests
;;; ============================================================

(test-group tagless-chronicle-execution
            (define-test runtime-creates-state
              (let ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)])
                   (assert-true (list? runtime))
                   (assert-equal 'entrance (list-ref runtime 2))))
            
            (define-test runtime-gets-current-scene
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     [scene (chronicle-runtime-scene runtime)])
                    (assert-equal 'scene (car scene))
                    (assert-equal 'entrance (cadr scene))))
            
            (define-test runtime-gets-text
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     [text (chronicle-runtime-text runtime)])
                    (assert-true (string? text))
                    (assert-true (string-contains text "mysterious door"))))
            
            (define-test runtime-filters-choices-by-guard
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     [choices (chronicle-runtime-choices runtime)])
                    ;; Without key: "Try the door", "Search the area", "Leave" = 3
                    ;; "Use the key" should be filtered out
                    (assert-equal 3 (length choices))))
            
            (define-test runtime-choose-transitions-to-new-scene
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     ;; Choose "Leave" (should be last visible choice, idx 2)
                     [new-runtime (chronicle-runtime-choose runtime 2)])
                    (assert-equal 'outside (list-ref new-runtime 2))))
            
            (define-test runtime-search-adds-key
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     ;; Search the area (choice index 1) adds key
                     [new-runtime (chronicle-runtime-choose runtime 1)]
                     [state (list-ref new-runtime 3)]
                     [inv (assq 'inventory state)])
                    ;; Should now have key
                    (assert-true (pair? inv))
                    (assert-true (contains? (cdr inv) 'key))))
            
            (define-test runtime-with-key-shows-different-choices
              (let* ([runtime (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
                     ;; Search to get key (choice idx 1)
                     [with-key (chronicle-runtime-choose runtime 1)]
                     [choices (chronicle-runtime-choices with-key)])
                    ;; With key: "Use the key" and "Leave" are visible
                    ;; "Try the door" and "Search the area" hidden (have guard-not item key)
                    (assert-equal 2 (length choices)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Tagless Chronicle Tests\n")
(display "========================================\n\n")

(run-all-tests)
