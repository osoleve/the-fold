;;; playpen/chronicle/playtest-chronicle.ss — Interactive Chronicle Playtest
;;;
;;; Play through "The Mysterious Door" chronicle to test the engine.

(load "user/chronicle/tagless-chronicle.ss")

(display "========================================\n")
(display "Chronicle Engine Playtest\n")
(display "========================================\n\n")

;;; Helper to extract choice text
(define (choice-text c)
  (cadr c))  ; (choice text target guard effect)

;;; Create a runtime and test a specific path
(define (test-path)
  (display "Testing a story path:\n")
  (display "---------------------\n\n")
  
  ;; Start at entrance
  (let* ([runtime1 (make-chronicle-runtime mysterious-door-chronicle 'entrance)]
         [text1 (chronicle-runtime-text runtime1)]
         [choices1 (chronicle-runtime-choices runtime1)])
        (display (format "Scene: entrance\n"))
        (display (format "Text: ~a\n" text1))
        (display (format "Choices: ~a\n" (map choice-text choices1)))
        (display (format "Choice count: ~a\n\n" (length choices1)))
        
        ;; Choose "Search the area" (index 1)
        (let* ([runtime2 (chronicle-runtime-choose runtime1 1)]
               [text2 (chronicle-runtime-text runtime2)]
               [choices2 (chronicle-runtime-choices runtime2)]
               [state2 (list-ref runtime2 3)])
              (display (format "After searching (choice 1):\n"))
              (display (format "Scene: ~a\n" (list-ref runtime2 2)))
              (display (format "State: ~a\n" state2))
              (display (format "Choices: ~a\n" (map choice-text choices2)))
              (display (format "Choice count: ~a\n\n" (length choices2)))
              
              ;; Now we should have the key - try "Use the key" (should be index 0)
              (let* ([runtime3 (chronicle-runtime-choose runtime2 0)]
                     [text3 (chronicle-runtime-text runtime3)]
                     [choices3 (chronicle-runtime-choices runtime3)])
                    (display (format "After using key:\n"))
                    (display (format "Scene: ~a\n" (list-ref runtime3 2)))
                    (display (format "Text: ~a\n" text3))
                    (display (format "Choices: ~a\n" (map choice-text choices3)))
                    (display (format "Choice count: ~a\n\n" (length choices3)))
                    
                    (display "Playthrough complete!\n\n")))))

;;; Run validation
(display "1. Validation Check\n")
(display "-------------------\n")
(let ([result (validate-chronicle mysterious-door-chronicle 'entrance)])
     (display (format "Valid: ~a\n" (cdr (assq 'valid result))))
     (display (format "Scenes: ~a\n" (cdr (assq 'scenes result))))
     (display (format "Dead ends: ~a\n\n" (cdr (assq 'dead-ends result)))))

;;; Run analysis
(display "2. Analysis\n")
(display "-----------\n")
(let ([result (analyze-chronicle mysterious-door-chronicle)])
     (display (format "Scene count: ~a\n" (cdr (assq 'scene-count result))))
     (display (format "Total choices: ~a\n" (cdr (assq 'total-choices result))))
     (display (format "Total text length: ~a chars\n" (cdr (assq 'total-text-length result))))
     (display (format "Avg choices/scene: ~a\n" (cdr (assq 'avg-choices-per-scene result))))
     (display (format "Avg text/scene: ~a chars\n\n" (cdr (assq 'avg-text-per-scene result)))))

;;; Generate graph exports
(display "3. Graph Export\n")
(display "---------------\n")
(let ([dot (chronicle->dot mysterious-door-chronicle 'mysterious_door)])
     (display (format "DOT graph: ~a bytes\n" (string-length dot))))
(let ([mermaid (chronicle->mermaid mysterious-door-chronicle)])
     (display (format "Mermaid graph: ~a bytes\n\n" (string-length mermaid))))

;;; Play through the story
(display "4. Story Execution Test\n")
(display "------------------------\n")
(test-path)

(display "✓ Playtest complete!\n")
