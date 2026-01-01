;;; playpen/satin/examples/test-examples.ss — Test that examples compile
;;;
;;; Run from project root:
;;;   scheme --script playpen/satin/examples/test-examples.ss

;; Load prelude and Satin
(load "core/prelude.ss")
(load "user/satin/satin.ss")

(display "Testing Satin Examples
")
(display "======================

")

;; Test 1: scheme-basics.satin
(display "1. scheme-basics.satin... ")
(let ([spec (call-with-input-file "user/satin/examples/scheme-basics.satin" read)])
     (let-values ([(story issues) (satin-compile spec)])
                 (let ([errors (filter satin-error? issues)])
                      (if (null? errors)
                          (begin
                           (display "OK
")
                           (display "   - Story ID: ")
                           (display (quill-story-id story))
                           (display "
")
                           (display "   - Nodes: ")
                           (display (length (quill-story-nodes story)))
                           (display "
")
                           (let ([meta (quill-story-meta story)])
                                (when (assq 'lessons meta)
                                      (display "   - Lessons: ")
                                      (display (length (cdr (assq 'lessons meta))))
                                      (display "
"))
                                (when (assq 'masteries meta)
                                      (display "   - Masteries: ")
                                      (display (length (cdr (assq 'masteries meta))))
                                      (display "
"))
                                (when (assq 'exercises meta)
                                      (display "   - Exercises: ")
                                      (display (length (cdr (assq 'exercises meta))))
                                      (display "
"))))
                          (begin
                           (display "FAILED
")
                           (for-each
                            (lambda (e)
                                    (display "   ERROR: ")
                                    (display (satin-format-issue e))
                                    (display "
"))
                            errors))))))

(display "
")

;; Test 2: the-garden.satin
(display "2. the-garden.satin... ")
(let ([spec (call-with-input-file "user/satin/examples/the-garden.satin" read)])
     (let-values ([(story issues) (satin-compile spec)])
                 (let ([errors (filter satin-error? issues)])
                      (if (null? errors)
                          (begin
                           (display "OK
")
                           (display "   - Story ID: ")
                           (display (quill-story-id story))
                           (display "
")
                           (display "   - Nodes: ")
                           (display (length (quill-story-nodes story)))
                           (display "
")
                           (let ([meta (quill-story-meta story)])
                                (when (assq 'quests meta)
                                      (display "   - Quests: ")
                                      (display (length (cdr (assq 'quests meta))))
                                      (display "
"))
                                (when (assq 'dialogues meta)
                                      (display "   - Dialogues: ")
                                      (display (length (cdr (assq 'dialogues meta))))
                                      (display "
"))
                                (when (assq 'rules meta)
                                      (display "   - Rules: ")
                                      (display (length (cdr (assq 'rules meta))))
                                      (display "
"))
                                (when (assq 'timeline meta)
                                      (display "   - Timeline events: ")
                                      (display (length (cdr (assq 'timeline meta))))
                                      (display "
"))))
                          (begin
                           (display "FAILED
")
                           (for-each
                            (lambda (e)
                                    (display "   ERROR: ")
                                    (display (satin-format-issue e))
                                    (display "
"))
                            errors))))))

(display "
======================
")
(display "Examples test complete!
")
