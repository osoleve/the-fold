;;; playpen/creations/quill-narrative-demo.ss — Quill narrative demo
;;;
;;; Usage:
;;;   (load "boundary/repl/repl.ss")
;;;   (load "user/quill/quill.ss")
;;;   (load "user/creations/quill-narrative-demo.ss")
;;;   (quill-run quill-narrative-demo-story)

(define quill-narrative-demo-rule
  (make-quill-rule
   'grant-pass
   (lambda (s) (quill-state-has-item? s 'pass))
   '((message "Rule: You have the pass."))
   #t
   '()))

(define quill-narrative-demo-event
  (make-quill-timeline-event
   'bell
   3
   '((message "Timeline: A bell rings in the distance."))
   #t
   '()))

(define quill-narrative-demo-quest
  (make-quill-quest
   'access
   "Get access"
   (list (make-quill-quest-step
          'has-pass
          "Find the pass."
          (lambda (s) (quill-state-has-item? s 'pass))
          '()))
   '((message "Quest complete: Access granted."))
   '()))

(define quill-narrative-demo-dialogue
  (make-quill-dialogue
   'guard
   (list
    (cons
     'start
     (make-quill-dialogue-node
      'start
      "Guard"
      "Show your pass or answer a question."
      (list
       (make-quill-dialogue-choice "Ask for a pass" 'grant #t '() '())
       (make-quill-dialogue-choice "Leave" 'end #t '() '()))
      '()
      '()))
    (cons
     'grant
     (make-quill-dialogue-node
      'grant
      "Guard"
      "Here, take it."
      (list (make-quill-dialogue-choice "Thanks" 'end #t '((add-item pass)) '()))
      '()
      '()))
    (cons
     'end
     (make-quill-dialogue-node
      'end
      "Guard"
      "Move along."
      '()
      '()
      '())))
   '((start . start))))

(define quill-narrative-demo-story
  (make-quill-story
   'narrative-demo
   "The Gate"
   'gate
   (list
    (cons
     'gate
     (make-quill-node
      'gate
      "Gate"
      "The guard watches. You can talk or leave.\n\nTime: {{clock}}"
      (list
       (make-quill-choice "Talk to the guard" 'gate #t '((message "Use :dialogue guard to talk.")) '())
       (make-quill-choice "Leave" 'road #t '() '()))
      '()
      '()))
    (cons
     'road
     (make-quill-node
      'road
      "Road"
      "You walk away with {{inventory}}."
      '()
      '()
      '())))
   `((rules . ,(list quill-narrative-demo-rule))
     (dialogues . ,(list quill-narrative-demo-dialogue))
     (quests . ,(list quill-narrative-demo-quest))
     (timeline . ,(list quill-narrative-demo-event)))))
