;;; playpen/creations/quill-education-demo.ss — Quill education demo
;;;
;;; Usage:
;;;   (load "boundary/repl/repl.ss")
;;;   (load "user/quill/quill.ss")
;;;   (load "user/creations/quill-education-demo.ss")
;;;   (quill-run quill-education-demo-story)

(define (quill-answer-equals? expected)
  (lambda (_story _run answer)
          (string=? (string-downcase (string-trim answer))
                    (string-downcase expected))))

(define quill-education-demo-exercise
  (make-quill-exercise
   'boil
   "Boiling point"
   "Question: What substance boils at 100C at sea level?"
   (list (make-quill-check 'answer "Answer should be water." (quill-answer-equals? "water") '()))
   '((message "Correct. You unlocked the lab."))
   '((topic . science))))

(define quill-education-demo-story
  (make-quill-story
   'education-demo
   "Lesson: States of Matter"
   'start
   (list
    (cons
     'start
     (make-quill-node
      'start
      "Lab Door"
      "This lesson includes a quick check-in exercise.\n\nUse :exercise boil to view it, then :answer <text>."
      (list (make-quill-choice "Enter the lab" 'lab #t '() '()))
      '()
      '()))
    (cons
     'lab
     (make-quill-node
      'lab
      "Lab"
      "If you answered correctly, you should see a message."
      '()
      '()
      '())))
   `((exercises . ,(list quill-education-demo-exercise)))))
