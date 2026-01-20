;;; Load DUCKIE dialogue
(load "core/base/prelude.ss")
(load "user/creations/duckie-dialogue.ss")

(doc 'module 'boundary/assistants/duckie-interact)
(doc 'description "DUCKIE REPL Interaction - simple commands for talking to DUCKIE")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Shell code providing IO functions for player interaction")

(doc 'section 'state)

;;; DUCKIE's current mood (persists across REPL calls in daemon)
(define *duckie-mood* 'curious)

(doc 'section 'available-moods)

(define *duckie-valid-moods*
  '(happy curious sleepy content lonely playful))

(doc 'section 'commands)
(define (to-duckie msg)
  (doc 'type (-> String String))
  (doc 'description "Send a message to DUCKIE and get a mood-based response")
  (doc 'note "Message is currently ignored - response is mood-based")
  (let ([response (get-talk-response *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* response))
       response))

(define (duckie-mood)
  (doc 'type (-> Symbol))
  (doc 'description "Show DUCKIE's current mood")
  (display (format "~nDUCKIE is feeling ~a~n~n" *duckie-mood*))
  *duckie-mood*)

(define (set-duckie-mood! mood)
  (doc 'type (-> Symbol Void))
  (doc 'description "Change DUCKIE's mood to a valid mood symbol")
  (if (memq mood *duckie-valid-moods*)
      (begin
       (set! *duckie-mood* mood)
       (display (format "~nDUCKIE's mood changed to ~a~n~n" mood)))
      (display (format "~nInvalid mood. Valid moods: ~a~n~n" *duckie-valid-moods*))))

(define (duckie-greet)
  (doc 'type (-> String))
  (doc 'description "Get a greeting from DUCKIE based on current mood")
  (let ([greeting (get-greeting *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* greeting))
       greeting))

(define (duckie-farewell)
  (doc 'type (-> String))
  (doc 'description "Get a farewell from DUCKIE based on current mood")
  (let ([farewell (get-farewell *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* farewell))
       farewell))
