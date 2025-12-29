;;; thimble/duckie-interact.ss — DUCKIE REPL Interaction
;;;
;;; Simple REPL commands for talking to DUCKIE.
;;; Loads DUCKIE's dialogue system and provides convenient commands.
;;;
;;; This is Shell code: provides IO functions for player interaction.

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; Load DUCKIE dialogue
(load "playpen/creations/duckie-dialogue.ss")

;;; ============================================================
;;; State
;;; ============================================================

;;; DUCKIE's current mood (persists across REPL calls in daemon)
(define *duckie-mood* 'curious)

;;; Available moods (from playpen/duckie.ss)
(define *duckie-valid-moods*
  '(happy curious sleepy content lonely playful))

;;; ============================================================
;;; Commands
;;; ============================================================

;;; to-duckie : String → String
;;; Send a message to DUCKIE and get a response based on current mood.
;;; The message is ignored for now (simple version), response is mood-based.
(define (to-duckie msg)
  (let ([response (get-talk-response *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* response))
       response))

;;; duckie-mood : → Symbol
;;; Show DUCKIE's current mood
(define (duckie-mood)
  (display (format "~nDUCKIE is feeling ~a~n~n" *duckie-mood*))
  *duckie-mood*)

;;; set-duckie-mood! : Symbol → void
;;; Change DUCKIE's mood
(define (set-duckie-mood! mood)
  (if (memq mood *duckie-valid-moods*)
      (begin
       (set! *duckie-mood* mood)
       (display (format "~nDUCKIE's mood changed to ~a~n~n" mood)))
      (display (format "~nInvalid mood. Valid moods: ~a~n~n" *duckie-valid-moods*))))

;;; duckie-greet : → String
;;; Get a greeting from DUCKIE based on current mood
(define (duckie-greet)
  (let ([greeting (get-greeting *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* greeting))
       greeting))

;;; duckie-farewell : → String
;;; Get a farewell from DUCKIE based on current mood
(define (duckie-farewell)
  (let ([farewell (get-farewell *duckie-mood*)])
       (display (format "~n  DUCKIE (~a): ~a~n~n" *duckie-mood* farewell))
       farewell))
