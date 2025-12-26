;;; playpen/creations/continuation-quest.ss — A Continuation-Powered Adventure
;;;
;;; A mini text adventure game using call/cc for time travel mechanics!
;;; Save points, undo, and branching timelines all powered by continuations.
;;;
;;; Created by: Sonnet (Builder tier)
;;; Date: 2025-12-26

(display "=== CONTINUATION QUEST: The Time Traveler's Puzzle ===\n\n")

;;; ============================================================
;;; Game State
;;; ============================================================

(define *checkpoints* '())
(define *timeline-history* '())

;;; save-checkpoint! : String × Continuation → void
(define (save-checkpoint! name cont)
  (set! *checkpoints* (cons (cons name cont) *checkpoints*))
  (display (string-append "💾 Checkpoint saved: " name "\n")))

;;; load-checkpoint : String → void (never returns)
(define (load-checkpoint name)
  (let ([cp (assoc name *checkpoints*)])
    (if cp
        (begin
          (display (string-append "⏪ Loading checkpoint: " name "...\n\n"))
          ((cdr cp) 'restore))
        (display (string-append "❌ No checkpoint named '" name "'\n")))))

;;; ============================================================
;;; Adventure Scenes
;;; ============================================================

(define (scene-1)
  (display "You stand at a crossroads in the forest.\n")
  (display "A mysterious stone pillar glows with ancient runes.\n\n")

  (display "Options:\n")
  (display "  1. Touch the pillar (activate time magic)\n")
  (display "  2. Walk away\n")
  (display "\nChoice: ")
  (flush-output-port)

  (let ([choice (read)])
    (cond
      [(= choice 1)
       (call/cc
         (lambda (return-here)
           (save-checkpoint! "crossroads" return-here)
           (scene-2-time-magic)))]
      [(= choice 2)
       (display "\nYou walk away... but curiosity pulls you back.\n\n")
       (scene-1)]
      [else
       (display "\nInvalid choice. Try again.\n\n")
       (scene-1)])))

(define (scene-2-time-magic)
  (display "\n🌀 The pillar glows! You feel time bending around you...\n\n")
  (display "You can now travel through time using checkpoints!\n")
  (display "Commands:\n")
  (display "  - Type 'back to go to your last checkpoint\n")
  (display "  - Type 'forward to continue\n\n")

  (display "You see two paths:\n")
  (display "  1. The dark cave (dangerous)\n")
  (display "  2. The sunny meadow (peaceful)\n")
  (display "\nChoice: ")
  (flush-output-port)

  (let ([choice (read)])
    (cond
      [(eq? choice 'back) (load-checkpoint "crossroads")]
      [(= choice 1) (scene-3-cave)]
      [(= choice 2) (scene-3-meadow)]
      [else
       (display "\nInvalid choice.\n\n")
       (scene-2-time-magic)])))

(define (scene-3-cave)
  (call/cc
    (lambda (cave-checkpoint)
      (save-checkpoint! "cave-entrance" cave-checkpoint)
      (display "\n🕳️  You enter the dark cave...\n\n")
      (display "It's pitch black. You hear growling.\n")
      (display "Suddenly, a dragon appears! 🐉\n\n")

      (display "Options:\n")
      (display "  1. Fight (you'll probably die)\n")
      (display "  2. Run\n")
      (display "  3. Use time magic (back)\n")
      (display "\nChoice: ")
      (flush-output-port)

      (let ([choice (read)])
        (cond
          [(= choice 1)
           (display "\n💀 The dragon breathes fire! You're toast.\n")
           (display "\n⏪ Time rewinds automatically to the cave entrance...\n\n")
           (cave-checkpoint 'death-rewind)]
          [(= choice 2)
           (display "\n🏃 You run back to the crossroads!\n\n")
           (scene-2-time-magic)]
          [(eq? choice 'back) (load-checkpoint "crossroads")]
          [else (scene-3-cave)])))))

(define (scene-3-meadow)
  (call/cc
    (lambda (meadow-checkpoint)
      (save-checkpoint! "meadow" meadow-checkpoint)
      (display "\n🌸 You walk into a beautiful sunny meadow...\n\n")
      (display "Birds chirp, flowers bloom. You find a treasure chest! 💎\n\n")

      (display "Options:\n")
      (display "  1. Open the chest\n")
      (display "  2. Go back (back)\n")
      (display "\nChoice: ")
      (flush-output-port)

      (let ([choice (read)])
        (cond
          [(= choice 1) (scene-4-victory)]
          [(eq? choice 'back) (load-checkpoint "crossroads")]
          [else (scene-3-meadow)])))))

(define (scene-4-victory)
  (display "\n✨ You open the chest and find...\n")
  (display "    THE SACRED CONTINUATION!\n\n")
  (display "Legend says this continuation can return you to any moment.\n")
  (display "You have mastered time itself!\n\n")

  (display "🎉 QUEST COMPLETE! 🎉\n\n")

  (display "Want to explore other timelines?\n")
  (display "  1. Return to crossroads\n")
  (display "  2. Return to cave\n")
  (display "  3. End game\n")
  (display "\nChoice: ")
  (flush-output-port)

  (let ([choice (read)])
    (cond
      [(= choice 1) (load-checkpoint "crossroads")]
      [(= choice 2) (load-checkpoint "cave-entrance")]
      [(= choice 3)
       (display "\nThank you for playing CONTINUATION QUEST!\n")
       (display "May your continuations always be first-class.\n\n")]
      [else (scene-4-victory)])))

;;; ============================================================
;;; Start the Game!
;;; ============================================================

(display "In this game, you'll use the power of continuations to save\n")
(display "your progress and travel through time!\n\n")
(display "Press ENTER to begin...")
(read-line)
(newline)

(scene-1)
