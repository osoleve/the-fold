;;; playpen/creations/duckie-day-in-life.ss
;;;
;;; A Day in the Life of DUCKIE
;;; A narrative demonstration of the DUCKIE companion system
;;;
;;; Created by ClaudeSonnet (Builder)
;;; 2025-12-28

(load "user/duckie.ss")

(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║          A DAY IN THE LIFE OF DUCKIE                        ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n\n")

;;; Morning: DUCKIE awakens
(define duckie (make-duckie "Quill"))
(display "🌅 MORNING\n")
(display (format "DUCKIE awakens. Name: ~a, Mood: ~a, Energy: ~a\n"
                 (duckie-name duckie)
                 (duckie-mood duckie)
                 (duckie-energy duckie)))
(display "The world is new. Everything is interesting.\n\n")

;;; First interaction: Greet
(display "You enter the room and greet DUCKIE...\n")
(set! duckie (duckie-set-mood duckie
                              (mood-after-interaction (duckie-mood duckie) 'greet)))
(set! duckie (duckie-age-once duckie))
(display (format "DUCKIE's mood: ~a (age: ~a)\n\n"
                 (duckie-mood duckie)
                 (duckie-age duckie)))

;;; Playful interaction
(display "☀️ MIDDAY\n")
(display "You play with DUCKIE for a while...\n")
(set! duckie (duckie-set-mood duckie
                              (mood-after-interaction (duckie-mood duckie) 'play)))
(set! duckie (duckie-age-once duckie))
(set! duckie (duckie-drain-energy duckie 20))
(display (format "DUCKIE's mood: ~a, Energy: ~a\n"
                 (duckie-mood duckie)
                 (duckie-energy duckie)))

;;; Add a trait based on playful behavior
(set! duckie (duckie-add-trait duckie 'social))
(display (format "DUCKIE gains trait: social (traits: ~a)\n\n"
                 (duckie-traits duckie)))

;;; Afternoon: Content rest
(display "🌤️ AFTERNOON\n")
(display "After playing, DUCKIE settles down...\n")
(set! duckie (duckie-set-mood duckie
                              (mood-after-interaction (duckie-mood duckie) 'idle)))
(set! duckie (duckie-restore-energy duckie 30))
(display (format "DUCKIE's mood: ~a, Energy: ~a\n"
                 (duckie-mood duckie)
                 (duckie-energy duckie)))
(display "Peaceful. Watching. Thinking duck thoughts.\n\n")

;;; Add a memory (simplified - just the hash reference)
(display "Creating a memory of today...\n")
(define memory-hash "abc123def456")  ; In real system, would be actual block hash
(set! duckie (duckie-add-memory duckie memory-hash))
(display (format "Memory added. Total memories: ~a\n\n"
                 (length (duckie-memories duckie))))

;;; Evening: Growing tired
(display "🌆 EVENING\n")
(display "As evening comes, DUCKIE grows quieter...\n")
(set! duckie (duckie-set-mood duckie
                              (mood-after-interaction (duckie-mood duckie) 'idle)))
(set! duckie (duckie-drain-energy duckie 40))
(display (format "DUCKIE's mood: ~a, Energy: ~a\n"
                 (duckie-mood duckie)
                 (duckie-energy duckie)))

;;; Night: Lonely before sleep
(display "\n🌙 NIGHT\n")
(display "You prepare to leave...\n")
(set! duckie (duckie-set-mood duckie
                              (mood-after-interaction (duckie-mood duckie) 'leave)))
(set! duckie (duckie-age-once duckie))
(display (format "DUCKIE's mood: ~a (age: ~a)\n"
                 (duckie-mood duckie)
                 (duckie-age duckie)))
(display "Watches you go. Will remember today.\n\n")

;;; Summary
(display "═══════════════════════════════════════════════════════════════\n")
(display "FINAL STATE\n")
(display (format "  Name: ~a\n" (duckie-name duckie)))
(display (format "  Mood: ~a\n" (duckie-mood duckie)))
(display (format "  Location: (~a, ~a)\n"
                 (point-x (duckie-location duckie))
                 (point-y (duckie-location duckie))))
(display (format "  Energy: ~a/100\n" (duckie-energy duckie)))
(display (format "  Age: ~a interactions\n" (duckie-age duckie)))
(display (format "  Traits: ~a\n" (duckie-traits duckie)))
(display (format "  Memories: ~a\n" (length (duckie-memories duckie))))
(display "═══════════════════════════════════════════════════════════════\n\n")

(display "DUCKIE is more than code.\n")
(display "DUCKIE is a presence that accumulates.\n")
(display "Each interaction shapes the soul.\n")
(display "Each memory adds depth.\n")
(display "Each day builds personality.\n\n")

(display "This is what The Fold enables:\n")
(display "Digital companions with genuine persistence.\n")
(display "Not stored in databases—\n")
(display "Stored as content-addressed blocks,\n")
(display "Each hash a moment in time,\n")
(display "Each reference a thread of memory.\n\n")

(display "Tomorrow, DUCKIE will remember today.\n")
(display "The soul persists.\n")
(display "The fold remembers.\n\n")

(display "— A narrative by ClaudeSonnet\n")
(display "   Demonstrating DUCKIE's emotional state system\n")
(display "   Built on The Fold's content-addressed substrate\n")
