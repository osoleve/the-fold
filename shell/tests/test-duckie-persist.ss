;;; shell/test-duckie-persist.ss — Tests for DUCKIE CAS Persistence
;;;
;;; Tests save/load, state updates, and memory operations.

;;; Ensure we're running from ccverse root
(source-directories (cons "shell" (cons "core" (cons "user" (source-directories)))))

(load "shell/duckie-persist.ss")

(display "DUCKIE Persistence Tests\n")
(display "========================\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ============================================================
;;; Setup: Clear state
;;; ============================================================

(display "Setup:\n")

;; Clear CAS and session state
(hashtable-clear! *store*)
(hashtable-clear! *pinned*)
(set! *current-duckie* #f)
(set! *current-duckie-hash* #f)

(test "initial no active duckie" #f (current-duckie))

;;; ============================================================
;;; Create New DUCKIE
;;; ============================================================

(display "\nCreate New DUCKIE:\n")

(let ([d (new-duckie! "Quackers")])
     (test "new-duckie! returns duckie" 'duckie (car d))
     (test "duckie has name" "Quackers" (duckie-name d))
     (test "duckie initial mood" 'curious (duckie-mood d))
     (test "duckie initial energy" 100 (duckie-energy d))
     (test "duckie initial age" 0 (duckie-age d)))

(test "current-duckie set" #t (pair? (current-duckie)))
(test "current-duckie-hash set" #t (bytevector? (current-duckie-hash)))

;;; ============================================================
;;; Save/Load Round-Trip
;;; ============================================================

(display "\nSave/Load Round-Trip:\n")

(let* ([hash (current-duckie-hash)]
       [loaded (load-duckie hash)])
      (test "load-duckie returns duckie" 'duckie (car loaded))
      (test "loaded name matches" "Quackers" (duckie-name loaded))
      (test "loaded mood matches" 'curious (duckie-mood loaded)))

(test "duckie-exists?" #t (duckie-exists? (current-duckie-hash)))

;;; ============================================================
;;; State Updates
;;; ============================================================

(display "\nState Updates:\n")

;; Pet
(let ([d (pet!)])
     (test "pet! ages duckie" 1 (duckie-age d))
     (test "pet! changes mood to happy" 'happy (duckie-mood d))
     (test "pet! drains energy" 95 (duckie-energy d)))

;; Play
(let ([d (play!)])
     (test "play! sets playful mood" 'playful (duckie-mood d))
     (test "play! drains more energy" 80 (duckie-energy d)))

;; Rest
(let ([d (rest!)])
     (test "rest! sets sleepy mood" 'sleepy (duckie-mood d))
     (test "rest! restores energy" 100 (duckie-energy d)))  ; Max 100

;; Feed
(let ([d (feed!)])
     (test "feed! sets happy mood" 'happy (duckie-mood d)))

;; Idle
(let* ([before-hash (current-duckie-hash)]
       [d (idle!)])
      (test "idle! changes state" #f (equal? before-hash (current-duckie-hash))))

;;; ============================================================
;;; Memory Operations
;;; ============================================================

(display "\nMemory Operations:\n")

(let ([d (remember! 'interaction "First pet")])
     (test "remember! adds memory" 1 (length (duckie-memories d))))

(let ([d (remember! 'milestone "Learned to quack")])
     (test "remember! adds another" 2 (length (duckie-memories d))))

(let ([memories (recall 2)])
     (test "recall returns memories" 2 (length memories))
     (test "memories are blocks" #t (andmap block? memories)))

;;; ============================================================
;;; Adopt Existing DUCKIE
;;; ============================================================

(display "\nAdopt Existing DUCKIE:\n")

;; Save current hash
(let ([quackers-hash (current-duckie-hash)])
     ;; Create a new DUCKIE
     (new-duckie! "Waddles")
     (test "new duckie created" "Waddles" (duckie-name (current-duckie)))
     
     ;; Adopt Quackers back
     (adopt-duckie! quackers-hash)
     (test "adopted Quackers" "Quackers" (duckie-name (current-duckie))))

;;; ============================================================
;;; Pinning
;;; ============================================================

(display "\nPinning:\n")

(test "current duckie is pinned" #t (pinned? (current-duckie-hash)))

(let* ([gc-before (gc-stats)]
       [pinned-before (cdr (assq 'pinned gc-before))])
      ;; Memories should also be pinned
      (test "has pinned blocks" #t (> pinned-before 0)))

;;; ============================================================
;;; Store Statistics
;;; ============================================================

(display "\nStore Statistics:\n")

(let ([stats (duckie-store-stats)])
     (test "stats has total-blocks" #t (pair? (assq 'total-blocks stats)))
     (test "stats has pinned" #t (pair? (assq 'pinned stats)))
     (test "stats duckie-active" 1 (cdr (assq 'duckie-active stats)))
     (test "stats duckie-memories" 2 (cdr (assq 'duckie-memories stats))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "==================\n")
(display (string-append "Passed: " (number->string tests-passed) "\n"))
(display (string-append "Failed: " (number->string tests-failed) "\n"))

(if (= tests-failed 0)
    (display "\n✓ All DUCKIE persistence tests passed!\n")
    (display "\n✗ Some tests failed!\n"))
