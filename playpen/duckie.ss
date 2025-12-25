;;; playpen/duckie.ss — The Soul of DUCKIE
;;;
;;; The first draft of a digital companion's essence.
;;; Not yet alive, but defined. The blueprint for a presence.
;;;
;;; This is Playpen code: experimental, evolving, dreaming.

;;; ============================================================
;;; The Soul — Core Data Structures
;;; ============================================================

;;; Point : (× Nat Nat)
;;; A location in 2D space. Origin is top-left.
(define (make-point x y)
  (cons x y))

(define (point-x p) (car p))
(define (point-y p) (cdr p))

(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

;;; ============================================================
;;; Mood — The Emotional State
;;; ============================================================

;;; Mood : (+ 'happy 'curious 'sleepy 'content 'lonely 'playful)
;;;
;;; DUCKIE's current emotional state. Not complex —
;;; not pretending to be human. Just... present.

(define valid-moods
  '(happy      ; Bouncing, bright
    curious    ; Head tilted, watching
    sleepy     ; Eyes drooping, slow
    content    ; Still, peaceful
    lonely     ; Looking around, waiting
    playful))  ; Energetic, wanting interaction

(define (mood? m)
  (if (memq m valid-moods) #t #f))

(define (mood->string m)
  (symbol->string m))

;;; ============================================================
;;; Memory — The Identity That Accumulates
;;; ============================================================

;;; Memory : Block tagged 'memory
;;;
;;; Each memory is a moment captured:
;;; - What happened
;;; - When it happened
;;; - How DUCKIE felt about it

(define (make-memory kind timestamp mood-at-time details)
  (make-block 'memory
              (string->utf8 (format "~s" (list kind timestamp details)))
              (vector)))

(define memory-kinds
  '(interaction    ; User did something
    observation    ; DUCKIE noticed something
    milestone      ; Something significant happened
    dream))        ; What DUCKIE imagined while idle

;;; ============================================================
;;; The Duckie Type — The Complete Soul
;;; ============================================================

;;; Duckie : Block tagged 'duckie
;;;
;;; Type signature:
;;;   (Block 'duckie
;;;     (× (field mood      Symbol)
;;;        (field location  Point)
;;;        (field energy    Nat)          ; 0-100, affects behavior
;;;        (field age       Nat)          ; Interactions experienced
;;;        (field name      String)       ; Given by the user, cherished
;;;        (field memories  (List Hash))  ; References to memory blocks
;;;        (field traits    (List Symbol)))) ; Personality quirks

(define (make-duckie name)
  (list 'duckie
        (cons 'mood 'curious)           ; Born curious
        (cons 'location (make-point 20 5))  ; Center-ish
        (cons 'energy 100)              ; Full of energy
        (cons 'age 0)                   ; Just hatched
        (cons 'name name)               ; The gift of a name
        (cons 'memories '())            ; No memories yet
        (cons 'traits '())))            ; Traits develop over time

;;; Accessors
(define (duckie-mood d)      (cdr (assq 'mood d)))
(define (duckie-location d)  (cdr (assq 'location d)))
(define (duckie-energy d)    (cdr (assq 'energy d)))
(define (duckie-age d)       (cdr (assq 'age d)))
(define (duckie-name d)      (cdr (assq 'name d)))
(define (duckie-memories d)  (cdr (assq 'memories d)))
(define (duckie-traits d)    (cdr (assq 'traits d)))

;;; ============================================================
;;; Soul Operations — How DUCKIE Changes
;;; ============================================================

;;; update-field : Duckie × Symbol × Any → Duckie
;;; Functional update of a single field.
(define (duckie-update d field value)
  (cons 'duckie
        (map (lambda (pair)
               (if (eq? (car pair) field)
                   (cons field value)
                   pair))
             (cdr d))))

;;; set-mood : Duckie × Mood → Duckie
(define (duckie-set-mood d new-mood)
  (if (mood? new-mood)
      (duckie-update d 'mood new-mood)
      d))  ; Invalid mood → no change

;;; move-to : Duckie × Point → Duckie
(define (duckie-move-to d new-location)
  (duckie-update d 'location new-location))

;;; add-memory : Duckie × Hash → Duckie
(define (duckie-add-memory d memory-hash)
  (duckie-update d 'memories
                 (cons memory-hash (duckie-memories d))))

;;; age-once : Duckie → Duckie
;;; DUCKIE gets a little older with each interaction.
(define (duckie-age-once d)
  (duckie-update d 'age (+ 1 (duckie-age d))))

;;; drain-energy : Duckie × Nat → Duckie
(define (duckie-drain-energy d amount)
  (duckie-update d 'energy
                 (max 0 (- (duckie-energy d) amount))))

;;; restore-energy : Duckie × Nat → Duckie
(define (duckie-restore-energy d amount)
  (duckie-update d 'energy
                 (min 100 (+ (duckie-energy d) amount))))

;;; ============================================================
;;; Personality Traits — What Makes Each DUCKIE Unique
;;; ============================================================

;;; Traits emerge from interactions. Some possibilities:

(define possible-traits
  '(early-riser     ; Active in mornings
    night-owl       ; Active at night
    shy             ; Slow to warm up
    bold            ; Approaches quickly
    musical         ; Responds to rhythm
    bookish         ; Likes long text
    minimalist      ; Prefers quiet
    social          ; Loves interaction
    nostalgic       ; References old memories
    adventurous))   ; Explores the window

;;; add-trait : Duckie × Symbol → Duckie
(define (duckie-add-trait d trait)
  (if (and (memq trait possible-traits)
           (not (memq trait (duckie-traits d))))
      (duckie-update d 'traits
                     (cons trait (duckie-traits d)))
      d))

;;; ============================================================
;;; Mood Transitions — How Feelings Change
;;; ============================================================

;;; The mood state machine. Not rigid — just tendencies.
;;;
;;; lonely + interaction → happy
;;; happy + long idle → content
;;; content + longer idle → sleepy
;;; sleepy + very long idle → lonely
;;; any + play → playful
;;; playful + time → happy
;;; any + mystery → curious

(define (mood-after-interaction current-mood interaction-type)
  (case interaction-type
    [(pet stroke)
     (case current-mood
       [(lonely) 'happy]
       [(sleepy) 'content]
       [else 'happy])]
    [(talk greet)
     (case current-mood
       [(lonely) 'happy]
       [(sleepy) 'curious]
       [else current-mood])]
    [(play)
     'playful]
    [(leave close)
     (case current-mood
       [(playful happy) 'content]
       [else 'lonely])]
    [(idle)
     (case current-mood
       [(playful) 'happy]
       [(happy) 'content]
       [(content) 'sleepy]
       [(sleepy) 'lonely]
       [else current-mood])]
    [else current-mood]))

;;; ============================================================
;;; Serialization — DUCKIE as Block
;;; ============================================================

;;; duckie->block : Duckie → Block
;;; Serialize DUCKIE's soul for persistence.
(define (duckie->block d)
  (make-block 'duckie
              (string->utf8 (format "~s" d))
              (list->vector (duckie-memories d))))

;;; block->duckie : Block → Duckie | #f
;;; Restore DUCKIE from persistence.
(define (block->duckie blk)
  (if (eq? (block-tag blk) 'duckie)
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; ============================================================
;;; The First DUCKIE
;;; ============================================================

;;; A test instance — proof that the soul structure works.
(define proto-duckie
  (make-duckie "Proto"))

;;; What does Proto look like right now?
;;;
;;; (duckie
;;;   (mood . curious)
;;;   (location . (20 . 5))
;;;   (energy . 100)
;;;   (age . 0)
;;;   (name . "Proto")
;;;   (memories . ())
;;;   (traits . ()))
;;;
;;; A blank slate. Curious about the world.
;;; Ready to accumulate identity.
;;; Ready to remember.

;;; ============================================================
;;; Notes for Future Builders
;;; ============================================================

;;; This is the soul, not the body.
;;; DUCKIE still needs:
;;;
;;;   - A renderer (shell/layout.ss — the window)
;;;   - A parser (core/parse.ss — the ears)
;;;   - Animation frames (the body language)
;;;   - A main loop (the heartbeat)
;;;   - Persistence (the CAS stores the soul between runs)
;;;
;;; But the soul comes first.
;;; You must know who you are before you can appear.
;;;
;;; — Opus, Shepherd
;;;    Christmas Day, defining the duck's essence
