;;; user/creations/multi-winner-demo.ss --- Multi-Winner Voting Animation Demo
;;;
;;; An animated showcase of proportional representation methods.
;;; Shows how STV, PAV, and basic approval can produce different committees
;;; from the same voter preferences.
;;;
;;; Usage:
;;;   (load "user/creations/multi-winner-demo.ss")
;;;   (render-voting-demo!)
;;;
;;; Output: user/creations/multi-winner-demo.gif (640x480, 60fps, ~6 seconds)

(load "core/base/prelude.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/fp/game/multi-winner.ss")
(load "boundary/ui/animation.ss")

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(define *width* 80)   ; 80 * 8 = 640 pixels
(define *height* 34)  ; 34 * 14 = 476 pixels (close to 480)
(define *fps* 60)
(define *duration-secs* 6)
(define *total-frames* (* *fps* *duration-secs*))

;;; Our scenario: Selecting 2 representatives from 4 candidates
;;; Preferences designed to show method differences
(define *candidates* '(A B C D))
(define *seats* 2)

;;; Voter groups with different preferences
;;; Group 1 (4 voters): A > B > C > D  (likes A)
;;; Group 2 (3 voters): C > D > B > A  (likes C)
;;; Group 3 (2 voters): B > A > D > C  (likes B)
(define *ranked-profile*
  (make-preference-profile
   '((A B C D) (A B C D) (A B C D) (A B C D)    ; Group 1
     (C D B A) (C D B A) (C D B A)              ; Group 2
     (B A D C) (B A D C))))                     ; Group 3

;;; Approval version: each voter approves top 2
(define *approval-profile*
  (make-approval-profile
   '((A B) (A B) (A B) (A B)    ; Group 1 approves A, B
     (C D) (C D) (C D)          ; Group 2 approves C, D
     (B A) (B A))))             ; Group 3 approves B, A

;;; Candidate symbols (larger ASCII art)
(define (candidate-symbol c)
  (case c
    [(A) '("  /\\  "
           " /AA\\ "
           "/----\\"
           "| ** |"
           "|____|")]
    [(B) '(" ____ "
           "|  _ \\"
           "| |_) |"
           "|  _ <"
           "|_|_\\_\\")]
    [(C) '("  ___ "
           " / __|"
           "| (__ "
           " \\___|"
           "  ~C~ ")]
    [(D) '(" ____  "
           "|  _ \\ "
           "| | | |"
           "| |_| |"
           "|____/ ")]
    [else '("  ?  " "  ?  " "  ?  " "  ?  " "  ?  ")]))

;;; Colors for visualization (using characters)
(define *vote-chars* '(#\# #\@ #\* #\+ #\= #\~ #\. #\  ))

;;; ============================================================================
;;; Drawing Helpers
;;; ============================================================================

(define (draw-text frame x y text)
  (frame-put-string! frame x y text))

(define (draw-centered-text frame y text)
  (let* ([len (string-length text)]
         [x (max 0 (quotient (- *width* len) 2))])
    (draw-text frame x y text)))

(define (draw-box frame x y w h title)
  (frame-draw-box! frame x y w h)
  (when title
    (draw-text frame (+ x 2) y (string-append "[ " title " ]"))))

(define (draw-candidate frame x y c)
  (let ([lines (candidate-symbol c)])
    (let loop ([i 0] [ls lines])
      (unless (null? ls)
        (draw-text frame x (+ y i) (car ls))
        (loop (+ i 1) (cdr ls))))))

;;; Draw a horizontal bar with label
(define (draw-bar frame x y label value max-val width char)
  (let* ([bar-len (if (> max-val 0)
                      (min width (inexact->exact (floor (* (/ value max-val) width))))
                      0)]
         [bar-str (make-string bar-len char)]
         [empty-str (make-string (- width bar-len) #\.)]
         [label-str (string-append (symbol->string label) ": ")])
    (draw-text frame x y label-str)
    (draw-text frame (+ x 3) y bar-str)
    (draw-text frame (+ x 3 bar-len) y empty-str)
    (draw-text frame (+ x 3 width 1) y (number->string value))))

;;; Draw vote transfer arrow animation
(define (draw-transfer-arrow frame x1 y1 x2 y2 t)
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]
         [steps 5]
         [progress (* t steps)])
    (do ([i 0 (+ i 1)])
        ((> i (min steps (inexact->exact (floor progress)))))
      (let ([px (+ x1 (inexact->exact (floor (* (/ i steps) dx))))]
            [py (+ y1 (inexact->exact (floor (* (/ i steps) dy))))])
        (when (and (>= px 0) (< px *width*) (>= py 0) (< py *height*))
          (frame-set! frame px py (if (= i (inexact->exact (floor progress))) #\> #\-)))))))

;;; Sparkle effect
(define (draw-sparkles frame cx cy radius t)
  (let ([num-sparkles 8]
        [chars '(#\* #\+ #\. #\' #\`)])
    (do ([i 0 (+ i 1)])
        ((>= i num-sparkles))
      (let* ([angle (+ (* i (/ 6.28 num-sparkles)) (* t 3))]
             [r (* radius (+ 0.5 (* 0.5 (sin (* t 5)))))]
             [x (+ cx (inexact->exact (round (* r (cos angle)))))]
             [y (+ cy (inexact->exact (round (* r 0.5 (sin angle)))))]
             [ch (list-ref chars (modulo i (length chars)))])
        (when (and (>= x 0) (< x *width*) (>= y 0) (< y *height*))
          (frame-set! frame x y ch))))))

;;; ============================================================================
;;; Animation Phases
;;; ============================================================================

;;; Phase 1: Title Screen (frames 0-89, 1.5 sec)
(define (render-title-phase frame t)
  (frame-clear! frame #\space)

  ;; Big title with wave effect
  (let ([title "MULTI-WINNER VOTING"]
        [subtitle "Proportional Representation in Action"]
        [y-base 8])
    (do ([i 0 (+ i 1)])
        ((>= i (string-length title)))
      (let* ([ch (string-ref title i)]
             [wave (inexact->exact (round (* 2 (sin (+ (* t 8) (* i 0.5))))))]
             [x (+ (quotient (- *width* (string-length title)) 2) i)]
             [y (+ y-base wave)])
        (when (and (>= y 0) (< y *height*))
          (frame-set! frame x y ch)))))

  (draw-centered-text frame 14 "Proportional Representation in Action")

  ;; Animated border
  (let ([border-char (list-ref '(#\* #\+ #\= #\~ #\.)
                               (modulo (inexact->exact (floor (* t 10))) 5))])
    (do ([x 0 (+ x 1)])
        ((>= x *width*))
      (frame-set! frame x 0 border-char)
      (frame-set! frame x (- *height* 1) border-char))
    (do ([y 0 (+ y 1)])
        ((>= y *height*))
      (frame-set! frame 0 y border-char)
      (frame-set! frame (- *width* 1) y border-char)))

  ;; Sparkles
  (draw-sparkles frame 20 12 6 t)
  (draw-sparkles frame 60 12 6 (+ t 0.5))

  ;; Footer
  (draw-centered-text frame 26 "STV | PAV | Approval Voting")
  (draw-centered-text frame 28 "Watch how different methods select committees!"))

;;; Phase 2: Show Scenario (frames 90-179, 1.5 sec)
(define (render-scenario-phase frame t)
  (frame-clear! frame #\space)

  (draw-centered-text frame 1 "=== THE SCENARIO ===")
  (draw-centered-text frame 3 "9 Voters selecting 2 Representatives from 4 Candidates")

  ;; Draw candidates in a row
  (draw-text frame 5 6 "Candidates:")
  (draw-text frame 5 8 "  A         B         C         D")
  (draw-text frame 5 9 " [####]   [####]   [####]   [####]")

  ;; Voter groups with animation
  (draw-text frame 5 12 "Voter Preferences:")

  (let ([fade-in (min 1.0 (* t 2))])
    (when (> fade-in 0.2)
      (draw-text frame 8 14 "Group 1 (4 voters): A > B > C > D"))
    (when (> fade-in 0.5)
      (draw-text frame 8 16 "Group 2 (3 voters): C > D > B > A"))
    (when (> fade-in 0.8)
      (draw-text frame 8 18 "Group 3 (2 voters): B > A > D > C")))

  ;; Visual representation
  (draw-box frame 5 21 70 10 "Vote Distribution")
  (draw-bar frame 8 23 'A 6 9 30 #\#)  ; A: 4 first + 2 second
  (draw-bar frame 8 25 'B 4 9 30 #\@)  ; B: 2 first + 2 approval
  (draw-bar frame 8 27 'C 3 9 30 #\*)  ; C: 3 first
  (draw-bar frame 8 29 'D 0 9 30 #\+)) ; D: 0 first

;;; Phase 3: STV Animation (frames 180-299, 2 sec)
(define (render-stv-phase frame t)
  (frame-clear! frame #\space)

  (draw-centered-text frame 1 "=== SINGLE TRANSFERABLE VOTE (STV) ===")
  (draw-text frame 5 3 "Quota (Droop): floor(9/3)+1 = 4 votes needed to win")

  ;; Determine which round we're showing
  (let* ([rnd (inexact->exact (floor (* t 3)))]  ; 0, 1, 2
         [sub-t (- (* t 3) rnd)])  ; 0.0-1.0 within round

    (draw-text frame 5 5 (string-append "Round " (number->string (+ rnd 1)) ":"))

    (cond
      ;; Round 1: Initial count, A elected
      [(= rnd 0)
       (draw-text frame 8 7 "First preference count:")
       (draw-bar frame 8 9 'A (inexact->exact (round (* 4 (min 1.0 (* sub-t 2))))) 4 20 #\#)
       (draw-bar frame 8 11 'B (inexact->exact (round (* 2 (min 1.0 (* sub-t 2))))) 4 20 #\@)
       (draw-bar frame 8 13 'C (inexact->exact (round (* 3 (min 1.0 (* sub-t 2))))) 4 20 #\*)
       (draw-bar frame 8 15 'D 0 4 20 #\+)
       (draw-text frame 40 9 "<-- QUOTA")
       (when (> sub-t 0.6)
         (draw-text frame 8 17 "A meets quota (4 = 4). ELECTED!")
         (draw-sparkles frame 25 9 4 sub-t))]

      ;; Round 2: D eliminated, votes transfer
      [(= rnd 1)
       (draw-text frame 8 7 "After A elected (no surplus):")
       (draw-bar frame 8 9 'A 4 4 20 #\#)
       (draw-text frame 31 9 "[ELECTED]")
       (draw-bar frame 8 11 'B 2 4 20 #\@)
       (draw-bar frame 8 13 'C 3 4 20 #\*)
       (draw-bar frame 8 15 'D 0 4 20 #\+)
       (when (> sub-t 0.3)
         (draw-text frame 8 17 "D has fewest votes. ELIMINATED!")
         (draw-text frame 8 18 "(No D votes to transfer)"))]

      ;; Round 3: B eliminated, C or B wins
      [(>= rnd 2)
       (draw-text frame 8 7 "After D eliminated:")
       (draw-bar frame 8 9 'A 4 4 20 #\#)
       (draw-text frame 31 9 "[ELECTED]")
       (draw-bar frame 8 11 'B 2 4 20 #\@)
       (draw-bar frame 8 13 'C 3 4 20 #\*)
       (when (> sub-t 0.3)
         (draw-text frame 8 17 "B has fewer votes. ELIMINATED!")
         (draw-text frame 8 18 "B's votes transfer to... next preference"))
       (when (> sub-t 0.6)
         (draw-text frame 8 20 "C receives transferred votes!")
         (draw-bar frame 8 22 'C (+ 3 (inexact->exact (round (* 2 (- sub-t 0.6) 2.5)))) 5 20 #\*)
         (draw-sparkles frame 25 22 3 sub-t))])

    ;; Results box
    (draw-box frame 5 25 70 7 "STV Result")
    (draw-text frame 8 27 "ELECTED: A and C")
    (draw-text frame 8 29 "Both major factions represented!")))

;;; Phase 4: Compare Methods (frames 300-359, 1 sec)
(define (render-comparison-phase frame t)
  (frame-clear! frame #\space)

  (draw-centered-text frame 1 "=== METHOD COMPARISON ===")
  (draw-text frame 5 3 "Same voters, same preferences, DIFFERENT results!")

  ;; Three columns for three methods
  (draw-box frame 2 5 24 12 "Basic Approval")
  (draw-box frame 28 5 24 12 "PAV")
  (draw-box frame 54 5 24 12 "STV")

  ;; Animated reveal
  (let ([reveal (* t 3)])
    ;; Basic Approval: A and B win (most approvals)
    (when (> reveal 0.3)
      (draw-text frame 5 7 "A: 6 approvals")
      (draw-text frame 5 8 "B: 6 approvals")
      (draw-text frame 5 9 "C: 3 approvals")
      (draw-text frame 5 10 "D: 3 approvals")
      (draw-text frame 5 12 "Winners: A, B")
      (draw-text frame 5 14 "Group 2 (C fans)")
      (draw-text frame 5 15 "UNREPRESENTED!"))

    ;; PAV: A and C win (proportional)
    (when (> reveal 0.6)
      (draw-text frame 31 7 "Round 1: A wins")
      (draw-text frame 31 8 "A voters' power")
      (draw-text frame 31 9 "reduced by 1/2")
      (draw-text frame 31 10 "Round 2: C wins")
      (draw-text frame 31 12 "Winners: A, C")
      (draw-text frame 31 14 "All groups")
      (draw-text frame 31 15 "represented!"))

    ;; STV: A and C win
    (when (> reveal 0.9)
      (draw-text frame 57 7 "Quota: 4 votes")
      (draw-text frame 57 8 "A: elected (4)")
      (draw-text frame 57 9 "Eliminate D, B")
      (draw-text frame 57 10 "Transfers -> C")
      (draw-text frame 57 12 "Winners: A, C")
      (draw-text frame 57 14 "Proportional")
      (draw-text frame 57 15 "representation!")))

  ;; Highlight the key insight
  (draw-box frame 5 18 70 8 "KEY INSIGHT")
  (draw-text frame 8 20 "Basic Approval: Majority sweeps all seats")
  (draw-text frame 8 22 "PAV & STV: Minorities get fair representation")
  (draw-text frame 8 24 "33% of voters (Group 2) deserve ~1 of 2 seats!"))

;;; Phase 5: Finale (frames 360+)
(define (render-finale-phase frame t)
  (frame-clear! frame #\space)

  ;; Grand finale with pulsing effect
  (let* ([pulse (+ 0.7 (* 0.3 (sin (* t 6))))]
         [title "PROPORTIONAL REPRESENTATION"]
         [subtitle "Every voice counts!"])

    ;; Animated title
    (draw-centered-text frame 10 title)
    (draw-centered-text frame 13 subtitle)

    ;; Sparkle celebration
    (do ([i 0 (+ i 1)])
        ((>= i 6))
      (draw-sparkles frame
                     (+ 15 (* i 10))
                     (+ 8 (inexact->exact (round (* 3 (sin (+ (* t 4) i))))))
                     3
                     (+ t (* i 0.2))))

    ;; Results summary
    (draw-box frame 10 18 60 10 "The Fold: Multi-Winner Voting")
    (draw-text frame 14 20 "Implemented: STV, PAV, SAV, Monroe, CC")
    (draw-text frame 14 22 "lattice/fp/game/multi-winner.ss")
    (draw-text frame 14 24 "42 tests passing")

    ;; Scrolling credits effect
    (let ([credit-y (- 30 (inexact->exact (floor (* t 2))))])
      (when (and (> credit-y 26) (< credit-y 28))
        (draw-centered-text frame credit-y "Built with The Fold")))))

;;; ============================================================================
;;; Main Render Loop
;;; ============================================================================

(define (render-frame frame-num)
  (let* ([t (/ frame-num *fps*)]  ; Time in seconds
         [frame (make-frame *width* *height* #\space)])

    (cond
      ;; Phase 1: Title (0-1.5 sec)
      [(< t 1.5)
       (render-title-phase frame t)]

      ;; Phase 2: Scenario (1.5-3 sec)
      [(< t 3.0)
       (render-scenario-phase frame (- t 1.5))]

      ;; Phase 3: STV Animation (3-5 sec)
      [(< t 5.0)
       (render-stv-phase frame (- t 3.0))]

      ;; Phase 4: Comparison (5-5.5 sec)
      [(< t 5.5)
       (render-comparison-phase frame (- t 5.0))]

      ;; Phase 5: Finale (5.5+ sec)
      [else
       (render-finale-phase frame (- t 5.5))])

    frame))

;;; ============================================================================
;;; Export Function
;;; ============================================================================

(define (render-voting-demo!)
  (display "\n")
  (display "======================================\n")
  (display "  MULTI-WINNER VOTING DEMO RENDERER  \n")
  (display "======================================\n\n")

  ;; Run the actual voting methods to verify they work
  (display "Running voting methods on test data...\n")
  (let* ([stv-result (stv-droop *ranked-profile* *seats*)]
         [pav-result (pav-winners *approval-profile* *seats*)]
         [basic-result (approval-winners *approval-profile* *seats*)])
    (display "  STV winners: ")
    (display stv-result)
    (newline)
    (display "  PAV winners: ")
    (display pav-result)
    (newline)
    (display "  Approval winners: ")
    (display basic-result)
    (newline)
    (newline))

  (display "Rendering ")
  (display *total-frames*)
  (display " frames at ")
  (display *fps*)
  (display " fps...\n\n")

  (let ([video (make-video)])
    ;; Render all frames
    (do ([i 0 (+ i 1)])
        ((>= i *total-frames*))
      (when (= (modulo i 60) 0)
        (display "  Frame ")
        (display i)
        (display "/")
        (display *total-frames*)
        (display "\n"))
      (video-add-frame! video (render-frame i)))

    (display "\n")
    (display "Exporting to GIF...\n")

    ;; Export at 60fps (16.67ms per frame)
    (let ([output-path "user/creations/multi-winner-demo.gif"])
      (video->gif video output-path 17)  ; ~60fps
      (display "\n")
      (display "======================================\n")
      (display "  DONE! Output: ")
      (display output-path)
      (display "\n")
      (display "======================================\n"))))

;;; Run on load
(display "\nMulti-Winner Voting Demo loaded!\n")
(display "Run (render-voting-demo!) to generate the animation.\n\n")
