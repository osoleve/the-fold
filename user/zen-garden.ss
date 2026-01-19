;;; playpen/zen-garden.ss — Digital Zen Garden
;;;
;;; Generate peaceful ASCII gardens for contemplation.
;;; Each garden is unique, created from a seed.
;;;
;;; This is Playpen code: creative meditation through procedural generation.

(load "boundary/ui/layout.ss")

;;; ====
;;; Random Utilities
;;; ====

;;; seed-random : Nat → void
;;; Seed the random number generator for reproducible gardens.
(define (seed-random seed)
  (random-seed seed))

;;; random-element : List → α
;;; Pick a random element from a list.
(define (random-element lst)
  (list-ref lst (random (length lst))))

;;; ====
;;; Garden Elements
;;; ====

(define *rocks* '(#\o #\O #\0 #\● #\◆))
(define *plants* '(#\i #\| #\! #\† #\‡))
(define *trees* '(#\♣ #\♠ #\⚘ #\❀))
(define *water* '(#\~ #\≈ #\∼))
(define *rake* '(#\- #\= #\≡))

;;; ====
;;; Perlin-like Noise (Simple)
;;; ====

;;; noise : Float × Float × Nat → Float
;;; Simple 2D noise function (pseudo-random but smooth-ish).
(define (noise x y seed)
  (let* ([n (+ (* x 57.0) (* y 131.0) seed)]
         [n2 (* n n)]
         [result (sin (* n2 0.001))])
        result))

;;; ====
;;; Garden Patterns
;;; ====

;;; rake-pattern : Canvas × Int × Int × Int → void
;;; Draw raked sand patterns (horizontal waves).
(define (rake-pattern canvas x1 y1 len)
  (let ([y y1]
        [rake-char (random-element *rake*)])
       (let loop ([x x1])
            (when (and (< x (+ x1 len))
                       (< x (canvas-width canvas))
                       (< y (canvas-height canvas))
                       (>= x 0) (>= y 0))
                  (canvas-set! canvas x y rake-char)
                  (loop (+ x 1))))))

;;; place-rock : Canvas × Int × Int → void
;;; Place a rock or rock cluster.
(define (place-rock canvas x y)
  (let ([rock (random-element *rocks*)])
       (when (and (< x (canvas-width canvas))
                  (< y (canvas-height canvas))
                  (>= x 0) (>= y 0))
             (canvas-set! canvas x y rock)
             ;; Sometimes add adjacent rocks
             (when (< (random 10) 3)
                   (let ([dx (- (random 3) 1)]
                         [dy (- (random 3) 1)])
                        (when (and (< (+ x dx) (canvas-width canvas))
                                   (< (+ y dy) (canvas-height canvas))
                                   (>= (+ x dx) 0) (>= (+ y dy) 0))
                              (canvas-set! canvas (+ x dx) (+ y dy) rock)))))))

;;; place-plant : Canvas × Int × Int → void
;;; Place a plant or grass.
(define (place-plant canvas x y)
  (let ([plant (random-element *plants*)])
       (when (and (< x (canvas-width canvas))
                  (< y (canvas-height canvas))
                  (>= x 0) (>= y 0))
             (canvas-set! canvas x y plant))))

;;; place-tree : Canvas × Int × Int → void
;;; Place a decorative tree.
(define (place-tree canvas x y)
  (let ([tree (random-element *trees*)])
       (when (and (< x (canvas-width canvas))
                  (< y (canvas-height canvas))
                  (>= x 0) (>= y 0))
             (canvas-set! canvas x y tree))))

;;; water-pool : Canvas × Int × Int × Int → void
;;; Draw a small water feature.
(define (water-pool canvas cx cy radius)
  (let ([water-char (random-element *water*)])
       (let y-loop ([dy (- radius)])
            (when (<= dy radius)
                  (let x-loop ([dx (- radius)])
                       (when (<= dx radius)
                             (let* ([x (+ cx dx)]
                                    [y (+ cy dy)]
                                    [dist (sqrt (+ (* dx dx) (* dy dy)))])
                                   (when (and (< dist radius)
                                              (>= x 0) (>= y 0)
                                              (< x (canvas-width canvas))
                                              (< y (canvas-height canvas)))
                                         (canvas-set! canvas x y water-char)))
                             (x-loop (+ dx 1))))
                  (y-loop (+ dy 1))))))

;;; ====
;;; Garden Generation
;;; ====

;;; generate-garden : Nat × Nat × Nat → Canvas
;;; Generate a zen garden from a seed.
(define (generate-garden width height seed)
  (seed-random seed)
  (let ([canvas (make-canvas width height)])
       
       ;; Fill with base sand pattern
       (let y-loop ([y 0])
            (when (< y height)
                  (rake-pattern canvas 0 y width)
                  (y-loop (+ y 2))))
       
       ;; Add some curved rakes for variety
       (let loop ([i 0])
            (when (< i 5)
                  (let ([y (random height)]
                        [len (+ 10 (random 30))])
                       (rake-pattern canvas 0 y len))
                  (loop (+ i 1))))
       
       ;; Place rocks (5-15)
       (let loop ([i 0])
            (when (< i (+ 5 (random 10)))
                  (place-rock canvas (random width) (random height))
                  (loop (+ i 1))))
       
       ;; Place plants (10-20)
       (let loop ([i 0])
            (when (< i (+ 10 (random 10)))
                  (place-plant canvas (random width) (random height))
                  (loop (+ i 1))))
       
       ;; Place trees (2-5)
       (let loop ([i 0])
            (when (< i (+ 2 (random 3)))
                  (place-tree canvas (random width) (random height))
                  (loop (+ i 1))))
       
       ;; Maybe add a water feature
       (when (< (random 10) 5)
             (water-pool canvas
                         (+ 10 (random (- width 20)))
                         (+ 5 (random (- height 10)))
                         (+ 2 (random 4))))
       
       canvas))

;;; ====
;;; Haiku Integration
;;; ====

(define *zen-haiku*
  '(("Rocks rest in silence" "Raked sand patterns flow" "Peace in each line drawn")
    ("Digital garden" "Bits arrange as stone and plant" "Beauty from chaos")
    ("Random seed planted" "Algorithms grow beauty" "Code becomes garden")
    ("Terminal blooms bright" "ASCII flowers never fade" "Electric serenity")
    ("Electrons dancing" "Form patterns of rock and wave" "Zen in the machine")))

;;; random-haiku : → (List String String String)
(define (random-haiku)
  (random-element *zen-haiku*))

;;; display-haiku : (List String String String) → void
(define (display-haiku haiku)
  (display "   ")
  (display (car haiku))
  (newline)
  (display "   ")
  (display (cadr haiku))
  (newline)
  (display "   ")
  (display (caddr haiku))
  (newline))

;;; ====
;;; Display Functions
;;; ====

;;; garden : [Nat] → void
;;; Display a zen garden. If no seed provided, use timestamp.
(define garden
  (case-lambda
   [() (garden (modulo (current-second) 10000))]
   [(seed)
    (let* ([w 70]
           [h 20]
           [canvas (generate-garden w h seed)]
           [haiku (random-haiku)])
          (newline)
          (display "╔══════════════════════════════════════════════════════════════════════╗\n")
          (display "║                         DIGITAL ZEN GARDEN                           ║\n")
          (display (format "║                            Seed: ~a~a║\n"
                           seed
                           (make-string (max 0 (- 47 (string-length (number->string seed))))
                                        #\space)))
          (display "╚══════════════════════════════════════════════════════════════════════╝\n")
          (newline)
          (display (canvas->string canvas))
          (newline)
          (newline)
          (display-haiku haiku)
          (newline)
          (display "────────────────────────────────────────────────────────────────────────\n")
          (display "  Legend:  ○ Rocks    | Plants    ♣ Trees    ~ Water    - Raked sand\n")
          (display "────────────────────────────────────────────────────────────────────────\n")
          (newline)
          (display "  Try: (garden)       - Random garden\n")
          (display "       (garden 1234)  - Garden from seed\n")
          (newline))]))

;;; garden-series : Nat → void
;;; Display multiple gardens in sequence.
(define (garden-series n)
  (let loop ([i 0])
       (when (< i n)
             (garden (+ i 1000))
             (loop (+ i 1)))))

;;; ====
;;; Loaded
;;; ====

(display "\n")
(display "Digital Zen Garden loaded.\n")
(display "\n")
(display "  Commands:\n")
(display "    (garden)        - Generate a random zen garden\n")
(display "    (garden seed)   - Generate garden from specific seed\n")
(display "    (garden-series 5) - Display 5 gardens\n")
(display "\n")
(display "Find peace in procedural generation.\n")
(display "\n")
