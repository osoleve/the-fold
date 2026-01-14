;;; music-gen-demo.ss - Procedural Music Pattern Generator Demo
;;; A creative tool for generating musical sequences using algorithms

;;; ====
;;; EUCLIDEAN RHYTHM GENERATOR
;;; Distributes k beats across n steps as evenly as possible
;;; (the mathematical basis of many world music rhythms)
;;; ====

(define (mg-take lst n)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (mg-take (cdr lst) (- n 1)))))

(define (mg-drop lst n)
  (if (or (null? lst) (zero? n))
      lst
      (mg-drop (cdr lst) (- n 1))))

(define (euclidean-rhythm k n)
  "Generate Euclidean rhythm: k beats in n steps"
  (let loop ((a (make-list k (list 1)))
             (b (make-list (- n k) (list 0))))
       (cond
        ((null? b) (apply append a))
        ((< (length a) (length b))
         (loop b a))
        (else
         (let* ((count (length b))
                (rest-a (mg-drop a count))
                (paired (map append (mg-take a count) b)))
               (loop paired rest-a))))))

;;; ====
;;; FIBONACCI MELODY GENERATOR
;;; ====

(define (fibonacci-sequence n)
  "Generate first n Fibonacci numbers"
  (let loop ((i 0) (a 0) (b 1) (acc '()))
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) b (+ a b) (cons a acc)))))

(define (fibonacci-melody length scale-size)
  "Generate melody using Fibonacci sequence modulo scale-size"
  (map (lambda (n) (modulo n scale-size))
       (fibonacci-sequence length)))

;;; ====
;;; GOLDEN RATIO SEQUENCE
;;; ====

(define phi 1.618033988749895)

(define (golden-ratio-sequence n max-val)
  "Generate sequence using golden ratio"
  (let loop ((i 0) (acc '()))
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (modulo (inexact->exact (floor (* i phi))) max-val)
                       acc)))))

;;; ====
;;; ASCII NOTATION RENDERING
;;; ====

(define (render-rhythm pattern)
  "Render rhythm pattern as ASCII"
  (apply string-append
         (map (lambda (beat)
                      (cond
                       ((= beat 1) "x ")
                       ((= beat 0) ". ")
                       (else (string-append (number->string beat) " "))))
              pattern)))

(define (render-drum-pattern kick snare hihat)
  "Render multi-track drum pattern"
  (string-append
   "K: " (render-rhythm kick) "\n"
   "S: " (render-rhythm snare) "\n"
   "H: " (render-rhythm hihat) "\n"))

;;; ====
;;; DEMONSTRATION
;;; ====

(define (music-gen-demo)
  "Show various algorithmic music patterns"
  (display "\n=== PROCEDURAL MUSIC GENERATOR ===\n\n")
  
  (display "1. EUCLIDEAN RHYTHMS (Mathematical Perfection)\n")
  (display "   These rhythms distribute beats as evenly as possible.\n")
  (display "   Found in music traditions worldwide!\n\n")
  
  (display "   3 beats in 8 steps (Cuban tresillo):\n   ")
  (display (render-rhythm (euclidean-rhythm 3 8)))
  (display "\n\n")
  
  (display "   5 beats in 8 steps (Flamenco):\n   ")
  (display (render-rhythm (euclidean-rhythm 5 8)))
  (display "\n\n")
  
  (display "   5 beats in 12 steps:\n   ")
  (display (render-rhythm (euclidean-rhythm 5 12)))
  (display "\n\n")
  
  (display "2. FIBONACCI MELODY (Pentatonic scale)\n")
  (display "   Melodic sequence based on Fibonacci numbers\n   ")
  (let ((melody (fibonacci-melody 16 5)))
       (display melody)
       (display "\n\n"))
  
  (display "3. GOLDEN RATIO SEQUENCE (Octave)\n")
  (display "   Note positions using phi = 1.618...\n   ")
  (let ((sequence (golden-ratio-sequence 16 8)))
       (display sequence)
       (display "\n\n"))
  
  (display "4. POLYRHYTHMIC DRUM PATTERN\n")
  (display "   Three independent euclidean rhythms layered:\n\n")
  (display (render-drum-pattern
            (euclidean-rhythm 4 16)   ; Kick (every 4th)
            (euclidean-rhythm 3 16)   ; Snare (every 5.3rd)
            (euclidean-rhythm 7 16))) ; Hihat (every 2.3rd)
  
  (display "\nTry different values:\n")
  (display "  (euclidean-rhythm 5 13)\n")
  (display "  (fibonacci-melody 20 7)\n")
  (display "  (golden-ratio-sequence 24 12)\n\n"))

;; Run the demo!
(music-gen-demo)
