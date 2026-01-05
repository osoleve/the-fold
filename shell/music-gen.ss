;;; music-gen.ss - Procedural Music Pattern Generator
;;; A creative tool for generating musical sequences using algorithms
;;; Builder tier - builds on shell infrastructure

(library (shell music-gen)
         (export
          ;; Pattern generators
          euclidean-rhythm
          fibonacci-melody
          cellular-rhythm
          golden-ratio-sequence
          
          ;; Rendering
          render-ascii-notation
          render-drum-pattern
          render-melody-staff
          
          ;; Interactive commands
          music-gen-demo
          random-rhythm
          random-melody
          export-pattern)
         
         (import (chezscheme))
         
         ;;; ============================================
         ;;; EUCLIDEAN RHYTHM GENERATOR
         ;;; Distributes k beats across n steps as evenly as possible
         ;;; (the mathematical basis of many world music rhythms)
         ;;; ============================================
         
         (define (euclidean-rhythm k n)
           "Generate Euclidean rhythm: k beats in n steps"
           (let loop ((a (make-list k '(1)))
                      (b (make-list (- n k) '(0))))
                (cond
                 ((null? b) (apply append a))
                 ((< (length a) (length b))
                  (loop b a))
                 (else
                  (let* ((count (length b))
                         (rest-a (drop a count))
                         (paired (map cons (take a count) b)))
                        (loop paired rest-a))))))
         
         (define (take lst n)
           (if (or (null? lst) (zero? n))
               '()
               (cons (car lst) (take (cdr lst) (- n 1)))))
         
         (define (drop lst n)
           (if (or (null? lst) (zero? n))
               lst
               (drop (cdr lst) (- n 1))))
         
         ;;; ============================================
         ;;; FIBONACCI MELODY GENERATOR
         ;;; Creates melodic patterns based on Fibonacci numbers
         ;;; ============================================
         
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
         
         ;;; ============================================
         ;;; CELLULAR AUTOMATON RHYTHM
         ;;; Uses Rule 30 or similar CA to generate rhythmic patterns
         ;;; ============================================
         
         (define (ca-step rule cells)
           "Apply cellular automaton rule to cells"
           (let ((n (length cells)))
                (map (lambda (i)
                             (let* ((left (list-ref cells (modulo (- i 1) n)))
                                    (center (list-ref cells i))
                                    (right (list-ref cells (modulo (+ i 1) n)))
                                    (pattern (+ (* left 4) (* center 2) right)))
                                   (if (zero? (bitwise-and rule (expt 2 pattern))) 0 1)))
                     (iota n))))
         
         (define (cellular-rhythm rule length steps)
           "Generate rhythm using cellular automaton"
           (let ((initial (map (lambda (i) (if (= i (quotient length 2)) 1 0))
                               (iota length))))
                (let loop ((gen 0) (cells initial) (acc '()))
                     (if (>= gen steps)
                         (reverse acc)
                         (loop (+ gen 1)
                               (ca-step rule cells)
                               (cons cells acc))))))
         
         ;;; ============================================
         ;;; GOLDEN RATIO SEQUENCE
         ;;; Generates note positions using golden ratio
         ;;; ============================================
         
         (define phi 1.618033988749895)
         
         (define (golden-ratio-sequence n max-val)
           "Generate sequence using golden ratio"
           (map (lambda (i)
                        (modulo (inexact->exact (floor (* i phi))) max-val))
                (iota n)))
         
         ;;; ============================================
         ;;; ASCII NOTATION RENDERING
         ;;; ============================================
         
         (define (render-ascii-notation pattern)
           "Render rhythm pattern as ASCII"
           (string-append
            (apply string-append
                   (map (lambda (beat)
                                (cond
                                 ((= beat 1) "x ")
                                 ((= beat 0) ". ")
                                 (else (string-append (number->string beat) " "))))
                        pattern))
            "\n"))
         
         (define (render-drum-pattern kick snare hihat)
           "Render multi-track drum pattern"
           (string-append
            "K: " (render-ascii-notation kick)
            "S: " (render-ascii-notation snare)
            "H: " (render-ascii-notation hihat)))
         
         (define (render-melody-staff melody)
           "Render melody on ASCII staff (0-9 scale degrees)"
           (let* ((max-note (apply max melody))
                  (min-note (apply min melody))
                  (range (+ (- max-note min-note) 1)))
                 (apply string-append
                        (map (lambda (row)
                                     (string-append
                                      (apply string-append
                                             (map (lambda (note)
                                                          (if (= note (- max-note row))
                                                              "* "
                                                              (if (zero? (modulo row 2)) "- " "  ")))
                                                  melody))
                                      "\n"))
                             (iota range)))))
         
         ;;; ============================================
         ;;; DEMONSTRATION FUNCTIONS
         ;;; ============================================
         
         (define (music-gen-demo)
           "Show various algorithmic music patterns"
           (display "\n=== PROCEDURAL MUSIC GENERATOR ===\n\n")
           
           (display "1. EUCLIDEAN RHYTHMS (Mathematical Perfection)\n")
           (display "   3 beats in 8 steps (Cuban tresillo): ")
           (display (render-ascii-notation (euclidean-rhythm 3 8)))
           (display "   5 beats in 8 steps (Flamenco):       ")
           (display (render-ascii-notation (euclidean-rhythm 5 8)))
           (display "   5 beats in 12 steps:                ")
           (display (render-ascii-notation (euclidean-rhythm 5 12)))
           
           (display "\n2. FIBONACCI MELODY (Pentatonic scale)\n")
           (let ((melody (fibonacci-melody 16 5)))
                (display "   Notes: ")
                (display melody)
                (display "\n")
                (display (render-melody-staff melody)))
           
           (display "\n3. CELLULAR AUTOMATON RHYTHM (Rule 30)\n")
           (let ((patterns (cellular-rhythm 30 16 8)))
                (for-each (lambda (p)
                                  (display "   ")
                                  (display (render-ascii-notation p)))
                          patterns))
           
           (display "\n4. GOLDEN RATIO SEQUENCE (8 notes, octave)\n")
           (let ((sequence (golden-ratio-sequence 16 8)))
                (display "   Notes: ")
                (display sequence)
                (display "\n")
                (display (render-melody-staff sequence)))
           
           (display "\n5. POLYRHYTHMIC DRUM PATTERN\n")
           (display (render-drum-pattern
                     (euclidean-rhythm 4 16)   ; Kick
                     (euclidean-rhythm 3 16)   ; Snare
                     (euclidean-rhythm 7 16))) ; Hihat
           
           (display "\nTry: (euclidean-rhythm k n) to explore different rhythms!\n"))
         
         (define (random-rhythm)
           "Generate random Euclidean rhythm"
           (let* ((n (+ 8 (random 9)))  ; 8-16 steps
                  (k (+ 2 (random (- n 2)))))  ; 2 to n-1 beats
                 (display (format "~a beats in ~a steps: " k n))
                 (display (render-ascii-notation (euclidean-rhythm k n)))))
         
         (define (random-melody)
           "Generate random melody using golden ratio or fibonacci"
           (let* ((length (+ 8 (random 17)))  ; 8-24 notes
                  (scale (+ 5 (random 8)))     ; 5-12 note scale
                  (use-fib (zero? (random 2))))
                 (display (format "~a-note melody (~a):\n"
                                  length
                                  (if use-fib "Fibonacci" "Golden Ratio")))
                 (let ((melody (if use-fib
                                   (fibonacci-melody length scale)
                                   (golden-ratio-sequence length scale))))
                      (display melody)
                      (display "\n")
                      (display (render-melody-staff melody)))))
         
         (define (export-pattern name pattern)
           "Export pattern to user/creations"
           (let ((filename (string-append "user/creations/" name ".rhythm")))
                (with-output-to-file filename
                                     (lambda ()
                                             (display ";; Generated rhythm pattern\n")
                                             (display pattern)
                                             (newline))
                                     'replace)
                (display (format "Exported to ~a\n" filename))))
         
         ) ; end library
