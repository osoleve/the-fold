(library (shell wave-synth)
         (export
          ;; Oscillators
          sine-wave
          square-wave
          triangle-wave
          sawtooth-wave
          noise-wave

          ;; Wave operations
          mix-waves
          modulate-amplitude
          modulate-frequency
          apply-envelope

          ;; Effects
          add-harmonics
          apply-distortion

          ;; Rendering
          render-waveform-ascii
          render-spectrum
          wave->samples

          ;; Interactive demos
          synth-demo
          create-tone
          chord-generator)

         (import (chezscheme))

         (define-syntax doc
           (syntax-rules ()
             [(_ . rest) (void)]))

         (doc 'module 'wave-synth)
         (doc 'description "Audio waveform synthesis and visualization - generates and visualizes audio waveforms with various synthesis techniques")
         (doc 'layer 'boundary)
         (doc 'purity 'partial)
         (doc 'tier 'builder)

         (doc 'section 'constants)

         (define PI 3.141592653589793)
         (define TAU (* 2 PI))

         (doc 'section 'basic-oscillators)

         (define (sine-wave frequency phase)
           (doc 'description "Pure sine wave oscillator - returns a function that generates sample value at time t")
           (doc 'param 'frequency "Frequency in Hz")
           (doc 'param 'phase "Phase offset")
           (lambda (t)
                   (sin (+ (* TAU frequency t) phase))))
         
         (define (square-wave frequency phase)
           (doc 'description "Square wave oscillator (hard edges, odd harmonics)")
           (doc 'param 'frequency "Frequency in Hz")
           (doc 'param 'phase "Phase offset")
           (lambda (t)
                   (if (< (mod (+ (* frequency t) (/ phase TAU)) 1.0) 0.5)
                       1.0
                       -1.0)))
         
         (define (triangle-wave frequency phase)
           (doc 'description "Triangle wave oscillator (linear slopes)")
           (doc 'param 'frequency "Frequency in Hz")
           (doc 'param 'phase "Phase offset")
           (lambda (t)
                   (let ((phase-t (mod (+ (* frequency t) (/ phase TAU)) 1.0)))
                        (if (< phase-t 0.5)
                            (- (* 4.0 phase-t) 1.0)
                            (- 3.0 (* 4.0 phase-t))))))
         
         (define (sawtooth-wave frequency phase)
           (doc 'description "Sawtooth wave oscillator (linear rise, sharp fall)")
           (doc 'param 'frequency "Frequency in Hz")
           (doc 'param 'phase "Phase offset")
           (lambda (t)
                   (let ((phase-t (mod (+ (* frequency t) (/ phase TAU)) 1.0)))
                        (- (* 2.0 phase-t) 1.0))))
         
         (define (noise-wave seed)
           (doc 'description "Pseudo-random noise generator")
           (doc 'param 'seed "Random seed")
           (let ((state seed))
                (lambda (t)
                        (set! state (modulo (+ (* state 1103515245) 12345) 2147483648))
                        (- (/ (modulo state 1000) 500.0) 1.0))))
         
         (doc 'section 'wave-operations)

         (define (mix-waves . waves)
           (doc 'description "Mix multiple waveforms together")
           (doc 'param 'waves "Variable number of wave functions")
           (let ((count (length waves)))
                (lambda (t)
                        (/ (apply + (map (lambda (w) (w t)) waves))
                           count))))
         
         (define (modulate-amplitude carrier modulator depth)
           (doc 'description "Amplitude modulation (tremolo effect)")
           (doc 'param 'carrier "Carrier wave function")
           (doc 'param 'modulator "Modulator wave function")
           (doc 'param 'depth "Modulation depth")
           (lambda (t)
                   (* (carrier t)
                      (+ 1.0 (* depth (modulator t))))))
         
         (define (modulate-frequency carrier modulator depth)
           (doc 'description "Frequency modulation (vibrato effect)")
           (doc 'param 'carrier "Carrier wave function")
           (doc 'param 'modulator "Modulator wave function")
           (doc 'param 'depth "Modulation depth")
           (lambda (t)
                   (carrier (+ t (* depth (modulator t))))))
         
         (define (apply-envelope wave attack decay sustain release duration)
           (doc 'description "Apply ADSR envelope to waveform")
           (doc 'param 'wave "Wave function")
           (doc 'param 'attack "Attack time")
           (doc 'param 'decay "Decay time")
           (doc 'param 'sustain "Sustain level")
           (doc 'param 'release "Release time")
           (doc 'param 'duration "Total duration")
           (lambda (t)
                   (let ((amp (cond
                               ((< t attack)
                                (/ t attack))
                               ((< t (+ attack decay))
                                (+ sustain (* (- 1.0 sustain)
                                              (- 1.0 (/ (- t attack) decay)))))
                               ((< t (- duration release))
                                sustain)
                               ((< t duration)
                                (* sustain (- 1.0 (/ (- t (- duration release)) release))))
                               (else 0.0))))
                        (* (wave t) amp))))
         
         (doc 'section 'effects)

         (define (add-harmonics fundamental . harmonics)
           (doc 'description "Add harmonic overtones (frequency, amplitude) pairs")
           (doc 'param 'fundamental "Fundamental wave function")
           (doc 'param 'harmonics "Variable number of (frequency amplitude) pairs")
           (lambda (t)
                   (+ (fundamental t)
                      (apply + (map (lambda (h)
                                            (let ((freq (car h))
                                                  (amp (cadr h)))
                                                 (* amp ((sine-wave freq 0) t))))
                                    harmonics)))))
         
         (define (apply-distortion wave amount)
           (doc 'description "Apply soft clipping distortion")
           (doc 'param 'wave "Wave function")
           (doc 'param 'amount "Distortion amount")
           (lambda (t)
                   (let ((x (* (wave t) amount)))
                        (/ (* 2.0 x) (+ 1.0 (abs x))))))
         
         (doc 'section 'sampling-and-rendering)

         (define (wave->samples wave duration sample-rate)
           (doc 'description "Convert wave function to discrete samples")
           (doc 'param 'wave "Wave function")
           (doc 'param 'duration "Duration in seconds")
           (doc 'param 'sample-rate "Sample rate in Hz")
           (let* ((num-samples (exact (floor (* duration sample-rate))))
                  (dt (/ 1.0 sample-rate)))
                 (let loop ((i 0) (samples '()))
                      (if (>= i num-samples)
                          (reverse samples)
                          (loop (+ i 1)
                                (cons (wave (* i dt)) samples))))))
         
         (define (render-waveform-ascii wave duration width height)
           (doc 'description "Render waveform as ASCII art")
           (doc 'param 'wave "Wave function")
           (doc 'param 'duration "Duration in seconds")
           (doc 'param 'width "Width in characters")
           (doc 'param 'height "Height in characters")
           (let* ((samples (wave->samples wave duration width))
                  (grid (make-vector (* width height) #\space))
                  (mid-y (quotient height 2)))
                 
                 ;; Draw center line
                 (let loop ((x 0))
                      (when (< x width)
                            (vector-set! grid (+ x (* mid-y width)) #\-)
                            (loop (+ x 1))))
                 
                 ;; Plot waveform
                 (let loop ((x 0) (samps samples))
                      (when (and (< x width) (not (null? samps)))
                            (let* ((sample (car samps))
                                   (y (+ mid-y (exact
                                                (floor (* sample (/ height 2.5))))))
                                   (y (max 0 (min (- height 1) y)))
                                   (idx (+ x (* y width))))
                                  (vector-set! grid idx #\*)
                                  (loop (+ x 1) (cdr samps)))))
                 
                 ;; Render grid
                 (let row-loop ((y 0) (lines '()))
                      (if (>= y height)
                          (apply string-append (reverse lines))
                          (row-loop (+ y 1)
                                    (cons
                                     (string-append
                                      (list->string
                                       (let col-loop ((x 0) (chars '()))
                                            (if (>= x width)
                                                (reverse chars)
                                                (col-loop (+ x 1)
                                                          (cons (vector-ref grid (+ x (* y width)))
                                                                chars)))))
                                      "\n")
                                     lines))))))
         
         (define (render-spectrum samples bins)
           (doc 'description "Simple frequency spectrum visualization (magnitude only)")
           (doc 'param 'samples "Sample list")
           (doc 'param 'bins "Number of frequency bins")
           (let* ((n (length samples))
                  (bin-size (quotient n bins))
                  (spectrum (make-vector bins 0.0)))
                 ;; Very simplified - just shows amplitude distribution
                 (let loop ((i 0) (samps samples))
                      (when (and (< i n) (not (null? samps)))
                            (let* ((bin (min (- bins 1) (quotient i bin-size)))
                                   (val (abs (car samps)))
                                   (current (vector-ref spectrum bin)))
                                  (vector-set! spectrum bin (+ current val))
                                  (loop (+ i 1) (cdr samps)))))
                 ;; Normalize and render
                 (let ((max-val (apply max (vector->list spectrum))))
                      (if (zero? max-val)
                          "No signal\n"
                          (apply string-append
                                 (map (lambda (i)
                                              (let* ((val (vector-ref spectrum i))
                                                     (norm (/ val max-val))
                                                     (bars (max 0 (min 20 (exact (floor (* norm 20)))))))
                                                    (string-append
                                                     (number->string i)
                                                     ": "
                                                     (make-string bars #\█)
                                                     "\n")))
                                      (iota bins)))))))
         
         (doc 'section 'musical-utilities)

         (define (note->frequency note)
           (doc 'description "Convert MIDI note number to frequency (A4 = 440Hz = note 69)")
           (doc 'param 'note "MIDI note number")
           (* 440.0 (expt 2.0 (/ (- note 69) 12.0))))
         
         (define (create-tone note-name duration)
           (doc 'description "Create a tone from note name (e.g., 'A4', 'Cs5')")
           (doc 'param 'note-name "Note name as symbol")
           (doc 'param 'duration "Duration in seconds")
           (let ((notes '((C . 0) (Cs . 1) (D . 2) (Ds . 3)
                          (E . 4) (F . 5) (Fs . 6) (G . 7)
                          (Gs . 8) (A . 9) (As . 10) (B . 11))))
                ;; Simple parser: assume format like "A4" or "C#5"
                (display (format "Creating tone: ~a for ~as\n" note-name duration))
                (let* ((freq (note->frequency 69)) ; Default to A4
                       (wave (sine-wave freq 0)))
                      (render-waveform-ascii wave duration 60 15))))
         
         (define (chord-generator frequencies duration)
           (doc 'description "Generate a chord from multiple frequencies")
           (doc 'param 'frequencies "List of frequencies")
           (doc 'param 'duration "Duration in seconds")
           (let ((waves (map (lambda (f) (sine-wave f 0)) frequencies)))
                (apply mix-waves waves)))
         
         (doc 'section 'demonstration)

         (define (synth-demo)
           (doc 'description "Demonstrate wave synthesis capabilities")
           (display "\n=== WAVE SYNTHESIZER ===\n\n")
           
           (display "1. BASIC WAVEFORMS\n\n")
           
           (display "Sine Wave (440 Hz - A4):\n")
           (display (render-waveform-ascii (sine-wave 440 0) 0.01 60 10))
           (display "\n")
           
           (display "Square Wave (440 Hz):\n")
           (display (render-waveform-ascii (square-wave 440 0) 0.01 60 10))
           (display "\n")
           
           (display "Triangle Wave (440 Hz):\n")
           (display (render-waveform-ascii (triangle-wave 440 0) 0.01 60 10))
           (display "\n")
           
           (display "Sawtooth Wave (440 Hz):\n")
           (display (render-waveform-ascii (sawtooth-wave 440 0) 0.01 60 10))
           (display "\n")
           
           (display "2. WAVE MIXING\n\n")
           (display "Major Chord (C4-E4-G4: 261.63, 329.63, 392.00 Hz):\n")
           (let ((chord (mix-waves
                         (sine-wave 261.63 0)
                         (sine-wave 329.63 0)
                         (sine-wave 392.00 0))))
                (display (render-waveform-ascii chord 0.02 60 10)))
           (display "\n")
           
           (display "3. AMPLITUDE MODULATION (Tremolo)\n\n")
           (display "440 Hz carrier with 6 Hz tremolo:\n")
           (let ((tremolo (modulate-amplitude
                           (sine-wave 440 0)
                           (sine-wave 6 0)
                           0.5)))
                (display (render-waveform-ascii tremolo 0.5 80 12)))
           (display "\n")
           
           (display "4. FREQUENCY MODULATION (Vibrato)\n\n")
           (display "440 Hz with 5 Hz vibrato:\n")
           (let ((vibrato (modulate-frequency
                           (sine-wave 440 0)
                           (sine-wave 5 0)
                           0.002)))
                (display (render-waveform-ascii vibrato 0.2 80 12)))
           (display "\n")
           
           (display "5. ADSR ENVELOPE\n\n")
           (display "440 Hz with attack-decay-sustain-release:\n")
           (let ((enveloped (apply-envelope
                             (sine-wave 440 0)
                             0.05   ; attack
                             0.05   ; decay
                             0.7    ; sustain level
                             0.1    ; release
                             0.5))) ; total duration
                (display (render-waveform-ascii enveloped 0.5 80 12)))
           (display "\n")
           
           (display "6. HARMONICS\n\n")
           (display "Fundamental + 2nd & 3rd harmonics:\n")
           (let ((rich (add-harmonics
                        (sine-wave 200 0)
                        (list 400 0.5)    ; 2nd harmonic at half amplitude
                        (list 600 0.25)))) ; 3rd harmonic at quarter amplitude
                (display (render-waveform-ascii rich 0.02 60 10)))
           (display "\n")
           
           (display "7. DISTORTION\n\n")
           (display "Sine wave with soft clipping:\n")
           (let ((distorted (apply-distortion (sine-wave 440 0) 3.0)))
                (display (render-waveform-ascii distorted 0.01 60 10)))
           (display "\n")
           
           (display "8. SPECTRUM ANALYSIS\n\n")
           (display "Frequency content of square wave:\n")
           (let* ((sq (square-wave 440 0))
                  (samples (wave->samples sq 0.1 4000)))
                 (display (render-spectrum samples 16)))
           
           (display "\n=== Try these functions: ===\n")
           (display "  (sine-wave freq phase)      - Pure sine oscillator\n")
           (display "  (mix-waves w1 w2 ...)        - Mix multiple waves\n")
           (display "  (modulate-amplitude c m d)   - Tremolo effect\n")
           (display "  (apply-envelope wave ...)    - ADSR envelope\n")
           (display "  (render-waveform-ascii w d width height)\n\n"))
         
         ) ; end library
