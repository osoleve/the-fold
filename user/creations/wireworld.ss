#!/usr/bin/env scheme-script
;; Wireworld - Digital Circuit Cellular Automaton
;; 4-state CA that simulates electronic circuits

(load "core/base/prelude.ss")

;; === WIREWORLD STATES ===
;; 0 = Empty (background)
;; 1 = Conductor (wire)
;; 2 = Electron head (blue)
;; 3 = Electron tail (red)

(define (count-electron-heads grid x y)
  "Count electron heads in Moore neighborhood"
  (let* ([height (vector-length grid)]
         [width (vector-length (vector-ref grid 0))])
        (let ([count 0])
             (do ([dy -1 (+ dy 1)])
                 ((> dy 1) count)
                 (do ([dx -1 (+ dx 1)])
                     ((> dx 1))
                     (when (not (and (= dx 0) (= dy 0)))
                           (let* ([nx (modulo (+ x dx) width)]
                                  [ny (modulo (+ y dy) height)]
                                  [cell (vector-ref (vector-ref grid ny) nx)])
                                 (when (= cell 2)
                                       (set! count (+ count 1))))))))))

(define (evolve-wireworld grid)
  "Evolve Wireworld one generation"
  (let* ([height (vector-length grid)]
         [width (vector-length (vector-ref grid 0))]
         [new-grid (make-vector height)])
        (do ([y 0 (+ y 1)])
            ((= y height) new-grid)
            (let ([row (make-vector width)])
                 (do ([x 0 (+ x 1)])
                     ((= x width))
                     (let* ([state (vector-ref (vector-ref grid y) x)]
                            [heads (count-electron-heads grid x y)]
                            [new-state
                             (cond
                              [(= state 0) 0]  ; Empty stays empty
                              [(= state 2) 3]  ; Head becomes tail
                              [(= state 3) 1]  ; Tail becomes conductor
                              [(= state 1)     ; Conductor becomes head if 1-2 heads nearby
                               (if (or (= heads 1) (= heads 2)) 2 1)]
                              [else 0])])
                           (vector-set! row x new-state)))
                 (vector-set! new-grid y row)))))

(define (render-wireworld grid)
  "Render Wireworld grid with colored ASCII"
  (do ([y 0 (+ y 1)])
      ((= y (vector-length grid)))
      (let ([row (vector-ref grid y)])
           (display
            (apply string-append
                   (map (lambda (cell)
                                (cond [(= cell 0) "  "]     ; Empty
                                      [(= cell 1) "▒▒"]     ; Conductor (wire)
                                      [(= cell 2) "██"]     ; Electron head
                                      [(= cell 3) "▓▓"]     ; Electron tail
                                      [else "  "]))
                        (vector->list row))))
           (newline))))

(define (list->wireworld-grid pattern)
  "Convert pattern list to grid"
  (let* ([height (length pattern)]
         [width (if (> height 0) (length (car pattern)) 0)]
         [grid (make-vector height)])
        (do ([y 0 (+ y 1)])
            ((= y height) grid)
            (let ([row (make-vector width)])
                 (do ([x 0 (+ x 1)])
                     ((= x width))
                     (vector-set! row x (list-ref (list-ref pattern y) x)))
                 (vector-set! grid y row)))))

(define (run-wireworld grid generations)
  "Run Wireworld simulation"
  (let loop ([gen 0] [current grid])
       (when (<= gen generations)
             (display (string-append "\n=== Generation " (number->string gen) " ===\n"))
             (render-wireworld current)
             (when (< gen generations)
                   (loop (+ gen 1) (evolve-wireworld current))))))

;; === CLASSIC WIREWORLD PATTERNS ===

;; Simple wire with electron
(define wire-pattern
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 2 3 1 1 1 1 1 1 1 1 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; Diode (FIXED: 2-wide wire with branch structure for propagation)
(define diode-pattern
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 2 3 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0)
    (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; OR gate (FIXED: 2-wide wires for horizontal propagation)
(define or-gate-pattern
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 2 3 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
    (0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; Clock generator (oscillator)
(define clock-pattern
  '((0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 1 1 0 0 0)
    (0 0 1 0 0 0 1 0 0 0)
    (0 2 1 0 0 0 1 3 0 0)
    (0 0 1 0 0 0 1 0 0 0)
    (0 0 1 1 1 1 1 0 0 0)
    (0 0 0 0 0 0 0 0 0 0)))

;; XOR gate (more complex)
(define xor-gate-pattern
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 2 3 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 1 1 1 1 1 1 1 1 1 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 1 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 1 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 1 0 0 0)
    (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
    (0 0 0 0 0 0 1 0 1 1 1 1 1 1 1 1 1 0 0 0)
    (0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; Binary counter (4-bit)
(define counter-pattern
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0)
    (0 2 1 1 1 1 1 0 1 0 0 0 1 0 1 1 1 1 1 1 1 1 1 0 0)
    (0 3 0 0 0 0 1 0 1 1 1 1 1 0 1 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; === DEMO ===

(define (demo)
  (display "\n╔════════════════════════════════════════════════════════╗\n")
  (display "║              WIREWORLD CIRCUIT SIMULATOR               ║\n")
  (display "╚════════════════════════════════════════════════════════╝\n")
  (display "\nLegend: ▒▒=wire  ██=electron head  ▓▓=electron tail\n")
  
  (display "\n🔌 Simple Wire - Electron Flowing\n")
  (display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  (run-wireworld (list->wireworld-grid wire-pattern) 8)
  
  (display "\n⚡ Diode - One-Way Current Flow\n")
  (display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  (run-wireworld (list->wireworld-grid diode-pattern) 10)
  
  (display "\n🔄 Clock Generator - Oscillating Signal\n")
  (display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  (run-wireworld (list->wireworld-grid clock-pattern) 12)
  
  (display "\n🚪 OR Gate - Logic Circuit\n")
  (display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  (run-wireworld (list->wireworld-grid or-gate-pattern) 15)
  
  (display "\n✨ Wireworld Demo Complete!\n")
  (display "\nWireworld rules:\n")
  (display "  • Empty → stays empty\n")
  (display "  • Electron head → becomes tail\n")
  (display "  • Electron tail → becomes wire\n")
  (display "  • Wire → becomes head if 1-2 heads nearby\n"))

(demo)
